open Lwt
open TicTacToe

exception Bad_Command

type player = Lwt_io.input_channel * Lwt_io.output_channel
exception Disconnected of player
exception Invalid_command of string

let _ = Random.self_init()

(* sends string to player *)
let send player msg =
	let (_, ch_out) = player in
		try_lwt
			Lwt_io.write_line ch_out (msg ^ "\r")
		with 
			Lwt_io.Channel_closed _ -> Lwt.fail (Disconnected player)


(* recives string from player *)
let recv player =
	let (ch_in, _) = player in
		try_lwt
			Lwt_io.read_line ch_in >>= (fun s -> 
				(* Removes unsupported chars *)
				let filter_input_chars = function
						('a'..'z' | 'A'..'Z' | '0'..'9' | '-') as c -> c
					| _ -> ' '
				in
					Lwt.return (String.map (filter_input_chars) s))
		with
			(End_of_file | Unix.Unix_error(Unix.ECONNRESET, _, _)) -> Lwt.fail (Disconnected player)


(* recives command from player *)
let recv_cmd cmd_parser cmd_validator player =
	lwt str = recv player in
		lwt parsed_cmd = cmd_parser (String.trim str) in
			cmd_validator parsed_cmd >> Lwt.return parsed_cmd
			

(* wait correct command from player *)
let rec wait_cmd player cmd_reader invalid_cmd_handler =
	try_lwt
		cmd_reader player	
	with
		(Invalid_command s | Invalid_argument s) as exn ->
			lwt _ = invalid_cmd_handler player exn in
				wait_cmd player cmd_reader invalid_cmd_handler


(* Command parsers *)
let parser_start_cmd str = Lwt.return str

let parser_turn_cmd str =
	try_lwt
		let r = Scanf.sscanf str " %s %d %d " (fun cmd a1 a2 -> cmd, a1, a2) in
			Lwt.return r
	with
		_ ->
			Lwt.fail (Invalid_command str)

let parser_msg_cmd str =
	try_lwt
		let len = String.length str in
			Lwt.return (String.trim (String.sub str 0 4), String.trim (String.sub str 4 (len - 4)))
	with
		_ ->
			Lwt.fail (Invalid_command str)				
	

(* Command validators *)
let validator_start_cmd cmd =
	match String.uppercase cmd with
		"START"	->	Lwt.return ()
	|	_ as cmd -> Lwt.fail(Invalid_command cmd)


let validator_turn_cmd (cmd, x, y) =
	if String.uppercase cmd = "TURN" then
		if x < 1 || x > 3 then
			Lwt.fail (Invalid_argument "row")
		else
			if y < 1 || y > 3 then
				Lwt.fail (Invalid_argument "column")	
			else
				Lwt.return ()
	else
		Lwt.fail (Invalid_command cmd)
			

let validator_msg_cmd (cmd, message) =
	if String.uppercase cmd = "MSG" then
		if String.length message > 0 then
			Lwt.return ()
		else
			Lwt.fail (Invalid_argument "message")
	else
		Lwt.fail (Invalid_command cmd)


(* Logs invalid command and send error text to player *)
let invalid_command_handler player = function
	Invalid_command cmd_str				->	Lwt_log.debug ("Invalid command \"" ^ cmd_str ^ "\"") >> send player "Invalid command"
| Invalid_argument description	->	Lwt_log.debug ("Invalid command argument \"" ^ description ^ "\"") >> send player ("Invalid value of " ^ description ^ " argument")
| _ -> Lwt.fail (Failure "impossible")


(* Command waiters *)
(* START *)
let wait_cmd_start player =
	let cmd_reader = recv_cmd parser_start_cmd validator_start_cmd in
		wait_cmd player cmd_reader invalid_command_handler


(* TURN *)
let wait_cmd_turn player =
	let cmd_reader = recv_cmd parser_turn_cmd validator_turn_cmd in
		wait_cmd player cmd_reader invalid_command_handler

(* MSG *)
let wait_cmd_msg player =
	let cmd_reader = recv_cmd parser_msg_cmd validator_msg_cmd in
		wait_cmd player cmd_reader invalid_command_handler
			

(* Waits until player typed "START" and moves him to pending queue *)
let wait_for_start pending_player_box player =
	let intro_msg = 
		send player "Type \"START\" to start the game"
	and read_start _ = 
		wait_cmd_start player
	and start_game _ = 
		lwt _ = send player "Searching opponent" in
			Lwt_mvar.put pending_player_box player 
	in
		try_lwt
			intro_msg >>= read_start >>= start_game
		with
			Disconnected _ -> Lwt_log.info "User disconnected"


let colored_str_of_field game = 
	"\r\n" ^ (TicTacToe.colored_str_of_field game) ^ "\r\n"


(* Shows result of the game to players, and suggests them to begin a new game *)
let game_over p1 p2 game pending_player_box =
	let msg_for_p1, msg_for_p2 = 
		match TicTacToe.is_draw game with
			true ->	"Draw", "Draw"
		| false -> "You win", "You lose"
	in
		lwt _ = send p1 (colored_str_of_field game)
		and _ = send p2 (colored_str_of_field game)
		in
			lwt _ = send p1 msg_for_p1
			and _ = send p2 msg_for_p2
			in
				let wait_for_start = wait_for_start pending_player_box in
					Lwt.ignore_result (wait_for_start p1);
					Lwt.ignore_result (wait_for_start p2);
					Lwt.return ()


(* Provides ability to send a message to the current player from opponent, which waits of his turn *)
let rec msg_cmd_loop p1 p2 = 
	lwt (_, message) = wait_cmd_msg p2 in
		send p1 ("Opponent says: " ^ message) >> msg_cmd_loop p1 p2


(* Game loop *)
let rec play p1 p2 game pending_player_box =
	lwt _ = send p1 (colored_str_of_field game) in
		lwt _ = send p1 "It's your turn. Type \"turn <row> <column>\""
		and _ = send p2 "Opponent makes his move. You can send him a message, type \"msg <message>\"" in
			let rec _loop p1 p2 game pending_player_box = 
				lwt (_, y, x) = Lwt.pick [ wait_cmd_turn p1; msg_cmd_loop p1 p2 ] in
					try_lwt
						let game = TicTacToe.make_move game (x - 1) (y - 1) in
							if TicTacToe.game_is_over game then
								game_over p1 p2 game pending_player_box
							else
								play p2 p1 game pending_player_box
					with
						Failure s -> (
							lwt _ = send p1 s in
								_loop p1 p2 game pending_player_box
						)
			in
				_loop p1 p2 game pending_player_box


(* Returns player's opponent *)
let opponent p1 p2 p =
	if p == p1 then p2 else p1


(* Checks player's connection *)
let check_link p = Lwt.pick [ recv p; Lwt_unix.sleep 0.2 >> Lwt.return "" ]


(* Checks players connection *)
let check_players_link p1 p2 = check_link p1 >> check_link p2


(* Shuffles players and starts the game *)
let create_game p1 p2 pending_player_box =
	let msg = "Game started"
	and opponent = opponent p1 p2
	and p1, p2 = if Random.int 2 = 1 then p2, p1 else p1, p2
	in
		try_lwt
				lwt _ = check_players_link p1 p2 >> send p1 msg >> send p2 msg 
				in
					try_lwt
						play p1 p2 (TicTacToe.new_game ()) pending_player_box
					with
						Disconnected p -> (
							(try_lwt
									send (opponent p) "Opponent disconnected, please wait another"
								with
									Disconnected _ -> Lwt.return ()
							) >> Lwt.fail (Disconnected p)
						)
		with 
			Disconnected p -> (
				Lwt_log.info "User disconnected" >> Lwt_mvar.put pending_player_box (opponent p) >> Lwt.return ()
			)


(* Listen "pending_player_box" for new awaiting player, creates a new game for a pair of awaiting players *)
let rec pending_loop pending_player_box =
	let take () = Lwt_mvar.take pending_player_box in
		lwt p1 = take () and p2 = take () in
				Lwt.ignore_result (create_game p1 p2 pending_player_box);
				pending_loop pending_player_box	


let pending_player_box = Lwt_mvar.create_empty () in 
	let _ = 
		let host = "0.0.0.0" and port = 2023 in let sockaddr = Unix.ADDR_INET ((Unix.inet_addr_of_string host), port) 
			and _process_conn player = 
				try_lwt
					Lwt_log.info "User connected" >>= (fun () -> wait_for_start pending_player_box player)
				with
					exn -> Lwt_log.error ~exn "Not caught exception"
			in
				Lwt_io.establish_server sockaddr (fun s -> Lwt.ignore_result (_process_conn s))
	in
		Lwt_main.run (pending_loop pending_player_box)
