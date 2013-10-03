open Lwt
open TicTacToe

exception Bad_Command

type player = Lwt_io.input_channel * Lwt_io.output_channel
exception Disconnected of player
exception Invalid_Command of string

let _ = Random.self_init()

(* sends string to player *)
let send player msg =
	let (_, ch_out) = player in
		try_lwt
			Lwt_io.write_line ch_out msg
		with 
			Lwt_io.Channel_closed _ -> Lwt.fail (Disconnected player)

(* recives string from player *)
let recv player =
	let (ch_in, _) = player in
		try_lwt
			Lwt_io.read_line ch_in
		with
			End_of_file -> Lwt.fail (Disconnected player)


(* recives command from player *)
let resv_cmd cmd_parser cmd_validator player =
	lwt str = recv player in
		lwt parsed_cmd = cmd_parser (String.uppercase (String.trim str)) in
			match_lwt cmd_validator parsed_cmd with
				true -> Lwt.return parsed_cmd
			| _ -> Lwt.fail (Invalid_Command str)
			

(* wait correct command from player *)
let rec wait_cmd player cmd_reader invalid_cmd_handler =
	try_lwt
		cmd_reader player	
	with
		Invalid_Command s ->
			lwt _ = invalid_cmd_handler player s in
				wait_cmd player cmd_reader invalid_cmd_handler


(* Command parsers *)
let parser_start_cmd str =
	match str with
		"START" -> Lwt.return str
	| _ -> Lwt.fail (Invalid_Command str)


let parser_turn_cmd str =
	try_lwt
		let r = Scanf.sscanf str " %s %d %d " (fun cmd a1 a2 -> cmd, a1, a2) in
			Lwt.return r
	with
		_ ->
			Lwt.fail (Invalid_Command str)


(* Command validators *)
let validator_start_cmd cmd = Lwt.return (cmd = "START")
let validator_turn_cmd (cmd, x, y) = Lwt.return (cmd = "TURN" && x >= 1 && x <= 3 && y >= 1 && y <= 3)


(* Logs invalid command and send error text to player *)
let invalid_command_handler player cmd_str =
	lwt _ = Lwt_log.debug ("Invalid command \"" ^ cmd_str ^ "\"")
	and _ = send player "Invalid command" in
		Lwt.return ()


(* Command waiters *)
(* START *)
let wait_cmd_start player =
	let cmd_reader = resv_cmd parser_start_cmd validator_start_cmd in
		wait_cmd player cmd_reader invalid_command_handler


(* TURN *)
let wait_cmd_turn player =
	let cmd_reader = resv_cmd parser_turn_cmd validator_turn_cmd in
		wait_cmd player cmd_reader invalid_command_handler


(* Ignores all commands from player *)
let ignore_all_cmds player error_msg =
	let ignore_parser str = Lwt.fail (Invalid_Command str)
	and ignore_validator _ = Lwt.return false
	and invalid_command_handler player _ =
		lwt _ = send player error_msg in
			Lwt.return ()
	in
		let cmd_reader = resv_cmd ignore_parser ignore_validator in 
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


(* Shows result of the game to players, and suggests them to begin a new game *)
let game_over p1 p2 game pending_player_box =
	let msg_for_p1, msg_for_p2 = 
		match TicTacToe.is_draw game with
			true ->	"Draw", "Draw"
		| false -> "You win", "You lose"
	in
		lwt _ = send p1 msg_for_p1
		and _ = send p2 msg_for_p2
		in
			let wait_for_start = wait_for_start pending_player_box in
				Lwt.ignore_result (wait_for_start p1);
				Lwt.ignore_result (wait_for_start p2);
				Lwt.return ()


(* Game loop *)
let rec play p1 p2 game pending_player_box =
	lwt _ = send p1 (TicTacToe.field_to_string game) in
		lwt _ = send p1 "It's your turn"
		and _ = send p2 "Opponent makes his move" in
			let rec _loop p1 p2 game pending_player_box = 
				lwt (_, y, x) = Lwt.pick [ wait_cmd_turn p1; ignore_all_cmds p2 "Be patient, opponent makes his move" ] in
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


(* Shuffles players and starts the game *)
let create_game p1 p2 pending_player_box =
	let msg = "Game started"
	and p1, p2 = if Random.int 100 mod 2 = 1 then p2, p1 else p1, p2
	in
		try_lwt
			lwt _ = send p1 msg and _ = send p2 msg
			in
				play p1 p2 (TicTacToe.new_game ()) pending_player_box
		with 
			Disconnected p -> (
				let opponent = if p == p1 then p2 else p1 in
					lwt _ = Lwt_log.info "User disconnected"
					and _ =
						try_lwt
							send opponent "Opponent disconnected, please wait another" >>= (fun () -> Lwt_mvar.put pending_player_box opponent)
						with
							Disconnected _ -> Lwt_log.info "User disconnected"
					in
						Lwt.return ()
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
