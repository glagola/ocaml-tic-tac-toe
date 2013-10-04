module TicTacToe : sig
	type game

	val new_game : unit -> game
	val make_move : game -> int -> int -> game
	val game_is_over : game -> bool
	val is_draw : game -> bool
	val colored_str_of_field : game -> string
end = struct

type player = X | O
type state = Continue of player | Win of player | Draw
type game = state * string

(* private *)

let _win_combos = [
	(* rows *)
	[0;1;2];
	[3;4;5];
	[6;7;8];

	(* columns *)
	[0;3;6];
	[1;4;7];
	[2;5;8];

	(* diagonals *)
	[0;4;8];
	[2;4;6];
]


let _make_move field turn x y =
	let i = x + y * 3 in
		match field.[i] with
			'.' -> field.[i] <- turn; field
		| _ -> raise (Failure "Cell is busy")


let _next_turn = function
	X -> O
| O -> X


let _player_to_char = function
	X -> 'X'
| O -> 'O'


let _char_to_player = function
	'X' -> X
| 'O' -> O
| _ -> raise (Invalid_argument "Only X/O allowed")

let _find_solution field =
	let solution combo =
		let combo_chars = List.map (String.get field) combo in
			match combo_chars with
				[a; b; c] when a = b && b = c && c <> '.' -> true
			| _ -> false
	in
		List.find (solution) _win_combos

let _next_state field turn =
	try
		let a = List.hd (_find_solution field) in
			Win (_char_to_player field.[a])
	with
		Not_found -> if String.contains field '.' then turn else Draw


(* public *)
let new_game () =
	let empty_field = String.make 9 '.' in
		(Continue X, empty_field)


let make_move game x y =
	let (state, field) = game in
		match state with
		| Continue turn ->
			let next_field = _make_move (String.copy field) (_player_to_char turn) x y in
				(_next_state next_field (Continue (_next_turn turn)), next_field)
		| _ -> game


let game_is_over (state, _) =
	match state with
	  Continue _ -> false
	| _ -> true


let is_draw (state, _) = state = Draw


let colored_str_of_field (_, field) =
	let win_combo_str =
		let win_combo =
			try
				_find_solution field
			with
				Not_found -> []
		in
			let win_combo_str_arr = List.map (fun i -> string_of_int i) win_combo
			in
				String.concat "" win_combo_str_arr
	in
		let rec str_loop accum i =
			let esc = String.make 1 (char_of_int 27)
			in
				let red		= esc ^ "[0;31m"
				and blue	= esc ^ "[1;34m"
				and cyan	= esc ^ "[0;36m"
				and reset	= esc ^ "[0m"
				in
					let color_of_cell i = function
							s	when String.contains win_combo_str i -> cyan
						|	"X" -> red
						|	"O" -> blue
						| _		-> ""
					in
						if i = String.length field then
							List.rev accum
						else
							let s = String.make 1 field.[i]
							in
								let colored_cell = (color_of_cell (string_of_int i).[0] s) ^ s ^ reset
								in
									str_loop (colored_cell :: accum) (i + 1)
		in
			let str_arr = str_loop [] 0
			in
				let rows = match str_arr with
						[_0;_1;_2;_3;_4;_5;_6;_7;_8] -> [[_0;_1;_2]; [_3;_4;_5]; [_6;_7;_8]]
					| _ -> raise (Failure "impossible")
				in
					let columns chars =
						" " ^ String.concat " | " chars
					in
						let columns_rows = List.map (columns) rows
						in
							let f i s = Printf.sprintf (" %d" ^^ "%s") (i+1) s
							in
								let crlf = "\r\n"
								in
									let rules_columns_rows = List.mapi (f) columns_rows
									and rule = "   1   2   3" ^ crlf
									in
										rule ^ (String.concat (crlf ^ "   ---------" ^ crlf) rules_columns_rows)

end
