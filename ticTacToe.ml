module TicTacToe : sig
	type game

	val new_game : unit -> game
	val make_move : game -> int -> int -> game
	val game_is_over : game -> bool
	val is_draw : game -> bool
	val field_to_string : game -> string
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


let _next_state field turn =
	let solution combo =
		let combo_chars = List.map (String.get field) combo in
			match combo_chars with
				[a; b; c] when a = b && b = c && c <> '.' -> true
			| _ -> false
	in
		try
			let a = List.hd (List.find (solution) _win_combos) in
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


let field_to_string (_, field) =
	let rows s =
		let columns s =
			let str = String.make 1 in
				let chars = [ str s.[0]; str s.[1]; str s.[2] ] in
					" " ^ String.concat " | " chars		
		in
			let sub = String.sub s in
				let raw_rows = [ sub 0 3; sub 3 3; sub 6 3 ] in
					let columns_rows = List.map (columns) raw_rows in
						String.concat "\n ---------\n" columns_rows
	in
		rows field
	
end
