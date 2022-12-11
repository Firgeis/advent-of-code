type play =
| Scissors of int
| Rock of int
| Paper of int

let rock = Rock 1
let paper = Paper 2
let scissors = Scissors 3
let hand_to_play input =
  match input with
  | "A" -> rock
  | "B" -> paper
  | "C" -> scissors
  | "X" -> rock
  | "Y" -> paper
  | "Z" -> scissors
  | _ -> assert false

let play_result played =
  match played with
  | Scissors _, Paper x -> 0 + x
  | Scissors _, Scissors x -> 3 + x
  | Scissors _ , Rock x -> 6 + x
  | Paper _, Rock x -> 0 + x
  | Paper _, Paper x -> 3 + x
  | Paper _ , Scissors x -> 6 + x
  | Rock _, Scissors x -> 0 + x
  | Rock _, Rock x -> 3 + x
  | Rock _, Paper x -> 6 + x 

let strategy_1 =
  List.map (fun hand -> hand_to_play hand)

let game strategy = 
  Helpers.lines "day2.txt" 
  |> Seq.map (fun play -> String.split_on_char(' ') play |> strategy |> fun plays -> (List.nth plays 0, List.nth plays 1))
  |> Seq.map (fun plays -> play_result plays)
  |> Seq.fold_left (fun acc score -> acc + score) 0

let result1 =
  game strategy_1

let hand_to_play_2 input =
  match input with
  | "A", "X" -> scissors
  | "A", "Y" -> rock
  | "A", "Z" -> paper
  | "B", "X" -> rock
  | "B", "Y" -> paper
  | "B", "Z" -> scissors
  | "C", "X" -> paper
  | "C", "Y" -> scissors
  | "C", "Z" -> rock
  | _ -> assert false

let strategy_2 plays =
  let hands = 
    match plays with
    | a::b::_ -> a, b
    | _ -> assert false
  in
  let opponent = hand_to_play (fst hands) in
  let me = hand_to_play_2 hands in
  opponent::me::[]

  let result2 =
    game strategy_2