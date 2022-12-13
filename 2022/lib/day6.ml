module CharSet = Set.Make(Char)
let add_char list c quantity =
  if List.length list < quantity then
    c::list
  else
    c::list |> List.rev |> List.tl |> List.rev

let is_signal list quantity =
  let charset = CharSet.of_list list in
  CharSet.cardinal charset = quantity

let search_signal window chars quantity = 
  let rec aux window chars idx =
    if is_signal window quantity then
      idx
    else
      match chars with
      | [] -> assert false
      | h::t -> 
        aux (add_char window h quantity) t (idx + 1)
  in
  aux window chars 0

let radio signal_size =
  let chars =
    Helpers.lines "day6.txt"
    |> Seq.uncons
    |> fun input -> Option.get input |> fst |> String.to_seq
    |> List.of_seq
  in

  search_signal [] chars signal_size

let result1 =
  radio 4

let result2 =
  radio 14