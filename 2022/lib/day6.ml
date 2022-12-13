module CharSet = Set.Make(Char)

let is_signal list quantity =
  let charset = CharSet.of_list (list |> Array.to_list) in
  CharSet.cardinal charset = quantity

let search_signal chars quantity = 
  let rec aux window idx =
    if is_signal window quantity then
      idx - 1 + quantity
    else
      aux (Array.sub chars idx quantity) (idx + 1)
  in
  aux (Array.sub chars 0 quantity) 0

let radio signal_size =
  let chars =
    Helpers.lines "day6.txt"
    |> Seq.uncons
    |> fun input -> Option.get input |> fst |> String.to_seq
    |> Array.of_seq
  in

  search_signal chars signal_size

let result1 =
  radio 4

let result2 =
  radio 14