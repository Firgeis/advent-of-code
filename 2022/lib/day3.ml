module Charset = Set.Make(Char)
let get_priority item =
  let code = Char.code item in
  if code > 96 then
    code - 96
  else
    code - 65 + 27

let find_in_rucksacks rucksacks =
  let half = (String.length rucksacks / 2) in
  let compartment_1 = String.sub rucksacks 0 half |> String.to_seq |> Charset.of_seq in
  let compartment_2 = String.sub rucksacks half half |> String.to_seq |> Charset.of_seq in
  Charset.inter compartment_1 compartment_2
  |> Charset.min_elt


let result1 =
  Helpers.lines "day3.txt"
  |> Seq.fold_left (fun acc sack -> find_in_rucksacks sack |> get_priority |> fun x -> acc + x) 0

let group_3 groupings item =
  let current = List.hd groupings in
  if List.length current < 3 then
    (item::current)::(List.tl groupings)
  else
    [item]::groupings

let find_in_group group =
  let charsets = group |> List.map (fun sack -> String.to_seq sack |> Charset.of_seq) in
  match charsets with
  | a::b::c::_ -> Charset.inter a b |> Charset.inter c |> Charset.min_elt
  | _ -> assert false

let result2 =
  Helpers.lines "day3.txt"
  |> Seq.fold_left group_3 [[]]
  |> List.map find_in_group
  |> List.fold_left (fun acc prio -> get_priority prio + acc) 0