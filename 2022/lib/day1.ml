let build elves line =
  if (String.length line) < 2 then
    []::elves
  else
    match elves with
    | [] -> [(int_of_string line)]::elves
    | h::t -> ((int_of_string line)::h)::t

let elves = Seq.fold_left (fun acc x -> build acc x) [] (Helpers.lines "day1.txt")
let sum = List.map (fun elf -> List.fold_left (fun acc x -> acc + x) 0 elf ) elves
let result1 = List.fold_left (fun acc x -> if acc > x then acc else x) 0 sum
let compare value =
  match value with
  | x, y when x > y -> -1
  | x, y when x = y -> 0
  | x, y when x < y -> 1
  | _ -> assert false
let sorted = List.sort (fun x y -> compare (x, y)) sum 
let result2 =
  match sorted with
  | a::b::c::_ -> a + b + c
  | _ -> assert false