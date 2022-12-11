let parse_zones zones =
  match zones with
  | a::b::_ -> int_of_string a, int_of_string b
  | _ -> assert false

let parse_sections line =
  let sections =
    String.split_on_char ',' line
    |> List.map (fun assign -> String.split_on_char '-' assign |> parse_zones)
  in
  match sections with
  | a::b::_ -> a, b
  | _ -> assert false

let is_section_enveloping section =
  match section with
  | (a, b) , (c ,d) when a <= c && b >= d -> 1
  | (a, b) , (c ,d) when c <= a && d >= b -> 1
  | _ -> 0

let processor section_func =
  Helpers.lines "day4.txt"
  |> Seq.map parse_sections
  |> Seq.map section_func
  |> Seq.fold_left (fun acc is_overlapped -> acc + is_overlapped) 0

let result1 =
  processor is_section_enveloping

let is_section_overlapping section =
  match section with
  | (a, b) , (c ,_) when a <= c && b >= c -> 1
  | (a, b) , (_ ,d) when a <= d && b >= d -> 1
  | (a, _) , (c ,d) when c <= a && d >= a -> 1
  | (_, b) , (c, d) when c <= b && d >= b -> 1
  | _ -> 0

let result2 =
  processor is_section_overlapping