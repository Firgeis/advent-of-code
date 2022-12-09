let read_line file_channel =
  try
    Some (input_line file_channel)
  with
  | End_of_file -> None
let lines filename = 
  let file_channel = open_in filename in 
  Seq.forever (fun _ -> read_line file_channel)
  |> Seq.take_while (fun line -> Option.is_some line)
  |> Seq.map (fun line -> Option.get line)