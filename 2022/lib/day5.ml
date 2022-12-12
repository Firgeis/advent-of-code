let line_to_stacks line stacks =
  line
  |> String.to_seq
  |> Seq.iteri (fun idx item -> 
      match idx with
      | x when x = 1 -> if Char.code item <> 32 then Array.get stacks 0 |> Stack.push item
      | x when (x - 1) mod 4 = 0 -> if Char.code item <> 32 then Array.get stacks (x/4) |> Stack.push item
      | _ -> ())

let parse_command line =
  let words = String.split_on_char ' ' line in
  int_of_string (List.nth words 1), int_of_string (List.nth words 3), int_of_string (List.nth words 5)

let execute_commands_9000 stacks commands = 
  let quantity, from, dest = commands in
  let origin = Array.get stacks (from - 1) in
  let destination = Array.get stacks (dest - 1) in
  for _ = 1 to quantity do
    Stack.push (Stack.pop origin) destination
  done

let mover execute_func =
  let first_line, lines = Seq.uncons (Helpers.lines "day5.txt") |> Option.get in
  let stack_count = ((String.length first_line) / 4) + 1 in
  let stacks = Array.init stack_count (fun _ -> Stack.create ()) in
  lines
  |> Seq.take_while (fun line -> not (String.contains line '1'))
  |> List.of_seq
  |> List.rev
  |> List.iter (fun line -> line_to_stacks line stacks);
  line_to_stacks first_line stacks;
  Seq.uncons lines |> ignore;
  Seq.iter (fun line -> parse_command line |> execute_func stacks) lines;
  Array.fold_left (fun acc stack -> acc ^ (Stack.top stack |> Char.escaped)) String.empty stacks 

let result1 =
  mover execute_commands_9000

let execute_commands_9001 stacks commands =
  let quantity, from, dest = commands in
  let origin = Array.get stacks (from - 1) in
  let destination = Array.get stacks (dest - 1) in
  List.init quantity (fun _ -> Stack.pop origin)
  |> List.rev
  |> List.iter (fun item -> Stack.push item destination)

let result2 =
  mover execute_commands_9001
