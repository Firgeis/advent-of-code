let print_list name list =
  Printf.printf "\n%s[" name;
  Array.iter (fun x -> Printf.printf "%i, " x) list;
  Printf.printf "]";
  ()

let sweep forest =
  let maxx = (Array.length forest - 1) in
  let maxy = (Array.length (Array.get forest 0) - 1) in
  print_endline "";
  let rec aux index indey laterals horizontals direction =
    let current_tree = forest.(indey).(index) in
    let current_lateral = Array.get laterals indey in
    let current_horizontal = Array.get horizontals index in
    let next_lateral = if (fst current_lateral < current_tree) then (current_tree, index) else current_lateral in
    let next_horizontal = if (fst current_horizontal < current_tree) then (current_tree, indey) else current_horizontal in
    (*Printf.printf "CurrentH: %i\n" (fst current_horizontal);
    Printf.printf "Lateral: %i %i\n " (fst next_lateral) (snd next_lateral);
    Printf.printf "Horizontal: %i %i\n " (snd next_horizontal) (snd next_horizontal);*)
    Array.set laterals indey next_lateral;
    Array.set horizontals index next_horizontal;
    if direction then 
      (match index, indey with
      | x, y when x < maxx -> aux (x + 1) y laterals horizontals direction
      | x, y when x = maxx && y < maxy -> aux 0 (y + 1) laterals horizontals direction
      | x, y when x = maxx && y = maxy -> (laterals, horizontals)
      | _ -> assert false)
    else
      (match index, indey with
      | x, y when x > 0 -> aux (x - 1) y laterals horizontals direction
      | x, y when x = 0 && y > 0 -> aux maxx (y - 1) laterals horizontals direction
      | x, y when x = 0 && y = 0 -> (laterals, horizontals)
      | _ -> assert false)
  in
  
  let laterals, horizontals = aux 0 0 (Array.init (maxx + 1) (fun _ -> 0,0)) (Array.init (maxy + 1) (fun _ -> 0,0)) true in
  let from_left = Array.map snd laterals in
  let from_top = Array.map snd horizontals in

  let laterals, horizontals = aux maxx maxy (Array.init (maxx + 1) (fun _ -> 0,0)) (Array.init (maxy + 1) (fun _ -> 0,0)) true in
  let from_right = Array.map snd laterals in
  let from_bottom = Array.map snd horizontals in

  (*print_list "Left" from_left;
  print_list "Right" from_right;
  print_list "Top" from_top;
  print_list "Bottom" from_bottom;*)

  let range_laterals = Array.combine from_left from_right in
  let range_horizontals = Array.combine from_top from_bottom in

  let not_visible =
    Array.mapi (fun i x -> let h = Array.get range_horizontals i in if (fst x) < (fst h) && (snd x) > (snd h) then 1 else 0 ) range_laterals
    |> Array.fold_left (fun acc x -> acc + x) 0
  in

  (maxx + 1) * (maxy + 1) - not_visible



let result1 =
  let forest =
    Helpers.lines "Day8.txt"
    |> Seq.map (fun line -> line |> String.to_seq |> Seq.map (fun c -> int_of_char c - 48) |> Array.of_seq)
    |> Array.of_seq
  in

  List.iter (fun x -> Printf.printf "\n[";  print_list "" x; Printf.printf "]";) forest;

  sweep forest