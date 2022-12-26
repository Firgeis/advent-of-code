type direction =
| Left
| Right
| Top
| Bottom

let sweep forest =
  let maxy = (Array.length forest - 1) in
  let maxx = (Array.length (Array.get forest 0) - 1) in
  let rec aux index indey direction maxline new_arr =
    let current = forest.(indey).(index) in
    if current > maxline then
      new_arr.(indey).(index) <- 1
    else
      new_arr.(indey).(index) <- 0
    ;
    let setmaxline = if current > maxline then current else maxline in
    match direction with
    | Left ->
      (match index, indey with
      | x, y when x < maxx -> Printf.printf "\n%i" x; aux (x + 1) y direction setmaxline new_arr
      | x, y when x = maxx && y < maxy -> Printf.printf "A\n%i" x; aux 0 (y + 1) direction 0 new_arr
      | _ -> new_arr)
    | Right -> 
      (match index, indey with
      | x, y when x < maxx -> aux (x - 1) y direction setmaxline new_arr
      | x, y when x = maxx && y < maxy -> aux maxx (y + 1) direction 0 new_arr
      | _ -> new_arr)
    | Top ->
      (match index, indey with
      | x, y when y < maxy -> aux x (y + 1) direction setmaxline new_arr
      | x, y when y = maxy && x < maxx -> aux (x + 1) 0 direction 0 new_arr
      | _ -> new_arr)
    | Bottom ->
      (match index, indey with
      | x, y when y < maxy -> aux x (y - 1) direction setmaxline new_arr
      | x, y when y = maxy && x < maxx -> aux (x + 1) maxy direction 0 new_arr
      | _ -> new_arr)
  in

  let left = aux 0 0 Left 0 (Array.make_matrix maxx maxy 0) in
  let right = aux maxx maxy Right 0 (Array.make_matrix maxx maxy 0) in
  let top = aux 0 0 Top 0 (Array.make_matrix maxx maxy 0) in
  let bottom = aux maxx maxy Bottom 0 (Array.make_matrix maxx maxy 0) in

  let result = Array.make_matrix maxx maxy 0 in

  Array.iteri (
    fun y ys ->
      Array.iteri (fun x _ ->
        result.(y).(x) <- left.(y).(x) * right.(y).(x) * top.(y).(x) * bottom.(y).(x))
    ys) result;

  result
  |> Array.fold_left (fun acc x -> acc + Array.fold_left (fun accc xx -> accc + xx) 0 x) 0

let result1 =
  Printf.printf "%s" (Sys.getcwd ());
  let forest =
    Helpers.lines "day8.txt"
    |> Seq.map (fun line -> line |> String.to_seq |> Seq.map (fun c -> int_of_char c - 48) |> Array.of_seq)
    |> Array.of_seq
  in

  sweep forest