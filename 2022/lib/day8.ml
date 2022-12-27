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
      | x, y when x < maxx -> aux (x + 1) y direction setmaxline new_arr
      | x, y when x = maxx && y < maxy -> aux 0 (y + 1) direction 0 new_arr
      | _ -> new_arr)
    | Right -> 
      (match index, indey with
      | x, y when x > 0 -> aux (x - 1) y direction setmaxline new_arr
      | x, y when x = 0 && y < maxy -> aux maxx (y + 1) direction 0 new_arr
      | _ -> new_arr)
    | Top ->
      (match index, indey with
      | x, y when y < maxy -> aux x (y + 1) direction setmaxline new_arr
      | x, y when y = maxy && x < maxx -> aux (x + 1) 0 direction 0 new_arr
      | _ -> new_arr)
    | Bottom ->
      (match index, indey with
      | x, y when y > 0 -> aux x (y - 1) direction setmaxline new_arr
      | x, y when y = 0 && x < maxx -> aux (x + 1) maxy direction 0 new_arr
      | _ -> new_arr)
  in
  let length_x = maxx + 1 in
  let length_y = maxy + 1 in
  let left = aux 0 0 Left 0 (Array.make_matrix length_x length_y 0) in
  let right = aux maxx 0 Right 0 (Array.make_matrix length_x length_y 0) in
  let top = aux 0 0 Top 0 (Array.make_matrix length_x length_y 0) in
  let bottom = aux 0 maxy Bottom 0 (Array.make_matrix length_x length_y 0) in

  let result = Array.make_matrix length_x length_y 0 in

  Array.iteri (
    fun y ys ->
      Array.iteri (fun x _ ->
        if x = 0 || y = 0 || x = maxx || y = maxy then
          result.(y).(x) <- 1
        else
          result.(y).(x) <- left.(y).(x) + right.(y).(x) + top.(y).(x) + bottom.(y).(x))
    ys) result;

  result
  |> Array.fold_left (fun acc x -> acc + Array.fold_left (fun accc xx -> if xx = 0 then accc + 1 else accc) 0 x) 0
  |> fun x -> (length_x * length_y) - x

let direction_score x y maxx maxy forest direction =
  let current = forest.(y).(x) in
  match direction with
  | Left ->
    Seq.unfold (fun x -> 
      if not (x = -1) then 
        if (forest.(y).(x)) >= current then 
          Some (forest.(y).(x), -1)
        else 
          Some (forest.(y).(x),(x - 1))
      else 
        None) (x - 1)
    |> fun s -> max (Seq.length s) 1
  | Right ->
    Seq.unfold (fun x -> 
      if not (x > maxx) then 
        if (forest.(y).(x)) >= current then 
          Some (forest.(y).(x), maxx + 1)
        else 
          Some (forest.(y).(x),(x + 1))
      else
        None) (x + 1)
    |> fun s -> max (Seq.length s) 1
  | Top ->
    Seq.unfold (fun y -> 
      if not (y = -1) then 
        if (forest.(y).(x)) >= current then
          Some (forest.(y).(x), -1)
        else 
          Some (forest.(y).(x),(y - 1))
      else
        None) (y - 1)
    |> fun s -> max (Seq.length s) 1
  | Bottom ->
    Seq.unfold (fun y -> 
      if not (y > maxy) then 
        if (forest.(y).(x)) >= current then 
          Some (forest.(y).(x), maxy + 1) 
        else 
          Some (forest.(y).(x),(y + 1)) 
      else 
        None) (y + 1)
    |> fun s -> max (Seq.length s) 1

let find_scenic_spot forest =
  let maxy = (Array.length forest - 1) in
  let maxx = (Array.length (Array.get forest 0) - 1) in
  let max = ref 0 in
  for y = 1 to (maxy - 1) do
    for x = 1 to (maxx - 1) do
      let this_direction_score = direction_score x y maxx maxy forest in
      let left = this_direction_score Left in
      let right = this_direction_score Right in
      let top = this_direction_score Top in
      let bottom = this_direction_score Bottom in
      let scenic_score = left * right * top * bottom in
      if scenic_score > !max then max := scenic_score;
    done;
  done;
  !max

let forest =
  Helpers.lines "day8.txt"
  |> Seq.map (fun line -> line |> String.to_seq |> Seq.map (fun c -> int_of_char c - 48) |> Array.of_seq)
  |> Array.of_seq

let result1 =
  sweep forest
let result2 =
  find_scenic_spot forest