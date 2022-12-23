(* Done horribly with a GADT but hey it typechecks *)
type _ filesystem_node =
| Folder : folder -> folder filesystem_node 
| File : file -> file filesystem_node
and filefolder = Filefolder: _ filesystem_node -> filefolder
and folder = { folder_name: string; mutable contents: filefolder list; parent: folder filesystem_node option}
and file = { file_name: string; size: int }

type command =
| CD of string
| LS
| DIR of string
| FILE of int * string

let parse_command line =
  match String.split_on_char ' ' line with
  | [_; command; arg] ->
    (match command with
    | "cd" -> CD arg
    | _ -> assert false)
  | [command; arg] ->
    (match command with
    | "$" -> LS
    | "dir" -> DIR arg
    | _ -> FILE (int_of_string command, arg))
  | _ -> assert false

let add_folder_to_folder filefolder name =
  let new_folder = Filefolder (Folder { folder_name = name; contents = []; parent = Some filefolder }) in
  match filefolder with | Folder f -> f.contents <- new_folder::f.contents; filefolder

let add_file_to_folder filefolder file =
  let new_file = Filefolder (File { file_name = snd file; size = fst file }) in
  match filefolder with | Folder f -> f.contents <- new_file::f.contents; filefolder

let find_root filefolder =
  let rec aux current =
    match current with | Folder f -> match f.parent with | Some p -> aux p | _ -> current in

  aux filefolder

let execute_command filefolder command =
  match command with
  | CD arg -> 
    (match arg with
    | ".." -> (match filefolder with | Folder f -> match f.parent with | Some p -> p | _ -> filefolder)
    | "/" -> find_root filefolder
    | _ -> 
      List.find (fun (Filefolder x: filefolder) -> match x with | Folder f -> f.folder_name = arg | File _ -> false) (match filefolder with | Folder f -> f.contents) |> fun ( Filefolder y: filefolder) -> match y with Folder f -> Folder f | File _ -> assert false )
  | LS -> filefolder
  | DIR arg -> add_folder_to_folder filefolder arg
  | FILE (a, b) -> add_file_to_folder filefolder (a, b)

let calculate_folder_sizes filesystem =
  let rec aux fs folders =
    let rec iter current_size current_folder items parsed_folders =
      match items with
      | [] -> ((current_size, current_folder.folder_name), parsed_folders)
      | (Filefolder item: filefolder)::t ->
        (match item with
        | Folder f -> 
          let folder_size, folders = aux f folders in
          let folders_parsed = folder_size::folders in
          iter (current_size + fst folder_size) current_folder t (List.append folders_parsed parsed_folders)
        | File f -> iter (current_size + f.size) current_folder t parsed_folders)
    in
    iter 0 fs fs.contents folders
  in
  let root = find_root filesystem in
  match root with 
  Folder f -> 
    let root_folder, folders = aux f [] in
    root_folder::folders

let get_filesystem =
  let root = Folder { folder_name = "/"; contents = []; parent = None} in
 
  Helpers.lines "day7.txt"
  |> Seq.map parse_command
  |> Seq.fold_left ( fun acc c -> execute_command acc c) root

let result1 =
  get_filesystem
  |> calculate_folder_sizes
  |> List.filter (fun x -> (fst x) < 100000)
  |> List.fold_left (fun acc x -> acc + fst x ) 0

let result2 =
  let folder_sizes = get_filesystem |> calculate_folder_sizes in
  let root_size = List.hd folder_sizes in
  let space_left = 70000000 - fst root_size in

  folder_sizes
  |> List.sort (fun a b -> fst a - fst b)
  |> List.find (fun x -> fst x >= 30000000 - space_left)
  |> fst