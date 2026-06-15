
(* Copy a folder into another… assuming that it only contains most files, but not with a different
  structure. *)

module List = struct
  include List

  let last = function
    | [] -> invalid_arg "last"
    | x :: l ->
      let rec aux previous = function
        | [] -> previous
        | x :: l -> aux x l in
      aux x l

end


(* Usage : cpstruct -from ... -to ... *)

let source_dir = ref ""
let dest_dir = ref ""
let verbose = ref false

let log fmt =
  if !verbose then Printf.ksprintf print_endline fmt
  else Printf.isprintf fmt

let warning =
  Printf.ksprintf (fun s -> prerr_endline ("🟧 Warning: " ^ s))

let error =
  Printf.ksprintf (fun s -> prerr_endline ("🟥 Error: " ^ s)) ;
  exit 1

let () =
  if not Sys.unix then warning "This program was designed and tested for Unix systems."

let () =
  Arg.parse (Arg.align (List.map (fun (c, a, d) -> (c, a, "\t" ^ d)) [
      ("-from", Arg.Set_string source_dir, "The source directory") ;
      ("-to", Arg.Set_string dest_dir, "The destination directory") ;
      ("-verbose", Arg.Set verbose, "Describe everything that is being done")
    ]))
    (fun f -> error "ignored argument %s" f)
    ("Usage: cpstruct [-verbose] -from source_dir -to dest_dir")

let () =
  let check name dir =
    if dir = "" then error "Empty %s directory." name ;
    if not (Sys.file_exists dir) then error "Unknown directory %s." dir ;
    if not (Sys.is_directory dir) then error "File %s is not a directory." dir in
  check "source" !source_dir ;
  check "destination" !dest_dir ;
  let normalise dir =
    if (!dir).(String.length !dir - 1) <> "/" then
      dir := !dir ^ "/" in
  normalise source_dir ;
  normalise dest_dir ;
  if !source_dir = !dest_dir then
    error "The source directory is identical to the destination directory."

(* We represent a path as the list of directories followed by the filename. *)
type path = string list

let path_to_file ?from (p : path) =
  match from with
  | None ->
    if p = [] then (
      warning "Somehow getting the root directory during computation." ;
      "/"
    ) else String.concat "/" p
  | Some from ->
    if p = [] then from
    else (from ^ "/" ^ String.concat "/" p)

(* Return as a list of list the content of a directory.
 For each file, we return the path (excluding the global diractory) as a list.
 An singleton path means that the file is directly in the [dir] directory. *)
let get_all_files global_dir : path list =
  log "Getting all files from %s." dir
  let rec aux prefix =
    log " Entering folder %s" (path_to_file ~from:global_dir prefix) ;
    let all_direct_files =
      Array.to_list (Sys.readdir (path_to_file ~from:global_dir prefix)) in
    List.concat_map (fun file ->
      let path = prefix @ [file] in
      if Sys.is_directory (path_to_file ~from:global_dir path) then
        aux path
      else path) (all_direct_files dir) in
  prefix []

module SMap = Map.Make (String)

(* For each file, we store its path. *)
let build_listing dir =
  List.fold_left (fun database path ->
    let file = List.last path in
    match SMap.find_opt file database with
    | None -> SMap.add file path database
    | Some p' ->
      warning "Filename %s present both in %s and %s. Ignoring the former." file
        (path_to_file ~from:dir path) (path_to_file ~from:dir p') ;
      database) SMap.empty (get_all_files dir)

let source_listing = build_listing !source_dir
let dest_listing = build_listing !dest_dir

let same_files path1 path2 =
  let file1 = path_to_file path1 in
  let file2 = path_to_file path2 in
  Digest.file file1 = Digest.file file2

let cp path1 path2 =
  ()

let mv path1 path2 =
  ()

let () =
  SMap.iter (fun file path ->
    match SMap.find_opt file dest_listing with
    | None ->
      log "🟨 File %s only present in source." file ;
      cp (source_dir :: path) (dest_dir :: path)
    | Some path' ->
      if same_files (source_dir :: path) (dest_dir :: path') then
        (* In this case, we don't copy it: we only move it in the target folder. *)
        mv (dest_dir :: path') (dest_dir :: path)
      else (
        warning "Files %s and %s don't match. Considering them to be different files."
          (path_to_file ~from:source_dir path) (path_to_file ~from:dest_dir path') ;
        cp (source_dir :: path) (dest_dir :: path)
      )
    ) source_listing

