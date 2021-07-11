(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(* [@@@warning "-37"] *)
type t =
  (* | File of string  *)
  | Dir of string
[@@unboxed]

let simple_convert_node_path_to_os_path =
  if Sys.unix then fun x -> x
  else if Sys.win32 || Sys.cygwin then
    Ext_string.replace_slash_backward
  else failwith ("Unknown OS : " ^ Sys.os_type)


let cwd = lazy (Sys.getcwd())

let split_by_sep_per_os : string -> string list =
  if Ext_sys.is_windows_or_cygwin then
  fun x ->
    (* on Windows, we can still accept -bs-package-output lib/js *)
    Ext_string.split_by
      (fun x -> match x with |'/' |'\\' -> true | _ -> false) x
  else
  fun x -> Ext_string.split x '/'

(** example
    {[
      "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
        "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
    ]}

    The other way
    {[

      "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
        "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
    ]}
    {[
      "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib//ocaml_array.ml"
    ]}
    {[
      /a/b
      /c/d
    ]}
*)
let node_relative_path
    ~from:(file_or_dir_2 : t )
    (file_or_dir_1 : t)
  =
  let relevant_dir1 =
    match file_or_dir_1 with
    | Dir x -> x
    (* | File file1 ->  Filename.dirname file1 *) in
  let relevant_dir2 =
    match file_or_dir_2 with
    | Dir x -> x
    (* | File file2 -> Filename.dirname file2  *) in
  let dir1 = split_by_sep_per_os relevant_dir1 in
  let dir2 = split_by_sep_per_os relevant_dir2 in
  let rec go (dir1 : string list) (dir2 : string list) =
    match dir1, dir2 with
    | "." :: xs, ys -> go xs ys
    | xs , "." :: ys -> go xs ys
    | x::xs , y :: ys when x = y
      -> go xs ys
    | _, _ ->
      Ext_list.map_append  dir2  dir1  (fun _ ->  Literals.node_parent)
  in
  match go dir1 dir2 with
  | (x :: _ ) as ys when x = Literals.node_parent ->
    String.concat Literals.node_sep ys
  | ys ->
    String.concat Literals.node_sep
    @@ Literals.node_current :: ys


let node_concat ~dir base =
  dir ^ Literals.node_sep ^ base

let node_rebase_file ~from ~to_ file =

  node_concat
    ~dir:(
      if from = to_ then Literals.node_current
      else node_relative_path ~from:(Dir from) (Dir to_))
    file

let backslash = Str.regexp "\\"

let concat_path ?force_forward_slash_on_win path1 path2 =
  let concatenated = Filename.concat path1 path2 in
  match force_forward_slash_on_win with
    | None -> concatenated
    | Some () ->
      if Ext_sys.is_windows_or_cygwin then 
        Str.global_replace backslash "/" concatenated
      else concatenated

(***
   {[
     Filename.concat "." "";;
     "./"
   ]}
*)
let combine_specifically ?force_forward_slash_on_win path1 path2 =
  if Filename.is_relative path2 then
    if Ext_string.is_empty path2 then
      path1
    else
    if path1 = Filename.current_dir_name then
      path2
    else
    if path2 = Filename.current_dir_name
    then path1
    else
      concat_path ?force_forward_slash_on_win path1 path2
  else
    path2

let combine path1 path2 = combine_specifically path1 path2

let combine_for_merlin path1 path2 =
  combine_specifically ~force_forward_slash_on_win:() path1 path2





let (//) x y =
  if x = Filename.current_dir_name then y
  else if y = Filename.current_dir_name then x
  else Filename.concat x y


let concat ?force_forward_slash_on_win x y =
  let concatenated = x // y in
  match force_forward_slash_on_win with
    | None -> concatenated
    | Some () ->
      if Ext_sys.is_windows_or_cygwin then 
        Str.global_replace backslash "/" concatenated
      else concatenated

(**
   {[
     split_aux "//ghosg//ghsogh/";;
     - : string * string list = ("/", ["ghosg"; "ghsogh"])
   ]}
   Note that
   {[
     Filename.dirname "/a/" = "/"
       Filename.dirname "/a/b/" = Filename.dirname "/a/b" = "/a"
   ]}
   Special case:
   {[
     basename "//" = "/"
       basename "///"  = "/"
   ]}
   {[
     basename "" =  "."
       basename "" = "."
       dirname "" = "."
       dirname "" =  "."
   ]}
*)
let split_aux p =
  let rec go p acc =
    let dir = Filename.dirname p in
    if dir = p then dir, acc
    else
      let new_path = Filename.basename p in
      (* In Windows, directory separator can either be `\` or `/`.
         [Filename.dir_sep] holds `\` in Windows.
      *)
      if Ext_string.equal new_path Filename.dir_sep || Ext_string.equal new_path "/" then
        go dir acc
        (* We could do more path simplification here
           leave to [rel_normalized_absolute_path]
        *)
      else
        go dir (new_path :: acc)

  in go p []

let dir_sep_pattern_on_win = Str.regexp "/"

let path_equal_canonically ?force_forward_slash_on_win path1 path2 =

  match force_forward_slash_on_win, Ext_sys.is_windows_or_cygwin with
    | None, true ->
      let left = Str.global_replace dir_sep_pattern_on_win "\\\\" path1 in
      let right = Str.global_replace dir_sep_pattern_on_win "\\\\" path2 in
      Ext_string.equal left right
    | _ -> Ext_string.equal path1 path2


(**
   TODO: optimization
   if [from] and [to] resolve to the same path, a zero-length string is returned

   This function is useed in [es6-global] and
   [amdjs-global] format and tailored for `rollup`
*)
let rel_normalized_absolute_path ?force_forward_slash_on_win ~from to_ =
  let merge_parent_segment acc segment =
    if segment = Filename.current_dir_name then
      acc
    else
      concat ?force_forward_slash_on_win acc Ext_string.parent_dir_lit
  in
  let root1, paths1 = split_aux from in
  let root2, paths2 = split_aux to_ in
  if not (path_equal_canonically ?force_forward_slash_on_win root1 root2) then
    root2
  else
    let rec go xss yss =
      match xss, yss with
      | x::xs, y::ys ->
        if Ext_string.equal x  y then go xs ys
        else if x = Filename.current_dir_name then go xs yss
        else if y = Filename.current_dir_name then go xss ys
        else
          let start =
            Ext_list.fold_left xs Ext_string.parent_dir_lit merge_parent_segment
          in
          Ext_list.fold_left yss start (concat ?force_forward_slash_on_win)
      | [], [] -> Ext_string.empty
      | [], y::ys -> Ext_list.fold_left ys y (concat ?force_forward_slash_on_win)
      | x::xs, [] ->
        let start = if x = Filename.current_dir_name then "" else Ext_string.parent_dir_lit in
        Ext_list.fold_left xs start merge_parent_segment
     in
    let v =  go paths1 paths2  in

    if Ext_string.is_empty v then  Literals.node_current
    else
    if
      v = "."
      || v = ".."
      || Ext_string.starts_with v "./"
      || Ext_string.starts_with v "../"
      || Ext_string.starts_with v ("." ^ Filename.dir_sep)
      || Ext_string.starts_with v (".." ^ Filename.dir_sep)
    then v
    else concat_path ?force_forward_slash_on_win "." v

(*TODO: could be hgighly optimized later
  {[
    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/a/b/../c../d/e/f";;

    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/gsho/./../..";;

    normalize_absolute_path "/a/b/c/d";;

    normalize_absolute_path "/a/b/c/d/";;

    normalize_absolute_path "/a/";;

    normalize_absolute_path "/a";;
  ]}
*)
(** See tests in {!Ounit_path_tests} *)
let normalize_absolute_path ?force_forward_slash_on_win x =
  let drop_if_exist xs =
    match xs with
    | [] -> []
    | _ :: xs -> xs in
  let rec normalize_list acc paths =
    match paths with
    | [] -> acc
    | x :: xs ->
      if Ext_string.equal x Ext_string.current_dir_lit then
        normalize_list acc xs
      else if Ext_string.equal x Ext_string.parent_dir_lit then
        normalize_list (drop_if_exist acc ) xs
      else
        normalize_list (x::acc) xs
  in
  let root, paths = split_aux x in
  let rev_paths =  normalize_list [] paths in
  let rec go acc rev_paths =
    match rev_paths with
    | [] -> concat_path ?force_forward_slash_on_win root acc
    | last::rest ->  go (concat_path ?force_forward_slash_on_win last acc ) rest  in
  match rev_paths with
  | [] -> root
  | last :: rest -> go last rest




let absolute_path ?force_forward_slash_on_win cwd s =
  let process s =
    let s =
      if Filename.is_relative s then
        concat_path ?force_forward_slash_on_win (Lazy.force cwd) s
      else s in
    (* Now simplify . and .. components *)
    let rec aux s =
      let base,dir  = Filename.basename s, Filename.dirname s  in
      if dir = s then dir
      else if base = Filename.current_dir_name then aux dir
      else if base = Filename.parent_dir_name then Filename.dirname (aux dir)
      else concat_path ?force_forward_slash_on_win (aux dir) base
    in aux s  in
  process s

let absolute_cwd_path ?force_forward_slash_on_win s =
  absolute_path ?force_forward_slash_on_win cwd  s

(* let absolute cwd s =
  match s with
  | File x -> File (absolute_path cwd x )
  | Dir x -> Dir (absolute_path cwd x) *)

let check_suffix_case =
  Ext_string.ends_with

(* Input must be absolute directory *)
let rec find_root_filename ~cwd filename   =
  if Sys.file_exists ( Filename.concat cwd  filename) then cwd
  else
    let cwd' = Filename.dirname cwd in
    if String.length cwd' < String.length cwd then
      find_root_filename ~cwd:cwd'  filename
    else
      Ext_fmt.failwithf
        ~loc:__LOC__
        "%s not found from %s" filename cwd


let find_package_json_dir cwd  =
  find_root_filename ~cwd  Literals.bsconfig_json

let package_dir = lazy (find_package_json_dir (Lazy.force cwd))

let real_path link_path =
  match Unix.lstat link_path with
  | { st_kind = S_LNK; _ } ->
    let realpath = Unix.readlink link_path in
    if Filename.is_relative realpath then
      (Filename.dirname link_path) // realpath
    else
      realpath
  | _other ->
    link_path

