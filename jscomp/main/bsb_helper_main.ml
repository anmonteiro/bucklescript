(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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




let () =
  let namespace = ref None in
  let root = ref None in
  let cwd = ref None in
  let dirs = ref [] in
  let argv = Sys.argv in
  let l = Array.length argv in
  let current = ref 1 in
  let rev_list = ref [] in
  while !current < l do
    let s = argv.(!current) in
    incr current;
    if s <> "" && s.[0] = '-' then begin
      match s with
      | "-root" ->
        let root_arg = argv.(!current) in
        root := Some root_arg;
        incr current
      | "-cwd" ->
        let cwd_arg = argv.(!current) in
        cwd := Some cwd_arg;
        incr current
      | "-I" ->
        let dir = argv.(!current) in
        dirs := dir :: !dirs;
        incr current
      | "-bs-ns" ->
        let ns = argv.(!current) in
        namespace := Some ns;
        incr current
      | s ->
        prerr_endline ("unknown options: " ^ s);
        prerr_endline ("available options: -bs-ns [ns]; -g; -cwd; -root");
        exit 2
    end else
      rev_list := s :: !rev_list
  done;
  match !cwd, !root with
  | Some cwd, Some root ->
    (
      match !rev_list with
      | [x]
        ->  Bsb_helper.Bsb_helper_depfile_gen.compute_dependency_info
              ~root
              ~cwd
              ~dirs:!dirs
              !namespace x ""
      | [y; x] (* reverse order *)
        ->
        Bsb_helper.Bsb_helper_depfile_gen.compute_dependency_info
          ~root
          ~cwd
          ~dirs:!dirs
          !namespace x y
      | _ ->
        prerr_endline "too many arguments to bsb_helper";
        exit 2
    )
  | _ ->
    prerr_endline "-cwd is a required option";
    exit 2

;;

