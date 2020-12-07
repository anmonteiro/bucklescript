(* Copyright (C) 2017- Authors of BuckleScript
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


let (//) = Ext_path.combine

let install_targets cwd ({ namespace; pinned_dependencies} as config : Bsb_config_types.t ) =
  let deps = config.package_specs in
  let lib_artifacts_dir = Bsb_config.lib_bs in
  let destdir = cwd // Bsb_config.lib_ocaml in (* lib is already there after building, so just mkdir [lib/ocaml] *)
  if not @@ Sys.file_exists destdir then begin Unix.mkdir destdir 0o777  end;
  begin
    Bsb_log.info "@{<info>Installing started@}@.";
    let file_groups = ref [] in
    let queue =
      Bsb_build_util.walk_all_deps  cwd ~pinned_dependencies in
    queue |> Queue.iter (fun ({ top; proj_dir} : Bsb_build_util.package_context) ->
      match top with
      | Expect_none -> ()
      | Expect_name s ->
        let is_pinned =  Set_string.mem pinned_dependencies s in
        let dep_config =
          Bsb_config_parse.interpret_json
          ~package_kind:(if is_pinned then Pinned_dependency deps else Dependency deps)
          ~per_proj_dir:proj_dir in
        file_groups := (proj_dir, dep_config.file_groups) :: !file_groups
    );
    let lib_bs_dir = cwd // lib_artifacts_dir in
    Bsb_build_util.mkp lib_bs_dir;
    Bsb_watcher_gen.generate_sourcedirs_meta
      ~name:(lib_bs_dir // Literals.sourcedirs_meta)
      !file_groups;
    Bsb_log.info "@{<info>Installing finished@} @.";
  end


let build_bs_deps cwd ~pinned_dependencies (deps : Bsb_package_specs.t) =
  let dep_dirs = ref [] in
  let digest_buf = Ext_buffer.create 1024 in
  let queue =
    Bsb_build_util.walk_all_deps  cwd ~pinned_dependencies in
  queue |> Queue.iter (fun ({top; proj_dir} : Bsb_build_util.package_context) ->
      match top with
      | Expect_none -> ()
      | Expect_name s ->
        let is_pinned =  Set_string.mem pinned_dependencies s in
        (if is_pinned then
          print_endline ("Dependency pinned on " ^ s )
        else print_endline ("Dependency on " ^ s ));
        dep_dirs := proj_dir :: !dep_dirs;
        match
          Bsb_ninja_regen.regenerate_ninja
            ~package_kind:(if is_pinned then Pinned_dependency deps else Dependency deps)
            ?deps_digest:None
            ~forced:false
            ~per_proj_dir:proj_dir with (* set true to force regenrate ninja file so we have [config_opt]*)
        | Some (_config, digest) ->
          Ext_buffer.add_string digest_buf digest
        | None -> ()
  );

  if !dep_dirs <> [] && Sys.file_exists (cwd // Literals.node_modules) then begin
   let dirs = Ext_list.map !dep_dirs (fun dir ->
     let rel_dir =
       Ext_path.rel_normalized_absolute_path
         ~from:(cwd // Literals.node_modules)
         dir
     in
     let rel_dir = Ext_path.split_by_sep_per_os rel_dir in
     match rel_dir with
     | "." :: x :: _ -> x
     | _ -> List.hd rel_dir)
   in
   let buf = Buffer.create 1024 in
   Buffer.add_string buf "\n(dirs";
   Ext_list.iter dirs (fun dir ->
     Buffer.add_string buf Ext_string.single_space;
     Buffer.add_string buf dir);
   Buffer.add_string buf ")\n";
   Bsb_ninja_targets.revise_dune (cwd // Literals.node_modules // Literals.dune) buf
  end;
  Digest.to_hex (Ext_buffer.digest digest_buf)





let make_world_deps cwd (config : Bsb_config_types.t option) =
  Bsb_log.info "Making the dependency world!@.";
  let deps, pinned_dependencies =
    match config with
    | None ->
      (* When this running bsb does not read bsconfig.json,
         we will read such json file to know which [package-specs]
         it wants
      *)
      Bsb_config_parse.package_specs_from_bsconfig ()
    | Some config -> config.package_specs, config.pinned_dependencies
  in
  build_bs_deps cwd deps ~pinned_dependencies
