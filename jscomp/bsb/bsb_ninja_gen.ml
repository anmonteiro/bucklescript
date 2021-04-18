(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript
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

(* we need copy package.json into [_build] since it does affect build output
   it is a bad idea to copy package.json which requires to copy js files
*)


(* let dash_i = "-I" *)

let dependencies_directories deps =
  Ext_list.flat_map deps (fun { Bsb_config_types.package_dirs; _ } -> package_dirs)


let get_bsc_flags
    (bsc_flags : string list)
  : string =
  String.concat Ext_string.single_space bsc_flags

let bsc_lib_includes (bs_dependencies : Bsb_config_types.dependencies) =
  (Ext_list.flat_map bs_dependencies (fun x -> x.package_install_dirs))

let generate_ppxlib_source ~subdir ~ppx_config buf =
  Buffer.add_string buf "(subdir ";
  Buffer.add_string buf (subdir // Literals.melange_eobjs_dir);
  Bsb_ppxlib.ppxlib buf ~ppx_config;
  Buffer.add_string buf ")\n"

let output_ninja_and_namespace_map
    ~buf
    ~per_proj_dir
    ~root_dir
    ~package_kind
    ({
      package_name;
      external_includes;
      bsc_flags ;
      pp_file;
      ppx_config ;

      bs_dependencies;
      bs_dev_dependencies;
      js_post_build_cmd;
      package_specs;
      file_groups = { files = bs_file_groups};
      files_to_install;
      built_in_dependency;
      reason_react_jsx;
      generators ;
      namespace ;
      warning;
      gentype_config;

    } : Bsb_config_types.t) : unit
  =
  let lib_artifacts_dir = Bsb_config.lib_bs in
  let warnings = Bsb_warning.to_bsb_string ~package_kind warning in
  let bsc_flags = (get_bsc_flags bsc_flags) in
  let bs_groups : Bsb_db.t = {lib = Map_string.empty; dev = Map_string.empty} in
  let source_dirs : string list Bsb_db.cat = {lib = []; dev = []} in
  Ext_list.iter bs_file_groups (fun
      {sources; dir; is_dev} ->
    if is_dev then begin
      bs_groups.dev <- Bsb_db_util.merge bs_groups.dev sources ;
      source_dirs.dev <- dir :: source_dirs.dev;
    end else begin
      bs_groups.lib <- Bsb_db_util.merge bs_groups.lib sources ;
      source_dirs.lib <- dir :: source_dirs.lib
    end);
  let global_config =
    Bsb_ninja_global_vars.make
      ~package_name
      ~db:bs_groups
      ~per_proj_dir
      ~bsc:(Ext_filename.maybe_quote Bsb_global_paths.vendor_bsc)
      ~bsdep:(Ext_filename.maybe_quote Bsb_global_paths.vendor_bsdep)
      ~bs_dep_parse:(Ext_filename.maybe_quote Bsb_global_paths.bs_dep_parse)
      ~warnings
      ~bsc_flags
      ~g_dpkg_incls:(bsc_lib_includes bs_dev_dependencies)
      ~g_dev_incls:source_dirs.dev
      ~g_sourcedirs_incls:source_dirs.lib
      ~g_lib_incls:(bsc_lib_includes bs_dependencies)
      ~external_incls:external_includes
      ~gentypeconfig:(Ext_option.map gentype_config (fun x ->
          ("-bs-gentype " ^ x.path)))
      ~ppx_config
      ~pp_flags:(Ext_option.map pp_file Bsb_build_util.pp_flag)
      ~namespace
  in
  let lib = bs_groups.lib in
  let dev = bs_groups.dev in
  Bsb_db_util.sanity_check lib;
  Bsb_db_util.sanity_check dev;
  Map_string.iter dev
    (fun k a ->
       if Map_string.mem lib k  then
         raise (Bsb_db_util.conflict_module_info k a (Map_string.find_exn lib k))
    ) ;
  let rules : Bsb_ninja_rule.builtin =
      Bsb_ninja_rule.make_custom_rules
      ~global_config
      ~has_postbuild:js_post_build_cmd
      ~pp_file
      ~has_builtin:built_in_dependency
      ~reason_react_jsx
      ~package_specs
      generators in
  let bs_dependencies =
    dependencies_directories bs_dependencies
  in
  let bs_dev_dependencies =
    dependencies_directories bs_dev_dependencies
  in
  Buffer.add_char buf '\n';

  (* output_static_resources static_resources rules.copy_resources oc ; *)
  (** Generate build statement for each file *)
  Ext_list.iter bs_file_groups
    (fun files_per_dir ->
       Bsb_ninja_file_groups.handle_files_per_dir buf
         ~global_config
         ~rules
         ~package_specs
         ~files_to_install
         ~js_post_build_cmd
         ~bs_dev_dependencies
         ~bs_dependencies
         ~root_dir
         files_per_dir)
  ;

  Buffer.add_char buf '\n';
  Ext_option.iter namespace (fun ns ->
      let namespace_dir =
        Ext_path.rel_normalized_absolute_path ~from:root_dir (per_proj_dir // lib_artifacts_dir)
      in
      Bsb_namespace_map_gen.output
        ~dir:namespace_dir ns
        bs_file_groups;
      Buffer.add_string buf "(subdir ";
      Buffer.add_string buf namespace_dir;
      Bsb_ninja_targets.output_build namespace_dir buf
        ~outputs:[ns ^ Literals.suffix_cmi]
        ~inputs:[ns ^ Literals.suffix_mlmap]
        ~rule:rules.build_package;
      Buffer.add_string buf ")";
    );
  Buffer.add_char buf '\n';

  match package_kind with
  | Bsb_package_kind.Toplevel ->
    generate_ppxlib_source ~subdir:"" ~ppx_config buf;
    Buffer.add_string buf "\n(data_only_dirs node_modules ";
    Buffer.add_string buf Literals.melange_eobjs_dir;
    (* for the edge case of empty sources (either in user config or because a
       source dir is empty), we emit an empty `bsb_world` alias. This avoids
       showing the user an error when they haven't done anything. *)
    Buffer.add_string buf ")\n(alias (name bsb_world))\n";
  | Dependency _ ->
    let subd =
      Ext_path.rel_normalized_absolute_path ~from:root_dir per_proj_dir
    in
    Buffer.add_string buf "(subdir ";
    Buffer.add_string buf subd;
    Buffer.add_string buf "(data_only_dirs ";
    Buffer.add_string buf Literals.melange_eobjs_dir;
    Buffer.add_string buf "))\n";

    generate_ppxlib_source ~subdir:subd ~ppx_config buf;

