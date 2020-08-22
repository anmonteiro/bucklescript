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

let () =  Bsb_log.setup ()
let current_theme = ref "basic"
let generate_theme_with_path = ref None
let separator = "--"
let watch_mode = ref false
let make_world = ref false
let do_install = ref false
let bs_version_string = Bs_version.version
let print_version_string () =
  print_string bs_version_string;
  print_newline ();
  exit 0
type spec = Bsb_arg.spec

let call_spec f : spec = Unit (Unit_call f )
let  unit_set_spec b : spec = Unit (Unit_set b)


let force_regenerate = ref false
let bsb_main_flags : (string * spec * string) array =
  [|
    "-v", call_spec print_version_string,
    "Print version and exit";
    "-version", call_spec print_version_string,
    "Print version and exit";
    "-verbose", call_spec Bsb_log.verbose,
    "Set the output(from bsb) to be verbose";
    "-w", unit_set_spec watch_mode,
    "Watch mode" ;
    "-clean-world",call_spec (fun _ ->
        Bsb_clean.clean_bs_deps  Bsb_global_paths.cwd),
    "Clean all bs dependencies";
    "-clean", call_spec (fun _ ->
        Bsb_clean.clean_self  Bsb_global_paths.cwd),
    "Clean only current project";
    "-make-world", unit_set_spec make_world,
    "Build all dependencies and itself ";
    "-install", unit_set_spec do_install,
    "Install public interface files into lib/ocaml";
    "-init", String (String_call (fun path -> generate_theme_with_path := Some path)),
    "Init sample project to get started. \n\
    Note (`bsb -init sample` will create a sample project while \n\
    `bsb -init .` will reuse current directory)";
    "-theme", String (String_set current_theme),
    "The theme for project initialization. \n\
    default is basic:\n\
    https://github.com/rescript-lang/rescript-compiler/tree/master/jscomp/bsb/templates";

    "-regen", unit_set_spec force_regenerate,
    "*internal* \n\
    Always regenerate build.ninja no matter bsconfig.json is changed or not";
    "-themes", call_spec Bsb_theme_init.list_themes,
    "List all available themes";
    "-where",
    call_spec (fun _ ->
        print_endline (Filename.dirname Sys.executable_name)),
    "Show where bsb.exe is located";
(** Below flags are only for bsb script, it is not available for bsb.exe
  we make it at this time to make `bsb -help` easier
*)
    "-ws", call_spec ignore,
    "[host:]port \n\
    specify a websocket number (and optionally, a host). \n\
    When a build finishes, we send a message to that port. \n\
    For tools that listen on build completion." ;
#ifdef BS_NATIVE
    "-backend", String (String_call (fun s ->
        match s with
        | "js"       -> Bsb_global_backend.set_backend Bsb_config_types.Js
        | "native"   -> Bsb_global_backend.set_backend Bsb_config_types.Native
        | "bytecode" -> Bsb_global_backend.set_backend Bsb_config_types.Bytecode
        | _ -> failwith "-backend should be one of: 'js', 'bytecode' or 'native'."
      )),
    "Builds the entries specified in the bsconfig that match the given backend. Can be either 'js', 'bytecode' or 'native'.";
#endif
  |]


(*Note that [keepdepfile] only makes sense when combined with [deps] for optimization*)

(* Execute the underlying ninja build call, then exit (as opposed to keep watching) *)
let ninja_command_exit ?(dirs: Bsb_file_groups.file_groups option) ninja_args  =
  let mk_args targets =
    let ninja_common_args = Array.append [|Literals.dune; "build"|] targets in
    let ninja_args_len = Array.length ninja_args in
     let args =
       if ninja_args_len = 0 then ninja_common_args else
         Array.append ninja_common_args ninja_args
     in
     args
  in
  (* [dirs] = [None]: -make_world
   * [dirs] = [Some dirs]: just the current project (don't build node_modules)
   *)
  let depends_args, world_args = match dirs with
  | None ->
    [| ("@" ^ Literals.bsb_depends) |], [| ("@" ^ Literals.bsb_world) |]
  | Some dirs ->
    (* TODO(anmonteiro): this doesn't work if the directory starts with `@`.
     * See: https://github.com/ocaml/dune/issues/3716 *)
    let depends_args = (Ext_array.of_list_map dirs (fun {dir} ->
      Ext_path.(("@@" ^ dir) // Literals.bsb_depends)))
    in
    let world_args = (Ext_array.of_list_map dirs (fun {dir} ->
      Ext_path.(("@@" ^ dir) // Literals.bsb_world)))
    in
    depends_args, world_args
  in
  let depends_args, world_args = mk_args depends_args, mk_args world_args in
  let depends_command = {
      Bsb_unix.cmd = Literals.dune;
      cwd = Bsb_global_paths.cwd;
      args = depends_args
    }
  in
  let eid = match dirs with
    | None ->
      Bsb_log.info "@{<info>Running:@} %s@." (String.concat " " (Array.to_list depends_args));
      Bsb_unix.run_command_execvp depends_command
    | Some _ -> 0
  in
  if eid <> 0 then
    Bsb_unix.command_fatal_error depends_command eid
  else begin
   Bsb_log.info "@{<info>Running:@} %s@." (String.concat " " (Array.to_list world_args));
   Unix.execvp Literals.dune world_args
  end



(**
   Cache files generated:
   - .bsdircache in project root dir
   - .bsdeps in builddir

   What will happen, some flags are really not good
   ninja -C _build
*)
let usage = "Usage : bsb.exe <bsb-options> -- <ninja_options>\n\
             For ninja options, try bsb.exe --  -h.  \n\
             Note they are supposed to be internals and not reliable.\n\
             ninja will be loaded either by just running `bsb.exe' or `bsb.exe .. -- ..`\n\
             It is always recommended to run ninja via bsb.exe"

let handle_anonymous_arg ~rev_args =
  match rev_args with
  | [] -> ()
  | arg:: _ ->
    Bsc_args.bad_arg ("Unknown arg \"" ^ arg ^ "\"")


let program_exit () =
  exit 0

let install_target config_opt =
  let config =
    match config_opt with
    | None ->
      let config =
        Bsb_config_parse.interpret_json
          ~toplevel_package_specs:None
          ~per_proj_dir:Bsb_global_paths.cwd in
      let _ = Ext_list.iter config.file_groups.files (fun group ->
          let check_file = match group.public with
            | Export_all -> fun _ -> true
            | Export_none -> fun _ -> false
            | Export_set set ->
              fun module_name ->
                Set_string.mem set module_name in
          Map_string.iter group.sources
            (fun  module_name module_info ->
               if check_file module_name then
                 begin Hash_set_string.add config.files_to_install module_info.name_sans_extension end
            )) in
      config
    | Some (config, _digest) -> config in
  Bsb_world.install_targets Bsb_global_paths.cwd config

(* see discussion #929, if we catch the exception, we don't have stacktrace... *)
let () =
  let argv = Sys.argv in
  let deps_digest = ref None in
  try begin
    match argv with
    | [| _ |]
    | [| _; "-verbose" |] ->  (* specialize this path [bsb.exe] which is used in watcher *)
      Bsb_log.verbose ();
      let config = Bsb_ninja_regen.regenerate_ninja
        ?deps_digest:!deps_digest
        ~toplevel_package_specs:None
        ~forced:false
        ~per_proj_dir:Bsb_global_paths.cwd
      in
      Ext_option.iter config (fun (config, _digest) ->
        ninja_command_exit ~dirs:config.file_groups.files [||])
    | argv ->
      begin
        let i =  Ext_array.rfind_with_index argv Ext_string.equal separator in
        if i < 0 then
          begin
            Bsb_arg.parse_exn ~usage ~argv bsb_main_flags handle_anonymous_arg;
            (* first, check whether we're in boilerplate generation mode, aka -init foo -theme bar *)
            match !generate_theme_with_path with
            | Some path -> Bsb_theme_init.init_sample_project ~cwd:Bsb_global_paths.cwd ~theme:!current_theme  path
            | None ->
              (* [-make-world] should never be combined with [-package-specs] *)
              let make_world = !make_world in
              let force_regenerate = !force_regenerate in
              let do_install = !do_install in
              if not make_world && not force_regenerate && not do_install then
                (* [regenerate_ninja] is not triggered in this case
                   There are several cases we wish ninja will not be triggered.
                   [bsb -clean-world]
                   [bsb -regen ]
                *)
                (if !watch_mode then
                    program_exit ()) (* bsb -verbose hit here *)
              else
                (if make_world then begin
                   let world_digest =
                     Bsb_world.make_world_deps Bsb_global_paths.cwd None
                   in
                   deps_digest := Some world_digest;
                 end;
                 let config_opt =
                   Bsb_ninja_regen.regenerate_ninja
                     ?deps_digest:!deps_digest
                     ~toplevel_package_specs:None
                     ~forced:force_regenerate ~per_proj_dir:Bsb_global_paths.cwd
                 in
                 if !watch_mode then begin
                   program_exit ()
                   (* ninja is not triggered in this case
                      There are several cases we wish ninja will not be triggered.
                      [bsb -clean-world]
                      [bsb -regen ]
                   *)
                 end else if make_world then begin
                   ninja_command_exit [||]
                 end else if do_install then begin
                   install_target config_opt
                 end)
          end
        else
           (* -make-world all dependencies fall into this category *)
          begin
            Bsb_arg.parse_exn
            ~usage
            ~argv:argv
            ~finish:i
            bsb_main_flags handle_anonymous_arg  ;
            let ninja_args = Array.sub argv (i + 1) (Array.length argv - i - 1) in
            (* [-make-world] should never be combined with [-package-specs] *)
            if !make_world then begin
             let world_digest =
               Bsb_world.make_world_deps Bsb_global_paths.cwd None
             in
             deps_digest := Some world_digest
            end;
            let config_opt =
              (Bsb_ninja_regen.regenerate_ninja
                ?deps_digest:!deps_digest
                ~toplevel_package_specs:None
                ~per_proj_dir:Bsb_global_paths.cwd
                ~forced:!force_regenerate) in
            if !do_install then
              install_target config_opt;
            if !watch_mode then program_exit ()
            else ninja_command_exit  ninja_args
          end
      end
  end
  with
  | Bsb_exception.Error e ->
    Bsb_exception.print Format.err_formatter e ;
    Format.pp_print_newline Format.err_formatter ();
    exit 2
  | Ext_json_parse.Error (start,_,e) ->
    Format.fprintf Format.err_formatter
      "File %S, line %d\n\
       @{<error>Error:@} %a@."
      start.pos_fname start.pos_lnum
      Ext_json_parse.report_error e ;
    exit 2
  | Bsb_arg.Bad s
  | Sys_error s ->
    Format.fprintf Format.err_formatter
      "@{<error>Error:@} %s@."
      s ;
    exit 2
  | e -> Ext_pervasives.reraise e
