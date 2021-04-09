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


(* TODO: sync up with {!Js_packages_info.module_system}  *)
type format = Ext_module_system.t =
  | NodeJS | Es6 | Es6_global

type spec = {
  format : format;
  in_source : bool;
  suffix : Ext_js_suffix.t
}

(*FIXME: use assoc list instead *)
module Spec_set = Set.Make( struct type t = spec
    let compare = Stdlib.compare
  end)

type t = {
  modules : Spec_set.t;
  runtime: string option;
  (* This has to be resolved as early as possible, since
    the path will be inherited in sub projects
  *)
}

let (.?()) = Map_string.find_opt

let bad_module_format_message_exn ~loc format =
  Bsb_exception.errorf ~loc "package-specs: `%s` isn't a valid output module format. It has to be one of:  %s, %s or %s"
    format
    Literals.commonjs
    Literals.es6
    Literals.es6_global

let supported_format (x : string) loc =
  if x = Literals.commonjs then NodeJS
  else if x = Literals.es6 then Es6
  else if x = Literals.es6_global then Es6_global
  else bad_module_format_message_exn ~loc x

let string_of_format (x : format) =
  match x with
  | NodeJS -> Literals.commonjs
  | Es6 -> Literals.es6
  | Es6_global -> Literals.es6_global


let rec from_array suffix (arr : Ext_json_types.t array) : Spec_set.t =
  let spec = ref Spec_set.empty in
  let has_in_source = ref 0 in
  let distinct_suffixes = ref Set_string.empty in
  Ext_array.iter arr (fun x ->
      let result = from_json_single suffix x  in
      distinct_suffixes := Set_string.add !distinct_suffixes (Ext_js_suffix.to_string result.suffix);
      if result.in_source then begin
        incr has_in_source;
        let distinct_suffixes = Set_string.cardinal !distinct_suffixes in
        if !has_in_source > distinct_suffixes then
          Bsb_exception.errorf
            ~loc:(Ext_json.loc_of x)
            "package-specs: %d module formats are configured to output targets \
             `in-source`, but only %d distinct suffix(es) were found." !has_in_source distinct_suffixes
      end;
      spec := Spec_set.add result !spec
    );
  !spec

(* TODO: FIXME: better API without mutating *)
and from_json_single suffix (x : Ext_json_types.t) : spec =
  match x with
  | Str {str = format; loc } ->
      {format = supported_format format loc  ; in_source = false ; suffix }
  | Obj {map; loc} ->
    begin match map .?("module") with
      | Some(Str {str = format}) ->
        let in_source =
          match map.?(Bsb_build_schemas.in_source) with
          | Some (True _) -> true
          | Some _
          | None -> false
        in
        let suffix =
          match map.?("suffix") with
          | Some (Str {str = suffix; loc}) ->
            let s = Ext_js_suffix.of_string suffix in
            if s = Unknown_extension then
              Bsb_exception.errorf ~loc "expect .js,.bs.js,.mjs or .cjs"
            else  s
          | Some _ ->
            Bsb_exception.errorf ~loc:(Ext_json.loc_of x) "expect a string field"
          | None -> suffix in
        {format = supported_format format loc ; in_source ; suffix}
      | Some _ ->
        Bsb_exception.errorf ~loc
          "package-specs: when the configuration is an object, `module` field should be a string, not an array. If you want to pass multiple module specs, try turning package-specs into an array of objects (or strings) instead."
      | None ->
        Bsb_exception.errorf ~loc
          "package-specs: when the configuration is an object, the `module` field is mandatory."
    end
  | _ -> Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
           "package-specs: we expect either a string or an object."

let  from_json suffix (x : Ext_json_types.t) : Spec_set.t =
  match x with
  | Arr {content ; _} -> from_array suffix content
  | _ -> Spec_set.singleton (from_json_single suffix x )

let bs_package_output = "-bs-package-output"

(** Assume input is valid
    coordinate with command line flag
    {[ -bs-package-output commonjs:lib/js/jscomp/test:.js ]}
*)
let package_flag ({ format; suffix } : spec) dir =
  Ext_string.inter2
    bs_package_output
    (Ext_string.concat5
       (string_of_format format)
       Ext_string.single_colon dir
      Ext_string.single_colon
      (Ext_js_suffix.to_string suffix)
    )

(* FIXME: we should adapt it *)
let package_flag_of_package_specs (package_specs : t)
    ~(dirname : string ) : string  =
  let res = Spec_set.fold (fun format acc ->
      Ext_string.inter2 acc (package_flag format dirname )
    ) package_specs.modules Ext_string.empty in
  match package_specs.runtime with
  | None -> res
  | Some x ->
    res ^ " -runtime " ^ x
let default_package_specs suffix =
  Spec_set.singleton
    { format = NodeJS ; in_source = false; suffix  }



(**
    [get_list_of_output_js specs "src/hi/hello"]

*)
let get_list_of_output_js
    (package_specs : t)
    (output_file_sans_extension : string)
    =
  Spec_set.fold
    (fun (spec : spec) acc ->
        let basename =
          Ext_namespace.change_ext_ns_suffix
             output_file_sans_extension
             (Ext_js_suffix.to_string spec.suffix)
        in
        (Bsb_config.rev_lib_bs_prefix basename, spec.in_source)
       :: acc
    ) package_specs.modules []


type json_map = Ext_json_types.t Map_string.t

let extract_bs_suffix_exn (map : json_map) : Ext_js_suffix.t =
  match map.?(Bsb_build_schemas.suffix) with
  | None -> Js
  | Some (Str {str; loc}) ->
    let s =  Ext_js_suffix.of_string str  in
    if s = Unknown_extension then
      Bsb_exception.errorf ~loc
        "expect .bs.js, .js, .cjs, .mjs here"
    else s
  | Some config ->
    Bsb_exception.config_error config
      "expect a string exteion like \".js\" here"

let from_map ~(cwd:string) map =
  let suffix = extract_bs_suffix_exn map in
  let modules = match map.?(Bsb_build_schemas.package_specs) with
  | Some x ->
    from_json suffix x
  | None ->  default_package_specs suffix in
  let runtime =
    match map.?(Bsb_build_schemas.external_stdlib) with
    | None -> None
    | Some(Str{str; _}) ->
       Some (Bsb_pkg.resolve_bs_package ~cwd (Bsb_pkg_types.string_as_package str))
    | _ -> assert false in

  let distinct_suffixes =
    Spec_set.fold
      (fun spec acc -> Set_string.add acc (Ext_js_suffix.to_string spec.suffix))
      modules Set_string.empty
  in

  let distinct_suffixes = Set_string.cardinal distinct_suffixes in
  let distinct_modules = Spec_set.cardinal modules in
  if distinct_suffixes < distinct_modules then begin
    Bsb_log.warn
      "@{<warning>Warning:@} @[Found %d package specs, but only %d distinct suffix(es).@;\
      @[Melange can only output multiple package specs if they have distinct suffixes.@]@]@."
      distinct_modules
      distinct_suffixes
    ;
  end;

  {
    runtime;
    modules
  }

