(* Copyright (C) 2019 - Authors of BuckleScript
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


(* Target backend *)
val backend : Bsb_config_types.compilation_kind_t ref

(* path to all intermediate build artifacts, would be lib/bs when compiling to JS *)
val lib_artifacts_dir : string ref

(* path to the compiled artifacts, would be lib/ocaml when compiling to JS *)
val lib_ocaml_dir : string ref

val dune_build_dir : string ref

(* string representation of the target backend, would be "js" when compiling to js *)
val backend_string: string ref


#ifdef BS_NATIVE
(* Flag to track whether backend has been set to a value. *)
val backend_is_set : bool ref

(* convenience setter to update all the refs according to the given target backend *)
val set_backend : Bsb_config_types.compilation_kind_t -> unit
#endif
