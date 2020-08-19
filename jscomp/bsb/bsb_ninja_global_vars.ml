(* Copyright (C) 2017 Authors of BuckleScript
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

type t =
 {
  g_pkg_flg : string;
  src_root_dir: string;
  bsc: string;
  bsdep: string;
  bs_dep_parse: string;
  warnings: string;
  bsc_flags: string;
  ppx_flags: string;
  g_dpkg_incls: string;
  g_dev_incls: string list;
  g_stdlib_incl: string list;
  g_lib_incls: string;
  g_sourcedirs_incls: string list;
  g_ns : string ;
  gentypeconfig: string option;
  pp_flags: string option;
  namespace: string option
 }

let make
  ~g_pkg_flg
  ~src_root_dir
  ~bsc
  ~bsdep
  ~bs_dep_parse
  ~warnings
  ~bsc_flags
  ~ppx_flags
  ~g_dpkg_incls
  ~g_ns
  ~g_dev_incls
  ~g_stdlib_incl
  ~g_lib_incls
  ~g_sourcedirs_incls
  ~gentypeconfig
  ~pp_flags
  ~namespace =
 {
  g_pkg_flg;
  src_root_dir;
  bsc;
  bsdep;
  bs_dep_parse;
  warnings;
  bsc_flags;
  ppx_flags;
  g_dpkg_incls;
  g_dev_incls;
  g_stdlib_incl;
  g_lib_incls;
  g_sourcedirs_incls;
  g_ns;
  gentypeconfig;
  pp_flags;
  namespace;
 }

let g_pkg_flg = "g_pkg_flg"

let bsc = "bsc"

let src_root_dir = "src_root_dir"
let bsdep = "bsdep"

let bsc_flags = "bsc_flags"

let ppx_flags = "ppx_flags"

let pp_flags = "pp_flags"


let g_dpkg_incls = "g_dpkg_incls"

let refmt = "refmt"

let refmt_flags = "refmt_flags"

let postbuild = "postbuild"

let g_ns = "g_ns"

let warnings = "warnings"

let gentypeconfig = "gentypeconfig"

let g_dev_incls = "g_dev_incls"

