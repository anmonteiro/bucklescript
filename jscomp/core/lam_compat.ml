(* Copyright (C) 2018 Hongbo Zhang, Authors of ReScript
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



type boxed_integer = Lambda.boxed_integer =
    Pnativeint | Pint32 | Pint64


let eq_boxed_integer (p : boxed_integer) (p1 : boxed_integer ) =
  match p with
  | Pnativeint -> p1 = Pnativeint
  | Pint32 -> p1 = Pint32
  | Pint64 -> p1 = Pint64

type integer_comparison = Lambda.integer_comparison =
    Ceq | Cne | Clt | Cgt | Cle | Cge

type float_comparison = Lambda.float_comparison =
    CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

let eq_comparison ( p : integer_comparison) (p1:integer_comparison) =
  match p with
  | Cge -> p1 =  Cge
  | Cgt -> p1 =  Cgt
  | Cle -> p1 =  Cle
  | Clt -> p1 =  Clt
  | Ceq -> p1 =  Ceq
  | Cne -> p1 =  Cne

let eq_float_comparison ( p : float_comparison) (p1:float_comparison) =
  match p with
  | CFeq -> p1 = CFeq
  | CFneq -> p1 = CFneq
  | CFlt -> p1 = CFlt
  | CFnlt -> p1 = CFnlt
  | CFgt -> p1 = CFgt
  | CFngt -> p1 = CFngt
  | CFle -> p1 = CFle
  | CFnle -> p1 = CFnle
  | CFge -> p1 = CFge
  | CFnge -> p1 = CFnge

let cmp_int32 (cmp : integer_comparison) (a : int32) b : bool =
  match cmp with
  | Ceq -> a = b
  | Cne -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b

let cmp_int64 (cmp : integer_comparison) (a : int64) b : bool =
  match cmp with
  | Ceq -> a = b
  | Cne -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b

let cmp_nativeint (cmp : integer_comparison) (a : nativeint) b : bool =
  match cmp with
  | Ceq -> a = b
  | Cne -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b

let cmp_float (cmp : float_comparison) (a : float) b : bool =
  match cmp with
  | CFeq ->  a = b
  | CFneq -> a <> b
  | CFlt  -> a < b
  | CFnlt -> not (a < b)
  | CFgt -> a > b
  | CFngt -> not (a > b)
  | CFle -> a <= b
  | CFnle -> not (a <= b)
  | CFge -> a >= b
  | CFnge -> not (a >= b)


let cmp_int (cmp : integer_comparison) (a : int) b : bool =
  match cmp with
  | Ceq -> a = b
  | Cne -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b


type compile_time_constant =
    | Big_endian
    | Ostype_unix
    | Ostype_win32
    | Ostype
    | Backend_type

(** relies on the fact that [compile_time_constant] is enum type *)
let eq_compile_time_constant ( p : compile_time_constant) (p1 : compile_time_constant) =
  p = p1

type let_kind = Lambda.let_kind
= Strict
| Alias
| StrictOpt
| Variable


type meth_kind = Lambda.meth_kind
  = Self
| Public of string option
| Cached

type field_dbg_info = Lambda.field_dbg_info =
  | Fld_na of string
  | Fld_record of { name : string; mutable_flag : Asttypes.mutable_flag}
  | Fld_module of { name : string }
  | Fld_record_inline of { name : string}
  | Fld_record_extension of {name : string }
  | Fld_tuple
  | Fld_poly_var_tag
  | Fld_poly_var_content
  | Fld_extension
  | Fld_variant
  | Fld_cons
  | Fld_array

let str_of_field_info (x : field_dbg_info) : string option =
  match x with
  | Fld_na  s -> if s = "" then None else Some s
  | Fld_array
  | Fld_extension
  | Fld_variant
  | Fld_cons
  | Fld_poly_var_tag
  | Fld_poly_var_content
  | Fld_tuple
     -> None
  | Fld_record {name ; _}
  | Fld_module {name ; _}
  | Fld_record_inline {name}
  | Fld_record_extension {name}
     ->
      Some name

type set_field_dbg_info = Lambda.set_field_dbg_info =
  | Fld_set_na
  | Fld_record_set of string
  | Fld_record_inline_set of string
  | Fld_record_extension_set of string


type bigarray_kind = Lambda.bigarray_kind = 
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64


(* let eq_bigarray_kind (p : bigarray_kind) (p1 : bigarray_kind) = 
  match p with   
  | Pbigarray_unknown -> p1 = Pbigarray_unknown
  | Pbigarray_float32 -> p1 = Pbigarray_float32
  | Pbigarray_float64 -> p1 =  Pbigarray_float64
  | Pbigarray_sint8 -> p1 = Pbigarray_sint8
  | Pbigarray_uint8 -> p1 = Pbigarray_uint8
  | Pbigarray_sint16 -> p1 = Pbigarray_sint16 
  | Pbigarray_uint16 -> p1 = Pbigarray_uint16
  | Pbigarray_int32  -> p1 = Pbigarray_int32
  | Pbigarray_int64 -> p1 = Pbigarray_int64
  | Pbigarray_caml_int -> p1 = Pbigarray_caml_int
  | Pbigarray_native_int -> p1 = Pbigarray_native_int
  | Pbigarray_complex32  -> p1 = Pbigarray_complex32
  | Pbigarray_complex64 -> p1 = Pbigarray_complex64 *)
  
  
type bigarray_layout = Lambda.bigarray_layout = 
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout
