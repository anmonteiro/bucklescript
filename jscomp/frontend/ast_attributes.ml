open! Ext
open! Common

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

type attr =  Parsetree.attribute
type t =  attr list

type ('a,'b) st =
  { get : 'a option ;
    set : 'b option }


let process_method_attributes_rev (attrs : t) =
  Ext_list.fold_left attrs ({get = None ; set = None}, [])
    (fun (st,acc) ({ attr_name = {txt ; loc}; attr_payload = payload} as attr ) ->
      match txt  with
      | "bs.get" | "get" (* @bs.get{null; undefined}*)
        ->
        let result =
          Ext_list.fold_left (Ast_payload.ident_or_record_as_config loc payload) (false, false)
            (fun (null, undefined) ({txt ; loc}, opt_expr) ->
              match txt with
              | "null" ->
                (match opt_expr with
                 | None -> true
                 | Some e ->
                   Ast_payload.assert_bool_lit e), undefined

              |  "undefined" ->
                null,
                (match opt_expr with
                 | None ->  true
                 | Some e ->
                   Ast_payload.assert_bool_lit e)
              | "nullable" ->
                begin match opt_expr with
                  | None -> true, true
                  | Some e ->
                    let v = Ast_payload.assert_bool_lit e in
                    v,v
                end
              | _ -> Bs_syntaxerr.err loc Unsupported_predicates
            ) in

        ({st with get = Some result}, acc  )

      | "bs.set" | "set"
        ->
        let result =
          Ext_list.fold_left (Ast_payload.ident_or_record_as_config loc payload) `Get
            (fun _st ({txt ; loc}, opt_expr)  -> (*FIXME*)
               if txt =  "no_get" then
                 match opt_expr with
                 | None -> `No_get
                 | Some e ->
                   if Ast_payload.assert_bool_lit e then
                     `No_get
                   else `Get
               else Bs_syntaxerr.err loc Unsupported_predicates
            )  in
        (* properties -- void
              [@@set{only}]
        *)
        {st with set = Some result }, acc
      | _ ->
        (st, attr::acc  )
    )

type attr_kind =
  | Nothing
  | Meth_callback of attr
  | Uncurry of attr
  | Method of attr

let process_attributes_rev (attrs : t) : attr_kind * t =
  Ext_list.fold_left attrs ( Nothing, []) (fun (st, acc) ({ attr_name = {txt; loc}; _ } as attr) ->
      match txt, st  with
      | "bs", (Nothing | Uncurry _)
        ->
        Uncurry attr, acc (* TODO: warn unused/duplicated attribute *)
      | ("bs.this" | "this"), (Nothing | Meth_callback _)
        ->  Meth_callback attr, acc
      | ("bs.meth" | "meth"),  (Nothing | Method _)
        -> Method attr, acc
      | ("bs" | "bs.this" | "this"), _
        -> Bs_syntaxerr.err loc Conflict_bs_bs_this_bs_meth
      | _ , _ ->
        st, attr::acc
    )

let process_pexp_fun_attributes_rev (attrs : t) =
  Ext_list.fold_left attrs (false, []) (fun (st, acc) ({ attr_name = {txt; loc=_}; _} as attr ) ->
      match txt  with
      | "bs.open"
        ->
        true, acc
      | _  ->
        st, attr::acc
    )


let process_bs (attrs : t) =
  Ext_list.fold_left attrs (false, []) (fun (st, acc) ({ attr_name = {txt; loc=_}; _} as attr ) ->
      match txt, st  with
      | "bs", _
        ->
        true, acc
      | _ , _ ->
        st, attr::acc
    )

let external_attrs = [|
  "get";
  "set";
  "get_index";
  "return";
  "obj";
  "val";
  "module";
  "scope";
  "variadic";
  "send";
  "new";
  "set_index";
  Literals.gentype_import
|]
(* ATT: Special cases for built-in attributes handling *)
let external_needs_to_be_encoded (attrs : t)=
  Ext_list.exists attrs
    (fun { attr_name = {txt; _}; _} ->
       Ext_string.starts_with txt "bs." ||
       Ext_array.exists external_attrs (fun (x : string) -> txt = x) )

let is_inline : attr -> bool =
  (fun
    ({ attr_name = {txt;};_}) ->
     txt = "bs.inline" || txt = "inline"
  )


let has_inline_payload (attrs : t)  =
  Ext_list.find_first attrs is_inline


type derive_attr = {
  bs_deriving : Ast_payload.action list option
} [@@unboxed]

let process_derive_type (attrs : t) : derive_attr * t =
  Ext_list.fold_left attrs ({bs_deriving = None }, [])
    (fun (st, acc) ({ attr_name = {txt ; loc}; attr_payload = payload}  as attr)  ->
       match   txt  with
       |  "bs.deriving" | "deriving"
         ->
         begin match st.bs_deriving with
           |  None ->
             {
               bs_deriving = Some
                   (Ast_payload.ident_or_record_as_config loc payload)}, acc
           | Some _
             ->
             Bs_syntaxerr.err loc Duplicated_bs_deriving
         end
       | _  ->
         st, attr::acc
    )



(* duplicated @uncurry @string not allowed,
  it is worse in @uncurry since it will introduce
  inconsistency in arity
 *)
let iter_process_bs_string_int_unwrap_uncurry (attrs : t) =
  let st = ref `Nothing in
  let assign v ({ attr_name = {loc;_}; _ } as attr : attr) =
    if !st = `Nothing then
    begin
      Bs_ast_invariant.mark_used_bs_attribute attr;
      st := v ;
    end
    else Bs_syntaxerr.err loc Conflict_attributes  in
  Ext_list.iter attrs (fun ({ attr_name = {txt ; loc=_}; attr_payload = payload } as attr)  ->
      match  txt with
      | "bs.string" | "string"
        -> assign `String attr
      | "bs.int" | "int"
        -> assign `Int attr
      | "bs.ignore" | "ignore"
        -> assign `Ignore attr
      | "bs.unwrap" | "unwrap"
        -> assign `Unwrap attr
      | "bs.uncurry" | "uncurry"
        ->
        assign (`Uncurry (Ast_payload.is_single_int payload)) attr
      | _ -> ()
    ) ;
  !st


let iter_process_bs_string_as  (attrs : t) : string option =
  let st = ref None in
  Ext_list.iter attrs
    (fun
      ({ attr_name = {txt ; loc}; attr_payload = payload } as attr )  ->
      match  txt with
      | "bs.as" | "as"
        ->
        if !st = None then
          match Ast_payload.is_single_string payload with
          | None ->
            Bs_syntaxerr.err loc Expect_string_literal
          | Some  (v,_dec) ->
            Bs_ast_invariant.mark_used_bs_attribute attr ;
            st:= Some v
        else
          Bs_syntaxerr.err loc Duplicated_bs_as
      | _  -> ()
    ) ;
  !st


let has_bs_optional  (attrs : t) : bool =
  Ext_list.exists attrs (fun
      ({ attr_name = {txt ; }; _ } as attr)  ->
      match  txt with
      | "bs.optional" | "optional"
        ->
        Bs_ast_invariant.mark_used_bs_attribute attr ;
        true
      | _  -> false
    )



let iter_process_bs_int_as  (attrs : t) =
  let st = ref None in
  Ext_list.iter attrs
    (fun
      ({ attr_name = {txt ; loc}; attr_payload = payload } as attr)  ->
      match  txt with
      | "bs.as" | "as"
        ->
        if !st =  None then
          match Ast_payload.is_single_int payload with
          | None ->
            Bs_syntaxerr.err loc Expect_int_literal
          | Some  _ as v->
            Bs_ast_invariant.mark_used_bs_attribute attr ;
            st := v
        else
          Bs_syntaxerr.err loc Duplicated_bs_as
      | _  -> ()
    ) ; !st

type as_const_payload =
  | Int of int
  | Str of string
  | Js_literal_str of string
let iter_process_bs_string_or_int_as (attrs : Parsetree.attributes) =
  let st = ref None in
  Ext_list.iter attrs
    (fun
      ({ attr_name = {txt ; loc}; attr_payload = payload } as attr)  ->
      match  txt with
      | "bs.as" | "as"
        ->
        if !st = None then
          (Bs_ast_invariant.mark_used_bs_attribute attr ;
           match Ast_payload.is_single_int payload with
           | None ->
             begin match  payload with
               | PStr [ {
                   pstr_desc =
                     Pstr_eval (
                       {pexp_desc =
                          Pexp_constant
                            (Pconst_string(s, _, (None | Some "json" as dec)))
                       ; pexp_loc ;
                         _},_);
                   _}] ->
                 if dec = None then
                   st := Some (Str (s))
                 else
                 begin
                   (match Classify_function.classify ~check:(pexp_loc, Bs_flow_ast_utils.flow_deli_offset dec)
                            s with
                   | Js_literal _ -> ()
                   | _ ->
                    Location.raise_errorf ~loc:pexp_loc "an object literal expected");
                   st := Some (Js_literal_str s )
                 end
               | _ ->
                 Bs_syntaxerr.err loc Expect_int_or_string_or_json_literal
             end
           | Some v->
             st := (Some (Int v))
          )
        else
          Bs_syntaxerr.err loc Duplicated_bs_as
      | _ -> ()

    ) ;
  !st

let locg = Location.none
(* let bs : attr
  =  {txt = "bs" ; loc = locg}, Ast_payload.empty *)

let is_bs (attr : attr) =
  match attr with
  | { attr_name = {Location.txt = "bs"; _}; _} -> true
  | _ -> false





let bs_get : attr
=  { attr_name = {txt = "bs.get"; loc = locg}; attr_payload = Ast_payload.empty; attr_loc = locg }

let bs_get_index : attr =
  { attr_name = {txt = "bs.get_index"; loc = locg}; attr_payload = Ast_payload.empty; attr_loc = locg}

let bs_get_arity : attr =
  { attr_name = {txt = "internal.arity"; loc = locg};
    attr_payload = PStr [{pstr_desc =
      Pstr_eval (
        Ast_compatible.const_exp_int ~loc:locg 1
           ,
           [])
      ; pstr_loc = locg}];
    attr_loc = locg }


let bs_set : attr
  =  { attr_name = {txt = "bs.set"; loc = locg}; attr_payload = Ast_payload.empty; attr_loc = locg }

let bs_return_undefined : attr =
  { attr_name = {txt = "bs.return"; loc = locg };
    attr_payload = PStr [
        {pstr_desc =
           Pstr_eval (
             {pexp_desc =
                Pexp_ident
                  { txt = Lident "undefined_to_opt";
                    loc = locg};
              pexp_loc = locg;
              pexp_loc_stack = [];
              pexp_attributes = []
             },[])
         ; pstr_loc = locg}];
    attr_loc = locg
  }

