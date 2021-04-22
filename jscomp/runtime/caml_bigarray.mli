type buffer

type ('a, 'b, 'c) genarray

val caml_array_bound_error : unit -> 'a

val caml_invalid_argument : string -> 'a

val caml_ba_custom_name : string

val caml_ba_get_size : int array -> int

val caml_ba_get_size_per_element : int -> int

val caml_ba_create_buffer : int -> int -> buffer

val caml_ba_create_unsafe
  :  int
  -> int
  -> int array
  -> buffer
  -> ('a, 'b, 'c) genarray

val caml_ba_create : int -> int -> int array -> ('a, 'b, 'c) genarray

val caml_ba_kind : ('a, 'b, 'c) genarray -> int

val caml_ba_layout : ('a, 'b, 'c) genarray -> int

val caml_ba_num_dims : ('a, 'b, 'c) genarray -> int

val caml_ba_change_layout
  :  ('a, 'b, 'c) genarray
  -> int
  -> ('a, 'b, 'c) genarray

val caml_ba_dim : ('a, 'b, 'c) genarray -> int -> int

val caml_ba_dim_1 : ('a, 'b, 'c) genarray -> int

val caml_ba_dim_2 : ('a, 'b, 'c) genarray -> int

val caml_ba_dim_3 : ('a, 'b, 'c) genarray -> int

val caml_ba_get_generic : ('a, 'b, 'c) genarray -> int array -> 'a

val caml_ba_get_1 : ('a, 'b, 'c) genarray -> int -> 'a

val caml_ba_unsafe_get_1 : ('a, 'b, 'c) genarray -> int -> 'a

val caml_ba_get_2 : ('a, 'b, 'c) genarray -> int -> int -> 'a

val caml_ba_unsafe_get_2 : ('a, 'b, 'c) genarray -> int -> int -> 'a

val caml_ba_get_3 : ('a, 'b, 'c) genarray -> int -> int -> int -> 'a

val caml_ba_unsafe_get_3 : ('a, 'b, 'c) genarray -> int -> int -> int -> 'a

val caml_ba_set_generic : ('a, 'b, 'c) genarray -> int array -> 'a -> unit

val caml_ba_set_1 : ('a, 'b, 'c) genarray -> int -> 'a -> unit

val caml_ba_unsafe_set_1 : ('a, 'b, 'c) genarray -> int -> 'a -> unit

val caml_ba_set_2 : ('a, 'b, 'c) genarray -> int -> int -> 'a -> unit

val caml_ba_unsafe_set_2 : ('a, 'b, 'c) genarray -> int -> int -> 'a -> unit

val caml_ba_set_3 : ('a, 'b, 'c) genarray -> int -> int -> int -> 'a -> unit

val caml_ba_unsafe_set_3
  :  ('a, 'b, 'c) genarray
  -> int
  -> int
  -> int
  -> 'a
  -> unit

val caml_ba_fill : ('a, 'b, 'c) genarray -> 'a -> unit

val caml_ba_blit : ('a, 'b, 'c) genarray -> ('d, 'e, 'f) genarray -> unit

val caml_ba_sub : ('a, 'b, 'c) genarray -> int -> int -> ('d, 'e, 'f) genarray

val caml_ba_slice : ('a, 'b, 'c) genarray -> int array -> ('d, 'e, 'f) genarray

val caml_ba_reshape
  :  ('a, 'b, 'c) genarray
  -> int array
  -> ('d, 'e, 'f) genarray
