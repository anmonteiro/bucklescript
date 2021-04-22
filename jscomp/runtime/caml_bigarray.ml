(* Bigarray. - all bigarray types including Int64 and Complex. - fortran + c
   layouts - sub/slice/reshape - retain fast path for 1d array access *)

module Array = Caml_array_extern

let caml_invalid_argument err = raise (Invalid_argument err)

let caml_array_bound_error () = caml_invalid_argument "index out of bounds"

let caml_ba_custom_name = "_bigarr02"

[%%bs.raw
{|
  function Ml_Bigarray (kind, layout, dims, buffer) {
    this.kind   = kind ;
    this.layout = layout;
    this.dims   = dims;
    this.data = buffer;
  }

  Ml_Bigarray.prototype.caml_custom = caml_ba_custom_name;

  Ml_Bigarray.prototype.offset = function (arg) {
    var ofs = 0;
    if(typeof arg === "number") arg = [arg];
    if (! (arg instanceof Array)) caml_invalid_argument("genarray.js: invalid offset");
    if (this.dims.length != arg.length)
      caml_invalid_argument("Bigarray.get/set: bad number of dimensions");
    if(this.layout == 0 /* c_layout */) {
      for (var i = 0; i < this.dims.length; i++) {
        if (arg[i] < 0 || arg[i] >= this.dims[i])
          caml_array_bound_error();
        ofs = (ofs * this.dims[i]) + arg[i];
      }
    } else {
      for (var i = this.dims.length - 1; i >= 0; i--) {
        if (arg[i] < 1 || arg[i] > this.dims[i]){
          caml_array_bound_error();
        }
        ofs = (ofs * this.dims[i]) + (arg[i] - 1);
      }
    }
    return ofs;
  }

  Ml_Bigarray.prototype.get = function (ofs) {
    switch(this.kind){
    case 7:
      // Int64
      var l = this.data[ofs * 2 + 0];
      var h = this.data[ofs * 2 + 1];
      return [h, l];
    case 10: case 11:
      // Complex32, Complex64
      var r = this.data[ofs * 2 + 0];
      var i = this.data[ofs * 2 + 1];
      return {re: r, im: i};
    default:
      return this.data[ofs]
    }
  }

  Ml_Bigarray.prototype.set = function (ofs,v) {
    switch(this.kind){
    case 7:
      // Int64
      this.data[ofs * 2 + 0] = v[1];
      this.data[ofs * 2 + 1] = v[0];
      break;
    case 10: case 11:
      // Complex32, Complex64
      this.data[ofs * 2 + 0] = v.im;
      this.data[ofs * 2 + 1] = v.re;
      break;
    default:
      this.data[ofs] = v;
      break;
    }
    return 0
  }

  Ml_Bigarray.prototype.fill = function (v) {
    switch(this.kind){
    case 7:
      // Int64
      var a = v[1];
      var b = v[0];
      if(a == b){
        this.data.fill(a);
      }
      else {
        for(var i = 0; i<this.data.length; i++){
          this.data[i] = (i%2 == 0) ? a : b;
        }
      }
      break;
    case 10: case 11:
      // Complex32, Complex64
      var im = v.im;
      var re = v.re;
      if(im == re){
        this.data.fill(im);
      }
      else {
        for(var i = 0; i<this.data.length; i++){
          this.data[i] = (i%2 == 0) ? im : re;
        }
      }
      break;
    default:
      this.data.fill(v);
      break;
    }
  }

  Ml_Bigarray.prototype.compare = function (b, total) {
    if (this.layout != b.layout || this.kind != b.kind) {
      var k1 = this.kind | (this.layout << 8);
      var k2 =    b.kind | (b.layout << 8);
      return k2 - k1;
    }
    if (this.dims.length != b.dims.length) {
      return b.dims.length - this.dims.length;
    }
    for (var i = 0; i < this.dims.length; i++)
      if (this.dims[i] != b.dims[i])
        return (this.dims[i] < b.dims[i]) ? -1 : 1;
    switch (this.kind) {
    case 0:
    case 1:
    case 10:
    case 11:
      // Floats
      var x, y;
      for (var i = 0; i < this.data.length; i++) {
        x = this.data[i];
        y = b.data[i];
        if (x < y)
          return -1;
        if (x > y)
          return 1;
        if (x != y) {
          if (!total) return NaN;
          if (x == x) return 1;
          if (y == y) return -1;
        }
      }
      break;
    case 7:
      // Int64
      for (var i = 0; i < this.data.length; i+=2) {
        // Check highest bits first
        if (this.data[i+1] < b.data[i+1])
          return -1;
        if (this.data[i+1] > b.data[i+1])
          return 1;
        if ((this.data[i] >>> 0) < (b.data[i] >>> 0))
          return -1;
        if ((this.data[i] >>> 0) > (b.data[i] >>> 0))
          return 1;
      }
      break;
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 8:
    case 9:
    case 12:
      for (var i = 0; i < this.data.length; i++) {
        if (this.data[i] < b.data[i])
          return -1;
        if (this.data[i] > b.data[i])
          return 1;
      }
      break;
    }
    return 0;
  }
|}]

[%%bs.raw
{|
  function Ml_Bigarray_c_1_1(kind, layout, dims, buffer) {
    this.kind   = kind ;
    this.layout = layout;
    this.dims   = dims;
    this.data   = buffer;
  }

  Ml_Bigarray_c_1_1.prototype = new Ml_Bigarray()
  Ml_Bigarray_c_1_1.prototype.offset = function (arg) {
    if(typeof arg !== "number"){
      if((arg instanceof Array) && arg.length == 1)
        arg = arg[0];
      else caml_invalid_argument("Ml_Bigarray_c_1_1.offset");
    }
    if (arg < 0 || arg >= this.dims[0])
      caml_array_bound_error();
    return arg;
  }

  Ml_Bigarray_c_1_1.prototype.get = function (ofs) {
    return this.data[ofs];
  }

  Ml_Bigarray_c_1_1.prototype.set = function (ofs,v) {
    this.data[ofs] = v;
    return 0
  }

  Ml_Bigarray_c_1_1.prototype.fill = function (v) {
    this.data.fill(v);
    return 0
  }
|}]

type buffer

type ('a, 'b, 'c) genarray

external ml_Bigarray
  :  int
  -> int
  -> int array
  -> buffer
  -> ('a, 'b, 'c) genarray
  = "Ml_Bigarray"
  [@@bs.new]

external ml_Bigarray_c_1_1
  :  int
  -> int
  -> int array
  -> buffer
  -> ('a, 'b, 'c) genarray
  = "Ml_Bigarray_c_1_1"
  [@@bs.new]

external buffer : ('a, 'b, 'c) genarray -> buffer = "data" [@@bs.get]

external buffer_set : buffer -> buffer -> unit = "set" [@@bs.send]

external sub : buffer -> int -> int -> buffer = "subarray" [@@bs.send]

external buffer_length : buffer -> int = "length" [@@bs.get]

external dims : ('a, 'b, 'c) genarray -> int array = "dims" [@@bs.get]

external get_dim : ('a, 'b, 'c) genarray -> int -> int = ""
  [@@bs.scope "dims"] [@@bs.get_index]

external offset : ('a, 'b, 'c) genarray -> int array -> int = "offset"
  [@@bs.send]

external get : ('a, 'b, 'c) genarray -> int -> 'a = "get" [@@bs.send]

external set : ('a, 'b, 'c) genarray -> int -> 'a -> unit = "set" [@@bs.send]

external fill : ('a, 'b, 'c) genarray -> 'a -> unit = "fill" [@@bs.send]

let caml_ba_get_size (dims: int array) =
  let n_dims = Array.length dims in
  let size = { contents = 1 } in
  for i = 0 to n_dims - 1 do
    if Array.unsafe_get dims i < 0 then
      caml_invalid_argument "Bigarray.create: negative dimension";
    size.contents <- size.contents * Array.unsafe_get dims i
  done;
  size.contents

let caml_ba_get_size_per_element = function 7 | 10 | 11 -> 2 | _ -> 1

let caml_ba_create_buffer : int -> int -> buffer =
  [%raw
    {|
  function (kind, size){
    var view;
    switch(kind){
    case 0:  view = Float32Array; break;
    case 1:  view = Float64Array; break;
    case 2:  view = Int8Array; break;
    case 3:  view = Uint8Array; break;
    case 4:  view = Int16Array; break;
    case 5:  view = Uint16Array; break;
    case 6:  view = Int32Array; break;
    case 7:  view = Int32Array; break;
    case 8:  view = Int32Array; break;
    case 9:  view = Int32Array; break;
    case 10: view = Float32Array; break;
    case 11: view = Float64Array; break;
    case 12: view = Uint8Array; break;
    }
    if (!view) caml_invalid_argument("Bigarray.create: unsupported kind");
    var data = new view(size * caml_ba_get_size_per_element(kind));
    return data;
  }
|}]

let caml_ba_create_unsafe kind layout dims data =
  let size_per_element = caml_ba_get_size_per_element kind in
  if caml_ba_get_size dims * size_per_element != buffer_length data then
    caml_invalid_argument "length doesn't match dims";
  if layout = 0 && Array.length dims = 1 && size_per_element = 1 then
    ml_Bigarray_c_1_1 kind layout dims data
  else
    ml_Bigarray kind layout dims data

let caml_ba_create kind layout dims =
  let data = caml_ba_create_buffer kind (caml_ba_get_size dims) in
  caml_ba_create_unsafe kind layout dims data

external caml_ba_kind : ('a, 'b, 'c) genarray -> int = "kind" [@@bs.get]

external caml_ba_layout : ('a, 'b, 'c) genarray -> int = "layout" [@@bs.get]

let caml_ba_kind ba = caml_ba_kind ba

let caml_ba_layout ba = caml_ba_layout ba

let caml_ba_num_dims ba = Array.length (dims ba)

let caml_ba_change_layout ba layout =
  if caml_ba_layout ba == layout then
    ba
  else
    let new_dims = Array.new_uninitialized 0 in
    for i = 0 to caml_ba_num_dims ba - 1 do
      Array.unsafe_set new_dims i (get_dim ba (caml_ba_num_dims ba - i - 1))
    done;
    caml_ba_create_unsafe (caml_ba_kind ba) layout new_dims (buffer ba)

let caml_ba_dim ba i =
  if i < 0 || i >= caml_ba_num_dims ba then caml_invalid_argument "Bigarray.dim";
  get_dim ba i

let caml_ba_dim_1 ba = caml_ba_dim ba 0

let caml_ba_dim_2 ba = caml_ba_dim ba 1

let caml_ba_dim_3 ba = caml_ba_dim ba 2

let caml_ba_get_generic ba i =
  let ofs = offset ba i in
  get ba ofs

let caml_ba_get_1 ba i0 = get ba (offset ba [| i0 |])

let caml_ba_unsafe_get_1 = caml_ba_get_1

let caml_ba_get_2 ba i0 i1 = get ba (offset ba [| i0; i1 |])

let caml_ba_unsafe_get_2 = caml_ba_get_2

let caml_ba_get_3 ba i0 i1 i2 = get ba (offset ba [| i0; i1; i2 |])

let caml_ba_unsafe_get_3 = caml_ba_get_3

let caml_ba_set_generic ba i v =
  let ofs = offset ba i in
  set ba ofs v

let caml_ba_set_1 ba i0 v = set ba (offset ba [| i0 |]) v

let caml_ba_unsafe_set_1 = caml_ba_set_1

let caml_ba_set_2 ba i0 i1 v = set ba (offset ba [| i0; i1 |]) v

let caml_ba_unsafe_set_2 = caml_ba_set_2

let caml_ba_set_3 ba i0 i1 i2 v = set ba (offset ba [| i0; i1; i2 |]) v

let caml_ba_unsafe_set_3 = caml_ba_set_3

let caml_ba_fill ba v = fill ba v

let caml_ba_blit src dst =
  if caml_ba_num_dims dst != caml_ba_num_dims src then
    caml_invalid_argument "Bigarray.blit: dimension mismatch";
  for i = 0 to caml_ba_num_dims dst - 1 do
    if get_dim dst i != get_dim src i then
      caml_invalid_argument "Bigarray.blit: dimension mismatch"
  done;
  buffer_set (buffer dst) (buffer src)

let caml_ba_sub ba ofs len =
  let changed_dim = { contents = 0 } in
  let mul = { contents = 1 } in
  let ofs = { contents = ofs } in
  if caml_ba_layout ba = 0 then
    for i = 1 to caml_ba_num_dims ba - 1 do
      mul.contents <- mul.contents * get_dim ba i
    done
  else
    for i = 0 to caml_ba_num_dims ba - 2 do
      mul.contents <- mul.contents * get_dim ba i
    done;
  changed_dim.contents <- caml_ba_num_dims ba - 1;
  ofs.contents <- ofs.contents - 1;
  if
    ofs.contents < 0
    || len < 0
    || ofs.contents + len > get_dim ba changed_dim.contents
  then
    caml_invalid_argument "Bigarray.sub: bad sub-array";
  let new_dims = Array.new_uninitialized 0 in
  for i = 0 to caml_ba_num_dims ba - 1 do
    Array.unsafe_set new_dims i (get_dim ba i)
  done;
  Array.unsafe_set new_dims changed_dim.contents len;
  mul.contents <- mul.contents * caml_ba_get_size_per_element (caml_ba_kind ba);
  let new_data =
    sub
      (buffer ba)
      (ofs.contents * mul.contents)
      ((ofs.contents + len) * mul.contents)
  in
  caml_ba_create_unsafe (caml_ba_kind ba) (caml_ba_layout ba) new_dims new_data

let caml_ba_slice ba (vind : int array) =
  let num_inds = Array.length vind in
  let index = Array.new_uninitialized 0 in
  let sub_dims = { contents = Array.new_uninitialized 0 } in
  if num_inds > caml_ba_num_dims ba then
    caml_invalid_argument "Bigarray.slice: too many indices";
  if caml_ba_layout ba = 0 then (
    for i = 0 to num_inds - 1 do
      Array.unsafe_set index i (Array.unsafe_get vind i)
    done;
    for i = num_inds - 1 to caml_ba_num_dims ba - 1 do
      Array.unsafe_set index i 0
    done;
    sub_dims.contents <- [%raw {|ba.dims.slice(num_inds)|}])
  else (
    for i = 0 to num_inds - 1 do
      Array.unsafe_set
        index
        (caml_ba_num_dims ba - num_inds + i)
        (Array.unsafe_get vind i)
    done;
    for i = 0 to caml_ba_num_dims ba - num_inds - 1 do
      Array.unsafe_set index i 1
    done;
    sub_dims.contents <- [%raw {|ba.dims.slice(0, ba.dims.length - num_inds)|}]);
  let ofs = offset ba index in
  let size = caml_ba_get_size sub_dims.contents in
  let size_per_element = caml_ba_get_size_per_element (caml_ba_kind ba) in
  let new_data =
    sub (buffer ba) (ofs * size_per_element) ((ofs + size) * size_per_element)
  in
  caml_ba_create_unsafe
    (caml_ba_kind ba)
    (caml_ba_layout ba)
    sub_dims.contents
    new_data

let caml_ba_reshape ba (vind : int array) =
  let new_dim = Array.new_uninitialized 0 in
  let num_dims = Array.length vind in
  if num_dims < 0 || num_dims > 16 then
    caml_invalid_argument "Bigarray.reshape: bad number of dimensions";
  let num_elts = { contents = 1 } in
  for i = 0 to num_dims - 1 do
    Array.unsafe_set new_dim i (Array.unsafe_get vind i);
    if Array.unsafe_get new_dim i < 0 then
      caml_invalid_argument "Bigarray.reshape: negative dimension";
    num_elts.contents <- num_elts.contents * Array.unsafe_get new_dim i
  done;
  let size = caml_ba_get_size (dims ba) in
  if num_elts.contents != size then
    caml_invalid_argument "Bigarray.reshape: size mismatch";
  caml_ba_create_unsafe
    (caml_ba_kind ba)
    (caml_ba_layout ba)
    new_dim
    (buffer ba)
