


type spec =
  | Set of bool ref            
  | String of (string -> unit) 
  | Set_string of string ref   

type key = string
type doc = string

type anon_fun = rev_args:string list -> unit

val parse_exn :
  progname:string -> 
  argv:string array -> 
  start:int ->
  (key * spec * doc) list -> 
  anon_fun  -> unit



