(* some test functions *)
external c_hello : int -> string = "c_hello"
val hello_world : unit -> string

(* actual support code *)
val get_args : unit -> string list
