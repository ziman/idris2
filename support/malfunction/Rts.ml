(* test code *)
external c_hello : int -> string = "c_hello"

let hello_world (_ : unit) : string =
    print_string "hello from ocaml, getting a secret string from C";
    print_newline ();
    let secret = c_hello 42 in
    print_string "returning from ocaml";
    print_newline ();
    secret

(* actual support code *)

let get_args (_ : unit) : string list = Array.to_list Sys.argv
