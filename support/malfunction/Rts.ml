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

(* get_args works only because the low-level representation of Idris lists
 * is the same as the low-level representation of the OCaml lists
 *
 * https://ocaml.org/releases/4.09/htmlman/intfc.html#sec434
 *)
let get_args : string list = Array.to_list Sys.argv

module File = struct
    type file_ptr =
        | FileR of in_channel
        | FileW of out_channel

    let rec fopen (path : string) (mode : string) (_ : int) : file_ptr option =
        try
            Some(match mode with
            | "r" -> FileR (open_in path)
            | "w" -> FileW (open_out path)
            | "rb" -> FileR (open_in_bin path)
            | "wb" -> FileW (open_out_bin path)
            | _ -> failwith ("unknown file open mode: " ^ mode))
        with Sys_error msg -> None
end
