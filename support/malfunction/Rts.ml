module Types = struct
    (* this is made to match the Idris constructor tags *)
    type 'a idris_list =
        | Nil                         (* int 0 *)
        | UNUSED of int               (* block, tag 0 *)
        | Cons of 'a * 'a idris_list  (* block, tag 1 *)

    let rec to_idris_list = function
        | [] -> Nil
        | x :: xs -> Cons (x, to_idris_list xs)

end
open Types

module IORef = struct
    let write (r : 'a ref) (x : 'a) : unit = r := x
end

module System = struct
    let get_args : string idris_list =
            to_idris_list (Array.to_list Sys.argv)
end

module Debug = struct
    (* %foreign "ML:Rts.Debug.inspect"
     * prim__inspect : (x : a) -> (1 w : %World) -> IORes ()
     *
     * inspect : a -> IO ()
     * inspect x = primIO (prim__inspect x)
     *)
    external inspect : 'ty -> 'a -> unit = "inspect"
end

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

(* some test code *)
module Demo = struct
    external c_hello : int -> string = "c_hello"

    let hello_world (_ : unit) : string =
        print_string "hello from ocaml, getting a secret string from C";
        print_newline ();
        let secret = c_hello 42 in
        print_string "returning from ocaml";
        print_newline ();
        secret
end
