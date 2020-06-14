(* These types are made to match the Idris representation *)
module Types = struct
    type world = World

    module IdrisList = struct
        type 'a idris_list =
            | Nil                         (* int 0 *)
            | UNUSED of int               (* block, tag 0 *)
            | Cons of 'a * 'a idris_list  (* block, tag 1 *)

        let rec of_list = function
            | [] -> Nil
            | x :: xs -> Cons (x, of_list xs)

        let rec to_list = function
            | Nil -> []
            | UNUSED _ -> failwith "UNUSED tag in idris list"
            | Cons (x, xs) -> x :: to_list xs

        let rec foldl f z = function
            | Nil -> z
            | UNUSED _ -> failwith "UNUSED tag in idris list"
            | Cons (x, xs) -> foldl f (f z x) xs
    end
end
open Types
open Types.IdrisList

module Debug = struct
    (* %foreign "ML:Rts.Debug.inspect"
     * prim__inspect : (x : a) -> (1 w : %World) -> IORes ()
     *
     * inspect : a -> IO ()
     * inspect x = primIO (prim__inspect x)
     *)
    external inspect : 'ty -> 'a -> unit = "inspect"
end

module IORef = struct
    let write (r : 'a ref) (x : 'a) : unit = r := x
end

module System = struct
    let get_args : string idris_list =
        IdrisList.of_list (Array.to_list Sys.argv)

    let fork_thread (sub : world -> unit) : Thread.t =
        Thread.create sub World

    let os_name : string =
        match Sys.os_type with
        | "Unix" -> "unix"
        | "Win32" -> "windows"
        | "Cygwin" -> "windows"
        | _ -> "unknown"
end

module String = struct
    let reverse (src : bytes) : bytes =
        let len = Bytes.length src in
        let dst = Bytes.create len in
        let rec go (ofs_src : int) (ofs_dst : int) =
            match ofs_dst with
            | 0 -> dst
            | _ -> (match LowLevel.utf8_read ofs_src src with
                | LowLevel.EOF -> failwith "impossible: desynchronised"
                | LowLevel.Character (c, w) ->
                    LowLevel.utf8_write c (ofs_dst - w) dst;
                    go (ofs_src + w) (ofs_dst - w)
                | LowLevel.Malformed -> failwith "malformed string"
                )
          in go 0 len

    (* get the byte offset after skipping N chars from the starting byte offset *)
    (* this function stops at the end of the string without throwing an error there *)
    let rec get_end_ofs (ofs : int) (nchars : int) (s : bytes) : int =
        match nchars with
        | 0 -> ofs
        | _ -> match LowLevel.utf8_read ofs s with
            | LowLevel.EOF -> ofs (* failwith "string too short" *)
            | LowLevel.Character (_, w) -> get_end_ofs (ofs + w) (nchars - 1) s
            | LowLevel.Malformed -> failwith "malformed string"

    let cons (c : char) (s : bytes) : bytes =
        let w = LowLevel.utf8_width c in
        let l = Bytes.length s in
        let s' = Bytes.create (w + Bytes.length s) in
        LowLevel.utf8_write c 0 s';
        Bytes.blit s 0 s' w l;
        s'

    let length (s : bytes) : int =
        let rec go (acc : int) (ofs : int) =
            match LowLevel.utf8_read ofs s with
            | LowLevel.EOF -> acc
            | LowLevel.Character (_, w) -> go (acc + 1) (ofs + w)
            | LowLevel.Malformed -> failwith "malformed string"
          in go 0 0

    let sub (ofs_chars : int) (nchars : int) (s : bytes) : bytes =
        let ofs = get_end_ofs 0 ofs_chars s in
        let len = get_end_ofs ofs nchars s - ofs in
        let result = Bytes.create len in
        Bytes.blit s ofs result 0 len;
        result

    let head (s : bytes) : char =
        match LowLevel.utf8_read 0 s with
        | LowLevel.EOF -> failwith "String.head: empty string"
        | LowLevel.Character (c, _) -> c
        | LowLevel.Malformed -> failwith "malformed string"

    let tail (s : bytes) : bytes =
        match LowLevel.utf8_read 0 s with
        | LowLevel.EOF -> failwith "String.tail: empty string"
        | LowLevel.Character (_, w) ->
            let nbytes = Bytes.length s - w in
            let s' = Bytes.create nbytes in
            Bytes.blit s w s' 0 nbytes;
            s'
        | LowLevel.Malformed -> failwith "malformed string"

    let get (s : bytes) (i : int) : char =
        let rec go (j : int) (ofs : int) =
            match LowLevel.utf8_read ofs s with
            | LowLevel.EOF -> failwith "string too short"
            | LowLevel.Character (c, w) ->
                (match j with
                | 0 -> c
                | _ -> go (j - 1) (ofs + w))
            | LowLevel.Malformed -> failwith "malformed string"
          in go i 0

    let unpack (s : bytes) : char idris_list =
        let rec decode (acc : char list) (ofs : int) =
            match LowLevel.utf8_read ofs s with
            | LowLevel.EOF ->
              let rec rev (acc : char idris_list) = function
                  | [] -> acc
                  | x :: xs -> rev (Cons (x, acc)) xs
                in rev Nil acc
            | LowLevel.Character (c, w) ->
                decode (c :: acc) (ofs + w)
            | LowLevel.Malformed -> failwith "malformed string"
          in decode [] 0

    let pack (cs : char idris_list) : bytes =
        let total_length = IdrisList.foldl (fun l c -> l + LowLevel.utf8_width c) 0 cs in
        let result = Bytes.create total_length in
        let rec fill (ofs : int) = function
            | IdrisList.Nil -> result
            | IdrisList.UNUSED _ -> failwith "UNUSED in idris list"
            | IdrisList.Cons (c, xs) ->
                LowLevel.utf8_write c ofs result;
                fill (ofs + LowLevel.utf8_width c) xs
          in fill 0 cs
end

module Bytes = struct
    (* pre-allocate a big buffer once and copy all strings in it *)
    let concat (ssi : bytes idris_list) : bytes =
        let ss = IdrisList.to_list ssi in
        let total_length = List.fold_left (fun l s -> l + Bytes.length s) 0 ss in
        let result = Bytes.create total_length in
        let rec write_strings (ofs : int) = function
            | IdrisList.Nil -> ()
            | IdrisList.UNUSED _ -> failwith "UNUSED"
            | IdrisList.Cons (src, rest) ->
                let len = Bytes.length src in
                Bytes.blit src 0 result ofs len;
                write_strings (ofs+len) rest
          in
        write_strings 0 ssi;
        result

    let append (x : bytes) (y : bytes) : bytes =
        let xlen = Bytes.length x in
        let ylen = Bytes.length y in
        let result = Bytes.create (xlen + ylen) in
        Bytes.blit x 0 result 0 xlen;
        Bytes.blit y 0 result xlen ylen;
        result
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
