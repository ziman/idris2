type utf8_read =
    | EOF
    | Character of char * int
    | Malformed

(* the order of functions must be exactly the same as in the MLF file! *)

val utf8_width : char -> int
val utf8_read : int -> bytes -> utf8_read
val utf8_write : char -> int -> bytes -> unit

val bytes_allocate : int -> bytes
val bytes_length : bytes -> int

(* src, ofs_src, dst, ofs_dst, nbytes *)
val bytes_blit : bytes -> int -> bytes -> int -> int -> unit
