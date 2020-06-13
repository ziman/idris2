type utf8_read =
    | EOF
    | Character of char * int
    | Malformed

(* the order of functions must be exactly the same as in the MLF file! *)

val utf8_width : char -> int
val utf8_read : int -> bytes -> utf8_read
val utf8_write : char -> int -> bytes -> unit

(* use Bytes.create for allocation *)
val bytes_length : bytes -> int
