type utf8 =
    | Unallocated
    | Allocated of int * int * Bytes.t

type utf8_uncons =
    | Nil
    | Cons of char * utf8
    | Malformed

(* the order must be exactly the same as in the MLF file! *)
val utf8_cons : char -> utf8 -> utf8
val utf8_uncons : utf8 -> utf8_uncons
