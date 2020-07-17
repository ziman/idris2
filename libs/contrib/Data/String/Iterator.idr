module Data.String.Iterator

export
record StringIterator where
  constructor MkSI
  string : String

  -- backend-dependent offset into the string
  -- on ML, this is in bytes
  -- in Scheme, this is in characters
  offset : Int

export
fromString : String -> StringIterator
fromString s = MkSI s 0

private
data ReadResult
  = EOF
  | Character Char Int  -- character, width

private
%foreign
  "scheme:read-string-char"
  "ML:Rts.String.readChar"
prim__readChar : Int -> String -> ReadResult

export
uncons : StringIterator -> Maybe (Char, StringIterator)
uncons (MkSI s ofs) =
  case prim__readChar ofs s of
    EOF => Nothing
    Character ch width => Just (ch, MkSI s (ofs + width))

export
foldl : (a -> Char -> a) -> a -> String -> a
foldl f acc s = loop 0 acc
  where
    loop : Int -> a -> a
    loop ofs acc =
      case prim__readChar ofs s of
        EOF => acc
        Character ch width => loop (ofs + width) (f acc ch)
