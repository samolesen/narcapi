type token =
  | IDENT of (string)
  | INTEGER of (int)
  | EQUAL
  | LPAREN
  | RPAREN
  | COMMA
  | SLASH
  | SEPARATOR
  | NEWLINE
  | ARROW
  | COLON
  | KNOW
  | PRIVATE
  | GENERATES
  | SHARE
  | DOT
  | EOF

val narration_dot :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.narration
