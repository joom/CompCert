{
open JsonParser
}

(* Identifiers *)
let digit = ['0'-'9']
let sign = ['-' '+']
let int = '-'? digit digit*
let exponent = ['e' 'E']
let float = '-'? digit+ '.'? digit+ (exponent sign? digit+)?
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Rules *)
rule read =
  parse
  | whitespace { read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true" { TRUE }
  | "false" { FALSE }
  | "null" { NULL }
  | '"' { STRING (read_string (Buffer.create 256) lexbuf) }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | ":" { COLON }
  | "," { COMMA }
  | _ { raise (Failure ("Unexpected char: '" ^ Lexing.lexeme lexbuf ^ "'")) }
  | eof { EOF }

and read_string buf =
  parse
  | '"'       { (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure ("String is not terminated")) }

