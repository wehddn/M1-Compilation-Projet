{ (* -*- tuareg -*- *)
  open Lexing
  open Error
  open Position
  open HopixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)


}

let newline       = ('\010' | '\013' | "\013\010")

let blank         = [' ' '\009' '\012']

let digit         = ['0'-'9']



(* id!!!
char -{'} et delete
 *)
let id            = ['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*  
let var_id        = ['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*
let constr_id     = ['A'-'Z']['A'-'Z''a'-'z''0'-'9''_']*
let label_id      = ['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*
let type_con      = ['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*
let type_variable = '\''['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*
let int           = '-'?['0'-'9']+|'0''x'['0'-'9''a'-'f''A'-'F']+|'0''b'['0'-'1']+|'0''o'['0'-'7']+
let atom          = ('\\'((['0''1']['0'-'9']['0'-'9']|'2'['0'-'4']['0'-'9']|'2''5'['0'-'5'])|['\\''\'''n''t''b''r']))|[' '-'~']
let char          = [''']atom[''']
let string        = '"'(('\\'((['0''1']['0'-'9']['0'-'9']|'2'['0'-'4']['0'-'9']|'2''5'['0'-'5'])|['\\''\'''n''t''b''r']))|([' '-'~']#['"'])*)'"'


rule token = parse
  (** Layout *)
  | newline               { next_line_and token lexbuf }
  | blank+                { token lexbuf               }
  | eof                   { EOF           }
  | "type"                { TYPE          }
  | "extern"              { EXTERN        }
  | "let"                 { LET           }
  | "fun"                 { FUN           }
  | "and"                 { AND           }
  | "match"               { MATCH         }
  | "if"                  { IF            }
  | "then"                { THEN          }
  | "else"                { ELSE          }
  | "ref"                 { REF           }
  | "while"               { WHILE         }
  | "do"                  { DO            }
  | "until"               { UNTIL         }
  | "for"                 { FOR           }
  | "from"                { FROM          }
  | "to"                  { TO            }
  | "->"                  { ARROW         }
  | id as id              { ID id         }
  | type_variable as tv   { TYPE_VARIABLE tv }
  | constr_id as ci       { CONSTR_ID ci  }
  | string as s           { STRING s      }
  | char as c             { CHAR c        }
  | '<'                   { LESS          }
  | '>'                   { GREATER       }
  | '='                   { EQUAL         }
  | ','                   { COMMA         }
  | ':'                   { COLON         }
  | '|'                   { BAR           }
  | '('                   { LPAREN        }
  | ')'                   { RPAREN        }
  | '{'                   { LCBRACKET     }
  | '}'                   { RCBRACKET     }
  | '['                   { LSBRACKET     }
  | ']'                   { RSBRACKET     }
  | '.'                   { DOT           }
  | '\'                   { BACKSLASH     }
  | '!'                   { EXCLAMATION   }
  | ';'                   { SEMICOLON     }

  

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

