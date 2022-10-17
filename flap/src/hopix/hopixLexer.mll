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



let id            = ['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*  
let constr_id     = ['A'-'Z']['A'-'Z''a'-'z''0'-'9''_']*
let type_variable = '`'['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*
let int           = '-'?['0'-'9']+|'0''x'['0'-'9''a'-'f''A'-'F']+|'0''b'['0'-'1']+|'0''o'['0'-'7']+
let atom          = ('\\'((['0''1']['0'-'9']['0'-'9']|'2'['0'-'4']['0'-'9']|'2''5'['0'-'5'])
                      |['\\''\'''n''t''b''r']
                      |(['0']['x']?['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])))
                      |([' '-'~']#['''])
let char          = [''']atom[''']
let string        = '"'(('\\'((['0''1']['0'-'9']['0'-'9']|'2'['0'-'4']['0'-'9']|'2''5'['0'-'5'])
                      |['\\''\'''n''t''b''r']))
                      |([' '-'~']#['"']|(['\\']['\"']))*)'"'
let bad_string    = '"'(('\\'((['0''1']['0'-'9']['0'-'9']|'2'['0'-'4']['0'-'9']|'2''5'['0'-'5'])
                      |['\\''\'''n''t''b''r']))
                      |([' '-'~']#['"']|(['\\']['\"']))*)
(* Récupérer de Hobix *)
let infix_alien_identifier = "`" (['A'-'Z''a'-'z'] | [ '+' '-' '*' '/' '<' '=' '>' '?' '&' ] | digit)+ "`"

let binop = "+" | "-" | "*" | "/" | "&&" | "||" | "=?" | "<=?" | ">=?" | "<?" | ">?"

rule token = parse
  (** Layout *)
  | newline               { next_line_and token lexbuf }
  | blank+                { token lexbuf               }
  | "{*"                  { comment 1 lexbuf           }
  | "**"                  { commentline lexbuf         }
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
  | ":="                  { ASSIGN        }
  | id as id              { ID id         }
  | type_variable as tv   { TYPE_VARIABLE tv }
  | constr_id as ci       { CONSTR_ID ci  }
  | int as i              { INT (Mint.of_string i) }
  | infix_alien_identifier as i { INFIXID i}
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
  | '\\'                  { BACKSLASH     }
  | '!'                   { EXCLAMATION   }
  | ';'                   { SEMICOLON     }
  | '*'                   { STAR          }
  | '+'                   { PLUS          }
  | '-'                   { MINUS         }
  | '/'                   { SLASH         }
  | "&&"                  { LAND          }
  | "||"                  { LOR           }
  | "=?"                  { EQ            }
  | "<=?"                 { LTE           }
  | ">=?"                 { GTE           }
  | "<?"                  { LT            }
  | ">?"                  { GT            }
  | '_'                   { UNDERSCORE    }
  | '&'                   { AMPERSAND     }
  | bad_string            { error lexbuf "Unterminated string." }
  | string as s           
  { let explode s =
      let rec exp i l =
       if i < 0 then l else exp (i - 1) (s.[i] :: l) in
      exp (String.length s - 1) [] in

    let rec do_all lst =
      match lst with
      | [] -> []
      | '\\' :: x :: xs -> let x_res = match x with 
                            | '\\' -> '\\'
                            | '\'' -> '\''
                            | 'n' -> '\n'
                            | 't' -> '\t'
                            | 'b' -> '\b'
                            | 'r' -> '\r'
                            | '0' -> 's'
                            | _ -> ' '
                            in x_res :: (do_all xs)
      | x :: xs -> x :: (do_all xs) in

    let res = String.sub s 1 ((String.length s)-2) in
    let char_list = explode res in
    let char_res = do_all char_list in
    let str_res = String.concat "" (List.map (String.make 1) char_res) in

    STRING str_res      
  }

  | char as c             
  { let res = match c.[1] with
    | '\\' -> match c with
      | "'\\\\'" -> '\\'
      | "'\\''"  -> '\''
      | "'\\n'"  -> '\n'
      | "'\\t'"  -> '\t'
      | "'\\b'"  -> '\b'
      | "'\\r'"  -> '\r'
      | code -> let code_str = String.sub code 2 ((String.length code)-3) in 
                let code_int = int_of_string code_str in
                char_of_int code_int
    | ch -> let code_int = int_of_string ch in
                char_of_int code_int
    in 
    CHAR res      
  }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }


and comment level = parse
  | "*}" {
    if level = 1 then
      token lexbuf
    else
      comment (pred level) lexbuf
  }
  | "{*" {
    comment (succ level) lexbuf
  }
  | eof {
    error lexbuf "unterminated comment."
  }
  | newline {
    next_line_and (comment level) lexbuf
  }
  | _ {
    comment level lexbuf
  }

and commentline = parse
  | newline { next_line_and token lexbuf }
  | eof { EOF }
  | _ { commentline lexbuf }
