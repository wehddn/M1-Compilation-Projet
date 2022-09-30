%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position

%}

%token EOF TYPE LESS GREATER EQUAL COMMA EXTERN 
COLON BAR LPAREN RPAREN LET FUN AND MATCH IF THEN ELSE REF WHILE DO UNTIL FOR FROM TO LCBRACKET RCBRACKET 
LSBRACKET RSBRACKET DOT BACKSLASH EXCLAMATION SEMICOLON ARROW

%token <string> ID TYPE_VARIABLE CONSTR_ID STRING CHAR
%token <Mint.t> INT

%start<HopixAST.t> program

%%

program:
  | p=located(definition)* EOF                 {p}

definition:
  | TYPE tc=located(tcon) df=definition_variables dt=definition_tdefinition  {DefineType(tc,df,dt)}
  | EXTERN vi=located(varid) COLON ts=located(type_scheme)   {DeclareExtern(vi,ts)}
  | v=vdefinition                                                             {DefineValue(v)}

vdefinition:
  | LET vi=located(varid) EQUAL e=located(expr)                                 {SimpleValue(vi,None,e)}
  | LET vi=located(varid) COLON ts=located(type_scheme)? EQUAL e=located(expr)  {SimpleValue(vi,ts,e)}
  | FUN f=separated_nonempty_list(AND, fundef)                                  {RecFunctions(f)}

fundef:
  | COLON ts=located(type_scheme)? vi=located(varid) p=located(pattern) EQUAL e=located(expr)   {(vi,ts,FunctionDefinition(p,e))}
  | vi=located(varid) p=located(pattern) EQUAL e=located(expr)                                  {(vi,None,FunctionDefinition(p,e))}

pattern: vi=located(varid)  {PVariable(vi)}

varid: i=ID {Id(i)}

tcon: i=ID {TCon(i)}

definition_variables:
  |                                                       {[]}
  | LESS tvl=separated_list(COMMA, located(tid)) GREATER  {tvl}

tid: i=TYPE_VARIABLE      {TId(i)}

definition_tdefinition:
  |                       {Abstract}
  | EQUAL t=tdefinition   {t}

tdefinition: 
  | ci=located(cid) ct=tdefinition_types                              {DefineSumType([(ci,ct)])}
  | ctl=tdefinition_constr*                                           {DefineSumType(ctl)}
  | LCBRACKET tll=separated_list(COMMA, tdefinition_label) RCBRACKET  {DefineRecordType(tll)}

cid: i=CONSTR_ID {KId(i)}

tdefinition_constr: 
  | BAR ci=located(cid) ct=tdefinition_types       {(ci,ct)}

tdefinition_types:
  |                             {[]}
  | LPAREN tl=separated_nonempty_list(COMMA,located(ty)) RPAREN   {tl}


tdefinition_label: id=located(id) COLON t=located(ty)           {(id, t)}

id: i=ID {LId(i)}

ty: t=tid {TyVar(t)}

expr:
  | l=located(literal)  {Literal(l)}

literal:
  | i=INT {LInt i}

type_scheme:
  | ty=located(ty)                                                {ForallTy([],ty)}
  | LSBRACKET tsv=located(tid)+ RSBRACKET ty=located(ty)  {ForallTy(tsv,ty)}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}