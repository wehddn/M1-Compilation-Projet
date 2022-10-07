%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position

%}

%token EOF TYPE LESS GREATER EQUAL COMMA EXTERN 
COLON BAR LPAREN RPAREN LET FUN AND MATCH IF THEN ELSE REF WHILE DO UNTIL FOR FROM TO LCBRACKET RCBRACKET 
LSBRACKET RSBRACKET DOT BACKSLASH EXCLAMATION SEMICOLON ARROW STAR UNDERSCORE

%token <string> ID TYPE_VARIABLE CONSTR_ID STRING CHAR
%token <Mint.t> INT

%left ARROW

%start<HopixAST.t> program

%%

program:
  | p=located(definition)* EOF                 {p}

definition:
  | TYPE tc=located(tcon) df=definition_variables dt=definition_tdefinition  {DefineType(tc,df,dt)}
  | EXTERN vi=located(varid) COLON ts=located(type_scheme)   {DeclareExtern(vi,ts)}
  | v=vdefinition                                                             {DefineValue(v)}

vdefinition:
  | LET vi=located(varid) ts=preceded(COLON, located(type_scheme))? EQUAL e=located(expr)  {SimpleValue(vi,ts,e)}
  | FUN f=separated_nonempty_list(AND, fundef)                                  {RecFunctions(f)}

fundef: ts=preceded(COLON, located(type_scheme))? vi=located(varid) p=located(pattern) EQUAL e=located(expr)   {(vi,ts,FunctionDefinition(p,e))}

varid: i=ID {Id(i)}

tcon: i=ID {TCon(i)}

definition_variables:
  | tvl=loption(delimited(LESS, separated_list(COMMA, located(tid)), GREATER))  {tvl}

tid: i=TYPE_VARIABLE      {TId(i)}

definition_tdefinition:
  |                       {Abstract}
  | EQUAL t=tdefinition   {t}

tdefinition: 
  | ci=located(cid) ct=tdefinition_types                              {DefineSumType([(ci,ct)])}
  | ctl=tdefinition_constr*                                           {DefineSumType(ctl)}
  | tll=delimited(LCBRACKET, separated_list(COMMA, tdefinition_label), RCBRACKET)  {DefineRecordType(tll)}

cid: i=CONSTR_ID {KId(i)}

tdefinition_constr: 
  | BAR ci=located(cid) ct=tdefinition_types       {(ci,ct)}

tdefinition_types:
  | tl=loption(delimited(LPAREN, separated_nonempty_list(COMMA,located(ty)), RPAREN))   {tl}


tdefinition_label: id=located(id) COLON t=located(ty)           {(id, t)}

id: i=ID {LId(i)}

ty:
  (*| tc=tcon ty=loption(delimited(LESS, separated_nonempty_list(COMMA, located(ty)), GREATER)) {TyCon(tc, ty)}*)
  | t1=located(ty) ARROW t2=located(ty) {TyArrow(t1,t2)}
  (*| t=separated_nonempty_list(STAR, located(ty)) {TyTuple(t)}*) 
  | t=tid {TyVar(t)}
  | t=delimited(LPAREN, ty, RPAREN) {t}

expr:
  | l=located(literal)  {Literal(l)}
  | vid=located(varid) tyl=tyList {Variable(vid,tyl)}
  | cid=located(cid) tyl=tyList expl=loption(delimited(LPAREN, separated_nonempty_list(COMMA,located(expr)), RPAREN)) {Tagged(cid,tyl,expl)}
  | tupl=loption(delimited(LPAREN,separated_nonempty_list(COMMA,located(expr)), RPAREN)) {Tuple(tupl)}

tyList:
  | l=loption(delimited(LESS,separated_nonempty_list(COMMA,located(ty)),GREATER))  {Some l}

literal:
  | i=INT {LInt(i)}
  | s=STRING {LString(s)}
  | c=CHAR {LChar(c.[0])}

type_scheme:
  | tsv=loption(delimited(LSBRACKET, located(tid)+, RSBRACKET)) ty=located(ty)  {ForallTy(tsv,ty)}

pattern:
  | vi=located(varid)  {PVariable(vi)}
  | UNDERSCORE {PWildcard}
  | p=pattern_tuple {PTuple(p)}
  | p=located(pattern) COMMA t=located(ty) {PTypeAnnotation(p,t)}
  | l=located(literal) {PLiteral(l)}
  | pt=pattern_tcon {pt}

pattern_tuple: p=delimited(LPAREN, separated_nonempty_list(COMMA, located(pattern)), RPAREN) {p}

pattern_tcon: t=located(cid) ty=pattern_ty? p=loption(pattern_tuple) {PTaggedValue(t,ty,p)}

pattern_ty: ty=delimited(LESS, separated_nonempty_list(COMMA, located(ty)), GREATER) {ty}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}