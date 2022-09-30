%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}

%token EOF TYPE LESS GREATER EQUAL COMMA EXTERN 
COLON BAR LPAREN RPAREN LET FUN AND LCBRACKET RCBRACKET 
LSBRACKET RSBRACKET

%token <string> ID TYPE_VARIABLE CONSTR_ID STRING CHAR

%start<HopixAST.t> program

%%

program:
  | p=located(definition)* EOF                 {p}

definition:
  | TYPE tc=located(tcon) df=definition_variables dt=definition_tdefinition  {DefineType(tc,df,dt)}
  | EXTERN vi=located(varid) COLON ts=located(type_scheme)   {DeclareExtern(vi,ts)}

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
  | LPAREN tl=located(ty)* RPAREN  {tl}


tdefinition_label:
  | id=located(id) COLON t=located(ty)           {(id, t)}

id: i=ID {LId(i)}

ty: t=tid {TyVar(t)}

type_scheme:
  | ty=located(ty)                                                {ForallTy([],ty)}
  | LSBRACKET tsv=nonempty_list(located(tid)) RSBRACKET ty=located(ty)  {ForallTy(tsv,ty)}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}