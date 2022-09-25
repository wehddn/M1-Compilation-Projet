%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}

%token EOF TYPE LESS GREATER EQUAL COMMA EXTERN 
COLON BAR LPAREN RPAREN LET FUN AND LCBRACKET RCBRACKET 
LSBRACKET RSBRACKET

%token <string> ID TYPE_VARIABLE CONSTR_ID

%start<HopixAST.t> program

%%

program:
  p=definition_list EOF                 {p}

definition_list: 
  | d=located(definition)                        {[d]}
  | d=located(definition) dl=definition_list     {d::dl}

definition:
  | TYPE tc=located(tcon) df=definition_variables dt=definition_tdefinition  {DefineType(tc,df,dt)}
  | EXTERN vi=located(varid) COLON ts=located(type_scheme)   {DeclareExtern(vi,ts)}

varid: i=ID {Id(i)}

tcon: i=ID {TCon(i)}

definition_variables:
  |                                                 {[]}
  | LESS tvl=type_variable_list GREATER             {tvl}

type_variable_list: 
  | tv=located(tid)                                {[tv]}  
  | tv=located(tid) COMMA tvl=type_variable_list   {tv::tvl}

tid: i=TYPE_VARIABLE {TId(i)}

definition_tdefinition:
  |                        {Abstract}
  | EQUAL t=tdefinition    {t}

tdefinition: 
  | ci=located(cid) ct=tdefinition_types            {DefineSumType([(ci,ct)])}
  | ctl=constr_type_list                            {DefineSumType(ctl)}
  | LCBRACKET tll=tdefinition_label_list RCBRACKET  {DefineRecordType(tll)}

cid: i=CONSTR_ID {KId(i)}

constr_type_list:
  | tc=tdefinition_constr                       {[tc]}
  | tc=tdefinition_constr ctl=constr_type_list  {tc::ctl}

tdefinition_constr: 
  | BAR ci=located(cid) ct=tdefinition_types       {(ci,ct)}

tdefinition_types:
  |                             {[]}
  | LPAREN tl=type_list RPAREN  {tl}

type_list:
  | t=located(ty)                       {[t]}
  | t=located(ty) tl=type_list          {t::tl}

tdefinition_label_list:
  | tl=tdefinition_label                                    {[tl]}
  | tl=tdefinition_label COMMA tll=tdefinition_label_list   {tl::tll}

tdefinition_label:
  | id=located(id) COLON t=located(ty)           {(id, t)}

id: i=ID {LId(i)}

ty: t=tid {TyVar(t)}

type_scheme:
  | ty=located(ty)                                                {ForallTy([],ty)}
  | LSBRACKET tsv=type_scheme_variables RSBRACKET ty=located(ty)  {ForallTy(tsv,ty)}

type_scheme_variables: 
  | tv=located(tid)                          {[tv]}  
  | tv=located(tid) tvl=type_variable_list   {tv::tvl}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}