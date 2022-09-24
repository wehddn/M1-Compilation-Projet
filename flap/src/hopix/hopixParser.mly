%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}

%token EOF TYPE TYPE_VARIABLE LESS GREATER EQUAL COMMA EXTERN 
COLON ID BAR CONSTR_ID LPAREN RPAREN LET FUN AND LCBRACKET RCBRACKET


%start<HopixAST.t> program

%%

program:
  p=definition_list EOF                 {p}

definition_list: 
  | d=definition                        {[d]}
  | d=definition dl=definition_list     {d::dl}

definition: 
  | TYPE tc=ID df=definition_variables dt=definition_tdefinition  {(tc,df,dt)}
  | EXTERN te=ID COLON ts=type_scheme                             {(te,ts)}
  | v=vdefinition                                                 {v}

definition_variables:
  |                                                 {[]}
  | LESS tvl=type_variable_list GREATER             {tvl}

type_variable_list: 
  | tv=TYPE_VARIABLE                                {[tv]}  
  | tv=TYPE_VARIABLE COMMA tvl=type_variable_list   {tv::tvl}

definition_tdefinition:
  |                        {[]}
  | EQUAL t=tdefinition    {t}

tdefinition: 
  | ci=CONSTR_ID ct=tdefinition_types               {[(ci,ct)]}
  | ctl=constr_type_list                            {ctl}
  | LCBRACKET tll=tdefinition_label_list RCBRACKET  {tll}

constr_type_list:
  | tc=tdefinition_constr                       {tc}
  | tc=tdefinition_constr ctl=constr_type_list  {tc::ctl}

tdefinition_constr: 
  | BAR ci=CONSTR_ID ct=tdefinition_types       {[(ci,ct)]}

tdefinition_types:
  |                             {[]}
  | LPAREN tl=type_list RPAREN  {tl}

type_list:
  | t=typ                       {[t]}
  | t=typ tl=type_list          {t::tl}

tdefinition_label_list:
  | tl=tdefinition_label                                    {[tl]}
  | tl=tdefinition_label COMMA tll=tdefinition_label_list   {tl::tll}

tdefinition_label:
  | id=ID COLON t=typ           {[(id, t)]}

vdefinition:
  | LET id=ID EQUAL e=expr                        {[]}                   
  | LET id=ID COLON ts=type_scheme EQUAL e=expr   {[]}
  | FUN vf=vdefinition_fun                        {[]}

vdefinition_fun:
  | fundef                {[]}
  | fundef AND fundef     {[]}

fundef: {[]}

expr: {[]}

typ: {[]}

type_scheme:
{
  []
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}