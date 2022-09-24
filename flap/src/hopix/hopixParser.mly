%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}

%token EOF TYPE TYPE_VARIABLE LESS GREATER EQUAL COMMA EXTERN 
COLON ID BAR CONSTR_ID LPAREN RPAREN LET FUN AND


%start<HopixAST.t> program

%%

program: definition_list EOF
{
  []
}

definition_list: definition
{
  []
}
| definition_list definition
{
  []
}

definition: TYPE ID definition_continue
{
  []
}
| EXTERN ID COLON type_scheme
{
  []
}
| vdefinition
{
  []
}

definition_continue:
{
  []
}
| definition_variables
{
  []
}
| definition_tdefinition
{
  []
}
| definition_variables definition_tdefinition
{
  []
}

definition_variables: LESS type_variable_list GREATER
{
  []
}

definition_tdefinition: EQUAL tdefinition
{
  []
}

type_variable_list: TYPE_VARIABLE 
{
  []
}
| type_variable_list COMMA TYPE_VARIABLE
{
  []
}

tdefinition: BAR constr tdefinition_end
{
  []
}
| constr tdefinition_end
{
  []
}

constr: CONSTR_ID constr_type_list
{
  []
}
| CONSTR_ID
{
  []
}

constr_type_list: LPAREN constr_type RPAREN
{
  []
}

constr_type: TYPE 
{
  []
}
| constr_type COMMA TYPE
{
  []
}

tdefinition_end:
{
  []
}
| tdefinition_end BAR CONSTR_ID constr_type_list
{
  []
}

vdefinition:LET ID vdefinition_end
{
  []
}

vdefinition_end: COLON type_scheme EQUAL expr
{
  []
}
| FUN fundef vdefinition_fun_end
{
  []
}

vdefinition_fun_end:
{
  []
}
| vdefinition_fun_end AND fundef
{
  []
}

fundef: BAR type_scheme fundef_end
{
  []
}
| fundef_end
{
  []
}

fundef_end: ID pattern EQUAL
{
  []
}

type_scheme:
{
  []
}

expr:
{
  []
}

pattern:
{
  []
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}