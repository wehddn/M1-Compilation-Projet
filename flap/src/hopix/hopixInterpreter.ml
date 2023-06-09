open Position
open Error
open HopixAST

(** [error pos msg] reports execution error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of Hopix evaluates into a [value].

   The [value] type is not defined here. Instead, it will be defined
   by instantiation of following ['e gvalue] with ['e = environment].
   Why? The value type and the environment type are mutually recursive
   and since we do not want to define them simultaneously, this
   parameterization is a way to describe how the value type will use
   the environment type without an actual definition of this type.

*)
type 'e gvalue =
  | VInt       of Mint.t
  | VChar      of char
  | VString    of string
  | VUnit
  | VTagged    of constructor * 'e gvalue list
  | VTuple     of 'e gvalue list
  | VRecord    of (label * 'e gvalue) list
  | VLocation  of Memory.location
  | VClosure   of 'e * pattern located * expression located
  | VPrimitive of string * ('e gvalue Memory.t -> 'e gvalue -> 'e gvalue)

(** Two values for booleans. *)
let ptrue  = VTagged (KId "True", [])
let pfalse = VTagged (KId "False", [])

(**
    We often need to check that a value has a specific shape.
    To that end, we introduce the following coercions. A
    coercion of type [('a, 'e)] coercion tries to convert an
    Hopix value into a OCaml value of type ['a]. If this conversion
    fails, it returns [None].
*)

type ('a, 'e) coercion = 'e gvalue -> 'a option
let fail = None
let ret x = Some x
let value_as_int      = function VInt x -> ret x | _ -> fail
let value_as_char     = function VChar c -> ret c | _ -> fail
let value_as_string   = function VString s -> ret s | _ -> fail
let value_as_tagged   = function VTagged (k, vs) -> ret (k, vs) | _ -> fail
let value_as_record   = function VRecord fs -> ret fs | _ -> fail
let value_as_location = function VLocation l -> ret l | _ -> fail
let value_as_closure  = function VClosure (e, p, b) -> ret (e, p, b) | _ -> fail
let value_as_primitive = function VPrimitive (p, f) -> ret (p, f) | _ -> fail
let value_as_bool = function
  | VTagged (KId "True", []) -> true
  | VTagged (KId "False", []) -> false
  | _ -> assert false

(**
   It is also very common to have to inject an OCaml value into
   the types of Hopix values. That is the purpose of a wrapper.
 *)
type ('a, 'e) wrapper = 'a -> 'e gvalue
let int_as_value x  = VInt x
let bool_as_value b = if b then ptrue else pfalse

(**

  The flap toplevel needs to print the result of evaluations. This is
   especially useful for debugging and testing purpose. Do not modify
   the code of this function since it is used by the testsuite.

*)
let print_value m v =
  (** To avoid to print large (or infinite) values, we stop at depth 5. *)
  let max_depth = 5 in

  let rec print_value d v =
    if d >= max_depth then "..." else
      match v with
        | VInt x ->
          Mint.to_string x
        | VChar c ->
          "'" ^ Char.escaped c ^ "'"
        | VString s ->
          "\"" ^ String.escaped s ^ "\""
        | VUnit ->
          "()"
        | VLocation a ->
          print_array_value d (Memory.dereference m a)
        | VTagged (KId k, []) ->
          k
        | VTagged (KId k, vs) ->
          k ^ print_tuple d vs
        | VTuple (vs) ->
           print_tuple d vs
        | VRecord fs ->
           "{"
           ^ String.concat ", " (
                 List.map (fun (LId f, v) -> f ^ " = " ^ print_value (d + 1) v
           ) fs) ^ "}"
        | VClosure _ ->
          "<fun>"
        | VPrimitive (s, _) ->
          Printf.sprintf "<primitive: %s>" s
    and print_tuple d vs =
      "(" ^ String.concat ", " (List.map (print_value (d + 1)) vs) ^ ")"
    and print_array_value d block =
      let r = Memory.read block in
      let n = Mint.to_int (Memory.size block) in
      "[ " ^ String.concat ", " (
                 List.(map (fun i -> print_value (d + 1) (r (Mint.of_int i)))
                         (ExtStd.List.range 0 (n - 1))
               )) ^ " ]"
  in
  print_value 0 v

let print_values m vs =
  String.concat "; " (List.map (print_value m) vs)

module Environment : sig
  (** Evaluation environments map identifiers to values. *)
  type t

  (** The empty environment. *)
  val empty : t

  (** [bind env x v] extends [env] with a binding from [x] to [v]. *)
  val bind    : t -> identifier -> t gvalue -> t

  (** [update pos x env v] modifies the binding of [x] in [env] so
      that [x ↦ v] ∈ [env]. *)
  val update  : Position.t -> identifier -> t -> t gvalue -> unit

  (** [lookup pos x env] returns [v] such that [x ↦ v] ∈ env. *)
  val lookup  : Position.t -> identifier -> t -> t gvalue

  (** [UnboundIdentifier (x, pos)] is raised when [update] or
      [lookup] assume that there is a binding for [x] in [env],
      where there is no such binding. *)
  exception UnboundIdentifier of identifier * Position.t

  (** [last env] returns the latest binding in [env] if it exists. *)
  val last    : t -> (identifier * t gvalue * t) option

  (** [print env] returns a human readable representation of [env]. *)
  val print   : t gvalue Memory.t -> t -> string
end = struct

  type t =
    | EEmpty
    | EBind of identifier * t gvalue ref * t

  let empty = EEmpty

  let bind e x v =
    EBind (x, ref v, e)

  exception UnboundIdentifier of identifier * Position.t

  let lookup' pos x =
    let rec aux = function
      | EEmpty -> raise (UnboundIdentifier (x, pos))
      | EBind (y, v, e) ->
        if x = y then v else aux e
    in
    aux

  let lookup pos x e = !(lookup' pos x e)

  let update pos x e v =
    lookup' pos x e := v

  let last = function
    | EBind (x, v, e) -> Some (x, !v, e)
    | EEmpty -> None

  let print_binding m (Id x, v) =
    x ^ " = " ^ print_value m !v

  let print m e =
    let b = Buffer.create 13 in
    let push x v = Buffer.add_string b (print_binding m (x, v)) in
    let rec aux = function
      | EEmpty -> Buffer.contents b
      | EBind (x, v, EEmpty) -> push x v; aux EEmpty
      | EBind (x, v, e) -> push x v; Buffer.add_string b "\n"; aux e
    in
    aux e

end

(**
    We have everything we need now to define [value] as an instantiation
    of ['e gvalue] with ['e = Environment.t], as promised.
*)
type value = Environment.t gvalue

(**
   The following higher-order function lifts a function [f] of type
   ['a -> 'b] as a [name]d Hopix primitive function, that is, an
   OCaml function of type [value -> value].
*)
let primitive name ?(error = fun () -> assert false) coercion wrapper f
: value
= VPrimitive (name, fun x ->
    match coercion x with
      | None -> error ()
      | Some x -> wrapper (f x)
  )

type runtime = {
  memory      : value Memory.t;
  environment : Environment.t;
}

type observable = {
  new_memory      : value Memory.t;
  new_environment : Environment.t;
}

(** [primitives] is an environment that contains the implementation
    of all primitives (+, <, ...). *)
let primitives =
  let intbin name out op =
    let error m v =
      Printf.eprintf
        "Invalid arguments for `%s': %s\n"
        name (print_value m v);
      assert false (* By typing. *)
    in
    VPrimitive (name, fun m -> function
      | VInt x ->
         VPrimitive (name, fun m -> function
         | VInt y -> out (op x y)
         | v -> error m v)
      | v -> error m v)
  in
  let bind_all what l x =
    List.fold_left (fun env (x, v) -> Environment.bind env (Id x) (what x v))
      x l
  in
  (* Define arithmetic binary operators. *)
  let binarith name =
    intbin name (fun x -> VInt x) in
  let binarithops = Mint.(
    [ ("`+`", add); ("`-`", sub); ("`*`", mul); ("`/`", div) ]
  ) in
  (* Define arithmetic comparison operators. *)
  let cmparith name = intbin name bool_as_value in
  let cmparithops =
    [ ("`=?`", ( = ));
      ("`<?`", ( < ));
      ("`>?`", ( > ));
      ("`>=?`", ( >= ));
      ("`<=?`", ( <= )) ]
  in
  let boolbin name out op =
    VPrimitive (name, fun _ x -> VPrimitive (name, fun _ y ->
        out (op (value_as_bool x) (value_as_bool y))))
  in
  let boolarith name = boolbin name (fun x -> if x then ptrue else pfalse) in
  let boolarithops =
    [ ("`||`", ( || )); ("`&&`", ( && )) ]
  in
  let generic_printer =
    VPrimitive ("print", fun m v ->
      output_string stdout (print_value m v);
      flush stdout;
      VUnit
    )
  in
  let print s =
    output_string stdout s;
    flush stdout;
    VUnit
  in
  let print_int =
    VPrimitive  ("print_int", fun _ -> function
      | VInt x -> print (Mint.to_string x)
      | _ -> assert false (* By typing. *)
    )
  in
  let print_string =
    VPrimitive  ("print_string", fun _ -> function
      | VString x -> print x
      | _ -> assert false (* By typing. *)
    )
  in
  let bind' x w env = Environment.bind env (Id x) w in
  Environment.empty
  |> bind_all binarith binarithops
  |> bind_all cmparith cmparithops
  |> bind_all boolarith boolarithops
  |> bind' "print"        generic_printer
  |> bind' "print_int"    print_int
  |> bind' "print_string" print_string
  |> bind' "true"         ptrue
  |> bind' "false"        pfalse
  |> bind' "nothing"      VUnit

let initial_runtime () = {
  memory      = Memory.create (640 * 1024 (* should be enough. -- B.Gates *));
  environment = primitives;
}

let rec evaluate runtime ast =
  try
    let runtime' = List.fold_left definition runtime ast in
    (runtime', extract_observable runtime runtime')
  with Environment.UnboundIdentifier (Id x, pos) ->
    Error.error "interpretation" pos (Printf.sprintf "`%s' is unbound." x)

(** [definition pos runtime d] evaluates the new definition [d]
    into a new runtime [runtime']. In the specification, this
    is the judgment:

                        E, M ⊢ dv ⇒ E', M'

*)
and definition runtime d =
  match Position.value d with
    | DefineValue d ->
    {
      runtime with environment = value_definition runtime.environment runtime.memory d
    }
    | _ -> failwith "Students! This is your job (def)!"

and value_definition environment memory = function
  | SimpleValue (x, _, e) -> 
    let v = expression' environment memory e in 
    bind_identifier environment x v
  | RecFunctions rf -> 
    let rec aux rf environment = 
    begin match rf with
      | [] -> environment
      | (id,_, FunctionDefinition(pattern, expr))::t -> 
        let closure = VClosure (environment, pattern, expr) in
        try Environment.update (Position.position id) (Position.value id) environment closure; aux t environment
        with Environment.UnboundIdentifier _ -> let environment = bind_identifier environment id closure in
      aux t environment 
      end
    in
    aux rf environment
    
    (*let ((id,_, FunctionDefinition(pattern, expr))::t) = rf in
    let closure = VClosure (environment, pattern, expr) in
    bind_identifier environment id closure*)

and bind_identifier environment x v =
  Environment.bind environment (Position.value x) v

and expression' environment memory e =
  expression (position e) environment memory (value e)

(** [expression pos runtime e] evaluates into a value [v] if

                          E, M ⊢ e ⇓ v, M'

   and E = [runtime.environment], M = [runtime.memory].
*)
and expression _ environment memory = function
  | Apply (a, b) ->
    let vb = expression' environment memory b in
    begin match expression' environment memory a with
      | VPrimitive (_, f) ->
        f memory vb
      | VClosure (_,p,e) -> 
        let (_,env) = patternM environment vb (Position.value p) in
          expression' env memory e
      | _ -> match Position.value a with
        | Apply (a1,b2) -> failwith "Students! This is your job expr - apply tuple!"
        | _ -> failwith "Apply error!"
    end
  
  | Variable (x, _) ->
    Environment.lookup (Position.position x) (Position.value x) environment

  | Sequence(ls) ->
    let vs = List.map (expression' environment memory) ls in
    List.(hd vs)

  | Literal l ->
    literal (Position.value l)

  | Tagged (c, _, el) -> 
    let vc = Position.value c in
    let vel = List.map (expression' environment memory) el in
    VTagged(vc, vel)

  | Record (l,_) -> 
    let vl = List.map (fun (x,y) -> (Position.value x, expression' environment memory y)) l in
    VRecord(vl)

  | Fun (FunctionDefinition(p,e)) -> VClosure(environment,p,e)

  | Tuple t -> 
    let vt = List.map (expression' environment memory) t in
    VTuple(vt)

  | Field (e,l) -> 
    let v = expression' environment memory e in
    begin match v with 
    | VRecord list -> let vi = List.assoc (Position.value l ) list in vi
    | _ -> assert false
    end

  | Define (vd,e) -> 
    let runtime = value_definition environment memory vd in
    expression (Position.position e) runtime memory (Position.value e)

  | Ref e -> 
      let el = expression' environment memory e in
      let a = Memory.allocate memory 1L el in
      VLocation a

  | Assign (e1,e2) -> 
    let e1 = expression' environment memory e1 in
    let e2 = expression' environment memory e2 in
      begin match e1 with
      | VLocation a -> 
        let da = Memory.dereference memory a in
        (Memory.write da 0L e2); 
        e2 
      | _ -> failwith "Assign error"
      end

  | Read (e) ->
    let e = expression' environment memory e in
      begin match e with
      | VLocation a -> 
        let da = Memory.dereference memory a in 
        Memory.read da 0L 
      | _ -> failwith "Read error"
      end
 
  | IfThenElse (c, t, f) ->
    let v = expression' environment memory c in
    begin match value_as_bool v with
      | true -> expression' environment memory t
      | false -> expression' environment memory f
    end

    | While (c, e) ->
      let rec aux () =
        let v = expression' environment memory c in 
        match value_as_bool v  with
          | true ->
            ignore (expression' environment memory e);
            aux ()
          | false ->
            VUnit
      in
      aux ()

    | For (x,e1,e2,e3) ->
    let rec aux(env) =
      let en = Environment.lookup x.position x.value env in
      let e = expression' env memory e2 in
      match en, e with
        | VInt v, VInt c ->
          if v <= c then
            (ignore(expression' env memory e3);
            let ipp = VInt (Mint.of_int (Mint.to_int v + 1)) in
            Environment.update x.position x.value env ipp;
            aux env)
            else VUnit
        | _,_ -> assert false
          in 
    let i = expression' environment memory e1 in
    let env = bind_identifier environment x i in 
    aux(env) 

  | Case (e,branches) -> 
    let a = expression' environment memory e in
    let rec aux b =
      match b with
      | [] -> VUnit
      | v::r ->
        let Branch(pattern,expr) = v.value in
        let (b,env) = patternM environment a (pattern.value) in
        if b
          then expression' env memory expr
      else aux r
    in aux branches
      
  | _ -> failwith "Students! This is your job expr!"

and patternM env e pattern : bool*Environment.t =
  match pattern with
  | PVariable id-> 
    let new_env = Environment.bind env (Position.value id) e in
    (true,new_env)

  | PWildcard ->
    (true,env)

  | PTypeAnnotation(pat,_)->
    patternM env e (Position.value pat)

  | PLiteral l->
    begin
    match Position.value l,e with
    | LInt i1, VInt i2 -> (i1=i2,env)
    | LChar c1, VChar c2 -> (c1=c2,env)
    | LString s1, VString s2 -> (s1=s2,env)
    | _,_ -> (false,env)
    end
  
  | PTaggedValue(cons,_,pattern_list)->
  begin
    match e with
    | VTagged(cons2,expr_list) ->
    if ((Position.value cons) <> cons2) then (false,env)
    else
      let pattern_list = List.map (Position.value) pattern_list in 
      let rec aux pattern_list expr_list env = 
        begin match pattern_list,expr_list with
        | [],[] -> (true,env)
        | p::r, e::r2 -> 
          let (b,env) = patternM env e p in
          if b then aux r r2 env else (false,env)
        | _,_ -> assert false
        end in 
      aux pattern_list expr_list env
    | _ -> failwith "Pattern - PTaggedValue error"
  end
  | PRecord(_,_) ->
    failwith("TODO PRECORD")

  | PTuple(patternlist) ->
    let rec aux patternlist env =
      begin
      match patternlist with
      | [] -> (true,env)
      | h::q -> 
        let (b,new_env) = patternM env e (Position.value h) in
        if b then aux q new_env else (false,new_env)
      end
      in aux patternlist env

  | POr(patternlist)  ->
    let rec aux patternlist env =
    begin
    match patternlist with
    | [] -> (false,env)
    | h::q -> 
      let (b,new_env) = patternM env e (Position.value h) in
      if b then (true,new_env) else aux q new_env
    end
    in aux patternlist env

  | PAnd(patternlist)  ->
    let rec aux patternlist env =
      begin
      match patternlist with
      | [] -> (true,env)
      | h::q -> 
        let (b,new_env) = patternM env e (Position.value h) in
        if b then aux q new_env else (false,new_env)
      end
      in aux patternlist env

and literal = function
  | LInt x -> VInt x
  | LChar c -> VChar c
  | LString s -> VString s

and expressions environment memory es =
  let rec aux vs = function
    | [] ->
      List.rev vs
    | e :: es ->
      let v = expression environment memory e in
      aux (v :: vs) es
  in
  aux [] es

(** This function returns the difference between two runtimes. *)
and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
        | None -> assert false (* Absurd. *)
        | Some (x, v, env') ->
          let new_environment = Environment.bind new_environment x v in
          substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.empty runtime.environment runtime'.environment;
    new_memory =
      runtime'.memory
  }

(** This function displays a difference between two runtimes. *)
let print_observable (_ : runtime) observation =
  Environment.print observation.new_memory observation.new_environment

