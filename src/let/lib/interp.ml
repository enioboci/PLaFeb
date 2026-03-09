(* Name: Enio Boci
*)

open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)

  | Var(id) ->
    apply_env id

  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))

  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))

  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))

  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))

  | Let(id,def,body) ->
    eval_expr def >>=
    extend_env id >>+
    eval_expr body 

  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3

  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))

  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"

  | IsEmpty(e1) ->
    eval_expr e1 >>=
    tree_of_treeVal >>= fun t ->
    (match t with
     | Empty -> return (BoolVal true)
     | Node(_,_,_) -> return (BoolVal false))

  | EmptyTree(_t) ->
    return (TreeVal Empty)

  | Node(e1,e2,e3) ->
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>=
    tree_of_treeVal >>= fun t2 ->
    eval_expr e3 >>=
    tree_of_treeVal >>= fun t3 ->
    return (TreeVal (Node(v1,t2,t3)))

  | CaseT(e1,e2,id1,id2,id3,e3) ->
    eval_expr e1 >>=
    tree_of_treeVal >>= fun t ->
    (match t with
     | Empty ->
       eval_expr e2
     | Node(v,l,r) ->
       extend_env id1 v >>+
       extend_env id2 (TreeVal l) >>+
       extend_env id3 (TreeVal r) >>+
       eval_expr e3)

  | Record(fs) ->
    if has_duplicate_fields fs
    then error "Record: duplicate fields"
    else
      let rec eval_fields fs =
        match fs with
        | [] -> return []
        | (id,(b,exp))::rest ->
          eval_expr exp >>= fun v ->
          eval_fields rest >>= fun rest_vals ->
          return ((id,(b,v))::rest_vals)
      in
      eval_fields fs >>= fun vals ->
      return (RecordVal vals)

  | Proj(e1,id) ->
    eval_expr e1 >>=
    record_of_recordVal >>= fun fs ->
    lookup_record_field id fs

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
