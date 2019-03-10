open Lexer
open Parser
open Ast

exception CEKError of string
let cek_error msg = raise (CEKError msg)

let is_v = function
  | Con _ | Clo _ -> true
  | _ -> false

let is_var = function
  | Var _ -> true
  | _ -> false

let get_clos = function
  | EMt -> Hashtbl.create 5
  | EClo c -> c

let eval t = match t with
  | ST (x, e, k) when is_var x ->
    begin try
    ST (Hashtbl.find (get_clos e) x, e, k)
    with Not_found -> cek_error ("Unbound variable: " ^ str_m x)
    end
  | ST (App (m, n), e, k) ->
    ST (m, e, Fn (n, e, k))
  | ST (v, e, Fn (n, e', k)) when is_v v ->
    ST (n, e', Ar (v, k))
  | ST (Abs (x, m), e, k) ->
    ST (Clo (Abs (x, m), e), e, k)
  | ST (v, e, Ar (Clo (Abs (x, m), e'), k)) when is_v v ->
    let env' = get_clos e' in
    Hashtbl.add env' (Var x) v;
    ST (m, EClo env', k)
  | ST (Prm (o, (m::n)), e, k) ->
    ST (m, e, Pr (o, [], n, k))
  | ST (v, e, Pr (o, u, m::n, k)) ->
    ST (m, e, Pr (o, u@[v], n, k))
  | ST (b, e, Pr (o, bs, [], k)) ->
    begin match o with
    | "+" ->
      let value = List.fold_left (fun acc (Con i) -> acc + i) 0 (b::bs) in
      ST (Con value, e, k)
    | _ -> cek_error "Unknown primitive operation"
    end
  | _ -> cek_error ("Unknown state: " ^ str_st t)

let rec eval_program s = match s with
  | ST (v, _, Mt) when is_v v -> ST (v, EMt, Mt)
  | _ -> eval_program (eval s)

let run s = eval_program @@ ST (s, EMt, Mt)

let rec repl () =
  print_string "> ";
  let str = read_line () in
  let buffer = Lexing.from_string str in
  let ast = main token buffer in
  print_endline @@ str_st @@ run ast;
  repl ()

let _ = repl ()
