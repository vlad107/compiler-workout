open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval (stack, (state, input, output)) prog = 
	match prog with
	| [] -> (stack, (state, input, output))
	| BINOP op :: rest_prog -> let (y :: x :: rest) = stack in
				       			eval ((Expr.eval_binop op x y) :: rest, (state, input, output)) rest_prog
    | CONST const :: rest_prog -> eval (const :: stack, (state, input, output)) rest_prog
    | READ :: rest_prog -> let (elem :: inp) = input in
    					 	eval (elem :: stack, (state, inp, output)) rest_prog
    | WRITE :: rest_prog -> let (elem :: rest_stack) = stack in
    						 eval (rest_stack, (state, input, output @ [elem])) rest_prog 
    | LD var :: rest_prog -> eval ((state var) :: stack, (state, input, output)) rest_prog
    | ST var :: rest_prog -> let (elem :: rest_stack) = stack in
    						  eval (rest_stack, (Expr.update var elem state, input, output)) rest_prog

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile_expr expr = 
	match expr with
	| Expr.Const (const) -> [CONST const]
	| Expr.Var (var) -> [LD var]
	| Expr.Binop (op, x, y) -> (compile_expr x) @ (compile_expr y) @ [BINOP op] 

let rec compile t = 
  match t with
  | Stmt.Read (var) -> [READ; ST var]
  | Stmt.Write (expr) -> (compile_expr expr) @ [WRITE]
  | Stmt.Assign (var, expr) -> (compile_expr expr) @ [ST var]
  | Stmt.Seq (t1, t2) -> (compile t1) @ (compile t2)