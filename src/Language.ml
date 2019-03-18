(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let to_bool x = x != 0

    let from_bool x = if x then 1 else 0

    let eval_boolop op x y = 
      match op with
      | "!!" -> (to_bool x) || (to_bool y)
      | "&&" -> (to_bool x) && (to_bool y)
      | "==" -> x == y
      | "!=" -> x != y
      | "<=" -> x <= y
      | "<"  -> x < y
      | ">=" -> x >= y
      | ">"  -> x > y

    let rec eval_binop op x y = 
      match op with
      | "+" -> x + y
      | "-" -> x - y
      | "*" -> x * y
      | "/" -> x / y
      | "%" -> x mod y
      | bool_op -> from_bool (eval_boolop bool_op x y)

    let rec eval state expr = 
      match expr with
      | Const (const) -> const
      | Var (var) -> state var
      | Binop (op, x, y) -> eval_binop op (eval state x) (eval state y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)

    let binops = [|
                    `Lefta, ["!!"];
                    `Lefta, ["&&"];
                    `Nona , ["<="; "<"; ">="; ">"; "=="; "!="];
                    `Lefta, ["+"; "-"];
                    `Lefta, ["*"; "/"; "%"];
                  |]

    let parse_binop op = ostap(- $(op)), (fun x y -> Binop (op, x, y))
    ostap (
        expr: !(Ostap.Util.expr (fun x -> x)
          (Array.map (fun (assoc, ops) -> (assoc, List.map parse_binop ops)) binops)
          primary
        );
  
        primary: n:DECIMAL {Const n} | x:IDENT {Var x} | -"(" expr -")"
      )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (state, input, output) t = 
    match t with
    | Read (var) -> let first::rest = input 
                    in (Expr.update var first state, rest, output)
    | Write (expr) -> (state, input, output @ [Expr.eval state expr])
    | Assign (var, expr) -> (Expr.update var (Expr.eval state expr) state, input, output)
    | Seq (t1, t2) -> eval (eval (state, input, output) t1) t2
    | _ -> failwith ""

    (* Statement parser *)
    ostap (
        stmt: "read" "(" var:IDENT ")" {Read var}
            | "write" "(" e:!(Expr.expr) ")" {Write e}
            | var:IDENT ":=" e:!(Expr.expr) {Assign (var, e)};
  
        parse: s:stmt ";" rest:parse {Seq (s, rest)} | stmt
      )
        
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
