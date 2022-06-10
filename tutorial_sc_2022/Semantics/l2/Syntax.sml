(* 2002-11-08 -- Time-stamp: <2003-04-19 15:01:21 pes20>    -*-SML-*- *)


(* L2 *)


(* *********************)
(* the abstract syntax *)
(* *********************)

(* types of expressions and of locations*)

datatype type_expr =
         int
       | unit
       | bool
       | func of type_expr * type_expr

datatype type_loc =
         intref


(* locations *)

type loc = string

(* We've chosen to represent locations as strings, to make them easy
to pretty-print. *)
  

(* operations *)

datatype oper = Plus | GTEQ


(* first, the raw abstract syntax, in which variables are represented
as strings *)

type var_raw = string



(* raw expressions *)

datatype expr_raw = 
         Integer_raw of int
       | Boolean_raw of bool
       | Op_raw of expr_raw * oper * expr_raw
       | If_raw of expr_raw * expr_raw * expr_raw
       | Assign_raw of loc * expr_raw
       | Deref_raw of loc
       | Skip_raw
       | Seq_raw of expr_raw * expr_raw
       | While_raw of expr_raw * expr_raw
       | Var_raw of var_raw
       | Fn_raw of var_raw * type_expr * expr_raw
       | App_raw of expr_raw * expr_raw
       | Let_raw of var_raw * type_expr * expr_raw * expr_raw
       | Letrecfn_raw of var_raw * type_expr * var_raw * type_expr * expr_raw * expr_raw

(* expressions up to alpha conversion *)

(* In the abstract syntax up to alpha conversion, in which variables
are represented as a pointer to their binder - a count of the number
of Fn-nodes outwards (and similarly for Let and Letrec).  This differs
from the expr_raw type in the Var and Fn clauses.

The scope resolution function in Semantics.sml converts from expr_raw
to expr, for example:

  raw:          fn x:int => x+7         fn x:int =>(fn y:int => x+y)
  up to alpha:  fn .:int => var_0 + 7   fn .:int =>(fn .:int => var_1 + var_0)
*)

datatype expr = 
         Integer of int
       | Boolean of bool
       | Op of expr * oper * expr
       | If of expr * expr * expr
       | Assign of loc * expr
       | Deref of loc
       | Skip
       | Seq of expr * expr
       | While of expr * expr
       | Var of int
       | Fn of type_expr * expr
       | App of expr * expr
       | Let of  type_expr * expr * expr
       | Letrecfn of type_expr * type_expr * expr * expr


(* Note that this does not exactly match the semantics, as that
allowed arbitrary integers whereas here we use the bounded mosml
integers -- so not every term of the abstract syntax is representable
as an element of type expr, and the interpreter will fail with an
overflow exception if + overflows.
*)

(* Typically, even in an implementation that (as ours) uses De Bruijn
indices, you might expect to record the original names of binders in
the abstract-up-to-alpha syntax too, so that meaningful error messages
can be printed. We don't do that here, as we don't want to obscure the
essential machinery. *)

