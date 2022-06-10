(* 2002-11-08 -- Time-stamp: <2003-05-01 11:57:57 pes20>    -*-SML-*- *)

(* L2 semantics *)

(* NB, this is a prototype interpreter - aiming to show how the
semantics of L2 can be most easily implemented, so that one can
experiment with the behaviour and typing of L2 programs.  It's not
aiming to be an efficient implementation! *)

(* Coding style: this largely avoids use of fancy ML constructs, eg
exceptions, even though ultimately they would be the most natural
idiom in places. *)


open Syntax;


(* **********************************)
(* scope resolution                 *)
(* **********************************)

exception Resolve of string;;

(*
fun find_first : var_raw -> var_raw list -> int -> int option

find_first x ys 0 returns either SOME m', where m' is the index of the
first occurrence of x in ys, or NONE, if there is no occurrence of x
in ys 
*)

fun find_first x [] m = NONE
 | find_first x (y::ys) m =  if y=x then SOME m else find_first x ys (m+1)


(* resolve : var_raw list -> expr_raw -> expr *)
fun resolve env (Integer_raw n)      = Integer n
  | resolve env (Boolean_raw b)      = Boolean b
  | resolve env (Op_raw (e1,opr,e2)) = Op (resolve env e1,opr,resolve env e2)
  | resolve env (If_raw (e1,e2,e3))  = If (resolve env e1, resolve env e2, 
                                           resolve env e3)
  | resolve env (Assign_raw (l,e))   = Assign(l,resolve env e)
  | resolve env (Deref_raw l)        = Deref l
  | resolve env (Skip_raw )          = Skip
  | resolve env (Seq_raw (e1,e2))    = Seq (resolve env e1,resolve env e2)
  | resolve env (While_raw (e1,e2))  = While (resolve env e1,resolve env e2)
  | resolve env (Var_raw x) 
    = ( case find_first x env 0 of 
            NONE => raise (Resolve ("Bogus unbound var_raw: " ^ x))
          | SOME m => Var m )
  | resolve env (Fn_raw (x,t,e))     = Fn (t,resolve (x::env) e) 
  | resolve env (App_raw (e1,e2))    = App (resolve env e1,resolve env e2)
  | resolve env (Let_raw (x,t,e1,e2)) 
    = Let (t,resolve env e1,resolve (x::env) e2)                 
  | resolve env (Letrecfn_raw (x,tx,y,ty,e1,e2))
    = Letrecfn (tx,ty,resolve (y::x::env) e1,resolve (x::env) e2)         
                            
fun resolve_scopes e = resolve [] e


(* **********************************)
(* an interpreter for the semantics *)
(* **********************************)


fun is_value (Integer n) = true
  | is_value (Boolean b) = true
  | is_value (Skip) = true
  | is_value (Fn (t,e)) = true
  | is_value _ = false


  (* In the semantics, a store is a finite partial function from
  locations to integers.  In the implementation, we represent a store
  as a list of loc*int pairs containing, for each l in the domain of
  the store, exactly one element of the form (l,n).  The operations

    lookup : store * loc         -> int option
    update : store * (loc * int) -> store option

  both return NONE if given a location that is not in the domain of
  the store.

  This is not a very efficient implementation, but it is simple. *)


type store = (loc * int) list


  (* now define the single-step function

     reduce :  expr * store -> (expr * store) option 

  which takes a configuration (e,s) and returns either NONE, if it has
  no transitions, or SOME (e',s'), if it has a transition (e,s) -->
  (e',s'). 

  Note that the code depends on global properties of the semantics,
  including the fact that it defines a deterministic transition
  system, so the comments indicating that particular lines of code
  implement particular semantic rules are not the whole story.

  *)



(* auxiliary functions for association lists *)

fun lookup ( [], l ) = NONE
  | lookup ( (l',n')::pairs, l) = 
    if l=l' then SOME n' else lookup (pairs,l)

fun update'  front [] (l,n) = NONE
 |  update'  front ((l',n')::pairs) (l,n) = 
    if l=l' then 
        SOME(front @ ((l,n)::pairs) )
    else 
        update' ((l',n')::front) pairs (l,n)

fun update (s, (l,n)) = update' [] s (l,n)

exception Reduce of string;


(* subst e 0 e' substitutes e for the outermost var in e'.  Note that this
definition is only sensible if e is closed, otherwise something more
delicate is required (cf Pierce S6.2) *)

fun subst e n (Integer n')      = Integer n'
  | subst e n (Boolean b)       = Boolean b
  | subst e n (Op (e1,opr,e2))  = Op (subst e n e1,opr,subst e n e2)
  | subst e n (If (e1,e2,e3))   = If (subst e n e1, subst e n e2, subst e n e3)
  | subst e n (Assign (l,e1))   = Assign(l,subst e n e1)
  | subst e n (Deref l)         = Deref l
  | subst e n (Skip)            = Skip
  | subst e n (Seq (e1,e2))     = Seq (subst e n e1,subst e n e2)
  | subst e n (While (e1,e2))   = While (subst e n e1,subst e n e2)
  | subst e n (Var n1)          = if n=n1 then e else Var n1
  | subst e n (Fn (t,e1))       = Fn (t,subst e (n+1) e1) 
  | subst e n (App (e1,e2))     = App (subst e n e1,subst e n e2)
  | subst e n (Let (t,e1,e2))   = Let (t,subst e n e1,subst e (n+1) e2)
  | subst e n (Letrecfn (tx,ty,e1,e2))
    = Letrecfn (tx,ty,subst e (n+2) e1,subst e (n+1) e2)


(* the shift and swap auxiliary functions are required for
manipulating De Bruijn indices in the Letrecfn case of reduce
below. *)

(* shift n e increments (by 1) all the variable indices >=n in e *)
fun shift n (Integer n')      = Integer n'
  | shift n (Boolean b)       = Boolean b
  | shift n (Op (e1,opr,e2))  = Op (shift n e1,opr,shift n e2)
  | shift n (If (e1,e2,e3))   = If (shift n e1, shift n e2, shift n e3)
  | shift n (Assign (l,e1))   = Assign(l,shift n e1)
  | shift n (Deref l)         = Deref l
  | shift n (Skip)            = Skip
  | shift n (Seq (e1,e2))     = Seq (shift n e1,shift n e2)
  | shift n (While (e1,e2))   = While (shift n e1,shift n e2)
  | shift n (Var n1)          = if n1>=n then Var (n1+1) else Var n1
  | shift n (Fn (t,e1))       = Fn (t,shift (n+1) e1) 
  | shift n (App (e1,e2))     = App (shift n e1,shift n e2)
  | shift n (Let (t,e1,e2))   = Let (t,shift n e1,shift (n+1) e2)
  | shift n (Letrecfn (tx,ty,e1,e2))
    = Letrecfn (tx,ty,shift (n+2) e1,shift (n+1) e2)


(* swap n e   swaps the nth and n+1th variable indices in e *)
fun swap n (Integer n')      = Integer n'
  | swap n (Boolean b)       = Boolean b
  | swap n (Op (e1,opr,e2))  = Op (swap n e1,opr,swap n e2)
  | swap n (If (e1,e2,e3))   = If (swap n e1, swap n e2, swap n e3)
  | swap n (Assign (l,e1))   = Assign(l,swap n e1)
  | swap n (Deref l)         = Deref l
  | swap n (Skip)            = Skip
  | swap n (Seq (e1,e2))     = Seq (swap n e1,swap n e2)
  | swap n (While (e1,e2))   = While (swap n e1,swap n e2)
  | swap n (Var n1)          = if n1=n then Var (n+1) 
                               else (if n1=n+1 then Var n else Var n1)
  | swap n (Fn (t,e1))       = Fn (t,swap (n+1) e1) 
  | swap n (App (e1,e2))     = App (swap n e1,swap n e2)
  | swap n (Let (t,e1,e2))   = Let (t,swap n e1,swap (n+1) e2)
  | swap n (Letrecfn (tx,ty,e1,e2))
    = Letrecfn (tx,ty,swap (n+2) e1,swap (n+1) e2)
                                      


(* the main one-step reduction function. This is for the call-by-value
semantics. Note that the code only makes sense for closed expressions. *)

fun reduce (Integer n,s) = NONE
  | reduce (Boolean b,s) = NONE
  | reduce (Op (e1,opr,e2),s) = 
    (case (e1,opr,e2) of
         (Integer n1, Plus, Integer n2) => SOME(Integer (n1+n2), s)   (*op + *)
       | (Integer n1, GTEQ, Integer n2) => SOME(Boolean (n1 >= n2), s)(*op >=*)
       | (e1,opr,e2) =>                                                
         if (is_value e1) then                                         
             case reduce (e2,s) of                                     
                 SOME (e2',s') => SOME (Op(e1,opr,e2'),s')     (* (op2) *)
               | NONE => NONE                                  
         else                                                  
             case reduce (e1,s) of                             
                 SOME (e1',s') => SOME(Op(e1',opr,e2),s')      (* (op1) *)
               | NONE => NONE )                                
  | reduce (If (e1,e2,e3),s) =                                  
    (case e1 of                                                 
         Boolean true => SOME (e2,s)                           (* (if1) *)
       | Boolean false => SOME (e3,s)                          (* (if2) *)
       | _ => (case reduce (e1,s) of                           
                   SOME (e1',s') => SOME(If(e1',e2,e3), s')    (* (if3) *)
                 | NONE => NONE  ) )                          
  | reduce (Deref l,s) = 
    (case lookup  (s,l) of                
          SOME n => SOME(Integer n,s)                          (* (deref) *)
        | NONE => NONE )                  
  | reduce (Assign (l,e),s) =                                  
    (case e of                                                 
         Integer n => (case update (s,(l,n)) of 
                           SOME s' => SOME(Skip, s')           (* (assign1) *)
                         | NONE => NONE)                                   
       | _ => (case reduce (e,s) of                           
                   SOME (e',s') => SOME(Assign (l,e'), s')     (* (assign2) *)
                 | NONE => NONE  ) )                          
  | reduce (Skip,s) = NONE                                     
  | reduce (Seq (e1,e2),s) =                                   
    (case e1 of                                                 
         Skip => SOME(e2,s)                                    (* (seq1) *)
       | _ => ( case reduce (e1,s) of                           
                    SOME (e1',s') => SOME(Seq (e1',e2), s')    (* (seq2) *)
                  | NONE => NONE ))                                         
  | reduce (While (e1,e2),s) = SOME( If(e1,Seq(e2,While(e1,e2)),Skip),s) 
                                                               (* (while) *)
  | reduce (Var n,s) = raise (Reduce "bogus unbound Var")
  | reduce (Fn (t,e),s) = NONE
  | reduce (App (e1,e2),s) = 
    (case e1 of
         Fn (t,e) =>  
         (if (is_value e2) then 
              SOME (subst e2 0 e,s)                            (* (fn) *)
          else
              (case reduce (e2,s) of 
                   SOME(e2',s') => SOME(App (e1,e2'),s')       (* (app2) *)
                 | NONE => NONE))
       | _ => (case reduce (e1,s) of
                   SOME (e1',s') => SOME(App (e1',e2), s')     (* (app1) *)  
                 | NONE => NONE ))
  | reduce (Let (t,e1,e2),s) = 
    (if is_value e1 then 
         SOME (subst e1 0 e2,s)                                (* (let2) *)
     else
         (case reduce (e1,s) of
              SOME(e1',s') => SOME(Let (t,e1',e2),s')          (* (let1) *)
            | NONE => NONE))
  | reduce (Letrecfn(tx,ty,e1,e2),s) = 

    (* First, construct the representation of 

         (fn y:T1=> let val rec x:T1->T2 = (fn y:T1=>e1) in e1 end),

    adjusting De Bruijn indices to reflect the
    fact that the two copies of e1 are under different binding
    contexts than they were in the original 

         let val rec x:T1->T2 = (fn y:T1=>e1) in e2 end. 

    Then, substitute that term for x in e2. *)

    let val e = Fn(tx, Letrecfn(tx,ty,shift 2 e1,swap 0 e1)) in
        SOME(subst e 0 e2,s)                                   (* (letrecfn) *)
    end



(* now define the many-step evaluation function

     evaluate :  expr * store -> (expr * store) option 

which takes a configuration (e,s) and returns the unique (e',s')
such that   (e,s) -->* (e',s') -/->.  *)


fun evaluate (e,s) = case reduce (e,s) of 
                         NONE => (e,s)
                       | SOME (e',s') => evaluate (e',s')



(* ****************)
(* type inference *)
(* ****************)


(* Here we express the type inference algorithm over the up-to-alpha
syntax, not over the raw syntax.  Could do either, really, but
up-to-alpha has a clearer relation to the mathematical definition of
the type system. *)

type typeEnv = (loc*type_loc) list  * type_expr list


(* in the semantics, type environments gamma are partial functions
from locations to the singleton set {intref} and from variables to
expression types. Here, just as we did for stores, we represent the
first as a list of loc*type_loc pairs containing, for each l in the
domain of the type environment, exactly one element of the form
(l,intref). We represent the second as a list of types for the binders
from this point outwards. *)



(* again, we depend on a uniqueness property, without which we would
have to have infertype return a type_expr list of all the possible
types *)

fun nth [] m = NONE
  | nth (y::ys) m = if m=0 then SOME y else nth ys (m-1)
                                              
(* infertype : typeEnv -> expr -> type_expr option *)

fun infertype gamma (Integer n) = SOME int
  | infertype gamma (Boolean b) = SOME bool
  | infertype gamma (Op (e1,opr,e2)) 
    = (case (infertype gamma e1, opr, infertype gamma e2) of
          (SOME int, Plus, SOME int) => SOME int
        | (SOME int, GTEQ, SOME int) => SOME bool
        | _ => NONE)
  | infertype gamma (If (e1,e2,e3)) 
    = (case (infertype gamma e1, infertype gamma e2, infertype gamma e3) of
          (SOME bool, SOME t2, SOME t3) => 
          if t2=t3 then SOME t2 else NONE
        | _ => NONE)
  | infertype gamma (Deref l) 
    = (case lookup (#1 gamma, l) of
          SOME intref => SOME int
        | NONE => NONE)
  | infertype gamma (Assign (l,e)) 
    = (case (lookup (#1 gamma,l), infertype gamma e) of
          (SOME intref,SOME int) => SOME unit
        | _ => NONE)
  | infertype gamma (Skip) = SOME unit
  | infertype gamma (Seq (e1,e2))  
    = (case (infertype gamma e1, infertype gamma e2) of
           (SOME unit, SOME t2) => SOME t2
         | _ => NONE )
  | infertype gamma (While (e1,e2)) 
    = (case (infertype gamma e1, infertype gamma e2) of
           (SOME bool, SOME unit) => SOME unit 
      | _ => NONE)
  | infertype gamma (Var n) = nth (#2 gamma) n 
  | infertype gamma (Fn (t,e)) 
    = (case infertype (#1 gamma, t::(#2 gamma)) e of
           SOME t' => SOME (func(t,t') )
         | NONE => NONE )
  | infertype gamma (App (e1,e2)) 
    = (case (infertype gamma e1, infertype gamma e2) of
           (SOME (func(t1,t1')), SOME t2) => if t1=t2 then SOME t1' else NONE
         | _ => NONE )
  | infertype gamma (Let (t,e1,e2)) 
    = (case (infertype gamma e1, infertype (#1 gamma, t::(#2 gamma)) e2) of
           (SOME t1,SOME t') => if t1=t then SOME t' else NONE
         | _ => NONE)
  | infertype gamma (Letrecfn (t12,t1,e1,e2)) 
    = (case (infertype (#1 gamma, t1::t12::(#2 gamma)) e1, infertype (#1 gamma, t12::(#2 gamma)) e2) of
           (SOME t2,SOME t) => if t12=func(t1,t2) then SOME t else NONE
         | _ => NONE)
;

