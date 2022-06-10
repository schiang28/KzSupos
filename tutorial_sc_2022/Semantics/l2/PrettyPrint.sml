(* 2002-11-08 -- Time-stamp: <2003-01-06 18:30:33 pes20>    -*-SML-*- *)


(* ************************** *)
(* pretty-printing machinery  *)
(* ************************** *)

open Syntax;
open Semantics;



(* pretty print operators, types, and expressions *)

fun pp_op Plus = "+"
  | pp_op GTEQ = ">="

fun pp_type int = "int"
  | pp_type unit = "unit"
  | pp_type bool = "bool"
  | pp_type (func(t1,t2)) = "(" ^ (pp_type t1) ^ "->" ^ (pp_type t2)^")"

                         
fun pp_expr (Integer n) = Int.toString n
  | pp_expr (Boolean b) = Bool.toString b
  | pp_expr (Op (e1,opr,e2)) 
    = "(" ^ (pp_expr e1) ^ (pp_op opr) 
      ^ (pp_expr e2) ^ ")"
  | pp_expr (If (e1,e2,e3)) 
    = "(if " ^ (pp_expr e1 ) ^ " then " ^ (pp_expr e2)
      ^ " else " ^ (pp_expr e3) ^")"
  | pp_expr (Deref l) = "!" ^ l
  | pp_expr (Assign (l,e)) = "("^ l ^ ":=" ^ (pp_expr e )^")"
  | pp_expr (Skip) = "skip"
  | pp_expr (Seq (e1,e2)) = "("^ (pp_expr e1 ) ^ ";" ^ (pp_expr e2)^")"
  | pp_expr (While (e1,e2)) =  "(while " ^ (pp_expr e1 ) 
                               ^ " do " ^ (pp_expr e2) ^")"
  | pp_expr (Var n) = "var_" ^ Int.toString n
  | pp_expr (Fn (t,e)) =  "(fn . : " ^ (pp_type t ) ^ " => " ^ (pp_expr e) ^")"
                          
  | pp_expr (App (e1,e2)) =  "(" ^ (pp_expr e1 ) 
                             ^ " " ^ (pp_expr e2)^")"
  | pp_expr (Let (t,e1,e2)) =  "let val .:" ^ (pp_type t )  ^ "= " ^(pp_expr e1 ) 
                             ^ " in " ^ (pp_expr e2)^" end "
  | pp_expr (Letrecfn (t12,t1,e1,e2)) =  "let val rec .:" ^ (pp_type t12 )  ^ "= (fn .:" ^(pp_type t1 )  ^"=> " ^ (pp_expr e1 ) 
                             ^ ") in " ^ (pp_expr e2)^" end "


fun pp_fold f [] = ""
  | pp_fold f (x::[]) = f x
  | pp_fold f (x::xs) = (f x) ^ ", " ^ (pp_fold f xs)

(* pretty print stores, first sorting by location names for readability *)


fun pp_store_plain s = pp_fold (fn (l,n)=> l^"="^(Int.toString n)) s

fun pp_store pairs = 
    let val pairs' = Listsort.sort 
                         (fn ((l,n),(l',n')) => String.compare (l,l'))
                         pairs
    in
        pp_store_plain pairs'  
    end


(* pretty print configurations *)

fun pp_config (e,s) = "< " ^ (pp_expr e) 
                             ^ " , {" ^ (pp_store s) ^ "} >"


(* pretty print type environments *)

fun pp_typeenv_element_loc (l,intref) = l ^ ": intref"
fun pp_typeenv_element_var t = pp_type t

fun fst (x,y) = x
fun snd (x,y) = y

fun pp_typeenv tenv = "({"^  ( pp_fold pp_typeenv_element_loc (fst tenv))
                      ^"}, {" ^(pp_fold pp_typeenv_element_var (snd tenv))
                      ^"})"
                      

(* perform a reduction sequence, printing the initial state and the
   state after each reduction step *)

fun prettyreduce' (e,s) = 
    case reduce (e,s) of 
        SOME (e',s') => 
        ( TextIO.print ("\n -->  " ^ pp_config (e',s') ) ;
          prettyreduce' (e',s'))
      | NONE => (TextIO.print "\n -/->  " ; 
                 if is_value e then 
                     TextIO.print "(a value)\n" 
                 else 
                     TextIO.print "(stuck - not a value)\n" )


fun prettyreduce (e,s) = (TextIO.print ("Reductions:\n      "^(pp_config (e,s))) ;
                          prettyreduce' (e,s))



(* calculate the set of all locations occuring in an expression *)

fun merge xs []      = xs
  | merge xs (y::ys) = if List.exists (fn z=>y=z) xs then merge xs ys else y::(merge xs ys) 

fun locs (Integer n)      = []
  | locs (Boolean b)      = []
  | locs (Op (e1,opr,e2)) = merge (locs e1)  (locs e2) 
  | locs (If (e1,e2,e3))  = merge (locs e1)  (merge (locs e2) (locs e3))
  | locs (Deref l)        = [l]
  | locs (Assign (l,e))   = merge [l] (locs e)
  | locs (Skip)           = []
  | locs (Seq (e1,e2))    = merge (locs e1)  (locs e2) 
  | locs (While (e1,e2))  = merge (locs e1)  (locs e2) 
  | locs (Var n)          = []
  | locs (Fn (t,e))       = locs e
  | locs (App (e1,e2))    = merge (locs e1)  (locs e2) 
  | locs (Let (t,e1,e2))  = merge (locs e1)  (locs e2) 
  | locs (Letrecfn (t12,t1,e1,e2))    = merge (locs e1)  (locs e2) 


fun zero_store e = map (fn l => (l,0)) (Listsort.sort String.compare (locs e))

(* do type inference, printing the resulting type or an error *)

(* first, calculate an initial type environment from an initial store *)
fun initial_type_environment s = 
    let fun f [] = []
          | f ((l,n)::s) = (l,intref)::(f s) 
    in
        ( f s, [])
    end
        
fun prettytype e s = 
    let val tenv = initial_type_environment s in
        TextIO.print 
            ("Expression " 
             ^ (pp_expr e) 
             ^ (case (infertype tenv e) of 
                    NONE => " has no type wrt " ^ (pp_typeenv tenv )
                  | SOME t => " has type " ^ pp_type t 
                              ^" wrt " ^ (pp_typeenv tenv ))
             ^ "\n")
    end
