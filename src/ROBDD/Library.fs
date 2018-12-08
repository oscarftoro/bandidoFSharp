
namespace ROBDD

module Types = 
   type Inf  = INF of struct ( int * Option<int> * Option<int>)
   type U    = int
   and  V    = int
   and  Low  = int
   and  High = int
   and  T    = Map<U,Inf>
   and  H    = Map<Inf,U>

   //boolean expression with variables
   type BVarExpr = | And  of BVarExpr * BVarExpr
                   | Or   of BVarExpr * BVarExpr
                   | Then of BVarExpr * BVarExpr
                   | Iff  of BVarExpr * BVarExpr
                   | Not  of BVarExpr
                   | X    of int
                   | B    of bool

   //a meta type for BVarExpr THIS CAN BE A POSSIBILITY to check the max X
   type BVE = { maxX : int ; bve: BVarExpr }

  //when an expression is shannon expanded is a boolean expression
   type BExpr = Conj  of BExpr * BExpr
               | Disj of BExpr * BExpr 
               | Imp  of BExpr * BExpr 
               | BImp of BExpr * BExpr 
               | Neg  of BExpr
               | Bl   of bool

module T =
  open Types
  let init(t: T) (i: int)= 
    let t0 = Map.add(0, struct(i,None,None)) t in
    let t1 = Map.add(1,struct(i,None,None)) t0 in
    t1

  let add (u: U) (inf : Inf) (m: T) = Map.add u inf m

  let v (u: U) (m: T) = let (INF struct (v,_,_))  = Map.find u m in v

  let low (u: U) (m : T) = let (INF struct (_,lw,_)) = Map.find u m in lw

  let high (u: U) (m: T) = let (INF struct (_,_,hg)) = Map.find u m in hg


module H = 
  // H : (I,l,h) := U
  open Types
  let init : H = Map.empty<Inf,U>  

  let isMember (inf: Inf) (h: H) : bool = Map.exists (fun inf' _ -> inf = inf') h

  let lookup (inf: Inf) (h : H)  = Map.find inf h 

  let add (inf: Inf) (v: U) (h : Map<Inf,U>) = Map.add inf v h 

module BDD =
   open Types
   open T
   open H 

   let mkAnd be0 be1 =  And(be0, be1)
   let mfOr be0 be1 = Or(be0,be1)
   let mkThen be0 be1 = Then(be0,be1)
   let mkIff be0 be1 = Iff(be0,be1)
   let mkNot be0 = Not be0
   let mkBVE be0 = 
     //check the biggest x in be0
     // construct the type
     B true

   let mkTH = Map.empty<U,Inf> , Map.empty<Inf,U>  
       
   //given a Boolean Expression be, a variable number x and a boolean b
   //bind x to b
   let rec expand (be: BVarExpr) (x: int) (b: bool) =
   //it should fail here when x > maxVar(be) or x < maxVar(be) 
        match be with
        | And(be0,be1)            -> And(expand be0 x b, expand be1 x b)
        | Or(be0,be1)             -> Or(expand be0 x b, expand be1 x b)
        | Then(be0,be1)           -> Then(expand be0 x b, expand be1 x b)
        | Iff(be0,be1)            -> Iff(expand be0 x b, expand be1 x b)
        | Not(be0)                -> Not(expand be0 x b)
        | B b0                    -> B b0
        | X e when e = x          -> B b
        | X e when e <> x         -> X e
   //   | X e when x > e || x < e -> failwith "variable number x not found! because is out of bound in expand"          
        | X ______                -> failwith "something went very wrong with x in expand. It is not out of bound but is bad!"
       
   let shannonExpand(be: BVarExpr) (x: int) = (expand be x true, expand be x false)
   
   //precondition: there is no variable number x in the BVarExpr
   // it transform to a Boolean Expression that can be evaluated with eval
   let rec bve2be = 
     function
     | And(be0,be1)    -> Conj(bve2be be0, bve2be be1)
     | Or(be0,be1)     -> Disj(bve2be be0, bve2be be1)
     | Then(be0,be1)   -> Imp(bve2be be0, bve2be be1)
     | Iff(be0,be1)    -> BImp(bve2be be0, bve2be be1)
     | Not(be0)        -> Neg(bve2be be0)
     | B b0            -> Bl b0
     | X _             -> failwith "we are not expecting variables x when converting to Boolean Expressions"
                
   let rec eval = 
     function
     | Conj(be0,be1) -> eval be0 && eval be1
     | Disj(be0,be1) -> eval be0 || eval be1
     | Imp(be0,be1)  -> (eval (Neg(be0)) ) || eval be1 
     | BImp(be0,be1) -> (eval (Imp(be0,be1)) ) && (eval (Imp(be1,be0)) )
     | Neg(be0)      -> not (eval be0)
     | Bl b          -> b


   let hello name =
      printfn "Hello %s" name
