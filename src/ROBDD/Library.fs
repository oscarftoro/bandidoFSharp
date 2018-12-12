namespace ROBDD

open System

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
  //T : Map<U,Inf>
  let init(i: int) (t: T) : T = 
    let t0 : T = Map.add 0 (INF struct(i,None,None)) t in
    let t1 : T = Map.add 1 (INF struct(i,None,None)) t0 in
    t1

  let add (inf : Inf) (t: T) = 
    //precondition: t is never empty. ALLWAYS CALL init :  int -> T -> T before calling ADD  
    let u = (Map.toArray t) |> Array.map(fun (k,_v) -> k) 
            |> Array.max //last entered u 
    (u+1, Map.add (u+1) inf t)  

  let v (u: U) (m: T) = let (INF struct (v,_,_))  = Map.find u m in v

  let low (u: U) (m : T) = let (INF struct (_,lw,_)) = Map.find u m in lw

  let high (u: U) (m: T) = let (INF struct (_,_,hg)) = Map.find u m in hg


module H = 
  // H : (i,l,h) := U
  open Types
  let initEmpty : H = Map.empty<Inf,U>  

  let isMember (inf: Inf) (h: H) : bool = Map.exists (fun inf' _ -> inf = inf') h

  let lookup (inf: Inf) (h : H)  = Map.tryFind inf h 

  let insert (inf: Inf) (u: U) (h : H) : H = Map.add inf u h 

  let init i h : H = 
    let h0 = insert (INF struct (i,None,None)) 0 h
    //since for values 0 and 1 the key is identicall we only save the first one: 0
    //we could add 0 and 1 at the end such that i = 5 becomes 50 and 51 
    h0

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


   let mk (INF struct (i,lw,hg):Inf) (t:T) (h:H) =
     if lw = hg then (Option.get lw,t,h) 
     else if (H.isMember (INF struct (i,lw,hg)) h) then
       let u = Option.get (H.lookup (INF struct (i,lw,hg)) h) in
       (u,t,h)
     else 
       let (u,newT) = T.add (INF struct (i,lw,hg)) t  in 
       printfn "u: %A" u  //there is a bug here
       let newH     = H.insert (INF struct (i,lw,hg)) u h in 
       (u,newT,newH)


   let build (bve: BVarExpr) (n: int)  =
    
    let  (t0,h0) : (T * H) = Map.ofList [], Map.ofList [] in
    let mutable (t,h) = T.init n t0, H.init n h0 
    let rec build' (bve0 : BVarExpr) (n0: int) (i0: int)  =
      if i0 > n0 then 
        //printfn "variable i : %i" i0
       // printfn "variable bve : %A" bve0
        let b : bool = (bve2be >> eval ) bve0 
        //printfn "variable b: %A" b
        if (not b) then (0,t,h) else (1,t,h)
      else 
        let (u0,_,_) = build' (expand bve0 i0 false) n0 (i0+1) 
        let (u1,_,_) = build' (expand bve0 i0 true ) n0 (i0+1) 
        
        let (v,t0,h0) =  mk (INF struct (i0,Some u0,Some u1)) t h
        t  <- t0
        h  <- h0
        (v,t,h)
    build' bve n 1 
    

   let hello name =
      printfn "Hello %s" name


module Plot =
  open Types
 
  let tEntry2dotLine (u : int)  ((INF struct (i,olw,ohg )) as inf)  (dot: string) =
    match u with 
    | 0 -> dot + "graph { 1 [shape=box] 0 [shape=box] "
    | 1 -> dot
    | u1 -> 
      let lw , hg = Option.get olw , Option.get ohg

      let labels = dot    + sprintf " %i [label=<X<SUB>%i</SUB>>,shape=circle, xlabel=%i] " u1 i u1  in
      let lablow = labels + sprintf " %i -- %i [style=dashed]" u1 lw in
      lablow + sprintf " %i -- %i " u1 hg
      
           
      
 
  let t2dot (t:T) : string =
    //iterate the map
    let arr = Map.toArray t |> Array.sortBy (fun (k,_v) -> -k)
    let beg = Array.foldBack (fun (k, v) acc ->  (tEntry2dotLine k v acc) ) arr ""
    in beg + "}"
    //let result = Map.foldBack (fun k v acc ->  (tEntry2dotLine k v acc) ) t ""
    //translate entries into nodes and edges in dot*
    //result    
  
   

//how to execute command:
// System.Dianostics.Process.Start("cmd", "/c dir")

//define how the code should look like to *DONE
//draw a figure *DONE

//translate each k,v in T to nodes and edges in dot *DONE