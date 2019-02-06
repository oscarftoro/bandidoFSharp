namespace ROBDD

open System

module Types = 

   type UId0 = Zero 
             | One 
   and  UId  = U of int
   and  V    = int
   and  Low  = UId
   and  High = UId
   and  Inf  = INF of struct (V * Low * High)    (* to the glory *)
             | INF0 of struct (V * UId0 * UId0) (* to initialization *)
   
  
   and  T    = Map<UId,Inf>
   and  H    = Map<Inf,UId>

   //boolean expression with variables
   type BVarExpr =   And  of BVarExpr * BVarExpr
                   | Or   of BVarExpr * BVarExpr
                   | Then of BVarExpr * BVarExpr
                   | Iff  of BVarExpr * BVarExpr
                   | Not  of BVarExpr
                   | X    of int
                   | B    of bool

   //a meta type for BVarExpr THIS CAN BE A POSSIBILITY to check the max X
   type BVE = { maxX : int ; bve: BVarExpr }

  //when an expression is shannon expanded is a boolean expression
   type BExpr =   Conj  of BExpr * BExpr
                | Disj of BExpr * BExpr 
                | Imp  of BExpr * BExpr 
                | BImp of BExpr * BExpr 
                | Neg  of BExpr
                | Bl   of bool
   let mkU i : UId = U i
   let uid02i uid = if (uid = Zero) then U 0 else U 1
   let uid2bool (U u1) (U u2 ) : bool * bool = 
     let i2b = fun i -> (i <> 0) in
     (i2b u1, i2b u2)
   let b2uid = fun b -> if b then U 1 else U 0 
   let bool2int (b1: bool) (b2: bool) =
    (b2uid b1, b2uid b2)

module T =
  open Types
  //T : Map<U,Inf>
  let init(i: int) (t: T) : T = 
    let t0 : T = Map.add (U 0) (INF0 struct(i,Zero,Zero)) t in
    let t1 : T = Map.add (U 1) (INF0 struct(i,One,One)) t0 in
    t1

  let add (inf : Inf) (t: T) = 
    //precondition: t is never empty. ALLWAYS CALL init :  int -> T -> T before calling ADD  
    let (U u) = (Map.toArray t) |> Array.map(fun (k,_v) -> k) 
              |> Array.max in //last entered u 
    (U (u+1), Map.add (U (u+1)) inf t)  

  let v (u: UId) (m: T) = match Map.find u m with
                          | (INF struct (v,_,_))  -> v 
                          | (INF0 struct (v,_,_)) -> v

  let low (u: UId) (m : T) = match Map.find u m with
                             | (INF struct (_,lw,_))  -> lw 
                             | (INF0 struct (_,lw,_)) -> uid02i lw

  let high (u: UId) (m: T) = match Map.find u m with
                             | (INF struct (_,_,hg)) -> hg
                             | (INF0 struct (_,_,hg)) -> uid02i hg
   
  // wouln't be cool to have a function t2h s.t. given a t it returns an H?
  //the problem with H is that the two first values: 0 and 1 have the same value
  let t2h t = Map.foldBack (fun k v acc -> (v, k) :: acc) t []  |> Map.ofList


module H = 
  // H : (i,l,h) := U
  open Types
  let initEmpty : H = Map.empty<Inf,UId>  

  let isMember (inf: Inf) (h: H) : bool = Map.exists (fun inf' _ -> inf = inf') h

  let lookup (inf: Inf) (h : H)  = Map.tryFind inf h 

  let insert (inf: Inf) (u: UId) (h : H) : H = Map.add inf u h 

  let init i h : H = insert (INF0 struct (i,Zero,Zero)) (U 0) h
                  |> insert (INF0 struct (i, One, One)) (U 1)
    //since for values 0 and 1 the key is identicall we only save the first one: 0
    //we could add 0 and 1 at the end such that i = 5 becomes 50 and 51 
    

module BDD =
  open Types
  open T
  open H 

  let mkAnd be0 be1 =  And(be0, be1)
  let mfOr be0 be1 = Or(be0, be1)
  let mkThen be0 be1 = Then(be0, be1)
  let mkIff be0 be1 = Iff(be0, be1)
  let mkNot be0 = Not be0
  let mkBVE be0 = 
   //check the biggest x in be0
   // construct the type
   B true

  let mkTH = (Map.empty<UId, Inf>, Map.empty<Inf, UId>)  
     
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
  let rec bve2be = function
    | And(be0,be1)    -> Conj(bve2be be0, bve2be be1)
    | Or(be0,be1)     -> Disj(bve2be be0, bve2be be1)
    | Then(be0,be1)   -> Imp(bve2be be0, bve2be be1)
    | Iff(be0,be1)    -> BImp(bve2be be0, bve2be be1)
    | Not(be0)        -> Neg(bve2be be0)
    | B b0            -> Bl b0
    | X _             -> failwith "we are not expecting variables x when converting to Boolean Expressions"
              
  let rec eval = function
    | Conj(be0, be1) -> eval be0 && eval be1
    | Disj(be0, be1) -> eval be0 || eval be1
    | Imp(be0, be1)  -> (eval (Neg(be0)) ) || eval be1 
    | BImp(be0, be1) -> (eval (Imp(be0, be1)) ) && (eval (Imp(be1, be0)) )
    | Neg(be0)       -> not (eval be0)
    | Bl b           -> b


  let mk (inf) (t:T) (h:H) : UId * T * H =
    match inf with
    | INF struct (i,lw,hg) -> 
        if lw = hg then (lw, t, h) 
        else if (H.isMember (INF struct (i,lw,hg)) h) then
         let u0 = (H.lookup (INF struct (i,lw,hg)) h) in
         let u1 = Option.get u0
         
         (u1, t, h)
        else 
         let (u,newT) = T.add (INF struct (i,lw,hg)) t  in 
         
         //printfn "u: %A" u
         let newH     = H.insert (INF struct (i,lw,hg)) u h in 
         (u, newT, newH)
    | INF0 struct (i, Zero, Zero) -> (U 0, t, h)
    | INF0 struct (i, One, One)   -> (U 1, t, h)
    | INF0 (_) -> failwith "error: INF0 has an insolit structure"

  let build (bve: BVarExpr) (n: int)  =

    let  (t0,h0) : (T * H) = (Map.ofList [], Map.ofList []) in
    let mutable (t,h) = (T.init n t0, H.init n h0)
    let rec build' (bve0 : BVarExpr) (n0) (U i0)  =
      if i0 > n0 then 
        //printfn "variable i : %i" i0
       // printfn "variable bve : %A" bve0
        let b : bool = (bve2be >> eval ) bve0 
        //printfn "variable b: %A" b
        if (not b) then (U 0, t, h) else (U 1, t, h)
      else 
        let (u0,_,_) = build' (expand bve0 i0 false) n0 (U (i0+1) ) 
        let (u1,_,_) = build' (expand bve0 i0 true ) n0 (U (i0+1) ) 
        
        let (v,t0,h0) =  mk (INF struct (i0, u0, u1)) t h
        t  <- t0
        h  <- h0
        (v, t, h)
    build' bve n (U 1) 

  //apply uses dynamic programming to apply a boolean operation 
  //between two BDD's nodes
  //precondition: tables t and h are not empty
  let apply (op: bool -> bool -> BExpr) u1 u2 t h =
   let mutable g : Map<UId*UId,UId> = Map.ofList [] in
   //let  (t00,h00) : (T * H) = Map.ofList [], Map.ofList [] in
   //let (t0,h0) : (T * H) = T.init t0, H.init h0 //given a t init taking the last largest u in table T

   let rec app u1 u2 t0 h0 =
     if Map.containsKey (u1,u2) g then (Map.find (u1, u2) g, t0, h0)
     else if (List.contains u1 [U 0;U 1] && List.contains u2 [U 0;U 1]) 
     then 
       let (b1,b2) = uid2bool u1 u2 in 
       let u = eval (op b1 b2)  |> b2uid in
       (u, t0, h0)
     else if T.v (u1) t0 = T.v(u2) t0 then
       let (low,t1,h1)  = app (T.low u1 t0) (T.low u2 t0) t0 h0 
       let (high,t2,h2) = app (T.high u1 t0) (T.high u2 t0) t1 h1
       
       let (u,t3,h3) = mk (INF struct (T.v u1 t0, low, high)) t2 h2  in
       g <- Map.add (u1, u2) u g 
       
       (u, t3, h3)
     else if T.v(u1) t0 < T.v(u2) t0 then
       let (low,t1,h1)  = app (T.low u1 t0) u2 t0 h0 
       let (high,t2,h2) = app (T.high u1 t0) u2 t1 h1
       
       let (u,t3,h3) = mk(INF struct (T.v u1 t0, low,  high)) t2 h2 in
       g <- Map.add (u1, u2) u g 
       
       (u, t3, h3)
     else 
       let (low,t1,h1)  = app u1  (T.low u2 t0) t0 h0 
       let (high,t2,h2) = app u1  (T.high u2 t0) t1 h1
       
       let (u,t3,h3) = mk (INF struct (T.v u2 t0,  low,  high)) t2 h2 in
       g <- Map.add (u1, u2) u g 
       
       (u, t3, h3)

   app u1 u2 t h

 
  //computing restriction to a function is straightforward
  //according to Randal E. Bryant; I desagree

  //restrict with an exponential running time
  //singleton boolean assignment($[b/x_j], b \in {0,1} )
  // it can be improved using dynamic programming

  let restrict3 u j b t = 
    //precondition: b is either 1 or 0
    let (U maxVar) = u
    let tRes = Map.ofList [] |> T.init maxVar // give me a T to build a result tr table,
    let hRes = T.t2h tRes   //an H to pass to mk called hRes

    let rec res u0 t0 h0  =
      // updates table t1 and h1 with value u0 from table t
      let update t1 h1 u0 = 
        let tr = Map.add u0 (Map.find u0 t) t1    in
        let hr = Map.add (Map.find u0 t) u0 h1    in
        (tr, hr)
      
      match T.v u0 t with
      | i when i > j -> (u0, t0, h0)
      | i when i < j ->
          let (low,t1,h1)  = res (T.low u0 t)  t0 h0   in
          let t15,h15 = update t1 h1 low

          let (high,t2,h2) = res (T.high u0 t) t15 h15 in 
          let t25, h25 = update t2 h2 high
       
          mk(INF struct(T.v u0 t,low,high )) t25 h25
        
      | _ -> match b with
             | 0 -> res (T.low u0 t) t0 h0
             | 1 -> res (T.high u0 t) t0 h0
             | _ -> failwith ("this should never happen, b is setted to something else than 0 or 1")

    res u tRes hRes 

  // restrict improved using dynamic programming
  let restrict4 u j b t = 
    //precondition: b is either 1 or 0
    let mutable g = Map.ofList [] 
    let (U maxVar) = u
    let tRes = Map.ofList [] |> T.init maxVar // give me a T to build a result tr table,
    let hRes = T.t2h tRes   //an H to pass to mk called hRes

    let rec res u0 t0 h0  =
      // updates table t1 and h1 with value u0 from table t
      let update t1 h1 u0 = 
        let tr = Map.add u0 (Map.find u0 t) t1    in
        let hr = Map.add (Map.find u0 t) u0 h1    in
        (tr, hr)
      let u0Val = T.v u0 t
      match u0Val with
      | i when i > j -> (u0, t0, h0)
      | i when i < j ->
          if Map.containsKey u0 g then (Map.find u0 g, t0, h0)
          else 
            let (low,t1,h1)  = res (T.low u0 t)  t0 h0   in
            let t15,h15 = update t1 h1 low

            let (high,t2,h2) = res (T.high u0 t) t15 h15 in 
            let t25, h25 = update t2 h2 high
            
            let (uRes, tRes, hRes) = mk(INF struct(T.v u0 t,low,high )) t25 h25
            g <- Map.add u0 uRes g
            (uRes, tRes, hRes)
        
      | _ -> match b with
             | 0 -> res (T.low u0 t) t0 h0
             | 1 -> res (T.high u0 t) t0 h0
             | _ -> failwith ("this should never happen, b is setted to something else than 0 or 1")

    res u tRes hRes 

  let satCount u t = 
    
    let rec count u =
      let (U i) = u in 
      match i with 
      | 0 -> 0
      | 1 -> 1
      | _ -> 
          let low = T.low u t in
          let high = T.high u t in 
          let plow =  (pown 2 ((T.v (low) t) - (T.v u t) - 1))  in
          let phigh = (pown 2 ((T.v (high) t) - (T.v u t) - 1)) in
          plow  * (count low)  + phigh * (count high) 

    let partRes = pown 2 ((T.v u t) - 1) in 
    partRes * count u


  let hello name =
    printfn "Hello %s" name


module Plot =
  open Types
 
  let tEntry2dotLine (u)  (inf)  (dot: string) =
    match inf with
    | (INF struct (i,olw,ohg )) ->
        match u with 
        | U u1 -> 
            let U lw ,U hg = (olw , ohg)

            let labels = dot    + sprintf " %i [label=<X<SUB>%i</SUB>>,shape=circle, xlabel=%i] " u1 i u1  in
            let lablow = labels + sprintf " %i -- %i [style=dashed]" u1 lw in
            lablow + sprintf " %i -- %i " u1 hg
    | (INF0 struct (i,Zero,Zero )) -> dot + "graph { 1 [shape=box] 0 [shape=box] "
    | (INF0 struct (i,One,One ))   -> dot
    | (INF0 _)                     -> 
        failwith "something went wrong in tEntry2dotLine INF0 struct tuple is irrepresentable"
      
           
  let t2dot (t:T) : string =
    //iterate the map
    let arr = Map.toArray t |> Array.sortBy (fun (U k, _v) -> -k)
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