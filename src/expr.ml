(*
 * Vers une nouvelle méthode générale de représenter des quantités abstraites
 *)

let rec list_uniq l =
  match l with
  | [] -> []
  | hd :: tl -> if List.mem hd tl then list_uniq tl else hd :: list_uniq tl

module SM = Map.Make(struct
                       type t = string
                       let compare = String.compare
                     end)

type 'a map = 'a SM.t;;

type 'a expr = ('a map -> 'a);;

type 'a measure = ('a expr -> int expr);;

(* typically 'a should be int or float I guess ? *)
type symb_calc =
  | Impasse of float expr (* we don't know what we have, we can just eval it *)
  | Cons of float
  | Id of string (* so what 'a do we create with this constructor ? *)
  | Sum of symb_calc * symb_calc
  | Prod of symb_calc * symb_calc
  (* now some functions. Problem if they exist only for a certain 'a ? *)
  | Cos of symb_calc
  | Sin of symb_calc
  | Log of symb_calc
  | Exp of symb_calc
  | Pow of symb_calc * float

type comparison = Lesser | Equal | Greater

let comp_to_int c = 
  match c with
  | Lesser -> -1
  | Equal -> 0
  | Greater -> 1

let compare_int n1 n2 =
  match n1 < n2 with
  | true -> 
      Lesser
  | false -> 
      (match n1 > n2 with
       | true -> Greater
       | false -> Equal)

(* normalizing order *)
let order_score sc =
  match sc with
  | Impasse _ -> 0
  | Cons _ -> 1
  | Id _ -> 2
  | Cos _ -> 3
  | Sin _ -> 4
  | Log _ -> 5
  | Exp _ -> 6
  | Pow _ -> 7
  | Prod _ -> 8
  | Sum _ -> 9

let compare_sc sc1 sc2 =
  compare_int (order_score sc1) (order_score sc2)

let map_of_assoc_list l =
  let m_r = ref (SM.empty) in
  List.iter (fun (k, v) -> (m_r := SM.add k v !m_r)) l ;
  !m_r

module type ORDERED =
  sig
    type t
    val compare : t -> t -> comparison
  end

(*
module type SET =
  sig
    type t
    type set
    val union : set -> set -> set
    val inter : set -> set -> set
    val cross : set -> set -> set
  end

module SetMake (Ord : ORDERED) : SET =
  struct
    type t = Ord.t
    (* Maybe start by building a native set and build on that ? *)
    type set =
      | Empty
      | Finite of t list
      | Interv of (t * t)
      | All
      | Cross of set list
    
    let rec union e1 e2 =
      match e1, e2 with
      | (Cross _, Finite _ | Cross _, Interv _ | Cross _, Empty | Cross _, All)
       -> failwith "not the same dimension"
      | (Finite _, Cross _ | Interv _, Cross _ | Empty, Cross _ | All, Cross _)
       -> failwith "not the same dimension"
      | Empty, _ -> 
          e2
      | _, Empty -> 
          e1
      | (All, _ | _, All) -> 
          All
      | Finite l1, Finite l2 -> 
          Finite (list_uniq (l1 @ l2))
      | Interv (x1, x2), Interv (x3, x4) ->
          (* FIXME what do we do if the intervals are disjoint ? *)
          let y1 = min (min x1 x2) (min x3 x4) in
          let y2 = max (max x1 x2) (max x3 x4) in
          Interv (y1, y2)
      | (Finite l, Interv (x1, x2) | Interv (x1, x2), Finite l) ->
          (* c'est l'intersection ça non ? *)
          let f x =
            if Ord.compare x1 x2 = Equal then
              if Ord.compare x1 x = Equal
              then true else false
            else if Ord.compare x1 x2 = Lesser then
              if (Ord.compare x1 x = Lesser && Ord.compare x x2 = Greater)
              then true else false
            else
              if (Ord.compare x1 x = Greater && Ord.compare x x2 = Lesser)
              then true else false
           in Finite (List.filter f l)
      | Cross l1, Cross l2 -> Cross (List.map2 union l1 l2)
    
    let inter e1 e2 =
      match e1, e2 with
      | (Cross _, Finite _ | Cross _, Interv _ | Cross _, Empty | Cross _, All)
       -> failwith "not the same dimension"
      | (Finite _, Cross _ | Interv _, Cross _ | Empty, Cross _ | All, Cross _)
       -> failwith "not the same dimension"
      | (Empty, _ | _, Empty) -> 
          Empty
      | (All, e3 | e3, All) -> 
          e3
      | Finite l1, Finite l2 -> 
          Finite (List.filter 
                 (fun x -> List.exists (fun y -> Ord.compare x y = Equal) l2)
                 l1)
      | Interv (x1, x2), Interv (x3, x4) ->
          
      | (Finite l, Interv (x1, x2) | Interv (x1, x2), Finite l) ->
          let f x =
            if Ord.compare x1 x2 = Equal then
              if Ord.compare x1 x = Equal
              then true else false
            else if Ord.compare x1 x2 = Lesser then
              if (Ord.compare x1 x = Lesser && Ord.compare x x2 = Greater)
              then true else false
            else
              if (Ord.compare x1 x = Greater && Ord.compare x x2 = Lesser)
              then true else false
           in Finite (List.filter f l)
      | Cross l1, Cross l2 -> Cross (List.map2 union l1 l2)
  end
*)

type symb_bool =
  | Is_equal of string * float
  | And of symb_bool * symb_bool
  | Or of symb_bool * symb_bool

let ( +| ) (e1 : int expr) (e2 : int expr) = 
  fun m -> e1 m + e2 m;;

let ( +|. ) (e1 : float expr) (e2 : float expr) = 
  fun m -> e1 m +. e2 m;;

let ( *| ) (e1 : int expr)  (e2 : int expr) = 
  fun m -> e1 m * e2 m;;

let ( *|. ) (e1 : float expr)  (e2 : float expr) = 
  fun m -> e1 m *. e2 m;;

let id (x : string) = fun m -> SM.find x m;;

let consf (lambd : float) = fun m -> lambd;;

let consn (lambd : int) = fun m -> lambd;; 
(* the counting measure = sum of all these. How do we represent it ? *)
(* most elegant would be a clever function "infinite sum". *)



let dirac (e1 : 'a expr) (e2 : 'a expr) =
  fun m -> if e1 m = e2 m then 1 else 0;; 
  (* est-ce ce qu'on veut ? well yeah they're equal or not depending on m 
   * it's just that in some cases it's the constant one (typically dirac e1 e1).
   * do we need a kind of symbolic evaluation of expressions, that would be
   * able to handle correctly id, consf, consn and dirac ? *)

let ( +~ ) (nu1 : 'a measure) (nu2 : 'a measure) =
  fun e -> nu1 e +| nu2 e;;

let rec eval sc = 
  match sc with
  | Impasse e -> e
  | Cons a -> (fun m -> a)
  | Id s -> id s
  (* careful, we're losing the polymorphism *)
  | Sum (sc1, sc2) -> eval sc1 +|. eval sc2
  | Prod (sc1, sc2) -> eval sc1 *|. eval sc2
  | Cos sc1 -> (fun m -> cos ((eval sc1) m))
  | Sin sc1 -> (fun m -> sin ((eval sc1) m))
  | Log sc1 -> (fun m -> log ((eval sc1) m))
  | Exp sc1 -> (fun m -> exp ((eval sc1) m))
  | Pow (sc1, x) -> (fun m -> ((eval sc1) m) ** x)

(* get the normal form of the expression (-> as a list) *)
(* Probably totally inefficient for simplifying expressions though ? *)
let rec normalize sc =
  match sc with
  | ( Impasse _ | Cons _ | Id _ | Cos _ | Sin _ | Log _ | Exp _ | Pow _ ) ->
      sc
  | Sum (Sum (sc1, sc2), Sum (sc3, sc4)) ->
      normalize (Sum (sc1, Sum (sc2, Sum (sc3, sc4))))
  | Prod (Prod (sc1, sc2), Prod (sc3, sc4)) ->
      normalize (Prod (sc1, Prod (sc2, Prod (sc3, sc4))))
  | Sum (sc1, sc2) ->
      (match compare_sc sc1 sc2 with
       | Lesser -> Sum (normalize sc1, normalize sc2)
       | Greater -> Sum (normalize sc2, normalize sc1)
       (* case 0 : the case Sum (Sum, Sum) has already been taken care of *)
       | Equal -> Sum (normalize sc1, normalize sc2))
  | Prod (sc1, sc2) ->
      (match compare_sc sc1 sc2 with
       | Lesser -> Prod (normalize sc1, normalize sc2)
       | Greater -> Prod (normalize sc2, normalize sc1)
       (* case 0 : the case Prod (Prod, Prod) has already been taken care of *)
       | Equal -> Prod (normalize sc1, normalize sc2))

(* TODO rewrite with the normalized form in mind. Make it exhaustive *)
let rec factorize sc =
  match sc with
  | Impasse _ -> failwith "Reached an impasse"
  | ( Cons _ | Id _ ) ->
      sc
  | Cos sc -> 
      Cos (factorize sc)
  | Sin sc -> 
      Sin (factorize sc)
  | Log sc -> 
      Log (factorize sc)
  | Exp sc -> 
      Exp (factorize sc)
  | Pow (sc, x) -> 
      Pow (factorize sc, x)
  (* non terminal product *)
  | Sum (Prod (sc1, sc2), sc3) -> (* sc3 is not a Prod so it's a Sum *)
      (match factorize sc3 with
       | Prod (sc4, sc5) when sc1 = sc4 ->
           factorize (Prod (sc1, Sum (sc2, sc5)))
       | Prod (sc4, sc5) when sc1 = sc5 ->
           factorize (Prod (sc1, Sum (sc2, sc4)))
       | Prod (sc4, sc5) when sc2 = sc4 ->
           factorize (Prod (sc2, Sum (sc1, sc5)))
       | Prod (sc4, sc5) when sc2 = sc5 ->
           factorize (Prod (sc2, Sum (sc1, sc4)))
       (* trouver un moyen pour propager vers le haut correctement *)
       | Sum _ ->
           sc)
  | Sum (sc1, sc2) -> 
      Sum (factorize sc1, factorize sc2)
  | Prod (sc1, sc2) -> 
      Prod (factorize sc1, factorize sc2)

(* TODO rewrite with the normalized form in mind. Make it exhaustive *)
let rec distribute sc =
  match sc with
  | Impasse _ -> failwith "Reached an impasse"
  | ( Cons _ | Id _ ) -> sc
  | Cos sc -> 
      Cos (distribute sc)
  | Sin sc -> 
      Sin (distribute sc)
  | Log sc -> 
      Log (distribute sc)
  | Exp sc -> 
      Exp (distribute sc)
  | Pow (sc, x) -> 
      Pow (distribute sc, x)
  | Prod (sc1, Sum (sc2, sc3)) ->
      distribute (Sum (Prod (sc1, sc2), Prod (sc1, sc3)))
  | Sum (sc1, sc2) -> 
      Sum (distribute sc1, distribute sc2)
  | Prod (sc1, sc2) -> 
      Prod (distribute sc1, distribute sc2)

(* apply to a normalized expression *)
let rec reduce sc =
  match sc with
  | (Id _ | Cons _) ->
      sc
  | Sin sc ->
      Sin (reduce sc)
  | Cos sc ->
      Cos (reduce sc)
  | Exp sc ->
      Exp (reduce sc)
  | Log sc ->
      Log (reduce sc)
  | Pow (sc, x) -> 
      Pow (reduce sc, x)
  | Sum (Cons 0., x) | Sum (x, Cons 0.) -> 
      reduce x
  | Sum (Log sc1, Sum (Log sc2, sc3)) -> 
      reduce (Sum (Log (Prod (sc1, sc2)), sc3))
  | Prod (Cons 0., _) | Prod (_, Cons 0.) -> 
      Cons 0.
  | Prod (Cons 1., x) | Prod (x, Cons 1.) -> 
      reduce x
  | Prod (Exp sc1, Exp sc2) -> 
      Exp (Sum (sc1, sc2))
  | Sum (Cons x, sc2) ->
      (match sc2 with
       | Cons y ->
          Cons (x +. y)
       | Sum (Cons y, sc3) ->
          reduce (Sum (Cons (x +. y), sc3)))
  | Prod (Cons x, sc2) ->
      (match sc2 with
       | Cons y ->
          Cons (x *. y)
       | Prod (Cons y, sc3) ->
          reduce (Prod (Cons (x *. y), sc3)))
  | Sum (sc1, sc2) -> 
      Sum (reduce sc1, reduce sc2)
  | Prod (sc1, sc2) -> 
      Prod (reduce sc1, reduce sc2)
  | sc -> sc

let rec keep_reducing sc =
  if reduce sc = sc then sc else keep_reducing (reduce sc)

(* We assume integer values between 0 and +inf *)
(*
let minimize sc =
  let sc = 
*)

let rec repr sc =
  match sc with
  | Impasse e -> failwith "Reached an impasse"
  | Cons x -> string_of_float x
  | Id s -> s
  | Sum (sc1, sc2) -> repr sc1 ^ " +. " ^ repr sc2
  | Prod (sc1, sc2) -> repr sc1 ^ " *. " ^ repr sc2
  | Cos sc1 -> "cos (" ^ repr sc1 ^ ")"
  | Sin sc1 -> "sin (" ^ repr sc1 ^ ")"
  | Log sc1 -> "log (" ^ repr sc1 ^ ")"
  | Exp sc1 -> "exp (" ^ repr sc1 ^ ")"
  | Pow (sc1, x) -> repr sc1 ^ " ** " ^ string_of_float x

let deriv s sc =
  let rec d sc =
    match sc with
    | Impasse e -> failwith "Reached an impasse"
    | Cons _ -> Cons 0.
    | Id s1 -> if s = s1 then Cons 1. else Cons 0.
    | Sum (sc1, sc2) -> Sum (d sc1, d sc2)
    | Prod (sc1, sc2) -> Sum (Prod (d sc1, sc2), Prod (sc1, d sc2))
    | Cos sc1 -> Prod (d sc1, Prod (Cons ~-.1.,  Sin sc1))
    | Sin sc1 -> Prod (d sc1, Cos sc1)
    | Log sc1 -> Prod (d sc1, Pow (sc1, ~-. 1.))
    | Exp sc1 -> Prod (d sc1, Exp sc1)
    | Pow (sc1, x) -> Prod (d sc1, Pow (sc1, x -. 1.))
  in d sc

let grad l sc =
      List.map (fun s -> deriv s sc) l 

let sum_l sc_l =
  List.fold_left
  (fun sc1 -> fun sc2 -> Sum (sc1, sc2))
  (Cons 0.) sc_l

let sum_a sc_a =
  Array.fold_left
  (fun sc1 -> fun sc2 -> Sum (sc1, sc2))
  (Cons 0.) sc_a

let dot sc_l1 sc_l2 =
  List.fold_left2 
  (fun sc1 -> fun sc2 -> fun sc3 -> Sum (sc1, Prod (sc2, sc3)))
  (Cons 0.) sc_l1 sc_l2

let ( +^. ) sc1 sc2 = Sum (sc1, sc2)
let ( *^. ) sc1 sc2 = Prod (sc1, sc2)

(* test it *)

let m = SM.singleton "x" 1.;;
let m = SM.add "y" 1. m;;
let fmap = SM.add "z" 2. m;;

let x = id "x";;
let y = id "y";;
let dx = dirac x;;
let dy = dirac y;;
let mu = dx +~ dy;;

let f = Sum (Pow (Id "x", 1. /. 2.), Sum (Cos (Prod (Cons 2., Id "y")), Id "z"));;
