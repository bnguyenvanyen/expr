
(** String map of 'a *)
type 'a map

(** Concrete expression returning 'a when evaluated *)
type 'a expr = ('a map -> 'a)

(** Integer measures *)
type 'a measure = ('a expr -> int expr)

(** Symbolic expression. Don't hesitate to add your own functions appropriately *)
type symb_calc =
  | Impasse of float expr (** A concrete expression we know nothing about *)
  | Cons of float (** A constant value *)
  | Id of string (** The identity function *)
  | Sum of symb_calc * symb_calc (** Sum *)
  | Prod of symb_calc * symb_calc (** Product *)
  | Cos of symb_calc (** Cosine *)
  | Sin of symb_calc (** Sine *)
  | Log of symb_calc (** Natural logarithm *)
  | Exp of symb_calc (** Natural exponential *)
  | Pow of symb_calc * float (** Exponentiation *)

(** Create a map from an association list *)
val map_of_assoc_list : (string * 'a) list -> 'a map

(** Sum of int expr *)
val ( +| ) : int expr -> int expr -> int expr

(** Sum of float expr *)
val ( +|. ) : float expr -> float expr -> float expr

(** Product of int expr *)
val ( *| ) : int expr -> int expr -> int expr

(** Product of float expr *)
val ( *|. ) : float expr -> float expr -> float expr

(** Identity function *)
val id : string -> 'a map -> 'a

(** (Float) constant function *)
val consf : float -> 'a map -> float

(** (Integer) constant function *)
val consn : int -> 'a map -> int 

(** dirac e1 is the dirac measure in e1 *)
val dirac : 'a expr -> 'a expr -> 'a map -> int

(** Sum of (integer) measures *)
val ( +~ ) : 'a measure -> 'a measure -> 'a measure

(** Evaluate an symbolic expression to a function expression *)
val eval : symb_calc -> float expr

(** Factorize a symbolic expression *)
val factorize : symb_calc -> symb_calc

(** Distribute a symbolic expression *)
val distribute : symb_calc -> symb_calc

(** Reduce a symbolic expression. Get rid of 0., etc... *)
val reduce : symb_calc -> symb_calc

(** Reduce until nothing changes anymore. We're supposing that this actually 
  * happens... Unsafe *)
val keep_reducing : symb_calc -> symb_calc

(** String representation of a symbolic expression *)
val repr : symb_calc -> string

(** Derivate a symbolic expression *)
val deriv : string -> symb_calc -> symb_calc

(** Gradient of a symbolic expression in respect to a list of variables *)
val grad : string list -> symb_calc -> symb_calc list

(** Sum of a symbolic list *)
val sum_l : symb_calc list -> symb_calc

(** Sum of a symbolic array *)
val sum_a : symb_calc array -> symb_calc

(** Scalar product of two symbolic lists *)
val dot : symb_calc list -> symb_calc list -> symb_calc

(** Sum of symbolic expressions *)
val ( +^. ) : symb_calc -> symb_calc -> symb_calc

(** Product of symbolic expressions *)
val ( *^. ) : symb_calc -> symb_calc -> symb_calc
