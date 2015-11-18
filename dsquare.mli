type square =
  { x : int; y : int; size : int; }
type noise =
  { min : float; max : float; cur : float;
    attenuate: noise -> float }
type header =
  { seed : int; sqr : square;
    max_size : int; min_size : int;
    noise : noise }
type content =
  { sqr : square;
    pts :
      int * int * int * int *
      int * int * int * int *
      int * int * int * int *
      int * int * int * int;
  }

val create :
  seed:int -> max:int -> min:int -> noise ->
  (content, header) Tree.t


val move_upward :
  (content, header) Tree.t ->
  (content, header) Tree.t

val move_downward :
  (content, header) Tree.t ->
  (content, header) Tree.t

val move_leftward :
  (content, header) Tree.t ->
  (content, header) Tree.t

val move_rightward :
  (content, header) Tree.t ->
  (content, header) Tree.t


val compute :
  pos:(int * int) ->
  (content, header) Tree.t ->
  (content, header) Tree.t

val iter :
  (header -> content -> unit) ->
  (content,header) Tree.t -> unit

