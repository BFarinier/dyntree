type square = { x : int; y : int; size : int; }

type header = {
  seed : int;
  sqr : square;
  base : int;
  lim : float;
  red : float -> float;
}

type content = {
  sqr : square;
  pts :
    int * int * int * int *
    int * int * int * int *
    int * int * int * int *
    int * int * int * int;
}

val create :
  int -> int -> int ->
  float -> (float -> float) ->
  (content, header) Tree.t

val compute :
  pos:(int * int) ->
  (content, header) Tree.t ->
  (content, header) Tree.t

val iter :
  (header -> content -> unit) ->
  (content,header) Tree.t -> unit
