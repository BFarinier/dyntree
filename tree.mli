type ('a,'b) t
type ('a,'b) chunk = 'b * 'a * 'a * 'a * 'a

val create :
  ('a,'b) chunk -> ('a,'b) chunk ->
  ('a,'b) chunk -> ('a,'b) chunk ->
  ('b -> 'a -> ('a,'b) chunk) ->
  ('a,'b) t

val move_upward :
  ?equal:(('a, 'b) chunk -> ('a, 'b) chunk -> bool) ->
  (('a,'b) chunk -> ('a,'b) chunk) ->
  ('a,'b) t -> ('a,'b) t

val move_downward :
  ?equal:(('a, 'b) chunk -> ('a, 'b) chunk -> bool) ->
  (('a,'b) chunk -> ('a,'b) chunk) ->
  ('a,'b) t -> ('a,'b) t

val move_leftward :
  ?equal:(('a, 'b) chunk -> ('a, 'b) chunk -> bool) ->
  (('a,'b) chunk -> ('a,'b) chunk) ->
  ('a,'b) t -> ('a,'b) t

val move_rightward :
  ?equal:(('a, 'b) chunk -> ('a, 'b) chunk -> bool) ->
  (('a,'b) chunk -> ('a,'b) chunk) ->
  ('a,'b) t -> ('a,'b) t

val compute :
  ('b -> bool) ->
  ('a,'b) t -> ('a,'b) t

val map :
  (('a,'b) chunk -> ('c,'d) chunk) ->
  ('d -> 'c -> ('c,'d) chunk) ->
  ('a,'b) t -> ('c,'d) t

val iter :
  (('a,'b) chunk -> unit) ->
  ('a,'b) t -> unit

