type ('a,'b) t
type ('a,'b) chunk = 'b * 'a * 'a * 'a * 'a

val create :
  ('a,'b) chunk -> ('a,'b) chunk ->
  ('a,'b) chunk -> ('a,'b) chunk ->
  ('b -> 'a -> ('a,'b) chunk) ->
  ('a,'b) t

val move_upward :
  (('a,'b) chunk -> ('a,'b) chunk) ->
  ('a,'b) t -> ('a,'b) t

val move_downward :
  (('a,'b) chunk -> ('a,'b) chunk) ->
  ('a,'b) t -> ('a,'b) t

val move_leftward :
  (('a,'b) chunk -> ('a,'b) chunk) ->
  ('a,'b) t -> ('a,'b) t

val move_rightward :
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

