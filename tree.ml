type ('a,'b) chunk = 'b * 'a * 'a * 'a * 'a

type ('a,'b) tree =
  | Leaf of
      ('a,'b) chunk
  | Node of
      ('a,'b) chunk *
      ('a,'b) tree * ('a,'b) tree *
      ('a,'b) tree * ('a,'b) tree


let bloom f = function
  | Node _ -> assert false
  | Leaf ((b,a0,a1,a2,a3) as chunk) ->
    let t0 = Leaf (f b a0) in
    let t1 = Leaf (f b a1) in
    let t2 = Leaf (f b a2) in
    let t3 = Leaf (f b a3) in
    Node (chunk,t0,t1,t2,t3)


let rec grow f g t =
  match t with
  | Node ((b,a0,a1,a2,a3 as chunk),t0,t1,t2,t3) ->
    if g b
    then
      let t0 = grow f g t0 in
      let t1 = grow f g t1 in
      let t2 = grow f g t2 in
      let t3 = grow f g t3 in
      Node (chunk,t0,t1,t2,t3)
    else
      Leaf chunk
  | Leaf(b,_,_,_,_) ->
    if g b then
      grow f g (bloom f t)
    else t

let rec map f t =
  match t with
  | Node(chunk,t0,t1,t2,t3) ->
    let chunk = f chunk in
    Node
      (chunk,
       map f t0, map f t1,
       map f t2, map f t3)
  | Leaf chunk ->
    Leaf (f chunk)

let rec iter f t =
  match t with
  | Node(chunk,t0,t1,t2,t3) ->
    f chunk;
    iter f t0; iter f t1;
    iter f t2; iter f t3
  | Leaf chunk ->
    f chunk

type ('a,'b) t =
  (
    ('a,'b) chunk * ('a,'b) tree,
    'b -> 'a -> ('a,'b) chunk
  ) chunk

let create c0 c1 c2 c3 f =
  (f,
   (c0,bloom f (Leaf c0)),
   (c1,bloom f (Leaf c1)),
   (c2,bloom f (Leaf c2)),
   (c3,bloom f (Leaf c3)))

let get (f,(k0,_),(k1,_),(k2,_),(k3,_)) = f,k0,k1,k2,k3


let move_upward
    (f: ('a,'b) chunk -> ('a,'b) chunk)
    (g,((k2,t2) as c2),((k3,t3) as c3),_,_): ('a,'b) t =
  let k0 = f k2 in
  let k1 = f k3 in
  (g,(k0,bloom g (Leaf k0)),(k1,bloom g (Leaf k1)),c2,c3)

let move_downward
    (f: ('a,'b) chunk -> ('a,'b) chunk)
    (g,_,_,((k0,t0) as c0),((k1,t1) as c1)): ('a,'b) t =
  let k2 = f k0 in
  let k3 = f k1 in
  (g,c0,c1,(k2,bloom g (Leaf k2)),(k3,bloom g (Leaf k3)))

let move_leftward
    (f: ('a,'b) chunk -> ('a,'b) chunk)
    (g,((k1,t1) as c1),_,((k3,t3) as c3),_): ('a,'b) t =
  let k0 = f k1 in
  let k2 = f k3 in
  (g,(k0,bloom g (Leaf k0)),c1,(k2,bloom g (Leaf k2)),c3)

let move_rightward
    (f: ('a,'b) chunk -> ('a,'b) chunk)
    (g,_,((k0,t0) as c0),_,((k2,t2) as c2)): ('a,'b) t =
  let k1 = f k0 in
  let k3 = f k2 in
  (g,c0,(k1,bloom g (Leaf k1)),c2,(k3,bloom g (Leaf k3)))


let compute g ((f,(k0,t0),(k1,t1),(k2,t2),(k3,t3)): ('a,'b) t) =
  (f,
   (k0, grow f g t0),
   (k1, grow f g t1),
   (k2, grow f g t2),
   (k3, grow f g t3))

let map f h ((_,(k0,t0),(k1,t1),(k2,t2),(k3,t3)): ('a,'b) t): ('c,'d) t =
  h,
  (f k0, map f t0),
  (f k1, map f t1),
  (f k2, map f t2),
  (f k3, map f t3)

let iter f ((_,c0,c1,c2,c3): ('a,'b) t): unit =
  iter f (snd c0);
  iter f (snd c1);
  iter f (snd c2);
  iter f (snd c3)
