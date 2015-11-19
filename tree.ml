type ('a,'b) tree =
  | Leaf of
      'b * 'a * 'a * 'a * 'a
  | Node of
      'b * 'a * 'a * 'a * 'a *
      ('a,'b) tree * ('a,'b) tree *
      ('a,'b) tree * ('a,'b) tree


let bloom f t =
  match t with
  | Node _ -> assert false
  | Leaf(b,a0,a1,a2,a3) ->
    let t0 =
      let b,a0,a1,a2,a3 = f b a0 in
      Leaf(b,a0,a1,a2,a3) in
    let t1 =
      let b,a0,a1,a2,a3 = f b a1 in
      Leaf(b,a0,a1,a2,a3) in
    let t2 =
      let b,a0,a1,a2,a3 = f b a2 in
      Leaf(b,a0,a1,a2,a3) in
    let t3 =
      let b, a0, a1, a2, a3 = f b a3 in
      Leaf(b,a0,a1,a2,a3) in
    Node(b,a0,a1,a2,a3,t0,t1,t2,t3)


let rec grow f g t =
  match t with
  | Node(b,a0,a1,a2,a3,t0,t1,t2,t3) ->
    if g b
    then
      let t0 = grow f g t0 in
      let t1 = grow f g t1 in
      let t2 = grow f g t2 in
      let t3 = grow f g t3 in
      Node(b,a0,a1,a2,a3,t0,t1,t2,t3)
    else
      Leaf(b,a0,a1,a2,a3) 
  | Leaf(b,_,_,_,_) ->
    if g b then
      grow f g (bloom f t)
    else t

let rec map f t =
  match t with
  | Node(b,a0,a1,a2,a3,t0,t1,t2,t3) ->
    let b,a0,a1,a2,a3 = f (b,a0,a1,a2,a3) in
    Node
      (b,a0,a1,a2,a3,
       map f t0, map f t1,
       map f t2, map f t3)
  | Leaf(b,a0,a1,a2,a3) ->
    let b,a0,a1,a2,a3 = f (b,a0,a1,a2,a3) in
    Leaf(b,a0,a1,a2,a3)

let rec iter f t =
  match t with
  | Node(b,a0,a1,a2,a3,t0,t1,t2,t3) ->
    f (b,a0,a1,a2,a3);
    iter f t0; iter f t1;
    iter f t2; iter f t3
  | Leaf(b,a0,a1,a2,a3) ->
    f (b,a0,a1,a2,a3)



type ('a,'b) chunk = 'b * 'a * 'a * 'a * 'a
type ('a,'b) t =
  (
    ('a,'b) chunk * ('a,'b) tree,
    'b -> 'a -> ('a,'b) chunk
  ) chunk

let pack (b,a0,a1,a2,a3) = Leaf(b,a0,a1,a2,a3)

let create c0 c1 c2 c3 f =
  (f,
   (c0,bloom f (pack c0)),
   (c1,bloom f (pack c1)),
   (c2,bloom f (pack c2)),
   (c3,bloom f (pack c3)))

let get (f,(k0,_),(k1,_),(k2,_),(k3,_)) = f,k0,k1,k2,k3


let move_upward
    (f: ('a,'b) chunk -> ('a,'b) chunk)
    (g,((k2,t2) as c2),((k3,t3) as c3),_,_): ('a,'b) t =
  let k0 = f k2 in
  let k1 = f k3 in
  (g,(k0,bloom g (pack k0)),(k1,bloom g (pack k1)),c2,c3)

let move_downward
    (f: ('a,'b) chunk -> ('a,'b) chunk)
    (g,_,_,((k0,t0) as c0),((k1,t1) as c1)): ('a,'b) t =
  let k2 = f k0 in
  let k3 = f k1 in
  (g,c0,c1,(k2,bloom g (pack k2)),(k3,bloom g (pack k3)))

let move_leftward
    (f: ('a,'b) chunk -> ('a,'b) chunk)
    (g,((k1,t1) as c1),_,((k3,t3) as c3),_): ('a,'b) t =
  let k0 = f k1 in
  let k2 = f k3 in
  (g,(k0,bloom g (pack k0)),c1,(k2,bloom g (pack k2)),c3)

let move_rightward
    (f: ('a,'b) chunk -> ('a,'b) chunk)
    (g,_,((k0,t0) as c0),_,((k2,t2) as c2)): ('a,'b) t =
  let k1 = f k0 in
  let k3 = f k2 in
  (g,c0,(k1,bloom g (pack k1)),c2,(k3,bloom g (pack k3)))


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
