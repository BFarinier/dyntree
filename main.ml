type square = { x: int; y: int; size: int }
type header =
  { seed: int;
    sqr: square; base: int;
    lim: float; red: float -> float } 
type content =
  { sqr: square;
    pts: int * int * int * int *
         int * int * int * int *
         int * int * int * int *
         int * int * int * int }


let split square =
  let size = square.size / 2 in
  let x0 = square.x in
  let y0 = square.y in
  let x1 = x0 + size in
  let y1 = y0 + size in
  { x=x0; y=y1; size },
  { x=x1; y=y1; size },
  { x=x0; y=y0; size },
  { x=x1; y=y0; size }


let average i0 i1 i2 i3 =
  (i0+i1+i2+i3) / 4

let rand n = ((n * 1103515245 + 12345) / 66536) mod 32768
let rand s x y = rand (s + x * 5 + y * 17)

let generate (h: header) (c: content) : (content,header) Tree.chunk =
  let x = c.sqr.x in
  let y = c.sqr.y in
  let size = c.sqr.size in
  let lim = max 1 (truncate h.lim) in
  let flim = h.red h.lim in
  let
    p00,p01,p02,p03,
    p10,p11,p12,p13,
    p20,p21,p22,p23,
    p30,p31,p32,p33
    = c.pts in

  let m00 = rand h.seed (x - size / 2)     (y + size * 3 / 2) in
  let m02 = rand h.seed (x + size / 2)     (y + size * 3 / 2) in
  let m04 = rand h.seed (x + size * 3 / 2) (y + size * 3 / 2) in
  let m20 = rand h.seed (x - size / 2)     (y + size * 2) in
  let m22 = rand h.seed (x + size / 2)     (y + size * 2) in
  let m24 = rand h.seed (x + size * 3 / 2) (y + size * 2) in
  let m40 = rand h.seed (x - size / 2)     (y - size * 2) in
  let m42 = rand h.seed (x + size / 2)     (y - size * 2) in
  let m44 = rand h.seed (x + size * 3 / 2) (y - size * 2) in

  let m00 = average p00 p01 p10 p11 + (m00 mod lim) in
  let m02 = average p01 p02 p11 p12 + (m02 mod lim) in
  let m04 = average p02 p03 p12 p13 + (m04 mod lim) in
  let m20 = average p10 p11 p20 p21 + (m20 mod lim) in
  let m22 = average p11 p12 p21 p22 + (m22 mod lim) in
  let m24 = average p12 p13 p22 p23 + (m24 mod lim) in
  let m40 = average p20 p21 p30 p31 + (m40 mod lim) in
  let m42 = average p21 p22 p31 p32 + (m42 mod lim) in
  let m44 = average p22 p23 p32 p33 + (m44 mod lim) in

  let lim = max 1 (truncate flim) in
  let flim = h.red flim in

  let i01 = rand h.seed  x                 (y + size * 3 / 2) in
  let i03 = rand h.seed (x + size)         (y + size * 3 / 2) in
  let i10 = rand h.seed (x - size / 2)     (y + size) in
  let i12 = rand h.seed (x + size / 2)     (y + size) in
  let i14 = rand h.seed (x + size * 3 / 2) (y + size) in
  let i21 = rand h.seed  x                 (y + size / 2) in
  let i23 = rand h.seed (x + size)         (y + size / 2) in
  let i30 = rand h.seed (x - size / 2)      y in
  let i32 = rand h.seed (x + size / 2)      y in
  let i34 = rand h.seed (x + size * 3 / 2)  y in
  let i41 = rand h.seed  x                 (y - size / 2) in
  let i43 = rand h.seed (x + size)         (y - size / 2) in

  let i01 = average p01 m00 m02 p11 + (i01 mod lim) in
  let i03 = average p02 m02 m04 p12 + (i03 mod lim) in
  let i10 = average m00 p10 p11 m20 + (i10 mod lim) in
  let i12 = average m02 p11 p12 m22 + (i12 mod lim) in
  let i14 = average m04 p12 p13 m24 + (i14 mod lim) in
  let i21 = average p11 m20 m22 p21 + (i21 mod lim) in
  let i23 = average p12 m22 m24 p22 + (i23 mod lim) in
  let i30 = average m20 p20 p21 m40 + (i30 mod lim) in
  let i32 = average m22 p21 p22 m42 + (i32 mod lim) in
  let i34 = average m24 p22 p23 m44 + (i34 mod lim) in
  let i41 = average p21 m40 m42 p22 + (i41 mod lim) in
  let i43 = average p22 m42 m44 p32 + (i43 mod lim) in

  let pts0 =
    (m00,i01,m02,i03,
     i10,p11,i12,p12,
     m20,i21,m22,i23,
     i30,p21,i32,p22) in
  let pts1 =
    (i01,m02,i03,m04,
     p11,i12,p12,i14,
     i21,m22,i23,m24,
     p21,i23,p22,i34) in
  let pts2 =
    (i10,p11,i12,p12,
     m20,i21,m22,i23,
     i30,p21,i32,p22,
     m40,i41,m42,i43) in
  let pts3 =
    (p11,i12,p12,i14,
     i21,m22,i23,m24,
     p21,i32,p22,i34,
     i41,m42,i43,m44) in

  let s0,s1,s2,s3 = split c.sqr in
  let c0 = { sqr = s0; pts = pts0 } in
  let c1 = { sqr = s1; pts = pts1 } in
  let c2 = { sqr = s2; pts = pts2 } in
  let c3 = { sqr = s3; pts = pts3 } in
  let h = { h with sqr = c.sqr; lim = flim } in
  h, c0, c1, c2, c3

let create size base seed lim red =
  let s0 = { x = -size; y = 0; size } in
  let s1 = { x = 0; y = 0; size } in
  let s2 = { x = -size; y = -size; size } in
  let s3 = { x = 0; y = -size; size } in
  let pts = 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 in
  let h = { seed; base; sqr={ x=(-size); y=(-size); size=2*size }; lim; red } in
  let c0 = generate h { pts; sqr=s0 } in
  let c1 = generate h { pts; sqr=s1 } in
  let c2 = generate h { pts; sqr=s2 } in
  let c3 = generate h { pts; sqr=s3 } in
  Tree.create c0 c1 c2 c3 generate

let compute (x,y) =
  Tree.compute
    (fun (h: header) ->
       let size = h.sqr.size in
       h.base < size
       && x + size/2 >= h.sqr.x
       && x - size/2 <= h.sqr.x + size
       && y + size/2 >= h.sqr.y
       && y - size/2 <= h.sqr.y + size)

let iter f =
  Tree.iter
    (fun (b,a0,a1,a2,a3) ->
       f a0; f a1; f a2; f a3)

let rec sleep t =
  if t > 0. then
    let now = Unix.gettimeofday () in
    (try ignore (Unix.select [] [] [] t) with
     | _ -> ());
    sleep (t -. ((Unix.gettimeofday ()) -. now))

let () =
  Random.self_init ();
  let open Graphics in
  open_graph "";
  auto_synchronize false;

  let center
      (_,_,_,_,
       _,a0,a1,_,
       _,a2,a3,_,
       _,_,_,_) =
    a0,a1,a2,a3
  in

  let fill c =
    let a0,a1,a2,a3 = center c.pts in
    let v = 255 - average a0 a1 a2 a3 in
    set_color (rgb v v v);
    fill_rect c.sqr.x c.sqr.y c.sqr.size c.sqr.size
  in

  let rec loop (t: (content,header) Tree.t) =
    let t = compute (mouse_pos ()) t in
    iter fill t;
    synchronize ();
    sleep 0.1;
    loop t
  in
  loop (create 512 1
          (Random.int (1 lsl 30 - 1))
          256. (fun f -> f /. 2.))
