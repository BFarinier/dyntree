type square =
  { x: int; y: int; size: int }
type noise =
  { min: float; max: float; cur: float;
    attenuate: noise -> float }
type header =
  { seed: int; sqr: square;
    max_size: int; min_size: int;
    noise: noise } 
type content =
  { sqr: square;
    pts: int * int * int * int *
         int * int * int * int *
         int * int * int * int *
         int * int * int * int }


let attenuate v =
  let cur = v.attenuate v in
  { v with cur }


let split square =
  let size = square.size / 2 in
  let x0 = square.x in
  let y0 = square.y in
  let x1 = x0 + size in
  let y1 = y0 + size in
  { x=x0; y=y0; size },
  { x=x1; y=y0; size },
  { x=x0; y=y1; size },
  { x=x1; y=y1; size }



let average i0 i1 i2 i3 =
  (i0+i1+i2+i3) / 4

let rand n = ((n * 1103515245 + 12345) / 66536) mod 32768
let rand s x y = rand (s + x * 5 + y * 17)

let multirand seed x y size =
  let size = size / 2 in
  let p00 = rand seed (x - size)     (y - size) in
  let p01 = rand seed  x             (y - size) in
  let p02 = rand seed (x + size)     (y - size) in
  let p03 = rand seed (x + 2 * size) (y - size) in
  let p04 = rand seed (x + 3 * size) (y - size) in
  let p10 = rand seed (x - size)      y in
  let p11 = rand seed  x              y in
  let p12 = rand seed (x + size)      y in
  let p13 = rand seed (x + 2 * size)  y in
  let p14 = rand seed (x + 3 * size)  y in
  let p20 = rand seed (x - size)     (y + size) in
  let p21 = rand seed  x             (y + size) in
  let p22 = rand seed (x + size)     (y + size) in
  let p23 = rand seed (x + 2 * size) (y + size) in
  let p24 = rand seed (x + 3 * size) (y + size) in
  let p30 = rand seed (x - size)     (y + 2 * size) in
  let p31 = rand seed  x             (y + 2 * size) in
  let p32 = rand seed (x + size)     (y + 2 * size) in
  let p33 = rand seed (x + 2 * size) (y + 2 * size) in
  let p34 = rand seed (x + 3 * size) (y + 2 * size) in
  let p40 = rand seed (x - size)     (y + 3 * size) in
  let p41 = rand seed  x             (y + 3 * size) in
  let p42 = rand seed (x + size)     (y + 3 * size) in
  let p43 = rand seed (x + 2 * size) (y + 3 * size) in
  let p44 = rand seed (x + 3 * size) (y + 3 * size) in
  p00,p01,p02,p03,p04,
  p10,p11,p12,p13,p14,
  p20,p21,p22,p23,p24,
  p30,p31,p32,p33,p34,
  p40,p41,p42,p43,p44

let map25 f
    (p00,p01,p02,p03,p04,
     p10,p11,p12,p13,p14,
     p20,p21,p22,p23,p24,
     p30,p31,p32,p33,p34,
     p40,p41,p42,p43,p44) =
  f p00,f p01,f p02,f p03,f p04,
  f p10,f p11,f p12,f p13,f p14,
  f p20,f p21,f p22,f p23,f p24,
  f p30,f p31,f p32,f p33,f p34,
  f p40,f p41,f p42,f p43,f p44




let generate (h: header) (c: content) : (content,header) Tree.chunk =
  let
    p00,p01,p02,p03,
    p10,p11,p12,p13,
    p20,p21,p22,p23,
    p30,p31,p32,p33
    = c.pts in

  let m00,i01,m02,i03,m04,
      i10, _ ,i12, _ ,i14,
      m20,i21,m22,i23,m24,
      i30, _ ,i32, _ ,i34,
      m40,i41,m42,i43,m44
    = multirand h.seed c.sqr.x c.sqr.y c.sqr.size in

  let noise = attenuate h.noise in
  let lim = truncate noise.cur in

  let m00 = average p00 p01 p10 p11 + (m00 mod lim) in
  let m02 = average p01 p02 p11 p12 + (m02 mod lim) in
  let m04 = average p02 p03 p12 p13 + (m04 mod lim) in
  let m20 = average p10 p11 p20 p21 + (m20 mod lim) in
  let m22 = average p11 p12 p21 p22 + (m22 mod lim) in
  let m24 = average p12 p13 p22 p23 + (m24 mod lim) in
  let m40 = average p20 p21 p30 p31 + (m40 mod lim) in
  let m42 = average p21 p22 p31 p32 + (m42 mod lim) in
  let m44 = average p22 p23 p32 p33 + (m44 mod lim) in

  let noise = attenuate noise in
  let lim = truncate noise.cur in

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
     p21,i32,p22,i34) in
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
  let h = { h with sqr = c.sqr; noise } in
  h, c0, c1, c2, c3



let pts seed sqr noise =
  let p00,p01,p02,p03,p04,
      p10,p11,p12,p13,p14,
      p20,p21,p22,p23,p24,
      p30,p31,p32,p33,p34,
      p40,p41,p42,p43,p44
    = map25
      (fun i -> i mod (truncate noise.cur))
      (multirand seed sqr.x sqr.y sqr.size) in
  (p00,p01,p02,p03,
   p10,p11,p12,p13,
   p20,p21,p22,p23,
   p30,p31,p32,p33),
  (p01,p02,p03,p04,
   p11,p12,p13,p14,
   p21,p22,p23,p24,
   p31,p32,p33,p34),
  (p10,p11,p12,p13,
   p20,p21,p22,p23,
   p30,p31,p32,p33,
   p40,p41,p42,p43),
  (p11,p12,p13,p14,
   p21,p22,p23,p24,
   p31,p32,p33,p34,
   p41,p42,p43,p44)

let create ~seed ~max ~min noise =
  let sqr = { x=(-max); y=(-max); size=2*max } in
  let s0,s1,s2,s3 = split sqr in
  let pts0,pts1,pts2,pts3 = pts seed sqr noise in
  let h = { seed; max_size=max; min_size=min; noise; sqr } in
  let c0 = generate h { pts=pts0; sqr=s0 } in
  let c1 = generate h { pts=pts1; sqr=s1 } in
  let c2 = generate h { pts=pts2; sqr=s2 } in
  let c3 = generate h { pts=pts3; sqr=s3 } in
  Tree.create c0 c1 c2 c3 generate


let combine h sqr = 
       let s0,s1,s2,s3 = split sqr in
       let pts0,pts1,pts2,pts3 = pts h.seed sqr h.noise in
       let h = { h with sqr } in
       let c0 = { pts=pts0; sqr=s0 } in
       let c1 = { pts=pts1; sqr=s1 } in
       let c2 = { pts=pts2; sqr=s2 } in
       let c3 = { pts=pts3; sqr=s3 } in
       h,c0,c1,c2,c3

let move_upward =
  Tree.move_upward
    (fun ((h:header),_,_,_,_) ->
       let sqr = { h.sqr with y = h.sqr.y - h.sqr.size } in
       combine h sqr)

let move_downward =
  Tree.move_downward
    (fun ((h:header),_,_,_,_) ->
       let sqr = { h.sqr with y = h.sqr.y + h.sqr.size } in
       combine h sqr)

let move_leftward =
  Tree.move_leftward
    (fun ((h:header),_,_,_,_) ->
       let sqr = { h.sqr with x = h.sqr.x - h.sqr.size } in
       combine h sqr)

let move_rightward =
  Tree.move_rightward
    (fun ((h:header),_,_,_,_) ->
       let sqr = { h.sqr with x = h.sqr.x + h.sqr.size } in
       combine h sqr)


let compute ~pos:(x,y) =
  Tree.compute
    (fun (h: header) ->
       let hx = h.sqr.x in
       let hy = h.sqr.y in
       let size = h.sqr.size in
       h.min_size < size
       && x + size >= hx && x - size <= hx + size
       && y + size >= hy && y - size <= hy + size)

let iter f =
  Tree.iter
    (fun (h,c0,c1,c2,c3) ->
       f h c0; f h c1; f h c2; f h c3)

