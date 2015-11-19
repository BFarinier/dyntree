open Dsquare
open Graphics


let rec sleep t =
  if t > 0. then
    let now = Unix.gettimeofday () in
    (try ignore (Unix.select [] [] [] t) with
     | _ -> ());
    sleep (t -. ((Unix.gettimeofday ()) -. now))

let center
    (_,_,_,_,
     _,a0,a1,_,
     _,a2,a3,_,
     _,_,_,_) =
  a0,a1,a2,a3


let color x l =
  let v = min 255 (truncate (255. *. (1. -. exp (-2. *. (abs_float x) /. l)))) in
  if x < 0. then
    rgb 255 (255 - v) (255 - v)
  else
    rgb (255 - v) (255 - v) 255

let fill l _ c =
  let a0,a1,a2,a3 = center c.pts in
  let col = color (float ((a0+a1+a2+a3) / 4)) l in
  set_color col;
  fill_rect c.sqr.x c.sqr.y c.sqr.size c.sqr.size


let () =
  Random.self_init ();
  open_graph "";
  auto_synchronize false;

  let rec loop (t: (content,header) Tree.t) =
    let noise,sqr =
      let (_,(h,_,_,_,_),_,_,_) = Tree.get t in
      let x = h.sqr.x in
      let y = h.sqr.y in
      let size = h.sqr.size in
      h.noise,{ x; y; size = size * 2 }
    in    
    let x,y = mouse_pos () in
    Printf.printf
      "\r%4i,%4i -- %4i,%4i,%4i%!"
      x y sqr.x sqr.y sqr.size;
    if y < sqr.y then
      loop (move_upward t)
    else
    if y > sqr.y + sqr.size then
      loop (move_downward t)
    else
    if x < sqr.x then
      loop (move_leftward t)
    else
    if x > sqr.x + sqr.size then
      loop (move_rightward t)
    else (
      let t = compute (x,y) t in
      clear_graph();
      iter (fill noise.max) t;
      synchronize ();
      sleep 0.1;
      loop t
    )
  in

  let noise =
    { max = 16384.; min = 1.; cur = 16384. ;
      attenuate = (fun n -> max n.min (n.cur /. 2.)) }
  in

  loop (create 
          ~seed:(Random.int (1 lsl 30 - 1))
          ~max:512 ~min:1
          noise)
