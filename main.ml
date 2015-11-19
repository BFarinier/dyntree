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

let fill _ c =
  let a0,a1,a2,a3 = center c.pts in
  let v = max 0 (255 - (a0+a1+a2+a3) / 4) in
  set_color (rgb v v v);
  fill_rect c.sqr.x c.sqr.y c.sqr.size c.sqr.size


let () =
  Random.self_init ();
  open_graph "";
  auto_synchronize false;

  let rec loop (t: (content,header) Tree.t) =
    let sqr =
      let (_,(h,_,_,_,_),_,_,_) = Tree.get t in
      let x = h.sqr.x in
      let y = h.sqr.y in
      let size = h.sqr.size in
      { x; y; size = size * 2 }
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
      iter fill t;
      synchronize ();
      sleep 0.1;
      loop t
    )
  in

  let noise =
    { max = 256.; min = 1.; cur = 256. ;
      attenuate = (fun n -> max n.min (n.cur /. 2.)) }
  in

  loop (create 
          ~seed:(Random.int (1 lsl 30 - 1))
          ~max:512 ~min:1
          noise)
