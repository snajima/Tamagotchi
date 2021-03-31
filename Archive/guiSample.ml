exception End

type state = {
  maxx : int;
  maxy : int;
  mutable x : int;
  mutable y : int;
  scale : int;
  bc : Graphics.color;
  fc : Graphics.color;
  pc : Graphics.color;
}

let skel f_init f_end f_key f_mouse f_except =
  f_init ();
  try
    while true do
      try
        let s =
          Graphics.wait_next_event
            [ Graphics.Button_down; Graphics.Key_pressed ]
        in
        if s.Graphics.keypressed then f_key s.Graphics.key
        else if s.Graphics.button then
          f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
      with
      | End -> raise End
      | e -> f_except e
    done
  with End -> f_end ()

let next_line () =
  let x, y = Graphics.current_point () in
  if y > 12 then Graphics.moveto 0 (y - 12) else Graphics.moveto 0 y

let handle_char c =
  match c with
  | '&' -> raise End
  | '\n' -> next_line ()
  | '\r' -> next_line ()
  | _ -> Graphics.draw_char c

let go () =
  skel
    (fun () ->
      Graphics.clear_graph ();
      Graphics.moveto 0 (Graphics.size_y () - 12))
    (fun () -> Graphics.clear_graph ())
    handle_char
    (fun x y -> Graphics.moveto x y)
    (fun e -> ())

let draw_point x y s c =
  Graphics.set_color c;
  Graphics.fill_rect (s * x) (s * y) s s

let t_init s () =
  Graphics.open_graph
    (" "
    ^ string_of_int (s.scale * s.maxx)
    ^ "x"
    ^ string_of_int (s.scale * s.maxy));
  Graphics.set_color s.bc;
  Graphics.fill_rect 0 0
    ((s.scale * s.maxx) + 1)
    ((s.scale * s.maxy) + 1);
  draw_point s.x s.y s.scale s.pc

let t_end s () =
  Graphics.close_graph ();
  print_string "Good bye...";
  print_newline ()

let t_mouse s x y = ()

let t_except s ex = ()

let t_key s c =
  draw_point s.x s.y s.scale s.fc;
  (match c with
  | 'w' -> if s.y < s.maxy then s.y <- s.y + 1
  | 's' -> if s.y > 0 then s.y <- s.y - 1
  | 'a' -> if s.x > 0 then s.x <- s.x - 1
  | 'd' -> if s.x < s.maxx then s.x <- s.x + 1
  | 'c' ->
      Graphics.set_color s.bc;
      Graphics.fill_rect 0 0
        ((s.scale * s.maxx) + 1)
        ((s.scale * s.maxy) + 1);
      Graphics.clear_graph ()
  | 'x' -> raise End
  | _ -> ());
  draw_point s.x s.y s.scale s.pc

let sample_state =
  {
    maxx = 120;
    maxy = 120;
    x = 60;
    y = 60;
    scale = 4;
    bc = Graphics.rgb 255 255 255;
    fc = Graphics.black;
    pc = Graphics.red;
  }

let slate () =
  skel (t_init sample_state) (t_end sample_state) (t_key sample_state)
    (t_mouse sample_state) (t_except sample_state)

let _ = slate ()
