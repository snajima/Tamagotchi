open Notty

let rad n color =
  let a1 = A.fg color in
  let a2 = A.(st blink ++ a1) in
  I.(
    string a2 "Rad" |> hpad n 0
    <-> (string a1 "(⌐■_■)" |> hpad (n + 7) 0))

let colors = A.[ red; green; yellow; blue; magenta; cyan ]

let _ =
  colors
  |> List.mapi I.(fun i c -> rad i c |> pad ~t:i ~l:(2 * i))
  |> I.zcat
