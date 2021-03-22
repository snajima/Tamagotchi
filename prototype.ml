open Nottui
module W = Nottui_widgets

let _ = Ui_loop.run (Lwd.pure (W.printf "Hello world"))
