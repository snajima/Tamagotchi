open Main

type node = {
  mutable prev : node;
  mutable next : node;
  mutable index : int;
}

let l_input current = current.prev

let r_input current = current.next

(* let m_input current = if current.index = 0 then [open first] else if
   current.index = 1 then [open second] ... *)
