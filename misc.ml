module StrSet = Set.Make (struct
                            type t = string
                            let compare = String.compare
                          end)

let rec iterl3 f x y z = match x, y , z with
    xh::xt, yh::yt, zh::zt -> f xh yh zh; iterl3 f xt yt zt
  | [], [], [] -> ()
  | _, _, _ -> raise (Invalid_argument "Misc.iterl3")

let rec combine3 x y z = match x, y, z with
    xh::xt, yh::yt, zh::zt -> (xh,yh,zh)::(combine3 xt yt zt)
  | [],[],[] -> []
  | _, _, _ -> raise (Invalid_argument "Misc.combine3")
let rec combine4 w x y z = match w, x, y, z with
    wh::wt, xh::xt, yh::yt, zh::zt -> (wh,xh,yh,zh)::(combine4 wt xt yt zt)
  | [], [],[],[] -> []
  | _, _, _, _ -> raise (Invalid_argument "Misc.combine3")

let ebound = StrSet.empty
  
(* for debug *)
let print_set s set =
  print_endline s;
  StrSet.iter (fun s -> print_string (Printf.sprintf "%s," s)) set;
  print_newline ()
    
let union_of_dbounds b1 b2 err = 
  if StrSet.equal (StrSet.inter b1 b2) ebound then
    StrSet.union b1 b2
  else
    err "One or more Variables is bound several times"

let list_of_set = StrSet.fold (fun s acc -> s::acc)
