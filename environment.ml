type 'a t = (Syntax.id * 'a) list

exception Not_bound

let empty = []
let extend x v env = (x,v)::env
let extendl ids vs env =
  List.fold_left2 (fun env id v -> extend id v env) env ids vs 
  
let rec lookup x env = 
  try List.assoc x env with Not_found -> raise Not_bound

let rec map f = function
    [] -> []
  | (id, v)::rest -> (id, f v) :: map f rest

let rec fold_right f env a = 
  match env with
      [] -> a
    | (_, v)::rest -> f v (fold_right f rest a)
