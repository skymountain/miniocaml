let rec iterl3 f x y z = match x, y , z with
    xh::xt, yh::yt, zh::zt -> f xh yh zh; iterl3 f xt yt zt
  | [], [], [] -> ()
  | _, _, _ -> raise (Invalid_argument "Misc.iterl3")
