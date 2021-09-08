(* Test solving example NLP from IPOPT documentation
   https://coin-or.github.io/Ipopt/INTERFACES.html *)

let eval_f x = (x.{0} *. x.{3} *. (x.{0} +. x.{1} +. x.{2})) +. x.{2}

let eval_grad_f x grad_f =
  grad_f.{0} <- (x.{0} *. x.{3}) +. (x.{3} *. (x.{0} +. x.{1} +. x.{2})) ;
  grad_f.{1} <- x.{0} *. x.{3} ;
  grad_f.{2} <- (x.{0} *. x.{3}) +. 1. ;
  grad_f.{3} <- x.{0} *. (x.{0} +. x.{1} +. x.{2}) ;
  ()

let eval_g x g =
  g.{0} <- x.{0} *. x.{1} *. x.{2} *. x.{3} ;
  g.{1} <-
    (x.{0} *. x.{0}) +. (x.{1} *. x.{1}) +. (x.{2} *. x.{2}) +. (x.{3} *. x.{3}) ;
  ()

let jac_g_structure =
  [|(0, 0); (0, 1); (0, 2); (0, 3); (1, 0); (1, 1); (1, 2); (1, 3)|]

let eval_jac_g x jac_g =
  jac_g.{0} <- x.{1} *. x.{2} *. x.{3} ;
  jac_g.{1} <- x.{0} *. x.{2} *. x.{3} ;
  jac_g.{2} <- x.{0} *. x.{1} *. x.{3} ;
  jac_g.{3} <- x.{0} *. x.{1} *. x.{2} ;
  jac_g.{4} <- 2. *. x.{0} ;
  jac_g.{5} <- 2. *. x.{1} ;
  jac_g.{6} <- 2. *. x.{2} ;
  jac_g.{7} <- 2. *. x.{3} ;
  ()

let h_structure =
  [| (0, 0)
   ; (1, 0)
   ; (1, 1)
   ; (2, 0)
   ; (2, 1)
   ; (2, 2)
   ; (3, 0)
   ; (3, 1)
   ; (3, 2)
   ; (3, 3) |]

let eval_h ~sigma ~x ~lambda ~h =
  h.{0} <- sigma *. (2. *. x.{3}) ;
  h.{1} <- sigma *. x.{3} ;
  h.{2} <- 0. ;
  h.{3} <- sigma *. x.{3} ;
  h.{4} <- 0. ;
  h.{5} <- 0. ;
  h.{6} <- sigma *. ((2. *. x.{0}) +. x.{1} +. x.{2}) ;
  h.{7} <- sigma *. x.{0} ;
  h.{8} <- sigma *. x.{0} ;
  h.{9} <- 0. ;
  (* add the portion for the first constraint *)
  h.{1} <- h.{1} +. (lambda.{0} *. (x.{2} *. x.{3})) ;
  h.{3} <- h.{3} +. (lambda.{0} *. (x.{1} *. x.{3})) ;
  h.{4} <- h.{4} +. (lambda.{0} *. (x.{0} *. x.{3})) ;
  h.{6} <- h.{6} +. (lambda.{0} *. (x.{1} *. x.{2})) ;
  h.{7} <- h.{7} +. (lambda.{0} *. (x.{0} *. x.{2})) ;
  h.{8} <- h.{8} +. (lambda.{0} *. (x.{0} *. x.{1})) ;
  (* add the portion for the second constraint *)
  h.{0} <- h.{0} +. (lambda.{1} *. 2.) ;
  h.{2} <- h.{2} +. (lambda.{1} *. 2.) ;
  h.{5} <- h.{5} +. (lambda.{1} *. 2.) ;
  h.{9} <- h.{9} +. (lambda.{1} *. 2.) ;
  ()

let lb = Bigarray.(Array1.of_array Float64 c_layout [|1.; 1.; 1.; 1.|])

let ub = Bigarray.(Array1.of_array Float64 c_layout [|5.; 5.; 5.; 5.|])

let constraints_lb = Bigarray.(Array1.of_array Float64 c_layout [|25.; 40.|])

let constraints_ub =
  Bigarray.(Array1.of_array Float64 c_layout [|Float.infinity; 40.|])
