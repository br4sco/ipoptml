open Ipoptml

let test_solve_problem1 () =
  let p =
    let open Problem1 in
    create_nlp ~eval_f ~eval_grad_f ~eval_g ~jac_g_structure ~eval_jac_g
      ~h_structure ~eval_h ~lb ~ub ~constraints_lb ~constraints_ub
  in
  add_num_option p "tol" 3.82e-6 ;
  add_str_option p "mu_strategy" "adaptive" ;
  let x = Bigarray.(Array1.of_array Float64 c_layout [|1.; 5.; 5.; 1.|]) in
  Alcotest.(check bool)
    "solved" true
    (solve p x |> function SolveSucceeded, _ -> true | _ -> false)

let () =
  let open Alcotest in
  run "Ipoptml"
    [("problems", [("solved problem 1", `Slow, test_solve_problem1)])]
