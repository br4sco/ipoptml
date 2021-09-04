open Ctypes
open Foreign

(* C INTERFACE *)

let number_t = double

let index_t = int

let bool_t = int

(* True *)
let tt_c = 1

(* False *)
let ff_c = 0

type ipopt_problem = unit ptr

let ipopt_problem_t : ipopt_problem typ = ptr void

type user_data = unit ptr

let user_data_t : user_data typ = ptr void

let eval_f_cb_t =
  index_t @-> ptr number_t @-> bool_t @-> ptr number_t @-> user_data_t
  @-> returning bool_t

let eval_grad_f_cb_t =
  index_t @-> ptr number_t @-> bool_t @-> ptr number_t @-> user_data_t
  @-> returning bool_t

let eval_g_cb_t =
  index_t @-> ptr number_t @-> bool_t @-> index_t @-> ptr number_t
  @-> user_data_t @-> returning bool_t

let eval_jac_g_cb_t =
  index_t @-> ptr number_t @-> bool_t @-> index_t @-> index_t @-> ptr index_t
  @-> ptr index_t @-> ptr number_t @-> user_data_t @-> returning bool_t

let eval_h_cb_t =
  index_t @-> ptr number_t @-> bool_t @-> number_t @-> index_t @-> ptr number_t
  @-> bool_t @-> index_t @-> ptr index_t @-> ptr index_t @-> ptr number_t
  @-> user_data_t @-> returning bool_t

let create_ipopt_problem_c =
  foreign "CreateIpoptProblem"
    ( index_t @-> ptr number_t @-> ptr number_t @-> index_t @-> ptr number_t
    @-> ptr number_t @-> index_t @-> index_t @-> index_t @-> funptr eval_f_cb_t
    @-> funptr eval_g_cb_t @-> funptr eval_grad_f_cb_t
    @-> funptr eval_jac_g_cb_t @-> funptr eval_h_cb_t
    @-> returning ipopt_problem_t )

let free_ipopt_problem_c =
  foreign "FreeIpoptProblem" (ipopt_problem_t @-> returning void)

let add_ipopt_str_option_c =
  foreign "AddIpoptStrOption"
    (ipopt_problem_t @-> string @-> string @-> returning bool_t)

let add_ipopt_num_option_c =
  foreign "AddIpoptNumOption"
    (ipopt_problem_t @-> string @-> number_t @-> returning bool_t)

let add_ipopt_int_option_c =
  foreign "AddIpoptIntOption"
    (ipopt_problem_t @-> string @-> index_t @-> returning bool_t)

type application_return_status =
  | SolveSucceeded
  | SolvedToAcceptableLevel
  | InfeasibleProblemDetected
  | SearchDirectionBecomesTooSmall
  | DivergingIterates
  | UserRequestedStop
  | FeasiblePointFound
  | MaximumIterationsExceeded
  | RestorationFailed
  | ErrorInStepComputation
  | MaximumCpuTimeExceeded
  | NotEnoughDegreesOfFreedom
  | InvalidProblemDefinition
  | InvalidOption
  | InvalidNumberDetected
  | UnrecoverableException
  | NonIpoptExceptionThrown
  | InsufficientMemory
  | InternalError

let application_return_status_retcode = function
  | SolveSucceeded ->
      0
  | SolvedToAcceptableLevel ->
      1
  | InfeasibleProblemDetected ->
      2
  | SearchDirectionBecomesTooSmall ->
      3
  | DivergingIterates ->
      4
  | UserRequestedStop ->
      5
  | FeasiblePointFound ->
      6
  | MaximumIterationsExceeded ->
      -1
  | RestorationFailed ->
      -2
  | ErrorInStepComputation ->
      -3
  | MaximumCpuTimeExceeded ->
      -4
  | NotEnoughDegreesOfFreedom ->
      -10
  | InvalidProblemDefinition ->
      -11
  | InvalidOption ->
      -12
  | InvalidNumberDetected ->
      -13
  | UnrecoverableException ->
      -100
  | NonIpoptExceptionThrown ->
      -101
  | InsufficientMemory ->
      -102
  | InternalError ->
      -19

let application_return_status_t =
  let of_int = function
    | 0 ->
        SolveSucceeded
    | 1 ->
        SolvedToAcceptableLevel
    | 2 ->
        InfeasibleProblemDetected
    | 3 ->
        SearchDirectionBecomesTooSmall
    | 4 ->
        DivergingIterates
    | 5 ->
        UserRequestedStop
    | 6 ->
        FeasiblePointFound
    | -1 ->
        MaximumIterationsExceeded
    | -2 ->
        RestorationFailed
    | -3 ->
        ErrorInStepComputation
    | -4 ->
        MaximumCpuTimeExceeded
    | -10 ->
        NotEnoughDegreesOfFreedom
    | -11 ->
        InvalidProblemDefinition
    | -12 ->
        InvalidOption
    | -13 ->
        InvalidNumberDetected
    | -100 ->
        UnrecoverableException
    | -101 ->
        NonIpoptExceptionThrown
    | -102 ->
        InsufficientMemory
    | -199 ->
        InternalError
    | _ ->
        raise (Invalid_argument "Unexpected C enum")
  in
  view ~read:of_int ~write:application_return_status_retcode int

let ipopt_solve_c =
  foreign "IpoptSolve"
    ( ipopt_problem_t @-> ptr number_t @-> ptr number_t @-> ptr number_t
    @-> ptr number_t @-> ptr number_t @-> ptr number_t @-> user_data_t
    @-> returning application_return_status_t )

(* OCAML INTERFACE *)

exception Callback_failure of string

type problem = ipopt_problem

type vector =
  ( float
  , Bigarray.float64_elt
  , Bigarray_compat.c_layout )
  Bigarray_compat.Array1.t

let create_problem ~eval_f ~eval_grad_f ~eval_g ~eval_jac_g_structure
    ~eval_jac_g ~eval_h_structure ~eval_h ~lb ~ub ~constraints_lb
    ~constraints_ub =
  let n = Bigarray.Array1.dim lb in
  if Bigarray.Array1.dim ub != n then
    raise
      (Invalid_argument
         "Dimension of variable lower and upper bounds does not match" )
  else
    let m = Bigarray.Array1.dim constraints_lb in
    if Bigarray.Array1.dim constraints_ub != m then
      raise
        (Invalid_argument
           "Dimension of constraints lower and upper bounds does not match" )
    else
      let x_L = bigarray_start array1 lb in
      let x_U = bigarray_start array1 ub in
      let g_L = bigarray_start array1 constraints_lb in
      let g_U = bigarray_start array1 constraints_ub in
      (* 0 for C style, 1 for Fortran style *)
      let index_style = 0 in
      let nele_jac = Array.length eval_jac_g_structure in
      let nele_hess = Array.length eval_h_structure in
      let try_eval name res =
        try res ; tt_c
        with Callback_failure msg ->
          Printf.eprintf "there was an error in %s: %s\n" name msg ;
          ff_c
      in
      let eval_f_grad_f_c eval eval_name n x obj_value =
        let x' = bigarray_of_ptr array1 n Bigarray.Float64 x in
        let f = bigarray_of_ptr array1 n Bigarray.Float64 obj_value in
        try_eval eval_name (eval x' f)
      in
      let eval_f_c n x _ f _ = eval_f_grad_f_c eval_f "eval_f" n x f in
      let eval_grad_f_c n x _ grad_f _ =
        eval_f_grad_f_c eval_grad_f "eval_grad_f" n x grad_f
      in
      let eval_g_c n x _ m g _ =
        let x' = bigarray_of_ptr array1 n Bigarray.Float64 x in
        let g' = bigarray_of_ptr array1 m Bigarray.Float64 g in
        try_eval "eval_g" (eval_g x' g')
      in
      let compute_structure structure irow jcol =
        Array.iteri
          (fun k (i, j) ->
            irow +@ k <-@ i ;
            jcol +@ k <-@ j )
          structure ;
        ()
      in
      let eval_jac_g_c n x _ _ nele_jac irow jcol values _ =
        if is_null x then (
          (* compute the structure of the Jacobian *)
          compute_structure eval_jac_g_structure irow jcol ;
          tt_c )
        else
          (* compute Jacobian *)
          let x' = bigarray_of_ptr array1 n Bigarray.Float64 x in
          let values' =
            bigarray_of_ptr array1 nele_jac Bigarray.Float64 values
          in
          try_eval "eval_jac_g" (eval_jac_g x' values')
      in
      let eval_h_c n x _ obj_factor m lambda _ nele_hess irow jcol values _ =
        if is_null x then (
          (* compute the structure of the Hessian *)
          compute_structure eval_h_structure irow jcol ;
          tt_c )
        else
          (* compute Hessian *)
          let x' = bigarray_of_ptr array1 n Bigarray.Float64 x in
          let lambda' = bigarray_of_ptr array1 m Bigarray.Float64 lambda in
          let h = bigarray_of_ptr array1 nele_hess Bigarray.Float64 values in
          try_eval "eval_h" (eval_h ~sigma:obj_factor ~x:x' ~lambda:lambda' ~h)
      in
      let p =
        create_ipopt_problem_c n x_L x_U m g_L g_U nele_jac nele_hess
          index_style eval_f_c eval_g_c eval_grad_f_c eval_jac_g_c eval_h_c
      in
      Gc.finalise (fun _ -> free_ipopt_problem_c p) p ;
      p

let add_str_option p key value =
  if add_ipopt_str_option_c p key value = tt_c then ()
  else raise (Invalid_argument "Invalid option")

let add_num_option p key value =
  if add_ipopt_num_option_c p key value = tt_c then ()
  else raise (Invalid_argument "Invalid option")

let add_int_option p key value =
  if add_ipopt_int_option_c p key value = tt_c then ()
  else raise (Invalid_argument "Invalid option")

let solve p ?(g = None) ?(f = None) ?(mult_g = None) ?(mult_lb = None)
    ?(mult_ub = None) x =
  let x' = bigarray_start array1 x in
  let value (ba : vector option) =
    ba
    |> Option.map (bigarray_start array1)
    |> Option.value ~default:(from_voidp number_t null)
  in
  let obj_value = value f in
  let g' = value g in
  let mult_g' = value mult_g in
  let mult_x_L = value mult_lb in
  let mult_x_U = value mult_ub in
  ipopt_solve_c p x' g' obj_value mult_g' mult_x_L mult_x_U null
