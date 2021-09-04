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

val application_return_status_retcode : application_return_status -> int

exception Callback_failure of string

type problem

type vector =
  ( float
  , Bigarray.float64_elt
  , Bigarray_compat.c_layout )
  Bigarray_compat.Array1.t

val create_problem :
     eval_f:(vector -> vector -> unit)
  -> eval_grad_f:(vector -> vector -> unit)
  -> eval_g:(vector -> vector -> unit)
  -> eval_jac_g_structure:(int * int) array
  -> eval_jac_g:(vector -> vector -> unit)
  -> eval_h_structure:(int * int) array
  -> eval_h:(sigma:float -> x:vector -> lambda:vector -> h:vector -> unit)
  -> lb:vector
  -> ub:vector
  -> constraints_lb:vector
  -> constraints_ub:vector
  -> problem

val add_str_option : problem -> string -> string -> unit

val add_num_option : problem -> string -> float -> unit

val add_int_option : problem -> string -> int -> unit

val solve :
     problem
  -> ?g:vector option
  -> ?f:vector option
  -> ?mult_g:vector option
  -> ?mult_lb:vector option
  -> ?mult_ub:vector option
  -> vector
  -> application_return_status
