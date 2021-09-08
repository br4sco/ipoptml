(** OCaml interface to the IPOPT constrained Non-Linear Program (NLP) solver.
  @see <https://coin-or.github.io/Ipopt/index.html> for the IPOPT documentation. *)

(** Return status for the Optimize call for an application. *)
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

(** Maps a return status to the return code of the underlying IPOPT
    implementation.
    @see <https://coin-or.github.io/Ipopt/IpReturnCodes__inc_8h_source.html>
    [enum ApplicationReturnStatus] *)
val application_return_status_retcode : application_return_status -> int

(** Exception that should be thrown by failing callback functions. *)
exception Callback_failure of string

(** Constrained Non-Linear Program. *)
type nlp

(** Alias for floating point {!Bigarray.Array1}. *)
type vec =
  ( float
  , Bigarray.float64_elt
  , Bigarray_compat.c_layout )
  Bigarray_compat.Array1.t

(** Callback function to evaluate the objective function f(x).
   The function argument [x] is the values of the variables. *)
type eval_f_t = vec -> float

(** Callback function to evaluate the gradient 𝛁f(x) of the objective
   function. The function arguments are:
   - [x], the values of the variables,
   - [grad_f], a vector for storing the values of 𝛁f(x), in the same order
             as [x]. I.e. [grad_f.{i}] should hold the value of the gradient of
             f(x) with respect to [x.{i}].  *)
type eval_grad_f_t = vec -> vec -> unit

(** Callback function to evaluate the constraints g(x). The function arguments
   are:
   - [x], the values of the variables,
   - [g], a vector for storing the value of g(x).  *)
type eval_g_t = vec -> vec -> unit

(** Encodes the structure of a sparse matrix. The tuple [(i, j)] in position [k]
   associates the matrix element [m.(i, j)] with the array element [a.(k)]. If
   [(i, j)] appears multiple times in the structure array, [m.(i, j)] is
   associated with the sum of these elements in [a]. Matrix elements missing
   from the structure array are assumed to be zero.
   @see <https://coin-or.github.io/Ipopt/IMPL.html#TRIPLET> for documentation on
   the triplet format. *)
type structure_t = (int * int) array

(** Callback function to evaluate the Jacobian 𝛁g(x){^T} of the
   constraints. The function arguments are:
   - [x], the values of the variables,
   - [jac_g], a vector for storing the non-zero values of 𝛁g(x){^T},
              where [jac_g] assumes some pre-defined {!structure_t}. *)
type eval_jac_g_t = vec -> vec -> unit

(** Callback function to evaluate the Hessian
   σ𝛁{^2}f(x{_k}) + Σ{_i}\[λ{_i}𝛁{^2}g{_i}(x{_k})\]
   of the Lagrangian. The function arguments are:
   - [sigma], the factor σ in front of the objective term,
   - [x], the values of the variables,
   - [lambda], the values of the constraint multiplier λ,
   - [h], a vector for storing the non-zero values of the Hessian, where [h]
        assumes some pre-defined {!structure_t}. This matrix is symmetric and
        only the lower diagonal entries must be specified. *)
type eval_h_t = sigma:float -> x:vec -> lambda:vec -> h:vec -> unit

(** Creates a constrained NLP:
  min{_x}\[f(x)\] s.t. xL{_k} ≤ x{_k} ≤ xU{_k} and gL{_i} ≤ g{_i}(x) ≤ gU{_i}.
  @param eval_f Callback function to evaluate objective function {!eval_f_t}.
  @param eval_grad_f
    Callback function to evaluate the gradient of the objective function
    {!eval_grad_f_t}.
  @param eval_g
    Callback function to evaluate the constraint function {!eval_g_t}.
  @param jac_g_structure
    Structure of the constraint Jacobian {!structure_t}.
  @param eval_jac_g
    Callback function to evaluate the Jacobian of the constraint function
    {!eval_jac_g_t}.
  @param h_structure
    Structure of the Hessian of the Lagrangian {!structure_t}.
  @param eval_h
    Callback function to evaluate the Hessian of the Lagrangian {!eval_h_t}.
  @param lb Lower bounds on the variables xL{_k}.
  @param ub Upper bounds on the variables xU{_k}.
  @param constraints_lb Lower bounds on the constraints gL{_i}.
  @param constraints_ub Upper bounds on the constraints gU{_i}.
  @return A constrained NLP. *)
val create_nlp :
     eval_f:eval_f_t
  -> eval_grad_f:eval_grad_f_t
  -> eval_g:eval_g_t
  -> jac_g_structure:structure_t
  -> eval_jac_g:eval_jac_g_t
  -> h_structure:structure_t
  -> eval_h:eval_h_t
  -> lb:vec
  -> ub:vec
  -> constraints_lb:vec
  -> constraints_ub:vec
  -> nlp

(** [add_str_option p key val] sets the option [key] to the [string] value [val]
  in the NLP [p].
  @raise Invalid_argument If the option is invalid.
  @see <https://coin-or.github.io/Ipopt/OPTIONS.html> for a summary of options. *)
val add_str_option : nlp -> string -> string -> unit

(** As {!add_str_option}, but for [float] values. *)
val add_num_option : nlp -> string -> float -> unit

(** As {!add_str_option}, but for [int] values. *)
val add_int_option : nlp -> string -> int -> unit

(** [solve p x] tries to solve the constrained NLP [p] with initial values
   [x].
   @param p The constrained NLP.
   @param g A vector for storing the final values of the constraints.
   @param mult_g
    Initial values of the constraint multipliers (only if warm start option is
    chosen). Will hold final values of the constraint multipliers after calling
    [solve].
   @param mult_lb
    Initial values for the multiplies of the variable lower bounds (only if warm
    start option is chosen). Will hold final values for the multipliers of the
    variable lower bounds after calling [solve].
   @param mult_ub
    Initial values for the multiplies of the variable upper bounds (only if warm
    start option is chosen). Will hold final values for the multipliers of the
    variable upper bounds after calling [solve].
   @param x
    Initial values of the variables. Will hold the optimal solution after
    calling [solve].
   @return
    The tuple [(rs, f)], where [rs] is the return status
    {!application_return_status} and [f] is the final value of the objective
    function.
 *)
val solve :
     nlp
  -> ?g:vec option
  -> ?mult_g:vec option
  -> ?mult_lb:vec option
  -> ?mult_ub:vec option
  -> vec
  -> application_return_status * float
