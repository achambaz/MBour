#' tlrider: A Companion Package to the Article "A Ride in Targeted Learning Territory"
#'
#' The  'tlrider'  package  provides   two  categories  of  important
#' functions: 
#' 
#' @section  Methods  for  \code{LAW}  objects:
#' An object of \code{class} \code{LAW} describes a law for (W,A,Y) in [0,1] x
#' {0,1} x  [0,1].  \code{Methods} \code{\link{sample_from}} samples  from the
#' law  (if  it is  fully  characterized),  \code{\link{reveal}} reveals  some
#' relevant features  of the  law, \code{\link{alter}} modifies  some relevant
#' features of the law.  Method \code{\link{evaluate_psi}} evaluates the value
#' at  the  law  of  \eqn{Psi}  given by  \deqn{\Psi(P)  =  E_P  {Qbar(1,W)  -
#' Qbar(0,W)}}    with     \eqn{Qbar(A,W)    =    E_P     (Y|A,W)}.     Method
#' \code{\link{evaluate_eic}}  evaluates  the  efficient  influence  curve  of
#' \eqn{Psi}  at the  law,  which is  given  by  \eqn{D^* (P)  =  D_1^* (P)  +
#' D_2^*(P)} where  \deqn{D_1^*(P)(W,A,Y) =  Qbar(1,W) - Qbar(0,W)  - \Psi(P)}
#' and \deqn{D_2^*(P)(W,A,Y)  = \frac{2A-1}{lGbar(A,W)} (Y -  Qbar(A,W))} with
#' \eqn{Gbar(W) = P (A=1|W)} and \eqn{lGbar(A,W) = A  * Gbar(W) + (1 - A) * (1
#' - Gbar(W))}.
#'
#' @section  Estimation routines:
#' Functions to  learn from data sets  sampled from an object  of \code{class}
#' \code{LAW}.    Functions   \code{estimate_QW},   \code{estimate_Gbar}   and
#' \code{estimate_Qbar} respectively  estimate the marginal distribution  of W
#' non-parametrically,  conditional  probability  that  A=1  given  W  (a.k.a.
#' 'Gbar')  and conditional  expectation of  Y given  (A,W) (a.k.a.   'Qbar').
#' Functions    \code{compute_lGbar_hatAW}    and    \code{compute_Qbar_hatAW}
#' respectively  compute  the conditional  likelihood  of  A_i given  W_i  and
#' conditional expectation of Y given (A,W)=(A_i,W_i) at given (A_i,W_i) based
#' on  the  fit  obtained   by  invoking  functions  \code{estimate_Gbar}  and
#' \code{estimate_Qbar}.
#'
#' @docType package
#' 
#' @name tlrider
#'
#' @references Benkeser & Chambaz, "A Ride in Targeted Learning Territory" (2018).
#' 
#' @examples
#'
#' expit <- plogis
#' logit <- qlogis
#' 
#' ## 
#' ## First example
#' ##
#'
#' experiment <- LAW()
#' alter(experiment, 
#'       Gbar = function(W) {
#'         expit(1 + 2 * W - 4 * sqrt(abs((W - 5/12))))
#'       },
#'       Qbar =  function(AW) {
#'         A <- AW[, "A"]
#'         W <- AW[, "W"]
#'         A * (cos((-1/2 + W) * pi) * 2/5 + 1/5 +
#'              (1/3 <= W & W <= 1/2) / 5 +
#'              (W >= 3/4) * (W - 3/4) * 2) +
#'           (1 - A) * (sin(4 * W^2 * pi) / 4 + 1/2) 
#'       },
#'       QW = function(W,
#'                     mixture_weights = c(1/10, 9/10, 0),
#'                     mins = c(0, 11/30, 0),
#'                     maxs = c(1, 14/30, 1)) {
#'         out <- sapply(1:length(mixture_weights),
#'                       function(ii){
#'                         mixture_weights[ii] *
#'                           stats::dunif(W,
#'                                        min = mins[ii],
#'                                        max = maxs[ii])
#'                       })
#'         return(rowSums(out))
#'       },
#'       qY = function(obs, Qbar, shape10 = 2, shape11 = 3){
#'         A <- obs[, "A"]
#'         AW <- obs[, c("A", "W")]
#'         QAW <- Qbar(AW)
#'         shape1 <- ifelse(A == 0, shape10, shape11)
#'         stats::dbeta(Y,
#'                      shape1 = shape1,
#'                      shape2 = shape1 * (1 - QAW) / QAW)
#'       },
#'       sample_from = function(n, ideal = FALSE) {
#'         ## preliminary
#'         n <- R.utils::Arguments$getInteger(n, c(1, Inf))
#'         ideal <- R.utils::Arguments$getLogical(ideal)
#'         ## ## 'Gbar' and 'Qbar' factors
#'         Gbar <- experiment$.Gbar
#'         Qbar <- experiment$.Qbar
#'         ## sampling
#'         ## ## context
#'         params <- formals(experiment$.QW)
#'         mixture_weights <- eval(params$mixture_weights)
#'         mins <- eval(params$mins)
#'         maxs <- eval(params$maxs)
#'         W <- sample_from_mixture_of_uniforms(n, mixture_weights,
#'                                              mins, maxs)
#'         ## ## counterfactual rewards
#'         zeroW <- cbind(A = 0, W)
#'         oneW <- cbind(A = 1, W)
#'         Qbar_zeroW <- Qbar(zeroW)
#'         Qbar_oneW <- Qbar(oneW)
#'         Yzero <- stats::rbeta(n,
#'                               shape1 = 2,
#'                               shape2 = 2 * (1 - Qbar_zeroW) / Qbar_zeroW)
#'         Yone <- stats::rbeta(n,
#'                              shape1 = 3,
#'                              shape2 = 3 * (1 - Qbar_oneW) / Qbar_oneW)
#'         ## ## action undertaken
#'         A <- stats::rbinom(n, size = 1, prob = Gbar(W))
#'         ## ## actual reward
#'         Y <- A * Yone + (1 - A) * Yzero
#'         ## ## observation
#'         if (ideal) {
#'           obs <- cbind(W = W, Yzero = Yzero, Yone = Yone, A = A, Y = Y)
#'         } else {
#'           obs <- cbind(W = W, A = A, Y = Y)
#'         }
#'         return(obs)
#'       })
#' (sample_from(experiment, n = 3))
#' (evaluate_psi(experiment))
#'
#' ##
#' ## Second example
#' ## 
#'
#' another_experiment <- LAW()
#' alter(another_experiment,
#'       Gbar = function(W) {
#'         sin((1 + W) * pi / 6)
#'       },
#'       Qbar =   function(AW, h) {
#'         A <- AW[, "A"]
#'         W <- AW[, "W"]
#'         expit( logit( A *  W + (1 - A) * W^2 ) +
#'                h * 10 * sqrt(W) * A )
#'       },
#'       QW = function(x, min = 1/10, max = 9/10){
#'              stats::dunif(x, min = min, max = max)
#'       },
#'       qY = function(obs, Qbar, shape1 = 4){
#'         AW <- obs[, c("A", "W")]
#'         QAW <- Qbar(AW)
#'         stats::gdbeta(Y,
#'                       shape1 = shape1,
#'                       shape2 = shape1 * (1 - QAW) / QAW)
#'       },
#'       sample_from = function(n, h) {
#'         ## preliminary
#'         n <- R.utils::Arguments$getInteger(n, c(1, Inf))
#'         h <- R.utils::Arguments$getNumeric(h)
#'         ## ## 'Gbar' and 'Qbar' factors
#'         Gbar <- another_experiment$.Gbar
#'         Qbar <- another_experiment$.Qbar
#'         ## sampling
#'         ## ## context
#'         params <- formals(another_experiment$.QW)
#'         W <- stats::runif(n, min = eval(params$min),
#'                    max = eval(params$max))
#'         ## ## action undertaken
#'         A <- stats::rbinom(n, size = 1, prob = Gbar(W))
#'         ## ## reward
#'         params <- formals(another_experiment$.qY)
#'         shape1 <- eval(params$shape1)
#'         QAW <- Qbar(cbind(A = A, W = W), h = h)
#'         Y <- stats::rbeta(n,
#'                           shape1 = shape1,
#'                           shape2 = shape1 * (1 - QAW) / QAW)
#'         ## ## observation
#'         obs <- cbind(W = W, A = A, Y = Y)
#'         return(obs)
#'       })
#'
#' (sample_from(another_experiment, n = 3, h = 0))
#' (evaluate_psi(another_experiment, h = 0))
#' 
#' ## Object 'another_experiment' is parametrized by the real number 'h'
#' ## (see argument 'h' in entry 'sample_from' above), hence it characterizes
#' ## a one-dimensional model. It happens that the score of the model equals
#' ## (up to a constant) function 'sigma0' defined below with its argument
#' ## 'law' set to 'another_experiment':
#'
#' sigma0 <- function(obs, law = another_experiment) {
#'   ## preliminary
#'   Qbar <- get_feature(law, "Qbar", h = 0)
#'   QAW <- Qbar(obs[, c("A", "W")])
#'   params <- formals(get_feature(law, "qY", h = 0))
#'   shape1 <- eval(params$shape1)
#'   ## computations
#'   betaAW <- shape1 * (1 - QAW) / QAW
#'   out <- log(1 - obs[, "Y"])
#'   for (int in 1:shape1) {
#'     out <- out + 1/(int - 1 + betaAW)
#'   }
#'   out <- - out * shape1 * (1 - QAW) / QAW *
#'            10 * sqrt(obs[, "W"]) * obs[, "A"]
#'   ## no need to center given how we will use it
#'   return(out)
#'  }
NULL
