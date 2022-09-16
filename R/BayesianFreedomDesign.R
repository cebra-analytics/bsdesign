#' Bayesian area freedom design base class builder
#'
#' Builds a class to represent area freedom design functionality utilizing
#' Bayesian approaches to assess the likelihood of freedom, or an invasive
#' species being absent when it has not been detected for a sequence of time
#' intervals or applications of a surveillance system.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of a bio-security surveillance and area freedom design.
#' @param detected A logical (> 0) temporal vector recording when the invasive
#'   species was detected/sighted at previous intervals. Default is
#'   \code{FALSE} implying nothing detected.
#' @param pr_detect The probability of detecting the invasive species given its
#'   presence. Also known as system sensitivity or detection confidence for
#'   a surveillance system. Default is \code{NULL} implying only detection
#'   records are available.
#' @param pr_freedom The prior probability of invasive species freedom or
#'   absence used in the first iteration of the Bayesian process. Values are
#'   typically estimated via expert elicitation. Default is \code{0.5} for an
#'   uninformed prior.
#' @param iterations The number of time intervals (specified by the
#'   \code{time_unit} parameter in the \code{context}), or sequential
#'   surveillance system applications, used to estimate the likelihood of area
#'   freedom. Default is \code{NULL} implying \code{confidence} will be
#'   utilized as a stopping mechanism.
#' @param confidence The target confidence level (e.g. 0.95) in area freedom,
#'   or the probability of freedom (absence) given a sequence of no detection
#'   via a surveillance system. Default is \code{NULL} implying that the
#'   \code{iterations} parameter will be utilized as a stopping mechanism.
#' @param ... Additional parameters.
#' @return A \code{BayesianFreedomDesign} class object (list) containing
#'   inherited and extended functions from the base \code{AreaFreedomDesign}
#'   class to provide evidence for area freedom, given detection records,
#'   over a number of iterations, via Bayesian approaches:
#'   \describe{
#'     \item{\code{get_iterations()}}{Get the number of time intervals, or
#'       sequential surveillance system applications, used to provide evidence
#'       for area freedom. The value returned will be either the specified
#'       \code{iterations} parameter, or the iterations recorded when the
#'       specified target \code{confidence} level for absence given no
#'       detection is reached.}
#'     \item{\code{get_evidence()}}{Get a sequence of values of confidence in
#'       area freedom, or probability of absence given no detection of an
#'       invasive species, for each iteration or time interval specified, or
#'       for the iterations required to satisfy the target \code{confidence}.}
#'   }
#' @references
#'   Anderson, D. P., Ramsey, D. S. L., Nugent, G., Bosson, M., Livingstone,
#'   P., Martin, P. A. J., Sergeant, E., Gormley, A. M., & Warburton, B.
#'   (2013). A novel approach to assess the probability of disease eradication
#'   from a wild-animal reservoir host. \emph{Epidemiology and Infection},
#'   141(7), 1509–1521. \doi{10.1017/S095026881200310X}
#'
#'   Anderson, D. P., Gormley, A. M., Ramsey, D. S. L., Nugent, G., Martin,
#'   P. A. J., Bosson, M., Livingstone, P., & Byrom, A. E. (2017).
#'   Bio-economic optimisation of surveillance to confirm broadscale
#'   eradications of invasive pests and diseases. \emph{Biological Invasions},
#'   19(10), 2869–2884. \doi{10.1007/s10530-017-1490-5}
#'
#'   Anderson, D. P., Pepper, M. A., Travers, S., Michaels, T. A., Sullivan,
#'   K., & Ramsey, D. S. L. (2022). Confirming the broadscale eradication
#'   success of nutria (Myocastor coypus) from the Delmarva Peninsula, USA.
#'   \emph{Biological Invasions}. \doi{10.1007/s10530-022-02855-x}
#'
#'   Rout, T. (2017). Declaring Eradication of an Invasive Species. In
#'   A. Robinson, T. Walshe, M. Burgman, & M. Nunn (Eds.),
#'   \emph{Invasive Species: Risk Assessment and Management} (pp. 334-347).
#'   Cambridge: Cambridge University Press.
#'   \doi{doi.org/10.1017/9781139019606.017}
#'
#'   Solow, A. R. (1993). Inferring Extinction from Sighting Data.
#'   \emph{Ecology}, 74(3), 962–964. https://doi.org/10.2307/1940821
#' @include AreaFreedomDesign.R
#' @export
BayesianFreedomDesign <- function(context,
                                  detected = FALSE,
                                  pr_detect = NULL,
                                  pr_freedom = 1,
                                  iterations = NULL,
                                  confidence = NULL, ...) {
  UseMethod("BayesianFreedomDesign")
}

#' @name BayesianFreedomDesign
#' @export
BayesianFreedomDesign.Context <- function(context,
                                          detected = FALSE,
                                          pr_detect = NULL,
                                          pr_freedom = 1,
                                          iterations = NULL,
                                          confidence = NULL, ...) {

  # Build via base class (for checks)
  self <- AreaFreedomDesign(context = context,
                            detected = detected,
                            pr_detect = pr_detect,
                            iterations = iterations,
                            class = "BayesianFreedomDesign", ...)

  # Check class parameters
  if (!is.null(pr_freedom) &&
      (!is.numeric(pr_freedom) || pr_freedom < 0 || pr_freedom > 1)) {
    stop(paste("The prior probability of freedom parameter must be numeric,",
               ">= 0, and <= 1."), call. = FALSE)
  }
  if (!is.null(confidence) &&
      (!is.numeric(confidence) || confidence < 0 || confidence > 1)) {
    stop(paste("The confidence of freedom parameter must be numeric, >= 0,",
               "and <= 1."), call. = FALSE)
  }

  # Get the number of time intervals or surveillance system sequences
  self$get_iterations <- function() {
    # TODO ####
  }

  # Get a sequence of values that provide evidence for area freedom
  self$get_evidence <- function() {
    # TODO ####
  }

  return(self)
}


