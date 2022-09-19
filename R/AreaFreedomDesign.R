#' Area freedom design base class builder
#'
#' Builds a base class to represent area freedom design functionality for
#' assessing probabilities relating to the presence or absence of an invasive
#' species over a sequence time intervals or applications of a surveillance
#' system.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of a bio-security surveillance and area freedom design.
#' @param detected A logical (> 0) temporal vector recording when the invasive
#'   species was detected/sighted at previous intervals. Default is
#'   \code{FALSE} implying nothing detected.
#' @param pr_detect The probability of detecting the invasive species given its
#'   presence. Also known as system sensitivity or detection confidence for
#'   surveillance systems. Default is \code{NULL} implying only detection
#'   records are available.
#' @param pr_persist The probability that the invasive species persists at each
#'   time interval (specified by the \code{time_unit} parameter in the
#'   \code{context}). Default is \code{1} implies that the invasive species
#'   will persist across time intervals if present, representing the worst case
#'   scenario when persistence probability is unknown.
#' @param iterations The number of time intervals (specified by the
#'   \code{time_unit} parameter in the \code{context}), or sequential
#'   surveillance system applications, used to estimate the likelihood
#'   of area freedom. Default is \code{NULL} implying another stopping
#'   mechanism is utilized, such as a threshold probability or target
#'   confidence in freedom (see inherited classes).
#' @param ... Additional parameters.
#' @return A \code{AreaFreedomDesign} base class object (list) containing
#'   functions to provide evidence for area freedom, given detection records,
#'   over a number of iterations, including the likelihood an invasive species
#'   evades detection (for hypothesis testing), or the confidence in freedom
#'   (obtained via Bayesian approaches). The following template functions are
#'   provided in the base class:
#'   \describe{
#'     \item{\code{get_evidence()}}{Get a sequence of values that provide
#'       evidence for area freedom, for each iteration or time interval
#'       specified, or for the iterations required to satisfy the stopping
#'       criteria (see inherited classes).}
#'     \item{\code{get_iterations()}}{Get the number of time intervals, or
#'       sequential surveillance system applications, used to provide evidence
#'       of area freedom. The value returned will be either the specified
#'       \code{iterations} parameter, or the iterations recorded when a
#'       specified stopping criteria for the freedom evidence has been
#'       satisfied, such as a threshold probability or target confidence in
#'       freedom (see inherited classes).}
#'   }
#' @references
#'   Rout, T. (2017). Declaring Eradication of an Invasive Species. In
#'   A. Robinson, T. Walshe, M. Burgman, & M. Nunn (Eds.),
#'   \emph{Invasive Species: Risk Assessment and Management} (pp. 334-347).
#'   Cambridge: Cambridge University Press.
#'   \doi{doi.org/10.1017/9781139019606.017}
#' @include Context.R
#' @export
AreaFreedomDesign <- function(context,
                              detected = FALSE,
                              pr_detect = NULL,
                              pr_persist = 1,
                              iterations = NULL,
                              class = character(), ...) {
  UseMethod("AreaFreedomDesign")
}

#' @name AreaFreedomDesign
#' @export
AreaFreedomDesign.Context <- function(context,
                                      detected = FALSE,
                                      pr_detect = NULL,
                                      pr_persist = 1,
                                      iterations = NULL,
                                      class = character(), ...) {

  # Check parameters
  detected <- as.logical(detected)
  if (any(is.na(detected)) && length(detected) < 1) {
    stop(paste("The temporal detected parameter should be a logical vector",
               "with length >= 1."), call. = FALSE)
  }
  if (!is.null(pr_detect) &&
      (!is.numeric(pr_detect) || pr_detect < 0 || pr_detect > 1)) {
    stop(paste("The probability of detection parameter must be numeric, >= 0,",
               "and <= 1."), call. = FALSE)
  }
  if (!is.null(pr_persist) &&
      (!is.numeric(pr_persist) || pr_persist < 0 || pr_persist > 1)) {
    stop(paste("The probability of persistence parameter must be numeric,",
               ">= 0, and <= 1."), call. = FALSE)
  }
  if (!is.null(iterations) &&
      (!is.numeric(iterations) || pr_detect < 1)) {
    stop("The iterations parameter must be numeric and >= 1.", call. = FALSE)
  }

  # Create a class structure
  self <- structure(list(), class = c(class, "AreaFreedomDesign"))

  # Get a sequence of values that provide evidence for area freedom
  self$get_evidence <- function() {
    # overridden in inherited classes
  }

  # Get the number of time intervals or surveillance system sequences
  self$get_iterations <- function() {
    # overridden in inherited classes
  }

  return(self)
}
