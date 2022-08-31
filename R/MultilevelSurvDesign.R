#' Multilevel sampling surveillance design class builder
#'
#' Builds a generic class to represent a surveillance design functionality for
#' the effective allocation of surveillance sampling across one or more
#' levels or stages via multilevel sampling methods specified with surveillance
#' and/or incursion management costs, benefits, detection sensitivities, and/or
#' overall detection confidence.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of a bio-security surveillance and area freedom design.
#' @param levels A \code{Divisions} or inherited class object representing
#'   one or more multistage levels, for the surveillance design. The levels
#'   should be ordered from lowest to highest (e.g. leaves, trees, rows,
#'   orchards). Only the lowest level can utilize either continuous
#'   density-based or discrete sampling, higher levels can only utilize
#'   discrete sampling. Note that more than five levels may not be
#'   computationally feasible.
#' @param sensitivity A single sample sensitivity value for the lowest level
#'  of a multilevel/stage sampling when \code{multilevel} is enabled. Default
#'  is \code{1}.
#' @param prevalence A vector of discrete sampling design prevalence values for
#'   each multistage level specified by \code{divisions}. Note that this
#'   parameter may represent apparent prevalence (Cannon, 2009) when the
#'   \code{sensitivity} is set to \code{1}. Default is \code{NULL}.
#' @param total_indiv A vector of total individual discrete sampling units
#'   (e.g. leaves, trees, rows, orchards) present at each multistage level
#'   specified by \code{divisions}. Default is \code{NULL}.
#' @param design_dens A single design density value for the lowest level of the
#'   multilevel sampling. Default is \code{NULL}.
#' @param sample_area The area of a single sample in a continuous sampling
#'   design. Note that when set to 1, the total number of samples will be
#'   equivalent to the total area sampled. Default is \code{NULL}.
#' @param sample_cost The cost of individual samples. Default is \code{1}.
#'   An attribute \code{units} may be used to specify the cost units (e.g. "$"
#'   or "hours").
#' @param confidence The desired (minimum) system detection sensitivity or
#'   confidence of the surveillance design. Default is \code{0.95}.
#' @param ... Additional parameters.
#' @return A \code{MultilevelSurvDesign} class object (list) containing
#'   inherited and extended functions from the base \code{SurveillanceDesign}
#'   class for for allocating resources, and calculating (unit and overall)
#'   detection sensitivities:
#'   \describe{
#'     \item{\code{get_allocation()}}{Get allocated surveillance resources via
#'       specified strategy, utilizing costs, benefits, budget constraints,
#'       and/or desired confidence level.}
#'     \item{\code{get_sensitivity()}}{Get the unit/location detection
#'        sensitivities of the allocated surveillance design.}
#'     \item{\code{get_confidence()}}{Get the overall system sensitivity or
#'       confidence of the allocated surveillance design.}
#'   }
#' @references
#'   Cannon, R. M. (2009). Inspecting and monitoring on a restricted budget -
#'   where best to look? \emph{Preventive Veterinary Medicine}, 92(1–2),
#'   163-174. \doi{10.1016/j.prevetmed.2009.06.009}
#'
#'   Kean, J. M., Burnip, G. M. & Pathan, A. (2015). Detection survey design
#'   for decision making during biosecurity incursions. In F. Jarrad,
#'   S. Low-Choy & K. Mengersen (eds.),
#'   \emph{Biosecurity surveillance: Quantitative approaches} (pp. 238– 250).
#'   Wallingford, UK: CABI.
#' @include SurveillanceDesign.R
#' @export
MultilevelSurvDesign <- function(context,
                                 levels,
                                 sensitivity = 1,
                                 prevalence = NULL,
                                 total_indiv = NULL,
                                 design_dens = NULL,
                                 sample_area = NULL,
                                 sample_cost = 1,
                                 confidence = 0.95,
                                 class = character(), ...) {
  UseMethod("MultilevelSurvDesign")
}

#' @name MultilevelSurvDesign
#' @export
MultilevelSurvDesign.Context <- function(context,
                                         levels,
                                         sensitivity = 1,
                                         prevalence = NULL,
                                         total_indiv = NULL,
                                         design_dens = NULL,
                                         sample_area = NULL,
                                         sample_cost = 1,
                                         confidence = 0.95,
                                         class = character(), ...) {

  # Build via base class
  self <- SurveillanceDesign(context = context,
                             divisions = levels,
                             establish_pr = 1,
                             confidence = confidence,
                             class = "MultilevelSurvDesign", ...)

  # Get the allocated surveillance resource values of the surveillance design
  self$get_allocation <- function() {
    # overridden in inherited classes
  }

  # Get the unit/location detection sensitivities of the surveillance design
  self$get_sensitivity <- function() {
    # overridden in inherited classes
  }

  # Get the overall system sensitivity/confidence of the surveillance design
  self$get_confidence <- function() {
    system_sens <- NULL
    return(system_sens)
  }

  return(self)
}
