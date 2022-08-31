#' Sampling surveillance design class builder
#'
#' Builds a class to represent a surveillance design functionality for the
#' effective allocation of surveillance sampling across one or more
#' divisions (parts, locations, categories, etc.) via Lagrange-based methods
#' for optimizing objective functions specified with surveillance and/or
#' incursion management costs, benefits, detection sensitivities, and/or
#' overall detection confidence.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of a bio-security surveillance and area freedom design.
#' @param divisions A \code{Divisions} or inherited class object representing
#'   one or more divisions (parts, locations, categories, etc.) for the
#'   surveillance design.
#' @param establish_pr A vector of (relative) probability values to represent
#'   the likelihood of pest establishment at each division part (location,
#'   category, etc.) specified by \code{divisions}. Values are assumed to be
#'   relative when their maximum is greater than 1, or an attribute
#'   \code{relative = TRUE} is attached to the parameter.
#' @param sensitivity A vector of sample sensitivity values for each division
#'   part (location, category, etc.) specified by \code{divisions}. Default
#'   is \code{1}.
#' @param prevalence A vector of discrete sampling design prevalence values for
#'   each division part (location, category, etc.) specified by
#'   \code{divisions}. Note that this parameter may represent apparent
#'   prevalence (Cannon, 2009) when the \code{sensitivity} is set to \code{1}.
#'   Default is \code{NULL}.
#' @param total_indiv A vector of total individual discrete sampling units
#'   (e.g. trees, traps) present at each division part (location, category,
#'   etc.) specified by \code{divisions}. Default is \code{NULL}.
#' @param design_dens A vector of continuous sampling design density values for
#'   each division part (location, category, etc.). Default is \code{NULL}.
#' @param sample_area The area of a single sample in a continuous sampling
#'   design. Note that when set to 1, the total number of samples will be
#'   equivalent to the total area sampled. Default is \code{NULL}.
#' @param optimal The strategy used for finding an effective sampling
#'   allocation. One of (minimum) \code{"cost"}, (maximum) \code{"benefit"},
#'   or (maximum) \code{"detection"} sensitivity (up to \code{"confidence"}
#'   level when specified).
#' @param mgmt_cost A list of vectors to represent estimated management costs
#'   for when the incursion is detected and undetected. Each vector specifies
#'   costs at each division part (location, category, etc.) specified by
#'   \code{divisions}. List elements should be named \code{detected} and
#'   \code{undetected}. Default is an empty list. An attribute \code{units}
#'   may be used to specify the cost units (e.g. "$" or "hours").
#' @param benefit A vector of values quantifying the benefit of detection
#'   at each division part (location, category, etc.) specified by
#'   \code{divisions}. Default is \code{NULL}. An attribute \code{units} may
#'   be used to specify the benefit units (e.g. "$" or "hours").
#' @param alloc_units The units for the allocated surveillance resource costs
#'   (e.g. "$", "hours") consistent with the \code{context}. This may be
#'   different to those specified for \code{mgmt_cost} or \code{benefit}.
#'   Default is \code{NULL}.
#' @param sample_cost The cost of individual samples. Default is \code{NULL}.
#'   Units are specified in \code{alloc_units}.
#' @param fixed_cost A vector of fixed costs, such as travel costs or time, at
#'   each division part (location, category, etc.) specified by
#'   \code{divisions}. Default is \code{NULL}. Units are specified in
#'   \code{alloc_units}.
#' @param budget The cost budget or constraint for the sampling allocation in
#'   the surveillance design. Default is \code{NULL}. Units are specified in
#'   \code{alloc_units}.
#' @param confidence The desired (minimum) system detection sensitivity or
#'   confidence of the surveillance design (e.g. 0.95). Default is \code{NULL}.
#' @param exist_sens A vector of detection sensitivity values of existing
#'   surveillance present at each division part (location, category, etc.)
#'   specified by \code{divisions}. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{SamplingSurvDesign} class object (list) containing inherited
#'   and extended functions from the base \code{LagrangeSurvDesign} class for
#'   for allocating samples, and calculating (unit and overall) detection
#'   sensitivities:
#'   \describe{
#'     \item{\code{get_allocation()}}{Get allocated samples via specified
#'       strategy, utilizing costs, benefits, budget constraints, and/or
#'       desired confidence level.}
#'     \item{\code{get_sensitivity()}}{Get the division part detection
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
#' @include LagrangeSurvDesign.R
#' @export
SamplingSurvDesign <- function(context,
                               divisions,
                               establish_pr,
                               sensitivity = 1,
                               prevalence = NULL,
                               total_indiv = NULL,
                               design_dens = NULL,
                               sample_area = NULL,
                               optimal = c("cost", "benefit", "detection"),
                               mgmt_cost = NULL,
                               benefit = NULL,
                               alloc_units = NULL,
                               sample_cost = NULL,
                               fixed_cost = NULL,
                               budget = NULL,
                               confidence = NULL,
                               exist_sens = NULL,
                               class = character(), ...) {
  UseMethod("SamplingSurvDesign")
}

#' @name SamplingSurvDesign
#' @export
SamplingSurvDesign.Context <- function(context,
                                       divisions,
                                       establish_pr,
                                       sensitivity = 1,
                                       prevalence = NULL,
                                       total_indiv = NULL,
                                       design_dens = NULL,
                                       sample_area = NULL,
                                       optimal = c("cost", "benefit", "detection"),
                                       mgmt_cost = NULL,
                                       benefit = NULL,
                                       alloc_units = NULL,
                                       sample_cost = NULL,
                                       fixed_cost = NULL,
                                       budget = NULL,
                                       confidence = NULL,
                                       exist_sens = NULL,
                                       class = character(), ...) {

  # Build via base class
  self <- LagrangeSurvDesign(context = context,
                             divisions = divisions,
                             establish_pr = establish_pr,
                             lambda = lambda, # TODO
                             optimal = optimal,
                             mgmt_cost = mgmt_cost,
                             benefit = benefit,
                             alloc_units = alloc_units,
                             fixed_cost = fixed_cost,
                             budget = budget,
                             confidence = confidence,
                             exist_sens = exist_sens,
                             class = "SamplingSurvDesign", ...)

  # Get the allocated surveillance resource values of the surveillance design
  self$get_allocation <- function() {
    # overridden in inherited classes
  }

  # Get the division part detection sensitivities of the surveillance design
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
