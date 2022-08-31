#' Spatial surveillance design class builder
#'
#' Builds a class to represent a surveillance design functionality for the
#' effective allocation of surveillance resources across spatial locations
#' via Lagrange-based methods for optimizing objective functions specified
#' with surveillance and/or incursion management costs, benefits, detection
#' sensitivities, and/or overall detection confidence.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of a bio-security surveillance and area freedom design.
#' @param divisions A \code{Divisions} or inherited class object representing
#'   spatial locations (grid-cells or sub-regions/patches) for the surveillance
#'   design.
#' @param establish_pr A vector of (relative) probability values to represent
#'   the likelihood of pest establishment at each spatial location specified
#'   by \code{divisions}. Values are assumed to be relative when their
#'   maximum is greater than 1, or an attribute \code{relative = TRUE} is
#'   attached to the parameter.
#' @param lambda A vector of efficacy or detection rates for each spatial
#'   location specified by \code{divisions}, such that the probability of
#'   detecting an incursion when present at a part can be expressed via
#'   \code{pr(detect|presence) = 1 - exp(-lambda*allocation)},
#'   for a given allocation of surveillance resources.
#' @param optimal The strategy used for finding an effective surveillance
#'   resource allocation. One of (minimum) \code{"cost"}, (maximum)
#'   \code{"benefit"}, or (maximum) \code{"detection"} sensitivity (up to
#'   \code{"confidence"} level when specified).
#' @param mgmt_cost A list of vectors to represent estimated management costs
#'   for when the incursion is detected and undetected. Each vector specifies
#'   these costs at each spatial location specified by \code{divisions}. List
#'   elements should be named \code{detected} and \code{undetected}. Default is
#'   an empty list. An attribute \code{units} may be used to specify the cost
#'   units (e.g. "$" or "hours").
#' @param benefit A vector of values quantifying the benefit of detection
#'   at each spatial location specified by \code{divisions}. Default is
#'   \code{NULL}. An attribute \code{units} may be used to specify the benefit
#'   units (e.g. "$" or "hours").
#' @param alloc_units The units for the allocated surveillance resource costs
#'   (e.g. "$", "hours") consistent with the \code{context}. This may be
#'   different to those specified for \code{mgmt_cost} or \code{benefit}.
#'   Default is \code{NULL}.
#' @param fixed_cost A vector of fixed costs, such as travel costs or time, for
#'   each spatial location specified by \code{divisions}. Default is
#'   \code{NULL}. Units are specified in \code{alloc_units}.
#' @param budget The cost budget or constraint for the resource allocation in
#'   the surveillance design. Default is \code{NULL}. Units are specified in
#'   \code{alloc_units}.
#' @param confidence The desired (minimum) system detection sensitivity or
#'   confidence of the surveillance design (e.g. 0.95). Default is \code{NULL}.
#' @param exist_sens A vector of detection sensitivity values of existing
#'   surveillance present at each spatial location specified by
#'   \code{divisions}. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{SpatialSurvDesign} class object (list) containing inherited
#'   and extended functions from the base \code{LagrangeSurvDesign} class for
#'   for allocating resources, and calculating (location and overall) detection
#'   sensitivities:
#'   \describe{
#'     \item{\code{get_allocation()}}{Get allocated surveillance resources via
#'       specified strategy, utilizing costs, benefits, budget constraints,
#'       and/or desired confidence level.}
#'     \item{\code{get_sensitivity()}}{Get the location detection sensitivities
#'       of the allocated surveillance design.}
#'     \item{\code{get_confidence()}}{Get the overall system sensitivity or
#'       confidence of the allocated surveillance design.}
#'   }
#' @references
#'   Hauser, C. E., & McCarthy, M. A. (2009). Streamlining 'search and
#'   destroy': cost-effective surveillance for invasive species management.
#'   \emph{Ecology Letters}, 12(7), 683–692.
#'   \doi{10.1111/j.1461-0248.2009.01323.x}
#'
#'   McCarthy, M. A., Thompson, C. J., Hauser, C., Burgman, M. A., Possingham,
#'   H. P., Moir, M. L., Tiensin, T., & Gilbert, M. (2010). Resource allocation
#'   for efficient environmental management. \emph{Ecology Letters}, 13(10),
#'   1280–1289. \doi{10.1111/j.1461-0248.2010.01522.x}
#'
#'   Moore, A. L., McCarthy, M. A., & Lecomte, N. (2016). Optimizing ecological
#'   survey effort over space and time.
#'   \emph{Methods in Ecology and Evolution}, 7(8), 891–899.
#'   \doi{10.1111/2041-210X.12564}
#' @include LagrangeSurvDesign.R
#' @export
SpatialSurvDesign <- function(context,
                              divisions,
                              establish_pr,
                              lambda,
                              optimal = c("cost", "benefit", "detection"),
                              mgmt_cost = NULL,
                              benefit = NULL,
                              alloc_units = NULL,
                              fixed_cost = NULL,
                              budget = NULL,
                              confidence = NULL,
                              exist_sens = NULL,
                              class = character(), ...) {
  UseMethod("SpatialSurvDesign")
}

#' @name SpatialSurvDesign
#' @export
SpatialSurvDesign.Context <- function(context,
                                      divisions,
                                      establish_pr,
                                      lambda,
                                      optimal = c("cost", "benefit",
                                                  "detection"),
                                      mgmt_cost = NULL,
                                      benefit = NULL,
                                      alloc_units = NULL,
                                      fixed_cost = NULL,
                                      budget = NULL,
                                      confidence = NULL,
                                      exist_sens = NULL,
                                      class = character(), ...) {

  # Build via base class
  self <- LagrangeSurvDesign(context = context,
                             divisions = divisions,
                             establish_pr = establish_pr,
                             lambda = lambda,
                             optimal = optimal,
                             mgmt_cost = mgmt_cost,
                             benefit = benefit,
                             alloc_units = alloc_units,
                             fixed_cost = fixed_cost,
                             budget = budget,
                             confidence = confidence,
                             exist_sens = exist_sens,
                             class = "SpatialSurvDesign", ...)

  # Get the allocated surveillance resource values of the surveillance design
  self$get_allocation <- function() {
    # overridden in inherited classes
  }

  # Get the location detection sensitivities of the surveillance design
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
