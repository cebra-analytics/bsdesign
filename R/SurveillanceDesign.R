#' Generic surveillance design class builder
#'
#' Builds a base class to represent a surveillance design with functionality
#' for the effective allocation of surveillance resources via methods that
#' consider surveillance and incursion management costs, benefits, detection
#' sensitivities, and/or overall detection confidence.
#'
#' @param region A \code{raster::RasterLayer}, \code{terra::SpatRaster}, or
#'   \code{Region} or inherited class object representing the spatial region
#'   (template) for the surveillance design.
#' @param context A \code{Context} or inherited class object representing the
#'   context of a bio-security surveillance and area freedom design.
#' @param establish_pr A vector of (relative) probability values to represent
#'   the likelihood of pest establishment at each location specified by the
#'   \code{region}. Values are assumed to be relative when their range is
#'   outside of 0-1, or an attribute \code{relative = TRUE} is attached to the
#'   parameter.
#' @param optimal The strategy used for finding an effective surveillance
#'   resource allocation. One of (minimum) \code{"cost"}, (maximum)
#'   \code{"benefit"}, or (maximum) \code{"detection"} sensitivity (up to
#'   \code{"confidence"} level when specified).
#' @param mgmt_cost A list of vectors to represent management costs specific to
#'   the method implemented in the inherited class. Each vector specifies costs
#'   at each location specified by the \code{region}. Default is empty list
#'   (in the base class). An attribute \code{units} may be used to specify the
#'   cost units (e.g. "$" or "hours").
#' @param benefit A vector of values quantifying the benefit of detection
#'   at each location specified by the \code{region}. Default is \code{NULL}.
#'   An attribute \code{units} may be used to specify the benefit units (e.g.
#'   "$" or "hours").
#' @param alloc_units The units for the allocated surveillance resources (e.g.
#'   "$", "hours", or "traps") consistent with the \code{context}. This may be
#'   different to those specified for \code{mgmt_cost} or \code{benefit}.
#'   Default is \code{NULL}.
#' @param fixed_cost A vector of fixed costs, such as travel costs, at each
#'   location specified by the \code{region}. Default is \code{NULL}. Units are
#'   specified in \code{alloc_units}.
#' @param budget The cost budget or constraint for the resource allocation in
#'   the surveillance design. Default is \code{NULL}. Units are specified in
#'   \code{alloc_units}.
#' @param confidence The desired (minimum) system detection sensitivity or
#'   confidence of the surveillance design (e.g. 0.95). Default is \code{NULL}.
#' @param exist_sens A vector of detection sensitivity values of existing
#'   surveillance present at each location specified by the \code{region}.
#'   Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{SurveillanceDesign} class object (list) containing functions
#'   for allocating resources, and calculating (unit and overall) detection
#'   sensitivities:
#'   \describe{
#'     \item{\code{get_allocation()}}{Get allocated surveillance resources via
#'       specified strategy, utilizing costs, benefits, budget constraints,
#'       and/or desired confidence level.}
#'     \item{\code{get_sensitivity()}}{Get the unit/location detection
#'        sensitivities of the allocated surveillance design.}
#'     \item{\code{get_confidence()}}{Get the overall system sensitivity or
#'       confidence of the allocated surveillance design.}
#'   }
#' @include Context.R
#' @include Region.R
#' @export
SurveillanceDesign <- function(region, context, establish_pr,
                               optimal = c("cost", "benefit", "detection"),
                               mgmt_cost = NULL,
                               benefit = NULL,
                               alloc_units = NULL,
                               fixed_cost = NULL,
                               budget = NULL,
                               confidence = NULL,
                               exist_sens = NULL, ...) {
  UseMethod("SurveillanceDesign")
}

#' @name SurveillanceDesign
#' @export
SurveillanceDesign.Raster <- function(region, ...) {
  # Call Region class version
  SurveillanceDesign(Region(region), ...)
}

#' @name SurveillanceDesign
#' @export
SurveillanceDesign.SpatRaster <- function(region, ...) {
  # Call Region class version
  SurveillanceDesign(Region(region), ...)
}

#' @name SurveillanceDesign
#' @export
SurveillanceDesign.Region <- function(region, context, establish_pr,
                                      optimal = c("cost", "benefit",
                                                  "detection"),
                                      mgmt_cost = list(),
                                      benefit = NULL,
                                      alloc_units = NULL,
                                      fixed_cost = NULL,
                                      budget = NULL,
                                      confidence = NULL,
                                      exist_sens = NULL, ...) {

  # Number of region locations
  locations <- region$get_locations()

  # Check context
  if (!inherits(context, "Context")) {
    stop("Context model must be a 'Context' or inherited class object.",
         call. = FALSE)
  }

  # Check establish_pr
  if (!is.numeric(establish_pr) || establish_pr < 0 ||
      length(establish_pr) != locations) {
    stop(paste("The establishment probability must be numeric,  >= 0, and",
               "and match the number of region locations."), call. = FALSE)
  }
  if (attr(establish_pr, "relative") || max(establish_pr) > 1) {
    relative_establish_pr <- TRUE
  } else {
    relative_establish_pr <- FALSE
  }

  # Match optimal arguments
  optimal <- match.arg(optimal)

  # Check mgmt_cost, benefit, fixed_cost, budget, confidence, and exist_sens
  if (!is.list(mgmt_cost) ||
      !all(sapply(mgmt_cost, length) %in% c(1, locations))) {
    stop(paste("The management cost parameter must be a list of numeric",
               "vectors with values for each region location."), call. = FALSE)
  }
  if (!is.numeric(benefit) ||
      !all(sapply(benefit, length) %in% c(1, locations))) {
    stop(paste("The benefit parameter must be a numeric vector with values",
               "for each region location."), call. = FALSE)
  }
  if (!is.numeric(fixed_cost) ||
      !all(sapply(fixed_cost, length) %in% c(1, locations))) {
    stop(paste("The fixed cost parameter must be a numeric vector with values",
               "for each region location."), call. = FALSE)
  }
  if (!is.numeric(budget) || budget < 0) {
    stop("The budget parameter must be numeric and >= 0.", call. = FALSE)
  }
  if (!is.numeric(confidence) || confidence < 0 || confidence > 1) {
    stop("The confidence parameter must be numeric, >= 0 and <= 1.",
         call. = FALSE)
  }
  if (!is.numeric(exist_sens) ||
      !all(sapply(exist_sens, length) %in% c(1, locations))) {
    stop(paste("The existing sensitivity parameter must be a numeric vector",
               "with values for each region location."), call. = FALSE)
  }

  # Ensure relevant parameter are present for optimal strategy
  if (optimal == "cost" && length(mgmt_cost) == 0) {
    stop("The management cost parameter must be specified for optimal cost.",
         call. = FALSE)
  } else if (optimal == "benefit" && is.null(benefit)) {
    stop("The benefit parameter must be specified for optimal benefit.",
         call. = FALSE)
  } else if (optimal == "detection" &&
             (is.null(budget) || is.null(confidence))) {
    stop(paste("Either the budget or confidence parameter must be specified",
               "for optimal detection."), call. = FALSE)
  }

  # Create a class structure
  self <- structure(list(), class = "SurveillanceDesign")

  # Get the allocated surveillance resource values of the surveillance design
  # (overridden in inherited classes)
  allocation <- NULL
  self$get_allocation <- function() {
    return(allocation)
  }

  # Get the unit/location detection sensitivities of the surveillance design
  # (overridden in inherited classes)
  unit_sens <- NULL
  self$get_sensitivity <- function() {
    return(unit_sens)
  }

  # Get the overall system sensitivity/confidence of the surveillance design
  system_sens <- NULL
  self$get_confidence <- function() {
    return(system_sens)
  }

  return(self)
}
