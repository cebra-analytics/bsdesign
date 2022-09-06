#' Surveillance design base class builder
#'
#' Builds a base class to represent a surveillance design functionality for the
#' effective allocation of surveillance resources across one or more divisions
#' (parts, locations, categories, etc.) via methods that utilize surveillance
#' and/or incursion management costs, benefits, detection sensitivities,
#' and/or overall detection confidence.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of a bio-security surveillance and area freedom design.
#' @param divisions A \code{Divisions} or inherited class object representing
#'   one or more divisions (parts, locations, categories, etc.) for the
#'   surveillance design.
#' @param establish_pr A vector of (relative) probability values to represent
#'   the likelihood of pest establishment at each division part (location,
#'   category, etc.) specified by \code{divisions}. Default is \code{NULL}.
#'   Values are assumed to be relative when their maximum is greater than 1, or
#'   an attribute \code{relative = TRUE} is attached to the parameter.
#' @param optimal The strategy used for finding an effective surveillance
#'   resource allocation. One of (minimum) \code{"cost"}, (maximum)
#'   \code{"benefit"}, or (maximum) \code{"detection"} sensitivity (up to
#'   \code{"confidence"} level when specified).
#' @param mgmt_cost A list of vectors to represent management costs specific to
#'   the method implemented in the inherited class. Each vector specifies costs
#'   at each division part (location, category, etc.) specified by
#'   \code{divisions}. Default is an empty list. Units should be consistent
#'   with the \code{cost_unit} parameter specified in the \code{context}.
#' @param benefit A vector of values quantifying the benefit of detection
#'   at each division part (location, category, etc.) specified by
#'   \code{divisions}. Default is \code{NULL}. Units should be consistent with
#'   the \code{cost_unit} parameter specified in the \code{context}.
#' @param alloc_cost A vector of cost per unit of allocated surveillance
#'   resources. Default is \code{NULL}. Units should be consistent with the
#'   \code{cost_unit} parameter specified in the \code{context}.
#' @param fixed_cost A vector of fixed costs, such as travel costs or time, at
#'   each division part (location, category, etc.) specified by
#'   \code{divisions}. Default is \code{NULL}. Units should be consistent with
#'   \code{alloc_cost} when specified. Otherwise the units should be consistent
#'   with the \code{surv_qty_unit} parameter specified in the \code{context}.
#' @param budget The cost budget or constraint for the resource allocation in
#'   the surveillance design. Default is \code{NULL}. Units should be
#'   consistent with \code{alloc_cost} when specified. Otherwise the units
#'   should be consistent with the \code{surv_qty_unit} parameter specified in
#'   the \code{context}.
#' @param confidence The desired (minimum) system detection sensitivity or
#'   confidence of the surveillance design (e.g. 0.95). Default is \code{NULL}.
#' @param exist_sens A vector of detection sensitivity values of existing
#'   surveillance present at each division part (location, category,
#'   etc.) specified by \code{divisions}. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{SurveillanceDesign} class object (list) containing functions
#'   for allocating resources, and calculating (unit and overall) detection
#'   sensitivities:
#'   \describe{
#'     \item{\code{get_allocation()}}{Get allocated surveillance resources via
#'       specified strategy, utilizing costs, benefits, budget constraints,
#'       and/or desired confidence level.}
#'     \item{\code{get_sensitivity()}}{Get the division part detection
#'        sensitivities of the allocated surveillance design.}
#'     \item{\code{get_confidence()}}{Get the overall system sensitivity or
#'       confidence of the allocated surveillance design.}
#'   }
#' @include Context.R
#' @include Divisions.R
#' @export
SurveillanceDesign <- function(context,
                               divisions,
                               establish_pr = NULL,
                               optimal = c("cost", "benefit", "detection"),
                               mgmt_cost = list(),
                               benefit = NULL,
                               alloc_cost = NULL,
                               fixed_cost = NULL,
                               budget = NULL,
                               confidence = NULL,
                               exist_sens = NULL,
                               class = character(), ...) {
  UseMethod("SurveillanceDesign")
}

#' @name SurveillanceDesign
#' @export
SurveillanceDesign.Context <- function(context,
                                       divisions,
                                       establish_pr = NULL,
                                       optimal = c("cost", "benefit",
                                                   "detection"),
                                       mgmt_cost = list(),
                                       benefit = NULL,
                                       alloc_cost = NULL,
                                       fixed_cost = NULL,
                                       budget = NULL,
                                       confidence = NULL,
                                       exist_sens = NULL,
                                       class = character(), ...) {

  # Check divisions
  if (!inherits(divisions, "Divisions")) {
    stop(paste("Divisions parameter must be a 'Divisions' or inherited class",
               "object."), call. = FALSE)
  }

  # Number of division parts
  parts <- divisions$get_parts()

  # Check establish_pr
  if (!is.null(establish_pr) &&
      (!is.numeric(establish_pr) || any(establish_pr < 0) ||
       !length(establish_pr) %in% c(1, parts))) {
    stop(paste("The establishment probability must be a numeric vector with",
               "values  >= 0 for each division part."), call. = FALSE)
  }

  # Match optimal arguments
  optimal <- match.arg(optimal)

  # Check mgmt_cost, benefit, and confidence
  if (!is.list(mgmt_cost) ||
      !all(sapply(mgmt_cost, length) %in% c(1, parts))) {
    stop(paste("The management cost parameter must be a list of numeric",
               "vectors with values for each division part."), call. = FALSE)
  }
  if (!is.null(benefit) &&
      (!is.numeric(benefit) || !length(benefit) %in% c(1, parts))) {
    stop(paste("The benefit parameter must be a numeric vector with values",
               "for each division part."), call. = FALSE)
  }
  if (!is.null(confidence) &&
      (!is.numeric(confidence) || confidence < 0 || confidence > 1)) {
    stop("The confidence parameter must be numeric, >= 0 and <= 1.",
         call. = FALSE)
  }

  # Ensure relevant parameters are present for optimal strategy
  if (optimal == "cost" && length(mgmt_cost) == 0) {
    stop("The management cost parameter must be specified for optimal cost.",
         call. = FALSE)
  } else if (optimal == "benefit" && is.null(benefit)) {
    stop("The benefit parameter must be specified for optimal benefit.",
         call. = FALSE)
  } else if (optimal == "detection" &&
             (is.null(budget) && is.null(confidence))) {
    stop(paste("Either the budget or confidence parameter must be specified",
               "for optimal detection."), call. = FALSE)
  }

  # Check alloc_cost, fixed_cost, budget, and exist_sens
  if (!is.null(alloc_cost) &&
      (!is.numeric(alloc_cost) || !length(alloc_cost) %in% c(1, parts))) {
    stop(paste("The allocation cost parameter must be a numeric vector with ",
               "values for each division part."), call. = FALSE)
  }
  if (!is.null(fixed_cost) &&
      (!is.numeric(fixed_cost) || !length(fixed_cost) %in% c(1, parts))) {
    stop(paste("The fixed cost parameter must be a numeric vector with values",
               "for each division part."), call. = FALSE)
  }
  if (!is.null(budget) && (!is.numeric(budget) || budget < 0)) {
    stop("The budget parameter must be numeric and >= 0.", call. = FALSE)
  }
  if (!is.null(exist_sens) &&
      (!is.numeric(exist_sens) || !length(exist_sens) %in% c(1, parts))) {
    stop(paste("The existing sensitivity parameter must be a numeric vector",
               "with values for each division part."), call. = FALSE)
  }

  # Create a class structure
  self <- structure(list(), class = c(class, "SurveillanceDesign"))

  # Get the allocated surveillance resource quantities of the design
  self$get_allocation <- function() {
    # overridden in inherited classes
  }

  # Get the detection sensitivities for each division part of the design
  self$get_sensitivity <- function() {
    # overridden in inherited classes
  }

  # Get the overall system sensitivity/confidence of the design
  self$get_confidence <- function() {
    system_sens <- NULL
    return(system_sens)
  }

  return(self)
}
