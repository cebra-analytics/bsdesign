#' Surveillance design base class builder
#'
#' Builds a base class to represent surveillance design functionality for the
#' effective allocation of surveillance resources across one or more divisions
#' (parts, locations, categories, etc.) via methods that utilize surveillance
#' and/or incursion management costs, benefits, detection sensitivities,
#' and/or an overall desired system-wide sensitivity or detection probability.
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
#'   \code{"saving"} (or cost-dependent benefit), (maximum) \code{"benefit"}
#'   (independent of surveillance costs), (maximum) number of
#'   \code{"detections"}, (maximum) overall system-wide \code{"sensitivity"}
#'   or \code{"none"} for representing existing surveillance designs only.
#' @param mgmt_cost A list of vectors to represent management costs specific to
#'   the method implemented in the inherited class. Each vector specifies costs
#'   at each division part (location, category, etc.) specified by
#'   \code{divisions}. Default is an empty list. Units should be consistent
#'   with the \code{cost_unit} parameter specified in the \code{context}.
#' @param benefit A vector of values quantifying the benefit (or cost-based
#'   saving) associated with detection at each division part (location,
#'   category, etc.) specified by \code{divisions}. Default is \code{NULL}.
#'   When the benefit refers to cost-based savings (i.e. \code{optimal} is
#'   \code{"saving"}), then the units should be consistent with the
#'   \code{cost_unit} parameter specified in the \code{context}.
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
#' @param system_sens The desired (minimum) system sensitivity or detection
#'   probability of the surveillance design (e.g. 0.95). Default is
#'   \code{NULL}.
#' @param min_alloc A vector of minimum permissible allocated surveillance
#'   resource quantities at each division part (location, category, etc.)
#'   specified by \code{divisions}. Used to avoid impractically low allocation
#'   quantities. Default is \code{NULL}.
#' @param discrete_alloc A logical to indicate that the allocated surveillance
#'   resource quantities at each division part (location, category, etc.)
#'   specified by \code{divisions} should be discrete integers. Used to
#'   allocate discrete surveillance units, such as traps or detectors. Default
#'   is \code{FALSE} for continuous resources quantities, such as survey hours.
#' @param exist_alloc A vector of existing surveillance resource quantities at
#'   each division part (location, category, etc.) specified by
#'   \code{divisions}. Should only be used to represent existing surveillance
#'   designs when \code{optimal = "none"}. Default is \code{NULL}.
#' @param exist_sens A vector, or list of vectors, of detection sensitivity
#'   values of existing surveillance present at each division part (location,
#'   category, etc.) specified by \code{divisions}. Multiple existing
#'   surveillance layers may be specified in a list. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{SurveillanceDesign} class object (list) containing functions
#'   for allocating resources, and calculating (unit and overall) detection
#'   sensitivities:
#'   \describe{
#'     \item{\code{get_context()}}{Get context object.}
#'     \item{\code{get_divisions()}}{Get divisions object.}
#'     \item{\code{get_allocation()}}{Get allocated resources via specified
#'       strategy, utilizing costs, benefits, budget constraints, and/or
#'       desired system sensitivity or detection probability.}
#'     \item{\code{get_sensitivity()}}{Get the division part detection
#'       sensitivities of the allocated surveillance design combined with any
#'       existing sensitivities specified via \code{exist_sens}.}
#'     \item{\code{get_system_sens(growth = NULL)}}{Get the overall system
#'       sensitivity or detection probability of the surveillance design. The
#'       optional \code{growth} parameter may provide a vector of relative
#'       increasing multipliers (e.g. 1, 1.8, 4.3, 7.5) applied to the
#'       prevalence or density of the design over time or a sequence of
#'       repeated surveillance efforts, which provide a proxy for invasive
#'       species growth. When present, increasing system sensitivity values are
#'       returned for each multiplier or time/repeat.}
#'     \item{\code{save_design(...)}}{Save the surveillance design as a
#'       collection of raster TIF and/or comma-separated value (CSV) files,
#'       appropriate for the \code{divisions} type, including the surveillance
#'       \code{allocation}, the corresponding sensitivity \code{alloc_sens}
#'       when existing sensitivity is included, the overall \code{sensitivity},
#'       \code{surv_cost} (combined surveillance allocation and fixed costs),
#'       and a \code{summary} (CSV) of the total allocation, total costs (when
#'       applicable), and the overall allocation sensitivity
#'       (\code{alloc_sens}) when applicable, and the system-wide sensitivity
#'       (\code{system_sens}). \code{Terra} raster write options may be passed
#'       to the function for saving grid-based designs.}
#'   }
#' @references
#'   Cannon, R. M. (2009). Inspecting and monitoring on a restricted budget -
#'   where best to look? \emph{Preventive Veterinary Medicine}, 92(1–2),
#'   163-174. \doi{10.1016/j.prevetmed.2009.06.009}
#' @include Context.R
#' @include Divisions.R
#' @export
SurveillanceDesign <- function(context,
                               divisions,
                               establish_pr = NULL,
                               optimal = c("cost", "saving", "benefit",
                                           "detections", "sensitivity",
                                           "none"),
                               mgmt_cost = list(),
                               benefit = NULL,
                               alloc_cost = NULL,
                               fixed_cost = NULL,
                               budget = NULL,
                               system_sens = NULL,
                               min_alloc = NULL,
                               discrete_alloc = FALSE,
                               exist_alloc = NULL,
                               exist_sens = NULL,
                               class = character(), ...) {
  UseMethod("SurveillanceDesign")
}

#' @name SurveillanceDesign
#' @export
SurveillanceDesign.Context <- function(context,
                                       divisions,
                                       establish_pr = NULL,
                                       optimal = c("cost", "saving", "benefit",
                                                   "detections", "sensitivity",
                                                   "none"),
                                       mgmt_cost = list(),
                                       benefit = NULL,
                                       alloc_cost = NULL,
                                       fixed_cost = NULL,
                                       budget = NULL,
                                       system_sens = NULL,
                                       min_alloc = NULL,
                                       discrete_alloc = FALSE,
                                       exist_alloc = NULL,
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
               "values >= 0 for each division part."), call. = FALSE)
  }

  # Resolve if establish_pr is relative
  if (!is.null(establish_pr) &&
      ((!is.null(attr(establish_pr, "relative")) &&
        as.logical(attr(establish_pr, "relative"))) ||
       max(establish_pr) > 1)) {
    relative_establish_pr <- TRUE
  } else {
    relative_establish_pr <- FALSE
  }

  # Match optimal arguments
  optimal <- match.arg(optimal)

  # Check mgmt_cost, benefit, and system_sens
  if (!is.list(mgmt_cost) ||
      (length(mgmt_cost) > 0 &&
       !all(sapply(mgmt_cost, length) %in% c(1, parts)))) {
    stop(paste("The management cost parameter must be a list of numeric",
               "vectors with values for each division part."), call. = FALSE)
  }
  if (!is.null(benefit) &&
      (!is.numeric(benefit) || !length(benefit) %in% c(1, parts))) {
    stop(paste("The benefit parameter must be a numeric vector with values",
               "for each division part."), call. = FALSE)
  }
  if (!is.null(system_sens) &&
      (!is.numeric(system_sens) || system_sens < 0 || system_sens > 1)) {
    stop("The system sensitivity parameter must be numeric, >= 0 and <= 1.",
         call. = FALSE)
  }

  # Ensure relevant parameters are present for optimal strategy
  if (optimal == "cost" && length(mgmt_cost) == 0) {
    stop("The management cost parameter must be specified for optimal cost.",
         call. = FALSE)
  } else if (optimal == "saving" && is.null(benefit)) {
    stop("The benefit parameter must be specified for optimal saving.",
         call. = FALSE)
  } else if (optimal == "benefit" && is.null(benefit)) {
    stop("The benefit parameter must be specified for optimal benefit.",
         call. = FALSE)
  } else if (optimal == "benefit" &&
             (is.null(budget) && is.null(system_sens))) {
    stop(paste("Either the budget or system sensitivity parameter must be",
               "specified for optimal benefit."), call. = FALSE)
  } else if (optimal == "detections" &&
             (is.null(budget) && is.null(system_sens))) {
    stop(paste("Either the budget or system sensitivity parameter must be",
               "specified for optimal detections."), call. = FALSE)
  } else if (optimal == "sensitivity" &&
             (is.null(budget) && is.null(system_sens))) {
    stop(paste("Either the budget or system sensitivity parameter must be",
               "specified for optimal sensitivity."), call. = FALSE)
  }

  # Check alloc_cost, fixed_cost, budget, min_alloc, exist_alloc, & exist_sens
  if (!is.null(alloc_cost) &&
      (!is.numeric(alloc_cost) || !length(alloc_cost) %in% c(1, parts))) {
    stop(paste("The allocation cost parameter must be a numeric vector with",
               "values for each division part."), call. = FALSE)
  }
  if (!is.null(fixed_cost) &&
      (!is.numeric(fixed_cost) || !length(fixed_cost) %in% c(1, parts))) {
    stop(paste("The fixed cost parameter must be a numeric vector with values",
               "for each division part."), call. = FALSE)
  }
  if (!is.null(budget) && (!is.numeric(budget) || budget <= 0)) {
    stop("The budget parameter must be numeric and > 0.", call. = FALSE)
  }
  if (!is.null(min_alloc) &&
      (!is.numeric(min_alloc) || !length(min_alloc) %in% c(1, parts))) {
    stop(paste("The minimum allocation parameter must be a numeric vector",
               "with values for each division part."), call. = FALSE)
  }
  if (!is.logical(discrete_alloc)) {
    stop("The discrete allocation indicator parameter must be logical.",
         call. = FALSE)
  }
  if (!is.null(exist_alloc) && optimal != "none") {
    stop(paste("The existing allocation parameter should only be specified",
               "when the optimal parameter is 'none'."), call. = FALSE)
  }
  if (!is.null(exist_alloc) &&
      (!is.numeric(exist_alloc) || !length(exist_alloc) == parts)) {
    stop(paste("The existing allocation parameter must be a numeric vector",
               "with values for each division part."), call. = FALSE)
  }
  if (!is.null(exist_sens) &&
      (!(is.numeric(exist_sens) || is.list(exist_sens)) ||
       (is.numeric(exist_sens) && !length(exist_sens) %in% c(1, parts)) ||
       (is.list(exist_sens) &&
        !all(sapply(exist_sens, length) %in% c(1, parts))))) {
    stop(paste("The existing sensitivity parameter must be a numeric vector,",
               "or list of numeric vectors, with values for each division",
               "part."), call. = FALSE)
  }

  # Create a class structure
  self <- structure(list(), class = c(class, "SurveillanceDesign"))

  # Get context object
  self$get_context <- function() {
    return(context)
  }

  # Get divisions object
  self$get_divisions <- function() {
    return(divisions)
  }

  # Get the allocated surveillance resource quantities of the design
  self$get_allocation <- function() {
    # overridden in inherited classes
  }

  # Get the detection sensitivities for each division part of the design
  sensitivity <- NULL
  self$get_sensitivity <- function() {

    # Combine (union) multiple existing sensitivities
    if (is.list(exist_sens)) {
      sensitivity <<- 1
      for (i in 1:length(exist_sens)) {
        sensitivity <<- sensitivity*(1 - exist_sens[[i]])
      }
      sensitivity <<- 1 - sensitivity
    } else {
      sensitivity <<- exist_sens
    }

    # Ensure values for each division part
    if (length(sensitivity) == 1) {
      sensitivity <<- rep(sensitivity, parts)
    }

    return(sensitivity)
  }

  # Get the overall system sensitivity or detection probability of the design
  self$get_system_sens <- function(growth = NULL) {
    # overridden in inherited classes
  }

  # Save the surveillance design as a collection of appropriate files
  self$save_design <- function(...) {
    # overridden in inherited classes
  }

  return(self)
}
