#' Spatial surveillance design class builder
#'
#' Builds a class to represent surveillance design functionality for the
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
#' @param prevalence The cell-level design prevalence indicating the minimum
#'   number of location cells that are expected to be infected with the
#'   invasive species if the region of interest specified by \code{divisions}
#'   is infected. Default is \code{1}. Higher values can be used as a proxy
#'   to population growth over time.
#' @param optimal The strategy used for finding an effective surveillance
#'   resource allocation. One of (minimum) \code{"cost"}, (maximum)
#'   \code{"benefit"}, (maximum) \code{"detection"} sensitivity (up to
#'   \code{"confidence"} level when specified), or \code{"none"} for
#'   representing existing surveillance designs only.
#' @param mgmt_cost A list of vectors to represent estimated management costs
#'   for when the incursion is detected and undetected. Each vector specifies
#'   these costs at each spatial location specified by \code{divisions}. List
#'   elements should be named \code{detected} and \code{undetected}. Default is
#'   an empty list. Units should be consistent with the \code{cost_unit}
#'   parameter specified in the \code{context}.
#' @param benefit A vector of values quantifying the benefit of detection
#'   at each spatial location specified by \code{divisions}. Default is
#'   \code{NULL}. Units should be consistent with the \code{cost_unit}
#'   parameter specified in the \code{context}.
#' @param alloc_cost A vector of cost per unit of allocated surveillance
#'   resources at each spatial location specified by \code{divisions}. Default
#'   is \code{NULL}. Units should be consistent with the \code{cost_unit}
#'   parameter specified in the \code{context}.
#' @param fixed_cost A vector of fixed costs, such as travel costs or time, at
#'   each spatial location specified by \code{divisions}. Default is
#'   \code{NULL}. Units should be consistent with \code{alloc_cost} when
#'   specified. Otherwise the units should be consistent with the
#'   \code{surv_qty_unit} parameter specified in the \code{context}.
#' @param budget The cost budget or constraint for the resource allocation in
#'   the surveillance design. Default is \code{NULL}. Units should be
#'   consistent with \code{alloc_cost} when specified. Otherwise the units
#'   should be consistent with the \code{surv_qty_unit} parameter specified in
#'   the \code{context}.
#' @param confidence The desired (minimum) system sensitivity or detection
#'   confidence of the surveillance design (e.g. 0.95). Default is \code{NULL}.
#' @param min_alloc A vector of minimum permissible allocated surveillance
#'   resource quantities at each spatial location specified by
#'   \code{divisions}. Used to avoid impractically low allocation quantities.
#'   Default is \code{NULL}.
#' @param discrete_alloc A logical to indicate that the allocated surveillance
#'   resource quantities at each division part (location, category, etc.)
#'   specified by \code{divisions} should be discrete integers. Used to
#'   allocate discrete surveillance units, such as traps or detectors. Default
#'   is \code{FALSE} for continuous resources quantities, such as survey hours.
#' @param exist_alloc A vector of existing surveillance resource quantities at
#'   each spatial location specified by \code{divisions}. Should only be used
#'   to represent existing surveillance designs when \code{optimal = "none"}.
#'   Default is \code{NULL}.
#' @param exist_sens A vector, or list of vectors, of detection sensitivity
#'   values of existing surveillance present at each spatial location specified
#'   by \code{divisions}. Multiple existing surveillance layers may be
#'   specified in a list. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{SpatialSurvDesign} class object (list) containing inherited
#'   and extended functions from the base \code{SurveillanceDesign} class for
#'   for allocating resources, and calculating (location and overall) detection
#'   sensitivities:
#'   \describe{
#'     \item{\code{get_allocation()}}{Get allocated resources via specified
#'       strategy, utilizing costs, benefits, budget constraints, and/or
#'       desired detection confidence level.}
#'     \item{\code{get_sensitivity()}}{Get the location detection sensitivities
#'       of the allocated surveillance design combined with any existing
#'       sensitivities specified via \code{exist_sens}.}
#'     \item{\code{get_confidence(growth = NULL)}}{Get the overall system
#'       sensitivity or detection confidence of the allocated surveillance
#'       design. The optional \code{growth} parameter may provide a vector of
#'       relative increasing multipliers (e.g. 1, 1.8, 4.3, 7.5) applied to the
#'       cell-level design \code{prevalence} over time or a sequence of
#'       repeated surveillance efforts, which provide a proxy for invasive
#'       species growth. When present, increasing system sensitivity values are
#'       returned for each multiplier or time/repeat.}
#'     \item{\code{save_design(...)}}{Save the surveillance design as a
#'       collection of raster TIF and/or comma-separated value (CSV) files,
#'       appropriate for the \code{divisions} type, including the surveillance
#'       \code{allocation}, \code{sensitivity}, and a \code{summary} (CSV) of
#'       the total allocation, total costs (when applicable), and the
#'       detection confidence (system sensitivity). \code{Terra} raster write
#'       options may be passed to the function for saving grid-based designs.}
#'   }
#' @references
#'   Anderson, D. P., Gormley, A. M., Ramsey, D. S. L., Nugent, G., Martin,
#'   P. A. J., Bosson, M., Livingstone, P., & Byrom, A. E. (2017).
#'   Bio-economic optimisation of surveillance to confirm broadscale
#'   eradications of invasive pests and diseases. \emph{Biological Invasions},
#'   19(10), 2869–2884. \doi{10.1007/s10530-017-1490-5}
#'
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
#' @include SurveillanceDesign.R
#' @include LagrangeSurvDesign.R
#' @export
SpatialSurvDesign <- function(context,
                              divisions,
                              establish_pr,
                              lambda,
                              prevalence = 1,
                              optimal = c("cost", "benefit", "detection",
                                          "none"),
                              mgmt_cost = list(),
                              benefit = NULL,
                              alloc_cost = NULL,
                              fixed_cost = NULL,
                              budget = NULL,
                              confidence = NULL,
                              min_alloc = NULL,
                              discrete_alloc = FALSE,
                              exist_alloc = NULL,
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
                                      prevalence = 1,
                                      optimal = c("cost", "benefit",
                                                  "detection", "none"),
                                      mgmt_cost = list(),
                                      benefit = NULL,
                                      alloc_cost = NULL,
                                      fixed_cost = NULL,
                                      budget = NULL,
                                      confidence = NULL,
                                      min_alloc = NULL,
                                      discrete_alloc = FALSE,
                                      exist_alloc = NULL,
                                      exist_sens = NULL,
                                      class = character(), ...) {

  # Build via base class (for checks and system sensitivity)
  self <- SurveillanceDesign(context = context,
                             divisions = divisions,
                             establish_pr = establish_pr,
                             optimal = optimal,
                             mgmt_cost = mgmt_cost,
                             benefit = benefit,
                             alloc_cost = alloc_cost,
                             fixed_cost = fixed_cost,
                             budget = budget,
                             confidence = confidence,
                             min_alloc = min_alloc,
                             discrete_alloc = discrete_alloc,
                             exist_alloc = exist_alloc,
                             exist_sens = exist_sens,
                             class = "SpatialSurvDesign", ...)

  # Number of division parts
  parts <- divisions$get_parts()

  # Resolve if establish_pr is relative
  if ((!is.null(attr(establish_pr, "relative")) &&
       as.logical(attr(establish_pr, "relative"))) || max(establish_pr) > 1) {
    relative_establish_pr <- TRUE
  } else {
    relative_establish_pr <- FALSE
  }

  # Check lambda and prevalence
  if (!is.numeric(lambda) || any(lambda < 0) ||
      !length(lambda) %in% c(1, parts)) {
    stop(paste("The lambda parameter must be numeric, >= 0, and match the",
               "number of division parts."), call. = FALSE)
  } else if (length(lambda) == 1) {
    lambda <- rep(lambda, parts)
  }
  if (!is.numeric(prevalence) || prevalence < 0) {
    stop("The prevalence parameter must be numeric and >= 0.", call. = FALSE)
  }

  # Match optimal arguments
  optimal <- match.arg(optimal)

  # Resolve alloc_cost, fixed_cost, min_alloc, and exist_sens
  if (length(alloc_cost) == 1) {
    alloc_cost <- rep(alloc_cost, parts)
  } else if (is.null(alloc_cost)) {
    alloc_cost <- rep(1, parts)
  }
  if (length(fixed_cost) == 1) {
    fixed_cost <- rep(fixed_cost, parts)
  } else if (is.null(fixed_cost)) {
    fixed_cost <- rep(0, parts)
  }
  if (!is.null(min_alloc)) {
    if (length(min_alloc) == 1) {
      min_alloc <- rep(min_alloc, parts)
    }
    if (discrete_alloc) {
      min_alloc <- pmax(ceiling(min_alloc), 1)
    }
  } else {
    if (discrete_alloc) {
      min_alloc <- rep(1, parts)
    } else {
      min_alloc <- rep(0, parts)
    }
  }
  if (is.null(exist_sens)) {
    exist_sens <- rep(0, parts)
  } else {
    exist_sens <- self$get_sensitivity() # combine via base class
  }

  # Check and resolve empty optimal strategy parameters
  if (optimal == "cost") {
    if (!all(c("detected", "undetected") %in% names(mgmt_cost))) {
      stop(paste("The management cost parameter must contain list elements",
                 "'detected' and 'undetected'."), call. = FALSE)
    } else {
      benefit <- mgmt_cost$undetected - mgmt_cost$detected
    }
  } else if (optimal != "benefit") {
    benefit <- 1
  }

  ## Lagrange optimization of allocated cost per division part x_alloc
  ## given the surveillance resource quantity allocation qty_alloc
  ## where qty_alloc = (x_alloc - fixed_cost)/alloc_cost

  # Lagrange optimization parameters/functions
  f_obj <- NULL # Objective function
  f_deriv <- NULL # Derivative of objective function
  f_pos <- NULL # Pseudo-inverse of derivative given marginal benefit alpha
  alpha_unconstr <- NULL # Unconstrained marginal benefit alpha
  alpha_min <- NULL # Minimum marginal benefit alpha
  f_unit_sens <- NULL # Unit sensitivity calculation function
  f_inv_unit_sens <- NULL # Inverse of unit sensitivity calculation function
  search_alpha <- NULL # Search alpha for optimal objective
  set_lagrange_params <- function() {

    # Objective function
    f_obj <<- function(x_alloc) {

      # Quantity allocation (units)
      n_alloc <- (x_alloc >= fixed_cost)*(x_alloc - fixed_cost)/alloc_cost

      if (optimal == "detection" && !relative_establish_pr) {

        # maximum detection
        return(log(1 - (establish_pr*
                          (1 - ((1 - exist_sens)*exp(-1*lambda*n_alloc))))))
      } else {

        # minimum cost or maximum benefit (benefit = 1 for detection)
        incl_x <- (optimal == "cost")
        return(benefit*establish_pr*(1 - exist_sens)*exp(-1*lambda*n_alloc) +
                 (n_alloc > 0)*x_alloc*incl_x)
      }
    }

    # Derivative of objective function
    f_deriv <<- function(x_alloc) {

      # Quantity allocation (units)
      n_alloc <- (x_alloc >= fixed_cost)*(x_alloc - fixed_cost)/alloc_cost

      if (optimal == "detection" && !relative_establish_pr) {

        # maximum detection
        return(-1*establish_pr*(1 - exist_sens)*
                 lambda/alloc_cost*exp(-1*lambda*n_alloc)/
                 (1 - (establish_pr*
                         (1 - ((1 - exist_sens)*exp(-1*lambda*n_alloc))))))
      } else {

        # minimum cost or maximum benefit (benefit = 1 for detection)
        incl_x <- (optimal == "cost")
        return((n_alloc > 0)*incl_x -
                 (benefit*establish_pr*(1 - exist_sens)*
                    lambda/alloc_cost*exp(-1*lambda*n_alloc)))
      }
    }

    # Pseudo-inverse of derivative given marginal benefit alpha
    f_pos <<- function(alpha) {
      values <- lambda/alloc_cost*benefit*establish_pr*(1 - exist_sens)
      idx <- which(values > 0)
      values[-idx] <- 0
      if (optimal == "detection" && !relative_establish_pr) {

        # maximum detection
        values[idx] <- pmax(
          ((alpha > -1*lambda[idx]/alloc_cost[idx])*
             (alloc_cost[idx]/lambda[idx]*
                (log(-1*lambda[idx]/alloc_cost[idx]/alpha - 1) -
                   log(1/establish_pr[idx] - 1) +
                   log(1 - exist_sens[idx])))), 0)
        idx <- which(values > 0)
        values[idx] <- (pmax(min_alloc[idx]*alloc_cost[idx], values[idx]) +
                          fixed_cost[idx])

      } else {

        # minimum cost or maximum benefit (benefit = 1 for detection)
        incl_x <- (optimal == "cost")
        values[idx] <-
          (((alpha - 1*incl_x) >= -1*values[idx])*
             (pmax(min_alloc[idx]*alloc_cost[idx],
                   (-1*alloc_cost[idx]/lambda[idx]*
                      log(-1*(alpha - 1*incl_x)/values[idx]))) +
                fixed_cost[idx]))

        # limit to zero cost allocation via f_obj(0)
        if (optimal == "cost") {
          values <- (values < benefit*establish_pr*(1 - exist_sens))*values
        }
      }

      return(values)
    }

    # Unconstrained marginal benefit alpha
    alpha_unconstr <<- (optimal == "cost") - 1

    # Minimum marginal benefit alpha
    alpha_min <<- min(f_deriv(fixed_cost))

    # Function for calculating unit sensitivity
    f_unit_sens <<- function(x_alloc) {
      return(1 - ((1 - exist_sens)*
                    exp((-1*lambda*(x_alloc - (x_alloc > 0)*fixed_cost)/
                           alloc_cost))))
    }

    # Function for calculating inverse of unit sensitivity
    f_inv_unit_sens <<- function(unit_sens) {
      x_alloc <- -1*alloc_cost/lambda*log((1 - unit_sens)/(1 - exist_sens))
      return(x_alloc + (x_alloc > 0)*fixed_cost)
    }

    # Search alpha for optimal objective (even when no constraints)
    search_alpha <<- any(fixed_cost > 0 | min_alloc > 0)
  }
  set_lagrange_params()

  # Function for calculating sensitivities
  calculate_sensitivity <- function(n_alloc) {
    return(1 - (1 - exist_sens)*exp(-1*lambda*n_alloc))
  }

  # Function for calculating detection confidence
  calculate_confidence <- function(sensitivity) {
    if (relative_establish_pr) {
      return(sum(establish_pr*sensitivity)/sum(establish_pr))
    } else {
      return((1 - prod(1 - establish_pr*sensitivity))/
               (1 - prod(1 - establish_pr)))
    }
  }

  # Get the allocated surveillance resource values of the surveillance design
  qty_alloc <- NULL
  self$get_allocation <- function() {
    if (optimal != "none" && is.null(qty_alloc)) {

      if (discrete_alloc) {

        # Make a copy of altered parameters
        fixed_cost_orig <- fixed_cost
        budget_orig <- budget
        min_alloc_orig <- min_alloc
        exist_sens_orig <- exist_sens

        # Initial allocation
        qty_alloc <<- rep(0, parts)
      }

      # Iterative addition of discrete allocations or single continuous
      add_allocation <- TRUE
      while (add_allocation) {

        # Get cost allocation x_alloc via Lagrange surveillance design
        lagrangeSurvDesign <- LagrangeSurvDesign(context,
                                                 divisions,
                                                 establish_pr,
                                                 f_obj,
                                                 f_deriv,
                                                 f_pos,
                                                 alpha_unconstr,
                                                 alpha_min,
                                                 f_unit_sens,
                                                 f_inv_unit_sens,
                                                 budget = budget,
                                                 confidence = confidence,
                                                 min_alloc = min_alloc,
                                                 search_alpha = search_alpha)
        x_alloc <- lagrangeSurvDesign$get_cost_allocation()

        # Optimal resource allocation
        if (discrete_alloc) {

          # Add discrete allocation
          n_alloc <- floor((x_alloc >= fixed_cost)*
                             (x_alloc - fixed_cost)/alloc_cost)
          # n_alloc <- (n_alloc*alloc_cost > fixed_cost)*n_alloc
          qty_alloc <<- qty_alloc + n_alloc

          # Alter parameters and indicate further allocation required
          fixed_cost[which(qty_alloc > 0)] <<- 0
          exist_sens <<- calculate_sensitivity(n_alloc)
          add_allocation <- (sum(x_alloc) > 0)
          if (is.numeric(budget)) {
            total_x_alloc <- sum(qty_alloc*alloc_cost +
                                   (qty_alloc > 0)*fixed_cost_orig)
            add_allocation <- (total_x_alloc < budget && add_allocation)
            budget <<- budget - total_x_alloc
          }
          if (is.numeric(confidence)) {
            add_allocation <- (calculate_confidence(exist_sens) < confidence &&
                                 add_allocation)
          }
          min_alloc[which(qty_alloc > 0)] <<- 1

          # Reset Lagrange parameters
          set_lagrange_params()

        } else {

          # Continuous allocation
          qty_alloc <<- ((x_alloc >= fixed_cost)*
                           (x_alloc - fixed_cost)/alloc_cost)
          add_allocation <- FALSE
        }
      }

      # Return altered parameters to their original values
      if (discrete_alloc) {
        fixed_cost <<- fixed_cost_orig
        budget <<- budget_orig
        min_alloc <<- min_alloc_orig
        exist_sens <<- exist_sens_orig
      }
    }

    return(qty_alloc)
  }

  # Get the location detection sensitivities of the surveillance design
  sensitivity <- NULL
  self$get_sensitivity <- function() {
    if (is.null(sensitivity)) {
      if (optimal != "none" && !is.null(qty_alloc)) {
        sensitivity <<- calculate_sensitivity(qty_alloc)
      } else if (optimal == "none" && !is.null(exist_alloc)) {
        sensitivity <<- calculate_sensitivity(exist_alloc)
      }
    }
    return(sensitivity)
  }

  # Get the overall system sensitivity or detection confidence of the design
  self$get_confidence <- function(growth = NULL) {
    system_sens <- NULL
    sensitivity <- self$get_sensitivity()
    if (!is.null(sensitivity)) {

      # Calculate base system sensitivity
      if (parts == 1) {
        system_sens <- sensitivity
      } else if (!is.null(establish_pr)) {
        system_sens <- calculate_confidence(sensitivity)
      }

      # Apply prevalence with growth if present
      if (is.numeric(growth)) {
        prevalence <- prevalence*growth
      }
      system_sens <- 1 - (1 - system_sens)^prevalence
    }

    return(system_sens)
  }

  # Save the surveillance design as a collection of appropriate files
  self$save_design <- function(...) {

    # Save allocation and sensitivity
    if (divisions$get_type() == "grid") {
      terra::writeRaster(divisions$get_rast(self$get_allocation()),
                         "allocation.tif", ...)
      terra::writeRaster(divisions$get_rast(self$get_sensitivity()),
                         "sensitivity.tif", ...)
    } else if (divisions$get_type() == "patch") {
      write.csv(cbind(divisions$get_coords(extra_cols = TRUE),
                      allocation = self$get_allocation(),
                      sensitivity = self$get_sensitivity()),
                file = "design.csv", row.names = FALSE)
    } else if (divisions$get_type() == "other") {
      write.csv(cbind(divisions$get_data(),
                      allocation = self$get_allocation(),
                      sensitivity = self$get_sensitivity()),
                file = "design.csv", row.names = FALSE)
    }

    # Save summary
    summary_data <- data.frame(total_allocation = sum(self$get_allocation()))
    if (!all(alloc_cost == 1)) {
      summary_data$allocation_cost <- sum(self$get_allocation()*alloc_cost)
    }
    if (!all(fixed_cost == 0)) {
      summary_data$fixed_cost <- sum((self$get_allocation() > 0)*fixed_cost)
    }
    if (optimal == "cost") {
      summary_data$mgmt_cost <- sum(
        establish_pr*(mgmt_cost$detected*self$get_sensitivity() +
                        mgmt_cost$undetected*(1 - self$get_sensitivity())))
      summary_data$total_cost <-
        (summary_data$mgmt_cost +  sum(self$get_allocation()*alloc_cost) +
           sum((self$get_allocation() > 0)*fixed_cost))
    }
    summary_data$detection_confidence <- self$get_confidence()
    write.csv(summary_data, file = "summary.csv", row.names = FALSE)

    return(summary_data)
  }

  return(self)
}
