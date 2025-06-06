#' Lagrange surveillance design class builder
#'
#' Builds a generic class to represent surveillance design functionality for
#' the effective allocation of surveillance resources across one or more
#' divisions (parts, locations, categories, etc.) via Lagrange-based methods
#' for optimizing objective functions specified with surveillance and/or
#' incursion management costs, benefits, detection sensitivities, and/or
#' overall system-wide sensitivity or detection probability.
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
#' @param f_obj The objective function for calculating values to optimize
#'   (minimize) in the form \code{function(x_alloc)}, where \code{x_alloc}
#'   represents a candidate resource allocation at each division part
#'   (location, category, etc.) specified by \code{divisions}.
#' @param f_deriv The derivative of the objective function \code{f_obj} in the
#'   form \code{function(x_alloc)}, where \code{x_alloc} represents a candidate
#'   resource allocation at each division part (location, category, etc.)
#'   specified by \code{divisions}.
#' @param f_pos The pseudo-inverse of the derivative function \code{f_deriv} in
#'   the form \code{function(alpha)}, where \code{alpha} represents the
#'   marginal benefit value for a candidate resource allocation \code{x_alloc},
#'   which is returned by the function for each division part (location,
#'   category, etc.) specified by \code{divisions}.
#' @param alpha_unconstr The marginal benefit value to utilize when the search
#'   for the optimal resource allocation is not constrained via a \code{budget}
#'   or a desired system sensitivity (\code{system_sens}).
#' @param alpha_min The minimum marginal benefit value to utilize when
#'   searching for the optimal resource allocation.
#' @param f_unit_sens A function for calculating the unit (division part)
#'   sensitivity, or probability of detecting an incursion when present. The
#'   function should be in the form \code{function(x_alloc)}, where
#'   \code{x_alloc} represents a candidate resource allocation at each
#'   division part (location, category, etc.) specified by \code{divisions}.
#' @param f_inv_unit_sens A function for calculating the inverse of the unit
#'   sensitivity, or candidate resource allocation. The function should be in
#'   the form \code{function(unit_sens)}, where \code{unit_sens} represents the
#'   unit sensitivity at each division part (location, category, etc.)
#'   specified by \code{divisions}.
#' @param budget The cost budget or constraint for the resource allocation in
#'   the surveillance design. Default is \code{NULL}.
#' @param system_sens The desired (minimum) system sensitivity or detection
#'   probability of the surveillance design (e.g. 0.95). Default is
#'   \code{NULL}.
#' @param min_alloc A vector of minimum permissible allocated surveillance
#'   resource quantities at each division part (location, category, etc.)
#'   specified by \code{divisions}. Used to avoid impractically low allocation
#'   quantities. Default is \code{NULL}.
#' @param search_alpha A logical indicator to search for the optimal resource
#'   allocation, even when it is not constrained (via \code{budget} or
#'   \code{system_sens}), such as when fixed costs are present. Default is
#'   \code{FALSE} indicates that search only occurs when constrained.
#' @param ... Additional parameters.
#' @return A \code{LagrangeSurvDesign} class object (list) containing functions
#'   for allocating resource costs:
#'   \describe{
#'     \item{\code{get_cost_allocation()}}{Get allocated surveillance resources
#'       (costs) via Lagrange-based method, utilizing costs, benefits, budget
#'       constraints, and/or desired system sensitivity or detection
#'       probability.}
#'   }
#' @references
#'   Cannon, R. M. (2009). Inspecting and monitoring on a restricted budget -
#'   where best to look? \emph{Preventive Veterinary Medicine}, 92(1–2),
#'   163-174. \doi{10.1016/j.prevetmed.2009.06.009}
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
#' @export
LagrangeSurvDesign <- function(context,
                               divisions,
                               establish_pr,
                               f_obj,
                               f_deriv,
                               f_pos,
                               alpha_unconstr,
                               alpha_min,
                               f_unit_sens,
                               f_inv_unit_sens,
                               budget = NULL,
                               system_sens = NULL,
                               min_alloc = NULL,
                               search_alpha = FALSE, ...) {
  UseMethod("LagrangeSurvDesign")
}

#' @name LagrangeSurvDesign
#' @export
LagrangeSurvDesign.Context <- function(context,
                                       divisions,
                                       establish_pr,
                                       f_obj,
                                       f_deriv,
                                       f_pos,
                                       alpha_unconstr,
                                       alpha_min,
                                       f_unit_sens,
                                       f_inv_unit_sens,
                                       budget = NULL,
                                       system_sens = NULL,
                                       min_alloc = NULL,
                                       search_alpha = FALSE, ...) {

  # Check divisions
  if (!inherits(divisions, "Divisions")) {
    stop(paste("Divisions parameter must be a 'Divisions' or inherited class",
               "object."), call. = FALSE)
  }

  # Number of division parts
  parts <- divisions$get_parts()

  # Check establish_pr, alpha_unconstr, and alpha_min
  if (!is.numeric(establish_pr) || any(establish_pr < 0) ||
      length(establish_pr) != parts) {
    stop(paste("The establishment probability must be numeric,  >= 0, and",
               "match the number of division parts."), call. = FALSE)
  }
  if ((!is.null(attr(establish_pr, "relative")) &&
       as.logical(attr(establish_pr, "relative"))) || max(establish_pr) > 1) {
    relative_establish_pr <- TRUE
  } else {
    relative_establish_pr <- FALSE
  }
  if (!is.numeric(alpha_unconstr)) {
    stop("The unconstrained marginal benefit alpha value must be numeric.",
         call. = FALSE)
  }
  if (!is.numeric(alpha_min)) {
    stop("The minimum marginal benefit alpha value must be numeric.",
         call. = FALSE)
  }

  # Check f_obj, f_deriv, f_pos, f_unit_sens, and f_inv_unit_sens
  if (!is.function(f_obj) || length(formalArgs(f_obj)) != 1) {
    stop("The objective function should have form function(x_alloc).",
         call. = FALSE)
  }
  if (!is.function(f_deriv) || length(formalArgs(f_deriv)) != 1) {
    stop("The derivative function should have form function(x_alloc).",
         call. = FALSE)
  }
  if (!is.function(f_pos) || length(formalArgs(f_pos)) != 1) {
    stop(paste("The pseudo-inverse-derivative function should have form",
               "function(alpha)."), call. = FALSE)
  }
  if (!is.function(f_unit_sens) || length(formalArgs(f_unit_sens)) != 1) {
    stop("The unit sensitivity function should have form function(x_alloc).",
         call. = FALSE)
  }
  if (!is.function(f_inv_unit_sens) ||
      length(formalArgs(f_inv_unit_sens)) != 1) {
    stop(paste("The inverse unit sensitivity function should have form",
               "function(unit_sens)."), call. = FALSE)
  }

  # Check budget, system_sens, min_alloc, and search_alpha (indicator)
  if (!is.null(budget) && (!is.numeric(budget) || budget <= 0)) {
    stop("The budget parameter must be numeric and > 0.", call. = FALSE)
  }
  if (!is.null(system_sens) &&
      (!is.numeric(system_sens) || system_sens < 0 || system_sens > 1)) {
    stop("The system sensitivity parameter must be numeric, >= 0 and <= 1.",
         call. = FALSE)
  }
  if (!is.null(min_alloc) &&
      (!is.numeric(min_alloc) || !length(min_alloc) %in% c(1, parts))) {
    stop(paste("The minimum allocation parameter must be a numeric vector",
               "with values for each division part."), call. = FALSE)
  }
  if (!is.logical(search_alpha)) {
    stop("The search alpha indicator parameter must be logical.",
         call. = FALSE)
  }

  # Resolve min_alloc
  if (!is.null(min_alloc)) { # LATER -> discrete ####
    if (length(min_alloc) == 1) {
      min_alloc <- rep(min_alloc, parts)
    }
  } else {
    min_alloc <- rep(0, parts)
  }

  ## Lagrange optimization of allocated cost per division part x_alloc
  ## given the surveillance resource quantity allocation qty_alloc
  ## where qty_alloc = (x_alloc - fixed_cost)/alloc_cost

  # Optimal cost allocation for alpha value within budget or system sensitivity
  allocate <- function(alpha) {

    # Generate full allocation
    x_alloc <- f_pos(alpha)

    # Optimal within budget or target system sensitivity
    if (is.numeric(budget) || is.numeric(system_sens)) {

      # Order by f(f+(a))/f+(a)
      rank_values <- abs(f_obj(x_alloc)/x_alloc)
      rank_values[which(!is.finite(rank_values))] <- 0
      idx <- order(rank_values, decreasing = TRUE)

      # Determine allocation within budget
      nonzero <- which(x_alloc[idx] > 0)

      # Optimal within budget
      if (is.numeric(budget)) {
        cum_cost <- cumsum(x_alloc[idx][nonzero])
        over_budget <- which(cum_cost > budget)
        if (length(over_budget)) {
          x_alloc[idx][nonzero][over_budget[1]] <-
            (x_alloc[idx][nonzero][over_budget[1]] -
               max(cum_cost[over_budget[1]] - budget,
                   min_alloc[idx][nonzero][over_budget[1]]))
          x_alloc[idx][nonzero][over_budget[-1]] <- 0
        }
      }

      # Optimal up to system sensitivity
      if (is.numeric(system_sens)) {

        # Unit sensitivity
        exist_sens <- f_unit_sens(0)
        new_sens <- f_unit_sens(x_alloc)

        # Calculate cumulative system sensitivity
        if (length(nonzero)) {
          if (relative_establish_pr) {
            cum_conf <- ((sum(establish_pr*exist_sens) +
                            cumsum((establish_pr*
                                      (new_sens - exist_sens))[idx][nonzero]))/
                           sum(establish_pr))
          } else {
            cum_conf <-
              ((1 - (prod(1 - (establish_pr*exist_sens))*
                       cumprod(
                         ((1 - establish_pr*new_sens)/
                            (1 - establish_pr*exist_sens))[idx][nonzero])))/
                 (1 - prod(1 - establish_pr)))
          }
        } else {
          cum_conf <- 0
        }

        # Select allocation up to system sensitivity
        over_conf <- which(cum_conf > system_sens)
        idx_w <- idx[nonzero][-over_conf] # within system_sens
        i_th <- idx[nonzero][over_conf[1]] # at threshold
        idx_o <- c(idx[nonzero][over_conf[-1]], idx[-nonzero]) # over or zero
        if (length(over_conf)) {
          if (relative_establish_pr) {
            th_sens <-
              (system_sens*sum(establish_pr) -
                 (sum((establish_pr*new_sens)[idx_w]) +
                    sum((establish_pr*exist_sens)[idx_o])))/
              establish_pr[i_th]
          } else {
            th_sens <-
              (1 - ((1 - system_sens*(1 - prod(1 - establish_pr)))/
                      (prod((1 - establish_pr*new_sens)[idx_w])*
                         prod((1 - establish_pr*exist_sens)[idx_o]))))/
              establish_pr[i_th]
          }
          x_alloc[i_th] <- max(f_inv_unit_sens(th_sens)[i_th], min_alloc[i_th])
          x_alloc[idx_o] <- 0
        }

        # Add system_sens as an attribute
        if (relative_establish_pr) {
          conf <- (sum(establish_pr*f_unit_sens(x_alloc))/sum(establish_pr))
        } else {
          conf <- ((1 - prod(1 - establish_pr*f_unit_sens(x_alloc)))/
                     (1 - prod(1 - establish_pr)))
        }
        attr(x_alloc, "system_sens") <- conf
      }
    }

    # Add total as an attribute
    attr(x_alloc, "total") <- sum(x_alloc)

    return(x_alloc)
  }

  # Create a class structure
  self <- structure(list(), class = "LagrangeSurvDesign")

  # Get the allocated surveillance resource costs for the design
  self$get_cost_allocation <- function() {

    # No constraint
    best_alpha <- alpha_unconstr

    # Search for minimum objective via marginal benefit (alpha) values
    if (is.numeric(budget) || is.numeric(system_sens) || search_alpha) {
      interval <- (0:100)/100*alpha_min
      alpha_range <- range(interval)[2] - range(interval)[1]
      precision <- 8 # for alpha
      while (alpha_range > abs(best_alpha*10^(-1*precision))) {

        # Get allocation for each alpha in interval
        alloc <- as.data.frame(t(sapply(interval[-1], function(a) {
          alloc <- allocate(a)
          c(obj = sum(f_obj(alloc)), total = attr(alloc, "total"),
            system_sens = attr(alloc, "system_sens"))})))

        # Choose alpha corresponding to the last repeated minimum objective
        if (is.numeric(system_sens) && max(alloc$system_sens) >= system_sens) {
          idx <- which(alloc$system_sens >= system_sens)
          idx <- idx[which(alloc$total[idx] <= min(alloc$total[idx]))]
          if (min(alloc$obj) < 0) {
            i <- max(idx[which(alloc$obj[idx] <=
                                 min(alloc$obj[idx])*(1 - 10^(-1*precision)))])
          } else {
            i <- max(idx[which(alloc$obj[idx] <=
                                 min(alloc$obj[idx])*(1 + 10^(-1*precision)))])
          }
        } else {
          i <- max(which(alloc$obj <= min(alloc$obj)))
        }
        best_alpha <- interval[i + 1]

        # Update interval and range for next iteration
        if (i < length(interval) - 1) {
          interval <- (0:100)/100*(interval[i + 2] - interval[i]) + interval[i]
        } else {
          interval <- (0:100)/100*(interval[i + 1] - interval[i]) + interval[i]
        }
        alpha_range <- range(interval)[2] - range(interval)[1]
      }
    }

    return(as.numeric(allocate(best_alpha)))
  }

  return(self)
}
