#' Lagrange surveillance design class builder
#'
#' Builds a generic class to represent a surveillance design functionality for
#' the effective allocation of surveillance resources across one or more
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
#' @param lambda A vector of efficacy or detection rates for each division part
#'   (location, category, etc.) specified by \code{divisions}, such that the
#'   probability of detecting an incursion when present at a part can be
#'   expressed via \code{pr(detect|presence) = 1 - exp(-lambda*allocation)},
#'   for a given allocation of surveillance resources.
#' @param optimal The strategy used for finding an effective surveillance
#'   resource allocation. One of (minimum) \code{"cost"}, (maximum)
#'   \code{"benefit"}, or (maximum) \code{"detection"} sensitivity (up to
#'   \code{"confidence"} level when specified).
#' @param mgmt_cost A list of vectors to represent estimated management costs
#'   for when the incursion is detected and undetected. Each vector specifies
#'   costs at each division part (location, category, etc.) specified by
#'   \code{divisions}. List elements should be named \code{detected} and
#'   \code{undetected}. Default is an empty list. Units should be consistent
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
#' @return A \code{SurveillanceDesign} class object (list) containing inherited
#'   and extended functions from the base \code{SurveillanceDesign} class for
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
#' @include SurveillanceDesign.R
#' @export
LagrangeSurvDesign <- function(context,
                               divisions,
                               establish_pr,
                               lambda,
                               optimal = c("cost", "benefit", "detection"),
                               mgmt_cost = NULL,
                               benefit = NULL,
                               alloc_cost = NULL,
                               fixed_cost = NULL,
                               budget = NULL,
                               confidence = NULL,
                               exist_sens = NULL,
                               class = character(), ...) {
  UseMethod("LagrangeSurvDesign")
}

#' @name LagrangeSurvDesign
#' @export
LagrangeSurvDesign.Context <- function(context,
                                       divisions,
                                       establish_pr,
                                       lambda,
                                       optimal = c("cost", "benefit",
                                                   "detection"),
                                       mgmt_cost = NULL,
                                       benefit = NULL,
                                       alloc_cost = NULL,
                                       fixed_cost = NULL,
                                       budget = NULL,
                                       confidence = NULL,
                                       exist_sens = NULL,
                                       class = character(), ...) {

  # Build via base class
  self <- SurveillanceDesign(context = context,
                             divisions = divisions, # TODO subset ####
                             class = "LagrangeSurvDesign", ...)

  # Number of division parts
  parts <- divisions$get_parts()

  # Check establish_pr
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

  # Check lambda
  if (!is.numeric(lambda) || lambda < 0 || !length(lambda) %in% c(1, parts)) {
    stop(paste("The lambda parameter must be numeric,  >= 0, and match the",
               "number of division parts."), call. = FALSE)
  } else if (length(lambda) == 1) {
    lambda <- rep(lambda, parts)
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
      (!is.numeric(benefit) ||
       !all(sapply(benefit, length) %in% c(1, parts)))) {
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
             (is.null(budget) || is.null(confidence))) {
    stop(paste("Either the budget or confidence parameter must be specified",
               "for optimal detection."), call. = FALSE)
  }

  # Check and resolve alloc_cost, fixed_cost, budget, and exist_sens
  if (!is.null(alloc_cost) &&
      (!is.numeric(alloc_cost) ||
       !all(sapply(alloc_cost, length) %in% c(1, parts)))) {
    stop(paste("The fixed cost parameter must be a numeric vector with values",
               "for each division part."), call. = FALSE)
  } else if (length(alloc_cost) == 1) {
    alloc_cost <- rep(alloc_cost, parts)
  } else if (is.null(alloc_cost)) {
    alloc_cost <- rep(1, parts)
  }
  if (!is.null(fixed_cost) &&
      (!is.numeric(fixed_cost) ||
       !all(sapply(fixed_cost, length) %in% c(1, parts)))) {
    stop(paste("The fixed cost parameter must be a numeric vector with values",
               "for each division part."), call. = FALSE)
  } else if (length(fixed_cost) == 1) {
    fixed_cost <- rep(fixed_cost, parts)
  } else if (is.null(fixed_cost)) {
    fixed_cost <- rep(0, parts)
  }
  if (!is.null(budget) && (!is.numeric(budget) || budget < 0)) {
    stop("The budget parameter must be numeric and >= 0.", call. = FALSE)
  }
  if (!is.null(exist_sens) &&
      (!is.numeric(exist_sens) ||
       !all(sapply(exist_sens, length) %in% c(1, parts)))) {
    stop(paste("The existing sensitivity parameter must be a numeric vector",
               "with values for each division part."), call. = FALSE)
  } else if (is.null(exist_sens)) {
    exist_sens <- rep(0, parts)
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

  # Objective function
  f_obj <- function(x_alloc) {
    if (optimal == "detection") { # maximum detection
      return(
        (x_alloc >= fixed_cost)*
          log(1 - (establish_pr*
                     (1 - ((1 - exist_sens)*
                             exp(-1*lambda*
                                   (x_alloc - fixed_cost)/alloc_cost))))))
    } else { # minimum cost or maximum benefit
      incl_x <- (optimal == "cost")
      return(
        benefit*establish_pr*(1 - exist_sens)*
           ((x_alloc < fixed_cost)*1 +
              ((x_alloc >= fixed_cost)*
                 (x_alloc*incl_x + exp(-1*lambda* # cost only
                                         (x_alloc - fixed_cost)/alloc_cost)))))
    }
  }

  # Derivative of objective function
  f_deriv <- function(x_alloc) {
    if (optimal == "detection") { # maximum detection
      return(
        (x_alloc >= fixed_cost)*-1*establish_pr*(1 - exist_sens)*
          lambda/alloc_cost*exp(-1*lambda*(x_alloc - fixed_cost)/alloc_cost)/
          (1 - (establish_pr*
                  (1 - ((1 - exist_sens)*
                          exp(-1*lambda*
                                (x_alloc - fixed_cost)/alloc_cost))))))
    } else { # minimum cost or maximum benefit
      incl_x <- (optimal == "cost")
      return(
        (x_alloc >= fixed_cost)*
          (1*incl_x - (benefit*establish_pr*(1 - exist_sens)*
                         lambda/alloc_cost*
                         exp(-1*lambda*(x_alloc - fixed_cost)/alloc_cost))))
    }
  }

  # Pseudo-inverse of derivative given marginal benefit alpha
  f_pos <- function(alpha) {
    values <- lambda/alloc_cost*benefit*establish_pr*(1 - exist_sens)
    idx <- which(values > 0)
    values[-idx] <- 0
    if (optimal == "detection") { # maximum detection
      values[idx] <-
        pmax(0, ((alpha > -1*lambda[idx]/alloc_cost[idx])*
                   (alloc_cost[idx]/lambda[idx]*
                      (log(-1*lambda[idx]/alloc_cost[idx]/alpha - 1) -
                         log(1/establish_pr[idx] - 1) +
                         log(1 - exist_sens[idx])) +
                      fixed_cost[idx])))
    } else { # minimum cost or maximum benefit
      incl_x <- (optimal == "cost")
      values[idx] <-
        (((alpha - 1*incl_x) >= -1*values[idx])*
           (-1*alloc_cost[idx]/lambda[idx]*
              log(-1*(alpha - 1*incl_x)/(values[idx])) + fixed_cost[idx]))
    }
    return(values)
  }

  # Optimal cost allocation for alpha value within budget or confidence level
  allocate <- function(alpha) {

    # Generate full allocation
    x_alloc <- f_pos(alpha)

    # Optimal within budget or target confidence
    if (is.numeric(budget) || is.numeric(confidence)) {

      # Order by f(f+(a))/f+(a)
      rank_values <- f_obj(x_alloc)/x_alloc
      rank_values[which(!is.finite(rank_values))] <- 0
      idx <- order(rank_values, decreasing = TRUE)

      # Determine allocation within budget
      nonzero <- which(x_alloc[idx] > 0)

      # Optimal within budget
      if (is.numeric(budget)) {
        cum_cost <- cumsum(x_alloc[idx][nonzero])
        over_budget <- which(cum_cost > budget)
        if (length(over_budget)) {
          x_alloc[idx][over_budget[1]] <-
            x_alloc[idx][over_budget[1]] - (cum_cost[over_budget[1]] - budget)
          x_alloc[idx][over_budget[-1]] <- 0
        }
      }

      # Optimal up to confidence-level
      if (is.numeric(confidence)) {

        # Unit sensitivity
        new_sens <- 1 - ((1 - exist_sens)*
                           exp(-1*lambda*(x_alloc - fixed_cost)/alloc_cost))

        # Calculate confidence
        if (relative_establish_pr) {
          cum_conf <-
            ((1 - cumprod((1 - establish_pr*new_sens)[idx][nonzero]))/
               (1 - prod(1 - establish_pr)))
        } else {
          cum_conf <- (cumsum((establish_pr*new_sens)[idx][nonzero])/
                         sum(establish_pr))
        }

        # Select allocation up to confidence level
        over_conf <- which(cum_conf > confidence)
        if (length(over_conf)) {
          x_alloc[idx][over_conf[-1]] <- 0
        }
      }
    }

    return(x_alloc)
  }

  # Create a class structure
  self <- structure(list(), class = c(class, "SurveillanceDesign"))

  # Get the allocated surveillance resource quantities for the design
  qty_alloc <- NULL
  self$get_allocation <- function() {

    if (is.null(qty_alloc)) {

      # No constraint
      best_alpha <- (optimal == "cost") - 1

      # Search for minimum objective via marginal benefit (alpha) values
      if (is.numeric(budget) || is.numeric(confidence)) {
        alpha_min <- min(f_deriv(fixed_cost))
        interval <- (0:10)/10*alpha_min
        alpha_range <- range(interval)[2] - range(interval)[1]
        precision <- 8 # for alpha
        best_alpha <- 0
        while (alpha_range > abs(best_alpha*10^(-1*precision))) {
          obj <- sapply(interval[-1], function(a) sum(f_obj(allocate(a))))
          i <- which.min(obj)
          best_alpha <- interval[i + 1]
          interval <- (0:10)/10*(interval[i + 2] - interval[i]) + interval[i]
          alpha_range <- range(interval)[2] - range(interval)[1]
        }
      }

      # Optimal allocation
      qty_alloc <- (allocate(best_alpha) - fixed_cost)/alloc_cost
      qty_alloc[which(qty_alloc < 0)] <- 0
    }

    return(qty_alloc)
  }

  # # Union when p_i is establishment probability
  # system_conf <- ((1 - prod(1 - establish_pr*(1 - (1 - exist_sens)*
  #                                      exp(-1*lambda*(x_alloc - fixed_cost)))))/
  #                   (1 - prod(1 - establish_pr))); system_conf
  # # Approximation when p_i is relative establishment risk (or small)
  # system_conf <- sum(establish_pr*(1 - (1 - exist_sens)*exp(-1*lambda*(x_alloc - fixed_cost))))/sum(establish_pr); system_conf
  #

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
