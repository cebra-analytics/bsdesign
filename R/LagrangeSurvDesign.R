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
#' @param fixed_cost A vector of fixed costs, such as travel costs or time, at
#'   each division part (location, category, etc.) specified by
#'   \code{divisions}. Default is \code{NULL}. Units are specified in
#'   \code{alloc_units}.
#' @param budget The cost budget or constraint for the resource allocation in
#'   the surveillance design. Default is \code{NULL}. Units are specified in
#'   \code{alloc_units}.
#' @param confidence The desired (minimum) system detection sensitivity or
#'   confidence of the surveillance design (e.g. 0.95). Default is \code{NULL}.
#' @param exist_sens A vector of detection sensitivity values of existing
#'   surveillance present at each division part (location, category,
#'   etc.) specified by \code{divisions}. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{LagrangeSurvDesign} class object (list) containing inherited
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
                               alloc_units = NULL,
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
                                       alloc_units = NULL,
                                       fixed_cost = NULL,
                                       budget = NULL,
                                       confidence = NULL,
                                       exist_sens = NULL,
                                       class = character(), ...) {

  # Build via base class
  self <- SurveillanceDesign(context = context,
                             divisions = divisions,
                             establish_pr = establish_pr,
                             optimal = optimal,
                             mgmt_cost = mgmt_cost,
                             benefit = benefit,
                             alloc_units = alloc_units,
                             fixed_cost = fixed_cost,
                             budget = budget,
                             confidence = confidence,
                             exist_sens = exist_sens,
                             class = "LagrangeSurvDesign", ...)

  # Number of division parts
  parts <- divisions$get_parts()

  # Check lambda
  if (!is.numeric(lambda) || lambda < 0 || length(lambda) != parts) {
    stop(paste("The lambda parameter must be numeric,  >= 0, and match the",
               "number of division parts."), call. = FALSE)
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
    benefit <- rep(1, parts)
  }

  # Resolve null empty costs and existing sensitivities
  if (is.null(fixed_cost)) {
    fixed_cost <- rep(0, parts)
  }
  if (is.null(exist_sens)) {
    exist_sens <- rep(0, parts)
  }

  # Objective function
  f_obj <- function(x_alloc) {
    if (optimal == "detection") { # maximum detection
      return(
        (x_alloc >= fixed_cost)*
          log(1 - (establish_pr*(1 - ((1 - exist_sens)*
                                        exp(-1*lambda*
                                              (x_alloc - fixed_cost)))))))
    } else { # minimum cost or maximum benefit
      return(
        benefit*establish_pr*(1 - exist_sens)*
           ((x_alloc < fixed_cost)*1 +
              (x_alloc >= fixed_cost)*exp(-1*lambda*(x_alloc - fixed_cost))))
    }
  }

  # Derivative of objective function
  f_deriv <- function(x_alloc) {
    if (optimal == "detection") { # maximum detection
      return(
        (x_alloc >= fixed_cost)*-1*establish_pr*(1 - exist_sens)*lambda*
          exp(-1*lambda*(x_alloc - fixed_cost))/
          (1 - establish_pr*(1 - ((1 - exist_sens)*
                                    exp(-1*lambda*(x_alloc - fixed_cost))))))
    } else { # minimum cost or maximum benefit
      return(
        (x_alloc >= fixed_cost)*-1*lambda*benefit*establish_pr*
          (1 - exist_sens)*exp(-1*lambda*(x_alloc - fixed_cost)))
    }
  }

  # Pseudo-inverse of derivative given marginal benefit alpha
  f_pos <- function(alpha) {
    values <- lambda*benefit*establish_pr*(1 - exist_sens)
    idx <- which(values > 0)
    values[-idx] <- 0
    if (optimal == "detection") { # maximum detection
      values[idx] <-
        pmax(0, ((alpha > -1*lambda[idx])*
                   (1/lambda[idx]*(log(-1*lambda[idx]/alpha- 1) -
                                     log(1/establish_pr[idx] - 1) +
                                     log(1 - exist_sens[idx])) +
                      fixed_cost[idx])))
    } else { # minimum cost or maximum benefit
      values[idx] <-
        ((alpha>= -1*values[idx])*
           (-1/lambda[idx]*log(-1*alpha/(values[idx])) + fixed_cost[idx]))
    }
    return(values)
  }

  # Optimal allocation for an alpha value within budget or confidence level
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
        new_sens <- 1 - (1 - exist_sens)*exp(-1*lambda*(x_alloc - fixed_cost))

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

  # Get the allocated surveillance resource values of the design
  x_alloc <- NULL
  self$get_allocation <- function() {

    if (is.null(x_alloc)) {

      # Search for minimum objective via marginal benefit (alpha) values
      alpha_min <- min(f_deriv(fixed_cost))
      obj_range <- Inf
      interval <- (0:10)/10*alpha_min
      precision <- 10 # TODO automate ####
      while(obj_range > 10^(-1*precision)) {
        obj <- sapply(interval[-1], function(a) sum(f_obj(allocate(a))))
        obj_range <- range(obj)[2] - range(obj)[1]
        i <- which.min(obj)
        best_a <- interval[i + 1]
        interval <- (0:10)/10*(interval[i + 2] - interval[i]) + interval[i]
      }

      # Optimal allocation
      x_alloc <- allocate(best_a)
    }

    return(x_alloc)
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
