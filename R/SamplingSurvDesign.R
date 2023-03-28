#' Sampling surveillance design class builder
#'
#' Builds a class to represent surveillance design functionality for the
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
#' @param sample_sens A vector of sample sensitivity values for each division
#'   part (location, category, etc.) specified by \code{divisions}. Default
#'   is \code{1}.
#' @param sample_type The type of sampling used. One of \code{"discrete"} or
#'   \code{"continuous"}.
#' @param prevalence A vector of discrete sampling design prevalence values for
#'   each division part (location, category, etc.) specified by
#'   \code{divisions}. Note that this parameter may represent apparent
#'   prevalence (Cannon, 2009) when the \code{sample_sens} is set to \code{1}.
#'   Default is \code{NULL}.
#' @param total_indiv A vector of total individual discrete sampling units
#'   (e.g. trees, traps) present at each division part (location, category,
#'   etc.) specified by \code{divisions}. Default is \code{NULL}.
#' @param design_dens A vector of continuous sampling design density values for
#'   each division part (location, category, etc.). Default is \code{NULL}.
#' @param sample_area The area of a single sample in a continuous sampling
#'   design. Note that when set to \code{1}, the total number of samples will
#'   be equivalent to the total area sampled. Default is \code{NULL}.
#' @param optimal The strategy used for finding an effective surveillance
#'   resource allocation. One of (minimum) \code{"cost"}, (maximum)
#'   \code{"benefit"}, (maximum) \code{"detection"} sensitivity (up to
#'   \code{"confidence"} level when specified), or \code{"none"} for
#'   representing existing surveillance designs only.
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
#' @param sample_cost A vector of cost per sample of allocated surveillance
#'   resources at each division part (location, category, etc.) specified by
#'   \code{divisions}. Default is \code{NULL}. Units should be consistent with
#'   the \code{cost_unit} parameter specified in the \code{context}.
#' @param fixed_cost A vector of fixed costs, such as travel costs or time, at
#'   each division part (location, category, etc.) specified by
#'   \code{divisions}. Default is \code{NULL}. Units should be consistent with
#'   \code{sample_cost} when specified. Otherwise the units should be
#'   consistent with the \code{surv_qty_unit} parameter specified in the
#'   \code{context} (e.g. traps or samples).
#' @param budget The cost budget or constraint for the sampling allocation in
#'   the surveillance design. Default is \code{NULL}. Units should be
#'   consistent with \code{sample_cost} when specified. Otherwise the units
#'   should be consistent with the \code{surv_qty_unit} parameter specified in
#'   the \code{context} (e.g. traps or samples).
#' @param confidence The desired (minimum) system sensitivity or detection
#'   confidence of the surveillance design (e.g. 0.95). Default is \code{NULL}.
#' @param exist_alloc A vector of existing surveillance resource quantities at
#'   each division part (location, category, etc.) specified by
#'   \code{divisions}. Should only be used to represent existing surveillance
#'   designs when \code{optimal = "none"}. Default is \code{NULL}.
#' @param exist_sens A vector, or list of vectors, of detection sensitivity
#'   values of existing surveillance present at each division part (location,
#'   category, etc.) specified by \code{divisions}. Multiple existing
#'   surveillance layers may be specified in a list. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{SamplingSurvDesign} class object (list) containing inherited
#'   and extended functions from the base \code{SurveillanceDesign} class for
#'   for allocating samples, and calculating (unit and overall) detection
#'   sensitivities:
#'   \describe{
#'     \item{\code{get_allocation()}}{Get allocated samples via specified
#'       strategy, utilizing costs, benefits, budget constraints, and/or
#'       desired detection confidence level.}
#'     \item{\code{get_sensitivity()}}{Get the division part detection
#'       sensitivities of the allocated surveillance design combined with any
#'       existing sensitivities specified via \code{exist_sens}.}
#'     \item{\code{get_confidence(growth = NULL)}}{Get the overall system
#'       sensitivity or detection confidence of the allocated surveillance
#'       design. The optional \code{growth} parameter may provide a vector of
#'       relative increasing multipliers (e.g. 1, 1.8, 4.3, 7.5) applied to the
#'       discrete sampling design \code{prevalence}, or the continuous sampling
#'       design density (\code{design_dens}), over time or a sequence of
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
#' @include LagrangeSurvDesign.R
#' @export
SamplingSurvDesign <- function(context,
                               divisions,
                               establish_pr,
                               sample_sens = 1,
                               sample_type = c("discrete", "continuous"),
                               prevalence = NULL,
                               total_indiv = NULL,
                               design_dens = NULL,
                               sample_area = NULL,
                               optimal = c("cost", "benefit", "detection",
                                           "none"),
                               mgmt_cost = list(),
                               benefit = NULL,
                               sample_cost = NULL,
                               fixed_cost = NULL,
                               budget = NULL,
                               confidence = NULL,
                               exist_alloc = NULL,
                               exist_sens = NULL,
                               class = character(), ...) {
  UseMethod("SamplingSurvDesign")
}

#' @name SamplingSurvDesign
#' @export
SamplingSurvDesign.Context <- function(context,
                                       divisions,
                                       establish_pr,
                                       sample_sens = 1,
                                       sample_type = c("discrete",
                                                       "continuous"),
                                       prevalence = NULL,
                                       total_indiv = NULL,
                                       design_dens = NULL,
                                       sample_area = NULL,
                                       optimal = c("cost", "benefit",
                                                   "detection", "none"),
                                       mgmt_cost = list(),
                                       benefit = NULL,
                                       sample_cost = NULL,
                                       fixed_cost = NULL,
                                       budget = NULL,
                                       confidence = NULL,
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
                             fixed_cost = fixed_cost,
                             budget = budget,
                             confidence = confidence,
                             exist_alloc = exist_alloc,
                             exist_sens = exist_sens,
                             class = "SamplingSurvDesign", ...)

  # Number of division parts
  parts <- divisions$get_parts()

  # Ensure establish_pr has values for each division part
  if (length(establish_pr) == 1) {
    establish_pr <- rep(establish_pr, parts)
  }

  # Resolve if establish_pr is relative
  if ((!is.null(attr(establish_pr, "relative")) &&
       as.logical(attr(establish_pr, "relative"))) || max(establish_pr) > 1) {
    relative_establish_pr <- TRUE
  } else {
    relative_establish_pr <- FALSE
  }

  # Check sampling parameters (not in base class)
  if (!is.numeric(sample_sens) || any(sample_sens < 0) ||
      any(sample_sens > 1) || !length(sample_sens) %in% c(1, parts)) {
    stop(paste("The sample sensitivity parameter must be numeric with values",
               ">= 0 and <= 1 for each division part."), call. = FALSE)
  }
  if (!is.null(prevalence) &&
      (!is.numeric(prevalence) || any(prevalence < 0) ||
       any(prevalence > 1) || !length(prevalence) %in% c(1, parts))) {
    stop(paste("The prevalence parameter must be numeric with values",
               ">= 0 and <= 1 for each division part."), call. = FALSE)
  }
  if (!is.null(total_indiv) &&
      (!is.numeric(total_indiv) || any(total_indiv <= 0) ||
       length(total_indiv) != parts)) {
    stop(paste("The total individuals parameter must be numeric with values",
               "> 0 for each division part."), call. = FALSE)
  }
  if (!is.null(design_dens) &&
      (!is.numeric(design_dens) || any(design_dens < 0) ||
       !length(design_dens) %in% c(1, parts))) {
    stop(paste("The design density parameter must be numeric with values",
               ">= 0 for each division part."), call. = FALSE)
  }
  if (!is.null(sample_area) &&
      (!is.numeric(sample_area) || sample_area <= 0)) {
    stop("The sample area parameter must be numeric with value > 0.",
         call. = FALSE)
  }
  if (!is.null(sample_cost) &&
      (!is.numeric(sample_cost) || !length(sample_cost) %in% c(1, parts))) {
    stop(paste("The sample cost parameter must be a numeric vector with ",
               "values for each division part."), call. = FALSE)
  }

  # Discrete or continuous sampling?
  sample_type <- match.arg(sample_type)
  if (sample_type == "discrete" && is.null(prevalence)) {
    stop("The prevalence parameter is required for discrete sampling.",
         call. = FALSE)
  } else if (sample_type == "continuous" &&
             (is.null(design_dens) || is.null(sample_area))) {
    stop(paste("The design density and sample area parameters are required",
               "for continuous sampling."), call. = FALSE)
  }

  # Match optimal arguments
  optimal <- match.arg(optimal)

  # Resolve sample_sens, prevalence, total_indiv, sample_cost, fixed_cost,
  # and exist_sens
  if (length(sample_sens) == 1) {
    sample_sens <- rep(sample_sens, parts)
  }
  if (length(prevalence) == 1) {
    prevalence <- rep(prevalence, parts)
  }
  if (length(total_indiv) == 1) {
    total_indiv <- rep(total_indiv, parts)
  }
  if (length(sample_cost) == 1) {
    sample_cost <- rep(sample_cost, parts)
  } else if (is.null(sample_cost)) {
    sample_cost <- rep(1, parts)
  }
  if (length(fixed_cost) == 1) {
    fixed_cost <- rep(fixed_cost, parts)
  } else if (is.null(fixed_cost)) {
    fixed_cost <- rep(0, parts)
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
  ## given the surveillance sampling resource allocation quantity qty_alloc
  ## where qty_alloc = (x_alloc - fixed_cost)/sample_cost

  # Define lambda via sampling parameters
  if (sample_type == "discrete") {

    # When no total individuals N specified or when sample fraction n/N <= 0.1
    lambda <- -1*log(1 - sample_sens*prevalence) # n/N <= 0.1 only

    # Sample fraction n/N > 0.1, thus use different equations below
    sample_fract_gt_0_1 <- FALSE # resolve later

  } else { # continuous
    lambda <- sample_sens*sample_area*design_dens
  }

  # Objective function
  f_obj <- function(x_alloc) {
    if (sample_type == "discrete" && sample_fract_gt_0_1 &&
        optimal == "detection" && !relative_establish_pr) {
      # Maximum detection for discrete sampling with n/N > 0.1
      return(
        (x_alloc >= fixed_cost)*
          log(1 - (establish_pr*
                     (1 - ((1 - exist_sens)*
                             ((1 - (sample_sens/total_indiv*
                                     (x_alloc - fixed_cost)/sample_cost))
                              ^(prevalence*total_indiv)))))))
    } else if (sample_type == "discrete" && sample_fract_gt_0_1) {
      # Minimum cost or maximum benefit (benefit = 1 for detection)
      # for discrete sampling with n/N > 0.1
      incl_x <- (optimal == "cost")
      return(
        (benefit*establish_pr*(1 - exist_sens)*
           ((x_alloc < fixed_cost)*1 +
              ((x_alloc >= fixed_cost)*
                 ((1 - (sample_sens/total_indiv*
                          (x_alloc - fixed_cost)/sample_cost))
                  ^(prevalence*total_indiv))))) +
          (x_alloc >= fixed_cost)*x_alloc*incl_x)
    } else if (optimal == "detection" && !relative_establish_pr) {
      # Maximum detection
      return(
        (x_alloc >= fixed_cost)*
          log(1 - (establish_pr*
                     (1 - ((1 - exist_sens)*
                             exp(-1*lambda*
                                   (x_alloc - fixed_cost)/sample_cost))))))
    } else {
      # Minimum cost or maximum benefit (benefit = 1 for detection)
      incl_x <- (optimal == "cost")
      return(
        (benefit*establish_pr*(1 - exist_sens)*
           ((x_alloc < fixed_cost)*1 +
              ((x_alloc >= fixed_cost)*
                 exp(-1*lambda*(x_alloc - fixed_cost)/sample_cost)))) +
          (x_alloc >= fixed_cost)*x_alloc*incl_x)
    }
  }

  # Derivative of objective function
  f_deriv <- function(x_alloc) {
    if (sample_type == "discrete" && sample_fract_gt_0_1) {
      # Use derivative of second objective function (above) for all discrete
      # sampling with n/N > 0.1 (avoids unsolvable pseudo-inverse)
      incl_x <- (optimal == "cost")
      return(
        (x_alloc >= fixed_cost)*
          (1*incl_x -
             (benefit*establish_pr*(1 - exist_sens)*
                sample_sens*prevalence/sample_cost*
                ((1 - (sample_sens/total_indiv*
                         (x_alloc - fixed_cost)/sample_cost))
                 ^(prevalence*total_indiv - 1)))))
    } else if (optimal == "detection" && !relative_establish_pr) {
      # Maximum detection
      return(
        (x_alloc >= fixed_cost)*-1*establish_pr*(1 - exist_sens)*
          lambda/sample_cost*exp(-1*lambda*(x_alloc - fixed_cost)/sample_cost)/
          (1 - (establish_pr*
                  (1 - ((1 - exist_sens)*
                          exp(-1*lambda*
                                (x_alloc - fixed_cost)/sample_cost))))))
    } else {
      # Minimum cost or maximum benefit (benefit = 1 for detection)
      incl_x <- (optimal == "cost")
      return(
        (x_alloc >= fixed_cost)*
          (1*incl_x - (benefit*establish_pr*(1 - exist_sens)*
                         lambda/sample_cost*
                         exp(-1*lambda*(x_alloc - fixed_cost)/sample_cost))))
    }
  }

  # Pseudo-inverse of derivative given marginal benefit alpha
  f_pos <- function(alpha) {
    if (sample_type == "discrete" && sample_fract_gt_0_1) {
      # Use pseudo-inverse of derivative of objective function (above) for all
      # discrete sampling with n/N > 0.1 (detection version unsolvable)
      values <- (benefit*establish_pr*sample_sens*prevalence*total_indiv*
                   (1 - exist_sens))
      idx <- which(values > 0)
      values[-idx] <- 0
      incl_x <- (optimal == "cost")
      values[idx] <-
        ((total_indiv[idx]*sample_cost[idx]/sample_sens[idx]*
            (1 - (-1*(alpha - 1*incl_x)*total_indiv[idx]*sample_cost[idx]/
                    values[idx])^(1/(prevalence[idx]*total_indiv[idx] - 1))))
         + fixed_cost[idx])
    } else {
      values <- lambda/sample_cost*benefit*establish_pr*(1 - exist_sens)
      idx <- which(values > 0)
      values[-idx] <- 0
      if (optimal == "detection" && !relative_establish_pr) {
        # Maximum detection
        values[idx] <-
          pmax(0, ((alpha > -1*lambda[idx]/sample_cost[idx])*
                     (sample_cost[idx]/lambda[idx]*
                        (log(-1*lambda[idx]/sample_cost[idx]/alpha - 1) -
                           log(1/establish_pr[idx] - 1) +
                           log(1 - exist_sens[idx])) +
                        fixed_cost[idx])))
      } else {
        # Minimum cost or maximum benefit (benefit = 1 for detection)
        incl_x <- (optimal == "cost")
        values[idx] <-
          (((alpha - 1*incl_x) >= -1*values[idx])*
             (-1*sample_cost[idx]/lambda[idx]*
                log(-1*(alpha - 1*incl_x)/(values[idx])) + fixed_cost[idx]))
      }
    }
    return(values)
  }

  # Unconstrained marginal benefit alpha
  alpha_unconstr <- (optimal == "cost") - 1

  # Check if the sample fraction n/N > 0.1, thus use different equations above
  if (sample_type == "discrete" && is.numeric(total_indiv)) {

    # Estimate sample fraction when unconstrained
    if (is.null(budget) && is.null(confidence)) {

      # Calculate sample number via pseudo-inverse of derivative given
      # unconstrained alpha with sample fraction n/N > 0.1 assumed
      sample_fract_gt_0_1 <- TRUE
      sample_n <- (f_pos(alpha_unconstr) - fixed_cost)/sample_cost

      # Are any n/N > 0.1?
      sample_fract_gt_0_1 <- any(sample_n/total_indiv > 0.1)

    } else {

      # Estimate sample fraction required to meet confidence level
      if (is.numeric(confidence)) {

        # Calculate n/N to gain specified confidence for each part
        sample_fract <-
          ceiling(total_indiv/sample_sens*
                    (1 - (((1 - confidence)/(1 - exist_sens))^
                            (1/prevalence/total_indiv))))/total_indiv

        # Are any n/N > 0.1?
        sample_fract_gt_0_1 <- any(sample_fract > 0.1)
      }

      # Estimate sample fraction implied by the budget
      if (is.numeric(budget)) {
        sample_fract_gt_0_1 <-
          (sample_fract_gt_0_1 ||
             budget > sum(total_indiv*0.1*sample_cost + fixed_cost))
      }
    }
  }

  # Minimum marginal benefit alpha
  alpha_min <- min(f_deriv(fixed_cost))

  # Function for calculating unit sensitivity
  f_unit_sens <- function(x_alloc) {
    if (sample_type == "discrete" && sample_fract_gt_0_1) {

      # Discrete sample fraction n/N > 0.1
      return(1 - ((1 - exist_sens)*
                    ((1 - (sample_sens*(x_alloc - (x_alloc > 0)*fixed_cost)/
                             sample_cost/total_indiv))^
                       (prevalence*total_indiv))))
    } else {

      # Discrete sample fraction n/N <= 1 or continuous
      return(1 - ((1 - exist_sens)*
                    exp((-1*lambda*(x_alloc - (x_alloc > 0)*fixed_cost)/
                           sample_cost))))
    }
  }

  # Function for calculating inverse of unit sensitivity
  f_inv_unit_sens <- function(unit_sens) {
    if (sample_type == "discrete" && sample_fract_gt_0_1) {

      # Discrete sample fraction n/N > 0.1
      x_alloc <- (total_indiv*sample_cost/sample_sens*
                    ((1 - ((1 - unit_sens)/(1 - exist_sens))^
                        (1/(prevalence*total_indiv)))))
    } else {

      # Discrete sample fraction n/N <= 1 or continuous
      x_alloc <- -1*sample_cost/lambda*log((1 - unit_sens)/(1 - exist_sens))
    }

    return(x_alloc + (x_alloc > 0)*fixed_cost)
  }

  # Get the allocated surveillance resource values of the surveillance design
  qty_alloc <- NULL
  self$get_allocation <- function() {
    if (optimal != "none" && is.null(qty_alloc)) {

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
                                               confidence = confidence)
      x_alloc <- lagrangeSurvDesign$get_cost_allocation()

      # Optimal resource allocation
      qty_alloc <<- (x_alloc - fixed_cost)/sample_cost
      qty_alloc[which(qty_alloc < 0)] <<- 0
    }

    return(qty_alloc)
  }

  # Function for calculating sensitivities
  calculate_sensitivity <- function(multi = 1) { # internal version

    # Generated or existing allocation?
    n_alloc <- NULL
    if (optimal != "none" && !is.null(qty_alloc)) {
      n_alloc <- qty_alloc
    } else if (optimal == "none" && !is.null(exist_alloc)) {
      n_alloc <- exist_alloc
    }

    # Calculate sensitivities for allocation
    unit_sens <- NULL
    if (!is.null(n_alloc)) {

      # Check if discrete sample fraction n/N > 0.1
      if (sample_type == "discrete") {
        if (is.numeric(total_indiv) && any(n_alloc/total_indiv > 0.1)) {
          unit_sens <-
            1 - (1 - exist_sens)*((1 - sample_sens*n_alloc/total_indiv)
                                  ^(prevalence*multi*total_indiv))
        } else {
          unit_sens <-
            1 - (1 - exist_sens)*(1 - sample_sens*prevalence*multi)^n_alloc
        }
      } else { # continuous
        unit_sens <-
          1 - ((1 - exist_sens)*
                 exp(-1*sample_sens*sample_area*n_alloc*design_dens*multi))
      }
    }

    return(unit_sens)
  }

  # Get the detection sensitivities for each division part of the design
  sensitivity <- NULL
  self$get_sensitivity <- function() { # class version
    if (is.null(sensitivity)) {
      sensitivity <<- calculate_sensitivity()
    }
    return(sensitivity)
  }

  # Get the overall system sensitivity or detection confidence of the design
  self$get_confidence <- function(growth = NULL) {

    # Set single growth when not specified
    if (!is.numeric(growth)) {
      growth <- 1
    }

    # Calculate system sensitivity for each value of growth
    system_sens <- NULL
    for (multi in growth) {
      sensitivity <- calculate_sensitivity(multi)
      if (!is.null(sensitivity)) {
        if (parts == 1) {
          system_sens <- c(system_sens, sensitivity)
        } else if (!is.null(establish_pr)) {
          if (relative_establish_pr) {
            system_sens <- c(system_sens,
                             sum(establish_pr*sensitivity)/sum(establish_pr))
          } else {
            system_sens <- c(system_sens,
                             ((1 - prod(1 - establish_pr*sensitivity))/
                              (1 - prod(1 - establish_pr))))
          }
        }
      }
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
    if (!all(sample_cost == 1)) {
      summary_data$allocation_cost <- sum(self$get_allocation()*sample_cost)
    }
    if (!all(fixed_cost == 0)) {
      summary_data$fixed_cost <- sum((self$get_allocation() > 0)*fixed_cost)
    }
    if (optimal == "cost") {
      summary_data$mgmt_cost <- sum(
        establish_pr*(mgmt_cost$detected*self$get_sensitivity() +
                        mgmt_cost$undetected*(1 - self$get_sensitivity())))
      summary_data$total_cost <-
        (summary_data$mgmt_cost +  sum(self$get_allocation()*sample_cost) +
           sum((self$get_allocation() > 0)*fixed_cost))
    }
    summary_data$detection_confidence <- self$get_confidence()
    write.csv(summary_data, file = "summary.csv", row.names = FALSE)

    return(summary_data)
  }

  # Save the area freedom design evidence
  self$save_design <- function() {

    # Save evidence
    evidence <- data.frame(iterations = 1:self$get_iterations(),
                           evidence = self$get_evidence())
    write.csv(evidence, file = "evidence.csv", row.names = FALSE)

    return(evidence)
  }

  return(self)
}
