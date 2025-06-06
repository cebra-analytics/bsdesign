#' Multilevel sampling surveillance design class builder
#'
#' Builds a class to represent surveillance design functionality for the
#' effective allocation of surveillance sampling across one or more levels or
#' stages via multilevel sampling methods specified with surveillance detection
#' sensitivities, sample costs, and overall detection probability.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of a bio-security surveillance and area freedom design.
#' @param divisions A \code{Divisions} or inherited class object representing
#'   one or more multistage levels, for the surveillance design. The levels
#'   should be ordered from lowest to highest (e.g. leaves, trees, rows,
#'   orchards). Only the lowest level can utilize either continuous
#'   density-based or discrete sampling, higher levels can only utilize
#'   discrete sampling. Note that more than five levels may not be
#'   computationally feasible.
#' @param sample_sens A single sample sensitivity value for the lowest level
#'  of a multilevel/stage sampling. Default is \code{1}.
#' @param sample_type The type of sampling used for the lowest level of the
#'   multilevel sampling. One of \code{"discrete"} or \code{"continuous"}.
#' @param prevalence A vector of discrete sampling design prevalence values for
#'   each multistage level specified by \code{divisions}. Note that this
#'   parameter may represent apparent prevalence (Cannon, 2009) when the
#'   \code{sensitivity} is set to \code{1}. Default is \code{NULL}.
#' @param total_indiv A vector of total individual discrete sampling units
#'   (e.g. leaves, trees, rows, orchards) present at each multistage level
#'   specified by \code{divisions}. Default is \code{NULL}.
#' @param design_dens A single design density value for when the lowest level
#'   of the multilevel sampling is continuous. Default is \code{NULL}.
#' @param sample_area The area of a single sample when the lowest level for
#'   when the lowest level of the multilevel sampling is continuous. Note that
#'   when set to 1, the total number of samples will be equivalent to the total
#'   area sampled. Default is \code{NULL}.
#' @param sample_cost The cost of samples at each level. Default is \code{1}.
#'   An attribute \code{units} may be used to specify the cost units (e.g. "$"
#'   or "hours").
#' @param system_sens The desired (minimum) system sensitivity or detection
#'   probability of the surveillance design (e.g. 0.95). Default is
#'   \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{MultilevelSurvDesign} class object (list) containing
#'   inherited and extended functions from the base \code{SurveillanceDesign}
#'   class for for allocating resources, and calculating (unit and overall)
#'   detection sensitivities:
#'   \describe{
#'     \item{\code{get_context()}}{Get context object.}
#'     \item{\code{get_divisions()}}{Get divisions object.}
#'     \item{\code{get_allocation()}}{Get allocated surveillance resources for
#'       each level to achieve a minimal total cost given a specified desired
#'       system sensitivity or detection probability.}
#'     \item{\code{get_sensitivity()}}{Get the level detection
#'       sensitivities of the allocated surveillance design.}
#'     \item{\code{get_system_sens(growth = NULL)}}{Get the overall system
#'       sensitivity or detection probability of the allocated surveillance
#'       design. The optional \code{growth} parameter may provide a vector of
#'       relative increasing multipliers (e.g. 1, 1.8, 4.3, 7.5) applied to the
#'       discrete sampling design \code{prevalence} over time or a sequence of
#'       repeated surveillance efforts, which provide a proxy for invasive
#'       species growth. When present, increasing system sensitivity values are
#'       returned for each multiplier or time/repeat.}
#'     \item{\code{save_design()}}{Save the surveillance design as a
#'       collection of comma-separated value (CSV) files, including the
#'       surveillance \code{allocation}, \code{sensitivity}, and a
#'       \code{summary} of sample costs (when applicable) and the system
#'       sensitivity (detection probability).}
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
#' @export
MultilevelSurvDesign <- function(context,
                                 divisions,
                                 sample_sens = 1,
                                 sample_type = c("discrete", "continuous"),
                                 prevalence = NULL,
                                 total_indiv = NULL,
                                 design_dens = NULL,
                                 sample_area = NULL,
                                 sample_cost = 1,
                                 system_sens = 0.95,
                                 class = character(), ...) {
  UseMethod("MultilevelSurvDesign")
}

#' @name MultilevelSurvDesign
#' @export
MultilevelSurvDesign.Context <- function(context,
                                         divisions,
                                         sample_sens = 1,
                                         sample_type = c("discrete",
                                                         "continuous"),
                                         prevalence = NULL,
                                         total_indiv = NULL,
                                         design_dens = NULL,
                                         sample_area = NULL,
                                         sample_cost = 1,
                                         system_sens = 0.95,
                                         class = character(), ...) {

  # Build via base class
  self <- SurveillanceDesign(context = context,
                             divisions = divisions,
                             establish_pr = 1,
                             optimal = "cost",
                             mgmt_cost = list("not used"), # avoid error
                             system_sens = system_sens,
                             class = "MultilevelSurvDesign", ...)

  # Number of levels
  num_levels <- divisions$get_parts()

  # Discrete or continuous sampling?
  sample_type <- match.arg(sample_type)

  # Check sampling parameters (not in base class)
  if (!is.numeric(sample_sens) || sample_sens < 0 || sample_sens > 1) {
    stop(paste("The sample sensitivity parameter must be a numeric value",
               ">= 0 and <= 1."), call. = FALSE)
  }
  if (sample_type == "discrete" &&
      (is.null(prevalence) || !is.numeric(prevalence) ||
       any(prevalence < 0) || any(prevalence > 1) ||
       !length(prevalence) %in% c(1, num_levels))) {
    stop(paste("The prevalence parameter must be numeric with values",
               ">= 0 and <= 1 for each level."), call. = FALSE)
  } else if (sample_type == "continuous" &&
             ((is.null(prevalence) && num_levels > 1) ||
              (num_levels > 1 &&
               (!is.numeric(prevalence[-1]) || any(prevalence[-1] < 0) ||
                any(prevalence[-1] > 1) ||
                !length(prevalence) %in% c(1, num_levels))))) {
    stop(paste("The prevalence parameter must be numeric with values",
               ">= 0 and <= 1 for each level above the first."), call. = FALSE)
  }
  if (sample_type == "discrete" && !is.null(total_indiv) &&
      (!is.numeric(total_indiv) || any(total_indiv < 0) ||
       length(total_indiv) != num_levels)) {
    stop(paste("The total individuals parameter must be numeric with values",
               ">= 0 for each level."), call. = FALSE)
  } else if (sample_type == "continuous" && !is.null(total_indiv) &&
             num_levels > 1 &&
             (!is.numeric(total_indiv[-1]) || any(total_indiv[-1] <= 0) ||
              length(total_indiv) != num_levels)) {
    stop(paste("The total individuals parameter must be numeric with values",
               ">= 0 for each level above the first."), call. = FALSE)
  }
  if (sample_type == "continuous" &&
      (is.null(design_dens) || is.null(sample_area))) {
    stop(paste("The design density and sample area parameters are required",
               "for continuous sampling."), call. = FALSE)
  }
  if (!is.null(design_dens) &&
      (!is.numeric(design_dens) || design_dens <= 0)) {
    stop("The design density parameter must be a numeric value > 0.",
         call. = FALSE)
  }
  if (!is.null(sample_area) &&
      (!is.numeric(sample_area) || sample_area <= 0)) {
    stop("The sample area parameter must be a numeric value > 0.",
         call. = FALSE)
  }
  if (!is.null(sample_cost) &&
      (!is.numeric(sample_cost) ||
       !length(sample_cost) %in% c(1, num_levels))) {
    stop(paste("The sample cost parameter must be a numeric vector with ",
               "values for each level."), call. = FALSE)
  }

  # Resolve total_indiv when NULL or lowest level is NA
  if (is.null(total_indiv) ||
      (!is.null(total_indiv) && is.na(total_indiv[1]))) {
    total_indiv[1] <- 0 # maximum resolved later
  }

  # Resolve sample cost
  if (length(sample_cost) == 1) {
    sample_cost <- rep(sample_cost, num_levels)
  } else if (is.null(sample_cost)) {
    sample_cost <- rep(1, num_levels)
  }

  # Function to calculate level sensitivities
  calculate_sensitivity <- function(n_alloc, multi = 1) {

    # Calculate sensitivity at each level
    level_sens <- rep(NA, num_levels)
    for (l in 1:num_levels) {

      # Resolve sample sensitivity p for level
      if (l == 1) {
        p <- sample_sens
      } else {
        p <- level_sens[l - 1]
      }

      # Calculate level sensitivity
      if (l == 1 && sample_type == "continuous") {
        level_sens[l] <-
          1 - exp(-1*p*n_alloc[l]*sample_area*design_dens[l]*multi)
      } else { # discrete
        if (!is.null(total_indiv) && total_indiv[l] > 0 &&
            n_alloc[l]/total_indiv[l] > 0.1) {
          level_sens[l] <-
            1 - ((1 - p*min(1, n_alloc[l]/total_indiv[l]))^
                   (prevalence[l]*multi*total_indiv[l]))
        } else {
          level_sens[l] <- 1 - (1 - p*prevalence[l]*multi)^n_alloc[l]
        }
      }
    }

    return(level_sens)
  }

  # Function to calculate level costs
  calculate_cost <- function(n_alloc) {
    sum(sapply(1:num_levels,
               function(l) prod(n_alloc[l:num_levels])*sample_cost[l]))
  }

  # Get the allocated surveillance resource values of the surveillance design
  qty_alloc <- NULL
  self$get_allocation <- function() {
    if (is.null(qty_alloc)) {

      # Resolve total_indiv when lowest level is zero
      if (total_indiv[1] == 0) {
        while (calculate_sensitivity(total_indiv)[1] < 1) {
          total_indiv[1] <- total_indiv[1] + 1
        }
      }

      # Initial minimum and maximum allocated n values
      n_min <- rep(1, num_levels)
      n_max <- total_indiv

      # Resolve n_max when lowest level is zero
      if (n_max[1] == 0) {
        while (calculate_sensitivity(n_max)[1] < 1) {
          n_max[1] <- total_indiv[1] + 1
        }
      }

      # Refine minimum allocated values and cost via system sensitivity
      # constraint
      cost_min <- calculate_cost(n_max)
      for (l in 1:num_levels) {
        n_alloc <- n_max
        decr <- rep(0, num_levels)
        decr[l] <- 1
        while (calculate_sensitivity(n_alloc - decr)[num_levels] >=
               system_sens && n_alloc[l] - 1 >= n_min[l]) {
          n_alloc[l] <- n_alloc[l] - 1
        }
        n_min[l] <- n_alloc[l]
        n_cost <- calculate_cost(n_alloc)
        if (n_cost < cost_min) {
          cost_min <- n_cost
        }
      }

      # Refine maximum allocated values via minimum cost (so far)
      for (l in 1:num_levels) {
        n_alloc <- n_min
        incr <- rep(0, num_levels)
        incr[l] <- 1
        while (calculate_cost(n_alloc + incr) <= cost_min &&
               n_alloc[l] + 1 <= n_max[l]) {
          n_alloc[l] <- n_alloc[l] + 1
        }
        n_max[l] <- n_alloc[l]
      }

      # Generate all combinations of allocated values between limits
      n_comb <- lapply(1:num_levels, function(l) n_min[l]:n_max[l])
      n_comb <- unname(as.matrix(expand.grid(n_comb)))

      # Select combinations satisfying the sensitivity constraint
      idx <- which(apply(n_comb, 1, function(n) {
        calculate_sensitivity(n)[num_levels]}) >= system_sens)
      n_comb <- as.matrix(n_comb[idx,])

      # Select values of allocated values corresponding to minimum total cost
      idx <- which.min(apply(n_comb, 1, function(n) calculate_cost(n)))
      qty_alloc <<- n_comb[idx,]

    }

    return(qty_alloc)
  }

  # Get the detection sensitivities for each level of the design
  sensitivity <- NULL
  self$get_sensitivity <- function() {
    if (is.null(sensitivity) && !is.null(qty_alloc)) {
      sensitivity <<- calculate_sensitivity(qty_alloc)
    }
    return(sensitivity)
  }

  # Get the overall system sensitivity or detection probability of the design
  self$get_system_sens <- function(growth = NULL) {

    # Set single growth when not specified
    if (!is.numeric(growth)) {
      growth <- 1
    }

    # Calculate system sensitivity for each value of growth
    system_sens_vect <- NULL
    if (!is.null(qty_alloc)) {
      for (multi in growth) {
        sensitivity <- calculate_sensitivity(qty_alloc, multi)
        if (!is.null(sensitivity)) {
          system_sens_vect <- c(system_sens_vect, sensitivity[num_levels])
        }
      }
    }

    return(system_sens_vect)
  }

  # Save the surveillance design as a collection of appropriate files
  self$save_design <- function() {

    # Save allocation and sensitivity
    write.csv(cbind(divisions$get_data(),
                    samples = self$get_allocation(),
                    sensitivity = self$get_sensitivity()),
              file = "design.csv", row.names = FALSE)

    # Save summary
    if (!all(sample_cost == 1)) {
      summary_data <- data.frame(
        sample_cost = sum(self$get_allocation()*sample_cost),
        system_sens = self$get_system_sens())
    } else {
      summary_data <- data.frame(system_sens = self$get_system_sens())
    }
    write.csv(summary_data, file = "summary.csv", row.names = FALSE)

    return(summary_data)
  }

  return(self)
}
