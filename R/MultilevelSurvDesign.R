#' Multilevel sampling surveillance design class builder
#'
#' Builds a class to represent a surveillance design functionality for the
#' effective allocation of surveillance sampling across one or more levels or
#' stages via multilevel sampling methods specified with surveillance detection
#' sensitivities, sample costs, and overall detection confidence.
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
#' @param sample_cost The cost of individual samples. Default is \code{1}.
#'   An attribute \code{units} may be used to specify the cost units (e.g. "$"
#'   or "hours").
#' @param confidence The desired (minimum) system detection sensitivity or
#'   confidence of the surveillance design. Default is \code{0.95}.
#' @param ... Additional parameters.
#' @return A \code{MultilevelSurvDesign} class object (list) containing
#'   inherited and extended functions from the base \code{SamplingSurvDesign}
#'   class for for allocating resources, and calculating (unit and overall)
#'   detection sensitivities:
#'   \describe{
#'     \item{\code{get_allocation()}}{Get allocated surveillance resources for
#'       each level to achieve a minimal total cost given a specified desired
#'       confidence level.}
#'     \item{\code{get_sensitivity()}}{Get the level detection
#'        sensitivities of the allocated surveillance design.}
#'     \item{\code{get_confidence()}}{Get the overall system sensitivity or
#'       confidence of the allocated surveillance design.}
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
#' @include SamplingSurvDesign.R
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
                                 confidence = 0.95,
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
                                         confidence = 0.95,
                                         class = character(), ...) {

  # Resolve prevalence and total_indiv when lowest level is continuous
  sample_type <- match.arg(sample_type)
  if (sample_type == "continuous") {
    if (!is.null(prevalence) && is.na(prevalence[1])) {
      prevalence[1] <- 0 # design density used instead
    }
    if (is.null(total_indiv) ||
        (!is.null(total_indiv) && is.na(total_indiv[1]))) {
      total_indiv[1] <- 0 # resolve later
    }
  }

  # Build via base class
  self <- SamplingSurvDesign(context = context,
                             divisions = divisions,
                             establish_pr = 1,
                             sample_sens = sample_sens,
                             sample_type = sample_type,
                             prevalence = prevalence,
                             total_indiv = total_indiv,
                             design_dens = design_dens,
                             sample_area = sample_area,
                             optimal = "detection", # no separate mgmt costs
                             sample_cost = sample_cost,
                             confidence = confidence,
                             class = "MultilevelSurvDesign", ...)

  # Number of levels
  nlevels <- divisions$get_parts()

  # Set lowest level total_indiv when discrete
  if (is.null(total_indiv)) {
    total_indiv_present <- rep(FALSE, nlevels)
  } else {
    total_indiv_present <- (!is.na(total_indiv) & total_indiv > 0)
  }
  if (!total_indiv_present[1]) {
    total_indiv[1] <- 0 # resolve later
  }

  # Function to calculate level sensitivities
  calculate_sensitivity <- function(n_alloc) {

    # Calculate sensitivity at each level
    level_sens <- rep(NA, nlevels)
    for (l in 1:nlevels) {

      # Resolve sample sensitivity p for level
      if (l == 1) {
        p <- sample_sens
      } else {
        p <- level_sens[l - 1]
      }

      # Calculate level sensitivity
      if (l == 1 && sample_type == "continuous") {
        level_sens[l] <- 1 - exp(-1*p*n_alloc[l]*sample_area*design_dens[l])
      } else { # discrete
        if (total_indiv_present[l] && n_alloc[l]/total_indiv[l] > 0.1) {
          level_sens[l] <-
            1 - ((1 - p*min(1, n_alloc[l]/total_indiv[l]))^
                   (prevalence[l]*total_indiv[l]))
        } else {
          level_sens[l] <- 1 - (1 - p*prevalence[l])^n_alloc[l]
        }
      }
    }

    return(level_sens)
  }

  # Function to calculate level costs
  calculate_cost <- function(n_alloc) {
    sum(sapply(1:nlevels, function(l) prod(n_alloc[l:nlevels])*sample_cost[l]))
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
      n_min <- rep(1, nlevels)
      n_max <- total_indiv

      # Refine minimum allocated values and cost via confidence constraint
      cost_min <- calculate_cost(total_indiv)
      for (l in 1:nlevels) {
        n_alloc <- n_max
        decr <- rep(0, nlevels)
        decr[l] <- 1
        while (calculate_sensitivity(n_alloc - decr)[nlevels] >=
               confidence && n_alloc[l] - 1 >= n_min[l]) {
          n_alloc[l] <- n_alloc[l] - 1
        }
        n_min[l] <- n_alloc[l]
        n_cost <- calculate_cost(n_alloc)
        if (n_cost < cost_min) {
          cost_min <- n_cost
        }
      }

      # Refine maximum allocated values via minimum cost (so far)
      for (l in 1:nlevels) {
        n_alloc <- n_min
        incr <- rep(0, nlevels)
        incr[l] <- 1
        while (calculate_cost(n_alloc + incr) < cost_min &&
               n_alloc[l] + 1 <= n_max[l]) {
          n_alloc[l] <- n_alloc[l] + 1
        }
        n_max[l] <- n_alloc[l]
      }

      # Generate all combinations of allocated values between limits
      n_comb <- lapply(1:nlevels, function(l) n_min[l]:n_max[l])
      n_comb <- unname(as.matrix(expand.grid(n_comb)))

      # Select combinations satisfying the sensitivity constraint
      idx <- which(apply(n_comb, 1, function(n) {
        calculate_sensitivity(n)[nlevels]}) >= confidence)
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

  # Get the overall system sensitivity/confidence of the surveillance design
  self$get_confidence <- function() {
    system_sens <- NULL
    return(system_sens)
  }

  return(self)
}
