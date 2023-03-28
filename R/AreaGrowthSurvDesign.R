#' Area growth surveillance design class builder
#'
#' Builds a class to represent surveillance design functionality for the
#' cost-effective allocation of surveillance sampling densities across one or
#' more spatial sub-regions via a method that incorporates area-based
#' population growth, surveillance and management costs, including costs
#' associated with eradication, damages, and failure to contain incursions.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of a bio-security surveillance and area freedom design.
#' @param divisions A \code{Divisions} or inherited class object representing
#'   one or more sub-regions for the surveillance design.
#' @param subregion_area A vector of values to represent the area covered by
#'   each sub-region specified by \code{divisions}. Units should be consistent
#'   with the \code{dist_unit} parameter specified in the \code{context}.
#' @param establish_rate A vector of values to represent to the expected number
#'   of populations to establish in each sub-region (specified by
#'   \code{divisions}) within one time interval specified by the
#'   \code{time_unit} parameter in the \code{context}.
#' @param growth_rate The area-based radial population growth rate in distance
#'   per time interval in units specified by the \code{dist_unit} and
#'   \code{time_unit} parameters in the \code{context}.
#' @param size_class_max The maximum size class for populations represented in
#'   the growth model. Default is \code{10}
#' @param class_pops_max The maximum number of populations in a size class.
#'   Default is \code{100}
#' @param f_area_growth A function for calculating the area of a population
#'   given its size class and the \code{growth_rate}. The function should be in
#'   the form \code{function(growth_rate, size_class)}. The default function
#'   implements radial (circular) expansion:
#'   \code{pi*(growth_rate*size_class)^2}.
#' @param sample_sens A vector of sample sensitivity values for each sub-region
#'   specified by \code{divisions}. Default is \code{1}.
#' @param mgmt_cost A list of vectors to represent estimated management costs
#'   for population incursions. Each vector specifies costs at each sub-region
#'   specified by \code{divisions}. Named list elements should include
#'   \code{eradication} cost per area,  \code{damage} cost per area, and
#'   \code{penalty} cost per population of maximum size class. Default is an
#'   empty list. Units should be consistent with the \code{cost_unit} and
#'   \code{dist_unit} (area) parameters specified in the \code{context}.
#' @param sample_cost A vector of cost per sample of allocated surveillance
#'   resources at each sub-region specified by \code{divisions}. Default is
#'   \code{NULL}. Units should be consistent with the \code{cost_unit}
#'   parameter specified in the \code{context}.
#' @param budget The cost budget or constraint for the sampling allocation in
#'   the surveillance design. Default is \code{NULL}. Units should be
#'   consistent with \code{sample_cost} when specified. Otherwise the units
#'   should be consistent with the \code{surv_qty_unit} parameter specified in
#'   the \code{context} (e.g. traps or samples).
#' @param ... Additional parameters.
#' @return A \code{AreaGrowthSurvDesign} class object (list) containing inherited
#'   and extended functions from the base \code{SurveillanceDesign} class for
#'   for allocating sample densities, and calculating (unit and overall)
#'   detection sensitivities:
#'   \describe{
#'     \item{\code{get_allocation()}}{Get allocated sample densities via
#'       specified strategy, utilizing costs and budget constraints.}
#'     \item{\code{get_sensitivity()}}{Get the sub-region detection
#'       sensitivities of the allocated surveillance design.}
#'     \item{\code{get_confidence(growth = NULL)}}{Get the overall system
#'       sensitivity or detection confidence of the allocated surveillance
#'       design. The optional \code{growth} parameter may provide a vector of
#'       relative increasing multipliers (e.g. 1, 1.8, 4.3, 7.5) applied to the
#'       sampling design density implied by the method over time or a sequence
#'       of repeated surveillance efforts, which provide a proxy for invasive
#'       species growth. When present, increasing system sensitivity values are
#'       returned for each multiplier or time/repeat.}
#'     \item{\code{save_design()}}{Save the surveillance design as a
#'       collection of comma-separated value (CSV) files, including the
#'       surveillance allocation densities and numbers, surveillance,
#'       eradication, damage, penalty, and costs, and the sensitivities for
#'       each sub-region of the \code{design}; as well as a \code{summary} of
#'       total allocations, total costs, and the system sensitivity across the
#'       entire region.}
#'     \item{\code{set_cores(cores)}}{Set the number of cores available for
#'       parallel processing and thus enable parallel processing for
#'       calculating optimal sample density allocation.}
#'     \item{\code{set_precision(prec)}}{Set the precision used in the search
#'       for budget constrained allocation. Default is 3. Higher values may
#'       result in excessive computational times.}
#'   }
#' @references
#'   Epanchin-Niell, R. S., Haight, R. G., Berec, L., Kean, J. M., & Liebhold,
#'   A. M. (2012). Optimal surveillance and eradication of invasive species in
#'   heterogeneous landscapes. \emph{Ecology Letters}, 15(8), 803–812.
#'   \doi{10.1111/j.1461-0248.2012.01800.x}
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @include SamplingSurvDesign.R
#' @export
AreaGrowthSurvDesign <- function(context,
                                 divisions,
                                 subregion_area,
                                 establish_rate,
                                 growth_rate,
                                 size_class_max = 10,
                                 class_pops_max = 100,
                                 f_area_growth = function(g, s) {
                                   pi*(g*s)^2
                                 },
                                 sample_sens = 1,
                                 mgmt_cost = list(),
                                 sample_cost = NULL,
                                 budget = NULL, ...) {
  UseMethod("AreaGrowthSurvDesign")
}

#' @name AreaGrowthSurvDesign
#' @export
AreaGrowthSurvDesign.Context <- function(context,
                                         divisions,
                                         subregion_area,
                                         establish_rate,
                                         growth_rate,
                                         size_class_max = 10,
                                         class_pops_max = 100,
                                         f_area_growth = function(g, s) {
                                           pi*(g*s)^2
                                         },
                                         sample_sens = 1,
                                         mgmt_cost = list(),
                                         sample_cost = NULL,
                                         budget = NULL, ...) {

  # Build via base class (for checks and system sensitivity)
  self <- SurveillanceDesign(context = context,
                             divisions = divisions,
                             optimal = "cost",
                             mgmt_cost = mgmt_cost,
                             budget = budget,
                             class = "AreaGrowthSurvDesign", ...)

  # Number of subregions (division parts)
  subregions <- divisions$get_parts()

  # Check parameters (not checked in base class)
  if (!is.numeric(subregion_area) || any(subregion_area <= 0) ||
       !length(subregion_area) %in% c(1, subregions)) {
    stop(paste("The sub-region area parameter must be a numeric vector with",
               "values > 0 for each sub-region."), call. = FALSE)
  }
  if (!is.numeric(establish_rate) || any(establish_rate < 0) ||
      !length(establish_rate) %in% c(1, subregions)) {
    stop(paste("The establishment rate must be a numeric vector with",
               "values >= 0 for each sub-region."), call. = FALSE)
  }
  if (!is.numeric(growth_rate) || any(growth_rate < 0) ||
      !length(growth_rate) %in% c(1, subregions)) {
    stop(paste("The growth rate parameter must be a numeric vector with",
               "values >= 0 for each sub-region."), call. = FALSE)
  }
  if (!is.numeric(size_class_max) || size_class_max <= 0) {
    stop("The maximum size class parameter must be numeric and > 0.",
         call. = FALSE)
  }
  if (!is.numeric(class_pops_max) || class_pops_max <= 0) {
    stop(paste("The maximum number of populations in a size class parameter",
               "must be numeric and > 0."), call. = FALSE)
  }
  if (!is.function(f_area_growth) || length(formalArgs(f_area_growth)) != 2) {
    stop(paste("The area growth function should have form",
               "function(growth_rate, size_class)."),
         call. = FALSE)
  }
  if (!is.numeric(sample_sens) || sample_sens < 0 || sample_sens > 1) {
    stop(paste("The sample sensitivity parameter must be a numeric value",
               ">= 0 and <= 1."), call. = FALSE)
  }

  # Check and resolve mgmt_cost
  if (!all(c("eradication", "damage", "penalty") %in% names(mgmt_cost))) {
    stop(paste("The management cost parameter must contain list elements",
               "'eradication', 'damage', and 'penalty'."), call. = FALSE)
  }
  for (i in 1:length(mgmt_cost)) {
    if (length(mgmt_cost[[i]]) == 1) {
      mgmt_cost[[i]] <- rep(mgmt_cost[[i]], subregions)
    }
  }

  # Check and resolve sample_cost
  if (!is.null(sample_cost) &&
      (!is.numeric(sample_cost) ||
       !length(sample_cost) %in% c(1, subregions))) {
    stop(paste("The sample cost parameter must be a numeric vector with ",
               "values for each sub-region."), call. = FALSE)
  }
  if (length(sample_cost) == 1) {
    sample_cost <- rep(sample_cost, subregions)
  } else if (is.null(sample_cost)) {
    sample_cost <- rep(1, subregions)
  }

  # Set the number of cores available for parallel processing
  parallel_cores <- NULL
  self$set_cores <- function(cores) {
    parallel_cores <<- cores
  }

  # Set the precision used in the search for budget constrained allocation
  precision <- 3
  self$set_precision <- function(prec) {
    precision <<- prec
  }

  # Function for calculating total cost for a sub-region (index)
  subregion_total_cost <- function(sample_density, subregion) {

    # Probability of pop numbers (i in G) for size class (s in S) X_s_i
    size_class_pr <- list()

    # Initialize X_s_i for S = 1
    size_class_pr[[1]] <- stats::dpois(0:(class_pops_max - 1),
                                       establish_rate[subregion])
    size_class_pr[[1]] <- c(size_class_pr[[1]], 1 - sum(size_class_pr[[1]]))

    # Area of each size class a(s)
    area_size_class <- lapply(1:size_class_max,
                              function(s) f_area_growth(growth_rate, s))

    # Probability of transition from i pops of size class s to j pops of s + 1
    # P_s_i_j (i, j in G)
    transition_pr <- list()
    i_matrix <- matrix(0:class_pops_max, nrow = class_pops_max + 1,
                       ncol = class_pops_max + 1)
    j_matrix <- matrix(0:class_pops_max, nrow = class_pops_max + 1,
                       ncol = class_pops_max + 1, byrow = TRUE)
    mask <- j_matrix <= i_matrix
    i_matrix <- i_matrix*mask
    j_matrix <- j_matrix*mask
    for (s in 1:(size_class_max - 1)){
      undetect_pr <- exp(-1*area_size_class[[s]]*sample_density*sample_sens)
      transition_pr[[s]] <- (choose(i_matrix, i_matrix - j_matrix)*
                               (1 - undetect_pr)^(i_matrix - j_matrix)*
                               undetect_pr^j_matrix)*mask
    }

    # Transition probability for maximum size class => all detected/eradicated
    transition_pr[[size_class_max]] <- array(0, c(class_pops_max + 1,
                                                  class_pops_max + 1))
    transition_pr[[size_class_max]][,1] <- 1

    # Calculate X_s_i for S >= 2
    for (s in 2:size_class_max) {
      size_class_pr[[s]] <- size_class_pr[[s - 1]] %*% transition_pr[[s - 1]]
    }

    # Surveillance total cost
    surv_total_cost <- (sample_cost[subregion]*sample_density*
                          subregion_area[subregion])

    # Expected total eradication cost
    k_matrix <- i_matrix - j_matrix
    erad_total_cost <- sum(sapply(1:size_class_max, function(s) {
      (mgmt_cost$eradication[subregion]*area_size_class[[s]]*
         sum(array(size_class_pr[[s]],
                   c(class_pops_max + 1, class_pops_max + 1))*
               transition_pr[[s]]*k_matrix))
    }))

    # Expected total damage cost
    damage_total_cost <- sum(sapply(1:size_class_max, function(s) {
      (mgmt_cost$damage[subregion]*area_size_class[[s]]*(0:class_pops_max)*
         size_class_pr[[s]])
    }))

    # Expected total penalty cost
    penalty_total_cost <-
      (mgmt_cost$penalty[subregion]*
         sum((0:class_pops_max)*size_class_pr[[size_class_max]]))

    # Total cost
    total_cost <- (surv_total_cost + erad_total_cost + damage_total_cost +
                     penalty_total_cost)

    # Add attributes for cost components
    attr(total_cost, "surv_total_cost") <- surv_total_cost
    attr(total_cost, "erad_total_cost") <- erad_total_cost
    attr(total_cost, "damage_total_cost") <- damage_total_cost
    attr(total_cost, "penalty_total_cost") <- penalty_total_cost

    return(total_cost)
  }

  # Function for calculating total cost for all sub-regions (plural)
  subregion_total_costs <- function(sample_density) {

    # Calculate and gather the total costs for each sub-region
    if (is.numeric(parallel_cores) && min(parallel_cores, subregions) > 1) {
      doParallel::registerDoParallel(cores = min(parallel_cores, subregions))
      region_cost_list <- foreach(
        i = 1:subregions,
        .errorhandling = c("stop"),
        .noexport = c()) %dopar% {
          subregion_total_cost(sample_density[i], i)
        }
      doParallel::stopImplicitCluster()
    } else { # serial
      region_cost_list <- lapply(1:subregions, function(i) {
        subregion_total_cost(sample_density[i], i)
      })
    }
    subregion_costs <- as.data.frame(t(sapply(region_cost_list, function(l) {
      c(survey_cost = attr(l, "surv_total_cost"),
        erad_cost = attr(l, "erad_total_cost"),
        damage_cost = attr(l, "damage_total_cost"),
        penalty_cost = attr(l, "penalty_total_cost"),
        total_cost = l)
    })))

    # Return sub-region costs
    return(subregion_costs)
  }

  # Function for finding optimal unconstrained sub-region allocations
  unconstrained_allocation <- function() {

    # Search for optimal allocation for each sub-region
    region_cost_list <- list()
    for (i in 1:subregions) {
      lower = 0
      upper = mgmt_cost$penalty[i]/sample_cost[i]/subregion_area[i]
      region_cost_list[[i]] <-
        optimize(subregion_total_cost,
                 interval = c(lower, upper),
                 subregion = i)
    }

    # Pack the results of optimizer into a data frame
    unconstr_optim <- as.data.frame(t(sapply(region_cost_list, function(l) {
      c(sample_density = l$minimum,
        survey_cost = attr(l$objective, "surv_total_cost"),
        erad_cost = attr(l$objective, "erad_total_cost"),
        damage_cost = attr(l$objective, "damage_total_cost"),
        penalty_cost = attr(l$objective, "penalty_total_cost"),
        total_cost = l$objective)
    })))

    return(unconstr_optim)
  }

  # Function for finding optimal constrained sub-region allocations
  constrained_allocation <- function() {

    # Begin with the unconstrained allocation
    constr_optim <- unconstrained_allocation()
    constr_optim <- constr_optim[c("sample_density", "survey_cost",
                                   "total_cost")]

    # Calculate decrements to surveillance sample costs
    cost_decr <- min(10^(-1*precision)*sample_cost*subregion_area)

    # Calculate corresponding decrements to surveillance sample densities
    constr_optim$sample_density_decr <-
      (constr_optim$survey_cost - cost_decr)/sample_cost/subregion_area

    # Calculate corresponding decremented total costs
    subregion_cost_decr <-
      subregion_total_costs(constr_optim$sample_density_decr)
    constr_optim$survey_cost_decr <- subregion_cost_decr$survey_cost
    constr_optim$total_cost_decr <- subregion_cost_decr$total_cost
    total_survey_cost <- sum(constr_optim$survey_cost)

    # Reduce surveillance sample densities until the budget is reached
    while (total_survey_cost > budget) {

      # Identify decremented sample cost with the minimum total cost impact
      idx <- which.min(constr_optim$total_cost_decr - constr_optim$total_cost)

      # Update sample density and costs with selected decremented values
      constr_optim[idx, c("sample_density", "survey_cost", "total_cost")] <-
        constr_optim[idx, c("sample_density_decr", "survey_cost_decr",
                            "total_cost_decr")]

      # Update total surveillance cost
      total_survey_cost <- sum(constr_optim$survey_cost)

      # Calculate new decremented sample density and costs
      if (total_survey_cost > budget) {
        cost_decr <- min(cost_decr, total_survey_cost - budget)
        constr_optim$sample_density_decr <-
          (constr_optim$survey_cost - cost_decr)/sample_cost/subregion_area
        subregion_cost_decr <-
          subregion_total_cost(constr_optim$sample_density_decr[idx], idx)
        constr_optim[idx, c("survey_cost_decr", "total_cost_decr")] <-
          c(attr(subregion_cost_decr, "surv_total_cost"),
            as.numeric(subregion_cost_decr))
      }
    }

    # Return constrained sample density allocation and costs
    constr_optim <-
      cbind(sample_density = constr_optim$sample_density,
            subregion_total_costs(constr_optim$sample_density))

    return(constr_optim)
  }

  # Get the allocated sample density values of the surveillance design
  sample_density <- NULL
  self$get_allocation <- function() {
    if (is.null(sample_density)) {

      # Budget constraint?
      if (is.numeric(budget)) {
        optim_alloc <- constrained_allocation()
      } else {
        optim_alloc <- unconstrained_allocation()
      }

      # Extract sample density and add costs as an attribute
      sample_density <<- optim_alloc$sample_density
      attr(sample_density, "costs") <<- optim_alloc[-1]
    }

    return(sample_density)
  }


  # Get the detection sensitivities for each sub-region of the design
  calculate_sensitivity <- function(multi = 1) { # internal version
    unit_sens <- NULL
    if (!is.null(sample_density)) {
      unit_sens <- 1 - exp(-1*sample_sens*subregion_area*
                             as.numeric(sample_density)*multi)
    }
    return(unit_sens)
  }
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
        if (subregions == 1) {
          system_sens <- c(system_sens, sensitivity)
        } else if (!is.null(establish_rate)) {
          system_sens <- c(system_sens,
                           sum(establish_rate*sensitivity)/sum(establish_rate))
        }
      }
    }

    return(system_sens)
  }

  # Save the surveillance design as a collection of appropriate files
  self$save_design <- function() {

    # Save allocation, costs, and sensitivity
    design_data <- cbind(divisions$get_data(),
                         alloc_density = self$get_allocation(),
                         alloc_number = self$get_allocation()*subregion_area,
                         attr(self$get_allocation(), "costs"),
                         sensitivity = self$get_sensitivity())
    write.csv(design_data, file = "design.csv", row.names = FALSE)

    # Save summary
    summary_data <- data.frame(cbind(
      t(colSums(design_data[,c("alloc_number", "survey_cost", "damage_cost",
                             "penalty_cost", "total_cost")])),
      system_sensitivity = self$get_confidence()))
    names(summary_data)[1:4] <- paste0("total_", names(summary_data)[1:4])
    write.csv(summary_data, file = "summary.csv", row.names = FALSE)

    return(summary_data)
  }

  return(self)
}
