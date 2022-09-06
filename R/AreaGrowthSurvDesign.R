#' Area growth surveillance design class builder
#'
#' Builds a class to represent a surveillance design functionality for the
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
#'   specified by \code{divisions}. List elements should be named
#'   \code{eradication},  \code{damage}, and \code{penalty}. Default is an
#'   empty list. Units should be consistent with the \code{cost_unit} parameter
#'   specified in the \code{context}.
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
#'        sensitivities of the allocated surveillance design.}
#'     \item{\code{get_confidence()}}{Get the overall system sensitivity or
#'       confidence of the allocated surveillance design.}
#'   }
#' @references
#'   Epanchin-Niell, R. S., Haight, R. G., Berec, L., Kean, J. M., & Liebhold,
#'   A. M. (2012). Optimal surveillance and eradication of invasive species in
#'   heterogeneous landscapes. \emph{Ecology Letters}, 15(8), 803–812.
#'   \doi{10.1111/j.1461-0248.2012.01800.x}
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

  # Number of division parts
  parts <- divisions$get_parts()

  # Check parameters (not checked in base class)
  if (!is.numeric(subregion_area) || any(subregion_area < 0) ||
       !length(subregion_area) %in% c(1, parts)) {
    stop(paste("The sub-region area parameter must be a numeric vector with",
               "values  >= 0 for each sub-region."), call. = FALSE)
  }
  if (!is.numeric(establish_rate) || any(establish_rate < 0) ||
      !length(establish_rate) %in% c(1, parts)) {
    stop(paste("The establishment rate must be a numeric vector with",
               "values  >= 0 for each subregion."), call. = FALSE)
  }
  if (!is.numeric(growth_rate) || any(growth_rate < 0) ||
      !length(growth_rate) %in% c(1, parts)) {
    stop(paste("The growth rate parameter must be a numeric vector with",
               "values  >= 0 for each sub-region."), call. = FALSE)
  }
  if (!is.numeric(size_class_max) || size_class_max < 0) {
    stop("The maximum size class parameter must be numeric and >= 0.",
         call. = FALSE)
  }
  if (!is.numeric(class_pops_max) || class_pops_max < 0) {
    stop(paste("The maximum number of populations in a size class parameter",
               "must be numeric and >= 0."), call. = FALSE)
  }
  if (!is.function(f_area_growth)) {
    stop("The area growth function must be a function.", call. = FALSE)
  }

  # Check mgmt_cost
  if (!all(c("eradication", "damage", "penalty") %in% names(mgmt_cost))) {
    stop(paste("The management cost parameter must contain list elements",
               "'eradication', 'damage', and 'penalty'."), call. = FALSE)
  }

  # Check and resolve sample_cost
  if (!is.null(sample_cost) &&
      (!is.numeric(sample_cost) || !length(sample_cost) %in% c(1, parts))) {
    stop(paste("The sample cost parameter must be a numeric vector with ",
               "values for each division part."), call. = FALSE)
  }
  if (length(sample_cost) == 1) {
    sample_cost <- rep(sample_cost, parts)
  } else if (is.null(sample_cost)) {
    sample_cost <- rep(1, parts)
  }

  # context,
  # divisions,
  # subregion_area,
  # establish_rate,
  # growth_rate,
  # size_class_max = 10,
  # class_pops_max = 100,
  # f_area_growth = function(g, s) {
  #   pi*(g*s)^2
  # },
  # sample_sens = 1,
  # mgmt_cost = list(),
  # sample_cost = NULL,
  # budget = NULL

  return(self)
}
