#' Context class builder
#'
#' Builds a class to represent the context of a bio-security surveillance and
#' area freedom design, including information about the invasive pest, weed or
#' disease species or genus being monitored, the purpose and type of
#' surveillance, descriptive units for surveillance and management resource
#' quantities and costs, the status of the pest/weed/disease presence, and the
#' associated market access requirements for its absence or containment.
#'
#' @param species_name Invasive species (or genus) name.
#' @param species_type Type of invasive species. One of \code{"pest"},
#'   \code{"weed"}, or \code{"disease"}.
#' @param surveillance_purpose The purpose of the surveillance design. One of
#'   \code{"early_detection"} surveillance, incursion \code{"delimitation"}, or
#'   \code{"post-eradication"} monitoring.
#' @param surveillance_type The type of surveillance utilized in the design.
#'   One of \code{"survey"}, \code{"traps"}, \code{"detectors"},
#'   \code{"samples"}, \code{"reports"} (passive surveillance), or
#'   \code{"mixed"}.
#' @param surv_qty_unit The descriptive unit to describe surveillance resource
#'   quantities. One of \code{"units"}, \code{"hours"}, \code{"traps"},
#'   \code{"detectors"}, \code{"samples"}, or user specified.
#' @param cost_unit The descriptive unit to describe surveillance resource
#'   costs, and incursion management costs or surveillance benefit savings.
#'   One of \code{"$"}, \code{"hours"}, or user specified.
#' @param time_unit The descriptive unit to describe surveillance time
#'   intervals when applicable. One of \code{"years"}, \code{"months"},
#'   \code{"weeks"}, \code{"days"}, \code{"hours"}, or user specified.
#' @param dist_unit The descriptive unit to describe spatial distances (and
#'   areas) when applicable. One of \code{"metres"}, \code{"kilometres"},
#'   or user specified.
#' @param incursion_status The status of the invasive species presence. One of
#'   \code{"never_detected"}, \code{"detected"}, \code{"delimited"},
#'   \code{"contained"}, or \code{"eradicated"}.
#' @param area_freedom Logical to indicate if area freedom is declared.
#'   Default is \code{FALSE}.
#' @param market_access Logical to indicate that absence or containment is
#'   required for market access. Default is \code{FALSE}.
#' @param market_requirement The type of market access requirement. One of
#'   \code{"absence"}, \code{"low_prevalence"}, or \code{"contained"}.
#' @param ... Additional parameters.
#' @return A \code{Context} class object (list) containing functions for
#'   accessing attributes:
#'   \describe{
#'     \item{\code{get_species_name()}}{Get the invasive species name.}
#'     \item{\code{get_species_type()}}{Get the type of invasive species:
#'       "pest", "weed", or "disease".}
#'     \item{\code{get_surveillance_purpose()}}{Get the purpose of the
#'       surveillance design: "early_detection", "delimitation", or
#'       "post-eradication".}
#'     \item{\code{get_surveillance_type()}}{Get the type of surveillance:
#'       "survey", "traps", "detectors", "samples", "reports", or "mixed".}
#'     \item{\code{get_surv_qty_unit()}}{Get the unit for surveillance resource
#'       quantities: "units", "hours", "traps", "detectors", "samples", or user
#'       specified.}
#'     \item{\code{get_cost_unit()}}{Get the unit for surveillance, management,
#'       and/or benefit costs/savings: "$", "hours", or user specified.}
#'     \item{\code{get_time_unit()}}{Get the unit for surveillance time
#'       intervals: "years", "months", "weeks", "days" "hours", or user
#'       specified.}
#'     \item{\code{get_dist_unit()}}{Get the unit for spatial distances (and
#'        areas): "metres", "kilometres", or user specified.}
#'     \item{\code{get_incursion_status()}}{Get the incursion status:
#'       "never_detected", "detected", "delimited", "contained", or
#'       "eradicated".}
#'     \item{\code{get_area_freedom()}}{Get the area freedom indicator.}
#'     \item{\code{get_market_access()}}{Get the market access indicator.}
#'     \item{\code{get_market_requirement()}}{Get the market requirement type:
#'       "absence", "low_prevalence", or "contained".}
#'   }
#' @export
Context <- function(species_name,
                    species_type = c("pest",
                                     "weed",
                                     "disease"),
                    surveillance_purpose = c("early_detection",
                                             "delimitation",
                                             "post-eradication"),
                    surveillance_type = c("survey",
                                          "traps",
                                          "detectors",
                                          "samples",
                                          "reports",
                                          "mixed"),
                    surv_qty_unit = c("units",
                                      "hours",
                                      "traps",
                                      "detectors",
                                      "samples",
                                      "user"),
                    cost_unit = c("$",
                                  "hours",
                                  "user"),
                    time_unit = c("years",
                                  "months",
                                  "weeks",
                                  "days",
                                  "hours",
                                  "user"),
                    dist_unit = c("metres",
                                  "kilometres",
                                  "user"),
                    incursion_status = c("never_detected",
                                         "detected",
                                         "delimited",
                                         "contained",
                                         "eradicated"),
                    area_freedom = FALSE,
                    market_access = FALSE,
                    market_requirement = c("absence",
                                           "low_prevalence",
                                           "contained"), ...) {
  UseMethod("Context")
}

#' @name Context
#' @export
Context.default <- function(species_name,
                            species_type = c("pest",
                                             "weed",
                                             "disease"),
                            surveillance_purpose = c("early_detection",
                                                     "delimitation",
                                                     "post-eradication"),
                            surveillance_type = c("survey",
                                                  "traps",
                                                  "detectors",
                                                  "samples",
                                                  "reports",
                                                  "mixed"),
                            surv_qty_unit = c("units",
                                              "hours",
                                              "traps",
                                              "detectors",
                                              "samples",
                                              "user"),
                            cost_unit = c("$",
                                          "hours",
                                          "user"),
                            time_unit = c("years",
                                          "months",
                                          "weeks",
                                          "days",
                                          "hours",
                                          "user"),
                            dist_unit = c("metres",
                                          "kilometres",
                                          "user"),
                            incursion_status = c("never_detected",
                                                 "detected",
                                                 "delimited",
                                                 "contained",
                                                 "eradicated"),
                            area_freedom = FALSE,
                            market_access = FALSE,
                            market_requirement = c(NA,
                                                   "absence",
                                                   "low_prevalence",
                                                   "contained"), ...) {
  # Match arguments to selections
  species_type <- match.arg(species_type)
  surveillance_purpose <- match.arg(surveillance_purpose)
  surveillance_type <- match.arg(surveillance_type)
  if (!is.character(surv_qty_unit) || length(surv_qty_unit) > 1) {
    surv_qty_unit <- match.arg(surv_qty_unit)
  }
  if (!is.character(cost_unit) || length(cost_unit) > 1) {
    cost_unit <- match.arg(cost_unit)
  }
  if (!is.character(time_unit) || length(time_unit) > 1) {
    time_unit <- match.arg(time_unit)
  }
  if (!is.character(dist_unit) || length(dist_unit) > 1) {
    dist_unit <- match.arg(dist_unit)
  }
  incursion_status <- match.arg(incursion_status)
  market_requirement <- match.arg(market_requirement)

  # Create a class structure
  self <- structure(list(), class = "Context")

  # Get the type of invasive species
  self$get_species_name <- function() {
    return(species_name)
  }

  # Get the invasive species name
  self$get_species_type <- function() {
    return(species_type)
  }

  # Get the purpose of the surveillance design
  self$get_surveillance_purpose <- function() {
    return(surveillance_purpose)
  }

  # Get the type of surveillance
  self$get_surveillance_type <- function() {
    return(surveillance_type)
  }

  # Get the unit for surveillance resource quantities
  self$get_surv_qty_unit <- function() {
    return(surv_qty_unit)
  }

  # Get the cost unit for surveillance, management, benefit
  self$get_cost_unit <- function() {
    return(cost_unit)
  }

  # Get the unit for surveillance time intervals
  self$get_time_unit <- function() {
    return(time_unit)
  }

  # Get the unit for spatial distances (and areas)
  self$get_dist_unit <- function() {
    return(dist_unit)
  }

  # Get the incursion status
  self$get_incursion_status <- function() {
    return(incursion_status)
  }

  # Get the area freedom indicator
  self$get_area_freedom <- function() {
    return(area_freedom)
  }

  # Get the market access indicator
  self$get_market_access <- function() {
    return(market_access)
  }

  # Get the market access requirement type
  self$get_market_requirement <- function() {
    return(market_requirement)
  }

  return(self)
}
