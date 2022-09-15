#' Range kernel surveillance design class builder
#'
#' Builds a class to represent surveillance design functionality for the
#' effective allocation of surveillance resources that are each decreasingly
#' effective across a spatial range, modeled via an exponential decay kernel,
#' and specified with a surveillance resource budget and/or an overall desired
#' detection confidence.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of a bio-security surveillance and area freedom design.
#' @param divisions A \code{Divisions} or inherited class object representing
#'   spatial locations (grid-cells) for the surveillance design.
#' @param establish_pr A vector of (relative) probability values to represent
#'   the likelihood of pest establishment at each spatial location specified
#'   by \code{divisions}. Values are assumed to be relative when their
#'   maximum is greater than 1, or an attribute \code{relative = TRUE} is
#'   attached to the parameter.
#' @param lambda A vector of maximum detection or capture rates for each
#'   spatial location specified by \code{divisions}. Maximum detection/capture
#'   is achieved when a surveillance resource is placed in the center of the
#'   home range of an invasive species individual or colony. Detection/capture
#'   rate decays with distance via an exponential kernel such that the
#'   detection/capture probability of each allocated resource can be expressed
#'   via \code{1 - (1 - lambda*exp(-distance^2/(2sigma^2))^intervals)} for a
#'   given number of time \code{intervals} and up to a maximum distance of
#'   \code{4*sigma}, where \code{sigma} is the home range decay parameter.
#' @param sigma The spatial decay parameter for the (half-Normal) home range
#'   kernel (see \code{lambda}). Note that \code{sigma} also specifies the
#'   maximum effective distance of detection/capture resources (i.e. twice the
#'   home range radius or \code{4*sigma}). Units should be consistent
#'   with the \code{dist_unit} parameter specified in the \code{context}.
#' @param intervals The number of time intervals each detection/capture
#'   resource or device is utilized (e.g. nights a trap is set).
#' @param prevalence The cell-level design prevalence indicating the minimum
#'   number of location cells that are expected to be infected with the
#'   invasive species if the region of interest specified by \code{divisions}
#'   is infected. Default is \code{1}. Higher values can be used as a proxy
#'   to population growth over time.
#' @param budget The budget or constraint for the number of surveillance
#'   resource units or devices available for allocation in the surveillance
#'   design. Default is \code{NULL}.
#' @param confidence The desired (minimum) system sensitivity or detection
#'   confidence of the surveillance design (e.g. 0.95). Default is \code{NULL}.
#' @param exist_sens A vector of detection sensitivity values of existing
#'   surveillance present at each spatial location specified by
#'   \code{divisions}. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{RangeKernelSurvDesign} class object (list) containing
#'   inherited and extended functions from the base \code{SurveillanceDesign}
#'   class for for allocating resources, and calculating (location and overall)
#'   detection sensitivities:
#'   \describe{
#'     \item{\code{get_allocation()}}{Get allocated surveillance resources
#'       utilizing resource budget constraint and/or desired detection
#'       confidence level.}
#'     \item{\code{set_surv_locs(coords)}}{Set surveillance resource locations
#'       via a data frame (or matrix) of resource location coordinates in
#'       longitude and latitude (WGS84) with explicitly named columns "lon"
#'       and "lat".}
#'     \item{\code{get_surv_locs()}}{Get surveillance resource locations as a
#'       data frame of resource location coordinates in longitude and latitude
#'       (WGS84) with named columns "lon" and "lat".}
#'     \item{\code{get_sensitivity()}}{Get the location detection sensitivities
#'       of the allocated surveillance design.}
#'     \item{\code{get_confidence(growth = NULL)}}{Get the overall system
#'       sensitivity or detection confidence of the allocated surveillance
#'       design. The optional \code{growth} parameter may provide a vector of
#'       relative increasing multipliers (e.g. 1, 1.8, 4.3, 7.5) applied to the
#'       cell-level design \code{prevalence} over time or a sequence of
#'       repeated surveillance efforts, which provide a proxy for invasive
#'       species growth. When present, increasing system sensitivity values are
#'       returned for each multiplier or time/repeat.}
#'   }
#' @references
#'   Anderson, D. P., Ramsey, D. S. L., Nugent, G., Bosson, M., Livingstone,
#'   P., Martin, P. A. J., Sergeant, E., Gormley, A. M., & Warburton, B.
#'   (2013). A novel approach to assess the probability of disease eradication
#'   from a wild-animal reservoir host. \emph{Epidemiology and Infection},
#'   141(7), 1509–1521. \doi{10.1017/S095026881200310X}
#'
#'   Anderson, D. P., Gormley, A. M., Ramsey, D. S. L., Nugent, G., Martin,
#'   P. A. J., Bosson, M., Livingstone, P., & Byrom, A. E. (2017).
#'   Bio-economic optimisation of surveillance to confirm broadscale
#'   eradications of invasive pests and diseases. \emph{Biological Invasions},
#'   19(10), 2869–2884. \doi{10.1007/s10530-017-1490-5}
#'
#'   Anderson, D. P., Pepper, M. A., Travers, S., Michaels, T. A., Sullivan,
#'   K., & Ramsey, D. S. L. (2022). Confirming the broadscale eradication
#'   success of nutria (Myocastor coypus) from the Delmarva Peninsula, USA.
#'   \emph{Biological Invasions}. \doi{10.1007/s10530-022-02855-x}
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
RangeKernelSurvDesign <- function(context,
                                  divisions,
                                  establish_pr,
                                  lambda,
                                  sigma,
                                  intervals,
                                  prevalence = 1,
                                  budget = NULL,
                                  confidence = NULL,
                                  exist_sens = NULL, ...) {
  UseMethod("RangeKernelSurvDesign")
}

#' @name RangeKernelSurvDesign
#' @export
RangeKernelSurvDesign.Context <- function(context,
                                          divisions,
                                          establish_pr,
                                          lambda,
                                          sigma,
                                          intervals,
                                          prevalence = 1,
                                          budget = NULL,
                                          confidence = NULL,
                                          exist_sens = NULL, ...) {

  # Allow budget and confidence to be empty for setting resource locations
  if (is.null(budget) && is.null(confidence)) {
    budget <- confidence <- 0
  }

  # Build via base class (for checks and system sensitivity)
  self <- SurveillanceDesign(context = context,
                             divisions = divisions,
                             establish_pr = establish_pr,
                             optimal = "detection",
                             budget = budget,
                             confidence = confidence,
                             exist_sens = exist_sens,
                             class = "RangeKernelSurvDesign", ...)

  # Number of division parts
  parts <- divisions$get_parts()

  # Resolve if establish_pr is relative
  if ((!is.null(attr(establish_pr, "relative")) &&
       as.logical(attr(establish_pr, "relative"))) || max(establish_pr) > 1) {
    relative_establish_pr <- TRUE
  } else {
    relative_establish_pr <- FALSE
  }

  # Check lambda, sigma, intervals, and prevalence
  if (!is.numeric(lambda) || lambda < 0 || !length(lambda) %in% c(1, parts)) {
    stop(paste("The lambda parameter must be numeric,  >= 0, and match the",
               "number of division parts."), call. = FALSE)
  } else if (length(lambda) == 1) {
    lambda <- rep(lambda, parts)
  }
  if (!is.numeric(sigma) || sigma < 0) {
    stop("The sigma parameter must be numeric and  >= 0.", call. = FALSE)
  }
  if (!is.numeric(intervals) || intervals < 0) {
    stop("The time intervals parameter must be numeric and  >= 0.",
         call. = FALSE)
  }
  if (!is.numeric(prevalence) || prevalence < 0) {
    stop("The prevalence parameter must be numeric and  >= 0.", call. = FALSE)
  }

  # Resolve exist_sens
  if (is.null(exist_sens)) {
    exist_sens <- rep(0, parts)
  }

  # Function to calculate surveillance unit indices and pr(detection)
  alloc_surv_units <- NULL
  calc_surv_units <- function(loc_vect = divisions$get_feat()) {
    if (is.null(alloc_surv_units)) {
      surv_units <- terra::nearby(loc_vect, divisions$get_feat(),
                                   distance = sigma*4)
      surv_units <- lapply(1:length(loc_vect), function(i) {
        surv_unit <- list(idx = surv_units[which(surv_units[,1] == i), 2])
        if (length(surv_unit$idx)) {
          d <- terra::distance(loc_vect[i],
                               divisions$get_feat()[surv_unit$idx])[1,]
          surv_unit$detect <-
            (1 - (1 - lambda[surv_unit$idx]*exp(-1*d^2/(2*sigma^2)))^intervals)
        }
        return(surv_unit)
      })
    }
    return(surv_units)
  }

  # Initialize allocated/specified resources, and unit sensitivities
  qty_alloc <- NULL
  surv_vect <- NULL
  sensitivity <- NULL

  # Get the allocated surveillance resource values of the surveillance design
  self$get_allocation <- function() {
    if (is.null(qty_alloc) && is.null(surv_vect)) {

      # Possible surveillance units with reachable indices and pr(detection)
      surv_units <- calc_surv_units()

      # Incrementally select cells that contribute most to confidence
      selected <- c()
      unit_sens <- exist_sens
      system_conf <- 0

      # Allocate until budget or target confidence is reached
      while (length(selected) < min(budget, parts) &&
             (is.null(confidence) || system_conf < confidence)) {

        # Calculate additional (local) detection confidence for each unit
        new_unit_contrib <- numeric(parts)
        for (i in 1:parts) {
          if (!i %in% selected) {

            # New sensitivity for unit i
            idx <- surv_units[[i]]$idx
            new_unit_sens <- sapply(1:length(idx), function(j) {
              1 - prod(1 - c(unit_sens[idx[j]], surv_units[[i]]$detect[j]))
            })

            # New unit (scaled) contribution towards detection confidence
            if (relative_establish_pr) {
              new_unit_contrib[i] <-
                (sum(establish_pr[idx]*new_unit_sens) -
                   sum(establish_pr[idx]*unit_sens[idx]))
            } else {
              new_unit_contrib[i] <-
                ((1 - prod(1 - establish_pr[idx]*new_unit_sens)) -
                   (1 - prod(1 - establish_pr[idx]*unit_sens[idx])))
            }
          }
        }

        # Select unit with highest contribution
        new_selected <- which.max(new_unit_contrib)
        selected <- c(selected, new_selected)

        # Update combined selected unit detection values
        idx <- surv_units[[new_selected]]$idx
        unit_sens[idx] <- sapply(1:length(idx), function(j) {
          1 - prod(1 - c(unit_sens[idx[j]],
                         surv_units[[new_selected]]$detect[j]))
        })

        # Calculate overall confidence
        if (relative_establish_pr) {
          system_conf <- sum(establish_pr*unit_sens)/sum(establish_pr)
        } else {
          system_conf <- ((1 - prod(1 - establish_pr*unit_sens))/
                            (1 - prod(1 - establish_pr)))
        }
      }

      # Cell locations for allocated surveillance resource
      qty_alloc <<- numeric(parts)
      qty_alloc[selected] <<- 1

      # Set the allocated surveillance unit indices and pr(detection)
      alloc_surv_units <<- surv_units[selected]
    }

    return(qty_alloc)
  }

  # Set surveillance resource locations
  self$set_surv_locs <- function(coords) {

    # Only set when not already allocated
    if (!is.null(qty_alloc)) {
      stop("Surveillance resources have already been allocated.",
           call. = FALSE)
    }

    # Check coordinate data
    coords <- as.data.frame(coords)
    if (!all(c("lon", "lat") %in% names(coords))) {
      stop("Coordinate data must contain columns named 'lon' and 'lat'.",
           call. = FALSE)
    }

    # Convert the coordinates to spatial points
    surv_vect <<- terra::project(terra::vect(coords, crs = "EPSG:4326"),
                                 divisions$get_template())

    # Reset unit sensitivities
    sensitivity <<- NULL
  }

  # Get surveillance resource locations (lon/lat data)
  self$get_surv_locs <- function() {
    surv_locs <- NULL
    if (!is.null(qty_alloc)) {
      surv_locs <- as.data.frame(terra::crds(terra::project(
        divisions$get_feat()[which(qty_alloc > 0)], "EPSG:4326")))
      names(surv_locs) <- c("lon", "lat")
    } else if (!is.null(surv_vect)) {
      surv_locs <- as.data.frame(terra::crds(terra::project(
        surv_vect, "EPSG:4326")))
      names(surv_locs) <- c("lon", "lat")
    }
    return(surv_locs)
  }

  # Get the location detection sensitivities of the surveillance design
  self$get_sensitivity <- function() {
    if (is.null(sensitivity)) {

      # Calculate sensitivities for allocated or specified locations
      if (!is.null(qty_alloc) || !is.null(surv_vect)) {

        # Calculate surveillance units with reachable indices and pr(detection)
        if (!is.null(qty_alloc)) {
          surv_units <- alloc_surv_units
        } else if (!is.null(surv_vect)) {
          surv_units <- calc_surv_units(surv_vect)
        }

        # Calculate unit sensitivities
        sensitivity <<- exist_sens
        for (i in 1:length(surv_units)) {
          idx <- surv_units[[i]]$idx
          sensitivity[idx] <<- sapply(1:length(idx), function(j) {
            1 - prod(1 - c(sensitivity[idx[j]], surv_units[[i]]$detect[j]))
          })
        }
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
        if (relative_establish_pr) {
          system_sens <- sum(establish_pr*sensitivity)/sum(establish_pr)
        } else {
          system_sens <- ((1 - prod(1 - establish_pr*sensitivity))/
                            (1 - prod(1 - establish_pr)))
        }
      }

      # Apply prevalence with growth if present
      if (is.numeric(growth)) {
        prevalence <- prevalence*growth
      }
      system_sens <- 1 - (1 - system_sens)^prevalence
    }

    return(system_sens)
  }

  return(self)
}
