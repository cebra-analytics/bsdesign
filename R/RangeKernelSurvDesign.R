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
#'   via \code{1 - (1 - lambda*exp(-distance^2/(2*sigma^2))^intervals)} for a
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
#' @param optimal The strategy used for finding an effective surveillance
#'   resource allocation. Either (maximum) \code{"detection"} sensitivity (up
#'   to \code{"confidence"} level when specified), or \code{"none"} for
#'   representing existing surveillance designs only.
#' @param budget The budget or constraint for the number of surveillance
#'   resource units or devices available for allocation in the surveillance
#'   design. Default is \code{NULL}.
#' @param confidence The desired (minimum) system sensitivity or detection
#'   confidence of the surveillance design (e.g. 0.95). Default is \code{NULL}.
#' @param exist_alloc A vector of existing surveillance resource quantities at
#'   each spatial location specified by \code{divisions}, or a data frame
#'   (or matrix) of resource location coordinates in longitude and latitude
#'   (WGS84) with explicitly named columns "lon" and "lat". Should only be used
#'   to represent existing surveillance designs when \code{optimal = "none"}.
#'   Default is \code{NULL}.
#' @param exist_sens A vector, or list of vectors, of detection sensitivity
#'   values of existing surveillance present at each spatial location specified
#'   by \code{divisions}. Multiple existing surveillance layers may be
#'   specified in a list. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{RangeKernelSurvDesign} class object (list) containing
#'   inherited and extended functions from the base \code{SurveillanceDesign}
#'   class for for allocating resources, and calculating (location and overall)
#'   detection sensitivities:
#'   \describe{
#'     \item{\code{get_allocation(coords = FALSE)}}{Get allocated resources via
#'       specified strategy, utilizing costs, benefits, budget constraints,
#'       and/or desired detection confidence level. Returns resources at each
#'       location when \code{coords = FALSE} (default), or resource location
#'       coordinates in longitude and latitude (WGS84) when
#'       \code{coords = TRUE}.}
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
#'     \item{\code{save_design(...)}}{Save the surveillance design as a
#'       collection of raster TIF and comma-separated value (CSV) files,
#'       including the surveillance \code{allocation} cells (TIF) and
#'       coordinates (CSV), \code{sensitivity} (TIF), and a \code{summary}
#'       (CSV) of the total allocation and the detection confidence (system
#'       sensitivity). \code{Terra} raster write options may be passed to the
#'       function for saving grid-based designs.}
#'     \item{\code{set_cores(cores)}}{Set the number of cores available for
#'       parallel processing and thus enable parallel processing for
#'       calculating optimal resource allocation.}
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
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @include SurveillanceDesign.R
#' @export
RangeKernelSurvDesign <- function(context,
                                  divisions,
                                  establish_pr,
                                  lambda,
                                  sigma,
                                  intervals,
                                  prevalence = 1,
                                  optimal = c("detection", "none"),
                                  budget = NULL,
                                  confidence = NULL,
                                  exist_alloc = NULL,
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
                                          optimal = c("detection", "none"),
                                          budget = NULL,
                                          confidence = NULL,
                                          exist_alloc = NULL,
                                          exist_sens = NULL, ...) {

  # Build via base class (for checks and system sensitivity)
  self <- SurveillanceDesign(context = context,
                             divisions = divisions,
                             establish_pr = establish_pr,
                             optimal = optimal,
                             budget = budget,
                             confidence = confidence,
                             exist_sens = exist_sens,
                             class = "RangeKernelSurvDesign", ...)

  # Ensure divisions are grids
  if (divisions$get_type() != "grid") {
    stop("Divisions must be spatial grid type.", call. = FALSE)
  }

  # Number of division parts
  parts <- divisions$get_parts()

  # Match optimal arguments
  optimal <- match.arg(optimal)

  # Resolve if establish_pr is relative
  if ((!is.null(attr(establish_pr, "relative")) &&
       as.logical(attr(establish_pr, "relative"))) || max(establish_pr) > 1) {
    relative_establish_pr <- TRUE
  } else {
    relative_establish_pr <- FALSE
  }

  # Check lambda, sigma, intervals, and prevalence
  if (!is.numeric(lambda) || any(lambda < 0) ||
      !length(lambda) %in% c(1, parts)) {
    stop(paste("The lambda parameter must be numeric, >= 0, and match the",
               "number of division parts."), call. = FALSE)
  } else if (length(lambda) == 1) {
    lambda <- rep(lambda, parts)
  }
  if (!is.numeric(sigma) || sigma <= 0) {
    stop("The sigma parameter must be numeric and > 0.", call. = FALSE)
  }
  if (!is.numeric(intervals) || intervals <= 0) {
    stop("The time intervals parameter must be numeric and > 0.",
         call. = FALSE)
  }
  if (!is.numeric(prevalence) || prevalence <= 0) {
    stop("The prevalence parameter must be numeric and > 0.", call. = FALSE)
  }

  # Check and resolve existing allocation when allowed (optimal is "none")
  exist_vect <- NULL # existing allocation spatial points vector
  if (optimal == "none" && !is.null(exist_alloc)) {

    # Coordinate data, numeric vector, or invalid?
    if ((is.data.frame(exist_alloc) || # presumed coordinate data
         (is.matrix(exist_alloc) && ncol(exist_alloc) >= 2))) {

      # Check coordinate data
      exist_alloc <- as.data.frame(exist_alloc)
      if (!all(c("lon", "lat") %in% names(exist_alloc))) {
        stop(paste("When the existing allocation parameter is a data frame",
                   "(or matrix) it should contain coordinate data with",
                   "columns named 'lon' and 'lat'."), call. = FALSE)
      }

      # Set the existing allocation coordinates as spatial points
      exist_vect <- terra::project(terra::vect(exist_alloc, crs = "EPSG:4326"),
                                   divisions$get_template())

    } else if (is.numeric(exist_alloc)) {

      # Check allocation per location (cell)
      if (!length(exist_alloc) == parts) {
        stop(paste("When the existing allocation parameter is a numeric",
                   "vector it should have a value for each cell location."),
             call. = FALSE)
      }

      # Convert existing allocation to spatial points (also multiple per cell)
      exist_vect <-
        divisions$get_feat()[rep(which(exist_alloc > 0),
                                 exist_alloc[which(exist_alloc > 0)])]

    } else { # invalid
      stop(paste("The existing allocation parameter must be either a numeric",
                 "vector or a data frame (or matrix) of coordinates."),
           call. = FALSE)
    }

    # Clear (numeric vector) allocation
    exist_alloc <- NULL
  }

  # Resolve exist_sens
  if (is.null(exist_sens)) {
    exist_sens <- rep(0, parts)
  } else {
    exist_sens <- self$get_sensitivity() # combine via base class
  }

  # Set the number of cores available for parallel processing
  parallel_cores <- NULL
  self$set_cores <- function(cores) {
    parallel_cores <<- cores
  }

  # Function to calculate surveillance unit indices and pr(detection)
  alloc_surv_units <- NULL
  calc_surv_units <- function(loc_vect = divisions$get_feat()) {
    if (is.null(alloc_surv_units)) {
      if (is.numeric(parallel_cores) && min(parallel_cores, parts) > 1) {
        doParallel::registerDoParallel(cores = min(parallel_cores, parts))
        surv_units <- foreach(
          i = 1:length(loc_vect),
          .errorhandling = c("stop"),
          .noexport = c()) %dopar% {
            surv_unit <- list(
              idx = terra::nearby(loc_vect[i], divisions$get_feat(),
                                  distance = sigma*4, symmetrical = FALSE)[,2])
            if (length(surv_unit$idx)) {
              d <- terra::distance(loc_vect[i],
                                   divisions$get_feat()[surv_unit$idx])[1,]
              surv_unit$detect <-
                (1 - (1 - (lambda[surv_unit$idx]*
                             exp(-1*d^2/(2*sigma^2))))^intervals)
            }
            return(surv_unit)
          }
        doParallel::stopImplicitCluster()
      } else { # serial
        surv_units <- lapply(1:length(loc_vect), function(i) {
          surv_unit <- list(
            idx = terra::nearby(loc_vect[i], divisions$get_feat(),
                                distance = sigma*4, symmetrical = FALSE)[,2])
          if (length(surv_unit$idx)) {
            d <- terra::distance(loc_vect[i],
                                 divisions$get_feat()[surv_unit$idx])[1,]
            surv_unit$detect <-
              (1 - (1 - (lambda[surv_unit$idx]*
                           exp(-1*d^2/(2*sigma^2))))^intervals)
          }
          return(surv_unit)
        })
      }
    }
    return(surv_units)
  }

  # Get the allocated surveillance resource values of the surveillance design
  qty_alloc <- NULL
  self$get_allocation <- function(coords = FALSE) {
    if (optimal != "none" && is.null(qty_alloc)) {

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
        new_unit_sens <- lapply(1:parts, function(i) {
          1 - (1 - unit_sens[surv_units[[i]]$idx])*(1 - surv_units[[i]]$detect)
        })
        new_unit_contrib <- numeric(parts)
        for (i in 1:parts) {
          if (!i %in% selected) {

            # New unit (scaled) contribution towards detection confidence
            idx <- surv_units[[i]]$idx
            if (relative_establish_pr) {
              new_unit_contrib[i] <-
                (sum(establish_pr[idx]*new_unit_sens[[i]]) -
                   sum(establish_pr[idx]*unit_sens[idx]))
            } else { # inverted ratio for maximum
              new_unit_contrib[i] <-
                (prod(1 - establish_pr[idx]*unit_sens[idx])/
                   prod(1 - establish_pr[idx]*new_unit_sens[[i]]))
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

    # Return as coordinates or allocation vector?
    if (coords) {
      alloc_coords <- NULL
      if (!is.null(qty_alloc)) {
        alloc_coords <- as.data.frame(terra::crds(terra::project(
          divisions$get_feat()[which(qty_alloc > 0)], "EPSG:4326")))
        names(alloc_coords) <- c("lon", "lat")
      }
      return(alloc_coords)
    } else {
      return(qty_alloc)
    }
  }

  # Get the location detection sensitivities of the surveillance design
  sensitivity <- NULL
  self$get_sensitivity <- function() {
    if (is.null(sensitivity)) {

      # Calculate sensitivities for allocated or specified locations
      if (!is.null(qty_alloc) || !is.null(exist_vect)) {

        # Calculate surveillance units with reachable indices and pr(detection)
        if (optimal != "none" && !is.null(qty_alloc)) {
          surv_units <- alloc_surv_units
        } else if (optimal == "none" && !is.null(exist_vect)) {
          surv_units <- calc_surv_units(exist_vect)
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

  # Save the surveillance design as a collection of appropriate files
  self$save_design <- function(...) {

    # Save allocation and sensitivity
    terra::writeRaster(divisions$get_rast(self$get_allocation()),
                       "allocation_cells.tif", ...)
    write.csv(self$get_allocation(coords = TRUE),
              file = "allocation_coords.csv", row.names = FALSE)
    terra::writeRaster(divisions$get_rast(self$get_sensitivity()),
                       "sensitivity.tif", ...)

    # Save summary
    summary_data <- data.frame(total_allocation = sum(self$get_allocation()),
                               detection_confidence = self$get_confidence())
    write.csv(summary_data, file = "summary.csv", row.names = FALSE)

    return(summary_data)
  }

  return(self)
}
