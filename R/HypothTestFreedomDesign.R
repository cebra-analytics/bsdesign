#' Hypothesis testing area freedom design base class builder
#'
#' Builds a class to represent area freedom design functionality utilizing
#' hypothesis testing approaches to assess the likelihood of an invasive
#' species being present when it has not been detected for a sequence of time
#' intervals or applications of a surveillance system.
#'
#' @param context A \code{Context} or inherited class object representing the
#'   context of a bio-security surveillance and area freedom design.
#' @param detected A logical (> 0) temporal vector recording when the invasive
#'   species was detected/sighted at previous intervals. Default is
#'   \code{FALSE} implying nothing detected.
#' @param pr_detect The probability of detecting the invasive species given its
#'   presence. Also known as system sensitivity or detection confidence for
#'   a surveillance system. Default is \code{NULL} implying only detection
#'   records are available. Temporally changing values may be provided by a
#'   numeric vector, the length of which should be sufficient for the expected
#'   number of \code{iterations}, given the specified stopping criteria, else
#'   the last value of the vector is repeated.
#' @param pr_persist The probability that the invasive species persists at each
#'   time interval (specified by the \code{time_unit} parameter in the
#'   \code{context}). Default is \code{1} implies that the invasive species
#'   will persist across time intervals if present, representing the worst case
#'   scenario when persistence probability is unknown. Only utilized when
#'   \code{pr_detect} is given. Temporally changing values may be provided by a
#'   numeric vector, the length of which should be sufficient for the expected
#'   number of \code{iterations}, given the specified stopping criteria, else
#'   the last value of the vector is repeated.
#' @param iterations The number of time intervals (specified by the
#'   \code{time_unit} parameter in the \code{context}), or sequential
#'   surveillance system applications, used to estimate the likelihood of area
#'   freedom. Default is \code{NULL} implying that the \code{p_value} parameter
#'   will be utilized as a stopping mechanism.
#' @param p_value The threshold probability (e.g. 0.05) for rejecting the null
#'   hypothesis that the invasive species remains present given a sequence of
#'   no detection via a surveillance system. Default is \code{NULL} implying
#'   that the \code{iterations} parameter will be utilized as a stopping
#'   mechanism.
#' @param ... Additional parameters.
#' @return A \code{HypothTestFreedomDesign} class object (list) containing
#'   inherited and extended functions from the base \code{AreaFreedomDesign}
#'   class to provide evidence for area freedom, given detection records,
#'   over a number of iterations, via hypothesis testing approaches:
#'   \describe{
#'     \item{\code{get_evidence()}}{Get a sequence of values for the
#'       probability that an extant invasive species could evade surveillance
#'       system detection, for each iteration or time interval specified, or
#'       for the iterations required to satisfy the threshold probability
#'       \code{p_value} for rejecting the null hypothesis that the invasive
#'       species remains present.}
#'     \item{\code{get_iterations()}}{Get the number of time intervals, or
#'       sequential surveillance system applications, used to provide evidence
#'       for area freedom. The value returned will be either the specified
#'       \code{iterations} parameter, or the iterations recorded when the
#'       specified threshold probability \code{p_value} for rejecting the null
#'       hypothesis of presence given no detection is reached.}
#'   }
#' @references
#'   Barclay, H. J., & Hargrove, J. W. (2005). Probability models to facilitate
#'   a declaration of pest-free status, with special reference to tsetse
#'   (Diptera: Glossinidae). \emph{Bulletin Of Entomological Research}
#'   \emph{(London)}, 95(1), 1–12. \doi{10.1079/BER2004331}
#'
#'   Regan, T. J., McCarthy, M. A., Baxter, P. W., Panetta, F. D., &
#'   Possingham, H. P. (2006). Optimal eradication: when to stop looking for
#'   an invasive plant. \emph{Ecology Letters}, 9(7), 759–766.
#'   \doi{10.1111/j.1461-0248.2006.00920.x}
#'
#'   Rout, T. (2017). Declaring Eradication of an Invasive Species. In
#'   A. Robinson, T. Walshe, M. Burgman, & M. Nunn (Eds.),
#'   \emph{Invasive Species: Risk Assessment and Management} (pp. 334-347).
#'   Cambridge: Cambridge University Press.
#'   \doi{doi.org/10.1017/9781139019606.017}
#'
#'   Solow, A. R. (1993). Inferring Extinction from Sighting Data.
#'   \emph{Ecology}, 74(3), 962–964. https://doi.org/10.2307/1940821
#' @include AreaFreedomDesign.R
#' @export
HypothTestFreedomDesign <- function(context,
                                    detected = FALSE,
                                    pr_detect = NULL,
                                    pr_persist = 1,
                                    iterations = NULL,
                                    p_value = NULL, ...) {
  UseMethod("HypothTestFreedomDesign")
}

#' @name HypothTestFreedomDesign
#' @export
HypothTestFreedomDesign.Context <- function(context,
                                            detected = FALSE,
                                            pr_detect = NULL,
                                            pr_persist = 1,
                                            iterations = NULL,
                                            p_value = NULL, ...) {

  # Build via base class (for checks)
  self <- AreaFreedomDesign(context = context,
                            detected = detected,
                            pr_detect = pr_detect,
                            pr_persist = pr_persist,
                            iterations = iterations,
                            class = "HypothTestFreedomDesign", ...)

  # Logical detection record
  detected <- as.logical(detected)

  # Check class parameters
  if (!is.null(p_value) &&
      (!is.numeric(p_value) || p_value < 0 || p_value > 1)) {
    stop(paste("The hypothesis test p-value parameter must be numeric, >= 0,",
               "and <= 1."), call. = FALSE)
  }

  # Get a sequence of values for the probability of non-detection if an
  # invasive species remains present, providing evidence for area freedom
  pr_undetected <- NULL
  self$get_evidence <- function() {

    # Any invasive species previously detected?
    if (any(detected)) {
      n_pres <- length(which(detected))
      time_n <- which(detected)[n_pres]
      pr_undetected <<- rep(1, time_n)
    }

    # Calculate the probability of undetected presence for iterations or
    # when p-value is met
    if (is.numeric(pr_detect) || any(detected)) {
      while ((is.numeric(iterations) && is.numeric(p_value) &&
              length(pr_undetected) < iterations &&
              (length(pr_undetected) == 0 ||
               pr_undetected[length(pr_undetected)] > p_value)) ||
             (is.numeric(iterations) && is.null(p_value) &&
              length(pr_undetected) < iterations) ||
             (is.null(iterations) && is.numeric(p_value) &&
              (length(pr_undetected) == 0 ||
               pr_undetected[length(pr_undetected)] > p_value)) ||
             (is.null(iterations) && is.null(p_value) && any(detected) &&
              length(pr_undetected) < length(detected))) {

        # Use probability of detection when present, else use detection record
        if (is.numeric(pr_detect)) {
          if (length(pr_detect) >= length(pr_undetected) + 1) {
            pr_detect_i <- pr_detect[length(pr_undetected) + 1]
          } else {
            pr_detect_i <- pr_detect[length(pr_detect)]
          }
          if (length(pr_persist) >= length(pr_undetected) + 1) {
            pr_persist_i <- pr_persist[length(pr_undetected) + 1]
          } else {
            pr_persist_i <- pr_persist[length(pr_persist)]
          }
          if (length(pr_undetected)) {
            pr_undetected <<- c(pr_undetected,
                                (pr_persist_i*(1 - pr_detect_i)*
                                   pr_undetected[length(pr_undetected)]))
          } else {
            pr_undetected <<- pr_persist_i*(1 - pr_detect_i)
          }
        } else if (any(detected)) {
          pr_undetected <<- c(pr_undetected,
                              (time_n/(length(pr_undetected) + 1))^n_pres)
        }
      }
    }

    # Attach a descriptive attribute
    if (is.numeric(pr_undetected)) {
      attr(pr_undetected, "evidence") <- "Pr(undetected|present)"
    }

    return(pr_undetected)
  }

  # Get the number of time intervals or surveillance system sequences
  self$get_iterations <- function() {
    n_iter <- NULL
    if (length(pr_undetected)) {
      n_iter <- length(pr_undetected)
    }
    return(n_iter)
  }

  return(self)
}
