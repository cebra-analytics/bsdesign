#' Bayesian area freedom design base class builder
#'
#' Builds a class to represent area freedom design functionality utilizing
#' Bayesian approaches to assess the likelihood of freedom, or an invasive
#' species being absent when it has not been detected for a sequence of time
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
#' @param pr_freedom The prior probability of invasive species freedom or
#'   absence used in the first iteration of the Bayesian process. Values are
#'   typically estimated via expert elicitation. Default is \code{0.5} for an
#'   uninformed prior.
#' @param iterations The number of time intervals (specified by the
#'   \code{time_unit} parameter in the \code{context}), or sequential
#'   surveillance system applications, used to estimate the likelihood of area
#'   freedom. Default is \code{NULL} implying \code{confidence} will be
#'   utilized as a stopping mechanism.
#' @param confidence The target confidence level (e.g. 0.95) in area freedom,
#'   or the probability of freedom (absence) given a sequence of no detection
#'   via a surveillance system. Default is \code{NULL} implying that the
#'   \code{iterations} parameter will be utilized as a stopping mechanism.
#' @param ... Additional parameters.
#' @return A \code{BayesianFreedomDesign} class object (list) containing
#'   inherited and extended functions from the base \code{AreaFreedomDesign}
#'   class to provide evidence for area freedom, given detection records,
#'   over a number of iterations, via Bayesian approaches:
#'   \describe{
#'     \item{\code{get_evidence()}}{Get a sequence of values of confidence in
#'       area freedom, or probability of absence given no detection of an
#'       invasive species, for each iteration or time interval specified, or
#'       for the iterations required to satisfy the target \code{confidence}.}
#'     \item{\code{get_iterations()}}{Get the number of time intervals, or
#'       sequential surveillance system applications, used to provide evidence
#'       for area freedom. The value returned will be either the specified
#'       \code{iterations} parameter, or the iterations recorded when the
#'       specified target \code{confidence} level for absence given no
#'       detection is reached.}
#'     \item{\code{save_design()}}{Save the area freedom design as a
#'       comma-separated value (CSV) file containing iterative evidence for
#'       area freedom.}
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
#'   Rout, T. (2017). Declaring Eradication of an Invasive Species. In
#'   A. Robinson, T. Walshe, M. Burgman, & M. Nunn (Eds.),
#'   \emph{Invasive Species: Risk Assessment and Management} (pp. 334-347).
#'   Cambridge: Cambridge University Press.
#'   \doi{doi.org/10.1017/9781139019606.017}
#'
#'   Solow, A. R. (1993). Inferring Extinction from Sighting Data.
#'   \emph{Ecology}, 74(3), 962–964. \doi{10.2307/1940821}
#' @include AreaFreedomDesign.R
#' @export
BayesianFreedomDesign <- function(context,
                                  detected = FALSE,
                                  pr_detect = NULL,
                                  pr_persist = 1,
                                  pr_freedom = 0.5,
                                  iterations = NULL,
                                  confidence = NULL, ...) {
  UseMethod("BayesianFreedomDesign")
}

#' @name BayesianFreedomDesign
#' @export
BayesianFreedomDesign.Context <- function(context,
                                          detected = FALSE,
                                          pr_detect = NULL,
                                          pr_persist = 1,
                                          pr_freedom = 0.5,
                                          iterations = NULL,
                                          confidence = NULL, ...) {

  # Build via base class (for checks)
  self <- AreaFreedomDesign(context = context,
                            detected = detected,
                            pr_detect = pr_detect,
                            pr_persist = pr_persist,
                            iterations = iterations,
                            class = "BayesianFreedomDesign", ...)

  # Logical detection record
  detected <- as.logical(detected)

  # Check class parameters
  if (!is.null(pr_freedom) &&
      (!is.numeric(pr_freedom) || pr_freedom < 0 || pr_freedom > 1)) {
    stop(paste("The prior probability of freedom parameter must be numeric,",
               ">= 0, and <= 1."), call. = FALSE)
  }
  if (!is.null(confidence) &&
      (!is.numeric(confidence) || confidence < 0 || confidence > 1)) {
    stop(paste("The confidence of freedom parameter must be numeric, >= 0,",
               "and <= 1."), call. = FALSE)
  }

  # Get a sequence of evidence values for the confidence in area freedom, or
  # the probability of freedom (absence) given a sequence of non detection
  conf_freedom <- NULL
  self$get_evidence <- function() {

    # Any invasive species previously detected?
    if (any(detected)) {
      n_pres <- length(which(detected))
      time_n <- which(detected)[n_pres]
      conf_freedom <<- rep(0, time_n)
    }

    # Calculate the confidence in area freedom for iterations or when target
    # confidence is met
    if (is.numeric(pr_detect) || any(detected)) {
      prior_freedom <- pr_freedom
      while ((is.numeric(iterations) && is.numeric(confidence) &&
              length(conf_freedom) < iterations &&
              (length(conf_freedom) == 0 ||
              conf_freedom[length(conf_freedom)] < confidence)) ||
             (is.numeric(iterations) && is.null(confidence) &&
              length(conf_freedom) < iterations) ||
             (is.null(iterations) && is.numeric(confidence) &&
              (length(conf_freedom) == 0 ||
               conf_freedom[length(conf_freedom)] < confidence)) ||
             (is.null(iterations) && is.null(confidence) && any(detected) &&
              length(conf_freedom) < length(detected))) {

        # Use probability of detection when present, else use detection record
        if (is.numeric(pr_detect)) {
          if (length(pr_detect) >= length(conf_freedom) + 1) {
            pr_detect_i <- pr_detect[length(conf_freedom) + 1]
          } else {
            pr_detect_i <- pr_detect[length(pr_detect)]
          }
          if (length(pr_persist) >= length(conf_freedom) + 1) {
            pr_persist_i <- pr_persist[length(conf_freedom) + 1]
          } else {
            pr_persist_i <- pr_persist[length(pr_persist)]
          }
          conf_freedom <<- c(conf_freedom,
                             (prior_freedom/
                                (pr_persist_i*(1 - pr_detect_i)*
                                   (1 - prior_freedom) + prior_freedom)))
          prior_freedom <- conf_freedom[length(conf_freedom)]
        } else if (any(detected)) {
          Bayes_factor <- ((n_pres - 1)/
                             (((length(conf_freedom) + 1)/
                                 time_n)^(n_pres - 1) - 1))
          conf_freedom <<- c(conf_freedom,
                             1 - 1/(1 + (prior_freedom/
                                           ((1 - prior_freedom)*
                                              Bayes_factor))))
        }
      }
    }

    # Attach a descriptive attribute
    if (is.numeric(conf_freedom)) {
      attr(conf_freedom, "evidence") <- "Pr(freedom|undetected)"
    }

    return(conf_freedom)
  }

  # Get the number of time intervals or surveillance system sequences
  self$get_iterations <- function() {
    n_iter <- NULL
    if (length(conf_freedom)) {
      n_iter <- length(conf_freedom)
    }
    return(n_iter)
  }

  # Save the area freedom design evidence
  self$save_design <- function() {

    # Save evidence
    evidence <- data.frame(iterations = 1:self$get_iterations(),
                           confidence = self$get_evidence())
    write.csv(evidence, file = "confidence.csv", row.names = FALSE)

    return(evidence)
  }

  return(self)
}
