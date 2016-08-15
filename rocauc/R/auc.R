#' Simple numerical integration using the midpoint rule.
#' Rectangles are unevenly spaced and are as wide as the gaps
#' between the x's.
#'
#' @param x A variable.
#' @param y f(x).
#' @return The approximate area under f(x).
#' @export
integrate_finite <- function(x, y) {
  x <- x[!is.na(x) & !is.na(y)]
  y <- y[!is.na(x) & !is.na(y)]
  x_vals <- x[order(x)]
  y_vals <- y[order(x)]
  deltas <- x_vals[2:length(x_vals)] - x_vals[1:(length(x_vals) - 1)]
  heights <- (y_vals[2:length(y_vals)] + y_vals[1:(length(y_vals) - 1)])/2.
  return(sum(heights*deltas))
}

#' Compute AUC from prediction statistics.
#'
#' @param tpr A vector of true positive rates.
#' @param fpr A vector, of the same length, of false positive rates.
#' @return The area under the ROC curve.
#' @export
auc <- function(tpr, fpr) {
  auc_partial <- integrate_finite(fpr, tpr)
  box_area <- (max(fpr, na.rm=T)-min(fpr, na.rm=T))*
    (max(tpr, na.rm=T)-min(tpr, na.rm=T))
  if (box_area > 0.0 & box_area < 1.0)  {
    warning(paste0("Only partial ROC was calculated (area ", box_area, ")"))
    browser()
    return(auc_partial/box_area)
  }
  return(auc_partial)
}

#' Compute AUC for observations drawn from two classes.
#'
#' @param measure A vector of observations.
#' @param classes A vector of class labels.
#' @param goldleft The value of classes corresponding to the "negative" group, i.e.,
#' the one which will be on the < side of the classifier.
#' @return The area under the ROC curve.
#' @export
auc_by <- function(measure, classes, goldleft) {
  if (!(length(unique(classes)) == 2)) {
    stop("need exactly two classes")
  }
  if (!(goldleft %in% unique(classes))) {
    stop("need gold left class in the class list")
  }
  classes_f <- factor(classes)
  if (goldleft != levels(classes_f)[1]) {
    classes_f <- relevel(classes_f, goldleft)
  }
  return(with(pred_stats(measure, classes_f), auc(tpr, fpr)))
}

#' Compute AUC for observations in a data frame
#'
#' @param d A data frame
#' @param measure_var The name of the measure variable
#' @param class_var The name of the class variable
#' @param goldleft The value of the class variable corresponding to the
#' "negative" group, i.e., the one which will be on the < side of the
#' classifier.
#' @export
auc_by_df <- function(d, measure_var, class_var, goldleft) {
  return(auc_by(d[[measure_var]], d[[class_var]], goldleft))
}
