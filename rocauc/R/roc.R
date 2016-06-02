binary_factor_as_logical <- function(x) {
  return(as.logical(as.numeric(x) - 1))
}

positive_prediction_stats_linear_classifier_by_rank <- function(x, y_true, extend=T) {
  x_orig <- x
  y_true_orig <- y_true
  x <- x[!is.na(x_orig) & !is.na(y_true_orig)]
  y_true <- y_true[!is.na(x_orig) & !is.na(y_true_orig)]
  n <- length(x)
  y_true_bool <- binary_factor_as_logical(y_true)
  y_true_bool_ordered <- y_true_bool[order(x)]
  descending_tp <- cumsum(rev(y_true_bool_ordered))
  descending_fp <- 1:n - descending_tp
  all_pos <- sum(y_true_bool_ordered)
  all_neg <- n - all_pos
  if (all_pos==0) {
    descending_tpr <- rep(0., n)
  } else {
    descending_tpr <- descending_tp/all_pos
  }
  if (all_neg==0) {
    descending_fpr <- rep(1., n)
  } else {
    descending_fpr <- descending_fp/all_neg
  }
  last_entry <- rev(!duplicated(x[order(x)]))
  tpr_by_rank <- descending_tpr[last_entry]
  fpr_by_rank <- descending_fpr[last_entry]
  if (extend) {
    tpr_by_rank <- c(0.0, tpr_by_rank)
    fpr_by_rank <- c(0.0, fpr_by_rank)
  }
  result <- data.frame(tpr=tpr_by_rank, fpr=fpr_by_rank)
  return(result)
}

pred_stats <- function(sim, same) {
  pred_stats <- positive_prediction_stats_linear_classifier_by_rank(sim, same)
  pred_stats_above_zero <- pred_stats[pred_stats$tpr > 0 |
                                        pred_stats$fpr > 0,,drop=F]
  pred_stats_above_zero <- with(pred_stats_above_zero,
                                aggregate(tpr, by=list(fpr=fpr), FUN=max))
  names(pred_stats_above_zero)[2] <- "tpr"
  pred_stats_zero <- pred_stats[pred_stats$tpr <= 0 &
                                  pred_stats$fpr <= 0,c("tpr","fpr"),drop=F]
  pred_stats <- rbind(pred_stats_zero, pred_stats_above_zero)
  return(pred_stats)
}
