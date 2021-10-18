# This function finds the worst retrieval within a
# stripe/year. It is called repeatedly during the
# iterative filtering.
# It returns the index of the worst retrieval
# relative to the points_cur_ids (so it returns
# an integer between 1 and length(points_cur_ids)).
# It returns NA if there is no worst retrieval (i.e.
# no conflicts).
func_find_worst <- function(dat_raw,
                            points_cur_ids,
                            sl_decrease_tolerance) {
  
  # Find conflicts: a retrieval within a stripe conflicts
  # with all others which are both (significantly) higher and earlier,
  # as well as all which are both (significantly) lower and later.
  # (Significantly) means that we actually have a tolerance, since the
  # assignment of a retrieval to an elevation band can be slightly inaccurate.
  # We make a square table with one row and one column per retrieval,
  # to compare all retrievals against each other.
  # The final number of conflicts for a retrieval is the sum of its COLUMN in the matrix.
  points_cur <- dat_raw[points_cur_ids,]
  points_cur_n <- length(points_cur_ids)
  
  # This line below can be made fancier to accommodate
  # special cases such as autumn snowfalls, which
  # lower the SL without being outliers.
  points_cur_tolerances <- rep(sl_decrease_tolerance,
                               points_cur_n)
  
  
  conflicts_cur <- matrix(data = NA, nrow = points_cur_n, ncol = points_cur_n)
  points_cur$conflicts_n <- NA_integer_
  
  for (point_id in 1:points_cur_n) {
    
    conflicts_cur[,point_id] <- ((points_cur$date < points_cur$date[point_id]) &
                                   (points_cur$SL - points_cur_tolerances[point_id] > points_cur$SL[point_id])) |
                                 ((points_cur$date > points_cur$date[point_id]) &
                                   (points_cur$SL + points_cur_tolerances[point_id] < points_cur$SL[point_id]))
    points_cur$conflicts_n[point_id] <- length(which(conflicts_cur[,point_id]))
  }
  conflicts_n_sum <- sum(points_cur$conflicts_n)
  
  if (conflicts_n_sum > 0) {
    # cat("Found", conflicts_n_sum, "conflict(s) to solve... ")
    points_cur$score <- (points_cur$conflicts_n > 0) * (2 + points_cur$conflicts_n)
    id_worst <- which.max(points_cur$score)
    return(id_worst)
    
  } else {
    
    # cat("No conflicts here. The current stripe/year is done.\n")
    return(NA)
  }

} 
