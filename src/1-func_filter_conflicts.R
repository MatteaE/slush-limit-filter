# This function finds suspicious SL retrievals
# in the dataset based on iterative conflict removal.
# It returns dat_raw with a new column: a logical vector
# indicating pass/fail for the filter.
func_filter_conflicts <- function(dat_raw,
                                  sl_decrease_tolerance) {
  
  cat("Filtering: step 1...\n")
  
  #### FIND AND RESOLVE CONFLICTS in the data ####
  # This will indicate pass/fail for the current filter.
  dat_raw$valid_filter_step1 <- TRUE
  
  # Setup the loop: which years and stripes are available?
  years   <- sort(unique(dat_raw$year))
  stripes <- sort(unique(dat_raw$stripe), decreasing = TRUE)
  
  
  # Iterate over the years and then (inner loop) over the stripes.
  for (year_id in 1:length(years)) {
    
    year_cur <- years[year_id]
    
    cat(paste0("\rYear ", year_cur, "... "))
    
    for (stripe_id in 1:length(stripes)) {
      
      stripe_cur <- stripes[stripe_id]
      
      # cat("\nYear", year_cur, "| Stripe", stripe_cur, "\n")
      
      # Select points from current year.
      points_cur_ids <- which((dat_raw$year == year_cur) &
                                (dat_raw$stripe == stripe_cur))
      points_cur_ids_sel <- points_cur_ids

      # Find and resolve conflicts, only if
      # the stripe has more than 1 retrieval.
      # points_cur_ids has the indices (within dat_raw)
      # of all the points of the current stripe/year.
      # points_cur_ids_sel starts out as a copy of points_cur_ids,
      # then it is progressively stripped of the indices of the
      # worst retrievals.
      points_cur_n <- length(points_cur_ids)
      if (points_cur_n > 1) {
        remove_id <- func_find_worst(dat_raw,
                                     points_cur_ids_sel,
                                     sl_decrease_tolerance)
        while (!is.na(remove_id)) {
          # cat("Removing retrieval", paste0(points_cur_ids_sel[remove_id], "...\n"))
          dat_raw$valid_filter_step1[points_cur_ids_sel[remove_id]] <- FALSE
          points_cur_ids_sel <- points_cur_ids_sel[-remove_id]
          remove_id <- func_find_worst(dat_raw,
                                       points_cur_ids_sel,
                                       sl_decrease_tolerance)
        }
      }

    }
  }
  
  cat("\n=== Step 1 finished. Marked", paste0(length(which(!dat_raw$valid_filter_step1)), "/", nrow(dat_raw)), "retrievals as invalid.\n")
  
  return(dat_raw)
}
