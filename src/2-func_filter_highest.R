# This function finds suspicious SL retrievals
# in the dataset. It returns dat_raw with a new
# column: a logical vector indicating pass/fail of the filter.
func_filter_highest <- function(dat_raw) {
  
  cat("Filtering: step 2...\n")
  
  # Status flag for each retrieval:
  # 0 = valid,
  # 1 = marked as suspicious by func_filter_highest (temporary status, gone after running func_check_neighborhoods)
  # 2 = rescued by func_check_neighborhoods,
  # 3 = NOT rescued by func_check neighborhoods.
  # All retrievals are initially valid.
  dat_raw$suspicious_status <- 0
  
  # Used only to display statistics.
  valid_filter1_n <- length(which(dat_raw$valid_filter_step1 == TRUE))
  
  # Setup the loop: which years and stripes are available?
  years   <- sort(unique(dat_raw$year))
  stripes <- sort(unique(dat_raw$stripe), decreasing = TRUE)
  
  # Iterate over the years and then (inner loop) over the stripes.
  for (year_id in 1:length(years)) {
    
    year_cur <- years[year_id]
    
    cat(paste0("\rYear ", year_cur, "... "))
    
    for (stripe_id in 1:length(stripes)) {
      
      stripe_cur <- stripes[stripe_id]
      
      # cat("\rYear", year_cur, "| Stripe", stripe_cur)
      
      points_cur_ids <- which((dat_raw$year == year_cur) &
                                (dat_raw$stripe == stripe_cur) &
                                (dat_raw$valid_filter_step1 == TRUE))
      
      # Only look at stripes with 2+ points.
      points_cur_n <- length(points_cur_ids)
      if (points_cur_n > 1) {
        
        points_cur <- dat_raw[points_cur_ids,]
        dat_raw$suspicious_status[points_cur_ids] <- func_find_suspicious(points_cur)

      }
    }
  }
  
  cat("\nStep 2a: marked", paste0(length(which(dat_raw$suspicious_status == 1)), "/", valid_filter1_n), "additional retrievals as suspicious.\n")
  
  return(dat_raw)
}
