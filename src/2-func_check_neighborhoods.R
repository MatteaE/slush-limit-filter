# This function checks the neighborhood
# of each stripe/year with suspicious retrievals,
# to see if they are supported by their neighbors.
func_check_neighborhoods <- function(dat_raw,
                                     stripes_neighbors_n) {
  
  cat("Examining the stripe neighborhoods to see if we can rescue any suspicious retrieval...\n")
  
  # Used only to display statistics.
  suspicious_points_n <- length(which(dat_raw$suspicious_status == 1))
  
  # Here we will put the number of supporting neighbors.
  dat_raw$supporting_n <- NA_integer_
  
  years   <- sort(unique(dat_raw$year))
  stripes <- sort(unique(dat_raw$stripe), decreasing = TRUE)
  stripes_n <- length(stripes)
  
  # Iterate over the years and then (inner loop) over the stripes.
  for (year_id in 1:length(years)) {
    
    year_cur <- years[year_id]
    
    cat(paste0("\rYear ", year_cur, "... "))
    
    for (stripe_id in 1:stripes_n) {
      
      stripe_cur <- stripes[stripe_id]
      
      # cat("Year", year_cur, "| Stripe", stripe_cur, "\n")
      
      points_cur_ids <- which((dat_raw$year == year_cur) &
                                (dat_raw$stripe == stripe_cur) &
                                (dat_raw$valid_filter_step1 == TRUE))
      
      if (length(points_cur_ids) > 0) {
        
        points_cur <- dat_raw[points_cur_ids,]
        points_cur_suspicious_ids <- which(points_cur$suspicious_status == 1)
        suspicious_n <- length(points_cur_suspicious_ids)
        
        if (suspicious_n > 0) {
          # cat("\nThis stripe has", suspicious_n, "suspicious retrieval(s). I am investigating the neighborhood...\n")
          
          stripes_neighbors_ids <- setdiff(stripe_id + (-stripes_neighbors_n:stripes_neighbors_n), stripe_id)
          # Remove out-of-bounds stripes at the edges.
          stripes_neighbors_ids <- stripes_neighbors_ids[(stripes_neighbors_ids > 0) & (stripes_neighbors_ids <= stripes_n)]
          
          for (suspicious_id in 1:suspicious_n) {
            
            suspicious_doy <- points_cur$day[points_cur_suspicious_ids[suspicious_id]]
            suspicious_sl <- points_cur$SL[points_cur_suspicious_ids[suspicious_id]]
            
            supporting_points_n <- func_find_supporting_points_n(dat_raw,
                                                                 year_cur,
                                                                 stripes,
                                                                 stripe_id,
                                                                 stripes_neighbors_ids,
                                                                 suspicious_doy,
                                                                 suspicious_sl)
            # cat("Found", supporting_points_n, "supporting retrieval(s) in the neighborhood.")
            
            dat_raw$supporting_n[points_cur_ids][points_cur_suspicious_ids[suspicious_id]] <- supporting_points_n
            
            if (supporting_points_n > 0) {
              dat_raw$suspicious_status[points_cur_ids][points_cur_suspicious_ids[suspicious_id]] <- 2
              # cat(" The suspicious retrieval is rescued.\n\n")
            } else {
              dat_raw$suspicious_status[points_cur_ids][points_cur_suspicious_ids[suspicious_id]] <- 3
              # cat(" The suspicious retrieval is kept discarded.\n\n")
            }
          }
        }
      }
    }
  }
  
  # TRUE (= valid) if status is either 0 (i.e. never suspicious)
  # or 2 (i.e. suspicious but rescued by neighborhood).
  # FALSE (= invalid) if status is 3.
  dat_raw$valid_filter_step2 <- (dat_raw$suspicious_status != 3)

  # Print statistics.  
  cat("\nStep 2b: rescued", paste0(length(which(dat_raw$suspicious_status == 2)), "/", suspicious_points_n), "retrievals thanks to their neighborhood.\n")
  cat("=== Step 2 finished. Marked", length(which(!dat_raw$valid_filter_step2)), "additional retrievals as invalid.\n")
  
  return(dat_raw)
}
