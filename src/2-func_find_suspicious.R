# This function finds suspicious SL retrievals
# within a single stripe/year. It returns a logical vector.
func_find_suspicious <- function(points_cur) {
  
  # Elevation bands data.
  points_cur_ele_band <- round(points_cur$SL / 20)
  ele_bands_present <- sort(unique(points_cur_ele_band))
  points_cur_ele_band_ids <- match(points_cur_ele_band, ele_bands_present)
  ele_bands_npoints <- rep(NA_integer_, length(ele_bands_present))
  for (band_id in 1:length(ele_bands_present)) {
    ele_bands_npoints[band_id] <- length(which(points_cur_ele_band == ele_bands_present[band_id]))
  }
  points_ele_bands_npoints <- ele_bands_npoints[points_cur_ele_band_ids]
  points_ele_band_below_npoints <- ele_bands_npoints[pmax(1,points_cur_ele_band_ids - 1)]
  
  
  points_cur_n <- nrow(points_cur)
  suspicious_logi <- rep(NA, points_cur_n)
  
  sl_max <- max(points_cur$SL)

  diff_date_prev <- c(1, as.numeric(diff(points_cur$date)))   # Difference in days to the previous one.
  diff_date_next <- c(as.numeric(diff(points_cur$date)), 1)   # Difference in days to the next one.
  diff_sl_prev <- abs(c(0, diff(points_cur$SL))) # Difference between SL and previous retrieval's SL.
  # diff_sl_next <- abs(c(diff(points_cur$SL), 0))
  # diff_sl_max <- pmax(diff_sl_prev, diff_sl_next)
  rate_sl_prev <- abs(diff_sl_prev / diff_date_prev)
  # rate_sl_next <- abs(diff_sl_next / diff_date_next)
  # rate_sl_max <- pmax(rate_sl_prev, rate_sl_next)
  
  # (1) Compute number of retrievals after this one which are at least as high (with 25 m tolerance).
  # (2) Compute number of retrievals which are within +/- 25 m of the current one.
  # (3) Compute difference to the retrieval just below,
  #     no matter where in the season it is.
  #     If said difference is a single elevation band,
  #     compute difference to the subsequent band below instead.
  sl_similar_n <- rep(NA_integer_, points_cur_n)
  sl_higher_n <- rep(NA_integer_, points_cur_n)
  diff_eleband <- rep(NA_integer_, points_cur_n)
  for (point_id in 1:points_cur_n) {
    
    sl_higher_n[point_id] <- length(which((points_cur$SL + 5 > points_cur$SL[point_id]))) - 1
    sl_similar_n[point_id] <- length(which(abs(points_cur$SL - points_cur$SL[point_id]) < 25)) - 1
    
    if (points_cur_ele_band_ids[point_id] == 1) {
      diff_eleband[point_id] <- 0
    } else {
      diff_eleband[point_id] <- points_cur_ele_band[point_id] - ele_bands_present[points_cur_ele_band_ids[point_id] - 1]
    }
  }
  
  
  cond1 <- sl_similar_n <= 1
  cond2 <- sl_higher_n  <= 1
  cond3 <- (sl_max - points_cur$SL) < 25
  cond4 <- diff_eleband >= 2
  cond5 <- diff_sl_prev >= 45
  cond6 <- rate_sl_prev >= 10
  cond7 <- diff_sl_prev >= 65
  cond8 <- diff_eleband >= 10
  # cond7 <- points_ele_bands_npoints == 1
  # cond8 <- points_ele_band_below_npoints >= 5
  
  suspicious_logi <- (cond1 & cond2 & cond3 & (cond4 | cond5) & ((cond6 & cond7) | cond8))
  
  return(as.integer(suspicious_logi)) # TRUE -> 1, FALSE -> 0.
  
}
