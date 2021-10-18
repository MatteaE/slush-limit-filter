# This function finds retrievals which support
# the truthfulness of a given retrieval thanks
# to a close position within neighboring stripes.
func_find_supporting_points_n <- function(dat_raw,
                                          year_cur,
                                          stripes,
                                          stripe_id,
                                          stripes_neighbors_ids,
                                          suspicious_doy,
                                          suspicious_sl) {
  
  points_supporting_n <- 0
  supporting_bands_dist_thresh <- 3 # Maximum distance (expressed in elevation bands) to accept a supporting point.
  
  stripes_neighbors_dists <- abs(stripes_neighbors_ids - stripe_id)
  stripes_neighbors_n <- length(stripes_neighbors_ids)
  for (stripe_neighbor_id in 1:stripes_neighbors_n) {
    
    stripe_neighbor_cur <- stripes[stripes_neighbors_ids[stripe_neighbor_id]]
    points_neighbor_ids <- which((dat_raw$year == year_cur) &
                              (dat_raw$stripe == stripe_neighbor_cur) &
                              (dat_raw$valid_filter_step1 == TRUE))
    
    if (length(points_neighbor_ids) > 0) {
      points_neighbor <- dat_raw[points_neighbor_ids,]
      # Conditions to accept supporting points:
      # within +/- 2 days, with a similar SL as the suspicious one.
      # "Similar SL": within 3 elevation bands (65 m).
      # Note that a suspicious SL retrieval can support another one
      # (is a cluster of suspicious retrievals still suspicious?)
      points_supporting_cur_ids <- which((abs(as.numeric(points_neighbor$day - suspicious_doy)) <= 5) &
                                       (abs(points_neighbor$SL - suspicious_sl) <= (20*supporting_bands_dist_thresh + 5)))
      points_supporting_n <- points_supporting_n + length(points_supporting_cur_ids)
    }
    
  }
  
  return(points_supporting_n)
}
