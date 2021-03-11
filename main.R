# NOTE: you need to install the used packages first.
# install.packages("ggplot2")
# install.packages("openxlsx")
# install.packages("scales")

filename <- "../_slush-limit_output_table_SW.xlsx"

outdirname <- "../modis_SW_filtercomplex"

# unlink(outdirname, recursive = TRUE)
dir.create(outdirname)

Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8") # To have month names in English.
library(ggplot2)
library(openxlsx)
library(scales)


#### Filter parameters --------------------------------------------------------------- ####
# These two thresholds regulate the overall aggressiveness of the filtering.
# _decr and _incr refer to the filtering of up- and downwards-moving SL respectively.
thresh_base_decr <- -400 # Base value for the threshold on (SL rate * SL change) when the SL is decreasing.
thresh_base_incr <- 20000 # Base value for the threshold on (SL rate * SL change) when the SL is increasing.

# These four variables regulate the importance of the quality index in the filtering.
# The two thresholds above are multiplied by (sl_qi_mult * quality_index + sl_qi_sum).
# Again with specialization over increase and decrease of the SL.
# A high _sum makes the quality index less important.
sl_qi_sum_incr <- -0.2
sl_qi_sum_decr <- -0.6
sl_qi_mult_incr <- 1.5
sl_qi_mult_decr <- 2

sl_compare_time_range <- 45 # [days]: points farther apart than this are never considered conflicting.

sl_cluster_max_dt <- 7 # [days]: maximum time difference to look for other points which may form a cluster.
sl_cluster_max_dz <- 45 # [m]: maximum vertical distance to look for other points which may form a cluster.

# We rescale the quality index through this function.
# It preserves the ordering (monotonic function)
# but changes the relative distribution, to enable
# easier use of the index later.
# Replace this function with the identity to disable this part
# (also the weights above need to be changed then).
sl_qi_func <- function(x) {2*atan((x - 12) / 3) + 3}
# sl_qi_func <- function(x) {x}
# --------------------------------------------------------------------------------------- #


# This routine examines a point in a stripe
# in comparison to all the others, individually
# (i.e. pairwise). It looks at the rates of rise
# and absolute SL differences.
# Currently it also processes the cluster logic
# (i.e. rewarding points for being part of a cluster);
# this may change later.
# The function is called twice in the code below:
# once before entering the loop, then again
# until there are no conflicts left.
func_eval_point_pairs <- function(points_stripe, point_id) {
  point_dz <- points_stripe$SL[point_id] - points_stripe$SL
  point_dt <- points_stripe$day[point_id] - points_stripe$day
  point_rate <- point_dz / point_dt
  point_rate[is.nan(point_rate)] <- 0 # Rate w.r.t. itself is 0/0 = NaN, but we don't care.
  
  # If at least one point of the pair is after July 31, we relax the
  # metric for the SL decrease, since late summer snowfall can happen
  # and decrease the SL by a lot.
  point_latesummer <- (points_stripe$day > 214) | (points_stripe$day[point_id] > 214)
  
  # This enables big snowfalls (large SL reductions)
  # after July 31; the atan increases the allowed SL reduction
  # if the post-snowfall observation is somewhat isolated.
  point_latesummer_snowfall_mult <- (1 - 0.8 * (point_latesummer & (point_rate < 0))) / (atan((abs(point_dt)) / 20) * 5 +1)
  
  # Cluster logic.
  # Find possible cluster. We define as "cluster" a set of
  # at least 3 points (including the one we are investigating)
  # within short time and elevation distances (user-defined).
  # The cluster score is computed from the quality indices of
  # the points in the cluster [[<disabled>combined with the distance of the
  # investigation point from the center of the cluster:
  # if a cluster is very uniform, the point has to be closer
  # to the mean of the cluster.</disabled>]]
  # We do this by computing the IQR of the SL in the cluster and
  # comparing it to the difference between point SL and cluster mean SL.
  # Clusters can only reinforce the confidence in a point: points are
  # rewarded for being part of a cluster but not penalized for not being part of one.
  # Here we use logical indexing.
  point_cluster_logi <- ((abs(point_dt) < sl_cluster_max_dt) & (abs(point_dz) < sl_cluster_max_dz))
  # point_cluster_logi[point_id] <- FALSE # No cluster with the point itself.

  # Do we really have a cluster?
  # At least 2 elements including the point itself.
  if (length(which(point_cluster_logi)) >= 2) {
    # point_cluster_meanSL <- mean(points_stripe$SL[point_cluster_logi])
    # point_cluster_iqrSL <- max(1,IQR(points_stripe$SL) / 20) # We work in terms of 20-m elevation bands.
    # point_cluster_mean_dist <- abs(points_stripe$SL[point_id] - point_cluster_meanSL)/20 # Distance in bands from the center of the cluster.
    # Cluster score.
    # The higher the cluster score, the better cluster we are in with point_id.
    # So we use this cluster score to decrease the point_sl_metric below.
    # point_cluster_score <- max(1, sum(points_stripe$qindex_proc[point_cluster_logi]) / (1+(point_cluster_mean_dist / point_cluster_iqrSL)) )
    point_cluster_score <- max(1, sum(points_stripe$qindex_proc[point_cluster_logi]) * length(which(point_cluster_logi)))
    # cat("Point", point_id, " | SL:", points_stripe$SL[point_id], "| cluster score:", point_cluster_score, "\n")
  } else {
    point_cluster_score <- 1
  }
  
  # This below is the metric which we check to decide whether we have a conflict.
  # High absolute value of this metric = conflict likely.
  point_sl_metric <- point_latesummer_snowfall_mult * point_dz * point_rate * sign(point_dz) * (abs(point_dt) <= sl_compare_time_range) / point_cluster_score
  
  return(point_sl_metric)
}



# This function compares a point within a stripe
# against the two nearest neighbors together
# (triplet). A point is not nice if it has
# high rates of rise (in absolute value)
# with different sign on the two sides.
# The computed metric is used as a multiplier (always > 1)
# to increase the score of a point (highest scoring
# points are iteratively removed).
# This metric does not apply to the first and last point
# of each stripe (no triplet possible).
func_eval_point_triplets <- function(points_stripe, point_id) {
  
  point_triplet_metric <- 1
  
  if (!((point_id == 1) || (point_id == length(points_stripe[,1])))) {
    
    # 1 = w.r.t. previous point, 2 = w.r.t. next point.
    # Beware of the signs.
    point_dz1 <- points_stripe$SL[point_id] - points_stripe$SL[point_id-1]
    point_dz2 <- points_stripe$SL[point_id+1] - points_stripe$SL[point_id]
    point_dt1 <- points_stripe$day[point_id] - points_stripe$day[point_id-1]
    point_dt2 <- points_stripe$day[point_id+1] - points_stripe$day[point_id]
    point_rate1 <- point_dz1 / point_dt1
    point_rate2 <- point_dz2 / point_dt2
    # A very negative point_combi means the point is a bad spike.
    # Positive ones are good ones, we ignore them.
    # Also, we only consider changes beyond 1 elevation band
    # (possible noise).
    point_combi <- (min(abs(point_dz1), abs(point_dz2)) > 25) * min(0, abs(point_rate1 * point_rate2 * sqrt(abs(point_dz1)) * sqrt(abs(point_dz2))) * sign(point_rate1*point_rate2))
    
    # if (point_combi < -1000) {
    #   cat("point_combi =", point_combi, "\n")
    #   point_combis <<- append(point_combis, point_combi)
    # }
    
    point_triplet_metric <- max(1, log(-point_combi) / 5) # Empirical!
  }
  
  return(point_triplet_metric)
  
}



# Read the raw data.
# dat_raw <- read.csv("sat_modis_proc_l4/slush-limit_output_table.csv")
dat_raw <- openxlsx::read.xlsx(filename, sheet = "Sheet1", colNames = TRUE, detectDates = TRUE)
dat_raw$date <- as.POSIXct(dat_raw$date, format = "%Y-%m-%d")

dat_raw$qindex_proc <- sl_qi_func(dat_raw$SL_qindex)

# Setup the loop: which years and stripes are available?
years   <- sort(unique(dat_raw$year))
stripes <- sort(unique(dat_raw$stripe), decreasing = TRUE)


# Iterate over the years and then (inner loop) over the stripes.
for (year_id in 1:length(years)) {

  # year_id <- 11
  # stripe_id <- 12
  
  year_removed_X <- NULL # For the plot of all stripes in a year.
  year_cur <- years[year_id]
  
  for (stripe_id in 1:length(stripes)) {
    
    stripe_cur <- stripes[stripe_id]
    
    cat("\nProcessing", stripe_cur, "/", year_cur, "\n")
    
    points_cur_ids <- which((dat_raw$year == year_cur) & (dat_raw$stripe == stripe_cur))
    
    # There are some stripes with no retrievals!
    if (length(points_cur_ids) > 0) {
    
      points_cur <- dat_raw[points_cur_ids,]
    
      
      # Find conflicts with tolerances:
      # for each pair of points of a single annual stripe, 
      # provided they are close enough in time (user-defined),
      # the first point conflicts with the second one if a composite
      # metric of (1) total SL change, (2) rate of SL change and (3) point clustering
      # is outside an admissible range.
      # The admissible range is computed from the points
      # quality index: within a single pair the slush limit evolution
      # could also be counted as a conflict for just one of the two points
      # if their quality index is very different, only the low-quality point
      # would be assigned as "in conflict".
      # The composite metric for point z_i of pair (z_i, z_k)
      # with measure dates (t_i, t_k) is defined as
      # sign(z_i - z_k) · (z_i - z_k) · (z_i - z_k) / (t_i - t_k) / cluster_score_i.
      # It includes the contributions from total change and rate of change,
      # and the sign is consistent (i.e. the same when computed
      # from the two points, since the SL change is a characteristic
      # of the pair and not of the individual point).
      # Conflicts are not just counted, each conflict is assigned a score
      # which depends on both quality indices of the pair:
      # for point1 in pair (point1, point2) the score of a found conflict is computed as
      # point2_qi / (point1_qi^2), so that both the absolute and relative
      # values of the quality index of the current point are taken into account.
      # So that a conflict with a low-quality point is less relevant than a
      # conflict with a high-quality point, and conflicts count less for
      # high-quality points.

      npoints_cur <- length(points_cur[,1])
      conflicts_cur <- matrix(data = NA, nrow = npoints_cur, ncol = npoints_cur)
      points_cur$pairs_score <- NA
      points_cur$triplets_score <- NA
      for (point_id in 1:npoints_cur) {
        
        point_cur_pairs_metric <- func_eval_point_pairs(points_cur, point_id)
        point_cur_triplets_metric <- func_eval_point_triplets(points_cur, point_id)
        
        # High triplets_score = point has strongly contrasting SL rates on its two sides (past and future).
        # We try to use the triplets score to change (reduce) the thresholds for the conflicts.
        # (EXPERIMENTAL: use it also at the same time to compute the final score).
        points_cur$triplets_score[point_id] <- point_cur_triplets_metric
        
        point_threshold_decreasing <- thresh_base_decr * (sl_qi_mult_decr * points_cur$qindex_proc[point_id] + sl_qi_sum_decr) / point_cur_triplets_metric
        point_threshold_increasing <- thresh_base_incr * (sl_qi_mult_incr * points_cur$qindex_proc[point_id] + sl_qi_sum_incr) / point_cur_triplets_metric
        
        conflicts_cur[,point_id] <- (points_cur$qindex_proc / (points_cur$qindex_proc[point_id])^2) * as.numeric(((point_cur_pairs_metric > point_threshold_increasing) | (point_cur_pairs_metric < point_threshold_decreasing)))
      
        # High pairs_score = point is in conflict with a lot of high-quality points.
        points_cur$pairs_score[point_id] <- sum(conflicts_cur[,point_id])

      }
      
      # Create "pruned" data frame which we will strip of the
      # conflicting points, one by one until all conflicts are solved.
      points_cur_pruned <- points_cur
      points_removed_X <- NULL
      
      # Iterative removal of the worst conflicting point.
      # The worst conflicting point is not simply the one
      # with the largest number of conflicts.
      # We also consider the rate of change of the slush limit
      # compared to the neighbors.
      while (any(points_cur_pruned$pairs_score > 0)) {
        
        # cat("Still", sum(points_cur_pruned$pairs_score > 0), "conflict(s) to solve...\n")
        
        # Compute the final point score, which is used to remove the worst point in the set.
        # points_cur_pruned$score <- points_cur_pruned$n_conflicts * (mean(points_cur_pruned_rate_mean) + points_cur_pruned_rate_mean) 
        # points_cur_pruned$score <- (points_cur_pruned$n_conflicts > 0) * (2 + points_cur_pruned$n_conflicts) * (mean(points_cur_pruned_rate_mean) + points_cur_pruned_rate_mean) * (20 - points_cur_pruned$SL_qindex)
        points_cur_pruned$score <- points_cur_pruned$triplets_score * (points_cur_pruned$pairs_score > 0) * (.1 + points_cur_pruned$pairs_score) * (5 - points_cur_pruned$qindex_proc)
        
        # cat("Pair scores:", round(points_cur_pruned$pairs_score, 1), "\n")
        # cat("Triplet scores:", round(points_cur_pruned$triplets_score, 1), "\n")
        # cat("Scores:", round(points_cur_pruned$score, 1), "\n")
        
        id_worst <- which.max(points_cur_pruned$score)
        
        # cat("Removing point", id_worst, "out of", length(points_cur_pruned$score),"| score = ", points_cur_pruned$score[id_worst], "\n")
        # cat("\n")
        points_removed_X <- append(points_removed_X, points_cur_pruned$X[id_worst]) # To keep track of the removed points.
        points_cur_pruned <- points_cur_pruned[-id_worst,]
        npoints_cur_pruned <- length(points_cur_pruned[,1])

        conflicts_cur <- matrix(data = NA, nrow = npoints_cur_pruned, ncol = npoints_cur_pruned)
        points_cur_pruned$pairs_score <- NA
        points_cur_pruned$triplets_score <- NA
        for (point_id in 1:npoints_cur_pruned) {
          
          point_cur_pairs_metric <- func_eval_point_pairs(points_cur_pruned, point_id)
          point_cur_triplets_metric <- func_eval_point_triplets(points_cur_pruned, point_id)
          
          points_cur_pruned$triplets_score[point_id] <- point_cur_triplets_metric
          
          point_threshold_decreasing <- thresh_base_decr * (sl_qi_mult_decr * points_cur_pruned$qindex_proc[point_id] + sl_qi_sum_decr) / point_cur_triplets_metric
          point_threshold_increasing <- thresh_base_incr * (sl_qi_mult_incr * points_cur_pruned$qindex_proc[point_id] + sl_qi_sum_incr) / point_cur_triplets_metric
          
          conflicts_cur[,point_id] <- (points_cur_pruned$qindex_proc / (points_cur_pruned$qindex_proc[point_id])^2) * as.numeric((point_cur_pairs_metric > point_threshold_increasing) | (point_cur_pairs_metric < point_threshold_decreasing))
          
          points_cur_pruned$pairs_score[point_id] <- sum(conflicts_cur[,point_id])

        }
        
        # Intermediate (debug) plot after each point removal.
        # ggplot(points_cur_pruned) +
        #   geom_point(aes(x = date, y = SL, fill = n_conflicts), shape = 21, stroke = 0, size = 3) +
        #   geom_line(aes(x = date, y = SL), size = 0.1) +
        #   geom_text(aes(x = date, y = SL + 50, label = n_conflicts), size = 2) +
        #   scale_fill_gradientn(colors = c("#888888", "#DE26D7", "#2121CF", "#44AAF0", "#86F6C6", "#F9F961", "#D42E3B", "#000000")) +
        #   scale_x_datetime(date_labels = "%Y-%m-%d") +
        #   theme_bw()
      }
      
      # npoints_removed <- length(points_cur[,1]) - length(points_cur_pruned[,1])
      npoints_removed_phase1 <- length(points_removed_X)
      if (length(points_removed_X) > 0) {
        cat("All conflicts solved! Removed", npoints_removed_phase1, "point(s).\n")
      }
      
      
      # New: additional filter on the highest points of each stripe.
      # This tries to remove points which pollute
      # the information on the highest SL elevation
      # in a season.
      # It works by checking the points in the highest
      # elevation band found:
      # if they are low quality w.r.t. points in the two (non-empty) bands below the highest,
      # then the highest ones might be considered outliers and be removed.
      # The comparison uses the number of points, sum of quality indices and max quality index
      # (max: so that a good quality point in a band can be used to validate the trustworthiness of other low quality points).
      points_cur_pruned$ele_band <- round(points_cur_pruned$SL / 20)
      ele_bands_unique <- sort(unique(points_cur_pruned$ele_band))
      ele_bands_n <- length(ele_bands_unique)
      
      # Apply this filter only if we
      # have at least 4 elevation
      # bands, else it is too risky.
      if (ele_bands_n > 3) {
        ele_bands_top1 <- ele_bands_unique[ele_bands_n]
        ele_bands_top2 <- ele_bands_unique[ele_bands_n-1]
        ele_bands_top3 <- ele_bands_unique[ele_bands_n-2]
        
        points_top1_ids <- which(points_cur_pruned$ele_band == ele_bands_top1)
        points_top1_n <- length(points_top1_ids)
        points_top1_qi <- points_cur_pruned$SL_qindex[points_top1_ids]
        
        points_top2_ids <- which(points_cur_pruned$ele_band == ele_bands_top2)
        points_top2_n <- length(points_top2_ids)
        points_top2_qi <- points_cur_pruned$SL_qindex[points_top2_ids]
        
        points_top3_ids <- which(points_cur_pruned$ele_band == ele_bands_top3)
        points_top3_n <- length(points_top3_ids)
        points_top3_qi <- points_cur_pruned$SL_qindex[points_top3_ids]
        
        
        points_top1_qi_thresh <- ifelse(max(points_top1_ids) == length(points_cur_pruned[,1]), 6, 8)
        highest_point_safe <- FALSE # This flag is used in case the highest band only contains the last point in the series. That one receives special processing.
        # If TRUE: points in the highest band are not that good.
        # Check the lower bands for some more quality.
        if ((sum(points_top1_qi) < points_top1_qi_thresh) || ((max(points_top1_qi) < 5) && (points_top1_n < 5)) || ((max(points_top1_qi) < median(c(points_top1_qi, points_top2_qi, points_top3_qi))) && (median(points_top1_qi) < 7)) || ((points_top1_n == 1) && (points_top1_qi < (max(points_top2_qi) - 2)) && (points_top1_qi < 10) ) ) {
          
          # If the low-quality highest band contains just a
          # single point, we first check whether that point
          # is actually a good continuation of the points before, with
          # (almost) the same mean rate (computed with least squares on the earlier points).
          # We look at earlier points within 60 days (but not more than 8 earlier points)
          # and compute dz, dt and rates.
          if ((points_top1_n == 1) && (points_top1_ids == length(points_cur_pruned[,1]))) {
            
            # dz_all <- points_cur_pruned$SL[points_top1_ids] - points_cur_pruned$SL
            dt_all <- points_cur_pruned$day[points_top1_ids] - points_cur_pruned$day
            
            ids_recent <- which(dt_all < 60)
            if (length(ids_recent) > 3) { # Only do this estimation if we have a few points to estimate the rate on.
              points_recent_sel <- points_cur_pruned[max(1, length(points_cur_pruned[,1]) - 8):(length(points_cur_pruned[,1])-1),]
              
              rate_recent <- lm(data = points_recent_sel, formula = SL~day)
              point_highest_prediction <- predict.lm(rate_recent, newdata = data.frame(day = points_cur_pruned$day[points_top1_ids]), se.fit = TRUE)
              point_highest_meas <- points_cur_pruned$SL[points_top1_ids]
              # The last point is "safe" (i.e. not further examined for filtering)
              # if it is within +/- twice the standard error from its linear prediction.
              if ((point_highest_meas < point_highest_prediction$fit + 2*point_highest_prediction$se.fit) && (point_highest_meas > point_highest_prediction$fit - 2*point_highest_prediction$se.fit)) {
                highest_point_safe <- TRUE
              } else {
                highest_point_safe <- FALSE
              }
            }
          }
          
          # Continue with the filter only if highest_point_safe is FALSE,
          # i.e. if we are NOT in the case where the highest band only has
          # the last point and that last point (even if low-quality) is a
          # good linear continuation of the points before it.
          if (highest_point_safe == FALSE) {
            
            # Second band from top is good! (Or at least much much better than the first).
            # If the second band includes the last point in the series,
            # we relax a bit the threshold for the quality of the points,
            # since we may have a last point going up which could be important.
            points_top2_qi_thresh <- ifelse(max(points_top2_ids) == length(points_cur_pruned[,1]), 7, 10)
            # points_top2_qi_thresh <- 10
            if ((sum(points_top2_qi) >= min(points_top2_qi_thresh, max(points_top1_qi) + 4)) && (max(points_top2_qi) > max(points_top1_qi))) {
              
              # If the second band is good and is >20 m below
              # the first, discard the first (else the
              # first could be correct but slightly noisy, keep it).
              # Discard the first also if the second is immediately
              # below, BUT the points in it are far from the points
              # in the highest (e.g. -2578250/2007).
              if (((ele_bands_top1 - ele_bands_top2) > 1)) {
                points_removed_X <- append(points_removed_X, points_cur_pruned$X[points_top1_ids])
                points_cur_pruned <- points_cur_pruned[-points_top1_ids,]
              }
              
              # Second band from top is not that good, check the third one.
            } else {
              
              # Third band is good! Discard the first,
              # and (only if it is not immediately contiguous)
              # also the second.
              if ((sum(points_top3_qi) >= 10) && (max(points_top3_qi) > max(points_top1_qi)) && (((ele_bands_top2 - ele_bands_top3) < 6))) {
                if ((ele_bands_top2 - ele_bands_top3) > 1) {
                  points_removed_X <- append(points_removed_X, points_cur_pruned$X[c(points_top1_ids, points_top2_ids)])
                  points_cur_pruned <- points_cur_pruned[-c(points_top1_ids, points_top2_ids),]
                } else {
                  if ((ele_bands_top1 - ele_bands_top2) > 1) {
                    points_removed_X <- append(points_removed_X, points_cur_pruned$X[points_top1_ids])
                    points_cur_pruned <- points_cur_pruned[-points_top1_ids,]
                  }
                }
              }
            }
          }
        }
      } # End of the filter on the highest bands.
      
      
      point_removed_ids <- pmatch(points_removed_X, points_cur$X)
      points_removed <- points_cur[point_removed_ids,]
      year_removed_X <- append(year_removed_X, points_removed_X) # Keep annual track of the removed Xs.
      
      npoints_removed_topfilter <- length(points_removed_X) - npoints_removed_phase1
      if (npoints_removed_topfilter > 0) {
        cat("Highest bands filter: removed", npoints_removed_topfilter, "more point(s).\n")
      }
      
      # Plot the stripe.
      mult <- 2
      ggplot() +
        geom_point(data = points_cur, aes(x = date, y = SL, fill = SL_qindex), shape = 21, stroke = 0.1 * mult, size = 1.7 * mult) +
        geom_line(data = points_cur_pruned, aes(x = date, y = SL), size = 0.1 * mult) +
        geom_point(data = points_removed, aes(x = date, y = SL), shape = 4, size = 0.8 * mult, stroke = 0.15 * mult, color = "#000000") +
        geom_text(data = points_cur, aes(x = date, y = SL + 30, label = round(pairs_score,1)), size = 0.7 * mult) +
        geom_text(data = points_cur, aes(x = date, y = SL + 60, label = round(triplets_score,1)), size = 0.7 * mult) +
        xlab("Date") +
        ylab("Slush limit [m a.s.l.]") +
        ggtitle(paste("Slush limits year ", year_cur, " - centre latitude ", stripe_cur, sep = "")) +
        scale_y_continuous(limits = c(400,2200), breaks = seq(400, 2200, 200),
                           minor_breaks = seq(400, 2200, 40)) +
        scale_fill_fermenter(name = "Quality\nindex", type = "div", palette = "RdYlGn",
                             direction = 1, limits = c(0,15), breaks = c(2,3,4,5,6,7,8,9,10,11)) +
                             # labels = c("1.0", "3.0","5", "6", "7", "8", "9", "11.0", "13.0")) +
        # scale_fill_gradientn(name = "Initial #\nof conflicts",
                             # colors = c("#888888", "#DE26D7", "#2121CF", "#44AAF0", "#86F6C6", "#D9D951", "#D42E3B", "#000000")) +
        scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month", limits = as.POSIXct(paste(year_cur, c(5,10), 1), format = "%Y %m %d")) +
        theme_bw(base_size = 8 * mult) +
        theme(axis.text.x.bottom = element_text(angle = 30, hjust = 1),
              plot.title = element_text(hjust = 0.5),
              panel.grid.minor = element_line(color = "#000000", size = 0.05 * mult),
              panel.grid.major = element_line(color = "#000000", size = 0.1 * mult),
              legend.key.height = unit(0.3 * mult, "in"))
      ggsave(filename = paste0(outdirname, "/aSL_", stripe_cur, "_", year_cur, ".jpg"), width = 5*mult, height = 3*mult)
    
    }
  }
  
  dat_year <- dat_raw[which(dat_raw$year == year_cur),]
  year_removed_ids <- pmatch(year_removed_X, dat_year$X)
  dat_year_pruned <- dat_year[-year_removed_ids,]
  
}
