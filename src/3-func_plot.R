# Plot all the stripe/years.
func_plot <- function(dat_raw) {
  
  unlink("../filter_plots", recursive = TRUE)
  dir.create("../filter_plots/interesting", recursive = TRUE)
  dir.create("../filter_plots/not_interesting")
  
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
      points_cur_n <- length(points_cur_ids)
      
      
      #### PLOT the stripe/year ####
      if (points_cur_n > 0) {
        points_cur               <- dat_raw[points_cur_ids,] # All retrievals in the stripe/year
        points_cur$valid <- "0v"
        points_cur_valid         <- points_cur[(points_cur$valid_filter_step1) & (points_cur$valid_filter_step2),]
        if (any(points_cur$valid_filter_step1 == FALSE)) {
          points_cur_invalid_step1 <- points_cur[which(!(points_cur$valid_filter_step1)),] # Invalid retrievals according to filtering step 1
          points_cur$valid[which(!points_cur$valid_filter_step1)] <- "f1"
        }
        if (any(points_cur$valid_filter_step2 == FALSE)) {
          points_cur_invalid_step2 <- points_cur[which(!(points_cur$valid_filter_step2)),] # Invalid retrievals according to filtering step 2
          points_cur$valid[which(!points_cur$valid_filter_step2)] <- "f2"
        }
        points_cur_removed_n     <- length(which(!(points_cur$valid_filter_step1 & points_cur$valid_filter_step2))) # Total number of invalid retrievals.
        mult <- 2 # To scale the plot uniformly with resolution.
        ggplot() +
          geom_point(data = points_cur, aes(x = date, y = SL, fill = valid), shape = 21, stroke = 0.1 * mult, size = 1.7 * mult) +
          geom_line(data = points_cur_valid, aes(x = date, y = SL), size = 0.1 * mult) +
          
          {if (any(points_cur$valid_filter_step1 == FALSE)) geom_point(data = points_cur_invalid_step1, aes(x = date, y = SL), color = "#93320f", shape = 4, size = 0.8 * mult, stroke = 0.45 * mult)} +
          {if (any(points_cur$valid_filter_step2 == FALSE)) geom_point(data = points_cur_invalid_step2, aes(x = date, y = SL), color = "#41557b", shape = 4, size = 0.8 * mult, stroke = 0.45 * mult)} +
          
          xlab("Date") +
          ylab("Slush limit [m a.s.l.]") +
          ggtitle(paste("Slush limits year ", year_cur, " - centre latitude ", stripe_cur, sep = "")) +
          scale_y_continuous(limits = c(130,2700), breaks = seq(0, 2700, 200),
                             minor_breaks = seq(120, 2700, 40), expand = expansion(0,0)) +
          scale_fill_manual(values = c("0v" = "#66c2a5",
                                       "f1" = "#fc8d62",
                                       "f2" = "#8da0cb"),
                            labels = c("0v" = "Valid",
                                       "f1" = "Step 1",
                                       "f2" = "Step 2"),
                            name = "Filter outcome") +
          scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month", limits = as.POSIXct(paste(year_cur, c(5,10), 1), format = "%Y %m %d"), expand = expansion(0,0)) +
          theme_bw(base_size = 8 * mult) +
          theme(axis.text.x.bottom = element_text(angle = 30, hjust = 1),
                plot.title = element_text(hjust = 0.5),
                panel.grid.minor = element_line(color = "#000000", size = 0.05 * mult),
                panel.grid.major = element_line(color = "#000000", size = 0.1 * mult),
                legend.key.height = unit(0.3 * mult, "in"))

        if ((nrow(points_cur) > 3) || (points_cur_removed_n > 0)) {
          ggsave(filename = paste("../filter_plots/interesting/aSL_", stripe_cur, "_", year_cur, ".jpg", sep=""), width = 5*mult, height = 3*mult)
        } else {
          ggsave(filename = paste("../filter_plots/not_interesting/aSL_", stripe_cur, "_", year_cur, ".jpg", sep=""), width = 5*mult, height = 3*mult)
        }
      }
      
    }
  } 
}
