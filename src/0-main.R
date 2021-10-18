# This filter removes outliers in the Greenland
# slush limit detections, using two steps:
# (1) iterative removal of the worst conflicting retrievals
# (2) filtering of highest retrievals based on neighborhood comparison.

#### Set language and load builtin packages ####
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")
library(ggplot2)
library(openxlsx)
library(scales)


#### Load function definitions of the filters ####
source("1-func_filter_conflicts.R")
source("1-func_find_worst.R") 
source("2-func_filter_highest.R")
source("2-func_find_suspicious.R")
source("2-func_find_supporting_points_n.R")
source("2-func_check_neighborhoods.R")
source("3-func_plot.R")


#### Configure filters ####
# Step 1
# Set tolerance for SL decrease before declaring a conflict between two retrievals.
sl_decrease_tolerance <- 45 # [m]

# Step 2
# Look this number of stripes in each direction to find supporting retrievals.
stripes_neighbors_n <- 5


#### Read the raw data ####
cat("Loading data...\n")
dat_raw <- openxlsx::read.xlsx("../_slush-limit_output_table_all_20km.xlsx", sheet = "Sheet1", colNames = TRUE, detectDates = TRUE)
dat_raw$date <- as.POSIXct(dat_raw$date, format = "%Y-%m-%d")


#### Step 1: iterative removal of the worst conflicting retrieval ####
dat_raw <- func_filter_conflicts(dat_raw,
                                 sl_decrease_tolerance)


#### Step 2: filtering of the highest retrievals based on their neighborhood ####
# a) Find suspicious highest retrievals.
dat_raw <- func_filter_highest(dat_raw)

# b) Try to rescue them using the neighborhoods.
dat_raw <- func_check_neighborhoods(dat_raw,
                                    stripes_neighbors_n)

#### Plot all stripe/years ####
suppressMessages(func_plot(dat_raw))


#### Write output to XLSX sheet ####
dat_output <- dat_raw[,c(1:44, 47)]
dat_output$date <- as.Date(dat_output$date)
dat_output$valid_filter_step1 <- as.integer(dat_output$valid_filter_step1)
dat_output$valid_filter_step2 <- as.integer(dat_output$valid_filter_step2)
openxlsx::write.xlsx(dat_output, "../_slush-limit_output_table_all_20km_filtered.xlsx",
                     colNames = TRUE,
                     sheetName = "Sheet1")
