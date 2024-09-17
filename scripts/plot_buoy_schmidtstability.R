# script to read in Rotorua buoy data, do some formatting, plot the data, and calculate schmidt stability
library(tidyverse)
library(rLakeAnalyzer)
library(readxl)
library(ggpubr)

# read in the buoy data csv
buoy <- read.csv('./data/Rotorua_202202-202409_profiles.csv')
head(buoy)

# we need to format the DateTime column
# because it stored both date and time, we will use as.POSIXct() (as opposed to as.Date())
?as.POSIXct
buoy$DateTime <- as.POSIXct(buoy$DateTime, format = "%Y-%m-%d %H:%M:%OS")
head(buoy)


# plot one column, using depth as the color
ggplot(buoy, aes(x = DateTime, y = FlChlr, color = DptSns)) +
  geom_point()

## create another plot with a different variable
  ## INSERT CODE ##

################################################################################
# ok, that's a lot of data and it take kind of a long time to plot
# let's simplify down to a daily average and round the depths to the nearest meter

buoy_avg <- buoy %>%  # we're taking buoy and modifying, so we're going to create a new dataframe
  mutate(date = as.Date(DateTime),  # make a column that is just the date (doesn't include time)
         depth_rnd = floor(DptSns))  # make another column that rounds the depth to the nearest meter
view(buoy_avg)
head(buoy_avg)

buoy_avg <- buoy_avg %>% 
  group_by(date, depth_rnd) %>% 
  summarise(temp_C = mean(TmpWtr,  na.rm = TRUE), 
            DO_mgL = mean(DOconc, na.rm = TRUE),
            DO_sat = mean(DOpsat, na.rm = TRUE),
            sp_cond_uSm = mean(SpCond, na.rm = TRUE),
            pH = mean(pH, na.rm = TRUE),
            chl_RFU = mean(FlChlr, na.rm = TRUE))

# now let's plot the data
## modify this code which is copied from above to plot the new dataframe, buoy_avg
#ggplot(buoy, aes(x = DateTime, y = FlChlr, color = DptSns)) +
#  geom_point()

# similar to with the CTD data, it's easier to visualize all of these if we pivot to a longer format
buoy_long <- buoy_avg %>% 
  pivot_longer(temp_C:chl_RFU, names_to = 'variable', values_to = 'value')

## make a plot, similar ot what we did with the CTD data where you plot all the variables, using
## facet_wrap(~variable) to make different panels for each variable


##################################################################################
############## now let's calculate Schmidt stability #############################
?schmidt.stability
# the help documentation tells us that we need bathymetry data to calculate this, so let's read it in

# read in bathy estimates
bty <- read_excel('./data/Rotlakes_bathymetry.xls', skip = 1)

# I don't like the column names, so we're going to rename them for readability
colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 'planar_sa_m2', 
                   'model_sd_m2')
head(bty)

# the depth are negative but we don't really care about that, so let's take the negative off
bty$depth_m <- abs(bty$depth_m)

# this dataframe include all the lakes, so we need to subset to Rotorua only
bty <- bty %>% 
  filter(lake=='Rotorua')

buoy_avg <- na.omit(buoy_avg)

# now, for each day, let's calculate thermocline depth and schmidt stability
mix <- buoy_avg %>% 
  group_by(date) %>% 
  summarise(thermo_depth = thermo.depth(temp_C, depth_rnd, seasonal = TRUE),
            thermo_depth = ifelse(is.na(thermo_depth), 0, thermo_depth), # thermo.depth will return NA if the lake is mixed, so we will set these to 0
            schmidt_stability = schmidt.stability(temp_C, depth_rnd, bthA = bty$model_sd_m2, bthD = bty$depth_m))

# now let's make some fun plots
a <- ggplot(mix, aes(x = date, y = thermo_depth)) +
  geom_point() +
  theme_bw()

b <- ggplot(mix, aes(x = date, y = schmidt_stability)) +
  geom_point() +
  theme_bw()

ggarrange(a, b)
