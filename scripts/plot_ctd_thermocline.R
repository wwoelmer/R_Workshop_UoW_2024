# script to read in CTD .cnv files, format, calculate thermocline depth, and plot

# load the libraries needed (essentially you are 'activating' the packages)
library(oce)
library(ocedata)
library(tidyverse)
library(rLakeAnalyzer)
## if you get an error when running any of those packages, it means you need to install it
## do that by running install.packages("PACKAGE") and put the package name in quotes


# create a custom function which trims the dataframe to include ONLY the downcast
## some day you might write your own functions, but for now, all you need to know
## is that you have to run this bit of code and R will recognize it as a function
## which you can use (you should see it pop up in your environment on the right)
trim_function <- function(data, parameters){
  max_depth_index <- data$scan[which.max(data$depth)]
  scan_index <- data$scan[max(which(data$depth < parameters[1] & data$scan < max_depth_index))]
  if(!is.finite(scan_index)){scan_index <- 1}
  data$scan >= scan_index
}

#################################################################################
# now let's work with our data

# check what directory you are working in, this will display a location on your computer
getwd()

# identify the folder directory where the CTD files are stored
data_dir <- './data/ctd_cnv_files/' # you can use a period . to write a relative directory
                                    # then, you just need to specify the file path starting from your working directory
                                    # which you can see by using getwd()

# this function will give you a list of all the files in that directory
fls <- list.files(data_dir, pattern = '*.cnv')
fls

# you can index fls to choose which file you want to look at 
fls[1]
  # how can you print out the name of the second file in the list?

# now open a file from the diretory
cnv_data <- read.ctd(paste0(data_dir, fls[1]))
  ## another way to read in data is by specifying the directory, instead of indexing fls
  ## on the line below, uncomment the line and write the relative path to one of the .cnv files
#cnv_data <- read.ctd('INSERT PATH')


# Check the first records of the cnv data
head(data)
##################################################################################
############### woah what is all that????? #######################################
##################################################################################

#################################################################################
# moving on...

# the CTD cast has a downcast and an upcast, which we need to trim
# pass the cnv file object into your trimming function which you ran above on line 9
ctdTrimmed <- ctdTrim(cnv_data, # what is the name of your CTD data object
                      trim_function, # which is the name of the function, you made this one above
                      parameters = list(depth= 0.8)) # what depth do you want to trim at (this currently takes out everything above 0.8m)

# save the trimmed data as a dataframe (this makes it easier to work with in R)
data <- as.data.frame(ctdTrimmed[["data"]])

# Calculate the thermocline using thermo.depth() function from RLakeAnalyzer
?thermo.depth # this will bring up the help documentation so you can find out what info 
              # the function needs

thermo_depth <- thermo.depth(data$temperature, data$depth)

# now I want to save the thermocline depth in the same dataframe
data$thermo_depth <- thermo_depth

# now I want to save the Lake Name (Okaro) as a column
data$lake <- 'Okaro'
  # why do I have to put this is quotations ''  ?

## create a new column which includes the date of the sampling 
  # HINT: the date is stored in the file name which you read in, look at fls[1]

## click on data in your Environment (upper right) and see what it looks like now

##################################################################################
############## let's make a depth profile of our data ############################

# we will be using ggplot, in the data's current form, we can plot one variable at a time
ggplot(data, aes(y = depth, x = temperature)) +
  geom_point() +
  scale_y_reverse() # we do this so that depth goes from 0 (at the top) to the bottom of the lake

################################
### make a plot of oxygen data now instead (copy the code above and modify it using the 'oxygen4' column)

## insert code #

# but we have lots of variables in data that we want to look at
# we can use pivot_longer to transform the shape of our data frame
?pivot_longer
head(data)

data_long <- data %>% 
  select(temperature, conductivity, oxygen2, par, fluorescence) %>%  # select only the variables we want to keep
  pivot_longer(temperature:fluorescence, # name the columns you want to "pivot", using the colon : selects everything between teh two named columns
               names_to = 'variable',# what do you want the new column to be called which will hold the current column names
               values_to = 'value')   # what do you want the new column to be called which hold the numbers in the current columns
head(data_long)
  # uh oh where did the depth information go??? we made a mistake, let's fix it
  # go back and rerun the line of code above, but including the depth column
###################################################################################
### STOP: before you go any further, did you fix the code so depth is included? ###

# now that we have our data in "long format", we can plot all of our variables at once
ggplot(data_long, aes(y = depth, x = value, color = variable)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~variable, scales = 'free') # this makes a separate panel for each variable, 
                                         # we set scales = 'free' so that the x axis can be different for each variable

#### remember above we calculated thermocline depth? using the same code as above, 
#### add thermo_depth to your plot using the function geom_hline()
thermo_depth
?geom_hline()

## copy code from line 112 and add geom_hline with thermo_depth ##

#################################################################################
##################### a few discussion questions ################################

# where is the highest amount of phytoplankton production? why?
# does the lake show anoxia? if so, where? why?
# why might conductivity be highest in the bottom?
