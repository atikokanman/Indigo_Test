##########Grain Quality Case Study-Sam Peters##########
#Set work directory
setwd ("C:/Jobs/Assessments")
#Import needed libraries
library(data.table) 
library(plyr)
library(pastecs)
library(car)
library (sjPlot)
library (ggplot2)
library (raster)
library(rgdal)
library (spdep)
library (ape)

################################################################
#DATA CLEANING AND MANAGEMENT
#####Convert Zip Codes in Grain Files to Counties and Merge#####
#Import three yearly wheat data csvs
Grain_2014 = read.csv("2014 Wheat Data.csv")
Grain_2015 = read.csv("2015 Wheat Data.csv") 
Grain_2016 = read.csv("2016 Wheat Data.csv")
#Import Zip to County HUD csv
Zip_County = read.csv("ZIP_COUNTY_122016.csv")
#Change heading of zip code in grain file to match zip_county file for merging
Grain_2014$ZIP=Grain_2014$zip_code
#Cut down HUD file to only zip codes that are in grain file
Zipdiff <- setdiff(x=Zip_County$ZIP, y=Grain_2014$ZIP)
Zipclean <- Zip_County[!Zip_County$ZIP %in% Zipdiff, ]
#Create file of only highest percentage county for each zip code 
require(data.table) 
Highzip <- as.data.table(Zipclean)
Highzip <- Highzip[Highzip[, .I[TOT_RATIO == max(TOT_RATIO)], by=ZIP]$V1]
#Aggregate Grain by zip code
Grain_2014_Agg = aggregate(Grain_2014,by = list(Grain_2014$ZIP), FUN = mean)
#Get rid of extra two zip codes by making new dataset with only matches between Agg and High zip
Zipdiff.2 <- setdiff(x=Grain_2014_Agg$ZIP, y=Highzip$ZIP)
Zipclean.2 <- Grain_2014_Agg[!Grain_2014_Agg$ZIP %in% Zipdiff.2, ]
#Merge Zipclean.2 and Highzip to get counties and zips on grain file
Grain_2014_Count <- merge(Zipclean.2,Highzip,by="ZIP")
#Aggregate grain by county now that that is a variable
Grain_2014_Agg = aggregate(Grain_2014_Count,by = list(Grain_2014_Count$COUNTY), FUN = mean)

#####Get weather file to match grain file and merge#####
#Import total weather file
Weather = read.csv("daily_weather.csv") 
#Make COUNTY variable of the numbers in adm2code column
Weather$COUNTY = as.numeric(gsub("\\US", "", Weather$adm2_code)) 
#Cut down weather data to only 2014
Weather_2014 = subset (Weather, year == 2014)
#Cut out any weather data that isn't for the counties in grain data set
Weather_2014_diff <- setdiff(x=Weather_2014$COUNTY, y=Grain_2014_Count$COUNTY)
Weather_Clean_2014 <- Weather_2014[!Weather_2014$COUNTY %in% Weather_2014_diff, ]
#Aggregate by the counties 
Weather_Agg_2014 = aggregate(Weather_Clean_2014, by = list(Weather_Clean_2014$COUNTY), FUN = mean) #Less counties in Weather than Grain
#Make grain data frame with only counties in weather
Grain_2014_diff <- setdiff(x=Grain_2014_Agg$COUNTY, y=Weather_Agg_2014$COUNTY)  ###146 counties in grain 2014 and 96 in 2014 weather
Grain_Clean_2014 <- Grain_2014_Agg[!Grain_2014_Agg$COUNTY %in% Grain_2014_diff, ]
#Merge what's left of Grain and Weather that match up
Total_2014 <- merge(Grain_Clean_2014,Weather_Agg_2014,by="COUNTY")
#Delete duplicate columns to get clean 2014
Total_2014_Clean = subset(Total_2014, select = -c(Group.1.x, Group.1.x, year.x, zip_code, X.x, Group.1.y, X.y, adm2_code, date, area) )
#Export 2014 data to csv
write.csv(Total_2014_Clean, "2014_Clean.csv")

###Repeat for 2015 and 2016 (All notes can be found above in steps for 2014 data cleaning)###
#2015
Grain_2015$ZIP=Grain_2015$zip_code
Zipdiff <- setdiff(x=Zip_County$ZIP, y=Grain_2015$ZIP)
Zipclean <- Zip_County[!Zip_County$ZIP %in% Zipdiff, ]
Highzip <- as.data.table(Zipclean)
Highzip <- Highzip[Highzip[, .I[TOT_RATIO == max(TOT_RATIO)], by=ZIP]$V1]
Grain_2015_Agg = aggregate(Grain_2015,by = list(Grain_2015$ZIP), FUN = mean)
Zipdiff.2 <- setdiff(x=Grain_2015_Agg$ZIP, y=Highzip$ZIP)
Zipclean.2 <- Grain_2015_Agg[!Grain_2015_Agg$ZIP %in% Zipdiff.2, ]
Grain_2015_Count <- merge(Zipclean.2,Highzip,by="ZIP")
Grain_2015_Agg = aggregate(Grain_2015_Count,by = list(Grain_2015_Count$COUNTY), FUN = mean)

Weather_2015 = subset (Weather, year == 2015)
Weather_2015_diff <- setdiff(x=Weather_2015$COUNTY, y=Grain_2015_Count$COUNTY)
Weather_Clean_2015 <- Weather_2015[!Weather_2015$COUNTY %in% Weather_2015_diff, ]
Weather_Agg_2015 = aggregate(Weather_Clean_2015, by = list(Weather_Clean_2015$COUNTY), FUN = mean) 
Grain_2015_diff <- setdiff(x=Grain_2015_Agg$COUNTY, y=Weather_Agg_2015$COUNTY)  ###152 counties in 2015 grain and 60 in 2015 weather
Grain_Clean_2015 <- Grain_2015_Agg[!Grain_2015_Agg$COUNTY %in% Grain_2015_diff, ]
Total_2015 <- merge(Grain_Clean_2015,Weather_Agg_2015,by="COUNTY")
Total_2015_Clean = subset(Total_2015, select = -c(Group.1.x, Group.1.x, year.x, zip_code, X.x, Group.1.y, X.y, adm2_code, date, area) )
write.csv(Total_2015_Clean, "2015_Clean.csv")

#2016
Grain_2016$ZIP=Grain_2016$zip_code
Zipdiff <- setdiff(x=Zip_County$ZIP, y=Grain_2016$ZIP)
Zipclean <- Zip_County[!Zip_County$ZIP %in% Zipdiff, ]
Highzip <- as.data.table(Zipclean)
Highzip <- Highzip[Highzip[, .I[TOT_RATIO == max(TOT_RATIO)], by=ZIP]$V1] 
Grain_2016_Agg = aggregate(Grain_2016,by = list(Grain_2016$ZIP), FUN = mean)
Zipdiff.2 <- setdiff(x=Grain_2016_Agg$ZIP, y=Highzip$ZIP)
Zipclean.2 <- Grain_2016_Agg[!Grain_2016_Agg$ZIP %in% Zipdiff.2, ]
Grain_2016_Count <- merge(Zipclean.2,Highzip,by="ZIP")
Grain_2016_Agg = aggregate(Grain_2016_Count,by = list(Grain_2016_Count$COUNTY), FUN = mean)

Weather_2016 = subset (Weather, year == 2016)
Weather_2016_diff <- setdiff(x=Weather_2016$COUNTY, y=Grain_2016_Count$COUNTY)
Weather_Clean_2016 <- Weather_2016[!Weather_2016$COUNTY %in% Weather_2016_diff, ]
Weather_Agg_2016 = aggregate(Weather_Clean_2016, by = list(Weather_Clean_2016$COUNTY), FUN = mean) 
Grain_2016_diff <- setdiff(x=Grain_2016_Agg$COUNTY, y=Weather_Agg_2016$COUNTY)    ###141 counties in 2016 grain and 64 in 2016 weather
Grain_Clean_2016 <- Grain_2016_Agg[!Grain_2016_Agg$COUNTY %in% Grain_2016_diff, ]
Total_2016 <- merge(Grain_Clean_2016,Weather_Agg_2016,by="COUNTY")
Total_2016_Clean = subset(Total_2016, select = -c(Group.1.x, Group.1.x, year.x, zip_code, X.x, Group.1.y, X.y, adm2_code, date, area) )
write.csv(Total_2016_Clean, "2016_Clean.csv")

#############################################################################################
###EXPLORATORY STATISTICS
###Import full clean and merged dataset of all three years (three years combined in Excel)###
Indigo_Grain = read.csv("Indigo_Total.csv")
#Make year a categorical variable for regressions
Indigo_Grain$Year <- factor(Indigo_Grain$Year) 

###CHECKING FOR NORMALITY###
#Check normality of all variables with histograms (Two graphs modified for presentation)
hist(Indigo_Grain$moisture, main="Histogram for Grain Moisture (a)",  xlab="Grain Moisture (percent)", border="black", col="blue",las=1) #good
hist(Indigo_Grain$kernel_weight) #good
hist(Indigo_Grain$actual_wheat_ash) #good
hist(Indigo_Grain$protein_12) #good
hist(Indigo_Grain$protein_12) #good
hist(Indigo_Grain$met_avg_t) #good
hist(Indigo_Grain$met_gdd) #good
hist(Indigo_Grain$met_max_rh) #could be left skewed
hist(Indigo_Grain$met_max_t) #good
hist(Indigo_Grain$met_p_mm, main="Histogram of Precipitation (b)", xlab="Average Daily Precipitation (mm)", border="black", col="red", las=1) #right skewed
hist(Indigo_Grain$met_sh) #maybe right skewed
hist(Indigo_Grain$met_sr_wm2) #maybe right skewed
#Testing for normality using Shapiro Wilks to see if non-normal histograms are confirmed (all had sig p-values)
shapiro.test(Indigo_Grain$met_max_rh) #p-value <0.0001
shapiro.test(Indigo_Grain$met_p_mm) #p-value 0.0005
shapiro.test(Indigo_Grain$met_sh) #p-value <0.0001
shapiro.test(Indigo_Grain$met_sr_wm2) #p-value <0.0001

###SUMMARY STATISTICS###
stat.desc(Indigo_Grain)
Indigo_Stats = stat.desc(Indigo_Grain, basic=F)
write.csv(Indigo_Stats, "Indigo Descriptive Stats.csv")

#######################################################################
#EXPLORING RELATIONSHIPS BETWEEN GRAIN AND WEATHER VARIABLES
###ONE VARIABLE LINEAR MODELS TO SEE POTENTIAL INITIAL RELATIONSHIPS###
#Moisture percentage
moisture_met_avg_t = lm (moisture~met_avg_t, data=Indigo_Grain) #Average temp
summary(moisture_met_avg_t) #-0.26496    0.05186  -5.109 7.81e-07 ***
moisture_met_gdd = lm (moisture~met_gdd, data=Indigo_Grain) #Growing degree days
summary(moisture_met_gdd) #-0.5154     0.1210   -4.26  3.2e-05 ***
moisture_met_max_rh = lm (moisture~met_max_rh, data=Indigo_Grain) #Max relative humidity
summary(moisture_met_max_rh) #-0.07594    0.01607  -4.726 4.43e-06 ***
moisture_met_max_t = lm (moisture~met_max_t, data=Indigo_Grain) #Maximum temp
summary(moisture_met_max_t) #-0.34633    0.03846  -9.005   <2e-16 ***            
moisture_met_max_vpd = lm (moisture~met_max_vpd, data=Indigo_Grain) #Maximun vapor pressure deficit
summary(moisture_met_max_vpd) #0.9782     0.4567   2.142   0.0335 *
moisture_met_min_rh = lm (moisture~met_min_rh, data=Indigo_Grain) #Minimum relative humidity
summary(moisture_met_min_rh) #0.04624    0.01024   4.517  1.1e-05 ***
moisture_met_min_t = lm (moisture~met_min_t, data=Indigo_Grain) #Minimum temperature
summary(moisture_met_min_t) #-0.01818    0.04493  -0.405    0.686
moisture_met_p_mm = lm (moisture~met_p_mm, data=Indigo_Grain) #Observed precipitation 
summary(moisture_met_p_mm) #0.3033     0.1049   2.891  0.00428 **
moisture_met_sh = lm (moisture~met_sh, data=Indigo_Grain) #Observed specific humidity
summary(moisture_met_sh) #-152.925    139.708  -1.095    0.275
moisture_met_sr_wm2 = lm (moisture~met_sr_wm2, data=Indigo_Grain) #Observed downwelling shortwave radiation 
summary(moisture_met_sr_wm2) #-0.039707   0.006448  -6.158 4.26e-09 ***

###MODEL WITH ALL VARIABLES AND TEST FOR COLINEARITY TO SEE IF ANY SHOULD BE REMOVED###
moisture_reg_full = lm (moisture~met_avg_t + met_gdd + met_max_rh + met_max_t + met_max_vpd + met_min_rh + met_min_t + met_p_mm + met_sh + met_sr_wm2, data = Indigo_Grain)
summary (moisture_reg_full)
vif(moisture_reg_full) #calcualtes VIF to test for colinearity (everything is very high)

#Removed several variables that were highly correlated to get the following model with VIFs of 4 and below
moisture_colin = lm (moisture~ met_avg_t + met_max_rh + met_p_mm + met_sr_wm2, data = Indigo_Grain)
moisture_colin = lm (moisture~ met_avg_t + met_max_rh + met_p_mm + met_sr_wm2 + Year, data = Indigo_Grain) #With year included as categorical variable
summary(moisture_colin)
vif(moisture_colin)
tab_model(moisture_colin) #Makes table with CI and p-values for model


#kernel_weight percentage
kernel_weight_met_avg_t = lm (kernel_weight~met_avg_t, data=Indigo_Grain) #Average temp
summary(kernel_weight_met_avg_t) #0.09724    0.16690   0.583    0.561
kernel_weight_met_gdd = lm (kernel_weight~met_gdd, data=Indigo_Grain) #Growing degree days
summary(kernel_weight_met_gdd) #0.2199     0.3822   0.575    0.566
kernel_weight_met_max_rh = lm (kernel_weight~met_max_rh, data=Indigo_Grain) #Max relative humidity
summary(kernel_weight_met_max_rh) #-0.08765    0.05091  -1.721   0.0868 .
kernel_weight_met_max_t = lm (kernel_weight~met_max_t, data=Indigo_Grain) #Maximum temp
summary(kernel_weight_met_max_t) #0.08459    0.13856   0.611    0.542
kernel_weight_met_max_vpd = lm (kernel_weight~met_max_vpd, data=Indigo_Grain) #Maximun vapor pressure deficit
summary(kernel_weight_met_max_vpd) #1.8559     1.3898   1.335    0.183
kernel_weight_met_min_rh = lm (kernel_weight~met_min_rh, data=Indigo_Grain) #Minimum relative humidity
summary(kernel_weight_met_min_rh) #-0.03360    0.03245  -1.035    0.302
kernel_weight_met_min_t = lm (kernel_weight~met_min_t, data=Indigo_Grain) #Minimum temperature
summary(kernel_weight_met_min_t) #0.04741    0.13575   0.349    0.727
kernel_weight_met_p_mm = lm (kernel_weight~met_p_mm, data=Indigo_Grain) #Observed precipitation 
summary(kernel_weight_met_p_mm) #-0.3109     0.3230  -0.963    0.337
kernel_weight_met_sh = lm (kernel_weight~met_sh, data=Indigo_Grain) #Observed specific humidity
summary(kernel_weight_met_sh) #-91.991    423.381  -0.217    0.828
kernel_weight_met_sr_wm2 = lm (kernel_weight~met_sr_wm2, data=Indigo_Grain) #Observed downwelling shortwave radiation 
summary(kernel_weight_met_sr_wm2) #0.01818    0.02129   0.854    0.394

#Using reduced model previously determined
kernel_weight_colin = lm (kernel_weight~ met_avg_t + met_max_rh + met_p_mm + met_sr_wm2, data = Indigo_Grain)
kernel_weight_colin = lm (kernel_weight~ met_avg_t + met_max_rh + met_p_mm + met_sr_wm2 + Year, data = Indigo_Grain) #With year
summary(kernel_weight_colin)
vif(kernel_weight_colin)
tab_model(kernel_weight_colin)

#actual_wheat_ash percentage
actual_wheat_ash_met_avg_t = lm (actual_wheat_ash~met_avg_t, data=Indigo_Grain) #Average temp
summary(actual_wheat_ash_met_avg_t) #-3.226e-05  1.950e-04  -0.165    0.869
actual_wheat_ash_met_gdd = lm (actual_wheat_ash~met_gdd, data=Indigo_Grain) #Growing degree days
summary(actual_wheat_ash_met_gdd) #-0.0001523  0.0004464  -0.341    0.733
actual_wheat_ash_met_max_rh = lm (actual_wheat_ash~met_max_rh, data=Indigo_Grain) #Max relative humidity
summary(actual_wheat_ash_met_max_rh) #2.431e-04  5.726e-05   4.246 3.39e-05 ***
actual_wheat_ash_met_max_t = lm (actual_wheat_ash~met_max_t, data=Indigo_Grain) #Maximum temp
summary(actual_wheat_ash_met_max_t) #-0.0003151  0.0001603  -1.966   0.0508 .
actual_wheat_ash_met_max_vpd = lm (actual_wheat_ash~met_max_vpd, data=Indigo_Grain) #Maximun vapor pressure deficit
summary(actual_wheat_ash_met_max_vpd) #-0.0072459  0.0015434  -4.695 5.09e-06 ***
actual_wheat_ash_met_min_rh = lm (actual_wheat_ash~met_min_rh, data=Indigo_Grain) #Minimum relative humidity
summary(actual_wheat_ash_met_min_rh) #1.350e-04  3.671e-05   3.676 0.000307 ***
actual_wheat_ash_met_min_t = lm (actual_wheat_ash~met_min_t, data=Indigo_Grain) #Minimum temperature
summary(actual_wheat_ash_met_min_t) #0.0002595  0.0001574   1.648    0.101
actual_wheat_ash_met_p_mm = lm (actual_wheat_ash~met_p_mm, data=Indigo_Grain) #Observed precipitation 
summary(actual_wheat_ash_met_p_mm) #0.0013855  0.0003644   3.802 0.000193 ***
actual_wheat_ash_met_sh = lm (actual_wheat_ash~met_sh, data=Indigo_Grain) #Observed specific humidity
summary(actual_wheat_ash_met_sh) #1.528802   0.481786   3.173  0.00176 **
actual_wheat_ash_met_sr_wm2 = lm (actual_wheat_ash~met_sr_wm2, data=Indigo_Grain) #Observed downwelling shortwave radiation 
summary(actual_wheat_ash_met_sr_wm2) #-7.634e-05  2.428e-05  -3.144  0.00193 **

#Using reduced model previously determined
actual_wheat_ash_colin = lm (actual_wheat_ash~ met_avg_t + met_max_rh + met_p_mm + met_sr_wm2, data = Indigo_Grain)
actual_wheat_ash_colin = lm (actual_wheat_ash~ met_avg_t + met_max_rh + met_p_mm + met_sr_wm2 + Year, data = Indigo_Grain) #With year
summary(actual_wheat_ash_colin)
tab_model(actual_wheat_ash_colin)
vif(actual_wheat_ash_colin)

#falling_no percentage
falling_no_met_avg_t = lm (falling_no~met_avg_t, data=Indigo_Grain) #Average temp
summary(falling_no_met_avg_t) #3.127      1.310   2.386    0.018 *
falling_no_met_gdd = lm (falling_no~met_gdd, data=Indigo_Grain) #Growing degree days
summary(falling_no_met_gdd) #6.178      3.012   2.051   0.0416 *
falling_no_met_max_rh = lm (falling_no~met_max_rh, data=Indigo_Grain) #Max relative humidity
summary(falling_no_met_max_rh) #2.0646     0.3801   5.431 1.69e-07 ***
falling_no_met_max_t = lm (falling_no~met_max_t, data=Indigo_Grain) #Maximum temp
summary(falling_no_met_max_t) #0.9866     1.1018   0.895    0.372
falling_no_met_max_vpd = lm (falling_no~met_max_vpd, data=Indigo_Grain) #Maximun vapor pressure deficit
summary(falling_no_met_max_vpd) #-55.646     10.360  -5.371 2.26e-07 ***
falling_no_met_min_rh = lm (falling_no~met_min_rh, data=Indigo_Grain) #Minimum relative humidity
summary(falling_no_met_min_rh) #0.7370     0.2535   2.907  0.00408 **
falling_no_met_min_t = lm (falling_no~met_min_t, data=Indigo_Grain) #Minimum temperature
summary(falling_no_met_min_t) #3.187      1.056   3.018  0.00289 **
falling_no_met_p_mm = lm (falling_no~met_p_mm, data=Indigo_Grain) #Observed precipitation 
summary(falling_no_met_p_mm) #8.373      2.505   3.343 0.000999 ***
falling_no_met_sh = lm (falling_no~met_sh, data=Indigo_Grain) #Observed specific humidity
summary(falling_no_met_sh) #13473.09    3226.66   4.176 4.52e-05 ***
falling_no_met_sr_wm2 = lm (falling_no~met_sr_wm2, data=Indigo_Grain) #Observed downwelling shortwave radiation 
summary(falling_no_met_sr_wm2) #-0.2414     0.1689  -1.429    0.154

#Using reduced model previously determined
falling_no_colin = lm (falling_no~ met_avg_t + met_max_rh + met_p_mm + met_sr_wm2, data = Indigo_Grain)
falling_no_colin = lm (falling_no~ met_avg_t + met_max_rh + met_p_mm + met_sr_wm2 + Year, data = Indigo_Grain) #With year
summary(falling_no_colin)
tab_model(falling_no_colin)
vif(falling_no_colin) 

#protein_12 percentage
protein_12_met_avg_t = lm (protein_12~met_avg_t, data=Indigo_Grain) #Average temp
summary(protein_12_met_avg_t) #-0.43094    0.07086  -6.082 6.36e-09 ***
protein_12_met_gdd = lm (protein_12~met_gdd, data=Indigo_Grain) #Growing degree days
summary(protein_12_met_gdd) #-0.8599     0.1660   -5.18 5.61e-07 ***
protein_12_met_max_rh = lm (protein_12~met_max_rh, data=Indigo_Grain) #Max relative humidity
summary(protein_12_met_max_rh) #-0.18156    0.01982  -9.161   <2e-16 ***
protein_12_met_max_t = lm (protein_12~met_max_t, data=Indigo_Grain) #Maximum temp
summary(protein_12_met_max_t) #-0.12013    0.06369  -1.886   0.0608 .
protein_12_met_max_vpd = lm (protein_12~met_max_vpd, data=Indigo_Grain) #Maximun vapor pressure deficit
summary(protein_12_met_max_vpd) #5.0875     0.5322    9.56   <2e-16 ***
protein_12_met_min_rh = lm (protein_12~met_min_rh, data=Indigo_Grain) #Minimum relative humidity
summary(protein_12_met_min_rh) #-0.09546    0.01341   -7.12 2.13e-11 ***
protein_12_met_min_t = lm (protein_12~met_min_t, data=Indigo_Grain) #Minimum temperature
summary(protein_12_met_min_t) #-0.45436    0.05366  -8.467 6.59e-15 ***
protein_12_met_p_mm = lm (protein_12~met_p_mm, data=Indigo_Grain) #Observed precipitation 
summary(protein_12_met_p_mm) #-1.0242     0.1305  -7.851 2.89e-13 ***
protein_12_met_sh = lm (protein_12~met_sh, data=Indigo_Grain) #Observed specific humidity
summary(protein_12_met_sh) #-1736.469    150.736  -11.52   <2e-16 ***
protein_12_met_sr_wm2 = lm (protein_12~met_sr_wm2, data=Indigo_Grain) #Observed downwelling shortwave radiation 
summary(protein_12_met_sr_wm2) #0.034860   0.009557   3.648 0.000341 ***

#Using reduced model previously determined
protein_12_colin = lm (protein_12~ met_avg_t + met_max_rh + met_p_mm + met_sr_wm2, data = Indigo_Grain)
protein_12_colin = lm (protein_12~ met_avg_t + met_max_rh + met_p_mm + met_sr_wm2 + Year, data = Indigo_Grain) #With year
summary(protein_12_colin)
tab_model(protein_12_colin)
vif(protein_12_colin) 

#Example visualization of single variable linear correlation (strongest for each grain variable)
ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title= paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                      " Slope =",signif(fit$coef[[2]], 3)
    ))
}
ggplotRegression(moisture_met_max_t)
ggplotRegression(actual_wheat_ash_met_max_vpd)
ggplotRegression(falling_no_met_max_rh)
ggplotRegression(protein_12_met_sh)

###################################
#SPATIAL VISUALIZATION & STATS
#Add latitude and longitude to File
#Import ZIP and lat/lon file
zip_lat_lon = read.csv("ZIP_Lat_Lon.csv")
#Merge files to get lat/lon on main file
Indigo_Space <- merge(Indigo_Grain,zip_lat_lon,by="ZIP")

#Plot locations of zip codes with data points
plot(Indigo_Space$LNG, Indigo_Space$LAT, xlab = "Longitude", ylab = "Latitude", main = expression(paste("Figure 3: Zip Codes with Grain and Weather Data")), pch = 16, col =2)
map("county", add = T, lty = 3, col = 4)
map("state", add = T, lwd = 2)
legend("bottomright", lty = c(1, 3), lwd = 2, col = c(1, 4), legend = c("State boundary", "County boundary"), bg = "white", cex=0.75)

#Plot an average of kernel weight by county to visually see range (can do with other variables as well)
Spoint= SpatialPoints(coord= Indigo_Space[, c("LNG", "LAT")], bbox = NULL)
summary (Spoint)
bbox (Spoint)
SData = SpatialPointsDataFrame(Spoint, data = Indigo_Space[, c(1, 4)])
str(SData)
spplot(SData, zcol = "kernel_weight", col.regions = matlab.like(500), colorkey = T, xlab = "Longitude",ylab = "Latitude", main = expression(paste("Figure 4: Kernel Weight")))
state.map = map("state", fill = T,plot = F)
state.map = map2SpatialPolygons(state.map, ID = state.map$names)
spplot(SData, zcol = "kernel_weight", cex = 1.5, key.space = list(x = 1, y = 0.95, corner = c(0,1)
                                                          , scales = list(draw = T)), colorkey = T, col.regions =                                                     
         matlab.like(500), xlab = "Longitude", ylab = "Latitude"
       , main = expression(paste("Figure 4: Kernel Weight Range"))) +
  latticeExtra::layer(sp.polygons(state.map, lwd = 2, col = "grey"))

#Test for spatial autocorrelation of kernel weight
dists <- as.matrix(dist(cbind(Indigo_Space$LNG, Indigo_Space$LAT))) #make matrix of distances between kernel weight data points
dists.bin <- (dists > 0 & dists <= .75) #Saying that distance of 0.75 is break point to if they are related or not
Moran.I(Indigo_Space$kernel_weight, dists.bin) #Morans I to test for autocorrelation, p-value=0.0003

#Maps and Moran's I for all other grain variables (see code for kernel weight above for notes)
#####Moisture
SData = SpatialPointsDataFrame(Spoint, data = Indigo_Space[, c(1, 3)])
str(SData)
spplot(SData, zcol = "moisture", col.regions = matlab.like(500), colorkey = T, xlab = "Longitude",ylab = "Latitude", main = expression(paste("Moisture")))
state.map = map("state", fill = T,plot = F)
state.map = map2SpatialPolygons(state.map, ID = state.map$names)
spplot(SData, zcol = "moisture", cex = 1.5, key.space = list(x = 1, y = 0.95, corner = c(0,1)
                                                                  , scales = list(draw = T)), colorkey = T, col.regions =                                                     
         matlab.like(500), xlab = "Longitude", ylab = "Latitude"
       , main = expression(paste("Moisture Weight Range"))) +
  latticeExtra::layer(sp.polygons(state.map, lwd = 2, col = "grey"))

Moran.I(Indigo_Space$moisture, dists.bin) 

######Actual Wheat Ash
SData = SpatialPointsDataFrame(Spoint, data = Indigo_Space[, c(1, 5)])
str(SData)
spplot(SData, zcol = "actual_wheat_ash", col.regions = matlab.like(500), colorkey = T, xlab = "Longitude",ylab = "Latitude", main = expression(paste("Actual Wheat Ash")))
state.map = map("state", fill = T,plot = F)
state.map = map2SpatialPolygons(state.map, ID = state.map$names)
spplot(SData, zcol = "actual_wheat_ash", cex = 1.5, key.space = list(x = 1, y = 0.95, corner = c(0,1)
                                                                  , scales = list(draw = T)), colorkey = T, col.regions =                                                     
         matlab.like(500), xlab = "Longitude", ylab = "Latitude"
       , main = expression(paste("Actual Wheat Ash Weight Range"))) +
  latticeExtra::layer(sp.polygons(state.map, lwd = 2, col = "grey"))

Moran.I(Indigo_Space$actual_wheat_ash, dists.bin) 

######Falling No
SData = SpatialPointsDataFrame(Spoint, data = Indigo_Space[, c(1, 6)])
str(SData)
spplot(SData, zcol = "falling_no", col.regions = matlab.like(500), colorkey = T, xlab = "Longitude",ylab = "Latitude", main = expression(paste("Falling No")))
state.map = map("state", fill = T,plot = F)
state.map = map2SpatialPolygons(state.map, ID = state.map$names)
spplot(SData, zcol = "falling_no", cex = 1.5, key.space = list(x = 1, y = 0.95, corner = c(0,1)
                                                                  , scales = list(draw = T)), colorkey = T, col.regions =                                                     
         matlab.like(500), xlab = "Longitude", ylab = "Latitude"
       , main = expression(paste("Falling No Weight Range"))) +
  latticeExtra::layer(sp.polygons(state.map, lwd = 2, col = "grey"))

Moran.I(Indigo_Space$falling_no, dists.bin) #Morans I to test for autocorrelation, p-value=0.01


#####Protein 12
SData = SpatialPointsDataFrame(Spoint, data = Indigo_Space[, c(1, 7)])
str(SData)
spplot(SData, zcol = "protein_12", col.regions = matlab.like(500), colorkey = T, xlab = "Longitude",ylab = "Latitude", main = expression(paste("Protein 12")))
state.map = map("state", fill = T,plot = F)
state.map = map2SpatialPolygons(state.map, ID = state.map$names)
spplot(SData, zcol = "protein_12", cex = 1.5, key.space = list(x = 1, y = 0.95, corner = c(0,1)
                                                                  , scales = list(draw = T)), colorkey = T, col.regions =                                                     
         matlab.like(500), xlab = "Longitude", ylab = "Latitude"
       , main = expression(paste("Protein 12 Weight Range"))) +
  latticeExtra::layer(sp.polygons(state.map, lwd = 2, col = "grey"))

Moran.I(Indigo_Space$protein_12, dists.bin)

