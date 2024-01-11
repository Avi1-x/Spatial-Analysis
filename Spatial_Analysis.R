#-----------------------------------
# Loading all necessary libraries
#-----------------------------------

library(sf)
library(tidyverse)
library(spdep)
library(spatialreg)
library(tmap)
library(car)
library(datawizard)

#-----------------------------------
# Data loading and pre-processing
#-----------------------------------

# Loading in IMD 2019 spatial data and processing it by standardising  
# column names and selecting relevant variables.

imd_2019 <- read_sf("IMD_2019.shp") %>% st_make_valid() %>%
  mutate(LSOA = lsoa11cd,
         LocalName = LADnm) %>%
  select(LSOA, LocalName, IncScore, EmpScore, EduScore,
         HDDScore, CriScore, EnvScore)

# Checking for missing values in the dataset

any(is.na(imd_2019))

# Creating list of London Boroughs

london_boroughs <- c("Barking and Dagenham", "Barnet", "Bexley",
                     "Brent", "Bromley", "Camden", "City of London", "Croydon",
                     "Ealing", "Enfield", "Greenwich", "Hackney", 
                     "Hammersmith and Fulham", "Haringey", "Harrow", "Havering",
                     "Hillingdon", "Hounslow", "Islington", 
                     "Kensington and Chelsea", "Kingston upon Thames",
                     "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge",
                     "Richmond upon Thames", "Southwark", "Sutton", 
                     "Tower Hamlets", "Waltham Forest", 
                     "Wandsworth", "Westminster")

# Filtering data for London based on boroughs

london_2019 <- imd_2019 %>% filter(LocalName %in% london_boroughs)

# Selecting Birmingham

birmingham_2019 <- imd_2019 %>% filter(LocalName == "Birmingham")

# Selecting Manchester

manchester_2019 <- imd_2019 %>% filter(LocalName == "Manchester")

#-----------------------------------
# Exploratory analysis
#-----------------------------------

# Gathering descriptive statistics of dependent variable
#------------------------------------------------------------

summary(london_2019$CriScore)

summary(birmingham_2019$CriScore)

summary(manchester_2019$CriScore)

# Calculating all correlation matrices
#--------------------------------------

london_cor_matrix <- cor(select(st_drop_geometry(london_2019),
                                IncScore, EmpScore, EduScore, HDDScore, 
                                CriScore, EnvScore))

birmingham_cor_matrix <- cor(select(st_drop_geometry(birmingham_2019), 
                                    IncScore, EmpScore, EduScore, HDDScore, 
                                    CriScore, EnvScore))

manchester_cor_matrix <- cor(select(st_drop_geometry(manchester_2019), 
                                    IncScore, EmpScore, EduScore, HDDScore, 
                                    CriScore, EnvScore))

# Testing for multicollinearity
#--------------------------------

# Constructing linear regression model

london_lm_model <- lm(CriScore ~ IncScore + EmpScore + EduScore +
                        HDDScore + EnvScore,
                      london_2019)

# Calculating VIF values 

vif(london_lm_model) 

# Repeating the process for the Birmingham data

birmingham_lm_model <- lm(CriScore ~ IncScore + EmpScore + EduScore +
                            HDDScore + EnvScore,
                          birmingham_2019)

vif(birmingham_lm_model) 

# Repeating the process for the Manchester data

manchester_lm_model <- lm(CriScore ~ IncScore + EmpScore + EduScore +
                             HDDScore + EnvScore,
                           manchester_2019)

vif(manchester_lm_model) 

#-----------------------------------
# Spatial analysis
#-----------------------------------

# Conducting a hot spot analysis of crime scores
#---------------------------------------------------------------

# Creating a spatial weights matrix for the London data

london_2019_lw <- poly2nb(london_2019, queen = TRUE) %>% nb2listw()

# Computing Getis-Ord Gi* statistic for the London data

london_2019$GI_Values <- localG(london_2019$CriScore, london_2019_lw)

# Visualising the distribution of Gi* values on the map of London. 

# In order to improve model interpretability in London, the Gi* values are  
# overlaid on the boroughs of London instead of the lower super-output areas.
# As such, an empty map of the London boroughs are first created.

london_boroughs_grouped <- london_2019 %>% 
  group_by(LocalName) %>%
  summarise(geometry = st_union(geometry))

london_map_boroughs <- tm_shape(london_boroughs_grouped) +
  tm_borders()

# Then the Gi* values are overlaid onto the previously created map

london_map_gi_2019 <- tm_shape(london_2019) +
  tm_fill(col = "GI_Values",
          title = "Gi* Value", 
          midpoint = NA, 
          palette = "-RdYlBu", 
          breaks = c(-8, -6, -4, -2, -1, 0, 1, 2, 4, 6, 8)) +
  tm_layout(legend.position = c("left", "bottom"), 
            main.title = "Crime Hotspots in London",
            title.position = c("left", "top"), 
            frame = FALSE, 
            main.title.fontface = "bold.italic",
            fontfamily = "serif") +
  tm_credits("Data Source: CDRC English\nIndex of Multiple Deprivation",
             size = 0.8)	+
  tm_compass(type = "8star",
             position = c("right", "top")) +
  london_map_boroughs

# Finally, the map is saved

tmap_save(london_map_gi_2019, 
          filename = "/Users/Avi/Downloads/Coursework/INF6027/London_Hotspots.png", 
          width = 8, 
          height = 6, 
          dpi = 300)

# Repeating the process for the Birmingham data

birmingham_2019_lw <- poly2nb(birmingham_2019, queen = TRUE) %>% nb2listw()

birmingham_2019$GI_Values <- localG(birmingham_2019$CriScore, 
                                    birmingham_2019_lw)

# The Getis-Ord Gi* values for Birmingham and Manchester were 
# visualised onto the original lower super-output areas.
# Apart from that, the same process remained the same

birmingham_map_gi_2019 <- tm_shape(birmingham_2019) +
  tm_borders() +
  tm_fill(col = "GI_Values", 
          title = "Gi* Value", 
          midpoint = NA, 
          palette = "-RdYlBu", 
          breaks = c(-8, -6, -4, -2, -1, 0, 1, 2, 4, 6)) +
  tm_layout(legend.position = c("left", "bottom"), 
            main.title = "Crime Hotspots in Birmingham",
            title.position = c("left", "top"), 
            frame = FALSE, 
            asp = 0, 
            main.title.fontface = "bold.italic", 
            fontfamily = "serif") +
  tm_credits("Data Source: CDRC English\nIndex of Multiple Deprivation", 
             size = 0.8)	+
  tm_compass(type = "8star", 
             position = c("right", "top"))

tmap_save(birmingham_map_gi_2019, 
          filename = "/Users/Avi/Downloads/Coursework/INF6027/Birmingham_Hotspots.png", 
          width = 8, 
          height = 6, 
          dpi = 300)

# Repeating the process for the Manchester data

manchester_2019_lw <- poly2nb(manchester_2019, queen = TRUE) %>% nb2listw()

manchester_2019$GI_Values <- localG(manchester_2019$CriScore, 
                                    manchester_2019_lw)

manchester_map_gi_2019 <- tm_shape(manchester_2019) +
  tm_borders() +
  tm_fill(col = "GI_Values", 
          title = "Gi* Value", 
          midpoint = NA,
          palette = "-RdYlBu", 
          breaks = c(-4, -2, -1, 0, 1, 2, 4, 6)) +
  tm_layout(legend.position = c("left", "bottom"), 
            main.title = "Crime Hotspots in Manchester",
            title.position = c("left", "top"), 
            frame = FALSE, asp = 0,
            main.title.fontface = "bold.italic", 
            fontfamily = "serif") +
  tm_credits("Data Source: CDRC English\nIndex of Multiple Deprivation", 
             size = 0.8)	+
  tm_compass(type = "8star", 
             position = c("right", "top"))

tmap_save(manchester_map_gi_2019, 
          filename = "/Users/Avi/Downloads/Coursework/INF6027/Manchester_Hotspots.png", 
          width = 8, 
          height = 6, 
          dpi = 300)

# Checking for global spatial autocorrelation 
#---------------------------------------------

# Computing Moran's I statistics for the London data

moran.test(london_2019$CriScore, listw = london_2019_lw)

# Repeating the same test for the Birmingham data

moran.test(birmingham_2019$CriScore, listw = birmingham_2019_lw)

# Repeating the same test for the Manchester data

moran.test(manchester_2019$CriScore, listw = manchester_2019_lw)

# Outlier handling
#------------------

# Creating a list of variables of interest

variables_of_interest <- c("IncScore", "EmpScore", "EduScore", "HDDScore",
                           "CriScore", "EnvScore")

# Managing outliers through winsorization of extreme values

for (var in variables_of_interest) {
  london_2019[[var]] <- datawizard::winsorize(london_2019[[var]], 
                                              method = "zscore", 
                                              threshold = 3)
  }

# Repeating the same process for the Birmingham data

for (var in variables_of_interest) {
  birmingham_2019[[var]] <- datawizard::winsorize(birmingham_2019[[var]], 
                                                  method = "zscore", 
                                                  threshold = 3)
  }

# Repeating the same process for the Manchester data

for (var in variables_of_interest) {
  manchester_2019[[var]] <- datawizard::winsorize(manchester_2019[[var]], 
                                                  method = "zscore", 
                                                  threshold = 3)
  }

# Standardizing all variables
#-----------------------------

# Transforming London data using z-score transformation 
# This is done to improve interpretability by ensuring that all variables
# are on the same scale

for (col in variables_of_interest) {
  london_2019[[paste0(col, "_Standardized")]] <- scale(london_2019[[col]])
  }  

# Repeating the same process for the Birmingham data

for (col in variables_of_interest) {
  birmingham_2019[[paste0(col, "_Standardized")]] <- scale(birmingham_2019[[col]])
  }  

# Repeating the same process for the Manchester data

for (col in variables_of_interest) {
  manchester_2019[[paste0(col, "_Standardized")]] <- scale(manchester_2019[[col]])
  } 

# Principal component analysis of variables with multicollinearity 
#------------------------------------------------------------------

# Extracting the variables of interest

london_pca_vars <- london_2019 %>% 
  st_drop_geometry() %>% 
  select(IncScore_Standardized, EmpScore_Standardized)

# Calculating principal components

london_pca_result <- princomp(london_pca_vars)

# Examining the results of the PCA

summary(london_pca_result)

# Extracting the principal component

london_pca_component <- predict(london_pca_result)[, "Comp.1"]

# Combining it with the original dataset as a new composite variable

london_2019 <- london_2019 %>% 
  mutate(IECScore_Standardized = london_pca_component)

# Repeating the same process for the Birmingham data

birmingham_pca_vars <- birmingham_2019 %>% 
  st_drop_geometry() %>% 
  select(IncScore_Standardized, EmpScore_Standardized)

birmingham_pca_result <- princomp(birmingham_pca_vars)

summary(birmingham_pca_result)

birmingham_pca_component <- predict(birmingham_pca_result)[, "Comp.1"]

birmingham_2019 <- birmingham_2019 %>% 
  mutate(IECScore_Standardized = birmingham_pca_component)

# Repeating the same process for the Manchester data

manchester_pca_vars <- manchester_2019 %>% 
  st_drop_geometry() %>% 
  select(IncScore_Standardized, EmpScore_Standardized)

manchester_pca_result <- princomp(manchester_pca_vars)

summary(manchester_pca_result)

manchester_pca_component <- predict(manchester_pca_result)[, "Comp.1"]

manchester_2019 <- manchester_2019 %>% 
  mutate(IECScore_Standardized = manchester_pca_component)

# Spatial modelling
#-------------------

# Creating a spatial lag model for the London data 
# using the standardized variables and the composite variable

london_pca_lag_model <- lagsarlm(CriScore_Standardized ~ 
                                   IECScore_Standardized + 
                                   EduScore_Standardized +
                                   HDDScore_Standardized + 
                                   EnvScore_Standardized, 
                                 data = london_2019,
                                 listw = london_2019_lw)

# Retrieving the summary of the model

summary(london_pca_lag_model)

# Repeating the same process for the Birmingham data

birmingham_pca_lag_model <- lagsarlm(CriScore_Standardized ~ 
                                       IECScore_Standardized + 
                                       EduScore_Standardized +
                                       HDDScore_Standardized + 
                                       EnvScore_Standardized,
                                     data = birmingham_2019,
                                     listw = birmingham_2019_lw)

summary(birmingham_pca_lag_model)

# Repeating the same process for the Manchester data

manchester_pca_lag_model <- lagsarlm(CriScore_Standardized ~ 
                                       IECScore_Standardized + 
                                       EduScore_Standardized +
                                       HDDScore_Standardized + 
                                       EnvScore_Standardized,
                                     data = manchester_2019,
                                     listw = manchester_2019_lw)

summary(manchester_pca_lag_model)


# Visualizing the diagnostic plots for the London model
#-------------------------------------------------------

# Opening an empty PNG file

png("London_Model_Diagnostic_Plots.png", 
    width = 1400, 
    height = 550, 
    res = 150)

# Setting the layout of the plots, the font family and the outer margins 
# of the plots

par(mfrow = c(1, 3), 
    family = "Times", 
    oma = c(0, 0, 1.5, 0))

# Creating a plot of Residuals vs Fitted values

plot(resid(london_pca_lag_model) ~ fitted(london_pca_lag_model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot",
     cex.main = 1.5,
     cex.lab = 1.4)

# Creating a Scale-Location Plot

plot(sqrt(abs(resid(london_pca_lag_model))) ~ fitted(london_pca_lag_model),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Residuals",
     main = "Scale-Location Plot",
     cex.main = 1.5,
     cex.lab = 1.4)

# Adding a title to the center of the combined plots
# By positioning this here the title is placed in the center 
# This is because the mtext will be placed above the second image, which in
# this case is above the middle image

mtext("London Model Diagnostic Plots", 
      side = 3, 
      line = 3.5, 
      cex = 1.5, 
      font = 2)

# Creating a normal Q-Q plot

qqnorm(resid(london_pca_lag_model), 
       cex.main = 1.5,
       cex.lab = 1.4,
       main = "Q-Q Plot")

# Adding a line of reference for the Q-Q plot

qqline(resid(london_pca_lag_model))

# Closing the PNG file and saving it

dev.off()

# Repeating the same process for the Birmingham Model

png("Birmingham_Model_Diagnostic_Plots.png", 
    width = 1400, 
    height = 550, 
    res = 150)

par(mfrow = c(1, 3), 
    family = "Times", 
    oma = c(0, 0, 1.5, 0))

plot(resid(birmingham_pca_lag_model) ~ fitted(birmingham_pca_lag_model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot",
     cex.main = 1.5,
     cex.lab = 1.4)

plot(sqrt(abs(resid(birmingham_pca_lag_model))) ~ fitted(birmingham_pca_lag_model),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Residuals",
     main = "Scale-Location Plot",
     cex.main = 1.5,
     cex.lab = 1.4)

mtext("Birmingham Model Diagnostic Plots", 
      side = 3, 
      line = 3.5, 
      cex = 1.5, 
      font = 2)

qqnorm(resid(birmingham_pca_lag_model),
       cex.main = 1.5,
       cex.lab = 1.4,
       main = "Q-Q Plot")

qqline(resid(birmingham_pca_lag_model))

dev.off()

# Repeating the same process for the Manchester Model

png("Manchester_Model_Diagnostic_Plots.png", 
    width = 1400, 
    height = 550, 
    res = 150)

par(mfrow = c(1, 3), 
    family = "Times", 
    oma = c(0, 0, 1.5, 0))

plot(resid(manchester_pca_lag_model) ~ fitted(manchester_pca_lag_model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot",
     cex.main = 1.5,
     cex.lab = 1.4)

plot(sqrt(abs(resid(manchester_pca_lag_model))) ~ fitted(manchester_pca_lag_model),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Residuals",
     main = "Scale-Location Plot",
     cex.main = 1.5,
     cex.lab = 1.4)

mtext("Manchester Model Diagnostic Plots", 
      side = 3, 
      line = 3.5, 
      cex = 1.5, 
      font = 2)

qqnorm(resid(manchester_pca_lag_model),
       cex.main = 1.5,
       cex.lab = 1.4,
       main = "Q-Q Plot")

qqline(resid(manchester_pca_lag_model))

dev.off()
