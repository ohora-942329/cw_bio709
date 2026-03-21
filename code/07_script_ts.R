#' DESCRIPTION:
#' Script for time-series

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               forecast,
               lterdatasampler,
               daymetr,
               glarma)

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_ts_anormaly.csv"
(df_ts <- read_csv(url))
url

## draw a figure; Plot time-series anomalies

df_ts %>% 
  ggplot(aes(x = year,        
             y = anormaly)) +  
  geom_line() +                  
  geom_point() +                 
  theme_bw() +                   
  labs(
    x = "Year",                    
    y = "Anomaly"                   
  )

#Regression : Fit a simple linear model: anomaly as a function of year
m_lm <- lm(anormaly ~ year, data = df_ts)

#summary
summary(m_lm)

##figure 

df_ts %>% 
  ggplot(aes(x = year, 
             y = anormaly)) +
  geom_line(linetype = "dotted") +     
  geom_point(alpha = 0.25) +          
  geom_abline(intercept = coef(m_lm)[1],  
              slope = coef(m_lm)[2]) +
  theme_bw()
              
y<-NULL
y[1]<-0
for (i in 1:99){
  y[i +1] <-y[i] + rnorm(1, mean= 0, sd = 1)
}

tibble(x= 1,
       y= y: length(y)) %>% 
  ggplot(aes(x=x,
             y=y))+
  geom_point()+
  geom_abline()

# Create a tibble (modern data frame) from the LakeHuron time series
df_huron <- tibble(
  year = time(LakeHuron),                
  water_level = as.numeric(LakeHuron)    
) %>% 
  arrange(year)                           

# visualize

df_huron %>% 
  ggplot(aes(x = year, y = water_level)) +
  geom_point(alpha = 0.25) +       
  geom_line(linetype = "dotted") + 
  geom_smooth(method = "lm",      
              color = "black",
              linewidth = 0.5) +
  theme_bw() +
  labs(x = "Year", y = "Water Level")

## autoregressive model

(m_ar1 <-Arima(
  df_huron$water_level,
  order= c(1, 0, 0)# it must be three elements 
))

## fitted values 

df_huron_ar1 <- df_huron %>% 
  mutate(fit = fitted(m_ar1) %>%
           as.numeric())

# Plot observed and fitted values

df_huron_ar1 %>% 
  ggplot() +
  geom_point(aes(x = year, 
                 y = water_level),
             alpha = 0.25) +        
  geom_line(aes(x = year, 
                y = fit),           
            color = "steelblue") +
  theme_bw()

##Moving average model 
(m_ma1 <- Arima(
  df_huron$water_level,      
  order = c(0, 0, 1)          
))

## ARMA MODEL
(m_arma1 <- Arima(
  df_huron$water_level,      
  order = c(1, 0, 1)          
))

## ARIMA model

m_arima1 <- Arima(df_huron$water_level,
                  order = c(1, 1, 0))
##model selection 
auto.arima(
  df_huron$water_level, 
  stepwise = FALSE,  # if we put true it may take more time and except no much difference 
  ic = "aic" 
)

##ARIMAX model

data("ntl_icecover") 
ntl_icecover

df_ice<- ntl_icecover %>% 
  as_tibble() %>% 
  filter(between(year, 1980, 2014), 
         lakeid == "Lake Mendota") %>% 
  arrange(year)

## Download daily climate data from Daymet for Lake Mendota 
list_mendota <- download_daymet(
  site = "Lake_Mendota",   # Arbitrary name you assign to this site
  lat = 43.1,              # Latitude of the lake
  lon = -89.4,             # Longitude of the lake
  start = 1980,            # Start year
  end = 2024,              # End year
  internal = TRUE          # Return the data as an R object rather than saving to disk
)

df_temp <- list_mendota$data %>% 
  as_tibble() %>%                  
  janitor::clean_names() %>%       
  mutate(
    date = as.Date(paste(year, yday, sep = "-"), format = "%Y-%j"),
    month = month(date)
  ) %>% 
  arrange(year, yday) %>% 
  group_by(year) %>% # Group by year
  summarize(temp_min = round(mean(tmin_deg_c), 2)) 
df_temp

df_ice <- df_ice %>% 
  left_join(df_temp, by = "year")

#don't 
#lm( as they are not independent data 

#Do 
obj_arima <- auto.arima(
  df_ice$ice_duration,
  xreg = df_ice$temp_min, 
  stepwise = FALSE 
)

confint(obj_arima, level = 0.95)

df_ice %>% 
  ggplot() +
  geom_point(aes(x = year, y = ice_duration), alpha = 0.25)





# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: Bison Body Mass, Climate, and Time-Series Analysis
# ============================================================

library(lterdatasampler)

# The "knz_bison" dataset contains long-term monitoring data
# on bison captured at Konza Prairie Biological Station.
#
# ------------------------------------------------------------
# Key columns may include:
# rec_year      : Year of capture
# animal_sex    : Sex of the individual (e.g., female, male)
# animal_weight : Body mass of bison
# ------------------------------------------------------------
#
# In this exercise, you will explore long-term trends in bison
# body mass and evaluate how climate variability may influence
# weight dynamics over time.
# 1. Explore the structure of the knz_bison dataset.
#    - Inspect variable types and missing values.
#    - Reformat variables as needed for analysis.

data(knz_bison)

# Structure
str(knz_bison)

# Summary statistics
summary(knz_bison)

# Missing values
colSums(is.na(knz_bison))

# Reformat variables
knz_bison <- knz_bison %>%
  mutate(
    rec_year = as.numeric(rec_year),
    animal_sex = as.factor(animal_sex),
    animal_weight = as.numeric(animal_weight)
  )

# 2. Subset the data to include observations from 1994–2012.

bison_sub <- knz_bison %>%
  filter(rec_year >= 1994 & rec_year <= 2012)

# 3. Calculate the average body mass for female and male bison
#    for each year in the selected time period.

bison_summary <- bison_sub %>%
  group_by(rec_year, animal_sex) %>%
  summarise(mean_weight = mean(animal_weight, na.rm = TRUE),
            .groups = "drop")
bison_summary


# Convert to time-series tibble

df_bison <- bison_summary %>% 
  rename(year = rec_year,
         weight = mean_weight) %>% 
  arrange(animal_sex, year)

# Visualization 

df_bison %>% 
  ggplot(aes(x = year, 
             y = weight,
             color = animal_sex)) +
  geom_line(linetype = "dotted") +     
  geom_point(alpha = 0.25) +          
  theme_bw() +
  labs(x = "Year", y = "Mean Body Mass")

# 4. Obtain climate data from the daymetr dataset.
#    - Identify relevant climate variables (e.g., temperature,
#      precipitation).
#    - Associate climate data with knz_bison by year.
#    - Coordinates: Lat 39.09300	Lon -96.57500

climate_data <- download_daymet(
  site = "konza",
  lat = 39.09300,
  lon = -96.57500,
  start = 1994,
  end = 2020,
  internal = TRUE
) %>% 
  {.[["data"]]} %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  mutate(
    date = as.Date(paste(year, yday, sep = "-"),
                   format = "%Y-%j")
  ) %>% 
  
  relocate(date)

climate_df <- climate_data %>%
  group_by(year) %>% 
  summarize(cprcp = sum(prcp_mm_day))


# Merged 

bison_climate <- bison_summary %>%
  left_join(climate_df, by = c("rec_year" = "year"))

df_bison_climate <- bison_climate %>%
  rename(
    year = rec_year,
    weight = mean_weight
  )
  
# plot weight vs climate

ggplot(df_bison_climate,
       aes(x = year, y = weight, color = animal_sex)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(y = "Mean Body Mass", x = "Year")


# 5. Perform a time-series analysis to examine whether selected
#    climate variables influence annual bison body mass.
#    - Consider temporal autocorrelation and lag effects.
#    - Model males and females separately


# ---------------------------
m_male <- df_bison_climate %>% 
  filter(animal_sex == "M") %>% 
  arrange(year) %>% 
  { auto.arima(
    y = .$weight,
    xreg = .$cprcp,
    stepwise = FALSE,
    d = 0
  )
  }

confint.default(m_male)

## with lag effect included 


m_female <- df_bison_climate %>% 
  filter(animal_sex == "F") %>% 
  arrange(year) %>% 
  { auto.arima(
    y = .$weight,
    xreg = .$cprcp,
    stepwise = FALSE,
    d = 0
  )
  }

confint.default(m_female)


# 6. Using your fitted model, compare observed bison body mass
#    with predicted values for the period 2014–2020.
#    - Evaluate model performance and discuss sources of uncertainty.



##Predictions



##Compare observed vs predicted
