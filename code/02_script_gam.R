#' DESCRIPTION:
#' Script for GAMs
pacman::p_load(tidyverse,
               ggeffects,
               mgcv)
# in-class ----------------------------------------------------------------
link <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_water_temp.csv"

(df_wt_raw <- read_csv(link))
sapply(df_wt_raw, class)
df_wt<-df_wt_raw %>% 
  mutate(date = as.Date(date_time,
                        format = "%m/%d/%Y"),
         year = year(date),
             month = month(date)
  ) %>% 
  filter(year == 2022,
         between(month, 3, 10))
##daily averages
df_wt_daily <- df_wt %>% 
  group_by(date, site) %>%
  summarize( temp = mean(temp, na.rm = TRUE) %>% round(3)
  )
# Visualize daily water temperature for each wetland site
df_wt_daily %>% 
  ggplot(aes(
    x = date,      
    y = temp,     
    color = site   
  )) +
  geom_point(alpha = 0.25) +
  theme_bw() +
  labs(
    x = "Date",                
    y = "Water Temperature",   
    color = "Wetland Type"     
  )
## convert data types
df_wt_daily <- df_wt_daily %>% 
  mutate(
    j_date = yday(date),
    site = factor(site)
  )
df_wt_daily
# Fit a Generalized Linear Model (GLM) with Gaussian family
m_glm <- glm(
  temp ~ j_date + site,
  data = df_wt_daily,    
  family = "gaussian"
  )

summary(m_glm)

# Generate model predictions across all Julian days and wetland sites
df_pred <- ggpredict(m_glm,
                     terms = c(
                       "j_date [all]",  
                       "site [all]")) %>% 
  rename(site = group,
         j_date = x)

# Plot daily water temperature and overlay model predictions
df_wt_daily %>% 
  ggplot(aes(
    x = j_date,
    y = temp,
    color = site )) +
  geom_point(alpha = 0.25) +
  geom_line(data = df_pred,
            aes(y = predicted)) +
  theme_bw() +
  labs(x = "Julian Date",
       y = "Water Temperature",   
       color = "Wetland Type")
#GAM Application 

m_gam <- gam(temp ~ site + s(j_date),
             data = df_wt_daily,
             family = "gaussian")
summary(m_gam)

df_pred_gam <- ggpredict(m_gam,
                         terms = c(
                           "j_date [all]", 
                           "site [all]")
) %>% 
  rename(site = group,
         j_date = x)

df_wt_daily %>% 
  ggplot(aes(
    x = j_date,
    y = temp, 
    color = site
  )) +
  geom_point(alpha = 0.25) +
  # Overlay predicted values from the GAM
  geom_line(data = df_pred_gam,
            aes(y = predicted)) +
  theme_bw() +
  labs(x = "Julian Date",         # x-axis label
       y = "Water Temperature",   # y-axis label
       color = "Wetland Type"     # Legend title for site color
  )


# lab ---------------------------------------------------------------------

# 1. Read directly from the raw GitHub URL
url <- "https://raw.githubusercontent.com/aterui/public-proj_restore-aqua-complex/v.1.0/data_raw/data_bat.csv"

# Try reading normally

df_bat <- read_csv(url, show_col_types = FALSE) %>% 
  janitor::clean_names()

# ============================================================
# DATA GUIDE: Bat Detector Data
# ============================================================

# ----------------------------
# Raw data columns
# ----------------------------

# Site
#   Location where bat detectors are deployed.
#   Levels:
#     "RECCON"  = prairie site without wetland
#     "RECWET"  = prairie site with constructed wetland
#     "WOODCON" = woody site without wetland
#     "WOODWET" = woody site with constructed wetland

# DATE
#   Calendar date of each bat pass record.
#   Expected format: YYYY-MM-DD (verify and standardize).

# TIME
#   Time of bat pass detection.
#   Expected format: HH:MM:SS (verify and standardize).

# AUTO ID*
#   Automatically identified bat species.
#   Species IDs may contain misclassifications or unknown labels
#   that should be carefully reviewed during data cleaning.

# ============================================================
# GOAL 1: Clean data
# ============================================================

# 1. Format column names
#   - Convert column names to a clean format
df_bat <- df_bat %>%
  rename_with(~ str_replace_all(tolower(.x), "[^a-z0-9]+", "_")) %>%
  rename_with(~ str_replace(.x, "_$", ""))

df_bat

# 2. Examine each column carefully
#   - Check for missing values, inconsistent formats, and typos
#   - Confirm DATE and TIME are properly parsed as date/time objects
#   - Inspect AUTO ID values for NA
#   - Remove or correct invalid or unusable records as needed

# New derived columns to create:
# Site-level categories:
#   Prairie sites: "RECCON", "RECWET"
#   Woody sites:   "WOODCON", "WOODWET"
sapply(df_bat, function(x) sum(is.na(x)))

# 3. habitat_type
#   Broad site classification:
#     "prairie" = RECCON, RECWET
#     "woody"   = WOODCON, WOODWET

df_bat <- df_bat %>%
  mutate(
    habitat_type = case_when(
      site %in% c("RECCON", "RECWET")   ~ "prairie",
      site %in% c("WOODCON", "WOODWET") ~ "woody",
      TRUE ~ NA_character_ 
    ),
    
  
# 4. wetland_status
#   Presence/absence of wetland:
#     "no_wetland" = RECCON, WOODCON
#     "wetland"    = RECWET, WOODWET

# Wetland presence/absence
wetland_status = case_when(
  site %in% c("RECCON", "WOODCON") ~ "no_wetland",
  site %in% c("RECWET", "WOODWET") ~ "wetland",
  TRUE ~ NA_character_  
)
  )


# ============================================================
# GOAL 2: Visualize daily bat activity
# ============================================================

# Objective:
#   Quantify and visualize bat activity as the number of bat passes per day.

# 1. Read directly from the raw GitHub URL
url <- "https://raw.githubusercontent.com/aterui/public-proj_restore-aqua-complex/v.1.0/data_raw/data_bat.csv"

# Try reading normally

df_bat <- read_csv(url, show_col_types = FALSE) %>% 
  janitor::clean_names()

# ============================================================
# DATA GUIDE: Bat Detector Data
# ============================================================

# ----------------------------
# Raw data columns
# ----------------------------

# Site
#   Location where bat detectors are deployed.
#   Levels:
#     "RECCON"  = prairie site without wetland
#     "RECWET"  = prairie site with constructed wetland
#     "WOODCON" = woody site without wetland
#     "WOODWET" = woody site with constructed wetland

# DATE
#   Calendar date of each bat pass record.
#   Expected format: YYYY-MM-DD (verify and standardize).

# TIME
#   Time of bat pass detection.
#   Expected format: HH:MM:SS (verify and standardize).

# AUTO ID*
#   Automatically identified bat species.
#   Species IDs may contain misclassifications or unknown labels
#   that should be carefully reviewed during data cleaning.

# ============================================================
# GOAL 1: Clean data
# ============================================================

# 1. Format column names
#   - Convert column names to a clean format
df_bat <- df_bat %>%
  rename_with(~ str_replace_all(tolower(.x), "[^a-z0-9]+", "_")) %>%
  rename_with(~ str_replace(.x, "_$", ""))

df_bat

# 2. Examine each column carefully
#   - Check for missing values, inconsistent formats, and typos
#   - Confirm DATE and TIME are properly parsed as date/time objects
#   - Inspect AUTO ID values for NA
#   - Remove or correct invalid or unusable records as needed

# New derived columns to create:
# Site-level categories:
#   Prairie sites: "RECCON", "RECWET"
#   Woody sites:   "WOODCON", "WOODWET"
sapply(df_bat, function(x) sum(is.na(x)))

# 3. habitat_type
#   Broad site classification:
#     "prairie" = RECCON, RECWET
#     "woody"   = WOODCON, WOODWET
# 4. wetland_status
#   Presence/absence of wetland:
#     "no_wetland" = RECCON, WOODCON
#     "wetland"    = RECWET, WOODWET

df_bat <- df_bat %>%
  mutate(
    habitat_type = case_when(
      site %in% c("RECCON", "RECWET")   ~ "prairie",
      site %in% c("WOODCON", "WOODWET") ~ "woody",
      TRUE ~ NA_character_
    ),
    wetland_status = case_when(
      site %in% c("RECCON", "WOODCON") ~ "no_wetland",
      site %in% c("RECWET", "WOODWET") ~ "wetland",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# GOAL 2: Visualize daily bat activity
# ============================================================

# Objective:
#   Quantify and visualize bat activity as the number of bat passes per day.

# Steps:
#   - Aggregate data to calculate daily bat passes
#   - Convert DATE to Julian date
#   - Plot number of bat passes as a function of Julian date
#   - Optionally:
#       * Color or facet plots by site
#       * Smooth trends to visualize seasonal patterns
df_n <- df_bat %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>% 
  group_by(date, site, habitat_type, wetland_status) %>%
  summarise(pass = n(), .groups = "drop") %>%
  mutate(
    month = month(date),
    year = year(date),
    j_date = yday(date) 
  ) %>%
  filter(year == 2021)
#plot by habitat type and wetland status
df_n %>% 
  ggplot(aes(x=date,
             y= pass,
             color= wetland_status))+
  geom_point()+
  facet_wrap(facets = ~ habitat_type)+
  theme_bw()

# ============================================================
# GOAL 3: Model differences among sites
# ============================================================

# Objective:
#   Test whether bat activity differs among the four detector sites.
#   Does the presence of wetland affect bat activity?
#   Is the effect of wetland is site-dependent?

# Modeling considerations:
#   - Response variable: daily bat passes
#   - Predictors may include:
#       * habitat_type
#       * wetland_status
#       * site (four-level factor)
#       * Julian date (to account for seasonality)
#   - Consider appropriate count models

# Steps:
#   - Aggregate data to calculate daily bat passes
#   - Convert DATE to Julian date
#   - Plot number of bat passes as a function of Julian date
#   - Optionally:
#       * Color or facet plots by site
#       * Smooth trends to visualize seasonal patterns
mean(df_n$pass)
var(df_n$pass)
m_bat1<- gam(pass~habitat_type + wetland_status + s(j_date),
           family = "nb",
           data = df_n)
summary(m_bat1)
## with interaction
m_bat2<- gam(pass~ habitat_type + wetland_status + habitat_type * wetland_status+ s(j_date),
             family = "nb",
             data = df_n)
summary(m_bat2)
#compare the two 

AIC(m_bat1, m_bat2)