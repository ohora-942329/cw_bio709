# NOTE:
# When instructed to "test XXX", you must report the outcome as comments
# that clearly summarize the relevant statistical results (e.g., effect size,
# direction, significance, and interpretation).
# Providing code alone without documenting and interpreting the results
# in comments will result in point deductions.

# dataset 1 -------------------------------------------------------------

library(tidyverse)

link1 <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_insect_emergence.rds"
df_emg <- readRDS(url(link1, "rb"))

# This dataset ('df_emg') contains daily measurements of aquatic insect emergence
# from two wetland sites over a full calendar year (Jan 1–Dec 31).

# Data structure:
# t           : Day of the year (integer), where 1 = January 1 and 365 = December 31
# site        : Site identifier (factor), with "s1" and "s2" representing the two wetlands
# emergence   : Emergence flux of aquatic insects (g/day)

# Q1. Visualize seasonal patterns in emergence flux at both sites
#     (e.g., plot emergence vs. day of year, with separate lines or colors for each site).
#     [1 point]
plot_emg<-df_emg %>% 
ggplot(
  aes(x = t, 
      y = emergence, 
      color = site)) +
  geom_line(alpha = 0.7) +
  labs(x = "Day of Year", y = "Emergence flux (g/day)") +
  theme_minimal()

plot(plot_emg)

# Q2. Test whether emergence flux differs significantly between the two sites,
#     while appropriately accounting for seasonal variation
#     [4 points]

# Use GAM to model nonlinear seasonal trend

library(mgcv)

mod_emg <- gam(emergence ~ site + 
                 s(t, by = site), 
               data = df_emg)

summary(mod_emg) ## i could say there is significant difference between the two sites 

# dataset 2 ---------------------------------------------------------------

link2 <- "https://raw.githubusercontent.com/aterui/cw_bio709/master/data_fmt/data_lake_invert.rds"
df_inv <- readRDS(url(link2, "rb"))

head(df_inv)

# This dataset 'df_inv' contains 100 observations from 10 lakes.
# Within each lake, 10 plots were established, spaced ~500 m apart.
# At each plot, the following variables were measured:

# s          : Species richness of invertebrates associated with aquatic plants at each plot
# hb         : Standing biomass of invertebrates associated with aquatic plants at each plot
# prod       : Production rate of aquatic plants (macrophytes), measured as g/month
# substrate  : Median diameter of substrate materials (mm)
# cond       : Water electrical conductivity (µS/cm);
#              a proxy for ionized nutrient levels (higher values may indicate eutrophication)
# lake       : lake ID

# Researcher's hypothesis was that: 
# (a) conductivity influences the productivity of macrophyes.
# (b) macrophyte's production rate ('prod') dictates invertebrate biomass ('hb') through bottom-up effects
# (c) macrophyte's production rate ('prod') dictates invertebrate richness ('s') through bottom-up effects 

# Q1. Create a scatter plot of macrophyte production ('prod', y-axis)
#     versus water conductivity ('cond', x-axis), with points colored by lake identity.
#     [1 point]

ggplot(df_inv,
  aes(x = cond, 
      y = prod, 
      color = lake)) +
  geom_point() +
  theme_minimal()

# Q2. Create a scatter plot of raw invertebrate biomass ('hb', y-axis)
#     versus macrophyte production ('prod', x-axis), with points colored by lake identity.
#     [1 point]

ggplot(df_inv,
       aes(x = prod, 
           y = hb, 
           color = lake)) +
  geom_point() +
  theme_minimal()

# Q3. Create a scatter plot of "log-transformed" invertebrate biomass ('hb', y-axis)
#     versus macrophyte production ('prod', x-axis), with points colored by lake identity.
#     [1 point]

ggplot(df_inv, 
       aes(x = prod, 
           y = log(hb), 
           color = lake)) +
  geom_point() +
  theme_minimal()

# Q4. Test hypothesis (a) by modeling macrophyte production while
#     statistically controlling for potential confounding variables ('substrate', 'lake').
#     [3 points]

library(lme4)

mod_prod <- lmer(prod ~ cond + 
                   substrate +
                   (1 | lake), 
                 data = df_inv)

summary(mod_prod)

# Q5. Test hypotheses (a–c) simultaneously using a unified modeling framework.
#     Based on the resulting statistical tests, determine whether the overarching
#     hypothesis (a–c, combined) is supported or rejected.
#     - Use appropriate probability distributions.
#     - Use variable transformation if appropriate given the data.
#     [4 points]
library(lme4)
library(piecewiseSEM)

mod_a <- lmer(prod ~ cond + substrate + (1 | lake), data = df_inv)
mod_b <- lmer(log(hb) ~ prod + (1 | lake), data = df_inv)
mod_c <- lmer(s ~ prod + (1 | lake), data = df_inv)

sem_mod <- psem(mod_a, mod_b, mod_c)
summary(sem_mod)

# the result shows in most cases the path is significant and supported 
# dataset 3 ---------------------------------------------------------------

link3 <- "https://raw.githubusercontent.com/aterui/cw_bio709/master/data_fmt/nutrient.rds"
nutrient <- readRDS(url(link3, "rb"))

print(trees)

# This dataset ('trees') contains measurements of 31 felled black cherry trees.
# The three variables represent tree diameter, height, and timber volume.
# Note: the variable 'Girth' is actually the diameter measured at 4 ft 6 in above ground.

# Data structure:
# Girth   : Numeric, tree diameter in inches (mislabelled as girth)
# Height  : Numeric, tree height in feet
# Volume  : Numeric, timber volume in cubic feet

# Q1. Visualize relationships among tree diameter ('Girth'), height ('Height'),
#     and timber volume ('Volume') (e.g., using scatterplot matrix or pairwise scatter plots).
#     [1 point]

library(tidyverse)
library(GGally)

data(trees)

trees %>%
  as_tibble() %>%
  ggpairs(
    progress = FALSE,
    columns = c("Girth",
                "Height",
                "Volume"),
    aes(alpha = 0.5)
  ) +
  theme_bw()

# Q2. Perform an appropriate ordination or dimension reduction method to 
#     summarize these three variables into fewer composite axes.
#     Then, identify and retain axes that explain meaningful variation in the original variables
#     [3 points]
df_tree <- trees %>% 
  select(Girth, Height, Volume)
pca_trees <- prcomp(
  x = df_tree,   
  center = TRUE,
  scale = TRUE   
)
print(pca_trees)
summary(pca_trees)
pca_trees$rotation



# Q3. If justified, test whether the retained axis (or axes) is significantly 
#     related to "nutrient"; 
#     skip regression if the ordination does not support meaningful interpretation.
#     [1 point]

pc1_scores<- pca_trees$x[, 1]

df_pca <- data.frame(
  pc1 = pc1_scores,
  nutrient = nutrient
)

mod_pc1 <- lm(pc1 ~ nutrient, data = df_pca)
summary(mod_pc1)

# dataset 4 ---------------------------------------------------------------

df_nile <- dplyr::tibble(
  year = time(Nile), # observation year
  discharge = as.numeric(Nile) # discharge
)

df_sunspot <- dplyr::tibble(
  year = time(sunspot.year), # observation year
  sunspots = as.numeric(sunspot.year) # the number of sunspots
)

# These datasets contain:
# - df_nile    : Annual discharge of the Nile River (Nile dataset)
# - df_sunspot : Annual sunspot counts (sunspot.year dataset)

# Q1. Create a combined data frame aligning the observation years
#     (i.e., only include years present in both datasets)
#     [1 point]

library(dplyr)

df_combined <- df_nile %>%
  inner_join(df_sunspot, by = "year") %>%
  mutate(
    year = as.numeric(year),      
    discharge = as.numeric(discharge),
    sunspots = as.numeric(sunspots)
  )
df_combined

# Q2. Test whether the number of sunspots is significantly related to Nile's discharge
#     [4 points]

plot(df_combined$sunspots, 
     df_combined$discharge,
     xlab = "Sunspots",
     ylab = "Nile Discharge",
     main = "Sunspots vs Nile Discharge")
abline(lm(discharge ~ sunspots, data = df_combined), col = "blue")

model <- lm(discharge ~ sunspots, data = df_combined)

#I can use ARIMAX to account for non-independence of time series data and reduce error due to auto correction

library(forecast)

model_arimax <- auto.arima(df_combined$discharge,
                           xreg = df_combined$sunspots)

summary(model_arimax)
#the effect of sunspots is not statistically significant
