#' DESCRIPTION:
#' Script for SEM
pacman::p_load(tidyverse,
               GGally,
               vegan,
               lavaan,
               lavaanPlot)
library(piecewiseSEM)
# in-class ----------------------------------------------------------------
# Specify the URL of the raw CSV file on GitHub
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_foodweb.csv"
#the arrow receiving is the response variable 
# Read the CSV file into a tibble
(df_fw <- read_csv(url))
# visualization 
df_fw %>% 
  select(-plot_id) %>%        
  ggpairs(                    
    progress = FALSE         
  ) +
  theme_bw()
#write path diagram 
m1<-'
  mass_herbiv ~ mass_plant + cv_h_plant
  mass_pred ~ mass_herbiv
'
m1
  
  
  (fit1 <- sem(model = m1,
               data = df_fw))
# is the p-value is greater your <0.5 is not good as the expected value is used 
summary(fit1)
summary(fit1, standardize = TRUE)

lavaanPlot(model = fit1, coefs = TRUE, stand = TRUE)
 
 ##Model comparison
m2<-'
  mass_herbiv ~ mass_plant + cv_h_plant
  mass_pred ~ mass_herbiv + cv_h_plant
'
m2

(fit2 <- sem(model = m2,
             data = df_fw))

lavaanPlot(model = fit2, coefs = TRUE, stand = TRUE)

#model comparison with ANOVA () i.e., likelihood ratio test

anova(fit1, fit2)

#SEM vs. Path analysis

#path  analysis is all variables are measurable but in structural equation modeling have latent variables 

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_herbivory.csv"

(df_herbv <- read_csv(url))
## visualization 

df_herbv %>% 
  ggpairs(
    progress = FALSE,
    columns = c("soil_n",
                "sla",
                "cn_ratio",
                "per_lignin")
  ) +
  theme_bw()

## latent variable AND  regression
m_sem <- '
  palatability =~ sla + cn_ratio + per_lignin #=~ we can use as both response and predictor 
  palatability ~ soil_n
  herbivory ~ palatability
'
m_sem

(fit_sem <- sem(m_sem,
                data = df_herbv))
## summary

summary(fit_sem, standardize = TRUE)

lavaanPlot(model = fit_sem, coefs = TRUE, stand = TRUE)


# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: Path Analysis and Covariance Visualization
# ============================================================
#install.packages("piecewiseSEM")

library(piecewiseSEM)
data("keeley")

# The "keeley" dataset contains fire-related vegetation data
# collected from shrublands in California.
#
# ------------------------------------------------------------
# Column descriptions:
# elev  : Elevation of the site
# slope : Slope steepness
# aspect: Slope aspect (orientation)
# heat  : Heat load index (a function of slope and aspect)
# firesev: Fire severity
# age   : Time since last fire
# cover : Vegetation cover
# rich  : Plant species richness
# ------------------------------------------------------------
#
# In this exercise, you will explore relationships among variables
# using covariance and path analysis. You will replicate a published
# path model and propose an alternative.

# 1. For the variables depicted in Figure 22.1, draw a figure
#    showing the covariance between variables.

library(piecewiseSEM)
data(keeley)
names(keeley)

df_vars <- keeley[, c("distance", "elev","abiotic","age", "hetero",
                   "firesev","cover","rich")]

cov_matrix <- cov(df_vars, use = "complete.obs")
round(cov_matrix, 2)
#Visualize 

df_vars %>% 
  ggpairs(
    progress = FALSE,
    columns = c("distance", "elev","abiotic","age", "hetero",
                "firesev","cover","rich")
  ) +
  theme_bw()


# 2. Following Figure 22.1, develop a path model using the
#    same variables and relationships. Examine if this model
#    captures the data structure using a Chi-Square test.

m_var1 <- '
  age~distance
  abiotic ~ distance
  hetero  ~ distance 
  firesev ~ distance + age
  cover   ~ distance + age+firesev
  rich    ~ cover+ abiotic +hetero
'
m_var1

(fit_m_var1 <- sem(m_var1,
                data = df_vars))
## summary

summary(fit_m_var1, standardize = TRUE)

lavaanPlot(model = fit_m_var1, coefs = TRUE, stand = TRUE)


summary(m_var1)


# 3. Develop an alternative path model that you consider more
#    appropriate based on theory or observed data patterns.

mod_alt <- '
  abiotic ~ distance + elev + age
  hetero  ~ abiotic
  firesev ~ abiotic
  cover   ~ hetero + firesev
  rich    ~ cover + hetero + abiotic
'

fit_alt <- sem(mod_alt, data = df_vars)

summary(fit_alt,
        standardized = TRUE,
        fit.measures = TRUE)

lavaanPlot(model = fit_alt,
           coefs = TRUE,
           stand = TRUE)


# 4. Compare the performance of the published model (Figure 22.1)
#    and your alternative model.
#    - Consider fit indices, path coefficients, and interpretability.

