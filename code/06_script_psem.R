#' DESCRIPTION:
#' Script for piecewise SEM

# in-class ----------------------------------------------------------------
pacman::p_load(tidyverse,glmmTMB,
               GGally,
               piecewiseSEM)

data("keeley")

(df_keeley <- keeley %>% 
    as_tibble())

# Define individual models, piecewise but all normal
#rich is discreet variable. 
m1 <- lm(abiotic ~ distance, data = df_keeley)
m2 <- lm(hetero ~ distance, data = df_keeley)
m3 <- lm(firesev ~ age, data = df_keeley)
m4 <- lm(cover ~ firesev, data = df_keeley)
m5 <- lm(rich ~ cover + abiotic + hetero, data = df_keeley)

# Combine into piecewise SEM
sem_model <- psem(m1, m2, m3, m4, m5)
sem_model

# Evaluate
summary(sem_model, .progressBar = FALSE)

m1 <- lm(abiotic ~ distance, data = df_keeley)
m2 <- lm(hetero ~ distance, data = df_keeley)
m3 <- lm(firesev ~ age, data = df_keeley)
# m4 now included in to direct effect

m4 <- lm(cover ~ firesev + hetero, data = df_keeley)  

# m5 now models richness as negative binomial (MASS::glm.nb) 
# and includes direct effect of distance on richness (added path)

m5<- MASS:: glm.nb(rich ~ cover + abiotic + hetero + distance, 
                  data = df_keeley)
# Combine into piecewise SEM
sem_model <- psem(m1, m2, m3, m4, m5)

# Evaluate model
summary(sem_model, .progressBar = FALSE)

#plot sem_model for visualization 

plot(sem_model)

# including random effects 
data("shipley")

df_shipley <- shipley %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  drop_na(growth)

df_shipley %>% 
  group_by(site) %>% 
  summarise(n_tree = n_distinct(tree))

## visualization 
df_shipley %>% 
        ggpairs(
        columns = c("dd", 
                    "date",
                    "growth",
                    "live"),
        progress = FALSE      
      ) +
      theme_bw()   

## Tree growth: implementation; latent variables is not implemented in the current package.


# Model 1: date depends on dd, with random intercepts for site and tree
m1 <- glmmTMB(date ~ dd + (1 | site) + (1 | tree), 
              data = df_shipley,
              family = "gaussian")

# Model 2: growth depends on date, same random effects
m2 <- glmmTMB(growth ~ date + (1 | site) + (1 | tree), 
              data = df_shipley,
              family = "gaussian")

# Model 3: live (binary) depends on growth, logistic mixed model
m3 <- glmmTMB(live ~ growth + (1 | site) + (1 | tree), 
              data = df_shipley, 
              family = "binomial")

# Combine models into a piecewise SEM
sem_glmm <- psem(m1, m2, m3)

# Summarize SEM (paths, significance, and Shipley's test); Marginal effects represent the average, population-level impact of a variable, whereas conditional effects show the specific effect for a certain group
summary(sem_glmm, .progressBar = FALSE)




# lab ---------------------------------------------------------------------

library(piecewiseSEM)
data("meadows")

# =========================================
# EXERCISE: Piecewise SEM with Meadows Data
# =========================================
(df_meadows <- meadows %>% 
   as_tibble())

# ------------------------------------------------------------
# Dataset: meadows (from piecewiseSEM package)
# Variables:
#   grazed - 0 = ungrazed, 1 = grazed
#   mass   - plant biomass (g/m²)
#   elev   - plot elevation above sea level
#   rich   - plant species richness per m²
# ------------------------------------------------------------
#
# 1. Explore the dataset (structure, summary, plots).

# Summary 
summary(meadows)

## pairwise plot, visualization 
df_meadows %>% 
  ggpairs(
    columns = c("elev", 
                "mass",
                "rich",
                "grazed"),
    progress = FALSE      
  ) +
  theme_bw() 

# 2. Develop a conceptual model: decide which variables influence others.
#    - Consider direct and indirect effects.
#    - Think about grazing as a disturbance factor.
## direct effect 
mod1 <- lm(grazed ~ mass + rich, data = df_meadows)
mod2 <- lm(mass ~ rich, data = df_meadows)
mod3 <- lm(elev ~ mass + rich, data = df_meadows)

## combining them in SEM 
mod_dir <- psem(mod1, mod2, mod3)
mod_dir
plot(mod_dir)
#indirect effect 

mod_ind_1 <- lm(mass ~ elev, data = df_meadows)
mod_ind_2 <- lm(rich ~ mass + elev, data = df_meadows)
psem_ind12 <- psem(mod_ind_1, mod_ind_2)

summary (psem_ind12)
plot(psem_ind12)

mod_ind_3 <- lm(mass ~ grazed, data = df_meadows)
mod_ind_4 <- lm(rich ~ mass + grazed, data = df_meadows)
psem_ind34 <- psem(mod_ind_3, mod_ind_4)

summary(psem_ind34)
plot(psem_ind34)

# 3. Fit component models (e.g., lm) for each hypothesized relationship.
# biomass 
mod_biom <- lm(mass ~ grazed + elev, data = df_meadows)

library(MASS)
## model for richness model 

mod_rich <- MASS::glm.nb(rich ~ mass + grazed + elev,
                   data = df_meadows)

# 4. Combine models into a piecewise SEM using psem().

sem_meadows <- psem(mod_biom, mod_rich)

sem_meadows

# 5. Evaluate the SEM: path coefficients, significance, variance explained.

summary(sem_meadows, .progressBar = FALSE)

# 6. Optional: try alternative models if your model deviates from the expectation.
#visualize 

plot(sem_meadows)

# Deliverables:
# - Code for component models and combined SEM
# - Conceptual SEM diagram
# - Short reasoning about your SEM results
