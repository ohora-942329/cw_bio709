#' DESCRIPTION:
#' Script for GLMMs

# in-class ----------------------------------------------------------------
pacman::p_load(tidyverse,
               lme4,
               glmmTMB)

data(Owls) # from `glmmTMB`


# convert Owls to tibble
df_owl_raw <- as_tibble(Owls)

(df_owl <- df_owl_raw %>%                 # Start with raw owl dataset and assign cleaned version
    janitor::clean_names() %>%              # Standardize column names (lowercase, underscores, etc.)
    mutate(across(.cols = where(is.factor), # Select all factor-type columns
                  .fns = str_to_lower))     # Convert factor levels to lowercase
  
)     
#Visualization 
df_owl %>%                          
  ggplot(aes(x = food_treatment,    
             y = neg_per_chick)) +  
  geom_boxplot(outliers = FALSE) +  
  geom_jitter(alpha = 0.25) +    # for individual observation    
  theme_bw()     
##ignoring nest effect 
m_glm <- MASS::glm.nb(sibling_negotiation ~ food_treatment + offset(log(brood_size)),
                      data = df_owl)
print (m_glm)
## add nest effects as fixed effects
m_nest <- MASS::glm.nb(sibling_negotiation ~ food_treatment + nest + offset(log(brood_size)),
                      data = df_owl)
print(m_nest)
## Visual for group-level effect 
v_g9 <- unique(df_owl$nest)[1:9]
df_owl %>%                          
  filter(nest %in% v_g9) %>%        
  ggplot(aes(x = food_treatment,    
             y = neg_per_chick)) +  
  geom_jitter(alpha = 0.25,         
              width = 0.1) +       
  facet_wrap(facets =~ nest,        
             ncol = 3,              
             nrow = 3) +
  theme_bw()   

## use of glmmTMB to include random effects 
#
## with random effect -y~x + (1|group)
# regular glm -y~X
m_ri <- glmmTMB(
  sibling_negotiation ~ 
    food_treatment + (1 | nest) +  offset(log(brood_size)),  
  data = df_owl,              
  family = nbinom2()         
)
print(m_ri)
  
## get random intercepts 

head(coef(m_ri)$cond$nest)
##
g0 <- exp(fixef(m_ri)$cond[1])


# ------------------------------------------------------------
# 2. Select a subset of nests to visualize
# ------------------------------------------------------------
# Plotting all 27 nests would be visually overwhelming,
# so we randomly select 9 nests for this example.
set.seed(123)  # ensures reproducibility
v_g9_ran <- sample(unique(df_owl$nest),
                   size = 9)


# ------------------------------------------------------------
# 3. Extract nest-specific coefficients (random intercept model)
# ------------------------------------------------------------
# coef(m_ri) returns the sum of fixed + random effects for each nest.
# These values are still on the log scale.
df_g9 <- coef(m_ri)$cond$nest %>% 
  as_tibble(rownames = "nest") %>%      # convert to tibble and keep nest ID
  filter(nest %in% v_g9_ran) %>%        # keep only the selected nests
  rename(
    log_g = `(Intercept)`,              # nest-specific intercept (log scale)
    b = food_treatmentsatiated          # fixed slope for food treatment
  ) %>% 
  mutate(
    g = exp(log_g),                     # intercept on response scale
    s = exp(log_g + b)                  # predicted value under satiated treatment
  )

# ------------------------------------------------------------
# 4. Create the figure
# ------------------------------------------------------------
df_owl %>% 
  filter(nest %in% v_g9_ran) %>%        # plot only the selected nests
  ggplot(aes(x = food_treatment,
             y = neg_per_chick)) +
  # Raw data points (jittered to reduce overlap)
  geom_jitter(width = 0.1,
              alpha = 0.5) +
  # Dashed horizontal lines:
  # nest-specific intercepts (baseline differences among nests)
  geom_hline(data = df_g9,
             aes(yintercept = g),
             alpha = 0.5,
             linetype = "dashed") +
  # Solid line segments:
  # predicted change from unfed to satiated treatment
  # using a common (fixed) slope across nests
  geom_segment(data = df_g9,
               aes(y = g,
                   yend = s,
                   x = 1,
                   xend = 2),
               linewidth = 0.5,
               linetype = "solid") +
  # Solid blue horizontal line:
  # global (population-level) intercept
  geom_hline(yintercept = g0,
             alpha = 0.5,
             linewidth = 1,
             linetype = "solid",
             color = "steelblue") +
  # Facet by nest to emphasize group-level structure
  facet_wrap(facets =~ nest,
             nrow = 3,
             ncol = 3) +
  theme_bw()


# lab ---------------------------------------------------------------------

# EXERCISE: GLMM Exercise - `sleep` Data Set
# ============================================================

# sleep dataset (built-in R dataset)
#
# This dataset contains measurements of increased sleep time
# after administering two different drugs.
#
# Structure:
# - 20 observations total
# - 10 subjects (each measured twice)
# - Paired / repeated-measures design
#
# Variables:
#   extra : Increase in hours of sleep compared to baseline
#   group : Indicates which drug was administered (factor with two levels ("1", "2"))
#   ID    : factor identifying each subject; Each subject appears once in each group
# ------------------------------------------------------------

# Q1 – Visualization:
# Compare sleep increase ("extra") between the two drug groups.
#
# Goals:
# - Show individual-level responses
# - Highlight paired structure (same subject in both groups)
# - Use color to identify subjects
# - facet by individual #Connect observations from the same subject using lines
print((sleep))

library(ggplot2)

ggplot(sleep, aes(x = group, y = extra, group = ID)) +
  geom_point(size = 3, color = "steelblue") +
  geom_line(alpha = 0.25, color = "steelblue") +
  facet_wrap(~ ID) +
  labs(
    x = "Drug",
    y = "Increase in Sleep (hours)"
  ) +
  theme_minimal()
#Using mutate 
sleep %>%
  mutate(group = as.numeric(group)) %>%
  ggplot(aes(x = group,
             y = extra,
             color = ID,
             group = ID)) +
  geom_point(size = 3) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ ID) +
  labs(
    x = "Drug",
    y = "Increase in Sleep (hours)"
  ) +
  theme_minimal()

# Q2 - Model development:
#
# Goal:
#   Examine how drug administration affects sleep duration.
#
# Key considerations:
#   - Response variable (extra) is continuous
#   - Drug (group) represents the treatment of interest
#   - Subject ID represents repeated measurements on the same
#     individuals
## via lme4
library(lme4)

sleep_lmm <- lmer(extra ~ group + (1 | ID), data = sleep)
summary(sleep_lmm)

##glmmTMB
library(glmmTMB)

m_sleep <- glmmTMB(
  extra ~ group + (1 | ID),
  data = sleep,
  family = gaussian()
)

summary(m_sleep)

# ============================================================
# EXERCISE: GLMM Exercise - `grouseticks` Data Set
# ============================================================
library(lme4)
data("grouseticks")
print()

# ------------------------------------------------------------
# grouseticks dataset (from lme4 package)
#
# This dataset contains counts of parasitic ticks
# collected from red grouse chicks across multiple years
# and locations in Scotland.
#
# Structure:
# - 403 observations
# - Repeated measurements across broods and years
# - Count data with hierarchical (nested) structure
#
# Variables:
#   TICKS : Number of ticks counted on a chick
#   YEAR  : Sampling year
#   HEIGHT: height above sea level (meters)
#   LOCATION : Sampling site (grouping variable)
#   INDEX : Observation-level identifier
#
# Key features of the dataset:
# - Response variable is count data
# - Observations are grouped by brood and year
# ------------------------------------------------------------

# Q1 – Visualization:
#
# Goal:
#   Examine the relationship between parasite load (ticks) at the brood level and height above sea level.
#
# Key considerations:
# - Calculate average tick counts for each brood
# - Plot mean ticks vs. height
# - Color points by sampling year
print(grouseticks)
library(dplyr)

brood_means <- grouseticks %>%
  group_by(BROOD, YEAR, HEIGHT) %>%
  summarise(
    mean_ticks = mean(TICKS),
    .groups = "drop"
  )
##plot
ggplot(brood_means,
       aes(x = HEIGHT, y = mean_ticks, color = factor(YEAR))) +
  geom_point(size = 2, alpha = 0.8) +
  labs(
    title = "Mean Tick Load vs Height",
    x = "Height above sea level (m)",
    y = "Mean number of ticks",
    color = "Year"
  ) +
  theme_minimal()

# Q2 – Model development:
#
# Goal:
#   Develop a model to examine the relationship between parasite load (ticks) at the brood level and height above sea level.
#
# Key considerations:
#   - Response variable (TICKS) is count
#   - HEIGHT represents the variable of interest
#   - BROOD represents a grouping factor of repeated measurements
#   - YEAR represents another grouping factor of repeated measurements

ticks_glmm <- glmer(
  TICKS ~ HEIGHT + (1 | BROOD) + (1 | YEAR),
  data = grouseticks,
  family = poisson
)

summary(ticks_glmm)

