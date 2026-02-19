#' DESCRIPTION:
#' Script for Constrained Ordination

# in-class ----------------------------------------------------------------
pacman::p_load(tidyverse,
               GGally,
               vegan)
data("varespec", "varechem")
#rename
m_y<- varespec
colnames(m_y) <- str_to_lower(colnames(varespec))
df_env <- as_tibble(varechem) %>% 
  janitor::clean_names()

#Visualization matrix of the first 3 species columns in m_y
m_y %>%
  ggpairs(
    progress = FALSE,      
    columns = 1:3,         
    aes(
      alpha = 0.5           
    )
  ) +
  theme_bw()  
#perform RDA
(obj_rda <- rda(m_y ~ n + p + ca,
                data = df_env))
#statistical test 
anova.cca(obj_rda, 
          by = "margin", 
          permutations = 999)
#Visualization 

df_rda<- scores(obj_rda,
       display = "site",
       scaling = 2) %>% 
  bind_cols(df_env) %>% 
  janitor::clean_names()

# RDA vectors for environmental predictors
df_bp <- scores(obj_rda, 
                display = "bp", 
                scaling = 2) %>% 
  as_tibble(rownames = "variable") %>% 
  janitor::clean_names()

# Create a ggplot2 ordination plot
df_rda %>% 
  ggplot(aes(x = rda1,
             y = rda2)) +        # color sites by nitrogen level
  geom_point(aes(color = n)) +
  geom_segment(data = df_bp,
               aes(x = 0, xend = rda1 * 10, # 10 is arbitrary scaling for visualization
                   y = 0, yend = rda2 * 10),
               arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_text(data = df_bp,
            aes(x = rda1 * 10.5,    # slightly beyond arrow tip
                y = rda2 * 10.5,
                label = variable),  # or use a variable column
            size = 4) +
  theme_bw() +
  labs(x = "RDA1",
       y = "RDA2",
       color = "Nitrogen") +
  scale_color_viridis_c()
#perform distance-based Redundancy Analysis (dbRDA)
(obj_db <- dbrda(m_y ~ n + p + ca,
                 data = df_env,
                 distance = "bray"))
#anova.cca test
anova.cca(obj_db,
          by = "margin",
          permutations = 999)

#visualization 

df_db <- scores(obj_db, 
                display = "sites",
                scaling = 2) %>% 
  as_tibble() %>%              
  bind_cols(df_env) %>%        
  janitor::clean_names()  

# dbRDA vectors for environmental predictors
df_bp <- scores(obj_db, 
                display = "bp", 
                scaling = 2) %>% 
  as_tibble(rownames = "variable") %>% 
  janitor::clean_names()

# Create a ggplot2 ordination plot
df_db %>% 
  ggplot(aes(x = db_rda1,
             y = db_rda2)) +        
  geom_point(aes(color = n)) +
  geom_segment(data = df_bp,
               aes(x = 0, xend = db_rda1,
                   y = 0, yend = db_rda2),
               arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_text(data = df_bp,
            aes(x = db_rda1 * 1.1,    
                y = db_rda2 * 1.1,
                label = variable), 
            size = 4) +
  theme_bw() +
  labs(x = "dbRDA1",
       y = "dbRDA2",
       color = "Nitrogen") +
  scale_color_viridis_c()

# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: Community Ordination and Environmental Gradients
# ============================================================

library(vegan)
data("mite", "mite.env")

# The mite datasets contain information on Oribatid mite communities
# sampled from a small peatland area (2.5 m × 10 m).
#
# There are linked datasets:
# ------------------------------------------------------------
# mite     : Species abundance data (35 mite species × 70 sites)
# mite.env : Environmental variables measured at the same sites
# ------------------------------------------------------------
#
# Environmental variable descriptions (mite.env):
# ------------------------------------------------------------
# SubsDens : Substrate density (g/L)
# WatrCont : Water content of the substrate (g/L)
# Substrate: Substrate type (factor with multiple levels)
# Shrub    : Shrub density (ordered factor: low → high)
# Topo     : Microtopography (Blanket vs Hummock)
# ------------------------------------------------------------

# 1. Explore and visualize interrelationships among species abundances.
#    - Examine patterns of co-occurrence.
#    - Assess whether relationships among species appear linear or nonlinear.
ggpairs(mite,
        columns = 1:5,
        progress= FALSE)

ggpairs(wisconsin(mite),
        columns = 1:5,
        progress = FALSE)

# Prepare environmental data

df_env <- as_tibble(mite.env) %>% 
  janitor::clean_names()

df_env$shrub <- factor(df_env$shrub, ordered = TRUE)
df_env$topo <- factor(df_env$topo)


# 2. Fit a redundancy analysis (RDA) model using environmental variables of your choice.
#    - Visualize the ordination results.
#    - Examine gradients and species–environment relationships.
#    - Evaluate whether the assumptions of RDA are appropriate for these data.

# Fit RDA model
# -------------------------------
# Using a few environmental variables of choice

m_hell <- decostand(mite, method = "hellinger")

obj_rda <- rda(m_hell ~ subs_dens + watr_cont + substrate + topo, data = df_env)
summary(obj_rda)


# Plot RDA

df_rda <- as.data.frame(scores(obj_rda, display = "sites"))
df_rda$moisture <- df_env$watr_cont  
head(df_rda)

df_bp <- as.data.frame(scores(obj_rda, display = "bp"))
df_bp$variable <- rownames(df_bp)

library(ggplot2)

ggplot(df_rda, aes(x = RDA1, y = RDA2)) +
  geom_point(aes(color = moisture), size = 3) +
  geom_segment(data = df_bp,
               aes(x = 0, xend = RDA1 * 10, y = 0, yend = RDA2 * 10),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black") +
  geom_text(data = df_bp,
            aes(x = RDA1 * 10.5, y = RDA2 * 10.5, label = variable),
            size = 4) +
  scale_color_viridis_c() +
  theme_bw() +
  labs(x = "RDA1", y = "RDA2", color = "Water content")

# 3. Apply alternative ordination methods.
#    - Canonical correspondence analysis (CCA; see ?cca()).
#    - Distance-based RDA (dbRDA).

# CCA
obj_cca <- cca(mite ~ subs_dens + watr_cont + substrate + topo, data = df_env)
summary(obj_cca)
plot(obj_cca, display = c("sites", "species", "bp"), main = "CCA of Mite Communities")

# dbRDA
mite_dist <- vegdist(mite, method = "bray")
obj_dbrda <- capscale(mite_dist ~ subs_dens + watr_cont + substrate + topo, data = df_env)
summary(obj_dbrda)
plot(obj_dbrda, main = "dbRDA of Mite Communities")

# 4. Compare RDA, CCA, and dbRDA.
#    - Perform permutation analysis to examine the significance of predictor variables
#    - Discuss which method is most appropriate for these data and why.
# Load required packages

anova(obj_rda, permutations = 999)
anova(obj_cca, permutations = 999)
anova(obj_dbrda, permutations = 999)

