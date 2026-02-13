#' DESCRIPTION:
#' Script for Unconstrained Ordination

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               GGally,
               vegan)

#use iris data
df_iris <- iris %>% 
  as_tibble() %>%           
  janitor::clean_names() 

  df_iris %>%
  ggpairs(
    progress = FALSE, 
    columns = c("sepal_length",
                "sepal_width",
                "petal_length",
                "petal_width"),
    aes( color = species,
      alpha = 0.5)
  ) +
  theme_bw() 
  
  df_petal <- df_iris %>% 
    select(starts_with("petal_"))
 
   obj_pca <- prcomp(
    x = df_petal,    
    center = TRUE,   
    scale = TRUE)
   obj_pca
   summary(obj_pca)
obj_pca$x

      df_pca <- bind_cols(
     df_iris,             
     as_tibble(obj_pca$x)
   )
      #draw a figure comparing PC1 Values between species 
      df_pca %>% 
        ggplot(aes(
          x = species,   
          y = PC1,      
          color = species 
        )) +
        geom_boxplot() +
        labs(x = "Species",
             y = "Petal shape (PC1)")
  
      
# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: PCA using the iris dataset
# ============================================================

# In this exercise, you will perform a Principal Component
# Analysis (PCA) using all morphological measurements in the
# iris dataset and visualize multivariate trait patterns
# among species.

# 1. Using all four morphological variables
#    (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
#    perform a PCA.
      df_ps<-df_iris %>% 
        select(-species)
      
      df_pca2 <- prcomp(x= df_ps,
                        center = TRUE,
                        scale. = TRUE)
      summary(df_pca2)
      
     
     df_iris %>% 
       bind_cols(df_iris)
      
# 2. Visualize flower morphology in PC axes whose cumulative
#    contribution exceeds 90%; color points by species.
     df_pca %>% 
       ggplot(aes( x = PC1,
                   y= PC2,
                   color = species)) +
       geom_point()

# 3. Which morphological traits contribute most strongly to
#    the first and second principal components? How?
           
#NMDS ###
       data("dune")
     dune %>% 
       as_tibble() %>%         
       select(1:3) %>%         
       ggpairs() +             
       theme_bw() 
    #calculate distance between units
     #column will be species and row is sites 
     m_bray <- vegdist(dune,
                       method = "bray") 
     m_bray
     #metaMDS(comm= dune)
     
     obj_nmds <- metaMDS( 
       m_bray,
       k = 2)
       obj_nmds
  #visualization of NMDS
       data(dune.env)
       
       df_nmds <- dune.env %>%           
         as_tibble() %>%                  
         bind_cols(obj_nmds$points) %>%   
         janitor::clean_names()  
       
       df_nmds %>% 
         ggplot(aes(
           x = mds1, 
           y = mds2,
           color = use         
         )) +
         geom_point(size = 3) +               
         stat_ellipse(level = 0.95,           
                      linetype = 2) +   
         theme_bw() +                         
         labs(color = "Land-use intensity",   
              x = "NMDS1",            
              y = "NMDS2")  
       
 # Perform PERMANOVA to test whether plant community composition differs by land-use intensity
       adonis2(m_bray ~ use,   
         data = df_nmds)
# ============================================================
# EXERCISE: NMDS using the BCI dataset
# ============================================================

# In this exercise, you will perform a Non-metric Multidimensional
# Scaling (NMDS) using the BCI tree community dataset and explore
# patterns in species composition among sites.
     
data("BCI", "BCI.env")

# 1. Using the BCI dataset, calculate a dissimilarity matrix
#    (e.g., Bray-Curtis) and perform NMDS.
       
       data("BCI")
       data("BCI.env")
       
       head(BCI.env)  
       
       obj_nmds <- metaMDS(BCI, distance = "bray", k = 2)
       
       df_nmds <- BCI.env %>%
         as_tibble() %>%
         bind_cols(as_tibble(obj_nmds$points)) %>%
         janitor::clean_names()
       
       head(df_nmds)
       
       
# 2. Visualize sites in NMDS space.
#    - How are sites positioned relative to each other?
#    - Color or shape points by environmental groups or site
#      characteristics of your choice.
       
       
       df_nmds %>% 
         ggplot(aes(x = mds1, y = mds2, color = habitat)) +
         geom_point(size = 3)
       
# 3. Perform PERMANOVA to examine if communities are grouped
#    by the environmental variable you selected.
       
       m_bray <- vegdist(BCI, method = "bray")
       adonis2(m_bray ~ Habitat, data = BCI.env)
       
       