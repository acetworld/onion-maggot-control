##############################################
#                                            #
#       Onion Maggot Shape Trials            #
#                                            #
##############################################


# ACET Lab + Nault Lab @ Cornell AgriTech
# May 2019

# Load Libraries and other resources --------------
library(tidyverse)
library(car)
library(emmeans)
source('./analysis/model_diagnostics.R')
source('./analysis/appl_ent_figure_theme.R')


# Dato Processing ----------------------------

# Load data for shape trial
shape_trial_results <- read_csv('./data/om_shape_trials_2005.csv', skip = 17)

# Shape treatment info from csv file
treatment_info <- tibble(
    treatment = 1:5,
    treatment_shape = factor(c('Short Cylinder',
                        'Tall Cylinder',
                        'Cube',
                        'Square Panel',
                        'Sphere'
                        ),
                        levels = c('Short Cylinder',
                                   'Sphere',
                                   'Cube',
                                   'Square Panel',
                                   'Tall Cylinder'
                        ))
)

# Reshape the data and join with treatment information
shape_trial <- shape_trial_results %>% 
    gather(key = sex, value = flies, -treatment, -replication) %>% 
    left_join(treatment_info)


# Modeling --------------------------

# Linear model relating sex and color to fly catch
shape_mod <- lm(flies ~ sex * treatment_shape, data = shape_trial)

# Print and plot model diagnostics
# model_diagnostics(shape_mod) # Run to see plots
model_diagnostics(shape_mod, to_plot = FALSE)

# Based on diagnostics, there seems to be an outlier
outlierTest(shape_mod) # check outlier

# Remodel with outlier removed
shape_mod_xoutlier <- lm(flies ~ sex * treatment_shape, data = shape_trial[-31,])

# Print and plot model diagnostics
# model_diagnostics(shape_mod_xoutlier) # Run to see plots
model_diagnostics(shape_mod_xoutlier, to_plot = FALSE)

# Contrasts and post-hoc tests
emmeans(shape_mod_xoutlier, ~sex ) %>% contrast(method = 'tukey')

emmeans(shape_mod_xoutlier, ~sex*treatment_shape) %>% 
    cld(by = 'sex')

stat_labels <- emmeans(shape_mod_xoutlier, ~sex*treatment_shape) %>% 
    cld(by = 'sex') %>% data.frame() %>% 
    filter(sex == 'females') %>% 
    mutate(label = c('B', 'B', 'AB', 'AB', 'A'))


# Figure generation -----------------------------

# Set plotting parameters
dodge_width = 0.48
jitter_width = 0.12

# Set seed for reproducibility
set.seed(72)

# generate figure
shape_trial_figure <- ggplot(shape_trial[-31,], aes(x = treatment_shape, y = flies, color = sex)) + 
    stat_summary(fun.data = 'mean_cl_boot', 
                 geom = 'errorbar',
                 position = position_dodge(dodge_width),
                 width = dodge_width/2) +
    stat_summary(fun.data = 'mean_cl_boot',
                 geom = 'point',
                 position = position_dodge(dodge_width)) +
    geom_point(position = position_jitterdodge(dodge.width = dodge_width,
                                               jitter.width = jitter_width),
               alpha = 0.48) + 
    geom_text(data = stat_labels, 
              aes(x = treatment_shape, y= 72, label = label),
              show.legend = FALSE) + 
    scale_color_grey(labels = c('Females', 'Males')) + 
    labs(x = 'Trap Shape', y = 'Onion Maggot Fly Adults') + 
    applied_entomology_theme

# Save figure
ggsave(plot = shape_trial_figure,
       filename = './figures/raw/shape_trial_figure.pdf',
       width = 5, height = 3)
