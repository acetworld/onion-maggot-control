#########################################
#                                       #
#       Onion Maggot Color Trials       #
#                                       #
#########################################


# ACET Lab + Nault Lab @ Cornell AgriTech
# May 2019

# Load Libraries and other resources --------------

library(tidyverse)
library(car)
library(emmeans)
source('./analysis/model_diagnostics.R')
source('./analysis/appl_ent_figure_theme.R')


# Dato Processing ----------------------------

# Load data for color trial
color_trials <- read_csv('./data/om_color_trials_2005.csv', skip = 19)

# Information on treatments documented in csv file
treatment_info <- tibble(
    treatment = c(8, 10,11,12),
    color = factor(c('White',
              'Yellow',
              'Green',
              'Red'),
              levels = c('White',
                         'Yellow',
                         'Green',
                         'Red'))
)

# Reshape the data and join with treatment information
color_trial_treatment <- color_trials %>% 
    gather(sex, flies, males, females) %>%
    left_join(treatment_info)

# Modeling --------------------------

# Linear model relating sex and color to fly catch
color_mod <- lm(flies ~ sex * color, data = color_trial_treatment)

# Print and plot model diagnostics
# model_diagnostics(color_mod) # Run to see plots
model_diagnostics(color_mod, to_plot = FALSE)

# Non-normality and slight heteroscedascity detected
# Square root transform applied
color_mod_transform <- lm(sqrt(flies) ~ sex * color, data = color_trial_treatment)

# Model diagnostics of new model - resolved issues
# model_diagnostics(color_mod_transform) # Run to see plots
model_diagnostics(color_mod_transform, to_plot = FALSE)

# Contrasts and post-hoc tests
emmeans(color_mod_transform, ~ sex * color, type = 'response') %>% 
    contrast(method = 'dunnett', by = 'sex')

emmeans(color_mod_transform, ~sex * color, type = 'response') %>%
    contrast(method = 'tukey', by = 'color')

# Figure generation -----------------------------

# Set plotting parameters
dodge_width = 0.48
jitter_width = 0.12

# Set seed for reproducibility
set.seed(72)

# generate figure
color_trial_figure <- ggplot(color_trial_treatment, aes(x = color, y = flies, color = sex)) + 
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
    scale_color_grey(labels = c('Females','Males')) + 
    labs(x = 'Trap Color', y = 'Onion Maggot Fly Adults') +
    applied_entomology_theme 

# Save figure
ggsave(plot = color_trial_figure, 
       filename = './figures/raw/color_trial_figure.pdf',
       width = 5, height = 3)
