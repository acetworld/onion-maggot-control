#########################################
#                                       #
#       Onion Maggot Size Trials       #
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

# Load data for 2005 size trial
size_trial_2005 <- read_csv('./data/om_size_trials_2005.csv', 
                            skip = 25)

# Information on treatments documented in csv file
treatment_info_2005 <- tibble(
    treatment = 5:9,
    #size is diameter of sphere in centimeters
    size = factor(c('7.5', '5.0', '6.25', '8.75', '10.0'),
                  levels = c('10.0', '8.75', '7.5', '6.25', '5.0'))
)

# Reformat 2005 data, add in treatment and year information
size_trial_treatment_2005 <- size_trial_2005 %>% 
    gather(sex, flies, males, females) %>% 
    left_join(treatment_info_2005) %>% 
    mutate(year = '2005') %>% 
    select(sex, size, flies, year)


# Load data for 2006 size trial
size_trial_2006 <- read_csv('./data/om_size_trials_2006.csv', 
                            skip = 16, col_types = c('ccccii'))

# Information on treatments documented in csv file
treatment_info_2006 <-tibble(
    treatment = as.character(6:10),
    size = factor(c('7.5', '5.0', '6.25', '8.75', '10.0'),
                  levels = c('10.0', '8.75', '7.5', '6.25', '5.0'))
    )

# Reformat 2006 data, add in treatment and year information
size_trial_treatment_2006 <- size_trial_2006 %>% 
    select(-size) %>% 
    gather(sex, flies, females, males) %>% 
    left_join(treatment_info_2006) %>%
    mutate(year = '2006') %>% 
    select(sex, size, flies, year)

# Combine years
size_trial <- bind_rows(size_trial_treatment_2006, size_trial_treatment_2005)

# Modeling --------------------------

# Linear model relating sex and color to fly catch
size_mod <- lm(flies ~ sex + size + year + sex:year, data = size_trial)

# Print and plot model diagnostics
# model_diagnostics(size_mod) # Run to see plots
model_diagnostics(size_mod, to_plot = FALSE)

# Non-normality and slight heteroscedascity detected
# Square root transform applied
size_mod_transform <- lm(sqrt(flies) ~ sex + size + year + sex:year, 
                         data = size_trial)

# Model diagnostics of new model - resolved issues
# model_diagnostics(size_mod_transform) # Run to see plots
model_diagnostics(size_mod_transform, to_plot = FALSE)

# Contrasts and post-hoc tests 
emmeans(size_mod_transform, ~ sex + size, type = 'response') %>% 
    CLD() 


# Figure generation -----------------------------

# generate contrasts
size_means <- emmeans(size_mod_transform, ~ sex + size, type = 'response') %>%
    CLD(Letters = LETTERS, reversed = TRUE) %>% 
    data.frame()

# Figure generation
size_figure <- ggplot(size_means, aes(x = size, y = response, color = sex)) + 
    geom_point(position = position_dodge(0.48)) + 
    geom_errorbar(aes(x = size, ymin = lower.CL, ymax = upper.CL),
                  position = position_dodge(0.48),
                  width = 0.24) + 
    geom_text(aes(x = size, y = upper.CL + 3, label = .group),
              position = position_dodge(0.48),
              show.legend = FALSE) + 
    scale_color_grey(labels = c('Females', 'Males')) + 
    labs(x = 'Trap Sphere Diameter (cm)', y = 'Onion Maggot Fly Adults') + 
    applied_entomology_theme

# Save figure
ggsave(plot = size_figure, 
       filename = './figures/raw/size_trial.pdf',
       width = 5, height = 3)
