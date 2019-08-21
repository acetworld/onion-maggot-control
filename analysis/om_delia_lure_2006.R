##############################################
#                                            #
#       Onion Maggot Delia Lure Trials       #
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

# Load data for color trial
delia_lure_trial <- read_csv('./data/om_delia_lure_2006.csv', skip = 9) %>% 
    # reported numbers are total counts for four sticky cards divided by four.
    # we'll multiply by 4 to bring back to total counts.
    mutate(total_onion_maggot = as.integer(onion_maggot * 4)) %>% 
    select(-onion_maggot)

# Reformat for t-test
prep_ttest <- delia_lure_trial %>% spread(treatment, total_onion_maggot)

# Modeling --------------------------

# Paired t-test by location and day
t.test(prep_ttest$Baited, prep_ttest$Unbaited, paired = TRUE)

# Check for normality
qqPlot(delia_lure_trial$total_onion_maggot)

# Figure generation -----------------------------

# Set seed for reproducibility
set.seed(24)

# Generate figure
delia_lure_figure <- ggplot(delia_lure_trial, aes(x = treatment, y = total_onion_maggot)) + 
    stat_summary(fun.data = 'mean_cl_boot', 
                 geom = 'errorbar',
                 width = 0.24) +
    stat_summary(fun.data = 'mean_cl_boot',
                 geom = 'point') + 
    geom_point(position = position_jitter(0.12),
               alpha = 0.48) + 
    labs(x = 'Treatment', y = 'Total Onion Maggot Fly Adults') +
    applied_entomology_theme

# Save figure
ggsave(plot = delia_lure_figure,
       filename = './figures/raw/delia_lure_figure.pdf',
       width = 4, height = 3)
