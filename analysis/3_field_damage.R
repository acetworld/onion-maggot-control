##############################################
#                                            #
#       Onion Maggot Spinosad                #
#        Field OM Damage 2007                #
#                                            #
##############################################


# ACET Lab + Nault Lab @ Cornell AgriTech
# May 2019

# Load Libraries and other resources --------------
library(tidyverse)
library(car)
library(emmeans)
library(lme4)
source('./analysis/0_model_diagnostics.R')
source('./analysis/0_pest_management_science_theme.R')


# Load data -----------

field_damage <- read_csv('./data/om-spinosad-field-damage-trial-2007.csv', skip = 17)

# Preprocess data ------------------

# Treatment information
control_treatment <- tibble(
    treatment = 1:6,
    control_type = c('Insecticide',
                     'No Insecticide',
                     'Sticky Card',
                     'Sticky Card',
                     'Spinosad Sphere',
                     'Spinosad Sphere')
)

# Look at field damage over time 
field_damage_time <- field_damage %>% 
    gather(key = 'info', value = 'damage', contains('dam')) %>%
    mutate(month = str_sub(info, 4,4),
           day = str_sub(info, 5),
           date = lubridate::mdy(paste(month, day, '2007')),
           delia_lure = if_else(treatment %in% c(4,6), 'Lure','No Lure')) %>%
    left_join(control_treatment) %>%
    arrange(treatment, replication, date) %>% 
    group_by(treatment, replication) %>% 
    mutate(cumulative = cumsum(damage),
           cumulative_adj = cumulative/initstnd,
           date = as.factor(date))

# Model damage over time -------------------

# Examine distribution of cumulative damage for modeling
qqPlot(field_damage_time$cumulative, 'norm') # Very clearly non-normal

# Try negative binomial
neg_binomial <- MASS::fitdistr(field_damage_time$cumulative, "Negative Binomial")
qqPlot(field_damage_time$cumulative, "nbinom", 
       size = neg_binomial$estimate[[1]], mu = neg_binomial$estimate[[2]])
mean(field_damage_time$cumulative)
var(field_damage_time$cumulative) # looks to be negative binomial

# Baseline model (with date as random effect)
null_damage_mod <- glmer.nb(cumulative ~ 1 + (1 | date),
                      data = field_damage_time)

# Evaluate just fixed effects
fixed_damage_mod <- MASS::glm.nb(cumulative ~ control_type * delia_lure, 
                                 data = field_damage_time)

# Evaluate full model
damage_mod <- glmer.nb(cumulative ~ control_type * delia_lure + (1 | date),
                 data = field_damage_time)

# Model diagnostics
summary(damage_mod)
anova(null_damage_mod, fixed_damage_mod, damage_mod)
Anova(damage_mod, type = 'III')
plot(damage_mod)
rcompanion::nagelkerke(damage_mod, null_damage_mod)
rmse <- resid(damage_mod)^2 %>% mean() %>% sqrt()
cv_rmse <- rmse/mean(field_damage_time$cumulative)

# Post-hoc
group_differences <- emmeans(damage_mod, ~ control_type * delia_lure, type = 'response') %>% 
    CLD(Letters = LETTERS, reversed = TRUE) %>%
    mutate(.group = str_trim(.group))

lure_differences <- emmeans(damage_mod, ~ control_type * delia_lure, type = 'response') %>% 
    contrast(by = 'control_type', method = 'tukey')

# Adjust dates for plotting
plot_field_damage <- field_damage_time %>% 
    mutate(plot_date = lubridate::ymd(date))

# Plot OM Cumulative damage results
dodge_width = 0.48

cumulative_om_damage_fig <- ggplot(group_differences, 
       aes(x = control_type, y = response, color = delia_lure)) + 
    geom_point(position = position_dodge(dodge_width)) + 
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                  position = position_dodge(dodge_width),
                  width = 0.24) + 
    scale_color_grey(start = 0.1, end = 0.5) + 
    labs(x = 'Treatment',
         y = 'Cumulative Onion Maggot Damage \n (Number of Dead Plants)') + 
    geom_text(aes(y = 44, label = .group),
              show.legend = FALSE,
              position = position_dodge(dodge_width)) + 
    pest_management_science +
    theme(legend.position = c(0.15, 0.75),
          axis.line.x = element_line(size = 0.48, linetype = 'solid', 
                                     color = 'black'),
          axis.line.y = element_line(size = 0.48, linetype = 'solid', 
                                     color = 'black')
    )

# Save Figure
ggsave(plot = cumulative_om_damage_fig,
       filename = './figures/raw-figures/figure4-cumulative-damage.pdf',
       width = 5, height = 3)
