##############################################
#                                            #
#       Onion Maggot Spinosad                #
#        Field OM Damage 2008                #
#                                            #
##############################################


# ACET Lab + Nault Lab @ Cornell AgriTech
# May 2019

# Load Libraries and other resources --------------
library(tidyverse)
library(car)
library(emmeans)
library(lme4)
source('./analysis/model_diagnostics.R')
source('./analysis/pest_management_science_theme.R')


# Load data -----------

field_damage <- read_csv('./data/om-spinosad-field-damage-trial-2008.csv', skip = 17)

# Preprocess data ------------------

# Treatment information
control_treatment <- tibble(
    treatment = 1:3,
    control_type = c('No Insecticide',
                     'Spinosad',
                     'Spinosad +')
)

# Look at field damage over time 
field_damage_time <- field_damage %>% 
    gather(key = 'info', value = 'damage', contains('dam')) %>%
    mutate(month = str_sub(info, 4,4),
           day = str_sub(info, 5),
           date = lubridate::mdy(paste(month, day, '2008'))) %>%
    left_join(control_treatment) %>%
    arrange(control_type, replication, date) %>% 
    group_by(control_type, replication) %>% 
    mutate(cumulative = cumsum(damage),
           date = as.factor(date))

# Model cumulative damage -------------------

# Null model for comparison
null_damage_mod <- lmer(cumulative ~ 1 + (1 | date),
                        data = field_damage_time)

# Fixed effects for comparison
fixed_damage_mod <- lm(cumulative ~ control_type,
                         data = field_damage_time)

# Full model evaluating treatment
field_damage_mod <- lmer(cumulative ~ control_type + 
                             (1 |date), 
                         data = field_damage_time)

# Examine models
plot(field_damage_mod)
summary(field_damage_mod)
anova(null_damage_mod, fixed_damage_mod, field_damage_mod)
Anova(field_damage_mod)
rcompanion::nagelkerke(field_damage_mod, null_damage_mod)
rmse <- resid(field_damage_mod)^2 %>% mean() %>% sqrt()
cv_rmse <- rmse/mean(field_damage_time$cumulative)

# Post hoc contrasts
control_evaluation <- emmeans(field_damage_mod, ~ control_type) %>% 
    CLD(Letters = LETTERS, 
        reversed = TRUE) %>% 
    mutate(.group = str_trim(.group))

# Contrasts figure
control_eval_figure <- ggplot(control_evaluation, aes(x = control_type, y = emmean)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL)) + 
    labs(x = 'Treatment',
         y = 'Cumulative Onion Maggot Damage') + 
    geom_text(aes(label = .group, y = 66)) + 
    pest_management_science

# Save figure
ggsave(plot = control_eval_figure, 
       filename = './figures/raw-figures/figure4-control-eval.pdf',
       width = 5, height = 3)
