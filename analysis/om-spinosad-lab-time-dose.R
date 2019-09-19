##############################################
#                                            #
#       Onion Maggot Spinosad                #
#           Lab Trials                       #
#                                            #
##############################################


# ACET Lab + Nault Lab @ Cornell AgriTech
# May 2019

# Load Libraries and other resources --------------
library(tidyverse)
library(car)
library(emmeans)
library(ggpubr)
source('./analysis/model_diagnostics.R')
source('./analysis/pest_management_science_theme.R')

# Load Data ------------------

lab_trial_2006 <- read_csv('./data/om-spinosad-lab-time-dose-trial-2006.csv', 
                           skip = 8)

# Preprocess data ------------------

# Pull out just the mortality by day for 2006 trial
mortality_2006 <- lab_trial_2006 %>% 
    select(replication, rate, period, 
           contains('female_'), contains('male_')) %>% 
    gather(key = 'evaluation', value = 'mortality', 
           -replication, -rate, -period) %>% 
    separate(evaluation, c('sex', 'day'))

# Pull out just the survivors for 2006
survivors_2006 <- lab_trial_2006 %>%
    select(replication, rate, period,
           contains('total_alive')) %>% 
    gather(key = 'type', value = 'alive', 
           contains('total_alive')) %>% 
    separate(type, c('total', 'status', 'sex')) %>% 
    select(-total, -status)

# Combine mortality and survivorship data
trial_2006 <- left_join(mortality_2006, survivors_2006)

# Model 2006 Data ---------------------

# Translate periods into number of months spheres left in the field
period_time_info <- tibble(period = c('JUN', 'JUL', 'AUG', 'SEP'),
                 month = 1:4)

# 2006 Mortality Rate
mortality_rate_2006 <- trial_2006 %>% 
    group_by(replication, rate, period, sex) %>% 
    summarize(dead = sum(mortality),
              alive = first(alive)) %>% 
    mutate(total = dead + alive,
           mortality_rate = dead/total) %>% 
    left_join(period_time_info) %>% 
    mutate(labels = factor(rate, levels = c('CK', 'LO', 'HI'),
                           labels = c('No Spinosad', 
                                      '0.5% Spinosad', 
                                      '1% Spinosad')))

# Linear model relating mortality to rate and month 
mortality_mod_2006 <- lm(mortality_rate ~ rate + month, 
                    data = mortality_rate_2006)
model_diagnostics(mortality_mod_2006, to_plot = FALSE)


# Raw data figure ---------------------------

# Set seed for reproducibility
set.seed(72)
dodge_width = 0.48
jitter_width = 0.12

# Plot figure
figure1a_lab_mortality <- ggplot(mortality_rate_2006, aes(x = month, y = mortality_rate, 
                                color = labels, shape = labels)) + 
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
    scale_color_grey() + 
    labs(x = 'Months of Exposure', y = 'Mortality of Onion Maggot Fly Adults') + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    theme_bw() + 
    theme(legend.background = element_rect(color = 'grey30'),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line.x = element_line(size = 0.48, linetype = 'solid', 
                                     color = 'black'),
          axis.line.y = element_line(size = 0.48, linetype = 'solid', 
                                     color = 'black'),
          legend.margin = ggplot2::margin(1.48, 1.48, 1.48, 1.48),
          legend.title = element_blank(),
          legend.position = 'none'
          )

# Extract legend for later formatting
figure1a_legend <- get_legend(figure1a_lab_mortality + theme(legend.position = 'bottom'))

# Save legend
ggsave(plot = figure1a_legend, 
       filename = './figures/raw-figures/figure1a_legend.pdf')

# Save figure
ggsave(plot = figure1a_lab_mortality, 
       filename = './figures/raw-figures/figure1a_lab_mortality.pdf',
       width = 5, height = 3)


# Rate Main Effects Figure ----------------------

# Post-hoc CLD/Tukey examination of marginal means for rate
rate_effect_2006 <- emmeans(mortality_mod_2006, ~ rate, type = 'response') %>% 
    CLD(Letters = LETTERS) %>% 
    data.frame() %>% 
    mutate(labels = factor(rate, levels = c('CK', 'LO', 'HI'),
                           labels = c('No Spinosad', 
                                      '0.5% Spinosad', 
                                      '1% Spinosad')))

emmeans(mortality_mod_2006, ~ rate, type = 'response') %>% 
    contrast(method = 'dunnett')

# Plot figure
figure1b_lab_rate <- ggplot(rate_effect_2006, aes(x = labels, y = emmean)) + 
    geom_point() + 
    geom_text(aes(x = labels, y = 0.64, label = .group)) + 
    geom_errorbar(aes(x = labels, ymin = lower.CL, ymax = upper.CL),
                  width = 0.36) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    labs(x = 'Treatment', y = 'Mortality of Onion Maggot Fly Adults') + 
    pest_management_science

# Save figure
ggsave(plot = figure1b_lab_rate, 
       filename = './figures/raw-figures/figure1b_lab_rate.pdf',
       width = 5, height = 3)

# Time Main Effects Figure -------------------------

# Get predictions and confidence intervals by month
month_predictions <- predict(mortality_mod_2006,
                             newdata = tibble(rate = 'LO',
                                        month = seq(1, 4, by = 0.2)),
        interval = 'confidence') %>% 
    data.frame() %>% 
    mutate(month = seq(1, 4, by = 0.2)) 

# Plot figure
figure1c_time_mortality <- ggplot(month_predictions, aes(x = month, y = fit)) + 
    geom_line() + 
    geom_ribbon(aes(x = month, ymax = upr, ymin = lwr),
                alpha = 0.5) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    labs(x = 'Months of Exposure', y = 'Mortality of Onion Maggot Fly Adults') + 
    pest_management_science

# Save figure
ggsave(plot = figure1c_time_mortality, 
       filename = './figures/raw-figures/figure1c_time_mortality.pdf',
       width = 5, height = 3)

# Examine cage trial time trends --------------------

# Extract out total numbers of flies used in trials
total_ind <- mortality_rate_2006 %>% 
    select(replication, rate, period, sex, total)

# Combine totals with daily mortality data for 
# both males and females
daily_mortality <- left_join(mortality_2006, total_ind) %>% 
    group_by(replication, rate, period, day) %>% 
    summarize(mortality = sum(mortality),
              total = sum(total)) %>% 
    ungroup() %>% 
    mutate(mortality_rate = mortality/total,
           hours = as.numeric(day) * 24,
           period = factor(period, 
                           levels = c('JUN', 'JUL','AUG','SEP'),
                           labels = c('June', 'July', 'August','September')
                           ), 
           rate = factor(rate,
                         levels = c('HI', 'LO', 'CK'),
                         labels = c('1% Spinosad',
                                    '0.5% Spinosad',
                                    'No Spinosad'))
    )

# Plot daily mortality
daily_mortality_figure <- ggplot(daily_mortality, aes(x = hours, y = mortality_rate)) + 
    facet_grid(rate ~ period) +
    geom_smooth() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = seq(0, 0.5, 0.1)) + 
    scale_x_continuous(breaks = c(0,24,48,72)) + 
    labs(y = 'Onion Maggot Mortality Rate',
         x = 'Hours Post Introduction') + 
    theme_bw() 

# Save daily mortality figure
ggsave(plot = daily_mortality_figure,
       filename = './figures/raw-figures/figure2-daily-mortality.pdf',
       width = 8, height = 5)


# Model 2007 Data -------------------
lab_trial_2007 <- read_csv('./data/om-spinosad-lab-time-dose-trial-2007.csv', 
                           skip = 8)

# Pull out just survivorship data from 2007 
mortality_2007 <- lab_trial_2007 %>% 
    select(replication, treatment, 
           contains('female_'), contains('male_')) %>% 
    gather(key = 'evaluation', value = 'mortality', 
           -replication, -treatment) %>% 
    separate(evaluation, c('sex', 'day'))

# Pull out just survivor information from 2007
survivors_2007 <- lab_trial_2007 %>%
    select(replication, treatment,
           contains('total_alive')) %>% 
    gather(key = 'type', value = 'alive', 
           contains('total_alive')) %>% 
    separate(type, c('total', 'status', 'sex')) %>% 
    select(-total, -status)

# Combine mortality and survivorship info for 2007
trial_2007 <- left_join(mortality_2007, survivors_2007)

# 2007 Mortality Rate
mortality_rate_2007 <- trial_2007 %>% 
    group_by(replication, treatment, sex) %>% 
    summarize(dead = sum(mortality),
              alive = first(alive)) %>% 
    mutate(total = dead + alive,
           mortality_rate = dead/total,
           treatment_desc = if_else(treatment == 'UNT', 'control', 'spinosad'),
           month = if_else(treatment == 'UNT', 0, 1, 
                           if_else(treatment == 'JUN', 1, 
                                   if_else(treatment == 'JUL', 2, 
                                           if_else(treatment == 'AUG', 3,4))))) 

# Does treatment increase mortality over controls?
t.test(mortality_rate ~ treatment_desc, mortality_rate_2007, 
       alternative = 'less')
