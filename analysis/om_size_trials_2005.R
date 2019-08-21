library(tidyverse)
library(car)
library(emmeans)
source('./analysis/model_diagnostics.R')


size_trial <- read_csv('./data/om_size_trials_2005.csv', 
                       skip = 25)

treatment_info <- tibble(
    treatment = 5:9,
    #size is diameter of sphere in centimeters
    size = factor(c('7.5', '5.0', '6.25', '8.75', '10.0'),
                         levels = c('10.0', '8.75', '7.5', '6.25', '5.0'))
)

size_trial_treatment <- size_trial %>% 
    gather(sex, flies, males, females) %>% 
    left_join(treatment_info)

size_mod <- lm(flies ~ sex * size, data = size_trial_treatment)

model_diagnostics(size_mod)

emmeans(size_mod, ~ sex * size) %>% contrast(by = 'sex', method = 'dunnett')
emmeans(size_mod, ~ sex * size) %>% contrast(simple = 'sex', method = 'tukey')
emmeans(size_mod, ~ sex * size) %>% cld(details = TRUE)

stat_labels <- emmeans(size_mod, ~ sex * size) %>% 
    cld() %>% data.frame() %>% 
    filter(sex == 'females') %>% 
    mutate(label = c('B', 'B', 'AB', 'AB', 'A'))

dodge_width = 0.48
jitter_width = 0.12
ggplot(size_trial_treatment, aes(x = size, y = flies, color = sex)) + 
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
              aes(x = size, y= 72, label = label),
              show.legend = FALSE) + 
    scale_color_grey() + 
    theme_bw() + 
    labs(x = 'Sphere Trap Diameter (cm)', y = 'Onion Maggot Fly Adults') + 
    theme(legend.title = element_blank(),
          legend.position = c(0.75, 0.75),
          legend.background = element_rect(color = 'grey30'))
