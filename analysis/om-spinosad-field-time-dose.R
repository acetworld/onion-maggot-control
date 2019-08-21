##############################################
#                                            #
#       Onion Maggot Spinosad                #
#           Field OM Catch                   #
#                                            #
##############################################


# ACET Lab + Nault Lab @ Cornell AgriTech
# May 2019

# Load Libraries and other resources --------------
library(tidyverse)
library(car)
library(emmeans)
source('./analysis/model_diagnostics.R')
source('./analysis/pest_management_science_theme.R')

# Load Data ------------------
field_trap_catch <- read_csv('./data/om-spinosad-field-time-dose-trial-2006.csv', skip = 8)

# Preprocess Data --------------------

# Reorganize data for plotting
daily_trap_catch <- field_trap_catch %>% 
    gather(key = 'info', value = 'catch', -replication) %>% 
    mutate(sex = if_else(str_sub(info, 1, 3) == 'fem',
                         'Females', 'Males'),
           month = str_sub(info, 4, 4),
           day = str_sub(info, 5),
           date = lubridate::mdy(paste(month, day, '2006')),
           catch = catch * 3)

# Plot figure -----------------

# Generate figure
population_figure <- ggplot(daily_trap_catch, aes(x = date, y = catch, shape = sex, 
                     color = sex, linetype = sex, fill = sex)) + 
    stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) + 
    geom_point(alpha = 0.48) + 
    scale_color_grey() +
    scale_fill_grey() + 
    scale_x_date(expand = c(0,0)) +
    labs(x = '', y = 'Onion Maggot Trap Catch') + 
    pest_management_science + 
    theme(legend.position = c(0.85, 0.75),
          axis.line.x = element_line(size = 0.48, linetype = 'solid', 
                                     color = 'black'),
          axis.line.y = element_line(size = 0.48, linetype = 'solid', 
                                     color = 'black'))

# Save Figure
ggsave(plot = population_figure,
       filename = './figures/raw-figures/figure0_population_figure.pdf',
       width = 5, height = 3)
