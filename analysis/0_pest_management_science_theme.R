###########################
#                          #
#       Figure Theme       #
#                          #
############################


# ACET Lab + Nault Lab @ Cornell AgriTech
# May 2019

pest_management_science <- theme_bw() +
    theme(legend.background = element_rect(color = 'grey30'),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line.x = element_line(size = 0.48, linetype = 'solid', 
                                     color = 'black'),
          axis.line.y = element_line(size = 0.48, linetype = 'solid', 
                                     color = 'black'),
          legend.margin = ggplot2::margin(1.24, 1.24, 1.24, 1.24),
          legend.title = element_blank()
    )
