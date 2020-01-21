################################################
#                                              #
#       Model Diagnostic Helper Function       #
#                                              #
################################################


# ACET Lab + Nault Lab @ Cornell AgriTech
# May 2019

model_diagnostics <- function(linear_model, to_plot = TRUE) {
    print('Model Summary')
    print('------------------')
    print(summary(linear_model))
    print('')
    
    print('ANOVA')
    print('------------------')
    print(car::Anova(linear_model, type = 'III'))
    print('')
    
    
    print('Information Criteria')
    print('------------------')
    print('AIC')
    print(AIC(linear_model))
    print('BIC')
    print(BIC(linear_model))
    
    print('Non-constant Variance Score Test')
    print('------------------')
    print(car::ncvTest(linear_model))
    print('')
    
    if(to_plot) {
        car::qqPlot(linear_model, type = 'III')
        car::crPlots(linear_model)
        plot(linear_model, which = 1:6)
    }
}