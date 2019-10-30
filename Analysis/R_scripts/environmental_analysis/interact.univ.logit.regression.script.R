#Stratum and one other variable: logistic regression to predict CWD
library(dplyr)
library(ggplot2)
source("color_schemes.R")

makelogit.interact <- function(df_source, var_of_interest, cwdfactor, cwd, varname, stratum){
  #make density plot
  density_plot_stratum <- df_source %>% ggplot(aes_string(x = var_of_interest,
                                                  color = cwdfactor))+
    geom_line(stat = "density", size = 1.5)+
    scale_color_manual(name = "Coarse Woody Debris",
                       values = c("black", "red"))+ #color by CWD presence
    guides(color = guide_legend(override.aes = list(size = 6)))+
    theme(text = element_text(size = 18))+
    ggtitle(paste("Distribution of", varname))+ #title by variable name
    ylab("Density")+
    xlab(varname)+
    facet_wrap(~stratum)+
    theme_bw()+
    theme(legend.position = c(0.83, 0.25))
    #theme(strip.background =element_rect(fill="red"))
      #This last line makes all the facet headings red. 
      #Is there a way to change the individual facet headings to different colors?
  
  #make formula for model
  formula3 <- paste0(cwd, "~", var_of_interest, "+", var_of_interest, "*", stratum)
  
  #make logistic model with this formula
  model <- glm(formula = formula3, data = df_source, family = "binomial")
  
  #get model summary
  model_summary <- summary(model)
  
  #define column to use
  xb <- df_source[,var_of_interest]
  xs <- df_source[,stratum]
  
  temp <- seq(from = min(xb, na.rm = T), 
              to = max(xb, na.rm = T), 
              by = (max(xb, na.rm = T) - min(xb, na.rm = T))/100
  )
  
  generated_data <- as.data.frame(expand.grid(var = temp, stratum = levels(xs)))
  names(generated_data) <- c(var_of_interest, stratum)
  
  fit <- predict.glm(model,
                     newdata = generated_data,
                     type = 'response',
                     se.fit = TRUE)
  
  results <- cbind(generated_data, fit)
  
  fit_var <- "fit" #define fit variable
  #make logit plot
  logit_plot <- results %>% 
    ggplot(aes_string(x = var_of_interest, 
                      y = fit_var,
                      group = stratum))+
    geom_ribbon(aes(ymin = fit - se.fit, 
                    ymax = fit + se.fit, fill = stratum),
                alpha = 0.2)+
    geom_line(size = 1.5, aes(color = stratum))+
    labs(x = varname,
         y = "Probability of CWD presence",
         title = paste("Prob. of CWD by", varname))+
    scale_y_continuous(limits = c(0,1))+
    theme_bw()+
    scale_color_manual(name = strata_codes, 
                       values = strataColors_codes)+
    scale_fill_manual(name = strata_codes,
                      values = strataColors_codes)+
    theme(text = element_text(size=14))
  
  return(list(densityplot_stratum = density_plot_stratum,
              model = model, 
              model_summary = model_summary,
              predictions = results,
              logit_plot = logit_plot))
}
