library(dplyr)
library(ggplot2)

makelogit <- function(df_source, var_of_interest, cwdfactor, cwd, varname){
  box_plot <- df_source %>% ggplot(aes_string(x = cwdfactor, #make boxplot
                          y = var_of_interest,
                          fill = cwdfactor))+ 
    geom_boxplot()+
    xlab("Coarse Woody Debris Presence")+
    ylab(varname)+
    coord_flip()+ #make horizontal
    ggtitle(paste("Boxplot of CWD by ", varname))+ #title based on variable
    scale_fill_manual(name = "CWD Presence",
                      values = c("darkgray", "red"))+ #color by CWD presence
    theme_bw()+
    theme(text = element_text(size=18)) #larger text
  
  density_plot <- df_source %>% ggplot(aes_string(x = var_of_interest, #make density plot
                          color = cwdfactor))+
    geom_line(stat = "density", size = 2)+
    scale_color_manual(name = "Coarse Woody Debris",
                       values = c("black", "red"))+ #color by CWD presence
    guides(color = guide_legend(override.aes = list(size = 6)))+
    theme(text = element_text(size = 18))+
    ggtitle(paste("Distribution of", varname))+ #title by variable name
    ylab("Density")+
    xlab(varname)
  
  density_plot_stratum <- density_plot +
    facet_wrap(~stratum)
  
  #make formula for model
  formula2 <- paste(cwd, "~", var_of_interest)
  
  #make logistic model with this formula
  model <- glm(formula = formula2, data = df_source, family = "binomial")

  #get model summary
  model_summary <- summary(model)
  
  #extract the p value
  p_value <- paste("p =", coef(summary(model))[,'Pr(>|z|)'][2])
  
  #define column to use
  xb <- df_source[,var_of_interest]
  
  temp <- as.data.frame(seq(from = min(xb, na.rm = T), 
                            to = max(xb, na.rm = T), 
                            by = (max(xb, na.rm = T)-min(xb, na.rm = T))/100)
  )
  names(temp) <- var_of_interest
  predictions <- predict.glm(model,
                            newdata = temp,
                            type = 'response',
                            se.fit = T)
  
  generated_data <- cbind(temp, predictions) #generate data to plot curve
  
  fit_var <- "fit" #define fit variable
  #make logit plot
  logit_plot <- generated_data %>% 
    ggplot(aes_string(x = var_of_interest, 
                      y = fit_var))+
    geom_ribbon(aes(ymin = fit - se.fit, 
                    ymax = fit + se.fit), 
                fill = rgb(0, 0, 0, 0.2))+
    geom_line(aes_string(y = fit_var), 
              col = "blue",
              size = 1)+
    labs(x = varname,
         y = "Probability of CWD presence",
         title = paste("Prob. of CWD by", varname))+
    scale_y_continuous(limits = c(0,1))+
    theme_bw()+
    theme(text = element_text(size=16))
  
  return(list(boxplot = box_plot,
              density_plot = density_plot,
              density_plot_stratum = density_plot_stratum,
              model = model, 
              model_summary = model_summary, 
              p_value = p_value, 
              predictions = predictions, 
              temp = temp, 
              generated_data = generated_data,
              logit_plot = logit_plot))
}
