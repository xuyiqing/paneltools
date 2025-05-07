#' @title matching.CIplot  
#'  
#' @description \code{matching.CIplot} visualize the dynamic treatment effects of the match episode 
#' and create an event study plot.  
#'  
#' @param df A dataframe containing the periods, estimates, and the CI
#'  
#' @return \code{matching.CIplot} return a ggplot object of the event study plot.
#' @author Hongyi Jiang <hyjiang2017@nsd.pku.edu.cn>  
#'  
#' @export  
matching.CIplot <- function(df){
  p <- ggplot(df, aes(x = !!sym("Periods"), y = !!sym("Coefs")))
  p <- p + 
    geom_line(color = "lightcoral") +    
    geom_point(color = "red") +   
    geom_errorbar(aes(ymin = !!sym("CI.lower"), ymax = !!sym("CI.upper")), width = 0.2, color = "black") +   
    labs(x = "Time", y = "Coefficients", title = "Event Study Plot") +  
    geom_hline(yintercept = 0, color = "gray", linewidth = 0.2) +    
    theme_minimal() +
    theme(  
      # Bolden axis lines using linewidth  
      axis.line = element_line(linewidth = 0.4, color = "black"),  
      
      # Remove grid lines  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      
      # Bolden axis ticks  
      axis.ticks = element_line(linewidth = 0.4),  
      
      # Customize axis text  
      axis.text = element_text(size = 8, face = "bold"),  
      
      # Customize axis titles  
      axis.title = element_text(size = 8, face = "bold") 
    )
  return(p)
}