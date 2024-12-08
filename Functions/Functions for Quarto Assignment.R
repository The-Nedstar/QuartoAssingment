### Summary Statistics
# prints out summary statistics of the inputted data to 3 d.p.
Stat <- function(Data){
  message(paste("Mean = ", round(mean(Data), digits = 3)))
  message(paste("Median = ", round(median(Data), digits = 3)))
  message(paste("Variance = ", round(var(Data), digits = 3)))
}


### Boxplot creation and saving as .svg
BoxGraph <- function(Dataset, Yaxis, Ytitle, Xaxis, Xtitle, 
                     Title, Stat, file, Height, Width, Scaling){
  # producing Summary data necessary for the model plot
  Summary <- Dataset %>%
    group_by(Year) %>%
    summarise(
      Mean = mean(Depth),
      Lower = mean(Depth) - qt(0.975, df = n() - 1) * sd(Depth) / sqrt(n()),
      Upper = mean(Depth) + qt(0.975, df = n() - 1) * sd(Depth) / sqrt(n())
    )
  # defining the plot
  plot <- ggplot(Dataset, aes(y = Yaxis, x = Xaxis, 
                              colour = Xaxis, fill = Xaxis)) +
    theme_bw() +
    scale_fill_manual(values = c("#FFF1F3","#E0F3F8"))+
    # produces a descriptive plot as standard
    (if (Stat != TRUE) {
      geom_boxplot(width = 0.6) 
    }) +
    geom_beeswarm(dodge.width = 1.5, alpha = 0.6) +
    # formats the graph as a linear model plot instead if prompted
    (if (Stat == TRUE) { 
      geom_smooth(method = "lm", aes(group = 1), 
                  se = FALSE, linetype = "solid", 
                  colour = "#606060") 
    }) +
    # Further linear model plot formatting
    (if (Stat == TRUE) { 
      geom_errorbar(data = Summary, aes(x = Year, 
                                        ymin = Lower, ymax = Upper),
                    width = 0.08, size = 0.6, color = "#606060", 
                    inherit.aes = FALSE) 
    }) +
    # Further linear model plot formatting
    (if (Stat == TRUE) { 
      stat_summary(fun = "mean", geom = "point", size = 4, 
                   color = "black", shape = 18) 
    }) +
    # setting visual formatting
    theme(axis.text.y   = element_text(size = 14, color = "black"),
          axis.text.x   = element_text(size = 14, color = "black", 
                                       angle = 45, hjust = 1),
          axis.title.y  = element_text(size = 16),
          axis.title.x  = element_text(size = 16),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 0.1),
          panel.border = element_rect(colour = "black", fill = NA,
                                      size = 1),
          plot.title = element_text(hjust = 0.02, vjust = 0.1,
                                    size = 16),
          legend.position = "none") + 
    xlab(Xtitle) +
    ylab(Ytitle) +
    ggtitle(Title)
  # saving this plot as an svg
  svglite(here("Figures", file), width = Width,
          height = Height,
          scaling = Scaling)
  print(plot)
  dev.off()
}


### creating Histograms
Histogram <- function(Dataset, Xaxis, Xtitle, Title, Num){
  # defining histogram
  plot <- ggplot(Dataset, aes(x = Xaxis)) +
    # formatting for all data
    (if (Num == 1) { 
      geom_histogram(colour = "black", fill = "lightgrey", bins = 15)
    }) + 
    # formatting for 1976
    (if (Num == 2) { 
      geom_histogram(colour = "#F8766D", fill = "#FFF1F3", bins = 15)
    }) +
    # formatting for 1978
    (if (Num == 3) { 
      geom_histogram(colour = "#00BFC4", fill = "#E0F3F8", bins = 15)
    }) +
    # defining formatting
    theme_bw() +
    theme(axis.text.y   = element_text(size = 14, color = "black"),
          axis.text.x   = element_text(size = 14, color = "black", 
                                       angle = 45, hjust = 1),
          axis.title.y  = element_text(size = 16),
          axis.title.x  = element_text(size = 16),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 0.1),
          panel.border = element_rect(colour = "black", fill = NA,
                                      size = 1),
          plot.title = element_text(hjust = 0.02, vjust = 0.1,
                                    size = 16),
          legend.position = "none") +
    xlab(Xtitle) +
    ggtitle(Title)
  # returning the plot
  return(plot)
}