#Boxplot creation
BoxGraph <- function(Dataset, Yaxis, Ytitle, Xaxis, Xtitle, 
                     Title, Stat){
  plot <- ggplot(Dataset, aes(y = Yaxis, x = Xaxis, 
                              colour = Xaxis)) +
    theme_bw() +
    (if (Stat == TRUE) {
      geom_smooth(method = "lm", aes(group = 1), 
                  se = FALSE, linetype = "solid")
    }) +
    (if (Stat != TRUE) {
      geom_boxplot(width = 0.7)
    }) +
    geom_beeswarm(dodge.width = 1.5) +
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
  return(plot)
}


