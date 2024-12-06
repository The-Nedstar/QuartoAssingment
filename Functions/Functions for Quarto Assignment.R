#Summary Statistics
Stat <- function(Data){
  message(paste("Mean = ", round(mean(Data), digits = 3)))
  message(paste("Median = ", round(median(Data), digits = 3)))
  message(paste("Variance = ", round(var(Data), digits = 3)))
}
  
#Boxplot creation
BoxGraph <- function(Dataset, Yaxis, Ytitle, Xaxis, Xtitle, 
                     Title, Stat){
  Summary <- Dataset %>%
    group_by(Year) %>%
    summarise(
      Mean = mean(Depth),
      Lower = mean(Depth) - qt(0.975, df = n() - 1) * sd(Depth) / sqrt(n()),
      Upper = mean(Depth) + qt(0.975, df = n() - 1) * sd(Depth) / sqrt(n())
    )
  
  plot <- ggplot(Dataset, aes(y = Yaxis, x = Xaxis, 
                              colour = Xaxis)) +
    theme_bw() +
    (if (Stat != TRUE) {
      geom_boxplot(width = 0.6)
    }) +
    geom_beeswarm(dodge.width = 1.5, alpha = 0.5) +
    (if (Stat == TRUE) {
      geom_smooth(method = "lm", aes(group = 1), 
                  se = FALSE, linetype = "solid", colour = "#606060")
    }) +
    (if (Stat == TRUE) {
      geom_errorbar(data = Summary, aes(x = Year, 
                                        ymin = Lower, ymax = Upper),
                    width = 0.08, size = 0.6, color = "#606060", inherit.aes = FALSE) 
    }) +
    (if (Stat == TRUE) {
      stat_summary(fun = "mean", geom = "point", size = 4, color = "black", shape = 18) 
    }) +
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

#saving graphs as SVG for improved compatability
SaveSVG <- function(data, file, Height, Width, Scaling){
  svglite(file, width = Width,
          height = Height,
          scaling = Scaling)
  print(data)
  dev.off()
}

#creating Histograms
Histogram <- function(Dataset, Xaxis, Xtitle, Title, Num){
  plot <- ggplot(Dataset, aes(x = Xaxis)) +
    (if (Num == 1) {
      geom_histogram(colour = "black", fill = "grey", bins = 15)
    }) +
    (if (Num == 2) {
      geom_histogram(colour = "skyblue", fill = "lightblue", bins = 15)
    }) +
    (if (Num == 3) {
      geom_histogram(colour = "coral", fill = "orange", bins = 15)
    }) +
    theme_bw()+
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
          legend.position = "none")+
    xlab(Xtitle) +
    ggtitle(Title)
}
    