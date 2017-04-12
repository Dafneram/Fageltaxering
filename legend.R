b<-list()
for(i in 1:length(Skogsfåglar)){
  art<-Skogsfåglar[2]
  b2<-    ggplot(subset(Total,arthela %in% art), aes(x=År)) + 
    geom_line(aes(y = var2, color = "Norra Norrland", linetype = "Norra Norrland")) +
    geom_line(aes(y = var3, color = "Södra Norrland", linetype = "Södra Norrland")) +
    geom_line(aes(y = var4, color = "Östra Svealand", linetype = "Östra Svealand")) +
    geom_line(aes(y = var5, color = "Västra Göta/Svealand", linetype = "Västra Göta/Svealand")) +
    geom_line(aes(y = var6, color = "Östra Götaland", linetype = "Östra Götaland")) +
    geom_line(aes(y = var7, color = "Södra Götaland", linetype = "Södra Götaland")) +
    geom_line(aes(y = var1, color = "Hela Sverige", linetype = "Hela Sverige")) +
    scale_colour_manual("", breaks = c("Norra Norrland","Södra Norrland","Östra Svealand","Västra Göta/Svealand","Östra Götaland","Södra Götaland","Hela Sverige"),
                        values = c("Norra Norrland" = "#D55E00", "Södra Norrland" = "#0072B2", "Östra Svealand" = "#E69F00", "Västra Göta/Svealand" = "#009E73", "Östra Götaland" = "#56B4E9", "Södra Götaland" = "#CC79A7","Hela Sverige" = "#000000")) +
    scale_linetype_manual("", breaks = c("Norra Norrland","Södra Norrland","Östra Svealand","Västra Göta/Svealand","Östra Götaland","Södra Götaland","Hela Sverige"),
                          values = c("Norra Norrland" = 1, "Södra Norrland" = 1, "Östra Svealand" = 1, "Västra Göta/Svealand" = 1, "Östra Götaland" = 1, "Södra Götaland" = 1,"Hela Sverige" = 2)) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm")) +
    theme(legend.margin=unit(-0.6,"cm")) +
    ggtitle(paste(art)) +
    guides(linetype=guide_legend(keywidth = 3.2, keyheight = 1, ncol=3),
           colour=guide_legend(keywidth = 3.2, keyheight = 1, ncol=3, override.aes = list(size=1)))

}

colour = guide_legend(override.aes = list(size=4))


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(b2)

grid.arrange(mylegend, b[[9]],b[[10]],b[[11]],b[[12]],b[[13]],b[[14]],b[[15]],b[[16]], nrow=4, ncol=2)
grid.arrange(mylegend, mylegend, b[[17]],b[[18]],b[[19]],b[[20]],b[[21]],b[[22]],b[[23]],b[[24]], nrow=5, ncol=2)
grid.arrange(b[[25]],b[[26]],b[[27]],b[[28]],b[[29]],b[[30]],b[[31]],b[[32]], nrow=4, ncol=2)
grid.arrange(b[[33]],b[[34]],b[[35]],b[[36]],b[[37]],b[[38]],b[[39]],b[[40]], nrow=4, ncol=2)
grid.arrange(b[[41]],b[[42]],b[[43]],b[[44]],b[[45]],b[[46]],b[[47]],b[[48]], nrow=4, ncol=2)
grid.arrange(b[[49]],b[[50]],b[[51]],b[[52]],b[[53]],b[[54]],b[[55]],b[[56]], nrow=4, ncol=2)
grid.arrange(b[[57]],b[[58]], nrow=4, ncol=2)