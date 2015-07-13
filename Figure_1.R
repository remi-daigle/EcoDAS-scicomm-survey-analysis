#New File Test- Figure 1

library(ggplot2)

ggplot(demo_data, aes(q20_undergrad, q21_MSc)) + geom_point(aes(color=q19_gender),size=4) + 
    stat_smooth(aes(color=q19_gender), method="lm", se=FALSE) + 
    xlab("No. of Undergrads in Lab") +
    ylab("No. of MSc Students") +    
    theme(text = element_text(size = 25), axis.text.x=element_text(colour="black"), 
          axis.text.y = element_text(colour="black"),
          panel.background = element_rect(fill = "gray92")) 
