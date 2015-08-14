#### Figure 4 ####
# REASONING:
# - show who provided/sponsored scicomm training
# - data from Q10
# - stacked bars, bottom - own institution, self, conf, etc
# - select categories to expand, e.g. oral, popular media etc. 

# get the right data
require(dplyr)
fig4_data <- survey_data %>% select(starts_with("Q10"))

# how many respondents received each type of funding (can be multiple subtypes)
n <- c(24,12,13,22,12,11)

# recode
fig4_recoded <- fig4_data
fig4_recoded[is.na(fig4_data)] <- 0
fig4_recoded[!is.na(fig4_data)] <- 1

fig4_summary <- as.data.frame(t(apply(fig4_recoded,2,function(x) sum(as.numeric(x)))))
fig4_summary <- fig4_summary %>% 
    gather() %>% 
    separate(key,c('question','category','type','subtype')) %>% 
    select(-question,-category) %>% 
    spread(subtype,value) %>% 
    filter(type!='other')

#re-scale so that columns are relative to n
fig4_summary[,-1] <- fig4_summary[,-1]/apply(fig4_summary[,-1],1,sum)*n/nrow(survey_data)*100

#re-order
fig4_summary <- fig4_summary[order(apply(fig4_summary[,-1],1,sum),decreasing = TRUE),]
fig4_summary <- fig4_summary[,c(1,1+order(as.numeric(apply(fig4_summary[,-1],2,sum))))]

#create labels
plot_labels <- as.character(fig4_summary$type)
legend_labels <- names(fig4_summary[,-1])

#plot
jpeg('fig4.jpg')
barplot(t(as.matrix(fig4_summary[,-1])),
        legend = legend_labels,
        args.legend = list(x="topright",bty='n'),
        names=plot_labels,
        las=3,
        ylim=c(0,100),
        ylab="Percent",
        col=rainbow(6))
dev.off()
