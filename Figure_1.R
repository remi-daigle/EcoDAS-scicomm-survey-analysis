#### Figure 1 ####
# REASONING:
# - show the overall importance of scicomm relative to other things that are taught
# - data from Q8
# - scicomm of ~ equal rank to in-department curriculum
# - also needs statistics

# load packages (install these if you don't have them)
require(dplyr)
require(tidyr)
require(PMCMR)
require(HH)

# get the right data
fig1_data <- data.frame(survey_data %>% select(starts_with("Q8")))

# do stats on long format data
fig1_data_long <- fig1_data %>% gather()
fig1_data_long$value <- suppressWarnings(as.numeric(fig1_data_long$value))
kruskal.test(value ~ key, data = fig1_data_long)
posthoc.kruskal.nemenyi.test(value ~ key, data = fig1_data_long)

#### make a likert-style plot ####
#re-organize
fig1_summary <- spread(data.frame(with(fig1_data_long, table(key, value))),key=value,value=Freq)
#re-order to amke a better looking plot
fig1_summary <- fig1_summary[c(3,7,5,1,2,6,4),]
#assign labels
fig1_summary$key <- sub(".+_.+_","",as.character(fig1_summary$key))
#plot
jpeg('fig1.jpg')
likert(key ~ . , fig1_summary,
       as.percent=TRUE,
       ylab=NULL,
       main="Ranking of graduate training")

dev.off()

# #### make a normal barplot with mean rank ####
# fig1_summary <- fig1_data_long %>%
#     group_by(key) %>%
#     summarize(mean_rank=mean(value,na.rm=TRUE),
#               median_rank=median(value,na.rm=TRUE),
#               sd_rank=sd(value,na.rm=TRUE)) %>% 
#     arrange(mean_rank)
# 
# hh <- fig1_summary$mean_rank
# se <- fig1_summary$sd_rank
# mp <- barplot(hh,las=3,ylim=c(0,1.2*max(hh+se)),cex.axis=1)
# segments(mp, hh, mp, hh + se)
# segments(mp+0.2, hh + se, mp-0.2, hh + se)
# axis(1,at=mp,labels=F)
# plot_labels <- sub(".+_.+_","",as.character(fig1_summary$key))
# text(cex=1, x=mp+0.5, y=-0.5, plot_labels, xpd=TRUE, srt=50, pos=2)
# 
# #### make a boxplot ####
# plot_labels <- sub(".+_.+_","",as.character(unique(fig1_data_long$key)))
# boxplot(value~key, data=fig1_data_long,names=plot_labels,las=3)
