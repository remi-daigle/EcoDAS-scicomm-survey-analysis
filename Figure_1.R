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
require(magick)


# get the right data
fig1_data <- data.frame(survey_data %>% dplyr::select(starts_with("Q8")))

# do stats on long format data
fig1_data_long <- fig1_data %>% gather(type,Rank)
fig1_data_long$Rank <- suppressWarnings(as.numeric(fig1_data_long$Rank))
# kruskal.test(Rank ~ type, data = fig1_data_long)
# posthoc.kruskal.nemenyi.test(Rank ~ type, data = fig1_data_long)

#### make a likert-style plot ####
#re-organize
fig1_summary <-  spread(data.frame(with(fig1_data_long, table(type, Rank))),key=Rank,value=Freq)
#re-order to make a better looking plot
fig1_summary <- fig1_summary[c(3,7,5,1,2,6,4),]
#assign labels
# fig1_summary$type <- sub(".+_.+_","",as.character(fig1_summary$type))

#alternate assign labels- to make labels more self-explanatory
fig1_summary$type <- c( "Educational", "Science\nComm.", "Job,\nInterview","Curriculum\n(In Dept.)","Curriculum\n(Outside Dept.)", "Laboratory","Grants,\nFunding") #!!!!!

# re-gather dataframe (This for some aweful reason seems to be the only way to get "Rank" above the legend)
fig1_summary <- fig1_summary %>% gather(Rank,Freq,-type)

# factorize Rank so that 1 is on top
fig1_summary$Rank <- factor(fig1_summary$Rank,levels=7:1)

#plot
tiff('fig1.tif',res=600,width=8.5,height=5,units="in", compression = "lzw")

likert(type ~ Rank , value='Freq', data=fig1_summary,
       as.percent=TRUE,
       ylab="Percent Relative Ranking\n Low                                    Neutral                                 High",
       xlab="Professional Development Training Category", 
       main=FALSE,
       horizontal = FALSE,
       rightAxis = FALSE,
       positive.order=T,
       scales = list(y = list(cex = 1), x = list(cex=1)),
       auto.key=list(cex=1))
dev.off()

fig <- image_read("fig1.tif")
info <- image_info(fig)
image_write(image_scale(fig,paste0(info$width/5,"X",info$height/5)),"fig1_small.png",format='png')

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
