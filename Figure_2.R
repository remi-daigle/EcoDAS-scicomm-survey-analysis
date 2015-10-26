#### Figure 2 ####
# REASONING:
# - show a) the importance of the scicomm categories; b) the frequency of participation
# - data from a) Q9, b)Q2-7
# - lump frequency
# - social media ranked low
# - oral - parallel w/ freq
# - written - ranked high, less freq
# - a) is the summary of the major categories
# - b) (and or c, d, etc) are the finer categories within the more important major categories

# load packages
require(dplyr)
require(tidyr)

# get the right data
fig2a_data <- survey_data %>% select(starts_with("Q9"))
fig2b_data <- survey_data %>% select(starts_with("Q2"),starts_with("Q3"),starts_with("Q4"),starts_with("Q5"),starts_with("Q6"),starts_with("Q7"),-contains("specify"))

# do stats on long format data
fig2a_data_long <- fig2a_data %>% gather()
fig2a_data_long$value <- suppressWarnings(as.numeric(fig2a_data_long$value))
kruskal.test(value ~ key, data = fig2a_data_long)
posthoc.kruskal.nemenyi.test(value ~ key, data = fig2a_data_long)

#### make a likert-style plot for 2a ####
#re-organize
fig2a_summary <- spread(data.frame(with(fig2a_data_long, table(key, value))),key=value,value=Freq)
#re-order to amke a better looking plot
fig2a_summary <- fig2a_summary[c(4,1,5,2,6,3,7),]
# #assign labels
# fig2a_summary$key <- sub(".+_.+_","",as.character(fig2a_summary$key))
#alternate assign labels- to make labels more self-explanatory
fig2a_summary$key <- c("Written", "Oral", "Visual", "Workshop", "Social\nMedia", "Popular\nMedia", "Other") #!!!!!

# re-gather dataframe
fig2a_summary <- fig2a_summary %>% gather(Rank,Freq,-key)

# factorize Rank so that 1 is on top
fig2a_summary$Rank <- factor(fig2a_summary$Rank,levels=7:1)


#plot
jpeg('fig2a.jpg',width = 720, height = 480)
likert(key ~  Rank, value='Freq', fig2a_summary,
       as.percent=TRUE,
       ylab="Percent Relative Ranking\n Low                                    Neutral                                 High",
       xlab="Science Communication Category",
       main="Ranking of Science Communication Training",
       horizontal = FALSE,
       rightAxis = FALSE,
       positive.order=T)
dev.off()

#### re-code data for 2b ####
fig2b_recoded <- fig2b_data %>% gather() %>% separate(key,sep="_",c('question','type','subtype'))
# lump frequencies
fig2b_recoded[fig2b_recoded=="Daily"] <- "Frequently"
fig2b_recoded[fig2b_recoded=="Weekly"] <- "Frequently"
fig2b_recoded[fig2b_recoded=="Monthly"] <- "Frequently"  #changed Monthly to be recoded as "Frequently"
fig2b_recoded[fig2b_recoded=="Yearly or less"] <- "Infrequently"
fig2b_recoded$value[is.na(fig2b_recoded$value)]<-"Never" #assumes that NAs (blanks) indicate that the activity was never done

#summarize each type by averaging subtypes
fig2b_summary <- spread(data.frame(with(fig2b_recoded, table(paste(type,subtype,sep="_"), value))),key=value,value=Freq)
fig2b_summary <- fig2b_summary %>% 
    separate(Var1,sep="_",c('type','subtype')) %>% 
    group_by(type) %>% 
    summarize(mean_Frequently=mean(Frequently)/nrow(survey_data)*100,
              mean_Infrequently=mean(Infrequently)/nrow(survey_data)*100,
              mean_Never=mean(Never)/nrow(survey_data)*100)   #calculating new category for figure 2b

#re-order
fig2b_summary <- fig2b_summary[c(4,1,5,2,6,3),]

#create labels
plot_labels <- sub(".+_.+_","",as.character(fig2b_summary$type))

#alternate create labels
plot_labels <- c("Visual\nArts","Oral","Workshop", "Popular\nMedia", "Written", "Social\nMedia")  

#plot
jpeg('fig2b.jpg')
par(mar=c(5.1, 4.1, 7.1, 2.1), xpd=TRUE) #allows legend to be outside of plot frame
barplot(t(as.matrix(fig2b_summary[,-1])),
        beside=F,  #changed to F to stack bars
        legend = c("At Least Monthly","Yearly or More", "Never"), #trying out new phrases for legend
        args.legend = list(x="top",bty='y', title="Frequency", inset=c(0,-0.25)), #positions legend outside of plot, add title to legend
        names=plot_labels,
        las=1, #make labels horizontal
        ylim=c(0,100),
        ylab="Percent",
        xlab="Communication Category")

dev.off()
