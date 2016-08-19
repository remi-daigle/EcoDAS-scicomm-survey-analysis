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
require(magick)


# get the right data
fig2a_data <- survey_data %>% dplyr::select(starts_with("Q9"))
fig2b_data <- survey_data %>% dplyr::select(starts_with("Q2"),starts_with("Q3"),starts_with("Q4"),starts_with("Q5"),starts_with("Q6"),starts_with("Q7"),-contains("specify"))

# do stats on long format data
fig2a_data_long <- fig2a_data %>% gather()
fig2a_data_long$value <- suppressWarnings(as.numeric(fig2a_data_long$value))
# kruskal.test(value ~ key, data = fig2a_data_long)
# posthoc.kruskal.nemenyi.test(value ~ key, data = fig2a_data_long)

#### make a likert-style plot for 2a ####
#re-organize
fig2a_summary <- spread(data.frame(with(fig2a_data_long, table(key, value))),key=value,value=Freq)
#re-order to amke a better looking plot
fig2a_summary <- fig2a_summary[c(7,1,5,6,4,3,2),]
# #assign labels
# fig2a_summary$key <- sub(".+_.+_","",as.character(fig2a_summary$key))
#alternate assign labels- to make labels more self-explanatory
fig2a_summary$key <- c("Written", "Oral", "Visual\nArts", "Workshop", "Social\nMedia", "Popular\nMedia", "Other") #!!!!!

# re-gather dataframe
fig2a_summary <- fig2a_summary %>% gather(Rank,Freq,-key)

# factorize Rank so that 1 is on top
fig2a_summary$Rank <- factor(fig2a_summary$Rank,levels=7:1)


#plot
tiff('fig2a.tif',res=1200,width=8.5,height=5,units="in")

likert(key ~  Rank, value='Freq', fig2a_summary,
       as.percent=TRUE,
       ylab="Percent Relative Ranking\n Low                             Neutral                          High",
       xlab="",
       main="",
       horizontal = FALSE,
       rightAxis = FALSE,
       positive.order=T)

dev.off()

#### re-code data for 2b ####
fig2b_recoded <- fig2b_data %>% gather() %>% separate(key,sep="_",c('question','type','subtype'))
# lump frequencies
# fig2b_recoded[fig2b_recoded=="Daily"] <- "Frequently"
# fig2b_recoded[fig2b_recoded=="Weekly"] <- "Frequently"
# fig2b_recoded[fig2b_recoded=="Monthly"] <- "Frequently"  #changed Monthly to be recoded as "Frequently"
fig2b_recoded[fig2b_recoded=="Yearly or less"] <- "Yearly"
fig2b_recoded$value[is.na(fig2b_recoded$value)]<-"Never" #assumes that NAs (blanks) indicate that the activity was never done

# do stats on long format data
fig2b_stats <- fig2b_recoded
fig2b_stats$type <- as.factor(fig2b_stats$type)
kruskal.test(value ~ type, data = fig2b_stats)
posthoc.kruskal.nemenyi.test(value ~ type, data = fig2b_stats)

#summarize each type by averaging subtypes
fig2b_summary <- spread(data.frame(with(fig2b_recoded, table(paste(type,subtype,sep="_"), value))),key=value,value=Freq)
fig2b_summary <- fig2b_summary %>% 
    separate(Var1,sep="_",c('type','subtype')) %>% 
    group_by(type) %>% 
    summarize(mean_Daily=mean(Daily)/nrow(survey_data)*100,
              mean_Weekly=mean(Weekly)/nrow(survey_data)*100,
              mean_Monthly=mean(Monthly)/nrow(survey_data)*100,
              mean_Yearly=mean(Yearly)/nrow(survey_data)*100,
              mean_Never=mean(Never)/nrow(survey_data)*100)   #calculating new category for figure 2b

#re-order
fig2b_summary <- fig2b_summary[c(6,1,4,5,3,2),]

#create labels
# plot_labels <- sub(".+_.+_","",as.character(fig2b_summary$type))

#alternate create labels
plot_labels <- c("Written","Oral","Visual\nArts","Workshop","Social\nMedia","Popular\nMedia")  

#plot
# jpeg('fig2b.jpg',,width = 720, height = 480)
tiff('fig2b.tif',res=1200,width=8.5,height=5,units="in")

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE) #allows legend to be outside of plot frame
barplot(t(as.matrix(fig2b_summary[,-1])),
        beside=F,  #changed to F to stack bars
        legend = c("Daily","Weekly","Monthly","Yearly or less", "Never"), #trying out new phrases for legend
        args.legend = list(x="right",bty='n', title="Frequency", inset=c(-0.19,0)), #positions legend outside of plot, add title to legend
        names=plot_labels,
        las=1, #make labels horizontal
        ylim=c(0,100),
        ylab="Percent",
        xlab="Science Communication Category",
        col=c('#00441b', '#006d2c','#41ae76','#99d8c9','#ccece6','#f7fcfd'))
box(bty='l')

dev.off()

fig2a <- image_read("fig2a.tif")
fig2b <- image_read("fig2b.tif")
fig2 <- image_append(c(fig2a,fig2b),stack=T)
image_write(fig2,"fig2.tif")
info <- image_info(fig2)
image_write(image_scale(fig2,paste0(info$width/5,"X",info$height/5)),"fig2_small.png",format='png')
