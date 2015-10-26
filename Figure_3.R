#### Figure 3 ####
# REASONING:
# - showing the frequency of participation in the subcategories of important ones in Fig 2b
# - data from Q2-7
# - lump frequency
# - select categories to expand, e.g. oral, popular media etc. 

# get the right data
require(dplyr)
fig3_data <- survey_data %>% select(starts_with("Q2"),starts_with("Q3"),starts_with("Q4"),starts_with("Q5"),starts_with("Q6"),starts_with("Q7"))


#### re-code data for 2b ####
fig3_recoded <- fig3_data %>% gather() %>% separate(key,sep="_",c('question','type','subtype'))
# lump frequencies
fig3_recoded$value[is.na(fig3_recoded$value)]<-"Never"  #assumes that NAs (blanks) indicate that the activity was never done
fig3_recoded[fig3_recoded=="Daily"] <- "Frequently"
fig3_recoded[fig3_recoded=="Weekly"] <- "Frequently"
fig3_recoded[fig3_recoded=="Monthly"] <-  "Frequently"  #changed Monthly to be recoded as "Frequently"
fig3_recoded[fig3_recoded=="Yearly or less"] <- "Infrequently"


jpeg('fig3.jpg')
layout(matrix(c(1:6),ncol=2))
#loop for type
for(i in unique(fig3_recoded$type)){
    #summarize each subtype within type
    fig3_summary <- spread(data.frame(with(fig3_recoded, table(paste(type,subtype,sep="_"), value))),key=value,value=Freq)
    fig3_summary <- fig3_summary %>% 
        separate(Var1,sep="_",c('type','subtype')) %>% 
        filter(type==i) %>% 
        group_by(subtype) %>% 
        summarize(mean_Frequently=Frequently/nrow(survey_data)*100,
                  mean_Infrequently=Infrequently/nrow(survey_data)*100,
                  mean_Never=Never/nrow(survey_data)*100) #Added calculation of mean for Never
    
    #re-order
    fig3_summary <- fig3_summary[order(fig3_summary$mean_Infrequently,decreasing = TRUE),]
    
    #remove other and otherspecify from subtype list
    fig3_summary <- fig3_summary[which(fig3_summary$subtype != "other" & fig3_summary$subtype != "otherspecify" &fig3_summary$subtype != "specifyother"), ] #!!
    
    #create labels
    plot_labels <- as.character(fig3_summary$subtype)
    
    #plot
    barplot(t(as.matrix(fig3_summary[,-1])),
            beside=F,  #changed to F to stack bars
            #legend = c("Frequently","Infrequently"),   #cancelled out legend to simplify figure and save headache
            #args.legend = list(x="top",bty='n'),
            names=plot_labels,
            las=3,
            ylim=c(0,100),
            ylab="Percent")
    title(i)
}
dev.off()
