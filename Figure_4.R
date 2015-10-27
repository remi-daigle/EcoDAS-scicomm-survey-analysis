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

# recode type
unique(fig4_summary$type)
fig4_summary$type[fig4_summary$type=="oral"]<-"Oral\nn = 24"
fig4_summary$type[fig4_summary$type=="workshop"]<-"Workshop\nn = 12"
fig4_summary$type[fig4_summary$type=="popmedia"]<-"Pop\nMedia\nn = 13"
fig4_summary$type[fig4_summary$type=="written"]<-"Written\nn = 22"
fig4_summary$type[fig4_summary$type=="visual"]<-"Visual\nn = 12"
fig4_summary$type[fig4_summary$type=="socialmedia"]<-"Social\nMedia\nn = 11"

#re-scale so that columns are relative to n
# fig4_summary[,-1] <- fig4_summary[,-1]/apply(fig4_summary[,-1],1,sum)*n/nrow(survey_data)*100

#re-order
fig4_summary <- fig4_summary[order(apply(fig4_summary[,-1],1,sum),decreasing = TRUE),]
fig4_summary <- fig4_summary[,c(1,1+order(as.numeric(apply(fig4_summary[,-1],2,sum))))]

#create labels
plot_labels <- as.character(fig4_summary$type)
legend_labels <- c("Your Univerisity/Institution",
                   "Self-guided tutorial/training",
                   "Other",
                   "Conference/Scientific Society",
                   "Another University/Institution",
                   "Government")

#plot
jpeg('fig4.jpg')
par(mar=c(6, 4, 0, 0) + 0.1)
mp <- barplot(t(as.matrix(fig4_summary[,-1])),
        legend = legend_labels,
        args.legend = list(x="topright",bty='n'),
        ylim=c(0,50),
        ylab="Total Number of Responses",
        col=rainbow(6),
        axes = FALSE,
        names=rep("",6))
mtext("Science Communication Category (n = # of individuals)",1,5)
axis(2)
axis(1,at=mp,labels=rep("",6))
axis(1,labels=plot_labels,at=mp,pos=c(-4,0),lwd=0)
box(bty='l')
dev.off()
