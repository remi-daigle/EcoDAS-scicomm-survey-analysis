#### Figure 4 ####
# REASONING:
# - show who provided/sponsored scicomm training
# - data from Q10
# - stacked bars, bottom - own institution, self, conf, etc
# - select categories to expand, e.g. oral, popular media etc. 

# get the right data
require(dplyr)
fig3_data <- survey_data %>% dplyr::select(starts_with("Q10"))

# how many respondents received each type of funding (can be multiple subtypes)
n <- c(24,12,13,22,12,11)

# recode
fig3_recoded <- fig3_data
fig3_recoded[is.na(fig3_data)] <- 0
fig3_recoded[!is.na(fig3_data)] <- 1

require(tidyr)  #necessary for creating summary
fig3_summary <- as.data.frame(t(apply(fig3_recoded,2,function(x) sum(as.numeric(x)))))
fig3_summary <- fig3_summary %>% 
    gather() %>% 
    separate(key,c('question','category','type','subtype')) %>% 
    dplyr::select(-question,-category) %>% 
    spread(subtype,value) %>% 
    filter(type!='other')

# recode type
unique(fig3_summary$type)
fig3_summary$type[fig3_summary$type=="oral"]<-"Oral\nn = 24"
fig3_summary$type[fig3_summary$type=="workshop"]<-"Workshop\nn = 12"
fig3_summary$type[fig3_summary$type=="popmedia"]<-"Popular\nMedia\nn = 13" 
fig3_summary$type[fig3_summary$type=="written"]<-"Written\nn = 22"
fig3_summary$type[fig3_summary$type=="visual"]<-"Visual\nn = 12"
fig3_summary$type[fig3_summary$type=="socialmedia"]<-"Social\nMedia\nn = 11"

#re-scale so that columns are relative to n
# fig3_summary[,-1] <- fig3_summary[,-1]/apply(fig3_summary[,-1],1,sum)*n/nrow(survey_data)*100

#re-order
fig3_summary <- fig3_summary[order(apply(fig3_summary[,-1],1,sum),decreasing = TRUE),]
fig3_summary <- fig3_summary[,c(1,1+order(as.numeric(apply(fig3_summary[,-1],2,sum))))]

# do stats on summary format data
fig3_stats <- fig3_recoded %>% 
    gather() %>% 
    separate(key, c('question','category','type','subtype'), sep="_") %>% 
    dplyr::select(-question,-category)
fig3_stats$value <- factor(fig3_stats$value)
mylogit <- glm(value ~ type+subtype, data = fig3_stats, family = "binomial")
summary(mylogit)
confint(mylogit)


#create labels
plot_labels <- as.character(fig3_summary$type)
legend_labels <- c("Government",
                   "Another University/Institution",
                   "Conference/Scientific Society",
                   "Other",
                   "Self-guided Tutorial/Training",
                   "Your University/Institution")  #revised order

#plot
require(RColorBrewer)
display.brewer.all(5,colorblindFriendly = T)
tiff('fig3.tif',res=600,width=8.5,height=5,units="in", compression = "lzw")
par(mar=c(6, 4, 1, 0) + 0.1)
mp <- barplot(t(as.matrix(fig3_summary[,-1])),
        legend = legend_labels,
        args.legend = list(x="topright",bty='n'),
        ylim=c(0,50),
        ylab="Total Number of Responses",
        col=brewer.pal(6,'Paired')[c(6,5,4,3,2,1)],
        #col=brewer.pal(6,'RdYlBu')[c(6,1,5,2,4,3)],   #Alternative 1 
        #col=brewer.pal(6,'Set2')[c(1,6,2,3,4,5)],    #Alternative 2 
        axes = FALSE,
        names=rep("",6))
mtext("Science Communication Category (n = # of individuals)",1,5)
axis(2)
axis(1,at=mp,labels=rep("",6))
axis(1,labels=plot_labels,at=mp,pos=c(-4,0),lwd=0)
box(bty='l')
dev.off()

fig <- image_read("fig3.tif")
info <- image_info(fig)
image_write(image_scale(fig,paste0(info$width/5,"X",info$height/5)),"fig3_small.png",format='png')
