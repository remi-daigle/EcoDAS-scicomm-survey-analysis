#### Figure 3 ####
# REASONING:
# - showing the frequency of participation in the subcategories of important ones in Fig 2b
# - data from Q2-7
# - lump frequency
# - select categories to expand, e.g. oral, popular media etc. 

# get the right data
require(dplyr)
fig3_data <- survey_data %>% dplyr::select(starts_with("Q2"),starts_with("Q3"),starts_with("Q4"),starts_with("Q5"),starts_with("Q6"),starts_with("Q7"))


#### re-code data ####
fig3_recoded <- fig3_data %>% gather() %>% separate(key,sep="_",c('question','type','subtype'))
# lump frequencies
fig3_recoded$value[is.na(fig3_recoded$value)]<-"Never"  #assumes that NAs (blanks) indicate that the activity was never done
# fig3_recoded[fig3_recoded=="Daily"] <- "Frequently"
# fig3_recoded[fig3_recoded=="Weekly"] <- "Frequently"
# fig3_recoded[fig3_recoded=="Monthly"] <-  "Frequently"  #changed Monthly to be recoded as "Frequently"
fig3_recoded[fig3_recoded=="Yearly or less"] <- "Yearly"

# recode type
unique(fig3_recoded$type)
fig3_recoded$type[fig3_recoded$type=="oral"]<-"Oral"
fig3_recoded$type[fig3_recoded$type=="workshop"]<-"Workshop"
fig3_recoded$type[fig3_recoded$type=="popmedia"]<-"Popular Media"
fig3_recoded$type[fig3_recoded$type=="written"]<-"Written"
fig3_recoded$type[fig3_recoded$type=="visual"]<-"Visual"
fig3_recoded$type[fig3_recoded$type=="social"]<-"Social"

# recode subtype
unique(fig3_recoded$subtype)
fig3_recoded$subtype[fig3_recoded$subtype=="townhall"]<-"Townhall"
fig3_recoded$subtype[fig3_recoded$subtype=="community"]<-"Community"
fig3_recoded$subtype[fig3_recoded$subtype=="publec"]<-"Public\nLecture"
fig3_recoded$subtype[fig3_recoded$subtype=="intesdisc"]<-"Inter-\ndisciplinary"
fig3_recoded$subtype[fig3_recoded$subtype=="specialint"]<-"Special\nInterest"
fig3_recoded$subtype[fig3_recoded$subtype=="postsec"]<-"Post-\nSecondary"
fig3_recoded$subtype[fig3_recoded$subtype=="directcitsci"]<-"Direct\nCitizen\nScience"
fig3_recoded$subtype[fig3_recoded$subtype=="indirectcitsci"]<-"Indirect\nCitizen\nScience"
fig3_recoded$subtype[fig3_recoded$subtype=="schoolscicomp"]<-"School\nScience\nCompetition"
fig3_recoded$subtype[fig3_recoded$subtype=="publicscience"]<-"Public\nScience"
fig3_recoded$subtype[fig3_recoded$subtype=="boothevent"]<-"Booth\nEvent"
fig3_recoded$subtype[fig3_recoded$subtype=="newspaper"]<-"Newspaper"
fig3_recoded$subtype[fig3_recoded$subtype=="radio"]<-"Radio"
fig3_recoded$subtype[fig3_recoded$subtype=="TV"]<-"TV"
fig3_recoded$subtype[fig3_recoded$subtype=="debate"]<-"Debate"
fig3_recoded$subtype[fig3_recoded$subtype=="popsci"]<-"Popular\nScience"
fig3_recoded$subtype[fig3_recoded$subtype=="pressrelease"]<-"Press\nRelease"
fig3_recoded$subtype[fig3_recoded$subtype=="curriculum"]<-"Curriculum"
fig3_recoded$subtype[fig3_recoded$subtype=="oped"]<-"Op-ed"
fig3_recoded$subtype[fig3_recoded$subtype=="illustration"]<-"Illustration"
fig3_recoded$subtype[fig3_recoded$subtype=="infographic"]<-"Infographic"
fig3_recoded$subtype[fig3_recoded$subtype=="video"]<-"Video"
fig3_recoded$subtype[fig3_recoded$subtype=="music"]<-"Music"
fig3_recoded$subtype[fig3_recoded$subtype=="dance"]<-"Dance"
fig3_recoded$subtype[fig3_recoded$subtype=="blogguest"]<-"Guest\nBlog"
fig3_recoded$subtype[fig3_recoded$subtype=="blogown"]<-"Own\nBlog"
fig3_recoded$subtype[fig3_recoded$subtype=="twitter"]<-"Twitter"
fig3_recoded$subtype[fig3_recoded$subtype=="facebook"]<-"Facebook"
fig3_recoded$subtype[fig3_recoded$subtype=="postvideo"]<-"Posted\nVideo"
fig3_recoded$subtype[fig3_recoded$subtype=="appearvideo"]<-"Appearance\nin\nVideo"
fig3_recoded$subtype[fig3_recoded$subtype=="podcast"]<-"Podcast"


tiff('figS2.tif',res=1200,width=8.5,height=10,units="in")
layout(matrix(c(1,1,2:7),ncol=2,byrow=TRUE),heights=c(1,4,4,4))
par(mar=c(0,0,0,0),cex=0.9)
plot.new()
legend("center",
       c("Daily","Weekly","Monthly","Yearly or less", "Never"),
       fill=c(grey.colors(4),"white"),
       bty="n",
       horiz=TRUE)
#loop for type
par(mar=c(5, 4, 2, 0) + 0.1,cex=0.9)
for(i in unique(fig3_recoded$type)[c(4,1,5,2,6,3)]){
    #summarize each subtype within type
    fig3_summary <- spread(data.frame(with(fig3_recoded, table(paste(type,subtype,sep="_"), value))),key=value,value=Freq)
    fig3_summary <- fig3_summary %>% 
        separate(Var1,sep="_",c('type','subtype')) %>% 
        filter(type==i) %>% 
        group_by(subtype) %>% 
        summarize(mean_Daily=mean(Daily)/nrow(survey_data)*100,
                  mean_Weekly=mean(Weekly)/nrow(survey_data)*100,
                  mean_Monthly=mean(Monthly)/nrow(survey_data)*100,
                  mean_Yearly=mean(Yearly)/nrow(survey_data)*100,
                  mean_Never=mean(Never)/nrow(survey_data)*100) #Added calculation of mean for Never
    
    #re-order
    fig3_summary <- fig3_summary[order(fig3_summary$mean_Never,decreasing = FALSE),]
    
    #remove other and otherspecify from subtype list
    fig3_summary <- fig3_summary[which(fig3_summary$subtype != "other" & fig3_summary$subtype != "otherspecify" &fig3_summary$subtype != "specifyother"), ] #!!
    
    # do stats on long format data
    fig3_stats <- fig3_recoded %>% filter(type==i)
    fig3_stats$subtype <- as.factor(fig3_stats$subtype)
    print(i)
    print(kruskal.test(value ~ subtype, data = fig3_stats))
    print(posthoc.kruskal.nemenyi.test(value ~ subtype, data = fig3_stats))
    
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
            ylab="Percent",
            mgp=c(3,0.5,0),
            col=c('#00441b', '#006d2c','#41ae76','#99d8c9','#ccece6','#f7fcfd'))
    title(i)
    box(bty='l')
}
dev.off()

fig <- image_read("figS2.tif")
info <- image_info(fig)
image_write(image_scale(fig,paste0(info$width/5,"X",info$height/5)),"figS2_small.png",format='png')