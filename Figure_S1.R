#### Figure S1 ####
# REASONING:
# - Show intended career trajectories of survey participants

# load packages (install these if you don't have them)
require(dplyr)
require(tidyr)
require(magick)

# get the right data
figS1_data <- data.frame(demo_data %>% dplyr::select(starts_with("Q25")))


#### make a bar plot ####
#re-organize
figS1_data_long <- figS1_data %>% gather(type,answer)
figS1_data_long$answer <- as.numeric(is.na(figS1_data_long$answer)==0)
figS1_summary <-  figS1_data_long %>% 
    group_by(type) %>% 
    summarize(counts=sum(answer)) %>% 
    ungroup()

#re-order to make a better looking plot
figS1_summary <- figS1_summary[c(6,7,3,9,10,4,8,5,2,1), ]
figS1_summary

#alternate assign labels- to make labels more self-explanatory
figS1_summary$type <- c('High School teaching',
                        'Industry',
                        'Entrepreneurship',
                        'Other',
                        'Science communication',
                        'Government - Policy',
                        'Non-governmental organization',
                        'Government - Research',
                        'Academia - Teaching',
                        'Academia - Research')

# make count numeric
figS1_summary$counts <- as.numeric(figS1_summary$counts)

#plot
tiff('figS1.tif',res=600,width=8.5,height=4,units="in", compression = "lzw")

par(mar=c(5.1, 15.1, 0.1, 0.6),cex=0.9)
barplot(figS1_summary$counts,
        horiz=T,
        names.arg = figS1_summary$type,
        las=1,
        xlim=c(0,20),
        xlab="Number of Responses",
        col=c('#99d8c9')
        )
box(bty='l')

dev.off()

fig <- image_read("figS1.tif")
info <- image_info(fig)
image_write(image_scale(fig,paste0(info$width/5,"X",info$height/5)),"figS1_small.png",format='png')
image_write(fig,"figS1_lrg.png",format='png')