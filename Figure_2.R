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

# get the right data
require(dplyr)
fig2a_data <- survey_data %>% select(starts_with("Q9"))
fig2b_data <- survey_data %>% select(starts_with("Q2"),starts_with("Q3"),starts_with("Q4"),starts_with("Q5"),starts_with("Q6"),starts_with("Q7"))

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
#assign labels
fig2a_summary$key <- sub(".+_.+_","",as.character(fig2a_summary$key))
#plot
likert(key ~ . , fig2a_summary,
       as.percent=TRUE,
       ylab=NULL,
       main="Ranking of scicomm training")

#### make 
