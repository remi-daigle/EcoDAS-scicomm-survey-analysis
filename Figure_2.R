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
