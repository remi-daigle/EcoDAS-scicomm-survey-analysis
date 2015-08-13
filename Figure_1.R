#### Figure 1 ####
# REASONING:
# - show the overall importance of scicomm relative to other things that are taught
# - data from Q8
# - scicomm of ~ equal rank to in-department curriculum
# - also needs statistics

# get the right data
require(dplyr)
fig1_data <- survey_data %>% select(starts_with("Q8"))
