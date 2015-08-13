#### Figure 4 ####
# REASONING:
# - show who provided/sponsored scicomm training
# - data from Q10
# - stacked bars, bottom - own institution, self, conf, etc
# - select categories to expand, e.g. oral, popular media etc. 

# get the right data
require(dplyr)
fig1_data <- survey_data %>% select(starts_with("Q10"))