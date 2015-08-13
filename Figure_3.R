#### Figure 3 ####
# REASONING:
# - showing the frequency of participation in the subcategories of important ones in Fig 2b
# - data from Q2-7
# - lump frequency
# - select categories to expand, e.g. oral, popular media etc. 

# get the right data
require(dplyr)
fig3_data <- survey_data %>% select(starts_with("Q2"),starts_with("Q3"),starts_with("Q4"),starts_with("Q5"),starts_with("Q6"),starts_with("Q7"))
