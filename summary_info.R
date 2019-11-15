library(readr)
library(dplyr)
smoking_data <- read_csv("data/smoking data (2010-2019).csv")
View(smoking_data)

#summary info calculations
summary_info <- function(dataset) {
  retlist <- list()
    retlist$num_counties <- nrow(dataset) - 5
    retlist$max_2019 <- select(smoking_data, 2019)
      filter(smoking_data, 2019 == max(2019, na.rm = TRUE)) %>%
      pull(County)
  return(retlist)
}