library(readr)
library(dplyr)
smoking_data <- read_csv("data/smoking data (2010-2019).csv")
View(smoking_data)

#summary info calculations
summary_info <- function(dataset) {
  retlist <- list()
    retlist$num_counties <- nrow(dataset)
    retlist$max_county <- list()
      retlist$max_county$X2019 <- smoking_data %>%
        filter(X2019_perc_smokers == max(X2019_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$X2018 <- smoking_data %>%
        filter(X2018_perc_smokers == max(X2018_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$X2017 <- smoking_data %>%
        filter(X2017_perc_smokers == max(X2017_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$X2017 <- smoking_data %>%
        filter(X2017_perc_smokers == max(X2018_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$X2016 <- smoking_data %>%
        filter(X2016_perc_smokers == max(X2016_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$X2015 <- smoking_data %>%
        filter(X2015_perc_smokers == max(X2015_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$X2014 <- smoking_data %>%
        filter(X2014_perc_smokers == max(X2014_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$X2013 <- smoking_data %>%
        filter(X2013_perc_smokers == max(X2013_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$X2012 <- smoking_data %>%
        filter(X2012_perc_smokers == max(X2012_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$X2011 <- smoking_data %>%
        filter(X2011_perc_smokers == max(X2011_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$X2010 <- smoking_data %>%
        filter(X2010_perc_smokers == max(X2010_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
    retlist$max_smokers <- list()
      retlist$max_smokers$X2019 <- smoking_data %>%
        filter(X2019_perc_smokers == max(X2019_perc_smokers, na.rm = TRUE)) %>%
        pull(X2019_perc_smokers)
      retlist$max_smokers$X2018 <- smoking_data %>%
        filter(X2018_perc_smokers == max(X2018_perc_smokers, na.rm = TRUE)) %>%
        pull(X2018_perc_smokers)
      retlist$max_smokers$X2017 <- smoking_data %>%
        filter(X2017_perc_smokers == max(X2017_perc_smokers, na.rm = TRUE)) %>%
        pull(X2017_perc_smokers)
      retlist$max_smokers$X2016 <- smoking_data %>%
        filter(X2016_perc_smokers == max(X2016_perc_smokers, na.rm = TRUE)) %>%
        pull(X2016_perc_smokers)
      retlist$max_smokers$X2015 <- smoking_data %>%
        filter(X2015_perc_smokers == max(X2015_perc_smokers, na.rm = TRUE)) %>%
        pull(X2015_perc_smokers)
      retlist$max_smokers$X2014 <- smoking_data %>%
        filter(X2014_perc_smokers == max(X2014_perc_smokers, na.rm = TRUE)) %>%
        pull(X2014_perc_smokers)
      retlist$max_smokers$X2013 <- smoking_data %>%
        filter(X2013_perc_smokers == max(X2013_perc_smokers, na.rm = TRUE)) %>%
        pull(X2013_perc_smokers)
      retlist$max_smokers$X2012 <- smoking_data %>%
        filter(X2012_perc_smokers == max(X2012_perc_smokers, na.rm = TRUE)) %>%
        pull(X2012_perc_smokers)
      retlist$max_smokers$X2011 <- smoking_data %>%
        filter(X2011_perc_smokers == max(X2011_perc_smokers, na.rm = TRUE)) %>%
        pull(X2011_perc_smokers)
      retlist$max_smokers$X2010 <- smoking_data %>%
        filter(X2010_perc_smokers == max(X2010_perc_smokers, na.rm = TRUE)) %>%
        pull(X2010_perc_smokers)
    retlist$mean <- list()
      retlist$mean$X2019 <- mean(smoking_data$X2019_perc_smokers, na.rm = TRUE)
      retlist$mean$X2018 <- mean(smoking_data$X2018_perc_smokers, na.rm = TRUE)
      retlist$mean$X2017 <- mean(smoking_data$X2017_perc_smokers, na.rm = TRUE)
      retlist$mean$X2016 <- mean(smoking_data$X2016_perc_smokers, na.rm = TRUE)
      retlist$mean$X2015 <- mean(smoking_data$X2015_perc_smokers, na.rm = TRUE)
      retlist$mean$X2014 <- mean(smoking_data$X2014_perc_smokers, na.rm = TRUE)
      retlist$mean$X2013 <- mean(smoking_data$X2013_perc_smokers, na.rm = TRUE)
      retlist$mean$X2012 <- mean(smoking_data$X2012_perc_smokers, na.rm = TRUE)
      retlist$mean$X2011 <- mean(smoking_data$X2011_perc_smokers, na.rm = TRUE)
      retlist$mean$X2010 <- mean(smoking_data$X2010_perc_smokers, na.rm = TRUE)
    retlist$mean_change <- mean(smoking_data$X2019_perc_smokers, na.rm = TRUE) - 
      mean(smoking_data$X2010_perc_smokers, na.rm = TRUE)
  return(retlist)
}





