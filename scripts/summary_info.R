library(readr)
library(dplyr)
smoking_data <- read_csv("data/smoking data (2010-2019).csv")
View(smoking_data)

#summary info calculations
summary_info <- function(dataset) {
  retlist <- list()
    retlist$num_counties <- nrow(dataset)
    retlist$max_county <- list()
      retlist$max_county$x2019 <- smoking_data %>%
        filter(X2019_perc_smokers == max(X2019_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$x2018 <- smoking_data %>%
        filter(X2018_perc_smokers == max(X2018_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$x2017 <- smoking_data %>%
        filter(X2017_perc_smokers == max(X2017_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$x2017 <- smoking_data %>%
        filter(X2017_perc_smokers == max(X2018_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$x2016 <- smoking_data %>%
        filter(X2016_perc_smokers == max(X2016_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$x2015 <- smoking_data %>%
        filter(X2015_perc_smokers == max(X2015_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$x2014 <- smoking_data %>%
        filter(X2014_perc_smokers == max(X2014_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$x2013 <- smoking_data %>%
        filter(X2013_perc_smokers == max(X2013_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$x2012 <- smoking_data %>%
        filter(X2012_perc_smokers == max(X2012_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$x2011 <- smoking_data %>%
        filter(X2011_perc_smokers == max(X2011_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
      retlist$max_county$x2010 <- smoking_data %>%
        filter(X2010_perc_smokers == max(X2010_perc_smokers, na.rm = TRUE)) %>%
        pull(County)
    retlist$max_smokers <- list()
      retlist$max_smokers$x2019 <- smoking_data %>%
        filter(X2019_perc_smokers == max(X2019_perc_smokers, na.rm = TRUE)) %>%
        pull(X2019_perc_smokers)
      retlist$max_smokers$x2018 <- smoking_data %>%
        filter(X2018_perc_smokers == max(X2018_perc_smokers, na.rm = TRUE)) %>%
        pull(X2018_perc_smokers)
      retlist$max_smokers$x2017 <- smoking_data %>%
        filter(X2017_perc_smokers == max(X2017_perc_smokers, na.rm = TRUE)) %>%
        pull(X2017_perc_smokers)
      retlist$max_smokers$x2016 <- smoking_data %>%
        filter(X2016_perc_smokers == max(X2016_perc_smokers, na.rm = TRUE)) %>%
        pull(X2016_perc_smokers)
      retlist$max_smokers$x2015 <- smoking_data %>%
        filter(X2015_perc_smokers == max(X2015_perc_smokers, na.rm = TRUE)) %>%
        pull(X2015_perc_smokers)
      retlist$max_smokers$x2014 <- smoking_data %>%
        filter(X2014_perc_smokers == max(X2014_perc_smokers, na.rm = TRUE)) %>%
        pull(X2014_perc_smokers)
      retlist$max_smokers$x2013 <- smoking_data %>%
        filter(X2013_perc_smokers == max(X2013_perc_smokers, na.rm = TRUE)) %>%
        pull(X2013_perc_smokers)
      retlist$max_smokers$x2012 <- smoking_data %>%
        filter(X2012_perc_smokers == max(X2012_perc_smokers, na.rm = TRUE)) %>%
        pull(X2012_perc_smokers)
      retlist$max_smokers$x2011 <- smoking_data %>%
        filter(X2011_perc_smokers == max(X2011_perc_smokers, na.rm = TRUE)) %>%
        pull(X2011_perc_smokers)
      retlist$max_smokers$x2010 <- smoking_data %>%
        filter(X2010_perc_smokers == max(X2010_perc_smokers, na.rm = TRUE)) %>%
        pull(X2010_perc_smokers)
    retlist$mean <- list()
      retlist$mean$x2019 <- mean(smoking_data$X2019_perc_smokers, na.rm = TRUE)
      retlist$mean$x2018 <- mean(smoking_data$X2018_perc_smokers, na.rm = TRUE)
      retlist$mean$x2017 <- mean(smoking_data$X2017_perc_smokers, na.rm = TRUE)
      retlist$mean$x2016 <- mean(smoking_data$X2016_perc_smokers, na.rm = TRUE)
      retlist$mean$x2015 <- mean(smoking_data$X2015_perc_smokers, na.rm = TRUE)
      retlist$mean$x2014 <- mean(smoking_data$X2014_perc_smokers, na.rm = TRUE)
      retlist$mean$x2013 <- mean(smoking_data$X2013_perc_smokers, na.rm = TRUE)
      retlist$mean$x2012 <- mean(smoking_data$X2012_perc_smokers, na.rm = TRUE)
      retlist$mean$x2011 <- mean(smoking_data$X2011_perc_smokers, na.rm = TRUE)
      retlist$mean$x2010 <- mean(smoking_data$X2010_perc_smokers, na.rm = TRUE)
    retlist$mean_change <- mean(smoking_data$X2019_perc_smokers,
                                na.rm = TRUE) -
      mean(smoking_data$X2010_perc_smokers,
           na.rm = TRUE)
  return(retlist)
}