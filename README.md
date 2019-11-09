# info201-final-project
Final project repository for info201


# Domain of interest
## Why are you interested in this field/domain?
- Our domain of interest is health care. We are interested in health care because it is one of the most promising and controverial fields. Health is always the basic and most important need of humans, and healthcare is always cared by humans.

- We will especially focus on smoking and cigarettes. Smoking/ecig is chosen as a potential domain of interest to study because the effects and marketing of ecigs is widely controversial, which presents the need to study the topic further.
-We chose to focus our geographic coverage to a single state, preferably a more populated one like California.

## What other examples of data driven project have you found related to this domain (share at least 3)?
1. E-Cigarette Use Among Youth and Young Adults by U.S. Public Health Service
   https://e-cigarettes.surgeongeneral.gov/documents/2016_sgr_full_report_non-508.pdf

2. E-Cigarettes: Use, Effects on Smoking, Risks, and Policy Implications
  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6251310/

3. Tobacco Control Simulation Models
https://www.hhs.gov/sites/default/files/consequences-smoking-appendix15-1-tobacco-control-simulation-models.pdf

## What data-driven questions do you hope to answer about this domain (share at least 3)?
1. What are the percentage of adult smokers in California Counties from 2010 to 2019?

2. What is the relationship between smoking rates and the number of smokers?

3.  What is the distrubution of Tobacco Retailers in California？


# Finding Data
## Source 1: Percentage of Adult Smokers in California Counties from 2010 to 2019
1. https://www.countyhealthrankings.org/app/california/2016/downloads
2. The data from this website uses a national survey known as "The Behavioral Risk Factor Surveillance System"" (BRFSS). They collected data via telephone interview and has had over 400,000 responses. The data covers many different health behaviors and health-related quality of life (HRQoL) indicators, but for the purposes of our project we will only be considering the data on adult smoking trends.
3. There are 57 rows that contain data (the three top rows offer information on the categories), which reflect the 57 counties in California that we will study.
4. There are 49 columns total, but we will most likely only consider the columns that relate to the percentage of smokers for a particular year and county.
5. From this data, we can answer questions about trends in adult smoking by county and by year.

## Source 2:
1. https://gis.cdc.gov/Cancer/USCS/DataViz.html
2. The data that is used in the CDC website is gathered by compiling information from cancer registeries across the United States, we will only be using the information gathered within the state of California. It is compiled within five year periods and breaks down the information by varying cancer types and by the counties.
3. The table has 8 columns and 59 rows which provide information on the counties such as the population of the county, the types of cancers that affect the county, as well as the rate of new cases.
4. From this data we can see the correlation, if any, between counties in California with high smoking rates and the number of smokers.

## Source 3:
1. https://www.accessdata.fda.gov/scripts/oce/inspections/oce_insp_searching.cfm
2. Compliance Check Inspections of Tobacco Product Retailers is collected by the US Food & Drug Administration, exported by different fiscal years. This dataset describes the distrubution in California region, geographic Granularity = city, from 2011-2019.
3. There are about 165085 observations in each fiscal year.
4. 14 features.
5. What is the distrubution of Tobacco Retailers in California.
