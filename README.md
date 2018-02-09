This is an attempt at demonstrating the utility of alternate data in supplementing financial analysis of listed companies. The example chosen here is Yirendai (NYSE: YRD), a peer-to-peer online lender in the burgeoning Chinese online consumption lending sector.

The analysis was originally done in September 2017, and uses publicly available loan/borrower data gathered from Yirendai's website to estimate key parameters essential for analysis, otherwise not available in their financial statements. The key findings, observations and information about the dataset are provided in the Analysis PDF available in this repo.

Other information used was Chinese city tiers to understand geographic split of Yirendai's vast loan facilitation volume. A large portion of the data was available in Mandarin, and had to be pre-processed (a one-off) through Google Translate to obtain meaningful insights - some information might have been lost in translation.

Please note that this was an academic exercise and is not meant as any sort of financial advice. The assumptions that went into the  findings, and any subsequent errors, are mine alone.

File descriptions:

Analysis - Github.pdf: Summary analysis presenting key findings from the exercise

analysis.R - Analysis of the post-processed data, to create insights on loan volume distribution across key parameters; also involves estimation of debt-to-income and other important parameters for gauging loan book quality

city_map_scraper.R - Scraper for obtaining CityMap.csv

CityMap.csv - Preliminary data on Chinese cities, including Tier & GDP per capita

scraper.R - Obtaining data from Yirendai's website and collecting in Temp_Data.xlsx to be further translated through Google's kickass Translate feature

Temp_data.xlsx - Collected data; A long table depicting 17 features for each loan ID that was translated from Mandarin.
