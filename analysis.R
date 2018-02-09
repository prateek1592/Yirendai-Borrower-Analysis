## Exploratory analysis of cleaned data to understand relationship b/w borrower features
## like education, marital status, income, location and others on loan characteristics

library(dplyr)
library(reshape2)
library(ggplot2)

## Reading in the data, basic processing/cleaning

setwd("C:/Users/prateek/Desktop/China/Fintech/YRD/Scrapes/")
df <- read.csv("All_Data.csv", encoding="UTF-8", stringsAsFactors = F)
names(df)[1] <- "ID"
df$Value <- gsub("^Confidentiality$",NA,df$Value)
df$Particular <- gsub(" ","_",df$Particular)
df$Particular <- gsub("^Annual_income$","Min_Annual_pretax_income",df$Particular)
df <- df[!duplicated(df[,c("ID","Particular")]),]
df <- dcast(df, ID ~ Particular)
df[,"Min_Annual_pretax_income"] <- gsub("\\,", "", df[,"Min_Annual_pretax_income"])
df[,"Min_Annual_pretax_income"] <- gsub("-.*", "", df[,"Min_Annual_pretax_income"])
df[,c("Annual_interest_rate", "Borrowing_period", "Credit_card_limit", "Loan_amount", "Min_Annual_pretax_income")] <- 
  sapply(df[,c("Annual_interest_rate", "Borrowing_period", "Credit_card_limit", "Loan_amount", "Min_Annual_pretax_income")], 
         as.numeric)

## Data split By Minimum Annual Income
df %>% group_by(Min_Annual_pretax_income) %>%
  summarize(credit = median(Credit_card_limit), loan = median(Loan_amount), total=n()/nrow(.), 
            period = mean(Borrowing_period)) %>% 
  data.frame(.) %>% 
  arrange(Min_Annual_pretax_income)

## Distribution of number of loans
p <- ggplot(df, aes(Borrowing_period, fill = factor(Min_Annual_pretax_income))) + geom_density(alpha = 0.2)
p <- p + guides(fill = guide_legend(title="Min_Income"))
p

## Cumulative Distribution of Loans by Loan Tenure
p <- ggplot(df, aes(Loan_amount, color=factor(Min_Annual_pretax_income))) + stat_ecdf(size=1)
p <- p + facet_grid(Borrowing_period ~ .)
p <- p + guides(color = guide_legend(title="Min_Income"))
p <- p + labs(x = "Loan Size", y = "Cumulative Distribution")
p

## Checking grade-wise income limits (Grades: Credit grade by company for their borrowers)
grade_split <- c(3.5, 5.8, 7.4, 83.3) / 100   # Given in company financial statements
df2 <- df
df2[is.na(df2$Min_Annual_pretax_income),"Min_Annual_pretax_income"] <- 0
df2 <- df2 %>% 
  arrange(desc(Min_Annual_pretax_income)) %>% 
  mutate(Cumulative_Loan_Amount = cumsum(Loan_amount)) %>% 
  select(Min_Annual_pretax_income, Cumulative_Loan_Amount)
df2$Cumulative_Loan_Amount <- df2$Cumulative_Loan_Amount / 
  as.numeric(tail(df2$Cumulative_Loan_Amount,1))

ggplot(df2, aes(x = Cumulative_Loan_Amount, y = Min_Annual_pretax_income)) + 
  geom_line() + geom_vline(xintercept=cumsum(grade_split), color='red')

## Calculate Debt-to-Income (DTI) : 
# Average (Upfront) Transaction Fee charged by company ~22%
# Income == Mid-point of income bracket given for each group
out <- df %>% 
  filter(!is.na(Min_Annual_pretax_income)) %>% 
  mutate(Avg_Annual_income = Min_Annual_pretax_income+25000,
         Interest_payment = Annual_interest_rate * Loan_amount / 100,
         Transaction_fee = 0.22 * Loan_amount / Borrowing_period * 12,
         Principal_payment = Loan_amount / Borrowing_period * 12,
         Total_Yearly_Payment = Interest_payment + Principal_payment + Transaction_fee)

out2 <- out %>%
  group_by(Min_Annual_pretax_income) %>%
  summarize(mean_DTI = sum(Total_Yearly_Payment) / sum(Avg_Annual_income), 
            best_case_DTI = sum(Total_Yearly_Payment) / sum(Avg_Annual_income+25000),
            total=n()) %>%
  data.frame(.)

out3 <- out %>%
  mutate(DTI = round(Total_Yearly_Payment / Avg_Annual_income,2)) %>%
  filter(Borrowing_purposes == "Daily consumption" | Borrowing_purposes == "Short-term turnover")
p <- ggplot(out3, aes(DTI, fill=factor(Borrowing_purposes))) + geom_density(alpha = 0.4)
p <- p + facet_grid(. ~ factor(Min_Annual_pretax_income))
p

## Proportion of long-term subprime loans
out4 <- out %>%
  group_by(Min_Annual_pretax_income, Borrowing_period) %>%
  summarize(mean_DTI = sum(Total_Yearly_Payment) / sum(Avg_Annual_income), 
            best_case_DTI = sum(Total_Yearly_Payment) / sum(Avg_Annual_income+25000),
            total=n()) %>%
  arrange(desc(Borrowing_period)) %>%
  data.frame(.)

## Include row items where Annual Income unavailable
df2 <- df
df2[is.na(df2$Min_Annual_pretax_income),"Min_Annual_pretax_income"] <- 0
df2 %>% 
  mutate(Avg_Annual_income = Min_Annual_pretax_income+25000,
         Interest_payment = Annual_interest_rate * Loan_amount / 100,
         Transaction_fee = 0.22 * Loan_amount / Borrowing_period * 12,
         Principal_payment = Loan_amount / Borrowing_period * 12,
         Total_Yearly_Payment = Interest_payment + Principal_payment + Transaction_fee) %>%
  select(Min_Annual_pretax_income, Avg_Annual_income, Loan_amount, Annual_interest_rate,
         Interest_payment, Borrowing_period, Transaction_fee, Principal_payment, 
         Total_Yearly_Payment) %>% 
  group_by(Min_Annual_pretax_income) %>%
  summarize(DTI = sum(Total_Yearly_Payment) / sum(Avg_Annual_income), total=n())

## Self confessed use of loan proceeds - Consumption v/s Refinancing
df4 <- df[df$Borrowing_purposes == "Short-term turnover" | df$Borrowing_purposes == "Daily consumption",]
p <- ggplot(df4, aes(Loan_amount, fill=factor(Borrowing_purposes))) + geom_density(alpha = 0.4)
p <- p + guides(fill = guide_legend(title="Purpose"))
p

## Gender characteristics of borrowers
ggplot(df, aes(Loan_amount, fill=factor(gender))) + geom_density(alpha = 0.4)

## Are the undisclosed short-term loans "FastTrack"? (Low checks, quick approval loans)
## Basis : FastTrack average size ~ 39k RMB (Company Financial statements)
df %>% filter(is.na(Min_Annual_pretax_income)) %>% 
  group_by(Borrowing_period) %>% 
  summarize(mean = mean(Loan_amount), max = max(Loan_amount), 
            min = min(Loan_amount), count = n() / nrow(df))

df %>% filter(is.na(Min_Annual_pretax_income), Loan_amount <= 100000) %>% 
  group_by(Borrowing_period) %>% 
  summarize(mean = mean(Loan_amount), max = max(Loan_amount), 
            min = min(Loan_amount), count = n())

df %>% filter(is.na(Min_Annual_pretax_income), Loan_amount <= 100000, 
              Borrowing_period == 12) %>%
  select(ID) %>% head(.)

## Distribution of data by Borrower Incomes
p <- ggplot(data = subset(df, !is.na(Min_Annual_pretax_income)), 
            aes(Annual_interest_rate, fill=factor(Min_Annual_pretax_income)))
p <- p + geom_density(alpha = 0.4)
p <- p + guides(fill = guide_legend(title="Min_Income"))
p

## Distribution of Loan by Tier - Geographic distribution of data
city_map <- read.csv("CityMap.csv", stringsAsFactors = F)
df_tiers <- merge(df, city_map, by="Working_city")
p <- ggplot(df_tiers, aes(Annual_interest_rate, fill=factor(Tier))) + 
  geom_density(alpha = 0.4)
p <- p + guides(fill = guide_legend(title="Tier"))
p

df_tiers %>% group_by(Tier, Min_Annual_pretax_income) %>%
  summarize(mean = mean(Loan_amount), max = max(Loan_amount), 
            min = min(Loan_amount), count = n())

## Marital Status of borrowers
df %>% group_by(marital_status) %>% 
  summarize(mean = mean(Loan_amount), count = n())
p <- ggplot(data = subset(df, !is.na(marital_status)), 
            aes(Loan_amount, fill=factor(marital_status)))
p <- p + geom_density(alpha = 0.4)
p <- p + guides(fill = guide_legend(title="Marital"))
p

## Education of borrowers
df %>% group_by(highest_education) %>% 
  summarize(mean = mean(Loan_amount), count = n())
p <- ggplot(data = subset(df, !is.na(highest_education)), aes(Loan_amount, fill=factor(highest_education))) + geom_density(alpha = 0.4)
p <- p + guides(fill = guide_legend(title="Ed"))
p