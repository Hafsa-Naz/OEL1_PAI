#OPEN ENDED LAB :1 PAI 
#Hafsa Naz(23f-AI-56)
#section:A2

#Task:1
library(readr)
library(tidyverse)
customer_churn <- read_csv("customer_churn.csv")
view(customer_churn)
customer_churn_yes<- customer_churn %>% filter(Churn=="Yes")
view(customer_churn_yes)

#Task:2
customer_churn<- customer_churn %>% mutate(ChargeGap=TotalCharges - (MonthlyCharges * Tenure),na.rm=TRUE)
view(customer_churn)

#Task:3
long_active_customer <- customer_churn %>% filter(Tenure>24 & Churn=="No")
view(long_active_customer)

#Task:4
avg_monthlycharge_contarct <- customer_churn %>% group_by(ContractType) %>% summarize(Average_monthlycharge=mean(MonthlyCharges))
view(avg_monthlycharge_contarct)

#Task:5
customer_churn <- customer_churn %>% mutate(AgeGroup = case_when( Age < 25 ~ "Youth",Age >= 25 & Age <= 55 ~ "Adult",
Age > 55 ~ "Senior",TRUE ~ "Unknown"  ) )
view(customer_churn)

#Task:6
Top5_cities <- customer_churn_yes %>% count(City) %>% arrange(desc(n)) %>% head(5)
view(Top5_cities)

#Task:7
name_cities_filtered_customer<- customer_churn %>% filter(TotalCharges > 3000, ContractType == "Month-to-Month", Churn == "Yes") %>%select(Name, City)
view(name_cities_filtered_customer)


#Task:8
contract_summary <- customer_churn %>% group_by(ContractType) %>% summarise(AverageTenure = mean(Tenure, na.rm = TRUE),TotalRevenue = sum(TotalCharges, na.rm = TRUE) )
view(contract_summary)





