# HWK 1 analysis
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
install.packages("knitr")
library(knitr)
library(ggplot2)
install.packages('scales')
library(scales)

## Enrollment Data

# 1. Total observations
full.ma.data <- read_rds("data/output/ma_data_2010.rds")
for (y in 2010:2015) {
  full.ma.data <- rbind(full.ma.data,read_rds(paste0("data/output/ma_data_",y,".rds")))
}
observations_total = nrow(full.ma.data)
print(observations_total)
    # 15824798 total observations

# 2. Number of plan_types
plans = unique(full.ma.data$plan_type)
print(length(plans))
    # 18 different plan types

# 3. Count of plans by year
plan_type_year_table = table(full.ma.data$plan_type, full.ma.data$year)
print(plan_type_year_table)
plan_type_year_df = as.data.frame.matrix(plan_type_year_table)
kable(plan_type_year_df, format = "markdown")

# 4. Remove special needs plans (SNP), employer group plans (eghp), and all "800-series" plans
final.plans <- full.ma.data %>%
  filter(snp == "No" & eghp == "No" &
           (planid < 800 | planid >= 900))
plan.type.year2 <- final.plans %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n)
plan.type.year2 <- pivot_wider(plan.type.year2, names_from="year",values_from="n", names_prefix="Count_")

# 5. Average Medicare Advantage enrollees per county
final.data <- final.plans %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!is.na(avg_enrollment))

contract.service.area <- readRDS('data/output/contract_service_area.rds')
final.data <- final.plans %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!is.na(avg_enrollment))
  
final.data.pen <- final.data %>%
  left_join( ma.penetration.data %>% ungroup() %>%
               rename(state_long=state, county_long=county), 
             by=c("fips", "year"))
final.state <- final.data.pen %>% 
  group_by(state) %>% 
  summarize(state_name=last(state_long, na.rm=TRUE))

final.data.pen <- final.data.pen %>%
  left_join(final.state,
            by=c("state"))

fig.avg.enrollment <- final.data %>%
  group_by(fips, year) %>% 
  select(fips, year, avg_enrollment) %>% 
  summarize(all_enroll = sum(avg_enrollment), .groups = 'drop') %>%
  ggplot(aes(x = as.factor(year), y = all_enroll)) + 
  stat_summary(fun.y = "mean", geom = "bar") +
  labs(
    x = "Year",
    y = "People",
    title = "Average MA Enrollment"
  ) +
  scale_y_continuous(labels = comma)

print(fig.avg.enrollment)

## Premium Data
plan.premiums <- readRDS('data/output/plan_premiums.rds')

# 6. Average premium over time
prem.data <- final.data.pen %>%
  left_join( plan.premiums,
             by=c("contractid","planid","state_name"="state","county","year"))

fig.avg.premium <- prem.data %>% ungroup() %>% group_by(year) %>%
  ggplot(aes(x=as.factor(year),y=premium, group=1)) + 
  stat_summary(fun.y="mean", geom="line", na.rm=TRUE) +
  labs(
    x="Year",
    y="Premium",
    title="Average Premium Over Time"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()

print(fig.avg.premium)

# 7. Percent $0 premium plans over time
fig.percent.zero <- prem.data %>% ungroup() %>%
  mutate(prem_0=(premium==0),
         prem_na=(is.na(premium))) %>%
  group_by(year) %>%
  summarize(all_count=n(),prem_0=sum(prem_0, na.rm=TRUE), prem_na=sum(prem_na)) %>%
  mutate(perc_0=prem_0/all_count) %>%
  ggplot(aes(x=as.factor(year), y=perc_0, group=1)) + geom_line() +
  labs(
    x="Year",
    y="Percent",
    title="Percent $0 Premium Plans Over Time"
  ) + scale_y_continuous(labels=percent)

print(fig.percent.zero)
