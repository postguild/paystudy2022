library(tidyverse)
library(lubridate)
library(knitr)
library(data.table)
library(fastDummies)
library(janitor)
library(vroom)

df <-vroom("raw_data/active_wd_2021.csv")

colnames(df) <- c("department","employee_id","gender",   
                  "race_ethnicity","education", "military_status",
                  "date_of_birth", "original_hire_date","hire_date","pay_rate_type", "current_base_pay", 
                  "job_profile_current","time_type_current", "cost_center_current",  
                  "effective_date1", "business_process_type1", "business_process_reason1", "pay_rate_type1", "base_pay_change1", "job_profile1", "time_type1", "cost_center1", 
                  "effective_date2", "business_process_type2", "business_process_reason2", "pay_rate_type2", "base_pay_change2", "job_profile2", "time_type2", "cost_center2", 
                  "effective_date3", "business_process_type3", "business_process_reason3", "pay_rate_type3", "base_pay_change3", "job_profile3", "time_type3", "cost_center3",
                  "effective_date4", "business_process_type4", "business_process_reason4", "pay_rate_type4", "base_pay_change4", "job_profile4", "time_type4", "cost_center4", 
                  "effective_date5", "business_process_type5", "business_process_reason5", "pay_rate_type5", "base_pay_change5", "job_profile5", "time_type5", "cost_center5", 
                  "effective_date6", "business_process_type6", "business_process_reason6", "pay_rate_type6", "base_pay_change6", "job_profile6", "time_type6", "cost_center6",
                  "effective_date7", "business_process_type7", "business_process_reason7", "pay_rate_type7", "base_pay_change7", "job_profile7", "time_type7", "cost_center7", 
                  "effective_date8", "business_process_type8", "business_process_reason8", "pay_rate_type8", "base_pay_change8", "job_profile8", "time_type8", "cost_center8", 
                  "effective_date9", "business_process_type9", "business_process_reason9", "pay_rate_type9", "base_pay_change9", "job_profile9", "time_type9", "cost_center9",
                  "effective_date10", "business_process_type10", "business_process_reason10", "pay_rate_type10", "base_pay_change10", "job_profile10", "time_type10", "cost_center10",
                  "effective_date11", "business_process_type11", "business_process_reason11", "pay_rate_type11", "base_pay_change11", "job_profile11", "time_type11", "cost_center11",
                  "effective_date12", "business_process_type12", "business_process_reason12", "pay_rate_type12", "base_pay_change12", "job_profile12", "time_type12", "cost_center12", 
                  "effective_date13", "business_process_type13", "business_process_reason13", "pay_rate_type13", "base_pay_change13", "job_profile13", "time_type13", "cost_center13", 
                  "effective_date14", "business_process_type14", "business_process_reason14", "pay_rate_type14", "base_pay_change14", "job_profile14", "time_type14", "cost_center14",
                  "effective_date15", "business_process_type15", "business_process_reason15", "pay_rate_type15", "base_pay_change15", "job_profile15", "time_type15", "cost_center15", 
                  "effective_date16", "business_process_type16", "business_process_reason16", "pay_rate_type16", "base_pay_change16", "job_profile16", "time_type16", "cost_center16", 
                  "effective_date17", "business_process_type17", "business_process_reason17", "pay_rate_type17", "base_pay_change17", "job_profile17", "time_type17", "cost_center17", 
                  "effective_date18", "business_process_type18", "business_process_reason18", "pay_rate_type18", "base_pay_change18", "job_profile18", "time_type18", "cost_center18", 
                  "effective_date19", "business_process_type19", "business_process_reason19", "pay_rate_type19", "base_pay_change19", "job_profile19", "time_type19", "cost_center19", 
                  "effective_date20", "business_process_type20", "business_process_reason20", "pay_rate_type20", "base_pay_change20", "job_profile20", "time_type20", "cost_center20", 
                  "effective_date21", "business_process_type21", "business_process_reason21", "pay_rate_type21", "base_pay_change21", "job_profile21", "time_type21", "cost_center21", 
                  "effective_date22", "business_process_type22", "business_process_reason22", "pay_rate_type22", "base_pay_change22", "job_profile22", "time_type22", "cost_center22", 
                  "effective_date23", "business_process_type23", "business_process_reason23", "pay_rate_type23", "base_pay_change23", "job_profile23", "time_type23", "cost_center23", 
                  "effective_date24", "business_process_type24", "business_process_reason24", "pay_rate_type24", "base_pay_change24", "job_profile24", "time_type24", "cost_center24", 
                  "effective_date25", "business_process_type25", "business_process_reason25", #"pay_rate_type_25", "base_pay_change25", 
                  "job_profile25", #"time_type25", 
                  "cost_center25", 
                  "effective_date_26", "business_process_type_26", "business_process_reason_26", #"pay_rate_type_26", "base_pay_change_26", 
                  "job_profile_26", #"time_type_26", 
                  "cost_center_26", "2008_annual_performance_rating", "2009_annual_performance_rating", "2010_annual_performance_rating",
                  "2011_annual_performance_rating", "2012_annual_performance_rating", "2013_annual_performance_rating",
                  "2014_annual_performance_rating", "2015_annual_performance_rating", "2016_annual_performance_rating",
                  "2017_annual_performance_rating", "2018_annual_performance_rating", "2019_annual_performance_rating",
                  "2020_annual_performance_rating")

#df <- clean_names(df)
#df2 <- clean_names(df2)
## Add fields for analysis

### Add age field


data_date <- ymd("2021-05-28")

df <- df %>% mutate(date_of_birth = mdy(date_of_birth),
                    age = floor(decimal_date(data_date) - decimal_date(date_of_birth)))

df <- df %>% 
  mutate(date_of_birth = case_when(
    age < 0 ~ date_of_birth-years(100),
    TRUE ~ date_of_birth),
    age = floor(decimal_date(data_date) - decimal_date(date_of_birth)))


df <- df %>% mutate(hire_date = mdy(hire_date),
                    years_of_service = floor(decimal_date(data_date) - decimal_date(hire_date)))

df <- df %>% 
  mutate(original_hire_date=mdy(original_hire_date))#,
#termination_date=ymd(termination_date),
#effective_date=mdy(effective_date))


df <- df %>% 
  mutate(age_group_5=case_when(
    age < 25 ~ "<25",
    age >= 25 & age < 30 ~ "25-29",
    age >= 30 & age < 35 ~ "30-34",
    age >= 35 & age < 40 ~ "35-39",
    age >= 40 & age < 45 ~ "40-44",
    age >= 45 & age < 50 ~ "45-49",
    age >= 50 & age < 55 ~ "50-54",
    age >= 55 & age < 60 ~ "55-59",
    age >= 60 & age < 65 ~ "60-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_))



df <- df %>% 
  mutate(age_group_10=case_when(
    age < 25 ~ "<25",
    age >= 25 & age < 35 ~ "25-34",
    age >= 35 & age < 45 ~ "35-44",
    age >= 45 & age < 55 ~ "45-55",
    age >= 55 & age < 65 ~ "55-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_))


df <- df %>% 
  mutate(years_of_service_grouped=case_when(
    years_of_service==0 ~ "0",
    years_of_service>=1  & years_of_service<3 ~ "1-2",
    years_of_service>=3  & years_of_service<3 ~ "3-5",
    years_of_service>=6  & years_of_service<3 ~ "6-10",
    years_of_service>=11  & years_of_service<3 ~ "11-15",
    years_of_service>=16  & years_of_service<3 ~ "16-20",
    years_of_service>=21  & years_of_service<3 ~ "21-25",
    years_of_service>=25 ~ "25+",
    TRUE ~ NA_character_))



df <- df %>%
  mutate(dept=case_when(
    department == "News" ~ "News", 
    department == "Editorial" ~ "News",
    department == "Client Solutions" ~ "Commercial",
    department == "Circulation" ~ "Commercial",
    department == "Finance" ~ "Commercial",
    department == "Marketing" ~ "Commercial",
    department == "WP News Media Services" ~ "Commercial",
    department == "Production" ~ "Commercial",
    department == "Public Relations" ~ "Commercial",
    department == "Administration" ~ "Commercial",
    department == "Product"~ "Commercial",
    TRUE ~ "Other"))

df <- df %>%
  mutate(desk=case_when(
    cost_center_current == "110000 News Operations" ~ "Operations",
    cost_center_current == "110001 News Digital Operations" ~ "Operations",
    cost_center_current == "110610 Audience Development and Engagement" ~ "Audience Development and Engagement",
    cost_center_current == "110620 News Audio" ~ "Audio",
    cost_center_current == "110604 Presentation Design" ~ "Design",
    cost_center_current == "110605 Presentation" ~ "Photography",
    cost_center_current == "110664 News National Apps" ~ "Emerging News Products",
    cost_center_current == "110665 News The Lily" ~ "Emerging News Products",
    cost_center_current == "110666 News Snapchat" ~ "Emerging News Products",
    cost_center_current == "110667 News By The Way" ~ "Emerging News Products",
    cost_center_current == "113210 Economy and Business" ~ "Financial",
    cost_center_current == "114000 Foreign Administration" ~ "Foreign",
    cost_center_current == "114095 News Foreign Brazil" ~ "Foreign",
    cost_center_current == "114100 Foreign Latam" ~ "Foreign",
    cost_center_current == "114220 News Foreign Istanbul" ~ "Foreign",
    cost_center_current == "114235 Foreign Western Europe" ~ "Foreign",
    cost_center_current == "114300 News Foreign West Africa" ~ "Foreign",
    cost_center_current == "114415 Foreign Hong Kong" ~ "Foreign",
    cost_center_current == "114405 Foreign Beijing Bureau" ~ "Foreign",
    cost_center_current == "114105 Foreign Mexico Bureau" ~ "Foreign",
    cost_center_current == "114005 Foreign Beirut Bureau" ~ "Foreign",
    cost_center_current == "114400 Foreign India Bureau" ~ "Foreign",
    cost_center_current == "114410 Foreign Tokyo Bureau" ~ "Foreign",
    cost_center_current == "114205 Foreign Islamabad Bureau" ~ "Foreign",
    cost_center_current == "114305 Foreign Nairobi Bureau" ~ "Foreign",
    cost_center_current == "114240 Foreign Rome Bureau" ~ "Foreign",
    cost_center_current == "114200 Foreign London Bureau" ~ "Foreign",
    cost_center_current == "114230 Foreign Moscow Bureau" ~ "Foreign",
    cost_center_current == "114225 Foreign Cairo Bureau" ~ "Foreign",
    cost_center_current == "114215 Foreign Berlin Bureau" ~ "Foreign",
    cost_center_current == "114310 Foreign Baghdad Bureau" ~ "Foreign",
    cost_center_current == "114315 Foreign Jerusalem Bureau" ~ "Foreign",
    cost_center_current == "110603 Presentation Graphics" ~ "Graphics",
    cost_center_current == "110450 Investigative" ~ "Investigative",
    cost_center_current == "112300 Local Politics and Government" ~ "Local",
    cost_center_current == "110601 Multiplatform Desk" ~ "Multiplatform",
    cost_center_current == "110500 Magazine" ~ "National",
    cost_center_current == "113200 National Politics and Government" ~ "National",
    cost_center_current == "113205 National Security" ~ "National",
    cost_center_current == "113215 News National Health & Science" ~ "National",
    cost_center_current == "113220 National Enterprise" ~ "National",
    cost_center_current == "113235 National America" ~ "National",
    cost_center_current == "113240 News National Environment" ~ "National",
    cost_center_current == "110006 News Content & Research" ~ "News Content and Research",
    cost_center_current == "110455 News Logistics" ~ "News Logistics",
    cost_center_current == "110410 Book World" ~ "Outlook",
    cost_center_current == "110460 Outlook" ~ "Outlook",
    cost_center_current == "110475 Polling" ~ "Polling",
    cost_center_current == "110015 Sports Main" ~ "Sports",
    cost_center_current == "110300 Style" ~ "Style",
    cost_center_current == "110435 Food" ~ "Style",
    cost_center_current == "110485 Travel" ~ "Style",
    cost_center_current == "110495 Local Living" ~ "Style",
    cost_center_current == "110505 Weekend" ~ "Style",
    cost_center_current == "110600 Universal Desk" ~ "Universal Desk",
    cost_center_current == "110652 News Video - General" ~ "Video",
    cost_center_current == "110663 Wake Up Report" ~ "Other",
    cost_center_current == "115000 Editorial Administration" ~ "Editorial",
    TRUE ~ "non-newsroom"))

df <- df %>%
  mutate(tier=case_when(
    desk == "National" ~ "Tier 1",
    desk == "Foreign" ~ "Tier 1",
    desk == "Financial" ~ "Tier 1",
    desk == "Investigative" ~ "Tier 1",
    desk == "Style" ~ "Tier 2",
    desk == "Local" ~ "Tier 2",
    desk == "Graphics" ~ "Tier 2",
    desk == "Universal Desk" ~ "Tier 2",
    desk == "Sports" ~ "Tier 2",
    desk == "Outlook" ~ "Tier 2",
    desk == "Editorial" ~ "Tier 2",
    desk == "Audio" ~ "Tier 3",
    desk == "Polling" ~ "Tier 3",
    desk == "Design" ~ "Tier 3",
    desk == "Operations" ~ "Tier 3",
    desk == "Multiplatform" ~ "Tier 3",
    desk == "Video" ~ "Tier 3",
    desk == "Audience Development and Engagement" ~ "Tier 3",
    desk == "News Logistics" ~ "Tier 4",
    desk == "News Content and Research" ~ "Tier 4",
    desk == "Emerging News Products" ~ "Tier 4",
    desk == "Other" ~ "Tier 4",
    TRUE ~ "Other"))


df <- df %>%
  mutate(race_grouping=case_when(
    race_ethnicity == "White (United States of America)" ~ "white",
    race_ethnicity == "Black or African American (United States of America)" ~ "person of color",
    race_ethnicity == "Asian (United States of America)" ~ "person of color",
    race_ethnicity == "Hispanic or Latino (United States of America)" ~ "person of color",
    race_ethnicity == "Two or More Races (United States of America)" ~ "person of color",
    race_ethnicity == "American Indian or Alaska Native (United States of America)" ~ "person of color",
    race_ethnicity == "Native Hawaiian or Other Pacific Islander (United States of America)" ~ "person of color",
    TRUE ~ "unknown"))


quart25 <- function(x) {
  num <- as.numeric(quantile(x, probs=c(0.25, .5, .75), na.rm=T))
  return(num[1])
}

quart50 <- function(x) {
  num <- as.numeric(quantile(x, probs=c(0.25, .5, .75), na.rm=T))
  return(num[2])
}

quart75 <- function(x) {
  num <- as.numeric(quantile(x, probs=c(0.25, .5, .75), na.rm=T))
  return(num[3])
}

summary1 <- df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active",
         group="News",
         pay="Salaried",
         id="gender, y40",
         race_grouping=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summary1b <- df2 %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="News",
         pay="Salaried",
         id="gender, y40",
         race_grouping=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summary2 <- df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="News",
         pay="Salaried",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summary2b <- df2 %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated",
         group="News",
         pay="Salaried",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summary3 <- df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="News",
         pay="Salaried",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summary3b <- df2 %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="News",
         pay="Salaried",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summary4 <- df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="News",
         pay="Salaried",
         id="race_ethnicity, y40",
         race_grouping=NA,
         gender=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summary4b <- df2 %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="News",
         pay="Salaried",
         id="race_ethnicity, y40",
         race_grouping=NA,
         gender=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)




summary5 <- df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="News",
         pay="Salaried",
         id="race_grouping, gender, y40",
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summary5b <- df2 %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="News",
         pay="Salaried",
         id="race_grouping, gender, y40",
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summary6 <- df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="News",
         pay="Salaried",
         id="race_ethnicity, gender, y40",
         race_grouping=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summary6b <- df2 %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="News",
         pay="Salaried",
         id="race_ethnicity, gender, y40",
         race_grouping=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)




summarycs1 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Salaried",
         id="gender, y40",
         race_grouping=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summarycs1b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="Commercial",
         pay="Salaried",
         id="gender, y40",
         race_grouping=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)




summarycs2 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Salaried",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summarycs2b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated",
         group="Commercial",
         pay="Salaried",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)




summarycs3 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Salaried",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summarycs3b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="Commercial",
         pay="Salaried",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)




summarycs4 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Salaried",
         id="race_ethnicity, y40",
         race_grouping=NA,
         gender=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summarycs4b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="Commercial",
         pay="Salaried",
         id="race_ethnicity, y40",
         race_grouping=NA,
         gender=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)




summarycs5 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Salaried",
         id="race_grouping, gender, y40",
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summarycs5b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="Commercial",
         pay="Salaried",
         id="race_grouping, gender, y40",
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summarycs6 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Salaried",
         id="race_ethnicity, gender, y40",
         race_grouping=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summarycs6b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="Commercial",
         pay="Salaried",
         id="race_ethnicity, gender, y40",
         race_grouping=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)




summarych1 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Salaried",
         id="gender, y40",
         race_grouping=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summarych1b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="Commercial",
         pay="Hourly",
         id="gender, y40",
         race_grouping=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)




summarych2 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Hourly",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summarych2b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated",
         group="Commercial",
         pay="Hourly",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summarych3 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Hourly",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summarych3b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="Commercial",
         pay="Hourly",
         id="race_grouping, y40",
         gender=NA,
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summarych4 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Hourly",
         id="race_ethnicity, y40",
         race_grouping=NA,
         gender=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summarych4b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="Commercial",
         pay="Hourly",
         id="race_ethnicity, y40",
         race_grouping=NA,
         gender=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)




summarych5 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Hourly",
         id="race_grouping, gender, y40",
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)


summarych5b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="Commercial",
         pay="Hourly",
         id="race_grouping, gender, y40",
         race_ethnicity=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summarych6 <- df %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="active", 
         group="Commercial",
         pay="Hourly",
         id="race_ethnicity, gender, y40",
         race_grouping=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



summarych6b <- df2 %>% 
  filter(dept=="Commercial") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "40 and over",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity, race_grouping, gender, y40) %>%
  summarize(total=n(),
            percentile_25 = quart25(current_base_pay),
            percentile_50 = quart50(current_base_pay),
            percentile_75 = quart75(current_base_pay)) %>%
  filter(total>=5) %>% 
  mutate(type="terminated", 
         group="Commercial",
         pay="Hourly",
         id="race_ethnicity, gender, y40",
         race_grouping=NA) %>% 
  select(type, group, pay, id, race_ethnicity, race_grouping, gender, y40, total, percentile_25, percentile_50, percentile_75)



e40 <- rbind(
  summary1, summary2, summary3, summary4, summary5, summary6,
  summary1b, summary2b, summary3b, summary4b, summary5b, summary6b,
  summarycs1, summarycs2, summarycs3, summarycs4, summarycs5, summarycs6,
  summarycs1b, summarycs2b, summarycs3b, summarycs4b, summarycs5, summarycs6b,
  summarych1, summarych2, summarych3, summarych4, summarych5, summarych6,
  summarych1b, summarych2b, summarych3b, summarych4b, summarych5, summarych6b,
  
)



groups <- data.frame(
  stringsAsFactors = FALSE,
  groups = c("dept, pay_rate_type, age_group_5, gender",
             "dept, pay_rate_type, age_group_5, race_ethnicity","dept, pay_rate_type, age_group_5, race_grouping",
             "dept, pay_rate_type, age_group_5, race_ethnicity, gender",
             "dept, pay_rate_type, age_group_5, race_grouping, gender",
             "dept, department, desk, pay_rate_type, age_group_5, gender",
             "dept, department, desk, pay_rate_type, age_group_5, race_ethnicity",
             "dept, department, desk, pay_rate_type, age_group_5, race_grouping",
             "dept, department, desk, pay_rate_type, age_group_5, race_ethnicity, gender",
             "dept, department, desk, pay_rate_type, age_group_5, race_grouping, gender",
             "dept, tier, pay_rate_type, age_group_5, gender",
             "dept, tier, pay_rate_type, age_group_5, race_ethnicity",
             "dept, tier, pay_rate_type, age_group_5, race_grouping",
             "dept, tier, pay_rate_type, age_group_5, race_ethnicity, gender",
             "dept, tier, pay_rate_type, age_group_5, race_grouping, gender",
             "dept, job_profile_current, pay_rate_type, age_group_5, gender",
             "dept, job_profile_current, pay_rate_type, age_group_5, race_ethnicity",
             "dept, job_profile_current, pay_rate_type, age_group_5, race_grouping",
             "dept, job_profile_current, pay_rate_type, age_group_5, race_ethnicity, gender",
             "dept, job_profile_current, pay_rate_type, age_group_5, race_grouping, gender","dept, pay_rate_type, age_group_10, gender",
             "dept, pay_rate_type, age_group_10, race_ethnicity",
             "dept, pay_rate_type, age_group_10, race_grouping",
             "dept, pay_rate_type, age_group_10, race_ethnicity, gender",
             "dept, pay_rate_type, age_group_10, race_grouping, gender",
             "dept, department, desk, pay_rate_type, age_group_10, gender",
             "dept, department, desk, pay_rate_type, age_group_10, race_ethnicity",
             "dept, department, desk, pay_rate_type, age_group_10, race_grouping",
             "dept, department, desk, pay_rate_type, age_group_10, race_ethnicity, gender",
             "dept, department, desk, pay_rate_type, age_group_10, race_grouping, gender",
             "dept, tier, pay_rate_type, age_group_10, gender",
             "dept, tier, pay_rate_type, age_group_10, race_ethnicity",
             "dept, tier, pay_rate_type, age_group_10, race_grouping",
             "dept, tier, pay_rate_type, age_group_10, race_ethnicity, gender",
             "dept, tier, pay_rate_type, age_group_10, race_grouping, gender",
             "dept, job_profile_current, pay_rate_type, age_group_10, gender",
             "dept, job_profile_current, pay_rate_type, age_group_10, race_ethnicity",
             "dept, job_profile_current, pay_rate_type, age_group_10, race_grouping",
             "dept, job_profile_current, pay_rate_type, age_group_10, race_ethnicity, gender",
             "dept, job_profile_current, pay_rate_type, age_group_10, race_grouping, gender",
             "gender","gender, pay_rate_type","race_ethnicity",
             "race_ethnicity, pay_rate_type","race_ethnicity, gender",
             "race_ethnicity, gender, pay_rate_type","age_group_5",
             "age_group_10","age_group_5, pay_rate_type",
             "age_group_10, pay_rate_type","dept","department","desk",
             "cost_center_current","pay_rate_type, desk",
             "pay_rate_type, cost_center_current","years_of_service_grouped",
             "years_of_service_grouped, pay_rate_type",
             "years_of_service_grouped, gender, pay_rate_type",
             "years_of_service_grouped, race_ethnicity",
             "years_of_service_grouped, race_ethnicity, pay_rate_type",
             "dept, department, desk, pay_rate_type, race_ethnicity",
             "dept, department, desk, pay_rate_type, gender",
             "dept, department, desk, pay_rate_type, race_ethnicity, gender",
             "dept, department, desk, pay_rate_type, race_grouping",
             "dept, department, desk, pay_rate_type, race_grouping, gender",
             "dept, department, desk, cost_center_current, pay_rate_type, race_ethnicity",
             "dept, department, desk, cost_center_current, pay_rate_type, gender",
             "dept, department, desk, cost_center_current, pay_rate_type, race_ethnicity, gender",
             "dept, department, desk, cost_center_current, pay_rate_type, race_grouping",
             "dept, department, desk, cost_center_current, pay_rate_type, race_grouping, gender",
             "dept, department, desk, cost_center_current, pay_rate_type",
             "dept, race_grouping, pay_rate_type","dept, race_grouping, pay_rate_type, gender",
             "pay_rate_type","dept, pay_rate_type",
             "dept, pay_rate_type, gender","dept, pay_rate_type, race_ethnicity",
             "dept, pay_rate_type, race_ethnicity, gender",
             "dept, department, desk, pay_rate_type",
             "dept, department, desk, age_group_5, pay_rate_type",
             "dept, department, desk, age_group_10, pay_rate_type","dept, age_group_10, pay_rate_type",
             "age_group_10, pay_rate_type")
)

select_columns <- c("dept", "department", "desk", "cost_center_current", "tier", "job_profile_current", "pay_rate_type", "race_ethnicity", "race_grouping", "age_group_5", "age_group_10", "years_of_service_grouped", 
                    "gender", "employees", "percentile_25_age", "percentile_50_age", "percentile_75_age", "percentile_25_pay", "percentile_50_pay", "percentile_75_pay")

select_columns_narrow <- c("dept", "department", "desk", "cost_center_current", "tier", "job_profile_current", "pay_rate_type", "race_ethnicity", "race_grouping", "age_group_5", "age_group_10", "years_of_service_grouped", 
                    "gender")

dfi <- data.frame(select_columns_narrow)

for (i in 1:nrow(groups)) {
  
  summary_columns <- strsplit(groups$groups[i], split=", ")[[1]]
  
  summary_columnsdf <- data.frame(summary_columns)
  
  blank_columns <-filter(dfi, !select_columns_narrow %in% summary_columns) %>% pull(select_columns_narrow)
  
  summarized <- df %>%  
    group_by_at(summary_columns) %>% 
    summarize(employees=n(), 
              percentile_25_age=quart25(age),
              percentile_50_age=quart50(age),
              percentile_75_age=quart75(age),
              percentile_25_pay=quart25(current_base_pay),
              percentile_50_pay=quart50(current_base_pay),
              percentile_75_pay=quart75(current_base_pay)) %>% 
    ungroup() %>% 
    filter(employees>=5) %>% 
    `[<-`(., blank_columns, value = NA) %>% 
    select_at(select_columns) %>% 
    mutate(id=i)
  
  if (i==1) {
    explorer_df <- summarized
  } else {
    explorer_df <- rbind(explorer_df, summarized)
  }
  
}
