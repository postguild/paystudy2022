
library(tidyverse)
library(lubridate)
library(knitr)

## Retention

at20 <- read_csv("raw_data/active_wd_2020.csv") %>% 
  select(employee_id, hire_date, race_ethnicity) %>% 
  mutate(year20=T)

active_21 <- read_csv("raw_data/active_wd_2021.csv")  %>% 
  select(employee_id=`Employee ID`, hire_date=`Hire Date`) %>% 
  mutate(hire_date=mdy(hire_date))

terminated_21 <- read_csv("raw_data/terminated_wd_2021.csv") %>% 
  select(employee_id=`Employee ID`, hire_date=`Hire Date`) %>% 
  mutate(hire_date=mdy(hire_date))

at21 <- rbind(active_21, terminated_21)  %>% 
  mutate(year21=T)

at2122 <- anti_join(at20, at21)
cat(paste0("Missing employees between 2020 and 2021 data: ", nrow(at2122)))


df <- read_csv("raw_data/active_wd_2021.csv")

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

for (i in 1:26) {
  
  df_year <- df %>% 
    select(department, employee_id, gender, race_ethnicity,
           military_status, date_of_birth,
           original_hire_date, hire_date, pay_rate_type,
           current_base_pay,
           job_profile_current, time_type_current, cost_center_current,
           ends_with(paste0("e", as.character(i))),
           ends_with(paste0("n", as.character(i))),
           ends_with(paste0("r", as.character(i)))
    ) %>% 
    rename(pay_rate_type_current=pay_rate_type) %>% 
    mutate(termination_date=ymd("1900-01-01"))
  
  colnames(df_year) <- gsub(paste0("n", as.character(i)), "n", colnames(df_year))
  colnames(df_year) <- gsub(paste0("e", as.character(i)), "e", colnames(df_year))
  colnames(df_year) <- gsub(paste0("r", as.character(i)), "r", colnames(df_year))
  
  
  if(!"effective_date" %in% colnames(df_year)) {
    df_year$effective_date <- NA
  }
  
  
  if(!"business_process_type" %in% colnames(df_year)) {
    df_year$business_process_type <- NA
  }
  
  
  if(!"business_process_reason" %in% colnames(df_year)) {
    df_year$business_process_reason <- NA
  }
  
  
  if(!"pay_rate_type" %in% colnames(df_year)) {
    df_year$pay_rate_type <- NA
  }
  
  
  if(!"base_pay_change" %in% colnames(df_year)) {
    df_year$base_pay_change <- NA
  }
  
  
  if(!"job_profile" %in% colnames(df_year)) {
    df_year$job_profile <- NA
  }
  
  if(!"time_type" %in% colnames(df_year)) {
    df_year$time_type<- NA
  }
  
  
  if(!"cost_center" %in% colnames(df_year)) {
    df_year$cost_center<- NA
  }
  
  df_year <- df_year %>% 
    select(department, employee_id, gender, race_ethnicity,
           military_status,
           date_of_birth,
           original_hire_date, hire_date, termination_date,
           pay_rate_type_current,
           current_base_pay,
           job_profile_current, time_type_current, cost_center_current,
           effective_date,
           business_process_type, business_process_reason, pay_rate_type,
           base_pay_change, job_profile, time_type, cost_center)
  df_year$workday <- i
  
  if (i==1) {
    df_mega <- df_year
  } else {
    df_mega <- rbind(df_mega, df_year)
  }
  
}

df_mega <- filter(df_mega, !is.na(cost_center))

data_date <- ymd("2021-05-28")

df_mega <- df_mega %>% mutate(date_of_birth = mdy(date_of_birth),
                              age = floor(decimal_date(data_date) - decimal_date(date_of_birth)))


df_mega <- df_mega %>% 
  mutate(date_of_birth = case_when(
    age < 0 ~ date_of_birth-years(100),
    TRUE ~ date_of_birth),
    age = floor(decimal_date(data_date) - decimal_date(date_of_birth)))

df_mega <- df_mega %>% mutate(hire_date = mdy(hire_date),
                              years_of_service = floor(decimal_date(data_date) - decimal_date(hire_date)))

df_mega <- df_mega %>% 
  mutate(hire_date = case_when(
    years_of_service < 0 ~ hire_date-years(100),
    TRUE ~ hire_date),
    years_of_service = case_when(
      years_of_service < 0 ~ floor(decimal_date(data_date) - decimal_date(hire_date)),
      TRUE ~ years_of_service
    ))

df_mega <- df_mega %>% 
  mutate(original_hire_date=mdy(original_hire_date),
         termination_date=ymd(termination_date),
         effective_date=mdy(effective_date))


df_mega <- df_mega %>% 
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


df_mega <- df_mega %>% 
  mutate(age_group_10=case_when(
    age < 25 ~ "<25",
    age >= 25 & age < 35 ~ "25-34",
    age >= 35 & age < 45 ~ "35-44",
    age >= 45 & age < 55 ~ "45-55",
    age >= 55 & age < 65 ~ "55-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_))


df_mega <- df_mega %>% 
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


df_mega <- df_mega %>%
  mutate(dept=case_when(
    department == 'News' ~ 'News', 
    department == 'Editorial' ~ 'News',
    department == 'Client Solutions' ~ 'Commercial',
    department == 'Circulation' ~ 'Commercial',
    department == 'Finance' ~ 'Commercial',
    department == 'Marketing' ~ 'Commercial',
    department == 'WP News Media Services' ~ 'Commercial',
    department == 'Production' ~ 'Commercial',
    department == 'Public Relations' ~ 'Commercial',
    department == 'Administration' ~ 'Commercial',
    department == 'Product'~ 'Commercial',
    TRUE ~ 'Other'))

df_mega <- df_mega %>%
  mutate(desk=case_when(
    cost_center_current == '110000 News Operations' ~ 'Operations',
    cost_center_current == '110001 News Digital Operations' ~ 'Operations',
    cost_center_current == '110610 Audience Development and Engagement' ~ 'Audience Development and Engagement',
    cost_center_current == '110620 News Audio' ~ 'Audio',
    cost_center_current == '110604 Presentation Design' ~ 'Design',
    cost_center_current == '110605 Presentation' ~ 'Photography',
    cost_center_current == '110664 News National Apps' ~ 'Emerging News Products',
    cost_center_current == '110665 News The Lily' ~ 'Emerging News Products',
    cost_center_current == '110666 News Snapchat' ~ 'Emerging News Products',
    cost_center_current == '110667 News By The Way' ~ 'Emerging News Products',
    cost_center_current == '113210 Economy and Business' ~ 'Financial',
    cost_center_current == '114000 Foreign Administration' ~ 'Foreign',
    cost_center_current == '114095 News Foreign Brazil' ~ 'Foreign',
    cost_center_current == '114100 Foreign Latam' ~ 'Foreign',
    cost_center_current == '114220 News Foreign Istanbul' ~ 'Foreign',
    cost_center_current == '114235 Foreign Western Europe' ~ 'Foreign',
    cost_center_current == '114300 News Foreign West Africa' ~ 'Foreign',
    cost_center_current == '114415 Foreign Hong Kong' ~ 'Foreign',
    cost_center_current == '114405 Foreign Beijing Bureau' ~ 'Foreign',
    cost_center_current == '114105 Foreign Mexico Bureau' ~ 'Foreign',
    cost_center_current == '114005 Foreign Beirut Bureau' ~ 'Foreign',
    cost_center_current == '114400 Foreign India Bureau' ~ 'Foreign',
    cost_center_current == '114410 Foreign Tokyo Bureau' ~ 'Foreign',
    cost_center_current == '114205 Foreign Islamabad Bureau' ~ 'Foreign',
    cost_center_current == '114305 Foreign Nairobi Bureau' ~ 'Foreign',
    cost_center_current == '114240 Foreign Rome Bureau' ~ 'Foreign',
    cost_center_current == '114200 Foreign London Bureau' ~ 'Foreign',
    cost_center_current == '114230 Foreign Moscow Bureau' ~ 'Foreign',
    cost_center_current == '114225 Foreign Cairo Bureau' ~ 'Foreign',
    cost_center_current == '114215 Foreign Berlin Bureau' ~ 'Foreign',
    cost_center_current == "114310 Foreign Baghdad Bureau" ~ "Foreign",
    cost_center_current == "114315 Foreign Jerusalem Bureau" ~ "Foreign",
    cost_center_current == '110603 Presentation Graphics' ~ 'Graphics',
    cost_center_current == '110450 Investigative' ~ 'Investigative',
    cost_center_current == '112300 Local Politics and Government' ~ 'Local',
    cost_center_current == '110601 Multiplatform Desk' ~ 'Multiplatform',
    cost_center_current == '110500 Magazine' ~ 'National',
    cost_center_current == '113200 National Politics and Government' ~ 'National',
    cost_center_current == '113205 National Security' ~ 'National',
    cost_center_current == '113215 News National Health & Science' ~ 'National',
    cost_center_current == '113220 National Enterprise' ~ 'National',
    cost_center_current == '113235 National America' ~ 'National',
    cost_center_current == '113240 News National Environment' ~ 'National',
    cost_center_current == '110006 News Content & Research' ~ 'News Content and Research',
    cost_center_current == '110455 News Logistics' ~ 'News Logistics',
    cost_center_current == '110410 Book World' ~ 'Outlook',
    cost_center_current == '110460 Outlook' ~ 'Outlook',
    cost_center_current == '110475 Polling' ~ 'Polling',
    cost_center_current == '110015 Sports Main' ~ 'Sports',
    cost_center_current == '110300 Style' ~ 'Style',
    cost_center_current == '110435 Food' ~ 'Style',
    cost_center_current == '110485 Travel' ~ 'Style',
    cost_center_current == '110495 Local Living' ~ 'Style',
    cost_center_current == '110505 Weekend' ~ 'Style',
    cost_center_current == '110600 Universal Desk' ~ 'Universal Desk',
    cost_center_current == '110652 News Video - General' ~ 'Video',
    cost_center_current == '110663 Wake Up Report' ~ 'Other',
    cost_center_current == '115000 Editorial Administration' ~ 'Editorial',
    TRUE ~ 'non-newsroom'))


df_mega <- df_mega %>%
  mutate(tier=case_when(
    desk == 'National' ~ 'Tier 1',
    desk == 'Foreign' ~ 'Tier 1',
    desk == 'Financial' ~ 'Tier 1',
    desk == 'Investigative' ~ 'Tier 1',
    desk == 'Style' ~ 'Tier 2',
    desk == 'Local' ~ 'Tier 2',
    desk == 'Graphics' ~ 'Tier 2',
    desk == 'Universal Desk' ~ 'Tier 2',
    desk == 'Sports' ~ 'Tier 2',
    desk == 'Outlook' ~ 'Tier 2',
    desk == 'Editorial' ~ 'Tier 2',
    desk == 'Audio' ~ 'Tier 3',
    desk == 'Polling' ~ 'Tier 3',
    desk == 'Design' ~ 'Tier 3',
    desk == 'Operations' ~ 'Tier 3',
    desk == 'Multiplatform' ~ 'Tier 3',
    desk == 'Video' ~ 'Tier 3',
    desk == 'Audience Development and Engagement' ~ 'Tier 3',
    desk == 'News Logistics' ~ 'Tier 4',
    desk == 'News Content and Research' ~ 'Tier 4',
    desk == 'Emerging News Products' ~ 'Tier 4',
    desk == 'Other' ~ 'Tier 4',
    TRUE ~ 'Other'))


df_mega <- df_mega %>%
  mutate(race_grouping=case_when(
    race_ethnicity == 'White (United States of America)' ~ 'white',
    race_ethnicity == 'Black or African American (United States of America)' ~ 'person of color',
    race_ethnicity == 'Asian (United States of America)' ~ 'person of color',
    race_ethnicity == 'Hispanic or Latino (United States of America)' ~ 'person of color',
    race_ethnicity == 'Two or More Races (United States of America)' ~ 'person of color',
    race_ethnicity == 'American Indian or Alaska Native (United States of America)' ~ 'person of color',
    race_ethnicity == 'Native Hawaiian or Other Pacific Islander (United States of America)' ~ 'person of color',
    TRUE ~ 'unknown'))


mega_df_columns <- data.frame(colnames(df_mega))

df_columns <- data.frame(colnames(df))

df2 <- read_csv("raw_data/terminated_wd_2021.csv")

colnames(df2) <- c("department","employee_id","gender",   
                   "race_ethnicity","education", "military_status",
                   "date_of_birth", "original_hire_date","hire_date", "termination_date",
                   "pay_rate_type", "current_base_pay", 
                   "job_profile_current","time_type_current", "cost_center_current",  
                   "effective_date1", "business_process_type1", "business_process_reason1", "pay_rate_type1", "base_pay_change1", "job_profile1", "time_type1", "cost_center1", 
                   "effective_date2", "business_process_type2", "business_process_reason2", "pay_rate_type2", "base_pay_change2", "job_profile2", "time_type2", "cost_center2", 
                   "effective_date3", "business_process_type3", "business_process_reason3", "pay_rate_type3", "base_pay_change3", "job_profile3", "time_type3", "cost_center3",
                   "effective_date4", "business_process_type4", "business_process_reason4", "pay_rate_type_4", "base_pay_change4", "job_profile4", "time_type4", "cost_center4", 
                   "effective_date5", "business_process_type5", "business_process_reason5", "pay_rate_type_5", "base_pay_change5", "job_profile5", "time_type5", "cost_center5", 
                   "effective_date6", "business_process_type6", "business_process_reason6", "pay_rate_type_6", "base_pay_change6", "job_profile6", "time_type6", "cost_center6",
                   "effective_date7", "business_process_type7", "business_process_reason7", "pay_rate_type_7", "base_pay_change7", "job_profile7", "time_type7", "cost_center7", 
                   "effective_date8", "business_process_type8", "business_process_reason8", "pay_rate_type_8", "base_pay_change8", "job_profile8", "time_type8", "cost_center8", 
                   "effective_date9", "business_process_type9", "business_process_reason9", "pay_rate_type_9", "base_pay_change9", "job_profile9", "time_type9", "cost_center_9",
                   "effective_date10", "business_process_type10", "business_process_reason10", "pay_rate_type10", "base_pay_change10", "job_profile10", "time_type10", "cost_center10",
                   "effective_date11", "business_process_type11", "business_process_reason11", "pay_rate_type11", "base_pay_change11", "job_profile11", "time_type11", "cost_center11",
                   "effective_date12", "business_process_type12", "business_process_reason12", "pay_rate_type12", "base_pay_change12", "job_profile12", "time_type12", "cost_center12", 
                   "effective_date13", "business_process_type13", "business_process_reason13", "pay_rate_type13", "base_pay_change13", "job_profile13", "time_type13", "cost_center13", 
                   "effective_date14", "business_process_type14", "business_process_reason14", "pay_rate_type14", "base_pay_change14", "job_profile14", "time_type14", "cost_center14",
                   "effective_date15", "business_process_type15", "business_process_reason15", "pay_rate_type15", "base_pay_change15", "job_profile15", "time_type15", "cost_center15", 
                   "2008_annual_performance_rating", "2009_annual_performance_rating", "2010_annual_performance_rating",
                   "2011_annual_performance_rating", "2012_annual_performance_rating", "2013_annual_performance_rating",
                   "2014_annual_performance_rating", "2015_annual_performance_rating", "2016_annual_performance_rating",
                   "2017_annual_performance_rating", "2018_annual_performance_rating", "2019_annual_performance_rating",
                   "2020_annual_performance_rating")



for (i in 1:15) {
  
  df2_year <- df2 %>% 
    select(department, employee_id, gender, race_ethnicity,
           military_status, date_of_birth,
           original_hire_date, hire_date, termination_date, 
           pay_rate_type,
           current_base_pay,
           job_profile_current, time_type_current, cost_center_current,
           ends_with(paste0("e", as.character(i))),
           ends_with(paste0("n", as.character(i))),
           ends_with(paste0("r", as.character(i)))
    ) %>% 
    rename(pay_rate_type_current=pay_rate_type) 
  
  colnames(df2_year) <- gsub(paste0("n", as.character(i)), "n", colnames(df2_year))
  colnames(df2_year) <- gsub(paste0("e", as.character(i)), "e", colnames(df2_year))
  colnames(df2_year) <- gsub(paste0("r", as.character(i)), "r", colnames(df2_year))
  
  if(!"effective_date" %in% colnames(df2_year)) {
    df2_year$effective_date <- NA
  }
  
  
  if(!"business_process_type" %in% colnames(df2_year)) {
    df2_year$business_process_type <- NA
  }
  
  
  if(!"business_process_reason" %in% colnames(df2_year)) {
    df2_year$business_process_reason <- NA
  }
  
  
  if(!"pay_rate_type" %in% colnames(df2_year)) {
    df2_year$pay_rate_type <- NA
  }
  
  
  if(!"base_pay_change" %in% colnames(df2_year)) {
    df2_year$base_pay_change <- NA
  }
  
  
  if(!"job_profile" %in% colnames(df2_year)) {
    df2_year$job_profile <- NA
  }
  
  if(!"time_type" %in% colnames(df2_year)) {
    df2_year$time_type<- NA
  }
  
  
  if(!"cost_center" %in% colnames(df2_year)) {
    df2_year$cost_center<- NA
  }
  
  df2_year <- df2_year %>% 
    select(department, employee_id, gender, military_status,
           race_ethnicity,
           date_of_birth,
           original_hire_date, hire_date, termination_date,
           pay_rate_type_current,
           current_base_pay,
           job_profile_current, time_type_current, cost_center_current,
           effective_date,
           business_process_type, business_process_reason, pay_rate_type,
           base_pay_change, job_profile, time_type, cost_center)
  df2_year$workday <- i
  
  if (i==1) {
    df2_mega <- df2_year
  } else {
    df2_mega <- rbind(df2_mega, df2_year)
  }
  
}


data_date <- ymd("2021-05-28")

df2_mega <- df2_mega %>% mutate(date_of_birth = mdy(date_of_birth),
                                age = floor(decimal_date(data_date) - decimal_date(date_of_birth)))


df2_mega <- df2_mega %>% 
  mutate(date_of_birth = case_when(
    age < 0 ~ date_of_birth-years(100),
    TRUE ~ date_of_birth),
    age = floor(decimal_date(data_date) - decimal_date(date_of_birth)))



df2_mega <- df2_mega %>% mutate(hire_date = mdy(hire_date),
                                years_of_service = floor(decimal_date(data_date) - decimal_date(hire_date)))

df2_mega <- df2_mega %>% 
  mutate(hire_date = case_when(
    years_of_service < 0 ~ hire_date-years(100),
    TRUE ~ hire_date),
    years_of_service = case_when(
      years_of_service < 0 ~ floor(decimal_date(data_date) - decimal_date(hire_date)),
      TRUE ~ years_of_service
    ))

df2_mega <- df2_mega %>% 
  mutate(original_hire_date=mdy(original_hire_date),
         termination_date=mdy(termination_date),
         effective_date=mdy(effective_date))

df2_mega <- df2_mega %>% 
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

df2_mega <- df2_mega %>% 
  mutate(age_group_10=case_when(
    age < 25 ~ "<25",
    age >= 25 & age < 35 ~ "25-34",
    age >= 35 & age < 45 ~ "35-44",
    age >= 45 & age < 55 ~ "45-55",
    age >= 55 & age < 65 ~ "55-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_))

df2_mega <- df2_mega %>% 
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


df2_mega <- df2_mega %>%
  mutate(dept=case_when(
    department == 'News' ~ 'News', 
    department == 'Editorial' ~ 'News',
    department == 'Client Solutions' ~ 'Commercial',
    department == 'Circulation' ~ 'Commercial',
    department == 'Finance' ~ 'Commercial',
    department == 'Marketing' ~ 'Commercial',
    department == 'WP News Media Services' ~ 'Commercial',
    department == 'Production' ~ 'Commercial',
    department == 'Public Relations' ~ 'Commercial',
    department == 'Administration' ~ 'Commercial',
    department == 'Product'~ 'Commercial',
    TRUE ~ 'Other'))


df2_mega <- df2_mega %>%
  mutate(desk=case_when(
    cost_center_current == '110000 News Operations' ~ 'Operations',
    cost_center_current == '110001 News Digital Operations' ~ 'Operations',
    cost_center_current == '110610 Audience Development and Engagement' ~ 'Audience Development and Engagement',
    cost_center_current == '110620 News Audio' ~ 'Audio',
    cost_center_current == '110604 Presentation Design' ~ 'Design',
    cost_center_current == '110605 Presentation' ~ 'Photography',
    cost_center_current == '110664 News National Apps' ~ 'Emerging News Products',
    cost_center_current == '110665 News The Lily' ~ 'Emerging News Products',
    cost_center_current == '110666 News Snapchat' ~ 'Emerging News Products',
    cost_center_current == '110667 News By The Way' ~ 'Emerging News Products',
    cost_center_current == '113210 Economy and Business' ~ 'Financial',
    cost_center_current == '114000 Foreign Administration' ~ 'Foreign',
    cost_center_current == '114095 News Foreign Brazil' ~ 'Foreign',
    cost_center_current == '114100 Foreign Latam' ~ 'Foreign',
    cost_center_current == '114220 News Foreign Istanbul' ~ 'Foreign',
    cost_center_current == '114235 Foreign Western Europe' ~ 'Foreign',
    cost_center_current == '114300 News Foreign West Africa' ~ 'Foreign',
    cost_center_current == '114415 Foreign Hong Kong' ~ 'Foreign',
    cost_center_current == '114405 Foreign Beijing Bureau' ~ 'Foreign',
    cost_center_current == '114105 Foreign Mexico Bureau' ~ 'Foreign',
    cost_center_current == '114005 Foreign Beirut Bureau' ~ 'Foreign',
    cost_center_current == '114400 Foreign India Bureau' ~ 'Foreign',
    cost_center_current == '114410 Foreign Tokyo Bureau' ~ 'Foreign',
    cost_center_current == '114205 Foreign Islamabad Bureau' ~ 'Foreign',
    cost_center_current == '114305 Foreign Nairobi Bureau' ~ 'Foreign',
    cost_center_current == '114240 Foreign Rome Bureau' ~ 'Foreign',
    cost_center_current == '114200 Foreign London Bureau' ~ 'Foreign',
    cost_center_current == '114230 Foreign Moscow Bureau' ~ 'Foreign',
    cost_center_current == '114225 Foreign Cairo Bureau' ~ 'Foreign',
    cost_center_current == '114215 Foreign Berlin Bureau' ~ 'Foreign',
    cost_center_current == "114310 Foreign Baghdad Bureau" ~ "Foreign",
    cost_center_current == "114315 Foreign Jerusalem Bureau" ~ "Foreign",
    cost_center_current == '110603 Presentation Graphics' ~ 'Graphics',
    cost_center_current == '110450 Investigative' ~ 'Investigative',
    cost_center_current == '112300 Local Politics and Government' ~ 'Local',
    cost_center_current == '110601 Multiplatform Desk' ~ 'Multiplatform',
    cost_center_current == '110500 Magazine' ~ 'National',
    cost_center_current == '113200 National Politics and Government' ~ 'National',
    cost_center_current == '113205 National Security' ~ 'National',
    cost_center_current == '113215 News National Health & Science' ~ 'National',
    cost_center_current == '113220 National Enterprise' ~ 'National',
    cost_center_current == '113235 National America' ~ 'National',
    cost_center_current == '113240 News National Environment' ~ 'National',
    cost_center_current == '110006 News Content & Research' ~ 'News Content and Research',
    cost_center_current == '110455 News Logistics' ~ 'News Logistics',
    cost_center_current == '110410 Book World' ~ 'Outlook',
    cost_center_current == '110460 Outlook' ~ 'Outlook',
    cost_center_current == '110475 Polling' ~ 'Polling',
    cost_center_current == '110015 Sports Main' ~ 'Sports',
    cost_center_current == '110300 Style' ~ 'Style',
    cost_center_current == '110435 Food' ~ 'Style',
    cost_center_current == '110485 Travel' ~ 'Style',
    cost_center_current == '110495 Local Living' ~ 'Style',
    cost_center_current == '110505 Weekend' ~ 'Style',
    cost_center_current == '110600 Universal Desk' ~ 'Universal Desk',
    cost_center_current == '110652 News Video - General' ~ 'Video',
    cost_center_current == '110663 Wake Up Report' ~ 'Other',
    cost_center_current == '115000 Editorial Administration' ~ 'Editorial',
    TRUE ~ 'non-newsroom'))


df2_mega <- df2_mega %>%
  mutate(tier=case_when(
    desk == 'National' ~ 'Tier 1',
    desk == 'Foreign' ~ 'Tier 1',
    desk == 'Financial' ~ 'Tier 1',
    desk == 'Investigative' ~ 'Tier 1',
    desk == 'Style' ~ 'Tier 2',
    desk == 'Local' ~ 'Tier 2',
    desk == 'Graphics' ~ 'Tier 2',
    desk == 'Universal Desk' ~ 'Tier 2',
    desk == 'Sports' ~ 'Tier 2',
    desk == 'Outlook' ~ 'Tier 2',
    desk == 'Editorial' ~ 'Tier 2',
    desk == 'Audio' ~ 'Tier 3',
    desk == 'Polling' ~ 'Tier 3',
    desk == 'Design' ~ 'Tier 3',
    desk == 'Operations' ~ 'Tier 3',
    desk == 'Multiplatform' ~ 'Tier 3',
    desk == 'Video' ~ 'Tier 3',
    desk == 'Audience Development and Engagement' ~ 'Tier 3',
    desk == 'News Logistics' ~ 'Tier 4',
    desk == 'News Content and Research' ~ 'Tier 4',
    desk == 'Emerging News Products' ~ 'Tier 4',
    desk == 'Other' ~ 'Tier 4',
    TRUE ~ 'Other'))


df2_mega <- df2_mega %>%
  mutate(race_grouping=case_when(
    race_ethnicity == 'White (United States of America)' ~ 'white',
    race_ethnicity == 'Black or African American (United States of America)' ~ 'person of color',
    race_ethnicity == 'Asian (United States of America)' ~ 'person of color',
    race_ethnicity == 'Hispanic or Latino (United States of America)' ~ 'person of color',
    race_ethnicity == 'Two or More Races (United States of America)' ~ 'person of color',
    race_ethnicity == 'American Indian or Alaska Native (United States of America)' ~ 'person of color',
    race_ethnicity == 'Native Hawaiian or Other Pacific Islander (United States of America)' ~ 'person of color',
    TRUE ~ 'unknown'))

df2_mega %>% count(year(termination_date))


df_mega$data <- "active"
df2_mega$data <- "terminated"
df3_mega <- rbind(df_mega, df2_mega)


df <- df3_mega

comes_goes <- df %>% 
  rename(data_type=data) %>% 
  mutate(termination_year=year(termination_date))

comes_goes$termination_year <- ifelse(comes_goes$termination_year==1900, NA, comes_goes$termination_year)

comes_goes$eff_year = ifelse(comes_goes$data_type=="active", 2022-comes_goes$years_of_service, comes_goes$termination_year)

comes_goes$data_year= year(comes_goes$effective_date)
comes_goes_active <- comes_goes %>% filter(data_type=="active")
comes_goes_active$year_type <- ifelse(year(comes_goes_active$hire_date)==year(comes_goes_active$effective_date), "hired", "active")
comes_goes_term <- comes_goes %>% filter(data_type=="terminated")

comes_goes_term <- comes_goes_term %>% 
  group_by(employee_id) %>% 
  arrange(employee_id, effective_date) %>% 
  mutate(row=row_number()) 

comes_goes_term_solo <- comes_goes_term %>% 
  filter(row_number()==n()) %>% 
  mutate(year_type="terminated")

comes_goes_term <- left_join(comes_goes_term, comes_goes_term_solo)
comes_goes_term$year_type <- ifelse(is.na(comes_goes_term$year_type), "active", comes_goes_term$year_type)
comes_goes_term$row <- NULL

comes_goes <- rbind(comes_goes_active, comes_goes_term)

comes_goes2 <- comes_goes %>% 
  group_by(employee_id, data_year) %>%
  arrange(employee_id, data_year, desc(year_type)) %>% 
  slice(1)

uniques <- comes_goes2 %>% 
  ungroup() %>% 
  select(employee_id, hire_date, termination_date, data_type, termination_year, data_year, year_type) %>% 
  unique() %>% 
  filter(year_type=="hired")

ids <- comes_goes %>% 
  ungroup() %>% 
  select(employee_id, hire_date, termination_date) %>% 
  unique() %>% 
  mutate(start_year=year(hire_date), end_year=year(termination_date))

ids$end_year <- ifelse(ids$end_year==1900, 2021, ids$end_year)


for(i in 1:nrow(ids)) {
  
  data_year <- ids$start_year[i]:ids$end_year[i]
  df_solo <- data.frame(data_year)
  df_solo$employee_id <- ids$employee_id[i]
  
  
  df_solo <- left_join(df_solo, uniques) %>% 
    select(employee_id, data_year, year_type)
  
  if (i==1) {
    df <- df_solo
  } else {
    df <- rbind(df, df_solo)
  }
  print(i)
}


cg_narrow <- comes_goes %>% 
  group_by(employee_id) %>% 
  arrange(employee_id, effective_date) %>% 
  filter(row_number()==n()) %>% 
  select(department, employee_id, gender, race_ethnicity,
         date_of_birth, original_hire_date, hire_date,
         termination_date, pay_rate_type_current,
         current_base_pay, job_profile_current,
         time_type_current, cost_center_current,
         cost_center, age, years_of_service,
         age_group_5, age_group_10, years_of_service_grouped,
         dept, desk, tier, race_grouping) %>% 
  unique()

# 
df<- df %>% 
  left_join(cg_narrow) %>% 
  mutate(year_type=case_when(
    year(termination_date)==data_year~ "terminated",
    TRUE ~ year_type
  ))

df$year_type <- ifelse(is.na(df$year_type), "active", df$year_type)

#### hires versus terminations by year (total company)

#From 2016 to 2021, The Post hired more women than men across the company. Much of that growth happened in the Newsroom.
df %>%   
  filter(data_year >=2016) %>% 
  count(gender, year_type)  %>%
  filter(year_type!="active") %>% 
  pivot_wider(names_from="year_type", values_from="n") %>% 
  kable()

#The number of women in Commercial, on the other hand, dramatically shrank.
#More than twice as many women as men in Commercial have departed. Our analysis showed that 2019 was a standout year for turnover among female staffers in Commercial, with nearly 40 women leaving The Post.
df %>% 
  filter(data_year >=2016) %>% 
  count(dept, gender, year_type) %>% 
  filter(year_type!="active") %>% 
  pivot_wider(names_from="year_type", values_from="n") %>% 
  mutate(category=case_when(
    hired>terminated ~ "More hired than left",
    terminated<hired ~ "More left than hired",
    terminated==hired ~ "Same",
    TRUE ~ ""
  )) %>% 
  kable()

#From 2016 to 2021, more workers of color left in Commercial than were hired, with the exception of Hispanic or Latino workers, whose numbers remained consistent. Demographic groups of fewer than five were excluded to protect individuals’ privacy.
df %>% 
  filter(data_year >=2016) %>% 
  count(dept, race_ethnicity, year_type) %>% 
  filter(year_type!="active") %>% 
  pivot_wider(names_from="year_type", values_from="n") %>% 
  filter(hired>=5 & terminated>=5) %>% 
  mutate(category=case_when(
    hired>terminated ~ "More hired than left",
    terminated<hired ~ "More left than hired",
    terminated==hired ~ "Same",
    TRUE ~ ""
  )) %>% 
  kable()

#In the Newsroom, on the other hand, more workers were hired than left. For every employee that left, 1.8 employees were hired. 
df %>% 
  filter(data_year >=2016) %>% 
  count(dept,year_type) %>% 
  filter(year_type!="active") %>% 
  pivot_wider(names_from="year_type", values_from="n") %>% 
  mutate(category=case_when(
    hired>terminated ~ "More hired than left",
    terminated<hired ~ "More left than hired",
    terminated==hired ~ "Same",
    TRUE ~ ""
  )) %>% 
  kable()
#370/205 = 1.8

#However, that was not the case for Black employees. For every Black employee who left, only 1.2 Black employees were hired to replace them. 

df %>% 
  filter(data_year >=2016) %>% 
  count(dept, race_ethnicity, year_type) %>% 
  filter(year_type!="active") %>% 
  pivot_wider(names_from="year_type", values_from="n") %>% 
  filter(hired>=5 & terminated>=5) %>% 
  mutate(category=case_when(
    hired>terminated ~ "More hired than left",
    terminated<hired ~ "More left than hired",
    terminated==hired ~ "Same",
    TRUE ~ ""
  )) %>% 
  kable()
#37/31 = 1.2

#A deeper analysis of the data shows that in 2020 alone more than 1 out of 3 workers who left the Newsroom were Black. Fewer than 1 in 5 of those hired that year were Black.
df %>% 
  filter(data_year ==2020) %>% 
  filter(dept=="News") %>% 
  count(dept, race_ethnicity, data_year, year_type) %>% 
  filter(year_type!="active") %>% 
  pivot_wider(names_from="year_type", values_from="n") %>% 
  #filter(hired>=5 & terminated>=5) %>% 
  mutate(category=case_when(
    hired>terminated ~ "More hired than left",
    terminated<hired ~ "More left than hired",
    terminated==hired ~ "Same",
    TRUE ~ ""
  )) %>% 
  kable()
#10/29 = .34
#9/51 =.176 or 1/5=.2

#In 2017, only 17 percent of Newsroom hires were people of color. 
#In 2020 and in the first half of 2021, however, they made up 55 percent of new hires.  
view_this <-df %>% 
  filter(dept=="News") %>% 
  count(dept, race_grouping, data_year, year_type) %>% 
  filter(year_type!="active") %>% 
  pivot_wider(names_from="year_type", values_from="n") %>% 
  filter(hired>=5 & terminated>=5) %>% 
  mutate(category=case_when(
    hired>terminated ~ "More hired than left",
    terminated<hired ~ "More left than hired",
    terminated==hired ~ "Same",
    TRUE ~ ""
  )) %>% 
  kable()

#12/(12+5+54)=.17


#Of the hires in 2020 and so far in 2021, 55 percent have been people of color. 

df %>% 
  filter(dept=="News") %>% 
  filter(data_year==2020 | data_year==2021) %>% 
  count(dept, race_grouping, year_type) %>% 
  filter(year_type!="active") %>% 
  pivot_wider(names_from="year_type", values_from="n") %>% 
  mutate(category=case_when(
    hired>terminated ~ "More hired than left",
    terminated<hired ~ "More left than hired",
    terminated==hired ~ "Same",
    TRUE ~ ""
  )) %>% 
  kable()

#43/(43+7+32) = .52
#18/(18+2+20) = .45


## Newsroom

#In the Newsroom, journalists of color were, on average, hired at a younger age than White workers. 
df %>% 
  filter(year_type=="hired") %>% 
  filter(data_year>=2016) %>% 
  mutate(hire_age=data_year-year(date_of_birth)) %>% 
  filter(dept=="News") %>% 
  group_by(race_ethnicity) %>% 
  summarize(count=n(),
            `average age`=round(mean(hire_age)),
            `median age`=round(median(hire_age))
  ) %>% 
  filter(count>=5) %>% 
  mutate(race_ethnicity=gsub(" \\(United States of America\\)", "", race_ethnicity)) %>% 
  filter(!is.na(race_ethnicity))%>% 
  kable()

#On the Commercial side, Black employees tend to be hired at an older age than employees of other races and ethnicities, with average and median hiring ages of 39 and 42, respectively. Asian employees, meanwhile, have the youngest average and median hiring ages at 27 and 26, respectively.
df %>% 
  filter(year_type=="hired") %>% 
  filter(data_year>=2016) %>% 
  mutate(hire_age=data_year-year(date_of_birth)) %>% 
  filter(dept=="Commercial") %>% 
  group_by(race_ethnicity) %>% 
  summarize(count=n(),
            `average age`=round(mean(hire_age)),
            `median age`=round(median(hire_age))
  ) %>% 
  filter(count>=5) %>% 
  mutate(race_ethnicity=gsub(" \\(United States of America\\)", "", race_ethnicity)) %>% 
  filter(!is.na(race_ethnicity))%>% 
  kable()

#Turnover by desk chart
turnoverbydesk <- df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type_current=="Salaried") %>% 
  filter(data_year>=2016) %>% 
  count(dept, desk, year_type) %>% 
  filter(year_type!="active") %>% 
  pivot_wider(names_from="year_type", values_from="n") %>% 
  filter(hired>=5 & terminated>=5) %>% 
  mutate(category=case_when(
    hired>terminated ~ "More hired than left",
    terminated<hired ~ "More left than hired",
    terminated==hired ~ "Same",
    TRUE ~ ""
  )) 
kable(turnoverbydesk)

#Within the Newsroom, White employees were often hired at a much higher rate than employees of color. In some sections, fewer than five employees of color were hired and therefore couldn’t even be counted within the data in order to protect workers’ privacy. This was the case for Local, Financial and Multiplatform, where 14, 22 and 13 White employees, respectively, were hired from 2016 to 2021.
#and chart
newsroom_desks <- df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type_current=="Salaried") %>% 
  filter(data_year>=2016) %>% 
  count(race_grouping, desk, year_type) %>% 
  filter(year_type!="active") %>% 
  pivot_wider(names_from="year_type", values_from="n") %>% 
  filter(hired>=5 & terminated>=5) %>% 
  mutate(category=case_when(
    hired>terminated ~ "More hired than left",
    terminated<hired ~ "More left than hired",
    terminated==hired ~ "Same",
    TRUE ~ ""
  )) 
kable(newsroom_desks)

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

df2 <- vroom("raw_data/terminated_wd_2021.csv")


colnames(df2) <- c("department","employee_id","gender",   
                   "race_ethnicity","education", "military_status",
                   "date_of_birth", "original_hire_date","hire_date", "termination_date",
                   "pay_rate_type", "current_base_pay", 
                   "job_profile_current","time_type_current", "cost_center_current",  
                   "effective_date1", "business_process_type1", "business_process_reason1", "pay_rate_type1", "base_pay_change1", "job_profile1", "time_type1", "cost_center1", 
                   "effective_date2", "business_process_type2", "business_process_reason2", "pay_rate_type2", "base_pay_change2", "job_profile2", "time_type2", "cost_center2", 
                   "effective_date3", "business_process_type3", "business_process_reason3", "pay_rate_type3", "base_pay_change3", "job_profile3", "time_type3", "cost_center3",
                   "effective_date4", "business_process_type4", "business_process_reason4", "pay_rate_type_4", "base_pay_change4", "job_profile4", "time_type4", "cost_center4", 
                   "effective_date5", "business_process_type5", "business_process_reason5", "pay_rate_type_5", "base_pay_change5", "job_profile5", "time_type5", "cost_center5", 
                   "effective_date6", "business_process_type6", "business_process_reason6", "pay_rate_type_6", "base_pay_change6", "job_profile6", "time_type6", "cost_center6",
                   "effective_date7", "business_process_type7", "business_process_reason7", "pay_rate_type_7", "base_pay_change7", "job_profile7", "time_type7", "cost_center7", 
                   "effective_date8", "business_process_type8", "business_process_reason8", "pay_rate_type_8", "base_pay_change8", "job_profile8", "time_type8", "cost_center8", 
                   "effective_date9", "business_process_type9", "business_process_reason9", "pay_rate_type_9", "base_pay_change9", "job_profile9", "time_type9", "cost_center_9",
                   "effective_date10", "business_process_type10", "business_process_reason10", "pay_rate_type10", "base_pay_change10", "job_profile10", "time_type10", "cost_center10",
                   "effective_date11", "business_process_type11", "business_process_reason11", "pay_rate_type11", "base_pay_change11", "job_profile11", "time_type11", "cost_center11",
                   "effective_date12", "business_process_type12", "business_process_reason12", "pay_rate_type12", "base_pay_change12", "job_profile12", "time_type12", "cost_center12", 
                   "effective_date13", "business_process_type13", "business_process_reason13", "pay_rate_type13", "base_pay_change13", "job_profile13", "time_type13", "cost_center13", 
                   "effective_date14", "business_process_type14", "business_process_reason14", "pay_rate_type14", "base_pay_change14", "job_profile14", "time_type14", "cost_center14",
                   "effective_date15", "business_process_type15", "business_process_reason15", "pay_rate_type15", "base_pay_change15", "job_profile15", "time_type15", "cost_center15", 
                   "2008_annual_performance_rating", "2009_annual_performance_rating", "2010_annual_performance_rating",
                   "2011_annual_performance_rating", "2012_annual_performance_rating", "2013_annual_performance_rating",
                   "2014_annual_performance_rating", "2015_annual_performance_rating", "2016_annual_performance_rating",
                   "2017_annual_performance_rating", "2018_annual_performance_rating", "2019_annual_performance_rating",
                   "2020_annual_performance_rating")

data_date <- ymd("2021-05-28")

df <- df %>% mutate(date_of_birth = mdy(date_of_birth),
                    age = floor(decimal_date(data_date) - decimal_date(date_of_birth)))

df <- df %>% 
  mutate(date_of_birth = case_when(
    age < 0 ~ date_of_birth-years(100),
    TRUE ~ date_of_birth),
    age = floor(decimal_date(data_date) - decimal_date(date_of_birth)))


df2 <- df2 %>% mutate(date_of_birth = mdy(date_of_birth),
                      age = floor(decimal_date(data_date) - decimal_date(date_of_birth)))

df2 <- df2 %>% 
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


df2 <- df2 %>% mutate(hire_date = mdy(hire_date),
                      termination_date=mdy(termination_date),
                      years_of_service = floor(decimal_date(data_date) - decimal_date(hire_date)))

df2 <- df2 %>% 
  mutate(original_hire_date=mdy(original_hire_date),
         termination_date=ymd(termination_date)#,
         #effective_date=mdy(effective_date)
  )


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


df2 <- df2 %>% 
  mutate(age_group_5=case_when(
    age < 25 ~ "<25",
    age >= 25 & age < 30 ~ "25-29",
    age >= 30 & age < 35 ~ "30-34",
    age >= 35 & age < 40 ~ "35-39",
    age >= 40 & age < 45 ~ "34-44",
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


df2 <- df2 %>% 
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


df2 <- df2 %>% 
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

df2 <- df2 %>%
  mutate(dept=case_when(
    department == "News" ~ "News", 
    department == "Editorial" ~ "News",
    department == "News Service and Syndicate" ~ "News", 
    department == "Audience Development and Insights" ~ "Commercial",
    department == "Client Solutions" ~ "Commercial",
    department == "Customer Care and Logistics" ~ "Commercial",
    department == "Finance" ~ "Commercial",
    department == "Legal" ~ "Commercial",
    department == "Marketing" ~ "Commercial",
    department == "WP News Media Services" ~ "Commercial",
    department == "Production" ~ "Commercial",
    department == "Public Relations" ~ "Commercial",
    department == "Washington Post Live" ~ "Commercial",
    department == "Product" ~ "Commercial",
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

df2 <- df2 %>%
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

df2 <- df2 %>%
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

df2 <- df2 %>%
  mutate(race_grouping=case_when(
    race_ethnicity == "White (United States of America)" ~ "white",
    race_ethnicity == "Black or African American (United States of America)" ~ "person of color",
    race_ethnicity == "Asian (United States of America)" ~ "person of color",
    race_ethnicity == "Hispanic or Latino (United States of America)" ~ "person of color",
    race_ethnicity == "Two or More Races (United States of America)" ~ "person of color",
    race_ethnicity == "American Indian or Alaska Native (United States of America)" ~ "person of color",
    race_ethnicity == "Native Hawaiian or Other Pacific Islander (United States of America)" ~ "person of color",
    TRUE ~ "unknown"))


pay_change_function <- function(x=df, cycles=26, label="active") {
  for (i in 1:cycles) {
    if (i <24) {
      df_year <- df %>% 
        select(employee_id, paste0("business_process_reason", as.character(i)),
               paste0("base_pay_change", as.character(i)),
               paste0("effective_date", as.character(i)),
               paste0("pay_rate_type", as.character(i)),
               paste0("time_type", as.character(i)),
               gender, race_ethnicity, race_grouping,
               age_group_5, dept, tier,
               `2008_annual_performance_rating`,
               `2009_annual_performance_rating`,
               `2010_annual_performance_rating`,
               `2011_annual_performance_rating`,
               `2012_annual_performance_rating`,
               `2013_annual_performance_rating`,
               `2014_annual_performance_rating`,
               `2015_annual_performance_rating`,
               `2016_annual_performance_rating`,
               `2017_annual_performance_rating`,
               `2018_annual_performance_rating`,
               `2019_annual_performance_rating`,
               `2020_annual_performance_rating`
        ) 
    } else {
      df_year <- df %>% 
        select(employee_id, paste0("business_process_reason", as.character(i)),
               #paste0("base_pay_change", as.character(i)),
               paste0("effective_date", as.character(i)),
               #paste0("pay_rate_type", as.character(i)),
               gender, race_ethnicity, race_grouping,
               age_group_5, dept, tier,
               `2008_annual_performance_rating`,
               `2009_annual_performance_rating`,
               `2010_annual_performance_rating`,
               `2011_annual_performance_rating`,
               `2012_annual_performance_rating`,
               `2013_annual_performance_rating`,
               `2014_annual_performance_rating`,
               `2015_annual_performance_rating`,
               `2016_annual_performance_rating`,
               `2017_annual_performance_rating`,
               `2018_annual_performance_rating`,
               `2019_annual_performance_rating`,
               `2020_annual_performance_rating`
        ) 
      
      df_year$base_pay_change <- NA
      df_year$pay_rate_type <- NA
      df_year$time_type <- NA
      
    }
    
    colnames(df_year) <- gsub(paste0("n", as.character(i)), "n", colnames(df_year))
    colnames(df_year) <- gsub(paste0("e", as.character(i)), "e", colnames(df_year))
    #colnames(df_year) <- gsub(paste0("r", as.character(i)), "r", colnames(df_year))
    
    df_year <- df_year %>% 
      select(employee_id, business_process_reason,base_pay_change,effective_date,
             pay_rate_type,gender, race_ethnicity, race_grouping,
             age_group_5, dept, tier,
             `2008_annual_performance_rating`,
             `2009_annual_performance_rating`,
             `2010_annual_performance_rating`,
             `2011_annual_performance_rating`,
             `2012_annual_performance_rating`,
             `2013_annual_performance_rating`,
             `2014_annual_performance_rating`,
             `2015_annual_performance_rating`,
             `2016_annual_performance_rating`,
             `2017_annual_performance_rating`,
             `2018_annual_performance_rating`,
             `2019_annual_performance_rating`,
             `2020_annual_performance_rating`) %>% 
      mutate(workday = paste0(label, "-", i))
    
    if (i==1) {
      mega_df <- df_year
    } else {
      mega_df <- rbind(mega_df, df_year)
    }
  }
  return(mega_df)
}

reason_for_change1 <- pay_change_function(df, cycles=25, label="active")
reason_for_change2 <- pay_change_function(df2, cycles=15, label="terminated")
reason_for_change_combined <- rbind(reason_for_change1, reason_for_change2)

emp_perf_function <- function(x=df_name, year_start=2008, year_end=2020, label="active") {
  for (i in year_start:year_end) {
    df_name <- x
    df_year <- df_name %>% 
      select(employee_id, paste0(as.character(i), "_annual_performance_rating"),
             gender, race_ethnicity, race_grouping, dept)
    
    colnames(df_year) <- c("employee_id", "performance_rating","gender",
                           "race_ethnicity","race_grouping","dept")
    
    df_year$workday <- paste0(label, "-", i)
    
    if (i==year_start) {
      mega_df <- df_year
    } else {
      mega_df <- rbind(mega_df, df_year)  
      
    }
  }
  return(mega_df)
}  

ratings1 <- emp_perf_function(df, year_start=2008, year_end=2020, label="active")
ratings2 <- emp_perf_function(df2, year_start=2008, year_end=2020, label="terminated")

ratings_combined <- rbind(ratings1, ratings2) %>% 
  filter(!is.na(performance_rating))

news_salaried <- filter(df, dept == "News", pay_rate_type == "Salaried")
news_hourly <- filter(df, dept == "News", pay_rate_type == "Hourly")
commercial_salaried <- filter(df, dept == "Commercial", pay_rate_type == "Salaried")
commercial_hourly <- filter(df, dept == "Commercial", pay_rate_type == "Hourly")

news_salaried2 <-  filter(df2, dept == "News", pay_rate_type == "Salaried")
news_hourly2 <-  filter(df2, dept == "News", pay_rate_type == "Hourly")
commercial_salaried2 <-  filter(df2, dept == "Commercial", pay_rate_type == "Salaried")
commercial_hourly2 <-  filter(df2, dept == "Commercial", pay_rate_type == "Hourly")

df %>% 
  filter(pay_rate_type=="Salaried") %>% 
  group_by(race_grouping) %>% 
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>% 
  mutate(percent=round(n/sum(n)*100,0))%>% 
  kable()

# Salaried White male Newsroom employees outnumbered salaried White female Newsroom employees (229 to 207), but salaried women of color outnumbered salaried men of color in the Newsroom (109 to 72). 
df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>% 
  count(gender, race_grouping) %>% 
  filter(n>=5)%>% 
  kable()


#The median salary for men was $120,977, compared with $102,700 for women, a gap of 18 percent. 
df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>% 
  group_by(gender) %>% 
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>% 
  mutate(percent=round(n/sum(n)*100,0))  %>% 
  filter(n>=5)%>% 
  kable()

#The median pay for women was $36 per hour, compared with $34 per hour for men. 
df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Hourly") %>% 
  group_by(gender) %>% 
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>% 
  mutate(percent=round(n/sum(n)*100,0))  %>% 
  filter(n>=5)%>% 
  kable()

#The disparity in median pay between men and women was starkest among employees under the age of 40, among whom there was a gap of 12.5 percent, amounting to nearly $13,000.
df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "Over 40",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(gender, y40) %>%
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>%
  mutate(percent=round(n/sum(n)*100,0))  %>%
  filter(n>=5)%>% 
  kable()
#103400-90400
#(103400-90400)/90400

#White journalists made nearly 16 percent more than journalists of color. 
#White journalists made a median salary of $113,810, while journalists of color had a median salary of $98,435. 
df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>% 
  group_by(race_grouping) %>% 
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>% 
  mutate(percent=round(n/sum(n)*100,0))  %>% 
  filter(n>=5)%>% 
  kable()

#This gap was particularly stark when comparing the median salaries for White men and women of color. White men had a median salary of $123,797, compared with a median salary of $94,840 for women of color. That gap was nearly $29,000 — or over 28 percent.
df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>% 
  group_by(gender, race_grouping) %>% 
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>% 
  mutate(percent=round(n/sum(n)*100,0))  %>% 
  filter(n>=5)%>% 
  kable()

#For the 320 salaried male Newsroom employees working at The Post, the median salary was $120,977. 
#For the 336 salaried female Newsroom employees, it was $102,700.
#The median age for salaried men working in the Newsroom was 42, compared with 36 for salaried women.

df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>% 
  group_by(gender) %>% 
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>% 
  mutate(percent=round(n/sum(n)*100,0))  %>% 
  filter(n>=5)%>% 
  kable()

#For the 83 hourly employees across the Newsroom, there was also a slight difference. 
#The median hourly wage for men was $34, compared with $36 for women. 
#Comparing by age is difficult because only 33 men worked hourly jobs in the Newsroom and their ages varied widely.
#That said, female hourly workers (50) made more than male hourly workers. 
#Under the age of 40, the gap was nearly $3, and over the age of 40, the gap was $10. 
#However, the sample size for hourly workers was low, meaning a few employees could lower or raise the median fairly drastically.

df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Hourly") %>% 
  group_by(gender) %>% 
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>% 
  mutate(percent=round(n/sum(n)*100,0))  %>% 
  filter(n>=5)%>% 
  kable()

#Over the age of 40: There were nearly 1.4 men for every woman in the Newsroom. 
#Their median salary was 2.4 percent more, a gap of over $3,000. 
#Under the age of 40: There were nearly 1.5 women for every man in the Newsroom. 
#Their median salary was 12.5 percent less, a gap of nearly $13,000.

df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "Over 40",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(gender, y40) %>%
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>%
  mutate(percent=round(n/sum(n)*100,0))  %>%
  filter(n>=5)%>% 
  kable()

#Among those under 40, Newsroom employees of color made about 4 percent less than their White counterparts, with median salaries of $92,000 and $96,000, respectively.
#The race disparity widened for journalists 40 and over. 
#Newsroom employees of color had a median salary of $121,875, while their White colleagues had a median salary of $130,800 — a gap of more than 7 percent.

df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "Over 40",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, y40) %>%
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>%
  mutate(percent=round(n/sum(n)*100,0))  %>%
  filter(n>=5)%>% 
  kable()


#Below are the median salaries by race and ethnicity across the Newsroom:
#  White: $113,810
#Black: $102,700
#Asian: $103,970
#Hispanic or Latino: $95,780
#Two or more races: $91,090

df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "Over 40",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_ethnicity) %>%
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>%
  mutate(percent=round(n/sum(n)*100,0))  %>%
  filter(n>=5)%>% 
  kable()

#The gap between White journalists in the Newsroom and journalists of color was nearly 16 percent, with a median salary of $113,810 for 437 White journalists and $98,435 for 181 journalists of color. 
df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "Over 40",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping) %>%
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>%
  mutate(percent=round(n/sum(n)*100,0))  %>%
  filter(n>=5)%>% 
  kable()


#About 58 of the hourly employees in the Newsroom were White — and they made a median wage of $35 an hour compared with $34 for hourly employees of color. 
df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Hourly") %>%
  mutate(y40 =case_when(age >=40 ~ "Over 40",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping) %>%
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>%
  mutate(percent=round(n/sum(n)*100,0))  %>%
  filter(n>=5)%>% 
  kable()

#The median salaries by group were as follows: 

#White men: $123,797
#Men of color: $104,420
#White women: $105,780
#Women of color: $94,840

#Median ages by group were as follows:

#White men: 43
#Men of color: 40
#White women: 37
#Women of color: 33

df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "Over 40",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(race_grouping, gender) %>%
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>%
  mutate(percent=round(n/sum(n)*100,0))  %>%
  filter(n>=5)%>% 
  kable()

# For most sections the median salary for men surpasses women CHART
#Of the 15 sections that had at least five men and five women:
#In 10 sections, men had higher median pay than women (beyond 5 percent): Audience, Design, Foreign, Investigative, Local, Multiplatform, National, Opinion, Photography and Style.
#One section, Sports, had a median pay disparity that favors women by 18 percent.
#In four sections, there was approximate pay equity between genders (within 5 percent): Emerging News Products, Financial, Graphics and Video.

desk_analysis <- df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "Over 40",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(desk, gender) %>%
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>%
  mutate(percent=round(n/sum(n)*100,0))  %>%
  filter(n>=5)

kable(desk_analysis)

# Workers of color have lower median salary in nearly all departments CHART
#Of 13 sections for which there were at least five White journalists and five journalists of color:
#Six sections had a median pay disparity favoring White journalists of more than 7 percent: the Newsroom, Emerging News Products, Video, Style, Photography and National.
#Seven sections had approximate pay equity (within 5 percent) between journalists of color and White journalists: Audience, Design, Financial, Graphics, Local, Multiplatform and Sports.

desk_analysis <- df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "Over 40",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(desk, race_grouping) %>%
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>%
  mutate(percent=round(n/sum(n)*100,0))  %>%
  filter(n>=5)

kable(desk_analysis)

# Race and gender pay disparities evident across sections CHART
#There were only four sections that had at least five White men, five White women, five men of color and five women of color: Design, Local, National and Sports.
#All four sections had racial and gender pay disparities. 
#For employees across these four sections, the disparity in median salaries between White men and women of color was 8 percent. 
#The starkest gaps in median salaries were between White men and men of color in National ($51,000) and between White women and women of color in Sports ($65,000).


desk_analysis <- df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>%
  mutate(y40 =case_when(age >=40 ~ "Over 40",
                        age < 40 ~ "Under 40",
                        TRUE ~ "Unknown")) %>% 
  group_by(desk, race_grouping, gender) %>%
  summarize(n=n(), median=median(current_base_pay, na.rm=T),
            age=median(age, na.rm=T)) %>%
  mutate(percent=round(n/sum(n)*100,0))  %>%
  filter(n>=5)

kable(desk_analysis)


#Across sections in which employees’ median salary was higher than $125,000 (including National, Investigative, Foreign, Financial and Opinions): 
#67 percent of employees were White and 55 percent of employees were male. 


df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>% 
  mutate(t_desk=case_when(
    desk=="National" | 
      desk=="Investigative" |
      desk=="Foreign" |
      desk=="Financial" |
      desk=="Editorial" ~ "Higher",
    TRUE ~ "Lower")) %>% 
  group_by(t_desk, race_grouping) %>% 
  summarise(
    count = length(current_base_pay),
    median = median(current_base_pay, na.rm = FALSE)
  ) %>% 
  mutate(percent=count/sum(count)*100)%>% 
  kable()


df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>% 
  mutate(t_desk=case_when(
    desk=="National" | 
      desk=="Investigative" |
      desk=="Foreign" |
      desk=="Financial" |
      desk=="Editorial" ~ "Higher",
    TRUE ~ "Lower")) %>% 
  group_by(t_desk, gender) %>% 
  summarise(
    count = length(current_base_pay),
    median = median(current_base_pay, na.rm = FALSE)
  ) %>% 
  mutate(percent=count/sum(count)*100)%>% 
  kable()

#40 percent of employees were White men, 27 percent were White women, 12 percent were women of color, and 8 percent were men of color (the rest did not indicate race). 

df %>% 
  filter(dept=="News") %>% 
  filter(pay_rate_type=="Salaried") %>% 
  mutate(t_desk=case_when(
    desk=="National" | 
      desk=="Investigative" |
      desk=="Foreign" |
      desk=="Financial" |
      desk=="Editorial" ~ "Higher",
    TRUE ~ "Lower")) %>% 
  group_by(t_desk, race_grouping, gender) %>% 
  summarise(
    count = length(current_base_pay),
    median = median(current_base_pay, na.rm = FALSE)
  ) %>% 
  mutate(percent=count/sum(count)*100)%>% 
  kable()