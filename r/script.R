library(tidyr)
library(dplyr)
data_maternal <- read.csv("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/original/maternal_mortality.csv", header = TRUE)

data_infant <- read.csv("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/original/infant_mortality.csv", header = TRUE)

data_neonatal <- read.csv("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/original/neonatal_mortality.csv", header = TRUE)

data_under <- read.csv("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/original/under5_mortality.csv", header = TRUE)
head(data)


data_maternal_long <- data_maternal %>%
  pivot_longer(
    cols = "X2000":"X2019",
    names_to = "year",
    values_to = "maternal_mortality"
  ) %>%
  mutate(
    year = as.numeric(sub("X", "", year)) 
  )%>%
  select(iso, year, maternal_mortality)


data_infant_long <- data_infant %>%
  pivot_longer(
    cols = "X2000":"X2019",
    names_to = "year",
    values_to = "infant_mortality"
  ) %>%
  mutate(
    year = as.numeric(sub("X", "", year)) 
  )%>%
  select(iso, year, infant_mortality)

data_under_long <- data_under %>%
  pivot_longer(
    cols = "X2000":"X2019",
    names_to = "year",
    values_to = "under5_mortality"
  ) %>%
  mutate(
    year = as.numeric(sub("X", "", year)) 
  )%>%
  select(iso, year, under5_mortality)

data_neonatal_long <- data_neonatal %>%
  pivot_longer(
    cols = "X2000":"X2019",
    names_to = "year",
    values_to = "neonatal_mortality"
  ) %>%
  mutate(
    year = as.numeric(sub("X", "", year)) 
  )%>%
  select(iso, year, neonatal_mortality)


data_disaster <- read.csv("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/original/disaster.csv", header = TRUE)

df_disaster_filtered <- data_disaster %>%
  filter(
    Year >= 2000 & Year <= 2019,
    Disaster.Type %in% c("Earthquake", "Drought")
    
  ) %>%
  select(Year, ISO, Disaster.Type)
df_disaster_dummy <- df_disaster_filtered %>%
  mutate(
    drought = if_else(`Disaster.Type` == "Drought", 1, 0),
    earthquake = if_else(`Disaster.Type` == "Earthquake", 1, 0)
  )

df_disaster_dummy

df_country_year <- df_disaster_dummy %>%
  group_by(ISO, Year) %>%  
  summarize(
    drought = max(drought),    
    earthquake = max(earthquake), 
    
  ) %>%
  ungroup()

countries <- read.table("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/original/countries.txt")[[2]]
df_country_filtered <- df_country_year %>%
  dplyr::filter(ISO %in% countries)
df_country_filtered <- df_country_filtered %>%
  rename(iso = ISO) %>%
  rename(year = Year)
df_country_filtered

data_conflict <- read.csv("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/original/conflictdata.csv")
data_conflict

conflict_year <- data_conflict %>%
  group_by(iso, year) %>%
  summarize(
    armed_conflict = if_else(sum(best, na.rm = TRUE) > 0, 1, 0),  
  ) %>%
  ungroup()
conflict_year<- conflict_year %>%
  arrange(iso, year) %>%
  group_by(iso) %>%
  mutate(armed_conflict_lag1 = lag(armed_conflict, n = 1, default = 0)) %>%
  ungroup()
conflict_year <- conflict_year %>%
  select(-armed_conflict)
conflict_year <- conflict_year %>%
  rename(armed_conflict = armed_conflict_lag1)
conflict_year
head(data_conflict, 10)

#data_long
#df_country_filtered
#conflict_year

covariates <- read.csv("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/original/covariates.csv")
dir.create("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/analytical", recursive = TRUE, showWarnings = FALSE)
library(dplyr)
library(purrr)
list_of_dfs <- list(data_maternal_long, data_infant_long, data_neonatal_long, data_under_long, df_country_filtered, conflict_year, covariates)
combined_all_dfs <- reduce(
  list_of_dfs,
  left_join,
  by = c("iso", "year")
)


combined_all_dfs
write.csv(combined_all_dfs, "/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/analytical/finaldataset.csv")

summary(combined_all_dfs)  

finaldata <- read.csv("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/analytical/finaldataset.csv")


finaldata <- finaldata %>% select(-1)


