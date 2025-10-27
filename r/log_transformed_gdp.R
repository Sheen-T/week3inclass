library(tidyr)
library(dplyr)
library(tidyverse)
library(janitor)
library(plm)
library(texreg)
library(sandwich)
library(lmtest)

finaldata <- read.csv("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/analytical/finaldataset.csv")

finaldata <- finaldata %>% select(-1)


finaldata <- finaldata |>
  mutate(
    lgdp       = ifelse(gdp_1000 > 0, log(gdp_1000), NA_real_),
  )

preds <- as.formula(" ~ armed_conflict + lgdp + oecd + pop_dens + urban +
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +  
                  iso + as.factor(year)")

maternal_mortalitymod <- plm(update.formula(preds, maternal_mortality ~ .), data = finaldata)
un5mormod <- plm(update.formula(preds, under5_mortality ~ .), data = finaldata)
infmormod <- plm(update.formula(preds, infant_mortality ~ .), data = finaldata)
neomormod <- plm(update.formula(preds, neonatal_mortality ~ .), data = finaldata)

mods <- list(maternal_mortalitymod, un5mormod, infmormod, neomormod)

model_names <- c("Maternal mortality", "Under-5 mortality", "Infant mortality", "Neonatal mortality")

coef_map <- c(
  "(Intercept)"      = "Intercept",
  "armed_conflict"   = "Armed conflict (t−1)",
  "lgdp"             = "log(GDP pc)",
  "oecd"             = "OECD",
  "urban"            = "Urban",
  "male_edu"         = "Male education",
  "drought"          = "Drought",
  "pop_dens"         = "Pop_dens",
  "earthquake"       = "Earthquake",
  "rainfall_1000"    = "Rainfall_1000",
  "temp"             = "Temp",
  "age_dep"          = "Age_dep"
)

vcovs <- lapply(list(maternal_mortalitymod, un5mormod, infmormod, neomormod),
                function(m) sandwich::vcovHC(m, type = "HC1", cluster = "group"))
ses   <- lapply(vcovs, function(V) sqrt(diag(V)))
pvals <- Map(function(m, V) lmtest::coeftest(m, vcov. = V)[, 4],
             list(maternal_mortalitymod, un5mormod, infmormod, neomormod), vcovs)


texreg::htmlreg(
  list(maternal_mortalitymod, un5mormod, infmormod, neomormod),
  file                = "/Users/achlys/Desktop/new_table/outputs/mortality_models_table.html",
  doctype             = TRUE,
  override.se         = ses,
  override.pvalues    = pvals,
  custom.model.names  = model_names,
  coef_map            = coef_map,
  digits = 3,
  stars  = c(0.001, 0.01, 0.05),
  caption = "Panel regressions of mortality outcomes",
  caption.above = TRUE,
  center = TRUE
)