library(tidyr)
library(dplyr)
library(mice)
library(textreg)
library(plm)
library(lmtest)
library(sandwich)
library(stringr)

finaldata <- read.csv("/Users/achlys/Desktop/week3-inclass-Sheen-T-main/data/analytical/finaldataset.csv")
finaldata <- finaldata %>% select(-1)


midata <- finaldata |>
  mutate(loggdp = log(gdp_1000),
         iso_num = as.numeric(as.factor(finaldata$iso))) |>
  select(-iso)

mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality"), "iso_num"] <- -2

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)

plot(mice.multi.out)

fit.mi.matmor <- with(mice.multi.out, 
                      lm(maternal_mortality ~ -1 + armed_conflict + loggdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))
out.mi.matmor <- pool(fit.mi.matmor)
summary(out.mi.matmor)

finaldata <- finaldata |>
  mutate(
    loggdp       = ifelse(gdp_1000 > 0, log(gdp_1000), NA_real_),
  )
preds <- as.formula(" ~ armed_conflict + loggdp + oecd + pop_dens + urban +
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
  "loggdp"             = "log(GDP pc)",
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

is_fixed_eff <- function(term) {
  grepl("^as\\.factor\\(year\\)", term) |
    grepl("^as\\.factor\\(iso|^iso(_num)?\\b", term)
}

order_by_map <- function(df, map) {
  df %>%
    filter(term %in% names(map)) %>%
    mutate(pretty = unname(map[term]),
           ord = match(term, names(map))) %>%
    arrange(ord)
}

to_texreg <- function(df, model_name = "Model") {
  texreg::createTexreg(
    coef.names = df$pretty,
    coef = df$estimate,
    ci.low = df$ci.low,
    ci.up  = df$ci.high,
    gof.names = character(0), gof = numeric(0), gof.decimal = logical(0)
  )
}

cc_ci <- function(model, vcov_mat, coef_map) {
  ct <- lmtest::coeftest(model, vcov. = vcov_mat)
  df <- data.frame(
    term = rownames(ct),
    estimate = ct[, 1],
    se = ct[, 2],
    stringsAsFactors = FALSE
  )
  z <- qnorm(0.975)
  df <- df %>%
    filter(!is_fixed_eff(term), term != "(Intercept)") %>%
    mutate(ci.low = estimate - z * se,
           ci.high = estimate + z * se) %>%
    order_by_map(coef_map)
  df
}

mi_ci <- function(pool_obj, coef_map, level = 0.95) {
  s <- summary(pool_obj, conf.int = TRUE, conf.level = level)
  df <- s %>%
    transmute(term = .data$term,
              estimate = .data$estimate,
              ci.low = .data[[grep("^\\s*2\\.?5", names(s), value = TRUE)[1]]],
              ci.high = .data[[grep("^\\s*97\\.?5", names(s), value = TRUE)[1]]]) %>%
    filter(!is_fixed_eff(term), term != "(Intercept)") %>%
    order_by_map(coef_map)
  df
}

coef_map <- c(
  "armed_conflict" = "Armed conflict (t−1)",
  "lgdp"           = "log(GDP pc)",
  "oecd"           = "OECD",
  "pop_dens"       = "Pop. density",
  "urban"          = "Urban",
  "age_dep"        = "Age dependency",
  "male_edu"       = "Male education",
  "temp"           = "Temperature",
  "rainfall_1000"  = "Rainfall (×1000)",
  "earthquake"     = "Earthquake",
  "drought"        = "Drought"
)


V_cc_matmor <- sandwich::vcovHC(maternal_mortalitymod, type = "HC1", cluster = "group")
cc_df_matmor <- cc_ci(maternal_mortalitymod, V_cc_matmor, coef_map)

mi_df_matmor <- mi_ci(out.mi.matmor, coef_map)

cc_tr  <- to_texreg(cc_df_matmor)
mi_tr  <- to_texreg(mi_df_matmor)

texreg::htmlreg(list(cc_tr, mi_tr),
        custom.model.names = c("CC (complete-case)", "MI (pooled)"),
        ci.force = TRUE, ci.test = 0, stars = 0, digits = 3,
        file = "cc_vs_mi.html")
