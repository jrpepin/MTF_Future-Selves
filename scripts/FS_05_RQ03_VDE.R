#-------------------------------------------------------------------------------
# FS Project
# FS_05_RQ03.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Do early expectations or trajectories predict transitions? 
# Do they have independent effects?

## Create a workbook (openxlsx::)
wb.RQ03       <- createWorkbook()

## Table formatting styles
header_style <- createStyle(
  textDecoration = "bold",
  fontSize = 11,
  halign = "left",
  border = "Bottom",
  borderStyle = "thin")

################################################################################
# Add intercepts & slopes from FS_04_RQ02.R
################################################################################

# This part of the file must be run AFTER FS_04_RQ02.R equations

## Extract random effects (intercept + slope) for each model -------------------
extract_params <- function(model) {
  re <- ranef(model)$MTFID
  tibble(MTFID = rownames(re),
         intercept = re[, "(Intercept)"] + fixef(model)["(Intercept)"],
         slope = re[, "age_c"] + fixef(model)["age_c"]
         )
}

mar_params <- extract_params(mods03[["gdsp"]])
par_params <- extract_params(mods03[["gdpa"]])
wrk_params <- extract_params(mods03[["gdwk"]])

mar_params <- mar_params |> rename(int_mar = intercept, slope_mar = slope)
par_params <- par_params |> rename(int_par = intercept, slope_par = slope)
wrk_params <- wrk_params |> rename(int_wrk = intercept, slope_wrk = slope)

# merge person-level parameters into one dataset
params <- mar_params |>
  left_join(par_params, by = "MTFID") |>
  left_join(wrk_params, by = "MTFID")

## add marital and parenthood transitions
df_person <- df |>
  distinct(MTFID, .keep_all = TRUE) |>
  filter(wave == 0) |>
  left_join(params) |>
  mutate(
    slope_mar_z = as.numeric(scale(slope_mar)), #standardize for interpretation ease
    slope_par_z = as.numeric(scale(slope_par))
  )

## Set survey design -----------------------------------------------------------

des_30 <- df_person |>
  filter(!is.na(mar_at_30) & !is.na(numkids_30)) |>
  as_survey_design(weights = weight)

des_35 <- df_person |>
  filter(!is.na(weight_35)) |>
  filter(!is.na(mar_at_35) & !is.na(numkids_35)) |>
  as_survey_design(weights = weight_35)

des_40 <- df_person |>
  filter(!is.na(weight_40)) |>
  filter(!is.na(mar_at_40) & !is.na(numkids_40)) |>
  as_survey_design(weights = weight_40)

des_45 <- df_person |>
  filter(!is.na(weight_45)) |>
  filter(!is.na(mar_at_45) & !is.na(numkids_45)) |>
  as_survey_design(weights = weight_45)

################################################################################
# Regressions predicting transitions using intercept & slope 
################################################################################

## Ever married ----------------------------------------------------------------

dvs <- c("ever_mar_30", "ever_mar_35", "ever_mar_40", "ever_mar_45")

equation <- "%s ~ int_mar + slope_mar_z + 
            age_c + sex + decades + mom_ba + raceeth + momwork + momdad"

designs <- list(
  ever_mar_30 = des_30,
  ever_mar_35 = des_35,
  ever_mar_40 = des_40,
  ever_mar_45 = des_45)

mods_ever_mar <- lapply(dvs, function(x) {
  m1 <- as.formula(sprintf(equation,x))
  
  svyglm(
    m1, 
    design = designs[[x]],
    family = quasibinomial())
})

names(mods_ever_mar) <- dvs

tab05a <- modelsummary(
  mods_ever_mar,
  exponentiate = TRUE,
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"),
  fmt = 3,
  output = "data.frame") 

tab05a

## Currently married -----------------------------------------------------------

dvs <- c("mar_at_30", "mar_at_35", "mar_at_40", "mar_at_45")

equation <- "%s ~ int_mar + slope_mar_z + 
            age_c + sex + decades + mom_ba + raceeth + momwork + momdad"

designs <- list(
  mar_at_30 = des_30,
  mar_at_35 = des_35,
  mar_at_40 = des_40,
  mar_at_45 = des_45)

mods_now_mar <- lapply(dvs, function(x) {
  m1 <- as.formula(sprintf(equation,x))
  
  svyglm(
    m1, 
    design = designs[[x]],
    family = quasibinomial())
})

names(mods_now_mar) <- dvs

tab05b <- modelsummary(
  mods_now_mar,
  exponentiate = TRUE,
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"),
  fmt = 3,
  output = "data.frame") 

tab05b


## Parent ----------------------------------------------------------------------

dvs <- c("parent_30", "parent_35", "parent_40", "parent_45")

equation <- "%s ~ int_par + slope_par_z + 
            age_c + sex + decades + mom_ba + raceeth + momwork + momdad"

designs <- list(
  parent_30 = des_30,
  parent_35 = des_35,
  parent_40 = des_40,
  parent_45 = des_45)

mods_parent <- lapply(dvs, function(x) {
  m1 <- as.formula(sprintf(equation,x))
  
  svyglm(
    m1, 
    design = designs[[x]],
    family = quasibinomial())
})

names(mods_parent) <- dvs

tab06a <- modelsummary(
  mods_parent,
  exponentiate = TRUE,
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"),
  fmt = 3,
  output = "data.frame") 

tab06a


## Number of Kids (max) --------------------------------------------------------

dvs <- c("numkids_30", "numkids_35", "numkids_40", "numkids_45")

equation <- "%s ~ int_par + slope_par_z + 
            age_c + sex + decades + mom_ba + raceeth + momwork + momdad"

designs <- list(
  numkids_30 = des_30,
  numkids_35 = des_35,
  numkids_40 = des_40,
  numkids_45 = des_45)

mods_numkids <- lapply(dvs, function(x) {
  m1 <- as.formula(sprintf(equation,x))
  
  svyglm(
    m1, 
    design = designs[[x]],
    family = quasipoisson()) ## numeric, could use gaussian() instead 
})

names(mods_numkids) <- dvs

tab06b <- modelsummary(
  mods_numkids,
  exponentiate = TRUE, # incidence rate ratios (IRRs)
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"),
  fmt = 3,
  output = "data.frame") 

tab06b

################################################################################
# Add tables to worksheet
################################################################################

## marriage variables ----------------------------------------------------------
tab_mar <- left_join(
  tab05a, tab05b,
  by = c("part", "term", "statistic"))

addWorksheet(wb.RQ03, "mods05_mar")

title_text <- c("Logistic Regression Models Predicting Marriage Transitions 
                using Age-based expectations from LMER estimates")

mergeCells(
  wb.RQ03, sheet = "mods05_mar", cols = 1:ncol(tab_mar), rows = 1)

writeData(
  wb.RQ03, sheet = "mods05_mar", x = title_text, startRow = 1, startCol = 1)

## add table
writeData(
  wb.RQ03, sheet = "mods05_mar", x = tab_mar, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ03, sheet = "mods05_mar", style = header_style, 
  rows = 1:2, cols = 1:ncol(tab_mar), gridExpand = TRUE)

setColWidths(
  wb.RQ03, sheet = "mods05_mar", 
  cols = 2:ncol(tab_mar), widths = "auto")


## parent variables ------------------------------------------------------------
tab_par <- left_join(
  tab06a, tab06b,
  by = c("part", "term", "statistic"))

addWorksheet(wb.RQ03, "mods06_par")

title_text <- c("Regression Models Predicting Parent Transitions & Number of Children 
                using Age-based expectations from LMER estimates")

mergeCells(
  wb.RQ03, sheet = "mods06_par", cols = 1:ncol(tab_par), rows = 1)

writeData(
  wb.RQ03, sheet = "mods06_par", x = title_text, startRow = 1, startCol = 1)

## add table
writeData(
  wb.RQ03, sheet = "mods06_par", x = tab_par, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ03, sheet = "mods06_par", style = header_style, 
  rows = 1:2, cols = 1:ncol(tab_par), gridExpand = TRUE)

setColWidths(
  wb.RQ03, sheet = "mods06_par", 
  cols = 2:ncol(tab_par), widths = "auto")

################################################################################
# Predicted values
################################################################################

## ever married (intercept) 
pp_ever_mar_30 <- avg_predictions(
  mods_ever_mar[["ever_mar_30"]],
  variables = list(int_mar = 1:5))

pp_ever_mar_35 <- avg_predictions(
  mods_ever_mar[["ever_mar_35"]],
  variables = list(int_mar = 1:5))

pp_ever_mar_40 <- avg_predictions(
  mods_ever_mar[["ever_mar_40"]],
  variables = list(int_mar = 1:5))

pp_ever_mar_45 <- avg_predictions(
  mods_ever_mar[["ever_mar_45"]],
  variables = list(int_mar = 1:5))

## combine dfs 
pp_ever_mar_30$outcome <- "ever_mar_30"
pp_ever_mar_35$outcome <- "ever_mar_35"
pp_ever_mar_40$outcome <- "ever_mar_40"
pp_ever_mar_45$outcome <- "ever_mar_45"

pp_ever_int <- rbind(pp_ever_mar_30, pp_ever_mar_35, pp_ever_mar_40, pp_ever_mar_45)

## currently married (intercept)
pp_now_mar_30 <- avg_predictions(
  mods_now_mar[["mar_at_30"]],
  variables = list(int_mar = 1:5))

pp_now_mar_35 <- avg_predictions(
  mods_now_mar[["mar_at_35"]],
  variables = list(int_mar = 1:5))

pp_now_mar_40 <- avg_predictions(
  mods_now_mar[["mar_at_40"]],
  variables = list(int_mar = 1:5))

pp_now_mar_45 <- avg_predictions(
  mods_now_mar[["mar_at_45"]],
  variables = list(int_mar = 1:5))

## combine dfs 
pp_now_mar_30$outcome <- "now_mar_30"
pp_now_mar_35$outcome <- "now_mar_35"
pp_now_mar_40$outcome <- "now_mar_40"
pp_now_mar_45$outcome <- "now_mar_45"

pp_now_int <- rbind(pp_now_mar_30, pp_now_mar_35, pp_now_mar_40, pp_now_mar_45)


## parent (intercept) 
pp_parent_30 <- avg_predictions(
  mods_parent[["parent_30"]],
  variables = list(int_par = 1:5))

pp_parent_35 <- avg_predictions(
  mods_parent[["parent_35"]],
  variables = list(int_par = 1:5))

pp_parent_40 <- avg_predictions(
  mods_parent[["parent_40"]],
  variables = list(int_par = 1:5))

pp_parent_45 <- avg_predictions(
  mods_parent[["parent_45"]],
  variables = list(int_par = 1:5))

## combine dfs 
pp_parent_30$outcome <- "parent_30"
pp_parent_35$outcome <- "parent_35"
pp_parent_40$outcome <- "parent_40"
pp_parent_45$outcome <- "parent_45"

pp_parent_int <- rbind(pp_parent_30, pp_parent_35, pp_parent_40, pp_parent_45)


## number of kids (intercept) 
pp_numkids_30 <- avg_predictions(
  mods_numkids[["numkids_30"]],
  variables = list(int_par = 1:5))

pp_numkids_35 <- avg_predictions(
  mods_numkids[["numkids_35"]],
  variables = list(int_par = 1:5))

pp_numkids_40 <- avg_predictions(
  mods_numkids[["numkids_40"]],
  variables = list(int_par = 1:5))

pp_numkids_45 <- avg_predictions(
  mods_numkids[["numkids_45"]],
  variables = list(int_par = 1:5))

## combine dfs 
pp_numkids_30$outcome <- "numkids_30"
pp_numkids_35$outcome <- "numkids_35"
pp_numkids_40$outcome <- "numkids_40"
pp_numkids_45$outcome <- "numkids_45"

pp_numkids_int <- rbind(pp_numkids_30, pp_numkids_35, pp_numkids_40, pp_numkids_45)

## ever married (slopes) 
pp_ever_slope_30 <- avg_predictions(
  mods_ever_mar[["ever_mar_30"]],
  variables = list(slope_mar_z = c(-1, 0, 1)))

pp_ever_slope_35 <- avg_predictions(
  mods_ever_mar[["ever_mar_35"]],
  variables = list(slope_mar_z = c(-1, 0, 1)))

pp_ever_slope_40 <- avg_predictions(
  mods_ever_mar[["ever_mar_40"]],
  variables = list(slope_mar_z = c(-1, 0, 1)))

pp_ever_slope_45 <- avg_predictions(
  mods_ever_mar[["ever_mar_45"]],
  variables = list(slope_mar_z = c(-1, 0, 1)))

## combine dfs 
pp_ever_slope_30$outcome <- "ever_mar_30"
pp_ever_slope_35$outcome <- "ever_mar_35"
pp_ever_slope_40$outcome <- "ever_mar_40"
pp_ever_slope_45$outcome <- "ever_mar_45"

pp_ever_slope <- rbind(pp_ever_slope_30, pp_ever_slope_35, 
                       pp_ever_slope_40, pp_ever_slope_45)

## currently married (slopes) 
pp_now_slope_30 <- avg_predictions(
  mods_now_mar[["mar_at_30"]],
  variables = list(slope_mar_z = c(-1, 0, 1)))

pp_now_slope_35 <- avg_predictions(
  mods_now_mar[["mar_at_35"]],
  variables = list(slope_mar_z = c(-1, 0, 1)))

pp_now_slope_40 <- avg_predictions(
  mods_now_mar[["mar_at_40"]],
  variables = list(slope_mar_z = c(-1, 0, 1)))

pp_now_slope_45 <- avg_predictions(
  mods_now_mar[["mar_at_45"]],
  variables = list(slope_mar_z = c(-1, 0, 1)))

## combine dfs 
pp_now_slope_30$outcome <- "mar_at_30"
pp_now_slope_35$outcome <- "mar_at_35"
pp_now_slope_40$outcome <- "mar_at_40"
pp_now_slope_45$outcome <- "mar_at_45"

pp_now_slope <- rbind(pp_now_slope_30, pp_now_slope_35, 
                      pp_now_slope_40, pp_now_slope_45)

## parent (slopes) 
pp_parent_slope_30 <- avg_predictions(
  mods_parent[["parent_30"]],
  variables = list(slope_par_z = c(-1, 0, 1)))

pp_parent_slope_35 <- avg_predictions(
  mods_parent[["parent_35"]],
  variables = list(slope_par_z = c(-1, 0, 1)))

pp_parent_slope_40 <- avg_predictions(
  mods_parent[["parent_40"]],
  variables = list(slope_par_z = c(-1, 0, 1)))

pp_parent_slope_45 <- avg_predictions(
  mods_parent[["parent_45"]],
  variables = list(slope_par_z = c(-1, 0, 1)))

## combine dfs 
pp_parent_slope_30$outcome <- "parent_30"
pp_parent_slope_35$outcome <- "parent_35"
pp_parent_slope_40$outcome <- "parent_40"
pp_parent_slope_45$outcome <- "parent_45"

pp_parent_slope <- rbind(pp_parent_slope_30, pp_parent_slope_35, 
                         pp_parent_slope_40, pp_parent_slope_45)

## numkids (slopes) 
pp_numkids_slope_30 <- avg_predictions(
  mods_numkids[["numkids_30"]],
  variables = list(slope_par_z = c(-1, 0, 1)))

pp_numkids_slope_35 <- avg_predictions(
  mods_numkids[["numkids_35"]],
  variables = list(slope_par_z = c(-1, 0, 1)))

pp_numkids_slope_40 <- avg_predictions(
  mods_numkids[["numkids_40"]],
  variables = list(slope_par_z = c(-1, 0, 1)))

pp_numkids_slope_45 <- avg_predictions(
  mods_numkids[["numkids_45"]],
  variables = list(slope_par_z = c(-1, 0, 1)))

## combine dfs 
pp_numkids_slope_30$outcome <- "numkids_30"
pp_numkids_slope_35$outcome <- "numkids_35"
pp_numkids_slope_40$outcome <- "numkids_40"
pp_numkids_slope_45$outcome <- "numkids_45"

pp_numkids_slope <- rbind(pp_numkids_slope_30, pp_numkids_slope_35, 
                          pp_numkids_slope_40, pp_numkids_slope_45)

# add to worksheet -------------------------------------------------------------

## clean datasets
pp_ever_int <- pp_ever_int |>
  mutate(parameter = "intercept",
         across(where(is.numeric), ~ round(.x, 3))) |>
  rename(level = int_mar) |>
  extract(outcome,
          into = c("outcome", "wave"),
          regex = "(.+)_(30|35|40|45)$") |>
  select(-c(df, s.value))

pp_now_int <- pp_now_int |>
  mutate(parameter = "intercept",
         across(where(is.numeric), ~ round(.x, 3))) |>
  rename(level = int_mar) |>
  extract(outcome,
          into = c("outcome", "wave"),
          regex = "(.+)_(30|35|40|45)$") |>
  select(-c(df, s.value))

pp_parent_int <- pp_parent_int |>
  mutate(parameter = "intercept",
         across(where(is.numeric), ~ round(.x, 3))) |>
  rename(level = int_par) |>
  extract(outcome,
          into = c("outcome", "wave"),
          regex = "(.+)_(30|35|40|45)$") |>
  select(-c(df, s.value))

pp_numkids_int <- pp_numkids_int |>
  mutate(parameter = "intercept",
         across(where(is.numeric), ~ round(.x, 3))) |>
  rename(level = int_par) |>
  extract(outcome,
          into = c("outcome", "wave"),
          regex = "(.+)_(30|35|40|45)$") |>
  select(-c(df, s.value))

pp_ever_slope <- pp_ever_slope |>
  mutate(parameter = "slope",
         across(where(is.numeric), ~ round(.x, 3))) |>
  rename(level = slope_mar_z) |>
  extract(outcome,
          into = c("outcome", "wave"),
          regex = "(.+)_(30|35|40|45)$") |>
  select(-c(df, s.value))

pp_now_slope <- pp_now_slope |>
  mutate(parameter = "slope",
         across(where(is.numeric), ~ round(.x, 3))) |>
  rename(level = slope_mar_z) |>
  extract(outcome,
          into = c("outcome", "wave"),
          regex = "(.+)_(30|35|40|45)$") |>
  select(-c(df, s.value))

pp_parent_slope <- pp_parent_slope |>
  mutate(parameter = "slope",
         across(where(is.numeric), ~ round(.x, 3))) |>
  rename(level = slope_par_z) |>
  extract(outcome,
          into = c("outcome", "wave"),
          regex = "(.+)_(30|35|40|45)$") |>
  select(-c(df, s.value))

pp_numkids_slope <- pp_numkids_slope |>
  mutate(parameter = "slope",
         across(where(is.numeric), ~ round(.x, 3))) |>
  rename(level = slope_par_z) |>
  extract(outcome,
          into = c("outcome", "wave"),
          regex = "(.+)_(30|35|40|45)$") |>
  select(-c(df, s.value))


## combine dfs 
df_pp <- rbind(
  pp_ever_int, pp_now_int, pp_parent_int, pp_numkids_int,
  pp_ever_slope, pp_now_slope, pp_parent_slope, pp_numkids_slope) |>
  ungroup() |>
  as.data.frame() 

save(df_pp, file = paste0(outDir, "/fs_mods05_06_pp.rda")) # save R data to load next time


## add table to worksheet ------------------------------------------------------

addWorksheet(wb.RQ03, "predictions")

title_text <- c("Model 05 & 06 predicted values by modal ages")

mergeCells(
  wb.RQ03, sheet = "predictions", cols = 1:ncol(df_pp), rows = 1)

writeData(
  wb.RQ03, sheet = "predictions", x = title_text, startRow = 1, startCol = 1)

### add table
writeData(
  wb.RQ03, sheet = "predictions", x = df_pp, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ03, sheet = "predictions", style = header_style, 
  rows = 1:2, cols = 1:ncol(df_pp), gridExpand = TRUE)

setColWidths(
  wb.RQ03, sheet = "predictions", 
  cols = 2:ncol(df_pp), widths = "auto")

## visualize it ----------------------------------------------------------------

### ever married (intercept)
pp_ever_int |>
  ggplot(
   aes(x = wave, y = estimate, 
       fill = factor(level), 
       group = level)) +
  geom_col(
    position = position_dodge(width = .8)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high), 
    position = position_dodge(width = .8), 
    width = .2) + 
  theme_minimal()

### currently married (intercept)
pp_now_int |>
  ggplot(
    aes(x = wave, y = estimate, 
        fill = factor(level), 
        group = level)) +
  geom_col(
    position = position_dodge(width = .8)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high), 
    position = position_dodge(width = .8), 
    width = .2) + 
  theme_minimal()

### parent (intercept)
pp_parent_int |>
  ggplot(
    aes(x = wave, y = estimate, 
        fill = factor(level), 
        group = level)) +
  geom_col(
    position = position_dodge(width = .8)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high), 
    position = position_dodge(width = .8), 
    width = .2) + 
  theme_minimal()

### numkids (intercept)
pp_numkids_int |>
  ggplot(
    aes(x = wave, y = estimate, 
        fill = factor(level), 
        group = level)) +
  geom_col(
    position = position_dodge(width = .8)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high), 
    position = position_dodge(width = .8), 
    width = .2) + 
  theme_minimal() +
  labs(
    x = NULL,
    y = "Predicted Number of Children",
    fill = "Expected Parenting Quality")

### ever married (slope)
pp_ever_slope |>
  mutate(
    slope_grp = fct_case_when(
      level == -1 ~ "Low (-1 SD)", 
      level == 0  ~  "Average", 
      level == 1  ~ "High (+1 SD)")) |>
  ggplot(
    aes(x = wave, y = estimate,
        color = slope_grp, fill = slope_grp)) +
  geom_point(position = position_dodge(width = .5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high), 
    position = position_dodge(width = .5), width = .2) + 
  theme_minimal()

### currently married (slope)
pp_now_slope |>
  mutate(
    slope_grp = fct_case_when(
      level == -1 ~ "Low (-1 SD)", 
      level == 0  ~  "Average", 
      level == 1  ~ "High (+1 SD)")) |>
  ggplot(
    aes(x = wave, y = estimate,
        color = slope_grp, fill = slope_grp)) +
  geom_point(position = position_dodge(width = .5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high), 
    position = position_dodge(width = .5), width = .2) + 
  theme_minimal()

### parent (slope)
pp_parent_slope |>
  mutate(
    slope_grp = fct_case_when(
      level == -1 ~ "Low (-1 SD)", 
      level == 0  ~  "Average", 
      level == 1  ~ "High (+1 SD)")) |>
  ggplot(
    aes(x = wave, y = estimate,
        color = slope_grp, fill = slope_grp)) +
  geom_point(position = position_dodge(width = .5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high), 
    position = position_dodge(width = .5), width = .2) + 
  theme_minimal()

### numkids (slope)
pp_numkids_slope |>
  mutate(
    slope_grp = fct_case_when(
      level == -1 ~ "Low (-1 SD)", 
      level == 0  ~  "Average", 
      level == 1  ~ "High (+1 SD)")) |>
  ggplot(
    aes(x = wave, y = estimate,
        color = slope_grp, fill = slope_grp)) +
  geom_point(position = position_dodge(width = .5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high), 
    position = position_dodge(width = .5), width = .2) + 
  theme_minimal()

################################################################################
# Regressions predicting transitions using intercept & slope & SEX
################################################################################

## Ever married ----------------------------------------------------------------

dvs <- c("ever_mar_30", "ever_mar_35", "ever_mar_40", "ever_mar_45")

equation <- "%s ~ int_mar * sex + slope_mar_z + 
            age_c + decades + mom_ba + raceeth + momwork + momdad"

designs <- list(
  ever_mar_30 = des_30,
  ever_mar_35 = des_35,
  ever_mar_40 = des_40,
  ever_mar_45 = des_45)

mods_ever_mar <- lapply(dvs, function(x) {
  m1 <- as.formula(sprintf(equation,x))
  
  svyglm(
    m1, 
    design = designs[[x]],
    family = quasibinomial())
})

names(mods_ever_mar) <- dvs

tab07a <- modelsummary(
  mods_ever_mar,
  exponentiate = TRUE,
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"),
  fmt = 3,
  output = "data.frame") 

tab07a

## Currently married -----------------------------------------------------------

dvs <- c("mar_at_30", "mar_at_35", "mar_at_40", "mar_at_45")

equation <- "%s ~ int_mar * sex + slope_mar_z + 
            age_c + decades + mom_ba + raceeth + momwork + momdad"

designs <- list(
  mar_at_30 = des_30,
  mar_at_35 = des_35,
  mar_at_40 = des_40,
  mar_at_45 = des_45)

mods_now_mar <- lapply(dvs, function(x) {
  m1 <- as.formula(sprintf(equation,x))
  
  svyglm(
    m1, 
    design = designs[[x]],
    family = quasibinomial())
})

names(mods_now_mar) <- dvs

tab07b <- modelsummary(
  mods_now_mar,
  exponentiate = TRUE,
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"),
  fmt = 3,
  output = "data.frame") 

tab07b


## Parent ----------------------------------------------------------------------

dvs <- c("parent_30", "parent_35", "parent_40", "parent_45")

equation <- "%s ~ int_par * sex + slope_par_z + 
            age_c + decades + mom_ba + raceeth + momwork + momdad"

designs <- list(
  parent_30 = des_30,
  parent_35 = des_35,
  parent_40 = des_40,
  parent_45 = des_45)

mods_parent <- lapply(dvs, function(x) {
  m1 <- as.formula(sprintf(equation,x))
  
  svyglm(
    m1, 
    design = designs[[x]],
    family = quasibinomial())
})

names(mods_parent) <- dvs

tab08a <- modelsummary(
  mods_parent,
  exponentiate = TRUE,
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"),
  fmt = 3,
  output = "data.frame") 

tab08a


## Number of Kids (max) --------------------------------------------------------

dvs <- c("numkids_30", "numkids_35", "numkids_40", "numkids_45")

equation <- "%s ~ int_par * sex + slope_par_z + 
            age_c + decades + mom_ba + raceeth + momwork + momdad"

designs <- list(
  numkids_30 = des_30,
  numkids_35 = des_35,
  numkids_40 = des_40,
  numkids_45 = des_45)

mods_numkids <- lapply(dvs, function(x) {
  m1 <- as.formula(sprintf(equation,x))
  
  svyglm(
    m1, 
    design = designs[[x]],
    family = quasipoisson()) ## numeric, could use gaussian() instead 
})

names(mods_numkids) <- dvs

tab08b <- modelsummary(
  mods_numkids,
  exponentiate = TRUE, # incidence rate ratios (IRRs)
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"),
  fmt = 3,
  output = "data.frame") 

tab08b

################################################################################
# Add tables to worksheet
################################################################################

## marriage variables ----------------------------------------------------------
tab_mar_sex <- left_join(
  tab07a, tab07b,
  by = c("part", "term", "statistic"))

addWorksheet(wb.RQ03, "mods07_mar_sex")

title_text <- c("Logistic Regression Models Predicting Marriage Transitions 
                using Age-based expectations from LMER estimates")

mergeCells(
  wb.RQ03, sheet = "mods07_mar_sex", cols = 1:ncol(tab_mar_sex), rows = 1)

writeData(
  wb.RQ03, sheet = "mods07_mar_sex", x = title_text, startRow = 1, startCol = 1)

## add table
writeData(
  wb.RQ03, sheet = "mods07_mar_sex", x = tab_mar_sex, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ03, sheet = "mods07_mar_sex", style = header_style, 
  rows = 1:2, cols = 1:ncol(tab_mar_sex), gridExpand = TRUE)

setColWidths(
  wb.RQ03, sheet = "mods07_mar_sex", 
  cols = 2:ncol(tab_mar_sex), widths = "auto")


## parent variables ------------------------------------------------------------
tab_par_sex <- left_join(
  tab08a, tab08b,
  by = c("part", "term", "statistic"))

addWorksheet(wb.RQ03, "mods08_par_sex")

title_text <- c("Regression Models Predicting Parent Transition & Number of Children 
                using Age-based expectations from LMER estimates")

mergeCells(
  wb.RQ03, sheet = "mods08_par_sex", cols = 1:ncol(tab_par_sex), rows = 1)

writeData(
  wb.RQ03, sheet = "mods08_par_sex", x = title_text, startRow = 1, startCol = 1)

## add table
writeData(
  wb.RQ03, sheet = "mods08_par_sex", x = tab_par_sex, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ03, sheet = "mods08_par_sex", style = header_style, 
  rows = 1:2, cols = 1:ncol(tab_par_sex), gridExpand = TRUE)

setColWidths(
  wb.RQ03, sheet = "mods08_par_sex", 
  cols = 2:ncol(tab_par_sex), widths = "auto")



################################################################################
# Save the workbook 
################################################################################

saveWorkbook(
  wb.RQ03,
  file.path(outDir, "FS_RQ03.xlsx"),
  overwrite = TRUE)
