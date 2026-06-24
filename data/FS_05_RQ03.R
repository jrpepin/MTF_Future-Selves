#-------------------------------------------------------------------------------
# FS Project
# FS_05_RQ03.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Do early expectations or trajectories predict transitions? 
# Do they have independent effects?

## Create a workbook (openxlsx::)
wb.logit <- createWorkbook()

## Table formatting styles
header_style <- createStyle(
  textDecoration = "bold",
  fontSize = 11,
  halign = "left",
  border = "Bottom",
  borderStyle = "thin")

################################################################################
# Logistic regression predicting transitions using intercept & slope 
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
transitions <- df |>
  select(MTFID, wave, mar_max, par_max, sex, decades, mom_ba, raceeth, 
         momwork, momdad, weight) |>
  distinct(MTFID, .keep_all = TRUE)

df_trans <- left_join(params, transitions, by = "MTFID") |>
  filter(wave == 0) |>
  as_survey_design(weights = weight)

## Run separate logistic regressions -------------------------------------------

### marriage

#### empty model
mod_married <- svyglm(
  mar_max ~ int_mar + slope_mar,
  design = df_trans,
  family = quasibinomial())

n <- nrow(mod_married$survey.design$variables) # unweighted sample size

tidy_mar01 <- tidy(mod_married, exponentiate = TRUE) |>
  mutate(n = n)

#### with demographic controls
mod_married_C <- svyglm(
  mar_max ~ int_mar + slope_mar + sex + decades + mom_ba + raceeth + momwork + momdad,
  design = df_trans,
  family = quasibinomial())

n <- nrow(mod_married_C$survey.design$variables) # unweighted sample size

tidy_mar02 <- tidy(mod_married_C, exponentiate = TRUE) |>
  mutate(n = n)

# int_mar = "How much does your starting point on marriage attitudes predict whether you marry?"
# slope_mar = "How much does your change over time in marriage attitudes predict whether you marry?"
# (Intercept) = baseline odds when both predictors are zero (not substantively important)

### parenthood

#### empty model
mod_parent <- svyglm(
  par_max ~ int_par + slope_par,
  design = df_trans,
  family = quasibinomial())

n <- nrow(mod_parent$survey.design$variables) # unweighted sample size

tidy_par01 <- tidy(mod_parent, exponentiate = TRUE) |>
  mutate(n = n)

#### with demographic controls
mod_parent_C <- svyglm(
  mar_max ~ int_par + slope_par + sex + decades + mom_ba + raceeth + momwork + momdad,
  design = df_trans,
  family = quasibinomial())

n <- nrow(mod_parent_C$survey.design$variables) # unweighted sample size

tidy_par02 <- tidy(mod_parent_C, exponentiate = TRUE) |>
  mutate(n = n)

tab05 <- bind_rows(
  mar01 = tidy_mar01,
  mar02 = tidy_mar02,
  par01 = tidy_par01,
  par02 = tidy_par02,
  .id = "source"
)

tab05

## add table to worksheet ------------------------------------------------------

addWorksheet(wb.logit, "mods05")

title_text <- c("Logistic Regression Models Predicting Transitions 
                using Age-based expectations from LMER estimates")

mergeCells(
  wb.logit, sheet = "mods05", cols = 1:ncol(tab05), rows = 1)

writeData(
  wb.logit, sheet = "mods05", x = title_text, startRow = 1, startCol = 1)

## add table
writeData(
  wb.logit, sheet = "mods05", x = tab05, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.logit, sheet = "mods05", style = header_style, 
  rows = 1:2, cols = 1:ncol(tab05), gridExpand = TRUE)

setColWidths(
  wb.logit, sheet = "mods05", 
  cols = 2:ncol(tab05), widths = "auto")

## Predicted values ------------------------------------------------------------

or_mar <- tidy(mod_married_C) |>
  mutate(
    OR = exp(estimate),
    OR_low = exp(estimate - 1.96 * std.error),
    OR_high = exp(estimate + 1.96 * std.error),
    domain = "Marriage"
  ) |>
  filter(term != "(Intercept)")

or_par <- tidy(mod_parent_C) |>
  mutate(
    OR = exp(estimate),
    OR_low = exp(estimate - 1.96 * std.error),
    OR_high = exp(estimate + 1.96 * std.error),
    domain = "Parenthood"
  ) |>
  filter(term != "(Intercept)")

or_all <- bind_rows(or_mar, or_par)

## add table to worksheet ------------------------------------------------------

addWorksheet(wb.logit, "mods05_OR")

title_text <- c("Model 05 Odd Ratios")

mergeCells(
  wb.logit, sheet = "mods05_OR", cols = 1:ncol(or_all), rows = 1)

writeData(
  wb.logit, sheet = "mods05_OR", x = title_text, startRow = 1, startCol = 1)

### add table
writeData(
  wb.logit, sheet = "mods05_OR", x = or_all, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.logit, sheet = "mods05_OR", style = header_style, 
  rows = 1:2, cols = 1:ncol(or_all), gridExpand = TRUE)

setColWidths(
  wb.logit, sheet = "mods05_OR", 
  cols = 2:ncol(or_all), widths = "auto")


## Visualize it! ---------------------------------------------------------------

or_all |>
  ggplot(aes(x = term, y = OR, color = domain)) +
  geom_point(size =3, 
             position = position_dodge(width = .5)) +
  geom_errorbar(aes(ymin = OR_low, ymax = OR_high), 
                width = .1,
                position = position_dodge(width = .5)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  labs(
    x = "",
    y = "Odds Ratio",
    title = "Effect of Marriage Expectations Intercept and Slope Predicting Transitions") +
  theme_minimal()

################################################################################
# logistic with BY categories 
################################################################################

# Use factor variables
df <- df |>
  mutate(
    gdsp = fct_case_when(
      gdsp == 1 ~ "Poor",
      gdsp == 2 ~ "Not so good",
      gdsp == 3 ~ "Fairly good",
      gdsp == 4 ~ "Good",
      gdsp == 5 ~ "Very good"),
    gdpa = fct_case_when(
      gdpa == 1 ~ "Poor",
      gdpa == 2 ~ "Not so good",
      gdpa == 3 ~ "Fairly good",
      gdpa == 4 ~ "Good",
      gdpa == 5 ~ "Very good")
    )

# Run separate logistic regressions --------------------------------------------
df$gdsp <- relevel(df$gdsp, ref = "Very good")
df$gdpa <- relevel(df$gdpa, ref = "Very good")

df_svy <- df |>
  filter(wave == 0) |>
  as_survey_design(weights = weight)

equation_06 <- list(
  mar_max ~ gdsp + sex + gdsp:sex + decades + mom_ba + raceeth + momwork + momdad,
  par_max ~ gdpa + sex + gdpa:sex + decades + mom_ba + raceeth + momwork + momdad)


mods06 <- lapply(equation_06, function(gd) {
  svyglm(
    formula = gd,
    design = df_svy,
    family = quasibinomial())
  }) |>
  setNames(c(
    "married", "parent"))

mods06_Ns <- sapply(mods06, function(m) nrow(model.frame(m))) # unweighted sample size


tab06 <- map(mods06, ~tidy(.x, exponentiate = TRUE)) |>
  bind_rows(.id = "model") |>
  mutate(
    p.value = sprintf("%.3f", p.value),
    N = mods06_Ns[model]
  )

tab06

## add table to worksheet ------------------------------------------------------

addWorksheet(wb.logit, "mods06")

title_text <- c("Logistic Regression Models Predicting Transitions 
                into Marriage and Parenthood using Categorical Base-Year Expectations")

mergeCells(
  wb.logit, sheet = "mods06", cols = 1:ncol(tab06), rows = 1)

writeData(
  wb.logit, sheet = "mods06", x = title_text, startRow = 1, startCol = 1)

## add table
writeData(
  wb.logit, sheet = "mods06", x = tab06, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.logit, sheet = "mods06", style = header_style, 
  rows = 1:2, cols = 1:ncol(tab06), gridExpand = TRUE)

setColWidths(
  wb.logit, sheet = "mods06", 
  cols = 2:ncol(tab06), widths = "auto")


## Predicted values ------------------------------------------------------------

pp_sp <- ggpredict(mods06[["married"]], terms = c("gdsp", "sex"))
pp_pa <- ggpredict(mods06[["parent"]], terms = c("gdpa", "sex"))

## these are the same as above
pp_sp <- predict_response(mods06[["married"]], terms = c("gdsp", "sex"))
pp_pa <- predict_response(mods06[["parent"]], terms = c("gdpa", "sex"))

## combine dfs
pp_sp$cat <- "Spouse"
pp_pa$cat <- "Parent"

df_pp <- rbind(pp_sp, pp_pa) |>
  as_tibble()

df_pp$x <- factor(df_pp$x, levels = c("Poor", "Not so good", "Fairly good", "Good", "Very good"))

vg_ci <- df_pp |>
  filter(x == "Very good") |>
  select(group, cat, vg_lower = conf.low, vg_upper = conf.high)

df_pp2 <- df_pp |>
  left_join(vg_ci, by = c("group", "cat")) |>
  mutate(
    non_overlap = conf.high < vg_lower | conf.low > vg_upper,
    highlight = x == "Very good" | non_overlap
  )

## add table to worksheet ------------------------------------------------------

addWorksheet(wb.logit, "mods06_predict")

title_text <- c("Model 06 predicted values by gender")

mergeCells(
  wb.logit, sheet = "mods06_predict", cols = 1:ncol(df_pp2), rows = 1)

writeData(
  wb.logit, sheet = "mods06_predict", x = title_text, startRow = 1, startCol = 1)

### add table
writeData(
  wb.logit, sheet = "mods06_predict", x = df_pp2, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.logit, sheet = "mods06_predict", style = header_style, 
  rows = 1:2, cols = 1:ncol(df_pp2), gridExpand = TRUE)

setColWidths(
  wb.logit, sheet = "mods06_predict", 
  cols = 2:ncol(df_pp2), widths = "auto")

## Visualize it! ---------------------------------------------------------------

df_pp2 |> 
  ggplot(aes(x=cat, y = predicted, fill = x, alpha = highlight,
             ymin = conf.low, ymax = conf.high)) + 
  geom_col(
    position = position_dodge(width = .8), width = .7) +
  geom_errorbar(
    position = position_dodge(width = .8), width = .2) +
  facet_wrap(~group) +
  theme_minimal()  +
  guides(alpha = "none")

################################################################################
# Save the workbook 
################################################################################

saveWorkbook(
  wb.logit,
  file.path(outDir, "FS_logit.xlsx"),
  overwrite = TRUE)
