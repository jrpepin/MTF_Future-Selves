#-------------------------------------------------------------------------------
# FS Project
# FS_04_RQ02_VDE.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Do expectations change over the life course (18-30)?

## lme4::lmer

## names of DVs for loops
vars <- c("gdsp", "gdpa", "gdwk")
df[vars] <- lapply(df[vars], as.numeric)

## Create a workbook (openxlsx::)
wb.RQ02 <- createWorkbook()

## Table formatting styles
header_style <- createStyle(
  textDecoration = "bold",
  fontSize = 11,
  halign = "left",
  border = "Bottom",
  borderStyle = "thin")

################################################################################
# unconditional means model (a.k.a random effects model) 
################################################################################

equation_00 <- "%s ~ (1 | MTFID)"

mods00 <- lapply(vars, function(gd) {
  m1 <- as.formula(sprintf(equation_00, gd))
  lmer(m1, data = subset(df, wave <=6), weights = weight)
  }) |>
  set_names(vars)

tab00 <- modelsummary(
  mods00,
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"
  ),
  fmt = 3,
  output = "data.frame") 

tab00

# Fixed effects intercept = 4.37 == over all time points and individuals, the average expectation is 4.37 (Good) on scale of 1-5.
# Random effects MTFID (intercept) == 0.2331 The between variation for the intercept is 0.2331
# Random effects Residual == 0.4794 The within variation for the intercept is 0.4794

# ICC = .34 About 34% of the variation in spousal expectations is caused by differences 
# between people while the remaining 66% is within people. 
# This means that the differences between people is less important than within one.

## add table to worksheet ------------------------------------------------------

addWorksheet(wb.RQ02, "mods00")

title_text <- c("Unconditional means model")

mergeCells(
  wb.RQ02, sheet = "mods00", cols = 1:ncol(tab00), rows = 1)

writeData(
  wb.RQ02, sheet = "mods00", x = title_text, startRow = 1, startCol = 1)

## add table
writeData(
  wb.RQ02, sheet = "mods00", x = tab00, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ02, sheet = "mods00", style = header_style, 
  rows = 1:2, cols = 1:ncol(tab00), gridExpand = TRUE)

setColWidths(
  wb.RQ02, sheet = "mods00", 
  cols = 2:ncol(tab00), widths = "auto")

################################################################################
# unconditional change model 
################################################################################

equation_01 <- "%s ~ age_c + (1 | MTFID)"

mods01 <- lapply(vars, function(gd) {
  m1 <- as.formula(sprintf(equation_01, gd))
  lmer(m1, data = subset(df, wave <=6), weights = weight)
}) |>
  set_names(vars)

tab01 <- modelsummary(
  mods01,
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"
  ),
  fmt = 3,
  output = "data.frame") 

tab01

summary(mods01[["gdsp"]])
# now we interpret the 4.37 (Fixed effects intercept) as the expected spouse rating at age 18.
# The effect of age, -0.001 tells us the average rate of change with the increase in age. So, the individual rating decreases with age.
# This model assumes there is no between variation in the rate of change (change is the same for everyone...probably not)

## add table to worksheet ------------------------------------------------------

addWorksheet(wb.RQ02, "mods01")

title_text <- c("Unconditional change model")

mergeCells(
  wb.RQ02, sheet = "mods01", cols = 1:ncol(tab01), rows = 1)

writeData(
  wb.RQ02, sheet = "mods01", x = title_text, startRow = 1, startCol = 1)

## add table
writeData(
  wb.RQ02, sheet = "mods01", x = tab01, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ02, sheet = "mods01", style = header_style, 
  rows = 1:2, cols = 1:ncol(tab01), gridExpand = TRUE)

setColWidths(
  wb.RQ02, sheet = "mods01", 
  cols = 2:ncol(tab01), widths = "auto")

################################################################################
# unconditional change model (a.k.a MLMC) with RE for change 
################################################################################

equation_02 <- "%s ~ age_c + (1 + age_c | MTFID)"

mods02 <- lapply(vars, function(gd) {
  m1 <- as.formula(sprintf(equation_02, gd))
  lmer(m1, data = subset(df, wave <=6), weights = weight)
  }) |>
  set_names(vars)

tab02 <- modelsummary(
  mods02,
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"),
  fmt = 3,
  output = "data.frame") 

tab02

summary(mods02[["gdsp"]])

# random part of model: intercept .33 represents the between variation at age 18, 
# the coefficient for age 0.003 represents between variation in the rates of change

## add table to worksheet ------------------------------------------------------

addWorksheet(wb.RQ02, "mods02")

title_text <- c("Unconditional change model with RE for change")

mergeCells(
  wb.RQ02, sheet = "mods02", cols = 1:ncol(tab02), rows = 1)

writeData(
  wb.RQ02, sheet = "mods02", x = title_text, startRow = 1, startCol = 1)

## add table
writeData(
  wb.RQ02, sheet = "mods02", x = tab02, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ02, sheet = "mods02", style = header_style, 
  rows = 1:2, cols = 1:ncol(tab02), gridExpand = TRUE)

setColWidths(
  wb.RQ02, sheet = "mods02", 
  cols = 2:ncol(tab02), widths = "auto")


################################################################################
# including time constant predictors 
################################################################################

equation_03 <- "%s ~ age_c + sex + decades + mom_ba + raceeth + momwork + momdad + (1 + age_c | MTFID)"

mods03 <- lapply(vars, function(gd) {
  m1 <- as.formula(sprintf(equation_03, gd))
  lmer(m1, data = subset(df, wave <=6), weights = weight)
}) |>
  set_names(vars)

tab03 <- modelsummary(
  mods03,
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"),
  fmt = 3,
  output = "data.frame") 

tab03

summary(mods03[["gdsp"]])
## fixed effect sexWomen = 0.09 women have slightly higher ratings than men 

## add table to worksheet ------------------------------------------------------

addWorksheet(wb.RQ02, "mods03")

title_text <- c("Change model with time constant predictors")

mergeCells(
  wb.RQ02, sheet = "mods03", cols = 1:ncol(tab03), rows = 1)

writeData(
  wb.RQ02, sheet = "mods03", x = title_text, startRow = 1, startCol = 1)

## add table
writeData(
  wb.RQ02, sheet = "mods03", x = tab03, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ02, sheet = "mods03", style = header_style, 
  rows = 1:2, cols = 1:ncol(tab03), gridExpand = TRUE)

setColWidths(
  wb.RQ02, sheet = "mods03", 
  cols = 2:ncol(tab03), widths = "auto")


## Predicted values ------------------------------------------------------------
pp_sp    <- avg_predictions(mods03[["gdsp"]], 
                            variables = list(age_c = c(0,2,4,6,8,10,12)),
                            re.form = NA)

pp_pa    <- avg_predictions(mods03[["gdpa"]], 
                            variables = list(age_c = c(0,2,4,6,8,10,12)),
                            re.form = NA)

pp_wk    <-avg_predictions(mods03[["gdwk"]], 
                           variables = list(age_c = c(0,2,4,6,8,10,12)),
                           re.form = NA)

## combine dfs
pp_sp$group    <- "Spouse"
pp_pa$group    <- "Parent"
pp_wk$group    <- "Worker"

df_pp <- rbind(pp_wk, pp_sp, pp_pa)

df_pp <- df_pp |>
  ungroup() |>
  as.data.frame() |>
  mutate(
    age_c = age_c + 18,
    across(where(is.numeric), ~ round(.x, 3))) |>
  select(-c(df, s.value))

save(df_pp, file = paste0(outDir, "/fs_mods03_pp.rda")) # save R data to load next time


## add table to worksheet ------------------------------------------------------

addWorksheet(wb.RQ02, "mods03_predict")

title_text <- c("Model 03 predicted values by modal ages")

mergeCells(
  wb.RQ02, sheet = "mods03_predict", cols = 1:ncol(df_pp), rows = 1)

writeData(
  wb.RQ02, sheet = "mods03_predict", x = title_text, startRow = 1, startCol = 1)

### add table
writeData(
  wb.RQ02, sheet = "mods03_predict", x = df_pp, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ02, sheet = "mods03_predict", style = header_style, 
  rows = 1:2, cols = 1:ncol(df_pp), gridExpand = TRUE)

setColWidths(
  wb.RQ02, sheet = "mods03_predict", 
  cols = 2:ncol(df_pp), widths = "auto")

## Visualize it! ---------------------------------------------------------------

### load data if not starting fresh
df_pp<- read.xlsx( paste0(outDir, "/FS_RQ02.xlsx"), sheet = "mods03_predict", startRow = 2)

df_pp$group <- factor(df_pp$group,
                    levels = c("Worker", 
                               "Spouse",
                               "Parent"))
df_pp |> 
  ggplot(aes(x=age_c, y = estimate, color = group)) + 
  geom_line(linewidth = 1) +
  theme_minimal() +
  scale_y_continuous(
    breaks = c(0, 1, 2, 3, 4, 5),
    limits = c(0, 5)) +
  scale_x_continuous(
    breaks = c(18,20,22,24,26,28,30))
  

################################################################################
# Interaction (allowing for different rates of change)
################################################################################

equation_04 <- "%s ~ age_c + sex + sex:age_c + decades + mom_ba + raceeth + momwork + momdad + (1 + age_c | MTFID)"

mods04 <- lapply(vars, function(gd) {
  m1 <- as.formula(sprintf(equation_04, gd))
  lmer(m1, data = subset(df, wave <=6), weights = weight)
  }) |>
  set_names(vars)

tab04 <- modelsummary(
  mods04,
  estimate = "{estimate}",
  statistic = c(
    SE = "{std.error}",
    p = "{p.value}"),
  fmt = 3,
  output = "data.frame") 

tab04

summary(mods04[["gdsp"]])
# age = rate of change in expectations for men
# sex = difference in rating at age 18
# age:sexWomen = how different is the rate of change for women than men? 
  # to get exact: rate of change for men (-0.00) + -0.001 (interaction) = -0.001 (not much)

## add table to worksheet ------------------------------------------------------

addWorksheet(wb.RQ02, "mods04")

title_text <- c("Change model with gender * age and time constant predictors")

mergeCells(
  wb.RQ02, sheet = "mods04", cols = 1:ncol(tab04), rows = 1)

writeData(
  wb.RQ02, sheet = "mods04", x = title_text, startRow = 1, startCol = 1)

## add table
writeData(
  wb.RQ02, sheet = "mods04", x = tab04, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ02, sheet = "mods04", style = header_style, 
  rows = 1:2, cols = 1:ncol(tab04), gridExpand = TRUE)

setColWidths(
  wb.RQ02, sheet = "mods04", 
  cols = 2:ncol(tab04), widths = "auto")

## Predicted values ------------------------------------------------------------
ppp_sp    <- avg_predictions(mods04[["gdsp"]], 
                            variables = list(
                              age_c = c(0,2,4,6,8,10,12),
                              sex = c("Men", "Women")),
                            re.form = NA)

ppp_pa    <- avg_predictions(mods04[["gdpa"]], 
                             variables = list(
                               age_c = c(0,2,4,6,8,10,12),
                               sex = c("Men", "Women")),
                             re.form = NA)

ppp_wk    <- avg_predictions(mods04[["gdwk"]], 
                             variables = list(
                               age_c = c(0,2,4,6,8,10,12),
                               sex = c("Men", "Women")),
                             re.form = NA)

## combine dfs 
ppp_sp$group <- "Spouse"
ppp_pa$group <- "Parent"
ppp_wk$group <- "Worker"

df_ppp <- rbind(ppp_wk, ppp_sp, ppp_pa)

df_ppp <- df_ppp |>
  ungroup() |>
  as.data.frame() |>
  mutate(age_c = age_c + 18,
         across(where(is.numeric), ~ round(.x, 3))) |>
  select(-c(df, s.value))

save(df_ppp, file = paste0(outDir, "/fs_mods04_pp.rda")) # save R data to load next time


## add table to worksheet ------------------------------------------------------

addWorksheet(wb.RQ02, "mods04_predict")

title_text <- c("Model 04 predicted values by gender and modal ages")

mergeCells(
  wb.RQ02, sheet = "mods04_predict", cols = 1:ncol(df_ppp), rows = 1)

writeData(
  wb.RQ02, sheet = "mods04_predict", x = title_text, startRow = 1, startCol = 1)

### add table
writeData(
  wb.RQ02, sheet = "mods04_predict", x = df_ppp, 
  startRow = 2, startCol = 1,
  colNames = TRUE)

addStyle(
  wb.RQ02, sheet = "mods04_predict", style = header_style, 
  rows = 1:2, cols = 1:ncol(df_ppp), gridExpand = TRUE)

setColWidths(
  wb.RQ02, sheet = "mods04_predict", 
  cols = 2:ncol(df_ppp), widths = "auto")

## Visualize it! ---------------------------------------------------------------

### load data if not starting fresh
df_ppp<- read.xlsx( paste0(outDir, "/FS_RQ02.xlsx"), sheet = "mods04_predict", startRow = 2)

df_ppp$group <- factor(df_ppp$group,
                      levels = c("Worker", 
                                 "Spouse",
                                 "Parent"))
df_ppp |> 
  ggplot(aes(x=age_c, y = estimate, color = sex)) + 
  geom_line(linewidth = 1) +
  facet_wrap(~group) +
  theme_minimal() +
  scale_y_continuous(
    breaks = c(0, 1, 2, 3, 4, 5),
    limits = c(0, 5)) +
  scale_x_continuous(
    breaks = c(18,20,22,24,26,28,30))


################################################################################
# Save the workbook 
################################################################################

saveWorkbook(
  wb.RQ02,
  file.path(outDir, "FS_RQ02.xlsx"),
  overwrite = TRUE)

################################################################################
# Sensitivity Tests
################################################################################

# binary model -----------------------------------------------------------------

m3_sp_vg <- lmerTest::lmer(data = subset(df, wave <=6), 
                           gdsp_vg ~ 1 + age_c + sex + decades + mom_ba + raceeth + momwork + momdad + (1 + age_c | MTFID), weights = weight)
m3_pa_vg <- lmerTest::lmer(data = subset(df, wave <=6), 
                           gdpa_vg ~ 1 + age_c + sex + decades + mom_ba + raceeth + momwork + momdad + (1 + age_c | MTFID), weights = weight)
m3_wk_vg <- lmerTest::lmer(data = subset(df, wave <=6), 
                           gdwk_vg ~ 1 + age_c + sex + decades + mom_ba + raceeth + momwork + momdad + (1 + age_c | MTFID), weights = weight)

summary(m3_sp_vg)
summary(m3_pa_vg)
summary(m3_wk_vg)

## same take-away --> no change over the lifespan


# Ordinal model ----------------------------------------------------------------

# glmmTMB doesn't run
# bms can't use complex survey weights
# clmm doesn't support random slopes


