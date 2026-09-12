#-------------------------------------------------------------------------------
# FS Project
# FS_04_RQ03_VDE.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Do early expectations or trajectories predict transitions? 
# Do they have independent effects?

################################################################################
# Marriage Table
################################################################################

tbl03_mar <- read_excel(
  here("data", "FS_RQ03.xlsx"), 
  sheet = "mods05_mar",
  skip = 1) |>
  select(-c(part)) 

# Split into coefficients, SEs, and p-values
coef_df <- tbl03_mar |>
  filter(statistic == "estimate") |>
  mutate(across(-c(term, statistic), as.numeric))

## model names
model_vars <- names(coef_df) |>
  grep("^(ever_mar_|mar_at_)", x = _, value = TRUE)

## SE
se_df <- tbl03_mar |>
  filter(statistic == "{std.error}") |>
  mutate(across(-c(term, statistic), as.numeric))

## p values
p_df <- tbl03_mar |>
  filter(statistic == "{p.value}")

# coefficient rows
coef_df <- coef_df |>
  mutate(
    across(
      all_of(model_vars),
      ~ paste0(sprintf("%.2f", .x), stars(p_df[[cur_column()]]))
    )
  )

# standard error rows
se_df <- se_df |>
  mutate(
    term = "",
    across(
      all_of(model_vars),
      ~ paste0("(", sprintf("%.2f", .x), ")")
    )
  )

# create significance stars function
stars <- function(p) {
  case_when(
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    TRUE     ~ ""
  )
}

# make sure p-values are numeric
p_df <- p_df |>
  mutate(across(-c(term, statistic), as.numeric))

# interleave coef and SE rows
tbl_display <- purrr::map_dfr(
  seq_len(nrow(coef_df)),
  ~ bind_rows(
    coef_df[.x, ],
    se_df[.x, ])) |>
  select(term, all_of(model_vars)) |>
  # remove empty SE rows for random effects
  filter(
    !(
      term == "" &
        if_all(all_of(model_vars), is.na))
  )


# Pretty variable labels
term_labels <- c(
  "(Intercept)"                 = "Intercept",
  "int_mar"                     = "Initial spouse expectation (random intercept)",
  "slope_mar_z"                 = "Change in spouse expectation (random slope)",
  "age_c"                       = "Age (centered)",
  "sexWomen"                    = "Woman",
  "decades1960s"                = "\u00A0\u00A0\u00A01960s",
  "decades1970s"                = "\u00A0\u00A0\u00A01970s",
  "decades1980s"                = "\u00A0\u00A0\u00A01980s",
  "decades1990s"                = "\u00A0\u00A0\u00A01990s",
  "decades2000s"                = "2000s",
  "mom_ba"                      = "Rs mom had BA degree or more",
  "momwork"                     = "Rs mom mostly/always employed",
  "momdad"                      = "R lived w/ both mom & dad at BY",
  "raceethBlack"                = "\u00A0\u00A0\u00A0Black",
  "raceethAnother race"         = "\u00A0\u00A0\u00A0Another Race"
  )

# create category headers 
decade_row <- tibble(
  term = "Birth Decade (ref. 1950s)",
  ever_mar_30 = "",
  ever_mar_35 = "",
  ever_mar_40 = "",
  ever_mar_45 = "",
  mar_at_30   = "",
  mar_at_35   = "",
  mar_at_40   = "",
  mar_at_45   = "")

race_row <- tibble(
  term = "Race/ethnicity (ref. White)",
  ever_mar_30 = "",
  ever_mar_35 = "",
  ever_mar_40 = "",
  ever_mar_45 = "",
  mar_at_30   = "",
  mar_at_35   = "",
  mar_at_40   = "",
  mar_at_45   = "")

# move intercept and SE row to bottom
intercept_row <- which(tbl_display$term == "(Intercept)")

tbl_display <- bind_rows(
  tbl_display[-c(intercept_row, intercept_row + 1), ],
  tbl_display[c(intercept_row, intercept_row + 1), ]
)

# find var heading insertion positions
decade_pos <- which(tbl_display$term == "decades1960s")[1]
race_pos <- which(tbl_display$term == "raceethBlack")[1]

# insert heading rows
tbl_display <- bind_rows(
  tbl_display[1:(decade_pos - 1), ], decade_row,
  tbl_display[decade_pos:(race_pos - 1), ], race_row,
  tbl_display[race_pos:nrow(tbl_display), ]) |>
  mutate(
    term = recode(term, !!!term_labels)) |>
  mutate(across(everything(),~ ifelse(. == "NA", "", .))) |>
  mutate(across(everything(),~ ifelse(. == "(NA)", "", .)))

# create the gt table:
tbl03 <- tbl_display  |>
  gt() |>
  tab_header(
    title = md(
      "**Table 03. Logistic Regression Models Predicting Marriage Outcomes**"),
    subtitle = md(
      "*From Initial Spouse Expectations and Changes in Spouse Expectations*")) |>
  cols_label(
    term = "",
    ever_mar_30 = "30",
    ever_mar_35 = "35",
    ever_mar_40 = "40",
    ever_mar_45 = "45",
    mar_at_30 = "30",
    mar_at_35 = "35",
    mar_at_40 = "40",
    mar_at_45 = "45") |>
  tab_spanner(
    label = md("**Ever Married by Age**"),
    columns = c(ever_mar_30, ever_mar_35, ever_mar_40, ever_mar_45)) |>
  tab_spanner(
    label = md("**Married at Age**"),
    columns = c(mar_at_30, mar_at_35, mar_at_40, mar_at_45)) |>
  tab_source_note(
    md("\\* p < .05; \\*\\* p < .01; \\*\\*\\* p < .001")) |>
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(2)
  )

tbl03    

# Export the table to word
gtsave(
  tbl03,
  here("output", "Table3.docx")
)


################################################################################
# Parenthood Table
################################################################################

tbl03_par <- read_excel(
  here("data", "FS_RQ03.xlsx"), 
  sheet = "mods06_par",
  skip = 1) |>
  select(-c(part)) 

# Split into coefficients, SEs, and p-values
coef_df <- tbl03_par |>
  filter(statistic == "estimate") |>
  mutate(across(-c(term, statistic), as.numeric))

## model names
model_vars <- names(coef_df) |>
  grep("^(parent_|numkids_)", x = _, value = TRUE)

## SE
se_df <- tbl03_par |>
  filter(statistic == "{std.error}") |>
  mutate(across(-c(term, statistic), as.numeric))

## p values
p_df <- tbl03_par |>
  filter(statistic == "{p.value}")

# coefficient rows
coef_df <- coef_df |>
  mutate(
    across(
      all_of(model_vars),
      ~ paste0(sprintf("%.2f", .x), stars(p_df[[cur_column()]]))
    )
  )

# standard error rows
se_df <- se_df |>
  mutate(
    term = "",
    across(
      all_of(model_vars),
      ~ paste0("(", sprintf("%.2f", .x), ")")
    )
  )

# create significance stars function
stars <- function(p) {
  case_when(
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    TRUE     ~ ""
  )
}

# make sure p-values are numeric
p_df <- p_df |>
  mutate(across(-c(term, statistic), as.numeric))

# interleave coef and SE rows
tbl_display <- purrr::map_dfr(
  seq_len(nrow(coef_df)),
  ~ bind_rows(
    coef_df[.x, ],
    se_df[.x, ])) |>
  select(term, all_of(model_vars)) |>
  # remove empty SE rows for random effects
  filter(
    !(
      term == "" &
        if_all(all_of(model_vars), is.na))
  )


# Pretty variable labels
term_labels <- c(
  "(Intercept)"                 = "Intercept",
  "int_par"                     = "Initial parent expectation (random intercept)",
  "slope_par_z"                 = "Change in parent expectation (random slope)",
  "age_c"                       = "Age (centered)",
  "sexWomen"                    = "Woman",
  "decades1960s"                = "\u00A0\u00A0\u00A01960s",
  "decades1970s"                = "\u00A0\u00A0\u00A01970s",
  "decades1980s"                = "\u00A0\u00A0\u00A01980s",
  "decades1990s"                = "\u00A0\u00A0\u00A01990s",
  "decades2000s"                = "2000s",
  "mom_ba"                      = "Rs mom had BA degree or more",
  "momwork"                     = "Rs mom mostly/always employed",
  "momdad"                      = "R lived w/ both mom & dad at BY",
  "raceethBlack"                = "\u00A0\u00A0\u00A0Black",
  "raceethAnother race"         = "\u00A0\u00A0\u00A0Another Race"
)

# create category headers 
decade_row <- tibble(
  term = "Birth Decade (ref. 1950s)",
  parent_30 = "",
  parent_35 = "",
  parent_40 = "",
  parent_45 = "",
  numkids_30   = "",
  numkids_35   = "",
  numkids_40   = "",
  numkids_45   = "")

race_row <- tibble(
  term = "Race/ethnicity (ref. White)",
  parent_30 = "",
  parent_35 = "",
  parent_40 = "",
  parent_45 = "",
  numkids_30   = "",
  numkids_35   = "",
  numkids_40   = "",
  numkids_45   = "")

# move intercept and SE row to bottom
intercept_row <- which(tbl_display$term == "(Intercept)")

tbl_display <- bind_rows(
  tbl_display[-c(intercept_row, intercept_row + 1), ],
  tbl_display[c(intercept_row, intercept_row + 1), ]
)

# find var heading insertion positions
decade_pos <- which(tbl_display$term == "decades1960s")[1]
race_pos <- which(tbl_display$term == "raceethBlack")[1]

# insert heading rows
tbl_display <- bind_rows(
  tbl_display[1:(decade_pos - 1), ], decade_row,
  tbl_display[decade_pos:(race_pos - 1), ], race_row,
  tbl_display[race_pos:nrow(tbl_display), ]) |>
  mutate(
    term = recode(term, !!!term_labels)) |>
  mutate(across(everything(),~ ifelse(. == "NA", "", .))) |>
  mutate(across(everything(),~ ifelse(. == "(NA)", "", .)))

# create the gt table:
tbl04 <- tbl_display  |>
  gt() |>
  tab_header(
    title = md(
      "**Table 04. Logistic Regression Models Predicting Parenthood Outcomes**"),
    subtitle = md(
      "*From Initial Parent Expectations and Changes in Parent Expectations*")) |>
  cols_label(
    term = "",
    parent_30 = "30",
    parent_35 = "35",
    parent_40 = "40",
    parent_45 = "45",
    numkids_30 = "30",
    numkids_35 = "35",
    numkids_40 = "40",
    numkids_45 = "45") |>
  tab_spanner(
    label = md("**Parent by Age**"),
    columns = c(parent_30, parent_35, parent_40, parent_45)) |>
  tab_spanner(
    label = md("**Number of Children at Age**"),
    columns = c(numkids_30, numkids_35, numkids_40, numkids_45)) |>
  tab_source_note(
    md("\\* p < .05; \\*\\* p < .01; \\*\\*\\* p < .001")) |>
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(2)
  )

tbl04    

# Export the table to word
gtsave(
  tbl04,
  here("output", "Table4.docx")
)

