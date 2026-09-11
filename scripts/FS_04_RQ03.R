#-------------------------------------------------------------------------------
# FS Project
# FS_04_RQ03_VDE.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Do early expectations or trajectories predict transitions? 
# Do they have independent effects?

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
    se_df[.x, ]
  )
) |>
  select(term, all_of(model_vars)) |>
  # remove empty SE rows for random effects
  filter(
    !(
      term == "" &
        if_all(all_of(model_vars), is.na)
    )
  )


# Pretty variable labels
term_labels <- c(
  "(Intercept)"                 = "Intercept",
  "int_mar"                     = "Initial spouse expectation (random intercept)",
  "slope_mar_z"                 = "Change in spouse expectation (random intercept)",
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
  "raceethAnother race"         = "\u00A0\u00A0\u00A0Another Race",
  "SD (Intercept MTFID)"        = "\u00A0\u00A0\u00A0SD Intercept",
  "SD (age_c MTFID)"            = "\u00A0\u00A0\u00A0SD Age Slope",
  "Cor (Intercept~age_c MTFID)" = "\u00A0\u00A0\u00A0Correlation (Intercept, Age)",
  "SD (Observations)"           = "\u00A0\u00A0\u00A0Residual SD"
)
