#-------------------------------------------------------------------------------
# FS Project
# FS_04_RQ02_VDE.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Do expectations change over the life course (18-30)?

tbl03 <- read_excel(
  here("data", "FS_RQ02.xlsx"), 
  sheet = "mods03",
  skip = 1) |>
  select(-c(part, gdwk)) 

# Split into coefficients, SEs, and p-values
coef_df <- tbl03 |>
  filter(statistic == "estimate") |>
  mutate(across(-c(term, statistic), as.numeric))

## model names
model_vars <- c("gdsp", "gdpa")

## SE
se_df <- tbl03 |>
  filter(statistic == "{std.error}") |>
  mutate(across(-c(term, statistic), as.numeric))

## p values
p_df <- tbl03 |>
  filter(statistic == "{p.value}")

# create the coefficient rows
coef_df <- coef_df |>
  mutate(
    gdsp = paste0(
      sprintf("%.2f", gdsp),
      stars(p_df$gdsp)),
    gdpa = paste0(
      sprintf("%.2f", gdpa),
      stars(p_df$gdpa))
  )

# create the se rows
se_df <- se_df |>
  mutate(
    term = "",
    gdsp = paste0("(", sprintf("%.2f", gdsp), ")"),
    gdpa = paste0("(", sprintf("%.2f", gdpa), ")")
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
  "age_c"                       = "Age (centered)",
  "sexWomen"                    = "Woman",
  "decades1960s"                = "\u00A0\u00A0\u00A01960s",
  "decades1970s"                = "\u00A0\u00A0\u00A01970s",
  "decades1980s"                = "\u00A0\u00A0\u00A01980s",
  "decades1990s"                = "\u00A0\u00A0\u00A01990s",
  "decades2000s"                = "\u00A0\u00A0\u00A02000s",
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

# create category headers 
decade_row <- tibble(
  term = "Birth Decade (ref. 1950s)",
  gdsp = "",
  gdpa = "")

race_row <- tibble(
  term = "Race/ethnicity (ref. White)",
  gdsp = "",
  gdpa = "")

re_row <- tibble(
  term = "Random Effects",
  gdsp = "",
  gdpa = "")

# find var heading insertion positions
re_pos <- which(
  grepl("^SD|^Cor", tbl_display$term)
)[1]

decade_pos <- which(tbl_display$term == "decades1960s")[1]
race_pos <- which(tbl_display$term == "raceethBlack")[1]

# insert heading rows
tbl_display <- bind_rows(
  tbl_display[1:(decade_pos - 1), ],
  decade_row,
  tbl_display[decade_pos:(race_pos - 1), ],
  race_row,
  tbl_display[race_pos:(re_pos - 1), ],
  re_row,
  tbl_display[re_pos:nrow(tbl_display),]
)


# create the table:
tbl03 <- tbl_display |>
  mutate(term = recode(term, !!!term_labels)) |>
  gt() |>
  tab_header(
    title = md("**Table 03. Multilevel Models of Age-Related Changes in Spouse and Parent Role Expectations**")) |>
  cols_label(
    term = "",
    gdsp = md("**Spouse**"),
    gdpa = md("**Parent**")) |>
  tab_source_note(
    md("\\* p < .05; \\** p < .01; \\*** p < .001")) |>
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

# Visualize it -----------------------------------------------------------------

df2_pp <- read_excel(
  here("data", "FS_RQ02.xlsx"), 
  sheet = "mods04_predict",
  skip = 1) |>
  filter(group != "Worker")

df2_pp$group <- factor(df2_pp$group,
                       levels = c("Spouse", "Parent"))

df2_pp |>
  ggplot(aes(x = age_c, y = estimate, color = sex)) +
  geom_line(linewidth = 1) +
  facet_wrap(~group) +
  theme_minimal() +
  scale_y_continuous(
    breaks = c(0, 1, 2, 3, 4, 5),
    limits = c(0, 5)) +
  scale_x_continuous(
    breaks = c(18,20,22,24,26,28,30)
  )


