#-------------------------------------------------------------------------------
# FS Project
# FS_03_RQ02_VDE.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Do expectations change over the life course (18-30)?

tbl02 <- read_excel(
  here("data", "FS_RQ02.xlsx"), 
  sheet = "mods03",
  skip = 1) |>
  select(-c(part)) 

# Split into coefficients, SEs, and p-values
coef_df <- tbl02 |>
  filter(statistic == "estimate") |>
  mutate(across(-c(term, statistic), as.numeric))

se_df <- tbl02 |>
  filter(statistic == "{std.error}") |>
  mutate(across(-c(term, statistic), as.numeric))

p_df <- tbl02 |>
  filter(statistic == "{p.value}")


# create the coefficient rows
coef_df <- coef_df |>
  mutate(
    gdsp = paste0(
      sprintf("%.2f", gdsp),
      stars(p_df$gdsp)),
    gdpa = paste0(
      sprintf("%.2f", gdpa),
      stars(p_df$gdpa)),
    gdwk = paste0(
      sprintf("%.2f", gdwk),
      stars(p_df$gdwk))
  )

# create the se rows
se_df <- se_df |>
  mutate(
    term = "",
    gdsp = paste0("(", sprintf("%.2f", gdsp), ")"),
    gdpa = paste0("(", sprintf("%.2f", gdpa), ")"),
    gdwk = paste0("(", sprintf("%.2f", gdwk), ")")
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
  select(term, gdsp, gdpa, gdwk)

#create the table:
tbl_display |>
  gt() |>
  cols_label(
    term = "",
    gdsp = md("**Spouse**"),
    gdpa = md("**Parent**"),
    gdwk = md("**Worker**")
  ) |>
  tab_source_note(
    md("*p* < .05; **p** < .01; ***p*** < .001")
  ) |>
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(2)
  )
    