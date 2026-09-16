#-------------------------------------------------------------------------------
# MTF FUTURE SELVES PROJECT
# FS_02_descriptives.R
# Joanna R. Pepin & Melissa Milkie
#-------------------------------------------------------------------------------

# Create table 01 --------------------------------------------------------------
mtf_svy$variables$race_char <- as.character(mtf_svy$variables$race) # avoiding weird NAs!

tab1 <- mtf_svy |>
  gtsummary::select(c(gdsp, gdpa, gdwk,
                      sex, mom_ba, momwork, momdad, race_char, year)) |>
  tbl_svysummary(
    by = sex,
    #    type = list(c(happy_N_std, lifesat_N_std) ~ "continuous2"),
    type  = list(
      c(mom_ba)  ~ "dichotomous",
      c(momwork) ~ "dichotomous",
      c(momdad)  ~ "dichotomous"),
    value = list(
      mom_ba  = "Completed college",
      momwork = "Most or all the time",
      momdad  = "Both Mother & Father"),
    label = list(
      gdsp            ~ "Expectations as spouse",
      gdpa            ~ "Expectations as parent",
      gdwk            ~ "Expectations as worker",
      mom_ba          ~ "Mom completed college",
      momwork         ~ "Mom employed most/all of the time",
      momdad          ~ "Living with both parents",
      race_char       ~ "Race identity"),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} {p}%"))  |>
  add_overall() |>
  #  add_p() |>
  # add_p(test = list(
  #all_continuous() ~ "svy.t.test",
  #all_categorical() ~ "svy.wald.test")) |>
  modify_header(
    label  = '**Variable**',
    all_stat_cols() ~ "**{level}**  
    N = {style_number(n_unweighted)} ({style_percent(p)}%)") |>
  modify_footnote(c(all_stat_cols()) ~ NA) |>
  modify_caption("Table 01. Weighted statistics of the pooled analytic sample") |>
  as_flex_table() 

#  add_footer_lines("notes")

tab1 # show table

save_as_docx(tab1, path = file.path(outDir, "FS_table01.docx"))
