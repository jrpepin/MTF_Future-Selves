#-------------------------------------------------------------------------------
# MTF FUTURE SELVES PROJECT
# FS_02_tables & figures.R
# Joanna R. Pepin & Melissa Milkie
#-------------------------------------------------------------------------------

# Create table 01 --------------------------------------------------------------
mtf_svy$variables$race_char <- as.character(mtf_svy$variables$race) # avoiding weird NAs!

tab1 <- mtf_svy %>%
  gtsummary::select(c(gdsp, gdpa, gdwk,
                      sex, momed, race_char, region)) %>%
  tbl_svysummary(
    by = sex,
#    type = list(c(happy_N_std, lifesat_N_std) ~ "continuous2"),
    type  = list(
      c(momed) ~ "dichotomous"),
    value = list(momed = "Completed college"),
    label = list(
      gdsp            ~ "Expectations as spouse",
      gdpa            ~ "Expectations as parent",
      gdwk            ~ "Expectations as worker",
      momed           ~ "Mom completed college",
      race_char       ~ "Race identity",
      region          ~ "Region"),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} {p}%"))  %>%
  add_overall() %>%
#  add_p() %>%
  # add_p(test = list(
  #all_continuous() ~ "svy.t.test",
  #all_categorical() ~ "svy.wald.test")) %>%
  modify_header(
    label  = '**Variable**',
    all_stat_cols() ~ "**{level}**  
    N = {style_number(n_unweighted)} ({style_percent(p)}%)") %>%
  modify_footnote(c(all_stat_cols()) ~ NA) %>%
  modify_caption("Table 01. Weighted statistics of the pooled analytic sample") %>%
  as_flex_table() 

#  add_footer_lines("notes")

tab1 # show table

save_as_docx(tab1, path = file.path(outDir, "PS_table01.docx"))

# Create 3 tables to make by hand Table 2 (for now) ----------------------------

## Spouse ----------------------------------------------------------------------
polr1.sp  <- polr(gdsp ~ year.c + I(year.c^2),
                  data = data, weights = svyweight, Hess = T)

polr2.sp  <- polr(gdsp ~ year.c + I(year.c^2) + 
                    sex + momed + race + region,
                  data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidy1.sp <- broom::tidy(polr1.sp)
tidy2.sp <- broom::tidy(polr2.sp)

## Transform output
tidy1.sp <- tidy1.sp %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidy2.sp <- tidy2.sp %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

## Turn into modelsummary objects
mod1.sp        <- list(tidy = tidy1.sp)
class(mod1.sp) <- "modelsummary_list"

mod2.sp        <- list(tidy = tidy2.sp)
class(mod2.sp) <- "modelsummary_list"

mods.sp <- list(
  "Model 1" = mod1.sp,
  "Model 2" = mod2.sp)

cm <- c('year.c'                             = 'Year',
        'I(year.c^2)'                        = 'Year squared',
        'sexWomen'                           = "Women",
        'momedCompleted college'             = 'Mom BA or more',
        'raceBlack'                          = "Black",
        'raceAnother race'                   = "Another race",
        'regionMidwest'                      = "Midwest",
        'regionSouth'                        = "South",
        'regionWest'                         = 'West',
        'Poor|Not so good'                   = 'Poor|Not so good',
        'Not so good|Fairly good'            = 'Not so good|Fairly good',
        'Fairly good|Good'                   = 'Fairly good|Good',
        'Good|Very good'                     = 'Good|Very good')

tab2.sp <- modelsummary(mods.sp,
                        shape = term ~ model + statistic,
                        stars = c("*" =.05, "**" = .01, "***" = .001),
                        coef_map = cm,
                        fmt = fmt_decimal(digits = 3, pdigits = 3),
                        output = "huxtable") %>%
  huxtable::as_flextable()

tab2.sp

read_docx() %>% 
  body_add_par(paste("Table 2. Spouse", sep="")) %>% 
  body_add_flextable(value = tab2.sp)          %>% 
  print(target = file.path(outDir, "PS_table02_sp.docx"))                 


## Parent ----------------------------------------------------------------------
polr1.pa  <- polr(gdpa ~ year.c + I(year.c^2),
                  data = data, weights = svyweight, Hess = T)

polr2.pa  <- polr(gdpa ~ year.c + I(year.c^2) + 
                    sex + momed + race + region,
                  data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidy1.pa <- broom::tidy(polr1.pa)
tidy2.pa <- broom::tidy(polr2.pa)

## Transform output
tidy1.pa <- tidy1.pa %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidy2.pa <- tidy2.pa %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

## Turn into modelsummary objects
mod1.pa        <- list(tidy = tidy1.pa)
class(mod1.pa) <- "modelsummary_list"

mod2.pa        <- list(tidy = tidy2.pa)
class(mod2.pa) <- "modelsummary_list"

mods.pa <- list(
  "Model 1" = mod1.pa,
  "Model 2" = mod2.pa)

# Use cm from spouse models

tab2.pa <- modelsummary(mods.pa,
                        shape = term ~ model + statistic,
                        stars = c("*" =.05, "**" = .01, "***" = .001),
                        coef_map = cm,
                        fmt = fmt_decimal(digits = 3, pdigits = 3),
                        output = "huxtable") %>%
  huxtable::as_flextable() 

tab2.pa

read_docx() %>% 
  body_add_par(paste("Table 2. Parent", sep="")) %>% 
  body_add_flextable(value = tab2.pa)          %>% 
  print(target = file.path(outDir, "PS_table02_pa.docx"))                 

## Worker ----------------------------------------------------------------------

# polr1.wk  <- polr(gdwk ~ poly(year.c, 2, raw = TRUE), data = data, weights = svyweight, Hess = T) <- same as I(year.c^2)

polr1.wk  <- polr(gdwk ~ year.c + I(year.c^2),
                  data = data, weights = svyweight, Hess = T)

polr2.wk  <- polr(gdwk ~ year.c + I(year.c^2) + 
                    sex + momed + race + region,
                  data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidy1.wk <- broom::tidy(polr1.wk)
tidy2.wk <- broom::tidy(polr2.wk)

## Transform output
tidy1.wk <- tidy1.wk %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidy2.wk <- tidy2.wk %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

## Turn into modelsummary objects
mod1.wk        <- list(tidy = tidy1.wk)
class(mod1.wk) <- "modelsummary_list"

mod2.wk        <- list(tidy = tidy2.wk)
class(mod2.wk) <- "modelsummary_list"

mods.wk <- list(
  "Model 1" = mod1.wk,
  "Model 2" = mod2.wk)

# Use cm from spouse models

tab2.wk <- modelsummary(mods.wk,
                        shape = term ~ model + statistic,
                        stars = c("*" =.05, "**" = .01, "***" = .001),
                        coef_map = cm,
                        fmt = fmt_decimal(digits = 3, pdigits = 3),
                        output = "huxtable") %>%
  huxtable::as_flextable()

tab2.wk

read_docx() %>% 
  body_add_par(paste("Table 2. Work", sep="")) %>% 
  body_add_flextable(value = tab2.wk)          %>% 
  print(target = file.path(outDir, "PS_table02_wk.docx"))                 

# Create Figure 1 --------------------------------------------------------------

## Average Predictions 
pp_sp   <- predict_response(polr1.sp, terms = c("year.c [all]"))
pp_pa   <- predict_response(polr1.pa, terms = c("year.c [all]"))
pp_wk   <- predict_response(polr1.wk, terms = c("year.c [all]"))

### These all do the same thing, I think -- check against Stata later
#pp_sp   <- ggaverage(polr1.sp, terms = c("year.c [all]"))
#pp_sp   <- predict_response(polr1.sp, terms = c("year.c [all]"), margin = "marginalmeans")

## Combine dfs
pp_sp$cat    <- "Spouse" 
pp_pa$cat    <- "Parent" 
pp_wk$cat    <- "Worker" 

df_pp <- rbind(pp_wk, pp_sp, pp_pa)

## Tidy variables
df_pp$response.level <- factor(df_pp$response.level, 
                               levels=c("Very good", 
                                        "Good", 
                                        "Fairly good", 
                                        "Not so good", 
                                        "Poor"))

df_pp$cat <- factor(df_pp$cat, 
                    levels=c("Worker", 
                             "Spouse", 
                             "Parent"))

data %>%
  filter(
    year == 1976 |
    year == 1985 | year == 1995 | 
    year == 2005 | year == 2015 | year == 2023) %>%
  distinct(year.c)


lables_year <- c("1976", "1985", "1995", "2005", "2015", "2023")

## Plot panels

p1_very <- df_pp %>%
  filter(response.level == "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = cat, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  facet_wrap(~response.level) +
  theme_minimal() + 
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position = "top",
    legend.title=element_blank(),
    strip.text.x = element_text(face = "bold"),
    panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-20.55, -11.55, -1.55, 8.45, 18.14, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ")

p1_other <- df_pp %>%
  filter(response.level != "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = cat, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  facet_wrap(~response.level) +
  theme_minimal() + 
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position = "none",
    legend.title=element_blank(),
    strip.text.x = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5), limits=c(0, .5), labels = scales::percent) +
  scale_x_continuous(breaks=c(-20.55, -11.55, -1.55, 8.45, 18.14, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ")

## Combine plots
p1 <- (p1_very | p1_other) + plot_layout(widths = c(1, 2)) +
  plot_annotation('How good do you think you would be as a _________', 
                  caption = "Monitoring the Future 12th Grade Surveys (1976-2023)")

p1

## Save Fig 1
agg_tiff(filename = file.path(here(outDir, figDir), "fig1.tif"), 
         width=8, height=6, units="in", res = 800, scaling = 1)

plot(p1)
invisible(dev.off())


# Table 3 ----------------------------------------------------------------------

polr2.sp.RS  <- polr(gdsp ~ year.c * sex + I(year.c^2) * sex +
                       momed + race + region,
                     data = data, weights = svyweight, Hess = T)

polr2.pa.RS  <- polr(gdpa ~ year.c * sex + I(year.c^2) * sex  + 
                       momed + race + region,
                     data = data, weights = svyweight, Hess = T)

polr2.wk.RS  <- polr(gdwk ~ year.c * sex + I(year.c^2) * sex  + 
                       momed + race + region,
                     data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidySP.3 <- broom::tidy(polr2.sp.RS)
tidyPA.3 <- broom::tidy(polr2.pa.RS)
tidyWK.3 <- broom::tidy(polr2.wk.RS)

## Transform output
tidySP.3 <- tidySP.3 %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidyPA.3 <- tidyPA.3 %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidyWK.3 <- tidyWK.3 %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

## Turn into modelsummary objects
modSP.3        <- list(tidy = tidySP.3)
class(modSP.3) <- "modelsummary_list"

modPA.3        <- list(tidy = tidyPA.3)
class(modPA.3) <- "modelsummary_list"

modWK.3        <- list(tidy = tidyWK.3)
class(modWK.3) <- "modelsummary_list"

mods.3 <- list(
  "Spouse" = modSP.3,
  "Parent" = modPA.3,
  "Worker" = modWK.3)

cm <- c('year.c'                             = 'Year',
        'I(year.c^2)'                        = 'Year squared',
        'sexWomen'                           = "Women",
        'year.c:sexWomen'                    = 'Year * Women',
        'sexWomen:I(year.c^2)'               = 'Year2 * Women',
        'momedCompleted college'             = 'Mom BA or more',
        'raceBlack'                          = "Black",
        'raceAnother race'                   = "Another race",
        'regionMidwest'                      = "Midwest",
        'regionSouth'                        = "South",
        'regionWest'                         = 'West',
        'Poor|Not so good'                   = 'Poor|Not so good',
        'Not so good|Fairly good'            = 'Not so good|Fairly good',
        'Fairly good|Good'                   = 'Fairly good|Good',
        'Good|Very good'                     = 'Good|Very good')


table3 <- modelsummary(mods.3,
                        shape = term ~ model + statistic,
                        stars = c("*" =.05, "**" = .01, "***" = .001),
                        coef_map = cm,
                        fmt = fmt_decimal(digits = 3, pdigits = 3),
                        output = "huxtable") %>%
  huxtable::as_flextable()  %>%
  add_footer_lines("Notes: 99,399")

table3

read_docx() %>% 
  body_add_par(paste("Table 3. Year & Gender interactions", sep="")) %>% 
  body_add_flextable(value = table3)          %>% 
  print(target = file.path(outDir, "PS_table03.docx"))                 


# Figure 2 ---------------------------------------------------------------------

## Average Predictions 
pp_sp_sex   <- predict_response(polr2.sp.RS, terms = c("year.c [all]", "sex"))
pp_pa_sex   <- predict_response(polr2.pa.RS, terms = c("year.c [all]", "sex"))
pp_wk_sex   <- predict_response(polr2.wk.RS, terms = c("year.c [all]", "sex"))

## Combine dfs
pp_sp_sex$cat    <- "Spouse" 
pp_pa_sex$cat    <- "Parent" 
pp_wk_sex$cat    <- "Worker" 

df_pp_sex <- rbind(pp_wk_sex, pp_sp_sex, pp_pa_sex)

## Tidy variables
df_pp_sex$response.level <- factor(df_pp_sex$response.level, 
                               levels=c("Very good", 
                                        "Good", 
                                        "Fairly good", 
                                        "Not so good", 
                                        "Poor"))

df_pp_sex$cat <- factor(df_pp_sex$cat, 
                    levels=c("Spouse", 
                             "Parent",
                             "Worker"))

lables_year <- c("1976", "1985", "1995", "2005", "2015", "2023")


## Draw figure

p2 <- df_pp_sex %>%
  ggplot(aes(x = x, y = predicted, color = response.level,
             ymin = conf.low, ymax = conf.high)) +
#  geom_errorbar(width = 0.2, color="grey80") +
  geom_line(aes(linetype = group), linewidth = 1) +
  facet_wrap(~cat) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
 #   plot.title = element_text(face = "bold"),
    legend.position  = "right",
    strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-20.55, -11.55, -1.55, 8.45, 18.14, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "How good do you think you would be as a _________",
        color    = "Response level",
        linetype = " ",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2023)") 

p2

## Save Fig 2
agg_tiff(filename = file.path(here(outDir, figDir), "fig2.tif"), 
         width=9, height=6.5, units="in", res = 800, scaling = 1)

plot(p2)
invisible(dev.off())


