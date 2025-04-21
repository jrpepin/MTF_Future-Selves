## PS Trends -------------------------------------------------------------------
# Averages of "very good"

avg_sp <- mtf_svy %>%
  select(year, gdsp_v) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdsp_v, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_pa <- mtf_svy %>%
  select(year, gdpa_v) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdpa_v, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_wk <- mtf_svy %>%
  select(year, gdwk_v) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdwk_v, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

#### Combine dfs
avg_sp$cat    <- "Spouse" 
avg_pa$cat    <- "Parent" 
avg_wk$cat    <- "Worker" 

df_avg <- rbind(avg_sp, avg_pa, avg_wk)

p1 <- df_avg %>%
  ggplot(aes(x = year, y = vals, color = cat, 
             ymin = vals_low, ymax = vals_upp)) +
  #  geom_ribbon(fill = "lightgrey", linetype = "dotted", alpha=0.1) +
  #  geom_line(linewidth = 1) +
  geom_smooth(alpha = .1, method="loess", se=TRUE, fullrange=FALSE, level=0.95) +
  geom_point(alpha = .5) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    panel.grid.minor = element_blank()) +
  guides(color = guide_legend(reverse=TRUE)) +
  scale_x_continuous(limits = c(1984, 2025), breaks = c(1985, 1995, 2005, 2015, 2025)) +
  scale_y_continuous(limits = c(0, .75), breaks = c(0, .25, .50, .75), labels = percent) +
  labs( x        = " ", 
        y        = " ", 
        color    = " ",
        title    = "% who think they would be 'very good' as a _________",
#        subtitle = "Scale: (1) Poor - (5) Very good",
        caption  = "Monitoring the Future 12th Grade Surveys (1984-2023)")

p1 

ggsave(file.path(here(outDir, figDir),"PS_fig01.png"), p1, 
       width = 5.5, height = 5.5, dpi = 300, bg = 'white')


## Logit Regressions -----------------------------------------------------------

m1_wk <- glm(gdwk ~ sex + race + momed + region + year.c + year.c.sq + religion + famstru.d + selfconcept,
             data = data, weights = svyweight, family = "binomial")

m1_sp <- glm(gdsp ~ sex + race + momed + region + year.c + year.c.sq + religion + famstru.d + selfconcept,
             data = data, weights = svyweight, family = "binomial")

m1_pa <- glm(gdpa ~ sex + race + momed + region + year.c + year.c.sq + religion + famstru.d + selfconcept,
             data = data, weights = svyweight, family = "binomial")

## Turn into tidy dataframes
tidy.wk <- broom::tidy(m1_wk)
tidy.sp <- broom::tidy(m1_sp)
tidy.pa <- broom::tidy(m1_pa)

## Transform output
tidy.wk <- tidy.wk %>%
  mutate(p.value  = round(p.value, 3),
         estimate = exp(estimate))

tidy.sp <- tidy.sp %>%
  mutate(p.value  = round(p.value, 3),
         estimate = exp(estimate))

tidy.pa <- tidy.pa %>%
  mutate(p.value  = round(p.value, 3),
         estimate = exp(estimate))

## Turn into modelsummary objects
mod.wk        <- list(tidy = tidy.wk)
mod.sp        <- list(tidy = tidy.sp)
mod.pa        <- list(tidy = tidy.pa)

class(mod.wk) <- "modelsummary_list"
class(mod.sp) <- "modelsummary_list"
class(mod.pa) <- "modelsummary_list"

mods <- list(
  "Worker" = mod.wk,
  "Spouse" = mod.sp,
  "Parent" = mod.pa)

## Regression Table

modelsummary(mods,
             stars = c("*" =.05, "**" = .01, "***" = .001),
             fmt = fmt_decimal(digits = 3, pdigits = 3),
             output = "huxtable") %>%
  huxtable::as_flextable()


wk_pp <- avg_predictions(m1_wk, variables = "year.c")
