
df_pp_sex %>%
  #  filter(response.level == "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = response.level,
             ymin = conf.low, ymax = conf.high)) +
  geom_line(aes(linetype = group), linewidth = 1) +
  facet_wrap(~cat) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    plot.title = element_text(face = "bold"),
    legend.position  = "right",
    strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.55, 8.45, 18.45, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = " ",
        color    = "Response level",
        linetype = " ") 





### spouse
p2_very_sp <- df_pp_sex %>%
  filter(cat == "Spouse" & response.level == "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    plot.title = element_text(face = "bold"),
    legend.position  = "none",
    legend.title     = element_blank(),
    #    strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.55, 8.45, 18.45, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "spouse")

p2_very_sp

p2_other_sp <- df_pp_sex %>%
  filter(cat == "Spouse" & response.level != "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = .75) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position  = "none",
    legend.title     = element_blank(),
    #    strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5), limits=c(0, .5), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.55, 8.45, 18.45, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = " ")

p2_other_sp

## Combine plots
p2.sp <- (p2_very_sp | p2_other_sp) + plot_layout(widths = c(1, 2)) 

p2.sp


### parent
p2_very_pa <- df_pp_sex %>%
  filter(cat == "Parent" & response.level == "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    plot.title = element_text(face = "bold"),
    legend.position  = "none",
    legend.title     = element_blank(),
    #strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.55, 8.45, 18.45, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "parent")

p2_very_pa

p2_other_pa <- df_pp_sex %>%
  filter(cat == "Parent" & response.level != "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = .75) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position  = "none",
    legend.title     = element_blank(),
    #strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5), limits=c(0, .5), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.60, 8.40, 18.40, 26.40), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = " ")

p2_other_pa

## Combine plots
p2.pa <- (p2_very_pa | p2_other_pa) + plot_layout(widths = c(1, 2)) 

p2.pa


### worker
p2_very_wk <- df_pp_sex %>%
  filter(cat == "Worker" & response.level == "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    plot.title = element_text(face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    #strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.60, 8.40, 18.40, 26.40), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "worker")

p2_very_wk

p2_other_wk <- df_pp_sex %>%
  filter(cat == "Worker" & response.level != "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = .75) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position  = "none",
    legend.title     = element_blank(),
    #strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5), limits=c(0, .5), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.60, 8.40, 18.40, 26.40), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = " ")

p2_other_wk

## Combine plots
p2.wk <- (p2_very_wk | p2_other_wk) + plot_layout(widths = c(1, 2)) 

p2.wk

### combine all selves figures

p2 <- (p2.sp / p2.pa / p2.wk/ guide_area()) + 
  plot_annotation('How good do you think you would be as a _________', 
                  caption = "Monitoring the Future 12th Grade Surveys (1984-2023)") +
  plot_layout(guides = "collect", heights = c(2,2,2,.5))

p2

## Save Fig 2
agg_tiff(filename = file.path(here(outDir, figDir), "fig2.tif"), 
         width=6.5, height=10, units="in", res = 800, scaling = 1)

plot(p2)
invisible(dev.off())


# Table 4 ----------------------------------------------------------------------

polr3.sp.ED  <- polr(gdsp ~ year.c * momed + I(year.c^2) +
                       sex + race + region,
                     data = data, weights = svyweight, Hess = T)

polr3.pa.ED  <- polr(gdpa ~ year.c * momed + I(year.c^2) + 
                       sex + race + region,
                     data = data, weights = svyweight, Hess = T)

polr3.wk.ED  <- polr(gdwk ~ year.c * momed + I(year.c^2) + 
                       sex + race + region,
                     data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidySP.4 <- broom::tidy(polr3.sp.ED)
tidyPA.4 <- broom::tidy(polr3.pa.ED)
tidyWK.4 <- broom::tidy(polr3.wk.ED)

## Transform output
tidySP.4 <- tidySP.4 %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidyPA.4 <- tidyPA.4 %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidyWK.4 <- tidyWK.4 %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

## Turn into modelsummary objects
modSP.4        <- list(tidy = tidySP.4)
class(modSP.4) <- "modelsummary_list"

modPA.4        <- list(tidy = tidyPA.4)
class(modPA.4) <- "modelsummary_list"

modWK.4        <- list(tidy = tidyWK.4)
class(modWK.4) <- "modelsummary_list"

mods.4 <- list(
  "Spouse" = modSP.4,
  "Parent" = modPA.4,
  "Worker" = modWK.4)

cm <- c('year.c'                             = 'Year',
        'I(year.c^2)'                        = 'Year squared',
        'sexWomen'                           = 'Women',
        'momedCompleted college'             = 'Mom BA or more',
        'year.c:momedCompleted college'      = "Year * Mom completed BA degree",
        'raceBlack'                          = "Black",
        'raceAnother race'                   = "Another race",
        'regionMidwest'                      = "Midwest",
        'regionSouth'                        = "South",
        'regionWest'                         = 'West',
        'Poor|Not so good'                   = 'Poor|Not so good',
        'Not so good|Fairly good'            = 'Not so good|Fairly good',
        'Fairly good|Good'                   = 'Fairly good|Good',
        'Good|Very good'                     = 'Good|Very good')


table4 <- modelsummary(mods.4,
                       shape = term ~ model + statistic,
                       stars = c("*" =.05, "**" = .01, "***" = .001),
                       coef_map = cm,
                       fmt = fmt_decimal(digits = 3, pdigits = 3),
                       output = "huxtable") %>%
  huxtable::as_flextable()  %>%
  add_footer_lines("Notes: N=61,404.")

table4

read_docx() %>% 
  body_add_par(paste("Table 4. Year & Mom edu interactions", sep="")) %>% 
  body_add_flextable(value = table4)          %>% 
  print(target = file.path(outDir, "PS_table04.docx"))                 

# Figure 3 ---------------------------------------------------------------------

## Average Predictions 
pp_sp_ED   <- predict_response(polr3.sp.ED, terms = c("year.c [all]", "momed"))
pp_pa_ED   <- predict_response(polr3.pa.ED, terms = c("year.c [all]", "momed"))
pp_wk_ED   <- predict_response(polr3.wk.ED, terms = c("year.c [all]", "momed"))

## Combine dfs
pp_sp_ED$cat    <- "Spouse" 
pp_pa_ED$cat    <- "Parent" 
pp_wk_ED$cat    <- "Worker" 

df_pp_ED <- rbind(pp_wk_ED, pp_sp_ED, pp_pa_ED)

## Tidy variables
df_pp_ED$response.level <- factor(df_pp_ED$response.level, 
                                  levels=c("Very good", 
                                           "Good", 
                                           "Fairly good", 
                                           "Not so good", 
                                           "Poor"))

df_pp_ED$cat <- factor(df_pp_ED$cat, 
                       levels=c("Spouse", 
                                "Parent",
                                "Worker"))

lables_year <- c("1984", "1995", "2005", "2015", "2023")

### spouse
p3_very_sp <- df_pp_ED %>%
  filter(cat == "Spouse" & response.level == "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    plot.title = element_text(face = "bold"),
    legend.position  = "none",
    legend.title     = element_blank(),
    #    strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.60, 8.40, 18.40, 26.40), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "spouse")

p3_very_sp

p3_other_sp <- df_pp_ED %>%
  filter(cat == "Spouse" & response.level != "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = .75) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position  = "none",
    legend.title     = element_blank(),
    #    strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5), limits=c(0, .5), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.60, 8.40, 18.40, 26.40), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = " ")

p3_other_sp

## Combine plots
p3.sp <- (p3_very_sp | p3_other_sp) + plot_layout(widths = c(1, 2)) 

p3.sp


### parent
p3_very_pa <- df_pp_ED %>%
  filter(cat == "Parent" & response.level == "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    plot.title = element_text(face = "bold"),
    legend.position  = "none",
    legend.title     = element_blank(),
    #strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.60, 8.40, 18.40, 26.40), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "parent")

p3_very_pa

p3_other_pa <- df_pp_ED %>%
  filter(cat == "Parent" & response.level != "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = .75) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position  = "none",
    legend.title     = element_blank(),
    #strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5), limits=c(0, .5), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.60, 8.40, 18.40, 26.40), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = " ")

p3_other_pa

## Combine plots
p3.pa <- (p3_very_pa | p3_other_pa) + plot_layout(widths = c(1, 2)) 

p3.pa


### worker
p3_very_wk <- df_pp_ED %>%
  filter(cat == "Worker" & response.level == "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    plot.title = element_text(face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    #strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.60, 8.40, 18.40, 26.40), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "worker")

p3_very_wk

p3_other_wk <- df_pp_ED %>%
  filter(cat == "Worker" & response.level != "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = .75) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position  = "none",
    legend.title     = element_blank(),
    #strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5), limits=c(0, .5), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.6, -1.60, 8.40, 18.40, 26.40), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = " ")

p3_other_wk

## Combine plots
p3.wk <- (p3_very_wk | p3_other_wk) + plot_layout(widths = c(1, 2)) 

p3.wk

### combine all selves figures

p3 <- (p3.sp / p3.pa / p3.wk/ guide_area()) + 
  plot_annotation('How good do you think you would be as a _________', 
                  caption = "Monitoring the Future 12th Grade Surveys (1984-2023)") +
  plot_layout(guides = "collect", heights = c(2,2,2,.5))

p3

## Save Fig 3
agg_tiff(filename = file.path(here(outDir, figDir), "fig3.tif"), 
         width=6.5, height=10, units="in", res = 800, scaling = 1)

plot(p3)
invisible(dev.off())



# BAR GRAPHS -------------------------------------------------------------------
### spouse
p2_very_sp <- df_pp_sex %>%
  filter(cat == "Spouse" & response.level == "Very good") %>%
  filter(x == "-12.55" | x == "8.45" | x == "26.45") %>%
  ggplot(aes(x = as_factor(x), y = predicted, fill = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_col(width = 0.75, position = position_dodge(0.9)) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    plot.title = element_text(face = "bold"),
    legend.position  = "none",
    legend.title     = element_blank(),
    #    strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_discrete(labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "spouse")


p2_other_sp <- df_pp_sex %>%
  filter(cat == "Spouse" & response.level != "Very good") %>%
  filter(x == "-12.55" | x == "8.45" | x == "26.45") %>%
  ggplot(aes(x = as_factor(x), y = predicted, fill = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_col(width = 0.5, position = position_dodge(0.6)) +
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
  scale_x_discrete(labels = lables_year) +
  labs( x        = " ", 
        y        = " ")

## Combine plots
p2.sp <- (p2_very_sp | p2_other_sp) + plot_layout(widths = c(1, 2)) 

p2.sp

### parent
p2_very_pa <- df_pp_sex %>%
  filter(cat == "Parent" & response.level == "Very good") %>%
  filter(x == "-12.55" | x == "8.45" | x == "26.45") %>%
  ggplot(aes(x = as_factor(x), y = predicted, fill = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_col(width = 0.75, position = position_dodge(0.9)) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    plot.title = element_text(face = "bold"),
    legend.position  = "none",
    legend.title     = element_blank(),
    #    strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_discrete(labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "parent")


p2_other_pa <- df_pp_sex %>%
  filter(cat == "Parent" & response.level != "Very good") %>%
  filter(x == "-12.55" | x == "8.45" | x == "26.45") %>%
  ggplot(aes(x = as_factor(x), y = predicted, fill = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_col(width = 0.5, position = position_dodge(0.6)) +
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
  scale_x_discrete(labels = lables_year) +
  labs( x        = " ", 
        y        = " ")

## Combine plots
p2.pa <- (p2_very_pa | p2_other_pa) + plot_layout(widths = c(1, 2)) 

p2.pa

### worker
p2_very_wk <- df_pp_sex %>%
  filter(cat == "Worker" & response.level == "Very good") %>%
  filter(x == "-12.55" | x == "8.45" | x == "26.45") %>%
  ggplot(aes(x = as_factor(x), y = predicted, fill = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_col(width = 0.75, position = position_dodge(0.9)) +
  facet_wrap(~response.level) +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    plot.title = element_text(face = "bold"),
    legend.position  = "none",
    legend.title     = element_blank(),
    #    strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_discrete(labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "worker")


p2_other_wk <- df_pp_sex %>%
  filter(cat == "Worker" & response.level != "Very good") %>%
  filter(x == "-12.55" | x == "8.45" | x == "26.45") %>%
  ggplot(aes(x = as_factor(x), y = predicted, fill = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_col(width = 0.5, position = position_dodge(0.6)) +
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
  scale_x_discrete(labels = lables_year) +
  labs( x        = " ", 
        y        = " ")

## Combine plots
p2.wk <- (p2_very_wk | p2_other_wk) + plot_layout(widths = c(1, 2)) 

p2.wk

### combine all selves figures

p2 <- (p2.sp / p2.pa / p2.wk/ guide_area()) + 
  plot_annotation('How good do you think you would be as a _________', 
                  caption = "Monitoring the Future 12th Grade Surveys (1984-2023)") +
  plot_layout(guides = "collect", heights = c(2,2,2,.5))

p2

## Save Fig 2
agg_tiff(filename = file.path(here(outDir, figDir), "fig2_bar.tif"), 
         width=6.5, height=10, units="in", res = 800, scaling = 1)

plot(p2)
invisible(dev.off())

