  
p_getmar <- mtf_svy %>%
  filter(!is.na(getmar)) %>%
  group_by(year, getmar) %>%
  summarize(vals  = survey_mean(vartype = "ci"))

p_mar3 <- mtf_svy %>%
  filter(!is.na(mar3)) %>%
  group_by(year, mar3) %>%
  summarize(vals  = survey_mean(vartype = "ci"))

p_mardum <- mtf_svy %>%
  filter(!is.na(mardum)) %>%
  group_by(year, mardum) %>%
  summarize(vals  = survey_mean(vartype = "ci"))

p_mardum_sex <- mtf_svy %>%
  filter(!is.na(mardum)) %>%
  group_by(year, sex, mardum) %>%
  summarize(vals  = survey_mean(vartype = "ci"))

p_markids <- mtf_svy %>%
  filter(!is.na(markids)) %>%
  group_by(year, markids) %>%
  summarize(vals  = survey_mean(vartype = "ci"))

p_kids3 <- mtf_svy %>%
  filter(!is.na(kids3)) %>%
  group_by(year, kids3) %>%
  summarize(vals  = survey_mean(vartype = "ci"))

p_kidsdum <- mtf_svy %>%
  filter(!is.na(kidsdum)) %>%
  group_by(year, kidsdum) %>%
  summarize(vals  = survey_mean(vartype = "ci"))

p_kidsdum_sex <- mtf_svy %>%
  filter(!is.na(kidsdum)) %>%
  group_by(year, sex, kidsdum) %>%
  summarize(vals  = survey_mean(vartype = "ci"))

#### Combine dfs
p_getmar$cat   <- "getmar" 
p_mar3$cat     <- "mar3" 
p_mardum$cat   <- "mardum" 
p_markids$cat  <- "markids" 
p_kids3$cat    <- "kids3" 
p_kidsdum$cat  <- "kidsdum" 

p_mardum_sex$cat   <- "mardum" 
p_kidsdum_sex$cat  <- "kidsdum" 

colnames(p_getmar)[colnames(p_getmar)=="getmar"]    <- "response"
colnames(p_mar3)[colnames(p_mar3)=="mar3"]          <- "response"
colnames(p_mardum)[colnames(p_mardum)=="mardum"]    <- "response"
colnames(p_markids)[colnames(p_markids)=="markids"] <- "response"
colnames(p_kids3)[colnames(p_kids3)=="kids3"]       <- "response"
colnames(p_kidsdum)[colnames(p_kidsdum)=="kidsdum"] <- "response"

colnames(p_mardum_sex)[colnames(p_mardum_sex)=="mardum"]    <- "response"
colnames(p_kidsdum_sex)[colnames(p_kidsdum_sex)=="kidsdum"] <- "response"

df_prop <- rbind(p_getmar, p_mar3, p_mardum, p_markids, p_kids3, p_kidsdum)

df_prop_sex <- rbind(p_mardum_sex, p_kidsdum_sex)



par1 <- df_prop %>%
  filter(cat == "markids") %>%
  ggplot(aes(x = year, y = vals, ymin = vals_low, ymax = vals_upp,
             colour = response, fill == response)) +
  geom_line(linewidth = .7) +
  geom_errorbar(alpha = .3) +
  geom_point() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1)) +
  scale_x_continuous(limit  = c(1976, 2025)) +
  labs(x        = NULL, 
       y        = NULL,
       color    = "Response level")


par2 <- df_prop %>%
  filter(cat == "kids3") %>%
  ggplot(aes(x = year, y = vals, ymin = vals_low, ymax = vals_upp,
             colour = response, fill == response)) +
  geom_line(linewidth = .7) +
  geom_errorbar(alpha = .3) +
  geom_point() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1)) +
  scale_x_continuous(limit  = c(1976, 2025)) +
  scale_colour_discrete(breaks=c('Unlikely/Uncertain', 'Fairly likely', 'Very likely')) +
  labs(x        = NULL, 
       y        = NULL,
       color    = "Response level")

## Combine plots
p_kids <- (par1 | par2) + plot_layout(widths = c(1, 1)) +
  plot_annotation('If you did get married (or are married)...how likely is it that you would want to have children?', 
                  caption = "Monitoring the Future 12th Grade Surveys (1976-2023)")

p_kids

df_prop %>%
  filter((cat == "mardum" |cat == "kidsdum") & response == 1) %>%
  ggplot(aes(x = year, y = vals, ymin = vals_low, ymax = vals_upp,
             colour = cat, fill == cat)) +
  geom_line(linewidth = .7) +
  geom_errorbar(alpha = .3) +
  geom_point() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1)) +
  scale_x_continuous(limit  = c(1976, 2025)) +
  scale_color_manual(breaks = c("mardum","kidsdum"),
                     labels = c("get married", "if married, have children"), 
                     values = c("#18BC9C", "#F39C12")) +
  theme(legend.position = "top") +
  labs(
    title    = "% of U.S. high school seniors who think they will _________",
    x        = NULL,
    y        = NULL,
    color    = NULL,
    caption  = "Monitoring the Future 12th Grade Surveys (1976-2023)")


# by gender
df_prop_sex %>%
  filter((cat == "mardum" |cat == "kidsdum") & response == 1) %>%
  ggplot(aes(x = year, y = vals, ymin = vals_low, ymax = vals_upp,
             colour = cat)) +
  geom_line(aes(linetype = sex), linewidth = .7) +
  geom_errorbar(alpha = .2) +
  geom_point() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1)) +
  scale_x_continuous(limit  = c(1976, 2025)) +
  scale_color_manual(breaks = c("mardum","kidsdum"),
                     labels = c("get married", "if married, have children"), 
                     values = c("#18BC9C", "#F39C12")) +
  theme(legend.position = "top") +
  labs(
    title    = "% of U.S. high school seniors who think they will _________",
    x        = NULL,
    y        = NULL,
    color    = NULL,
    caption  = "Monitoring the Future 12th Grade Surveys (1976-2023)")


# Table 4 ----------------------------------------------------------------------

# refactor reference level
data$mar3  <- relevel(data$mar3,  ref = 1)
data$kids3 <- relevel(data$kids3, ref = 3)

## Models with expectations as controls
polr2.sp.exp  <- polr(gdsp ~ year.c * sex + I(year.c^2) * sex + mar3 +
                       momed + race + region,
                     data = data, weights = svyweight, Hess = T)

polr2.pa.exp  <- polr(gdpa ~ year.c * sex + I(year.c^2) * sex  + kids3 +
                       momed + race + region,
                     data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidySP.4 <- broom::tidy(polr2.sp.exp)
tidyPA.4 <- broom::tidy(polr2.pa.exp)

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


## Turn into modelsummary objects
modSP.4        <- list(tidy = tidySP.4)
class(modSP.4) <- "modelsummary_list"

modPA.4        <- list(tidy = tidyPA.4)
class(modPA.4) <- "modelsummary_list"

mods.4 <- list(
  "Spouse" = modSP.4,
  "Parent" = modPA.4)

cm <- c('year.c'                             = 'Year',
        'I(year.c^2)'                        = 'Year squared',
        'sexWomen'                           = "Women",
        'year.c:sexWomen'                    = 'Year * Women',
        'sexWomen:I(year.c^2)'               = 'Year2 * Women',
        'mar3Not getting married'            = 'Not getting married',
        'mar3I have no idea'                 = 'I have no idea',
        'kids3Fairly likely'                 = 'kids3Fairly likely',
        'kids3Unlikely/Uncertain'            = 'Unlikely/Uncertain',
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
  add_footer_lines("Notes: 99,399")

table4


# Table 5 ----------------------------------------------------------------------

## Now, let's subset the sample to only marital and parental expectations


## Models with expectations as controls
polr2.sp.mar  <- polr(gdsp ~ year.c * sex + I(year.c^2) * sex + 
                        momed + race + region,
                      data=subset(data, mar3=="Getting married"), weights = svyweight, Hess = T)

polr2.pa.kids <- polr(gdpa ~ year.c * sex + I(year.c^2) * sex  +
                        momed + race + region,
                      data=subset(data, kids3=="Very likely"), weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidySP.5 <- broom::tidy(polr2.sp.mar)
tidyPA.5 <- broom::tidy(polr2.pa.kids)

## Transform output
tidySP.5 <- tidySP.5 %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidyPA.5 <- tidyPA.5 %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))


## Turn into modelsummary objects
modSP.5        <- list(tidy = tidySP.5)
class(modSP.5) <- "modelsummary_list"

modPA.5        <- list(tidy = tidyPA.5)
class(modPA.5) <- "modelsummary_list"

mods.5 <- list(
  "Spouse" = modSP.5,
  "Parent" = modPA.5)

cm <- c('year.c'                             = 'Year',
        'I(year.c^2)'                        = 'Year squared',
        'sexWomen'                           = "Women",
        'year.c:sexWomen'                    = 'Year * Women',
        'sexWomen:I(year.c^2)'               = 'Year2 * Women',
        'Poor|Not so good'                   = 'Poor|Not so good',
        'Not so good|Fairly good'            = 'Not so good|Fairly good',
        'Fairly good|Good'                   = 'Fairly good|Good',
        'Good|Very good'                     = 'Good|Very good')

table5 <- modelsummary(mods.5,
                       shape = term ~ model + statistic,
                       stars = c("*" =.05, "**" = .01, "***" = .001),
                       coef_map = cm,
                       fmt = fmt_decimal(digits = 3, pdigits = 3),
                       output = "huxtable") %>%
  huxtable::as_flextable()  %>%
  add_footer_lines("Notes: 99,399")

table5


# Figure 3 ---------------------------------------------------------------------
# sub-sample will marry & have kids

## Average Predictions 
pp_sp_sex_sub   <- predict_response(polr2.sp.mar, terms = c("year.c [all]", "sex"))
pp_pa_sex_sub   <- predict_response(polr2.pa.kids, terms = c("year.c [all]", "sex"))

## Combine dfs
pp_sp_sex_sub$cat    <- "Spouse \n (think will marry)" 
pp_pa_sex_sub$cat    <- "Parent \n (if marry, very likely have kids)" 

df_pp_sex_sub <- rbind(pp_sp_sex_sub, pp_pa_sex_sub)

## Tidy variables
df_pp_sex_sub$response.level <- factor(df_pp_sex_sub$response.level, 
                                   levels=c("Very good", 
                                            "Good", 
                                            "Fairly good", 
                                            "Not so good", 
                                            "Poor"))
df_pp_sex_sub$cat <- factor(df_pp_sex_sub$cat, 
                        levels=c("Spouse \n (think will marry)", 
                                 "Parent \n (if marry, very likely have kids)"))

lables_year <- c("1976", "1985", "1995", "2005", "2015", "2023")


## Draw figure

p2_sub <- df_pp_sex_sub %>%
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
        color    = "Response level",
        linetype = " ",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2023)") 

p2_sub


## Combine plots

## Combine dfs
df_pp_sex$sample      <- "All" 
df_pp_sex_sub$sample  <- "Sub" 

df_pp_p3 <- rbind(df_pp_sex, df_pp_sex_sub)

df_pp_p3$cat <- recode_factor(df_pp_p3$cat, Spouse = "Spouse \n (full sample)")
df_pp_p3$cat <- recode_factor(df_pp_p3$cat, Parent = "Parent \n (full sample)")

df_pp_p3$cat <- factor(df_pp_p3$cat, 
                       levels=c(
                         'Spouse \n (full sample)',
                         'Spouse \n (think will marry)',
                         'Parent \n (full sample)',
                         'Parent \n (if marry, very likely have kids)'))

p3 <- df_pp_p3 %>%
  filter(cat != "Worker") %>%
  ggplot(aes(x = x, y = predicted, color = response.level,
             ymin = conf.low, ymax = conf.high)) +
  #  geom_errorbar(width = 0.2, color="grey80") +
  geom_line(aes(linetype = group), linewidth = 1) +
  facet_wrap(~cat, ncol = 4) +
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
  labs( 
    title    = "% who think they will be ' __________' as a ____________",
    subtitle = "for the full sample and only youth expecting to take on each role",
    x        = " ", 
    y        = " ",
    color    = "Response level",
    linetype = " ",
    caption  = "Monitoring the Future 12th Grade Surveys (1976-2023)") 

p3

## Save Fig 3

agg_tiff(filename = file.path(here(outDir, figDir), "fig3.tif"), 
         width=9, height=6.5, units="in", res = 800, scaling = 1)

plot(p3)
invisible(dev.off())

