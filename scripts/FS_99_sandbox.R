# MTF POSSIBLE SELVES PROJECT

# Proportions per level --------------------------------------------------------
prop_sp <- mtf_svy %>%
  group_by(decade, gdsp) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% 
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

prop_pa <- mtf_svy %>%
  group_by(decade, gdpa) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% 
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

prop_wk <- mtf_svy %>%
  group_by(decade, gdwk) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% 
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

#### Combine dfs
prop_sp$cat    <- "Spouse" 
prop_pa$cat    <- "Parent" 
prop_wk$cat    <- "Worker" 

colnames(prop_sp)[colnames(prop_sp)=="gdsp"] <- "good"
colnames(prop_pa)[colnames(prop_pa)=="gdpa"] <- "good"
colnames(prop_wk)[colnames(prop_wk)=="gdwk"] <- "good"

df_prop <- rbind(prop_sp, prop_pa, prop_wk)

p00 <- df_prop %>%
  ggplot(aes(y = decade, x = vals, fill = cat, 
             ymin = vals_low, ymax = vals_upp)) +
  geom_col(position = position_dodge()) +
  facet_grid(vars(good), vars(fct_rev(cat))) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  # stat_smooth(method = "lm", size = .5, fill = "grey80") +
  scale_y_discrete(limits = rev) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "How good do you think you would be as a _________",
        subtitle = "annual proportions",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)") 
p00

ggsave(file.path(here(outDir, figDir),"proportions.png"), p00, 
       width = 6.5, height = 8.5, dpi = 300, bg = 'white')



## Spouse
mod.sp      <- polr(gdsp ~ year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.sp.ha   <- polr(gdsp ~ happy_N_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.sp.ls   <- polr(gdsp ~ lifesat_N_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.sp.sc   <- polr(gdsp ~ selfconcept_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.sp.all  <- polr(gdsp ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                    year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)

## Parent
mod.pa      <- polr(gdpa ~ year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.pa.ha   <- polr(gdpa ~ happy_N_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.pa.ls   <- polr(gdpa ~ lifesat_N_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.pa.sc   <- polr(gdpa ~ selfconcept_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.pa.all  <- polr(gdpa ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                    year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)

## Worker
mod.wk      <- polr(gdwk ~ year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.wk.ha   <- polr(gdwk ~ happy_N_std + year + sex + 
                      race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.wk.ls   <- polr(gdwk ~ lifesat_N_std + year + sex + 
                      race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.wk.sc   <- polr(gdwk ~ selfconcept_std + year + sex + 
                      race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.wk.all  <- polr(gdwk ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                      year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)

broom::tidy(mod.sp.all)
m1 <- broom::tidy(mod.wk.all)
m2 <- broom::tidy(mod.sp.all)
m3 <- broom::tidy(mod.pa.all)

mods <- list(
  Worker = m1,
  Spouse = m2,
  Parent = m3)

class(mods) <- "modelsummary_list"

modelsummary(mods)


# this is the key step to use tidy data with modelsummary()
# need tidy day to exponentiate results (but not the cuts)
mod <- list(
  tidy = m1)
class(mod) <- "modelsummary_list"

modelsummary(mod)

# Combine the models
combined_results <- bind_rows(
  m1 %>% mutate(Model = "Worker"),
  m2 %>% mutate(Model = "Spouse"),
  m3 %>% mutate(Model = "Parent"))





# Get average predictions for specific predictor values
newdata <- data.frame(
  year = c(1984, 1995, 2022),
  selfconcept_std = c(-3.69, 0, 1.15))

predictions <- avg_predictions(mod.sp.all, 
                               variables = list(year = c(1992, 2002, 2012, 2022),
                               selfconcept_std = c(-3.69, 0, 1.15))) %>%
  mutate(selfconcept_std = fct_case_when(
    selfconcept_std == 1.15  ~ "Maximum",
    selfconcept_std == 0     ~ "Average",
    selfconcept_std == -3.69 ~ "Minimum"))

         
p7 <- predictions %>%
  ggplot(aes(x = as_factor(year) , y = estimate, 
             ymin = conf.low, ymax = conf.high, 
             fill = group )) +
  geom_col() +
  facet_wrap(vars(selfconcept_std)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = "Spouse",
        title    = "Predicted proportions by self-concept scale",
        subtitle = "for key years",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)")

p7 

ggsave(file.path(here(outDir, figDir),"pp_spouse.png"), p7, 
       width = 6.5, height = 6.5, dpi = 300, bg = 'white')





models <- list(
  "Worker"     = polr(gdwk ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                        year + sex + race + momed + religion + famstru + region, 
                      data = data, weights = svyweight, Hess = T),
  "Spouse"     = polr(gdsp ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                        year + sex + race + momed + religion + famstru + region, 
                      data = data, weights = svyweight, Hess = T),
  "Parent"     = polr(gdpa ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                        year + sex + race + momed + religion + famstru + region, 
                      data = data, weights = svyweight, Hess = T))

summary_table_wk <- coef(summary(models[[1]]))
pval <- pnorm(abs(summary_table_wk[, "t value"]),lower.tail = FALSE)* 2
summary_table_wk <- cbind(summary_table_wk, "p value" = round(pval,3))
summary_table_wk


rows <- tribble(~term,          ~Worker,  ~Spouse, ~Parent,
                'Religiosity (ref = Never)', ' ',   ' ', ' ',
                'Family structure (ref = Both mother/father)', ' ', ' ', ' ',
                'Region (ref = Northeast)', ' ', ' ', ' ')
attr(rows, 'position') <- c(23, 30, 37)

tab2  <- modelsummary(models,
             gof_map = NA, stars = c("*" =.05, "**" = .01, "***" = .001),
             fmt = fmt_decimal(digits = 2, pdigits = 3),
             output = "huxtable", add_rows = rows,
             coef_rename = c("happy_N_std"      = "Happiness", 
                             "lifesat_N_std"    = "Life satisfaction",
                             "selfconcept_std"  = "Self-concept",
                             "year"             = "Year",
                             "sexWomen"         = "Women",
                             "raceBlack"        = "Black",
                             "momedCompleted college" = "Mom completed college",
                             "religionRarely"  = "Rarely",
                             "religionOnce or twice a month" = "1x - 2x a month",
                             "religionAbout once a week or more" = "1x + a week",
                             "famstruMother Only" = "Mother only",
                             "famstruFather Only" = "Father only",
                             "famstruNeither Mother/Father" = "Neither mother/father",
                             "regionMidwest"      = "Midwest",
                             "regionSouth"        = "South",
                             "regionWest"         = "West"))           %>%
  huxtable::as_flextable()                                             %>%
  add_footer_lines("Notes: N=50,200. Standard errors in parentheses.")

tab2

read_docx() %>% 
  body_add_par(paste("Table 2. Ordinal logistic regressions", sep="")) %>% 
  body_add_flextable(value = tab2)                                     %>% 
  print(target = file.path(outDir, "PS_table02.docx"))                 

avg.sp      <- avg_predictions(mod.sp, variables =  c("year"))
avg.pa      <- avg_predictions(mod.pa, variables =  c("year"))
avg.wk      <- avg_predictions(mod.wk, variables =  c("year"))
avg.sp.ha   <- avg_predictions(mod.sp.ha, variables =  c("year"))
avg.sp.ls   <- avg_predictions(mod.sp.ls, variables =  c("year"))
avg.sp.sc   <- avg_predictions(mod.sp.sc, variables =  c("year"))
avg.pa.sc   <- avg_predictions(mod.pa.sc, variables =  c("year"))
avg.wk.sc   <- avg_predictions(mod.wk.sc, variables =  c("year"))

avg.sp.ha      <- avg_predictions(mod.sp.all, variables =  c("happy_N_std"), 
                                   newdata = datagrid(year = c(1984, 2022)))
avg.sp.ls      <- avg_predictions(mod.sp.all, variables =  c("lifesat_N_std"), 
                                  newdata = datagrid(year = c(1984, 2022)))
avg.sp.sc      <- avg_predictions(mod.sp.all, variables =  c("selfconcept_std"), 
                                  newdata = datagrid(year = c(1984, 2022)))



 

avg.pa.all      <- avg_predictions(mod.pa.all, variables =  c("year"))
avg.wk.all      <- avg_predictions(mod.wk.all, variables =  c("year"))

avg.sp$cat        <- "Spouse"
avg.pa$cat        <- "Parent" 
avg.wk$cat        <- "Worker" 
avg.sp.ha$cat     <- "Spouse" 
avg.sp.ls$cat     <- "Spouse" 
avg.sp.sc$cat     <- "Spouse" 
avg.pa.sc$cat     <- "Parent" 
avg.wk.sc$cat     <- "Worker" 

avg.sp$index      <- "None" 
avg.pa$index      <- "None" 
avg.wk$index      <- "None" 
avg.sp.ha$index   <- "Happiness" 
avg.sp.ls$index   <- "Life satisfaction" 
avg.sp.sc$index   <- "Self-concept" 
avg.pa.sc$index   <- "Self-concept" 
avg.wk.sc$index   <- "Self-concept" 

avg_all <- rbind(avg.sp, avg.pa, avg.wk, avg.sp.ha, avg.sp.ls, avg.sp.sc, avg.pa.sc, avg.wk.sc)


avg_all %>%
  filter(index == "None" | index == "Self-concept") %>%
  ggplot(aes(x = year, y = estimate, color = group, ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(cat ~ index) +
  theme(panel.grid.minor = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends in expected work-family roles ",
        subtitle = "controlling for self-concept ",
        caption  = "Monitoring the Future 12th Grade Surveys (1984-2022)")


avg.sp.sc %>%
  ggplot(aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, color = group, fill = group)) +
  geom_line(linewidth = 1, na.rm=TRUE)



# year * gender
avg.sc.sp   <- avg_predictions(mod.sp.sc, variables =  c("year", "sex"))

avg.sc.sp %>%
  ggplot(aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, color = group, fill = group)) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_wrap(vars(sex))

# ------------------------------------------------------------------------------
## PS Trends -------------------------------------------------------------------
# Averages
avg_sp <- mtf_svy %>%
  select(year, gdsp_num) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdsp_num, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_pa <- mtf_svy %>%
  select(year, gdpa_num) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdpa_num, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_wk <- mtf_svy %>%
  select(year, gdwk_num) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdwk_num, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
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
  geom_smooth(alpha = .2, method="loess", se=TRUE, fullrange=FALSE, level=0.95) +
  geom_point(alpha = .5) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    panel.grid.minor = element_blank()) +
  guides(color = guide_legend(reverse=TRUE)) +
  ylim(3.5, 5) +
  labs( x        = " ", 
        y        = " ", 
        color    = " ",
        title    = "How good do you think you would be as a _________",
        subtitle = "Scale: (1) Poor - (5) Very good",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2023)
        Note: Survey years 2020, 2021, & 2022 were combined due to small sample sizes.")

p1 

ggsave(file.path(here(outDir, figDir),"PS_fig01.png"), p1, 
       width = 6, height = 5.5, dpi = 300, bg = 'white')




## MH Descriptives -------------------------------------------------------------

data_long <- data %>%
  pivot_longer(
    cols = c(happy, lifesat, 
             posatt, worth, welloth, satself,
             proud, nogood, wrong, lifeuse,
             meaning, enjoy, hopeless, alive, anxiety),
    names_to = "attribute",
    values_to = "response") %>%
  mutate(scale = case_when(
    response == "Completely dissatisfied"    | response == "Disagree"        | response == "Not too happy"  ~ 1,
    response == "Quite dissatisfied"         | response == "Mostly disagree" | response == "Pretty happy"   ~ 2,
    response == "Somewhat dissatisfied"      | response == "Neither"         | response == "Very happy"     ~ 3,
    response == "Neither, or mixed feelings" | response == "Mostly agree"                                   ~ 4,
    response == "Somewhat satisfied"         | response == "Agree"                                          ~ 5,
    response == "Quite satisfied"                                                                           ~ 6,
    response == "Completely satisfied"                                                                      ~ 7,
    TRUE                                                                                                    ~ NA_integer_)) %>%
  group_by(attribute) %>%
  mutate_at(c('scale'), ~(scale(.) %>% as.vector)) %>%
  ungroup()

table(data_long$response, data_long$attribute)

mtf_svy_l <- data_long %>%
  # weight data
  as_survey_design(id = 1,
                   weights = svyweight)

avg_mh <- mtf_svy_l %>%
  select(year, attribute, scale) %>%
  group_by(year, attribute) %>%
  summarize(vals  = survey_mean(scale, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2)) %>%
  mutate(index = fct_case_when(
    attribute == "happy"                                                        ~ "Happiness",
    attribute == "lifesat"                                                      ~ "Life satisfaction",
    attribute == "posatt"   | attribute == "worth"  | attribute == "welloth"  | 
      attribute == "satself"                                                    ~ "Self-esteem",
    attribute == "proud"    | attribute == "nogood" | attribute == "wrong"    | 
      attribute == "lifeuse"                                                    ~ "Self-derogation",
    attribute == "meaning"  | attribute == "enjoy"  | attribute == "hopeless" | 
      attribute == "alive"                                                      ~ "Depression",
    attribute == "anxiety"                                                      ~ "Anxiety",
    TRUE                                                                        ~  NA_character_)) 

p3 <- avg_mh %>%  
  filter(index != "Anxiety" & index != "Depression") %>%
  filter(vals != 0.00 & vals_low != 0.00 & vals_upp != 0.00) %>%
  ggplot(aes(x = year, y = vals, color = attribute, ymin = vals_low, ymax = vals_upp)) +
  #  geom_ribbon(fill = "lightgrey", linetype = "dotted", alpha=0.1) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(. ~ index) +
  theme(panel.grid.minor = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends",
        subtitle = " ",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)")

p3

ggsave(file.path(here(outDir, figDir),"PS-trends.png"), p3, 
       width = 8.5, height = 6.5, dpi = 300, bg = 'white')

## Combined scale trends -------------------------------------------------------

# Averages
avg_esteem <- mtf_svy_l %>%
  select(year, esteem_std) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(esteem_std, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_derogation <- mtf_svy_l %>%
  select(year, derogation_std) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(derogation_std, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_selfconcept <- mtf_svy_l %>%
  select(year, selfconcept_std) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(selfconcept_std, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

#### Combine dfs
avg_esteem$index          <- "Self-esteem" 
avg_derogation$index      <- "Self-derogation" 
avg_selfconcept$index     <- "Self-concept" 

avg_esteem$attribute      <- "AVERAGE"
avg_derogation$attribute  <- "AVERAGE"
avg_selfconcept$attribute <- "AVERAGE"

df_scale_avg <- rbind(avg_mh, avg_esteem, avg_derogation, avg_selfconcept)

index_names <- c(
  `Anxiety`         = "Anxiety",
  `Depression`      = "Depression",
  `Self-derogation` = "Self-derogation",
  `Self-esteem`     = "Self-esteem",
  `Well-being`      = "Happiness",
  `Self-concept`    = "Self-concept")

p4 <- df_scale_avg %>%  
  filter((attribute == "AVERAGE" | attribute == "happy" | attribute == "lifesat") & index != "Self-concept") %>%
  filter(vals != 0.00 & vals_low != 0.00 & vals_upp != 0.00) %>%
  ggplot(aes(x = year, y = vals, color = index, ymin = vals_low, ymax = vals_upp)) +
  #  geom_ribbon(fill = "lightgrey", linetype = "dotted", alpha=0.1) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(. ~ index) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends",
        subtitle = " ",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)")

p4 

ggsave(file.path(here(outDir, figDir),"scale_averages.png"), p4, 
       width = 8.5, height = 6.5, dpi = 300, bg = 'white')


p5 <- df_scale_avg %>%  
  filter(attribute == "happy" | attribute == "lifesat" | index == "Self-concept") %>%
  filter(vals != 0.00 & vals_low != 0.00 & vals_upp != 0.00) %>%
  ggplot(aes(x = year, y = vals, color = index, ymin = vals_low, ymax = vals_upp)) +
  #  geom_ribbon(fill = "lightgrey", linetype = "dotted", alpha=0.1) +
  #  geom_smooth(alpha = .1, method="loess", se=TRUE, fullrange=FALSE, level=0.95) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(. ~ index) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(1984, 2025), breaks = c(1985, 1995, 2005, 2015, 2025)) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends in happiness, life satisfaction, & self-concept (esteem + derogation)",
        subtitle = " ",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2023)")
p5 

ggsave(file.path(here(outDir, figDir),"scale_averages2.png"), p5, 
       width = 8.5, height = 6.5, dpi = 300, bg = 'white')


## REGRESSIONS -----------------------------------------------------------------

write.dta(data, file = file.path(outDir, "data.dta"))

data$gdsp   <- relevel(data$gdsp,   ref = "Very good")
data$gdpa   <- relevel(data$gdpa,   ref = "Very good")
data$gdwk   <- relevel(data$gdwk,   ref = "Very good")

## Happiness
mod.ha.sp   <- glm(happy_N_std ~ gdsp * year, data = data, weights=svyweight)
mod.ha.pa   <- glm(happy_N_std ~ gdpa * year, data = data, weights=svyweight)
mod.ha.wk   <- glm(happy_N_std ~ gdwk * year, data = data, weights=svyweight)

avg.ha.sp   <- avg_predictions(mod.ha.sp, by = c("year", "gdsp"))
avg.ha.pa   <- avg_predictions(mod.ha.pa, by = c("year", "gdpa"))
avg.ha.wk   <- avg_predictions(mod.ha.wk, by = c("year", "gdwk"))

## Life satisfaction
mod.ls.sp   <- glm(lifesat_N_std ~ gdsp * year, data = data, weights=svyweight)
mod.ls.pa   <- glm(lifesat_N_std ~ gdpa * year, data = data, weights=svyweight)
mod.ls.wk   <- glm(lifesat_N_std ~ gdwk * year, data = data, weights=svyweight)

avg.ls.sp   <- avg_predictions(mod.ls.sp, by = c("year", "gdsp"))
avg.ls.pa   <- avg_predictions(mod.ls.pa, by = c("year", "gdpa"))
avg.ls.wk   <- avg_predictions(mod.ls.wk, by = c("year", "gdwk"))

## Self-concept
mod.sc.sp   <- glm(selfconcept_std ~ gdsp * year, data = data, weights=svyweight)
mod.sc.pa   <- glm(selfconcept_std ~ gdpa * year, data = data, weights=svyweight)
mod.sc.wk   <- glm(selfconcept_std ~ gdwk * year, data = data, weights=svyweight)

avg.sc.sp   <- avg_predictions(mod.sc.sp, by = c("year", "gdsp"))
avg.sc.pa   <- avg_predictions(mod.sc.pa, by = c("year", "gdpa"))
avg.sc.wk   <- avg_predictions(mod.sc.wk, by = c("year", "gdwk"))

# Matching and identifying columns
colnames(avg.ha.sp)[colnames(avg.ha.sp)=="gdsp"] <- "good"
colnames(avg.ha.pa)[colnames(avg.ha.pa)=="gdpa"] <- "good"
colnames(avg.ha.wk)[colnames(avg.ha.wk)=="gdwk"] <- "good"

colnames(avg.ls.sp)[colnames(avg.ls.sp)=="gdsp"] <- "good"
colnames(avg.ls.pa)[colnames(avg.ls.pa)=="gdpa"] <- "good"
colnames(avg.ls.wk)[colnames(avg.ls.wk)=="gdwk"] <- "good"

colnames(avg.sc.sp)[colnames(avg.sc.sp)=="gdsp"] <- "good"
colnames(avg.sc.pa)[colnames(avg.sc.pa)=="gdpa"] <- "good"
colnames(avg.sc.wk)[colnames(avg.sc.wk)=="gdwk"] <- "good"

avg.ha.sp$cat     <- "Spouse" 
avg.ha.pa$cat     <- "Parent" 
avg.ha.wk$cat     <- "Worker" 
avg.ls.sp$cat     <- "Spouse" 
avg.ls.pa$cat     <- "Parent" 
avg.ls.wk$cat     <- "Worker" 
avg.sc.sp$cat     <- "Spouse" 
avg.sc.pa$cat     <- "Parent" 
avg.sc.wk$cat     <- "Worker" 

avg.ha.sp$index   <- "Happiness" 
avg.ha.pa$index   <- "Happiness" 
avg.ha.wk$index   <- "Happiness" 
avg.ls.sp$index   <- "Life satisfaction" 
avg.ls.pa$index   <- "Life satisfaction" 
avg.ls.wk$index   <- "Life satisfaction" 
avg.sc.sp$index   <- "Self-concept" 
avg.sc.pa$index   <- "Self-concept" 
avg.sc.wk$index   <- "Self-concept" 

avg_all <- rbind(avg.ha.sp, avg.ha.pa, avg.ha.wk, 
                 avg.ls.sp, avg.ls.pa, avg.ls.wk,
                 avg.sc.sp, avg.sc.pa, avg.sc.wk)

avg_all$good <- factor(avg_all$good, 
                       levels=c('Very good', 'Good', 'Fairly good', 
                                'Not so good', 'Poor'))

p6 <- avg_all %>%
  ggplot(aes(x = year, y = estimate, color = cat, ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(good ~ index) +
  theme(panel.grid.minor = element_blank()) +
  ylim(-1.5, 0.5) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends in happiness, life satisfaction, & self-concept",
        subtitle = "by expected work-family roles",
        caption  = "Monitoring the Future 12th Grade Surveys (1984-2022)")

p6

ggsave(file.path(here(outDir, figDir),"predicted trends.png"), p6, 
       width = 8.5, height = 6.5, dpi = 300, bg = 'white')


