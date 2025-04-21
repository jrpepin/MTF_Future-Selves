  
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


#### Combine dfs
p_getmar$cat   <- "getmar" 
p_mar3$cat     <- "mar3" 
p_mardum$cat   <- "mardum" 
p_markids$cat  <- "markids" 
p_kids3$cat    <- "kids3" 
p_kidsdum$cat  <- "kidsdum" 

colnames(p_getmar)[colnames(p_getmar)=="getmar"]    <- "response"
colnames(p_mar3)[colnames(p_mar3)=="mar3"]          <- "response"
colnames(p_mardum)[colnames(p_mardum)=="mardum"]    <- "response"
colnames(p_markids)[colnames(p_markids)=="markids"] <- "response"
colnames(p_kids3)[colnames(p_kids3)=="kids3"]       <- "response"
colnames(p_kidsdum)[colnames(p_kidsdum)=="kidsdum"] <- "response"

df_prop <- rbind(p_getmar, p_mar3, p_mardum, p_markids, p_kids3, p_kidsdum)

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



# Table 4 ----------------------------------------------------------------------

# refactor reference level
data$mar3  <- relevel(data$mar3,  ref = 1)
data$kids3 <- relevel(data$kids3, ref = 3)


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
