#-------------------------------------------------------------------------------
# MTF FUTURE SELVES PROJECT
# FS_03_RQ01.R
# Joanna R. Pepin & Melissa Milkie
#-------------------------------------------------------------------------------

# Create Table 2 ---------------------------------------------------------------

m1_sp  <- polr(gdsp ~ year_c * sex + I(year_c^2) * sex +
                 mom_ba + momwork + momdad + race,
                  data = sample, weights = svyweight, Hess = T)

m1_pa  <- polr(gdpa ~ year_c * sex + I(year_c^2) * sex +
                 mom_ba + momwork + momdad + race,
               data = sample, weights = svyweight, Hess = T)


## Turn into tidy dataframes
tidy_sp <- broom::tidy(m1_sp)
tidy_pa <- broom::tidy(m1_pa)

## Transform output
tidy_sp <- tidy_sp |>
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidy_pa <- tidy_pa |>
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

## Turn into modelsummary objects
mod1_sp        <- list(tidy = tidy_sp)
class(mod1_sp) <- "modelsummary_list"

mod1_pa        <- list(tidy = tidy_pa)
class(mod1_pa) <- "modelsummary_list"

mods_1 <- list(
  "Spouse" = mod1_sp,
  "Parent" = mod1_pa)

cm <- c('year_c'                             = 'Year',
        'I(year_c^2)'                        = 'Year squared',
        'sexWomen'                           = 'Women',
        'year_c × sexWomen'                  = 'Year * Women',
        'sexWomen × I(year_c^2)'             = 'Year squared * Women',
        'mom_baCompleted college'            = 'Mom BA or more',
        'momworkMost or all the time'        = 'Mom employed most/all the time',
        'momdadBoth Mother & Father'         = 'R living with both parents',
        'raceBlack'                          = "Black",
        'raceAnother race'                   = "Another race",
        'Poor|Not so good'                   = 'Poor|Not so good',
        'Not so good|Fairly good'            = 'Not so good|Fairly good',
        'Fairly good|Good'                   = 'Fairly good|Good',
        'Good|Very good'                     = 'Good|Very good')

tab2 <- modelsummary(mods_1,
                        shape = term ~ model + statistic,
                        stars = c("*" =.05, "**" = .01, "***" = .001),
                #        coef_map = cm,
                        fmt = fmt_decimal(digits = 3, pdigits = 3),
                        output = "huxtable") |>
  huxtable::as_flextable()

tab2

read_docx() |> 
  body_add_par(paste("Table 2.", sep="")) |> 
  body_add_flextable(value = tab2)        |> 
  print(target = file.path(outDir, "FS_table02.docx"))                 


# Create Figure 1 --------------------------------------------------------------

## Average Predictions 
pp_sp <- avg_predictions(
  m1_sp,
  newdata = datagrid(
    year_c = seq(min(sample$year_c),
                 max(sample$year_c),
                 by = 1),
    sex = c("Men", "Women")),
  by = c("year_c", "sex")
)

pp_pa <- avg_predictions(
  m1_pa,
  newdata = datagrid(
    year_c = seq(min(sample$year_c),
                 max(sample$year_c),
                 by = 1),
    sex = c("Men", "Women")),
  by = c("year_c", "sex")
)

## Combine dfs
pp_sp$cat    <- "Spouse" 
pp_pa$cat    <- "Parent" 

df_pp <- rbind(pp_sp, pp_pa)

## Tidy variables
df_pp$group <- factor(df_pp$group, 
                               levels=c("Very good", 
                                        "Good", 
                                        "Fairly good", 
                                        "Not so good", 
                                        "Poor"))

df_pp$cat <- factor(df_pp$cat, levels=c("Spouse", "Parent"))

sample |>
  filter(
    year == 1976 | year == 1992 | year == 2008  | year == 2020) |>
  distinct(year_c)

lables_year <- c("'76", "'92", "'08", "'20")

df_pp$group <- factor(df_pp$group, 
                      levels=c("Poor",
                               "Not so good",
                               "Fairly good",
                               "Good",
                               "Very good"))

data_end <- df_pp |>
  group_by(group, cat, sex) |>
  slice_max(year_c, n = 1) |>
  filter(group == "Very good")

data_min <- df_pp |>
  group_by(group, cat, sex) |>
  slice_min(year_c, n = 1) |>
  filter(group == "Very good")

data_max <- df_pp |>
  group_by(group, cat, sex) |>
  slice_max(estimate, n = 1) |>
  filter(group == "Very good")

# Visualize it -----------------------------------------------------------------

p1 <- df_pp |>
  ggplot(aes(x = year_c, y = estimate, fill = group)) +
  geom_area(position = "stack") +
  geom_line(
    data = subset(df_pp, group == "Very good"),
    aes(x = year_c, y = estimate, group = interaction(group, cat)),
    color = "#18BC9C",
    linewidth = 1) +
  geom_text_repel(data = data_end, 
                  aes(label = scales::percent(estimate, 1)), 
                  size = 2.5, nudge_y = .05, color = "grey20",
                  segment.color = 'transparent') +
  geom_text_repel(data = data_min, 
                  aes(label = scales::percent(estimate, 1)), 
                  size = 2.5, nudge_y = -.04, color = "grey20",
                  segment.color = 'transparent') +
  geom_text_repel(data = data_max, 
                  aes(label = scales::percent(estimate, 1)), 
                  size = 2.5, nudge_y = .03, nudge_x = -1, color = "grey20",
                  segment.color = 'transparent') +
  geom_text_repel(data = data_min |> filter(cat == "Spouse"), 
                  aes(label = sex), 
                  size = 3.5, nudge_x = 8, nudge_y = .15, 
                  segment.color = 'transparent', color = "grey20", fontface = "bold") +
  facet_grid(rows = vars(sex), cols = vars(cat)) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position   = "right",
    strip.text        = element_text(size = 12, face = "bold"),
    strip.text.y      = element_blank(),
    axis.title        = element_blank(),
    axis.text.y       = element_blank(),
    axis.ticks.y      = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.spacing     = unit(2, "lines"),
    plot.subtitle     = element_text(size = 12, color = "grey60", face = "italic")) +
  scale_x_continuous(breaks=c(-20.79975, -4.79975, 11.20025, 23.20025), labels = lables_year) +
  scale_fill_manual(values = rev(c("#18BC9C50", "#3498DB20", "#9966FF30", "#F39C1260", "#e74c3c80"))) + 
  labs(fill = "Expectations\n(at modal age 18)")
# title = "The share of youth expecting to be 'very good' spouses & parents 
  #     has stalled after decades of growth.")

p1

## Save Fig 1
agg_png(filename = file.path(here(outDir, figDir), "fig1.png"), 
         width=5.5, height=4, units="in", res = 800, scaling = 1)

plot(p1)
invisible(dev.off())