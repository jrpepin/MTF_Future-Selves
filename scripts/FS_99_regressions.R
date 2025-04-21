# sex + race + momed + region + year.c + year2.c + religion + famstru.d + selfconcept, 


# Average Predictions ----------------------------------------------------------
pp_sp   <- ggaverage(polr1.sp, terms = c("year.c [all]"))
pp_pa   <- ggaverage(polr1.pa, terms = c("year.c [all]"))
pp_wk   <- ggaverage(polr1.wk, terms = c("year.c [all]"))

#### Combine dfs
pp_sp$cat    <- "Spouse" 
pp_pa$cat    <- "Parent" 
pp_wk$cat    <- "Worker" 

df_pp <- rbind(pp_wk, pp_sp, pp_pa)

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

lables_year <- c("1984", "1995", "2005", "2015", "2023")

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
  scale_x_continuous(breaks=c(-12.5, -1.54, 8.45, 18.45, 26.45), labels = lables_year) +
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
  scale_x_continuous(breaks=c(-12.5, -1.54, 8.45, 18.45, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ")

p1 <- (p1_very | p1_other) + plot_layout(widths = c(1, 2)) +
  plot_annotation('How good do you think you would be as a _________', 
                  caption = "Monitoring the Future 12th Grade Surveys (1984-2023)")

p1

#Save Fig 1
agg_tiff(filename = file.path(here(outDir, figDir), "fig1.tif"), 
         width=8, height=6, units="in", res = 800, scaling = 1)

plot(p1)
invisible(dev.off())


## Stacked bar chart
pp_sp %>%
  ggplot(aes(x = x, y = predicted, fill = response.level, 
             ymin = conf.low, ymax = conf.high)) +
  geom_bar(position="fill", stat="identity") +
  theme_minimal() 

