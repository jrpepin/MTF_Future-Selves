#-------------------------------------------------------------------------------
# MTF FUTURE SELVES PROJECT
# FS_01_measures and sample.R
# Joanna R. Pepin & Melissa Milkie
#-------------------------------------------------------------------------------

# Project Environment ----------------------------------------------------------
## The FS_00-1_setup and packages.R script should be run before this script

# source(FS_00-1_setup and packages.R)

# DATA -------------------------------------------------------------------------
load(file.path("data/mtf_form2.Rda"))

## Load the data and create a new df containing only the variables of interest.  
data <- mtf_V2 |>
  # Create an ID variable
  mutate(ID = row_number()) |>
  select(ID, V5, ARCHIVE_WT, V1,          # Survey variables
         V2312, V2313, V2314,             # Project specific
         V2150, V2151, V2164, V2165,      # Demographic (V2165 - momemp ! 2022)
         V2169, V2155, V2156)

## Rename Variables
data <- dplyr::rename(data,      
                      wt7611   = V5,     wt1222   = ARCHIVE_WT,  year     = V1, 
                      gdsp     = V2312,  gdpa     = V2313,       gdwk     = V2314,  
                      gender   = V2150,  raceeth  = V2151,       momed    = V2164,  
                      momemp   = V2165,  father   = V2155,       mother   = V2156)

## Sample size
count(data)

## Create crosswalk of survey year and ICPSR Study ID 
studyid    <- c( 7927,  7928,  7929,  7930,
                 7900,  9013,  9045,  8387,  8388,
                 8546,  8701,  9079,  9259,  9397,
                 9745,  9871,  6133,  6367,  6517,
                 6716,  2268,  2477,  2751,  2939,
                 3184,  3425,  3753,  4019,  4264,
                 4536, 20022, 22480, 25382, 28401,
                 30985, 34409, 34861, 35218, 36263,
                 36408, 36798, 37182, 37416, 37841,
                 38156, 38503, 38882, 39172, 39444)

surveyyear <- c(1976, 1977, 1978, 1979,
                1980, 1981, 1982, 1983, 1984,
                1985, 1986, 1987, 1988, 1989,
                1990, 1991, 1992, 1993, 1994,
                1995, 1996, 1997, 1998, 1999,
                2000, 2001, 2002, 2003, 2004,
                2005, 2006, 2007, 2008, 2009,
                2010, 2011, 2012, 2013, 2014,
                2015, 2016, 2017, 2018, 2019,
                2020, 2021, 2022, 2023, 2024)

Xwalk <- data.frame(surveyyear, studyid)

# VARIABLES --------------------------------------------------------------------

## Year
data$year <- as.character(data$year)
data$year[data$year == "76"] <- "1976"
data$year[is.na(data$year)]  <- "1978" # 34 people in 1978 have a missing year variable

data$year <- as.integer(data$year)

### put data in chronological order, center, & square
data <- data |>
  arrange(year) |>
  mutate(
    year2    = year^2,
    year_c   = round(year - mean(year), 5),
    year2_c  = year_c^2)


## Weights (run after year correction)
colSums(!is.na(data))
table(data$year, !is.na(data$wt7611)) # 102810
table(data$year, !is.na(data$wt1222)) # 21861

data <- data |>
  mutate(
    svyweight = case_when(
      year  <= 2011 ~ wt7611,
      year  >= 2012 ~ wt1222))

# https://stats.stackexchange.com/questions/553014/problem-with-weigts-in-survey-analysis-of-gss-cross-sectional-data
# data[ , 'svyweight_scaled'] <- data[ , svyweight] * 124671 /nrow(data) ## THIS DIDN'T CHANGE ANYTHING

## Categorical Variables

data <- data |>
  mutate(
    # HOW GD AS SPOUSE
    gdsp = fct_case_when(
      gdsp == 1 | gdsp == "POOR"     | gdsp == "Poor"       | gdsp == "POOR:(1)"                               ~ "Poor",
      gdsp == 2 | gdsp == "NOT GOOD" | gdsp == "Not so good"| gdsp == "NOT GOOD:(2)"                           ~ "Not so good",
      gdsp == 3 | gdsp == "FRLY GD"  | gdsp == "Fairly good"| gdsp == "FRLY GD:(3)"  | gdsp == "FAIR GOOD:(3)" ~ "Fairly good",
      gdsp == 4 | gdsp == "GOOD"     | gdsp == "Good"       | gdsp == "GOOD:(4)"                               ~ "Good",
      gdsp == 5 | gdsp == "VRY GOOD" | gdsp == "Very good"  | gdsp == "VRY GOOD:(5)"                           ~ "Very good",
      TRUE                                                                                                     ~  NA_character_),
    # Good spouse dummy
    gdsp_v = case_when(
      gdsp  == "Very good"   ~ 1,
      gdsp  == "Good"        |
      gdsp  == "Fairly good" |
      gdsp  == "Not so good" |
      gdsp  == "Poor"        ~ 0),
    # Good spouse numeric
    gdsp_num = as.numeric(gdsp),
    # HOW GD AS PARENT
    gdpa = fct_case_when(
      gdpa == 1 | gdpa == "POOR"     | gdpa == "Poor"       | gdpa == "POOR:(1)"                               ~ "Poor",
      gdpa == 2 | gdpa == "NOT GOOD" | gdpa == "Not so good"| gdpa == "NOT GOOD:(2)"                           ~ "Not so good",
      gdpa == 3 | gdpa == "FRLY GD"  | gdpa == "Fairly good"| gdpa == "FRLY GD:(3)"  | gdpa == "FAIR GOOD:(3)" ~ "Fairly good",
      gdpa == 4 | gdpa == "GOOD"     | gdpa == "Good"       | gdpa == "GOOD:(4)"                               ~ "Good",
      gdpa == 5 | gdpa == "VRY GOOD" | gdpa == "Very good"  | gdpa == "VRY GOOD:(5)"                           ~ "Very good",
      TRUE                                                                                                     ~  NA_character_),
    # Good parent dummy
    gdpa_v = case_when(
      gdpa  == "Very good"   ~ 1,
      gdpa  == "Good"        |
      gdpa  == "Fairly good" |
      gdpa  == "Not so good" |
      gdpa  == "Poor"        ~ 0),
    # Good parent numeric
    gdpa_num = as.numeric(gdpa),
    # HOW GD AS WORKER
    gdwk = fct_case_when(
      gdwk == 1 | gdwk == "POOR"     | gdwk == "Poor"       | gdwk == "POOR:(1)"                               ~ "Poor",
      gdwk == 2 | gdwk == "NOT GOOD" | gdwk == "Not so good"| gdwk == "NOT GOOD:(2)"                           ~ "Not so good",
      gdwk == 3 | gdwk == "FRLY GD"  | gdwk == "Fairly good"| gdwk == "FRLY GD:(3)"  | gdwk == "FAIR GOOD:(3)" ~ "Fairly good",
      gdwk == 4 | gdwk == "GOOD"     | gdwk == "Good"       | gdwk == "GOOD:(4)"                               ~ "Good",
      gdwk == 5 | gdwk == "VRY GOOD" | gdwk == "Very good"  | gdwk == "VRY GOOD:(5)"                           ~ "Very good",
      TRUE                                                                                                     ~  NA_character_),
    # Good worker dummy
    gdwk_v = case_when(
      gdwk  == "Very good"   ~ 1,
      gdwk  == "Good"        |
      gdwk  == "Fairly good" |
      gdwk  == "Not so good" |
      gdwk  == "Poor"        ~ 0),
    # Good worker numeric
    gdwk_num = as.numeric(gdwk),
    # Gender
    sex = fct_case_when(
      gender == 1 | gender == "MALE"    | gender == "MALE:(1)"    | 
        gender == "Male"           ~ "Men",
      gender == 2 | gender == "FEMALE"  | gender == "FEMALE:(2)"  | 
        gender == "Female"         ~ "Women",
      TRUE ~ NA_character_),
    # Race
    race = fct_case_when(
      raceeth == 0 | raceeth == "WHITE"    | raceeth == "WHITE: (2)"    | 
        raceeth == "White (Caucasian)"          | raceeth == "WHITE:(2)"    ~ "White",
      raceeth == 1 | raceeth == "BLACK"    | raceeth == "BLACK: (1)"    | 
        raceeth == "Black or African-American"  | raceeth == "BLACK:(1)"    ~ "Black",
      TRUE ~ "Another race"),
    # Racesex
    racesex = fct_case_when(
      race == "White" & sex == "Men"   ~ "White men",
      race == "White" & sex == "Women" ~ "White women",
      race == "Black" & sex == "Men"   ~ "Black men",
      race == "Black" & sex == "Women" ~ "Black women",
      TRUE ~  NA_character_),
    # Mothers' Education
    mom_ba = fct_case_when(
      momed == "1" | momed == "GRDE SCH" | momed == "GRDE SCH:(1)" | momed == "Completed grade school or less"     |
      momed == "2" | momed == "SOME HS"  | momed == "SOME HS:(2)"  | momed == "Some high school"                   |
      momed == "3" | momed == "HS GRAD"  | momed == "HS GRAD:(3)"  | momed == "Completed high school"              |
      momed == "4" | momed == "SOME CLG" | momed == "SOME CLG:(4)" | momed ==  "Some college"                      ~ "No college degree",
      momed == "5" | momed == "CLG GRAD" | momed == "CLG GRAD:(5)" | momed ==  "Completed college"                 |
      momed == "6" | momed == "GRAD SCH" | momed == "GRAD SCH:(6)" | momed ==  "Graduate or professional school"   ~ "Completed college",
      momed == "7" | momed == "MISSING"  | momed == "DK:(7)"       | momed ==  "Don't know, or does not apply"     | # These don't match but missing who cares
        TRUE                                                                                                       ~  NA_character_ ),
    # Mothers' Employment
    momwork = fct_case_when(
      momemp == "1" | momemp == "NO"       | momemp == "NO:(1)"       | momemp == "No"                                       |
      momemp == "2" | momemp == "SOMETIME" | momemp == "SOMETIME:(2)" | momemp == "Yes, some of the time when growing up"    |
      momemp == "YES/SOME:(2)"                                                                                               ~ "No or sometimes",
      momemp == "3" | momemp == "MOSTTIME" | momemp == "MOSTTIME:(3)" | momemp == "Yes, most of the time"                    |
      momemp == "4" | momemp == "ALL TIME" | momemp == "ALL TIME:(4)" | momemp == "Yes, all or nearly all of the time"       |
      momemp == "MOSTTIME:(3)" | momemp == "ALL TIME:(4)" | momemp == "YES/MOST:(3)" | momemp == "YES/NRLY ALL:(4)"          ~ "Most or all the time",
      TRUE                                                                                                                   ~ NA_character_),
    # Family Structure
    mother = fct_case_when(
      mother == 1 | mother == "MARKED"   | mother == "MARKED:(1)"   | mother == "Yes" ~ "YES",
      mother == 0 | mother == "NT MARKD" | mother == "NT MARKD:(0)" | mother == "No"  ~ "NO",
      TRUE  ~ NA_character_),
    father = fct_case_when(
      father == 1 | father == "MARKED"   | father == "MARKED:(1)"   | father == "Yes" ~ "YES",
      father == 0 | father == "NT MARKD" | father == "NT MARKD:(0)" | father == "No"  ~ "NO",
      TRUE ~ NA_character_),
    famstru = fct_case_when(
      mother == "YES" & father == "YES" ~ "Both Mother & Father",
      mother == "YES" & father == "NO"  ~ "Mother Only",
      mother == "NO"  & father == "YES" ~ "Father Only",
      mother == "NO"  & father == "NO"  ~ "Neither Mother/Father",
      TRUE ~  NA_character_),
    # Family Structure Dummy
    momdad = case_when(
      famstru == "Both Mother & Father"    ~ "Both Mother & Father",
      famstru == "Mother Only"   | 
        famstru == "Father Only" | 
        famstru == "Neither Mother/Father" ~ "Another fam structure",
      TRUE ~ NA_character_)) |>
  select(ID, svyweight, year, year_c, year2, year2_c,
         gdsp, gdsp_v, gdsp_num,
         gdpa, gdpa_v, gdpa_num,
         gdwk, gdwk_v, gdwk_num,
         sex, race, racesex, mom_ba, momdad, momwork) 

### Add formatted level labels for plotting 
data <- data |>
  mutate(
    gdsp_lbl = as_factor(case_when(
      gdsp == "Poor"        ~ "Poor",     
      gdsp == "Not so good" ~ "Not so\ngood",
      gdsp == "Fairly good" ~ "Fairly\ngood",
      gdsp == "Good"        ~ "Good",
      gdsp == "Very good"   ~ "Very\ngood",
      TRUE                    ~  NA_character_ )),
    gdpa_lbl = as_factor(case_when(
      gdpa == "Poor"        ~ "Poor",     
      gdpa == "Not so good" ~ "Not so\ngood",
      gdpa == "Fairly good" ~ "Fairly\ngood",
      gdpa == "Good"        ~ "Good",
      gdpa == "Very good"   ~ "Very\ngood",
      TRUE                    ~  NA_character_ )),
    gdwk_lbl = as_factor(case_when(
      gdwk == "Poor"        ~ "Poor",     
      gdwk == "Not so good" ~ "Not so\ngood",
      gdwk == "Fairly good" ~ "Fairly\ngood",
      gdwk == "Good"        ~ "Good",
      gdwk == "Very good"   ~ "Very\ngood",
      TRUE                    ~  NA_character_ )))

# Sample -----------------------------------------------------------------------
glimpse(data)

## Original sample size
count(data)

## Missing data  
colSums(is.na(data))

## Sample Restrictions
mtf0 <- data |>
  filter(year <= 2020)

mtf1 <- mtf0 |>
  drop_na(gdwk, gdsp, gdpa)      # DVs observed

mtf2 <- mtf1 |>
  drop_na(sex, race, mom_ba, momwork, momdad)  # key IVs observed

sample_flow <- tibble(
  step = c(
    "Original sample",
    "2020 or earlier",
    "Non-missing DVs",
    "Non-missing key IVs"),
  n = c(
    nrow(data),
    nrow(mtf0),
    nrow(mtf1),
    nrow(mtf2))) |>
  mutate(
    lost = dplyr::lag(n) - n,
    pct_lost = round(100 * lost / dplyr::lag(n), 1),
    total_lost = dplyr::first(n) - n)

sample_flow

# Final analytic sample
sample <- mtf2 |>
  droplevels()

counts <- sample |>
  group_by(year) |>
  count()


# Create survey data -----------------------------------------------------------
mtf_svy <- sample |>
  select(c(gdsp, gdpa, gdwk, gdsp_v, gdpa_v, gdwk_v, gdsp_num, gdpa_num, gdwk_v, 
           svyweight, year, year_c, sex, race, mom_ba, momwork, momdad)) |>
  # weight data
  as_survey_design(
    ids = NULL,
    weights = svyweight)

save(sample, file = here("data", "df.Rda"))

message("End of FS_01_measures and sample") # Marks end of R Script

