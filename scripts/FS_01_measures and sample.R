#-------------------------------------------------------------------------------
# MTF FUTURE SELVES PROJECT
# FS_01_measures and sample.R
# Joanna R. Pepin & Melissa Milkie
#-------------------------------------------------------------------------------

# Project Environment ----------------------------------------------------------
## The PS_00-1_setup and packages.R script should be run before this script

# source(PS_00-1_setup and packages.R)

# DATA -------------------------------------------------------------------------
load(file.path("data/mtf_form2.Rda"))

## Load the data and create a new df containing only the variables of interest.  
data <- mtf_V2 %>%
  # Create an ID variable
  mutate(ID = row_number()) %>%
  select(ID, V5, ARCHIVE_WT, V1, V13, TABLET,         # Survey variables
         V2312, V2313, V2314, V2208, V2311,           # Project specific
         V2239, V2241,
         V2616, V2617, V2618, V2619, V2620,
         V2433, V2434, V2435, V2436, V2437, V2438,
         V2439, V2440,
         V2150, V2151, V2164,                         # Demographic (V2165 - momemp ! 2022)
         V2169, V2155, V2156)

## Rename Variables
data <- dplyr::rename(data,      
                      wt7611   = V5,     wt1222   = ARCHIVE_WT,  year     = V1, 
                      gdsp     = V2312,  gdpa     = V2313,       gdwk     = V2314,  
                      getmar   = V2239,  markids  = V2241,
                      happy    = V2208,  lifesat  = V2311,       anxiety  = V2620, 
                      meaning  = V2616,  enjoy    = V2617,       hopeless = V2618,
                      alive    = V2619,  posatt   = V2433,       worth    = V2434,
                      welloth  = V2435,  satself  = V2436,       proud    = V2437,
                      nogood   = V2438,  wrong    = V2439,       lifeuse  = V2440,
                      gender   = V2150,  raceeth  = V2151,       region   = V13,
                      momed    = V2164,  religion = V2169,
                      father   = V2155,  mother = V2156)

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
                 38156, 38503, 38882, 39172)

surveyyear <- c(1976, 1977, 1978, 1979,
                1980, 1981, 1982, 1983, 1984,
                1985, 1986, 1987, 1988, 1989,
                1990, 1991, 1992, 1993, 1994,
                1995, 1996, 1997, 1998, 1999,
                2000, 2001, 2002, 2003, 2004,
                2005, 2006, 2007, 2008, 2009,
                2010, 2011, 2012, 2013, 2014,
                2015, 2016, 2017, 2018, 2019,
                2020, 2021, 2022, 2023)

Xwalk <- data.frame(surveyyear, studyid)

# VARIABLES --------------------------------------------------------------------

## Year
data$year <- as.character(data$year)
data$year[data$year == "76"] <- "1976"
data$year[is.na(data$year)]  <- "1978" # 34 people in 1978 have a missing year variable

data$year <- as.integer(data$year)

#data <- data %>%
  # combining 2020 samples due to sample size issue
 # mutate(year = case_when(
  #  year == 2020 ~ 2023,
  #  year == 2021 ~ 2023,
  #  year == 2022 ~ 2023,
  #  TRUE ~ year))

### put data in chronological order, center, & square
data <- data %>%
  arrange(year) %>%
  mutate(
    year2    = year^2,
    year.c     = round(year - mean(year), 5),
    year2.c  = year.c^2)

## Weights
colSums(!is.na(data))
table(data$year, !is.na(data$wt7611)) # 102810
table(data$year, !is.na(data$wt1222)) # 21861

data <- data %>%
  mutate(
    svyweight = case_when(
      year  <= 2011 ~ wt7611,
      year  >= 2012 ~ wt1222))

# https://stats.stackexchange.com/questions/553014/problem-with-weigts-in-survey-analysis-of-gss-cross-sectional-data
# data[ , 'svyweight_scaled'] <- data[ , svyweight] * 124671 /nrow(data) ## THIS DIDN'T CHANGE ANYTHING


## Categorical Variables

data <- data %>%
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
    # VRY HPY THS DAYS
    happy = fct_case_when(
      happy == 1 | happy == "NT HAPPY" | happy == "Not too happy"  | happy == "NT HAPPY:(1)" ~ "Not too happy",
      happy == 2 | happy == "PRTY HPY" | happy == "Pretty happy"   | happy == "PRTY HPY:(2)" ~ "Pretty happy",
      happy == 3 | happy == "VRY HPY"  | happy == "Very happy"     | happy == "VRY HPY:(3)"  ~ "Very happy",
      TRUE                                                                                   ~  NA_character_),
    # Get married
    getmar = fct_case_when(
      getmar == 3 | getmar == "MARRY"    | getmar == "MARRY:(3)"    | getmar == "Getting married"     | getmar == "MARRIED:(3)"            ~ "GETTING MARRIED",
      getmar == 1 | getmar == "NT MARRY" | getmar == "NT MARRY:(1)" | getmar == "Not getting married" | getmar == "NOT MAR:(1)"            ~ "NOT GETTING MARRIED",
      getmar == 2 | getmar == "NO IDEA"  | getmar == "NO IDEA:(2)"  | getmar == "I have no idea"                                           ~ "I HAVE NO IDEA",
      getmar == 8                        | getmar == "MARRIED:(8)"                                    | getmar == "ALREADY MAR:(8)"        ~ "ALREADY MARRIED",
      getmar == 9 | getmar == "MISSING"  | getmar == "MISSING:(-9)" | getmar == "Missing"             | getmar == "R ASSIGNED ORANGE:(-8)" | 
        getmar == 0 ~ "MISSING"),
    # 3 category getmar
    mar3 = fct_case_when(
      getmar == "GETTING MARRIED" | getmar == "ALREADY MARRIED" ~ "Getting married",
      getmar == "NOT GETTING MARRIED"                           ~ "Not getting married",
      getmar == "I HAVE NO IDEA"                                ~ "I have no idea",
      TRUE                                                      ~  NA_character_ ),
    # getmar dummy
    mardum = fct_case_when(
      mar3     == "Not getting married"  |
      mar3     == "I have no idea"       ~ 0,
      mar3     == "Getting married"      ~ 1),
    # If you did get married (or are married) . . . How likely is it that you would want to have children?
    markids  = fct_case_when( 
      markids == 1 | markids == "V UNLKLY"    | markids == "V UNLKLY:(1)"      | markids == "Very unlikely"            | markids == "VRY UNLKLY:(1)"    ~ "Very unlikely",
      markids == 2 | markids == "FRLY UNL"    | markids == "FRLY UNL:(2)"      | markids == "Fairly unlikely"          | markids == "FAIRLY UNLK:(2)"   ~ "Fairly unlikely",
      markids == 3 | markids == "UNCERTN"     | markids == "UNCERTN:(3)"       | markids == "Uncertain"                                                 ~ "Uncertain",
      markids == 4 | markids == "FRLY LIK"    | markids == "FRLY LIK:(4)"      | markids == "Fairly likely"            | markids == "FAIRLY LK:(4)"     ~ "Fairly likely",
      markids == 5 | markids == "VY LIKLY"    | markids == "VY LIKLY:(5)"      | markids == "Very likely"              | markids == "VRY LIKELY:(5)"    ~ "Very likely",
      markids == 8                            | markids == "HAVE KID:(8)"      | markids == "Already have child(ren)"  | markids == "ALRDY HAVE:(8)"    ~ "Already have kid",
      markids == 9 | markids == "MISSING"     | markids == "MISSING:(-9)"      | markids == "Missing"                  | 
      markids == "R ASSIGNED ORANGE:(-8)"     | markids == "SCREEN BREAK:(-1)" | markids == "R ASSIGNED BLUE:(-8)"                                      ~  NA_character_ ),
    # 3 category markids
    kids3 = fct_case_when(
      markids == "Very unlikely" | markids == "Fairly unlikely" | markids == "Uncertain"  ~ "Unlikely/Uncertain",
      markids == "Fairly likely"                                                          ~ "Fairly likely",
      markids == "Very likely" | markids == "Already have kid"                            ~ "Very likely",
      TRUE                                                                                ~  NA_character_ ),
    # markids dummy
    kidsdum = fct_case_when(
      kids3 == "Unlikely/Uncertain" | 
      kids3 == "Fairly likely"      ~ 0,
      kids3 == "Very likely"        ~ 1),
    # CMP SATFD W/LIFE
    lifesat = fct_case_when(
      lifesat == 1 | lifesat == "COMP DIS " | lifesat == "Completely dissatisfied"    | lifesat == "COMP DIS:(1)"      ~ "Completely dissatisfied",
      lifesat == 2 | lifesat == "QUITE DS"  | lifesat == "Quite dissatisfied"         | lifesat == "QUITE DS:(2)" | 
        lifesat == "QUITE:(2)"  | lifesat == "QUITE DIS:(2)"                                                           ~ "Quite dissatisfied",
      lifesat == 3 | lifesat == "SMWT DIS"  | lifesat == "Somewhat dissatisfied"      | lifesat == "SMWT DIS:(3)" | 
        lifesat == "SOME DIS:(3)"                                                                                      ~ "Somewhat dissatisfied",
      lifesat == 4 | lifesat == "NEITHER"   | lifesat == "Neither, or mixed feelings" | lifesat == "NEITHER:(4)"       ~ "Neither, or mixed feelings",
      lifesat == 5 | lifesat == "SMWT SAT"  | lifesat == "Somewhat satisfied"         | lifesat == "SMWT SAT:(5)" | 
        lifesat == "SOME DIS:(5)" | lifesat == "SOME SAT:(5)"                                                          ~ "Somewhat satisfied",
      lifesat == 6 | lifesat == "QUITE ST"  | lifesat == "Quite satisfied"            | lifesat == "QUITE ST:(6)" | 
        lifesat == "QUITE:(6)"   | lifesat == "QUITE SAT:(6)"                                                          ~ "Quite satisfied",
      lifesat == 7 | lifesat == "COMP SAT"  | lifesat == "Completely satisfied"       | lifesat == "COMPLETE:(7)" | 
        lifesat == "COMP SAT:(7)"                                                                                      ~ "Completely satisfied",
      TRUE                                                                                                             ~  NA_character_),
    # LIFE MEANINGLESS
    meaning = fct_case_when(
      meaning == "DISAGREE:(1)" ~ "Disagree",
      meaning == "MOST DIS:(2)" ~ "Mostly disagree",
      meaning == "NEITHER:(3)"  ~ "Neither",
      meaning == "MOST AGR:(4)" ~ "Mostly agree",
      meaning == "AGREE:(5)"    ~ "Agree",
      TRUE                      ~  NA_character_),
    # I ENJOY LIFE
    enjoy = fct_case_when(
      enjoy == "DISAGREE:(1)" ~ "Disagree",
      enjoy == "MOST DIS:(2)" ~ "Mostly disagree",
      enjoy == "NEITHER:(3)"  ~ "Neither",
      enjoy == "MOST AGR:(4)" ~ "Mostly agree",
      enjoy == "AGREE:(5)"    ~ "Agree",
      TRUE                    ~  NA_character_),
    # FUTURE HOPELESS
    hopeless = fct_case_when(
      hopeless == "DISAGREE:(1)" ~ "Disagree",
      hopeless == "MOST DIS:(2)" ~ "Mostly disagree",
      hopeless == "NEITHER:(3)"  ~ "Neither",
      hopeless == "MOST AGR:(4)" ~ "Mostly agree",
      hopeless == "AGREE:(5)"    ~ "Agree",
      TRUE                       ~  NA_character_),
    # GOOD TO BE ALIVE
    alive = fct_case_when(
      alive == "DISAGREE:(1)" ~ "Disagree",
      alive == "MOST DIS:(2)" ~ "Mostly disagree",
      alive == "NEITHER:(3)"  ~ "Neither",
      alive == "MOST AGR:(4)" ~ "Mostly agree",
      alive == "AGREE:(5)"    ~ "Agree",
      TRUE                    ~  NA_character_),
    # OFTEN FEEL ANXIOUS
    anxiety = fct_case_when(
      anxiety == "DISAGREE:(1)" ~ "Disagree",
      anxiety == "MOST DIS:(2)" ~ "Mostly disagree",
      anxiety == "NEITHER:(3)"  ~ "Neither",
      anxiety == "MOST AGR:(4)" ~ "Mostly agree",
      anxiety == "AGREE:(5)"    ~ "Agree",
      TRUE                      ~  NA_character_),
    # POS ATT TWD SELF
    posatt = fct_case_when(
      posatt == 1 | posatt == "DISAGREE" | posatt == "Disagree"        | posatt == "DISAGREE:(1)"    ~ "Disagree",
      posatt == 2 | posatt == "MOST DIS" | posatt == "Mostly disagree" | posatt == "MOST DIS:(2)"    ~ "Mostly disagree",
      posatt == 3 | posatt == "NEITHER"  | posatt == "Neither"         | posatt == "NEITHER:(3)"     ~ "Neither",
      posatt == 4 | posatt == "MOST AGR" | posatt == "Mostly agree"    | posatt == "MOST AGR:(4)"    ~ "Mostly agree",
      posatt == 5 | posatt == "AGREE"    | posatt == "Agree"           | posatt == "AGREE:(5)"       ~ "Agree",
      TRUE                                                                                           ~  NA_character_),
    # AM PRSN OF WORTH
    worth = fct_case_when(
      worth == 1 | worth == "DISAGREE" | worth == "Disagree"        | worth == "DISAGREE:(1)"    ~ "Disagree",
      worth == 2 | worth == "MOST DIS" | worth == "Mostly disagree" | worth == "MOST DIS:(2)"    ~ "Mostly disagree",
      worth == 3 | worth == "NEITHER"  | worth == "Neither"         | worth == "NEITHER:(3)"     ~ "Neither",
      worth == 4 | worth == "MOST AGR" | worth == "Mostly agree"    | worth == "MOST AGR:(4)"    ~ "Mostly agree",
      worth == 5 | worth == "AGREE"    | worth == "Agree"           | worth == "AGREE:(5)"       ~ "Agree",
      TRUE                                                                                       ~  NA_character_),
    # DO WELL AS OTHRS
    welloth = fct_case_when(
      welloth == 1 | welloth == "DISAGREE" | welloth == "Disagree"        | welloth == "DISAGREE:(1)"    ~ "Disagree",
      welloth == 2 | welloth == "MOST DIS" | welloth == "Mostly disagree" | welloth == "MOST DIS:(2)"    ~ "Mostly disagree",
      welloth == 3 | welloth == "NEITHER"  | welloth == "Neither"         | welloth == "NEITHER:(3)"     ~ "Neither",
      welloth == 4 | welloth == "MOST AGR" | welloth == "Mostly agree"    | welloth == "MOST AGR:(4)"    ~ "Mostly agree",
      welloth == 5 | welloth == "AGREE"    | welloth == "Agree"           | welloth == "AGREE:(5)"       ~ "Agree",
      TRUE                                                                                               ~  NA_character_),
    # SATISFD W MYSELF
    satself = fct_case_when(
      satself == 1 | satself == "DISAGREE" | satself == "Disagree"        | satself == "DISAGREE:(1)"    ~ "Disagree",
      satself == 2 | satself == "MOST DIS" | satself == "Mostly disagree" | satself == "MOST DIS:(2)"    ~ "Mostly disagree",
      satself == 3 | satself == "NEITHER"  | satself == "Neither"         | satself == "NEITHER:(3)"     ~ "Neither",
      satself == 4 | satself == "MOST AGR" | satself == "Mostly agree"    | satself == "MOST AGR:(4)"    ~ "Mostly agree",
      satself == 5 | satself == "AGREE"    | satself == "Agree"           | satself == "AGREE:(5)"       ~ "Agree",
      TRUE                                                                                               ~  NA_character_),
    # -MUCH TO B PROUD
    proud = fct_case_when(
      proud == 1 | proud == "DISAGREE" | proud == "Disagree"        | proud == "DISAGREE:(1)"    ~ "Disagree",
      proud == 2 | proud == "MOST DIS" | proud == "Mostly disagree" | proud == "MOST DIS:(2)"    ~ "Mostly disagree",
      proud == 3 | proud == "NEITHER"  | proud == "Neither"         | proud == "NEITHER:(3)"     ~ "Neither",
      proud == 4 | proud == "MOST AGR" | proud == "Mostly agree"    | proud == "MOST AGR:(4)"    ~ "Mostly agree",
      proud == 5 | proud == "AGREE"    | proud == "Agree"           | proud == "AGREE:(5)"       ~ "Agree",
      TRUE                                                                                       ~  NA_character_),
    # I AM NO GOOD
    nogood = fct_case_when(
      nogood == 1 | nogood == "DISAGREE" | nogood == "Disagree"        | nogood == "DISAGREE:(1)"    ~ "Disagree",
      nogood == 2 | nogood == "MOST DIS" | nogood == "Mostly disagree" | nogood == "MOST DIS:(2)"    ~ "Mostly disagree",
      nogood == 3 | nogood == "NEITHER"  | nogood == "Neither"         | nogood == "NEITHER:(3)"     ~ "Neither",
      nogood == 4 | nogood == "MOST AGR" | nogood == "Mostly agree"    | nogood == "MOST AGR:(4)"    ~ "Mostly agree",
      nogood == 5 | nogood == "AGREE"    | nogood == "Agree"           | nogood == "AGREE:(5)"       ~ "Agree",
      TRUE                                                                                           ~  NA_character_),
    # I DO WRONG THING
    wrong = fct_case_when(
      wrong == 1 | wrong == "DISAGREE" | wrong == "Disagree"        | wrong == "DISAGREE:(1)"    ~ "Disagree",
      wrong == 2 | wrong == "MOST DIS" | wrong == "Mostly disagree" | wrong == "MOST DIS:(2)"    ~ "Mostly disagree",
      wrong == 3 | wrong == "NEITHER"  | wrong == "Neither"         | wrong == "NEITHER:(3)"     ~ "Neither",
      wrong == 4 | wrong == "MOST AGR" | wrong == "Mostly agree"    | wrong == "MOST AGR:(4)"    ~ "Mostly agree",
      wrong == 5 | wrong == "AGREE"    | wrong == "Agree"           | wrong == "AGREE:(5)"       ~ "Agree",
      TRUE                                                                                       ~  NA_character_),
    # MY LIFE NT USEFL
    lifeuse = fct_case_when(
      lifeuse == 1 | lifeuse == "DISAGREE" | lifeuse == "Disagree"        | lifeuse == "DISAGREE:(1)"    ~ "Disagree",
      lifeuse == 2 | lifeuse == "MOST DIS" | lifeuse == "Mostly disagree" | lifeuse == "MOST DIS:(2)"    ~ "Mostly disagree",
      lifeuse == 3 | lifeuse == "NEITHER"  | lifeuse == "Neither"         | lifeuse == "NEITHER:(3)"     ~ "Neither",
      lifeuse == 4 | lifeuse == "MOST AGR" | lifeuse == "Mostly agree"    | lifeuse == "MOST AGR:(4)"    ~ "Mostly agree",
      lifeuse == 5 | lifeuse == "AGREE"    | lifeuse == "Agree"           | lifeuse == "AGREE:(5)"       ~ "Agree",
      TRUE                                                                                               ~  NA_character_),
    # Decades
    decade = fct_case_when(
      year < 1980 ~ "1970s",
      year > 1979 & year < 1990 ~ "1980s",
      year > 1989 & year < 2000 ~ "1990s",
      year > 1999 & year < 2010 ~ "2000s",
      year > 2009 & year < 2020 ~ "2010s",
      year > 2019 ~ "2020s"),
    # tablet -- only avail. for 2019
    tablet = fct_case_when(
      TABLET == "TABLET:(1)" & year == 2019 ~ "Tablet",
      TABLET == "PAPER:(0)"  & year == 2019 ~ "Paper",
      TRUE ~ NA_character_),
    # Gender
    sex = fct_case_when(
      gender == 1 | gender == "MALE"    | gender == "MALE:(1)"    | gender == "Male"           ~ "Men",
      gender == 2 | gender == "FEMALE"  | gender == "FEMALE:(2)"  | gender == "Female"         ~ "Women",
      TRUE ~ NA_character_),
    # Race
    race = fct_case_when(
      raceeth == 0 | raceeth == "WHITE"    | raceeth == "WHITE: (2)"    | raceeth == "White (Caucasian)"          | raceeth == "WHITE:(2)"    ~ "White",
      raceeth == 1 | raceeth == "BLACK"    | raceeth == "BLACK: (1)"    | raceeth == "Black or African-American"  | raceeth == "BLACK:(1)"    ~ "Black",
      TRUE ~ "Another race"),
    # Racesex
    racesex = fct_case_when(
      race == "White" & sex == "Men"   ~ "White men",
      race == "White" & sex == "Women" ~ "White women",
      race == "Black" & sex == "Men"   ~ "Black men",
      race == "Black" & sex == "Women" ~ "Black women",
      TRUE ~  NA_character_),
    # Mothers' Education
    momed = fct_case_when(
      momed == "1" | momed == "GRDE SCH" | momed == "GRDE SCH:(1)" | momed == "Completed grade school or less"     |
      momed == "2" | momed == "SOME HS"  | momed == "SOME HS:(2)"  | momed == "Some high school"                   |
      momed == "3" | momed == "HS GRAD"  | momed == "HS GRAD:(3)"  | momed == "Completed high school"              |
      momed == "4" | momed == "SOME CLG" | momed == "SOME CLG:(4)" | momed ==  "Some college"                      ~ "No college degree",
      momed == "5" | momed == "CLG GRAD" | momed == "CLG GRAD:(5)" | momed ==  "Completed college"                 |
      momed == "6" | momed == "GRAD SCH" | momed == "GRAD SCH:(6)" | momed ==  "Graduate or professional school"   ~ "Completed college",
      momed == "7" | momed == "MISSING"  | momed == "DK:(7)"       | momed ==  "Don't know, or does not apply"     | # These don't match but missing who cares
        TRUE                                                                                                       ~  NA_character_ ),
    # Religiosity
    religion = fct_case_when(
      religion == 1 | religion == "NEVER"    | religion == "NEVER:(1)"    | religion == "Never"                     ~ "Never",
      religion == 2 | religion == "RARELY"   | religion == "RARELY:(2)"   | religion == "Rarely"                    ~ "Rarely",
      religion == 3 | religion == "1-2X/MO"  | religion == "1-2X/MO:(3)"  | religion == "Once or twice a month"     ~ "Once or twice a month",
      religion == 4 | religion == "1/WK OR+" | religion == "1/WK OR+:(4)" | religion == "About once a week or more" ~ "About once a week or more",
      TRUE  ~  NA_character_ ),
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
    famstru.d = case_when(
      famstru == "Both Mother & Father"    ~ 1,
      famstru == "Mother Only"   | 
        famstru == "Father Only" | 
        famstru == "Neither Mother/Father" ~ 0,
      TRUE ~ NA_integer_),
    # Region
    region = fct_case_when(
      region == 1 | region == "NE" | region == "NE:(1)"   | region == "NORTHEAST"     | region == "NORTHEAST:(1)" ~ "Northeast",
      region == 2 | region == "NC" | region == "NC:(2)"   | region == "NORTH CENTRAL" | region == "MIDWEST:(2)"   |
      region == "NORTH CENTRL:(2)" | region == "NORTH CENTRAL:(2)"                                                ~ "Midwest",
      region == 3 | region == "S"  | region == "S:(3)"    | region == "SOUTH"         | region == "SOUTH:(3)"     ~ "South",
      region == 4 | region == "W"  | region == "W:(4)"    | region == "WEST"          | region == "WEST:(4)"      ~ "West")) %>%
  select(ID, svyweight, year, year.c, year2, year2.c, decade, 
         gdsp, gdsp_v, gdsp_num,
         gdpa, gdpa_v, gdpa_num,
         gdwk, gdwk_v, gdwk_num,
         happy, lifesat, # Positive Affect (1976+)
         posatt, worth, welloth, satself, # Self-esteem (1984+)
         proud, nogood, wrong, lifeuse, # Self-derogation (1984+)
         meaning, enjoy, hopeless, alive, anxiety, # Depression & Anxiety (2022+)
         getmar, mar3, mardum, markids, kids3, kidsdum, # Expectations
         sex, race, racesex, momed, famstru, famstru.d, religion, region, tablet) 

### Add formatted level labels for plotting 
data <- data %>%
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

# Create numeric & scaled variables
data <- data %>%
  # Make numeric variables
  mutate_at(c("happy", "lifesat", "posatt", "worth", "welloth", "satself",
              "proud", "nogood", "wrong", "lifeuse"), list(N = as.numeric)) %>%
  # Standardize new variables
  mutate(across(
    .cols = contains('_N'), 
    .fns = scale,
    .names = "{.col}_std")) %>%
  # Back to numeric variable
  mutate(across(contains('_N'), as.numeric)) %>%
  # Round to 2 decimal places
  mutate(across(contains('_N_std'), round, 2))

new_col_order <- sort(names(data))

data %>% 
  relocate(
    ID, svyweight, starts_with("year"), decade, starts_with("gd"), all_of(new_col_order)) %>%
  head()

# Sample -----------------------------------------------------------------------
glimpse(data)

## Original sample size
count(data)

## Missing data  
colSums(is.na(data))

data <- data %>%
  # exclude cases missing on DVs
  drop_na(c(gdwk, gdsp, gdpa)) %>%
  # exclude cases missing on key IV
  drop_na(c(sex, race, momed, region)) 
#  drop_na(c(happy, lifesat, 
#            posatt, worth, welloth, satself, 
#            proud, nogood, wrong, lifeuse)) %>%
#  filter(decade != "1970s")

#data$decade <- droplevels(data$decade)

# Analytic sample size
count(data)

counts <- data %>%
  group_by(year) %>%
  count()

# ## Construct Scales ------------------------------------------------------------
# ### make after drop missing obvs
# 
# cor_matrix <- cor(data[, c('posatt_N', 'worth_N', 'welloth_N', 'satself_N',
#                            'proud_N', 'nogood_N', 'wrong_N', 'lifeuse_N')], use = "pairwise.complete.obs")
# p0 <- ggcorrplot(cor_matrix, type = "lower", lab = TRUE)
# 
# p0
# 
# ggsave(file.path(here(outDir, figDir),"alphas.png"), p0, width = 6.5, height = 6.5, dpi = 300, bg = 'white')
# 
# # Generate the alphas
# esteem <- data %>%
#   select('posatt_N', 'worth_N', 'welloth_N', 'satself_N') %>%
#   cov() %>%
#   psych::alpha()
# 
# derogation <- data %>%
#   select('proud_N', 'nogood_N', 'wrong_N', 'lifeuse_N') %>%
#   cov() %>%
#   psych::alpha()
# 
# selfconcept <- data %>%
#   select('posatt_N', 'worth_N', 'welloth_N', 'satself_N',
#          -'proud_N', -'nogood_N', -'wrong_N', -'lifeuse_N') %>%
#   cov() %>%
#   psych::alpha(check.keys = TRUE) # this auto reverse codes the derogation items
# 
# wellbeing <- data %>%
#   select('happy_N', 'lifesat_N') %>%
#   cov() %>%
#   psych::alpha()
# 
# index <- c('esteem', 'derogation', 'selfconcept', 'wellbeing')
# alphas <- c(esteem$total$raw_alpha, derogation$total$raw_alpha, 
#             selfconcept$total$raw_alpha, wellbeing$total$raw_alpha)
# alpha_df <- data.frame(index, alphas)
# alpha_df$alphas <- round(alpha_df$alphas, digits = 2)
# write_xlsx(alpha_df, path = file.path(outDir, "alphas.xlsx"))
# 
# remove(cor_matrix, esteem, derogation, selfconcept, wellbeing)
# 
# scoring_key = list(
#   esteem      = c('posatt_N', 'worth_N', 'welloth_N', 'satself_N'),
#   derogation  = c('proud_N', 'nogood_N', 'wrong_N', 'lifeuse_N'),
#   selfconcept = c('posatt_N', 'worth_N', 'welloth_N', 'satself_N',
#                   '-proud_N', '-nogood_N', '-wrong_N', '-lifeuse_N'))
# 
# scales <- as_tibble(psych::scoreFast(
#   keys   = scoring_key, 
#   items  = data,
#   totals = FALSE)) # average scores
# 
# data$esteem      <- scales$`esteem-A`
# data$derogation  <- scales$`derogation-A`
# data$selfconcept <- scales$`selfconcept-A`
# 
# data <- data %>%
# # Standardize new variables
# mutate(across(
#   .cols = c('esteem', 'derogation', 'selfconcept'), 
#   .fns = scale,
#   .names = "{.col}_std")) %>%
#   # Back to numeric variable
#   mutate(across(c('esteem_std', 'derogation_std', 
#                   'selfconcept_std'), as.numeric)) %>%
#   # Round to 2 decimal places
#   mutate(across(c('esteem_std', 'derogation_std', 
#                   'selfconcept_std'), round, 2))

# Create survey data -----------------------------------------------------------
mtf_svy <- data %>%
  select(c(gdsp, gdpa, gdwk, svyweight, year, year.c, 
           sex, momed, race, region,
           getmar, mar3, mardum, markids, kids3, kidsdum)) %>%
  # weight data
  as_survey_design(id = 1,
                   weights = svyweight)

message("End of PS_01_measures and sample") # Marks end of R Script

