#-------------------------------------------------------------------------------
# MTF FUTURE SELVES PROJECT
# FS_00-1_setup and packages.R
# Joanna R. Pepin & Melissa Milkie
#-------------------------------------------------------------------------------

################################################################################
## Install and load required packages
################################################################################
# install.packages("pacman")       # Install pacman package if not installed
library("pacman")                  # Load pacman package

# Install packages not yet installed & load them
pacman::p_load(
  here,            # relative file paths
  devtools,        # loading github packages
  tidyverse,       # dplyr tidyr forcats stringr tibble ggplot2
  icpsrdata,       # import ICPSR data
  foreign,         # import Stata data
  data.table,      # need for rbindlist 
  haven,           #
  plyr,            #
  srvyr,           # create weighted survey data
  sjPlot,          # generate a codebook
  MESS,            # round percentages
  MASS,            # ologits
  marginaleffects, # average marginal effects
  ggeffects,       # predicted probabilities
  effects,         # required for ggeffects
  patchwork,       # combine sep facet plots
  ragg,            # save high quality plots
  scales,          # percentages for ggplots axes
  psych,           # alphas
  ggcorrplot,      # correlation matrix plot
  officer,         # producing word output
  modelsummary,    # producing word tables
  writexl,         # exporting df to excel
  flextable,       # producing word output
  gt,              # tables with gt formatting
  pandoc,          # word docs from modelsummary
  weights,         # drop leading 0 in geom_text labels
  conflicted
)

# remotes::install_github("ddsjoberg/gtsummary")
library(gtsummary) # tables with unweighted Ns

sessionInfo()

# Address any conflicts in the packages
conflict_scout() # Identify the conflicts
conflict_prefer("remove", "base")
conflict_prefer("here", "here")
conflict_prefer("as_flextable", "gtsummary")

conflict_prefer("filter",    "dplyr")
conflict_prefer("mutate",    "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("count",     "dplyr")
conflict_prefer("rename",    "dplyr")
conflict_prefer("arrange",   "dplyr")
conflict_prefer("desc",      "dplyr")
conflict_prefer("select",    "gtsummary")

################################################################################
# Functions
################################################################################

### function to keep factor order
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
} 

################################################################################
# Set-up the Directories
################################################################################

# File path to this project's directory
projDir <- here::here()              

# Name of folder where the data will be downloaded
dataDir <- "./../../../Dropbox/Data/@Monitoring the Future/icpsr_data"         

# Name of the sub-folder where we will save our source code (R scripts)
srcDir  <- "scripts"       

# Name of the sub-folder where we will save results
outDir  <- "output"      

# Name of the sub-folder where we will save generated figures
figDir  <- "figs"                                 

## This will create sub-directory folders in the projDir if they don't exist
if (!dir.exists(here::here(srcDir))){
  dir.create(srcDir)
} else {
  print("SRC directory already exists!")
}

if (!dir.exists(here::here(outDir))){
  dir.create(outDir)
} else {
  print("Output directory already exists!")
}

if (!dir.exists(here::here(outDir, figDir))){
  dir.create(here(outDir, figDir))
} else {
  print("Figure directory already exists!")
}


# Download the data ------------------------------------------------------------
## Users should remove the # before the source() line to run the data download.
## This script only needs to be run the first time or if there is a new wave of data.

# source(FS_00-2_data download.R)

message("End of FS_00-1_setup and packages") # Marks end of R Script
