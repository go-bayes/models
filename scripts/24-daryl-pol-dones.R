## Daryl VT's study investigating the effects of religious loss on political orientation

## Estimand
# intervention: loss of religion as defined as disaffiliation
# outcome: one year-effect on political orientation

# inclusion criteria:
# baseline exposure: political orientation measured.

# import data

source("/Users/joseph/GIT/templates/functions/libs2.R")

# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/funs.R")

# experimental functions
source("/Users/joseph/GIT/templates/functions/experimental_funs.R")
#
#
# # ALERT: UNCOMMENT THIS AND DOWNLOAD THE LIBRARIES FROM JB's GITHUB
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")
#
# # ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")
#
#
# # ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source(
#   "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
# )



## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE. DO NOT USE THIS PATH
# nzavs data
pull_path <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data"
  )

# read data ---------------------------------------------------------------
dat <- arrow::read_parquet(pull_path)

### WARNING: FOR EACH NEW STUDY SET UP A DIFFERENT PATH OTHERWISE YOU WILL WRITE OVER YOUR MODELS
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/nzvs_mods/24/daryl-dones"
  )

# check path:is this correct?  check so you know you are not overwriting other directors
push_mods


levels(dat$religion_bigger_denominations)

# set up lmtp -------------------------------------------------------------


# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 10

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(0112358)

# set cores for estimation
library(future)
plan(multisession)
n_cores <- parallel::detectCores()

# super learner libraries
sl_lib <- c("SL.randomForest",
            "SL.xgboost",
            "SL.ranger")



# set exposure here
nzavs_exposure <- "religion_religious"

# get ids
ids_2018 <- dat %>%
  filter(year_measured == 1, wave == 2018 &
           !is.na(!!sym(nzavs_exposure))) |> # criteria, no missing
  pull(id)

# filter the original dataset for these IDs three waves
dat <- as.data.frame(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_widths(dat)

dat_long_full <- dat %>%
  dplyr::filter(id %in% ids_2018 &
                  wave %in% c(2018, 2019, 2020)) %>%
  arrange(id, wave) %>%
  dplyr::select(
    "id",
    "wave",
    "year_measured",
    # "sample_origin_names_combined",
    "education_level_coarsen",
    # Ordinal-Rank 0-10 NZREG codes (with overseas school quals coded as Level 3, and all other ancillary categories coded as missing)  Combined highschool levels See:https://www.nzqa.govt.nz/assets/Studying-in-NZ/New-Zealand-Qualification-Framework/requirements-nzqf.pdf
    "male",
    # 0 = female, 0.5 = neither female nor male, 1 = male.
    "age",
    "born_nz",
    #   "hlth_disability",
    # value label 0    No 1   Yes
    "eth_cat",
    #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    "employed",
    # Are you currently employed? (this includes self-employment or casual work)
    # "gen_cohort",
    "household_inc",
    # Please estimate your total household income (before tax) for the last year.
    "nz_dep2018",
    # see nzavs materials
    "nzsei_13_l",
    # see nzavs materials
    "partner",
    # 0 = no, 1 = yes
    "parent",
    # 0 = no, 1 = yes
    "political_conservative",
    #Please rate how politically liberal versus conservative you see yourself as being.
    "pol_wing",
    # Please rate how politically left-wing versus right-wing you see yourself as being.
    "sample_frame_opt_in",
    "urban",
    # see NZAVS,
    "have_siblings",
    #Do you have siblings?
    #"total_siblings",
    # sum siblings
    # "number_sisters_older",
    # #How many older sisters do you have?
    # "number_sisters_younger",
    # #	How many younger sisters do you have?
    # "number_brothers_older",
    # #	How many older brothers do you have?
    # "number_brothers_younger",
    # #	How many older brothers do you have?
    #"children_num",
    # How many children have you given birth to, fathered, or adopted?
    "hours_children",
    #Hours - Looking after children
    "hours_work",
    #Hours - Working in paid employment
    "hours_housework",
    # Hours - Housework/cooking
    "hours_exercise",
    "agreeableness",
    # Mini-IPIP6 Agreeableness (also modelled as empathy facet)
    # Sympathize with others' feelings.
    # Am not interested in other people's problems.
    # Feel others' emotions.
    # Am not really interested in others.
    "conscientiousness",
    # see mini ipip6
    # Get chores done right away.
    # Like order.
    # Make a mess of things.
    # Often forget to put things back in their proper place.
    "extraversion",
    # Mini-IPIP6 Extraversion
    # Am the life of the party.
    # Don't talk a lot.
    # Keep in the background.
    # Talk to a lot of different people at parties.
    "honesty_humility",
    # see mini ipip6
    # Would like to be seen driving around in a very expensive car.
    # Would get a lot of pleasure from owning expensive luxury goods.
    # Feel entitled to more of everything.
    # Deserve more things in life.
    "openness",
    # see mini ipip6
    # Have a vivid imagination.
    # Have difficulty understanding abstract ideas.
    # Do not have a good imagination.
    # Am not interested in abstract ideas.
    "neuroticism",
    # see mini ipip6
    # Have frequent mood swings.
    # Am relaxed most of the time.
    # Get upset easily.
    # Seldom feel blue.
    # "modesty",
    # # see mini ipip6
    # # I want people to know that I am an important person of high status,
    # # I am an ordinary person who is no better than others.
    # # I wouldn’t want people to treat me as though I were superior to them.
    # # I think that I am entitled to more respect than the average person is
    # "sdo",
    # "rwa",
    "religion_religious",
    # Do you identify with a religion and/or spiritual group?
    "w_gend_age_ethnic",
    # "alcohol_frequency",
    # #"How often do you have a drink containing alcohol?"
    # "alcohol_intensity",
    # How many drinks containing alcohol do you have on a typical day when drinking?
    "hlth_bmi",
    "neighbourhood_community",
    #I feel a sense of community with others in my local neighbourhood.
    "support",
    "belong",
    "belong_beliefs",
    # Know that people around me share my attitudes and beliefs.
    "charity_donate",
    #
    "hours_charity",
    #,#Hours spent in activities/Hours spent … voluntary/charitable work
    "rural_gch_2018_l",
    "alert_level_combined_lead",
    "alert_level_combined"
  ) %>%
  mutate(
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1),
    household_inc_log = log(household_inc + 1),
    hours_charity_log = log(hours_charity + 1),
    hours_exercise_log = log(hours_exercise + 1),
    hours_children_log = log(charity_donate + 1), 
    charity_donate_log = log(hours_children + 1)
  ) |>
  select(-c(
    hours_work,
    hours_housework,
    household_inc,
    hours_charity,
    hours_exercise, 
    hours_children,
    charity_donate
  )) |>
  mutate(
    #initialize 'censored'
    censored = ifelse(lead(year_measured) == 1, 1, 0),
    
    # modify 'censored' based on the condition; no need to check for NA here as 'censored' is already defined in the previous step
    censored =  ifelse(is.na(censored) &
                         year_measured == 1, 1, censored),
    
    # Apply the case_when condition for setting 'censored' based on 'wave' and the dynamic column specified by 'nzavs_exposure'
    censored = case_when(
      # Add this condition to keep previous modifications unless the specific condition is met!is.na(censored) ~ censored,
      
      # Then check if 'wave' is 2019 and the specified exposure is NA, adjusting the condition to reflect the accurate logic
      wave == 2019 & !is.na(!!sym(nzavs_exposure)) ~ 1,
      
      # Default case if none of the above apply; might not be necessary if all possibilities are covered
      TRUE ~ 0
    )
  ) |>
  select(-year_measured) |>
  mutate(
    rural_gch_2018_l = as.numeric(as.character(rural_gch_2018_l)),
    have_siblings = as.numeric(as.character(have_siblings)),
    parent = as.numeric(as.character(parent)),
    partner = as.numeric(as.character(partner)),
    born_nz = as.numeric(as.character(born_nz)),
    censored = as.numeric(as.character(censored)),
    employed = as.numeric(as.character(employed))
  ) |>
  droplevels()

# we need the "full" for the descriptive table
dat_long <- dat_long_full |> select(-alert_level_combined)



# check n participants ----------------------------------------------------

#community at baseline 
n_participants <- n_unique(dat_long$id) #32058 # reports hours with 

# check
n_participants

push_mods
# save n
here_save(n_participants, "n_participants")
          
# check
table1::table1(~ religion_religious + censored + sample_frame_opt_in |
                 wave,
               data = dat_long)

table(is.na((dat$nzsei_13_l)))


# check positivity

dt_positivity <- dat_long |>
  filter(wave == 2018 | wave == 2019) |>
  select(wave, id, religion_religious)


# create transition matrix
out_pos <- msm::statetable.msm(religion_religious,
                               id,
                               data = dt_positivity)

cats_labels <- c("Not Religious", "Religious")

transition_table  <- margot::transition_table(out_pos, cats_labels)
transition_table
here_save(transition_table, "transition_table")


# check baseline missingness ----------------------------------------------
dt_18_miss <- dat_long |>
  filter(wave == 2018) |> 
  select(-alert_level_combined_lead) |> 
  droplevels()

naniar::miss_case_summary (dt_18_miss)


naniar::vis_miss(dt_18_miss, warn_large_data=FALSE)

# set vars -----------------------------------------------------------------
dat_long_colnames <- colnames(dat_long)


# select vars for baseline
baseline_vars <- setdiff(dat_long_colnames, c('id', 'wave'))

# sort
baseline_vars <- sort(baseline_vars)

baseline_vars

# exposure
exposure_vars <- c(nzavs_exposure, "censored") #

# outcome vars

outcome_vars = c("political_conservative", "pol_wing")



# demographic table -------------------------------------------------------


# tables ------------------------------------------------------------------
library(gtsummary)
library(gt)


# REAL tables -----------------------
dt_18 <- dat_long |> 
  dplyr::filter(wave == 2018) |> droplevels() |> ungroup()

# get names
names_base_tab <- setdiff(baseline_vars, dt_18)
names_base_sorted <- sort(names_base_tab)
names_base_final <- c(nzavs_exposure, names_base_sorted)

names_base_final

##
selected_base_cols <- dt_18 %>% select(all_of(names_base_final)) |>  
  dplyr::select(-w_gend_age_ethnic,-religion_religious,
                -censored, -alert_level_combined_lead,-pol_wing, -political_conservative)
str(selected_base_cols)
nrow(selected_base_cols)

colnames(selected_base_cols)

selected_base_cols
# baseline table
selected_base_cols

table_baseline <- selected_base_cols |> 
  janitor::clean_names(case = "title") |> 
  tbl_summary(
    missing = "no",
    percent = "column",
    statistic = list(
      all_continuous() ~ c(
        "{mean} ({sd})", # Mean and SD
        "{min}, {max}", # Range (Min, Max)
        "{p25}, {p75}" # IQR (25th percentile, 75th percentile)
      )
    ),
    type = all_continuous() ~ "continuous2"
  ) |>
  modify_header(label = "**Exposure + Demographic Variables**") %>% # update the column header
  bold_labels() 

# save baseline
here_save(table_baseline, "table_baseline")


# table exposures
# exposure table ----------------------------------------------------------

dt_18_19 <- dat_long_full |> 
  dplyr::filter(wave == 2018 | wave == 2019) |> 
  droplevels()

# Political Orientation
# Please rate how politically liberal versus conservative you see yourself as being.
# Please rate how politically left-wing versus right-wing you see yourself as being.
selected_exposure_cols <-
  dt_18_19 %>% select(
    c(
      "religion_religious",
      "alert_level_combined",
      "wave"
    )
  ) |>
  mutate(religious_affiliation = factor(religion_religious, levels = c(0, 1), labels = c("no", "yes"))) %>%
  select(-religion_religious )


str(selected_exposure_cols)
library(gtsummary)
table_exposures <- selected_exposure_cols %>%
  janitor::clean_names(case = "title") %>% 
  labelled::to_factor() %>%  # ensure consistent use of pipe operator
  tbl_summary(
    by = "Wave",  #specify the grouping variable. Adjust "Wave" to match the cleaned column name
    missing = "always", 
    percent = "column",
    # statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables
  ) %>%
  #  add_n() %>%  # Add column with total number of non-missing observations
  modify_header(label = "**Baseline/Outcome Variables**") %>%  # Update the column header
  bold_labels()

table_exposures


# save baseline
here_save(table_exposures, "table_exposures")
table_exposures <- here_read("table_exposures")

table_exposures



# outcomes
dt_18_20 <- dat_long_full |> 
  dplyr::filter(wave == 2018 | wave == 2020) |> 
  droplevels()

names_outcomes_tab <- setdiff(outcome_vars, dt_18_20)
names_outcomes_sorted <- sort(names_outcomes_tab)
names_outcomes_final <- names_outcomes_sorted # consistent workflow
names_outcomes_final

names_outcomes_final



# names_outcomes_final
# 
selected_outcome_cols <- dt_18_20 %>% select(all_of(names_outcomes_final), 
                                               wave) |> 
  rename(political_right_wing = pol_wing) 
  

str(selected_outcome_cols)
nrow(selected_outcome_cols)

colnames(selected_outcome_cols)

str(selected_outcome_cols)

table_outcomes <- selected_outcome_cols %>%
  janitor::clean_names(case = "title") %>% 
  labelled::to_factor() %>%  # ensure consistent use of pipe operator
  tbl_summary(
    by = "Wave",  #specify the grouping variable. Adjust "Wave" to match the cleaned column name
    missing = "always", 
    percent = "column",
    # statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables
  ) %>%
#  add_n() %>%  # Add column with total number of non-missing observations
  modify_header(label = "**Baseline/Outcome Variables**") %>%  # Update the column header
  bold_labels()

table_outcomes


here_save(table_outcomes, "table_outcomes")
table_outcomes <- here_read("table_outcomes")



# baseline by category: personality 
# 
# table_baseline_personality  <- selected_base_cols %>%
#   janitor::clean_names(case = "title") |> 
#   tbl_summary(include = c(Agreeableness, Conscientiousness, Extraversion, Neuroticism, Openness, "Honesty Humility"),
#               missing = "no", 
#               percent = "column") |> 
#   #  add_n() %>% # add column with total number of non-missing observations
#   statistic = list(all_continuous() ~ "{mean} ({sd})") |>  # Calculate mean and standard deviation for continuous variables
#   modify_header(label = "**Personality Variables**") %>% # update the column header
#   bold_labels() 
# 
# table_baseline_personality
# # save baseline
# here_save(table_baseline_personality, "table_baseline_personality")
# 
# table_baseline_personality




# impute baseline ------------------------------------------------------------------

# check data structure
str(dat_long)

dat_long_df <- data.frame(dat_long)

my_data_filtered <- as.data.frame(dat_long_df)
my_data_filtered <- haven::zap_formats(dat_long_df)
my_data_filtered <- haven::zap_label(dat_long_df)
my_data_filtered <- haven::zap_widths(dat_long_df)
str(my_data_filtered)
# function imputes only baseline not outcome
prep_coop_all <- margot_wide_impute_baseline(
  my_data_filtered,
  baseline_vars = baseline_vars,
  exposure_var = exposure_vars,
  outcome_vars = outcome_vars
)

# check mi model
outlist <- row.names(prep_coop_all)[prep_coop_all$outflux < 0.5]
length(outlist)

# checks. We do not impute with weights: area of current research
head(prep_coop_all$loggedEvents, 10)
str(prep_coop_all)
push_mods
# save function -- will save to your "push_mod" directory
here_save(prep_coop_all, "prep_coop_all")

# read function
prep_coop_all <- here_read("prep_coop_all")

head(prep_coop_all)
naniar::vis_miss(prep_coop_all, warn_large_data = FALSE)
`dev.off()

# prepare for models

df_wide_censored <-
  prep_coop_all |>
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat),
    t0_education_level_coarsen = as.factor(t0_education_level_coarsen)
  ) |>
  relocate("t0_censored", .before = starts_with("t1_"))  %>%
  relocate("t1_censored", .before = starts_with("t2_"))


head(df_wide_censored)
df_wide_censored
# clean -------------------------------------------------------------------
library(dplyr)
str(df_clean)
library(dplyr)
library(dplyr)


dlibrary(dplyr)
library(dplyr)

library(dplyr)


df_clean <- df_wide_censored %>%
  # step 1: adjust t0_censored based on NA in t1_ columns
  mutate(t0_censored = ifelse(rowSums(is.na(
    select(., starts_with("t1_"))
  )) > 0, 0, t0_censored)) %>%
  # step 2: adjust t1_censored based on NA in t2_ columns
  mutate(t1_censored = ifelse(rowSums(is.na(
    select(., starts_with("t2_"))
  )) > 0, 0, t1_censored)) %>%
  # step 3: if t0_censored is 0, set all t1_ and t2_ columns to NA
  mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  # step 4: if t1_censored is 0, set all t2_ columns to NA
  mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .))) |>
  # standardise variables
  mutate(across(
    .cols = where(is.numeric),
    .fns = ~ scale(.),
    .names = "{.col}_z"
  )) %>%
  # select variables
  select(
    where(is.factor),
    t0_censored,
    t0_w_gend_age_ethnic,
    t0_religion_religious,
    t1_censored,
    t1_religion_religious,
    t2_political_conservative,
    t2_pol_wing,
    ends_with("_z"),
    -t0_w_gend_age_ethnic_z,
    -t0_religion_religious_z,
    -t1_religion_religious_z,
    -t0_censored_z,
    -t1_censored_z
  ) %>%
  # make religious numeric
  mutate(
    t0_religion_religious = as.numeric(t0_religion_religious),
    t1_religion_religious = as.numeric(t1_religion_religious)
  ) |>
  # get order in shape it is needed
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))


# save data
here_save(df_clean, "df_clean")

# visualis missingness
naniar::vis_miss(df_clean, warn_large_data = FALSE)

# read data --  start here if previous work already done
df_clean <- here_read("df_clean")

#check n
nrow(df_clean)

colnames(df_clean)
# get names
names_base <- df_clean |> select(starts_with("t0"),
                                 -t0_w_gend_age_ethnic,
                                 -t0_censored,
                                 t0_religion_religious) |> colnames()
# check
names_base

# check outcome names
names_outcomes <- df_clean |> select(starts_with("t2")) |> colnames()
names_outcomes


# exposure_varX1
outcome_vars


#### SET VARIABLE NAMES
#  model
A <- c("t0_religion_religious", "t1_religion_religious")
C <- c("t0_censored", "t1_censored")
W <- c(paste(names_base, collapse = ", "))
W
A

# check
print(W)



# shift all to 1 at baseline, then force deconversion

adjust_vars <- function(data, trt) {
  mtp_base <- function(data, trt) {
    ifelse(data[[trt]] == 0, 1, data[[trt]])
  }
  
  if (trt == "t0_religion_religious") {
    return(mtp_base(data, trt))
  }
  
  mtp_one <- function(data, trt) {
    ifelse(data[[trt]] == 1, 0, data[[trt]])
  }
  
  # Assuming trt is a variable name passed as a string to the function
  
  ifelse(trt == "t1_religion_religious",
         mtp_one(data, trt),
         data[[trt]])
}


# shift all to 1 at baseline, then force maintaining religion

null_shift <- function(data, trt) {
  mtp_base_null <- function(data, trt) {
    ifelse(data[[trt]] == 0, 1, data[[trt]])
  }
  
  if (trt == "t0_religion_religious") {
    return(mtp_base_null(data, trt))
  }
  
  mtp_one_null <- function(data, trt) {
    ifelse(data[[trt]] == 0, 1, data[[trt]])
  }
  
  # Assuming trt is a variable name passed as a string to the function
  
  ifelse(trt == "t1_religion_religious",
         mtp_one_null(data, trt),
         data[[trt]])
}


# shift comparing dones with always not religious 

# shift all to 1 at baseline, then force maintaining religion


contrast_never_religious <- function(data, trt) {

  # dones
  mtp_base_contrast <- function(data, trt) {
    ifelse(data[[trt]] == 1, 0, data[[trt]])
  }
  
  if (trt == "t0_religion_religious") {
    return(mtp_base_contrast(data, trt))
  }
  
  mtp_one_contrast <- function(data, trt) {
    ifelse(data[[trt]] == 1, 0, data[[trt]])
  }
  
  # Assuming trt is a variable name passed as a string to the function
  
  ifelse(trt == "t1_religion_religious",
         mtp_one_contrast(data, trt),
         data[[trt]])
}


# Usage
# new_data <- adjust_religious_variables_base(your_dataframe)




# copy data
delta <- df_clean

# Adjusting everyone to be religious at baseline in delta
delta$t0_religion_religious <-
  ifelse(delta$t0_religion_religious == 0,
         1,
         delta$t0_religion_religious)

# Adjusting everyone to be non-religious at t1 in delta, keeping NA as NA
delta$t1_religion_religious <-
  ifelse(ifelse(
    delta$t1_religion_religious == 1,
    0,
    delta$t1_religion_religious
  ),
  NA)

# Alternatively, we can simplify the t1 adjustment with a direct approach
# This leaves NA values untouched and sets all non-NA values to 0
delta$t0_religion_religious[!is.na(delta$t0_religion_religious)] <-
  1
delta$t1_religion_religious[!is.na(delta$t1_religion_religious)] <-
  0


# check libraries
sl_lib
# check vars
f
A
C

# "SL.earth" refers to a wrapper for the 'earth' function from the 'earth' R package in the SuperLearner library. This function implements Multivariate Adaptive Regression Splines (MARS), a non-parametric regression method that extends linear models by allowing for interactions and non-linear relationships between variables.
# MARS models can handle high-dimensional data well and can be a useful tool for capturing complex patterns in the data. They work by fitting piecewise linear models to the data, which allows for flexible and potentially non-linear relationships between predictors and the outcome.


# super learner libraries
library("SuperLearner")
library("ranger")
library("xgboost")
library("randomForest")


# BONUS: progressr progress bars!
progressr::handlers(global = TRUE)

library(future)
plan(multisession)
n_cores <- parallel::detectCores()

# check whether the dfs are different -------------------------------------

check_column_differences <- function(df1, df2) {
  # Identify columns unique to df1
  unique_to_df1 <- setdiff(names(df1), names(df2))
  
  # Identify columns unique to df2
  unique_to_df2 <- setdiff(names(df2), names(df1))
  
  # Print out the differences
  if (length(unique_to_df1) > 0) {
    cat("Columns unique to the first data frame:\n")
    print(unique_to_df1)
  } else {
    cat("No columns are unique to the first data frame.\n")
  }
  
  if (length(unique_to_df2) > 0) {
    cat("Columns unique to the second data frame:\n")
    print(unique_to_df2)
  } else {
    cat("No columns are unique to the second data frame.\n")
  }
  
  invisible(list(unique_to_df1 = unique_to_df1, unique_to_df2 = unique_to_df2))
}

# usage:
# Assume df1 and df2 are your data frames to compare
differences <- check_column_differences(df_clean, delta)

head(delta)
head(df_clean)

# Assuming df_clean and delta are your data frames
delta <- delta[, names(df_clean)]
delta

delta_rm <- delta |>
  select(-t0_religion_religious,
         -t1_religion_religious,
         -t0_censored,
         -t1_censored)

df_clean_rm <- df_clean |>
  select(-t0_religion_religious,
         -t1_religion_religious,
         -t0_censored,
         -t1_censored)

table(delta_rm == df_clean_rm)

# Ensure both data frames are in the same order of columns
delta_rm <- delta_rm[sort(names(delta_rm))]
df_clean_rm <- df_clean_rm[sort(names(df_clean_rm))]

# Initialize a list to hold names of columns with differences
diff_cols <- list()

# Loop through each column to check for differences
for (col in names(delta_rm)) {
  # Check if all values in the current column are the same between the two data frames
  if (!all(delta_rm[[col]] == df_clean_rm[[col]], na.rm = TRUE)) {
    # Save the column name if differences are found
    diff_cols[[length(diff_cols) + 1]] <- col
  }
}

# Print the names of columns with differences
diff_cols


# remove attributes:
df_clean <- data.frame(lapply(df_clean, function(x) {
  # Check if it's a numeric vector with attributes to remove
  if (is.numeric(x) && length(attributes(x)) > 1) {
    # Remove attributes by converting to a vector
    x <- as.vector(x)
  }
  return(x)
}))


delta <- data.frame(lapply(delta, function(x) {
  # Check if it's a numeric vector with attributes to remove
  if (is.numeric(x) && length(attributes(x)) > 1) {
    # Remove attributes by converting to a vector
    x <- as.vector(x)
  }
  return(x)
}))


# test data
df_clean_slice <- df_clean |>
  slice_head(n = 500) |>
  as.data.frame()
str(df_clean_test)

delta_test <- delta |>
  slice_head(n = 500) |>
  as.data.frame()

# checks
A <-
  c("t0_religion_religious", "t1_religion_religious") # Assuming these are your treatment columns
C <-
  c("t0_censored", "t1_censored") # Assuming these are your censoring columns

# All other columns that should match between df_clean_test and delta_test
cols_to_check <-
  setdiff(intersect(names(df_clean_test), names(delta_test)), c(A, C))
cols_to_check

for (col in cols_to_check) {
  if (!all(df_clean_test[[col]] == delta_test[[col]], na.rm = TRUE)) {
    cat("Difference found in column:", col, "\n")
  }
}
# Replace `column_name` with the actual name of the column where differences were found
column_name <- "example_column"
indices_diff <-
  which(df_clean_test[[column_name]] != delta_test[[column_name]])
head(indices_diff) # Display the first few indices where differences occur

# further tests
# Assuming df_clean_test and delta_test have the same columns in the same order
# If not, you should align them first

# Get the names of columns present in both data frames
common_cols <- intersect(names(df_clean_test), names(delta_test))

# Initialize an empty vector to store the names of columns with different classes
diff_class_cols <- character()

# Iterate over the common columns and compare their classes
for (col in common_cols) {
  class_df_clean_test <- class(df_clean_test[[col]])
  class_delta_test <- class(delta_test[[col]])
  
  if (!identical(class_df_clean_test, class_delta_test)) {
    # If the classes differ, save the column name
    diff_class_cols <- c(diff_class_cols, col)
  }
}

# Print columns with different classes
print(diff_class_cols)

# Optionally, print the classes of the differing columns for inspection
if (length(diff_class_cols) > 0) {
  sapply(df_clean_test[diff_class_cols], class)
  sapply(delta_test[diff_class_cols], class)
}


# measure time taken to run the model

library(testthat)
test_conditions <- function(data) {
  # Identifying columns
  t1_cols <- grep("^t1_", names(data), value = TRUE)
  t2_cols <- grep("^t2_", names(data), value = TRUE)
  
  # Test Condition 1: If any variable starting with t2_ is NA, then t1_censored must be 0 or NA
  for (col in t2_cols) {
    if (any(is.na(data[[col]]))) {
      incorrect_indices <-
        which(!data$t1_censored %in% c(0, NA) & is.na(data[[col]]))
      if (length(incorrect_indices) > 0) {
        stop(
          paste(
            "Failure: If any variable starting with t2_ is NA, then t1_censored must be 0 or NA. Found in rows:",
            paste(incorrect_indices, collapse = ", ")
          )
        )
      }
    }
  }
  
  # Test Condition 2 and others as before...
  
  return("All conditions are met.")
}

#  usage:
result <- test_conditions(df_clean)
print(result)

adjust_vars



# models ------------------------------------------------------------------


t2_political_conservative_z <- lmtp_sdr(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_political_conservative_z",
  cens = C,
  shift = adjust_vars,
  # shifted data
  mtp = TRUE,
  folds = 10,
  # k = 1,
  outcome_type = "continuous",
  weights = df_clean$t0_w_gend_age_ethnic,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)


here_save(t2_political_conservative_z, "t2_political_conservative_z")

t2_political_conservative_z_null <- lmtp_sdr(
  data = df_clean,
  trt = c("t0_religion_religious", "t1_religion_religious"),
  baseline = names_base,
  outcome = "t2_political_conservative_z",
  cens = c("t0_censored", "t1_censored"),
  shift = null_shift,
  mtp = TRUE,
  folds = 10,
  # k = 1,
  outcome_type = "continuous",
  weights = df_clean$t0_w_gend_age_ethnic,
  learners_trt =  "SL.ranger",
  learners_outcome =  "SL.ranger",
  parallel = n_cores
)

here_save(t2_political_conservative_z_null,
          "t2_political_conservative_z_null")



t2_political_conservative_z_contrast <- lmtp_sdr(
  data = df_clean,
  trt = c("t0_religion_religious", "t1_religion_religious"),
  baseline = names_base,
  outcome = "t2_political_conservative_z",
  cens = c("t0_censored", "t1_censored"),
  shift = contrast_never_religious,
  mtp = TRUE,
  folds = 10,
  # k = 1,
  outcome_type = "continuous",
  weights = df_clean$t0_w_gend_age_ethnic,
  learners_trt =  "SL.ranger",
  learners_outcome =  "SL.ranger",
  parallel = n_cores
)

here_save(t2_political_conservative_z_contrast,
          "t2_political_conservative_z_contrast")


# t2_political_conservative_z<- here_read("t2_political_conservative_z")

contrast_t2_political_conservative_z <-
  lmtp_contrast(t2_political_conservative_z,
                ref = t2_political_conservative_z_null, type = "additive")
contrast_t2_political_conservative_z

contrast_t2_political_conservative_contrast <-
  lmtp_contrast(t2_political_conservative_z,
                ref = t2_political_conservative_z_contrast, type = "additive")
contrast_t2_political_conservative_contrast

# model right wing --------------------------------------------------------
t2_pol_wing_z <- lmtp_sdr(
  data = df_clean,
  trt = c("t0_religion_religious", "t1_religion_religious"),
  baseline = names_base,
  outcome = "t2_political_conservative_z",
  cens = c("t0_censored", "t1_censored"),
  shift = adjust_vars,
  # shifted data
  mtp = TRUE,
  folds = 10,
  # k = 1,
  outcome_type = "continuous",
  weights = df_clean$t0_w_gend_age_ethnic,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)


here_save(t2_pol_wing_z, "t2_pol_wing_z")

t2_pol_wing_z_null <- lmtp_sdr(
  data = df_clean,
  trt = c("t0_religion_religious", "t1_religion_religious"),
  baseline = names_base,
  outcome = "t2_pol_wing_z",
  cens = c("t0_censored", "t1_censored"),
  shift = null_shift,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_w_gend_age_ethnic,
  learners_trt =  "SL.ranger",
  learners_outcome =  "SL.ranger",
  parallel = n_cores
)

here_save(t2_pol_wing_z_null, "t2_pol_wing_z_null")


t2_pol_wing_z_contrast <- lmtp_sdr(
  data = df_clean,
  trt = c("t0_religion_religious", "t1_religion_religious"),
  baseline = names_base,
  outcome = "t2_pol_wing_z",
  cens = c("t0_censored", "t1_censored"),
  shift = contrast_never_religious,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_w_gend_age_ethnic,
  learners_trt =  "SL.ranger",
  learners_outcome =  "SL.ranger",
  parallel = n_cores
)

here_save(t2_pol_wing_z_contrast, "t2_pol_wing_z_contrast")


contrast_t2_pol_wing_z <-
  lmtp_contrast(t2_pol_wing_z, ref = t2_pol_wing_z_null, type = "additive")
contrast_t2_pol_wing_z


contrast_t2_pol_wing_contrast <-
  lmtp_contrast(t2_pol_wing_z, ref = t2_pol_wing_z_contrast, type = "additive")
contrast_t2_pol_wing_contrast




###########################################################################
# only religious  ---------------------------------------------------------
###########################################################################
df_clean<- here_read("df_clean")
df_clean_religious_only <-  df_clean |> 
  dplyr::filter(t0_religion_religious == 1)




dt_positivity_att <- dat_long |>
  filter(wave == 2018 & religion_religious == 1| wave == 2019) |>
  select(wave, id, religion_religious)


# create transition matrix
out_pos_att <- msm::statetable.msm(religion_religious,
                               id,
                               data = dt_positivity_att)

cats_labels <- c("Religious", "Not Religious")

transition_table_att  <- margot::transition_table(out_pos_att)
transition_table_att
here_save(transition_table_att, "transition_table_att")


# check rows  n = 17141
n_participants_att <- nrow(df_clean_religious_only)

here_save(n_participants_att, "n_participants_att")

t2_political_conservative_z_rels <- lmtp_sdr(
  data = df_clean_religious_only,
  trt = A,
  baseline = names_base,
  outcome = "t2_political_conservative_z",
  cens = C,
  shift = adjust_vars,
  # shifted data
  mtp = TRUE,
  folds = 10,
  # k = 1,
  #  shift = NULL,
  outcome_type = "continuous",
  weights = df_clean_religious_only$t0_w_gend_age_ethnic,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)


here_save(t2_political_conservative_z_rels, "t2_political_conservative_z_rels")

t2_political_conservative_z_null_rels <- lmtp_sdr(
  data = df_clean_religious_only,
  trt = c("t0_religion_religious", "t1_religion_religious"),
  baseline = names_base,
  outcome = "t2_political_conservative_z",
  cens = c("t0_censored", "t1_censored"),
  shift = null_shift,
  # shifted data
  mtp = TRUE,
  folds = 10,
  # k = 1,
  outcome_type = "continuous",
  weights = df_clean_religious_only$t0_w_gend_age_ethnic,
  learners_trt =  "SL.ranger",
  learners_outcome =  "SL.ranger",
  parallel = n_cores
)

here_save(t2_political_conservative_z_null_rels,
          "t2_political_conservative_z_null_rels")



contrast_t2_political_conservative_z_rels <-
  lmtp_contrast(t2_political_conservative_z_rels,
                ref = t2_political_conservative_z_null_rels, type = "additive")
contrast_t2_political_conservative_z_rels



# model right wing --------------------------------------------------------
t2_pol_wing_z_rels <- lmtp_sdr(
  data = df_clean_religious_only,
  trt = c("t0_religion_religious", "t1_religion_religious"),
  baseline = names_base,
  outcome = "t2_political_conservative_z",
  cens = c("t0_censored", "t1_censored"),
  shift = adjust_vars,
  # shifted data
  mtp = TRUE,
  folds = 10,
  # k = 1,
  outcome_type = "continuous",
  weights = df_clean_religious_only$t0_w_gend_age_ethnic,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)


here_save(t2_pol_wing_z_rels, "t2_pol_wing_z_rels")

t2_pol_wing_z_null_rels <- lmtp_sdr(
  data = df_clean_religious_only,
  trt = c("t0_religion_religious", "t1_religion_religious"),
  baseline = names_base,
  outcome = "t2_pol_wing_z",
  cens = c("t0_censored", "t1_censored"),
  shift = null_shift,
  mtp = TRUE,
  folds = 10,
  # k = 1,
  outcome_type = "continuous",
  weights = df_clean_religious_only$t0_w_gend_age_ethnic,
  learners_trt =  "SL.ranger",
  learners_outcome =  "SL.ranger",
  parallel = n_cores
)

here_save(t2_pol_wing_z_null_rels, "t2_pol_wing_z_null_rels")



contrast_t2_pol_wing_z_rels <-
  lmtp_contrast(t2_pol_wing_z_rels, ref = t2_pol_wing_z_null_rels, type = "additive")
contrast_t2_pol_wing_z_rels




# results -----------------------------------------------------------------
push_mods
# full sample - conservative
t2_political_conservative_z <- here_read("t2_political_conservative_z")
t2_political_conservative_z_null <- here_read("t2_political_conservative_z_null")
t2_political_conservative_z_contrast <- here_read("t2_political_conservative_z_contrast")



contrast_t2_political_conservative_z <-
  lmtp_contrast(t2_political_conservative_z,
                ref = t2_political_conservative_z_null, type = "additive")
contrast_t2_political_conservative_z


tab_contrast_t2_political_conservative_z <- margot_tab_lmtp(contrast_t2_political_conservative_z, scale = "RD", 
                                                            new_name = "Dis-affiliation Effect: Pol.Conservative")
output_tab_contrast_t2_political_conservative_z <- lmtp_evalue_tab(tab_contrast_t2_political_conservative_z,  delta = 1, sd = 1, scale = c("RD"))
output_tab_contrast_t2_political_conservative_z

# check
tab_contrast_t2_political_conservative_z
output_tab_contrast_t2_political_conservative_z

# save table and output table
here_save(tab_contrast_t2_political_conservative_z, "tab_contrast_t2_political_conservative_z")
here_save(output_tab_contrast_t2_political_conservative_z, "output_tab_contrast_t2_political_conservative_z")


# full sample - political wing
t2_pol_wing_z <- here_read("t2_pol_wing_z")
t2_pol_wing_z_null <- here_read("t2_pol_wing_z_null")
t2_pol_wing_z_contrast <- here_read("t2_pol_wing_z_contrast")



contrast_t2_pol_wing_z <-
  lmtp_contrast(t2_pol_wing_z, ref = t2_pol_wing_z_null, type = "additive")
contrast_t2_pol_wing_z

tab_contrast_t2_pol_wing_z <- margot_tab_lmtp(contrast_t2_pol_wing_z, scale = "RD",
                                                            new_name = "Dis-affiliation Effect: Right Wing")
output_tab_contrast_t2_pol_wing_z <- lmtp_evalue_tab(tab_contrast_t2_pol_wing_z,  delta = 1, sd = 1, scale = c("RD"))


# check
tab_contrast_t2_pol_wing_z
output_tab_contrast_t2_pol_wing_z
# save table and output table
here_save(tab_contrast_t2_pol_wing_z, "tab_contrast_t2_pol_wing_z")
here_save(output_tab_contrast_t2_pol_wing_z, "output_tab_contrast_t2_pol_wing_z")

tab_full <- rbind(output_tab_contrast_t2_political_conservative_z, output_tab_contrast_t2_pol_wing_z)
group_tab_full <- group_tab(tab_full, type = "RD")
group_tab_full

here_save(tab_full, "tab_full")
here_save(group_tab_full, "group_tab_full")

# religious at baseline sample 
# pols
t2_political_conservative_z_rels <- here_read("t2_political_conservative_z_rels")
t2_political_conservative_z_null_rels <- here_read("t2_political_conservative_z_null_rels")
contrast_t2_political_conservative_z_rels <-
  lmtp_contrast(t2_political_conservative_z_rels,
                ref = t2_political_conservative_z_null_rels, type = "additive")
tab_contrast_t2_political_conservative_z_rels <- margot_tab_lmtp(contrast_t2_political_conservative_z_rels, scale = "RD", 
                                                                 new_name = "Dis-affiliation Effect (CATE): Pol.Conservative")
output_tab_contrast_t2_political_conservative_z_rels <- lmtp_evalue_tab(tab_contrast_t2_political_conservative_z_rels,  delta = 1, sd = 1, scale = c("RD"))
output_tab_contrast_t2_political_conservative_z_rels

# check
tab_contrast_t2_political_conservative_z_rels
output_tab_contrast_t2_political_conservative_z_rels

# save table and output table
here_save(tab_contrast_t2_political_conservative_z_rels, "tab_contrast_t2_political_conservative_z_rels")
here_save(output_tab_contrast_t2_political_conservative_z_rels, "output_tab_contrast_t2_political_conservative_z_rels")

# wing
t2_pol_wing_z_rels <- here_read("t2_pol_wing_z_rels")
t2_pol_wing_z_null_rels <- here_read("t2_pol_wing_z_null_rels")
contrast_t2_pol_wing_z_rels <-
  lmtp_contrast(t2_pol_wing_z_rels, ref = t2_pol_wing_z_null_rels, type = "additive")
contrast_t2_pol_wing_z_rels


tab_contrast_t2_pol_wing_z_rels <- margot_tab_lmtp(contrast_t2_pol_wing_z_rels, scale = "RD",
                                              new_name = "Dis-affiliation Effect (CATE): Right Wing")
output_tab_contrast_t2_pol_wing_z_rels <- lmtp_evalue_tab(tab_contrast_t2_pol_wing_z_rels,  delta = 1, sd = 1, scale = c("RD"))
output_tab_contrast_t2_pol_wing_z_rels

# check
tab_contrast_t2_pol_wing_z_rels
output_tab_contrast_t2_pol_wing_z_rels

# save table and output table
here_save(tab_contrast_t2_pol_wing_z_rels, "tab_contrast_t2_pol_wing_z_rels")
here_save(output_tab_contrast_t2_pol_wing_z_rels, "output_tab_contrast_t2_pol_wing_z_rels")

# Graphs 

tab_att <- rbind(output_tab_contrast_t2_political_conservative_z_rels, output_tab_contrast_t2_pol_wing_z_rels)
group_tab_att <- group_tab(tab_att, type = "RD")
group_tab_att

here_save(tab_att, "tab_att")
here_save(group_tab_att, "group_tab_att")



### Second contrast


contrast_t2_pol_wing_contrast <-
  lmtp_contrast(t2_pol_wing_z, ref = t2_pol_wing_z_contrast, type = "additive")
contrast_t2_pol_wing_contrast

tab_contrast_t2_pol_wing_contrast <- margot_tab_lmtp(contrast_t2_pol_wing_contrast, scale = "RD",
                                                   new_name = "Dis-affiliation Contrast (ATE): Right Wing")
tab_contrast_t2_pol_wing_contrast
output_tab_contrast_t2_pol_wing_contrast <- lmtp_evalue_tab(tab_contrast_t2_pol_wing_contrast,  delta = 1, sd = 1, scale = c("RD"))
output_tab_contrast_t2_pol_wing_contrast


# pol wing
contrast_t2_political_conservative_contrast <-
  lmtp_contrast(t2_political_conservative_z,
                ref = t2_political_conservative_z_contrast, type = "additive")
contrast_t2_political_conservative_contrast


tab_contrast_t2_political_conservative_contrast <- margot_tab_lmtp(contrast_t2_political_conservative_contrast, scale = "RD",
                                                     new_name = "Dis-affiliation Contrast (ATE): Pol.Conservative")
tab_contrast_t2_political_conservative_contrast
output_tab_contrast_t2_political_conservative_contrast <- lmtp_evalue_tab(tab_contrast_t2_political_conservative_contrast,  delta = 1, sd = 1, scale = c("RD"))
output_tab_contrast_t2_political_conservative_contrast

output_tab_contrast_t2_political_conservative_contrast
output_tab_contrast_t2_pol_wing_contrast
tab_contrast <- rbind(output_tab_contrast_t2_political_conservative_contrast, output_tab_contrast_t2_pol_wing_contrast)
tab_contrast

group_tab_contrast <- group_tab(tab_contrast, type = "RD")
group_tab_contrast


# margot_interpret_table <- function(df, causal_scale, estimand) {
#   estimand_description <- dplyr::case_when(
#     estimand == "PATE" ~ "The Population Average Treatment Effect (PATE) represents the expected difference in outcomes between treatment and control groups for the New Zealand population.",
#     estimand == "ATE" ~ "The Average Treatment Effect (ATE) represents the expected difference in outcomes between treatment and control groups for the population.",
#     estimand == "ATT" ~ "Average Treatment Effect (ATT) represents the expected difference in outcomes between treatment and control groups for the treated population.",
#     TRUE ~ "The specified estimand is not recognized. Please use one of the following: 'PATE', 'ATE', 'ATT'."
#   )
#   
#   interpretation <- df %>%
#     dplyr::mutate(
#       causal_contrast = dplyr::case_when(
#         causal_scale == "causal_difference" ~ round(`E[Y(1)]-E[Y(0)]`, 2),
#         TRUE ~ NA_real_  # Placeholder, adjust as needed if adding other scales
#       ),
#       E_Value = round(E_Value, 2),
#       E_Val_bound = round(E_Val_bound, 2),
#       `2.5 %` = round(`2.5 %`, 2),
#       `97.5 %` = round(`97.5 %`, 2)
#     ) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(
#       strength_of_evidence = dplyr::case_when(
#         E_Val_bound == 1 ~ "no reliable evidence for causality",
#         E_Val_bound <= 1 | (`2.5 %` <= 0 & `97.5 %` >= 0) ~ "no reliable evidence for causality",
#         E_Val_bound > 1 & E_Val_bound < 1.1 ~ "the evidence for causality is weak",
#         E_Val_bound > 2 ~ "strong evidence for causality",
#         TRUE ~ "evidence for causality"
#       ),
#       outcome_interpretation = if_else(E_Val_bound == 1, 
#                                        glue::glue("For the outcome '{outcome}', given the lower bound of the E-value equals 1, we find no reliable evidence for causality."),
#                                        glue::glue(
#                                          "For the outcome '{outcome}', the {estimand} is {causal_contrast} [{`2.5 %`},{`97.5 %`}]. ",
#                                          "The E-value for this effect estimate is {E_Value} ",
#                                          "with a lower bound of {E_Val_bound}. At this bound, an unmeasured confounder associated with both the treatment and outcome by a risk ratio of {E_Val_bound} each could explain away the observed effect; weaker confounding would not. ",
#                                          "Overall, we find {strength_of_evidence}."
#                                        )
#       )
#     ) %>%
#     dplyr::ungroup()
#   
#   result <- glue::glue(
#     "\n\n{estimand_description}\n\n{paste(interpretation$outcome_interpretation, collapse = '\n\n')}"
#   )
#   return(result)
# }

margot_interpret_table( group_tab_contrast, "causal_difference", "ATE" )

here_save(tab_contrast, "tab_contrast")
here_save(group_tab_contrast, "group_tab_contrast")





# check associations ------------------------------------------------------
dt_lm <- dat_long |>
  mutate(political_conservative_z = scale(political_conservative),
         pol_wing_z = scale(pol_wing),
         year = as.numeric((wave))-1
  ) |> 
  arrange(id, year) |> droplevels()

library(lme4)
# check association only
fit_conservative <- lm(political_conservative_z ~ religion_religious + year, data = dt_lm)
fit_conservative

fit_pol_wing <- lm(pol_wing_z ~ religion_religious + year, data = dt_lm)


here_save(fit_conservative, "fit_conservative")
here_save(fit_pol_wing, "fit_pol_wing")


df_clean_2 <-df_clean  
df_clean_2 <- data.frame(lapply(df_clean_2, function(x) {
  # Check if it's a numeric vector with attributes to remove
  if (is.numeric(x) && length(attributes(x)) > 1) {
    # Remove attributes by converting to a vector
    x <- as.vector(x)
  }
  return(x)
}))
  
model_parameters( 
 fit_wing <-  lm(t2_pol_wing_z ~ t1_religion_religious * t0_religion_religious + as.numeric(t0_pol_wing_z), data = df_clean) 
  )

model_parameters( 
  fit_wing <-  lm(t2_pol_wing_z ~ t1_religion_religious * t0_religion_religious, data = df_clean) 
)


plot ( 
  ggeffects::ggpredict( fit_wing, terms = c("t1_religion_religious", "t0_religion_religious"))
)




model_parameters( 
  fit_conservative <-  lm(t2_political_conservative_z ~ t1_religion_religious * t0_religion_religious + as.numeric(t0_pol_wing_z), data = df_clean) 
)

model_parameters( 
  fit_conservative <-  lm(t2_political_conservative_z ~ t1_religion_religious * t0_religion_religious, data = df_clean) 
)


plot ( 
  ggeffects::ggpredict( fit_wing, terms = c("t1_religion_religious", "t0_religion_religious"))
)

