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
dat_long <- dat %>%
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
    "alert_level_combined_lead"
  ) %>%
  mutate(
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1),
    household_inc_log = log(household_inc + 1),
    hours_charity_log = log(hours_charity + 1),
    hours_exercise_log = log(hours_exercise + 1),
    hours_children_log = log(hours_children + 1)
  ) |>
  select(-c(
    hours_work,
    hours_housework,
    household_inc,
    hours_charity,
    hours_exercise
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


str(dat$sample_origin_year)
str(dat_long)
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

transition_table  <- transition_table(out_pos, cats_labels)
transition_table
here_save(transition_table, "transition_table")

# set vars -----------------------------------------------------------------
dat_long_colnames <- colnames(dat_long)

# select vars for baseline
baseline_vars <- setdiff(dat_long_colnames, c('id', 'wave'))

# sort
baseline_vars <- sort(baseline_vars)


# exposure
exposure_vars <- c(nzavs_exposure, "censored") #

# outcome vars

outcome_vars = c("political_conservative", "pol_wing")



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
dev.off()

# prepare for models

df_wide_censored <-
  prep_coop_all |>
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat),
    t0_education_level_coarsen = as.factor(t0_education_level_coarsen)
  ) |>
  relocate("t0_censored", .before = starts_with("t1_"))  %>%
  relocate("t1_censored", .before = starts_with("t2_"))




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
  k = 1,
  #  shift = NULL,
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
  # shifted data
  mtp = TRUE,
  folds = 10,
  k = 1,
  outcome_type = "continuous",
  weights = df_clean$t0_w_gend_age_ethnic,
  learners_trt =  "SL.ranger",
  learners_outcome =  "SL.ranger",
  parallel = n_cores
)

here_save(t2_political_conservative_z_null,
          "t2_political_conservative_z_null")



contrast_t2_political_conservative_z <-
  lmtp_contrast(t2_political_conservative_z,
                ref = t2_political_conservative_z_null, type = "additive")

contrast_t2_political_conservative_z



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
  k = 1,
  #  shift = NULL,
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
  # shifted data
  mtp = TRUE,
  folds = 10,
  k = 1,
  outcome_type = "continuous",
  weights = df_clean$t0_w_gend_age_ethnic,
  learners_trt =  "SL.ranger",
  learners_outcome =  "SL.ranger",
  parallel = n_cores
)

here_save(t2_pol_wing_z_null, "t2_pol_wing_z_null")



contrast_t2_pol_wing_z <-
  lmtp_contrast(t2_pol_wing_z, ref = t2_pol_wing_z_null, type = "additive")
contrast_t2_pol_wing_z

###########################################################################
# only religious  ---------------------------------------------------------
###########################################################################

df_clean_religious_only <-  df_clean |> 
  dplyr::filter(t0_religion_religious == 1)

# check rows  n = 17141
nrow(df_clean_religious_only)


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
  k = 1,
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
  k = 1,
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
  k = 1,
  #  shift = NULL,
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
  # shifted data
  mtp = TRUE,
  folds = 10,
  k = 1,
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




# check associations ------------------------------------------------------
dt_18 <- dat_long |>
  filter(wave == 2018)

# check association only
summary(lm(political_conservative ~ religion_religious, data = dt_18))
summary(lm(pol_wing ~ religion_religious, data = dt_18))
