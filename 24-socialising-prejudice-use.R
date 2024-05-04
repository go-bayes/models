# v5-lmtp-ow-prejudice-church.R
# joseph.bulbulia@gmail.com
# april 7 2024
# no mising vals at baseline

# original script is in the 00drafts folder.
# this script brings the analysis for this study to the 'models" workflow

### ALWAYS RESTART R IN A FRESH SESSION ####
devtools::install_github("go-bayes/margot")
devtools::install_github("nt-williams/lmtp@devel")


# directory
push_mods <-  fs::path_expand(
  "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/24-socialising-prejudice"
)

# check path:is this correct?  check so you know you are not overwriting other directors
push_mods


#devtools::install_github("go-bayes/margot")
library(margot)
library(ggplot2)
library(skimr)
library(tidyverse)
library(margot)
library(SuperLearner)
library(lmtp)
library(cobalt)
library(WeightIt)
library(MatchThem)
library(future)
library(ranger)
library(xgboost)
library(lmtp)
library(rlang)
library(doParallel)

## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE.
pull_path <-
  fs::path_expand(
    #"/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs_refactor/nzavs_data_23"
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data_qs"
  )

# read data: note that you need use the arrow package in R
dat <- qs::qread(here::here(pull_path))

# total nzavs participants
n_total <- skimr::n_unique(dat$id)

# get comma in number
n_total <- prettyNum(n_total,big.mark=",")

# check
n_total

# save
margot::here_save(n_total, "n_total")

# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 10

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(0112358)
library(future)
library(SuperLearner)
plan(multisession)

#listWrappers()
exposure_var_string <- "hours_community_round"


# super learner libraries
sl_lib <- c("SL.ranger", "SL.glmnet", "SL.xgboost")


plan(multisession)
n_cores = parallel::detectCores() -2 

!!sym(exposure_var_string)
name_exposure_raw <- "hours_community"
# Obtain IDs for individuals who participated in 2018 and have no missing baseline exposure

# Obtain IDs for individuals who participated in 2018 and have no missing baseline exposure
ids_2018 <- dat %>%
  dplyr::filter(year_measured == 1, wave == 2018) %>%
  dplyr::filter(!is.na(!!sym(name_exposure_raw))) |> # criteria, no missing
  pull(id)

# Obtain IDs for individuals who participated in 2019/ NOT USED
# ids_2019 <- dat %>%
#   dplyr::filter(year_measured == 1, wave == 2019) %>%
#   dplyr::filter(!is.na(!!sym(name_exposure_raw))) |> # criteria, no missing
#   pull(id)
# 
# # Intersect IDs from 2018 and 2019 to ensure participation in both years
# ids_2018_2019 <- intersect(ids_2018, ids_2019) # not used 



# filter the original dataset for these IDs three waves
dat <- as.data.frame(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_widths(dat)
str(dat)


dat_long <- dat |>
  # dplyr::filter(id %in% ids_2018_2019 &
  #                 wave %in% c(2018, 2019, 2020)) |>
  dplyr::filter(id %in% ids_2018 &
                  wave %in% c(2018, 2019, 2020)) |>
  arrange(id, wave)|>
  select(
    "id",
    "wave",
    "year_measured",
    # "edu",
    "religion_church",
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
    "sample_origin_names_combined",
    "education_level_coarsen",
    # "alert_level_combined_lead",
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
    #    "pol_wing",
    # Please rate how politically left-wing versus right-wing you see yourself as being.
    "sample_frame_opt_in",
    # see NZAVS,
    "has_siblings",
    #Do you have siblings?
    # "total_siblings",
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
    "hours_community",
    # #Hours spent … socialising with friends
    # #Hours spent … socialising with community groups
    # #Hours spent … socialising with family
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
    #  "w_gend_age_ethnic",
    # perc_age_discrim,
    # "perc_gend_discrim",
    # "perc_religious_discrim",
    # "perc_discrim",
    #"neighbourhood_community",
    # #I feel a sense of community with others in my local neighbourhood.
    #"support",
    # "belong",
    "rural_gch_2018_l",
    # "hlth_disability",
    # value label 0    No 1   Yes    # see NZAVS,
    #"children_num",
    # How many children have you given birth to, fathered, or adopted?
    #"hours_children",
    # "hours_community",
    # "hours_friends",
    #"hours_family",
    #Hours - Looking after children
    # "religion_perceive_religious_discrim",
    #	I feel that I am often discriminated against because of my religious/spiritual beliefs.
    # "bigger_doms", #What religion or spiritual group?#  Not_Rel, Anglican , Buddist, Catholic , Christian_nfd, Christian_Others, Hindu, Jewish           Muslim, PresbyCongReform, TheOthers
    "modesty",
    # see above
    #"charity_donate",
    #How much money have you donated to charity in the last year?
    #"hours_charity",
    #,#Hours spent in activities/Hours spent … voluntary/charitable work
    "warm_asians",
    "warm_chinese",
    # #"warm_disabled" ,  missing at time 0
    # # begins w9
    "warm_immigrants",
    "warm_indians",
    "warm_elderly",
    # warm_lgbtq starts w12
    "warm_maori",
    "warm_mental_illness",
    "warm_muslims",
    "warm_nz_euro",
    "warm_overweight",
    "warm_pacific",
    "warm_refugees",
    # "issue_same_sex_marriage", not in range
    #"support",
    # three items as below
    # "support_help",
    # # 'There are people I can depend on to help me if I really need it.
    # "support_turnto",
    # # There is no one I can turn to for guidance in times of stress.
    # "support_rnoguidance",
    #There is no one I can turn to for guidance in times of stress.
    # "family_time",
    # "friends_time",
    # "community_time",
    # "family_money",
    # "friends_money",
    # "community_money",
    #Please estimate how much help you have received from the following sources in the last week?
    # Received help and support - hours
    # family
    # friends
    # others in my community
    # Received help and support - money
    # family
    # friends
    # others in my community
    # outcomewide,
    #w8,w10,w12-13 "I identify as a spiritual person."
    #   "religion_perceive_religious_discrim",
    #	I feel that I am often discriminated against because of my religious/spiritual beliefs.
    # "bigger_doms", #What religion or spiritual group?#  Not_Rel, Anglican , Buddist, Catholic , Christian_nfd, Christian_Others, Hindu, Jewish           Muslim, PresbyCongReform, TheOthers
    # sample_weights
    #  "alcohol_frequency",
    #"How often do you have a drink containing alcohol?"
    # "alcohol_intensity",
    # How many drinks containing alcohol do you have on a typical day when drinking?
    # "hlth_bmi",
    # " What is your height? (metres)\nWhat is your weight? (kg)\nKg
    "sfhealth",
    #"sfhealth_your_health",
    # # "In general, would you say your health is...
    # "sfhealth_get_sick_easier_reversed",
    #\nI seem to get sick a little easier than other people.
    # "sfhealth_expect_worse_health_reversed",
    #\nI expect my health to get worse." ****
    # "hlth_sleep_hours",
    #During the past month, on average, how many hours of actual sleep did you get per night?
    #  "smoker",
    #Do you currently smoke?
    #  "hlth_fatigue",
    #During the last 30 days, how often did.... you feel exhausted?
    # "rumination",
    # "kessler6_sum",
    "kessler_latent_depression",
    "kessler_latent_anxiety",
    "hlth_fatigue",
    "hlth_sleep_hours",
    "hlth_disability",
    #"support",
    #   "support_help",
    # 'There are people I can depend on to help me if I really need it.
    #  "support_turnto",
    # There is no one I can turn to for guidance in times of stress.
    #  "support_noguidance_reverseed",
    #There is no one I can turn to for guidance in times of stress.
    #"belong",
    #    "belong_accept",
    #Know that people in my life accept and value me.
    #    "belong_routside_reversed",
    # Feel like an outsider.
    #  "belong_beliefs",
    # Know that people around me share my attitudes and beliefs.
    #"family_money",
    #"friends_money",
    # "community_money",
    #"alert_level_combined_lead",
    "alert_level_combined",
    "alert_level_combined_lead"
  )|>
  mutate(religion_church_round = round(ifelse(religion_church >= 8, 8, religion_church), 0)) |>
  mutate(hours_community_round = round(ifelse(hours_community >= 24, 24, hours_community), 0)) |>
  mutate(
    #initialize 'censored'
    censored = ifelse(lead(year_measured) == 1, 1, 0),
    
    # modify 'censored' based on the condition; no need to check for NA here as 'censored' is already defined in the previous step
    censored =  ifelse(is.na(censored) &
                         year_measured == 1, 1, censored)
    
    # # Apply the case_when condition for setting 'censored' based on 'wave' and the dynamic column specified by 'nzavs_exposure'
    # censored = case_when(
    #   # Add this condition to keep previous modifications unless the specific condition is met!is.na(censored) ~ censored,
    #
    #   # Then check if 'wave' is 2019 and the specified exposure is NA, adjusting the condition to reflect the accurate logic
    #   wave == 2019 & !is.na(!!sym(nzavs_exposure)) ~ 1,
    #
    #   # Default case if none of the above apply; might not be necessary if all possibilities are covered
    #   TRUE ~ 0
    # )
  ) |>
  select(-year_measured) |>
  ungroup() |>
  dplyr::mutate(
    #  friends_money = ifelse(friends_money < 0, 0, friends_money),
    # someone gave neg number
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1),
    household_inc_log = log(household_inc + 1),
    #  hours_charity_log = log(hours_charity + 1),
    hours_exercise_log = log(hours_exercise + 1),
    hours_children_log = log(hours_children + 1),
    # total_siblings_log = log(total_siblings + 1),
    # hours_community_log = log(hours_community + 1),
    # hours_friends_log  = log(hours_friends + 1),
    # hours_family_log = log(hours_family + 1)#,
    # children_num_log = log(children_num + 1)
  ) |>
  dplyr::select(
    -c(
      religion_church,
      hours_work,
      hours_housework,
      household_inc,
      hours_exercise,
      hours_community,
      hours_children)
  ) |>
  droplevels() |>
  dplyr::rename(
    sample_origin =  sample_origin_names_combined,
    short_form_health = sfhealth) |>
  # dplyr::mutate(
  #   # make indicators binary
  #   family_time_binary = as.integer(ifelse(family_time > 0, 1, 0)),
  #   friends_time_binary = as.integer(ifelse(friends_time > 0, 1, 0)),
  #   community_time_binary = as.integer(ifelse(community_time > 0, 1, 0)),
  #   family_money_binary = as.integer(ifelse(family_money> 0, 1, 0)),
  #   friends_money_binary = as.integer(ifelse(friends_money > 0, 1, 0)),
  #   community_money_binary = as.integer(ifelse(community_money> 0, 1, 0))
  # ) |>  #shorter name
  # dplyr::select(
  #   -c(
#     religion_church,
#     family_time,
#     friends_time,
#     community_time,
#     hours_community,
#     hours_family,
#     hours_friends,
#     community_money,
#     friends_money,
#     family_money
#   )
# ) |>
arrange(id, wave) |>
  droplevels() |>
  data.frame() |>
  droplevels() |>
  arrange(id, wave) |>
  #   mutate(
  #   religion_church_coarsen = cut(
  #     religion_church,
  #     breaks = c(-Inf, 0, 1, 3.99, Inf),
  #     labels = c("zero", "one", "less_four", "four_up"),
  #     include.lowest = TRUE,
  #     right = TRUE,
  #     ordered = TRUE
  #   )
  # ) |>
  # mutate(
#   religion_church_coarsen_n = as.numeric(religion_church_coarsen) - 1,
#   religion_church_binary_n = as.numeric(religion_church_binary)
# ) |>
dplyr::mutate(
  rural_gch_2018_l = as.numeric(as.character(rural_gch_2018_l)),
  has_siblings = as.numeric(as.character(has_siblings)),
  parent = as.numeric(as.character(parent)),
  partner = as.numeric(as.character(partner)),
  born_nz = as.numeric(as.character(born_nz)),
  censored = as.numeric(as.character(censored)),
  employed = as.numeric(as.character(employed)),
  hlth_disability = as.numeric(as.character(hlth_disability))
) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()

n_participants <- n_unique(dat_long$id)
n_participants <- prettyNum(n_participants,big.mark=",")

margot::here_save(n_participants, n_participants)
n_participants


# create sample weights for male female -----------------------------------

# calculate gender weights assuming male is coded as 1 and female as 0
prop_male_population <- 0.5  # target proportion of males in the population
prop_female_population <- 0.5  # target proportion of females in the population

prop_male_sample <- mean(dat_long$male)
prop_female_sample <- 1 - prop_male_sample

gender_weight_male <- prop_male_population / prop_male_sample
gender_weight_female <- prop_female_population / prop_female_sample

dat_long$sample_weights <- ifelse(dat_long$male == 1, gender_weight_male, gender_weight_female)

hist(dat_long$sample_weights)

# check male are upweighted
head(dat_long[, c("male", "sample_weights")])


# baseline vars -----------------------------------------------------------
# dat_long <- dat_long_full |>
#   select(-alert_level_combined)

str(dat_long)
# check
table(dat_long$alert_level_combined_lead)

# select vars for baseline
dat_long_colnames <- colnames(dat_long)

dat_long_colnames <- sort(dat_long_colnames)
dat_long_colnames
dat_long_colnames <- setdiff(dat_long_colnames, c("alert_level_combined", "alert_level_combined_lead"))

# set baseline exposure and outcomes --------------------------------------

exposure_var = c("hours_community_round",
                 "censored"#,
                 # "hours_community_round"
) #


# set outcomes for prosocial domain
# save prejudice for separate paper
outcome_vars = c(
  "warm_asians",
  "warm_chinese",
  "warm_immigrants",
  "warm_indians",
  "warm_elderly",
  "warm_maori",
  "warm_mental_illness",
  "warm_muslims",
  "warm_nz_euro",
  "warm_overweight",
  "warm_pacific",
  "warm_refugees"
)


# baseline vars -----------------------------------------------------------

baseline_vars <-
  setdiff(dat_long_colnames, 
          c("id","wave", "alert_level_combined", "alert_level_combined_lead", "sample_weights", "censored"))

# c(outcome_vars, 'id', 'wave'))
baseline_vars
baseline_vars <- sort(baseline_vars)

baseline_vars




# just core baseline variables
base_var <-
  setdiff(baseline_vars, c("censored", "sample_weights", "alert_level_combined_lead", outcome_vars))
base_var

push_mods
here_save(base_var, "base_var")
here_save(baseline_vars, "baseline_vars")

push_mods

# double check path
push_mods

# check col names
colnames(dat)

# assess positivity
dt_positivity_full <- dat_long |>
  filter(wave == 2018 | wave == 2019) |>
  select(wave, id, hours_community_round, sample_weights) |>
  mutate(hours_community_shift_gain = ifelse(hours_community_round >= 2, 1, 0)) |> 
  mutate(hours_community_shift_zero = ifelse(hours_community_round > 0, 1, 0))




# create transition matrix
# compute transitions
out <-margot::create_transition_matrix(data = dt_positivity_full, state_var = "hours_community_round", id_var = "id")

out_binary_gain <- margot::create_transition_matrix(data = dt_positivity_full, state_var = "hours_community_shift_gain", id_var = "id")

out_binary_loss <- margot::create_transition_matrix(data = dt_positivity_full, state_var = "hours_community_shift_zero", id_var = "id")

# make table
transition_table  <- margot::transition_table(out)
transition_table
margot::here_save(transition_table, "transition_table")



gain_labs <- c("<2", ">=2")

transition_table_gain  <- margot::transition_table(out_binary_gain, state_names = gain_labs)
transition_table_gain
margot::here_save(transition_table_gain, "transition_table_gain")


loss_labs <- c("0", ">0")
transition_table_loss  <- margot::transition_table(out_binary_loss, state_names = loss_labs)
transition_table_loss
margot::here_save(transition_table_loss, "transition_table_loss")


# view
transition_table_gain
transition_table_loss


# check associations only -------------------------------------------------
dt_18 <- dat_long|>
  filter(wave == 2018) 

exposure = colnames(dt_18[[exposure_var_string]])

naniar::vis_miss(dt_18, warn_large_data = F)

fit_cross_sectional_warm_pacific <-
  regress_with_covariates(
    dt_18,
    outcome = "warm_pacific",
    exposure = exposure_var_string,
    baseline_vars = base_var
  )
summary(fit_cross_sectional_warm_pacific, ci_method="wald")
parameters::model_parameters(fit_cross_sectional_warm_pacific, ci_method="wald")[2, ]

fit_cross_sectional_muslims <-
  regress_with_covariates(
    dt_18,
    outcome = "warm_muslims",
    exposure = exposure_var_string,
    baseline_vars = base_var
  )
parameters::model_parameters(fit_cross_sectional_muslims, ci_method="wald")[2, ]



here_save(fit_cross_sectional_muslims, "fit_cross_sectional_muslims")

here_save(fit_cross_sectional_maori, "fit_cross_sectional_maori")


# tables ------------------------------------------------------------------
library(gtsummary)


library(gtsummary)
base_var

table_baseline <- dt_18 |> 
  select(all_of(base_var)) |> 
  janitor::clean_names(case = "title") |> 
  tbl_summary(
    missing = "ifany",
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
  modify_header(label = "**Exposure + Demographic Variables**") |> # update the column header
  bold_labels() 



table_baseline
# save baseline
here_save(table_baseline, "table_baseline")


## all outcomes

names_outcomes_tab <- setdiff(outcome_vars, dt_18)
names_outcomes_sorted <- sort(names_outcomes_tab)
names_outcomes_final <-
  names_outcomes_sorted # consistent workflow
names_outcomes_final

names_outcomes_final



# histogram exposure ------------------------------------------------------
table(is.na(dt_19$alert_level_combined))
dt_19 <- dat_long |> 
  filter(wave == 2019)

mean(dt_19$hours_community_round, na.rm = TRUE)
median(dt_19$hours_community_round, na.rm = TRUE)
naniar::vis_miss(dt_19, warn_large_data = F)
library(ggplot2)
library(dplyr)
#
# # generate bar plot
graph_density_of_exposure_up <- margot::coloured_histogram_shift(
  dt_19,
  col_name = exposure_var_string,
  binwidth = 1, 
  range_highlight = c(0,.99)
)
graph_density_of_exposure_up


graph_density_of_exposure_down <- margot::coloured_histogram_shift(
  dt_19,
  shift = "down",
  col_name = exposure_var_string,
  binwidth = 1, 
  range_highlight = c(.01,24)
)
graph_density_of_exposure_up

graph_density_of_exposure_down

here_save(graph_density_of_exposure_up, "graph_density_of_exposure_up")
here_save(graph_density_of_exposure_down, "graph_density_of_exposure_down")
#

# impute baseline ---------------------------------------------------------
# impute baseline data (we use censoring for the outcomes)
#colnames(dat_long)
# function imputes only baseline not outcome
#
#devtools::install_github("go-bayes/margot")
str(dat_long)
dat_long$sample_weights
dat_long_df <- dat_long # #select(-alert_level_combined)
dat_long_df <- data.frame(dat_long)

my_data_filtered <- as.data.frame(dat_long_df)
my_data_filtered <- haven::zap_formats(dat_long_df)
my_data_filtered <- haven::zap_label(dat_long_df)
my_data_filtered <- haven::zap_widths(dat_long_df)

dat_long_df <- data.frame(dat_long)

colnames(dat_long_df)

# check baseline vars
baseline_vars


# impute
prep_coop_all <- margot::margot_wide_impute_baseline(
  dat_long_df,
  baseline_vars = baseline_vars,
  exposure_var = exposure_var,
  outcome_vars = outcome_vars
)


# return variables to the imputed data frame
# sample weights
prep_coop_all$t0_sample_weights <- dt_18$sample_weights

# alert_level
prep_coop_all$t0_alert_level_combined_lead <- dt_18$alert_level_combined_lead

margot::here_save(prep_coop_all, "prep_coop_all")


# read function
prep_coop_all <-
  margot::here_read("prep_coop_all")

head(prep_coop_all)

#check data
naniar::vis_miss(prep_coop_all, warn_large_data = FALSE)

table(prep_coop_all$t0_censored)
head(prep_coop_all$t0_sam)

#check must be a dataframe
str(prep_coop_all)
nrow(prep_coop_all)



# spit shine --------------------------------------------------------------

df_wide_censored <- prep_coop_all |>
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat),
    t0_education_level_coarsen = as.factor(t0_education_level_coarsen)
  ) |>
  relocate("t0_censored", .before = starts_with("t1_")) |>
  relocate("t1_censored", .before = starts_with("t2_")) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))

# check
naniar::vis_miss(df_wide_censored, warn_large_data = FALSE)

table(df_wide_censored$t0_censored)

outcome_vars



# Assuming df_wide_censored is your dataframe

# Calculate the conditions before the mutate steps
t0_na_condition <- rowSums(is.na(select(df_wide_censored, starts_with("t1_")))) > 0
#t1_na_condition <- rowSums(is.na(select(df_wide_censored, starts_with("t2_")))) > 0

df_clean <- df_wide_censored %>%
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
  # mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%
  # mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
  #        across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  # mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .)))|>
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_censored &
     #   !t0_religion_church_round &
     #   !t0_hours_community_round &
        # !t0_charity_donate & 
        !t0_rural_gch_2018_l & 
        #    ! t0_nzsei_13_l& 
        !t0_sample_frame_opt_in & 
        !t0_alert_level_combined_lead  &
        !t0_alert_level_combined_lead  & 
        !t0_sample_weights & 
        !t1_hours_community_round &
        !t1_censored,
      .fns = ~ scale(.),
      .names = "{.col}_z"
    )
  ) |>
  # select(-t0_charity_donate,
  #        -t0_hours_charity) |> 
  select(
    where(is.factor),
    t0_sample_weights,
    #t0_religion_church_round,
    #t0_hours_community_round,
    #  t0_nzsei_13_l,
    t0_rural_gch_2018_l,
    t0_sample_frame_opt_in,
    t0_censored,
    t0_alert_level_combined_lead ,
    t0_alert_level_combined_lead ,
    t1_hours_community_round,    
    t1_censored,
    ends_with("_z")
  ) |>
  mutate(t0_lost = 1 - t0_censored) |> 
  mutate(t1_lost = 1 - t1_censored) |> 
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))

naniar::vis_miss(df_clean, warn_large_data = FALSE)


# checks
table(df_clean$t1_lost)
table(df_clean$t0_lost)

table(df_clean$t0_censored)

test <- df_wide_censored |> filter(t0_censored == 1)
nrow(test)

# 
# df_impute_base$t1_perfectionism_z = scale(df_impute_base$t1_perfectionism)

# get rid of attributes
df_clean <- margot::remove_numeric_attributes(df_clean)
str( df_clean )

nrow(df_clean)

# checks
naniar::vis_miss(df_clean, warn_large_data = FALSE)

here_save(df_clean, "df_clean")



# weights for treament ----------------------------------------------------
df_clean <- here_read("df_clean")
baseline_vars_models = df_clean |>  # post process of impute and combine
  dplyr::select(starts_with("t0"),-t0_censored, -t0_lost, -t0_sample_weights, -t0_alert_level_combined_lead)|> colnames() # note, we ear

baseline_vars_models




# fit propensity score model 

# use same libraries
sl_lib

listWrappers()


# clean vars
df_clean_pre <- df_clean[baseline_vars_models]

df_clean_pre$t0_rural_gch_2018_l <- as.factor(df_clean_pre$t0_rural_gch_2018_l)
levels(df_clean_pre$t0_sample_origin)

str(df_clean_pre)

# perform one-hot encoding using model.matrix
encoded_vars <- model.matrix(~ t0_education_level_coarsen + t0_eth_cat + t0_sample_origin + t0_rural_gch_2018_l  - 1, data = df_clean_pre)

# convert matrix to data frame
encoded_df <- as.data.frame(encoded_vars)


# make better names
encoded_df <- encoded_df %>% 
  janitor::clean_names()

# view the first few rows to confirm structure
head(encoded_df)

# bind the new one-hot encoded variables back to the original dataframe
# ensure to remove original categorical variables to avoid duplication
df_clean_hot_code <- df_clean %>%
  select(-c(id, t0_education_level_coarsen, t0_eth_cat, t0_sample_origin, t0_rural_gch_2018_l, t0_rural_gch_2018_l, t0_alert_level_combined_lead)) %>%
  bind_cols(encoded_df)

# extract and print the new column names for encoded variables
new_encoded_colnames <- colnames(encoded_df)
print(new_encoded_colnames)


# combine with base list of predictors
baseline_vars_set <- setdiff(names(df_clean_pre), c("t0_lost", "id", "t0_education_level_coarsen", "t0_eth_cat", "t0_rural_gch_2018_l", "t0_sample_origin"))

# Add the new encoded column names
full_predictor_vars <- c(baseline_vars_set, new_encoded_colnames)


full_predictor_vars

# no factors
str(df_clean_hot_code)


cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification


library(doParallel)
library(SuperLearner)


# parallel backend
no_cores <- detectCores()
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)


str(df_clean_hot_code[full_predictor_vars])

match_lib = c("SL.glmnet", "SL.xgboost", "SL.ranger")



sl <- SuperLearner(
  Y = df_clean_hot_code$t0_lost, 
  X = df_clean_hot_code[full_predictor_vars],  # Use all specified predictors
  SL.library = match_lib,
  family = binomial(), 
  method = "method.NNloglik", 
  cvControl = list(V = 10)
)
push_mods
here_save(sl, "sl")

# stop the cluster
stopCluster(cl)


# CHECKS 
print(sl)                  # Prints the summary of the SuperLearner output
summary(sl)                # Provides a detailed summary, including cross-validated risks

# For detailed examination of cross-validated performance
sl$cvRisk                  # Cross-validated risks for each learner
sl$coef                    # Weights assigned to each learner in the final ensemble



# Generate predictions
predictions <- predict(sl, newdata = df_clean_hot_code[full_predictor_vars], type = "response")

# Extract predictions from the 'pred' component and ensure it's a vector
df_clean_hot_code$pscore <- predictions$pred[, 1]

# Check the structure of the predictions
str(predictions)


# IPCW
df_clean_hot_code$weights <- ifelse(df_clean_hot_code$t0_lost == 1, 1 / df_clean_hot_code$pscore, 1 / (1 - df_clean_hot_code$pscore))


# check 
hist(df_clean_hot_code$weights)
min(df_clean_hot_code$weights)
max(df_clean_hot_code$weights)
# obtain stabalizing var (no need)
#df_clean_hot_code <- mean(df_clean_hot_code$t0_lost)

# stabalized weights
# #df_clean_hot_code$weights_stabilized <- ifelse(df_clean_hot_code$t0_lost == 1, 
#                                   marginal_censored / df_clean_hot_code$pscore, 
#                                   (1 - marginal_censored) / (1 - df_clean_hot_code$pscore))





# save output
here_save( df_clean_hot_code, "df_clean_hot_code") 

#
head(df_clean_hot_code)

df_clean

# new weights
df_clean$t0_combo_weights = df_clean_hot_code$weights * df_clean$t0_sample_weights

min( df_clean$t0_combo_weights)
max( df_clean$t0_combo_weights)

hist( df_clean$t0_combo_weights)

df_clean_t1 <- df_clean |> filter(t0_lost == 0)


hist( df_clean_t1$t0_combo_weights )

max(( df_clean_t1$t0_combo_weights ))
min(df_clean_t1$t0_combo_weights)

#
nrow(df_clean_t1)

table(is.na(df_clean_t1$t1_hours_community_round)) # 33198

# gets us the correct df for weights
naniar::vis_miss(df_clean_t1, warn_large_data = FALSE)


# nrow full
nrow(test)

# correct
nrow(df_clean_t1)

# next get data for t1
hist(df_clean_t1$t0_combo_weights)

# get correct censoring 

# redundant but OK
t0_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t1_")))) > 0

# use
t1_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t2_")))) > 0
# baseline_vars
# df_impute_base$t0_sample_weights


df_clean_t2 <- df_clean_t1 %>%
  # select(-t0_alert_level_combined_lead) |> 
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
  mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%
  mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .))) |>
  # mutate(t0_lost = 1 - t0_censored) |> 
  mutate(t1_lost = 1 - t1_censored) |> 
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_")) |> 
  select(-t1_lost, -t0_lost)


# test 
nrow(df_clean_t2)

# checks 
hist(df_clean_t2$t0_combo_weights)

# outcomes
naniar::vis_miss(df_clean_t2, warn_large_data = F)

#
here_save(df_clean_t2, "df_clean_t2")

# START HERE --------------------------------------------------------------
# read data --  start here if previous work already done
df_clean_t2 <- here_read("df_clean_t2")

colnames(df_clean_t2)
str(df_clean_t2)
# names of vars for modelling

names_base <-
  df_clean_t2 |> select(starts_with("t0"),
                        -t0_sample_weights, -t0_combo_weights,
                        -t0_censored) |> colnames()

names_base


names_outcomes <-
  df_clean_t2 |> select(starts_with("t2")) |> colnames()

names_outcomes

table( is.na( df_clean_t2$t0_alert_level_combined ))

names(df_clean_t2)
# explan names

names_base

df_final_base  <- df_clean_t2[names_base]


df_final_base$t0_rural_gch_2018_l <- as.factor(df_final_base$t0_rural_gch_2018_l)

# perform one-hot encoding using model.matrix
encoded_vars_final <- model.matrix(~ t0_education_level_coarsen + t0_eth_cat + t0_sample_origin + t0_rural_gch_2018_l  +  t0_alert_level_combined_lead - 1, data = df_final_base)

# convert matrix to data frame
encoded_vars_final <- as.data.frame(encoded_vars_final)

encoded_vars_final

# make better names
encoded_vars_final <- encoded_vars_final %>% 
  janitor::clean_names()

# View the first few rows to confirm structure
head(encoded_vars_final)

# bind the new one-hot encoded variables back to the original dataframe
# ensure to remove original categorical variables to avoid duplication
df_clean_t2_hot_code <- df_clean_t2 %>%
  select(-c(t0_education_level_coarsen, t0_eth_cat, t0_sample_origin, t0_rural_gch_2018_l, t0_alert_level_combined_lead)) %>%
  bind_cols(encoded_vars_final)

# extract and print the new column names for encoded variables
new_encoded_colnames_final <- colnames(encoded_vars_final)
print(new_encoded_colnames_final)


#  base list of predictors
baseline_vars_set_final <- setdiff(names(df_final_base), c("t0_lost", "id", "t0_education_level_coarsen", "t0_eth_cat", "t0_religion_church_round", "t0_rural_gch_2018_l", "t0_sample_origin", "t0_alert_level_combined_lead"))

# Add the new encoded column names
full_predictor_vars_final <- c(baseline_vars_set_final, new_encoded_colnames_final)


df_clean_t2_hot_code <-  df_clean_t2_hot_code |> 
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))

colnames(df_clean_t2_hot_code)
#cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification

library(SuperLearner)



#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(0112358)
library(future)
library(SuperLearner)
plan(multisession)


# 
# A list of lists is returned with the names of the variables in Ht to be used for estimation of the outcome regression and the treatment mechanism at every time t. Notice that variables A1 and A2 are included in the list of variables used for estimation of the treatment mechamism (trt). This is due to the fact that the nuisance parameter for the treatment mechanism is the density ratio rt, which is a function of A1 and A2.
# The density ratio is estimated based on a classification trick using an auxiliary variable Λ as a pseudo outcome and the treatment as a predictor. We now briefly describe how this density ratio estimation is done; the process is fully automated and hidden from the software user. Specifically, the TMLE and SDR estimation methods require estimation of the ratio of the densities of Adt and At, conditional on the history Ht, defined as rt above. This is achieved through computing the odds in a classification problem in an augmented dataset with 2n observations where the outcome is the auxiliary variable Λ (defined below) and the predictors are the variables At and Ht. In the 2n augmented data set, the data structure at time t is redefined as
# inline graphic
# where Λλ,i = λi indexes duplicate values. For all duplicated observations λ ∈ {0, 1} with the same i, Hλ,i,t is the same. For λ = 0, Aλ,i,t equals the observed exposure values Ai,t, whereas for λ = 1, Aλ,i,t equals the exposure values under the MTP d, namely Adt. The [End Page 111] classification approach to density ratio estimation proceeds by estimating the conditional probability that ∆ = 1 in this dataset, and dividing it by the corresponding estimate of the conditional probability that ∆ = 0. Specifically, denoting pλ the distribution of the data in the augmented dataset, we have:
#   inline graphic
# Further details on this algorithm may be found in our technical paper (Díaz et al., 2021).

#L <- list(NULL, c("t1_alert_level_combined"))
# W <- names_base_base

# check

#### SET VARIABLE NAMES
#  model
A <- "t1_hours_community_round"
C <- c("t1_censored")

#L <- list(c(NULL), c("t1_alert_level_combined"))
#W <-  c(paste(names_base, collapse = ", "))
names_base

# check


## Expland categorical variables again

# 
# gain_A <- function(data, trt) {
#   # make zero at baseline
#   mtp_base_A <- function(data, trt) {
#     ifelse(data[[trt]] > 0, 0, data[[trt]])
#   }
#   
#   if (trt == "t0_religion_church_round") {
#     return(mtp_base_A(data, trt))
#   }
#   
#   # shift to at least 4 at time 1
#   mtp_one_contrast_A <- function(data, trt) {
#     ifelse(data[[trt]] <= 4, 4,  data[[trt]])
#   }
#   
#   #  trt is a variable name passed as a string to the function
#   
#   ifelse(trt == "t1_religion_religious",
#          mtp_one_contrast_A(data, trt),
#          data[[trt]])
# }
# 
# 
# 
# gain_A <- function(data, trt) {
#   # make zero at baseline
#   mtp_base_A <- function(data, trt) {
#     ifelse(data[[trt]] <= 4, 4,  data[[trt]]))
#   }
#   
#   if (trt == "t0_religion_church_round") {
#     return(mtp_base_A(data, trt))
#   }
#   
#   # shift to at least 4 at time 1
#   mtp_one_contrast_A <- function(data, trt) {
#     ifelse(data[[trt]] <= 4, 4,  data[[trt]])
#   }
#   
#   #  trt is a variable name passed as a string to the function
#   
#   ifelse(trt == "t1_religion_religious",
#          mtp_one_contrast_A(data, trt),
#          data[[trt]])
# }

# 
# 
# 
# zero_A <- function(data, trt) {
#   
#   # make zero at baseline
#   mtp_base_zero <- function(data, trt) {
#     ifelse(data[[trt]] > 0, 0, data[[trt]])
#   }
#   
#   if (trt == "t0_religion_church_round") {
#     return(mtp_base_zero(data, trt))
#   }
#   
#   
#   # keep zero at exposure wave
#   
#   mtp_one_contrast_zero <- function(data, trt) {
#     ifelse(data[[trt]] > 0, 0, data[[trt]])
#   }
#   
#   #  trt is a variable name passed as a string to the function
#   
#   ifelse(trt == "t1_religion_church_round",
#          mtp_one_contrast_zero(data, trt),
#          data[[trt]])
# }




gain_A <- function(data, trt){
  ifelse( data[[trt]] < 1, 1,  data[[trt]] )
}

# 
# gain_A <- function(data, trt) {
#   # make zero at baseline
#   mtp_base_A <- function(data, trt) {
#     ifelse(data[[trt]] < 4, 4, data[[trt]])
#   }
#   
#   if (trt == "t0_religion_church_round") {
#     return(mtp_base_A(data, trt))
#   }
#   
#   # shift to at least 4 at time 1
#   mtp_one_contrast_A <- function(data, trt) {
#     ifelse(data[[trt]] < 4, 4,  data[[trt]])
#   }
#   
#   #  trt is a variable name passed as a string to the function
#   
#   ifelse(trt == "t1_religion_church_round",
#          mtp_one_contrast_A(data, trt),
#          data[[trt]])
# }

zero_A <- function(data, trt){
  ifelse( data[[trt]] > 0, 0,  data[[trt]] )
}




# BONUS: progressr progress bars!
progressr::handlers(global = TRUE)

library(future)
plan(multisession)

n_cores
# church: charity models ----------------------------------------------------------

A
C


#library("ranger")
full_predictor_vars_final
df_clean_t2_hot_code

names_base_hot <- full_predictor_vars_final

#better name
df_clean_t2_hot <- df_clean_t2_hot_code


# test data 
df_clean_slice <- df_clean_t2_hot_code |>
  slice_head(n = 1000) |>
  as.data.frame()
colnames(df_clean_slice)

library(SuperLearner)
library(xgboost)
library(ranger)

# measure time taken to run the model
# 
# names_base_t2_hours_charity_z <-
#   select_and_rename_cols(names_base = use_names,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_hours_charity_z")
# 
# names_base_t2_hours_charity_z
#m_hours_charity_z_test

# sl_lib <- c("SL.glmnet",
#             "SL.ranger", #
#             "SL.xgboost") #



L

A
listWrappers()


t2_warm_pacific_z_test_null <- lmtp_tmle(
  outcome = "t2_warm_pacific_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


summary(lm(t2_warm_pacific_z ~ t1_hours_community_round + t0_religion_church_round + t0_warm_pacific_z, df_clean_slice))
t2_warm_pacific_z_test_gain

t2_warm_pacific_z_test_null
t2_warm_pacific_z_test_null$fits_m
t2_warm_pacific_z_test_null$fits_r

# checks
gain_A

library(lmtp)
t2_warm_pacific_z_test_gain <- lmtp_tmle(
  outcome = "t2_warm_pacific_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


t2_warm_pacific_z_test_gain$fits_m
t2_warm_pacific_z_test_gain$fits_r


# warm asians -------------------------------------------------------------

# names_base_t2_warm_asians_z<- select_and_rename_cols(names_base = names_base,  
#                                                      baseline_vars = base_var, 
#                                                      outcome =  "t2_warm_asians_z")


t2_warm_asians_z_gain <- lmtp_tmle(
  outcome = "t2_warm_asians_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)         
here_save(t2_warm_asians_z_gain, "t2_warm_asians_z_gain")

# 
t2_warm_asians_z_zero <- lmtp_tmle(
  outcome = "t2_warm_asians_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)
here_save(t2_warm_asians_z_zero, "t2_warm_asians_z_zero")

t2_warm_asians_z_null <- lmtp_tmle(
  outcome = "t2_warm_asians_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_asians_z_null, "t2_warm_asians_z_null")


# warm_chinese ------------------------------------------------------------

# names_base_t2_warm_chinese_z<- select_and_rename_cols(names_base = names_base,  
#                                                       baseline_vars = base_var, 
#                                                       outcome =  "t2_warm_chinese_z")

t2_warm_chinese_z_gain <- lmtp_tmle(
  outcome = "t2_warm_chinese_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_chinese_z_gain, "t2_warm_chinese_z_gain")
t2_warm_chinese_z_gain
# 
t2_warm_chinese_z_zero <- lmtp_tmle(
  outcome = "t2_warm_chinese_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
here_save(t2_warm_chinese_z_zero, "t2_warm_chinese_z_zero")



t2_warm_chinese_z_null <- lmtp_tmle(
  outcome = "t2_warm_chinese_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_chinese_z_null, "t2_warm_chinese_z_null")


# t2_warm_immigrants_z ---------------------------------------------------------
# names_base_t2_warm_immigrants_z <- select_and_rename_cols(names_base = names_base,  
#                                                           baseline_vars = base_var, 
#                                                           outcome =  "t2_warm_immigrants_z")
# 


t2_warm_immigrants_z_gain <- lmtp_tmle(
  outcome = "t2_warm_immigrants_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_immigrants_z_gain, "t2_warm_immigrants_z_gain")

# 
t2_warm_immigrants_z_zero <- lmtp_tmle(
  outcome = "t2_warm_immigrants_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
here_save(t2_warm_immigrants_z_zero, "t2_warm_immigrants_z_zero")
# 


t2_warm_immigrants_z_null <- lmtp_tmle(
  outcome = "t2_warm_immigrants_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,  
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_immigrants_z_null, "t2_warm_immigrants_z_null")



# t2_warm_indians_z ------------------------------------------------------------
# names_base_t2_warm_indians_z <- select_and_rename_cols(names_base = names_base,  
#                                                        baseline_vars = base_var, 
#                                                        outcome =  "t2_warm_indians_z")
# 



t2_warm_indians_z_gain <- lmtp_tmle(
  outcome = "t2_warm_indians_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_indians_z_gain, "t2_warm_indians_z_gain")
# 
# 
t2_warm_indians_z_zero <- lmtp_tmle(
  outcome = "t2_warm_indians_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
here_save(t2_warm_indians_z_zero, "t2_warm_indians_z_zero")


t2_warm_indians_z_null <- lmtp_tmle(
  outcome = "t2_warm_indians_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_indians_z_null, "t2_warm_indians_z_null")


# t2_warm_elderly_z ------------------------------------------------------------

# names_base_t2_warm_elderly_z <- select_and_rename_cols(names_base = names_base,  
#                                                        baseline_vars = base_var, 
#                                                        outcome =  "t2_warm_elderly_z")

t2_warm_elderly_z_gain <- lmtp_tmle(
  outcome = "t2_warm_elderly_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_elderly_z_gain, "t2_warm_elderly_z_gain")
# 
# 
t2_warm_elderly_z_zero <- lmtp_tmle(
  outcome = "t2_warm_elderly_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
here_save(t2_warm_elderly_z_zero, "t2_warm_elderly_z_zero")



t2_warm_elderly_z_null <- lmtp_tmle(
  outcome = "t2_warm_elderly_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_elderly_z_null, "t2_warm_elderly_z_null")


# t2_warm_maori_z --------------------------------------------------------------
# names_base_t2_warm_maori_z <- select_and_rename_cols(names_base = names_base,  
#                                                      baseline_vars = base_var, 
#                                                      outcome =  "t2_warm_maori_z")


t2_warm_maori_z_gain <- lmtp_tmle(
  outcome = "t2_warm_maori_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         



here_save(t2_warm_maori_z_gain, "t2_warm_maori_z_gain")

# 
t2_warm_maori_z_zero <- lmtp_tmle(
  outcome = "t2_warm_maori_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
here_save(t2_warm_maori_z_zero, "t2_warm_maori_z_zero")



t2_warm_maori_z_null <- lmtp_tmle(
  outcome = "t2_warm_maori_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_maori_z_null, "t2_warm_maori_z_null")


# t2_warm_mental_illness_z -----------------------------------------------------
# names_base_t2_warm_mental_illness_z <- select_and_rename_cols(names_base = names_base,  
#                                                               baseline_vars = base_var, 
#                                                               outcome =  "t2_warm_mental_illness_z")


t2_warm_mental_illness_z_gain <- lmtp_tmle(
  outcome = "t2_warm_mental_illness_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_mental_illness_z_gain, "t2_warm_mental_illness_z_gain")
# 
# 
t2_warm_mental_illness_z_zero <- lmtp_tmle(
  outcome = "t2_warm_mental_illness_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
here_save(t2_warm_mental_illness_z_zero, "t2_warm_mental_illness_z_zero")



t2_warm_mental_illness_z_null<- lmtp_tmle(
  outcome = "t2_warm_mental_illness_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_mental_illness_z_null, "t2_warm_mental_illness_z_null")



# t2_warm_muslims_z ------------------------------------------------------------
# names_base_t2_warm_muslims_z <- select_and_rename_cols(names_base = names_base,  
#                                                        baseline_vars = base_var, 
#                                                        outcome =  "t2_warm_muslims_z")


t2_warm_muslims_z_gain <- lmtp_tmle(
  outcome = "t2_warm_muslims_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_muslims_z_gain, "t2_warm_muslims_z_gain")
# 
# 
t2_warm_muslims_z_zero <- lmtp_tmle(
  outcome = "t2_warm_muslims_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
here_save(t2_warm_muslims_z_zero, "t2_warm_muslims_z_zero")

listWrappers()
t2_warm_muslims_z_null <- lmtp_tmle(
  outcome = "t2_warm_muslims_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_muslims_z_null, "t2_warm_muslims_z_null")


# t2_warm_nz_euro_z ------------------------------------------------------------
# names_base_t2_warm_nz_euro_z <- select_and_rename_cols(names_base = names_base,  
#                                                        baseline_vars = base_var, 
#                                                        outcome =  "t2_warm_nz_euro_z")



t2_warm_nz_euro_z_gain <- lmtp_tmle(
  outcome = "t2_warm_nz_euro_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_nz_euro_z_gain, "t2_warm_nz_euro_z_gain")
# 
# 
t2_warm_nz_euro_z_zero <- lmtp_tmle(
  outcome = "t2_warm_nz_euro_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
here_save(t2_warm_nz_euro_z_zero, "t2_warm_nz_euro_z_zero")


t2_warm_nz_euro_z_null <- lmtp_tmle(
  outcome = "t2_warm_nz_euro_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_nz_euro_z_null, "t2_warm_nz_euro_z_null")



# t2_warm_overweight_z ---------------------------------------------------------
# names_base_t2_warm_overweight_z <- select_and_rename_cols(names_base = names_base,  
#                                                           baseline_vars = base_var, 
#                                                           outcome =  "t2_warm_overweight_z")



t2_warm_overweight_z_gain <- lmtp_tmle(
  outcome = "t2_warm_overweight_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_overweight_z_gain, "t2_warm_overweight_z_gain")
# 
# 
t2_warm_overweight_z_zero <- lmtp_tmle(
  outcome = "t2_warm_overweight_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
here_save(t2_warm_overweight_z_zero, "t2_warm_overweight_z_zero")

t2_warm_overweight_z_null <- lmtp_tmle(
  outcome = "t2_warm_overweight_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)         
here_save(t2_warm_overweight_z_null, "t2_warm_overweight_z_null")


# t2_warm_pacific_z ------------------------------------------------------------
# names_base_t2_warm_pacific_z <- select_and_rename_cols(names_base = names_base,  
#                                                        baseline_vars = base_var, 
#                                                        outcome =  "t2_warm_pacific_z")




tab_contrast_t2_warm_asians_z
tab_contrast_t2_warm_chinese_z
tab_contrast_t2_warm_immigrants_z
tab_contrast_t2_warm_indians_z
tab_contrast_t2_warm_elderly_z
tab_contrast_t2_warm_maori_z
tab_contrast_t2_warm_mental_illness_z
tab_contrast_t2_warm_muslims_z
tab_contrast_t2_warm_nz_euro_z
tab_contrast_t2_warm_overweight_z

t2_warm_pacific_z_gain <- lmtp_tmle(
  outcome = "t2_warm_pacific_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)       
t2_warm_pacific_z_gain


here_save(t2_warm_pacific_z_gain, "t2_warm_pacific_z_gain")


t2_warm_pacific_z_zero <- lmtp_tmle(
  outcome = "t2_warm_pacific_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)
here_save(t2_warm_pacific_z_zero, "t2_warm_pacific_z_zero")

t2_warm_pacific_z_null <- lmtp_tmle(
  outcome = "t2_warm_pacific_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)         
here_save(t2_warm_pacific_z_null, "t2_warm_pacific_z_null")






# t2_warm_refugees_z -----------------------------------------------------------
# names_base_t2_warm_refugees_z <- select_and_rename_cols(names_base = names_base,  
#                                                         baseline_vars = base_var, 
#                                                         outcome =  "t2_warm_refugees_z")


t2_warm_refugees_z_gain <- lmtp_tmle(
  outcome = "t2_warm_refugees_z",
  baseline = names_base_hot,
  shift = gain_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)         
here_save(t2_warm_refugees_z_gain, "t2_warm_refugees_z_gain")


t2_warm_refugees_z_zero <- lmtp_tmle(
  outcome = "t2_warm_refugees_z",
  baseline = names_base_hot,
  shift = zero_A,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)
here_save(t2_warm_refugees_z_zero, "t2_warm_refugees_z_zero")

t2_warm_refugees_z_null <- lmtp_tmle(
  outcome = "t2_warm_refugees_z",
  baseline = names_base_hot,
  shift = NULL,
  data = df_clean_t2_hot,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_t2_hot$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)         
here_save(t2_warm_refugees_z_null, "t2_warm_refugees_z_null")


# # t2_perc_gend_discrim_z -------------------------------------------------------
# names_base_t2_perc_gend_discrim_z <- select_and_rename_cols(names_base = names_base,  
#                                                             baseline_vars = base_var, 
#                                                             outcome =  "t2_perc_gend_discrim_z")
# 
# 
# t2_perc_gend_discrim_z_gain <- lmtp_tmle(
#   outcome = "t2_perc_gend_discrim_z",
#   baseline = names_base_hot,
#   shift = gain_A,
#   data = df_clean_t2_hot,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_t2_hot$t0_combo_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )         
# here_save(t2_perc_gend_discrim_z_gain, "t2_perc_gend_discrim_z_gain")
# 
# # 
# t2_perc_gend_discrim_z_zero <- lmtp_tmle(
#   outcome = "t2_perc_gend_discrim_z",
#   baseline = names_base_hot,
#   shift = zero_A,
#   data = df_clean_t2_hot,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_t2_hot$t0_combo_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
# here_save(t2_perc_gend_discrim_z_zero, "t2_perc_gend_discrim_z_zero")
# 
# 
# t2_perc_gend_discrim_z_null <- lmtp_tmle(
#   outcome = "t2_perc_gend_discrim_z",
#   baseline = names_base_hot,
#   shift = NULL,
#   data = df_clean_t2_hot,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_t2_hot$t0_combo_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )         
# here_save(t2_perc_gend_discrim_z_null, "t2_perc_gend_discrim_z_null")
# 
# 
# # t2_perc_discrim_z ------------------------------------------------------------
# names_base_t2_perc_discrim_z <- select_and_rename_cols(names_base = names_base,  
#                                                        baseline_vars = base_var, 
#                                                        outcome =  "t2_perc_discrim_z")
# 
# t2_perc_discrim_z_gain <- lmtp_tmle(
#   outcome = "t2_perc_discrim_z",
#   baseline = names_base_hot,
#   shift = gain_A,
#   data = df_clean_t2_hot,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_t2_hot$t0_combo_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )         
# here_save(t2_perc_discrim_z_gain, "t2_perc_discrim_z_gain")
# 
# # 
# t2_perc_discrim_z_zero <- lmtp_tmle(
#   outcome = "t2_perc_discrim_z",
#   baseline = names_base_hot,
#   shift = zero_A,
#   data = df_clean_t2_hot,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_t2_hot$t0_combo_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
# here_save(t2_perc_discrim_z_zero, "t2_perc_discrim_z_zero")
# 
# 
# 
# t2_perc_discrim_z_null <- lmtp_tmle(
#   outcome = "t2_perc_discrim_z",
#   baseline = names_base_hot,
#   shift = NULL,
#   data = df_clean_t2_hot,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_t2_hot$t0_combo_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )         
# here_save(t2_perc_discrim_z_null, "t2_perc_discrim_z_null")
# 
# 
# # t2_perc_religious_discrim_z --------------------------------------------------
# names_base_t2_perc_religious_discrim_z <- select_and_rename_cols(names_base = names_base,  
#                                                                  baseline_vars = base_var, 
#                                                                  outcome =  "t2_perc_religious_discrim_z")
# 
# 
# 
# t2_perc_religious_discrim_z_gain <- lmtp_tmle(
#   outcome = "t2_perc_religious_discrim_z",
#   baseline = names_base_hot,
#   shift = gain_A,
#   data = df_clean_t2_hot,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_t2_hot$t0_combo_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )         
# here_save(t2_perc_religious_discrim_z_gain, "t2_perc_religious_discrim_z_gain")
# t2_perc_religious_discrim_z_gain
# 
# # 
# t2_perc_religious_discrim_z_zero <- lmtp_tmle(
#   outcome = "t2_perc_religious_discrim_z",
#   baseline = names_base_t2_perc_religious_discrim_z,
#   shift = zero_A,
#   data = df_clean_t2_hot,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_t2_hot$t0_combo_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
# here_save(t2_perc_religious_discrim_z_zero, "t2_perc_religious_discrim_z_zero")
# 
# 
# t2_perc_religious_discrim_z_null <- lmtp_tmle(
#   outcome = "t2_perc_religious_discrim_z",
#   baseline = names_base_hot,
#   shift = NULL,
#   data = df_clean_t2_hot,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_t2_hot$t0_combo_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )         
# here_save(t2_perc_religious_discrim_z_null, "t2_perc_religious_discrim_z_null")


# read models -------------------------------------------------------------





# read models -------------------------------------------------------------


# asians ------------------------------------------------------------------

library(margot)

t2_warm_asians_z_gain<- here_read("t2_warm_asians_z_gain")
t2_warm_asians_z_zero<- here_read("t2_warm_asians_z_zero")
t2_warm_asians_z_null<- here_read("t2_warm_asians_z_null")



contrast_t2_warm_asians_z <-
  lmtp_contrast(t2_warm_asians_z_gain, ref =  t2_warm_asians_z_zero, type = "additive")

library(margot)
tab_contrast_t2_warm_asians_z <- margot::margot_lmtp_evalue(
  contrast_t2_warm_asians_z,
  scale = "RD",
  new_name = "community socialising: warm asians"
)


tab_contrast_t2_warm_asians_z

contrast_t2_warm_asians_z_null <-
  lmtp_contrast(t2_warm_asians_z_gain, ref =  t2_warm_asians_z_null, type = "additive")

tab_contrast_t2_warm_asians_z_null <- margot_lmtp_evalue(
  contrast_t2_warm_asians_z_null,
  scale = "RD",
  new_name = "community socialising: warm asians"
)


# chinese -----------------------------------------------------------------

t2_warm_chinese_z_gain<- here_read("t2_warm_chinese_z_gain")
t2_warm_chinese_z_zero<- here_read("t2_warm_chinese_z_zero")
t2_warm_chinese_z_null<- here_read("t2_warm_chinese_z_null")


contrast_t2_warm_chinese_z <-
  lmtp_contrast(t2_warm_chinese_z_gain, ref =  t2_warm_chinese_z_zero, type = "additive")

tab_contrast_t2_warm_chinese_z <- margot_lmtp_evalue(
  contrast_t2_warm_chinese_z,
  scale = "RD",
  new_name = "community socialising: warm chinese"
)



contrast_t2_warm_chinese_z_null <-
  lmtp_contrast(t2_warm_chinese_z_gain, ref =  t2_warm_chinese_z_null, type = "additive")

tab_contrast_t2_warm_chinese_z_null <- margot_lmtp_evalue(
  contrast_t2_warm_chinese_z_null,
  scale = "RD",
  new_name = "community socialising: warm chinese"
)


# immigrants --------------------------------------------------------------


t2_warm_immigrants_z_gain<- here_read("t2_warm_immigrants_z_gain")
t2_warm_immigrants_z_zero<- here_read("t2_warm_immigrants_z_zero")
t2_warm_immigrants_z_null<- here_read("t2_warm_immigrants_z_null")


contrast_t2_warm_immigrants_z <-
  lmtp_contrast(t2_warm_immigrants_z_gain, ref =  t2_warm_immigrants_z_zero, type = "additive")

tab_contrast_t2_warm_immigrants_z <- margot_lmtp_evalue(
  contrast_t2_warm_immigrants_z,
  scale = "RD",
  new_name = "community socialising: warm immigrants"
)

contrast_t2_warm_immigrants_z_null <-
  lmtp_contrast(t2_warm_immigrants_z_gain, ref =  t2_warm_immigrants_z_null, type = "additive")

tab_contrast_t2_warm_immigrants_z_null <- margot_lmtp_evalue(
  contrast_t2_warm_immigrants_z_null,
  scale = "RD",
  new_name = "community socialising: warm immigrants"
)
tab_contrast_t2_warm_immigrants_z_null



# indians -----------------------------------------------------------------



t2_warm_indians_z_gain<- here_read("t2_warm_indians_z_gain")
t2_warm_indians_z_zero<- here_read("t2_warm_indians_z_zero")
t2_warm_indians_z_null<- here_read("t2_warm_indians_z_null")


contrast_t2_warm_indians_z <-
  lmtp_contrast(t2_warm_indians_z_gain, ref =  t2_warm_indians_z_zero, type = "additive")

tab_contrast_t2_warm_indians_z<- margot_lmtp_evalue(
  contrast_t2_warm_indians_z,
  scale = "RD",
  new_name = "community socialising: warm indians"
)


contrast_t2_warm_indians_z_null <-
  lmtp_contrast(t2_warm_indians_z_gain, ref =  t2_warm_indians_z_null, type = "additive")

tab_contrast_t2_warm_indians_z_null<- margot_lmtp_evalue(
  contrast_t2_warm_indians_z_null,
  scale = "RD",
  new_name = "community socialising: warm indians"
)



# elderly -----------------------------------------------------------------

t2_warm_elderly_z_gain<- here_read("t2_warm_elderly_z_gain")
t2_warm_elderly_z_null<- here_read("t2_warm_elderly_z_null")
t2_warm_elderly_z_zero<- here_read("t2_warm_elderly_z_zero")


contrast_t2_warm_elderly_z <-
  lmtp_contrast(t2_warm_elderly_z_gain, ref =  t2_warm_elderly_z_zero, type = "additive")

tab_contrast_t2_warm_elderly_z <- margot_lmtp_evalue(
  contrast_t2_warm_elderly_z,
  scale = "RD",
  new_name = "community socialising: warm elderly"
)




# 
contrast_t2_warm_elderly_z_null <-
  lmtp_contrast(t2_warm_elderly_z_gain, ref =  t2_warm_elderly_z_null, type = "additive")

tab_contrast_t2_warm_elderly_z_null <- margot_lmtp_evalue(
  contrast_t2_warm_elderly_z_null,
  scale = "RD",
  new_name = "community socialising: warm elderly"
)





# maori -------------------------------------------------------------------

t2_warm_maori_z_gain<- here_read("t2_warm_maori_z_gain")
t2_warm_maori_z_zero<- here_read("t2_warm_maori_z_zero")
t2_warm_maori_z_null<- here_read("t2_warm_maori_z_null")


contrast_t2_warm_maori_z <-
  lmtp_contrast(t2_warm_maori_z_gain, ref =  t2_warm_maori_z_zero, type = "additive")

tab_contrast_t2_warm_maori_z <- margot_lmtp_evalue(
  contrast_t2_warm_maori_z,
  scale = "RD",
  new_name = "community socialising: warm maori"
)

# 
contrast_t2_warm_maori_z_null <-
  lmtp_contrast(t2_warm_maori_z_gain, ref =  t2_warm_maori_z_null, type = "additive")

tab_contrast_t2_warm_maori_z_null <- margot_lmtp_evalue(
  contrast_t2_warm_maori_z_null,
  scale = "RD",
  new_name = "community socialising: warm maori"
)




# mentally ill ------------------------------------------------------------

t2_warm_mental_illness_z_gain<- here_read("t2_warm_mental_illness_z_gain")
t2_warm_mental_illness_z_zero<- here_read("t2_warm_mental_illness_z_zero")
t2_warm_mental_illness_z_null<- here_read("t2_warm_mental_illness_z_null")

t2_warm_mental_illness_z_gain
t2_warm_mental_illness_z_zero
t2_warm_mental_illness_z_null

contrast_t2_warm_mental_illness_z <-
  lmtp_contrast(t2_warm_mental_illness_z_gain, ref =  t2_warm_mental_illness_z_zero, type = "additive")

tab_contrast_t2_warm_mental_illness_z <- margot_lmtp_evalue(
  contrast_t2_warm_mental_illness_z,
  scale = "RD",
  new_name = "community socialising: warm mental illness"
)


# 
# 
contrast_t2_warm_mental_illness_z_null <-
  lmtp_contrast(t2_warm_mental_illness_z_gain, ref =  t2_warm_mental_illness_z_null, type = "additive")

tab_contrast_t2_warm_mental_illness_z_null <- margot::margot_lmtp_evalue(
  contrast_t2_warm_mental_illness_z_null,
  scale = "RD",
  new_name = "community socialising: warm mental illness"
)

# muslims -----------------------------------------------------------------
t2_warm_muslims_z_gain<- here_read("t2_warm_muslims_z_gain")
t2_warm_muslims_z_zero<- here_read("t2_warm_muslims_z_zero")
t2_warm_muslims_z_null<- here_read("t2_warm_muslims_z_null")


contrast_t2_warm_muslims_z <-
  lmtp_contrast(t2_warm_muslims_z_gain, ref =  t2_warm_muslims_z_zero, type = "additive")

tab_contrast_t2_warm_muslims_z <- margot::margot_lmtp_evalue(
  contrast_t2_warm_muslims_z,
  scale = "RD",
  new_name = "community socialising: warm muslims"
)


# 
contrast_t2_warm_muslims_z_null <-
  lmtp_contrast(t2_warm_muslims_z_gain, ref =  t2_warm_muslims_z_null, type = "additive")

tab_contrast_t2_warm_muslims_z_null <- margot::margot_lmtp_evalue(
  contrast_t2_warm_muslims_z_null,
  scale = "RD",
  new_name = "community socialising: warm muslims"
)



# nzeuro ------------------------------------------------------------------

t2_warm_nz_euro_z_gain<- here_read("t2_warm_nz_euro_z_gain")
t2_warm_nz_euro_z_zero<- here_read("t2_warm_nz_euro_z_zero")
t2_warm_nz_euro_z_null<- here_read("t2_warm_nz_euro_z_null")


contrast_t2_warm_nz_euro_z <-
  lmtp_contrast(t2_warm_nz_euro_z_gain, ref =  t2_warm_nz_euro_z_zero, type = "additive")

tab_contrast_t2_warm_nz_euro_z <- margot::margot_lmtp_evalue(
  contrast_t2_warm_nz_euro_z,
  scale = "RD",
  new_name = "community socialising: warm nz euro"
)


# 
# 
contrast_t2_warm_nz_euro_z_null<-
  lmtp_contrast(t2_warm_nz_euro_z_gain, ref =  t2_warm_nz_euro_z_null, type = "additive")

tab_contrast_t2_warm_nz_euro_z_null <- margot::margot_lmtp_evalue(
  contrast_t2_warm_nz_euro_z_null,
  scale = "RD",
  new_name = "community socialising: warm nz euro"
)


# overweight --------------------------------------------------------------


t2_warm_overweight_z_gain<- here_read("t2_warm_overweight_z_gain")
t2_warm_overweight_z_zero<- here_read("t2_warm_overweight_z_zero")
t2_warm_overweight_z_null<- here_read("t2_warm_overweight_z_null")


contrast_t2_warm_overweight_z <-
  lmtp_contrast(t2_warm_overweight_z_gain, ref = t2_warm_overweight_z_zero, type = "additive")

tab_contrast_t2_warm_overweight_z <- margot::margot_lmtp_evalue(
  contrast_t2_warm_overweight_z,
  scale = "RD",
  new_name = "community socialising: warm overweight"
)



# 
contrast_t2_warm_overweight_z_null <-
  lmtp_contrast(t2_warm_overweight_z_gain, ref = t2_warm_overweight_z_null, type = "additive")

tab_contrast_t2_warm_overweight_z_null <- margot::margot_lmtp_evalue(
  contrast_t2_warm_overweight_z_null,
  scale = "RD",
  new_name = "community socialising: warm overweight"
)




# pacific -----------------------------------------------------------------


t2_warm_pacific_z_gain<- here_read("t2_warm_pacific_z_gain")
t2_warm_pacific_z_zero<- here_read("t2_warm_pacific_z_zero")
t2_warm_pacific_z_null<- here_read("t2_warm_pacific_z_null")


contrast_t2_warm_pacific_z <-
  lmtp_contrast(t2_warm_pacific_z_gain, ref =  t2_warm_pacific_z_zero, type = "additive")

tab_contrast_t2_warm_pacific_z <- margot::margot_lmtp_evalue(
  contrast_t2_warm_pacific_z,
  scale = "RD",
  new_name = "community socialising: warm pacific"
)
tab_contrast_t2_warm_pacific_z
#output_tab_contrast_t2_warm_pacific_z <- lmtp_evalue_tab(tab_contrast_t2_warm_pacific_z,  delta = 1, sd = 1, scale = c("RD"))
# 
# 

t2_warm_pacific_z_gain
t2_warm_pacific_z_null
contrast_t2_warm_pacific_z_null <-
  lmtp_contrast(t2_warm_pacific_z_gain, ref =  t2_warm_pacific_z_null, type = "additive")

tab_contrast_t2_warm_pacific_z_null<- margot::margot_lmtp_evalue(
  contrast_t2_warm_pacific_z_null,
  scale = "RD",
  new_name = "community socialising: warm pacific"
)

tab_contrast_t2_warm_pacific_z_null

# refugees ----------------------------------------------------------------


t2_warm_refugees_z_gain<- here_read("t2_warm_refugees_z_gain")
t2_warm_refugees_z_zero<- here_read("t2_warm_refugees_z_zero")
t2_warm_refugees_z_null<- here_read("t2_warm_refugees_z_null")


contrast_t2_warm_refugees_z <-
  lmtp_contrast(t2_warm_refugees_z_gain, ref =  t2_warm_refugees_z_zero, type = "additive")

tab_contrast_t2_warm_refugees_z <-  margot::margot_lmtp_evalue(
  contrast_t2_warm_refugees_z,
  scale = "RD",
  new_name = "community socialising: warm refugees"
)


# 
# 
contrast_t2_warm_refugees_z_null <-
  lmtp_contrast(t2_warm_refugees_z_gain, ref =  t2_warm_refugees_z_null, type = "additive")

tab_contrast_t2_warm_refugees_z_null <-  margot::margot_lmtp_evalue(
  contrast_t2_warm_refugees_z_null,
  scale = "RD",
  new_name = "community socialising: warm refugees"
)


# 
# 
# # perceive gender discrim -------------------------------------------------
# t2_perc_gend_discrim_z_gain<- here_read("t2_perc_gend_discrim_z_gain")
# t2_perc_gend_discrim_z_zero<- here_read("t2_perc_gend_discrim_z_zero")
# t2_perc_gend_discrim_z_null<- here_read("t2_perc_gend_discrim_z_null")

# 
# contrast_t2_perc_gend_discrim_z <-
#   lmtp_contrast(t2_perc_gend_discrim_z_gain, ref =  t2_perc_gend_discrim_z_zero, type = "additive")
# 
# tab_contrast_t2_perc_gend_discrim_z <- margot_tab_lmtp(
#   contrast_t2_perc_gend_discrim_z,
#   scale = "RD",
#   new_name = "community socialising: perceive gender discrim"
# )
# 
# output_tab_contrast_t2_perc_gend_discrim_z <- lmtp_evalue_tab(tab_contrast_t2_perc_gend_discrim_z ,  delta = 1, sd = 1, scale = c("RD"))
# 
# # 
# contrast_t2_perc_gend_discrim_z_null <-
#   lmtp_contrast(t2_perc_gend_discrim_z_gain, ref =  t2_perc_gend_discrim_z_null, type = "additive")
# 
# tab_contrast_t2_perc_gend_discrim_z_null <- margot_tab_lmtp(
#   contrast_t2_perc_gend_discrim_z_null,
#   scale = "RD",
#   new_name = "community socialising: perceive gender discrim"
# )
# 
# output_tab_contrast_t2_perc_gend_discrim_z_null <- lmtp_evalue_tab(tab_contrast_t2_perc_gend_discrim_z_null ,  delta = 1, sd = 1, scale = c("RD"))
# # 
# # perceive rel discrim ----------------------------------------------------
# t2_perc_religious_discrim_z_gain<- here_read("t2_perc_religious_discrim_z_gain")
# t2_perc_religious_discrim_z_zero<- here_read("t2_perc_religious_discrim_z_zero")
# t2_perc_religious_discrim_z_null<- here_read("t2_perc_religious_discrim_z_null")
# 
# contrast_t2_perc_religious_discrim_z <-
#   lmtp_contrast(t2_perc_religious_discrim_z_gain, ref =  t2_perc_religious_discrim_z_zero, type = "additive")
# 
# tab_contrast_t2_perc_religious_discrim_z <- margot_tab_lmtp(
#   contrast_t2_perc_religious_discrim_z,
#   scale = "RD",
#   new_name = "community socialising: perceive religious discrim"
# )
# 
# output_tab_contrast_t2_perc_religious_discrim_z <- lmtp_evalue_tab( tab_contrast_t2_perc_religious_discrim_z,  delta = 1, sd = 1, scale = c("RD"))
# output_tab_contrast_t2_perc_religious_discrim_z
# 
# output_tab_contrast_t2_perc_religious_discrim_z
# # 
# # 
# contrast_t2_perc_religious_discrim_z_null <-
#   lmtp_contrast(t2_perc_religious_discrim_z_gain, ref =  t2_perc_religious_discrim_z_null, type = "additive")
# 
# 
# tab_contrast_t2_perc_religious_discrim_z_null<- margot_tab_lmtp(
#   contrast_t2_perc_religious_discrim_z_null,
#   scale = "RD",
#   new_name = "community socialising: perceive religious discrim"
# )
# 
# output_tab_contrast_t2_perc_religious_discrim_z_null <- lmtp_evalue_tab( tab_contrast_t2_perc_religious_discrim_z_null,
#                                                                          delta = 1, sd = 1, scale = c("RD"))
# 
# output_tab_contrast_t2_perc_religious_discrim_z_null
# 
# push_mods
# # perceive ethnic discrim -------------------------------------------------
# # 
# t2_perc_discrim_z_gain<- here_read("t2_perc_discrim_z_gain")
# t2_perc_discrim_z_zero<- here_read("t2_perc_discrim_z_zero")
# t2_perc_discrim_z_null<- here_read("t2_perc_discrim_z_null")
# 
# 
# 
# contrast_t2_perc_discrim_z <-
#   lmtp_contrast(t2_perc_discrim_z_gain, ref =  t2_perc_discrim_z_zero, type = "additive")
# 
# tab_contrast_t2_perc_discrim_z <- margot_tab_lmtp(
#   contrast_t2_perc_discrim_z,
#   scale = "RD",
#   new_name = "community socialising: perceive ethnic discrim"
# )
# 
# output_tab_contrast_t2_perc_discrim_z <- lmtp_evalue_tab( tab_contrast_t2_perc_discrim_z,  delta = 1, sd = 1, scale = c("RD"))
# 
# 
# 
# contrast_t2_perc_discrim_z_null <-
#   lmtp_contrast(t2_perc_discrim_z_gain, ref =  t2_perc_discrim_z_null, type = "additive")
# 
# tab_contrast_t2_perc_discrim_z_null <- margot_tab_lmtp(
#   contrast_t2_perc_discrim_z_null,
#   scale = "RD",
#   new_name = "community socialising: perceive ethnic discrim"
# )
# 
# output_tab_contrast_t2_perc_discrim_z_null <- lmtp_evalue_tab( tab_contrast_t2_perc_discrim_z_null,  delta = 1, sd = 1, scale = c("RD"))
# 
# output_tab_contrast_t2_perc_discrim_z_null

# tables and graphs -------------------------------------------------------

# 
tab_all_warm <- rbind(
  tab_contrast_t2_warm_asians_z,
  tab_contrast_t2_warm_chinese_z,
  tab_contrast_t2_warm_immigrants_z,
  tab_contrast_t2_warm_indians_z,
  tab_contrast_t2_warm_elderly_z,
  tab_contrast_t2_warm_maori_z,
  tab_contrast_t2_warm_mental_illness_z,
  tab_contrast_t2_warm_muslims_z,
  tab_contrast_t2_warm_nz_euro_z,
  tab_contrast_t2_warm_overweight_z,
  tab_contrast_t2_warm_pacific_z,
  tab_contrast_t2_warm_refugees_z)

group_tab_all_warm <- group_tab(tab_all_warm, type = "RD")
group_tab_all_warm
here_save(tab_all_warm, "tab_all_warm")
here_save(group_tab_all_warm, "group_tab_all_warm")



# 
# tab_all_perceive <-
#   rbind(
#     output_tab_contrast_t2_perc_gend_discrim_z,
#     output_tab_contrast_t2_perc_religious_discrim_z,
#     output_tab_contrast_t2_perc_discrim_z
#   )
# 
# 
# group_tab_all_perceive <- group_tab(tab_all_perceive,  type = "RD")
# 
# here_save(tab_all_perceive , "tab_all_perceive")
# here_save(group_tab_all_perceive, "group_tab_all_perceive")
# 

# 

tab_all_warm_null <- rbind(
  tab_contrast_t2_warm_asians_z_null,
  tab_contrast_t2_warm_chinese_z_null,
  tab_contrast_t2_warm_immigrants_z_null,
  tab_contrast_t2_warm_indians_z_null,
  tab_contrast_t2_warm_elderly_z_null,
  tab_contrast_t2_warm_maori_z_null,
  tab_contrast_t2_warm_mental_illness_z_null,
  tab_contrast_t2_warm_muslims_z_null,
  tab_contrast_t2_warm_nz_euro_z_null,
  tab_contrast_t2_warm_overweight_z_null,
  tab_contrast_t2_warm_pacific_z_null,
  tab_contrast_t2_warm_refugees_z_null)

group_tab_all_warm_null <- group_tab(tab_all_warm_null, type = "RD")

here_save(tab_all_warm_null, "tab_all_warm_null")
here_save(group_tab_all_warm_null, "group_tab_all_warm_null")


# 
# 
# tab_all_perceive <-
#   rbind(
#     output_tab_contrast_t2_perc_gend_discrim_z,
#     output_tab_contrast_t2_perc_religious_discrim_z,
#     output_tab_contrast_t2_perc_discrim_z
#   )
# 
# 
# group_tab_all_perceive <- group_tab(tab_all_perceive,  type = "RD")
# 
# here_save(tab_all_perceive , "tab_all_perceive")
# here_save(group_tab_all_perceive, "group_tab_all_perceive")
# 


# #
# tab_all_perceive_null <-
#   rbind(
#     output_tab_contrast_t2_perc_gend_discrim_z_null,
#     output_tab_contrast_t2_perc_religious_discrim_z_null,
#     output_tab_contrast_t2_perc_discrim_z_null
#   )
# 
# 
# group_tab_all_perceive_null <- group_tab(tab_all_perceive_null,  type = "RD")

here_save(tab_all_perceive_null, "tab_all_perceive_null")
here_save(group_tab_all_perceive_null, "group_tab_all_perceive_null")


# graphs ------------------------------------------------------------------


title = "Hours with Community: One vs None"
title_null = "Hours with Community One Per Week vs No Intervention"

conflicted::conflicts_prefer(ggplot2::margin())
plot_group_tab_all_warm <- margot_plot(
  group_tab_all_warm,
  type = "RD",
  title =  "Hours with Community:  vs None",
  subtitle = "Out-Group Warmth",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = .5,
  title_size = 20,
  subtitle_size = 14,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.5,
  x_lim_lo = -.5,
  x_lim_hi = .5
)

plot_group_tab_all_warm

push_mods
ggsave(
  plot_group_tab_all_warm,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_all_warm.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)

# 
plot_group_tab_all_warm_null <- margot_plot(
  group_tab_all_warm_null,
  type = "RD",
  title = "Hours with Community: vs No Intervention",
  subtitle = "Out-Group Warmth",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = 2.5,
  title_size = 20,
  subtitle_size = 14,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.5,
  x_lim_lo = -.5,
  x_lim_hi = .5
  
)
# 
plot_group_tab_all_warm_null
plot_group_tab_all_warm
# 
# 
ggsave(
  plot_group_tab_all_warm_null,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "sl_lib_orig_plot_group_tab_all_warm_null.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)



plot_group_tab_all_perceive <- margot_plot(
  group_tab_all_perceive,
  type = "RD",
  title =  "Hours with Community: At Least Once Per Week vs None",
  subtitle = "Perceived Discrimination",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = 2.5,
  title_size = 20,
  subtitle_size = 14,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.5,
  x_lim_lo = -.5,
  x_lim_hi = .5
)

plot_group_tab_all_perceive

ggsave(
  plot_group_tab_all_perceive,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_all_perceive.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)


# 
plot_group_tab_all_perceive_null <- margot_plot(
  group_tab_all_perceive_null,
  type = "RD",
  title = "Religious Service At Least Once Per Week vs No Intervention",
  subtitle = "Perceived Discrimination",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = 2.5,
  title_size = 20,
  subtitle_size = 14,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.5,
  x_lim_lo = -.5,
)

plot_group_tab_all_perceive_null

# ggsave(
#   plot_group_tab_all_perceive_null,
#   path = here::here(here::here(push_mods)),
#   width = 16,
#   height = 9,
#   units = "in",
#   filename = "plot_group_tab_all_perceive.png",
#   device = 'png',
#   limitsize = FALSE,
#   dpi = 300
# )


# read material -----------------------------------------------------------


tab_all_warm <- here_read("tab_all_warm")
group_tab_all_warm<- here_read("group_tab_all_warm")

tab_all_warm<- here_read("tab_all_warm")
group_tab_all_warm<- here_read("group_tab_all_warm")


# plot_group_tab_all_perceive <- here_read(plot_group_tab_all_perceive, "plot_group_tab_all_perceive")
# plot_group_tab_all_warm<- here_read(plot_group_tab_all_warm, "plot_group_tab_all_warm")


transition_table <- here_read("transition_table")
transition_table_out_binary <-
  here_read("transition_table_out_binary")

n_participants <- here_read("n_participants")

table_baseline <- here_save("table_baseline")
table_exposures <- here_save("table_exposures")
table_outcomes <- here_save("table_outcomes")


graph_density_of_exposure_down<- here_read("graph_density_of_exposure_down")
graph_density_of_exposure_up<- here_read("graph_density_of_exposure_up")

df_clean$t0_alert_level_combined_lead
df_clean_nona<- df_clean |> drop_na() |> droplevels()

library(splines)
test_pacific <- glm(t2_warm_pacific_z ~ t1_religion_church_round + (bs(t0_religion_church_round, k = 2) + bs(t0_warm_pacific_z, k = 1) + 
                                                                      (t0_alert_level_combined_lead)), family = gaussian,
                    df_clean_nona, weights = t0_sample_weights)

test_pacific <- glm(t2_warm_pacific_z ~ t1_religion_church_round * ((t0_religion_church_round) + (t0_warm_pacific_z) + 
                                                                      (t0_alert_level_combined_lead)), family = gaussian,
                    df_clean_nona, weights = NULL)

baseline_vars
summary(test_pacific)
hist( df_clean$t0_warm_pacific_z )

hist(t0_ethnic)
