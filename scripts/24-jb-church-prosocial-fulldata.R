# 7 april
# original script is in the 00drafts folder.
# this script brings the analysis for this study to the 'models" workflow

### ALWAYS RESTART R IN A FRESH SESSION ####
listWrappers()
push_mods <-  fs::path_expand(
  "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/24-church-prejudice"
)

#devtools::install_github("go-bayes/margot")
#devtools::install_github("nt-williams/lmtp@devel")

# get devtools
if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# get 'margot' from my github (make sure to update)
# devtools::install_github("go-bayes/margot")



# Check if pacman is installed; if not, install it
if (!require(pacman, quietly = TRUE)) {
  install.packages("pacman")
  library(pacman)
}

# use p_load to load / install the packages
pacman::p_load(
  skimr,
  naniar,
  WeightIt,
  clarify,
  MatchThem,
  cobalt,
  MatchIt,
  kableExtra,
  janitor,
  lmtp,
  SuperLearner,
  ranger,
  xgboost,
  glmnet,
  doParallel,
  ggplot2,
  here,
  naniar,
  gtsummary,
  grf,
  progressr,
  tidyverse,
  ggplot2,
  parameters,
  kableExtra
)

# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
# source("/Users/joseph/GIT/templates/functions/libs2")

pull_path <-
  fs::path_expand(
    #"/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs_refactor/nzavs_data_23"
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data_qs"
  )

# read data: note that you need use the arrow package in R

#dat <- arrow::read_parquet("/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data")

dat <- qs::qread(here::here(pull_path))

# check path:is this correct?  check so you know you are not overwriting other directors
push_mods


# total nzavs participants
n_total <- skimr::n_unique(dat$id)

# get comma in number
n_total <- prettyNum(n_total, big.mark = ",")

# check n total
n_total

# save n for manuscript
margot::here_save(n_total, "n_total")

# set exposure here
nzavs_exposure <- "religion_church_round"


!!sym(exposure_var_string)
name_exposure_raw <- "religion_church"

# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 10

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(0112358)
library(future)
plan(multisession)
n_cores <- parallel::detectCores()-1

listWrappers()

# super learner libraries
sl_lib <- c("SL.glmnet",
            "SL.ranger",
            "SL.xgboost")


# check
push_mods

# check colnames
colnames(dat)
name_exposure_raw <- "religion_church"
# Obtain IDs for individuals who participated in 2018 and have no missing baseline exposure
ids_2018 <- dat %>%
  filter(year_measured == 1, wave == 2018) %>%
  filter(!is.na(!!sym(name_exposure_raw))) |> # criteria, no missing
  pull(id)

# Obtain IDs for individuals who participated in 2019
# ids_2019 <- dat %>%
#   filter(year_measured == 1, wave == 2019) %>%
#   filter(!is.na(!!sym(name_exposure_raw))) |> # criteria, no missing
#   pull(id)
# 
# # Intersect IDs from 2018 and 2019 to ensure participation in both years
# ids_2018_2019 <- intersect(ids_2018, ids_2019)



# filter the original dataset for these IDs three waves
dat <- as.data.frame(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_widths(dat)
str(dat)


dat_long <- dat |>
  dplyr::filter(id %in% ids_2018 &
                  wave %in% c(2018, 2019, 2020)) |>
  arrange(id, wave) |>
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
    # "parent",
    # 0 = no, 1 = yes
    "political_conservative",
    #Please rate how politically liberal versus conservative you see yourself as being.
    #    "pol_wing",
    # Please rate how politically left-wing versus right-wing you see yourself as being.
    "sample_frame_opt_in",
    # see NZAVS,
    #Do you have siblings?
    "total_siblings",
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
    #Hours spent … socialising with friends
    #Hours spent … socialising with community groups
    #Hours spent … socialising with family
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
   # "w_gend_age_ethnic",
    # perc_age_discrim,
    # "perc_gend_discrim",
    # "perc_religious_discrim",
    # "perc_discrim",
    "neighbourhood_community",
    # #I feel a sense of community with others in my local neighbourhood.
    "support",
    "belong",
    "rural_gch_2018_l",
    "hlth_disability",
    # value label 0    No 1   Yes
    # see NZAVS,
    "has_siblings",
    "children_num",
    # How many children have you given birth to, fathered, or adopted?
    "hours_children",
    #"hours_community",
    #"hours_friends",
    # "hours_family",
    # "hours_religious_community",
    #Hours - Looking after children
    # "religion_perceive_religious_discrim",
    #	I feel that I am often discriminated against because of my religious/spiritual beliefs.
    # "bigger_doms", #What religion or spiritual group?#  Not_Rel, Anglican , Buddist, Catholic , Christian_nfd, Christian_Others, Hindu, Jewish           Muslim, PresbyCongReform, TheOthers
    "modesty",
    # see above
    "charity_donate",
    #How much money have you donated to charity in the last year?
    "hours_charity",
    #,#Hours spent in activities/Hours spent … voluntary/charitable work
    # "warm_asians",
    # "warm_chinese",
    # #"warm_disabled" ,  missing at time 0
    # # begins w9
    # "warm_immigrants",
    # "warm_indians",
    # "warm_elderly",
    # # warm_lgbtq starts w12
    # "warm_maori",
    # "warm_mental_illness",
    # "warm_muslims",
    # "warm_nz_euro",
    # "warm_overweight",
    # "warm_pacific",
    # "warm_refugees",
    # "issue_same_sex_marriage", not in range
    "support",
    # three items as below
    # "support_help",
    # # 'There are people I can depend on to help me if I really need it.
    # "support_turnto",
    # # There is no one I can turn to for guidance in times of stress.
    # "support_rnoguidance",
    #There is no one I can turn to for guidance in times of stress.
    "family_time",
    "friends_time",
    "community_time",
    "family_money",
    "friends_money",
    "community_money",
    "hours_community",
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
    "kessler6_sum",
    # "kessler_latent_depression",
    # "kessler_latent_anxiety",
    "hlth_fatigue",
    "hlth_sleep_hours",
    "support",
    #   "support_help",
    # 'There are people I can depend on to help me if I really need it.
    #  "support_turnto",
    # There is no one I can turn to for guidance in times of stress.
    #  "support_noguidance_reverseed",
    #There is no one I can turn to for guidance in times of stress.
    "belong",
    #    "belong_accept",
    #Know that people in my life accept and value me.
    #    "belong_routside_reversed",
    # Feel like an outsider.
    #  "belong_beliefs",
    # Know that people around me share my attitudes and beliefs.
    "family_money",
    "friends_money",
    "community_money",
    "alert_level_combined_lead",
    "alert_level_combined"
  ) |>
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
  dplyr::mutate(
    friends_money = ifelse(friends_money < 0, 0, friends_money),
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
    #  hours_religious_community_log =  log(hours_religious_community + 1)
    #  children_num_log = log(children_num + 1)
  ) |>
  dplyr::select(
    -c(
      hours_work,
      hours_housework,
      household_inc,
      hours_exercise,
      hours_children,
      has_siblings,
      hours_community
      #    children_num,
      #    total_siblings
    )
  ) |>
  droplevels() |>
  dplyr::rename(#sample_weights = w_gend_age_ethnic,
                sample_origin =  sample_origin_names_combined) |>
  dplyr::mutate(
    # make indicators binary
    family_time_binary = as.integer(ifelse(family_time > 0, 1, 0)),
    friends_time_binary = as.integer(ifelse(friends_time > 0, 1, 0)),
    community_time_binary = as.integer(ifelse(community_time > 0, 1, 0)),
    family_money_binary = as.integer(ifelse(family_money> 0, 1, 0)),
    friends_money_binary = as.integer(ifelse(friends_money > 0, 1, 0)),
    community_money_binary = as.integer(ifelse(community_money> 0, 1, 0))
  ) |>  #shorter name
  dplyr::rename(short_form_health = sfhealth) |> 
  dplyr::select(
    -c(
      religion_church,
      family_time,
      friends_time,
      community_time,
      # hours_community,
      #  hours_family,
      #  hours_friends,
      community_money,
      friends_money,
      family_money#,
      #    hours_religious_community
    )
  ) |>
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
  mutate(
    rural_gch_2018_l = as.numeric(as.character(rural_gch_2018_l)),
    #   parent = as.numeric(as.character(parent)),
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
str(dat_long)
# check
table(dat_long$censored)

# select vars for baseline
dat_long_colnames <- colnames(dat_long)

dat_long_colnames <- sort(dat_long_colnames)

dat_long_colnames

# select vars for baseline
dat_long_colnames <- colnames(dat_long)

dat_long_colnames <- sort(dat_long_colnames)
dat_long_colnames
dat_long_colnames <- setdiff(dat_long_colnames, c("alert_level_combined", "alert_level_combined_lead", "sample_weights"))

dat_long_colnames


# set baseline exposure and outcomes --------------------------------------

exposure_var = c("religion_church_round",
                 "censored"#,
) #


# set outcomes for prosocial domain
# save prejudice for separate paper
outcome_vars = c(
  "hours_charity",
  "charity_donate",
  "family_time_binary",
  "friends_time_binary",
  "community_time_binary",
  "neighbourhood_community",
  "family_money_binary",
  "community_money_binary",
  "friends_money_binary",
  "support",
  "belong"
)

dat_long_colnames <- colnames(dat_long)
#
baseline_vars <-
  setdiff(dat_long_colnames,
          c("id","wave", "alert_level_combined", "alert_level_combined_lead", "sample_weights"))

# c(outcome_vars, 'id', 'wave'))
baseline_vars
baseline_vars <- sort(baseline_vars)

baseline_vars

# just core baseline variables
base_var <-
  setdiff(baseline_vars, c("censored", "sample_weights", "alert_level_combined_lead", outcome_vars))
base_var


#community at baseline
n_participants <-
  n_unique(dat_long$id) #47202 # reports hours with

# check
n_participants #46377

margot::here_save(n_participants, "n_participants")

# double check path
push_mods

# check col names
colnames(dat)

# assess positivity
dat_long$wave

dt_positivity_full <- dat_long |>
  filter(wave == 2018 | wave == 2019) |>
  select(wave, id, religion_church_round, sample_weights) |>
  mutate(religion_church_shift_gain = ifelse(religion_church_round >= 4, 1, 0)) |> 
  mutate(religion_church_shift_zero = ifelse(religion_church_round > 0, 1, 0))




# create transition matrix
out <-
  msm::statetable.msm(religion_church_round, id, data = dt_positivity_full)


library(margot)

out <-margot::create_transition_matrix(data = dt_positivity_full, state_var = "religion_church_round", id_var = "id")


# t_tab_2_labels <- c("< weekly", ">= weekly")
# transition table

transition_table  <- margot::transition_table(out)
transition_table
# for import later
margot::here_save(transition_table, "transition_table")



out_church_zero <- margot::create_transition_matrix(data = dt_positivity_full, state_var = "religion_church_shift_zero", id_var = "id")


t_tab_2_labels_zero <- c("0", "> 0")

transition_table_binary_zero <-
  margot::transition_table(out_church_zero,
                           state_names = t_tab_2_labels_zero)

transition_table_binary_zero

# for import later
here_save(transition_table_binary_zero,
          "transition_table_binary_zero")



# transition table
out_church_gain <- margot::create_transition_matrix(data = dt_positivity_full, state_var = "religion_church_shift_gain", id_var = "id")


t_tab_2_labels_gain <- c(">=4", "< 4")

transition_table_binary_gain <-
  margot::transition_table(out_church_gain,
                           state_names = t_tab_2_labels_gain)

transition_table_binary_gain

# for import later
here_save(transition_table_binary_gain,
          "transition_table_binary_gain")


# sd values ---------------------------------------------------------------

dt_outcome <-
  dat_long |>
  filter(wave == 2020)


dt_outcome$religion_church_round
mean_donations <-
  mean(dt_outcome$charity_donate, na.rm = TRUE)
mean_volunteer <-
  mean(dt_outcome$hours_charity, na.rm = TRUE)


mean_donations
margot::here_save(mean_donations, "mean_donations")
mean_volunteer
margot::here_save(mean_volunteer, "mean_volunteer")

push_mods


sd_donations <-
  sd(dt_outcome$charity_donate, na.rm = TRUE)
sd_volunteer <-
  sd(dt_outcome$hours_charity, na.rm = TRUE)



# save for manuscript
here_save(sd_donations, "sd_donations")
here_save(sd_volunteer, "sd_volunteer")

# read
sd_donations <-
  here_read("sd_donations")
sd_volunteer <-
  here_read("sd_volunteer")


sd_donations
sd_volunteer

baseline_vars

# check
baseline_vars

#
# check associations only -------------------------------------------------

dt_18 <- dat_long|>
  filter(wave == 2018) 


table(dt_18$censored)



# check association only
#dt_18_miss$sample_weights

dt_18_miss <- dt_18 |> 
  mutate(hours_charity_z = scale(hours_charity))

naniar::vis_miss(dt_18_miss, warn_large_data = F)

table((dt_18_miss$hours_religious_community))
dev.off()

# base_vars set above
fit_church_on_charity_donate <-
  margot::regress_with_covariates(
    dt_18_miss,
    outcome = "charity_donate",
    exposure = "religion_church_round",
    baseline_vars = base_var,
    sample_weights = "sample_weights"
  )
parameters::model_parameters(fit_church_on_charity_donate, ci_method="wald")[2, ] 
margot::here_save(fit_church_on_charity_donate, "fit_church_on_charity_donate")

#fit_church_on_hours_charity
#(0.198274  + 0.168511) * 60

fit_church_on_hours_charity <-
  margot::regress_with_covariates(
    dt_18_miss,
    outcome = "hours_charity",
    exposure = "religion_church_round",
    baseline_vars = base_var,
    sample_weights = "sample_weights"
  )
parameters::model_parameters(fit_church_on_hours_charity, ci_method="wald")[2, ]
margot::here_save(fit_church_on_hours_charity, "fit_church_on_hours_charity")

fit_church_on_community_time_binary <-
  margot::regress_with_covariates(
    dt_18,
    outcome = "community_time_binary",
    exposure = "religion_church_round",
    baseline_vars = base_var,
    family = "poisson"
  )
parameters::model_parameters(fit_church_on_community_time_binary, ci_method="wald")[2, ]
here_save(fit_church_on_community_time_binary, "fit_church_on_community_time_binary")


fit_church_on_community_money_binary <-
  margot::regress_with_covariates(
    dt_18,
    outcome = "community_money_binary",
    exposure = "religion_church_round",
    baseline_vars = base_var,
    family = "poisson"
  )
parameters::model_parameters(fit_church_on_community_money_binary, ci_method="wald")[2, ]

margot::here_save(fit_church_on_community_money_binary, "fit_church_on_community_money_binary")

fit_church_on_charity_donate<- margot::here_read('fit_church_on_charity_donate')
fit_church_on_hours_charity<- margot::here_read('fit_church_on_hours_charity')
fit_church_on_community_time_binary<- margot::here_read('fit_church_on_community_time_binary')
fit_church_on_community_money_binary<- margot::here_read('fit_church_on_community_money_binary')


# run once then comment out

# lm_coef_fit_church_on_charity_donate <- tbl_regression(fit_church_on_charity_donate)
# b_church_on_charity_donate <-inline_text(lm_coef_fit_church_on_charity_donate, variable = religion_church_round, pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")
# # # #
# b_church_on_charity_donate
# here_save(b_church_on_charity_donate, "b_church_on_charity_donate")
# 
# lm_coef_fit_church_on_hours_charity <- tbl_regression(fit_church_on_hours_charity)
# lm_coef_fit_church_on_hours_charity
# b_church_on_hours_charity <-inline_text(lm_coef_fit_church_on_hours_charity, variable = religion_church_round, pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")
# b_church_on_hours_charity
# # b_church_on_charity_donate
# here_save(b_church_on_hours_charity, "b_church_on_hours_charity")

# tables ------------------------------------------------------------------
library(gtsummary)

# table baseline ----------------------------------------------------------
# get names

selected_base_cols <-
  dt_18 |> select(all_of(base_var))

#check
colnames(selected_base_cols)

#chck
#selected_base_cols <- setdiff(selected_base_cols)
# baseline table

library(gtsummary)
table_baseline <- selected_base_cols |> 
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
# 

# table exposure ----------------------------------------------------------

# get first and second wave
dt_18_19 <- dat_long_full |> 
  dplyr::filter(wave == 2018 | wave == 2019) |> 
  droplevels()

# get vars.
selected_exposure_cols <-
  dt_18_19 %>% select(
    c(
      "religion_church_round",
      "alert_level_combined",
      "wave"
    )
  )

# check
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
  modify_header(label = "**Exposure Variables by Wave**") %>%  # Update the column header
  bold_labels()

table_exposures


# save baseline
here_save(table_exposures, "table_exposures")

table_exposures


# outcome table -----------------------------------------------------------
dt_18_20 <- dat_long_full |> 
  dplyr::filter(wave == 2018 | wave == 2020) |> 
  droplevels()

names_outcomes_tab <- setdiff(outcome_vars, dt_18_20)
names_outcomes_sorted <- sort(names_outcomes_tab)
names_outcomes_final <- names_outcomes_sorted # consistent workflow

names_outcomes_final

names_outcomes_final



# names_outcomes_final
# better names
selected_outcome_cols <-
  dt_18_20 %>% select(all_of(names_outcomes_final),
                      wave) |>
  mutate(Volunteers_binary = factor(ifelse(hours_charity > 0, "yes", "no"),
                                    levels = c("no", "yes"))) |> 
  rename(
    Social_belonging = belong,
    Annual_charity = charity_donate,
    Volunteering_hours = hours_charity,
    Community_gives_money_binary = community_money_binary,
    Community_gives_time_binary = community_time_binary,
    Family_gives_money_binary = family_money_binary,
    Family_gives_time_binary = family_time_binary,
    Friends_give_money_binary = friends_money_binary,
    Friends_give_time = friends_time_binary,
    Social_support = support,
    Sense_neighbourhood_community = neighbourhood_community
  )

# order names correctly
selected_outcome_cols <- selected_outcome_cols %>%
  select(sort(names(selected_outcome_cols)))

# checks
str(selected_outcome_cols)
colnames(selected_outcome_cols)

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
  modify_header(label = "**Outcome Variables by Wave**") %>%  # Update the column header
  bold_labels()

table_outcomes

here_save(table_outcomes, "table_outcomes")
table_outcomes <- here_read("table_outcomes")
table_outcomes
# histogram exposure ------------------------------------------------------

dt_19 <- dat_long |> 
  filter(wave == 2019)

library(ggplot2)
library(dplyr)
#
# # generate bar plot
graph_density_of_exposure_up <- margot::coloured_histogram_shift(
  dt_19,
  col_name = "religion_church_round",
  binwidth = .5, 
  range_highlight = c(0,3.9)
)
graph_density_of_exposure_up


graph_density_of_exposure_down <- margot::coloured_histogram_shift(
  dt_19,
  shift = "down",
  col_name = "religion_church_round",
  binwidth = .5, 
  range_highlight = c(1,8)
)
graph_density_of_exposure_up

graph_density_of_exposure_down

here_save(graph_density_of_exposure_up, "graph_density_of_exposure_up")
here_save(graph_density_of_exposure_down, "graph_density_of_exposure_down")
#
# graph_density_of_exposure
#
# here_save(graph_density_of_exposure, "graph_density_of_exposure")
# ggsave(
#   graph_density_of_exposure_up,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "graph_density_of_exposure_up.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )
# 
# ggsave(
#   graph_density_of_exposure_down,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "graph_density_of_exposure_down.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )
# 



# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
# source("/Users/joseph/GIT/templates/functions/funs.R")

# # ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source(
#   "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
# )
# # devtools::install_github("go-bayes/margot")


# impute baseline ---------------------------------------------------------
# impute baseline data (we use censoring for the outcomes)
#colnames(dat_long)
# function imputes only baseline not outcome
#
#devtools::install_github("go-bayes/margot")

dat_long$sample_weights
dat_long_df <- data.frame(dat_long)

my_data_filtered <- as.data.frame(dat_long_df)
my_data_filtered <- haven::zap_formats(dat_long_df)
my_data_filtered <- haven::zap_label(dat_long_df)
my_data_filtered <- haven::zap_widths(dat_long_df)

# needs to be a data frame
dat_long_df <- data.frame(dat_long)

# check before committing
colnames(dat_long_df)
baseline_vars
prep_coop_all <-
  margot_wide_impute_baseline(
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

prep_coop_all$t0_sample_weights
prep_coop_all$t0_alert_level_combined
prep_coop_all$t1_alert_level_combined

margot::here_save(prep_coop_all, "prep_coop_all")

# save function -- will save to your "push_mod" directory
margot::here_save(prep_coop_all, "prep_coop_all")

# Warning messages:
#   1: Number of logged events: 190 
# 2: Using an external vector in selections was deprecated in tidyselect 1.1.0.
# ℹ Please use `all_of()` or `any_of()` instead.
# # Was:
# data %>% select(t0_column_order)
# 
# # Now:
# data %>% select(all_of(t0_column_order))
# 
# See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.








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




# Assuming df_wide_censored is your dataframe

# Calculate the conditions before the mutate steps
t0_na_condition <- rowSums(is.na(select(df_wide_censored, starts_with("t1_")))) > 0
#t1_na_condition <- rowSums(is.na(select(df_wide_censored, starts_with("t2_")))) > 0

# check
naniar::vis_miss(df_wide_censored, warn_large_data = FALSE)


# Assuming df_wide_censored is your dataframe

# Calculate the conditions before the mutate steps
t0_na_condition <-
  rowSums(is.na(select(df_wide_censored, starts_with("t1_")))) > 0
# t1_na_condition <-
#   rowSums(is.na(select(df_wide_censored, starts_with("t2_")))) > 0
df_clean <- df_wide_censored %>%
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
  # mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%
  # mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
  #        across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  # mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .))) |>
  select(-c(ends_with("_volunteers_binary"))) |> 
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_censored &
        # !t0_nzsei_13_l & 
        !t0_sample_weights & 
        !t0_rural_gch_2018_l & 
       # !t0_nzsei_13_l& 
        !t0_sample_frame_opt_in & 
        # !t0_total_siblings & 
        # !t0_volunteers_binary &
        !t0_family_time_binary &
        !t0_friends_time_binary &
        !t0_community_time_binary &
        !t0_family_money_binary &
        !t0_friends_money_binary &
        !t0_community_money_binary &
        !t0_religion_church_round &
        !t0_alert_level_combined_lead &
        #  !t0_charity_donate & !t0_sample_weights &
        !t1_religion_church_round &
        #    !t1_alert_level_combined &
        !t1_censored &
        # !t2_charity_donate &!t2_volunteers_binary &
        !t2_family_time_binary &
        !t2_friends_time_binary &
        !t2_community_time_binary &
        !t2_family_money_binary &
        !t2_friends_money_binary &
        !t2_community_money_binary,
      #  !t2_hours_charity,
      .fns = ~ scale(.),
      .names = "{.col}_z"
    )
  ) |>
  # select(-t0_charity_donate,
  #        -t0_hours_charity) |>
  select(
    where(is.factor),
    t0_sample_weights,
    t0_rural_gch_2018_l,
  #  t0_nzsei_13_l,
    t0_sample_frame_opt_in,
    t0_alert_level_combined_lead,
    #  t0_volunteers_binary,
    t0_family_time_binary,
    t0_friends_time_binary,
    t0_community_time_binary,
    t0_family_money_binary,
    t0_friends_money_binary,
    t0_community_money_binary,
    t0_sample_weights,
    # t0_total_siblings,
    t0_religion_church_round,
    t0_censored,
    #  t1_alert_level_combined,
    t1_religion_church_round,
    #t1_hours_community_round,
    t1_censored,
    # t2_charity_donate,
    # t2_hours_charity,
    # t2_volunteers_binary,
    t2_family_time_binary,
    t2_friends_time_binary,
    t2_community_time_binary,
    t2_family_money_binary,
    t2_friends_money_binary,
    t2_community_money_binary,
    ends_with("_z")
  ) |>
  mutate(t0_lost = 1 - t0_censored) |> 
  mutate(t1_lost = 1 - t1_censored) |> 
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))


#check
naniar::vis_miss(df_clean, warn_large_data = FALSE)


# checks
table(df_clean$t1_lost)
table(df_clean$t0_lost)

test <- df_wide_censored |> filter(t0_censored == 1)
nrow(test)

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
# read data --  start here if previous work already done


# imputed data already from previous study
df_clean <- here_read("df_clean")
colnames(df_clean)
str(df_clean)
# names of vars for modelling


names_base <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     #  -t0_volunteers_binary, # redundant
                     -t0_censored) |> colnames()

names_base

here_save(names_base, "names_base")
names_base <- here_read("names_base")

push_mods
names_outcomes <-
  df_clean |> select(starts_with("t2")) |> colnames()

names_outcomes

here_save(names_outcomes, "names_outcomes")
names_outcomes <- here_read("names_outcomes")

names_base
df_clean$t0_sample_weights



# weights for treament ----------------------------------------------------
df_clean <- here_read("df_clean")


baseline_vars_models = df_clean |>  # post process of impute and combine
  dplyr::select(starts_with("t0"),-t0_censored, -t0_lost, -t0_sample_weights, -t0_alert_level_combined_lead, - t0_sample_weights)|> colnames() # note, we ear

# check
baseline_vars_models


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






#
head(df_clean_hot_code)

df_clean

# new weights
df_clean$t0_combo_weights = df_clean_hot_code$weights * df_clean$t0_sample_weights
df_clean$t0_alert_level_combined_lead <- df_clean$t0_alert_level_combined_lead

min( df_clean$t0_combo_weights)
max( df_clean$t0_combo_weights)

hist( df_clean$t0_combo_weights)

df_clean_t1 <- df_clean |> filter(t0_lost == 0)


hist( df_clean_t1$t0_combo_weights )

max(( df_clean_t1$t0_combo_weights ))
min(df_clean_t1$t0_combo_weights)

#
nrow(df_clean_t1)

table(is.na(df_clean_t1$t1_religion_church_round)) # 33198

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





# matching ----------------------------------------------------------------


# check imbalance ---------------------------------------------------------

df_clean_no_na_treatment_one <- df_clean_t2_hot_code |> filter(!is.na(t1_religion_church_round))

# df_clean_no_na_treatment_base <- df_clean |> filter(!is.na(t0_religion_church_round)) 
# 
# 
# 
df_clean_t2_hot_code
names_base_base <-df_clean_t2_hot_code |> select(starts_with("t0_"), -t0_sample_weights, -t0_censored, -starts_with("t0_alert_level_combined")) |> colnames()

names_base_base
# names_base_base
# 
# 
# match_ebal_base<- margot::match_mi_general(data = df_clean_no_na_treatment_base,
#                                            X = "t0_religion_church_round",
#                                            baseline_vars = names_base_base,
#                                            estimand = "ATE",
#                                            #  focal = 0, #for ATT
#                                            method = "ebal",
#                                            sample_weights = "t0_sample_weights")
# 
# love_plot_base <- love.plot(match_ebal_base, binary = "std", thresholds = c(m = .1),
#                             wrap = 50, position = "bottom", size = 3,  limits = list(m = c(-.5, 1))) 

# love_plot_base
# love_plot_base
# here_save(love_plot_base, "love_plot_base")

# summary_match_ebal_base <- summary(match_ebal_base)
# summary_match_ebal_base
# here_save(summary_match_ebal, "summary_match_ebal")


match_ebal_one<- match_mi_general(data = df_clean_t2_hot_code,
                                  X = "t1_religion_church_round",
                                  baseline_vars = names_base_base,
                                  estimand = "ATE",
                                  #  focal = 0, #for ATT
                                  method = "ebal",
                                  sample_weights = "t0_sample_weights")

match_ebal_one
# save
here_save(match_ebal_one, "match_ebal_one")



bal.tab(match_ebal_one)
love_plot_one <- love.plot(match_ebal_one, binary = "std", thresholds = c(m = .1),
                           wrap = 50, position = "bottom", size = 3,  limits = list(m = c(-.5, 1))) 
love_plot_one
here_save(love_plot_one, "love_plot_one")

# consider results 
summary_match_ebal_one <- summary(match_ebal_one)
summary_match_ebal_one
here_save(summary_match_ebal_one, "summary_match_ebal_one")

plot(summary_match_ebal)



# For trimmed weights e.g.
# #trim if needed (weights > 10 might be a problem)
# match_ebal_trim <- WeightIt::trim(match_ebal_one, at = .99)
# #bal.tab(match_ebal_trim_health)
# summary_match_ebal_trim<- summary(match_ebal_trim)
# plot(summary_match_ebal_trim)
# 


# set variable names ------------------------------------------------------



#### SET VARIABLE NAMES
#  model
# A <- c("t0_religion_church_round", "t1_religion_church_round")
# C <- c("t0_censored", "t1_censored")
# L <- list(c("t0_alert_level_combined"), c("t1_alert_level_combined")) # COULD PUT NULL HERE
# 
names_base
A <- c("t1_religion_church_round")
C <- c("t1_censored")
#L <- list(c("t0_alert_level_combined"), c("t1_alert_level_combined")) # COULD PUT NULL HERE


# redundant because estimate of A is always included: 
# https://muse.jhu.edu/article/883479
# 
# 
# A list of lists is returned with the names of the variables in Ht to be used for estimation of the outcome regression and the treatment mechanism at every time t. Notice that variables A1 and A2 are included in the list of variables used for estimation of the treatment mechamism (trt). This is due to the fact that the nuisance parameter for the treatment mechanism is the density ratio rt, which is a function of A1 and A2.
# The density ratio is estimated based on a classification trick using an auxiliary variable Λ as a pseudo outcome and the treatment as a predictor. We now briefly describe how this density ratio estimation is done; the process is fully automated and hidden from the software user. Specifically, the TMLE and SDR estimation methods require estimation of the ratio of the densities of Adt and At, conditional on the history Ht, defined as rt above. This is achieved through computing the odds in a classification problem in an augmented dataset with 2n observations where the outcome is the auxiliary variable Λ (defined below) and the predictors are the variables At and Ht. In the 2n augmented data set, the data structure at time t is redefined as
# inline graphic
# where Λλ,i = λi indexes duplicate values. For all duplicated observations λ ∈ {0, 1} with the same i, Hλ,i,t is the same. For λ = 0, Aλ,i,t equals the observed exposure values Ai,t, whereas for λ = 1, Aλ,i,t equals the exposure values under the MTP d, namely Adt. The [End Page 111] classification approach to density ratio estimation proceeds by estimating the conditional probability that ∆ = 1 in this dataset, and dividing it by the corresponding estimate of the conditional probability that ∆ = 0. Specifically, denoting pλ the distribution of the data in the augmented dataset, we have:
#   inline graphic
# Further details on this algorithm may be found in our technical paper (Díaz et al., 2021).

# L <- list(c("t0_religion_church_round"), c("t1_religion_church_round"))
# W <- names_base_base

# check

# # 
#
# gain_A <- function(data, trt) {
#   # make zero at baseline
#   mtp_base_A <- function(data, trt) {
#     ifelse(data[[trt]] <= 4, 4, data[[trt]])
#   }
# 
#   if (trt == "t0_religion_church_round") {
#     return(mtp_base_A(data, trt))
#   }
# 
#   # shift to at least 4 at time 1
#   mtp_one_contrast_A <- function(data, trt) {
#     ifelse(data[[trt]] <= 4, 4,  data[[trt]])
#    }
# 
#   #  trt is a variable name passed as a string to the function
# 
#   ifelse(trt == "t1_religion_church_round",
#          mtp_one_contrast_A(data, trt),
#          data[[trt]])
# }
# 
# 
# tt_a <- function(data, trt) {
#   # make zero at baseline
#   mtp_base_A <- function(data, trt) {
#     ifelse(data[[trt]] <= 4, 4,  data[[trt]])
#     }
# 
#   # natural value of treatment at baseline
#   if (trt == "t0_religion_church_round") {
#     return(mtp_base_A(data, trt))
#   }
# 
#   # shift to at least 4 at time 1
#   mtp_one_A <- function(data, trt) {
#     ifelse(data[[trt]] <= 4, 4,  data[[trt]])
#    }
# 
#   #  trt is a variable name passed as a string to the function
#     if (trt == "t1_religion_church_round"){
#         return( mtp_one_A(data, trt))
#     }
# }


# # 
# zero_A <- function(data, trt) {
#   # make zero at baseline
#   mtp_base_Az <- function(data, trt) {
#     ifelse(data[[trt]] > 0, 0,  data[[trt]])
#   }
#   
#   # natural value of treatment at baseline
#   if (trt == "t0_religion_church_round") {
#     return(mtp_base_Az(data, trt))
#   }
#   
#   # shift to at least 4 at time 1
#   mtp_one_Az <- function(data, trt) {
#     ifelse(data[[trt]] > 0 , 0,  data[[trt]])
#   }
#   
#   #  trt is a variable name passed as a string to the function
#   if (trt == "t1_religion_church_round"){
#     return( mtp_one_Az(data, trt))
#   }
# }


# # 
gain_A <- function(data, trt) {
  ifelse(data[[trt]] < 4, 4,  data[[trt]])
}
# 
zero_A <- function(data, trt){
  ifelse( data[[trt]] > 0, 0,  data[[trt]] )
}


# BONUS: progressr progress bars!
progressr::handlers(global = TRUE)

library(future)
plan(multisession)
# n_cores <-
#   parallel::detectCores()-2


# church: charity models ----------------------------------------------------------
n_cores 

library("ranger")

#library("ranger")
full_predictor_vars_final
df_clean_t2_hot_code

names_base_hot <- full_predictor_vars_final

names_base_final <- names_base_hot
here_save(names_base_final, "names_base_final")

#better name
df_final <- df_clean_t2_hot_code
here_save(df_final, "df_final")

# test data 
df_clean_slice <- df_final |>
  slice_head(n = 1000) |>
  as.data.frame()
colnames(df_clean_slice)

library(SuperLearner)
library(xgboost)
library(ranger)



library(SuperLearner)
library(xgboost)
sl_lib

#names_t2_hours_charity_z<- select_and_rename_cols(names_base = names_base, baseline_vars = base_var, outcom = "t2_hours_charity_z")

# summary( lm( t2_charity_donate_z ~ t1_religion_church_round  + 
#                t0_charity_donate_z + t0_religion_church_round, df_clean ) )

## SELECT AND RENAME -- REDUCE DIMENSIONS
# select_and_rename_cols <- function(names_base, baseline_vars, outcome) {
#   # Select columns that match with baseline_vars
#   selected_cols <- names_base[grepl(paste(baseline_vars, collapse = "|"), names_base)]
#   
#   # Rename the outcome variable prefix from t2 to t0
#   outcome_renamed <- gsub("t2_", "t0_", outcome)
#   # Append the renamed outcome to selected columns
#   final_cols <- c(selected_cols, outcome_renamed)
#   
#   return(final_cols)
# }


# redundant

# t2_charity_donate_z_test_gain_orig <- lmtp_tmle(
#   outcome = "t2_charity_donate_z",
#   baseline = names_base_final,
#   shift = gain_A,
#   data = df_clean_slice,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_slice$t0_sample_weights,
#   learners_trt= sl_lib,
#   learners_outcome= sl_lib
# 
# )
# t2_charity_donate_z_test_gain_orig
# 
# 
# t2_charity_donate_z_test_gain_five <- lmtp_tmle(
#   outcome = "t2_charity_donate_z",
#   baseline = names_base_final,
#   shift = gain_A,
#   data = df_clean_slice,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_slice$t0_sample_weights,
#   learners_trt= sl_lib,
#   learners_outcome= sl_lib
# 
# )
# t2_charity_donate_z_test_gain_five

#  As can be verified here: 
# t2_charity_donate_z_test_gain_time_vary <- lmtp_tmle(
#   outcome = "t2_charity_donate_z",
#   data = df_clean_slice,
#   baseline = W,
# #
#   shift = gain_A,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_clean_slice$t0_sample_weights,
#   learners_trt= sl_lib,
# 
#   learners_outcome= sl_lib
# 
# )
# t2_charity_donate_z_test_gain_time_vary


t2_charity_donate_z_test_gain <- lmtp_tmle(
  outcome = "t2_charity_donate_z",
  baseline = names_base_final,
  shift = gain_A,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_sample_weights,
  learners_trt= sl_lib,
  learners_outcome= sl_lib
)
t2_charity_donate_z_test_gain


t2_charity_donate_z_test_zero <- lmtp_tmle(
  outcome = "t2_charity_donate_z",
  baseline = names_base_final,
  shift = zero_A,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_sample_weights,
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

lmtp_contrast(t2_charity_donate_z_test_gain, ref = t2_charity_donate_z_test_zero, type = "additive")



# models ------------------------------------------------------------------

library(SuperLearner)
library(xgboost)
library(ranger)
library(margot)

#
gain_A <- function(data, trt) {
  ifelse(data[[trt]] < 4, 4,  data[[trt]])
}
# 
zero_A <- function(data, trt){
  ifelse( data[[trt]] > 0, 0,  data[[trt]] )
}


# set seed for reproducing results
set.seed(0112358)
library(future)
plan(multisession)
plan(multisession)
n_cores <- parallel::detectCores()-1



# super learner libraries
sl_lib <- c("SL.glmnet",
            "SL.ranger",
            "SL.xgboost")



names_base_final<- here_read("names_base_final")
df_final<- here_read("df_final")

push_mods

t2_hours_charity_z_gain <- lmtp_tmle(
  outcome = "t2_hours_charity_z",
  baseline = names_base_final,
  shift = gain_A,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_hours_charity_z_gain, "t2_hours_charity_z_gain")

t2_hours_charity_z_zero <- lmtp_tmle(
  outcome = "t2_hours_charity_z",
  baseline = names_base_final,
  shift = zero_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_hours_charity_z_zero, "t2_hours_charity_z_zero")


t2_hours_charity_z_null <- lmtp_tmle(
  outcome = "t2_hours_charity_z",
  baseline = names_base_final,
  shift = NULL,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib
 
)
t2_hours_charity_z_null
here_save(t2_hours_charity_z_null, "t2_hours_charity_z_null")


lmtp_contrast(t2_hours_charity_z_gain, ref = t2_hours_charity_z_zero, type = "additive")

# 

# church donations --------------------------------------------------------
# names_base_t2_charity_donate_z<- select_and_rename_cols(names_base = names_base,  
#                                                          baseline_vars = base_var, 
#                                                          outcome =  "t2_charity_donate_z")
# names_base_t2_charity_donate_z<- setdiff(names_base_t2_charity_donate_z, "t0_volunteers_binary")


t2_charity_donate_z_gain <- lmtp_tmle(
  outcome = "t2_charity_donate_z",
  baseline = names_base_final,
  shift = gain_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)
here_save(t2_charity_donate_z_gain, "t2_charity_donate_z_gain")
t2_charity_donate_z_gain

t2_charity_donate_z_zero <- lmtp_tmle(
  outcome = "t2_charity_donate_z",
  baseline = names_base_final,
  shift = zero_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_charity_donate_z_zero, "t2_charity_donate_z_zero")


t2_charity_donate_z_null <- lmtp_tmle(
  outcome = "t2_charity_donate_z",
  baseline = names_base_final,
  shift = NULL,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_charity_donate_z_null, "t2_charity_donate_z_null")


lmtp_contrast(t2_charity_donate_z_gain, ref = t2_charity_donate_z_zero, type = "additive")
lmtp_contrast(t2_hours_charity_z_gain, ref = t2_hours_charity_z_null, type = "additive")




#############################
# church subjective support -----------------------------------------------
#############################

# 
# 
# # church soc support ------------------------------------------------------
# names_base_t2_support_z<- select_and_rename_cols(names_base = names_base,  
#                                                         baseline_vars = base_var, 
#                                                         outcome =  "t2_support_z")
# names_base_t2_support_z<- setdiff(names_base_t2_support_z, "t0_volunteers_binary")
# 
# 
# t2_support_z_gain <- lmtp_tmle(
#   outcome = "t2_support_z",
#   baseline = names_base_final,
#   shift = gain_A,
#   data =  df_final,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights =  df_final$t0_combo_weights, 
#   learners_trt= sl_lib,
#   learners_outcome= sl_lib
# 
# )
# here_save(t2_support_z_gain, "t2_support_z_gain")
# 
# t2_support_z_zero <- lmtp_tmle(
#   outcome = "t2_support_z",
#   baseline = names_base_final,
#   shift = zero_A,
#   data =  df_final,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights =  df_final$t0_combo_weights, 
#   learners_trt= sl_lib,
#   learners_outcome= sl_lib
# 
# )
# 
# here_save(t2_support_z_zero, "t2_support_z_zero")
# 
# #
# #
# t2_support_z_null <- lmtp_tmle(
#   outcome = "t2_support_z",
#   baseline = names_base_final,
#   shift = NULL,
#   data =  df_final,
#   trt = A,
#   cens = C,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights =  df_final$t0_combo_weights, 
#   learners_trt= sl_lib,
#   learners_outcome= sl_lib
# 
# )
# 
# here_save(t2_support_z_null, "t2_support_z_null")
# 
# # church soc belong -------------------------------------------------------
# names_base_t2_belong_z<- select_and_rename_cols(names_base = names_base,  
#                                                  baseline_vars = base_var, 
#                                                  outcome =  "t2_belong_z")
# names_base_t2_belong_z<- setdiff(names_base_t2_belong_z, "t0_volunteers_binary")


t2_family_time_binary_gain <- lmtp_tmle(
  outcome = "t2_family_time_binary",
  baseline = names_base_final,
  shift = gain_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_family_time_binary_gain, "t2_family_time_binary_gain")

t2_family_time_binary_zero <- lmtp_tmle(
  outcome = "t2_family_time_binary",
  baseline = names_base_final,
  shift = zero_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_family_time_binary_zero, "t2_family_time_binary_zero")

# 
t2_family_time_binary_null <- lmtp_tmle(
  outcome = "t2_family_time_binary",
  baseline = names_base_final,
  shift = NULL,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_family_time_binary_null, "t2_family_time_binary_null")
# # 




# church: friends help time received ----------------------------------------------
# names_base_t2_friends_time_binary<- select_and_rename_cols(names_base = names_base,  
#                                                                  baseline_vars = base_var, 
#                                                                  outcome =  "t2_friends_time_binary")
# names_base_t2_friends_time_binary<- setdiff(names_base_t2_friends_time_binary, "t0_volunteers_binary")
# 


t2_friends_time_binary_gain <- lmtp_tmle(
  outcome = "t2_friends_time_binary",
  baseline = names_base_final,
  shift = gain_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_friends_time_binary_gain, "t2_friends_time_binary_gain")


t2_friends_time_binary_zero <- lmtp_tmle(
  outcome = "t2_friends_time_binary",
  baseline = names_base_final,
  shift = zero_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_friends_time_binary_zero, "t2_friends_time_binary_zero")


# 
t2_friends_time_binary_null <- lmtp_tmle(
  outcome = "t2_friends_time_binary",
  baseline = names_base_final,
  shift = NULL,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_friends_time_binary_null, "t2_friends_time_binary_null")
# 

# church: community time help received --------------------------------------------
# names_base_t2_community_time_binary<- select_and_rename_cols(names_base = names_base,  
#                                                            baseline_vars = base_var, 
#                                                            outcome =  "t2_community_time_binary")
# names_base_t2_community_time_binary<- setdiff(names_base_t2_community_time_binary, "t0_volunteers_binary")



t2_community_time_binary_gain <- lmtp_tmle(
  outcome = "t2_community_time_binary",
  baseline = names_base_final,
  shift = gain_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_community_time_binary_gain,
          "t2_community_time_binary_gain")


# 
t2_community_time_binary_zero <- lmtp_tmle(
  outcome = "t2_community_time_binary",
  baseline = names_base_final,
  shift = zero_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_community_time_binary_zero,
          "t2_community_time_binary_zero")

# 
t2_community_time_binary_null<- lmtp_tmle(
  outcome = "t2_community_time_binary",
  baseline = names_base_final,
  shift = NULL,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_community_time_binary_null,
          "t2_community_time_binary_null")


# money -------------------------------------------------------------------


# church_friends money received --------------------------------------------------
# names_base_t2_family_money_binary<- select_and_rename_cols(names_base = names_base,  
#                                                              baseline_vars = base_var, 
#                                                              outcome =  "t2_family_money_binary")
# names_base_t2_family_money_binary<- setdiff(names_base_t2_family_money_binary, "t0_volunteers_binary")
# 


t2_family_money_binary_gain <- lmtp_tmle(
  outcome = "t2_family_money_binary",
  baseline = names_base_final,
  shift = gain_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_family_money_binary_gain, "t2_family_money_binary_gain")

# 

# 
t2_family_money_binary_zero <- lmtp_tmle(
  outcome = "t2_family_money_binary",
  baseline = names_base_final,
  shift = zero_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_family_money_binary_zero, "t2_family_money_binary_zero")


# 
t2_family_money_binary_null <- lmtp_tmle(
  outcome = "t2_family_money_binary",
  baseline = names_base_final,
  shift = NULL,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_family_money_binary_null, "t2_family_money_binary_null")
t2_family_money_binary_null



# church: friends help money received ----------------------------------------------
# names_base_t2_friends_money_binary<- select_and_rename_cols(names_base = names_base,  
#                                                            baseline_vars = base_var, 
#                                                            outcome =  "t2_friends_money_binary")
# names_base_t2_friends_money_binary<- setdiff(names_base_t2_friends_money_binary, "t0_volunteers_binary")
# 

t2_friends_money_binary_gain <- lmtp_tmle(
  outcome = "t2_friends_money_binary",
  baseline = names_base_final,
  shift = gain_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_friends_money_binary_gain,
          "t2_friends_money_binary_gain")


t2_friends_money_binary_zero <- lmtp_tmle(
  outcome = "t2_friends_money_binary",
  baseline = names_base_final,
  shift = zero_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_friends_money_binary_zero,
          "t2_friends_money_binary_zero")


# 
t2_friends_money_binary_null<- lmtp_tmle(
  outcome = "t2_friends_money_binary",
  baseline = names_base_final,
  shift = NULL,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_friends_money_binary_null,
          "t2_friends_money_binary_null")

# church: community money help received --------------------------------------------

# names_base_t2_community_money_binary<- select_and_rename_cols(names_base = names_base,  
#                                                             baseline_vars = base_var, 
#                                                             outcome =  "t2_community_money_binary")
# names_base_t2_community_money_binary<- setdiff(names_base_t2_community_money_binary, "t0_volunteers_binary")


t2_community_money_binary_gain <- lmtp_tmle(
  outcome = "t2_community_money_binary",
  baseline = names_base_final,
  shift = gain_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_community_money_binary_gain,
          "t2_community_money_binary_gain")

t2_community_money_binary_zero <- lmtp_tmle(
  outcome = "t2_community_money_binary",
  baseline = names_base_final,
  shift = zero_A,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_community_money_binary_zero,
          "t2_community_money_binary_zero")

# 
t2_community_money_binary_null <- lmtp_tmle(
  outcome = "t2_community_money_binary",
  baseline = names_base_final,
  shift = NULL,
  data =  df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights =  df_final$t0_combo_weights, 
  learners_trt= sl_lib,
  learners_outcome= sl_lib

)

here_save(t2_community_money_binary_null,
          "t2_community_money_binary_null")




# results -----------------------------------------------------------------


# # results -----------------------------------------------------------------
# push_mods
# t2_volunteers_binary_gain <- here_read("t2_volunteers_binary_gain")
# t2_volunteers_binary_zero <- here_read("t2_volunteers_binary_zero")
# t2_volunteers_binary_null <- here_read("t2_volunteers_binary_null")
# t2_volunteers_binary_gain
# # t2_volunteers_binary_null
# t2_hours_charity_z_gain$theta * sd_volunteer
# t2_hours_charity_z_zero$theta * sd_volunteer
# t2_hours_charity_z_null$theta * sd_volunteer


t2_hours_charity_z_gain <-
  here_read("t2_hours_charity_z_gain") # note spelling
t2_hours_charity_z_zero <- here_read("t2_hours_charity_z_zero")
t2_hours_charity_z_null<- here_read("t2_hours_charity_z_null")



t2_charity_donate_z_gain <- here_read("t2_charity_donate_z_gain")
t2_charity_donate_z_zero <- here_read("t2_charity_donate_z_zero")


t2_support_z_gain <- here_read("t2_support_z_gain")
t2_support_z_zero <- here_read("t2_support_z_zero")
t2_support_z_null<- here_read("t2_support_z_null")



# t2_belong_z_gain <- here_read("t2_belong_z_gain")
# t2_belong_z_zero <- here_read("t2_belong_z_zero")
# t2_belong_z_null <- here_read("t2_belong_z_null")



# t2_neighbourhood_community_z_gain <-
#   here_read("t2_neighbourhood_community_z_gain")
# t2_neighbourhood_community_z_zero <-
#   here_read("t2_neighbourhood_community_z_zero")
# t2_neighbourhood_community_z_null <-
#   here_read("t2_neighbourhood_community_z_null")


t2_family_time_binary_gain <-
  here_read("t2_family_time_binary_gain")
t2_family_time_binary_zero <-
  here_read("t2_family_time_binary_zero")
t2_family_time_binary_null <-
  here_read("t2_family_time_binary_null")


t2_friends_time_binary_gain <-
  here_read("t2_friends_time_binary_gain")
t2_friends_time_binary_zero <-
  here_read("t2_friends_time_binary_zero")
t2_friends_time_binary_null <-
  here_read("t2_friends_time_binary_null")


t2_community_time_binary_gain <-
  here_read("t2_community_time_binary_gain")
t2_community_time_binary_zero <-
  here_read("t2_community_time_binary_zero")
t2_community_time_binary_null <-
  here_read("t2_community_time_binary_null")


t2_family_money_binary_gain <-
  here_read("t2_family_money_binary_gain")
t2_family_money_binary_zero <-
  here_read("t2_family_money_binary_zero")
t2_family_money_binary_null <-
  here_read("t2_family_money_binary_null")

push_mods

t2_friends_money_binary_gain <-
  here_read("t2_friends_money_binary_gain")
t2_friends_money_binary_zero <-
  here_read("t2_friends_money_binary_zero")
t2_friends_money_binary_null <-
  here_read("t2_friends_money_binary_null")


t2_community_money_binary_gain <-
  here_read("t2_community_money_binary_gain")
t2_community_money_binary_zero <-
  here_read("t2_community_money_binary_zero")
t2_community_money_binary_null <-
  here_read("t2_community_money_binary_null")





# results hours volunteer ---------------------------------------------------
library(lmtp)
contrast_hours_charity_z <-
  lmtp::lmtp_contrast(t2_hours_charity_z_gain, ref = t2_hours_charity_z_zero, type = "additive")

contrast_hours_charity_z

output_tab_contrast_hours_charity_z <- margot_lmtp_evalue(
  contrast_hours_charity_z,
  scale = "RD",
  new_name = "hours volunteer"
)
# tab_contrast_hours_charity_z
# 
# 
# output_tab_contrast_hours_charity_z <-
#   lmtp_evalue_tab(
#     tab_contrast_hours_charity_z,
#     delta = 1,
#     sd = 1,
#     scale = c("RD")
#   )
output_tab_contrast_hours_charity_z


# test --------------------------------------------------------------------


## test -- this works! :) 
library(margot)
test <- margot::margot_lmtp_evalue(contrast_hours_charity_z,  scale = "RD",
                                   new_name = "hours volunteer")


# end ---------------------------------------------------------------------


contrast_hours_charity_z_null <-
  lmtp::lmtp_contrast(t2_hours_charity_z_gain, ref = t2_hours_charity_z_null, type = "additive")


output_tab_contrast_hours_charity_z_null <- margot_lmtp_evalue(
  contrast_hours_charity_z_null,
  scale = "RD",
  new_name = "hours volunteer"
)


output_tab_contrast_hours_charity_z_null


contrast_hours_charity_z_null_zero <-
  lmtp::lmtp_contrast(t2_hours_charity_z_zero, ref = t2_hours_charity_z_null, type = "additive")


output_tab_contrast_hours_charity_z_null_zero <- margot_lmtp_evalue(
  contrast_hours_charity_z_null_zero,
  scale = "RD",
  new_name = "hours volunteer"
)


output_tab_contrast_hours_charity_z_null_zero





# results charity donate --------------------------------------------------


contrast_charity_donate_z <-
  lmtp_contrast(t2_charity_donate_z_gain , ref =  t2_charity_donate_z_zero, type = "additive")


output_tab_contrast_charity_donate_z <- margot_lmtp_evalue(
  contrast_charity_donate_z,
  scale = "RD",
  new_name = "donations"
)

output_tab_contrast_charity_donate_z


contrast_charity_donate_z_null <-
  lmtp_contrast(t2_charity_donate_z_gain , ref =  t2_charity_donate_z_null, type = "additive")


output_tab_contrast_charity_donate_z_null <- margot_lmtp_evalue(
  contrast_charity_donate_z_null,
  scale = "RD",
  new_name = "donations"
)



contrast_charity_donate_z_null_zero <-
  lmtp_contrast(t2_charity_donate_z_zero , ref =  t2_charity_donate_z_null, type = "additive")


## null zero
output_tab_contrast_charity_donate_z_null_zero <- margot_lmtp_evalue(
  contrast_charity_donate_z_null_zero,
  scale = "RD",
  new_name = "donations"
)


output_tab_contrast_charity_donate_z_null_zero


# results volunteers binary -----------------------------------------------

# contrast_volunteers_binary   <-
#   lmtp_contrast(t2_volunteers_binary_gain , ref =  t2_volunteers_binary_zero, type = "rr")
# 
# tab_contrast_volunteers_binary <- margot_lmtp_tab(
#   contrast_volunteers_binary,
#   scale = "RR",
#   new_name = "volunteers (binary)"
# )
# 


# volunteerbinary ---------------------------------------------------------
# t2_volunteers_binary_gain
# t2_volunteers_binary_zero
# 
# contrast_volunteers_binary   <-
#   lmtp_contrast(t2_volunteers_binary_gain , ref =  t2_volunteers_binary_zero, type = "rr")
# 
# tab_contrast_volunteers_binary <- margot_lmtp_tab(
#   contrast_volunteers_binary,
#   scale = "RR",
#   new_name = "volunteers (binary)"
# )
# 
# output_tab_contrast_volunteers_binary <- lmtp_evalue_tab(tab_contrast_volunteers_binary,  delta = 1, sd = 1, scale = c("RR"))
# 


# 
# 
# contrast_volunteers_binary_null   <-
#   lmtp_contrast(t2_volunteers_binary_gain , ref =  t2_volunteers_binary_null, type = "rr")
# 
# tab_contrast_volunteers_binary_null <- margot_lmtp_tab(
#   contrast_volunteers_binary_null,
#   scale = "RR",
#   new_name = "volunteers (binary)"
# )
# 
# output_tab_contrast_volunteers_binary_null <- lmtp_evalue_tab(tab_contrast_volunteers_binary_null,  delta = 1, sd = 1, scale = c("RR"))
# 
# output_tab_contrast_volunteers_binary
# output_tab_contrast_volunteers_binary_null
# support -----------------------------------------------------------------

# contrast_support_z <- 
#   lmtp_contrast(t2_support_z_gain , ref =  t2_support_z_zero, type = "additive")

# output_tab_contrast_support_z <- margot_lmtp_evalue(
#   contrast_support_z,
#   scale = "RD",
#   new_name = "social suport"
# )

# contrast_support_z_null <-
#   lmtp_contrast(t2_support_z_gain , ref =  t2_support_z_null, type = "additive")

# output_tab_contrast_support_z_null <- margot_lmtp_evalue(
#   contrast_support_z_null,
#   scale = "RD",
#   new_name = "social support"
# )


# # null zero
# contrast_support_z_null_zero <-
#   lmtp_contrast(t2_support_z_zero , ref =  t2_support_z_null, type = "additive")

# output_tab_contrast_support_z_null_zero <- margot_lmtp_evalue(
#   contrast_support_z_null_zero,
#   scale = "RD",
#   new_name = "social support"
# )
# results belong ----------------------------------------------------------

# # 
# contrast_belong_z <-
#   lmtp_contrast(t2_belong_z_gain, ref =  t2_belong_z_zero, type = "additive")
# 
# 
# tab_contrast_belong_z <- margot_lmtp_tab(
#   contrast_belong_z,
#   scale = "RD",
#   new_name = "social belonging"
# )
# 
# output_tab_contrast_belong_z<- lmtp_evalue_tab(tab_contrast_belong_z,  delta = 1, sd = 1, scale = c("RD"))
# 
# 
# 
# contrast_belong_z_null <-
#   lmtp_contrast(t2_belong_z_gain, ref =  t2_belong_z_null, type = "additive")
# 
# 
# tab_contrast_belong_z_null <- margot_lmtp_tab(
#   contrast_belong_z_null,
#   scale = "RD",
#   new_name = "social belonging"
# )
# 
# output_tab_contrast_belong_z_null<- lmtp_evalue_tab(tab_contrast_belong_z_null,  delta = 1, sd = 1, scale = c("RD"))
# 
# output_tab_contrast_belong_z
# output_tab_contrast_belong_z_null
# 
# 
# tab_contrast_belong_z_null <- margot_lmtp_tab(
#   contrast_belong_z_null,
#   scale = "RD",
#   new_name = "social belonging"
# )
# 
# output_tab_contrast_belong_z_null<- lmtp_evalue_tab(tab_contrast_belong_z_null,  delta = 1, sd = 1, scale = c("RD"))
# 
# # null
# 
# contrast_belong_z_null_zero <-
#   lmtp_contrast(t2_belong_z_zero, ref =  t2_belong_z_null, type = "additive")
# 
# 
# tab_contrast_belong_z_null_zero <- margot_lmtp_tab(
#   contrast_belong_z_null_zero,
#   scale = "RD",
#   new_name = "social belonging"
# )
# 
# output_tab_contrast_belong_z_null_zero<- lmtp_evalue_tab(tab_contrast_belong_z_null_zero,  delta = 1, sd = 1, scale = c("RD"))
# 
# output_tab_contrast_belong_z
# output_tab_contrast_belong_z_null
# output_tab_contrast_belong_z_null_zero
# 
# 
# 
# output_tab_contrast_belong_z
# output_tab_contrast_belong_z_null
# # results neighbourcommunity ----------------------------------------------
# 
# 
# contrast_neighbourhood_community_z <-
#   lmtp_contrast(t2_neighbourhood_community_z_gain ,
#                 ref = t2_neighbourhood_community_z_zero ,
#                 type = "additive")
# 
# tab_contrast_neighbourhood_community_z <- margot_lmtp_tab(
#   contrast_neighbourhood_community_z,
#   scale = "RD",
#   new_name = "neighbourhood community"
# )
# 
# output_tab_contrast_neighbourhood_community_z<- lmtp_evalue_tab(tab_contrast_neighbourhood_community_z,  delta = 1, sd = 1, scale = c("RD"))
# 
# 
# contrast_neighbourhood_community_z_null <-
#   lmtp_contrast(t2_neighbourhood_community_z_gain ,
#                 ref = t2_neighbourhood_community_z_null,
#                 type = "additive")
# 
# tab_contrast_neighbourhood_community_z_null <- margot_lmtp_tab(
#   contrast_neighbourhood_community_z_null,
#   scale = "RD",
#   new_name = "neighbourhood community"
# )
# 
# output_tab_contrast_neighbourhood_community_z_null<- lmtp_evalue_tab(tab_contrast_neighbourhood_community_z_null,  delta = 1, sd = 1, scale = c("RD"))
# 
# # null
# contrast_neighbourhood_community_z_null_zero <-
#   lmtp_contrast(t2_neighbourhood_community_z_zero ,
#                 ref = t2_neighbourhood_community_z_null,
#                 type = "additive")
# 
# tab_contrast_neighbourhood_community_z_null_zero <- margot_lmtp_tab(
#   contrast_neighbourhood_community_z_null_zero,
#   scale = "RD",
#   new_name = "neighbourhood community"
# )
# 
# output_tab_contrast_neighbourhood_community_z_null_zero<- lmtp_evalue_tab(tab_contrast_neighbourhood_community_z_null_zero,  delta = 1, sd = 1, scale = c("RD"))
# 
# 
# 
# output_tab_contrast_neighbourhood_community_z
# output_tab_contrast_neighbourhood_community_z_null
# output_tab_contrast_neighbourhood_community_z_null_zero

# results family time -----------------------------------------------------
# 
contrast_family_time_binary <-
  lmtp_contrast(t2_family_time_binary_gain, ref =  t2_family_time_binary_zero, type = "rr")
t2_family_time_binary_gain
t2_family_time_binary_zero

output_tab_contrast_family_time <- margot_lmtp_evalue(
  contrast_family_time_binary,
  scale = "RR",
  new_name = "family gives time"
)



contrast_family_time_binary_null <-
  lmtp_contrast(t2_family_time_binary_gain, ref =  t2_family_time_binary_null, type = "rr")


output_tab_contrast_family_time_null <- margot_lmtp_evalue(
  contrast_family_time_binary_null,
  scale = "RR",
  new_name = "family gives time"
)

t2_family_time_binary_gain
t2_family_time_binary_zero
t2_family_time_binary_null


contrast_family_time_binary_null_zero <-
  lmtp_contrast(t2_family_time_binary_zero, ref =  t2_family_time_binary_null, type = "rr")


output_tab_contrast_family_time_null_zero <- margot_lmtp_evalue(
  contrast_family_time_binary_null_zero,
  scale = "RR",
  new_name = "family gives time"
)



output_tab_contrast_family_time
output_tab_contrast_family_time_null
output_tab_contrast_family_time_null_zero

# results friends time ----------------------------------------------------
library(lmtp)
library(margot)
contrast_friends_time <-
  lmtp_contrast(t2_friends_time_binary_gain , ref =  t2_friends_time_binary_zero, type = "rr")

output_tab_contrast_friends_time <- margot_lmtp_evalue(
  contrast_friends_time,
  scale = "RR",
  new_name = "friends give time"
)


push_mods
contrast_friends_time_null <-
  lmtp_contrast(t2_friends_time_binary_gain , ref =  t2_friends_time_binary_null, type = "rr")

output_tab_contrast_friends_time_null <- margot_lmtp_evalue(
  contrast_friends_time_null,
  scale = "RR",
  new_name = "friends give time"
)


# null zero
contrast_friends_time_null_zero <-
  lmtp_contrast(t2_friends_time_binary_zero , ref =  t2_friends_time_binary_null, type = "rr")

output_tab_contrast_friends_time_null_zero <- margot_lmtp_evalue(
  contrast_friends_time_null_zero,
  scale = "RR",
  new_name = "friends give time"
)



output_tab_contrast_friends_time
output_tab_contrast_friends_time_null
output_tab_contrast_friends_time_null_zero



# results community time --------------------------------------------------

contrast_community_time <-
  lmtp_contrast(t2_community_time_binary_gain,
                ref =  t2_community_time_binary_zero,
                type = "rr")


output_tab_contrast_community_time <- margot_lmtp_evalue(
  contrast_community_time,
  scale = "RR",
  new_name = "community gives time"
)




contrast_community_time_null <-
  lmtp_contrast(t2_community_time_binary_gain,
                ref =  t2_community_time_binary_null,
                type = "rr")


output_tab_contrast_community_time_null <- margot_lmtp_evalue(
  contrast_community_time_null,
  scale = "RR",
  new_name = "community gives time"
)


contrast_community_time_null_zero <-
  lmtp_contrast(t2_community_time_binary_zero,
                ref =  t2_community_time_binary_null,
                type = "rr")


output_tab_contrast_community_time_null_zero <- margot_lmtp_evalue(
  contrast_community_time_null_zero,
  scale = "RR",
  new_name = "community gives time"
)



output_tab_contrast_community_time_null_zero

# results family money  ---------------------------------------------------

contrast_family_money <-
  lmtp_contrast(t2_family_money_binary_gain , ref =  t2_family_money_binary_zero, type = "rr")

contrast_family_money

output_tab_contrast_family_money <- margot_lmtp_evalue(
  contrast_family_money,
  scale = "RR",
  new_name = "family gives money"
)

output_tab_contrast_family_money

contrast_family_money_null <-
  lmtp_contrast(t2_family_money_binary_gain , ref =  t2_family_money_binary_null, type = "rr")

output_tab_contrast_family_money_null <- margot_lmtp_evalue(
  contrast_family_money_null,
  scale = "RR",
  new_name = "family gives money"
)

output_tab_contrast_family_money_null


# output_tab_contrast_family_money
output_tab_contrast_family_money_null

# null zero
contrast_family_money_null_zero <-
  lmtp_contrast(t2_family_money_binary_zero , ref =  t2_family_money_binary_null, type = "rr")

output_tab_contrast_family_money_null_zero <- margot_lmtp_evalue(
  contrast_family_money_null_zero,
  scale = "RR",
  new_name = "family gives money"
)

output_tab_contrast_family_money_null_zero




output_tab_contrast_family_money
output_tab_contrast_family_money_null
output_tab_contrast_family_money_null_zero


# results friends money ---------------------------------------------------


contrast_friends_money <-
  lmtp::lmtp_contrast(t2_friends_money_binary_gain,
                      ref =  t2_friends_money_binary_zero,
                      type = "rr")

output_tab_contrast_friends_money <- margot_lmtp_evalue(
  contrast_friends_money,
  scale = "RR",
  new_name = "friends give money"
)




# 
contrast_friends_money_null <-
  lmtp::lmtp_contrast(t2_friends_money_binary_gain,
                      ref =  t2_friends_money_binary_null,
                      type = "rr")

output_tab_contrast_friends_money_null <- margot_lmtp_evalue(
  contrast_friends_money_null,
  scale = "RR",
  new_name = "friends give money"
)


#output_tab_contrast_friends_money
output_tab_contrast_friends_money_null

# null zero
# 
contrast_friends_money_null_zero <-
  lmtp::lmtp_contrast(t2_friends_money_binary_zero,
                      ref =  t2_friends_money_binary_null,
                      type = "rr")

output_tab_contrast_friends_money_null_zero <- margot_lmtp_evalue(
  contrast_friends_money_null_zero,
  scale = "RR",
  new_name = "friends gives money"
)

output_tab_contrast_friends_money
output_tab_contrast_friends_money_null
output_tab_contrast_friends_money_null_zero


# results community money -------------------------------------------------
contrast_community_money <-
  lmtp_contrast(t2_community_money_binary_gain,
                ref = t2_community_money_binary_zero ,
                type = "rr")

output_tab_contrast_community_money <- margot_lmtp_evalue(
  contrast_community_money,
  scale = "RR",
  new_name = "community gives money"
)




contrast_community_money_null <-
  lmtp_contrast(t2_community_money_binary_gain,
                ref = t2_community_money_binary_null ,
                type = "rr")

output_tab_contrast_community_money_null <- margot_lmtp_evalue(
  contrast_community_money_null,
  scale = "RR",
  new_name = "community gives money"
)
output_tab_contrast_community_money_null


# output_tab_contrast_community_money
output_tab_contrast_community_money_null



contrast_community_money_null_zero <-
  lmtp_contrast(t2_community_money_binary_zero,
                ref = t2_community_money_binary_null ,
                type = "rr")

output_tab_contrast_community_money_null_zero <- margot_lmtp_evalue(
  contrast_community_money_null_zero,
  scale = "RR",
  new_name = "community gives money"
)


# output_tab_contrast_community_money
output_tab_contrast_community_money_null_zero


#######################
# tables and graphs -------------------------------------------------------

#######################
library(tidyverse)

tab_all_prosocial <- rbind( output_tab_contrast_charity_donate_z,
                            output_tab_contrast_hours_charity_z)
tab_all_prosocial
group_tab_all_prosocial <- group_tab(tab_all_prosocial, type = "RD")
group_tab_all_prosocial



tab_all_prosocial_null <- rbind( output_tab_contrast_charity_donate_z_null,
                                 output_tab_contrast_hours_charity_z_null)
group_tab_all_prosocial_null <- group_tab(tab_all_prosocial_null, type = "RD")
tab_all_prosocial_null
group_tab_all_prosocial_null

tab_all_prosocial_null_zero <- rbind( output_tab_contrast_charity_donate_z_null_zero,
                                      output_tab_contrast_hours_charity_z_null_zero)
group_tab_all_prosocial_null_zero <- group_tab(tab_all_prosocial_null_zero, type = "RD")
group_tab_all_prosocial_null_zero

here_save(tab_all_prosocial, "tab_all_prosocial")
here_save(group_tab_all_prosocial, "group_tab_all_prosocial")
here_save(tab_all_prosocial_null, "tab_all_prosocial_null")
here_save(group_tab_all_prosocial_null, "group_tab_all_prosocial_null")


# import this
here_save(tab_all_prosocial_null_zero, "tab_all_prosocial_null_zero")
here_save(group_tab_all_prosocial_null_zero, "group_tab_all_prosocial_null_zero")







tab_received_time <- rbind(
  output_tab_contrast_family_time,
  output_tab_contrast_friends_time,
  output_tab_contrast_community_time)


tab_received_time_null <- rbind(
  output_tab_contrast_family_time_null,
  output_tab_contrast_friends_time_null, 
  output_tab_contrast_community_time_null)


tab_received_time_null_zero <- rbind(
  output_tab_contrast_family_time_null_zero,
  output_tab_contrast_friends_time_null_zero, 
  output_tab_contrast_community_time_null_zero)

group_tab_received_time<- group_tab(tab_received_time, type = "RR")
group_tab_received_time_null<- group_tab(tab_received_time_null, type = "RR")
group_tab_received_time_null_zero<- group_tab(tab_received_time_null_zero, type = "RR")

tab_received_time
group_tab_received_time
group_tab_received_time
tab_received_time_null
group_tab_received_time_null

tab_received_time_null_zero
group_tab_received_time_null_zero



here_save(tab_received_time, "tab_received_time")
here_save(tab_received_time_null, "tab_received_time_null")
here_save(tab_received_time_null_zero, "tab_received_time_null_zero")
here_save(group_tab_received_time, "group_tab_received_time")
here_save(group_tab_received_time_null, "group_tab_received_time_null")
here_save(group_tab_received_time_null_zero, "group_tab_received_time_null_zero")




tab_all_received_money <- rbind(
  output_tab_contrast_family_money,
  output_tab_contrast_friends_money,
  output_tab_contrast_community_money
)

tab_all_received_money_null <- rbind(
  output_tab_contrast_family_money_null,
  output_tab_contrast_friends_money_null, 
  output_tab_contrast_community_money_null
)

tab_all_received_money_null_zero <- rbind(
  output_tab_contrast_family_money_null_zero ,
  output_tab_contrast_friends_money_null_zero , 
  output_tab_contrast_community_money_null_zero 
)


group_tab_all_received_money <- group_tab(tab_all_received_money, type = "RR")
group_tab_all_received_money_null <- group_tab(tab_all_received_money_null, type = "RR")
group_tab_all_received_money_null_zero <- group_tab(tab_all_received_money_null_zero, type = "RR")


here_save(tab_all_received_money, "tab_all_received_money")
here_save(group_tab_all_received_money, "group_tab_all_received_money")
here_save(tab_all_received_money_null, "tab_all_received_money_null")
here_save(group_tab_all_received_money_null, "group_tab_all_received_money_null")
here_save(tab_all_received_money_null_zero, "tab_all_received_money_null_zero")
here_save(group_tab_all_received_money_null_zero, "group_tab_all_received_money_null_zero")



# import to manuscript -------------------------------------------------------


tab_all_prosocial <- here_read("tab_all_prosocial")
tab_all_prosocial_null <- here_read("tab_all_prosocial_null")
tab_all_prosocial_null_zero <- here_read("tab_all_prosocial_null_zero")
tab_all_prosocial
tab_all_prosocial_null
tab_all_prosocial_null_zero


group_tab_all_prosocial<- here_read("group_tab_all_prosocial")
group_tab_all_prosocial_null<- here_read("group_tab_all_prosocial_null")
group_tab_all_prosocial_null_zero<- here_read("group_tab_all_prosocial_null_zero")

group_tab_all_prosocial
group_tab_all_prosocial_null
group_tab_all_prosocial_null_zero


tab_all_received_money <- here_read( "tab_all_received_money")
tab_all_received_money_null <- here_read( "tab_all_received_money_null")
tab_all_received_money_null_zero <- here_read( "tab_all_received_money_null_zero")
tab_all_received_money
tab_all_received_money_null
tab_all_received_money_null_zero

group_tab_all_received_money <- here_read( "group_tab_all_received_money")
group_tab_all_received_money_null <- here_read( "group_tab_all_received_money_null")
group_tab_all_received_money_null_zero <- here_read( "group_tab_all_received_money_null_zero")
group_tab_all_received_money
group_tab_all_received_money_null
group_tab_all_received_money_null_zero

tab_received_time <- here_read( "tab_received_time")
tab_received_time_null <- here_read( "tab_received_time_null")
tab_received_time_null_zero <- here_read( "tab_received_time_null_zero")

group_tab_received_time <- here_read( "group_tab_received_time")
group_tab_received_time_null <- here_read( "group_tab_received_time_null")
group_tab_received_time_null_zero <- here_read( "group_tab_received_time_null_zero")



# graphs ------------------------------------------------------------------
title = "Religious Service At Least Once Per Week vs None"
title_null = "Religious Service At Least Once Per Week vs No Intervention"
title_null_zero = "Loss of Any Religious Service vs No Intervention"

# 
# 
plot_group_tab_all_prosocial <- margot_plot(
  group_tab_all_prosocial,
  type = "RD",
  title = title,
  subtitle = "Self-Reported Prosociality",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = 3.5,
  title_size = 20,
  subtitle_size = 16,
  legend_text_size = 10,
  legend_title_size = 10,
  x_offset = -.5,
  x_lim_lo = -.5,
  x_lim_hi =  .5
)

plot_group_tab_all_prosocial

ggsave(
  plot_group_tab_all_prosocial,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_all_prosocial.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)

# null 

plot_group_tab_all_prosocial_null <- margot_plot(
  group_tab_all_prosocial_null,
  type = "RD",
  title = title_null,
  subtitle = "Self-Reported Prosociality",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = 3.5,
  title_size = 20,
  subtitle_size = 16,
  legend_text_size = 10,
  legend_title_size = 10,
  x_offset = -.5,
  x_lim_lo = -.5,
  x_lim_hi =  .5
)

plot_group_tab_all_prosocial_null

ggsave(
  plot_group_tab_all_prosocial_null,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_all_prosocial_null.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)

plot_group_tab_all_prosocial_null_zero <- margot_plot(
  group_tab_all_prosocial_null_zero,
  type = "RD",
  title = title_null_zero,
  subtitle = "Self-Reported Prosociality",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = 3.5,
  title_size = 20,
  subtitle_size = 16,
  legend_text_size = 10,
  legend_title_size = 10,
  x_offset = -.5,
  x_lim_lo = -.5,
  x_lim_hi =  .5
)

plot_group_tab_all_prosocial_null_zero

ggsave(
  plot_group_tab_all_prosocial_null_zero,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_all_prosocial_null_zero.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)



plot_group_tab_all_received_money <- margot_plot(
  group_tab_all_received_money,
  type = "RR",
  title = title,  
  subtitle = "Support Received From Others: Money",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = 3.5,
  title_size = 20,
  subtitle_size = 16,
  legend_text_size = 10,
  legend_title_size = 10,
  x_offset = 0,
  x_lim_lo = 0,
  x_lim_hi =  4
)

plot_group_tab_all_received_money
ggsave(
  plot_group_tab_all_received_money,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_all_received_money.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)



plot_group_tab_all_received_money_null <- margot_plot(
  group_tab_all_received_money_null,
  type = "RR",
  title = title_null,  
  subtitle = "Support Received From Others: Money",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = 3.5,
  title_size = 20,
  subtitle_size = 16,
  legend_text_size = 10,
  legend_title_size = 10,
  x_offset = 0,
  x_lim_lo = 0,
  x_lim_hi =  4
)
plot_group_tab_all_received_money_null

ggsave(
  plot_group_tab_all_received_money_null,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_all_received_money_null.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)

plot_group_tab_all_received_money_null_zero<- margot_plot(
  group_tab_all_received_money_null_zero,
  type = "RR",
  title = title_null_zero,  
  subtitle = "Support Received From Others: Money",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = 3.5,
  title_size = 20,
  subtitle_size = 16,
  legend_text_size = 10,
  legend_title_size = 10,
  x_offset = 0,
  x_lim_lo = 0,
  x_lim_hi =  4
)
plot_group_tab_all_received_money_null_zero


ggsave(
  plot_group_tab_all_received_money_null_zero,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_all_received_money_null_zero.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)





### TO read
transition_table_binary <-
  here_read("transition_table_binary")




graph_density_of_exposure_up <- here_read("graph_density_of_exposure_up")
graph_density_of_exposure_down <- here_read("graph_density_of_exposure_down")
graph_density_of_exposure_up
graph_density_of_exposure_down

table_exposures <- here_read("table_exposures")
table_baseline <- here_read("table_baseline")
table_outcomes <- here_read("table_outcomes")


