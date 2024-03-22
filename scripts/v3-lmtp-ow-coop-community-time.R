# 8 Nov 2023
# original script is in the 00drafts folder.
# this script brings the analysis for this study to the 'models" workflow

### ALWAYS RESTART R IN A FRESH SESSION ####


# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/libs2.R")

# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/funs.R")

# ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source(
#   "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
# )

# experimental functions (more functions)
# source(
#   "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
# )

library(arrow)
# arrow::install_arrow()

## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE.
pull_path <-
  fs::path_expand(
    #"/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs_refactor/nzavs_data_23"
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data"
  )

# read data: note that you need use the arrow package in R

dat <- arrow::read_parquet("/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data")

### WARNING: THIS PATH WILL NOT WORK FOR YOU. PLEASE SET A PATH TO YOUR OWN COMPUTER!! ###
### WARNING: FOR EACH NEW STUDY SET UP A DIFFERENT PATH OTHERWISE YOU WILL WRITE OVER YOUR MODELS
push_mods <-  fs::path_expand(
  "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/nzvs_mods/24/ow-coop-community"
)

# check path:is this correct?  check so you know you are not overwriting other directors
push_mods


# set exposure here
nzavs_exposure <- "hours_community_round"

# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 10

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(0112358)

library(future)
library(ranger)
plan(multisession)
n_cores <- parallel::detectCores()-2
n_cores

# super learner libraries
sl_lib <- c("SL.glmnet",
            "SL.ranger", #
            "SL.xgboost") #


# check
push_mods

# check colnames
colnames(dat)

# get ids
ids_2018 <- dat |>
  filter(year_measured == 1,
         wave == 2018) |>
  pull(id)

# filter the original dataset for these IDs three waves
dat <- as.data.frame(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_widths(dat)


str(dat)

# Time 10 [2018/2019]	FamilyTime.T10	Please estimate how much help you have received from the following sources in the last week?
#   Time 10 [2018/2019]	FriendsTime.T10	Please estimate how much help you have received from the following sources in the last week?
#   Time 10 [2018/2019]	CommunityTime.T10	Please estimate how much help you have received from the following sources in the last week?
#   Time 10 [2018/2019]	FamilyMoney.T10	Please estimate how much help you have received from the following sources in the last week?
# #   Time 10 [2018/2019]	FriendsMoney.T10	Please estimate how much help you have received from the following sources in the last week?
#   Time 10 [2018/2019]	CommunityMoney.T10	Please estimate how much help you have received from the following sources in the last week?
dat_long_full <- dat |>
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
    "urban",
    # see NZAVS,
    "have_siblings",
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
    "w_gend_age_ethnic",
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
    "urban",
    # see NZAVS,
    "have_siblings",
    "children_num",
    # How many children have you given birth to, fathered, or adopted?
    "hours_children",
    "hours_community",
    "hours_friends",
    "hours_family",
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
  mutate(hours_community_round = round(ifelse(hours_community >= 10, 10, hours_community), 0)) |>
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
  mutate(# eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban)) |>
  ungroup() |>
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
    hours_friends_log  = log(hours_friends + 1),
    hours_family_log = log(hours_family + 1)#,
    #  children_num_log = log(children_num + 1)
  ) |>
  dplyr::select(
    -c(
      hours_work,
      hours_housework,
      household_inc,
      hours_exercise,
      hours_children,
      have_siblings,
      children_num,
      total_siblings
    )
  ) |>
  droplevels() |>
  dplyr::rename(sample_weights = w_gend_age_ethnic,
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
  dplyr::select(
    -c(
      religion_church,
      family_time,
      friends_time,
      community_time,
      hours_community,
      hours_family,
      hours_friends,
      community_money,
      friends_money,
      family_money
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
dplyr::mutate(# religion_church_binary = as.factor(religion_church_binary),
  # eth_cat = as.integer(eth_cat),
  urban = as.numeric(urban)) |>
  mutate(
    rural_gch_2018_l = as.numeric(as.character(rural_gch_2018_l)),
    #   have_siblings = as.numeric(as.character(have_siblings)),
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



# baseline vars -----------------------------------------------------------


dat_long <- dat_long_full |>
  select(-alert_level_combined)

str(dat_long)
# check
table(dat_long$censored)

# select vars for baseline
dat_long_colnames <- colnames(dat_long)

dat_long_colnames <- sort(dat_long_colnames)

dat_long_colnames

# set baseline exposure and outcomes --------------------------------------
nzavs_exposure
exposure_var = c("hours_community_round",
                 "censored"
) 


# set outcomes for prosocial domain
# save prejudice for separate paper
outcome_vars = c(
  "hours_charity",
  "charity_donate",
  # "warm_asians",
  # "warm_chinese",
  # "warm_immigrants",
  # "warm_indians",
  # "warm_elderly",
  # "warm_maori",
  # "warm_mental_illness",
  # "warm_muslims",
  # "warm_nz_euro",
  # "warm_overweight",
  # "warm_pacific",
  # "warm_refugees",
  "family_time_binary",
  "friends_time_binary",
  "community_time_binary",
  "neighbourhood_community",
  "family_money_binary",
  "community_money_binary",
  "friends_money_binary",
  "support",
  "belong"
  # "perc_gend_discrim",
  # "perc_religious_discrim",
  # "perc_discrim"
)

dat_long_colnames <- colnames(dat_long)
#
baseline_vars <-
  setdiff(dat_long_colnames,
          c("id","wave"))

# c(outcome_vars, 'id', 'wave'))
baseline_vars
baseline_vars <- sort(baseline_vars)

baseline_vars

# just core baseline variables
base_var <-
  setdiff(baseline_vars, c("censored", "sample_weights"))
base_var

here_save(base_var, "base_var")
#community at baseline
n_participants <-
  n_unique(dat_long$id) #47202 # reports hours with

# check
n_participants

here_save(n_participants, "n_participants")

# double check path
push_mods

# check col names
colnames(dat)

# assess positivity
dt_positivity_full_socialising <-
  dat_long |>
  filter(wave == 2018 |
           wave == 2019) |>
  select(wave, id, hours_community_round, sample_weights) |>
  mutate(hours_community_round_shift = ifelse(hours_community_round >=
                                                1, 1, 0))


# create transition matrix
out_social <-
  msm::statetable.msm(hours_community_round, id, data = dt_positivity_full_socialising)

out_social

#t_tab_cats_labels <- c("No Cats", "Cats")
# transition table
transition_table <-
  margot::transition_table(out_social)



transition_table
here_save(transition_table,
          "transition_table")

out_shift_social <-
  msm::statetable.msm(hours_community_round_shift, id, data = dt_positivity_full_socialising)

out_shift_social

t_tab_2_social_labels <-
  c("< 1 weekly hours", ">= 1 weekly hours")
# transition table

transition_table_binary  <-
  margot::transition_table(out_shift_social,
                           state_names = t_tab_2_social_labels)

transition_table_binary
here_save(transition_table_binary,
          "transition_table_binary")
# double check path
push_mods

# check col names
colnames(dat)


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
mean_volunteer


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



# check associations only -------------------------------------------------
dt_18 <- dat_long|>
  filter(wave == 2018) 


table(dt_18$censored)

dat_long$charity_donate_log

# check association only
summary(lm(charity_donate ~ religion_church_round, data = dt_18))
summary(lm(hours_charity ~ religion_church_round, data = dt_18))


summary(lm(charity_donate ~ hours_community_round, data = dt_18))
summary(lm(hours_charity ~ hours_community_round, data = dt_18))


# 
dt_18_miss <- dt_18 |> select(-alert_level_combined_lead)

naniar::vis_miss(dt_18_miss, warn_large_data = F)


push_mods

# Then, call the function without quotes around `baseline_vars`:
fit_socialising_on_donate <-
  margot::regress_with_covariates(
    dt_18,
    outcome = "charity_donate",
    exposure = "hours_community_round",
    baseline_vars = base_var
  )

parameters::model_parameters(fit_socialising_on_donate, ci_method="wald")[2, ]

fit_socialising_on_volunteer <-
  margot::regress_with_covariates(
    dt_18,
    outcome = "hours_charity",
    exposure = "hours_community_round",
    baseline_vars = base_var
  )
parameters::model_parameters(fit_socialising_on_volunteer)[2, ]

fit_socialising_on_community_time <-
  margot::regress_with_covariates(
    dt_18,
    outcome = "community_time_binary",
    exposure = "hours_community_round",
    baseline_vars = base_var,
    family = "binomial"
  )
parameters::model_parameters(fit_socialising_on_community_time, ci_method="wald")[2, ]


fit_socialising_on_community_money <-
  margot::regress_with_covariates(
    dt_18,
    outcome = "community_money_binary",
    exposure = "hours_community_round",
    baseline_vars = base_var,
    family = "binomial"
  )
parameters::model_parameters(fit_socialising_on_community_money, ci_method="wald")[2, ]
here_save(fit_socialising_on_community_money, "fit_socialising_on_community_money")
fit_church_on_donate <-
  here_read("fit_church_on_donate")

here_save(fit_church_on_volunteer, "fit_church_on_volunteer")
fit_church_on_volunteer <-
  here_read("fit_church_on_volunteer")


here_save(fit_socialising_on_donate, "fit_socialising_on_donate")
fit_socialising_on_donate <-
  here_read("fit_socialising_on_donate")

here_save(fit_socialising_on_volunteer,
          "fit_socialising_on_volunteer")
fit_socialising_on_volunteer <-
  here_read("fit_socialising_on_volunteer")



# tables ------------------------------------------------------------------
library(gtsummary)



# table baseline ----------------------------------------------------------

# prepare df
selected_base_cols <-
  dt_18 |> select(all_of(base_var)) #


#check
colnames(selected_base_cols)

#chck
selected_base_cols
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
nzavs_exposure
# get vars.
selected_exposure_cols <-
  dt_18_19 %>% select(
    c(!!sym(nzavs_exposure),
      "hours_community_round",
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
    by = "Wave",
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
    Community_gives_money = community_money_binary,
    Community_gives_time = community_time_binary,
    Family_gives_money = family_money_binary,
    Family_gives_time = family_time_binary,
    Friends_give_money = friends_money_binary,
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
    by = "Wave",
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
  #  add_n() %>%  # Add column with total number of non-missing observations
  modify_header(label = "**Outcome Variables by Wave**") %>%  # Update the column header
  bold_labels()

table_outcomes


here_save(table_outcomes, "table_outcomes")
table_outcomes <- here_read("table_outcomes")




# histogram exposure ------------------------------------------------------

dt_19 <- dat_long |> 
  filter(wave == 2019)

library(ggplot2)
library(dplyr)


#
# # generate bar plot
!!sym(nzavs_exposure)
nzavs_exposure

graph_density_of_exposure_up <- margot::coloured_histogram_shift(
  dt_19,
  col_name = "hours_community_round",
  binwidth = .5, 
  range_highlight = c(0,10)
)
graph_density_of_exposure_up

push_mods
graph_density_of_exposure_down <- margot::coloured_histogram_shift(
  dt_19,
  shift = "down",
  col_name = "religion_church_round",
  binwidth = .5, 
  range_highlight = c(1,10)
)
graph_density_of_exposure_up
graph_density_of_exposure_up
push_mods
here_save(graph_density_of_exposure_down, "graph_density_of_exposure_down")
here_save(graph_density_of_exposure_up, "graph_density_of_exposure_up")

graph_density_of_exposure_down
# graph_density_of_exposure
#
#here_save(graph_density_of_exposure, "graph_density_of_exposure")
ggsave(
  graph_density_of_exposure_up,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "graph_density_of_exposure_up.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)

ggsave(
  graph_density_of_exposure_down,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "graph_density_of_exposure_up.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 400
)




# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/funs.R")

# ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
source(
  "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
)
#devtools::install_github("go-bayes/margot")

dt_19 <- dat_long |>
  filter(wave == 2019) |>
  select(c(religion_church_round, hours_community_round))

library(margot)

# impute baseline ---------------------------------------------------------
# impute baseline data (we use censoring for the outcomes)
#colnames(dat_long)
# function imputes only baseline not outcome
#
#devtools::install_github("go-bayes/margot")
str(dat_long)
dat_long$sample_weights
dat_long_df <- data.frame(dat_long)

my_data_filtered <- as.data.frame(dat_long_df)
my_data_filtered <- haven::zap_formats(dat_long_df)
my_data_filtered <- haven::zap_label(dat_long_df)
my_data_filtered <- haven::zap_widths(dat_long_df)

dat_long_df <- data.frame(dat_long)

prep_coop_all <-
  margot_wide_impute_baseline(
    dat_long_df,
    baseline_vars = baseline_vars,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars
  )

prep_coop_all$t0_sample_weights
# save function -- will save to your "push_mod" directory
here_save(prep_coop_all, "prep_coop_all")

# check mi model
outlist <-
  row.names(prep_coop_all)[prep_coop_all$outflux < 0.5]
.length(outlist)

# checks. We do not impute with weights: area of current research
head(prep_coop_all$loggedEvents, 10)

# 
# Warning messages:
#   1: Number of logged events: 5 
# 2: Using an external vector in selections was deprecated in tidyselect 1.1.0.
# ℹ Please use `all_of()` or `any_of()` instead.
# # Was:
# data %>% select(t0_column_order)
# 
# # Now:
# data %>% select(all_of(t0_column_order))
# 
# See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.



# read function
prep_coop_all <-
  here_read("prep_coop_all")

head(prep_coop_all)
naniar::vis_miss(prep_coop_all, warn_large_data = FALSE)
dev.off()

table(prep_coop_all$t0_censored)
head(prep_coop_all$t0_sam)

#check must be a dataframe
str(prep_coop_all)
nrow(prep_coop_all)





# spit shine --------------------------------------------------------------

df_wide_censored <- prep_coop_all |>
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat),
    t0_education_level_coarsen = as.factor(t0_education_level_coarsen),
    t0_volunteers_binary = ifelse(t0_hours_charity > 0, 1, 0),
    t2_volunteers_binary = ifelse(t0_hours_charity > 0, 1, 0),
  ) |>
  relocate("t0_censored", .before = starts_with("t1_")) |>
  relocate("t1_censored", .before = starts_with("t2_")) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))

# cehck
naniar::vis_miss(df_wide_censored, warn_large_data = FALSE)

table(df_wide_censored$t0_censored)

outcome_vars

outcome_vars
str(df_wide_censored)

library(dplyr)


library(dplyr)

# Assuming df_wide_censored is your dataframe

# Calculate the conditions before the mutate steps
t0_na_condition <-
  rowSums(is.na(select(df_wide_censored, starts_with("t1_")))) > 0
t1_na_condition <-
  rowSums(is.na(select(df_wide_censored, starts_with("t2_")))) > 0

df_clean <- df_wide_censored %>%
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
  mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%
  mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .))) |>
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_censored &
        !t0_volunteers_binary &
        !t0_family_time_binary &
        !t0_friends_time_binary &
        !t0_community_time_binary &
        !t0_family_money_binary &
        !t0_friends_money_binary &
        !t0_community_money_binary &
        !t0_religion_church_round &
        !t0_hours_community_round &
        #  !t0_charity_donate & !t0_sample_weights &
      #  !t1_religion_church_round &
        !t1_hours_community_round &
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
    t0_hours_community_round,
    t0_volunteers_binary,
    t0_family_time_binary,
    t0_friends_time_binary,
    t0_community_time_binary,
    t0_family_money_binary,
    t0_friends_money_binary,
    t0_community_money_binary,
    t0_sample_weights,
    t0_religion_church_round,
    t0_censored,
    #t1_religion_church_round,
    t1_hours_community_round,
    t1_censored,
    # t2_charity_donate,
    # t2_hours_charity,
    t2_volunteers_binary,
    t2_family_time_binary,
    t2_friends_time_binary,
    t2_community_time_binary,
    t2_family_money_binary,
    t2_friends_money_binary,
    t2_community_money_binary,
    ends_with("_z")
  ) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))





here_save(df_clean, "df_clean")
# read data --  start here if previous work already done



df_clean <- here_read("df_clean")

colnames(df_clean)
str(df_clean)
# names of vars for modelling


names_base <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     -t0_censored) |> colnames()

names_base
here_save(names_base, "names_base")
names_base <- here_read("names_base")

names_outcomes <-
  df_clean |> select(starts_with("t2")) |> colnames()

names_outcomes

here_save(names_outcomes, "names_outcomes")
names_outcomes <- here_read("names_outcomes")
names_outcomes

# 
# 
# 
# filter_variables <-
#   function(names_base,
#            names_outcomes,
#            prefixes = c("t0_", "t2_")) {
#     # Check if prefixes vector has exactly 2 elements
#     if (length(prefixes) != 2) {
#       stop("The 'prefixes' argument must contain exactly 2 elements.")
#     }
#     
#     # Extract base part of the variable names by removing prefixes
#     base_stripped <-
#       sub(paste0("^", prefixes[1]), "", names_base)
#     outcomes_stripped <-
#       sub(paste0("^", prefixes[2]), "", names_outcomes)
#     
#     # Perform set difference on base names not in outcomes
#     unique_base <-
#       setdiff(base_stripped, outcomes_stripped)
#     
#     # Reapply the first prefix to obtain the original variable names to keep
#     names_to_keep <- paste0(prefixes[1], unique_base)
#     
#     return(names_to_keep)
#   }


# # Example usage with the provided lists of variable names
# names_base <- c("t0_sample_origin", "t0_education_level_coarsen", "t0_eth_cat",
#                 "...") # and so on, fill in with the actual variable names
# names_outcomes <- c("t2_charity_donate", "t2_hours_charity", "t2_family_time_binary",
#                     "...") # and so on, fill in with the actual variable names
#
# # Call the function
# names_to_keep <- filterVariables(names_base, names_outcomes, c("t0_", "t2_"))
#
# # Print the variable names to keep
# print(names_to_keep)
#

# names_to_keep <-
#   filter_variables(names_base, names_outcomes, c("t0_", "t2_"))
# 
# names_to_keep
# colnames(df_clean)
# 
# 
# names_to_keep
# 
# 
# # use names
# use_names <-
#   setdiff(names_to_keep,  c("t0_sample_weights", "t0_censored"))
# use_names
# 
# names_outcomes <-
#   df_clean |> select(starts_with("t2")) |> colnames()




#### SET VARIABLE NAMES
#  model
nzavs_exposure
A <- c("t0_hours_community_round", "t1_hours_community_round")
C <- c("t0_censored", "t1_censored")

#L <- list(c("L1"), c("L2"))
W <-  c(paste(names_base, collapse = ", "))
names_base
W
# check
print(W)

# 
# 
# gain_A <- function(data, trt) {
#   mtp_base <- function(data, trt) {
#     ifelse(data[[trt]] > 0, 0,  data[[trt]])
#   }
#   
#   if (trt == "t0_hours_community_round") {
#     return(mtp_base(data, trt))
#   }
#   
#   mtp_one_contrast <- function(data, trt) {
#     ifelse(data[[trt]] < 1, 1, data[[trt]])
#   }
#   
#   #  trt is a variable name passed as a string to the function
#   
#   ifelse(trt == "t1_hours_community_round",
#          mtp_one_contrast(data, trt),
#          data[[trt]])
# }
# 
# 
# 
# 
# 
# NULL <- function(data, trt) {
#   mtp_base <- function(data, trt) {
#     ifelse(data[[trt]] > 0, 0, data[[trt]])
#   }
#   
#   if (trt == "t0_hours_community_round") {
#     return(mtp_base(data, trt))
#   }
#   
#   mtp_one_contrast <- function(data, trt) {
#     ifelse(data[[trt]] > 0, 0, data[[trt]])
#   }
#   
#   #  trt is a variable name passed as a string to the function
#   
#   ifelse(trt == "t1_hours_community_round",
#          mtp_one_contrast(data, trt),
#          data[[trt]])
# }



gain_A <- function(data, trt)  data[[trt]] + 1


# NULL <- function(data, trt){
#   ifelse( data[[trt]] > 0, 0,  data[[trt]] )
# }

gain_A


# BONUS: progressr progress bars!
progressr::handlers(global = TRUE)

library(future)
plan(multisession)
n_cores <-
  parallel::detectCores()-2

n_cores
# church: charity models ----------------------------------------------------------


library("ranger")


# test data
df_clean_slice <- df_clean |>
  slice_head(n = 500) |>
  as.data.frame()
colnames(df_clean_slice)

library(SuperLearner)
# library(xgboost)
# library(ranger)
# model charitable giving in population
# measure time taken to run the model
#
# names_base_t2_hours_charity_z <-
#   select_and_rename_cols(names_base = use_names,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_hours_charity_z")
#
# names_base_t2_hours_charity_z
#m_hours_charity_z_test

sl_lib <- c("SL.glmnet",
            "SL.ranger", #
            "SL.xgboost") #
library(ranger)



#names_t2_hours_charity_z<- select_and_rename_cols(names_base = names_base, baseline_vars = base_var, outcom = "t2_hours_charity_z")

t2_charity_donate_z_test_gain <- lmtp_tmle(
  outcome = "t2_charity_donate_z",
  baseline = names_base,
  shift = gain_A,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_sample_weights,
  learners_trt = "SL.ranger",
  # ranger much faster
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

t2_charity_donate_z_test_gain


t2_charity_donate_z_test_zero <- lmtp_tmle(
  outcome = "t2_charity_donate_z",
  baseline = names_base,
  shift = NULL,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_sample_weights,
  learners_trt = "SL.ranger",
  # ranger much faster
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

t2_charity_donate_z_test_zero
t2_charity_donate_z_test_gain

contrast_hours_charity_z_test <-
  lmtp_contrast(t2_charity_donate_z_test_gain, ref = t2_charity_donate_z_test_zero, type = "additive")
contrast_hours_charity_z_test

# models ------------------------------------------------------------------

t2_hours_charity_z_gain <- lmtp_tmle(
  outcome = "t2_hours_charity_z",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)
here_save(t2_hours_charity_z_gain, "t2_hours_charity_z_gain")


t2_hours_charity_z_zero <- lmtp_tmle(
  outcome = "t2_hours_charity_z",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_hours_charity_z_zero, "t2_hours_charity_z_zero")


t2_volunteers_binary_gain <- lmtp_tmle(
  outcome = "t2_volunteers_binary",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_volunteers_binary_gain, "t2_volunteers_binary_gain")


t2_volunteers_binary_zero <- lmtp_tmle(
  outcome = "t2_volunteers_binary",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_volunteers_binary_zero, "t2_volunteers_binary_zero")


# church donations --------------------------------------------------------
t2_charity_donate_z_gain <- lmtp_tmle(
  outcome = "t2_charity_donate_z",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)
here_save(t2_charity_donate_z_gain, "t2_charity_donate_z_gain")


t2_charity_donate_z_zero <- lmtp_tmle(
  outcome = "t2_charity_donate_z",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_charity_donate_z_zero, "t2_charity_donate_z_zero")


#############################
# church subjective support -----------------------------------------------
#############################



# church soc support ------------------------------------------------------


t2_support_z_gain <- lmtp_tmle(
  outcome = "t2_support_z",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)
here_save(t2_support_z_gain, "t2_support_z_gain")


t2_support_z_zero <- lmtp_tmle(
  outcome = "t2_support_z",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_support_z_zero, "t2_support_z_zero")

# church soc belong -------------------------------------------------------

t2_belong_z_gain <- lmtp_tmle(
  outcome = "t2_belong_z",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)
here_save(t2_belong_z_gain, "t2_belong_z_gain")


t2_belong_z_zero <- lmtp_tmle(
  outcome = "t2_belong_z",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_belong_z_zero, "t2_belong_z_zero")


# church neighbourhood ----------------------------------------------------

t2_neighbourhood_community_z_gain <- lmtp_tmle(
  outcome = "t2_neighbourhood_community_z",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)
here_save(t2_neighbourhood_community_z_gain,
          "t2_neighbourhood_community_z_gain")

## FIX THIS
t2_neighbourhood_community_z_zero <- lmtp_tmle(
  outcome = "t2_neighbourhood_community_z",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(
  t2_neighbourhood_community_z_zero,
  "t2_neighbourhood_community_z_zero"
)


# church: family time help received -----------------------------------------------

t2_family_time_binary_gain <- lmtp_tmle(
  outcome = "t2_family_time_binary",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_family_time_binary_gain, "t2_family_time_binary_gain")

t2_family_time_binary_zero <- lmtp_tmle(
  outcome = "t2_family_time_binary",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_family_time_binary_zero, "t2_family_time_binary_zero")


# church: friends help time received ----------------------------------------------

t2_friends_time_binary_gain <- lmtp_tmle(
  outcome = "t2_friends_time_binary",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_friends_time_binary_gain, "t2_friends_time_binary_gain")


t2_friends_time_binary_zero <- lmtp_tmle(
  outcome = "t2_friends_time_binary",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_friends_time_binary_zero, "t2_friends_time_binary_zero")



# church: community time help received --------------------------------------------
t2_community_time_binary_gain <- lmtp_tmle(
  outcome = "t2_community_time_binary",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_community_time_binary_gain,
          "t2_community_time_binary_gain")


t2_community_time_binary_zero <- lmtp_tmle(
  outcome = "t2_community_time_binary",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_community_time_binary_zero,
          "t2_community_time_binary_zero")

# money -------------------------------------------------------------------


# church_friends money received --------------------------------------------------
t2_family_money_binary_gain <- lmtp_tmle(
  outcome = "t2_family_money_binary",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_family_money_binary_gain, "t2_family_money_binary_gain")

t2_family_money_binary_zero <- lmtp_tmle(
  outcome = "t2_family_money_binary",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)
here_save(t2_family_money_binary_zero, "t2_family_money_binary_zero")

# church: friends help money received ----------------------------------------------

t2_friends_money_binary_gain <- lmtp_tmle(
  outcome = "t2_friends_money_binary",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_friends_money_binary_gain,
          "t2_friends_money_binary_gain")

t2_friends_money_binary_zero <- lmtp_tmle(
  outcome = "t2_friends_money_binary",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_friends_money_binary_zero,
          "t2_friends_money_binary_zero")


# church: community money help received --------------------------------------------
t2_community_money_binary_gain <- lmtp_tmle(
  outcome = "t2_community_money_binary",
  baseline = names_base,
  shift = gain_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_community_money_binary_gain,
          "t2_community_money_binary_gain")

t2_community_money_binary_zero <- lmtp_tmle(
  outcome = "t2_community_money_binary",
  baseline = names_base,
  shift = NULL,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores
)

here_save(t2_community_money_binary_zero,
          "t2_community_money_binary_zero")


# results -----------------------------------------------------------------

t2_volunteers_binary_gain <- here_read("t2_volunteers_binary_gain")
t2_volunteers_binary_zero <- here_read("t2_volunteers_binary_zero")
t2_hours_charity_z_gain <-
  here_read("t2_hours_charity_z_gain") # note spelling
t2_hours_charity_z_zero <- here_read("t2_hours_charity_z_zero")
t2_charity_donate_z_gain <- here_read("t2_charity_donate_z_gain")
t2_charity_donate_z_zero <- here_read("t2_charity_donate_z_zero")
t2_support_z_gain <- here_read("t2_support_z_gain")
t2_support_z_zero <- here_read("t2_support_z_zero")
t2_belong_z_gain <- here_read("t2_belong_z_gain")
t2_belong_z_zero <- here_read("t2_belong_z_zero")
t2_neighbourhood_community_z_gain <-
  here_read("t2_neighbourhood_community_z_gain")
t2_neighbourhood_community_z_zero <-
  here_read("t2_neighbourhood_community_z_zero")
t2_family_time_binary_gain <-
  here_read("t2_family_time_binary_gain")
t2_family_time_binary_zero <-
  here_read("t2_family_time_binary_zero")
t2_friends_time_binary_gain <-
  here_read("t2_friends_time_binary_gain")
t2_friends_time_binary_zero <-
  here_read("t2_friends_time_binary_zero")
t2_community_time_binary_gain <-
  here_read("t2_community_time_binary_gain")
t2_community_time_binary_zero <-
  here_read("t2_community_time_binary_zero")
t2_family_money_binary_gain <-
  here_read("t2_family_money_binary_gain")
t2_family_money_binary_zero <-
  here_read("t2_family_money_binary_zero")
t2_friends_money_binary_gain <-
  here_read("t2_friends_money_binary_gain")
t2_friends_money_binary_zero <-
  here_read("t2_friends_money_binary_zero")
t2_community_money_binary_gain <-
  here_read("t2_community_money_binary_gain")
t2_community_money_binary_zero <-
  here_read("t2_community_money_binary_zero")




push_mods
# results hours volunteer ---------------------------------------------------

contrast_hours_charity_z <-
  lmtp_contrast(t2_hours_charity_z_gain, ref = t2_hours_charity_z_zero, type = "additive")


tab_contrast_hours_charity_z <- margot_tab_lmtp(
  contrast_hours_charity_z,
  scale = "RD",
  new_name = "socializing: hours volunteers"
)

output_tab_contrast_hours_charity_z <-
  lmtp_evalue_tab(
    tab_contrast_hours_charity_z,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_tab_contrast_hours_charity_z


# results charity donate --------------------------------------------------


contrast_charity_donate_z <-
  lmtp_contrast(t2_charity_donate_z_gain , ref =  t2_charity_donate_z_zero, type = "additive")


tab_contrast_charity_donate_z <- margot_tab_lmtp(
  contrast_charity_donate_z,
  scale = "RD",
  new_name = "socializing: charity donations"
)



output_tab_contrast_charity_donate_z<- lmtp_evalue_tab(tab_contrast_charity_donate_z,  delta = 1, sd = 1, scale = c("RD"))
output_tab_contrast_charity_donate_z

# results volunteers binary -----------------------------------------------

contrast_volunteers_binary   <-
  lmtp_contrast(t2_volunteers_binary_gain , ref =  t2_volunteers_binary_zero, type = "rr")

tab_contrast_volunteers_binary <- margot_tab_lmtp(
  contrast_volunteers_binary,
  scale = "RR",
  new_name = "socializing: volunteers (binary)"
)
tab_contrast_volunteers_binary

output_tab_contrast_volunteers_binary <- lmtp_evalue_tab(tab_contrast_volunteers_binary,  delta = 1, sd = 1, scale = c("RR"))

output_tab_contrast_volunteers_binary


# results support ---------------------------------------------------------
# lmtp_evalue_tab  <-
#   function(x,
#            delta = 1,
#            sd = 1,
#            scale = c("RD", "RR")) {
#     require("EValue")
#     require(dplyr)
#     
#     scale <- match.arg(scale)
#     
#     tab0 <- as.data.frame(x)
#     
#     if (scale == "RD") {
#       evalout <- as.data.frame(round(
#         EValue::evalues.OLS(
#           tab0[1, 1],
#           se = tab0[1, 2],
#           sd = sd,
#           delta = delta,
#           true = 0
#         ),
#         2
#       ))
#     } else {
#       evalout <- as.data.frame(round(EValue::evalues.RR(
#         tab0[1, 1],
#         lo = tab0[1, 3],
#         hi = tab0[1, 4],
#         true = 1
#       ),
#       3))
#     }
#     
#     evalout2 <- subset(evalout[2, ])
#     evalout3 <- evalout2 |>
#       select_if( ~ !any(is.na(.)))
#     colnames(evalout3) <- c("E_Value", "E_Val_bound")
#     
#     if (scale == "RD") {
#       tab <-
#         cbind.data.frame(tab0, evalout3) |> dplyr::select(-c(standard_error))
#     } else {
#       tab <- cbind.data.frame(tab0, evalout3)  |> dplyr::select(-c(standard_error))
#     }
#     
#     return(tab)
#   }




contrast_support_z <-
  lmtp_contrast(t2_support_z_gain , ref =  t2_support_z_zero, type = "additive")

tab_contrast_support_z <- margot_tab_lmtp(
  contrast_support_z,
  scale = "RD",
  new_name = "socializing: social suport"
)
tab_contrast_support_z

output_tab_contrast_support_z<- lmtp_evalue_tab(tab_contrast_support_z,  delta = 1, sd = 1, scale = c("RD"))

output_tab_contrast_support_z


# results belong ----------------------------------------------------------


contrast_belong_z <-
  lmtp_contrast(t2_belong_z_gain, ref =  t2_belong_z_zero, type = "additive")




tab_contrast_belong_z <- margot_tab_lmtp(
  contrast_belong_z,
  scale = "RD",
  new_name = "socializing: social belonging"
)

output_tab_contrast_belong_z<- lmtp_evalue_tab(tab_contrast_belong_z,  delta = 1, sd = 1, scale = c("RD"))


# results neighbourcommunity ----------------------------------------------


contrast_neighbourhood_community_z <-
  lmtp_contrast(t2_neighbourhood_community_z_gain ,
                ref = t2_neighbourhood_community_z_zero ,
                type = "additive")

tab_contrast_neighbourhood_community_z <- margot_tab_lmtp(
  contrast_neighbourhood_community_z,
  scale = "RD",
  new_name = "socializing: neighbourhood community"
)

output_tab_contrast_neighbourhood_community_z<- lmtp_evalue_tab(tab_contrast_neighbourhood_community_z,  delta = 1, sd = 1, scale = c("RD"))


output_tab_contrast_neighbourhood_community_z

# results family time -----------------------------------------------------



contrast_family_time_binary <-
  lmtp_contrast(t2_family_time_binary_gain, ref =  t2_family_time_binary_zero, type = "rr")


tab_contrast_family_time_binary <- margot_tab_lmtp(
  contrast_family_time_binary,
  scale = "RR",
  new_name = "socializing: family gives time"
)
tab_contrast_family_time_binary

output_tab_contrast_family_time<- lmtp_evalue_tab(tab_contrast_family_time_binary,  delta = 1, sd = 1, scale = c("RR"))
output_tab_contrast_family_time


# results friends time ----------------------------------------------------


contrast_friends_time <-
  lmtp_contrast(t2_friends_time_binary_gain , ref =  t2_friends_time_binary_zero, type = "rr")

tab_contrast_friends_time <- margot_tab_lmtp(
  contrast_friends_time,
  scale = "RR",
  new_name = "socializing: friends gives time"
)


output_tab_contrast_friends_time<- lmtp_evalue_tab(tab_contrast_friends_time,  delta = 1, sd = 1, scale = c("RR"))
output_tab_contrast_friends_time


# results community time --------------------------------------------------



contrast_community_time <-
  lmtp_contrast(t2_community_time_binary_gain,
                ref =  t2_community_time_binary_zero,
                type = "rr")


tab_contrast_community_time <- margot_tab_lmtp(
  contrast_community_time,
  scale = "RR",
  new_name = "socializing: community gives time"
)


output_tab_contrast_community_time<- lmtp_evalue_tab(tab_contrast_community_time,  delta = 1, sd = 1, scale = c("RR"))
output_tab_contrast_community_time



# results family money  ---------------------------------------------------


contrast_family_money <-
  lmtp_contrast(t2_family_money_binary_gain , ref =  t2_family_money_binary_zero, type = "rr")

tab_contrast_family_money <- margot_tab_lmtp(
  contrast_family_money,
  scale = "RR",
  new_name = "socializing: family gives money"
)


output_tab_contrast_family_money<- lmtp_evalue_tab(tab_contrast_family_money,  delta = 1, sd = 1, scale = c("RR"))
output_tab_contrast_family_money


# results friends money ---------------------------------------------------


contrast_friends_money <-
  lmtp_contrast(t2_friends_money_binary_gain,
                ref =  t2_friends_money_binary_zero,
                type = "rr")

tab_contrast_friends_money <- margot_tab_lmtp(
  contrast_friends_money,
  scale = "RR",
  new_name = "socializing: friends gives money"
)

output_tab_contrast_friends_money<- lmtp_evalue_tab(tab_contrast_friends_money,  delta = 1, sd = 1, scale = c("RR"))





# results community money -------------------------------------------------


contrast_community_money <-
  lmtp_contrast(t2_community_money_binary_gain,
                ref = t2_community_money_binary_zero ,
                type = "rr")

tab_contrast_community_money <- margot_tab_lmtp(
  contrast_community_money,
  scale = "RR",
  new_name = "socializing: community gives money"
)

output_tab_contrast_community_money<- lmtp_evalue_tab(tab_contrast_community_money,  delta = 1, sd = 1, scale = c("RR"))

output_tab_contrast_community_money


#######################

# tables and graphs -------------------------------------------------------

#######################


tab_all_prosocial <- rbind( output_tab_contrast_charity_donate_z,
                            output_tab_contrast_hours_charity_z)
group_tab_all_prosocial <- group_tab(tab_all_prosocial, type = "RD")

here_save(tab_all_prosocial, "tab_all_prosocial")
here_save(group_tab_all_prosocial, "group_tab_all_prosocial")

tab_all_prosocial_rr <- output_tab_contrast_volunteers_binary
group_tab_all_prosocial_rr <- group_tab(tab_all_prosocial_rr, type = "RR")

here_save(tab_all_prosocial_rr, "tab_all_prosocial_rr")
here_save(group_tab_all_prosocial_rr, "group_tab_all_prosocial_rr")


tab_all_perceived_support <- rbind(
  output_tab_contrast_neighbourhood_community_z,
  output_tab_contrast_belong_z, 
  output_tab_contrast_support_z
)

group_tab_all_perceived_support <- group_tab(tab_all_perceived_support, type = "RD")
here_save( tab_all_perceived_support, "tab_all_perceived_support")
here_save( group_tab_all_perceived_support, "group_tab_all_perceived_support")

tab_all_received_money <- rbind(
  output_tab_contrast_family_money,
  output_tab_contrast_friends_money, 
  output_tab_contrast_community_money
)

group_tab_all_received_money <- group_tab(tab_all_received_money, type = "RR")

here_save(tab_all_received_money, "tab_all_received_money")
here_save(group_tab_all_received_money, "group_tab_all_received_money")

group_tab_all_received_money

tab_contrast_received_time <- rbind(
  output_tab_contrast_family_time,
  output_tab_contrast_friends_time, 
  output_tab_contrast_community_time)

group_tab_contrast_received_time<- group_tab(tab_contrast_received_time, type = "RR")

here_save(tab_contrast_received_time, "tab_contrast_received_time")
here_save(group_tab_contrast_received_time, "group_tab_contrast_received_time")

# import to manuscript -------------------------------------------------------


tab_all_prosocial <- here_read("tab_all_prosocial")
group_tab_all_prosocial<- here_read("group_tab_all_prosocial")

tab_all_prosocial_rr <- here_read("tab_all_prosocial_rr")
group_tab_all_prosocial_rr <- here_read("group_tab_all_prosocial_rr")

tab_all_perceived_support <- here_read("tab_all_perceived_support")
group_tab_all_perceived_support <- here_read("group_tab_all_perceived_support")

tab_all_received_money <- here_read( "tab_all_received_money")
group_tab_all_received_money <- here_read( "group_tab_all_received_money")

tab_contrast_received_time <- here_read( "tab_contrast_received_time")
group_tab_contrast_received_time <- here_read( "group_tab_contrast_received_time")

tab_all_prosocial
group_tab_all_prosocial_rr

tab_all_prosocial_rr
group_tab_all_prosocial_rr

tab_all_perceived_support
group_tab_all_perceived_support

tab_all_received_money
group_tab_all_received_money

tab_contrast_received_time
group_tab_contrast_received_time


group_tab_all_prosocial_rr
# graphs ------------------------------------------------------------------





# conflicts_prefer(ggplot2::margin)
# plot_group_tab_all_prosocial_rr <- margot_plot(
#   group_tab_all_prosocial_rr,
#   type = "RR",
#   title = "Religious service effect on reported volunteering (binary)",
#   subtitle = "Contrast: >= weekly vs. none ",
#   xlab = "",
#   ylab = "",
#   estimate_scale = 1,
#   base_size = 11,
#   text_size = 4.5,
#   point_size = 2.5,
#   title_size = 20,
#   subtitle_size = 14,
#   legend_text_size = 8,
#   legend_title_size = 10,
#   x_offset = -.5,
#   x_lim_lo = .5,
#   x_lim_hi =  1.5
# )
# 
# plot_group_tab_all_prosocial_rr

title = "Weekly Community Socializing For At Least One Hour vs No Intervention"

plot_group_tab_all_prosocial <- margot_plot(
  group_tab_all_prosocial,
  type = "RD",
  title =title,
  subtitle = "Self-Reported Prosociality",
  xlab = "",
  ylab = "",
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

here_save(plot_group_tab_all_prosocial, "plot_group_tab_all_prosocial")

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


plot_group_tab_all_perceived_support <- margot_plot(
  group_tab_all_perceived_support,
  type = "RD",
  title =title,
  subtitle = "Perceived Social Support",
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

plot_group_tab_all_perceived_support

here_save(plot_group_tab_all_perceived_support,"plot_group_tab_all_perceived_support")


ggsave(
  plot_group_tab_all_perceived_support,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_all_perceived_support.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)


plot_group_tab_all_received_time <- margot_plot(
  group_tab_contrast_received_time,
  type = "RR",
  title = title,
  subtitle = "Support Received From Others: Time",
  xlab = "",
  ylab = "",
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

plot_group_tab_all_received_time
here_save(plot_group_tab_all_received_time, "plot_group_tab_all_received_time")

ggsave(
  plot_group_tab_all_received_time,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_all_received_time.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)


plot_group_tab_all_received_money <- margot_plot(
  group_tab_all_received_money,
  type = "RR",
  title = title,
  subtitle = "Support Received From Others: Money",
  xlab = "",
  ylab = "",
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

here_save(plot_group_tab_all_received_money, "plot_group_tab_all_received_money")

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


### GET 

transition_table_socialising_shift <- here_read("transition_table_socialising_shift")
transition_table_socialising<- here_read( "transition_table_socialising")
n_participants <- here_read("n_participants")

table_baseline <- here_save("table_baseline")
table_exposures <- here_save("table_exposures")
table_outcomes <- here_save("table_outcomes")
                                          

graph_density_of_exposure_down<- here_read("graph_density_of_exposure_down")
graph_density_of_exposure_up<- here_read("graph_density_of_exposure_up")
graph_density_of_exposure_up
