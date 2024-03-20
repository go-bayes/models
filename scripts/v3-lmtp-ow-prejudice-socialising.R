# v3-lmtp-ow-prejudice-church.R
# joseph.bulbulia@gmail.com
# march 17 2024

# 8 Nov 2023
# original script is in the 00drafts folder.
# this script brings the analysis for this study to the 'models" workflow

### ALWAYS RESTART R IN A FRESH SESSION ####


# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/libs2.R")


# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/funs.R")

# ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
source(
  "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
)

# experimental functions (more functions)
# source(
#   "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
# )


## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE.
pull_path <-
  fs::path_expand(
    #"/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs_refactor/nzavs_data_23"
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data"
  )

# read data: note that you need use the arrow package in R

dat <- arrow::read_parquet(pull_path)


### WARNING: THIS PATH WILL NOT WORK FOR YOU. PLEASE SET A PATH TO YOUR OWN COMPUTER!! ###
### WARNING: FOR EACH NEW STUDY SET UP A DIFFERENT PATH OTHERWISE YOU WILL WRITE OVER YOUR MODELS
push_mods <-  fs::path_expand(
  "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/nzvs_mods/24/ow-soc-prej"
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
n_cores <- parallel::detectCores() - 2

# super learner libraries
sl_lib <- c("SL.glmnet",
            "SL.ranger", #
            "SL.xgboost") #


# large data
sl_lib_ranger <- c("SL.ranger") #

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
    "parent",
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
    "perc_gend_discrim",
    "perc_religious_discrim",
    "perc_discrim",
    #"neighbourhood_community",
    # #I feel a sense of community with others in my local neighbourhood.
    #"support",
    # "belong",
    "rural_gch_2018_l",
    # "hlth_disability",
    # value label 0    No 1   Yes
    "urban",
    # see NZAVS,
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
    "charity_donate",
    #How much money have you donated to charity in the last year?
    "hours_charity",
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
    "kessler6_sum",
    # "kessler_latent_depression",
    # "kessler_latent_anxiety",
    "hlth_fatigue",
    "hlth_sleep_hours",
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
    #  children_num_log = log(children_num + 1)
  ) |>
  # dplyr::select(
  #   -c(
  #     hours_work,
  #     hours_housework,
  #     household_inc,
  #     hours_exercise,
  #     hours_children,
  #     have_siblings,
  #     children_num,
  #     total_siblings
  #   )
# ) |>
droplevels() |>
  dplyr::rename(sample_weights = w_gend_age_ethnic,
                sample_origin =  sample_origin_names_combined) |>
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
dplyr::mutate(# religion_church_binary = as.factor(religion_church_binary),
  # eth_cat = as.integer(eth_cat),
  urban = as.numeric(urban)) |>
  mutate(
    rural_gch_2018_l = as.numeric(as.character(rural_gch_2018_l)),
    have_siblings = as.numeric(as.character(have_siblings)),
    parent = as.numeric(as.character(parent)),
    partner = as.numeric(as.character(partner)),
    born_nz = as.numeric(as.character(born_nz)),
    censored = as.numeric(as.character(censored)),
    employed = as.numeric(as.character(employed))#,
    #  hlth_disability = as.numeric(as.character(hlth_disability))
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

# baseline vars -----------------------------------------------------------

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

push_mods
here_save(base_var, "base_var")
here_save(baseline_vars, "baseline_vars")

push_mods
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
  "warm_refugees",
  "perc_gend_discrim",
  "perc_religious_discrim",
  "perc_discrim"
)



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
dat_long$wave

dt_positivity_full <- dat_long |>
  filter(wave == 2018 | wave == 2019) |>
  select(wave, id, hours_community_round, sample_weights) |>
  mutate(hours_community_round_shift = ifelse(hours_community_round >= 4, 1, 0))



# create transition matrix
out <-
  msm::statetable.msm(hours_community_round, id, data = dt_positivity_full)

out_binary <-
  msm::statetable.msm(hours_community_round_shift, id, data = dt_positivity_full)

out
out_binary

t_tab_binary_labels <- c("< weekly", ">= weekly")
# transition table

transition_table  <- margot::transition_table(out)
transition_table
# for import later
here_save(transition_table, "transition_table")

transition_table_out_binary <-
  margot::transition_table(out_binary,
                           state_names = t_tab_binary_labels)

transition_table_out_binary

# for import later
here_save(transition_table_binary,
          "transition_table_out_binary")
transition_table_out_binary <-
  here_read("transition_table_out_binary")
push_mods

# double check path
push_mods

# check col names
colnames(dat)


# sd values ---------------------------------------------------------------


# check associations only -------------------------------------------------



dt_18 <- dat_long|>
  filter(wave == 2018) 


dt_18_miss <- dt_18 |> select(-alert_level_combined_lead)

naniar::vis_miss(dt_18_miss, warn_large_data = F)

# Then, call the function without quotes around `baseline_vars`:
base_var <-
  setdiff(baseline_vars, c("censored", "sample_weights"))
base_var

fit_cross_sectional_maori <-
  regress_with_covariates(
    dt_18,
    outcome = "warm_maori",
    exposure = "hours_community_round",
    baseline_vars = base_var
  )
parameters::model_parameters(fit_cross_sectional_maori)[2, ]

fit_cross_sectional_muslims <-
  regress_with_covariates(
    dt_18,
    outcome = "warm_muslims",
    exposure = "hours_community_round",
    baseline_vars = base_var
  )
parameters::model_parameters(fit_cross_sectional_muslims)[2, ]


here_save(fit_cross_sectional_muslims, "fit_cross_sectional_muslims")
fit_cross_sectional_muslims <-
  here_read("fit_cross_sectional_muslims")

here_save(fit_cross_sectional_maori, "fit_cross_sectional_maori")
fit_cross_sectional_maori <-
  here_read("fit_cross_sectional_maori")


# tables ------------------------------------------------------------------
library(gtsummary)


# REAL tables -----------------------


# get names

names_base_sorted <- sort(base_var)
names_base_final <-
  c("religion_church_round",
    "hours_community_round",
    names_base_sorted)

names_base_final

##
selected_base_cols <-
  dt_18 |> select(all_of(names_base_final)) #|>  dplyr::select(-sample_weights)
str(selected_base_cols)
nrow(selected_base_cols)

colnames(selected_base_cols)

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


## all outcomes

names_outcomes_tab <- setdiff(outcome_vars, dt_18)
names_outcomes_sorted <- sort(names_outcomes_tab)
names_outcomes_final <-
  names_outcomes_sorted # consistent workflow
names_outcomes_final

names_outcomes_final



# histogram exposure ------------------------------------------------------

# dt_19 <- dat_long |>
#   mutate(wave = as.numeric(wave)) |>
#   filter(year_measured == 1 & wave == 2) |>
#   mutate(gratitude_z = scale(gratitude))
# dt_19$wave
#

library(ggplot2)
library(dplyr)

# histogram
# histogram_shift <- dt_19 |>
#   ggplot(aes(x = religion_church_round)) + # Map the variable to the x-axis
#   geom_histogram(binwidth = 1, fill = "lightgray", color = "black") + # Add histogram layer
#   labs(title = "Histogram of Religion Church Round",
#        x = "Religion Church (values above 8 rounded to eight)",
#        y = "Frequency") + # Add labels
#   theme_minimal() # Use a minimal theme for a cleaner look
#
# # Print the histogram
# print(histogram_shift)
#
# ggsave(
#   histogram_shift,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "histogram_shift.jpeg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )
#
#
# # generate bar plot
# graph_density_of_exposure <- coloured_histogram(dt_19, col_name = "religion_church_round", scale_min = 4, scale_max = 8)
#
# graph_density_of_exposure
#
# here_save(graph_density_of_exposure, "graph_density_of_exposure")
# ggsave(
#   graph_density_of_exposure,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "graph_density_of_exposure.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )



# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/funs.R")

# ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
source(
  "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
)

dt_19 <- dat_long |>
  filter(wave == 2019) |>
  select(c(religion_church_round, hours_community_round))

graph_density_of_exposure <-
  coloured_histogram_shift_range(
    dt_19,
    col_name = "hours_community_round",
    binwidth = 1,
    range_highlight = c(0, 1),
    shift = "up"
  )

graph_density_of_exposure


ggsave(
  graph_density_of_exposure,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "graph_density_of_exposure.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


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
exposure_var

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
    t0_education_level_coarsen = as.factor(t0_education_level_coarsen)
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
t0_na_condition <- rowSums(is.na(select(df_wide_censored, starts_with("t1_")))) > 0
t1_na_condition <- rowSums(is.na(select(df_wide_censored, starts_with("t2_")))) > 0

df_clean <- df_wide_censored %>%
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
  mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%
  mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .)))|>
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_censored &
        !t0_hours_community_round &
        #  !t0_charity_donate & 
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
    t0_hours_community_round,
    t0_censored,
    t1_hours_community_round,
    t1_censored,
    ends_with("_z")
  ) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))

naniar::vis_miss(df_clean, warn_large_data = FALSE)

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


#### SET VARIABLE NAMES
#  model
A <- c("t0_hours_community_round", "t1_hours_community_round")
C <- c("t0_censored", "t1_censored")

#L <- list(c("L1"), c("L2"))
W <-  c(paste(names_base, collapse = ", "))
names_base
W
# check
print(W)

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
#   ifelse(trt == "t1_religion_religious",
#          mtp_one_contrast_zero(data, trt),
#          data[[trt]])
# }


gain_A <- function(data, trt)  data[[trt]] + 1


zero_A <- NULL

# BONUS: progressr progress bars!
progressr::handlers(global = TRUE)

library(future)
plan(multisession)
n_cores <-
  parallel::detectCores()


# church: charity models ----------------------------------------------------------

# Checks
A
C


library("ranger")


# test data 
df_clean_slice <- df_clean |>
  slice_head(n = 500) |>
  as.data.frame()
colnames(df_clean_slice)

library(SuperLearner)
library(xgboost)
library(ranger)
library(biglasso)
# model charitable giving in population
# measure time taken to run the model
# 
# names_base_t2_hours_charity_z <-
#   select_and_rename_cols(names_base = use_names,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_hours_charity_z")
# 
# names_base_t2_hours_charity_z
m_hours_charity_z_test

# sl_lib <- c("SL.glmnet",
#             "SL.ranger", #
#             "SL.xgboost") #
library(ranger)


t2_warm_asians_z_test_gain <- lmtp_tmle(
  outcome = "t2_warm_asians_z",
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

t2_warm_asians_z_test_gain


t2_warm_asians_z_test_zero <- lmtp_tmle(
  outcome = "t2_warm_asians_z",
  baseline = names_base,
  shift = zero_A,
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

# warm asians -------------------------------------------------------------




t2_warm_asians_z_gain <- lmtp_tmle(
  outcome = "t2_warm_asians_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_asians_z_gain, "t2_warm_asians_z_gain")





t2_warm_asians_z_zero <- lmtp_tmle(
  outcome = "t2_warm_asians_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_asians_z_gain, "t2_warm_asians_z_gain")



# warm_chinese ------------------------------------------------------------
t2_warm_chinese_z_gain <- lmtp_tmle(
  outcome = "t2_warm_elderly_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_chinese_z_gain, "t2_warm_chinese_z_gain")


t2_warm_chinese_z_zero <- lmtp_tmle(
  outcome = "t2_warm_chinese_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_chinese_z_zero, "t2_warm_chinese_z_zero")


# t2_warm_immigrants_z ---------------------------------------------------------

t2_warm_immigrants_z_gain <- lmtp_tmle(
  outcome = "t2_warm_immigrants_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_immigrants_z_gain, "t2_warm_immigrants_z_gain")


t2_warm_immigrants_z_zero <- lmtp_tmle(
  outcome = "t2_warm_immigrants_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_immigrants_z_zero, "t2_warm_immigrants_z_zero")


# t2_warm_indians_z ------------------------------------------------------------

t2_warm_indians_z_gain <- lmtp_tmle(
  outcome = "t2_warm_indians_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_indians_z_gain, "t2_warm_indians_z_gain")


t2_warm_indians_z_zero <- lmtp_tmle(
  outcome = "t2_warm_indians_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_indians_z_zero, "t2_warm_indians_z_zero")




# t2_warm_elderly_z ------------------------------------------------------------

t2_warm_elderly_z_gain <- lmtp_tmle(
  outcome = "t2_warm_elderly_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_elderly_z_gain, "t2_warm_elderly_z_gain")


t2_warm_elderly_z_zero <- lmtp_tmle(
  outcome = "t2_warm_elderly_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_elderly_z_zero, "t2_warm_elderly_z_zero")


# t2_warm_maori_z --------------------------------------------------------------


t2_warm_maori_z_gain <- lmtp_tmle(
  outcome = "t2_warm_maori_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_maori_z_gain, "t2_warm_maori_z_gain")


t2_warm_maori_z_zero <- lmtp_tmle(
  outcome = "t2_warm_maori_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_maori_z_zero, "t2_warm_maori_z_zero")



# t2_warm_mental_illness_z -----------------------------------------------------


t2_warm_mental_illness_z_gain <- lmtp_tmle(
  outcome = "t2_warm_mental_illness_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_mental_illness_z_gain, "t2_warm_mental_illness_z_gain")


t2_warm_mental_illness_z_zero <- lmtp_tmle(
  outcome = "t2_warm_mental_illness_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_mental_illness_z_zero, "t2_warm_mental_illness_z_zero")



# t2_warm_muslims_z ------------------------------------------------------------


t2_warm_muslims_z_gain <- lmtp_tmle(
  outcome = "t2_warm_muslims_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_muslims_z_gain, "t2_warm_muslims_z_gain")


t2_warm_muslims_z_zero <- lmtp_tmle(
  outcome = "t2_warm_muslims_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_muslims_z_zero, "t2_warm_muslims_z_zero")


# t2_warm_nz_euro_z ------------------------------------------------------------



t2_warm_nz_euro_z_gain <- lmtp_tmle(
  outcome = "t2_warm_nz_euro_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_nz_euro_z_gain, "t2_warm_nz_euro_z_gain")


t2_warm_nz_euro_z_zero <- lmtp_tmle(
  outcome = "t2_warm_nz_euro_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_nz_euro_z_zero, "t2_warm_nz_euro_z_zero")



# t2_warm_overweight_z ---------------------------------------------------------


t2_warm_overweight_z_gain <- lmtp_tmle(
  outcome = "t2_warm_overweight_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_overweight_z_gain, "t2_warm_overweight_z_gain")


t2_warm_overweight_z_zero <- lmtp_tmle(
  outcome = "t2_warm_overweight_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_overweight_z_zero, "t2_warm_overweight_z_zero")



# t2_warm_pacific_z ------------------------------------------------------------


t2_warm_pacific_z_gain <- lmtp_tmle(
  outcome = "t2_warm_pacific_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_pacific_z_gain, "t2_warm_pacific_z_gain")


t2_warm_pacific_z_zero <- lmtp_tmle(
  outcome = "t2_warm_pacific_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_pacific_z_zero, "t2_warm_pacific_z_zero")




# t2_warm_refugees_z -----------------------------------------------------------

t2_warm_refugees_z_gain <- lmtp_tmle(
  outcome = "t2_warm_refugees_z",
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
  parallel = n_cores-1
)         
here_save(t2_warm_refugees_z_gain, "t2_warm_refugees_z_gain")


t2_warm_refugees_z_zero <- lmtp_tmle(
  outcome = "t2_warm_refugees_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_warm_refugees_z_zero, "t2_warm_refugees_z_zero")



# t2_perc_gend_discrim_z -------------------------------------------------------


t2_perc_gend_discrim_z_gain <- lmtp_tmle(
  outcome = "t2_perc_gend_discrim_z",
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
  parallel = n_cores-1
)         
here_save(t2_perc_gend_discrim_z_gain, "t2_perc_gend_discrim_z_gain")


t2_perc_gend_discrim_z_zero <- lmtp_tmle(
  outcome = "t2_perc_gend_discrim_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_perc_gend_discrim_z_zero, "t2_perc_gend_discrim_z_zero")


# t2_perc_discrim_z ------------------------------------------------------------


t2_perc_discrim_z_gain <- lmtp_tmle(
  outcome = "t2_perc_discrim_z",
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
  parallel = n_cores-1
)         
here_save(t2_perc_discrim_z_gain, "t2_perc_discrim_z_gain")


t2_perc_discrim_z_zero <- lmtp_tmle(
  outcome = "t2_perc_discrim_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_perc_discrim_z_zero, "t2_perc_discrim_z_zero")




# t2_perc_religious_discrim_z --------------------------------------------------




t2_perc_religious_discrim_z_gain <- lmtp_tmle(
  outcome = "t2_warm_chinese_z",
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
  parallel = n_cores-1
)         
here_save(t2_perc_religious_discrim_z_gain, "t2_perc_religious_discrim_z_gain")


t2_perc_religious_discrim_z_zero <- lmtp_tmle(
  outcome = "t2_perc_religious_discrim_z",
  baseline = names_base,
  shift = zero_A,
  data = df_clean,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger",
  parallel = n_cores-1
)         
here_save(t2_perc_religious_discrim_z_zero, "t2_perc_religious_discrim_z_zero")


# read models -------------------------------------------------------------


# asians ------------------------------------------------------------------


t2_warm_asians_z_gain<- read_here("t2_warm_asians_z_gain")
t2_warm_asians_z_zero<- read_here("t2_warm_asians_z_zero")

contrast_t2_warm_asians_z <-
  lmtp_contrast(t2_warm_asians_z_gain, ref =  t2_warm_asians_z_zero, type = "additive")

tab_contrast_t2_warm_asians_z <- margot_tab_lmtp(
  contrast_t2_warm_asians_z,
  scale = "RD",
  new_name = "community socialising: warm asians"
)

output_tab_contrast_t2_warm_asians_z <- lmtp_evalue_tab(tab_contrast_t2_warm_asians_z,  delta = 1, sd = 1, scale = c("RD"))


# chinese -----------------------------------------------------------------

t2_warm_chinese_z_gain<- read_here("t2_warm_chinese_z_gain")
t2_warm_chinese_z_zero<- read_here("t2_warm_chinese_z_zero")


contrast_t2_warm_chinese_z <-
  lmtp_contrast(t2_warm_chinese_z_gain, ref =  t2_warm_chinese_z_zero, type = "additive")

tab_contrast_t2_warm_chinese_z <- margot_tab_lmtp(
  contrast_t2_warm_chinese_z,
  scale = "RD",
  new_name = "community socialising: warm chinese"
)

output_contrast_t2_warm_chinese_z <- lmtp_evalue_tab(tab_contrast_t2_warm_chinese_z,  delta = 1, sd = 1, scale = c("RD"))


# immigrants --------------------------------------------------------------


t2_warm_immigrants_z_gain<- read_here("t2_warm_immigrants_z_gain")
t2_warm_immigrants_z_zero<- read_here("t2_warm_immigrants_z_zero")


contrast_t2_warm_immigrants_z <-
  lmtp_contrast(t2_warm_immigrants_z_gain, ref =  t2_warm_immigrants_z_zero, type = "additive")

tab_contrast_t2_warm_immigrants_z <- margot_tab_lmtp(
  contrast_t2_warm_immigrants_z,
  scale = "RD",
  new_name = "community socialising: warm immigrants"
)

output_tab_contrast_t2_warm_immigrants_z <- lmtp_evalue_tab(tab_contrast_t2_warm_immigrants_z,  delta = 1, sd = 1, scale = c("RD"))

# indians -----------------------------------------------------------------


t2_warm_indians_z_gain<- read_here("t2_warm_indians_z_gain")
t2_warm_indians_z_zero<- read_here("t2_warm_indians_z_zero")


contrast_t2_warm_indians_z <-
  lmtp_contrast(t2_warm_indians_z_gain, ref =  t2_warm_indians_z_zero, type = "additive")

tab_contrast_t2_warm_indians_z<- margot_tab_lmtp(
  contrast_t2_warm_indians_z,
  scale = "RD",
  new_name = "community socialising: warm indians"
)

output_tab_contrast_t2_warm_indians_z <- lmtp_evalue_tab(tab_contrast_t2_warm_indians_z,  delta = 1, sd = 1, scale = c("RD"))
# elderly -----------------------------------------------------------------

t2_warm_elderly_z_gain<- read_here("t2_warm_elderly_z_gain")
t2_warm_elderly_z_zero<- read_here("t2_warm_elderly_z_zero")


contrast_t2_warm_elderly_z <-
  lmtp_contrast(t2_warm_elderly_z_gain, ref =  t2_warm_elderly_z_zero, type = "additive")

tab_contrast_t2_warm_elderly_z <- margot_tab_lmtp(
  contrast_t2_warm_elderly_z,
  scale = "RD",
  new_name = "community socialising: warm elderly"
)

output_tab_contrast_t2_warm_elderly_z <- lmtp_evalue_tab(tab_contrast_t2_warm_elderly_z,  delta = 1, sd = 1, scale = c("RD"))
# maori -------------------------------------------------------------------

t2_warm_maori_z_gain<- read_here("t2_warm_maori_z_gain")
t2_warm_maori_z_zero<- read_here("t2_warm_maori_z_zero")

contrast_t2_warm_maori_z <-
  lmtp_contrast(t2_warm_maori_z_gain, ref =  t2_warm_maori_z_zero, type = "additive")

tab_contrast_t2_warm_maori_z <- margot_tab_lmtp(
  contrast_t2_warm_maori_z,
  scale = "RD",
  new_name = "community socialising: warm maori"
)

output_tab_contrast_t2_warm_maori_z <- lmtp_evalue_tab(tab_contrast_t2_warm_maori_z,  delta = 1, sd = 1, scale = c("RD"))

# mentally ill ------------------------------------------------------------

t2_warm_mental_illness_z_gain<- read_here("t2_warm_mental_illness_z_gain")
t2_warm_mental_illness_z_zero<- read_here("t2_warm_mental_illness_z_zero")

contrast_t2_warm_mental_illness_z <-
  lmtp_contrast(t2_warm_mental_illness_z_gain, ref =  t2_warm_mental_illness_z_zero, type = "additive")

tab_contrast_t2_warm_mental_illness_z <- margot_tab_lmtp(
  contrast_t2_warm_mental_illness_z,
  scale = "RD",
  new_name = "community socialising: warm mental illness"
)

output_tab_contrast_t2_warm_mental_illness_z <- lmtp_evalue_tab( tab_contrast_t2_warm_mental_illness_z,  delta = 1, sd = 1, scale = c("RD"))

# muslims -----------------------------------------------------------------
t2_warm_muslims_z_gain<- read_here("t2_warm_muslims_z_gain")
t2_warm_muslims_z_zero<- read_here("t2_warm_muslims_z_zero")


contrast_t2_warm_muslims_z <-
  lmtp_contrast(t2_warm_muslims_z_gain, ref =  t2_warm_muslims_z_zero, type = "additive")

tab_contrast_t2_warm_muslims_z <- margot_tab_lmtp(
  contrast_t2_warm_muslims_z,
  scale = "RD",
  new_name = "community socialising: warm muslims"
)

output_tab_contrast_t2_warm_muslims_z <- lmtp_evalue_tab(tab_contrast_t2_warm_muslims_z ,  delta = 1, sd = 1, scale = c("RD"))
# nzeuro ------------------------------------------------------------------

t2_warm_nz_euro_z_gain<- read_here("t2_warm_nz_euro_z_gain")
t2_warm_nz_euro_z_zero<- read_here("t2_warm_nz_euro_z_zero")

contrast_t2_warm_nz_euro_z <-
  lmtp_contrast(t2_warm_nz_euro_z_gain, ref =  t2_warm_nz_euro_z_zero, type = "additive")

tab_contrast_t2_warm_nz_euro_z <- margot_tab_lmtp(
  contrast_t2_warm_nz_euro_z,
  scale = "RD",
  new_name = "community socialising: warm nz euro"
)

output_tab_contrast_t2_warm_nz_euro_z <- lmtp_evalue_tab(tab_contrast_t2_warm_nz_euro_z,  delta = 1, sd = 1, scale = c("RD"))
# overweight --------------------------------------------------------------


t2_warm_overweight_z_gain<- read_here("t2_warm_overweight_z_gain")
t2_warm_overweight_z_zero<- read_here("t2_warm_overweight_z_zero")


contrast_t2_warm_overweight_z <-
  lmtp_contrast(t2_warm_overweight_z_gain, ref = t2_warm_overweight_z_zero, type = "additive")

tab_contrast_t2_warm_overweight_z <- margot_tab_lmtp(
  contrast_t2_warm_overweight_z,
  scale = "RD",
  new_name = "community socialising: warm overweight"
)

output_tab_contrast_t2_warm_overweight_z <- lmtp_evalue_tab(tab_contrast_t2_warm_overweight_z,  delta = 1, sd = 1, scale = c("RD"))
# pacific -----------------------------------------------------------------


t2_warm_pacific_z_gain<- read_here("t2_warm_pacific_z_gain")
t2_warm_pacific_z_zero<- read_here("t2_warm_pacific_z_zero")

contrast_t2_warm_pacific_z <-
  lmtp_contrast(t2_warm_pacific_z_gain, ref =  t2_warm_pacific_z_zero, type = "additive")

tab_contrast_t2_warm_pacific_z <- margot_tab_lmtp(
  contrast_t2_warm_pacific_z,
  scale = "RD",
  new_name = "community socialising: warm pacific"
)

output_tab_contrast_t2_warm_pacific_z <- lmtp_evalue_tab(tab_contrast_t2_warm_pacific_z,  delta = 1, sd = 1, scale = c("RD"))
# refugees ----------------------------------------------------------------


t2_warm_refugees_z_gain<- read_here("t2_warm_refugees_z_gain")
t2_warm_refugees_z_zero<- read_here("t2_warm_refugees_z_zero")


contrast_t2_warm_refugees_z <-
  lmtp_contrast(t2_warm_refugees_z_gain, ref =  t2_warm_refugees_z_zero, type = "additive")

tab_contrast_t2_warm_refugees_z <- margot_tab_lmtp(
  contrast_t2_warm_refugees_z,
  scale = "RD",
  new_name = "community socialising: warm refugees"
)

output_tab_contrast_t2_warm_refugees_z <- lmtp_evalue_tab( tab_contrast_t2_warm_refugees_z,  delta = 1, sd = 1, scale = c("RD"))
# perceive gender discrim -------------------------------------------------
t2_perc_gend_discrim_z_gain<- read_here("t2_perc_gend_discrim_z_gain")
t2_perc_gend_discrim_z_zero<- read_here("t2_perc_gend_discrim_z_zero")

contrast_t2_perc_gend_discrim_z <-
  lmtp_contrast(t2_perc_gend_discrim_z_gain, ref =  t2_perc_gend_discrim_z_zero, type = "additive")

tab_contrast_t2_perc_gend_discrim_z <- margot_tab_lmtp(
  contrast_t2_perc_gend_discrim_z,
  scale = "RD",
  new_name = "community socialising: perceive gender discrim"
)

output_tab_contrast_t2_perc_gend_discrim_z <- lmtp_evalue_tab(tab_contrast_t2_perc_gend_discrim_z ,  delta = 1, sd = 1, scale = c("RD"))
# perceive rel discrim ----------------------------------------------------
t2_perc_religious_discrim_z_gain<- read_here("t2_perc_religious_discrim_z_gain")
t2_perc_religious_discrim_z_zero<- read_here("t2_perc_religious_discrim_z_zero")

contrast_t2_perc_religious_discrim_z <-
  lmtp_contrast(t2_perc_religious_discrim_z_gain, ref =  t2_perc_religious_discrim_z_zero, type = "additive")

tab_contrast_t2_perc_religious_discrim_z <- margot_tab_lmtp(
  contrast_t2_perc_religious_discrim_z,
  scale = "RD",
  new_name = "community socialising: perceive religious discrim"
)

output_tab_contrast_t2_perc_religious_discrim_z <- lmtp_evalue_tab( tab_contrast_t2_perc_religious_discrim_z,  delta = 1, sd = 1, scale = c("RD"))

# perceive ethnic discrim -------------------------------------------------

t2_perc_discrim_z_gain<- read_here("t2_perc_discrim_z_gain")
t2_perc_discrim_z_zero<- read_here("t2_perc_discrim_z_zero")



contrast_t2_perc_discrim_z <-
  lmtp_contrast(t2_perc_discrim_z_gain, ref =  t2_perc_discrim_z_zero, type = "additive")

tab_contrast_t2_perc_discrim_z <- margot_tab_lmtp(
  contrast_t2_perc_discrim_z,
  scale = "RD",
  new_name = "community socialising: perceive ethnic discrim"
)

output_tab_contrast_t2_perc_discrim_z <- lmtp_evalue_tab( tab_contrast_t2_perc_discrim_z,  delta = 1, sd = 1, scale = c("RD"))



# tables and graphs -------------------------------------------------------


tab_all_warm <- rbind(
  output_tab_contrast_t2_warm_asians_z,
  output_contrast_t2_warm_chinese_z,
  output_tab_contrast_t2_warm_immigrants_z,
  output_tab_contrast_t2_warm_indians_z,
  output_tab_contrast_t2_warm_elderly_z,
  output_tab_contrast_t2_warm_maori_z,
  output_tab_contrast_t2_warm_mental_illness_z,
  output_tab_contrast_t2_warm_muslims_z,
  output_tab_contrast_t2_warm_nz_euro_z,
  output_tab_contrast_t2_warm_overweight_z,
  output_tab_contrast_t2_warm_pacific_z,
  output_tab_contrast_t2_warm_refugees_z)

group_tab_all_warm <- group_tab(tab_all_warm)

here_save(tab_all_warm, "tab_all_warm")
here_save(group_tab_all_warm, "group_tab_all_warm")




tab_all_perceive <-
  rbind(
    output_tab_contrast_t2_perc_gend_discrim_z,
    output_tab_contrast_t2_perc_religious_discrim_z,
    output_tab_contrast_t2_perc_discrim_z
  )


group_tab_all_perceive <- group_tab(tab_all_perceive)

here_save(tab_all_perceive , "tab_all_perceive")
here_save(group_tab_all_perceive, "group_tab_all_perceive")


# graphs ------------------------------------------------------------------



plot_group_tab_all_warm <- margot_plot(
  group_tab_all_warm,
  type = "RD",
  title =  "Community time effect warmth to groups",
  subtitle = "Contrast: weekly vs. none ",
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

plot_group_tab_all_warm

here_save(plot_group_tab_all_warm, 'plot_group_tab_all_warm')





plot_group_tab_all_perceive <- margot_plot(
  group_tab_all_perceive,
  type = "RD",
  title = "Community time effect on perceived prejudice against self",
  subtitle = "Contrast: weekly vs. none ",
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

plot_group_tab_all_perceive

here_save(plot_group_tab_all_perceive, 'plot_group_tab_all_perceive')





# read material -----------------------------------------------------------


tab_all_warm <- here_read("tab_all_warm")
group_tab_all_warm<- here_read("group_tab_all_warm")

tab_all_warm<- here_read("tab_all_warm")
group_tab_all_warm<- here_read("group_tab_all_warm")


plot_group_tab_all_perceive <- here_read(plot_group_tab_all_perceive, "plot_group_tab_all_perceive")
plot_group_tab_all_warm<- here_read(plot_group_tab_all_warm, "plot_group_tab_all_warm")


transition_table <- here_read("transition_table")
transition_table_out_binary <-
  here_read("transition_table_out_binary")

n_participants <- here_read("n_participants")

table_baseline <- here_save("table_baseline")
table_exposures <- here_save("table_exposures")
table_outcomes <- here_save("table_outcomes")


graph_density_of_exposure_down<- here_read("graph_density_of_exposure_down")
graph_density_of_exposure_up<- here_read("graph_density_of_exposure_up")


