## Bella work hour 27 June 2024

# reproducibility
set.seed(123)

# set path to save models
push_mods <-
  here::here('/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/24-bella-work')

# load packages
# get devtools
if (!require(devtools, quietly = TRUE)) {
      install.packages("devtools")
  library(devtools)
}

# get 'margot' from my github (make sure to update)
devtools::install_github("go-bayes/margot")


# Check if pacman is installed; if not, install it
if (!require(pacman, quietly = TRUE)) {
   install.packages("pacman")
  library(pacman)
}

library(margot)

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
  margot,
  xgboost,
  glmnet,
  doParallel,
  ggplot2,
  here,
  naniar,
  gtsummary,
  grf,
  gt,
  progressr,
  tidyverse,
  ggplot2,
  parameters,
  kableExtra
)

# correct sample #  w_gend_age_euro

# wrangling ---------------------------------------------------------------
pull_path <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data_qs"
  )

# read data: note that you need use the arrow package in R
dat <- qs::qread(here::here(pull_path))

# total nzavs participants
n_total <- skimr::n_unique(dat$id)

# get comma in number
n_total <- margot::pretty_number(n_total)

# check
n_total


# save for manuscript
margot::here_save(n_total, "n_total")

# eliminate haven labels
dat <- as.data.frame(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_widths(dat)


# name of exposure
name_exposure <-  "hours_work"

# get names
colnames(dat)

str(dat)
# check missing values so that you can figure out which to select.
# skimr::skim(dat) |>
#   arrange(n_missing)



# descriptive checks ---------------------------------------------
# check n by wave
df <- dat |>
  filter(year_measured == 1) |>
  group_by(wave) |> 
  summarize(number = n_distinct(id))
print(df)



# create a pretty table using gt
df |>
  gt() |>
  tab_header(title = "Unique IDs by Wave", subtitle = "Year given starts in October and runs to October the following year") |>
  fmt_number(columns = vars(number), decimals = 0) |>
  cols_label(wave = "Wave", number = "Number of Unique IDs")

# id counts
id_wave_counts <- dat |>
  filter(year_measured == 1) |>
  group_by(id) |>
  summarize(wave_count = n_distinct(wave)) |>
  ungroup()

# Summarize the number of participants by the number of waves they participated in
wave_summary <- id_wave_counts |>
  group_by(wave_count) |>
  summarize(number_of_participants = n())

# Print the summary table
print(wave_summary)

wave_summary |>
  gt() |>
  tab_header(title = "Participant Wave Summary", subtitle = "Number of participants by the number of waves they participated in") |>
  cols_label(wave_count = "Number of Waves", number_of_participants = "Number of Participants") |>
  fmt_number(columns = vars(number_of_participants),
             decimals = 0) |>
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12
  )

#Count the number of waves each person participated in
id_wave_counts <- dat |>
  filter(year_measured == 1) |>
  group_by(id) |>
  summarize(wave_count = n_distinct(wave)) |>
  ungroup()

# Summarize the number of participants by the number of waves they participated in
wave_summary <- id_wave_counts |>
  mutate(wave_count = if_else(wave_count >= 3, "3 or more", as.character(wave_count))) |>
  group_by(wave_count) |>
  summarize(number_of_participants = n()) |>
  ungroup()

# Create a pretty table using gt
wave_summary |>
  gt() |>
  tab_header(title = "Participant Wave Summary", subtitle = "Number of participants by the number of waves they participated in") |>
  cols_label(wave_count = "Number of Waves", number_of_participants = "Number of Participants") |>
  fmt_number(columns = vars(number_of_participants),
             decimals = 0) |>
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12
  )

# define data ------------------------------------------------------------
baseline_wave <- "2020"
exposure_wave <- "2021"
outcome_wave <- "2022"

# obtain ids for individuals who participated in 2018 and have no missing baseline exposure
ids_baseline <- dat |>
  dplyr::filter(year_measured == 1, wave == baseline_wave) |>
  dplyr::filter(!is.na(!!sym(name_exposure))) |> # criteria, no missing in baseline exposure
  pull(id)


# if you decide to include the exposure in the treatment wave,
# if you decide to include the exposure in the treatment wave,
# obtain ids for individuals who participated in 2019
# ids_2019 <- df_nz |>
#   dplyr::filter(year_measured == 1, wave == 2019) |>
#   dplyr::filter(!is.na(!!sym(name_exposure))) |> # criteria, no missing
#   pull(id)

# intersect IDs from 2018 and 2019 to ensure participation in both years
# ids_2018_2019 <- intersect(ids_2018, ids_2019)
dat$w_gend_age_euro
table1::table1( ~ w_sample_weight| wave, data = dat)




# checks -----------------------------------------------------------------
df_all_in <- dat |>
  filter(year_measured == 1)

# table
table1::table1(~ employed |wave, data = df_all_in)




# prepare data -----------------------------------------------------------

dat_long_0 <- dat |>
  # dplyr::filter(id %in% ids_2018_2019 &
  #                 wave %in% c(2018, 2019, 2020)) |>
  dplyr::filter(id %in% ids_baseline &
                  wave %in% c(baseline_wave, exposure_wave, outcome_wave)) |>
  arrange(id, wave) |>
  dplyr::select(
    "id",
    "wave",
    "age",
    "male",
    "w_gend_age_euro",
    "year_measured",
    "sample_frame_opt_in",
    "hlth_disability",
    "religion_identification_level",
    "religion_believe_spirit",
    "religion_believe_god",   
    "born_nz",
    "male",
    "eth_cat",
    #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    "employed",
    "sample_origin_names_combined",
    "education_level_coarsen",
    "household_inc",
    "nz_dep2018",
    "nzsei_13_l",
    "partner",
    "parent",
    "political_conservative",
    "hours_children",
    "hours_work",
    "hours_housework",
    "charity_donate",
    "hours_charity",
    "hours_exercise",
    "agreeableness",
    "conscientiousness",
    "extraversion",
    "honesty_humility",
    "openness",
    "neuroticism",
    # "modesty",
    "rural_gch_2018_l",
    "charity_donate",
    #How much money have you donated to charity in the last year?
    "hours_charity",
    "alcohol_frequency",
    #"How often do you have a drink containing alcohol?"
    "alcohol_intensity",
    # How many drinks containing alcohol do you have on a typical day when drinking?
    "hlth_bmi",
    "smoker",
    #Do you currently smoke?
    "hlth_fatigue",
    #During the last 30 days, how often did.... you feel exhausted?
    "rumination",
    # "kessler6_sum",
    "kessler_latent_depression",
    "kessler_latent_anxiety",
    "hlth_sleep_hours",
    # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
    "gratitude",
    ## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people
    "alcohol_frequency",
    #"How often do you have a drink containing alcohol?"
    "alcohol_intensity",
    # How many drinks containing alcohol do you have on a typical day when drinking?
    "hlth_bmi",
    # " What is your height? (metres)\nWhat is your weight? (kg)\nKg
    "hours_exercise",
    # Hours spent … exercising/physical activity
    "sfhealth",
    "hlth_sleep_hours",
    "smoker",
    "hlth_fatigue",
    "rumination",
    "kessler6_sum",
    "kessler_latent_depression",
    "kessler_latent_anxiety",
    "sexual_satisfaction",
    "bodysat",
    ## Am satisfied with the appearance, size and shape of my body.
    "vengeful_rumin",
    # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
    "perfectionism",
    #"power_no_control_composite",
    "self_esteem",
    "self_control_have_lots",
    #In general, I have a lot of self-control.
    "self_control_wish_more_reversed",
   # "emotion_regulation_out_control",
    "gratitude",
    "pwb_your_health",
    # #Your health.
    "pwb_your_relationships",
    # #Your personal relationships.
    "pwb_your_future_security",
    # #Your future security.
    "pwb_standard_living",
    #Your standard of living.
    "lifesat",
    "meaning_purpose",
    "meaning_sense",
    "neighbourhood_community",
    "support",
    "belong"
  )


dat_long <- dat_long_0|>
  rename(short_form_health = sfhealth) |> # better name
  mutate(male = as.numeric(male)) |>
  mutate(
#    religion_church_binary = ifelse(religion_church > 0, 1, 0), # not in these data :(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    not_lost = ifelse(is.na(not_lost) &
                        year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
  ) |>
  dplyr::rename(sample_weights = w_gend_age_euro) |>
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
  arrange(id, wave) |>
 # mutate(religion_church_round = round(ifelse(religion_church >= 8, 8, religion_church), 0)) |>
#  mutate(hours_community_round = round(ifelse(hours_community >= 24, 24, hours_community), 0)) |>
  mutate(
    #initialize 'not_lost'
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    # modify 'not_lost' based on the condition; no need to check for NA here as 'not_lost' is already defined in the previous step
    not_lost =  ifelse(is.na(not_lost) &
                         year_measured == 1, 1, not_lost)
  ) |>
  select(-year_measured) |>
  ungroup() |>
  dplyr::mutate(
    #  friends_money = ifelse(friends_money < 0, 0, friends_money),
    # someone gave neg number
    # hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1),
    household_inc_log = log(household_inc + 1),
    hours_charity_log = log(hours_charity + 1),
    hours_exercise_log = log(hours_exercise + 1),
    hours_children_log = log(hours_children + 1),
    charity_donate_log = log(charity_donate + 1)
  ) |>
  dplyr::select(
    -c(
    #  religion_church,
      #  hours_work,
      charity_donate,
      hours_housework,
      household_inc,
   #  hours_exercise,
     # hours_community,
      hours_children,
      hours_charity,
      charity_donate
    )
  ) |>
  droplevels() |>
  dplyr::rename(sample_origin =  sample_origin_names_combined) |>
  dplyr::mutate(
  #  rural_gch_2018_l = as.numeric(as.character(rural_gch_2018_l)),
  #  has_siblings = as.numeric(as.character(has_siblings)),
    parent = as.numeric(as.character(parent)),
    partner = as.numeric(as.character(partner)),
    born_nz = as.numeric(as.character(born_nz)),
    not_lost = as.numeric(as.character(not_lost)),
    employed = as.numeric(as.character(employed)),
    hlth_disability = as.numeric(as.character(hlth_disability))
  ) |>
  droplevels() |>
  arrange(id, wave) |>
  mutate(
    #rural_gch_2018_l = as.numeric(as.character(rural_gch_2018_l)),
    parent = as.numeric(as.character(parent)),
    partner = as.numeric(as.character(partner)),
    born_nz = as.numeric(as.character(born_nz)),
    not_lost = as.numeric(as.character(not_lost)),
    employed = as.numeric(as.character(employed))
  ) |>
  arrange(id, wave) |>
  data.frame()|>
  droplevels()


head(dat_long)
str(dat_long$rural_gch_2018_l)

n_participants <- skimr::n_unique(dat_long$id)

# get comma in number
n_participants <- margot::pretty_number(n_participants)

# check
n_participants

# save number for manuscript
margot::here_save(n_participants, "n_participants")

# inspect data
skimr::skim(dat_long)

# check names
sort(colnames(dat_long))

# set baseline variables --------------------------------------------------
# for confounding control

baseline_vars = c(
  "male",
  "age",
  "sample_frame_opt_in",
  "education_level_coarsen",
  # factors
  "eth_cat",
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  #"bigger_doms", #religious denomination
  "sample_origin",
  "nz_dep2018",
  "nzsei_13_l",
  "rural_gch_2018_l",
  "born_nz",
  "hlth_disability",
  "short_form_health",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  "support",
  "belong",
  "household_inc_log",
  # added: measured with error but OK for imputations
  "partner",
  "parent", 
  "political_conservative",
  "hours_children_log",
  # new
 # "hours_work_log",
  # new
  "religion_believe_god",
  "religion_believe_spirit",
  "religion_identification_level",
  "hours_housework_log",
  #new
  "hours_exercise_log",
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "honesty_humility",
  "openness",
  "neuroticism",
 # "modesty", # phasing out 
  "sample_weights"
  #"alert_level_combined_lead"
)


# treatment
exposure_var = c(name_exposure, "not_lost") # we will use the not_lost variable later
exposure_var


# outcome, can be many

# outcomes
outcome_vars = c(
  "alcohol_frequency",
  "alcohol_intensity",
  "hlth_bmi",
  "hours_exercise",
  "short_form_health",
  "hlth_sleep_hours",
  "smoker",
  "hlth_fatigue",
  "rumination",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  "bodysat",
  "perfectionism",
  #"power_no_control_composite",
  "self_esteem",
  "sexual_satisfaction",
  "self_control_have_lots",
  "self_control_wish_more_reversed",
 # "emotion_regulation_out_control",
  "vengeful_rumin",
  "gratitude",
  "short_form_health",
  "pwb_your_relationships",
  "pwb_your_future_security",
  "pwb_standard_living",
  "lifesat",
  "meaning_purpose",# My life has a clear sense of purpose.
  "meaning_sense", # I have a good sense of what makes my life meaningful.
 # "permeability_individual",
  "neighbourhood_community",
  "belong",
  "support"
)






# 
# # sample weights balanced male / female-------------------------------------# balance on gender weights
# # calculate gender weights assuming male is coded as 1 and female as 0
# prop_male_population <-
#   0.5  # target proportion of males in the population
# prop_female_population <-
#   0.5  # target proportion of females in the population
# 
# prop_male_sample <- mean(dat_long$male)
# prop_female_sample <- 1 - prop_male_sample
# 
# gender_weight_male <- prop_male_population / prop_male_sample
# gender_weight_female <- prop_female_population / prop_female_sample
# 
# dat_long$sample_weights <-
#   ifelse(dat_long$male == 1, gender_weight_male, gender_weight_female)
# 
# # we will upweight males and down weight non-males to obtain a balance of gender in the *target* population
# table(round(dat_long$sample_weights, 3))


# make your tables --------------------------------------------------------
dt_baseline <- dat_long |>
  filter(wave == baseline_wave)

# variables for the table
base_vars <- setdiff(baseline_vars, c("not_lost", "sample_weights", outcome_vars))

colnames(dt_baseline)
# get baseline cols
selected_base_cols <-
  dt_baseline |> select(all_of(base_vars))

# missing values visualisation
viss_miss_baseline <- naniar::vis_miss(dt_baseline, warn_large_data = F)
here_save(viss_miss_baseline, "base_var")


# check missing values a baseline
viss_miss_baseline


# the setdiff command allows us to remove names from the baseline vars list that we do not
table_baseline <- selected_base_cols |>
  janitor::clean_names(case = "title") |>
  tbl_summary(
    missing = "ifany",
    percent = "column",
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{min}, {max}", "{p25}, {p75}")
    ),
    type = list(all_continuous() ~ "continuous2")
  ) |>
  modify_header(label = "**Exposure + Demographic Variables**") |>
  bold_labels()

# baseline
table_baseline

# save your baseline table
margot::here_save(table_baseline, "table_baseline")

# exposure table ----------------------------------------------------------
# get first and second wave
dt_baseline_exposure <- dat_long |>
  dplyr::filter(wave == baseline_wave | wave == outcome_wave) |>
  droplevels()


# get vars.
name_exposure # check 

# select exposure 
selected_exposure_cols <-
  dt_baseline_exposure |> select(c(name_exposure, "wave"))

# check
#str(selected_exposure_cols)
table_exposures <- selected_exposure_cols |>
  janitor::clean_names(case = "title") |>
  labelled::to_factor() |>
  tbl_summary(by = "Wave",
              # Specify the grouping variable. Adjust "Wave" to match the cleaned column name.
              # Uncomment the following line and adjust if you have continuous variables to summarize
              # statistic = list(all_continuous() ~ "{mean} ({sd})"),
              # add_n(),  # Add a column with total number of non-missing observations (uncomment if needed))
              missing = "always",
              percent = "column") |>
  modify_header(label = "**Exposure Variables by Wave**") |>
  bold_labels()

# save baseline
here_save(table_exposures, "table_exposures")

# check
table_exposures
hist( selected_exposure_cols$hours_work) 

# outcome table -----------------------------------------------------------
dt_baseline_outcome <- dat_long |>
  dplyr::filter(wave == 2021 | wave == 2022) |>
  droplevels()

names_outcomes_tab <- setdiff(outcome_vars, dt_baseline_outcome)
names_outcomes_sorted <- sort(names_outcomes_tab)
names_outcomes_final <- names_outcomes_sorted # consistent workflow

# better names if desirable
selected_outcome_cols <-
  dt_baseline_outcome |> select(all_of(names_outcomes_final), wave)
# |> # example if you want to rename your variables for the table
#   rename(
#     Social_belonging = belong,
#     Annual_charity = charity_donate,
#     Volunteering_hours = hours_charity,
#     Community_gives_money_binary = community_money_binary,
#     Community_gives_time_binary = community_time_binary,
#     Family_gives_money_binary = family_money_binary,
#     Family_gives_time_binary = family_time_binary,
#     Friends_give_money_binary = friends_money_binary,
#     Friends_give_time = friends_time_binary,
#     Social_support = support,
#     Sense_neighbourhood_community = neighbourhood_community
#   )



# order names correctly
selected_outcome_cols <- selected_outcome_cols |>
  select(sort(names(selected_outcome_cols)))

# checks
# str(selected_outcome_cols)
# colnames(selected_outcome_cols)

table_outcomes <- selected_outcome_cols |>
  janitor::clean_names(case = "title") |>
  labelled::to_factor() |>  # ensure consistent use of pipe operator
  tbl_summary(by = "Wave",
              #specify the grouping variable. Adjust "Wave" to match the cleaned column name
              # statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables) |>
              #  add_n()
              missing = "always",
              percent = "column") |>  # Add column with total number of non-missing observations
  modify_header(label = "**Outcome Variables by Wave**") |>  # Update the column header
  bold_labels()

str(dat_long$rural_gch_2018_l) 
head(dat_long)

# save
margot::here_save(table_outcomes, "table_outcomes")

# read if needed
table_outcomes <- margot::here_read("table_outcomes")
table_outcomes

# histogram of the exposure -----------------------------------------------
# select exposure wave


dt_exposure_wave  <- dat_long |> dplyr::filter(wave == exposure_wave)

# mean of exposure
mean_exposure <- mean(dt_exposure_wave[[name_exposure]], na.rm = TRUE)

# view
mean_exposure

# save
here_save(mean_exposure, "mean_exposure")

# sd of exposure
sd_exposure <- sd(dt_exposure_wave[[name_exposure]], na.rm = TRUE)

# save
here_save(sd_exposure, "sd_exposure")

# check
sd_exposure

# median
median_exposure <- median(dt_exposure_wave[[name_exposure]], na.rm = TRUE)

median_exposure

# check if you like
# median_exposure
# mean_exposure

# graph_density_shift_function <- margot::coloured_histogram(
#   dt_exposure,
#   col_name = name_exposure,
#   binwidth = .1,
#   unit_of_change = 1,
#   scale_min = 0,
#   scale_max = 100,
#   highlight_range = "hightest" # "lowest" if you want the lowest, "both" if both
# )
# graph_density_shift_function
# margot::here_save(graph_density_shift_function,
#                   "graph_density_shift_function")

# change in exposure ------------------------------------------------------
dt_positivity <- dat_long |>
  dplyr::filter(wave == baseline_wave |
                  wave == exposure_wave) |>
  mutate(working = ifelse(hours_work > 0, 1, 0 )) |> 
  dplyr::select(working, id, wave) |>
  droplevels()

out <- margot::create_transition_matrix(data = dt_positivity,
                                        state_var = "working",
                                        id_var = "id")


# t_tab_2_labels <- c("< weekly", ">= weekly")
# transition table
transition_table  <- margot::transition_table(out)


# for import later
margot::here_save(transition_table, "transition_table")

# view
print(transition_table$table)
print(transition_table$explanation)

# example of a cross sectional analysis -----------------------------------

# dt_baseline_regress <- dat_long |>
#   filter(wave == 2018) |>
#   mutate(
#     #perfectionism_z = scale(perfectionism),
#     kessler_latent_anxiety_z = scale(kessler_latent_anxiety),
#     kessler_latent_depression_z = scale(kessler_latent_depression)
#   )

# # check
# hist(dt_baseline_regress$kessler_latent_depression_z)
# # check
# hist(dt_baseline_regress$kessler_latent_anxiety_z)


# # code from above
# base_var <-
#   setdiff(baseline_vars, c("not_lost", "sample_weights", outcome_vars))

# # fit model
# fit_kessler_latent_anxiety <-
#   margot::regress_with_covariates(
#     dt_baseline_regress,
#     outcome = "kessler_latent_anxiety_z",
#     exposure = "perfectionism",
#     baseline_vars = base_var
#   )

# # view
# parameters::model_parameters(fit_kessler_latent_anxiety, ci_method = "wald")[2, ]

# # save results
# here_save(fit_kessler_latent_anxiety, "fit_kessler_latent_anxiety")

# # view
# fit_kessler_latent_depression <-
#   regress_with_covariates(
#     dt_baseline_regress,
#     outcome = "kessler_latent_depression_z",
#     exposure = "perfectionism",
#     baseline_vars = base_var
#   )
# # view model
# parameters::model_parameters(fit_kessler_latent_depression, ci_method =
#                                "wald")[2, ]

# # save results
# here_save(fit_kessler_latent_depression,
#           "fit_kessler_latent_depression")


# # calculate betas anxiety
# lm_fit_kessler_latent_anxiety <- tbl_regression(fit_kessler_latent_anxiety)
# here_save(lm_fit_kessler_latent_anxiety,
#           "lm_fit_kessler_latent_anxiety")

# # calculate betas depression
# lm_fit_kessler_latent_depression <- tbl_regression(fit_kessler_latent_depression)
# here_save(lm_fit_kessler_latent_depression,
#           "lm_fit_kessler_latent_depression")

# #
# #
# b_lm_fit_kessler_latent_anxiety <- inline_text(lm_fit_kessler_latent_anxiety,
#                                                variable = perfectionism,
#                                                pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")

# # view
# here_save(b_lm_fit_kessler_latent_anxiety,
#           "b_lm_fit_kessler_latent_anxiety")

# # #
# b_lm_fit_kessler_latent_depression <-
#   inline_text(lm_fit_kessler_latent_depression,
#               variable = perfectionism,
#               pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")

# here_save(b_lm_fit_kessler_latent_depression,
#           "b_lm_fit_kessler_latent_depression")


# impute missing values at baseline ---------------------------------------
exposure_vars <- c(exposure_var, "not_lost")
baseline_vars <- setdiff(baseline_vars, "sample_weights")

# check
baseline_vars


# prepare for imputation --------------------------------
dat_long <- margot::remove_numeric_attributes(dat_long)

# check
str(dat_long)

# here we impute the baseline
df_impute_base <- margot::margot_wide_impute_baseline(
  dat_long,
  baseline_vars = baseline_vars,
  exposure_var = exposure_vars,
  outcome_vars = outcome_vars
)



# get sample weights back to data
dt_baseline <- dat_long |> filter(wave == baseline_wave)

# add sample weights
df_impute_base$t0_sample_weights = dt_baseline$sample_weights

table( is.na(df_impute_base$t0_sample_weights) ) 

?as.factor()

df_impute_base$t0_rural_gch_2018_l <- as.factor( df_impute_base$t0_rural_gch_2018_l)

# save
margot::here_save(df_impute_base, "df_impute_base")


# data wrangling for censoring weights ------------------------------------
df_impute_base <- margot::here_read("df_impute_base")

# check data types
str(df_impute_base)

df_wide_not_lost <- df_impute_base |>
  relocate("t0_not_lost", .before = starts_with("t1_")) |>
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_"))


# save
here_save(df_wide_not_lost, "df_wide_not_lost")


# read if needed
df_wide_not_lost <- here_read("df_wide_not_lost")

# check missing values
naniar::vis_miss(df_wide_not_lost, warn_large_data = FALSE)

# people lost at wave 2
table(df_wide_not_lost$t0_not_lost)


# Assuming df_wide_not_lost is your dataframe

# Calculate the conditions before the mutate steps
t0_na_condition <- rowSums(is.na(select(df_wide_not_lost, starts_with("t1_")))) > 0


# used if you did not censor at exposure wave
#t1_na_condition <- rowSums(is.na(select(df_wide_not_lost, starts_with("t2_")))) > 0

df_clean <- df_wide_not_lost |>
  dplyr::mutate(t0_not_lost = ifelse(t0_na_condition, 0, t0_not_lost)) |>
  dplyr::mutate(t0_lost = 1 - t0_not_lost) |> 
  # mutate(t1_not_lost = ifelse(t1_na_condition, 0, t1_not_lost)) |>. # use if not_lost at t1
  # mutate(across(starts_with("t1_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
  #        across(starts_with("t2_"), ~ ifelse(t0_not_lost == 0, NA_real_, .))) |>
  # mutate(across(starts_with("t2_"), ~ ifelse(t1_not_lost == 0, NA_real_, .)))|>
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_not_lost &
        !t0_lost &
        !t0_smoker &
        !t0_sample_weights &
        !t0_hours_work & ### EXPOSURE
        !t0_sample_origin  &
        !t0_rural_gch_2018_l  &
        !t1_hours_work & ### EXPOSURE
        !t1_not_lost & 
        !t2_smoker,
      .fns = ~ scale(.),
      .names = "{.col}_z"
    )
  ) |>
  # select(-t0_charity_donate,
  #        -t0_hours_charity) |>
  select(
    where(is.factor),
    t0_not_lost,
    t0_lost,
    t0_smoker,
    t0_sample_weights,
    t0_sample_origin ,
    t0_rural_gch_2018_l  ,
    t0_hours_work,
    t0_not_lost,
    t1_hours_work, ### EXPOSURE
    t1_not_lost,
    t2_smoker,
    ends_with("_z")
  ) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_"))

# check what we have
colnames(df_clean) # note this  "t1_perfectionism"               "t1_perfectionism_z"

# get rid of attributes
df_clean <- margot::remove_numeric_attributes(df_clean)

str(df_clean)
# check again
naniar::vis_miss(df_clean, warn_large_data = FALSE)

# checks
table(df_clean$t0_not_lost)
table(df_clean$t1_not_lost)

# checks
table(df_clean$t0_not_lost)

# checks
test <- df_wide_not_lost |> filter(t0_not_lost == 1)
nrow(test) # 28024

#
# df_impute_base$t1_perfectionism_z = scale(df_impute_base$t1_perfectionism)


# checks
str(df_clean)

# checks
nrow(df_clean)




# weights for treatment ----------------------------------------------------
baseline_vars_models = df_clean |>  # post process of impute and combine
  dplyr::select(starts_with("t0_"), -t0_not_lost, -t0_sample_weights) |> colnames() # note

baseline_vars_models

# create fresh dataset
df_clean_pre <- df_clean[baseline_vars_models]

# checks
str(df_clean_pre)

# if this variable were not a factor, make sure it is
# df_clean_pre$t0_eth_cat <- as.factor(df_clean_pre$t0_eth_cat)


# perform one-hot encoding using model.matrix
# we need factors to be 0 or 1
encoded_vars <- model.matrix(~ t0_education_level_coarsen + t0_eth_cat + t0_sample_origin + t0_rural_gch_2018_l  - 1, data = df_clean_pre)
head(encoded_vars)


# convert matrix to data frame
encoded_df <- as.data.frame(encoded_vars)

# make better names
encoded_df <- encoded_df |>
  janitor::clean_names()

# View the first few rows to confirm structure
head(encoded_df)

# bind the new one-hot encoded variables back to the original dataframe

# ensure to remove original categorical variables to avoid duplication
df_clean_hot <- df_clean |>
  select(-c(t0_eth_cat, t0_education_level_coarsen, t0_sample_origin,t0_rural_gch_2018_l)) |>
  bind_cols(encoded_df)

# extract and print the new column names for encoded variables
new_encoded_colnames <- colnames(encoded_df)
print(new_encoded_colnames)


# get baseline variable set without factors
baseline_vars_set <- setdiff(names(df_clean_pre), 
c("t0_lost", 
"id", 
"t0_eth_cat",
"t0_education_level_coarsen", 
"t0_sample_origin",
"t0_rural_gch_2018_l"))

# check
baseline_vars_set

# add the new encoded column names
full_predictor_vars <- c(baseline_vars_set, new_encoded_colnames)

# check
full_predictor_vars

# check
str(df_clean_hot)

# set up super learner

library(SuperLearner)

# library for multicore processing
library(doParallel)

# learners
listWrappers()

# set up superlearner
cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification


# Set up parallel back end
no_cores <- detectCores()
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)


# you can probably just use "SL.glmnet"
match_lib = c("SL.glmnet", "SL.xgboost", "SL.ranger")

# run super learner
sl <- SuperLearner(
  Y = df_clean_hot$t0_lost,
  X = df_clean_hot[full_predictor_vars],
  # use specified predictors
  SL.library = match_lib,
  family = binomial(),
  method = "method.NNloglik",
  cvControl = list(V = 10)
)

# stop the cluster
stopCluster(cl)

# save your super learner model
here_save(sl, "sl")

# read 
sl <- here_read("sl")

# check outputs
# summary of the SuperLearner output
print(sl)

#a detailed summary, including cross-validated risks
summary(sl)                #

# examination of cross-validated performance
# cross-validated risks for each learner
sl$cvRisk

# weights assigned to each learner in the final ensemble
sl$coef

# generate predictions
predictions <- predict(sl, newdata = df_clean_hot[full_predictor_vars], type = "response")

# extract predictions from the 'pred' component and ensure it's a vector
df_clean_hot$pscore <- predictions$pred[, 1]

# check the structure of the predictions
str(df_clean_hot$pscore)

# check pscore
hist(df_clean_hot$pscore)

# make censoring weights
df_clean_hot$weights <- ifelse(df_clean_hot$t0_lost == 1,
                               1 / df_clean_hot$pscore,
                               1 / (1 - df_clean_hot$pscore))

# check 
hist(df_clean_hot$weights)# nothing extreme

# obtain stablise weights
# marginal_not_lost <- mean(df_clean_hot$t0_lost)

# # check (fyi)
# marginal_not_lost


# # stabalised weights
# df_clean_hot$weights_stabilised <- ifelse(
#   df_clean_hot$t0_lost == 1,
#   marginal_not_lost / df_clean_hot$pscore,
#   (1 - marginal_not_lost) / (1 - df_clean_hot$pscore)
# )

# checks
# hist(df_clean_hot$weights_stabilised)
# max(df_clean_hot$weights_stabilised)
# min(df_clean_hot$weights_stabilised)

# save output of hot code dataset
here_save(df_clean_hot, "df_clean_hot")
df_clean_hot <- here_read( "df_clean_hot")

# get weights into the model
# new weights by combining censor and sample weights, using stabalised weights
df_clean$t0_combo_weights = df_clean_hot$weights * df_clean$t0_sample_weights

# checks
min(df_clean$t0_combo_weights)
max(df_clean$t0_combo_weights)

# check distrobution of weights
hist(df_clean$t0_combo_weights)

# next remove those who were lost between t0 and t1
df_clean_t1 <- df_clean |> filter(t0_lost == 0) |>
  select(
    #-t1_hours_work, #### EXPOSURE 
   # -t1_lost, 
    -t0_lost, 
   # -t0_not_lost,
    -t0_sample_weights) |>
  relocate("t0_combo_weights", .before = starts_with("t1_"))

# check
hist(df_clean_t1$t0_combo_weights)

# checks
max(df_clean_t1$t0_combo_weights)
min(df_clean_t1$t0_combo_weights)

# number of weighted sample at t1, again check
n_not_lost_sample <- nrow(df_clean_t1)
n_not_lost_sample <- prettyNum(n_not_lost_sample, big.mark = ",")

# save output for manuscript
here_save(n_not_lost_sample, "n_not_lost_sample")

# check
n_not_lost_sample

# no one missing in exposure
# check
table(is.na(df_clean_t1$t1_hours_work)) # none
table((df_clean_t1$t1_not_lost)) # none


# gets us the correct df for weights

# check column oder and missing ness
naniar::vis_miss(df_clean_t1, warn_large_data = FALSE)

#check
nrow(df_clean_t1)

# next get data for t1
hist(df_clean_t1$t0_combo_weights)

margot::here_save(df_clean_t1, "df_clean_t1")
df_clean_t1 <- margot::here_read("df_clean_t1")

# get correct censoring -----------------------------------------
# THIS CODE IS NOT REDUNDANT 
t0_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t1_")))) > 0

t1_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t2_")))) > 0
# baseline_vars
# df_impute_base$t0_sample_weights

df_clean_t2 <- df_clean_t1 %>%
  # select(-t0_alert_level_combined_lead) |>
  mutate(t0_not_lost = ifelse(t0_na_condition, 0, t0_not_lost)) %>%
  mutate(t1_not_lost = ifelse(t1_na_condition, 0, t1_not_lost)) %>%
  mutate(across(starts_with("t1_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_not_lost == 0, NA_real_, .))) %>%
  mutate(across(starts_with("t2_"), ~ ifelse(t1_not_lost == 0, NA_real_, .))) |>
  # mutate(t0_lost = 1 - t0_not_lost) |>
  mutate(t1_lost = 1 - t1_not_lost) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  select(-t1_lost, -t0_not_lost)

# test
nrow(df_clean_t2)
colnames(df_clean_t2)
# checks
hist(df_clean_t2$t0_combo_weights)

# outcomes
naniar::vis_miss(df_clean_t2, warn_large_data = F)

# save
here_save(df_clean_t2, "df_clean_t2")



# check propensity scores -------------------------------------------------
# imbalance plot ----------------------------------------------------------
df_clean_t2 <- here_read("df_clean_t2")

# view
hist(df_clean_t2$t0_combo_weights)

# if you are comparing 2 x subgroups out of n > 2 groups,  do this
# df_subgroup <-df_clean_t2 |> filter(t0_eth_cat == "maori" | t0_eth_cat == "euro") |> droplevels()
#
# # save
# here_save(df_subgroup, "df_subgroup")


# START ANALYSIS HERE --------------------------------------------------------------
# read data --  start here if previous work already done
df_clean_t2 <- here_read("df_clean_t2")

# check
colnames(df_clean_t2)

# check
str(df_clean_t2)


# names of vars for modelling
names_base <-
  df_clean_t2 |> select(starts_with("t0"), -t0_combo_weights) |> colnames()

# check
names_base

# get outcome names for checks
names_outcomes <-
  df_clean_t2 |> select(starts_with("t2")) |> colnames()

# check
names_outcomes

# obsessively check
names(df_clean_t2)

# check against this
names_base

# df_final_base  <- df_clean_t2[names_base]
str(df_clean_t2)


# lets one hot encode any categorical vars, here only t0_eth_cat

# this code is the same as above
encoded_vars <- model.matrix(~ t0_education_level_coarsen + t0_eth_cat + t0_sample_origin + t0_rural_gch_2018_l - 1,
  data = df_clean_t2
)


head(encoded_vars)


# convert matrix to data frame
encoded_df <- as.data.frame(encoded_vars)

# make better names
encoded_df <- encoded_df |>
  janitor::clean_names()

# view the first few rows to confirm structure
head(encoded_df)

# bind the new one-hot encoded variables back to the original dataframe

# ensure to remove original categorical variables to avoid duplication

# note new data `df_clean_t2`
df_clean_hot_t2 <- df_clean_t2 |>
  select(-c( t0_education_level_coarsen, t0_eth_cat, t0_sample_origin, t0_rural_gch_2018_l)) |>
  bind_cols(encoded_df) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  #  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_"))

# check names
colnames(df_clean_hot_t2)

# # extract and print the new column names for encoded variables
# new_encoded_colnames_t2 <- colnames(encoded_df)
# print(new_encoded_colnames_t2)
#
# print(new_encoded_colnames_t2)
# # get baseline variable set without factors
#
# baseline_vars_set_t2 <- setdiff(names(df_clean_hot_t2), c("id","t0_eth_cat"))
# set_final_names <- c(baseline_vars_set_t2, new_encoded_colnames_t2)



# set names for analysis
set_final_names <-
  df_clean_hot_t2 |> select(starts_with("t0"), -t0_combo_weights) |> colnames()

# check
set_final_names

# add the new encoded column names

# check
colnames(df_clean_hot_t2)



# model estimation --------------------------------------------------------


# estimate models ---------------------------------------------------------
library(lmtp)
library(SuperLearner)
library(xgboost)
library(ranger)
library(future)
# model charitable giving
# this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
plan(multisession)

# hopefully you have 10 :). if not, consider decreasing the number of folds (for this assessment)
n_cores <- parallel::detectCores() - 2

#### SET VARIABLE NAMES
#  model
A <- c("t1_hours_work")
C <- c("t1_not_lost")
W <- set_final_names


# get max value of data
# make data_final
df_final <- df_clean_hot_t2


# for later use



# last checks
colnames(df_final)
naniar::vis_miss(df_final, warn_large_data = F)



here_save(df_final, "df_final")
here_save(W, "W")


# really start here -------------------------------------------------------


W <- margot::here_read("W")

df_final <- margot::here_read("df_final")
# 
# #  write function(s) for shift(s) ------------------------------------------------------
# # get enpoints
# max_data <- max(df_final$t1_hours_work, na.rm =TRUE)
# max_data

gain_A <- function(data, trt) {
  ifelse( (data[[trt]]  +  data[[trt]] * .2) < 80, data[[trt]] + data[[trt]] * .2, 
  data[[trt]]) # increase all by 20 up to 80 hours per week
}

# # shift function
# gain_A <- function(data, trt) {
#   ifelse(data[[trt]] < max_data - 1, data[[trt]] + 1, max_data)
# }

loss_A <- function(data, trt) {
  ifelse(
    (data[[trt]]  -  data[[trt]] * .2) <= 0, data[[trt]] -  data[[trt]] * .2, 
    0)
}
# 
# 
# 
# # changing your function to be fixed at 7 if you like...
# fixed_shift_to_7 <- function(data, trt) {
#   ifelse(data[[trt]] != 7, 7, data[[trt]])
# }

# # changing your function to be fixed at 0 if you like...
# fixed_shift_to_0 <- function(data, trt) {
#   ifelse(data[[trt]] != 0, 0, data[[trt]])
# }


# set libraries
sl_lib <- c("SL.glmnet", "SL.ranger", #
            "SL.xgboost") #

# view superlearners
listWrappers()

# test data
df_clean_slice <- df_final |>
  slice_head(n = 1000) |>
  as.data.frame()
df_clean_slice$t2_kessler_latent_anxiety_z
hist( df_clean_slice$t2_sexual_satisfaction_z) 
W
# Models!
names_base
# W_1 <- select_and_rename_cols(
#   W,
#   baseline_vars,
#   outcome = 't0_sexual_satisfaction_z',
#   from_prefix = "t2",
#   to_prefix = "t0"
# )

t2_kessler_latent_anxiety_z_null_test <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = NULL,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_combo_weights,
  learners_trt =sl_lib,
  learners_outcome = sl_lib
)


# evaluate predictive performance of the models
# however prediction isn't always best
t2_kessler_latent_anxiety_z_null_test$fits_r
t2_kessler_latent_anxiety_z_null_test$fits_m
here_save(t2_kessler_latent_anxiety_z_null_test,
          "t2_kessler_latent_anxiety_z_null_test")


# test gain
t2_kessler_latent_anxiety_z_test <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = loss_A,
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

# view

# view learner performance
t2_kessler_latent_anxiety_z_test$fits_r
t2_kessler_latent_anxiety_z_test$fits_m

# save
here_save(t2_kessler_latent_anxiety_z_null_test,
          "t2_kessler_latent_anxiety_z_null_test")

t2_kessler_latent_anxiety_z_lose_test <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = loss_A,
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
here_save(t2_kessler_latent_anxiety_z_lose_test, "t2_kessler_latent_anxiety_z_lose_test")

# test contrast
test_contrast_anxiety <- lmtp::lmtp_contrast(t2_kessler_latent_anxiety_z_test , ref = t2_kessler_latent_anxiety_z_null_test)

# check
test_contrast_anxiety


# tests look good, let's run the model ------------------------------------
# models ------------------------------------------------------------------


# health models -----------------------------------------------------------
W
C
A
outcome_vars


t2_smoker_binary <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_smoker",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)



t2_smoker_binary
here_save(t2_smoker_binary, "t2_smoker_binary")
# t2_smoker_binary <-here_read( "t2_smoker_binary")
# t2_smoker_binary

# print timing info

t2_smoker_binary_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_smoker",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
  )




t2_smoker_binary_1
here_save(t2_smoker_binary_1, "t2_smoker_binary_1")

C

#Do you currently smoke?
t2_smoker_binary_null  <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_smoker",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_smoker_binary_null
here_save(t2_smoker_binary_null, "t2_smoker_binary_null")

# 
# 
# names_base_t2_alcohol_frequency_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_alcohol_frequency_z")
# names_base_t2_alcohol_frequency_z
# 

#"How often do you have a drink containing alcohol?"
t2_alcohol_frequency_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_frequency_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)



t2_alcohol_frequency_z
here_save(t2_alcohol_frequency_z, "t2_alcohol_frequency_z")



#"How often do you have a drink containing alcohol?"
t2_alcohol_frequency_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_frequency_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)



t2_alcohol_frequency_z_1
here_save(t2_alcohol_frequency_z_1, "t2_alcohol_frequency_z_1")



#"How often do you have a drink containing alcohol?"
t2_alcohol_frequency_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_frequency_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_alcohol_frequency_z_null
here_save(t2_alcohol_frequency_z_null, "t2_alcohol_frequency_z_null")



# 
# names_base_t2_alcohol_intensity_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_alcohol_intensity_z")
# names_base_t2_alcohol_intensity_z

# How many drinks containing alcohol do you have on a typical day when drinking?
t2_alcohol_intensity_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_intensity_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_alcohol_intensity_z
here_save(t2_alcohol_intensity_z, "t2_alcohol_intensity_z")




# How many drinks containing alcohol do you have on a typical day when drinking?
t2_alcohol_intensity_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_intensity_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_alcohol_intensity_z_1
here_save(t2_alcohol_intensity_z_1, "t2_alcohol_intensity_z_1")



# How many drinks containing alcohol do you have on a typical day when drinking?
t2_alcohol_intensity_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_intensity_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_alcohol_intensity_z_null
here_save(t2_alcohol_intensity_z_null, "t2_alcohol_intensity_z_null")




# names_base_t2_sfhealth_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_sfhealth_z")
# names_base_t2_sfhealth_z

# "In general, would you say your health is...
# "I seem to get sick a little easier than other people."
# "I expect my health to get worse." ****

# t2_sfhealth_z <- lmtp_tmle(
#   data = df_final,
#   trt = A,
#   baseline = names_base_t2_sfhealth_z,
#   outcome = "t2_sfhealth_z",
#   cens = C,
#   shift = gain_A,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_final$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
#
# t2_sfhealth_z
# here_save(t2_sfhealth_z, "t2_sfhealth_z")
#
# #
# names_base_t2_sfhealth_your_health_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_sfhealth_your_health_z")
# names_base_t2_sfhealth_your_health_z
# 

# "In general, would you say your health is...
t2_short_form_health_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_short_form_health_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_sfhealth_your_health_z
here_save(t2_sfhealth_your_health_z, "t2_sfhealth_your_health_z")


# "In general, would you say your health is...
t2_sfhealth_your_health_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,  
  outcome = "t2_sfhealth_your_health_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_sfhealth_your_health_z_1
here_save(t2_sfhealth_your_health_z_1, "t2_sfhealth_your_health_z_1")





# "In general, would you say your health is...
t2_sfhealth_your_health_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_sfhealth_your_health_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_sfhealth_your_health_z_null
here_save(t2_sfhealth_your_health_z_null,
          "t2_sfhealth_your_health_z_null")


# 
# names_base_t2_hours_exercise_log_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_hours_exercise_log_z")
# names_base_t2_hours_exercise_log_z


# Hours spent … exercising/physical activity
t2_hours_exercise_log_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hours_exercise_log_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_hours_exercise_log_z
here_save(t2_hours_exercise_log_z, "t2_hours_exercise_log_z")

# Hours spent … exercising/physical activity
t2_hours_exercise_log_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hours_exercise_log_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_hours_exercise_log_z_1
here_save(t2_hours_exercise_log_z_1, "t2_hours_exercise_log_z_1")




# Hours spent … exercising/physical activity
t2_hours_exercise_log_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hours_exercise_log_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_hours_exercise_log_z_null
here_save(t2_hours_exercise_log_z_null,
          "t2_hours_exercise_log_z_null")



# 
# names_base_t2_hlth_sleep_hours_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_hlth_sleep_hours_z")
# names_base_t2_hlth_sleep_hours_z


#During the past month, on average, how many hours of actual sleep did you get per night?
t2_hlth_sleep_hours_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_sleep_hours_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)
t2_hlth_sleep_hours_z
here_save(t2_hlth_sleep_hours_z, "t2_hlth_sleep_hours_z")



#During the past month, on average, how many hours of actual sleep did you get per night?
t2_hlth_sleep_hours_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_sleep_hours_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)
t2_hlth_sleep_hours_z_1
here_save(t2_hlth_sleep_hours_z_1, "t2_hlth_sleep_hours_z_1")




#During the past month, on average, how many hours of actual sleep did you get per night?
t2_hlth_sleep_hours_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_sleep_hours_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_hlth_sleep_hours_z_null
here_save(t2_hlth_sleep_hours_z_null, "t2_hlth_sleep_hours_z_null")


# 
# names_base_t2_hlth_bmi_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_hlth_bmi_z")
# names_base_t2_hlth_bmi_z


# " What is your height? (metres)\nWhat is your weight? (kg)\nKg
t2_hlth_bmi_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_bmi_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_hlth_bmi_z
here_save(t2_hlth_bmi_z, "t2_hlth_bmi_z")



# " What is your height? (metres)\nWhat is your weight? (kg)\nKg
t2_hlth_bmi_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_bmi_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_hlth_bmi_z_1
here_save(t2_hlth_bmi_z_1, "t2_hlth_bmi_z_1")



# " What is your height? (metres)\nWhat is your weight? (kg)\nKg
t2_hlth_bmi_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_bmi_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_hlth_bmi_z_null
here_save(t2_hlth_bmi_z_null, "t2_hlth_bmi_z_null")



# embodied models ----------------------------------------------------------------


## Am satisfied with the appearance, size and shape of my body.
# 
# names_base_t2_bodysat_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_bodysat_z")
# names_base_t2_bodysat_z


## Am satisfied with the appearance, size and shape of my body.

t2_bodysat_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_bodysat_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_bodysat_z
here_save(t2_bodysat_z, "t2_bodysat_z")


## Am satisfied with the appearance, size and shape of my body.



t2_bodysat_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_bodysat_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_bodysat_z_1
here_save(t2_bodysat_z_1, "t2_bodysat_z_1")


## Am satisfied with the appearance, size and shape of my body.
t2_bodysat_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_bodysat_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_bodysat_z_null
here_save(t2_bodysat_z_null,
          "t2_bodysat_z_null")



# During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
# During the last 30 days, how often did.... you feel that everything was an effort?
# During the last 30 days, how often did.... you feel hopeless?
# During the last 30 days, how often did.... you feel nervous?
# During the last 30 days, how often did.... you feel restless or fidgety?
# During the last 30 days, how often did.... you feel worthless?

# 
# 
# names_base_t2_kessler_latent_anxiety_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_kessler_latent_anxiety_z")
# 
# # check
# names_base_t2_kessler_latent_anxiety_z


# During the last 30 days, how often did.... you feel that everything was an effort?
# During the last 30 days, how often did.... you feel nervous?
# During the last 30 days, how often did.... you feel restless or fidgety?



t2_kessler_latent_anxiety_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_kessler_latent_anxiety_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_kessler_latent_anxiety_z
here_save(t2_kessler_latent_anxiety_z, "t2_kessler_latent_anxiety_z")




t2_kessler_latent_anxiety_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_kessler_latent_anxiety_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_kessler_latent_anxiety_z_1
here_save(t2_kessler_latent_anxiety_z_1, "t2_kessler_latent_anxiety_z_1")




#
# t2_kessler_latent_anxiety_z_clinical
# here_save(t2_kessler_latent_anxiety_z_clinical, "t2_kessler_latent_anxiety_z_clinical")

# During the last 30 days, how often did.... you feel that everything was an effort?
# During the last 30 days, how often did.... you feel nervous?
# During the last 30 days, how often did.... you feel restless or fidgety?

t2_kessler_latent_anxiety_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_kessler_latent_anxiety_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

# test
here_save(t2_kessler_latent_anxiety_z_null,
          "t2_kessler_latent_anxiety_z_null")


# depression

# During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
# During the last 30 days, how often did.... you feel hopeless?
# During the last 30 days, how often did.... you feel worthless?

# 
# names_base_t2_kessler_latent_depression_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_kessler_latent_depression_z")
# 
# # check
# names_base_t2_kessler_latent_depression_z


# During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
# During the last 30 days, how often did.... you feel hopeless?
# During the last 30 days, how often did.... you feel worthless?

t2_kessler_latent_depression_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_kessler_latent_depression_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_kessler_latent_depression_z
here_save(t2_kessler_latent_depression_z,
          "t2_kessler_latent_depression_z")




t2_kessler_latent_depression_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_kessler_latent_depression_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_kessler_latent_depression_z_1
here_save(t2_kessler_latent_depression_z_1,
          "t2_kessler_latent_depression_z_1")






t2_kessler_latent_depression_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_kessler_latent_depression_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_kessler_latent_depression_z_null
here_save(t2_kessler_latent_depression_z_null,
          "t2_kessler_latent_depression_z_null")




# During the last 30 days, how often did.... you feel exhausted?
# 
# names_base_t2_hlth_fatigue_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_hlth_fatigue_z")
# names_base_t2_hlth_fatigue_z

# During the last 30 days, how often did.... you feel exhausted?
t2_hlth_fatigue_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_fatigue_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


here_save(t2_hlth_fatigue_z, "t2_hlth_fatigue_z")

# During the last 30 days, how often did.... you feel exhausted?
t2_hlth_fatigue_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_fatigue_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_hlth_fatigue_z_1
here_save(t2_hlth_fatigue_z_1, "t2_hlth_fatigue_z_1")




# During the last 30 days, how often did.... you feel exhausted?
t2_hlth_fatigue_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_fatigue_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_hlth_fatigue_z_null
here_save(t2_hlth_fatigue_z_null, "t2_hlth_fatigue_z_null")



# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
# 
# 
# names_base_t2_rumination_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_rumination_z")
# names_base_t2_rumination_z
# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
t2_rumination_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_rumination_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_rumination_z
here_save(t2_rumination_z, "t2_rumination_z")


# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
t2_rumination_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_rumination_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_rumination_z_1
here_save(t2_rumination_z_1, "t2_rumination_z_1")




# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
t2_rumination_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_rumination_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_rumination_z_null
here_save(t2_rumination_z_null, "t2_rumination_z_null")




# ego models --------------------------------------------------------------

# 
# 
# names_base_t2_sexual_satisfaction_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_sexual_satisfaction_z")
# names_base_t2_sexual_satisfaction_z

#  How satisfied are you with your sex life?
t2_sexual_satisfaction_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_sexual_satisfaction_z
here_save(t2_sexual_satisfaction_z, "t2_sexual_satisfaction_z")


#  How satisfied are you with your sex life?
t2_sexual_satisfaction_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_sexual_satisfaction_z_1
here_save(t2_sexual_satisfaction_z_1, "t2_sexual_satisfaction_z_1")

#  How satisfied are you with your sex life?
t2_sexual_satisfaction_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_sexual_satisfaction_z_null
here_save(t2_sexual_satisfaction_z_null,
          "t2_sexual_satisfaction_z_null")


# 
# # 
# names_base_t2_power_no_control_composite_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_power_no_control_composite_z")
# names_base_t2_power_no_control_composite_z
# 
# # I do not have enough power or control over\nimportant parts of my life.
# # Other people have too much power or control over\nimportant parts of my life.
# t2_power_no_control_composite_z <- lmtp_tmle(
#   data = df_final,
#   trt = A,
#   baseline = W,
#   outcome = "t2_power_no_control_composite_z",
#   cens = C,
#   shift = gain_A,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_final$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
# 
# t2_power_no_control_composite_z
# here_save(t2_power_no_control_composite_z,
#           "t2_power_no_control_composite_z")
# 
# 
# t2_power_no_control_composite_z_1 <- lmtp_tmle(
#   data = df_final,
#   trt = A,
#   baseline = W,
#   outcome = "t2_power_no_control_composite_z",
#   cens = C,
#   shift = loss_A,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_final$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
# 
# t2_power_no_control_composite_z_1
# here_save(t2_power_no_control_composite_z_1,
#           "t2_power_no_control_composite_z_1")
# 
# 
# 
# # I do not have enough power or control over\nimportant parts of my life.
# # Other people have too much power or control over\nimportant parts of my life.
# t2_power_no_control_composite_z_null <- lmtp_tmle(
#   data = df_final,
#   trt = A,
#   baseline = W,
#   outcome = "t2_power_no_control_composite_z",
#   cens = C,
#   shift = NULL,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_final$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
# 
# t2_power_no_control_composite_z_null
# here_save(t2_power_no_control_composite_z_null,
#           "t2_power_no_control_composite_z_null")
# 
# # 
# 
# outcome_vars
# names_base_t2_self_esteem_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_self_esteem_z")
# names_base_t2_self_esteem_z


#  On the whole am satisfied with myself.
#  Take a positive attitude toward myself
#  Am inclined to feel that I am a failure.
t2_self_esteem_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_esteem_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_self_esteem_z
here_save(t2_self_esteem_z, "t2_self_esteem_z")



t2_self_esteem_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_esteem_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_self_esteem_z_1
here_save(t2_self_esteem_z_1, "t2_self_esteem_z_1")




#  On the whole am satisfied with myself.
#  Take a positive attitude toward myself
#  Am inclined to feel that I am a failure.
t2_self_esteem_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_esteem_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_self_esteem_z_null
here_save(t2_self_esteem_z_null, "t2_self_esteem_z_null")


# 
# names_base_t2_self_control_have_lots_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_self_control_have_lots_z")
# names_base_t2_self_control_have_lots_z

# In general, I have a lot of self-control.
t2_self_control_have_lots_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_have_lots_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_self_control_have_lots_z
here_save(t2_self_control_have_lots_z, "t2_self_control_have_lots_z")


t2_self_control_have_lots_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_have_lots_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_self_control_have_lots_z_1
here_save(t2_self_control_have_lots_z_1, "t2_self_control_have_lots_z_1")




#In general, I have a lot of self-control.
t2_self_control_have_lots_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_have_lots_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_self_control_have_lots_z_null
here_save(t2_self_control_have_lots_z_null,
          "t2_self_control_have_lots_z_null")


# 
# 
# names_base_t2_self_control_wish_more_reversed_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_self_control_wish_more_reversed_z")
# names_base_t2_self_control_wish_more_reversed_z


# I wish I had more self-discipline.(r)
t2_self_control_wish_more_reversed_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_wish_more_reversed_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_self_control_wish_more_reversed_z
here_save(t2_self_control_wish_more_reversed_z,
          "t2_self_control_wish_more_reversed_z")



# I wish I had more self-discipline.(r)
t2_self_control_wish_more_reversed_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_wish_more_reversed_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_self_control_wish_more_reversed_z_1
here_save(t2_self_control_wish_more_reversed_z_1,
          "t2_self_control_wish_more_reversed_z_1")



# I wish I had more self-discipline.(r)
t2_self_control_wish_more_reversed_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_wish_more_reversed_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_self_control_wish_more_reversed_z_null
here_save(
  t2_self_control_wish_more_reversed_z_null,
  "t2_self_control_wish_more_reversed_z_null"
)




# 
# 
# 
# names_base_t2_permeability_individual_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_permeability_individual_z")
# names_base_t2_permeability_individual_z

# I believe I am capable, as an individual\nof improving my status in society.
t2_permeability_individual_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_permeability_individual_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_permeability_individual_z
here_save(t2_permeability_individual_z,
          "t2_permeability_individual_z")



# I believe I am capable, as an individual\nof improving my status in society.
t2_permeability_individual_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_permeability_individual_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_permeability_individual_z_1
here_save(t2_permeability_individual_z_1,
          "t2_permeability_individual_z_1")



# I believe I am capable, as an individual\nof improving my status in society.
t2_permeability_individual_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_permeability_individual_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)
t2_permeability_individual_z_null
here_save(t2_permeability_individual_z_null,
          "t2_permeability_individual_z_null")

# 
# names_base_t2_emotion_regulation_out_control_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_emotion_regulation_out_control_z")
# names_base_t2_emotion_regulation_out_control_z
# 
# 
# # emotional regulation
# # When I feel negative emotions, my emotions feel out of control. w10 - w13
# t2_emotion_regulation_out_control_z <- lmtp_tmle(
#   data = df_final,
#   trt = A,
#   baseline = W,
#   outcome = "t2_emotion_regulation_out_control_z",
#   cens = C,
#   shift = gain_A,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_final$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
# 
# t2_emotion_regulation_out_control_z
# here_save(t2_emotion_regulation_out_control_z,
#           "t2_emotion_regulation_out_control_z")
# 
# 
# 
# # When I feel negative emotions, my emotions feel out of control. w10 - w13
# t2_emotion_regulation_out_control_z_1 <- lmtp_tmle(
#   data = df_final,
#   trt = A,
#   baseline = W,
#   outcome = "t2_emotion_regulation_out_control_z",
#   cens = C,
#   shift = loss_A,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_final$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
# 
# t2_emotion_regulation_out_control_z_1
# here_save(t2_emotion_regulation_out_control_z_1,
#           "t2_emotion_regulation_out_control_z_1")
# 
# 
# 
# 
# t2_emotion_regulation_out_control_z_null <- lmtp_tmle(
#   data = df_final,
#   trt = A,
#   baseline = W,
#   outcome = "t2_emotion_regulation_out_control_z",
#   cens = C,
#   shift = NULL,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_final$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
# t2_emotion_regulation_out_control_z_null
# here_save(
#   t2_emotion_regulation_out_control_z_null,
#   "t2_emotion_regulation_out_control_z_null"
# )
# 
#
# Not relevant
# names_base_t2_impermeability_group_z <- select_and_rename_cols(names_base = names_base, baseline_vars = baseline_vars, outcome = "t2_impermeability_group_z")
# names_base_t2_impermeability_group_z
#
# # The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
# ##(NEG CONTROL)
# t2_impermeability_group_z<- lmtp_tmle(
#   data = df_final,
#   trt = A,
#   baseline = names_base_t2_impermeability_group_z,
#   outcome = "t2_impermeability_group_z",
#   cens = C,
#   shift = gain_A,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_final$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
#
# t2_impermeability_group_z
# here_save(t2_impermeability_group_z, "t2_impermeability_group_z")
#
# # The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
# ##(NEG CONTROL)
# t2_impermeability_group_z_null <- lmtp_tmle(
#   data = df_final,
#   trt = A,
#   baseline = names_base_t2_impermeability_group_z,
#   outcome = "t2_impermeability_group_z",
#   cens = C,
#   shift = NULL,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_final$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
#
# t2_impermeability_group_z_null
# here_save(t2_impermeability_group_z_null, "t2_impermeability_group_z_null")
#

# 
# 
# names_base_t2_perfectionism_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_perfectionism_z")
# names_base_t2_perfectionism_z

# # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.
t2_perfectionism_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_perfectionism_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_perfectionism_z
here_save(t2_perfectionism_z, "t2_perfectionism_z")



# # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.
t2_perfectionism_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_perfectionism_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_perfectionism_z_1
here_save(t2_perfectionism_z_1, "t2_perfectionism_z_1")






# # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.
t2_perfectionism_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_perfectionism_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_perfectionism_z_null
here_save(t2_perfectionism_z_null, "t2_perfectionism_z_null")



# reflective models --------------------------------------------------------------

# 
# names_base_t2_gratitude_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_gratitude_z")
# names_base_t2_gratitude_z
# 


## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
t2_gratitude_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_gratitude_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_gratitude_z
here_save(t2_gratitude_z, "t2_gratitude_z")


## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
t2_gratitude_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_gratitude_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_gratitude_z_1
here_save(t2_gratitude_z_1, "t2_gratitude_z_1")



## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
t2_gratitude_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_gratitude_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_gratitude_z_null
here_save(t2_gratitude_z_null, "t2_gratitude_z_null")


# 
# 
# names_base_t2_vengeful_rumin_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_vengeful_rumin_z")
# names_base_t2_vengeful_rumin_z
# 
# 
# 
# 
# # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
t2_vengeful_rumin_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_vengeful_rumin_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_vengeful_rumin_z
here_save(t2_vengeful_rumin_z, "t2_vengeful_rumin_z")

# 
# 
# 
# # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
t2_vengeful_rumin_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_vengeful_rumin_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_vengeful_rumin_z_1
here_save(t2_vengeful_rumin_z_1, "t2_vengeful_rumin_z_1")

# Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
t2_vengeful_rumin_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_vengeful_rumin_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_vengeful_rumin_z_null
here_save(t2_vengeful_rumin_z_null, "t2_vengeful_rumin_z_null")




# names_base_t2_pwb_your_health_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_pwb_your_health_z")
# names_base_t2_pwb_your_health_z


# Your health.
t2_pwb_your_health_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_health_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_pwb_your_health_z
here_save(t2_pwb_your_health_z, "t2_pwb_your_health_z")



# Your health.
t2_pwb_your_health_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_health_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_pwb_your_health_z_1
here_save(t2_pwb_your_health_z_1, "t2_pwb_your_health_z_1")



# Your health.
t2_pwb_your_health_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_health_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_pwb_your_health_z_null
here_save(t2_pwb_your_health_z_null, "t2_pwb_your_health_z_null")


# 
# 
# names_base_t2_pwb_your_future_security_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_pwb_your_future_security_z")
# names_base_t2_pwb_your_future_security_z


# #Your future security.
t2_pwb_your_future_security_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_future_security_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_pwb_your_future_security_z
here_save(t2_pwb_your_future_security_z,
          "t2_pwb_your_future_security_z")


# #Your future security.
t2_pwb_your_future_security_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_future_security_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_pwb_your_future_security_z_1
here_save(t2_pwb_your_future_security_z_1,
          "t2_pwb_your_future_security_z_1")



# #Your future security.
t2_pwb_your_future_security_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_future_security_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_pwb_your_future_security_z_null
here_save(t2_pwb_your_future_security_z_null,
          "t2_pwb_your_future_security_z_null")


# 
# names_base_t2_pwb_your_relationships_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_pwb_your_relationships_z")
# names_base_t2_pwb_your_relationships_z


# Your personal relationships.
t2_pwb_your_relationships_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_relationships_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_pwb_your_relationships_z
here_save(t2_pwb_your_relationships_z, "t2_pwb_your_relationships_z")




# Your personal relationships.
t2_pwb_your_relationships_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_relationships_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_pwb_your_relationships_z_1
here_save(t2_pwb_your_relationships_z_1, "t2_pwb_your_relationships_z_1")



# Your personal relationships.
t2_pwb_your_relationships_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_relationships_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_pwb_your_relationships_z_null
here_save(t2_pwb_your_relationships_z_null,
          "t2_pwb_your_relationships_z_null")



# 
# names_base_t2_pwb_standard_living_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_pwb_standard_living_z")
# names_base_t2_pwb_standard_living_z


# Your standard of living.
t2_pwb_standard_living_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_standard_living_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_pwb_standard_living_z
here_save(t2_pwb_standard_living_z, "t2_pwb_standard_living_z")


# Your standard of living.
t2_pwb_standard_living_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_standard_living_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_pwb_standard_living_z_1
here_save(t2_pwb_standard_living_z_1, "t2_pwb_standard_living_z_1")


# Your standard of living.
t2_pwb_standard_living_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_standard_living_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_pwb_standard_living_z_null
here_save(t2_pwb_standard_living_z_null,
          "t2_pwb_standard_living_z_null")




# names_base_t2_lifemeaning_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_lifemeaning_z")
# names_base_t2_lifemeaning_z


# My life has a clear sense of purpose.
# I have a good sense of what makes my life meaningful.
# t2_lifemeaning_z <- lmtp_tmle(
#   data = df_final,
#   trt = A,
#   baseline = names_base_t2_lifemeaning_z,
#   outcome = "t2_lifemeaning_z",
#   cens = C,
#   shift = gain_A,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_final$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
# 
# 
# t2_lifemeaning_z
# here_save(t2_lifemeaning_z, "t2_lifemeaning_z")
# 
# 
# # My life has a clear sense of purpose.
# # I have a good sense of what makes my life meaningful.
# t2_lifemeaning_z_null <- lmtp_tmle(
#   data = df_final,
#   trt = A,
#   baseline = names_base_t2_lifemeaning_z,
#   outcome = "t2_lifemeaning_z",
#   cens = C,
#   shift = NULL,
#   mtp = TRUE,
#   folds = 10,
#   outcome_type = "continuous",
#   weights = df_final$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib
# 
# )
# 
# t2_lifemeaning_z_null
# here_save(t2_lifemeaning_z_null, "t2_lifemeaning_z_null")
# 

# My life has a clear sense of purpose.

# names_base_t2_meaning_purpose_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_meaning_purpose_z")
# names_base_t2_meaning_purpose_z

t2_meaning_purpose_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_purpose_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_meaning_purpose_z
here_save(t2_meaning_purpose_z, "t2_meaning_purpose_z")




t2_meaning_purpose_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_purpose_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_meaning_purpose_z_1
here_save(t2_meaning_purpose_z_1, "t2_meaning_purpose_z_1")




# My life has a clear sense of purpose.
t2_meaning_purpose_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_purpose_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_meaning_purpose_z_null
here_save(t2_meaning_purpose_z_null, "t2_meaning_purpose_z_null")



# I have a good sense of what makes my life meaningful.


# names_base_t2_meaning_sense_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_meaning_sense_z")
# names_base_t2_meaning_sense_z

# I have a good sense of what makes my life meaningful.

t2_meaning_sense_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_sense_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_meaning_sense_z
here_save(t2_meaning_sense_z, "t2_meaning_sense_z")




t2_meaning_sense_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_sense_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_meaning_sense_z_1
here_save(t2_meaning_sense_z_1, "t2_meaning_sense_z_1")



# I have a good sense of what makes my life meaningful.
t2_meaning_sense_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_sense_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_meaning_sense_z_null
here_save(t2_meaning_sense_z_null, "t2_meaning_sense_z_null")





# 
# 
# names_base_t2_lifesat_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_lifesat_z")
# names_base_t2_lifesat_z


# I am satisfied with my life.
# In most ways my life is close to ideal.
t2_lifesat_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_lifesat_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_lifesat_z
here_save(t2_lifesat_z, "t2_lifesat_z")


t2_lifesat_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_lifesat_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_lifesat_z_1
here_save(t2_lifesat_z_1, "t2_lifesat_z_1")

# I am satisfied with my life.
# In most ways my life is close to ideal.
t2_lifesat_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_lifesat_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_lifesat_z_null
here_save(t2_lifesat_z_null, "t2_lifesat_z_null")



# social models -----------------------------------------------------------

# 
# names_base_t2_support_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_support_z")
# names_base_t2_support_z

# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress (r)
# There is no one I can turn to for guidance in times of stress.
t2_support_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_support_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_support_z
here_save(t2_support_z, "t2_support_z")


t2_support_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_support_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_support_z_1
here_save(t2_support_z_1, "t2_support_z_1")




# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress (r)
# There is no one I can turn to for guidance in times of stress.
t2_support_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_support_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_support_z_null
here_save(t2_support_z_null, "t2_support_z_null")


# 
# 
# names_base_t2_neighbourhood_community_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_neighbourhood_community_z")
# names_base_t2_neighbourhood_community_z

# I feel a sense of community with others in my local neighbourhood.
t2_neighbourhood_community_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_neighbourhood_community_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_neighbourhood_community_z
here_save(t2_neighbourhood_community_z,
          "t2_neighbourhood_community_z")


t2_neighbourhood_community_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_neighbourhood_community_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_neighbourhood_community_z_1
here_save(t2_neighbourhood_community_z_1,
          "t2_neighbourhood_community_z_1")


# I feel a sense of community with others in my local neighbourhood.
t2_neighbourhood_community_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_neighbourhood_community_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_neighbourhood_community_z_null
here_save(t2_neighbourhood_community_z_null,
          "t2_neighbourhood_community_z_null")



# 
# names_base_t2_belong_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_belong_z")
# names_base_t2_belong_z

# Know that people in my life accept and value me.
# Feel like an outsider.
# Know that people around me share my attitudes and beliefs.
t2_belong_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_belong_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_belong_z
here_save(t2_belong_z, "t2_belong_z")



t2_belong_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_belong_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)

t2_belong_z_1
here_save(t2_belong_z_1, "t2_belong_z_1")


# Know that people in my life accept and value me.
# Feel like an outsider.
# Know that people around me share my attitudes and beliefs.
t2_belong_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_belong_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib

)


t2_belong_z_null
here_save(t2_belong_z_null, "t2_belong_z_null")


# contrasts health ---------------------------------------------------------------
A
# smoker
t2_smoker_binary <- here_read("t2_smoker_binary")
t2_smoker_binary_1 <- here_read("t2_smoker_binary_1")
t2_smoker_binary_null <-
  here_read("t2_smoker_binary_null")


# first contrast
contrast_t2_smoker_binary <-
  lmtp_contrast(t2_smoker_binary,
                ref = t2_smoker_binary_null,
                type = "rr")


tab_contrast_t2_smoker_binary <-
  margot::margot_lmtp_evalue(contrast_t2_smoker_binary,
                  scale = "RR",
                  new_name = "Smoker")

tab_contrast_t2_smoker_binary

here_save(tab_contrast_t2_smoker_binary, "ttab_contrast_t2_smoker_binary")

# second contrast
contrast_t2_smoker_binary_1 <-
  lmtp_contrast(t2_smoker_binary_1,
                ref = t2_smoker_binary_null,
                type = "rr")



tab_contrast_t2_smoker_binary_1 <-
  margot::margot_lmtp_evalue(contrast_t2_smoker_binary_1,
                  scale = "RR",
                  new_name = "Smoker")


tab_contrast_t2_smoker_binary_1

here_save(tab_contrast_t2_smoker_binary_1, "tab_contrast_t2_smoker_binary_1")

# # sf health
# t2_sfhealth_z <- here_read("t2_sfhealth_z")
# t2_sfhealth_z_null <- here_read("t2_sfhealth_z_null")
#
#
# contrast_t2_sfhealth_z <- lmtp_contrast(t2_sfhealth_z,
#                                         ref = t2_sfhealth_z_null,
#                                         type = "additive")
#
# tab_contrast_t2_sfhealth_z <-
#   margot_lmtp_tab(contrast_t2_sfhealth_z,
#                   scale = "RD",
#                   new_name = "Short form health")
#
#
# tab_contrast_t2_sfhealth_z <-
#   lmtp_evalue_tab(tab_contrast_t2_sfhealth_z,
#                   scale = c("RD"))
#
# tab_contrast_t2_sfhealth_z


# sf health, your health
t2_short_form_health_z <- here_read("t2_short_form_health_z")
t2_short_form_health_z_1 <- here_read("t2_short_form_health_z_1")

t2_short_form_health_z_null <-
  here_read("t2_short_form_health_z_null")

# first contrast 
contrast_t2_short_form_health_z_null <-
  lmtp_contrast(t2_short_form_health_z,
                ref = t2_short_form_health_z_null,
                type = "additive")

tab_contrast_t2_short_form_health_z <-
  margot::margot_lmtp_evalue(contrast_t2_short_form_health_z,
                  scale = "RD",
                  new_name = "Short form health, your health")

tab_contrast_t2_sfhealth_your_health_z

# second contrast

contrast_t2_sfhealth_your_health_z_1 <-
  lmtp_contrast(t2_sfhealth_your_health_z_1,
                ref = t2_sfhealth_your_health_z_null,
                type = "additive")


tab_contrast_t2_sfhealth_your_health_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_sfhealth_your_health_z_1,
                  scale = "RD",
                  new_name = "Short form health, your health")

tab_contrast_t2_sfhealth_your_health_z_1



# excercise
t2_hours_exercise_log_z <-
  here_read("t2_hours_exercise_log_z")
t2_hours_exercise_log_z_1 <-
  here_read("t2_hours_exercise_log_z_1")

t2_hours_exercise_log_z_null <-
  here_read("t2_hours_exercise_log_z_null")

# first contrast
contrast_t2_hours_exercise_log_z <-
  lmtp_contrast(t2_hours_exercise_log_z,
                ref = t2_hours_exercise_log_z_null,
                type = "additive")

tab_contrast_t2_hours_exercise_log_z <-
  margot::margot_lmtp_evalue(contrast_t2_hours_exercise_log_z,
                  scale = "RD",
                  new_name = "Hours excercise")

tab_contrast_t2_hours_exercise_log_z

# second contrast
contrast_t2_hours_exercise_log_z_1 <-
  lmtp_contrast(t2_hours_exercise_log_z_1,
                ref = t2_hours_exercise_log_z_null,
                type = "additive")

tab_contrast_t2_hours_exercise_log_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_hours_exercise_log_z_1,
                  scale = "RD",
                  new_name = "Hours excercise")

tab_contrast_t2_hours_exercise_log_z_1

# alcohol freq

t2_alcohol_frequency_z <-
  here_read("t2_alcohol_frequency_z")

t2_alcohol_frequency_z_1 <-
  here_read("t2_alcohol_frequency_z_1")

t2_alcohol_frequency_z_null <-
  here_read("t2_alcohol_frequency_z_null")

# first contrast
contrast_t2_alcohol_frequency_z <-
  lmtp_contrast(t2_alcohol_frequency_z,
                ref = t2_alcohol_frequency_z_null,
                type = "additive")


tab_contrast_t2_alcohol_frequency_z <-
  margot::margot_lmtp_evalue(contrast_t2_alcohol_frequency_z ,
                  scale = "RD",
                  new_name = "Alcohol frequency")
tab_contrast_t2_alcohol_frequency_z

# second contrast
contrast_t2_alcohol_frequency_z_1 <-
  lmtp_contrast(t2_alcohol_frequency_z_1,
                ref = t2_alcohol_frequency_z_null,
                type = "additive")


tab_contrast_t2_alcohol_frequency_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_alcohol_frequency_z_1,
                  scale = "RD",
                  new_name = "Alcohol frequency")

# alcohol intensity

t2_alcohol_intensity_z <-
  here_read("t2_alcohol_intensity_z")

t2_alcohol_intensity_z_1 <-
  here_read("t2_alcohol_intensity_z_1")

t2_alcohol_intensity_z_null <-
  here_read("t2_alcohol_intensity_z_null")

# first contrast
contrast_t2_alcohol_intensity_z <-
  lmtp_contrast(t2_alcohol_intensity_z,
                ref = t2_alcohol_intensity_z_null,
                type = "additive")

contrast_t2_alcohol_intensity_z_1 <-
  lmtp_contrast(t2_alcohol_intensity_z_1,
                ref = t2_alcohol_intensity_z_null,
                type = "additive")



tab_contrast_t2_alcohol_intensity_z <-
  margot::margot_lmtp_evalue(contrast_t2_alcohol_intensity_z,
                  scale = "RD",
                  new_name = "Alcohol intensity")



tab_contrast_t2_alcohol_intensity_z


tab_contrast_t2_alcohol_intensity_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_alcohol_intensity_z_1,
                  scale = "RD",
                  new_name = "Alcohol intensity")

tab_contrast_t2_alcohol_intensity_z_1



# hour sleep
t2_hlth_sleep_hours_z <- here_read("t2_hlth_sleep_hours_z")
t2_hlth_sleep_hours_z_1 <- here_read("t2_hlth_sleep_hours_z_1")


t2_hlth_sleep_hours_z_null <-
  here_read("t2_hlth_sleep_hours_z_null")

# first contrast
contrast_t2_hours_sleep_z <-
  lmtp_contrast(t2_hlth_sleep_hours_z,
                ref = t2_hlth_sleep_hours_z_null,
                type = "additive")

tab_contrast_t2_hours_sleep_z <-
  margot::margot_lmtp_evalue(contrast_t2_hours_sleep_z,
                  scale = "RD",
                  new_name = "Hours sleep")


tab_contrast_t2_hours_sleep_z <-
  lmtp_evalue_tab(tab_contrast_t2_hours_sleep_z,
                  scale = c("RD"))

tab_contrast_t2_hours_sleep_z

# second contrast
contrast_t2_hours_sleep_z_1 <-
  lmtp_contrast(t2_hlth_sleep_hours_z_1,
                ref = t2_hlth_sleep_hours_z_null,
                type = "additive")

tab_contrast_t2_hours_sleep_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_hours_sleep_z_1,
                  scale = "RD",
                  new_name = "Hours sleep")


tab_contrast_t2_hours_sleep_z_1


# bmi
t2_hlth_bmi_z <- here_read("t2_hlth_bmi_z")
t2_hlth_bmi_z_1 <- here_read("t2_hlth_bmi_z_1")

t2_hlth_bmi_z_null <- here_read("t2_hlth_bmi_z_null")

# first contrast
contrast_t2_bmi_z <- lmtp_contrast(t2_hlth_bmi_z,
                                   ref = t2_hlth_bmi_z_null,
                                   type = "additive")

tab_contrast_t2_bmi_z <-
  margot::margot_lmtp_evalue(contrast_t2_bmi_z, scale = "RD", new_name = "BMI")



# second contrast
contrast_t2_bmi_z_1 <- lmtp_contrast(t2_hlth_bmi_z_1,
                                     ref = t2_hlth_bmi_z_null,
                                     type = "additive")

tab_contrast_t2_bmi_z_1  <-
  margot::margot_lmtp_evalue(contrast_t2_bmi_z_1, scale = "RD", new_name = "BMI")


tab_contrast_t2_bmi_z_1 


# contrast embodied -------------------------------------------------------

# bodysat
t2_bodysat_z <- here_read("t2_bodysat_z")
t2_bodysat_z_1 <- here_read("t2_bodysat_z_1")

t2_bodysat_z_null <- here_read("t2_bodysat_z_null")

# first contrast
contrast_t2_bodysat_z <- lmtp_contrast(t2_bodysat_z,
                                       ref = t2_bodysat_z_null,
                                       type = "additive")

tab_contrast_t2_bodysat_z <-
  margot::margot_lmtp_evalue(contrast_t2_bodysat_z, scale = "RD", new_name = "Body satisfaction")



# second contrast
contrast_t2_bodysat_z_1 <- lmtp_contrast(t2_bodysat_z_1,
                                         ref = t2_bodysat_z_null,
                                         type = "additive")

tab_contrast_t2_bodysat_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_bodysat_z_1, scale = "RD", new_name = "Body satisfaction")


tab_contrast_t2_bodysat_z_1


# kessler 6

# t2_kessler6_sum_z <- here_read("t2_kessler6_sum_z")
# t2_kessler6_sum_z_null <-
#   here_read("t2_kessler6_sum_z_null")
#
# contrast_t2_kessler6_sum_z <-
#   lmtp_contrast(t2_kessler6_sum_z,
#                 ref = t2_kessler6_sum_z_null,
#                 type = "additive")
#
# tab_contrast_t2_kessler6_sum_z <-
#   margot_lmtp_tab(contrast_t2_kessler6_sum_z,
#                   scale = "RD",
#                   new_name = "Kessler 6 distress")
#
#
# tab_contrast_t2_kessler6_sum_z <-
#   lmtp_evalue_tab(tab_contrast_t2_kessler6_sum_z,
#                   scale = c("RD"))
#
# tab_contrast_t2_kessler6_sum_z


# depression

t2_kessler_latent_depression_z <-
  here_read("t2_kessler_latent_depression_z")

t2_kessler_latent_depression_z_1 <-
  here_read("t2_kessler_latent_depression_z_1")

t2_kessler_latent_depression_z_null <-
  here_read("t2_kessler_latent_depression_z_null")

t2_kessler_latent_depression_z_null

# first contrast
contrast_t2_kessler_latent_depression_z <-
  lmtp_contrast(t2_kessler_latent_depression_z,
                ref = t2_kessler_latent_depression_z_null,
                type = "additive")

tab_contrast_t2_kessler_latent_depression_z <-
  margot::margot_lmtp_evalue(contrast_t2_kessler_latent_depression_z,
                  scale = "RD",
                  new_name = "Kessler 6 depression")


tab_contrast_t2_kessler_latent_depression_z


# second contrast
contrast_t2_kessler_latent_depression_z_1 <-
  lmtp_contrast(t2_kessler_latent_depression_z_1,
                ref = t2_kessler_latent_depression_z_null,
                type = "additive")

tab_contrast_t2_kessler_latent_depression_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_kessler_latent_depression_z_1,
                  scale = "RD",
                  new_name = "Kessler 6 depression")

tab_contrast_t2_kessler_latent_depression_z_1


#anxiety

t2_kessler_latent_anxiety_z <-
  here_read("t2_kessler_latent_anxiety_z")

t2_kessler_latent_anxiety_z_1 <-
  here_read("t2_kessler_latent_anxiety_z_1")

t2_kessler_latent_anxiety_z_null <-
  here_read("t2_kessler_latent_anxiety_z_null")

# first contrast
contrast_t2_kessler_latent_anxiety_z <-
  lmtp_contrast(t2_kessler_latent_anxiety_z,
                ref = t2_kessler_latent_anxiety_z_null,
                type = "additive")

tab_contrast_t2_kessler_latent_anxiety_z <-
  margot::margot_lmtp_evalue(contrast_t2_kessler_latent_anxiety_z,
                  scale = "RD",
                  new_name = "Kessler 6 anxiety")

tab_contrast_t2_kessler_latent_anxiety_z

# second contrast
contrast_t2_kessler_latent_anxiety_z_1 <-
  lmtp_contrast(t2_kessler_latent_anxiety_z_1,
                ref = t2_kessler_latent_anxiety_z_null,
                type = "additive")

tab_contrast_t2_kessler_latent_anxiety_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_kessler_latent_anxiety_z_1,
                  scale = "RD",
                  new_name = "Kessler 6 anxiety")

tab_contrast_t2_kessler_latent_anxiety_z_1


# fatigue
t2_hlth_fatigue_z <- here_read("t2_hlth_fatigue_z")

t2_hlth_fatigue_z_1 <- here_read("t2_hlth_fatigue_z_1")

t2_hlth_fatigue_z_null <-
  here_read("t2_hlth_fatigue_z_null")

# first contrast
contrast_t2_hlth_fatigue_z <-
  lmtp_contrast(t2_hlth_fatigue_z,
                ref = t2_hlth_fatigue_z_null,
                type = "additive")


tab_contrast_t2_hlth_fatigue_z <-
  margot::margot_lmtp_evalue(contrast_t2_hlth_fatigue_z ,
                  scale = "RD",
                  new_name = "Fatigue")


tab_contrast_t2_hlth_fatigue_z


# second contrast
contrast_t2_hlth_fatigue_z_1 <-
  lmtp_contrast(t2_hlth_fatigue_z_1,
                ref = t2_hlth_fatigue_z_null,
                type = "additive")


tab_contrast_t2_hlth_fatigue_z_1  <-
  margot::margot_lmtp_evalue(contrast_t2_hlth_fatigue_z_1,
                  scale = "RD",
                  new_name = "Fatigue")

tab_contrast_t2_hlth_fatigue_z_1 

# rumination
t2_rumination_z <- here_read("t2_rumination_z")
t2_rumination_z_1 <- here_read("t2_rumination_z_1")
t2_rumination_z_null <-
  here_read("t2_rumination_z_null")

# first contrast
contrast_t2_rumination_z <-
  lmtp_contrast(t2_rumination_z,
                ref = t2_rumination_z_null,
                type = "additive")

tab_contrast_t2_rumination_z <-
  margot::margot_lmtp_evalue(contrast_t2_rumination_z ,
                  scale = "RD",
                  new_name = "Rumination")

tab_contrast_t2_rumination_z                  
# second contrast

contrast_t2_rumination_z_1 <-
  lmtp_contrast(t2_rumination_z_1,
                ref = t2_rumination_z_null,
                type = "additive")

tab_contrast_t2_rumination_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_rumination_z_1 ,
                  scale = "RD",
                  new_name = "Rumination")


tab_contrast_t2_rumination_z_1




# sex sat
t2_sexual_satisfaction_z <-
  here_read("t2_sexual_satisfaction_z")

t2_sexual_satisfaction_z_1 <-
  here_read("t2_sexual_satisfaction_z_1")

t2_sexual_satisfaction_z_null <-
  here_read("t2_sexual_satisfaction_z_null")

# first contrast
contrast_t2_sexual_satisfaction_z <-
  lmtp_contrast(t2_sexual_satisfaction_z,
                ref = t2_sexual_satisfaction_z_null,
                type = "additive")


tab_contrast_t2_sexual_satisfaction_z <-
  margot::margot_lmtp_evalue(contrast_t2_sexual_satisfaction_z,
                  scale = "RD",
                  new_name = "Sexual satisfaction")

tab_contrast_t2_sexual_satisfaction_z


# second contrast
contrast_t2_sexual_satisfaction_z_1 <-
  lmtp_contrast(t2_sexual_satisfaction_z_1,
                ref = t2_sexual_satisfaction_z_null,
                type = "additive")


tab_contrast_t2_sexual_satisfaction_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_sexual_satisfaction_z_1,
                  scale = "RD",
                  new_name = "Sexual satisfaction")

tab_contrast_t2_sexual_satisfaction_z_1



# contrasts ego -----------------------------------------------------------
# 
# # power no control
# t2_power_no_control_composite_z <-
#   here_read("t2_power_no_control_composite_z")
# 
# t2_power_no_control_composite_z_1 <-
#   here_read("t2_power_no_control_composite_z_1")
# 
# t2_power_no_control_composite_z_null <-
#   here_read("t2_power_no_control_composite_z_null")
# 
# # first contrast
# contrast_t2_power_no_control_composite_z <-
#   lmtp_contrast(t2_power_no_control_composite_z,
#                 ref = t2_power_no_control_composite_z_null,
#                 type = "additive")
# 
# 
# tab_contrast_t2_power_no_control_composite_z <-
#   margot_lmtp_tab(contrast_t2_power_no_control_composite_z,
#                   scale = "RD",
#                   new_name = "Power no control")
# 
# 
# tab_contrast_t2_power_no_control_composite_z <-
#   lmtp_evalue_tab(tab_contrast_t2_power_no_control_composite_z,
#                   scale = c("RD"))
# 
# tab_contrast_t2_power_no_control_composite_z
# 
# 
# 
# # second contrast
# contrast_t2_power_no_control_composite_z_1  <-
#   lmtp_contrast(t2_power_no_control_composite_z_1,
#                 ref = t2_power_no_control_composite_z_null,
#                 type = "additive")
# 
# 
# tab_contrast_t2_power_no_control_composite_z_1  <-
#   margot_lmtp_tab(contrast_t2_power_no_control_composite_z_1 ,
#                   scale = "RD",
#                   new_name = "Power no control")
# 
# 
# tab_contrast_t2_power_no_control_composite_z_1  <-
#   lmtp_evalue_tab(tab_contrast_t2_power_no_control_composite_z_1,
#                   scale = c("RD"))
# 
# tab_contrast_t2_power_no_control_composite_z_1 


# self esteem

t2_self_esteem_z <- here_read("t2_self_esteem_z")
t2_self_esteem_z_1 <- here_read("t2_self_esteem_z_1")
t2_self_esteem_z_null <-
  here_read("t2_self_esteem_z_null")


# first contrast
contrast_t2_self_esteem_z <-
  lmtp_contrast(t2_self_esteem_z,
                ref = t2_self_esteem_z_null,
                type = "additive")


tab_contrast_t2_self_esteem_z <-
  margot::margot_lmtp_evalue(contrast_t2_self_esteem_z,
                  scale = "RD",
                  new_name = "Self esteem")

tab_contrast_t2_self_esteem_z



# second contrast
contrast_t2_self_esteem_z_1 <-
  lmtp_contrast(t2_self_esteem_z_1,
                ref = t2_self_esteem_z_null,
                type = "additive")

tab_contrast_t2_self_esteem_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_self_esteem_z_1,
                  scale = "RD",
                  new_name = "Self esteem")

tab_contrast_t2_self_esteem_z_1

# perfectionism
t2_perfectionism_z <- here_read("t2_perfectionism_z")
t2_perfectionism_z_1 <- here_read("t2_perfectionism_z_1")
t2_perfectionism_z_null <-
  here_read("t2_perfectionism_z_null")

# first contrast
contrast_t2_perfectionism_z <-
  lmtp_contrast(t2_perfectionism_z,
                ref = t2_perfectionism_z_null,
                type = "additive")


tab_contrast_t2_perfectionism_z <-
  margot::margot_lmtp_evalue(contrast_t2_perfectionism_z ,
                  scale = "RD",
                  new_name = "Perfectionism")


tab_contrast_t2_perfectionism_z


# second contrast
contrast_t2_perfectionism_z_1 <-
  lmtp_contrast(t2_perfectionism_z_1,
                ref = t2_perfectionism_z_null,
                type = "additive")


tab_contrast_t2_perfectionism_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_perfectionism_z_1,
                  scale = "RD",
                  new_name = "Perfectionism")

tab_contrast_t2_perfectionism_z_1


# self control have
t2_self_control_have_lots_z <-
  here_read("t2_self_control_have_lots_z")

t2_self_control_have_lots_z_1 <-
  here_read("t2_self_control_have_lots_z_1")

t2_self_control_have_lots_z_null <-
  here_read("t2_self_control_have_lots_z_null")

# first contrast
contrast_t2_self_control_have_lots_z <-
  lmtp_contrast(t2_self_control_have_lots_z,
                ref = t2_self_control_have_lots_z_null,
                type = "additive")


tab_contrast_t2_self_control_have_lots_z <-
  margot::margot_lmtp_evalue(contrast_t2_self_control_have_lots_z ,
                  scale = "RD",
                  new_name = "Self control have")

tab_contrast_t2_self_control_have_lots_z


# second contrast
contrast_t2_self_control_have_lots_z_1 <-
  lmtp_contrast(t2_self_control_have_lots_z_1,
                ref = t2_self_control_have_lots_z_null,
                type = "additive")


tab_contrast_t2_self_control_have_lots_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_self_control_have_lots_z_1,
                  scale = "RD",
                  new_name = "Self control have")


tab_contrast_t2_self_control_have_lots_z_1




# self control wish
t2_self_control_wish_more_reversed_z <-
  here_read("t2_self_control_wish_more_reversed_z")

t2_self_control_wish_more_reversed_z_1 <-
  here_read("t2_self_control_wish_more_reversed_z_1")

t2_self_control_wish_more_reversed_z_null <-
  here_read("t2_self_control_wish_more_reversed_z_null")

# first contrast
contrast_t2_self_control_wish_more_reversed_z <-
  lmtp_contrast(t2_self_control_wish_more_reversed_z,
                ref = t2_self_control_wish_more_reversed_z_null,
                type = "additive")

tab_contrast_t2_self_control_wish_more_reversed_z <-
  margot::margot_lmtp_evalue(
    contrast_t2_self_control_wish_more_reversed_z,
    scale = "RD",
    new_name = "Self control wish more (reversed)"
  )


tab_contrast_t2_self_control_wish_more_reversed_z


# secind contrast
contrast_t2_self_control_wish_more_reversed_z_1 <-
  lmtp_contrast(t2_self_control_wish_more_reversed_z_1,
                ref = t2_self_control_wish_more_reversed_z_null,
                type = "additive")

tab_contrast_t2_self_control_wish_more_reversed_z_1 <-
  margot::margot_lmtp_evalue(
    contrast_t2_self_control_wish_more_reversed_z_1,
    scale = "RD",
    new_name = "Self control wish more (reversed)"
  )

tab_contrast_t2_self_control_wish_more_reversed_z_1
# 
# # emotional regulation
# t2_emotion_regulation_out_control_z <-
#   here_read("t2_emotion_regulation_out_control_z")
# 
# t2_emotion_regulation_out_control_z_1 <-
#   here_read("t2_emotion_regulation_out_control_z_1")
# 
# t2_emotion_regulation_out_control_z_null <-
#   here_read("t2_emotion_regulation_out_control_z_null")

# 
# # first contrast
# contrast_t2_emotion_regulation_out_control_z <-
#   lmtp_contrast(t2_emotion_regulation_out_control_z,
#                 ref = t2_emotion_regulation_out_control_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_emotion_regulation_out_control_z <-
#   margot_lmtp_tab(
#     contrast_t2_emotion_regulation_out_control_z ,
#     scale = "RD",
#     new_name = "Emotional regulation (out of control)"
#   )
# 
# 
# tab_contrast_t2_emotion_regulation_out_control_z <-
#   lmtp_evalue_tab(tab_contrast_t2_emotion_regulation_out_control_z,
#                   scale = c("RD"))
# 
# tab_contrast_t2_emotion_regulation_out_control_z
# 
# # second contrast
# contrast_t2_emotion_regulation_out_control_z_1 <-
#   lmtp_contrast(t2_emotion_regulation_out_control_z_1,
#                 ref = t2_emotion_regulation_out_control_z_null,
#                 type = "additive")
# 
# 
# 
# tab_contrast_t2_emotion_regulation_out_control_z_1 <-
#   margot_lmtp_tab(
#     contrast_t2_emotion_regulation_out_control_z_1 ,
#     scale = "RD",
#     new_name = "Emotional regulation (out of control)"
#   )
# 
# 
# tab_contrast_t2_emotion_regulation_out_control_z_1 <-
#   lmtp_evalue_tab(tab_contrast_t2_emotion_regulation_out_control_z_1,
#                   scale = c("RD"))
# 
# tab_contrast_t2_emotion_regulation_out_control_z_1
# 
# 
# #  permeability individual
# t2_permeability_individual_z <-
#   here_read("t2_permeability_individual_z")
# 
# t2_permeability_individual_z_1 <-
#   here_read("t2_permeability_individual_z_1")
# 
# t2_permeability_individual_z_null <-
#   here_read("t2_permeability_individual_z_null")
# 
# # first contrast
# contrast_t2_permeability_individual_z <-
#   lmtp_contrast(t2_permeability_individual_z,
#                 ref = t2_permeability_individual_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_permeability_individual_z <-
#   margot_lmtp_tab(contrast_t2_permeability_individual_z ,
#                   scale = "RD",
#                   new_name = "Permeability self")
# 
# 
# tab_contrast_t2_permeability_individual_z <-
#   lmtp_evalue_tab(tab_contrast_t2_permeability_individual_z,
#                   scale = c("RD"))
# 
# tab_contrast_t2_permeability_individual_z
# 
# 
# # second contrast
# contrast_t2_permeability_individual_z_1 <-
#   lmtp_contrast(t2_permeability_individual_z_1,
#                 ref = t2_permeability_individual_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_permeability_individual_z_1 <-
#   margot_lmtp_tab(contrast_t2_permeability_individual_z_1,
#                   scale = "RD",
#                   new_name = "Permeability self")
# 
# 
# tab_contrast_t2_permeability_individual_z_1 <-
#   lmtp_evalue_tab(tab_contrast_t2_permeability_individual_z_1,
#                   scale = c("RD"))
# 
# tab_contrast_t2_permeability_individual_z_1
# 


# more a political view
# t2_impermeability_group_z<- here_read("t2_impermeability_group_z")
# t2_impermeability_group_z_null <- here_read("t2_impermeability_group_z_null")
#
# contrast_t2_impermeability_group_z <- lmtp_contrast(
#   t2_impermeability_group_z,
#   ref = t2_impermeability_group_z_null,
#   type = "additive")


# contrasts reflective ----------------------------------------------------

# gratitude
t2_gratitude_z <- here_read("t2_gratitude_z")
t2_gratitude_z_1 <- here_read("t2_gratitude_z_1")

t2_gratitude_z_null <- here_read("t2_gratitude_z_null")
t2_gratitude_z_null
# first contrast
contrast_t2_gratitude_z <- lmtp_contrast(t2_gratitude_z,
                                         ref = t2_gratitude_z_null,
                                         type = "additive")
tab_contrast_t2_gratitude_z <-
  contrast_t2_gratitude_z(contrast_t2_gratitude_z,
                  scale = "RD",
                  new_name = "Gratitude")

tab_contrast_t2_gratitude_z

# second contrast
contrast_t2_gratitude_z_1 <- lmtp_contrast(t2_gratitude_z_1,
                                           ref = t2_gratitude_z_null,
                                           type = "additive")
tab_contrast_t2_gratitude_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_gratitude_z_1 ,
                  scale = "RD",
                  new_name = "Gratitude")

tab_contrast_t2_gratitude_z_1 


# 
# # vengence / forgive
# t2_vengeful_rumin_z <- here_read("t2_vengeful_rumin_z")
# t2_vengeful_rumin_z_1 <- here_read("t2_vengeful_rumin_z_1")
# 
# t2_vengeful_rumin_z_null <-
#   here_read("t2_vengeful_rumin_z_null")
# 
# # first contrast
# contrast_t2_vengeful_rumin_z <-
#   lmtp_contrast(t2_vengeful_rumin_z,
#                 ref = t2_vengeful_rumin_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_vengeful_rumin_z <-
#   margot_lmtp_tab(contrast_t2_vengeful_rumin_z,
#                   scale = "RD",
#                   new_name = "Vengefulness (forgiveness)")
# 
# 
# tab_contrast_t2_vengeful_rumin_z <-
#   lmtp_evalue_tab(tab_contrast_t2_vengeful_rumin_z,
#                   scale = c("RD"))
# 
# tab_contrast_t2_vengeful_rumin_z
# 
# 
# # second contrast
# contrast_t2_vengeful_rumin_z_1 <-
#   lmtp_contrast(t2_vengeful_rumin_z_1,
#                 ref = t2_vengeful_rumin_z_null,
#                 type = "additive")
# 
# tab_contrast_t2_vengeful_rumin_z_1  <-
#   margot_lmtp_tab(contrast_t2_vengeful_rumin_z_1 ,
#                   scale = "RD",
#                   new_name = "Vengefulness (forgiveness")
# 
# 
# tab_contrast_t2_vengeful_rumin_z_1  <-
#   lmtp_evalue_tab(tab_contrast_t2_vengeful_rumin_z_1 ,
#                   scale = c("RD"))
# 
# tab_contrast_t2_vengeful_rumin_z_1 
# 

# pwb your health

t2_pwb_your_health_z <-
  here_read("t2_pwb_your_health_z")

t2_pwb_your_health_z_1 <-
  here_read("t2_pwb_your_health_z_1")

t2_pwb_your_health_z_null <-
  here_read("t2_pwb_your_health_z_null")

# first contrast
contrast_t2_pwb_your_health_z <-
  lmtp_contrast(t2_pwb_your_health_z,
                ref = t2_pwb_your_health_z_null,
                type = "additive")

tab_contrast_t2_pwb_your_health_z <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_health_z,
                  scale = "RD",
                  new_name = "PWB your health")


tab_contrast_t2_pwb_your_health_z

# second contrast
contrast_t2_pwb_your_health_z_1 <-
  lmtp_contrast(t2_pwb_your_health_z_1,
                ref = t2_pwb_your_health_z_null,
                type = "additive")

tab_contrast_t2_pwb_your_health_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_health_z_1,
                  scale = "RD",
                  new_name = "PWB your health")



tab_contrast_t2_pwb_your_health_z_1

# pwb your furture security

t2_pwb_your_future_security_z <-
  here_read("t2_pwb_your_future_security_z")

t2_pwb_your_future_security_z_1 <-
  here_read("t2_pwb_your_future_security_z_1")

t2_pwb_your_future_security_z_null <-
  here_read("t2_pwb_your_future_security_z_null")

# first contrast
contrast_t2_pwb_your_future_security_z <-
  lmtp_contrast(t2_pwb_your_future_security_z,
                ref = t2_pwb_your_future_security_z_null,
                type = "additive")

tab_contrast_t2_pwb_your_future_security_z <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_future_security_z,
                  scale = "RD",
                  new_name = "PWB your future security")

tab_contrast_t2_pwb_your_future_security_z

# second contrast
contrast_t2_pwb_your_future_security_z_1 <-
  lmtp_contrast(t2_pwb_your_future_security_z_1,
                ref = t2_pwb_your_future_security_z_null,
                type = "additive")

tab_contrast_t2_pwb_your_future_security_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_future_security_z_1,
                  scale = "RD",
                  new_name = "PWB your future security")


# pwb your relationships

t2_pwb_your_relationships_z <-
  here_read("t2_pwb_your_relationships_z")

t2_pwb_your_relationships_z_1 <-
  here_read("t2_pwb_your_relationships_z_1")

t2_pwb_your_relationships_z_null <-
  here_read("t2_pwb_your_relationships_z_null")

# first contrast
contrast_t2_pwb_your_relationships_z <-
  lmtp_contrast(t2_pwb_your_relationships_z,
                ref = t2_pwb_your_relationships_z_null,
                type = "additive")


tab_contrast_t2_pwb_your_relationships_z <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_relationships_z ,
                  scale = "RD",
                  new_name = "PWB your relationships")


tab_contrast_t2_pwb_your_relationships_z


# second contrast
contrast_t2_pwb_your_relationships_z_1 <-
  lmtp_contrast(t2_pwb_your_relationships_z_1 ,
                ref = t2_pwb_your_relationships_z_null,
                type = "additive")


tab_contrast_t2_pwb_your_relationships_z_1  <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_relationships_z_1 ,
                  scale = "RD",
                  new_name = "PWB your relationships")

tab_contrast_t2_pwb_your_relationships_z_1 



# pwb your standard of living

t2_pwb_standard_living_z <-
  here_read("t2_pwb_standard_living_z")

t2_pwb_standard_living_z_1 <-
  here_read("t2_pwb_standard_living_z_1")

t2_pwb_standard_living_z_null <-
  here_read("t2_pwb_standard_living_z_null")

# first contrast
contrast_t2_pwb_standard_living_z <-
  lmtp_contrast(t2_pwb_standard_living_z,
                ref = t2_pwb_standard_living_z_null,
                type = "additive")

tab_contrast_t2_pwb_standard_living_z <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_standard_living_z ,
                  scale = "RD",
                  new_name = "PWB your standard living")


tab_contrast_t2_pwb_standard_living_z


# second contrast
contrast_t2_pwb_standard_living_z_1 <-
  lmtp_contrast(t2_pwb_standard_living_z_1,
                ref = t2_pwb_standard_living_z_null,
                type = "additive")

tab_contrast_t2_pwb_standard_living_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_standard_living_z_1 ,
                  scale = "RD",
                  new_name = "PWB your standard living")

tab_contrast_t2_pwb_standard_living_z_1




t2_meaning_purpose_z <- here_read("t2_meaning_purpose_z")
t2_meaning_purpose_z_1 <- here_read("t2_meaning_purpose_z_1")
t2_meaning_purpose_z_null <-
  here_read("t2_meaning_purpose_z_null")

# first contrast
contrast_t2_meaning_purpose_z <-
  lmtp_contrast(t2_meaning_purpose_z,
                ref = t2_meaning_purpose_z_null,
                type = "additive")

tab_contrast_t2_meaning_purpose_z <-
  margot::margot_lmtp_evalue(contrast_t2_meaning_purpose_z,
                  scale = "RD",
                  new_name = "Meaning: clear sense of purpose")

tab_contrast_t2_meaning_purpose_z

# second contrast
contrast_t2_meaning_purpose_z_1 <-
  lmtp_contrast(t2_meaning_purpose_z_1 ,
                ref = t2_meaning_purpose_z_null,
                type = "additive")

tab_contrast_t2_meaning_purpose_z_1  <-
  margot::margot_lmtp_evalue(contrast_t2_meaning_purpose_z_1 ,
                  scale = "RD",
                  new_name = "Meaning: clear sense of purpose")
# meaning sense

t2_meaning_sense_z <- here_read("t2_meaning_sense_z")
t2_meaning_sense_z_1 <- here_read("t2_meaning_sense_z_1")

t2_meaning_sense_z_null <-
  here_read("t2_meaning_sense_z_null")

# first contrast
contrast_t2_meaning_sense_z <-
  lmtp_contrast(t2_meaning_sense_z,
                ref = t2_meaning_sense_z_null,
                type = "additive")

tab_contrast_t2_meaning_sense_z <-
  margot::margot_lmtp_evalue(contrast_t2_meaning_sense_z,
                  scale = "RD",
                  new_name = "Meaning: good sense of what makes my life meaningful")


tab_contrast_t2_meaning_sense_z


# second contrast
contrast_t2_meaning_sense_z_1 <-
  lmtp_contrast(t2_meaning_sense_z_1,
                ref = t2_meaning_sense_z_null,
                type = "additive")

tab_contrast_t2_meaning_sense_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_meaning_sense_z_1,
                  scale = "RD",
                  new_name = "Meaning: good sense of what makes my life meaningful")

tab_contrast_t2_meaning_sense_z_1



# lifesat

t2_lifesat_z <- here_read("t2_lifesat_z")
t2_lifesat_z_1 <- here_read("t2_lifesat_z_1")

t2_lifesat_z_null <- here_read("t2_lifesat_z_null")


# first contrast
contrast_t2_lifesat_z <- lmtp_contrast(t2_lifesat_z,
                                       ref = t2_lifesat_z_null,
                                       type = "additive")

tab_contrast_t2_lifesat_z <-
  margot::margot_lmtp_evalue(contrast_t2_lifesat_z, scale = "RD", new_name = "Satisfaction with life")


tab_contrast_t2_lifesat_z

# second contrast
contrast_t2_lifesat_z_1 <- lmtp_contrast(t2_lifesat_z_1,
                                         ref = t2_lifesat_z_null,
                                         type = "additive")

tab_contrast_t2_lifesat_z_1 <-
  margot_lmtp_tab(contrast_t2_lifesat_z_1, scale = "RD", new_name = "Satisfaction with life")


tab_contrast_t2_lifesat_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_lifesat_z_1,
                  scale = c("RD"))

tab_contrast_t2_lifesat_z_1

# contrasts social --------------------------------------------------------

# social support
t2_support_z <- here_read("t2_support_z")
t2_support_z_1 <- here_read("t2_support_z_1")
t2_support_z_null <- here_read("t2_support_z_null")

# first contrast
contrast_t2_support_z <- lmtp_contrast(t2_support_z,
                                       ref = t2_support_z_null,
                                       type = "additive")
tab_contrast_t2_support_z <-
  margot_lmtp_tab(contrast_t2_support_z, scale = "RD", new_name = "Social support")


tab_contrast_t2_support_z <-
  lmtp_evalue_tab(tab_contrast_t2_support_z,
                  scale = c("RD"))

tab_contrast_t2_support_z

# second contrast
contrast_t2_support_z_1 <- lmtp_contrast(t2_support_z_1,
                                         ref = t2_support_z_null,
                                         type = "additive")
tab_contrast_t2_support_z_1 <-
  margot_lmtp_tab(contrast_t2_support_z_1, scale = "RD", new_name = "Social support")


tab_contrast_t2_support_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_support_z_1,
                  scale = c("RD"))

tab_contrast_t2_support_z_1

# neighbourhood community

t2_neighbourhood_community_z <-
  here_read("t2_neighbourhood_community_z")

t2_neighbourhood_community_z_1 <-
  here_read("t2_neighbourhood_community_z_1")

t2_neighbourhood_community_z_null <-
  here_read("t2_neighbourhood_community_z_null")

# first contrast
contrast_t2_neighbourhood_community_z <-
  lmtp_contrast(t2_neighbourhood_community_z,
                ref = t2_neighbourhood_community_z_null,
                type = "additive")

tab_contrast_t2_neighbourhood_community_z <-
  margot_lmtp_tab(contrast_t2_neighbourhood_community_z,
                  scale = "RD",
                  new_name = "Neighbourhood community")


tab_contrast_t2_neighbourhood_community_z <-
  lmtp_evalue_tab(tab_contrast_t2_neighbourhood_community_z,
                  scale = c("RD"))

tab_contrast_t2_neighbourhood_community_z

# second contrast
contrast_t2_neighbourhood_community_z_1 <-
  lmtp_contrast(t2_neighbourhood_community_z_1,
                ref = t2_neighbourhood_community_z_null,
                type = "additive")

tab_contrast_t2_neighbourhood_community_z_1 <-
  margot_lmtp_tab(contrast_t2_neighbourhood_community_z_1,
                  scale = "RD",
                  new_name = "Neighbourhood community")


tab_contrast_t2_neighbourhood_community_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_neighbourhood_community_z_1,
                  scale = c("RD"))

tab_contrast_t2_neighbourhood_community_z_1

# social belong
t2_belong_z <- here_read("t2_belong_z")
t2_belong_z_1 <- here_read("t2_belong_z_1")
t2_belong_z_null <- here_read("t2_belong_z_null")

# first contrast
contrast_t2_belong_z <- lmtp_contrast(t2_belong_z,
                                      ref = t2_belong_z_null,
                                      type = "additive")


tab_contrast_t2_belong_z <-
  margot_lmtp_tab(contrast_t2_belong_z, scale = "RD",
                  new_name = "Social belonging")


tab_contrast_t2_belong_z <-
  lmtp_evalue_tab(tab_contrast_t2_belong_z,
                  scale = c("RD"))

tab_contrast_t2_belong_z

# second contrast
contrast_t2_belong_z_1 <- lmtp_contrast(t2_belong_z_1 ,
                                        ref = t2_belong_z_null,
                                        type = "additive")


tab_contrast_t2_belong_z_1  <-
  margot_lmtp_tab(contrast_t2_belong_z_1 , scale = "RD",
                  new_name = "Social belonging")


tab_contrast_t2_belong_z_1  <-
  lmtp_evalue_tab(tab_contrast_t2_belong_z_1 ,
                  scale = c("RD"))

tab_contrast_t2_belong_z_1 


# make tables -------------------------------------------------------------

# don't forget to report smoking


f_1
# bind individual tables

tab_health_smoker <- rbind(
  tab_contrast_t2_smoker_binary
)
tab_health


tab_health <- rbind(
  tab_contrast_t2_hours_exercise_log_z,
  tab_contrast_t2_alcohol_frequency_z,
  tab_contrast_t2_alcohol_intensity_z,
  tab_contrast_t2_bmi_z,
  tab_contrast_t2_hours_sleep_z,
  tab_contrast_t2_sfhealth_your_health_z
)
tab_health


here_save(tab_health , "tab_health")

tab_body <- rbind(
  tab_contrast_t2_bodysat_z,
  tab_contrast_t2_kessler_latent_anxiety_z,
  tab_contrast_t2_kessler_latent_depression_z,
  tab_contrast_t2_hlth_fatigue_z,
  tab_contrast_t2_rumination_z,
  tab_contrast_t2_sexual_satisfaction_z
)
tab_body


here_save(tab_body, "tab_body")

tab_ego <- rbind(
  tab_contrast_t2_emotion_regulation_out_control_z,
  tab_contrast_t2_permeability_individual_z,
  tab_contrast_t2_perfectionism_z,
  tab_contrast_t2_power_no_control_composite_z,
  tab_contrast_t2_self_control_have_lots_z,
  tab_contrast_t2_self_control_wish_more_reversed_z,
  tab_contrast_t2_self_esteem_z
)

tab_ego

here_save(tab_ego, "tab_ego")

tab_reflective <- rbind(
  tab_contrast_t2_gratitude_z,
  tab_contrast_t2_meaning_purpose_z,
  tab_contrast_t2_meaning_sense_z,
  tab_contrast_t2_pwb_your_future_security_z,
  tab_contrast_t2_pwb_your_health_z,
  tab_contrast_t2_pwb_your_relationships_z,
  tab_contrast_t2_pwb_standard_living_z,
  tab_contrast_t2_lifesat_z
  # tab_contrast_t2_vengeful_rumin_z
)
tab_reflective

here_save(tab_reflective,"tab_reflective")

tab_social <- rbind(
  tab_contrast_t2_belong_z,
  tab_contrast_t2_neighbourhood_community_z,
  tab_contrast_t2_support_z
)
tab_social

here_save(tab_social,"tab_social")

# make group table
group_tab_health <- group_tab(tab_health  , type = "RD")

# save
here_save(group_tab_health, "group_tab_health")


# make group table
group_tab_body <- group_tab(tab_body , type = "RD")

# save
here_save(group_tab_body, "group_tab_body")

# make group table
group_tab_ego <- group_tab(tab_ego, type = "RD")

# save
here_save(group_tab_ego, "group_tab_ego")




# make group table
group_tab_reflective <- group_tab(tab_reflective, type = "RD")
tab_reflective
# save
here_save(group_tab_reflective, "group_tab_reflective")

# make group table
group_tab_social <- group_tab(tab_social, type = "RD")

# save
here_save(group_tab_social, "group_tab_social")

group_tab_health <- here_read("group_tab_health")
group_tab_body <- here_read("group_tab_body")
group_tab_ego <- here_read("group_tab_ego")
group_tab_reflective <- here_read("group_tab_reflective")
group_tab_social <- here_read("group_tab_social")


# create plots -------------------------------------------------------------

# check N
N
sub_title = "Forgiveness: shift exposure to average if below average, else take expected natural value, N = 34,749"


# graph health
plot_group_tab_health <- margot_plot(
  group_tab_health,
  type = "RD",
  title = "Health effects",
  subtitle = sub_title,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 15,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_health
dev.off()
# save graph
ggsave(
  plot_group_tab_health,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_health.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



# graph body
plot_group_tab_body <- margot_plot(
  group_tab_body,
  type = "RD",
  title = "Body effects",
  subtitle = sub_title,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

# save graph
ggsave(
  plot_group_tab_body,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_body.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)


plot_group_tab_body

# graph ego
plot_group_tab_ego <- margot_plot(
  group_tab_ego,
  type = "RD",
  title = "Ego effects",
  subtitle = sub_title,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_ego

# save graph
ggsave(
  plot_group_tab_ego,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_ego.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

plot_group_tab_ego

# graph reflective
plot_group_tab_reflective <- margot_plot(
  group_tab_reflective,
  type = "RD",
  title = "Reflective effects",
  subtitle = sub_title,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_reflective

# save graph
ggsave(
  plot_group_tab_reflective,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_reflective.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

# graph social
plot_group_tab_social <- margot_plot(
  group_tab_social,
  type = "RD",
  title = "Social effects",
  subtitle = sub_title,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_group_tab_social

# save graph
ggsave(
  plot_group_tab_social,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_social.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()


# make tables shift one up -------------------------------------------------------------

# don't forget to report smoking
# bind individual tables
tab_health_1 <- rbind(
  tab_contrast_t2_hours_exercise_log_z_1,
  tab_contrast_t2_alcohol_frequency_z_1,
  tab_contrast_t2_alcohol_intensity_z_1,
  tab_contrast_t2_bmi_z_1,
  tab_contrast_t2_hours_sleep_z_1,
  tab_contrast_t2_sfhealth_your_health_z_1
)
tab_health_1

here_save(tab_health_1, "tab_health_1")

tab_body_1 <- rbind(
  tab_contrast_t2_bodysat_z_1,
  tab_contrast_t2_kessler_latent_anxiety_z_1,
  tab_contrast_t2_kessler_latent_depression_z_1,
  tab_contrast_t2_hlth_fatigue_z_1,
  tab_contrast_t2_rumination_z_1,
  tab_contrast_t2_sexual_satisfaction_z_1
)
tab_body_1

here_save(tab_body_1, "tab_body_1")

tab_ego_1 <- rbind(
  tab_contrast_t2_emotion_regulation_out_control_z_1,
  tab_contrast_t2_permeability_individual_z_1,
  tab_contrast_t2_perfectionism_z_1,
  tab_contrast_t2_power_no_control_composite_z_1,
  tab_contrast_t2_self_control_have_lots_z_1,
  tab_contrast_t2_self_control_wish_more_reversed_z_1,
  tab_contrast_t2_self_esteem_z_1
)

tab_ego_1

here_save(tab_ego_1, "tab_ego_1")

tab_reflective_1 <- rbind(
  tab_contrast_t2_gratitude_z_1,
  tab_contrast_t2_meaning_purpose_z_1,
  tab_contrast_t2_meaning_sense_z_1,
  tab_contrast_t2_pwb_your_future_security_z_1,
  tab_contrast_t2_pwb_your_health_z_1,
  tab_contrast_t2_pwb_your_relationships_z_1,
  tab_contrast_t2_pwb_standard_living_z_1,
  tab_contrast_t2_lifesat_z_1
  # tab_contrast_t2_vengeful_rumin_z_1
)
tab_reflective_1

here_save(tab_reflective_1, "tab_reflective_1")

tab_social_1 <- rbind(
  tab_contrast_t2_belong_z_1,
  tab_contrast_t2_neighbourhood_community_z_1,
  tab_contrast_t2_support_z_1
)
tab_social_1
here_save(tab_social_1, "tab_social_1")

# make group table
group_tab_health_1 <- group_tab(tab_health_1, type = "RD")

# save
here_save(group_tab_health_1, "group_tab_health_1")


# make group table
group_tab_body_1 <- group_tab(tab_body_1 , type = "RD")

# save
here_save(group_tab_body_1, "group_tab_body_1")

# make group table
group_tab_ego_1 <- group_tab(tab_ego_1, type = "RD")

# save
here_save(group_tab_ego_1, "group_tab_ego_1")

# make group table
group_tab_reflective_1 <-
  group_tab(tab_reflective_1, type = "RD")

# save
here_save(group_tab_reflective_1, "group_tab_reflective_1")

# make group table
group_tab_social_1 <- group_tab(tab_social_1, type = "RD")

# save
here_save(group_tab_social_1, "group_tab_social_1")


group_tab_health_1 <- here_read("group_tab_health_1")
group_tab_body_1 <- here_read("group_tab_body_1")
group_tab_ego_1 <- here_read("group_tab_ego_1")
group_tab_reflective_1 <- here_read("group_tab_reflective_1")
group_tab_social_1 <- here_read("group_tab_social_1")

A
# create plots -------------------------------------------------------------

f
f1
# check N
N
sub_title_1 = "Forgiveness: shift exposure up by 1 point up to max 7, N = 34,749"

# graph health
plot_group_tab_health_1 <- margot_plot(
  group_tab_health_1,
  type = "RD",
  title = "Health effects",
  subtitle = sub_title_1,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 15,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_health+ plot_group_tab_health_1
dev.off()
# save graph
ggsave(
  plot_group_tab_health_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_health_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



# graph body
plot_group_tab_body_1 <- margot_plot(
  group_tab_body_1,
  type = "RD",
  title = "Body effects",
  subtitle = sub_title_1,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_group_tab_body + plot_group_tab_body_1
# save graph
ggsave(
  plot_group_tab_body_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_body_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)


plot_group_tab_body_1

# graph ego
plot_group_tab_ego_1 <- margot_plot(
  group_tab_ego_1,
  type = "RD",
  title = "Ego effects",
  subtitle = sub_title_1,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_ego_1


plot_group_tab_ego + plot_group_tab_ego_1

# save graph
ggsave(
  plot_group_tab_ego_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_ego_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

plot_group_tab_ego_1

# graph reflective
plot_group_tab_reflective_1 <- margot_plot(
  group_tab_reflective_1,
  type = "RD",
  title = "Reflective effects",
  subtitle = sub_title_1,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_reflective + plot_group_tab_reflective_1

# save graph
ggsave(
  plot_group_tab_reflective_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_reflective_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

# graph social
plot_group_tab_social_1 <- margot_plot(
  group_tab_social_1,
  type = "RD",
  title = "Social effects",
  subtitle = sub_title_1,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_group_tab_social_1

# save graph
ggsave(
  plot_group_tab_social_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_social_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()


# comparative intervention graphs -----------------------------------------

# combo graphs

plot_compare_health <- plot_group_tab_health + plot_group_tab_health_1 + plot_annotation(title = 
                                                                                           "Shift Intervention Comparisions", tag_level = "A")

plot_compare_health
ggsave(
  plot_compare_health,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_health.png",
  device = 'png',
  dpi = 600
)
dev.off()


plot_compare_body <- plot_group_tab_body + plot_group_tab_body_1  + plot_annotation(title = 
                                                                                      "Shift Intervention Comparisions", tag_level = "A")

plot_compare_body
ggsave(
  plot_compare_body,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_body.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()



plot_compare_ego <- plot_group_tab_ego + plot_group_tab_ego_1+ plot_annotation(title = 
                                                                                 "Shift Intervention Comparisions", tag_level = "A")


plot_compare_ego
ggsave(
  plot_compare_ego,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_ego.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()



plot_compare_reflective <- plot_group_tab_reflective + plot_group_tab_reflective_1+ plot_annotation(title = 
                                                                                                      "Shift Intervention Comparisions", tag_level = "A")

plot_compare_reflective
ggsave(
  plot_compare_reflective,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_reflective.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()




plot_compare_social  <-plot_group_tab_social + plot_group_tab_social_1+ plot_annotation(title = 
                                                                                          "Shift Intervention Comparisions", tag_level = "A")

plot_compare_social
ggsave(
  plot_compare_social,
  path = here::here(here::here(push_mods, "figs")),
  width = 25,
  height = 10,
  units = "in",
  filename = "plot_compare_social.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()




