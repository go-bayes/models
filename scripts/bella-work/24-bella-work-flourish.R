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

# obtain ids for individuals who participated in 2018 and have no missing baseline exposure
ids_baseline <- dat |>
  dplyr::filter(year_measured == 1, wave == 2020) |>
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

table1::table1( ~ modesty| wave, data = dat)


df_all_in  <- dat |>
  filter(year_measured == 1)
table1::table1(~ employed |wave, data = df_all_in)
ids_baseline


dat_long <- dat |>
  # dplyr::filter(id %in% ids_2018_2019 &
  #                 wave %in% c(2018, 2019, 2020)) |>
  dplyr::filter(id %in% ids_baseline &
                  wave %in% c(2020, 2021, 2022)) |>
  arrange(id, wave) |>
  dplyr::select(
    "id",
    "wave",
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
    # # see mini ipip6
    # # I want people to know that I am an important person of high status,
    # # I am an ordinary person who is no better than others.
    # # I wouldn’t want people to treat me as though I were superior to them.
    # # I think that I am entitled to more respect than the average person is
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
   # "sfhealth_your_health",
    # "In general, would you say your health is...
   # "sfhealth_get_sick_easier_reversed",
    #\nI seem to get sick a little easier than other people.
  #  "sfhealth_expect_worse_health_reversed",
    #\nI expect my health to get worse." ****
    "hlth_sleep_hours",
    #During the past month, on average, how many hours of actual sleep did you get per night?
    "smoker",
    #Do you currently smoke?
    "hlth_fatigue",
    #During the last 30 days, how often did.... you feel exhausted?
    "rumination",
    "kessler6_sum",
    # depression constructs,
    "kessler_latent_depression",
    "kessler_latent_anxiety",
    "sexual_satisfaction"
    "bodysat",
    ## Am satisfied with the appearance, size and shape of my body.
    "vengeful_rumin",
    # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
    "perfectionism",
    "power_no_control_composite",
    "self_esteem",
    "self_control_have_lots",
    #In general, I have a lot of self-control.
    "self_control_wish_more_reversed",
    "emotion_regulation_out_control",
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
  ) |>
  rename(short_form_health= sfhealth) |> 
  mutate(male = as.numeric(male)) |>
  mutate(
#    religion_church_binary = ifelse(religion_church > 0, 1, 0),
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
    # total_siblings_log = log(total_siblings + 1),
    # hours_community_log = log(hours_community + 1),
    # hours_friends_log  = log(hours_friends + 1),
    # hours_family_log = log(hours_family + 1)#,
    # children_num_log = log(children_num + 1)
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
    rural_gch_2018_l = as.numeric(as.character(rural_gch_2018_l)),
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
   # urban = as.numeric(as.character(urban)),
    parent = as.numeric(as.character(parent)),
    partner = as.numeric(as.character(partner)),
    born_nz = as.numeric(as.character(born_nz)),
    not_lost = as.numeric(as.character(not_lost)),
    employed = as.numeric(as.character(employed))
  ) |>
  arrange(id, wave) |>
  data.frame()|>
  droplevels()


n_participants <- skimr::n_unique(dat_long$id)

# get comma in number
n_participants <- margot::pretty_number(n_participants)

# check
n_participants

# save number for manuscrip
margot::here_save(n_participants, "n_participants")

# inspect data
skimr::skim(dat_long)

# checkk names
sort(colnames(dat_long))
# set baseline variables --------------------------------------------------

# for confounding control

baseline_vars = c(
  "male",
  "age",
  "education_level_coarsen",
  # factors
  "eth_cat",
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  #"bigger_doms", #religious denomination
  "sample_origin",
  "nz_dep2018",
  "nzsei_13_l",
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
 # "modesty",
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
  "power_no_control_composite",
  "self_esteem",
  "sexual_satisfaction",
  "self_control_have_lots",
  "self_control_wish_more_reversed",
  "emotion_regulation_out_control",
  "vengeful_rumin",
  "gratitude",
  "short_form_health",
  "pwb_your_relationships",
  "pwb_your_future_security",
  "pwb_standard_living",
  "lifesat",
  "meaning_purpose",# My life has a clear sense of purpose.
  "meaning_sense", # I have a good sense of what makes my life meaningful.
  "permeability_individual",
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
dt_baseline <- dat_long #|>
#  filter(wave == 2020)

# variables for the table
base_vars <- setdiff(baseline_vars, c("not_lost", "sample_weights", outcome_vars))


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
dt_baseline_19 <- dat_long |>
  dplyr::filter(wave == 2018 | wave == 2019) |>
  # we need to drop unused levels of the wave
  droplevels()

# get vars.
selected_exposure_cols <-
  dt_baseline_19 |> select(c("perfectionism", "wave"))

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


# outcome table -----------------------------------------------------------
dt_baseline_20 <- dat_long |>
  dplyr::filter(wave == 2018 | wave == 2020) |>
  droplevels()

names_outcomes_tab <- setdiff(outcome_vars, dt_baseline_20)
names_outcomes_sorted <- sort(names_outcomes_tab)
names_outcomes_final <- names_outcomes_sorted # consistent workflow

# better names if desirable
selected_outcome_cols <-
  dt_baseline_20 |> select(all_of(names_outcomes_final), wave)
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

# save
margot::here_save(table_outcomes, "table_outcomes")

# read if needed
table_outcomes <- margot::here_read("table_outcomes")
table_outcomes

# histogram of the exposure -----------------------------------------------
# select 2019 wave
dt_19 <- dat_long |> dplyr::filter(wave == 2019)

# mean of exposure
mean_exposure <- mean(dt_19$perfectionism, na.rm = TRUE)

# view
mean_exposure

# save
here_save(mean_exposure, "mean_exposure")

# check
mean_exposure

# sd of exposure
sd_exposure <- sd(dt_19$perfectionism, na.rm = TRUE)

# save
here_save(sd_exposure, "sd_exposure")

# check
# sd_exposure


# median
median_exposure <- median(dt_19$perfectionism, na.rm = TRUE)

median_exposure

# check if you like
# median_exposure
# mean_exposure
graph_density_shift_function <- margot::coloured_histogram(
  dt_19,
  col_name = "perfectionism",
  binwidth = .1,
  unit_of_change = 1,
  scale_min = 1,
  scale_max = 7,
  highlight_range = "hightest" # "lowest" if you want the lowest, "both" if both
)
graph_density_shift_function
margot::here_save(graph_density_shift_function,
                  "graph_density_shift_function")

# change in exposure ------------------------------------------------------
dt_baseline_19_positivity <- dat_long |>
  dplyr::filter(wave == 2018 |
                  wave == 2019) |>
  dplyr::mutate(perfectionism_round = round(perfectionism, digits = 0)) |>
  dplyr::select(perfectionism_round, id, wave) |>
  droplevels()

out <- margot::create_transition_matrix(data = dt_baseline_19_positivity,
                                        state_var = "perfectionism_round",
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

dt_baseline_regress <- dat_long |>
  filter(wave == 2018) |>
  mutate(
    #perfectionism_z = scale(perfectionism),
    kessler_latent_anxiety_z = scale(kessler_latent_anxiety),
    kessler_latent_depression_z = scale(kessler_latent_depression)
  )

# check
hist(dt_baseline_regress$kessler_latent_depression_z)
# check
hist(dt_baseline_regress$kessler_latent_anxiety_z)


# code from above
base_var <-
  setdiff(baseline_vars, c("not_lost", "sample_weights", outcome_vars))

# fit model
fit_kessler_latent_anxiety <-
  margot::regress_with_covariates(
    dt_baseline_regress,
    outcome = "kessler_latent_anxiety_z",
    exposure = "perfectionism",
    baseline_vars = base_var
  )

# view
parameters::model_parameters(fit_kessler_latent_anxiety, ci_method = "wald")[2, ]

# save results
here_save(fit_kessler_latent_anxiety, "fit_kessler_latent_anxiety")

# view
fit_kessler_latent_depression <-
  regress_with_covariates(
    dt_baseline_regress,
    outcome = "kessler_latent_depression_z",
    exposure = "perfectionism",
    baseline_vars = base_var
  )
# view model
parameters::model_parameters(fit_kessler_latent_depression, ci_method =
                               "wald")[2, ]

# save results
here_save(fit_kessler_latent_depression,
          "fit_kessler_latent_depression")


# calculate betas anxiety
lm_fit_kessler_latent_anxiety <- tbl_regression(fit_kessler_latent_anxiety)
here_save(lm_fit_kessler_latent_anxiety,
          "lm_fit_kessler_latent_anxiety")

# calculate betas depression
lm_fit_kessler_latent_depression <- tbl_regression(fit_kessler_latent_depression)
here_save(lm_fit_kessler_latent_depression,
          "lm_fit_kessler_latent_depression")

#
#
b_lm_fit_kessler_latent_anxiety <- inline_text(lm_fit_kessler_latent_anxiety,
                                               variable = perfectionism,
                                               pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")

# view
here_save(b_lm_fit_kessler_latent_anxiety,
          "b_lm_fit_kessler_latent_anxiety")

# #
b_lm_fit_kessler_latent_depression <-
  inline_text(lm_fit_kessler_latent_depression,
              variable = perfectionism,
              pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")

here_save(b_lm_fit_kessler_latent_depression,
          "b_lm_fit_kessler_latent_depression")


# impute missing values at baseline ---------------------------------------

exposure_vars <- c("perfectionism", "not_lost")
baseline_vars <- setdiff(baseline_vars, "sample_weights")

# check
baseline_vars

# here we imput the baseline
df_impute_base <- margot::margot_wide_impute_baseline(
  dat_long,
  baseline_vars = baseline_vars,
  exposure_var = exposure_vars,
  outcome_vars = outcome_vars
)

# save

# get sample weights back to data
dt_baseline <- dat_long |> filter(wave == 2018)

# add sample weights
df_impute_base$t0_sample_weights = dt_baseline$sample_weights

# save
here_save(df_impute_base, "df_impute_base")


# data wrangling for censoring weights ------------------------------------
df_impute_base <- here_read("df_impute_base")

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

# used if you did not censor at 2019
#t1_na_condition <- rowSums(is.na(select(df_wide_not_lost, starts_with("t2_")))) > 0

df_clean <- df_wide_not_lost |>
  mutate(t0_not_lost = ifelse(t0_na_condition, 0, t0_not_lost)) |>
  # mutate(t1_not_lost = ifelse(t1_na_condition, 0, t1_not_lost)) |>. # use if not_lost at t1
  # mutate(across(starts_with("t1_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
  #        across(starts_with("t2_"), ~ ifelse(t0_not_lost == 0, NA_real_, .))) |>
  # mutate(across(starts_with("t2_"), ~ ifelse(t1_not_lost == 0, NA_real_, .)))|>
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_not_lost &
        !t0_sample_weights &
        !t0_born_nz,
      #   !t1_perfectionism &  we will standardise perfectionism and use it later!t1_not_lost,
      .fns = ~ scale(.),
      .names = "{.col}_z"
    )
  ) |>
  # select(-t0_charity_donate,
  #        -t0_hours_charity) |>
  select(
    where(is.factor),
    t0_sample_weights,
    t0_born_nz,
    t0_not_lost,
    t1_perfectionism,
    t1_not_lost,
    ends_with("_z")
  ) |>
  mutate(t0_lost = 1 - t0_not_lost) |>
  mutate(t1_lost = 1 - t1_not_lost) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_"))

# check what we have
colnames(df_clean) # note this  "t1_perfectionism"               "t1_perfectionism_z"

# check again
naniar::vis_miss(df_clean, warn_large_data = FALSE)

# checks
table(df_clean$t1_lost)
table(df_clean$t0_lost)

# checks
table(df_clean$t0_not_lost)

# checks
test <- df_wide_not_lost |> filter(t0_not_lost == 1)
nrow(test)

#
# df_impute_base$t1_perfectionism_z = scale(df_impute_base$t1_perfectionism)

# get rid of attributes
df_clean <- margot::remove_numeric_attributes(df_clean)

# checks
str(df_clean)

# checks
nrow(df_clean)


# weights for treatment ----------------------------------------------------
baseline_vars_models = df_clean |>  # post process of impute and combine
  dplyr::select(starts_with("t0"), -t0_not_lost, -t0_lost, -t0_sample_weights) |> colnames() # note

# check this is correct.
baseline_vars_models <- c(baseline_vars_models)

# create fresh dataset
df_clean_pre <- df_clean[baseline_vars_models]

# checks
str(df_clean_pre)

# if this variable were not a factor, make sure it is
# df_clean_pre$t0_eth_cat <- as.factor(df_clean_pre$t0_eth_cat)


# perform one-hot encoding using model.matrix
# we need factors to be 0 or 1
encoded_vars <- model.matrix( ~ t0_eth_cat  - 1, data = df_clean_pre)
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
  select(-c(t0_eth_cat)) |>
  bind_cols(encoded_df)

# extract and print the new column names for encoded variables
new_encoded_colnames <- colnames(encoded_df)
print(new_encoded_colnames)


# get baseline variable set without factors
baseline_vars_set <- setdiff(names(df_clean_pre), c("t0_lost", "id", "t0_eth_cat"))

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
hist(df_clean_hot$weights)

# obtain stablise weights
marginal_not_lost <- mean(df_clean_hot$t0_lost)

# check (fyi)
marginal_not_lost


# stabalised weights
df_clean_hot$weights_stabilised <- ifelse(
  df_clean_hot$t0_lost == 1,
  marginal_not_lost / df_clean_hot$pscore,
  (1 - marginal_not_lost) / (1 - df_clean_hot$pscore)
)

# checks
hist(df_clean_hot$weights_stabilised)
max(df_clean_hot$weights_stabilised)
min(df_clean_hot$weights_stabilised)

# save output of hot code dataset
here_save(df_clean_hot, "df_clean_hot")

# get weights into the model
# new weights by combining censor and sample weights, using stabalised weights
df_clean$t0_combo_weights = df_clean_hot$weights_stabilised * df_clean$t0_sample_weights

# checks
min(df_clean$t0_combo_weights)
max(df_clean$t0_combo_weights)

# check distrobution of weights
hist(df_clean$t0_combo_weights)

# next remove those who were lost between t0 and t1
df_clean_t1 <- df_clean |> filter(t0_lost == 0) |>
  select(-t1_perfectionism_z, -t1_lost, -t0_lost, -t0_sample_weights) |>
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
table(is.na(df_clean_t1$n_not_lost_sample)) # none

# gets us the correct df for weights

# check column oder and missing ness
naniar::vis_miss(df_clean_t1, warn_large_data = FALSE)

#check
nrow(df_clean_t1)

# next get data for t1
hist(df_clean_t1$t0_combo_weights)

# get correct censoring -----------------------------------------
# THIS CODE IS redundant but NO HARM DONE
t0_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t1_")))) > 0

t1_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t2_")))) > 0
# baseline_vars
# df_impute_base$t0_sample_weights


df_clean_t2 <- df_clean_t1 |>
  # select(-t0_alert_level_combined_lead) |>
  mutate(t0_not_lost = ifelse(t0_na_condition, 0, t0_not_lost)) |>
  mutate(t1_not_lost = ifelse(t1_na_condition, 0, t1_not_lost)) |>
  mutate(across(starts_with("t1_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_not_lost == 0, NA_real_, .))) |>
  mutate(across(starts_with("t2_"), ~ ifelse(t1_not_lost == 0, NA_real_, .))) |>
  # mutate(t0_lost = 1 - t0_not_lost) |>
  mutate(t1_lost = 1 - t1_not_lost) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  select(-t1_lost, -t0_not_lost)

## END REDUNDANT
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

# copy data and make the group to be compared a factor
df_sub <- df_clean_t2

# make factor
df_sub$t0_born_nz <- as.factor(df_clean_t2$t0_born_nz)

# make propensity score model. Need correct covariates
baseline_vars_models = df_clean_t2 |>
  dplyr::select(starts_with("t0"), -t0_combo_weights) |> colnames()

# check
baseline_vars_models

# needed for subgroup ps
baseline_vars_models_sans_group <- setdiff(baseline_vars_models, "t0_born_nz")


# equation string
string <- formula_str <- as.formula(paste(
  "t1_perfectionism",
  "~",
  paste(baseline_vars_models, collapse = "+")
))

# equation string for subgroup analysis
string_sans <- formula_str <- as.formula(paste(
  "t1_perfectionism",
  "~",
  paste(baseline_vars_models_sans_group, collapse = "+")
))


# iptw marginal analysis
iptw_marginal  <- WeightIt::weightit(
  string,
  method = "ebal",
  estimand = "ATE",
  weights = "t0_combo_weights",
  #focal = "set",
  data = df_clean_t2
)
summary_iptw_marginal <- summary(iptw_marginal)
here_save(summary_iptw_marginal, "summary_iptw_marginal")


# note any extreme weights
plot(summary(iptw_marginal))

# save model
here_save(iptw_marginal, "iptw_marginal")


# iptw conditional analysis  won't work
# no diff in the groups
# iptw_conditional <- weightit(
#   string_sans,
#   method = "ebal",
#   estimand = "ATE",
#   by = "t0_born_nz",
#   weights = "t0_combo_weights",
#   #focal = "set", # if att
#   data = df_sub
# )
#
# # view
# summary(iptw_conditional)

# save model
# here_save(iptw_conditional, "iptw_conditional")
# summary_iptw_conditional <- summary(iptw_conditional)
# here_save(summary_iptw_conditional, "summary_iptw_conditional")



colnames(df_nz)

# visualise imbalance
love_plot_marginal <-
  love.plot(
    iptw_marginal,
    binary = "std",
    thresholds = c(m = .1),
    wrap = 50,
    position = "bottom",
    size = 3
  )

# view
love_plot_marginal

# save for manuscript
here_save(love_plot_marginal, "love_plot_marginal")
#
#love_plot_conditional
# love_plot_conditional <-
#   love.plot(
#     iptw_conditional,
#     cluster = "t0_born_nz",
#     binary = "std",
#     thresholds = c(m = .1),
#     wrap = 50,
#     position = "bottom",
#     size = 2
#   )
# love_plot_conditional



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
encoded_vars <- model.matrix( ~ t0_eth_cat  - 1, data = df_clean_t2)


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
  select(-c(t0_eth_cat)) |>
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

# check
set_final_names

# set names for analysis
set_final_names <-
  df_clean_hot_t2 |> select(starts_with("t0"), -t0_combo_weights) |> colnames()


# add the new encoded column names

# check
full_predictor_vars_t2

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
set.seed(0112358)
plan(multisession)

# hopefully you have 10 :). if not, consider decreasing the number of folds (for this assessment)
n_cores <- parallel::detectCores() - 2


#### SET VARIABLE NAMES
#  model
A <- c("t1_perfectionism")
C <- c("t1_not_lost")
W <- set_final_names


# get max value of data
# make data_final
df_final <- df_clean_hot_t2


# for later use
here_save(df_final, "df_final")
here_save(W, "W")


# last checks
colnames(df_final)
naniar::vis_miss(df_final, warn_large_data = F)



#  write function(s) for shift(s) ------------------------------------------------------
# get enpoints
max_data <- max(df_final$t1_perfectionism)


gain_A <- function(data, trt) {
  ifelse(data[[trt]] < max_data, data[[trt]], max_data)
}

# shift function
gain_A <- function(data, trt) {
  ifelse(data[[trt]] < max_data - 1, data[[trt]] + 1, max_data)
}

# loss_A <- function(data, trt) {
#   ifelse(data[[trt]] > min_scale + 1, data[[trt]] - 1, min_scale)
# }



# changing your function to be fixed at 7 if you like...
fixed_shift_to_7 <- function(data, trt) {
  ifelse(data[[trt]] != 7, 7, data[[trt]])
}

# changing your function to be fixed at 0 if you like...
fixed_shift_to_0 <- function(data, trt) {
  ifelse(data[[trt]] != 0, 0, data[[trt]])
}


# set libraries
sl_lib <- c("SL.glmnet", "SL.ranger", #
            "SL.xgboost") #

# view superlearners
listWrappers()

# test data
df_clean_slice <- df_final |>
  slice_head(n = 1000) |>
  as.data.frame()



# Models!
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
  learners_trt = sl_lib,
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

# view
t2_kessler_latent_anxiety_z_test

# view learner performance
t2_kessler_latent_anxiety_z_test$fits_r
t2_kessler_latent_anxiety_z_test$fits_m

# save
here_save(t2_kessler_latent_anxiety_z_null_test,
          "t2_kessler_latent_anxiety_z_null_test")

# test contrast
test_contrast_anxiety <- lmtp::lmtp_contrast(t2_kessler_latent_anxiety_z_test , ref = t2_kessler_latent_anxiety_z_null_test)

# check
test_contrast_anxiety


# tests look good, let's run the model ------------------------------------
# models ------------------------------------------------------------------


# anxiety -- marginal -----------------------------------------------------
t2_kessler_latent_anxiety_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = gain_A,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# checks
t2_kessler_latent_anxiety_z_gain$fits_r
t2_kessler_latent_anxiety_z_gain$fits_m

# view
t2_kessler_latent_anxiety_z_gain

# save model
here_save(t2_kessler_latent_anxiety_z_gain,
          "t2_kessler_latent_anxiety_z_gain")


# null model
t2_kessler_latent_anxiety_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = NULL,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
t2_kessler_latent_anxiety_z_null

# save model
here_save(t2_kessler_latent_anxiety_z_null,
          "t2_kessler_latent_anxiety_z_null")

# depression marginal  ----------------------------------------------------
t2_kessler_latent_depression_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W,
  shift = gain_A,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


# view
t2_kessler_latent_depression_z_gain

# save model
here_save(t2_kessler_latent_depression_z_gain,
          "t2_kessler_latent_depression_z_gain")


# null model
t2_kessler_latent_depression_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W,
  shift = NULL,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
t2_kessler_latent_depression_z_null

# save model
here_save(t2_kessler_latent_depression_z_null,
          "t2_kessler_latent_depression_z_null")



# subgroup models ---------------------------------------------------------
# df_final <- here_read("df_final") # if needed


dt_baseline <- dat_long |>
  filter(wave == 2018)

## in baseline sample
df_18 |> dplyr::filter(dplyr::filter(born_nz == 0)) |> droplevels()

# select participant n at basel
n_baseline_participants_born_nz_no <- dt_baseline |> dplyr::filter(born_nz == 0) |> droplevels()
n_baseline_participants_born_nz_yes <- dt_baseline |> dplyr::filter(born_nz == 1) |> droplevels()

n_baseline_participants_born_overseas <- nrow(n_baseline_participants_born_nz_no)
n_baseline_participants_born_nz <- nrow (n_baseline_participants_born_nz_yes)


# make pretty
n_baseline_participants_born_overseas <-   prettyNum(n_baseline_participants_born_overseas, big.mark = ",")
n_baseline_participants_born_nz <-
  prettyNum(n_baseline_participants_born_nz, big.mark = ",")

# save
here_save(n_baseline_participants_born_overseas,
          "n_baseline_participants_born_overseas")
here_save(n_baseline_participants_born_nz,
          "n_baseline_participants_born_nz")


# sanity checks
n_participants_born_nz
n_participants_born_overseas

here_save(n_participants_born_nz, "n_participants_born_nz")
here_save(n_participants_born_overseas,
          "n_participants_born_overseas")



##
df_born_nz_no  <- df_final |> dplyr::filter(t0_born_nz == 0) |> droplevels()
df_born_nz_yes <- df_final |> dplyr::filter(t0_born_nz == 1)  |> droplevels()

# checks
n_participants_born_nz <- nrow(df_born_nz_yes)
n_participants_born_overseas <- nrow(df_born_nz_no)



# make pretty
n_participants_born_nz <-
  prettyNum(n_participants_born_nz, big.mark = ",")
n_participants_born_overseas <-
  prettyNum(n_participants_born_overseas, big.mark = ",")

# sanity checks
n_participants_born_nz
n_participants_born_overseas

here_save(n_participants_born_nz, "n_participants_born_nz")
here_save(n_participants_born_overseas,
          "n_participants_born_overseas")

# need to remove the "born nz" name as we are stratifying
W_sub <- setdiff(W, "t0_born_nz")

# check
W_sub



# subgroup-  born overseas ------------------------------------------------

# anxiety -- born overseas -----------------------------------------------------
df_born_nz_no_t2_kessler_latent_anxiety_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W_sub,
  shift = gain_A,
  data = df_born_nz_no,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_no$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


# view
# save model
here_save(
  df_born_nz_no_t2_kessler_latent_anxiety_z_gain,
  "df_born_nz_no_t2_kessler_latent_anxiety_z_gain"
)


# null model
df_born_nz_no_t2_kessler_latent_anxiety_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W_sub,
  shift = NULL,
  data = df_born_nz_no,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_no$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
df_born_nz_no_t2_kessler_latent_anxiety_z_null

# save model
here_save(
  df_born_nz_no_t2_kessler_latent_anxiety_z_null,
  "df_born_nz_no_t2_kessler_latent_anxiety_z_null"
)

# depression born overseas  ----------------------------------------------------
df_born_nz_no_t2_kessler_latent_depression_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W_sub,
  shift = gain_A,
  data = df_born_nz_no,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_no$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


# view
df_born_nz_no_t2_kessler_latent_depression_z_gain

# save model
here_save(
  df_born_nz_no_t2_kessler_latent_depression_z_gain,
  "df_born_nz_no_t2_kessler_latent_depression_z_gain"
)


# null model
df_born_nz_no_t2_kessler_latent_depression_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W_sub,
  shift = NULL,
  data = df_born_nz_no,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_no$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
df_born_nz_no_t2_kessler_latent_depression_z_null

# save model
here_save(
  df_born_nz_no_t2_kessler_latent_depression_z_null,
  "df_born_nz_no_t2_kessler_latent_depression_z_null"
)


# subgroup born nz --------------------------------------------------------
# subgroup-  born nz ------------------------------------------------

# anxiety -- born nz -----------------------------------------------------
df_born_nz_yes_t2_kessler_latent_anxiety_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W_sub,
  shift = gain_A,
  data = df_born_nz_yes,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_yes$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
df_born_nz_yes_t2_kessler_latent_anxiety_z_gain

# save model
here_save(
  df_born_nz_yes_t2_kessler_latent_anxiety_z_gain,
  "df_born_nz_yes_t2_kessler_latent_anxiety_z_gain"
)


# null model
df_born_nz_yes_t2_kessler_latent_anxiety_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W_sub,
  shift = NULL,
  data = df_born_nz_yes,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_yes$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


# view
df_born_nz_yes_t2_kessler_latent_anxiety_z_null

# save model
here_save(
  df_born_nz_yes_t2_kessler_latent_anxiety_z_null,
  "df_born_nz_yes_t2_kessler_latent_anxiety_z_null"
)



# depression born nz  ----------------------------------------------------
df_born_nz_yes_t2_kessler_latent_depression_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W_sub,
  shift = gain_A,
  data = df_born_nz_yes,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_yes$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


# view
df_born_nz_yes_t2_kessler_latent_depression_z_gain

# save model
here_save(
  df_born_nz_yes_t2_kessler_latent_depression_z_gain,
  "df_born_nz_yes_t2_kessler_latent_depression_z_gain"
)


# null model
df_born_nz_yes_t2_kessler_latent_depression_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W_sub,
  shift = NULL,
  data = df_born_nz_yes,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_yes$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
df_born_nz_yes_t2_kessler_latent_depression_z_null

# save model
here_save(
  df_born_nz_yes_t2_kessler_latent_depression_z_null,
  "df_born_nz_yes_t2_kessler_latent_depression_z_null"
)




# compute contrasts -------------------------------------------------------

# import marginal models
t2_kessler_latent_anxiety_z_gain <- margot::here_read("t2_kessler_latent_anxiety_z_gain")
t2_kessler_latent_anxiety_z_null <- margot::here_read("t2_kessler_latent_anxiety_z_null")
t2_kessler_latent_depression_z_gain <- margot::here_read("t2_kessler_latent_depression_z_gain")
t2_kessler_latent_depression_z_null <- margot::here_read("t2_kessler_latent_depression_z_null")


# contrast marginal anxiety
contrast_t2_kessler_latent_anxiety_z <-
  lmtp_contrast(t2_kessler_latent_anxiety_z_gain,
                ref =  t2_kessler_latent_anxiety_z_null,
                type = "additive")

# save for manuscript
here_save(contrast_t2_kessler_latent_anxiety_z,
          "contrast_t2_kessler_latent_anxiety_z")

# make table
tab_contrast_t2_kessler_latent_anxiety_z <- margot::margot_lmtp_evalue(contrast_t2_kessler_latent_anxiety_z,
                                                                       scale = "RD",
                                                                       new_name = "marginal: anxiety")

#view
tab_contrast_t2_kessler_latent_anxiety_z

# contrast marginal depression
contrast_t2_kessler_latent_depression_z <-
  lmtp_contrast(t2_kessler_latent_depression_z_gain,
                ref =  t2_kessler_latent_depression_z_null,
                type = "additive")

# save for manuscript

here_save(
  contrast_t2_kessler_latent_depression_z,
  "contrast_t2_kessler_latent_depression_z"
)



# make table
tab_contrast_t2_kessler_latent_depression_z <- margot::margot_lmtp_evalue(contrast_t2_kessler_latent_depression_z,
                                                                          scale = "RD",
                                                                          new_name = "marginal: depression")

# view
tab_contrast_t2_kessler_latent_depression_z

# import born overseas models
df_born_nz_no_t2_kessler_latent_anxiety_z_gain <- margot::here_read("df_born_nz_no_t2_kessler_latent_anxiety_z_gain")
df_born_nz_no_t2_kessler_latent_anxiety_z_null <- margot::here_read("df_born_nz_no_t2_kessler_latent_anxiety_z_null")
df_born_nz_no_t2_kessler_latent_depression_z_gain <- margot::here_read("df_born_nz_no_t2_kessler_latent_depression_z_gain")
df_born_nz_no_t2_kessler_latent_depression_z_null <- margot::here_read("df_born_nz_no_t2_kessler_latent_depression_z_null")

# contrast born overseas anxiety
contrast_df_born_nz_no_t2_kessler_latent_anxiety_z <-
  lmtp_contrast(df_born_nz_no_t2_kessler_latent_anxiety_z_gain,
                ref =  df_born_nz_no_t2_kessler_latent_anxiety_z_null,
                type = "additive")

# make table
tab_contrast_df_born_nz_no_t2_kessler_latent_anxiety_z <- margot::margot_lmtp_evalue(
  contrast_df_born_nz_no_t2_kessler_latent_anxiety_z,
  scale = "RD",
  new_name = "born overseas: anxiety"
)

# view
tab_contrast_df_born_nz_no_t2_kessler_latent_anxiety_z

# contrast marginal depression
contrast_df_born_nz_no_t2_kessler_latent_depression_z <-
  lmtp_contrast(
    df_born_nz_no_t2_kessler_latent_depression_z_gain,
    ref =  df_born_nz_no_t2_kessler_latent_depression_z_null,
    type = "additive"
  )

# make table
tab_contrast_df_born_nz_no_t2_kessler_latent_depression_z <- margot::margot_lmtp_evalue(
  contrast_df_born_nz_no_t2_kessler_latent_depression_z,
  scale = "RD",
  new_name = "born overseas: depression"
)


#view
tab_contrast_df_born_nz_no_t2_kessler_latent_depression_z

# import born nz models
df_born_nz_yes_t2_kessler_latent_anxiety_z_gain <- margot::here_read("df_born_nz_yes_t2_kessler_latent_anxiety_z_gain")
df_born_nz_yes_t2_kessler_latent_anxiety_z_null <- margot::here_read("df_born_nz_yes_t2_kessler_latent_anxiety_z_null")
df_born_nz_yes_t2_kessler_latent_depression_z_gain <- margot::here_read("df_born_nz_yes_t2_kessler_latent_depression_z_gain")
df_born_nz_yes_t2_kessler_latent_depression_z_null <- margot::here_read("df_born_nz_yes_t2_kessler_latent_depression_z_null")


# contrast born nz anxiety
contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z <-
  lmtp_contrast(df_born_nz_yes_t2_kessler_latent_anxiety_z_gain,
                ref =  df_born_nz_yes_t2_kessler_latent_anxiety_z_null,
                type = "additive")

# make table
tab_contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z <- margot::margot_lmtp_evalue(
  contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z,
  scale = "RD",
  new_name = "born NZ: anxiety"
)

# view
tab_contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z

# contrast born nz depression
contrast_df_born_nz_yes_t2_kessler_latent_depression_z <-
  lmtp_contrast(
    df_born_nz_yes_t2_kessler_latent_depression_z_gain,
    ref =  df_born_nz_yes_t2_kessler_latent_depression_z_null,
    type = "additive"
  )

# make table
tab_contrast_df_born_nz_yes_t2_kessler_latent_depression_z <- margot::margot_lmtp_evalue(
  contrast_df_born_nz_yes_t2_kessler_latent_depression_z,
  scale = "RD",
  new_name = "born NZ: depression"
)

# view
tab_contrast_df_born_nz_yes_t2_kessler_latent_depression_z


# make tables -------------------------------------------------------------

# marginal tables
tab_marginal_outcomes <- rbind(
  tab_contrast_t2_kessler_latent_anxiety_z,
  tab_contrast_t2_kessler_latent_depression_z
)

# save
margot::here_save(tab_marginal_outcomes, "tab_marginal_outcomes")
# born overseas tables

# table with evalues for the graph
group_tab_marginal_outcomes <- margot::group_tab(tab_marginal_outcomes, type = "RD")

# view
group_tab_marginal_outcomes

# save
margot::here_save(group_tab_marginal_outcomes, "group_tab_marginal_outcomes")


# subgroups
tab_born_overseas_outcomes <-
  rbind(
    tab_contrast_df_born_nz_no_t2_kessler_latent_anxiety_z,
    tab_contrast_df_born_nz_no_t2_kessler_latent_depression_z
  )

# save
margot::here_save(tab_born_overseas_outcomes, "tab_born_overseas_outcomes")
tab_born_overseas_outcomes

# table with evalues for the graph
group_tab_born_overseas_outcomes <- margot::group_tab(tab_born_overseas_outcomes, type = "RD")

# view
group_tab_born_overseas_outcomes

# save
margot::here_save(group_tab_born_overseas_outcomes,
                  "group_tab_born_overseas_outcomes")



# born nz tables
tab_born_nz_outcomes <-
  rbind(
    tab_contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z,
    tab_contrast_df_born_nz_yes_t2_kessler_latent_depression_z
  )

# save
margot::here_save(tab_born_nz_outcomes , "tab_born_nz_outcomes")


# table with evalues for the graph
group_tab_born_nz_outcomes <- margot::group_tab(tab_born_nz_outcomes, type = "RD")

# save
margot::here_save(group_tab_born_nz_outcomes, "group_tab_born_nz_outcomes")


# view all
group_tab_marginal_outcomes
group_tab_born_overseas_outcomes
group_tab_born_nz_outcomes




# make graphs -------------------------------------------------------------

group_tab_marginal_outcomes
group_tab_not_born_overseas_outcomes
group_tab_born_nz_outcomes


n_participants

title = "Marginal Effect of One Point Increase in Perfectionism on Distress"
title_null = "N = 19,735, New Zealand Attitudes and Values Study (synthetic data)"


plot_group_tab_marginal_outcomes <- margot_plot(
  group_tab_marginal_outcomes,
  type = "RD",
  title =  "Perfectionism + One vs No Intervention ",
  subtitle = "on Depression Anxiety",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = 4,
  title_size = 20,
  subtitle_size = 14,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.5,
  x_lim_lo = -.5,
  x_lim_hi = .5
)

plot_group_tab_marginal_outcomes

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


# do the subgroup graphs here: --------------------------------------------





# subgroup comparisons ----------------------------------------------------

contrast_anxiety_yes <- contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z
contrast_anxiety_no <- contrast_df_born_nz_no_t2_kessler_latent_anxiety_z

contrast_anxiety_yes
contrast_anxiety_no

contrast_depression_yes <- contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z
contrast_depression_no <- contrast_df_born_nz_no_t2_kessler_latent_anxiety_z
# first calculate the difference in the means
g_hat_theta <- contrast_yes$vals$theta - contrast_no$vals$theta

# then calculate the se_diff

sqrt((contrast_yes$vals$std.error ^ 2) + (contrast_no$vals$std.error ^ 2))


# obtain difference
se_diff = sqrt((contrast_yes$vals$std.error ^ 2) + (contrast_no$vals$std.error ^ 2))

# compute confidence intervals
conf_low = g_hat_theta - (1.97 * se_diff)
conf_high = g_hat_theta + (1.97 * se_diff)


out <- cbind.data.frame(g_hat_theta, se_diff, conf_low, conf_high)

out <- round(out, 4)

out

# we can write a function to do this for us

compute_difference_means <- function(group1, group2) {
  # extract means and standard errors from each group
  mean_A <- group1$vals$theta
  mean_B <- group2$vals$theta
  se_A <- group1$vals$std.error
  se_B <- group2$vals$std.error
  
  #compute difference in means and standard error of the difference
  mean_difference <- mean_A - mean_B
  se_diff <- sqrt(se_A ^ 2 + se_B ^ 2)
  
  # compute 95% confidence intervals (using 1.96 for Z-value)
  conf_low <- mean_difference - (1.96 * se_diff)
  conf_high <- mean_difference + (1.96 * se_diff)
  
  # create output data frame and round the results
  out <- data.frame(
    mean_difference = round(mean_difference, 4),
    std_error = round(se_diff, 4),
    conf_low = round(conf_low, 4),
    conf_high = round(conf_high, 4)
  )
  
  return(out)
}


# use function
difference_in_group_means_anxiety  <- margot::compute_difference_means(contrast_anxiety_yes, contrast_anxiety_no)
difference_in_group_means_anxiety
here_save(difference_in_group_means_anxiety,
          "difference_in_group_means_anxiety")


difference_in_group_means_depression  <- margot::compute_difference_means(contrast_depression_yes, contrast_depression_no)
difference_in_group_means_depression
here_save(difference_in_group_means_depression,
          "difference_in_group_means_depression")




## use glue in your document as follows
glue::glue(
  "The difference in means is {difference_in_group_means$mean_difference} with a 95% CI of [{difference_in_group_means$conf_low}, {difference_in_group_means$conf_high}]."
)






# HETEROGENEITY -----------------------------------------------------------




# IGNORE BELOW -- THIS IS EXTRA FOR INVESTIGATING HETEROGENEITY -----------

# EXTRA: heterogeneity with GRF -------------------------------------------
# see: https://grf-labs.github.io/grf/
#devtools::install_github("grf-labs/grf", subdir = "r-package/grf")
library(grf)

# read data
colnames (df_final)

# read data
df_grf <- here_read("df_final")

# get baseline names
names_grf <- here_read("W")


# check all indicators are numeric or binary
colnames(df_grf)
str(df_grf). # we won't use "id"

# sample weights
t0_combo_weights <- df_grf$t0_combo_weights

table(df_grf$t1_not_lost)

# get censoring indicator, note that "not_lost" has the
# **opposite meaning in lmtp models!  we need to make D = "not_lost"
t1_lost = 1 - df_grf$t1_not_lost


#check
table(t1_lost)

# add to grf
df_grf$t1_lost <- t1_lost

# label this D
D <- as.factor (1 - df_grf$t1_not_lost)

# get key data features
nrow(df_grf)

#names_grf

# use standard deviation units for exposure, so that +1 = +1 SD units
t1_perfectionism_z <- scale(df_grf$t1_perfectionism)


# select exposure
selected_A = matrix(df_grf$t1_perfectionism_z) # standard deviation of exposure
selected_Y = matrix(df_clean_hot$t2_kessler_latent_anxiety_z)


# select covariates, make sure to remove attributes (we did this above)
cen_X <- cbind(df_grf[names_grf], t1_perfectionism_z)


# predict censoring
cen_forest <- probability_forest(cen_X, D)


# generate predictions
predictions_grf <- predict(cen_forest, newdata = cen_X, type = "response")
predictions_grf
# extract predictions from the 'pred' component and ensure it's a vector
pscore <- predictions_grf$pred[, 2]

hist(pscore)
mean(pscore)
sd(pscore)

df_grf$pscore <- pscore

# make censoring weights
df_grf$cen_weights <- ifelse(t1_lost == 1, 1 / pscore, 1 / (1 - pscore))

# view
hist(df_grf$cen_weights, breaks = 50)
# check
hist(df_grf$cen_weights)

# obtain stablise weights
marginal_not_lost <- mean(df_grf$t1_lost)

marginal_not_lost

df_grf$t1_lost

# stabalised weights
df_grf$weights_stabilised <- ifelse(
  df_grf$t1_lost == 1,
  marginal_not_lost / df_grf$pscore,
  (1 - marginal_not_lost) / (1 - df_grf$pscore)
)


# checks
hist(df_grf$weights_stabilised, breaks = 50)

max(df_grf$weights_stabilised)
min(df_grf$weights_stabilised)



# check

# set up data
df_grf$t1_not_lost = 1 - df_grf$t1_not_lost


# set up superlearner
cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification


# Set up parallel back end
no_cores <- detectCores()
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)


# you can probably just use "SL.glmnet"
match_lib = c("SL.glmnet", "SL.xgboost", "SL.ranger")


sl_2 <- SuperLearner(
  Y = df_grf$t1_not_lost,
  X = cen_X,
  # use specified predictors
  SL.library = match_lib,
  family = binomial(),
  method = "method.NNloglik",
  cvControl = list(V = 10)
)



# save your super learner model
here_save(sl_2, "sl_2")


# stop the cluster
stopCluster(cl)


# check outputs
print(sl_2)                  # summary of the SuperLearner output
summary(sl_2)                # a detailed summary, including cross-validated risks

# examination of cross-validated performance
sl_2$cvRisk                  # cross-validated risks for each learner
sl_2$coef                    # weights assigned to each learner in the final ensemble



# generate predictions
predictions_super <- predict(sl_2, newdata = cen_X, type = "response")


# very similar to grf
mean(predictions_super$pred[, 1])
sd(predictions_super$pred[, 1])


# extract predictions from the 'pred' component and ensure it's a vector
df_grf$super_pscore <- predictions_super$pred[, 1]

# check the structure of the predictions
str(df_grf$super_pscore)

# check pscore
hist(df_grf$super_pscore)

# make censoring weights
df_grf$super_weights <- ifelse(t1_lost == 1, 1 / df_grf$super_pscore, 1 / (1 - df_grf$super_pscore))

# check
hist(df_grf$super_weights, breaks = 50)


# stabalise
df_grf$weights_stabilised_super <- ifelse(
  df_grf$t1_lost == 1,
  marginal_not_lost / df_grf$super_pscore,
  (1 - marginal_not_lost) / (1 - df_grf$super_pscore)
)



# checks
hist(df_grf$weights_stabilised_super , breaks = 50)
max(df_grf$weights_stabilised_super)
min(df_grf$weights_stabilised_super)

# compare with causal forest
hist(df_grf$weights_stabilised , breaks = 50)
max(df_grf$weights_stabilised)
min(df_grf$weights_stabilised)


# lets use superlearner (it gives us forests and more)


# ok for combo weights to be labelled t0 because we just have a point estimate
df_grf$t0_combo_weights_w2 <- df_grf$weights_stabilised_super  * df_grf$t0_combo_weights

hist(df_grf$t0_combo_weights_w2 , breaks = 50)
max(df_grf$t0_combo_weights_w2)
min(df_grf$t0_combo_weights_w2)

colnames(df_grf)
here_save(df_grf, "df_grf")

df_grf_t2 <- df_grf |>
  filter(t1_not_lost == 1) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate(starts_with("t1_"), .before = starts_with("t2_")) |>
  relocate("t1_not_lost", .before = starts_with("t2_"))

colnames(df_grf_t2)


# save data with weights
here_save(df_grf, "df_grf")

summary(df_grf$t1_perfectionism)
#  make treatment binary
df_grf_t2 <- df_grf_t2 |>
  mutate(t1_perfectionism_binary = as.integer(ifelse(t1_perfectionism > 4, 1, 0)))

# variables need to be in matrix form

# make it so that large is good
g_W  = matrix(1 - df_grf_t2$t1_perfectionism_z)
t2_kessler_latent_depression_z = matrix(df_grf_t2$t2_kessler_latent_depression_z)

# make it so that large is good
g_Y = matrix(1 - df_grf_t2$t2_kessler_latent_anxiety_z)

# set binary exposure so that large is absence of perfectionism
g_W_binary = matrix(1 - df_grf_t2$t1_perfectionism_binary)


g_weights <- df_grf_t2$t0_combo_weights_w2

# checks
str(g_W)
str(g_X)
str(g_W)
str(g_Y)
str(g_weights)
g_Y
# model anxiety
tau_forest_t2_kessler_latent_anxiety_z <- grf::causal_forest(
  X = g_X,
  Y = g_Y,
  W = g_W,
  sample.weights = g_weights
)

# save
here_save(
  tau_forest_t2_kessler_latent_anxiety_z,
  'tau_forest_t2_kessler_latent_anxiety_z'
)


# view
tau_forest_t2_kessler_latent_anxiety_z

# ATE
anxiety_forest_ate <- average_treatment_effect(tau_forest_t2_kessler_latent_anxiety_z, target.sample = "all")


# save
here_save(anxiety_forest_ate, "anxiety_forest_ate")

# check out
anxiety_forest_ate

# model anxiety
tau_forest_anxiety_binary <- grf::causal_forest(
  X = g_X,
  Y = g_Y,
  W = g_W_binary,
  sample.weights = g_weights
)

# save
here_save(tau_forest_anxiety_binary, 'tau_forest_anxiety_binary')


# ATE
bin_anxiety_forest_ate <- average_treatment_effect(tau_forest_anxiety_binary, target.sample = "overlap")
bin_anxiety_forest_ate

# save
here_save(bin_anxiety_forest_ate, "bin_anxiety_forest_ate")


# not possible with a continuous treatement, but this is the code for binary treatments
bin_anxiety_att <- average_treatment_effect(tau_forest_anxiety_binary, target.sample = "treated")
bin_anxiety_att

# get a histogram that shows heterogeniety
tau.hat.oob <- predict(tau_forest_anxiety_binary)

# show
hist(tau.hat.oob$predictions)

# description of heterogeneity
best_linear_projection(tau_forest_anxiety_binary, g_X)

names_grf
# this only works for binary treatments
rate <- rank_average_treatment_effect(tau_forest_anxiety_binary, g_X[, "t0_eth_cateuro"])

#
plot(rate, ylab = "Euro", main = "TOC: ranked by decreasing weight")

# #
# forest.W <- regression_forest(g_X, g_W, tune.parameters = "all")
# #
# W.hat <- predict(forest.W)$predictions
#
# #
# forest.Y <- regression_forest(g_X, g_Y, tune.parameters = "all")
#
# #
# Y.hat <- predict(forest.Y)$predictions


forest.Y.varimp <- variable_importance(tau_forest_anxiety_binary)
#forest.Y.varimp
selected.vars <- which(forest.Y.varimp / mean(forest.Y.varimp) > 0.99)
selected.vars
colnames(g_X)

# obtain treatment effect in the most important predictors
tau.forest <- causal_forest(
  g_X[, selected.vars],
  g_Y,
  g_W_binary,
  W.hat = W.hat,
  Y.hat = Y.hat,
  tune.parameters = "all",
  sample.weights = g_weights
)

# not must different
average_treatment_effect(tau.forest, target.sample = "all")


# training sample
n <- nrow(g_X) # n in sample

set.seed(123)
train <- sample(1:n, n / 2) # get half sampl
train

# training sample
train.forest <- causal_forest(g_X[train, ], g_Y[train], g_W_binary[train], sample.weights = g_weights[train])

# eavaluation sample
eval.forest <- causal_forest(g_X[-train, ], g_Y[-train], g_W_binary[-train], sample.weights = g_weights[-train])

# rank on new data (ony supports binary treatment effects)
rate <- rank_average_treatment_effect(eval.forest, predict(train.forest, g_X[-train, ])$predictions)
plot(rate)

average_treatment_effect(train.forest, target.sample = "treated")
average_treatment_effect(eval.forest, target.sample = "treated")


#tau.hat <- predict(tau.forest, X.test, estimate.variance = TRUE)
# paste("AUTOC:", round(rate$estimate, 2), "+/", round(1.96 * rate$std.err, 2))


##
library(policytree)
library(DiagrammeR)

# get ate
ate <- average_treatment_effect(tau_forest_anxiety_binary)

# check for overlap
hist(tau_forest_anxiety_binary$W.hat)

# quick eval
varimp <- variable_importance(tau_forest_anxiety_binary)
varimp
ranked.vars <- order(varimp, decreasing = TRUE)
ranked.vars


# access the column names from your data frame using these indices
ranked.cols <- colnames(g_X)[ranked.vars]

# display the ordered
ranked.cols


# not much evidence for heterogeneity!
best_linear_projection(tau_forest_t2_kessler_latent_anxiety_z, g_X[ranked.vars[1:5]])


# Compute doubly robust scores
# dr.scores <- grf::get_scores(tau_forest_anxiety_binary)


# will only work for binary variables
dr.scores <- double_robust_scores(tau_forest_anxiety_binary)
dr.scores

# # Use as the ATE as a "cost" of program treatment to find something non-trivial
# cost <- ate[["estimate"]]
# cost
# -dr.scores
#
# dr.rewards <- cbind.data.frame(control = -dr.scores,
#                      treat = dr.scores - cost)
# dr.rewards
# # plot overlap
use_X <- g_X[, selected.vars]
head(use_X)
tree <- policy_tree(use_X, dr.scores, depth = 2)
tree_full <- policy_tree(g_X, dr.scores, depth = 2)

#save
here_save(tree, "tree")
here_save(tree_full, "tree_full")

print(tree)
plot(tree)
dev.off()
print(tree_full)
plot(tree_full)

# Predict the treatment assignment {1, 2} for each sample.
predicted <- predict(tree_full, g_X)
plot(X[, 1], X[, 2], col = predicted)
legend("topright",
       c("control", "treat"),
       col = c(1, 2),
       pch = 19)
abline(0, -1, lty = 2)
dev.off()
node.id <- predict(tree_full, g_X, type = "node.id")

values <- aggregate(
  dr.scores,
  by = list(leaf.node = node.id),
  FUN = function(x)
    c(mean = mean(x), se = sd(x) / sqrt(length(x)))
)
print(values, digits = 2)


# eval grf fit ------------------------------------------------------------


# eval fit

# The overlap assumption requires a positive probability of treatment for each 𝑋𝑖
# . We should not be able to deterministically decide the treatment status of an individual based on its covariates, meaning none of the estimated propensity scores should be close to one or zero. One can check this with a histogram:
hist(e.hat <- tau.forest$W.hat)

W = g_W
# One can also check that the covariates are balanced across the treated and control group by plotting the inverse-propensity weighted histograms of all samples, overlaid here for each feature (done with ggplot2 which supports weighted histograms):
IPW <- ifelse(W == 1, 1 / e.hat, 1 / (1 - e.hat))

min(IPW)

#Make long

df <- cbind.data.frame(g_W, g_X_binary, IPW)
df
head(df)
table(df$g_W)

# Load the necessary library
library(tidyr)

# Reshape the dataframe
df_long <- df |>
  pivot_longer(cols = starts_with("t0_"),
               names_to = "variable",
               values_to = "value") |>
  mutate(W = factor(g_W))

df_long$value

ggplot(df_long, aes(x = value, weight = IPW, fill = W)) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 bins = 30) +
  facet_wrap(~ variable, ncol = 2)


ggplot(df,
       aes(
         x = t0_religion_church_round_z,
         weight = IPW,
         fill = as.factor(g_W)
       )) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 bins = 30)




n <- 2000
p <- 10
X <- matrix(rnorm(n * p), n, p)
dim(X)
X
X.test <- matrix(0, 101, p)

dim(X.test)

X.test[, 1] <- seq(-2, 2, length.out = 101)
dim(X.test)
