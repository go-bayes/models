# outcome-wide-analysis-template
# Sept 11, 2023


# preliminaries -----------------------------------------------------------


# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/libs2.R")

# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/funs.R")


# WARNING: UNCOMMENT THIS AND DOWNLOAD THE LIBRARIES FROM JB's GITHUB
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# WARNING: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")


## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE.
pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs_refactor/nzavs_data_23"
  )

# read data: note that you need use the arrow package in R
dat <- arrow::read_parquet(pull_path)

### WARNING: THIS PATH WILL NOT WORK FOR YOU. PLEASE SET A PATH TO YOUR OWN COMPUTER!! ###
### WARNING: FOR EACH NEW STUDY SET UP A DIFFERENT PATH OTHERWISE YOU WILL WRITE OVER YOUR MODELS
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/data/nzvs_mods/00drafts/23-ow-fl-church")

# check path:is this correct?  check so you know you are not overwriting other directors
push_mods

# set exposure here
nzavs_exposure <- "religion_church_round"

# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 5

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(0112358)

# set cores for estimation
library(future)
plan(multisession)
n_cores <- parallel::detectCores()

# super learner libraries
sl_lib <- c("SL.glmnet",
            "SL.ranger", #
            "SL.xgboost") #
hist(dat$power_self_nocontrol)
hist(dat$power_others_control)
# dplyr::group_by(.imp) |>
#   rowwise() |>

import data and wrangle-------------------------------------------------dat |>
  group_by(id, wave) |>
  # calculate mean
  mutate(power_have_control_composite = mean(c_across(
    power_self_nocontrol, power_others_control
  ), na.rm = TRUE)) |>
  # ungroup
  ungroup()

dat_long  <- dat |>
  rowwise(wave) |>
  mutate(power_no_control_composite =
           mean(c(
             power_self_nocontrol, power_others_control
           ), na.rm = TRUE)) |>
  ungroup() |>
  # ungroup
  select(
    "wave",
    "year_measured",
    "id",
    # "edu",
    "sample_origin_names_combined",
    # Sample origin names combined
    #"alert_level_combined_lead",  not needed because all receive all levels by the point the outcome is measured
    # covid alert levels -> 2019-2020
    "education_level_coarsen",
    # Ordinal-Rank 0-10 NZREG codes (with overseas school quals coded as Level 3, and all other ancillary categories coded as missing)  Combined highschool levels See:https://www.nzqa.govt.nz/assets/Studying-in-NZ/New-Zealand-Qualification-Framework/requirements-nzqf.pdf
    "male",
    # 0 = female, 0.5 = neither female nor male, 1 = male.
    "age",
    "born_nz",
    "hlth_disability",
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
    "nzsei13",
    # see nzavs materials
    "partner",
    # 0 = no, 1 = yes
    "parent",
    # 0 = no, 1 = yes
    "political_conservative",
    #Please rate how politically liberal versus conservative you see yourself as being.
    "pol_wing",
    # Please rate how politically left-wing versus right-wing you see yourself as being.
    "urban",
    # see NZAVS,
    "have_siblings",
    #Do you have siblings?
    "total_siblings",
    # sum siblings
    "number_sisters_older",
    #How many older sisters do you have?
    "number_sisters_younger",
    #	How many younger sisters do you have?
    "number_brothers_older",
    #	How many older brothers do you have?
    "number_brothers_younger",
    #	How many older brothers do you have?
    "children_num",
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
    "modesty",
    # see mini ipip6
    # I want people to know that I am an important person of high status,
    # I am an ordinary person who is no better than others.
    # I wouldn’t want people to treat me as though I were superior to them.
    # I think that I am entitled to more respect than the average person is
    # "sdo",
    # "rwa",
    # "brk_relationship",
    # "began_relationship",
    "religion_religious",
    # Do you identify with a religion and/or spiritual group?
    # "religion_religious_not",  # reverse this indicator
    "religion_identification_level",
    #How important is your religion to how you see yourself?"
    "religion_prayer",
    # How many times did you pray in the last week?
    "religion_scripture",
    # How many times did you read religious scripture in the last week?
    "religion_church",
    # How many times did you attend a church or place of worship in the last month?
    "religion_believe_spirit",
    #Do you believe in some form of spirit or lifeforce?
    "religion_believe_spirit",
    #inverse believe in god
    "religion_believe_god",
    #Do you believe in a God
    "religion_believe_god_not",
    #inverse believe in god
    "religion_spiritual_identification",
    #w8,w10,w12-13 "I identify as a spiritual person."
    "religion_perceive_religious_discrim",
    #	I feel that I am often discriminated against because of my religious/spiritual beliefs.
    # "bigger_doms", #What religion or spiritual group?#  Not_Rel, Anglican , Buddist, Catholic , Christian_nfd, Christian_Others, Hindu, Jewish           Muslim, PresbyCongReform, TheOthers
    "w_gend_age_euro",
    # sample_weights.
    # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
    "gratitude",
    ## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of peopl
    "modesty",
    # see above
    "vengeful_rumin",
    "charity_donate",
    #How much money have you donated to charity in the last year?
    "hours_charity",
    #,#Hours spent in activities/Hours spent … voluntary/charitable work
    "warm_asians",
    "warm_chinese",
    #"warm_disabled" ,  missing at time 0
    # begins w9
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
    "religion_perceive_religious_discrim",
    #	I feel that I am often discriminated against because of my religious/spiritual beliefs.
    # "bigger_doms", #What religion or spiritual group?#  Not_Rel, Anglican , Buddist, Catholic , Christian_nfd, Christian_Others, Hindu, Jewish           Muslim, PresbyCongReform, TheOthers
    # sample_weights
    "alcohol_frequency",
    #"How often do you have a drink containing alcohol?"
    "alcohol_intensity",
    # How many drinks containing alcohol do you have on a typical day when drinking?
    "hlth_bmi",
    # " What is your height? (metres)\nWhat is your weight? (kg)\nKg
    "hours_exercise",
    # Hours spent … exercising/physical activity
    "sfhealth",
    "sfhealth_your_health",
    # "In general, would you say your health is...
    "sfhealth_get_sick_easier_reversed",
    #\nI seem to get sick a little easier than other people.
    "sfhealth_expect_worse_health_reversed",
    #\nI expect my health to get worse." ****
    "hlth_sleep_hours",
    #During the past month, on average, how many hours of actual sleep did you get per night?
    "smoker",
    #Do you currently smoke?
    "hlth_fatigue",
    #During the last 30 days, how often did.... you feel exhausted?
    "rumination",
    "kessler6_sum",
    # During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
    "kessler_depressed",
    #During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
    "kessler_effort",
    #During the last 30 days, how often did.... you feel that everything was an effort?
    "kessler_hopeless",
    # During the last 30 days, how often did.... you feel hopeless?
    "kessler_nervous",
    #During the last 30 days, how often did.... you feel nervous?
    "kessler_restless",
    #During the last 30 days, how often did.... you feel restless or fidgety?
    "kessler_worthless",
    # During the last 30 days, how often did.... you feel worthless?
    "sexual_satisfaction",
    #  How satisfied are you with your sex life?
    "bodysat",
    ## Am satisfied with the appearance, size and shape of my body.
    "vengeful_rumin",
    # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
    "perfectionism",
    # # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
    # I am hardly ever satisfied with my performance.
    "power_no_control_composite",
    "power_self_nocontrol",
    # I do not have enough power or control over\nimportant parts of my life.
    "power_others_control",
    # Other people have too much power or control over\nimportant parts of my life
    "self_esteem",
    "selfesteem_satself",
    #  On the whole am satisfied with myself.
    "selfesteem_postiveself",
    # Take a positive attitude toward myself
    "selfesteem_failure_reversed",
    # Am inclined to feel that I am a failure.
    #  "self_control",
    "self_control_have_lots",
    #In general, I have a lot of self-control.
    "self_control_wish_more_reversed",
    #I wish I had more self-discipline.(r)
    "emotion_regulation_out_control",
    # When I feel negative emotions, my emotions feel out of control. w10 - w13
    "emotion_regulation_hide_neg_emotions",
    # When I feel negative emotions, I suppress or hide my emotions. w10 - w13
    "emotion_regulation_change_thinking_to_calm",
    # When I feel negative emotions, I change the way I think to help me stay calm. w10 - w13
    # "emp_work_life_balance",# I have a good balance between work and other important things in my life. # not measured at baseline
    # "respect_self",  #If they knew me, most NZers would respect what I have accomplished in life. Missing at T12
    "gratitude",
    ## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
    "pwi",
    "pwb_your_health",
    # #Your health.
    "pwb_your_relationships",
    # #Your personal relationships.
    "pwb_your_future_security",
    # #Your future security.
    "pwb_standard_living",
    #Your standard of living.
    "lifesat",
    "lifesat_satlife",
    # I am satisfied with my life.
    "lifesat_ideal",
    # In most ways my life is close to ideal.
    "lifemeaning",
    # average meaning_purpose, meaning_sense
    "meaning_purpose",
    # My life has a clear sense of purpose.
    "meaning_sense",
    # I have a good sense of what makes my life meaningful.
    "permeability_individual",
    #I believe I am capable, as an individual\nof improving my status in society.
    "impermeability_group",
    #The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
    "neighbourhood_community",
    #I feel a sense of community with others in my local neighbourhood.
    "support",
    "support_help",
    # 'There are people I can depend on to help me if I really need it.
    "support_turnto",
    # There is no one I can turn to for guidance in times of stress.
    "support_noguidance_reverseed",
    #There is no one I can turn to for guidance in times of stress.
    "belong",
    "belong_accept",
    #Know that people in my life accept and value me.
    "belong_routside_reversed",
    # Feel like an outsider.
    "belong_beliefs",
    # Know that people around me share my attitudes and beliefs.
    "charity_donate",
    #How much money have you donated to charity in the last year?
    "hours_charity",
    #,#Hours spent in activities/Hours spent … voluntary/charitable work
    "nwi",
    # The economic situation in New Zealand./# The social conditions in New Zealand. # Business in New Zealand.
    "emp_job_sat",
    # How satisfied are you with your current job? #Eisenbarth, H., Hart, C. M., Zubielevitch, E., Keilor, T., Wilson, M. S., Bulbulia, J. A., Sibley, C. G., &
    #Sedikides, C. (in press). Aspects of psychopathic personality relate to lower subjective and objective professional success. Personality and Individual Differences, 186, 111340.
    "emp_job_secure",
    #only for employed people
    "emp_job_valued",
    "rural_gch2018",
    "hours_community",
    "hours_friends",
    "hours_family",
    "alert_level_combined_lead"
  ) |>
  # select variables
  # mutate(across(where(is.double), as.numeric)) |>
  mutate(
    hours_community_sqrt_raw = sqrt(hours_community),
    hours_friends_sqrt_raw = sqrt(hours_friends),
    hours_family_sqrt_raw = sqrt(hours_family)
  ) |>
  mutate(
    hours_community_sqrt_round = ifelse(hours_community_sqrt_raw >= 8, 8, hours_community_sqrt_raw),
    hours_friends_sqrt_round = ifelse(hours_friends_sqrt_raw >= 8, 8, hours_friends_sqrt_raw),
    hours_family_sqrt_round = ifelse(hours_family_sqrt_raw >= 8, 8, hours_family_sqrt_raw),
  ) |>
  mutate(male = as.numeric(male) - 1) |>
  mutate(total_siblings_factor = ordered(round(
    ifelse(total_siblings > 7, 7, total_siblings), 0
  ))) |>
  mutate(religion_prayer_binary = ifelse(religion_prayer > 0, 1, 0)) |>
  mutate(religion_church_binary = ifelse(religion_church > 0, 1, 0)) |>
  mutate(religion_church_f = ifelse(religion_church >= 21, 21, 0)) |>
  mutate(religion_scripture_binary = ifelse(religion_scripture > 0, 1, 0)) |>
  mutate(religion_church_round = round(ifelse(religion_church >= 8, 8, religion_church), 0)) |>
  mutate(hours_community_round = round(ifelse(hours_community >= 24, 24, hours_community), 0)) |>
  mutate(
    eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban),
    education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  dplyr::filter((wave == 2018 & year_measured  == 1) |
                  (wave == 2019  &
                     year_measured  == 1) |
                  (wave == 2020)) |>  # Eligibility criteria  Observed in 2018/2019 & Outcomes in 2020 or 2021
  group_by(id) |>
  ## MAKE SURE YOU HAVE ELIGIBILITY CRITERIA
  dplyr::mutate(meets_criteria_baseline = ifelse(year_measured == 1 &
                                                   !is.na(!!sym(nzavs_exposure)), 1, 0)) |>  # using R lang
  dplyr::mutate(sample_origin = sample_origin_names_combined) |>  #shorter name
  arrange(id) |>
  filter((wave == 2018 & year_measured == 1) |
           (wave == 2019 & year_measured == 1) |
           (wave == 2020)) %>%
  group_by(id) |>
  mutate(k_18 = ifelse(wave == 2018 &
                         meets_criteria_baseline == 1, 1, 0)) %>% # selection criteria
  mutate(h_18 = mean(k_18, na.rm = TRUE)) %>%
  mutate(k_19 = ifelse(wave == 2019 &
                         meets_criteria_baseline == 1, 1, 0)) %>% # selection criteria
  mutate(h_19 = mean(k_19, na.rm = TRUE)) %>%
  dplyr::filter(h_18 > 0) |>  # hack to enable repeat of baseline
  dplyr::filter(h_19 > 0) |>  # hack to enable repeat of baseline
  ungroup() %>%
  mutate(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    # not_lost = ifelse(lead(year_measured)== -1, 0, not_lost,
    # not_lost = ifelse(lead(year_measured) == 0, 0, not_lost,
    not_lost = ifelse(is.na(not_lost) &
                        year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
  ) |>
  ungroup() |>
  dplyr::mutate(
    friends_money = ifelse(friends_money < 0, 0, friends_money),
    # someone gave neg number
    household_inc_log = log(household_inc + 1),
    hours_children_log = log(hours_children + 1),
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1),
    hours_exercise_log = log(hours_exercise + 1)
  ) |>
  dplyr::rename(sample_weights = w_gend_age_euro) |>
  dplyr::mutate(sample_origin = sample_origin_names_combined) |>  #shorter name
  arrange(id, wave) |>
  droplevels() |>
  select(-h_18, -k_18, -h_19, -k_19) |>
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
  # ) %>%
  # mutate(
#   religion_church_coarsen_n = as.numeric(religion_church_coarsen) - 1,
#   religion_church_binary_n = as.numeric(religion_church_binary)
# ) |>
mutate(
  # eth_cat = as.integer(eth_cat),
  urban = as.numeric(urban),
  education_level_coarsen = as.integer(education_level_coarsen)
) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()


# check n
n_unique(dat_long$id) #33198 # reports hours with community at baseline

# double check path
push_mods

# check col names
colnames(dat)

baseline_vars = c(
  "male",
  "age",
  "education_level_coarsen",
  # factors
  "eth_cat",
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  # "bigger_doms", religious denomination
  "sample_origin",
  "nz_dep2018",
  "nzsei13",
  "total_siblings_factor",
  "born_nz",
  "hlth_disability",
  "hlth_bmi",
  # bmi
  # "pwi", # pwi
  #  "kessler6_sum",
  # "support", #soc support
  #  "belong", # social belonging
  # "smoker", # smoker
  # "sfhealth",
  #
  # "alcohol_frequency", measured with error
  # "alcohol_intensity",
  "hours_family_sqrt_round",
  "hours_friends_sqrt_round",
  "hours_community_sqrt_round",
  # "lifemeaning",
  "household_inc_log",
  # added: measured with error but OK for imputations
  "partner",
  # "parent",  # newly changed - have information in child number
  "political_conservative",
  #Please rate how politically liberal versus conservative you see yourself as being.
  # Sample origin names combined
  "urban",
  "children_num",
  "hours_children_log",
  # new
  "hours_work_log",
  # new
  "hours_housework_log",
  #new
  "hours_exercise_log",
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "honesty_humility",
  "openness",
  "neuroticism",
  "modesty",
  # I want people to know that I am an important person of high status, I am an ordinary person who is no better than others. , I wouldn’t want people to treat me as though I were superior to them. I think that I am entitled to more respect than the average person is.
  # "religion_religious", # Do you identify with a religion and/or spiritual group?
  # "religion_identification_level", #How important is your religion to how you see yourself?"  # note this is not a great measure of virtue, virtue is a mean between extremes.
  "religion_church_round",
  #  for prediction
  # "religion_religious", # perfectly colinear
  # "religion_spiritual_identification",
  #  "religion_identification_level",
  #  "religion_religious",
  #  "religion_church_binary",
  #  "religion_prayer_binary",
  #  "religion_scripture_binary",
  #  "religion_believe_god",
  # "religion_believe_spirit",
  "sample_weights",
  "alert_level_combined_lead"
)


# check
baseline_vars

# check
baseline_vars

# set exposure variable, can be both the continuous and the coarsened, if needed
exposure_var = c("religion_church_round", "not_lost") #


# outcomes
outcome_vars = c(
  "alcohol_frequency",
  # health
  "alcohol_intensity",
  # health
  "hlth_bmi",
  # health
  "hours_exercise_log",
  # health
  "sfhealth",
  # health
  # "sfhealth_your_health",# "In general, would you say your health is...
  # "sfhealth_get_sick_easier",#\nI seem to get sick a little easier than other people.
  # "sfhealth_expect_worse_health",
  "hlth_sleep_hours",
  # health
  "smoker",
  # health
  "hlth_fatigue",
  # embodied
  "rumination",
  # embodied
  "kessler6_sum",
  # embodied
  "bodysat",
  #ego
  "vengeful_rumin",
  #ego
  "bodysat",
  ## Am satisfied with the appearance, size and shape of my body.
  # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
  "perfectionism",
  # # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
  # I am hardly ever satisfied with my performance.
  "power_no_control_composite",
  # "power_self_nocontrol",
  # I do not have enough power or control over\nimportant parts of my life.
  #"power_others_control",
  # Other people have too much power or control over\nimportant parts of my life
  "self_esteem",
  # "selfesteem_satself", #  On the whole am satisfied with myself.
  # "selfesteem_postiveself",# Take a positive attitude toward myself
  # "selfesteem_rfailure", # Am inclined to feel that I am a failure.
  "sexual_satisfaction",
  "self_control_have_lots",
  #In general, I have a lot of self-control.
  "self_control_wish_more_reversed",
  #I wish I had more self-discipline.(r)
  "emotion_regulation_out_control",
  # When I feel negative emotions, my emotions feel out of control. w10 - w13
  # "emotion_regulation_hide_neg_emotions",
  # When I feel negative emotions, I suppress or hide my emotions. w10 - w13
  # "emotion_regulation_change_thinking_to_calm",#,#, # When I feel negative emotions, I change the way I think to help me stay calm. w10 - w13
  # "emp_work_life_balance"# I have a good balance between work and other important things in my life.
  #"respect_self",
  "vengeful_rumin",
  "gratitude",
  ## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
  "pwi_health",
  #Your health.
  "pwi_relationships",
  #Your personal relationships.
  "pwi_security",
  #Your future security.
  "pwi_standardliving",
  #Your standard of living.
  "lifesat",
  # "lifesat_satlife",# I am satisfied with my life.
  # "lifesat_ideal"#,# In most ways my life is close to ideal.
  "lifemeaning",
  #  "meaning_purpose",# My life has a clear sense of purpose.
  #  "meaning_sense"# I have a good sense of what makes my life meaningful.
  "permeability_individual",
  #I believe I am capable, as an individual\nof improving my status in society.
  "impermeability_group",
  #The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
  "neighbourhood_community",
  #I feel a sense of community with others in my local neighbourhood.
  "belong",
  "support"
)
# impute baseline data (we use censoring for the outcomes)
#colnames(dat_long)
# function imputes only baseline not outcome
prep_coop_all <- margot_wide_impute_baseline(
  dat_long,
  baseline_vars = baseline_vars,
  exposure_var = exposure_var,
  outcome_vars = outcome_vars
)

# check mi model
outlist <- row.names(prep_coop_all)[prep_coop_all$outflux < 0.5]
length(outlist)

# checks. We do not impute with weights: area of current research
head(prep_coop_all$loggedEvents, 10)


# save function -- will save to your "push_mod" directory
here_save(prep_coop_all, "prep_coop_all")

# read function
prep_coop_all <- here_read("prep_coop_all")

head(prep_coop_all)
naniar::vis_miss(prep_coop_all, warn_large_data = FALSE)
dev.off()


#check must be a dataframe
str(prep_coop_all)
nrow(prep_coop_all)

# spit and shine
df_wide_censored <-
  prep_coop_all |>
  select(-t1_hours_community_sqrt_round) |>  # exposure for negative control model
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat),
    t2_family_time_binary = as.integer(ifelse(t2_family_time > 0, 1, 0)),
    t2_friends_time_binary = as.integer(ifelse(t2_friends_time > 0, 1, 0)),
    t2_community_time_binary = as.integer(ifelse(t2_community_time > 0, 1, 0))
  ) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_"))


# save
here_save(df_wide_censored, "df_wide_censored")
df_wide_censored <- here_read("df_wide_censored")

#check
head(df_wide_censored)
dim(df_wide_censored)
str(df_wide_censored)


# spit and shine
df_clean <- df_wide_censored %>%
  mutate(t2_na_flag = rowSums(is.na(select(
    ., starts_with("t2_")
  ))) > 0) %>%
  mutate(t1_not_lost = ifelse(t2_na_flag, 0, t1_not_lost)) %>%
  # select(-t2_na_flag) %>%
  filter(!rowSums(is.na(select(
    ., starts_with("t0_")
  )))) |>
  dplyr::mutate(
    across(
      where(is.numeric) &
        !t0_not_lost &
        !t1_not_lost &
        !t0_hours_family_sqrt_round &
        !t0_hours_friends_sqrt_round &
        !t0_hours_community_sqrt_round &
        !t0_sample_weights &
        !t1_religion_church_round &
        !t2_charity_donate &
        !t2_family_time_binary &
        !t2_friends_time_binary &
        !t2_community_time_binary &
        !t2_gratitude &
        !t2_hours_charity,
      ~ scale(.x),
      .names = "{col}_z"
    )
  ) |>
  select(
    where(is.factor),
    t0_not_lost,
    t0_hours_family_sqrt_round,
    t0_hours_friends_sqrt_round,
    t0_hours_community_sqrt_round,
    t0_sample_weights,
    t1_not_lost,
    t1_religion_church_round,
    t2_gratitude,
    t2_charity_donate,
    t2_hours_charity,
    t2_family_time_binary,
    t2_friends_time_binary,
    t2_community_time_binary,
    ends_with("_z")
  ) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
  relocate(starts_with("t2_"), .after = starts_with("t1_"))  %>%
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  mutate(t0_sample_weights = as.numeric(t0_sample_weights)) |>
  data.frame()

dim(df_clean)
naniar::vis_miss(df_clean, warn_large_data = FALSE)
#dev.off()

table(df_clean$t2_community_time_binary)
# save data
push_mods
here_save(df_clean, "df_clean")
df_clean <- here_read("df_clean")


#check n
nrow(df_clean)

colnames(df_clean)
# get names
names_base <-
  df_clean |> select(starts_with("t0"), -t0_sample_weights, -t0_not_lost) |> colnames()
names_outcomes <-
  df_clean |> select(starts_with("t2")) |> colnames()

# check
# names_base
# names_outcomes

# exposure_varX1
outcome_vars





#### SET VARIABLE NAMES
#  model
A <- c("t1_religion_church_round")
C <- c("t1_not_lost")

#L <- list(c("L1"), c("L2"))
W <- c(paste(names_base, collapse = ", "))

# check
print(W)

table(df_clean$t1_religion_church_round)

# shift function -- what if everyone increased by .5 standard deviation, except those above 2

## SHIFT FUNCTION
# simple shift, everyone goes to church at least 4 times per week

f <- function(data, trt) {
  ifelse(data[[trt]] <= 4, 4,  data[[trt]])
}

# simple function # add 1 to all
# f_1 <- function (data, trt) data[[trt]] + 1


# Create a vector indicating what algorithms should be R. # used in the SuperLearner

# libraries
library(SuperLearner)
library(xgboost)

listWrappers()

f
A
C

# "SL.earth" refers to a wrapper for the 'earth' function from the 'earth' R package in the SuperLearner library. This function implements Multivariate Adaptive Regression Splines (MARS), a non-parametric regression method that extends linear models by allowing for interactions and non-linear relationships between variables.
# MARS models can handle high-dimensional data well and can be a useful tool for capturing complex patterns in the data. They work by fitting piecewise linear models to the data, which allows for flexible and potentially non-linear relationships between predictors and the outcome.

library(SuperLearner)
library(ranger)
library(xgboost)
library(glmnet)


# models ------------------------------------------------------------------



# model charitable giving in population
# measure time taken to run the model
timing_info <- system.time({
  m_hours_charity <- lmtp_tmle(
    data = df_clean,
    trt = A,
    baseline = names_base,
    outcome = "t2_hours_charity",
    cens = C,
    shift = f,
    mtp = TRUE,
    folds = 5,
    outcome_type = "continuous",
    weights = df_clean$t0_sample_weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    parallel = n_cores
  )
})

# print timing info
print(paste("Time taken: ", round(timing_info['elapsed'], 2), " seconds"))
m_hours_charity
here_save(m_hours_charity, "m_hours_charity")
m_hours_charity <- here_read("m_hours_charity")
