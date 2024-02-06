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
  "/Users/joseph/v-project\ Dropbox/data/nzvs_mods/00drafts/23-ow-coop-church-lmtp"
)

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

# boost xgboost
SL.xgboost = list(tree_method = 'gpu_hist')

# check
push_mods

# check colnames 
colnames(dat)


# process data

dat_long  <- dat |>
  labelled::remove_val_labels() |> 
  arrange(id, wave) |>
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
    "emotion_regulation_out_control", # make sense
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
    "lifemeaning", # average meaning_purpose, meaning_sense
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
    "hours_charity", #,#Hours spent in activities/Hours spent … voluntary/charitable work
    # "nwi", # The economic situation in New Zealand./# The social conditions in New Zealand. # Business in New Zealand.
    "emp_job_sat", # How satisfied are you with your current job? #Eisenbarth, H., Hart, C. M., Zubielevitch, E., Keilor, T., Wilson, M. S., Bulbulia, J. A., Sibley, C. G., &
    #Sedikides, C. (in press). Aspects of psychopathic personality relate to lower subjective and objective professional success. Personality and Individual Differences, 186, 111340.
    "emp_job_secure",  #only for employed people
    "emp_job_valued",
    "rural_gch2018",
    "hours_community",
    "hours_friends",
    "hours_family",
    "alert_level_combined_lead") |>
  # select variables
  # mutate(across(where(is.double), as.numeric)) |>
  mutate(
    hours_community_sqrt_raw = sqrt(hours_community),
    hours_friends_sqrt_raw = sqrt(hours_friends),
    hours_family_sqrt_raw = sqrt(hours_family)
  ) |>
  mutate(
    hours_community_sqrt_round = ifelse(hours_community_sqrt_raw >= 8, 8, hours_community_sqrt_raw),
    hours_friends_sqrt_round = ifelse(hours_friends_sqrt_raw >= 8, 8,hours_friends_sqrt_raw),
    hours_family_sqrt_round = ifelse(hours_family_sqrt_raw >= 8, 8,hours_family_sqrt_raw),
  ) |>
  mutate(male = as.numeric(male) - 1) |>
  mutate(total_siblings_factor = ordered(round(ifelse(total_siblings > 7, 7, total_siblings), 0))) |> 
  mutate(religion_prayer_binary = ifelse(religion_prayer > 0, 1, 0)) |>
  mutate(religion_church_binary = ifelse(religion_church > 0, 1, 0)) |>
  mutate(religion_church_f = ifelse(religion_church >= 21, 21, 0)) |>
  mutate(religion_scripture_binary = ifelse(religion_scripture > 0, 1, 0)) |>
  mutate(
    religion_church_round = round( ifelse(religion_church >=8, 8, religion_church), 0) )|> 
  mutate(hours_community_round = round(ifelse(hours_community >=24, 24, hours_community), 0) )|> 
  mutate(
    # eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban),
    #  education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  dplyr::filter((wave == 2018 & year_measured  == 1) |
                  (wave == 2019  &
                     year_measured  == 1) |
                  (wave == 2020 )) |>  # Eligibility criteria  Observed in 2018/2019 & Outcomes in 2020 or 2021
  group_by(id) |>
  ## MAKE SURE YOU HAVE ELIGIBILITY CRITERIA
  dplyr::mutate(meets_criteria_baseline = ifelse(year_measured == 1 &!is.na(!!sym(nzavs_exposure))&!is.na(hours_community_sqrt_raw), 1, 0)) |>  # using R lang
  dplyr::mutate(sample_origin = sample_origin_names_combined) |>  #shorter name
  arrange(id) |>
  filter((wave == 2018 & year_measured == 1) |
           (wave == 2019 & year_measured == 1) |
           (wave == 2020)) %>%
  group_by(id) |> 
  mutate(k_18 = ifelse(wave == 2018 &  meets_criteria_baseline == 1, 1, 0)) %>% # selection criteria
  mutate(h_18 = mean(k_18, na.rm = TRUE)) %>%
  mutate(k_19 = ifelse(wave == 2019 & meets_criteria_baseline == 1, 1,0)) %>% # selection criteria
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
    friends_money = ifelse(friends_money < 0, 0, friends_money), # someone gave neg number
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
  select(-h_18,-k_18,-h_19,-k_19) |> 
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
  # religion_church_binary = as.factor(religion_church_binary),
  # eth_cat = as.integer(eth_cat),
  urban = as.numeric(urban),
#  education_level_coarsen = as.integer(education_level_coarsen)
) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()

#community at baseline 
n_participants <- n_unique(dat_long$id) #32058 # reports hours with 

# check
n_participants

here_save(n_participants, "n_participants")

# double check path
push_mods

# check col names
colnames( dat )


# assess positivity
dat_long$wave

dt_positivity_full <- dat_long|>
  filter(wave == 2018 | wave == 2019) |> 
  select(wave, id, religion_church_round, sample_weights) |> 
  mutate(religion_church_shift = ifelse(religion_church_round >=4, 1, 0))



# create transition matrix
out <- msm::statetable.msm(religion_church_round, id, data = dt_positivity_full)

out_church_2 <- msm::statetable.msm(religion_church_shift, id, data = dt_positivity_full)

out
out_church_2

t_tab_2_labels <- c("< weekly", ">= weekly")
# transition table

transition_table  <- transition_table_2(out
                                      #state_names = t_tab_cats_labels
                                      )
transition_table
# for import later 
here_save(transition_table, "transition_table")

transition_table_out_church_2 <- transition_table_2(out_church_2,
                                        state_names = t_tab_2_labels
)

transition_table_out_church_2

# for import later 
here_save(transition_table_out_church_2, "transition_table_out_church_2")
transition_table_out_church_2 <- here_read("transition_table_out_church_2")

# Transition hours
dt_positivity_full_socialising <- dat_long|>
  filter(wave == 2018 | wave == 2019) |> 
  select(wave, id, hours_community_sqrt_round, sample_weights) |> 
  mutate(hours_community_sqrt_round = round(hours_community_sqrt_round,digits = 0)) |> 
  mutate(hours_community_sqrt_round_shift = ifelse(hours_community_sqrt_round >=2, 1, 0))


# create transition matrix
out_social <- msm::statetable.msm(hours_community_sqrt_round, id, data = dt_positivity_full_socialising)

out_social

#t_tab_cats_labels <- c("No Cats", "Cats")
# transition table
transition_table_socialising  <- transition_table_2(out_social)
transition_table_socialising
here_save(transition_table_socialising, "transition_table_socialising")

out_shift_social <- msm::statetable.msm(hours_community_sqrt_round_shift, id, data = dt_positivity_full_socialising)

out_shift_social

t_tab_2_social_labels <- c("< 1.4 weekly hours", ">= 1.4 weekly hours")
# transition table

transition_table_socialising_shift  <- transition_table_2(out_shift_social,
                                        state_names = t_tab_2_social_labels
)

transition_table_socialising_shift
here_save(transition_table_socialising_shift, "transition_table_socialising_shift")


# double check path
push_mods

# check col names
colnames(dat)


# sd values ---------------------------------------------------------------

dt_outcome <- dat_long|>
  filter(wave == 2020) 

dt_outcome$religion_church_round
mean_donations <- mean(dt_outcome$charity_donate, na.rm = TRUE)
mean_volunteer <- mean(dt_outcome$hours_charity, na.rm = TRUE)

mean_donations
mean_volunteer


sd_donations <- sd(dt_outcome$charity_donate, na.rm = TRUE)
sd_volunteer <- sd(dt_outcome$hours_charity, na.rm = TRUE)


sd_donations
sd_volunteer


#
baseline_vars = c(
  "male",
  "age",
  "education_level_coarsen",
  "eth_cat",
  "sample_origin",
  "nz_dep2018",
  "nzsei13",
  "total_siblings_factor",
  "born_nz",
  "hlth_disability",
  "hlth_bmi", 
  "kessler6_sum",
  "sfhealth", 
  "hours_family_sqrt_round",
  "hours_friends_sqrt_round",
  "hours_community_sqrt_round",
  "household_inc_log",
  "partner",
  "political_conservative",
  "urban",
  "children_num",
  "hours_children_log",
  "hours_work_log",
  "hours_housework_log",
  "hours_exercise_log",
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "honesty_humility",
  "openness",
  "neuroticism",
  "modesty",
  "religion_church_round",
  "sample_weights",
  "alert_level_combined_lead"
)
# check
baseline_vars

# check
baseline_vars

# set exposure variable, can be both the continuous and the coarsened, if needed
exposure_var = c("religion_church_round","not_lost","hours_community_sqrt_round") # 

# set outcomes for prosocial domain
outcome_vars = c(
  "modesty",
  "honesty_humility",
  "vengeful_rumin",
  "gratitude",
  "hours_charity",
  "charity_donate",
  "warm_asians",
  "warm_chinese",
  # "warm_disabled" , not at time 10
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
  "religion_perceive_religious_discrim",
  "family_time",
  "friends_time",
  "community_time",
  "support"
  # "support_help",
  # # 'There are people I can depend on to help me if I really need it.
  # "support_turnto",
  # # There is no one I can turn to for guidance in times of stress.
  # "support_rnoguidance"
  # #There is no one I can turn to for guidance in times of stress.
)


# check associations only -------------------------------------------------



dt_18 <- dat_long|>
  filter(wave == 2018) 

# check association only 
summary( lm( charity_donate ~ religion_church_round, data = dt_18) )
summary( lm( hours_charity ~ religion_church_round, data = dt_18) )


summary( lm( charity_donate ~ hours_community_sqrt_round, data = dt_18) )
summary( lm( hours_charity ~ hours_community_sqrt_round, data = dt_18) )


# Ensure you have a character vector of baseline variable names. For example:
baseline_vars_2 <- c(
  "male", "age", "education_level_coarsen", "eth_cat", "sample_origin", "nz_dep2018", "nzsei13",
  "total_siblings_factor", "born_nz", "hlth_disability", "hlth_bmi", "kessler6_sum", "sfhealth",
  "hours_family_sqrt_round", "hours_friends_sqrt_round", "hours_community_sqrt_round", "household_inc_log",
  "partner", "political_conservative", "urban", "children_num", "hours_children_log", "hours_work_log",
  "hours_housework_log", "hours_exercise_log", "agreeableness", "conscientiousness", "extraversion",
  "honesty_humility", "openness", "neuroticism", "sample_weights"
  # outcome or exposure.
)

# baseline_vars_2 <- c(
#   "male", "age", "education_level_coarsen", "eth_cat", "sample_origin", "nz_dep2018", "nzsei13",
#   "total_siblings_factor", "born_nz", "hlth_disability", "hlth_bmi", "kessler6_sum", "sfhealth", "household_inc_log",
#   "partner", "political_conservative", "urban", "children_num", "hours_children_log", "hours_work_log",
#   "hours_housework_log", "hours_exercise_log", "agreeableness", "conscientiousness", "extraversion",
#   "honesty_humility", "openness", "neuroticism"
#   # outcome or exposure.
# )


# Then, call the function without quotes around `baseline_vars`:
fit_church_on_donate <- regress_with_covariates(dt_18, outcome = "charity_donate", 
                                                exposure = "religion_church_round", 
                                                baseline_vars=baseline_vars_2)
parameters::model_parameters( fit_church_on_donate)[2,]

fit_church_on_volunteer <- regress_with_covariates(dt_18, outcome = "hours_charity", 
                                                exposure = "religion_church_round", 
                                                baseline_vars= baseline_vars_2)
parameters::model_parameters( fit_church_on_volunteer)[2,]


# Then, call the function without quotes around `baseline_vars`:
fit_socialising_on_donate <- regress_with_covariates(dt_18, outcome = "charity_donate", 
                                                exposure = "hours_community_sqrt_round", 
                                                baseline_vars=baseline_vars_2)

parameters::model_parameters( fit_socialising_on_donate)[2,]


fit_socialising_on_volunteer <- regress_with_covariates(dt_18, outcome = "hours_charity", 
                                                   exposure = "hours_community_sqrt_round", 
                                                   baseline_vars= baseline_vars_2)[2,]
parameters::model_parameters( fit_church_on_volunteer)[2,]


here_save(fit_church_on_donate, "fit_church_on_donate")
fit_church_on_donate <-here_read("fit_church_on_donate")

here_save(fit_church_on_volunteer, "fit_church_on_volunteer")
fit_church_on_volunteer <-here_read("fit_church_on_volunteer")


here_save(fit_socialising_on_donate, "fit_socialising_on_donate")
fit_socialising_on_donate<-here_read("fit_socialising_on_donate")

here_save(fit_socialising_on_volunteer, "fit_socialising_on_volunteer")
fit_socialising_on_volunteer <-here_read("fit_socialising_on_volunteer")



# tables ------------------------------------------------------------------
library(gtsummary)


# REAL tables -----------------------
dat_long$wave
dt_18 <- dat_long |> 
  dplyr::filter(wave == 2018)


# get names
names_base_tab <- setdiff(baseline_vars, dt_18)
names_base_sorted <- sort(names_base_tab)
names_base_final <- c(nzavs_exposure, names_base_sorted)

names_base_final

##
selected_base_cols <- dt_18 %>% select(all_of(names_base_final)) #|>  dplyr::select(-sample_weights) 
str(selected_base_cols)
nrow(selected_base_cols)

colnames(selected_base_cols)

selected_base_cols
# baseline table

table_baseline <- selected_base_cols %>%
  janitor::clean_names(case = "title") |> 
  tbl_summary(#include = c(agreeableness, conscientiousness, extraversion, neuroticism, openness, honesty_humility),
    missing = "no", 
    percent = "column") |> 
  add_n() %>% # add column with total number of non-missing observations
  statistic = list(all_continuous() ~ "{mean} ({sd})") |>  # Calculate mean and standard deviation for continuous variables
  modify_header(label = "**Baseline Variables**") %>% # update the column header
  bold_labels() 

# save baseline
here_save(table_baseline, "table_baseline")

table_baseline

## all outcomes

names_outcomes_tab <- setdiff(outcome_vars, dt_18)
names_outcomes_sorted <- sort(names_outcomes_tab)
names_outcomes_final <- names_outcomes_sorted # consistent workflow
names_outcomes_final

names_outcomes_final


# baseline by category: personality 

table_baseline_personality  <- selected_base_cols %>%
  janitor::clean_names(case = "title") |> 
  tbl_summary(include = c(Agreeableness, Conscientiousness, Extraversion, Neuroticism, Openness, "Honesty Humility"),
              missing = "no", 
              percent = "column") |> 
  #  add_n() %>% # add column with total number of non-missing observations
  statistic = list(all_continuous() ~ "{mean} ({sd})") |>  # Calculate mean and standard deviation for continuous variables
  modify_header(label = "**Personality Variables**") %>% # update the column header
  bold_labels() 

table_baseline_personality
# save baseline
here_save(table_baseline_personality, "table_baseline_personality")

table_baseline_personality



# baseline by category: demographic
demographic_vars = c(
  "hours_community_sqrt_round",
  "male",
  "age",
  "education_level_coarsen",
  # factors
  "eth_cat",
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  #"sample_origin",
  "nz_dep2018",
  "nzsei13",
  "total_siblings_factor",
  "born_nz",
  "kessler6_sum",
  "sfhealth", #
  "hours_family_sqrt_round",
  "hours_friends_sqrt_round",
  "hours_community_sqrt_round",
  "household_inc_log",
  "partner",
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
  "religion_church_round",
 # "sample_weights",
  "alert_level_combined_lead"
)


# select
names_demographic_vars <- setdiff(demographic_vars, dt_18)

# sort
sorted_names_demographic_vars <- sort(demographic_vars)


# add exposure
names_demographic_final <- c(nzavs_exposure, sorted_names_demographic_vars)


# fetch data
selected_sorted_names_demographic_vars <- dt_18 %>% select(all_of(names_demographic_final)) #|>  dplyr::select(-sample_weights) 
selected_sorted_names_demographic_vars
# make table with correct names
table_demographic_vars <- selected_sorted_names_demographic_vars %>%
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

# inspect
table_demographic_vars

# save
here_save(table_demographic_vars, "table_demographic_vars")

# test
table_demographic_vars|> 
  as_kable(format = "markdown", booktabs = TRUE)


## Confounding control
## IF NEEDED 
# baseline by category: confounding controul
# confounding_control_vars = c(
#   "sample_origin",
#   "political_conservative",
#   "hours_children_log",
#   "hours_work_log",
#   "hours_housework_log",
#   "hours_exercise_log",
#   "religion_church_round",
#   "religion_identification_level"
# )


# names_confounding_control_vars <- setdiff(confounding_control_vars, dt_18)

# sort
# sorted_names_confounding_control_vars <- sort(names_confounding_control_vars)

# fetch data
# selected_sorted_names_confounding_control_vars <- dt_18 %>% select(all_of(sorted_names_confounding_control_vars)) #|>  dplyr::select(-sample_weights) 

## make table
# table_confounding_control_vars <- selected_sorted_names_confounding_control_vars %>%
#   janitor::clean_names(case = "title") |> 
#   tbl_summary(#include = c(agreeableness, conscientiousness, extraversion, neuroticism, openness, honesty_humility),
#     missing = "no", 
#     percent = "column") |> 
#   # add_n() %>% # add column with total number of non-missing observations
#   modify_header(label = "**Confounding Control Variables**") %>% # update the column header
#   bold_labels() 

# view
# table_confounding_control_vars

# check
# table_confounding_control_vars|> 
  # as_kable(format = "markdown", booktabs = TRUE)


# save
# here_save(table_confounding_control_vars, "table_confounding_control_vars")



# OUTCOMES 

## health
virtue_vars = c(
  "hours_charity",
  "charity_donate")

mean( dt_18$hours_charity, na.rm=TRUE )

names_virtue_vars<- setdiff(virtue_vars, dt_18)
sorted_names_virtue_vars <- sort(names_virtue_vars)

selected_sorted_names_virtue_vars<- dt_18 %>% select(all_of(sorted_names_virtue_vars)) #|>  dplyr::select(-sample_weights) 



# get logs 


# table_virtue_vars <- selected_sorted_names_virtue_vars %>%
#   janitor::clean_names(case = "title") |> 
#   tbl_summary(#include = c(agreeableness, conscientiousness, extraversion, neuroticism, openness, honesty_humility),
#     missing = "no", 
#     percent = "column", 
#     statistic = list(all_continuous() ~ "{mean} ({sd})") # Calculate mean and standard deviation for continuous variables
#   ) |> 
#   #  add_n() %>% # add column with total number of non-missing observations
#   modify_header(label = "**Reported Charity Variables**") %>% # update the column header
#   bold_labels() 
# 



table_virtue_vars <- selected_sorted_names_virtue_vars %>%
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
  modify_header(label = "**Reported Charity Variables**") %>%
  bold_labels()


table_virtue_vars

here_save(table_virtue_vars, "table_virtue_vars")


table_demographic_vars|> 
  as_kable(format = "markdown", booktabs = TRUE)



# embody tables 

acceptance_vars = c(
  "warm_asians",
  "warm_chinese",
  # "warm_disabled" , not at time 10
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
  "religion_perceive_religious_discrim"
  )

names_acceptance_vars<- setdiff(acceptance_vars, dt_18)
sorted_names_acceptance_vars <- sort(names_acceptance_vars)


selected_sorted_names_acceptance_vars<- dt_18 %>% select(all_of(sorted_names_acceptance_vars)) #|>  dplyr::select(-sample_weights) 

table_acceptance_vars <- selected_sorted_names_acceptance_vars %>%
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
  #  add_n() %>% # add column with total number of non-missing observations
  modify_header(label = "**Social Group Prejudice/Acceptance**") %>% # update the column header
  bold_labels() 

table_acceptance_vars

table_acceptance_vars|> 
  as_kable(format = "markdown")

table_acceptance_vars |> 
  as_kable_extra( include = everything(),
                  addtl_fmt = TRUE)

table_acceptance_vars
here_save(table_acceptance_vars, "table_acceptance_vars")


# help received vars 
help_received_vars = c(
  "family_time",
  "friends_time",
  "community_time",
  "support"
)

names_help_received_vars<- setdiff(help_received_vars, dt_18)
sorted_names_help_received_vars <- sort(names_help_received_vars)


selected_sorted_names_help_received_vars<- dt_18 %>% select(all_of(sorted_names_help_received_vars)) #|>  dplyr::select(-sample_weights) 

table_selected_sorted_names_help_received_vars <- selected_sorted_names_help_received_vars %>%
  mutate(family_time_binary = factor( if_else(family_time > 0, 1, 0))) |> 
  mutate(community_time_binary = factor( if_else(community_time > 0, 1, 0))) |> 
  mutate(friends_time_binary = factor( if_else(friends_time > 0, 1, 0))) |> 
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
  modify_header(label = "**Received Help Variables**") %>% # update the column header
  bold_labels() 

table_selected_sorted_names_help_received_vars

table_selected_sorted_names_help_received_vars|> 
  as_kable(format = "markdown")

table_selected_sorted_names_help_received_vars |> 
  as_kable_extra( include = everything(),
                  addtl_fmt = TRUE)

here_save(table_selected_sorted_names_help_received_vars, "table_selected_sorted_names_help_received_vars")


## Collect demographic variables

# merge demographics
# table_baseline_personality
# tbl_merge_demographics <-
#   tbl_merge(
#     tbls = list(table_demographic_vars, table_baseline_personality),
#     tab_spanner = c("**Demographics**", "**Personality**")
#   )
# 
# 
# tbl_merge_demographics




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
# histogram_shift <- dt_19 %>%
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

graph_density_of_exposure <- coloured_histogram_shift_range(dt_19, col_name = "religion_church_round", binwidth = 1, range_highlight = c(0,3.9),  shift = "up")

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



# histogram socialising
# dt_positivity_full_socialising <- dat_long|>
#   filter(wave == 2018 | wave == 2019) |> 
#   select(wave, id, hours_community_sqrt_round, sample_weights) |> 
#   mutate(hours_community_sqrt_round = round(hours_community_sqrt_round,digits = 0))

dt_19_social  <- dat_long |>
  mutate(wave = as.numeric(wave)) |> 
  filter(year_measured == 1 & wave == 2) 


graph_density_of_exposure_socialising <- coloured_histogram_shift_range(dt_19_social, col_name = "hours_community_sqrt_round", binwidth = .25, range_highlight = c(0,1.9),  shift = "up")

graph_density_of_exposure_socialising

here_save(graph_density_of_exposure_socialising, "graph_density_of_exposure_socialising")


ggsave(
  graph_density_of_exposure_socialising,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "graph_density_of_exposure_socialising.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

# impute baseline ---------------------------------------------------------


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
outlist <-row.names(prep_coop_all)[prep_coop_all$outflux < 0.5]
length(outlist)

# checks. We do not impute with weights: area of current research
head(prep_coop_all$loggedEvents,10)


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
        # !t2_charity_donate &
        !t2_family_time_binary &
        !t2_friends_time_binary &
        !t2_community_time_binary &
        !t2_gratitude, #&
      #t2_hours_charity,
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
colnames(df_clean)
naniar::vis_miss(df_clean, warn_large_data = FALSE)
dev.off()

table(df_clean$t2_community_time_binary)
# save data
push_mods
here_save(df_clean,"df_clean")


# read data --  start here if previous work already done
df_clean <-here_read("df_clean")

#check n
nrow(df_clean)

colnames(df_clean)
# get names
names_base <- df_clean |> select( starts_with("t0"), - t0_sample_weights,-t0_not_lost )|> colnames()
names_outcomes <- df_clean|> select( starts_with("t2"))|> colnames()

names_base
# check
# names_base
# names_outcomes

# exposure_varX1
outcome_vars


#### SET VARIABLE NAMES 
#  model
A <- c( "t1_religion_church_round")
C <- c( "t1_not_lost")

#L <- list(c("L1"), c("L2")) 
W <- c(paste(names_base, collapse = ", "))

# check 
print(W) 

table(df_clean$t1_religion_church_round)

# shift function -- what if everyone increased by .5 standard deviation, except those above 2 

## SHIFT FUNCTION 
# simple shift, everyone goes to church at least 4 times per week

f <- function(data, trt){
  ifelse( data[[trt]] <=4, 4,  data[[trt]] )
}

# what if we lost church attendance? 
f_1 <- function(data, trt){
  ifelse( data[[trt]] > 0, 0,  data[[trt]] )
}



# simple function # add 1 to all
#f_1 <- function (data, trt) data[[trt]] + 1
# Create a vector indicating what algorithms should be R. # used in the SuperLearner 

# libraries
library(SuperLearner)
library(xgboost)

# checks
f
f_1
A
C

# "SL.earth" refers to a wrapper for the 'earth' function from the 'earth' R package in the SuperLearner library. This function implements Multivariate Adaptive Regression Splines (MARS), a non-parametric regression method that extends linear models by allowing for interactions and non-linear relationships between variables.
# MARS models can handle high-dimensional data well and can be a useful tool for capturing complex patterns in the data. They work by fitting piecewise linear models to the data, which allows for flexible and potentially non-linear relationships between predictors and the outcome.

library(SuperLearner)
library(ranger)
library(xgboost)
library(glmnet) # slow

# super learner libraries
sl_lib <- c("SL.glmnet",
            "SL.ranger", #
            "SL.xgboost") #


# BONUS: progressr progress bars!
progressr::handlers(global = TRUE)

library(future)
plan(multisession)
n_cores <- parallel::detectCores()


colnames(g_X)



# ignore ------------------------------------------------------------------



# GRF MODELS --------------------------------------------------------------
# see: https://grf-labs.github.io/grf/
#devtools::install_github("grf-labs/grf", subdir = "r-package/grf")
library(grf)


# data wrangle: create binary indicator

table(df_clean$t0_religion_church_round_z)
df_use_full_f <- df_clean |> 
  mutate(t1_reg_church_attends = ifelse( t1_religion_church_round >= 4, 1, 0)) |> 
# mutate(t0_reg_church_attends = ifelse( t0_religion_church_round_z <= 4, 1, 0)) |> 
  mutate(t0_has_siblings  = ifelse(t0_total_siblings_factor == 0, 0, 1)) |> 
  select( -t0_total_siblings_factor)




# train causal forest

# sample weights
t0_sample_weights <- df_use_full_f$t0_sample_weights

# get censoring indicator
D <- df_use_full_f$t1_not_lost

# get key data features 

# reduce covariates
df_use_full <-
  df_use_full_f |> select(
    starts_with("t1"),
    starts_with("t2"),
    starts_with("t0"),
    -starts_with("t0_warm"),-t0_hlth_disability_z,
    -t0_hours_family_sqrt_round,
    -t0_hours_friends_sqrt_round,-t0_sfhealth_z,
    -t0_has_siblings,
    -t0_hlth_bmi_z,
    -t0_born_nz_z,
    -t0_friends_time_z,-t0_family_time_z,
    -t0_sample_origin,
    -t0_vengeful_rumin_z,
    -t0_religion_perceive_religious_discrim_z,
    -t0_gratitude_z,
    -t0_support_z
  )



colnames( df_use_full )
nrow( df_use_full )


# Causal forest weights
grf_names_base <- df_use_full |> select( starts_with("t0"), -starts_with(c("t1","t2")), - t0_sample_weights,-t0_not_lost )|> colnames()

selected_W = matrix( df_use_full$t1_reg_church_attends )
selected_Y = matrix( df_use_full$t2_charity_donate )

selected_X <- df_use_full %>% select(all_of(grf_names_base)) |> 
  mutate(across(everything(), ~ {
    x <- .
    attributes(x) <- NULL
    x
  }))


# Y var is censoring time
Y = D + 1
D
sf_censor <- survival_forest(cbind(selected_X, selected_W), Y = Y, D = 1-D, prediction.type="Nelson-Aalen")
summary(sf_censor)

# K <- 1/predict(sf_censor, failure.times=pmin(Y,D), prediction.times="time")$predictions
# 
# hist(K)
here_save(sf_censor, "sf_censor")
censoring_prob <- sf_censor$predictions
hist(censoring_prob)

observed_events <- (D == 1)

# compute sample weights # not  quite right?
grf_sample_weights <- 1 / censoring_prob[observed_events]


grf_sample_weights
# inspect
length(grf_sample_weights)
hist(grf_sample_weights)


# try another method
# library(WeightIt)
# library(cobalt)
# 
# 
# 
# grf_names_base
# # propensity score matching using ebalance -- generally very good
# 
# 
# grf_names_base_2 <- c(grf_names_base, 't1_reg_church_attends')
# 
# grf_names_base_2
# 
# cen_ebal <- match_mi_general(data = df_use_full, 
#                                       X = "t1_not_lost", 
#                                       baseline_vars = grf_names_base_2, 
#                                       estimand = "ATE",  
#                                       # focal = "< >", for ATT
#                                       method = "ebal", 
#                                       sample_weights = "sample_weights")
# 
# summary(cen_ebal)
# bal.tab(cen_ebal, un = TRUE)
# love.plot(cen_ebal, binary = "std", thresholds = c(m = .1),
#           wrap = 50, position = "bottom", size =2) 
# 
# # get weights
# df_use_full$w_weights <- cen_ebal$weights
# df_use_full

df_use_full <- df_use_full |> filter(t1_not_lost == 1) |> 
  mutate(weights = grf_sample_weights * t0_sample_weights) 



g_weights = matrix( df_use_full$weights)
hist(g_weights)

#  One-hot encoding to make cat vars continuous
# Load necessary library
library(dplyr)

# rename dataframe
df <- df_use_full
colnames(df)
# identify categorical variables (excluding 'id')
categorical_vars <- sapply(df, is.factor) 
#categorical_vars["id"] <- FALSE

# Apply one-hot encoding to categorical variables and combine results
df_encoded <- df %>% 
  select(which(!categorical_vars)) %>% 
  bind_cols(
    lapply(names(df)[categorical_vars], function(col) {
      model.matrix(~ . - 1, data = df[col])
    })
  )


head(df_encoded)

# Reorder the columns in df_encoded
df_encoded <- df_encoded %>%
  select(t0_religion_church_round_z, t0_hours_charity_z,t0_charity_donate_z, everything())

head(df_encoded)



g_W  = matrix( df_encoded$t1_reg_church_attends )
t2_charity_donate = matrix( df_encoded$t2_charity_donate )
t2_hours_charity = matrix( df_encoded$t0_hours_charity_z )



use_names_base <- df_encoded |> select( starts_with("t0"), - t0_sample_weights,-t0_not_lost )|> colnames()
use_names_base


g_X <- df_encoded %>% select(all_of(use_names_base)) |> 
  mutate(across(everything(), ~ {
    x <- .
    attributes(x) <- NULL
    x
  }))


# g_XX <- g_X |> 
#   select(t0_religion_church_round_z,t0_charity_donate_z, t0_eth_euro, t0_age_z, t0_partner_z, t0_urban_z, t0_hours_work_log_z, t0_religion_church_round_z, t0_education_level_coarsen_z, t0_household_inc_log_z) 

str(g_W)
str(g_X)
str(g_W)
str(g_Y)
str(g_weights)

# model charity
tau_forest_t2_charity_donate <- grf::causal_forest(X= g_X, Y= t2_charity_donate, W = g_W, sample.weights = g_weights)

# save
here_save(tau_forest_t2_charity_donate, 'tau_forest_t2_charity_donate')


# view
tau_forest_t2_charity_donate

## ATE
average_treatment_effect(tau_forest_t2_charity_donate, target.sample = "all")

633.9836

average_treatment_effect(tau_forest_t2_charity_donate, target.sample = "treated")


tau.hat.oob <- predict(tau_forest_t2_charity_donate)
hist(tau.hat.oob$predictions)

best_linear_projection(tau_forest_t2_charity_donate, g_X)
rate <- rank_average_treatment_effect(tau_forest_t2_charity_donate, g_X[, "t0_religion_church_round_z"])
plot(rate, ylab = "Church", main = "TOC: ranked by decreasing weight")
forest.W <- regression_forest(g_X, g_W, tune.parameters = "all")
W.hat <- predict(forest.W)$predictions

g_Y <- t2_charity_donate
forest.Y <- regression_forest(g_X, g_Y, tune.parameters = "all")
Y.hat <- predict(forest.Y)$predictions


forest.Y.varimp <- variable_importance(tau_forest_t2_charity_donate)
forest.Y.varimp
selected.vars <- which(forest.Y.varimp / mean(forest.Y.varimp) > 0.95)
selected.vars
colnames(g_X)

# Forest in most import vars
tau.forest <- causal_forest(g_X[, selected.vars], g_Y, g_W,
                            W.hat = W.hat, Y.hat = Y.hat,
                            tune.parameters = "all", sample.weights = g_weights)

average_treatment_effect(tau.forest, target.sample = "all")



# get vec for key params

n<-nrow(g_X)
n

train <- sample(1:n, n / 2)
train
train.forest <- causal_forest(g_X[train, ], g_Y[train], g_W[train],  sample.weights = g_weights[train])
eval.forest <- causal_forest(g_X[-train, ], g_Y[-train], g_W[-train], sample.weights = g_weights[-train])
rate <- rank_average_treatment_effect(eval.forest,
                                      predict(train.forest, g_X[-train, ])$predictions)
plot(rate)

average_treatment_effect(train.forest, target.sample = "all")
average_treatment_effect(eval.forest, target.sample = "all")


# tau.hat <- predict(tau_forest, X.test, estimate.variance = TRUE)
# paste("AUTOC:", round(rate$estimate, 2), "+/", round(1.96 * rate$std.err, 2))


##
library(policytree)
library(DiagrammeR)

# get ate
ate <- average_treatment_effect(tau_forest_t2_charity_donate)


# quick eval
varimp <- variable_importance(tau_forest_t2_charity_donate)
varimp
ranked.vars <- order(varimp, decreasing = TRUE)
ranked.vars
colnames(g_X)


best_linear_projection(tau_forest_t2_charity_donate, g_X[ranked.vars[1:5]])


# Compute doubly robust scores
dr.scores <- grf::get_scores(tau_forest_t2_charity_donate)
dr.scores
# dr.scores <- double_robust_scores(tau_forest_t2_charity_donate)
# dr.scores
# # Use as the ATE as a "cost" of program treatment to find something non-trivial
# cost <- ate[["estimate"]]
# dr.rewards <- cbind(control=-dr.scores, treat=dr.scores - cost)

# plot overlap
use_X <- g_X[, selected.vars]

tree <- policy_tree(use_X, dr.scores, depth = 2)
tree_full <- policy_tree(g_X, dr.scores, depth = 2)
here_save(tree_full,"tree_full")
print(tree)
plot(tree)

print(tree)
plot(tree_full)

# Predict the treatment assignment {1, 2} for each sample.
predicted <- predict(tree, g_X)
plot(X[, 1], X[, 2], col = predicted)
legend("topright", c("control", "treat"), col = c(1, 2), pch = 19)
abline(0, -1, lty = 2)

node.id <- predict(tree, X, type = "node.id")

values <- aggregate(dr.scores, by = list(leaf.node = node.id),
                    FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x))))
print(values, digits = 2)


# eval grf fit ------------------------------------------------------------


# eval fit

# The overlap assumption requires a positive probability of treatment for each 𝑋𝑖
# . We should not be able to deterministically decide the treatment status of an individual based on its covariates, meaning none of the estimated propensity scores should be close to one or zero. One can check this with a histogram:
hist(e.hat <- tau.forest$W.hat)

W = g_W
# One can also check that the covariates are balanced across the treated and control group by plotting the inverse-propensity weighted histograms of all samples, overlaid here for each feature (done with ggplot2 which supports weighted histograms):
IPW <- ifelse(W == 1, 1 / e.hat, 1 / (1 - e.hat))

#Make long

df <- cbind(g_W, g_XX,IPW)

head(df)
table(df$g_W)

# Load the necessary library
library(tidyr)

# Reshape the dataframe
df_long <- df %>%
  pivot_longer(
    cols = starts_with("t0_"), 
    names_to = "variable", 
    values_to = "value"
  ) |> 
  mutate(W = factor(g_W))

df_long$value

ggplot(df_long, aes(x = value, weight = IPW, fill = W)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  facet_wrap( ~ variable, ncol = 2)


ggplot(df, aes(x = t0_religion_church_round_z, weight = IPW, fill = as.factor(g_W))) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) 



# resume ------------------------------------------------------------------


# LMPT MODELS -------------------------------------------------------------


# charity models ----------------------------------------------------------



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
}
)


# print timing info
print(paste("Time taken: ", round(timing_info['elapsed'], 2), " seconds"))
m_hours_charity
here_save(m_hours_charity, "m_hours_charity")

# run EXTRA 

m_hours_charity_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_hours_charity",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
here_save(m_hours_charity_1, "m_hours_charity_1")
m_hours_charity_1 <- here_read("m_hours_charity_1")





m_hours_charity_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_hours_charity",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)


m_hours_charity_null
here_save(m_hours_charity_null, "m_hours_charity_null")


# Standardized version
# measure time taken to run the model
t2_hours_charity_z <- lmtp_tmle(
    data = df_clean,
    trt = A,
    baseline = names_base,
    outcome = "t2_hours_charity_z",
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
}
)

# print timing info# print timing infog_W
print(paste("Time taken: ", round(timing_info['elapsed'], 2), " seconds"))
t2_hours_charity_z
here_save(t2_hours_charity_z, "t2_hours_charity_z")


f_1
m_hours_charity_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_hours_charity_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

here_save(m_hours_charity_z_1, "m_hours_charity_z_1")


null_t2_hours_charity_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_hours_charity_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)


null_t2_hours_charity_z
here_save(null_t2_hours_charity_z, "null_t2_hours_charity_z")

# annual charit
#min_wage_2022 = 21.20
# 
# hours_amount = .406 *min_wage_2022
# hours_amount
# year_hours = hours_amount * 52
# year_hours
# sum_hours = year_hours * nz_adult_population
# 
# sum_hours 
# sum_hours/(nz_annual_budget*4)
# 
# 0.007698764
# 
# nz annual budget in 2021

nz_adult_population = 3989000
nz_annual_budget = 14494000000 * 4

# nz_annual_budget
# 1,785,374,282

# charity_year = 1715270000
# hours_year = 1785374282
# 
# charity_year/nz_annual_budget
# (charity_year +hours_year)/nz_annual_budget


# not right
# church_four <- format_tab_tmle(church_four, scale = "RD", new_name = "church_four")
# church_four


# 
# theta <- contrast_hours_full$vals$theta
# # adult population
# nz_adult_population = 3989000
# 
# # min wage workers 
# min_wage_2023 = 22.70
# 
# # off the cuff
# hours_volunteering_gained = theta * nz_adult_population
# hours_volunteering_gained
# 
# # cash value of intervention
# hours_volunteering_gained * min_wage_2022
# 
# 
# ## donation model 


## NON STANDARD
null_t2_charity_donate <- here_read("null_t2_charity_donate")
null_t2_charity_donate
1113.8434

64.95096 * 4

t2_charity_donate <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_charity_donate",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(t2_charity_donate, "t2_charity_donate")
t2_charity_donate

t2_charity_donate<- here_read("t2_charity_donate")
t2_charity_donate$density_ratios

t2_charity_donate_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_charity_donate",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(t2_charity_donate_1, "t2_charity_donate_1")
t2_charity_donate_1

# under null
null_t2_charity_donate <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_charity_donate",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_t2_charity_donate, "null_t2_charity_donate")
null_t2_charity_donate


## STANDARD
t2_charity_donate_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_charity_donate_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(t2_charity_donate_z, "t2_charity_donate_z")
t2_charity_donate_z


# run 
t2_charity_donate_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_charity_donate_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(t2_charity_donate_z_1, "t2_charity_donate_z_1")
t2_charity_donate_z_1

# under null
null_t2_charity_donate_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_charity_donate_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_t2_charity_donate_z, "null_t2_charity_donate_z")
null_t2_charity_donate_z




# tables for church charity -----------------------------------------------

# raw hours
m_hours_charity <- here_read( "m_hours_charity")
m_hours_charity_1 <- here_read( "m_hours_charity_1")
m_hours_charity_null <- here_read( "m_hours_charity_null")


# z score hours
t2_hours_charity_z <- here_read("t2_hours_charity_z")
m_hours_charity_z_1 <- here_read( "m_hours_charity_z_1")
null_t2_hours_charity_z <- here_read( "null_t2_hours_charity_z")


# raw donations

t2_charity_donate<- here_read( "t2_charity_donate")
t2_charity_donate_1 <- here_read( "t2_charity_donate_1")
null_t2_charity_donate <- here_read( "null_t2_charity_donate")


# zscore donations
t2_charity_donate_z<- here_read( "t2_charity_donate_z")
t2_charity_donate_z_1 <- here_read( "t2_charity_donate_z_1")
null_t2_charity_donate_z <- here_read( "null_t2_charity_donate_z")

null_t2_hours_charity_z
# contrast volunteering

# calculate contrast volunteering z
contrast_church_volunteer_z <- lmtp_contrast(t2_hours_charity_z,ref = null_t2_hours_charity_z, type = "additive")
contrast_church_volunteer_z


tab_contrast_church_volunteer_z  <- margot_tab_lmtp(contrast_church_volunteer_z, scale = "RD", new_name = "Volunteering: weekly church >= 1")
tab_contrast_church_volunteer_z
output_tab_contrast_church_volunteer_z<- lmtp_evalue_tab(tab_contrast_church_volunteer_z,  delta = 1, sd = 1, scale = c("RD"))
output_tab_contrast_church_volunteer_z


# contrast volunteering raw
contrast_church_volunteer_raw <- lmtp_contrast(m_hours_charity,ref = m_hours_charity_null, type = "additive")
contrast_church_volunteer_raw

tab_contrast_church_volunteer_raw <- margot_tab_lmtp(contrast_church_volunteer_raw, scale = "RD", new_name = "Volunteering: weekly church >= 1")
output_tab_contrast_church_volunteer_raw <- lmtp_evalue_tab(tab_contrast_church_volunteer_raw,  delta = 1, sd = sd_volunteer, scale = c("RD"))
output_tab_contrast_church_volunteer_raw


# contrast loss volunteering z
contrast_church_volunteer_loss_z <- lmtp_contrast(m_hours_charity_z_1,ref = null_t2_hours_charity_z, type = "additive")
contrast_church_volunteer_loss_z

tab_contrast_church_volunteer_loss_z<- margot_tab_lmtp(contrast_church_volunteer_loss_z, scale = "RD", new_name = "Volunteering: any church lost")
output_tab_contrast_church_volunteer_loss_z <- lmtp_evalue_tab(tab_contrast_church_volunteer_loss_z,  delta = 1, sd = 1, scale = c("RD"))
output_tab_contrast_church_volunteer_loss_z

sd_donations


# contrast loss volunteering raw
contrast_church_volunteer_loss_raw <- lmtp_contrast(m_hours_charity_1,ref = m_hours_charity_null, type = "additive")
contrast_church_volunteer_loss_raw

tab_contrast_church_volunteer_loss_raw<- margot_tab_lmtp(contrast_church_volunteer_loss_raw, scale = "RD", new_name = "Volunteering: any church lost")
output_tab_contrast_church_volunteer_loss_raw <- lmtp_evalue_tab(tab_contrast_church_volunteer_loss_raw,  delta = 1, sd = sd_volunteer, scale = c("RD"))
output_tab_contrast_church_volunteer_loss_raw


here_save(output_tab_contrast_church_volunteer_loss_raw, 'output_tab_contrast_church_volunteer_loss_raw')



# calculate contrast donate z


contrast_church_gives_money_z <- lmtp_contrast(t2_charity_donate_z,ref = null_t2_charity_donate_z, type = "additive")
contrast_church_gives_money_z

tab_contrast_church_gives_money_z <- margot_tab_lmtp(contrast_church_gives_money_z, scale = "RD", new_name = "Donations: weekly church >= 1")
output_tab_contrast_church_gives_money_z<- lmtp_evalue_tab(tab_contrast_church_gives_money_z,  delta = 1, sd = 1, scale = c("RD"))
output_tab_contrast_church_gives_money_z

# calculate contrast donate raw
t2_charity_donate <- here_read("t2_charity_donate")

contrast_church_gives_money_raw <- lmtp_contrast(t2_charity_donate,ref = null_t2_charity_donate, type = "additive")
contrast_church_gives_money_raw

tab_contrast_church_gives_money_raw <- margot_tab_lmtp(contrast_church_gives_money_raw, scale = "RD", new_name = "Donations: weekly church >= 1")
output_tab_contrast_church_gives_money_raw <- lmtp_evalue_tab(tab_contrast_church_gives_money_raw,  delta = 1, sd = sd_donations, scale = c("RD"))
output_tab_contrast_church_gives_money_raw

sd_donations
sd_volunteer


# compare:
output_tab_contrast_church_gives_money_z
output_tab_contrast_church_gives_money_raw

0.1397 * sd_donations

## contrast loss donations z
contrast_church_gives_money_loss_z <- lmtp_contrast(t2_charity_donate_z_1,ref = null_t2_charity_donate_z, type = "additive")
contrast_church_gives_money_loss_z

tab_contrast_church_gives_money_loss_z <- margot_tab_lmtp(contrast_church_gives_money_loss_z, scale = "RD", new_name = "Donations: any church lost")
output_tab_contrast_church_gives_money_loss_z <- lmtp_evalue_tab(tab_contrast_church_gives_money_loss_z,  delta = 1, sd = 1, scale = c("RD"))
output_tab_contrast_church_gives_money_loss_z
output_tab_contrast_church_gives_money_loss_z

## contrast loss donations raw
contrast_church_gives_money_loss_raw <- lmtp_contrast(t2_charity_donate_1,ref = null_t2_charity_donate, type = "additive")
contrast_church_gives_money_loss_raw

tab_contrast_church_gives_money_loss_raw <- margot_tab_lmtp(contrast_church_gives_money_loss_raw, scale = "RD", new_name = "Donations: any church lost")
output_tab_contrast_church_gives_money_loss_raw <- lmtp_evalue_tab(tab_contrast_church_gives_money_loss_raw,  delta = 1, sd = sd_donations, scale = c("RD"))
output_tab_contrast_church_gives_money_loss_raw
output_tab_contrast_church_gives_money_loss_z


# TABLES TO SAVE FOR PUB --------------------------------------------------

## TABLES 
output_tab_contrast_church_gives_money_z
output_tab_contrast_church_volunteer_z

output_tab_contrast_church_volunteer_raw
output_tab_contrast_church_gives_money_raw

output_tab_contrast_church_volunteer_loss_z
output_tab_contrast_church_gives_money_loss_z

output_tab_contrast_church_volunteer_loss_raw
output_tab_contrast_church_gives_money_loss_raw

tab_compare_church_prosocial_behaviour_z  <- rbind(output_tab_contrast_church_gives_money_z, output_tab_contrast_church_volunteer_z)
tab_compare_church_prosocial_behaviour_raw  <- rbind(output_tab_contrast_church_gives_money_raw, output_tab_contrast_church_volunteer_raw)

here_save(tab_compare_church_prosocial_behaviour_z, "tab_compare_church_prosocial_behaviour_z")
tab_compare_church_prosocial_behaviour_z<- here_read("tab_compare_church_prosocial_behaviour_z")

here_save(tab_compare_church_prosocial_behaviour_raw, "tab_compare_church_prosocial_behaviour_raw")
tab_compare_church_prosocial_behaviour_raw<- here_read("tab_compare_church_prosocial_behaviour_raw")



output_tab_contrast_church_gives_money_loss_raw

tab_compare_church_LOSS_prosocial_behaviour_z <- rbind(output_tab_contrast_church_gives_money_loss_z, output_tab_contrast_church_volunteer_loss_z)
tab_compare_church_LOSS_prosocial_behaviour_z

here_save(tab_compare_church_LOSS_prosocial_behaviour_z, "tab_compare_church_LOSS_prosocial_behaviour_z")
tab_compare_church_LOSS_prosocial_behaviour_z<- here_read("tab_compare_church_LOSS_prosocial_behaviour_z")

tab_compare_church_LOSS_prosocial_behaviour_raw <- rbind(output_tab_contrast_church_gives_money_loss_raw, output_tab_contrast_church_volunteer_loss_raw)
tab_compare_church_LOSS_prosocial_behaviour_raw


here_save(tab_compare_church_LOSS_prosocial_behaviour_raw, "tab_compare_church_LOSS_prosocial_behaviour_raw")
tab_compare_church_LOSS_prosocial_behaviour_raw<- here_read("tab_compare_church_LOSS_prosocial_behaviour_raw")


group_tab_compare_church_prosocial_behaviour_raw <- group_tab(tab_compare_church_prosocial_behaviour_raw, type = "RD")
here_save(group_tab_compare_church_prosocial_behaviour_raw, "group_tab_compare_church_prosocial_behaviour_raw")
group_tab_compare_church_prosocial_behaviour_raw<- here_read("group_tab_compare_church_prosocial_behaviour_raw")
group_tab_compare_church_prosocial_behaviour_raw


group_tab_compare_church_prosocial_behaviour_z <- group_tab(tab_compare_church_prosocial_behaviour_z, type = "RD")
here_save(group_tab_compare_church_prosocial_behaviour_z, "group_tab_compare_church_prosocial_behaviour_z")
group_tab_compare_church_prosocial_behaviour_z<- here_read("group_tab_compare_church_prosocial_behaviour_z")
group_tab_compare_church_prosocial_behaviour_z


group_tab_compare_church_LOSS_prosocial_behaviour_z <- group_tab(tab_compare_church_LOSS_prosocial_behaviour_z, type = "RD")
here_save(group_tab_compare_church_LOSS_prosocial_behaviour_z, "group_tab_compare_church_LOSS_prosocial_behaviour_z")
group_tab_compare_church_LOSS_prosocial_behaviour_z<- here_read("group_tab_compare_church_LOSS_prosocial_behaviour_z")
group_tab_compare_church_LOSS_prosocial_behaviour_z

group_tab_compare_church_LOSS_prosocial_behaviour_raw <- group_tab(tab_compare_church_LOSS_prosocial_behaviour_raw, type = "RD")
here_save(group_tab_compare_church_LOSS_prosocial_behaviour_raw, "group_tab_compare_church_LOSS_prosocial_behaviour_raw")
group_tab_compare_church_LOSS_prosocial_behaviour_raw<- here_read("group_tab_compare_church_LOSS_prosocial_behaviour_raw")
group_tab_compare_church_LOSS_prosocial_behaviour_raw


output_tab_contrast_church_volunteer_loss_raw

###

## TABS FOR ACCEPTANCE

tab_compare_behaviour_raw <- rbind(output_tab_contrast_donate_full,output_tab_contrast_hours_full)
tab_compare_behaviour_raw
here_save(tab_compare_behaviour_raw, "tab_compare_behaviour_raw")


plot_charity_church_z <- margot_plot(
  group_tab_compare_behaviour_z,
  type = "RD",
  title = "Religious service effect on reported donations and volunteering",
  subtitle = ">= 1 x weekly service attendance",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_charity_church_z
# 
ggsave(
  plot_charity_church_z,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 6,
  units = "in",
  filename = "plot_charity_church_z.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

group_tab_charity_rd <- here_read("group_tab_charity_rd")
group_tab_charity_rd







# warmth models ---------------------------------------------------------



m_church_t2_warm_asians_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_asians_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_church_t2_warm_asians_z
here_save(m_church_t2_warm_asians_z, "m_church_t2_warm_asians_z")

m_church_t2_warm_asians_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_asians_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(m_church_t2_warm_asians_z_null, "null_m_church_t2_warm_asians_z")
m_church_t2_warm_asians_z_null

null_m_church_t2_warm_asians_z <- here_read("null_m_church_t2_warm_asians_z")



m_church_t2_warm_chinese_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_chinese_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_church_t2_warm_chinese_z
here_save(m_church_t2_warm_chinese_z, "m_church_t2_warm_chinese_z")

null_m_church_t2_warm_chinese_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_chinese_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_m_church_t2_warm_chinese_z_null, "null_m_church_t2_warm_chinese_z")
null_m_church_t2_warm_chinese_z_null

null_m_church_t2_warm_chinese_z <- here_read("null_m_church_t2_warm_chinese_z")


m_c_t2_warm_immigrants_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_immigrants_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_immigrants_z
here_save(m_c_t2_warm_immigrants_z, "m_c_t2_warm_immigrants_z")

null_m_c_t2_warm_immigrants_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_immigrants_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_m_c_t2_warm_immigrants_z, "null_m_c_t2_warm_immigrants_z")
null_m_c_t2_warm_immigrants_z


m_c_t2_warm_indians_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_indians_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_indians_z
here_save(m_c_t2_warm_indians_z, "m_c_t2_warm_indians_z")

m_c_t2_warm_indians_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_indians_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(m_c_t2_warm_indians_z_null, "m_c_t2_warm_indians_z_null")




m_c_t2_warm_elderly_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_elderly_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_elderly_z
here_save(m_c_t2_warm_elderly_z, "m_c_t2_warm_elderly_z")

null_m_c_t2_warm_elderly_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_elderly_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_m_c_t2_warm_elderly_z, "null_m_c_t2_warm_elderly_z")
null_m_c_t2_warm_elderly_z


m_c_t2_warm_maori_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_maori_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_maori_z
here_save(m_c_t2_warm_maori_z, "m_c_t2_warm_maori_z")

null_m_c_t2_warm_maori_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_maori_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_m_c_t2_warm_maori_z, "null_m_c_t2_warm_maori_z")
null_m_c_t2_warm_elderly_z


m_c_t2_warm_mental_illness_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_mental_illness_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_mental_illness_z
here_save(m_c_t2_warm_mental_illness_z, "m_c_t2_warm_mental_illness_z")

null_m_c_t2_warm_mental_illness_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_mental_illness_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_m_c_t2_warm_mental_illness_z, "null_m_c_t2_warm_mental_illness_z")
null_m_c_t2_warm_mental_illness_z


m_c_t2_warm_muslims_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_muslims_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_muslims_z
here_save(m_c_t2_warm_muslims_z, "m_c_t2_warm_muslims_z")

null_m_c_t2_warm_muslims_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_muslims_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_m_c_t2_warm_muslims_z, "null_m_c_t2_warm_muslims_z")
null_m_c_t2_warm_muslims_z


m_c_t2_warm_nz_euro_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_nz_euro_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_nz_euro_z
here_save(m_c_t2_warm_nz_euro_z, "m_c_t2_warm_nz_euro_z")

null_m_c_t2_warm_nz_euro_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_nz_euro_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_m_c_t2_warm_nz_euro_z, "null_m_c_t2_warm_nz_euro_z")
null_m_c_t2_warm_nz_euro_z



m_c_t2_warm_overweight_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_overweight_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_overweight_z
here_save(m_c_t2_warm_overweight_z, "m_c_t2_warm_overweight_z")

null_m_c_t2_warm_overweight_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_overweight_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_m_c_t2_warm_overweight_z, "null_m_c_t2_warm_overweight_z")
null_m_c_t2_warm_overweight_z




m_c_t2_warm_pacific_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_pacific_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_pacific_z
here_save(m_c_t2_warm_pacific_z, "m_c_t2_warm_pacific_z")

null_m_c_t2_warm_pacific_z<- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_pacific_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_m_c_t2_warm_pacific_z, "null_m_c_t2_warm_pacific_z")
null_m_c_t2_warm_pacific_z


m_c_t2_warm_refugees_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_refugees_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_pacific_z
here_save(m_c_t2_warm_refugees_z, "m_c_t2_warm_refugees_z")

null_m_c_t2_warm_refugees_z<- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_warm_refugees_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_m_c_t2_warm_refugees_z, "null_m_c_t2_warm_refugees_z")
null_m_c_t2_warm_refugees_z



church_religion_perceive_religious_discrim_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_religion_perceive_religious_discrim_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

church_religion_perceive_religious_discrim_z
here_save(church_religion_perceive_religious_discrim_z, "church_religion_perceive_religious_discrim_z")

null_church_religion_perceive_religious_discrim_z<- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_religion_perceive_religious_discrim_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_church_religion_perceive_religious_discrim_z, "null_church_religion_perceive_religious_discrim_z")
null_church_religion_perceive_religious_discrim_z




# church prejudice results ------------------------------------------------


# asians
m_church_t2_warm_asians_z <- here_read("m_church_t2_warm_asians_z")
null_m_church_t2_warm_asians_z <-
  here_read("null_m_church_t2_warm_asians_z")

contrast_church_asians <-
  lmtp_contrast(m_church_t2_warm_asians_z, ref = null_m_church_t2_warm_asians_z, type = "additive")
contrast_church_asians

tab_contrast_church_asians  <-
  margot_tab_lmtp(contrast_church_asians,
                  scale = "RD",
                  new_name = "Warm Asians: weekly church >= 1")

tab_contrast_church_asians

output_asians_church <- lmtp_evalue_tab(tab_contrast_church_asians,  delta = 1, sd = 1, scale = c("RD"))
output_asians_church


# chinese
m_church_t2_warm_chinese_z  <-
  here_read("m_church_t2_warm_chinese_z")
null_m_church_t2_warm_chinese_z <-
  here_read("null_m_church_t2_warm_chinese_z")

contrast_church_chinese <- lmtp_contrast(m_church_t2_warm_chinese_z,
                                         ref = null_m_church_t2_warm_chinese_z,
                                         type = "additive")
contrast_church_chinese

tab_contrast_church_chinese <-
  margot_tab_lmtp(contrast_church_chinese,
                  scale = "RD",
                  new_name = "Warm Chinese: weekly church >= 1")
tab_contrast_church_chinese

output_chinese_church <- lmtp_evalue_tab(tab_contrast_church_chinese,  delta = 1, sd = 1, scale = c("RD"))
output_chinese_church



# immigrants
m_c_t2_warm_immigrants_z  <- here_read("m_c_t2_warm_immigrants_z")
null_m_c_t2_warm_immigrants_z  <- here_read("null_m_c_t2_warm_immigrants_z")

contrast_church_immigrants <- lmtp_contrast(m_c_t2_warm_immigrants_z,
                                            ref = null_m_c_t2_warm_immigrants_z,
                                            type = "additive")
contrast_church_immigrants

tab_contrast_church_immigrants <- margot_tab_lmtp(contrast_church_immigrants, scale = "RD", 
                                                  new_name = "Warm Immigrants: weekly church >= 1")
tab_contrast_church_immigrants


output_immigrants_church <- lmtp_evalue_tab(tab_contrast_church_immigrants,  delta = 1, sd = 1, scale = c("RD"))
output_immigrants_church


#indians
m_c_t2_warm_indians_z  <- here_read("m_c_t2_warm_indians_z")
m_c_t2_warm_indians_z_null  <-
  here_read("m_c_t2_warm_indians_z_null")

contrast_church_indians <- lmtp_contrast(m_c_t2_warm_indians_z,
                                         ref = m_c_t2_warm_indians_z_null,
                                         type = "additive")


tab_contrast_church_indians <-
  margot_tab_lmtp(contrast_church_indians,
                  scale = "RD",
                  new_name = "Warm Indians: weekly church >= 1")



output_indians_church <- lmtp_evalue_tab(tab_contrast_church_indians,  delta = 1, sd = 1, scale = c("RD"))
output_indians_church


# elderly
m_c_t2_warm_elderly_z  <- here_read("m_c_t2_warm_elderly_z")
null_m_c_t2_warm_elderly_z <- here_read("null_m_c_t2_warm_elderly_z")

contrast_church_elderly <- lmtp_contrast(m_c_t2_warm_elderly_z,
                                         ref = null_m_c_t2_warm_elderly_z,
                                         type = "additive")


tab_contrast_church_elderly<- margot_tab_lmtp(contrast_church_elderly, scale = "RD", 
                                              new_name = "Warm Elderly: weekly church >= 1")

tab_contrast_church_elderly

output_elderly_church <-
  lmtp_evalue_tab(
    tab_contrast_church_elderly,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_elderly_church

# maori
m_c_t2_warm_maori_z  <- here_read("m_c_t2_warm_maori_z")
null_m_c_t2_warm_maori_z  <- here_read("null_m_c_t2_warm_maori_z")

contrast_church_maori <- lmtp_contrast(m_c_t2_warm_maori_z,
                                       ref = null_m_c_t2_warm_maori_z,
                                       type = "additive")


tab_contrast_church_maori <-
  margot_tab_lmtp(contrast_church_maori, scale = "RD",
                  new_name = "Warm Maori: weekly church >= 1")

tab_contrast_church_maori

output_maori_church <-
  lmtp_evalue_tab(
    tab_contrast_church_maori,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_maori_church


# mental illness
m_c_t2_warm_mental_illness_z  <-
  here_read("m_c_t2_warm_mental_illness_z")
null_m_c_t2_warm_mental_illness_z  <-
  here_read("null_m_c_t2_warm_mental_illness_z")


contrast_church_mental_illness <-
  lmtp_contrast(m_c_t2_warm_mental_illness_z,
                ref = null_m_c_t2_warm_mental_illness_z,
                type = "additive")


tab_contrast_church_mental_illness <-
  margot_tab_lmtp(contrast_church_mental_illness,
                  scale = "RD",
                  new_name = "Warm Mental Illness: weekly church >= 1")

tab_contrast_church_mental_illness

output_mental_illness_church <-
  lmtp_evalue_tab(
    tab_contrast_church_mental_illness,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_mental_illness_church


#  muslims
m_c_t2_warm_muslims_z <- here_read("m_c_t2_warm_muslims_z")
null_m_c_t2_warm_muslims_z <-
  here_read("null_m_c_t2_warm_muslims_z")

contrast_church_muslims <- lmtp_contrast(m_c_t2_warm_muslims_z,
                                         ref = null_m_c_t2_warm_muslims_z,
                                         type = "additive")


tab_contrast_church_muslims <-
  margot_tab_lmtp(contrast_church_muslims,
                  scale = "RD",
                  new_name = "Warm Muslims: weekly church >= 1")

tab_contrast_church_muslims

output_muslims_church <-
  lmtp_evalue_tab(
    tab_contrast_church_muslims,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_muslims_church

# nz euro
m_c_t2_warm_nz_euro_z <- here_read("m_c_t2_warm_nz_euro_z")
null_m_c_t2_warm_nz_euro_z  <-
  here_read("null_m_c_t2_warm_nz_euro_z")

contrast_church_euro <- lmtp_contrast(m_c_t2_warm_nz_euro_z,
                                      ref = null_m_c_t2_warm_nz_euro_z,
                                      type = "additive")


tab_contrast_church_euro <-
  margot_tab_lmtp(contrast_church_euro, scale = "RD",
                  new_name = "Warm NZEuro: weekly church >= 1")



output_euro_church <-
  lmtp_evalue_tab(
    tab_contrast_church_euro,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_euro_church



# overweight
m_c_t2_warm_overweight_z <- here_read("m_c_t2_warm_overweight_z")
null_m_c_t2_warm_overweight_z <-
  here_read("null_m_c_t2_warm_overweight_z")


contrast_church_overweight <-
  lmtp_contrast(m_c_t2_warm_overweight_z,
                ref = null_m_c_t2_warm_overweight_z,
                type = "additive")
contrast_church_overweight

tab_contrast_church_overweight <-
  margot_tab_lmtp(contrast_church_overweight,
                  scale = "RD",
                  new_name = "Warm Overweight: weekly church >= 1")

tab_contrast_church_overweight


output_overweight_church <-
  lmtp_evalue_tab(
    tab_contrast_church_overweight,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_overweight_church



# warm pacific
m_c_t2_warm_pacific_z  <- here_read("m_c_t2_warm_pacific_z")
null_m_c_t2_warm_pacific_z <-
  here_read("null_m_c_t2_warm_pacific_z")


contrast_church_pacific <- lmtp_contrast(m_c_t2_warm_pacific_z,
                                         ref = null_m_c_t2_warm_pacific_z,
                                         type = "additive")


tab_contrast_church_pacific <-
  margot_tab_lmtp(contrast_church_pacific,
                  scale = "RD",
                  new_name = "Warm Pacific: weekly church >= 1")

tab_contrast_church_pacific


output_pacific_church <-
  lmtp_evalue_tab(
    tab_contrast_church_pacific,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_pacific_church


#warm refugees
m_c_t2_warm_refugees_z <- here_read("m_c_t2_warm_refugees_z")
null_m_c_t2_warm_refugees_z <-
  here_read("null_m_c_t2_warm_refugees_z")


contrast_church_refugees <- lmtp_contrast(m_c_t2_warm_refugees_z,
                                          ref = null_m_c_t2_warm_refugees_z,
                                          type = "additive")


tab_contrast_church_refugees <-
  margot_tab_lmtp(contrast_church_refugees,
                  scale = "RD",
                  new_name = "Warm Refugees: weekly church >= 1")


tab_contrast_church_refugees


output_refugees_church <-
  lmtp_evalue_tab(
    tab_contrast_church_refugees,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_refugees_church

church_religion_perceive_religious_discrim_z <- here_read("church_religion_perceive_religious_discrim_z")
null_church_religion_perceive_religious_discrim_z <- here_read("null_church_religion_perceive_religious_discrim_z")

contrast_church_perceive_religious_discrim_z <- lmtp_contrast(church_religion_perceive_religious_discrim_z,
                                                              ref = null_church_religion_perceive_religious_discrim_z,
                                                              type = "additive")

tab_contrast_hours_perceive_religious_discrim_z  <- margot_tab_lmtp(contrast_church_perceive_religious_discrim_z, scale = "RD", 
                                                                    new_name = "Perc. Religious Discrim: weekly church >= 1")

output_perceive_rel_discrimination_church <-
  lmtp_evalue_tab(
    tab_contrast_hours_perceive_religious_discrim_z,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_perceive_rel_discrimination_church


# ALERT TABLES WARMTH CHRUCH ----------------------------------------------


# table
tab_warm_church <- rbind(
  output_asians_church,
  output_chinese_church,
  output_immigrants_church,
  output_indians_church,
  output_elderly_church,
  output_maori_church,
  output_mental_illness_church,
  output_muslims_church,
  output_euro_church,
  output_overweight_church,
  output_pacific_church,
  output_refugees_church,
  output_perceive_rel_discrimination_church
)

tab_warm_church
here_save(tab_warm_church, "tab_warm_church")


# 
# str(church_four_hours_donate)
# church_four_hours_donate
# 
# est_donate <- sapply(church_four_hours_donate, round, 0)
# 
# est_donate
# rounded_donate |> 
#   kbl(format = "markdown")
# # calculate proportion 4 or greater
# proportion = sum(df_clean_donate$t1_religion_church_round >= 4) / nrow(df_clean_donate)
# proportion
#  
# theta_donate <- contrast_donate$vals$theta
# theta_donate
# # adult population 
# nz_adult_population = 3989000
# 
# 
# # # min wage workers 
# # min_wage_2023 = 22.70
# 
# # off the cuff
# dollars_donate_gained = theta_donate * nz_adult_population
# dollars_donate_gained
# 
# min_donate_gained =  430 
# min_donate_gained
# # 832193252
# hours_volunteering_gained



# table

group_tab_warm_church <- group_tab(tab_warm_church, type = "RD")
group_tab_warm_church
saveRDS(group_tab_warm_church, here::here(push_mods, "group_tab_warm_church"))

group_tab_warm_church <- here_read("group_tab_warm_church")

plot_prejudice_church <- margot_plot(
  group_tab_warm_church,
  type = "RD",
  title = "Religious service effect on prejudice/acceptance",
  subtitle = ">= 1 x weekly service attendance",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_prejudice_church

ggsave(
  plot_prejudice_church,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 6,
  units = "in",
  filename = "plot_prejudice_church.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)





# help received models ----------------------------------------------------

m_time_community <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_community_time_binary",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "binomial",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)
m_time_community
here_save(m_time_community, "m_time_community")

m_time_community_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_community_time_binary",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "binomial",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(m_time_community_null, "m_time_community_null")
m_time_community_null
contrast_time_commmunity_full <- lmtp_contrast(m_time_community,ref = m_time_community_null, type = "rr")
contrast_time_commmunity_full


m_time_friends <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_friends_time_binary",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "binomial",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_time_friends
here_save(m_time_friends, "m_time_friends")

m_time_friends_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_friends_time_binary",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "binomial",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(m_time_friends_null, "m_time_friends_null")
m_time_friends_null



m_time_family <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_family_time_binary",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "binomial",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_time_family
here_save(m_time_family, "m_time_family")

m_time_family_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_family_time_binary",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "binomial",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(m_time_family_null, "m_time_family_null")
m_time_family_null



## TABLES
m_time_community <- here_read("m_time_community")
m_time_community_null<- here_read("m_time_community_null")
m_time_friends<- here_read("m_time_friends")
m_time_friends_null<- here_read("m_time_friends_null")
m_time_family<- here_read("m_time_family")
m_time_family_null<- here_read("m_time_family_null")

# community 
contrast_church_help_received_community_rr <- lmtp_contrast(m_time_community,ref = m_time_community_null, type = "rr")
contrast_church_help_received_community_rr



tab_contrast_church_help_received_community_rr <- margot_tab_lmtp(contrast_church_help_received_community_rr, scale = "RR", 
                                                                    new_name = "Help from Community: weekly church >= 1")


output_tab_contrast_church_help_received_community_rr <-
  lmtp_evalue_tab(
    tab_contrast_church_help_received_community_rr,
    delta = 1,
    sd = 1,
    scale = c("RR")
  )


### USE 
output_tab_contrast_church_help_received_community_rr


# friends
contrast_church_help_received_friends_rr <- lmtp_contrast(m_time_friends,ref = m_time_friends_null, type = "rr")
contrast_church_help_received_friends_rr


tab_contrast_church_help_received_friends_rr <- margot_tab_lmtp(contrast_church_help_received_friends_rr, scale = "RR", 
                                                                new_name = "Help from Friends: weekly church >= 1")
output_tab_contrast_church_help_received_friends_rr <-
  lmtp_evalue_tab(
    tab_contrast_church_help_received_friends_rr,
    delta = 1,
    sd = 1,
    scale = c("RR")
  )


## HERE 
output_tab_contrast_church_help_received_friends_rr



# family
contrast_church_help_received_family_rr <- lmtp_contrast(m_time_family,ref = m_time_family_null, type = "rr")
contrast_church_help_received_family_rr


tab_contrast_church_help_received_family_rr <- margot_tab_lmtp(contrast_church_help_received_family_rr, scale = "RR", new_name = "Help from Family: weekly church >= 1")
output_tab_contrast_church_help_received_family_rr <-
  lmtp_evalue_tab(
    tab_contrast_church_help_received_family_rr,
    delta = 1,
    sd = 1,
    scale = c("RR")
  )

## Here
output_tab_contrast_church_help_received_family_rr



# ALERT CHURCH HELP RECEIVED TABLE ----------------------------------------


tab_church_help_received <- rbind(output_tab_contrast_church_help_received_family_rr,output_tab_contrast_church_help_received_friends_rr, 
                         output_tab_contrast_church_help_received_community_rr)
tab_church_help_received
here_save(tab_church_help_received, "tab_church_help_received")
tab_church_help_received <- here_read('tab_church_help_received')



group_tab_church_help_received <- group_tab(tab_church_help_received, type = "RR")

saveRDS(group_tab_church_help_received, here::here(push_mods, "group_tab_church_help_received"))


group_tab_church_help_received <- here_read("group_tab_church_help_received")

# graph

plot_group_tab_time_church <- margot_plot(
  group_tab_church_help_received,
  type = "RR",
  title = "Religious service effect on help received",
  subtitle = ">= 1 x weekly service attendance",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = 0,
  x_lim_lo = 0,
  x_lim_hi =  2
)

plot_group_tab_time_church


ggsave(
  plot_group_tab_time_church,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 6,
  units = "in",
  filename = "plot_group_tab_time_church.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



# SOCIALISING COMPARATIVE STUDY -------------------------------------------


# MODEL FOR COMMUNITY TIME 
# this model is to contrast the church attendance model 


prep_coop_all <- here_read("prep_coop_all")


# analysis for time
df_wide_censored_only_time <-
  prep_coop_all |>
  select(-t1_religion_church_round) |>  # exposure for negative control model
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat),
    t2_family_time_binary = as.integer(ifelse(t2_family_time > 0, 1, 0)),
    t2_friends_time_binary = as.integer(ifelse(t2_friends_time > 0, 1, 0)),
    t2_community_time_binary = as.integer(ifelse(t2_community_time > 0, 1, 0))
  ) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_"))


# save
here_save(df_wide_censored_only_time, "df_wide_censored_only_time")
df_wide_censored_only_time<- here_read( "df_wide_censored_only_time")

#(df_wide_censored_only_time$t1_hours_community_sqrt_round)
# 2.5*sd(df_clean$t1_religion_church_round)
# 2.5*sd(df_wide_censored_only_time$t1_hours_community_sqrt_round)
# hist(df_wide_censored_only_time$t1_hours_community_sqrt_round)

# spit and shine
df_clean_time <- df_wide_censored_only_time %>%
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
        !t1_hours_community_sqrt_round &
        # !t2_charity_donate &
        !t2_family_time_binary &
        !t2_friends_time_binary &
        !t2_community_time_binary &
        !t2_gratitude,
      # !t2_hours_charity,
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
    t1_hours_community_sqrt_round,
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

naniar::vis_miss(df_clean_time, warn_large_data = FALSE)
dev.off()

# save data
push_mods
here_save(df_clean_time,"df_clean_time")
df_clean_time <-here_read("df_clean_time")

colnames(df_clean_time)
# get names
names_base_time <- df_clean_time |> select( starts_with("t0"), - t0_sample_weights,-t0_not_lost )|> colnames()
names_outcomes_time <- df_clean_time|> select( starts_with("t2"))|> colnames()

names_base_time
names_outcomes_time
names_base_time

#  model
A_2 <- c("t1_hours_community_sqrt_round")
C <- c( "t1_not_lost")

#L <- list(c("L1"), c("L2")) 
W <- c(paste(names_base_time, collapse = ", "))

# check 
print(W) 


#baseline confounders
#L <- as.list(names_base)
table(df_clean_time$t1_hours_community_sqrt_round)

# shift function -- what if everyone increased by .5 standard deviation, except those above 2 

# simple shift,2 hours per week. 
f_s <- function(data, trt){
  ifelse( data[[trt]] <=2, 2,  data[[trt]] )
}

# simple shift,2 hours per week. 
f_s_loss <- function(data, trt){
  ifelse( data[[trt]] >0, 0,  data[[trt]] )
}

# f_1 <- function (data, trt) data[[trt]] + 1

# Create a vector indicating what algorithms should be R. # used in the SuperLearner 

# libraries
library(SuperLearner)
library(xgboost)

# "SL.earth" refers to a wrapper for the 'earth' function from the 'earth' R package in the SuperLearner library. This function implements Multivariate Adaptive Regression Splines (MARS), a non-parametric regression method that extends linear models by allowing for interactions and non-linear relationships between variables.
# MARS models can handle high-dimensional data well and can be a useful tool for capturing complex patterns in the data. They work by fitting piecewise linear models to the data, which allows for flexible and potentially non-linear relationships between predictors and the outcome.
sl_lib
# super learner libraries
# check
sl_lib

# BONUS: progressr progress bars!
progressr::handlers(global = TRUE)

# we will only assess behaviour 

# recomend tmle for single time point
# recommend sdr for multiple time points
f_s_loss
A
C

df<- df_clean_time

names_base<- names_base_time
names_base
# model charitable giving in population 
m_hours_charity_time <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_hours_charity",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  outcome_type = "continuous",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)
m_hours_charity_time
here_save(m_hours_charity_time, "m_hours_charity_time")
m_hours_charity_time <- here_read("m_hours_charity_time")
m_hours_charity_time
m_hours_charity_null_time <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_hours_charity",
  cens = C,
  shift = NULL,
  folds = 5,
  outcome_type = "continuous",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)
m_hours_charity_null_time
here_save(m_hours_charity_null_time, "m_hours_charity_null_time")
m_hours_charity_null_time <- here_read( "m_hours_charity_null_time")
m_hours_charity_null_time


m_socialising_on_volunteer_loss_raw <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_hours_charity",
  cens = C,
  shift = f_s_loss,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  outcome_type = "continuous",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)
here_save(m_socialising_on_volunteer_loss_raw,"m_socialising_on_volunteer_loss_raw")
m_socialising_on_volunteer_loss_raw

m_socialising_on_volunteer_loss_z <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_hours_charity_z",
  cens = C,
  shift = f_s_loss,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  outcome_type = "continuous",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)
here_save(m_socialising_on_volunteer_loss_z,"m_socialising_on_volunteer_loss_z")
m_socialising_on_volunteer_loss_z <-here_read("m_socialising_on_volunteer_loss_z")


# caluclate contrast 
contrast_hours_full_time <- lmtp_contrast(m_hours_charity_time,ref = m_hours_charity_null_time, type = "additive")
str(contrast_hours_full_time)
contrast_hours_full_time

# not right
church_four_time <- format_tab_tmle(church_four_time, scale = "RD", new_name = "church_four")
church_four_time


m_hours_charity_time_z <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_hours_charity_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  outcome_type = "continuous",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)
m_hours_charity_time_z
here_save(m_hours_charity_time_z, "m_hours_charity_time_z")
m_hours_charity_time_z <- here_read("m_hours_charity_time_z")



m_hours_charity_null_time_z <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_hours_charity_z",
  cens = C,
  shift = NULL,
  folds = 5,
  outcome_type = "continuous",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)
m_hours_charity_null_time_z
here_save(m_hours_charity_null_time_z, "m_hours_charity_null_time_z")
m_hours_charity_null_time_z <-here_read('m_hours_charity_null_time_z')

# 
# theta <- contrast_hours_full$vals$theta
# # adult population
# nz_adult_population = 3989000
# 
# # min wage workers 
# min_wage_2023 = 22.70
# 
# # off the cuff
# hours_volunteering_gained = theta * nz_adult_population
# hours_volunteering_gained
# 
# # cash value of intervention
# hours_volunteering_gained * min_wage_2022
# 
# 
# ## donation model 
# t2_charity_donate

m_charity_donate_time <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_charity_donate",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores  
)
here_save(m_charity_donate_time, "m_charity_donate_time")
m_charity_donate_time <- here_read("m_charity_donate_time")
m_charity_donate_time

# under null
m_charity_donate_null_time <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_charity_donate",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  outcome_type = "continuous",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores  
)

here_save(m_charity_donate_null_time, "m_charity_donate_null_time")
m_charity_donate_null_time <- here_read("m_charity_donate_null_time")


m_socialising_on_donate_loss_raw <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_charity_donate",
  cens = C,
  shift = f_s_loss,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores  
)
here_save(m_socialising_on_donate_loss_raw, "m_socialising_on_donate_loss_raw")
m_socialising_on_donate_loss_raw <- here_read("m_socialising_on_donate_loss_raw")
m_socialising_on_donate_loss_raw



m_charity_donate_time_z <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_charity_donate_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  outcome_type = "continuous",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores  
)
here_save(m_charity_donate_time_z, "m_charity_donate_time_z")
m_charity_donate_time <- here_read("m_charity_donate_time")
m_charity_donate_time_z

# under null
m_charity_donate_null_time_z <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_charity_donate_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  outcome_type = "continuous",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores  
)
m_charity_donate_null_time_z
here_save(m_charity_donate_null_time_z, "m_charity_donate_null_time_z")

m_charity_donate_null_time_z <- here_read("m_charity_donate_null_time_z")



m_time_community_time <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_community_time_binary",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  outcome_type = "binomial",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores  
)
m_time_community_time
here_save(m_time_community_time, "m_time_community_time")
m_time_community_time <- here_read("m_time_community_time")
m_time_community_time

m_time_community_null_time <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_community_time_binary",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  outcome_type = "binomial",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores  
)

here_save(m_time_community_null_time, "m_time_community_null_time")
m_time_community_null_time <- here_read( "m_time_community_null_time")




m_time_friends_time <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_friends_time_binary",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  outcome_type = "binomial",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores  
)

m_time_friends_time
here_save(m_time_friends_time, "m_time_friends_time")
m_time_friends_time <- here_read("m_time_friends_time")

m_time_friends_null_time <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_friends_time_binary",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  outcome_type = "binomial",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores  
)

here_save(m_time_friends_null_time, "m_time_friends_null_time")
m_time_friends_null_time <- here_read("m_time_friends_null_time")
m_time_friends_null_time



m_time_family_time <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_family_time_binary",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  outcome_type = "binomial",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores  
)

m_time_family_time
here_save(m_time_family_time, "m_time_family_time")

m_time_family_null_time <- lmtp_tmle(
  data = df,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_family_time_binary",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  outcome_type = "binomial",
  weights = df$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores  
)

here_save(m_time_family_null_time, "m_time_family_null_time")
m_time_family_null_time



# contrasts socialising time  ---------------------------------------------
m_hours_charity_time <- here_read("m_hours_charity_time")
m_hours_charity_null_time <- here_read( "m_hours_charity_null_time")
m_hours_charity_time_z <- here_read("m_hours_charity_time_z")
m_hours_charity_null_time_z <-here_read('m_hours_charity_null_time_z')
m_charity_donate_time <- here_read("m_charity_donate_time")
m_charity_donate_null_time <- here_read("m_charity_donate_null_time")
m_charity_donate_time <- here_read("m_charity_donate_time")
m_charity_donate_null_time_z <- here_read("m_charity_donate_null_time_z")




m_time_family_church <- here_read( "m_time_family_time")
m_time_family_null_church <- here_read( "m_time_family_null_time")
m_time_friends_church<- here_read( "m_time_friends_time")
m_time_friends_null_church <- here_read( "m_time_friends_null_time")
m_time_community_church<- here_read( "m_time_community_time")
m_time_community_null_church <- here_read( "m_time_community_null_time")



# SEE BELOW FOR GENUINE CONSTRUCTION SOCIALISING TIME RECEIVED GRAPHS ----------------------------------
# 
# ## SKIP THIS, REPEATED BELOW
# 
# contrast_time_community_full_church <- lmtp_contrast(m_time_community_church,ref = m_time_family_null_church, type = "rr")
# contrast_time_community_full_church
# 
# contrast_time_friends_full_church <- lmtp_contrast(m_time_friends_church,ref = m_time_friends_null_church, type = "rr")
# contrast_time_friends_full_church
# 
# contrast_time_family_full_church <- lmtp_contrast(m_time_family_church,ref = m_time_family_null_church, type = "rr")
# contrast_time_family_full_church
# 
# 
# tab_contrast_time_community_full_church <- margot_tab_lmtp(contrast_time_community_full_church, scale = "RR", new_name = "Help from community: hours socialising >= 1.4 hours pw")
# 
# tab_contrast_time_community_full_church
# 
# 
# tab_hours_only_donate_charity_z <- margot_tab_lmtp(contrast_hours_only_donate_full_time_z, 
#                                                    scale = "RD", 
#                                                    new_name = "Donations LMTP: hours socialising >=2 hours pw")
# tab_hours_only_donate_charity_z
# 
# output_charity_time <- lmtp_evalue_tab(tab_hours_only_donate_charity_z,  
#                                        delta = 1, sd = 1, scale = c("RD"))
# output_charity_time
# 
# tab_charity_time <- rbind(output_volunteering_time,output_charity_time)
# group_tab_charity_time <- group_tab(tab_charity_time, type = "RD")
# group_tab_charity_time
# here_save(group_tab_charity_time,"group_tab_charity_time")
# 
# grouped_outcomes <- group_tab( contrast_hours_full, contrast_hours_full_time,  scale = "RD") 

#  USE THESE CALCULATIONS
# #sd(df_wide_censored_donate$t2_charity_donate, na.rm=TRUE) * .0175
# church_four_hours_donate <- margot_tab_lmtp(contrast_donate_full, scale = "RD", new_name = "LMTP + 4")
# church_four_hours_donate
# 
# str(church_four_hours_donate)
# church_four_hours_donate
# 
# est_donate <- sapply(church_four_hours_donate, round, 0)
# 
# est_donate
# rounded_donate |> 
#   kbl(format = "markdown")
# # calculate proportion 4 or greater
# proportion = sum(df_clean_donate$t1_religion_church_round >= 4) / nrow(df_clean_donate)
# proportion
#  
# theta_donate <- contrast_donate$vals$theta
# theta_donate
# # adult population 
# nz_adult_population = 3989000
# 
# 
# # # min wage workers 
# # min_wage_2023 = 22.70
# 
# # off the cuff
# dollars_donate_gained = theta_donate * nz_adult_population
# dollars_donate_gained
# 
# min_donate_gained =  430 
# min_donate_gained
# # 832193252
# hours_volunteering_gained

### PREJUDICE MODELS 

df_clean <- df

# Warmth time
m_church_t2_warm_asians_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_asians_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_church_t2_warm_asians_z_time
here_save(m_church_t2_warm_asians_z_time, "m_church_t2_warm_asians_z_time")
m_church_t2_warm_asians_z_time <- here_read( "m_church_t2_warm_asians_z_time")
<- here_read("")

null_m_church_t2_warm_asians_z_time  <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_asians_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

null_m_church_t2_warm_asians_z_time 
here_save(null_m_church_t2_warm_asians_z_time , "null_m_church_t2_warm_asians_z_time")
null_m_church_t2_warm_asians_z_time  <- here_read("null_m_church_t2_warm_asians_z_time")

m_church_t2_warm_chinese_z_time  <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_chinese_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_church_t2_warm_chinese_z_time 
here_save(m_church_t2_warm_chinese_z_time , "m_church_t2_warm_chinese_z_time")
m_church_t2_warm_chinese_z_time<- here_read("m_church_t2_warm_chinese_z_time")

null_m_church_t2_warm_chinese_z_time  <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_chinese_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

null_m_church_t2_warm_chinese_z_time
here_save(null_m_church_t2_warm_chinese_z_time , "null_m_church_t2_warm_chinese_z_time ")
null_m_church_t2_warm_chinese_z_time<- here_read("null_m_church_t2_warm_chinese_z_time")




m_c_t2_warm_immigrants_z_time  <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_immigrants_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_immigrants_z_time 
here_save(m_c_t2_warm_immigrants_z_time , "m_c_t2_warm_immigrants_z_time")
m_c_t2_warm_immigrants_z_time <- here_read("m_c_t2_warm_immigrants_z_time")

null_m_c_t2_warm_immigrants_z_time  <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_immigrants_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

null_m_c_t2_warm_immigrants_z_time
here_save(null_m_c_t2_warm_immigrants_z_time , "null_m_c_t2_warm_immigrants_z_time")
null_m_c_t2_warm_immigrants_z_time<- here_read("null_m_c_t2_warm_immigrants_z_time")

m_c_t2_warm_indians_z_time  <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_indians_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_indians_z_time 
here_save(m_c_t2_warm_indians_z_time , "m_c_t2_warm_indians_z_time ")
m_c_t2_warm_indians_z_time<- here_read("m_c_t2_warm_indians_z_time")


m_c_t2_warm_indians_z_null_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_indians_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_indians_z_null_time
here_save(m_c_t2_warm_indians_z_null_time, "m_c_t2_warm_indians_z_null_time")
m_c_t2_warm_indians_z_null_time<- here_read("m_c_t2_warm_indians_z_null_time")




m_c_t2_warm_elderly_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_elderly_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_elderly_z_time
here_save(m_c_t2_warm_elderly_z_time, "m_c_t2_warm_elderly_z_time")
m_c_t2_warm_elderly_z_time<- here_read("m_c_t2_warm_elderly_z_time")

null_m_c_t2_warm_elderly_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_elderly_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

null_m_c_t2_warm_elderly_z_time
here_save(null_m_c_t2_warm_elderly_z_time, "null_m_c_t2_warm_elderly_z_time")
null_m_c_t2_warm_elderly_z_time<- here_read("null_m_c_t2_warm_elderly_z_time")



m_c_t2_warm_maori_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_maori_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_maori_z_time
here_save(m_c_t2_warm_maori_z_time, "m_c_t2_warm_maori_z_time")
m_c_t2_warm_maori_z_time<- here_read("m_c_t2_warm_maori_z_time")

null_m_c_t2_warm_maori_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_maori_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

null_m_c_t2_warm_elderly_z_time
here_save(null_m_c_t2_warm_maori_z_time, "null_m_c_t2_warm_maori_z_time")
null_m_c_t2_warm_elderly_z_time<- here_read("null_m_c_t2_warm_elderly_z_time")



m_c_t2_warm_mental_illness_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_mental_illness_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_mental_illness_z_time
here_save(m_c_t2_warm_mental_illness_z_time, "m_c_t2_warm_mental_illness_z_time")
m_c_t2_warm_mental_illness_z_time <- here_read("m_c_t2_warm_mental_illness_z_time")

null_m_c_t2_warm_mental_illness_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_mental_illness_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_m_c_t2_warm_mental_illness_z_time, "null_m_c_t2_warm_mental_illness_z_time")
null_m_c_t2_warm_mental_illness_z_time
null_m_c_t2_warm_mental_illness_z_time<- here_read("null_m_c_t2_warm_mental_illness_z_time")


m_c_t2_warm_muslims_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_muslims_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_muslims_z_time
here_save(m_c_t2_warm_muslims_z_time, "m_c_t2_warm_muslims_z_time")
m_c_t2_warm_muslims_z_time<- here_read("m_c_t2_warm_muslims_z_time")

null_m_c_t2_warm_muslims_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_muslims_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

null_m_c_t2_warm_muslims_z_time
here_save(null_m_c_t2_warm_muslims_z_time, "null_m_c_t2_warm_muslims_z_time")
null_m_c_t2_warm_muslims_z_time<- here_read("null_m_c_t2_warm_muslims_z_time")



m_c_t2_warm_nz_euro_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_nz_euro_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_nz_euro_z_time
here_save(m_c_t2_warm_nz_euro_z_time, "m_c_t2_warm_nz_euro_z_time")
m_c_t2_warm_nz_euro_z_time<- here_read("m_c_t2_warm_nz_euro_z_time")

null_m_c_t2_warm_nz_euro_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_nz_euro_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)
null_m_c_t2_warm_nz_euro_z_time
here_save(null_m_c_t2_warm_nz_euro_z_time, "null_m_c_t2_warm_nz_euro_z_time")
null_m_c_t2_warm_nz_euro_z_time<- here_read("null_m_c_t2_warm_nz_euro_z_time")




m_c_t2_warm_overweight_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_overweight_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_overweight_z_time
here_save(m_c_t2_warm_overweight_z_time, "m_c_t2_warm_overweight_z_time")
m_c_t2_warm_overweight_z_time<- here_read("m_c_t2_warm_overweight_z_time")

null_m_c_t2_warm_overweight_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_overweight_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

null_m_c_t2_warm_overweight_z_time
here_save(null_m_c_t2_warm_overweight_z_time, "null_m_c_t2_warm_overweight_z_time")
null_m_c_t2_warm_overweight_z_time<- here_read("null_m_c_t2_warm_overweight_z_time")


m_c_t2_warm_pacific_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_pacific_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_pacific_z_time
here_save(m_c_t2_warm_pacific_z_time, "m_c_t2_warm_pacific_z_time")
m_c_t2_warm_pacific_z_time<- here_read("m_c_t2_warm_pacific_z_time")

null_m_c_t2_warm_pacific_z_time<- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_pacific_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

null_m_c_t2_warm_pacific_z_time
here_save(null_m_c_t2_warm_pacific_z_time, "null_m_c_t2_warm_pacific_z_time")
null_m_c_t2_warm_pacific_z_time<- here_read("null_m_c_t2_warm_pacific_z_time")


m_c_t2_warm_refugees_z_time <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_refugees_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

m_c_t2_warm_refugees_z_time
here_save(m_c_t2_warm_refugees_z_time, "m_c_t2_warm_refugees_z_time")
m_c_t2_warm_refugees_z_time<- here_read("m_c_t2_warm_refugees_z_time")

null_m_c_t2_warm_refugees_z_time<- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_warm_refugees_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)
null_m_c_t2_warm_refugees_z_time
here_save(null_m_c_t2_warm_refugees_z_time, "null_m_c_t2_warm_refugees_z_time")
null_m_c_t2_warm_refugees_z_time<- here_read("null_m_c_t2_warm_refugees_z_time")


hours_only_religion_perceive_religious_discrim_z <- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_religion_perceive_religious_discrim_z",
  cens = C,
  shift = f_s,
  mtp = TRUE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

hours_only_religion_perceive_religious_discrim_z
here_save(hours_only_religion_perceive_religious_discrim_z, "hours_only_religion_perceive_religious_discrim_z")
hours_only_religion_perceive_religious_discrim_z<- here_read("hours_only_religion_perceive_religious_discrim_z")

null_hours_only_religion_perceive_religious_discrim_z<- lmtp_tmle(
  data = df_clean,
  trt = A_2,
  baseline = names_base,
  outcome = "t2_religion_perceive_religious_discrim_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 5,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  #  id = "id",
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores 
)

here_save(null_hours_only_religion_perceive_religious_discrim_z, "null_hours_only_religion_perceive_religious_discrim_z")
null_hours_only_religion_perceive_religious_discrim_z


null_hours_only_religion_perceive_religious_discrim_z<- here_read("null_hours_only_religion_perceive_religious_discrim_z")



tab_hours_only_donate_charity <- margot_tab_lmtp(contrast_hours_only_donate_full_time,
                                                 scale = "RD",
                                                 new_name = "Donations LMTP: hours socialising >= 1.4 hours pw")
tab_hours_only_donate_charity



m_hours_charity_time_raw <- here_read("m_hours_charity_time")
m_hours_charity_null_time_raw <- here_read( "m_hours_charity_null_time")

m_hours_charity_time_z <- here_read("m_hours_charity_time_z")
m_hours_charity_null_time_z <- here_read( "m_hours_charity_null_time_z")




# Results -----------------------------------------------------------------

#sqrt(2)
# asians
m_church_t2_warm_asians_z_time<- here_read("m_church_t2_warm_asians_z_time")
null_m_church_t2_warm_asians_z_time<- here_read("null_m_church_t2_warm_asians_z_time")

contrast_church_asians_time <- lmtp_contrast(m_church_t2_warm_asians_z_time,ref = null_m_church_t2_warm_asians_z_time,  type = "additive")
contrast_church_asians_time

tab_contrast_church_asians_time  <- margot_tab_lmtp(contrast_church_asians_time, scale = "RD", 
                                                    new_name = "Warm Asians: social >= 1.4 hours pw")
tab_contrast_church_asians_time 
# chinese
m_church_t2_warm_chinese_z_time  <- here_read("m_church_t2_warm_chinese_z_time")
null_m_church_t2_warm_chinese_z_time <- here_read("null_m_church_t2_warm_chinese_z_time")

contrast_church_chinese_time <- lmtp_contrast(m_church_t2_warm_chinese_z_time,
                                              ref = null_m_church_t2_warm_chinese_z_time,
                                              type = "additive")
contrast_church_chinese_time

tab_contrast_church_chinese_time <- margot_tab_lmtp(contrast_church_chinese_time, scale = "RD", 
                                                    new_name = "Warm Chinese: social >= 1.4 hours pw")
tab_contrast_church_chinese_time

# immigrants
m_c_t2_warm_immigrants_z_time  <- here_read("m_c_t2_warm_immigrants_z_time")
null_m_c_t2_warm_immigrants_z_time  <- here_read("null_m_c_t2_warm_immigrants_z_time")

contrast_church_immigrants_time <- lmtp_contrast(m_c_t2_warm_immigrants_z_time,
                                                 ref = null_m_c_t2_warm_immigrants_z_time,
                                                 type = "additive")
contrast_church_immigrants_time

tab_contrast_church_immigrants_time <- margot_tab_lmtp(contrast_church_immigrants_time, scale = "RD", 
                                                       new_name = "Warm Immigrants: social >= 1.4 hours pw")
tab_contrast_church_immigrants_time


#indians
m_c_t2_warm_indians_z_time  <- here_read("m_c_t2_warm_indians_z_time")
m_c_t2_warm_indians_z_null_time  <- here_read("m_c_t2_warm_indians_z_null_time")

contrast_church_indians_time <- lmtp_contrast(m_c_t2_warm_indians_z_time,
                                              ref = m_c_t2_warm_indians_z_null_time,
                                              type = "additive")


tab_contrast_church_indians_time <- margot_tab_lmtp(contrast_church_indians_time, scale = "RD", 
                                                    new_name = "Warm Indians LMTP: social >= 1.4 hours pw")

tab_contrast_church_indians_time


# elderly
m_c_t2_warm_elderly_z_time  <- here_read("m_c_t2_warm_elderly_z_time")
null_m_c_t2_warm_elderly_z_time <- here_read("null_m_c_t2_warm_elderly_z_time")

contrast_church_elderly_time <- lmtp_contrast(m_c_t2_warm_elderly_z_time,
                                              ref = null_m_c_t2_warm_elderly_z_time,
                                              type = "additive")

contrast_church_elderly_time

tab_contrast_church_elderly_time<- margot_tab_lmtp(contrast_church_elderly_time, scale = "RD", 
                                                   new_name = "Warm Elderly LMTP: social >= 1.4 hours pw")

tab_contrast_church_elderly_time

# maori
m_c_t2_warm_maori_z_time  <- here_read("m_c_t2_warm_maori_z_time")
null_m_c_t2_warm_maori_z_time  <- here_read("null_m_c_t2_warm_maori_z_time")

contrast_church_maori_time <- lmtp_contrast(m_c_t2_warm_maori_z_time,
                                            ref = null_m_c_t2_warm_maori_z_time,
                                            type = "additive")

contrast_church_maori_time
tab_contrast_church_maori_time <- margot_tab_lmtp(contrast_church_maori_time, scale = "RD", 
                                                  new_name = "Warm Maori: social >= 1.4 hours pw")

tab_contrast_church_maori_time

# mental illness
m_c_t2_warm_mental_illness_z_time  <- here_read("m_c_t2_warm_mental_illness_z_time")
null_m_c_t2_warm_mental_illness_z_time <- here_read("null_m_c_t2_warm_mental_illness_z_time")


contrast_church_mental_illness_time <- lmtp_contrast(m_c_t2_warm_mental_illness_z_time,
                                                     ref = null_m_c_t2_warm_mental_illness_z_time,
                                                     type = "additive")


tab_contrast_church_mental_illness_time<- margot_tab_lmtp(contrast_church_mental_illness_time, scale = "RD", new_name = "Warm Mental Illness: social >= 1.4 hours pw")

tab_contrast_church_mental_illness_time


#  muslims
m_c_t2_warm_muslims_z_time <- here_read("m_c_t2_warm_muslims_z_time")
null_m_c_t2_warm_muslims_z_time <- here_read("null_m_c_t2_warm_muslims_z_time")

contrast_church_muslims_time <- lmtp_contrast(m_c_t2_warm_muslims_z_time,
                                              ref = null_m_c_t2_warm_muslims_z_time,
                                              type = "additive")


tab_contrast_church_muslims_time <- margot_tab_lmtp(contrast_church_muslims_time, scale = "RD", 
                                                    new_name = "Warm Muslim: social >= 1.4 hours pw")

tab_contrast_church_muslims_time

# nz euro
m_c_t2_warm_nz_euro_z_time <- here_read("m_c_t2_warm_nz_euro_z_time")
null_m_c_t2_warm_nz_euro_z_time  <- here_read("null_m_c_t2_warm_nz_euro_z_time")

contrast_church_euro_time <- lmtp_contrast(m_c_t2_warm_nz_euro_z_time,
                                           ref = null_m_c_t2_warm_nz_euro_z_time,
                                           type = "additive")


tab_contrast_church_euro_time <- margot_tab_lmtp(contrast_church_euro_time, scale = "RD", 
                                                 new_name = "Warm NZEuro: social >= 1.4 hours pw")

tab_contrast_church_euro_time

# overweight
m_c_t2_warm_overweight_z_time <- here_read("m_c_t2_warm_overweight_z_time")
null_m_c_t2_warm_overweight_z_time<- here_read("null_m_c_t2_warm_overweight_z_time")


contrast_church_overweight_time <- lmtp_contrast(m_c_t2_warm_overweight_z_time,
                                                 ref = null_m_c_t2_warm_overweight_z_time,
                                                 type = "additive")
contrast_church_overweight_time

tab_contrast_church_overweight_time<- margot_tab_lmtp(contrast_church_overweight_time, scale = "RD", 
                                                      new_name = "Warm Overweight: social >= 1.4 hours pw")

tab_contrast_church_overweight_time

# warm pacific_time
m_c_t2_warm_pacific_z_time  <- here_read("m_c_t2_warm_pacific_z_time")
null_m_c_t2_warm_pacific_z_time<- here_read("null_m_c_t2_warm_pacific_z_time")


contrast_church_pacific_time <- lmtp_contrast(m_c_t2_warm_pacific_z_time,
                                              ref = null_m_c_t2_warm_pacific_z_time,
                                              type = "additive")


tab_contrast_church_pacific_time<- margot_tab_lmtp(contrast_church_pacific_time, scale = "RD", 
                                                   new_name = "Warm Pacific: social >= 1.4 hours pw")

tab_contrast_church_pacific_time

#warm refugees
m_c_t2_warm_refugees_z_time <- here_read("m_c_t2_warm_refugees_z_time")
null_m_c_t2_warm_refugees_z_time <- here_read("null_m_c_t2_warm_refugees_z_time")

contrast_church_refugees_time <- lmtp_contrast(m_c_t2_warm_refugees_z_time,
                                               ref = null_m_c_t2_warm_refugees_z_time,
                                               type = "additive")


tab_contrast_church_refugees_time <- margot_tab_lmtp(contrast_church_refugees_time, scale = "RD", 
                                                     new_name = "Warm Refugees: social >= 1.4 hours pw")


tab_contrast_church_refugees_time

hours_only_religion_perceive_religious_discrim_z <- here_read("hours_only_religion_perceive_religious_discrim_z")
null_hours_only_religion_perceive_religious_discrim_z <- here_read("null_hours_only_religion_perceive_religious_discrim_z")

contrast_hours_perceive_religious_discrim_z <- lmtp_contrast(hours_only_religion_perceive_religious_discrim_z,
                                                             ref = null_hours_only_religion_perceive_religious_discrim_z,
                                                             type = "additive")


tab_contrast_contrast_hours_perceive_religious_discrim_z  <- margot_tab_lmtp(contrast_hours_perceive_religious_discrim_z, scale = "RD", 
                                                                             new_name = "Perc. Religious Discrim: social >= 1.4 hours pw")

tab_contrast_contrast_hours_perceive_religious_discrim_z




output_asians_time <- lmtp_evalue_tab(tab_contrast_church_asians_time,  delta = 1, sd = 1, scale = c("RD"))
output_asians_time

output_chinese_time<- lmtp_evalue_tab(tab_contrast_church_chinese_time,  delta = 1, sd = 1, scale = c("RD"))

output_chinese_time


output_immigrants_time <- lmtp_evalue_tab(tab_contrast_church_immigrants_time ,  delta = 1, sd = 1, scale = c("RD"))
output_immigrants_time 



output_indians_time <- lmtp_evalue_tab(tab_contrast_church_indians_time,  delta = 1, sd = 1, scale = c("RD"))
output_indians_time



output_elderly_time <-
  lmtp_evalue_tab(
    tab_contrast_church_elderly_time,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_elderly_time




output_maori_time <-
  lmtp_evalue_tab(
    tab_contrast_church_maori_time,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )



output_maori_time
output_mental_illness_time <-
  lmtp_evalue_tab(
    tab_contrast_church_mental_illness_time,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )





output_muslims_time <-
  lmtp_evalue_tab(
    tab_contrast_church_muslims_time,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )




output_euro_time <-
  lmtp_evalue_tab(
    tab_contrast_church_euro_time,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )




output_overweight_time <-
  lmtp_evalue_tab(
    tab_contrast_church_overweight_time,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_overweight_time





output_pacific_time <-
  lmtp_evalue_tab(
    tab_contrast_church_pacific_time,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )




output_refugees_time <-
  lmtp_evalue_tab(
    tab_contrast_church_refugees_time,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )

output_refugees_time



output_perceive_rel_discrimination_time <-
  lmtp_evalue_tab(
    tab_contrast_contrast_hours_perceive_religious_discrim_z,
    delta = 1,
    sd = 1,
    scale = c("RD")
  )
output_perceive_rel_discrimination_time


#tab

tab_warm_socialising <- rbind(
  output_asians_time,
  output_chinese_time,
  output_immigrants_time,
  output_indians_time,
  output_elderly_time,
  output_maori_time,
  output_mental_illness_time,
  output_muslims_time,
  output_euro_time,
  output_overweight_time,
  output_pacific_time,
  output_refugees_time,
  output_perceive_rel_discrimination_time
)

tab_warm_socialising

here_save(tab_warm_socialising, "tab_warm_socialising")

group_tab_warm_socialising <- group_tab(tab_warm_socialising, type = "RD")
group_tab_warm_socialising

saveRDS(group_tab_warm_socialising, here::here(push_mods, "group_tab_warm_socialising"))


# graph socialising prejudice ---------------------------------------------


# ALERT: GRAPHS WARMTH SOCIALISING ----------------------------------------

group_tab_warm_warm_socialising <- here_read("group_tab_warm_socialising")




plot_warm_socialising <- margot_plot(
  group_tab_warm_warm_socialising,
  type = "RD",
  title = "Socialing effect on prejudice/acceptance",
  subtitle = ">= 1.4  x weekly hours socialising",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_warm_socialising

ggsave(
  plot_warm_socialising,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 6,
  units = "in",
  filename = "plot_warm_socialising.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)


# ALERT: CHARITY SOCIALISING  -------------------------------------------



# contrasts socialising time  ---------------------------------------------
socializing_volunteer_raw  <- here_read("m_hours_charity_time")
socializing_volunteer_null_raw  <- here_read( "m_hours_charity_null_time")
socialising_on_volunteer_loss_raw <-here_read("m_socialising_on_volunteer_loss_raw")
socialising_on_volunteer_loss_z <-here_read("m_socialising_on_volunteer_loss_z") # NOT RUN because no effect
socialising_on_volunteer_loss_raw

socializing_volunteer_z<- here_read("m_hours_charity_time_z")
socializing_volunteer_null_z <-here_read('m_hours_charity_null_time_z')

socializing_donations_raw <- here_read("m_charity_donate_time")
socializing_donations_null_raw <- here_read("m_charity_donate_null_time")
socializing_donations_z <- here_read("m_charity_donate_time_z")
socializing_donations_null_z <- here_read("m_charity_donate_null_time_z"). # NOT RUN because no effect

socialising_on_donate_loss_raw <-here_read("m_socialising_on_donate_loss_raw")
socialising_on_donate_loss_z <-here_read("socialising_on_donate_loss_z")




# donations
contrast_socializing_donations_raw <- lmtp_contrast(socializing_donations_raw,
                                            ref = socializing_donations_null_raw, type = "additive")
contrast_socializing_donations_raw

tab_contrast_socializing_donations_raw <- margot_tab_lmtp(contrast_socializing_donations_raw, scale = "RD", new_name = "Donations: socialising >= 1.4 hours pw")

tab_contrast_socializing_donations_raw

output_tab_contrast_socializing_donations_raw <- lmtp_evalue_tab(tab_contrast_socializing_donations_raw,  
                                              delta = 1, sd = sd_donations, scale = c("RD"))

# HERE
output_tab_contrast_socializing_donations_raw


# loss on donation

contrast_socializing_donations_LOSS_raw <- lmtp_contrast(socialising_on_donate_loss_raw,
                                                    ref = socializing_donations_null_raw, type = "additive")
contrast_socializing_donations_LOSS_raw

tab_contrast_socializing_donations_LOSS_raw <- margot_tab_lmtp(contrast_socializing_donations_LOSS_raw, scale = "RD", new_name = "Donations: lost all socialising")

tab_contrast_socializing_donations_LOSS_raw

output_tab_contrast_socializing_donations_LOSS_raw<- lmtp_evalue_tab(tab_contrast_socializing_donations_LOSS_raw,  
                                                                 delta = 1, sd = sd_donations, scale = c("RD"))
tab_contrast_socializing_donations_LOSS_raw
output_tab_contrast_socializing_donations_LOSS_raw

here_save(output_tab_contrast_socializing_donations_LOSS_raw, "output_tab_contrast_socializing_donations_LOSS_raw")

# HERE
output_tab_contrast_socializing_donations_LOSS_raw



# donations
contrast_socializing_donations_z <- lmtp_contrast(socializing_donations_z,
                                                    ref = socializing_donations_null_z, type = "additive")
contrast_socializing_donations_z

tab_contrast_socializing_donations_z <- margot_tab_lmtp(contrast_socializing_donations_z, scale = "RD", new_name = "Donations: socialising >= 1.4 hours pw")

tab_contrast_socializing_donations_z

output_tab_contrast_socializing_donations_z <- lmtp_evalue_tab(tab_contrast_socializing_donations_z,  
                                                                 delta = 1, sd = 1, scale = c("RD"))

# HERE
output_tab_contrast_socializing_donations_z
output_tab_contrast_socializing_donations_raw


# z scores 
contrast_socializing_volunteer_z <- lmtp_contrast(socializing_volunteer_z,
                                            ref = socializing_volunteer_null_z, type = "additive")
contrast_socializing_volunteer_z

tab_contrast_socializing_volunteer_z<- margot_tab_lmtp(contrast_socializing_volunteer_z, scale = "RD", new_name = "Volunteering: socialising >= 1.4 hours pw")
tab_contrast_socializing_volunteer_z

output_tab_contrast_socializing_volunteer_z <- lmtp_evalue_tab(tab_contrast_socializing_volunteer_z,  
                                            delta = 1, sd = 1, scale = c("RD"))


#HERE 
output_tab_contrast_socializing_volunteer_z



# raw
contrast_socializing_volunteer_raw <- lmtp_contrast(socializing_volunteer_raw,
                                                  ref = m_hours_charity_null_time_raw, type = "additive")
contrast_socializing_volunteer_raw

tab_contrast_socializing_volunteer_raw<- margot_tab_lmtp(contrast_socializing_volunteer_raw, scale = "RD", new_name = "Volunteering: socialising >= 1.4 hours pw")
tab_contrast_socializing_volunteer_raw

output_tab_contrast_socializing_volunteer_raw <- lmtp_evalue_tab(tab_contrast_socializing_volunteer_raw,  
                                                               delta = 1, sd = sd_volunteer, scale = c("RD"))

# LOSS RAW


contrast_socializing_volunteer_LOSS_raw <- lmtp_contrast(socialising_on_volunteer_loss_raw,
                                                    ref = socializing_volunteer_null_raw, type = "additive")
contrast_socializing_volunteer_LOSS_raw


tab_contrast_socializing_volunteer_LOSS_raw<- margot_tab_lmtp(contrast_socializing_volunteer_LOSS_raw, scale = "RD", new_name = "Volunteering: lost all socialising")
tab_contrast_socializing_volunteer_LOSS_raw

output_tab_contrast_socializing_volunteer_LOSS_raw <- lmtp_evalue_tab(tab_contrast_socializing_volunteer_LOSS_raw,  
                                                                 delta = 1, sd = sd_volunteer, scale = c("RD"))

## HERE
output_tab_contrast_socializing_volunteer_LOSS_raw

### HERE
output_tab_contrast_socializing_volunteer_z
output_tab_contrast_socializing_volunteer_raw




tab_socializing_prosocial_behaviour_raw <- rbind(output_tab_contrast_socializing_donations_raw ,output_tab_contrast_socializing_volunteer_raw)
here_save(tab_socializing_prosocial_behaviour_raw, "tab_socializing_prosocial_behaviour_raw")
tab_socializing_prosocial_behaviour_raw<- here_read("tab_socializing_prosocial_behaviour_raw")


tab_socializing_prosocial_behaviour_z <- rbind(output_tab_contrast_socializing_donations_z,output_tab_contrast_socializing_volunteer_z)
here_save(tab_socializing_prosocial_behaviour_z, "tab_socializing_prosocial_behaviour_z")
tab_socializing_prosocial_behaviour_z<- here_read("tab_socializing_prosocial_behaviour_z")



group_tab_socializing_prosocial_behaviour_raw <- group_tab(tab_socializing_prosocial_behaviour_raw, type = "RD")
here_save(group_tab_socializing_prosocial_behaviour_raw, "group_tab_socializing_prosocial_behaviour_raw")
group_tab_socializing_prosocial_behaviour_raw<- here_read("group_tab_socializing_prosocial_behaviour_raw")


group_tab_socializing_prosocial_behaviour_z <- group_tab(tab_socializing_prosocial_behaviour_z, type = "RD")
here_save(group_tab_socializing_prosocial_behaviour_z, "group_tab_socializing_prosocial_behaviour_z")
group_tab_socializing_prosocial_behaviour_z<- here_read("group_tab_socializing_prosocial_behaviour_z")



plot_socializing_prosocial<- margot_plot(
  group_tab_socializing_prosocial_behaviour_z,
  type = "RD",
  title = "Socialising effect on charity",
  subtitle = ">= 1.4  x weekly hours socialising",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_socializing_prosocial


ggsave(
  plot_behaviour_socialising,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 6,
  units = "in",
  filename = "plot_charity_time.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)




# ALERT SOCiALISING HELP FROM OTHERS ----------------------------------

socializing_help_from_family_rr  <- here_read( "m_time_family_time")
socializing_help_from_family_null_rr <- here_read( "m_time_family_null_time")
socializing_help_from_friends_rr<- here_read( "m_time_friends_time")
socializing_help_from_friends_null_rr <- here_read( "m_time_friends_null_time")
socializing_help_from_community_rr<- here_read( "m_time_community_time")
socializing_help_from_community_null_rr <- here_read( "m_time_community_null_time")



contrast_socializing_help_from_family_rr <- lmtp_contrast(socializing_help_from_family_rr,ref = socializing_help_from_family_null_rr, type = "rr")
contrast_socializing_help_from_family_rr


tab_contrast_socializing_help_from_family_rr <- margot_tab_lmtp(contrast_socializing_help_from_family_rr, scale = "RR", 
                                                                      new_name = "Help from Family: socialising >= 1.4 hours pw")

tab_contrast_socializing_help_from_family_rr


output_tab_contrast_socializing_help_from_family_rr <-
  lmtp_evalue_tab(
    tab_contrast_socializing_help_from_family_rr,
    delta = 1,
    sd = 1,
    scale = c("RR")
  )

# HERE
output_tab_contrast_socializing_help_from_family_rr

### FRIENDS

contrast_socializing_help_from_friends_rr <- lmtp_contrast(socializing_help_from_friends_rr,ref = socializing_help_from_friends_null_rr, type = "rr")
contrast_socializing_help_from_friends_rr

tab_contrast_socializing_help_from_friends_rr  <- margot_tab_lmtp(contrast_socializing_help_from_friends_rr, scale = "RR", 
                                                                  new_name = "Help from Friends: socialising >= 1.4 hours pw")

tab_contrast_socializing_help_from_friends_rr

output_tab_contrast_socializing_help_from_friends_rr <-
  lmtp_evalue_tab(
    tab_contrast_socializing_help_from_friends_rr,
    delta = 1,
    sd = 1,
    scale = c("RR")
  )
output_tab_contrast_socializing_help_from_friends_rr



## COMMUNITY

contrast_socializing_help_from_community_rr <- lmtp_contrast(socializing_help_from_community_rr,ref = socializing_help_from_community_null_rr, type = "rr")
contrast_socializing_help_from_community_rr

tab_contrast_socializing_help_from_community_rr  <- margot_tab_lmtp(contrast_socializing_help_from_community_rr, scale = "RR", 
                                                             new_name = "Help from Community: socialising >= 1.4 hours pw")

tab_contrast_socializing_help_from_community_rr
output_tab_contrast_socializing_help_from_community_rr <-
  lmtp_evalue_tab(
    tab_contrast_socializing_help_from_community_rr,
    delta = 1,
    sd = 1,
    scale = c("RR")
  )
output_tab_contrast_socializing_help_from_community_rr
output_tab_contrast_socializing_help_from_community_rr

tab_socialising_help_received<- rbind( output_tab_contrast_socializing_help_from_family_rr, output_tab_contrast_socializing_help_from_friends_rr, output_tab_contrast_socializing_help_from_community_rr ) 


tab_socialising_help_received
here_save(tab_socialising_help_received, "tab_socialising_help_received")
tab_socialising_help_received<- here_read("tab_socialising_help_received")


group_tab_socialising_help_received <- group_tab(tab_socialising_help_received, type = "RR")

saveRDS(group_tab_socialising_help_received, here::here(push_mods, "group_tab_socialising_help_received"))


group_tab_socialising_help_received <- here_read("group_tab_socialising_help_received")

plot_help_time <- margot_plot(
  group_tab_socialising_help_received,
  type = "RR",
  title = "Socialising effect on help recieved",
  subtitle = ">= 1.4  x weekly hours socialising",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = 0,
  x_lim_lo = 0,
  x_lim_hi =  2
)

plot_help_time
ggsave(
  plot_help_time,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 6,
  units = "in",
  filename = "plot_help_time.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



# omni-plot ---------------------------------------------------------------


omni_plot_charity <- plot_charity_time / plot_charity_church_z
omni_plot_charity


ggsave(
  omni_plot_charity,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 12,
  units = "in",
  filename = "omni_plot_charity.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

plot_community_time

omni_plot_help_received <- plot_help_time/ plot_group_tab_time_church

omni_plot_help_received



ggsave(
  omni_plot_help_received,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 12,
  units = "in",
  filename = "omni_plot_help_received.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)


omni_plot_prejudice <- plot_warm_socialising / plot_prejudice_church
omni_plot_prejudice

ggsave(
  omni_plot_prejudice,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 12,
  units = "in",
  filename = "omni_plot_prejudice.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

# ALERT WORK OUT PRACTICAL EFFECTS  ---------------------------------------


# practical effects -------------------------------------------------------



#read estimates of unstandardised models 
m_charity_donate_raw <- here_read("m_charity_donate")
m_charity_donate_null_raw <- here_read("m_charity_donate_null")

# obtain contrast
contrast_donate_full_raw <- lmtp_contrast(m_charity_donate_raw,
                                          ref = m_charity_donate_null_raw,
                                          type = "additive")

# contrast 
contrast_donate_full_raw


# counterfactual giving 
counterfactual_charity_donate <- contrast_donate_full_raw$vals$shift

# actual giving 
estimate_charity_donate <- contrast_donate_full_raw$vals$ref

# nz_adult_population in 2021
nz_adult_population = 3989000


factual_charity_estimate = nz_adult_population * estimate_charity_donate
counterfactual_factual_charity_estimate = nz_adult_population * counterfactual_charity_donate

# difference
counterfactual_difference_charity_donate = counterfactual_factual_charity_estimate - factual_charity_estimate

counterfactual_difference_charity_donate

# almost 3 billion NZD 
# 2,806,882,916

# nz annual budget in 2021
# nz_annual_budget = 14494000000 * 4
# nz_annual_budget
# 
# counterfactual_difference_charity_donate/ nz_annual_budget
# 
# counterfactual_difference_charity_donate
# 
# options(scipen=999)
# nz_annual_budget
# 
# #
# 0.04841457
# 


