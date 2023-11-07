
## This is the script for the study INKUK is leading on trust in science and environmental attitudes
## Don Hine to lead, Inkuk, Chris


# libraries for jb (when internet is not accessible)
# read libraries
source("/Users/joseph/GIT/templates/functions/libs2.R")

# read functions
source("/Users/joseph/GIT/templates/functions/funs.R")

# experimental functions (more functions)
source(
  "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
)

# read data/ set to path in your computer
pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs_refactor/nzavs_data_23"
  )

# for saving models. # set path fo your computer
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/nzvs_mods/00drafts/23-lmtp-ow-env-meaning"
  )

# read data: note that you need use the arrow package in R
dat <- arrow::read_parquet(pull_path)



# check path:is this correct?  check so you know you are not overwriting other directors
push_mods


# for information on LMPT go to 
#https://github.com/nt-williams/lmtp
#devtools::install_github("nt-williams/lmtp@devel")

# for modified treatment policies
library("lmtp")
push_mods

# set exposure here
nzavs_exposure <-
  "lifemeaning"

# define exposure
A <-
  "t1_lifemeaning"

# set exposure variable, can be both the continuous and the coarsened, if needed
exposure_var = c("lifemeaning",
                 "not_lost") #

# define shift function (if any one is average make them average, otherwise leave alone)
# f <- function(data, trt) {
#   ifelse(data[[trt]] <= 0, 0,  data[[trt]])
# }



# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 5

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(1234)

# set cores for estimation
library(future)
plan(multisession)
n_cores <- parallel::detectCores()

# super learner libraries
sl_lib <- c("SL.glmnet",
            "SL.ranger",
            "SL.xgboost")

# libraries
library(SuperLearner)
library(ranger)
library(xgboost)
library(glmnet)

# boost speed
SL.xgboost = list(tree_method = 'gpu_hist')


# check options
listWrappers()


# import data

dat_long <- dat |>
  arrange(id, wave) |>
  rename(
    trust_science_high_confidence_scientific_community = science_trust01,
    trust_science_our_society_places_too_much_emphasis_reversed =
      science_trust02r
  ) |>
  mutate(covid19_timeline = as.factor(covid19_timeline)) |>
  dplyr::rename(sample_weights = w_gend_age_euro) |>
  arrange(id, wave) |>
  rowwise(wave) |>
  mutate(power_no_control_composite = mean(c(
    power_self_nocontrol, power_others_control
  ), na.rm = TRUE)) |>
  mutate(kessler_latent_depression =  mean(
    c(kessler_depressed, kessler_hopeless, kessler_worthless),
    na.rm = TRUE
  )) |>
  mutate(kessler_latent_anxiety  = mean(c(
    kessler_effort, kessler_nervous, kessler_restless
  ), na.rm = TRUE)) |>
  ungroup() |>
  mutate(religion_church_round = round(ifelse(religion_church >= 8, 8, religion_church), 0)) |>
  mutate(male = as.numeric(male)) |>
  mutate(total_siblings_factor = ordered(round(
    ifelse(total_siblings > 7, 7, total_siblings), 0
  ))) |>
  mutate(
  #  eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban)
   # education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  dplyr::mutate(
    friends_money = ifelse(friends_money < 0, 0, friends_money),
    # someone gave neg number
    household_inc_log = log(household_inc + 1),
    hours_children_log = log(hours_children + 1),
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1),
    hours_exercise_log = log(hours_exercise + 1)
  ) |>
  dplyr::mutate(sample_origin = sample_origin_names_combined) |>  #shorter name
  select(
    "wave",
    "year_measured",
    "id",
    "lifemeaning",
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
    "kessler_latent_anxiety",
    "kessler_latent_depression",
    # value label 0    No 1   Yes
    "eth_cat",
    #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    "employed",
    # Are you currently employed? (this includes self-employment or casual work)
    # "gen_cohort",
    "household_inc_log",
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
    "total_siblings_factor",
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
    "hours_children_log",
    #Hours - Looking after children
    "hours_work_log",
    #Hours - Working in paid employment
    "hours_housework_log",
    # Hours - Housework/cooking
    "hours_exercise_log",
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
    "sample_weights",
    # sample_weights.
    # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
    "gratitude",
    ## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of peopl
    "modesty",
    # see above
    "env_climate_chg_real",
    "env_climate_chg_cause",
    # "Climate change is caused by humans"
    "env_climate_chg_concern",
    "belong",
    "support",
    #"I am deeply concerned about climate change."
    "env_sat_nz_environment",
    "trust_science_high_confidence_scientific_community",
    "trust_science_our_society_places_too_much_emphasis_reversed",
    "alert_level_combined",
    "sample_origin",
  ) |>
  arrange(id, wave) %>%
  dplyr::filter((wave == 2019 & year_measured  == 1) |
                  (wave == 2020  &
                     year_measured  == 1) |
                  (wave == 2021)) |>  # Eligibility criteria  Observed in 2018/2019 & Outcomes in 2020 or 2021
  group_by(id) |>
  ## MAKE SURE YOU HAVE ELIGIBILITY CRITERIA
  dplyr::mutate(meets_criteria_baseline = ifelse(year_measured == 1 &
                                                   !is.na(!!sym(nzavs_exposure)), 1, 0)) |>  # using R lang
  dplyr::mutate(k_19 =  ifelse(wave == 2019 &
                                 meets_criteria_baseline == 1, 1, 0)) |>   # creating an indicator for the first wave. Inclusion criteria
  dplyr::mutate(h_19 = mean(k_19, na.rm = TRUE)) |>   # Hack
  dplyr::mutate(k_20 =  ifelse(wave == 2020 &
                                 meets_criteria_baseline == 1, #  Inclusion criteria
                               1,
                               0)) |>   # creating an indicator for the first wave; note that we allow people
  dplyr::mutate(h_20 = mean(k_20, na.rm = TRUE)) |>  # Hack
  dplyr::filter(h_19 > 0) |>  # hack to enable repeat of baseline
  dplyr::filter(h_20 > 0) |>  # hack to enable repeat of baseline
  ungroup() |>
  mutate(time = as.numeric(as.character(wave))-2018) |>
  mutate(wave = time) |>
  arrange(id, wave) |>
  droplevels() |>
  mutate(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    # not_lost = ifelse(lead(year_measured)== -1, 0, not_lost,
    # not_lost = ifelse(lead(year_measured) == 0, 0, not_lost,
    not_lost = ifelse(is.na(not_lost) &
                        year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
    
  ) |>
  select(-h_19, -k_20, -h_20, -k_19) |>
  data.frame()

N <- n_unique(dat_long$id) # 33281
N

# save N for manus
here_save(N, "N_participants")



# positivity --------------------------------------------------------------



# check col names
colnames(dat)


dt_positivity_full <- dat_long|>
  filter(wave == 1 | wave == 2) |> 
 # select(wave, id, lifemeaning) |> 
  mutate(lifemeaning_round = round(lifemeaning, 0))

dt_positivity_full$lifemeaning_round


out <- msm::statetable.msm(lifemeaning_round, id, data = dt_positivity_full)

out

#t_tab_cats_labels <- c("No Cats", "Cats")
# transition table
t_tab_transition <- transition_table(out)
t_tab_transition

here_save(t_tab_transition, "t_tab_transition")


dat$env_efficacy_action_belief
dat$env_efficacy_action_feeling

dat$religion_identification_level

# assocation only example
summary( lm( env_climate_chg_real ~ lifemeaning_round, data = dt_positivity_full) )


# eyeball distribution
# table(dat_long$wave)
dt_20 <- dat_long |>
  filter(year_measured ==1 & wave == 2) |> 
  mutate(lifemeaning_z =
           scale(lifemeaning))

hist(dt_20$lifemeaning_z)
table(dt_20$lifemeaning_z)



mean_exposure <- mean(dt_20$lifemeaning,
                      na.rm = TRUE)
# mean_exposure
# 
# max_score <- max(dt_20$lifemeaning_z, na.rm = TRUE)
# max_score
# 
# sd_exposure <- sd(dt_19$trust_science_our_society_places_too_much_emphasis_reversed,
#                   na.rm = TRUE)
# sd_exposure
# 
# one_point_in_sd_units <- 1/sd_exposure
# one_point_in_sd_units

# half_sd <- sd_exposure / 2
# half_sd




# histograms --------------------------------------------------------------


# generate bar plot
graph_density_of_exposure <-
  coloured_histogram(dt_20,
                     col_name = "lifemeaning",
                     scale_min = 1,
                     scale_max = 7)

graph_density_of_exposure

ggsave(
  graph_density_of_exposure,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "graph_density_of_exposure.jpg",
  device = 'jpg',
  limitsize = FALSE,
  dpi = 600
)



coloured_histogram_sd <- function(df, col_name, binwidth = 30) {
  # Compute statistics
  avg_val <- mean(df[[col_name]], na.rm = TRUE)
  std_val <- sd(df[[col_name]], na.rm = TRUE)
  
  # Create data frame for v-lines and their descriptions
  line_data <- data.frame(
    value = c(avg_val, avg_val - std_val, avg_val + std_val),
    description = c("Mean", "Mean - 1 SD", "Mean + 1 SD"),
    color = c("black", "dodgerblue", "gold2")
  )
  
  # Create the plot
  p <- ggplot(df, aes(x = !!sym(col_name))) +
    geom_histogram(aes(y = ..count..),
                   binwidth = binwidth,
                   fill = "grey60") +
    geom_vline(data = line_data,
               aes(xintercept = value, color = description),
               size = 1.5) +
    geom_segment(
      data = line_data,
      aes(
        x = avg_val,
        y = 0,
        xend = value,
        yend = 0,
        color = description
      ),
      arrow = arrow(
        type = "closed",
        ends = "last",
        length = unit(0.2, "inches")
      ),
      size = 1.5
    ) +
    scale_color_manual(values = c(
      "Mean - 1 SD" = "dodgerblue",
      "Mean + 1 SD" = "gold2"
    )) +
    labs(title = "Histogram with Mean and Standard Deviation Intervals",
         subtitle = "Arrows indicate one standard deviation from the mean.",
         color = "Legend") +
    theme_minimal()
  
  return(p)
}


standard_deviation_exposure <-
  coloured_histogram_sd(dt_20, col_name = "lifemeaning", binwidth = .2)

standard_deviation_exposure

ggsave(
  standard_deviation_exposure,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "standard_deviation_exposure.jpeg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)






max_score <- 7
min_score <- 1

#  increase everyone by one point, contrasted with what they would be anyway.
# only use this function for raw scores

f_1 <- function(data, trt) {
  ifelse(data[[trt]] <= max_score - 1, data[[trt]] + 1,  max_score)
}


# Define your baseline, exposure, and outcome variables without any prefix
str(dat_long$sample_origin_names_combined)

#
baseline_vars = c(
  "male",
  "age",
  "education_level_coarsen",
  # factors
  "eth_cat",
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  #"bigger_doms", #religious denomination
  "sample_origin_names_combined",
  "nz_dep2018",
  "nzsei13",
  "born_nz",
  "hlth_disability",
  # "hlth_bmi",
  # "pwi", # pwi
  # "kessler6_sum",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
 # "support",
  #soc support
 # "belong",
  # social belonging
  #"total_siblings_factor",
  #  "smoker", # smoker
  # "sfhealth",
  # "alcohol_frequency", measured with error
  # "alcohol_intensity",
  # "hours_family_log",
  # "hours_friends_log",
  # "hours_community_log",
  # "hours_community_sqrt_round",
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
  # "religion_church_round",
  # "religion_religious", #
  #  "religion_spiritual_identification",
  "religion_identification_level",
  #  "religion_religious",
  "religion_church_binary",
  #  "religion_prayer_binary",
  #  "religion_scripture_binary",
  #"religion_believe_god",
  #"religion_believe_spirit",
  "sample_weights",
  "alert_level_combined"
)

# check
exposure_var

outcome_vars <- c("env_climate_chg_real", "env_climate_chg_cause","env_climate_chg_concern", "env_sat_nz_environment")



prep_coop_all <- margot_wide_impute_baseline(
  dat_long,
  baseline_vars = baseline_vars,
  exposure_var = exposure_var,
  outcome_vars = outcome_vars
)



push_mods
# save function -- will save to your "push_mod" directory
here_save(prep_coop_all, "prep_coop_all")

# read function
prep_coop_all <- here_read("prep_coop_all")

table( prep_coop_all$t1_not_lost )

head(prep_coop_all)

#check 
naniar::vis_miss(prep_coop_all, warn_large_data = FALSE)

# arrange data for analysis -----------------------------------------------
# spit and shine

df_wide_censored <-
  prep_coop_all |>
  mutate(
    t0_eth_cat = as.factor(t0_eth_cat)#,
    #   t0_smoker_binary = as.integer(ifelse(t0_smoker > 0, 1, 0))#,
    #    t2_smoker_binary = as.integer(ifelse(t2_smoker > 0, 1, 0)),
  ) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_"))
#check
head(df_wide_censored)
dim(df_wide_censored)
str(df_wide_censored)

df_wide_censored$t0_lifemeaning

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
        !t0_sample_weights &
        !t0_lifemeaning,
      ~ scale(.x),
      .names = "{col}_z"
    )
  ) |>
  select(
    where(is.factor),
    #  t0_smoker_binary,
    t0_not_lost,
    t0_lifemeaning,
    t0_sample_weights,
    t1_not_lost,
    t1_lifemeaning,
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
dev.off()



# check path
push_mods

# save
here_save(df_clean, "df_clean")
df_clean <- here_read("df_clean")


# check
f_1

# get names
names_base <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     -t0_not_lost) |> colnames()

names_base

#### SET VARIABLE NAMES: Customise for each outcomewide model
#  model
A


C <- c("t1_not_lost")

#L <- list(c("L1"), c("L2"))
W <- c(paste(names_base, collapse = ", "))

# check
print(W)


# check shift
f

# model 1
# t2_env_climate_chg_real_z <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base,
#   outcome = "t2_env_climate_chg_real_z",
#   cens = C,
#   shift = f,
#   outcome_type = "continuous",
#   mtp = TRUE,
#   folds = 5,
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_env_climate_chg_real_z
# here_save(t2_env_climate_chg_real_z, "t2_env_climate_chg_real_z")



t2_env_climate_chg_real_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_env_climate_chg_real_z",
  cens = C,
  shift = f_1,
  outcome_type = "continuous",
  mtp = TRUE,
  folds = 5,
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_env_climate_chg_real_z_1
here_save(t2_env_climate_chg_real_z_1, "t2_env_climate_chg_real_z_1")



t2_env_climate_chg_real_z_null  <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_env_climate_chg_real_z",
  cens = C,
  shift = NULL,
  outcome_type = "continuous",
  mtp = FALSE,
  folds = 5,
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_env_climate_chg_real_z_null
here_save(t2_env_climate_chg_real_z_null, "t2_env_climate_chg_real_z_null")



# model 2 t2_env_climate_chg_cause_z
# t2_env_climate_chg_cause_z <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base,
#   outcome = "t2_env_climate_chg_cause_z",
#   cens = C,
#   shift = f,
#   outcome_type = "continuous",
#   mtp = TRUE,
#   folds = 5,
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_env_climate_chg_cause_z
# here_save(t2_env_climate_chg_cause_z, "t2_env_climate_chg_cause_z")


t2_env_climate_chg_cause_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_env_climate_chg_cause_z",
  cens = C,
  shift = f_1,
  outcome_type = "continuous",
  mtp = TRUE,
  folds = 5,
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_env_climate_chg_cause_z_1
here_save(t2_env_climate_chg_cause_z_1, "t2_env_climate_chg_cause_z_1")


t2_env_climate_chg_cause_z_null  <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_env_climate_chg_cause_z",
  cens = C,
  shift = NULL,
  outcome_type = "continuous",
  mtp = FALSE,
  folds = 5,
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_env_climate_chg_cause_z_null
here_save(t2_env_climate_chg_cause_z_null, "t2_env_climate_chg_cause_z_null")


# model 3  t2_env_climate_chg_concern_z

# t2_env_climate_chg_concern_z <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base,
#   outcome = "t2_env_climate_chg_concern_z",
#   cens = C,
#   shift = f,
#   outcome_type = "continuous",
#   mtp = TRUE,
#   folds = 5,
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_env_climate_chg_concern_z
# here_save(t2_env_climate_chg_concern_z, "t2_env_climate_chg_concern_z")


t2_env_climate_chg_concern_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_env_climate_chg_concern_z",
  cens = C,
  shift = f_1,
  outcome_type = "continuous",
  mtp = TRUE,
  folds = 5,
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_env_climate_chg_concern_z_1
here_save(t2_env_climate_chg_concern_z_1, "t2_env_climate_chg_concern_z_1")


t2_env_climate_chg_concern_z_null  <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_env_climate_chg_concern_z",
  cens = C,
  shift = NULL,
  outcome_type = "continuous",
  mtp = FALSE,
  folds = 5,
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_env_climate_chg_concern_z_null
here_save(t2_env_climate_chg_concern_z_null, "t2_env_climate_chg_concern_z_null")


# model 4 t2_env_sat_nz_environment_z

# t2_env_sat_nz_environment_z <- lmtp_tmle(
#   data = df_clean,
#   trt = A,
#   baseline = names_base,
#   outcome = "t2_env_sat_nz_environment_z",
#   cens = C,
#   shift = f,
#   outcome_type = "continuous",
#   mtp = TRUE,
#   folds = 5,
#   weights = df_clean$t0_sample_weights,
#   learners_trt = sl_lib,
#   learners_outcome = sl_lib,
#   parallel = n_cores
# )
# 
# t2_env_sat_nz_environment_z
# here_save(t2_env_sat_nz_environment_z, "t2_env_sat_nz_environment_z")

t2_env_sat_nz_environment_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_env_sat_nz_environment_z",
  cens = C,
  shift = f_1,
  outcome_type = "continuous",
  mtp = TRUE,
  folds = 5,
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_env_sat_nz_environment_z_1
here_save(t2_env_sat_nz_environment_z_1, "t2_env_sat_nz_environment_z_1")



t2_env_sat_nz_environment_z_null  <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_env_sat_nz_environment_z",
  cens = C,
  shift = NULL,
  outcome_type = "continuous",
  mtp = FALSE,
  folds = 5,
  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_env_sat_nz_environment_z_null
here_save(t2_env_sat_nz_environment_z_null, "t2_env_sat_nz_environment_z_null")

# contrasts

# contrasts 1 climate change is real
#t2_env_climate_chg_real_z <- here_read("t2_env_climate_chg_real_z")
t2_env_climate_chg_real_z_1 <- here_read("t2_env_climate_chg_real_z_1")
t2_env_climate_chg_real_z_null <-
  here_read("t2_env_climate_chg_real_z_null")

# # first contrast
# contrast_t2_env_climate_chg_real_z <-
#   lmtp_contrast(t2_env_climate_chg_real_z,
#                 ref = t2_env_climate_chg_real_z_null,
#                 type = "additive")
# 
# 
# tab_contrast_t2_env_climate_chg_real_z <-
#   margot_tab_lmtp(contrast_t2_env_climate_chg_real_z,
#                   scale = "RD",
#                   new_name = "Climate change is real.")
# 
# tab_contrast_t2_env_climate_chg_real_z
# 
# 
# out_tab_contrast_t2_env_climate_chg_real_z<-
#   lmtp_evalue_tab(tab_contrast_t2_env_climate_chg_real_z,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_env_climate_chg_real_z


# second contrats
contrast_t2_env_climate_chg_real_z_1 <-
  lmtp_contrast(t2_env_climate_chg_real_z_1,
                ref = t2_env_climate_chg_real_z_null,
                type = "additive")


tab_contrast_t2_env_climate_chg_real_z_1 <-
  margot_tab_lmtp(contrast_t2_env_climate_chg_real_z_1,
                  scale = "RD",
                  new_name = "Climate change is real.")

tab_contrast_t2_env_climate_chg_real_z_1
tab_contrast_t2_env_climate_chg_real_z

out_tab_contrast_t2_env_climate_chg_real_z_1<-
  lmtp_evalue_tab(tab_contrast_t2_env_climate_chg_real_z_1,
                  scale = c("RD"))

tab_contrast_t2_env_climate_chg_real_z_1



# contrasts: model 2 t2_env_climate_chg_cause_z
# climate change is real
#t2_env_climate_chg_cause_z <- here_read("t2_env_climate_chg_cause_z")
t2_env_climate_chg_cause_z_1 <- here_read("t2_env_climate_chg_cause_z_1")

t2_env_climate_chg_cause_z_null <-
  here_read("t2_env_climate_chg_cause_z_null")

# first contrast
# contrast_t2_env_climate_chg_cause_z <-
#   lmtp_contrast(t2_env_climate_chg_cause_z,
#                 ref = t2_env_climate_chg_cause_z_null,
#                 type = "additive")
# 
# 
# tab_contrast_t2_env_climate_chg_cause_z <-
#   margot_tab_lmtp(contrast_t2_env_climate_chg_cause_z,
#                   scale = "RD",
#                   new_name = "Climate change is caused by humans.")
# 
# tab_contrast_t2_env_climate_chg_cause_z
# 
# 
# out_tab_contrast_t2_env_climate_chg_cause_z<-
#   lmtp_evalue_tab(tab_contrast_t2_env_climate_chg_cause_z,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_env_climate_chg_cause_z

# second contrast
contrast_t2_env_climate_chg_cause_z_1 <-
  lmtp_contrast(t2_env_climate_chg_cause_z_1,
                ref = t2_env_climate_chg_cause_z_null,
                type = "additive")


tab_contrast_t2_env_climate_chg_cause_z_1 <-
  margot_tab_lmtp(contrast_t2_env_climate_chg_cause_z_1,
                  scale = "RD",
                  new_name = "Climate change is caused by humans.")

tab_contrast_t2_env_climate_chg_cause_z_1


out_tab_contrast_t2_env_climate_chg_cause_z_1<-
  lmtp_evalue_tab(tab_contrast_t2_env_climate_chg_cause_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_env_climate_chg_cause_z_1

#  contrasts: 3: t2_env_climate_chg_concern_z
#t2_env_climate_chg_concern_z <- here_read("t2_env_climate_chg_concern_z")
t2_env_climate_chg_concern_z_1 <- here_read("t2_env_climate_chg_concern_z_1")
t2_env_climate_chg_concern_z_null <-
  here_read("t2_env_climate_chg_concern_z_null")
# 
# # first contrast
# contrast_t2_env_climate_chg_concern_z <-
#   lmtp_contrast(t2_env_climate_chg_concern_z,
#                 ref = t2_env_climate_chg_concern_z_null,
#                 type = "additive")
# 
# 
# tab_contrast_t2_env_climate_chg_concern_z <-
#   margot_tab_lmtp(contrast_t2_env_climate_chg_concern_z,
#                   scale = "RD",
#                   new_name = "I am deeply concerned about climate change.")
# 
# tab_contrast_t2_env_climate_chg_concern_z
# 
# 
# out_tab_contrast_t2_env_climate_chg_concern_z<-
#   lmtp_evalue_tab(tab_contrast_t2_env_climate_chg_concern_z,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_env_climate_chg_concern_z
# 

# second contrast
contrast_t2_env_climate_chg_concern_z_1 <-
  lmtp_contrast(t2_env_climate_chg_concern_z_1,
                ref = t2_env_climate_chg_concern_z_null,
                type = "additive")


tab_contrast_t2_env_climate_chg_concern_z_1 <-
  margot_tab_lmtp(contrast_t2_env_climate_chg_concern_z_1,
                  scale = "RD",
                  new_name = "I am deeply concerned about climate change.")

tab_contrast_t2_env_climate_chg_concern_z_1


out_tab_contrast_t2_env_climate_chg_concern_z_1<-
  lmtp_evalue_tab(tab_contrast_t2_env_climate_chg_concern_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_env_climate_chg_concern_z_1



# contrasts: 4 t2_env_sat_nz_environment_z

#t2_env_sat_nz_environment_z <- here_read("t2_env_sat_nz_environment_z")
t2_env_sat_nz_environment_z_1 <- here_read("t2_env_sat_nz_environment_z_1")
t2_env_sat_nz_environment_z_null <-
  here_read("t2_env_sat_nz_environment_z_null")


# first contrast
# contrast_t2_env_sat_nz_environment_z <-
#   lmtp_contrast(t2_env_sat_nz_environment_z,
#                 ref = t2_env_sat_nz_environment_z_null,
#                 type = "additive")
# 
# 
# tab_contrast_t2_env_sat_nz_environment_z <-
#   margot_tab_lmtp(contrast_t2_env_sat_nz_environment_z,
#                   scale = "RD",
#                   new_name = "(Satisfied with the quality of New Zealand’s natural environment.")
# 
# tab_contrast_t2_env_sat_nz_environment_z
# 
# out_tab_contrast_t2_env_sat_nz_environment_z<-
#   lmtp_evalue_tab(tab_contrast_t2_env_sat_nz_environment_z,
#                   scale = c("RD"))
# 
# out_tab_contrast_t2_env_sat_nz_environment_z

# second contrast
contrast_t2_env_sat_nz_environment_z_1 <-
  lmtp_contrast(t2_env_sat_nz_environment_z_1,
                ref = t2_env_sat_nz_environment_z_null,
                type = "additive")


tab_contrast_t2_env_sat_nz_environment_z_1 <-
  margot_tab_lmtp(contrast_t2_env_sat_nz_environment_z_1,
                  scale = "RD",
                  new_name = "Satisfied with the quality of New Zealand’s natural environment.")

tab_contrast_t2_env_sat_nz_environment_z_1

out_tab_contrast_t2_env_sat_nz_environment_z_1<-
  lmtp_evalue_tab(tab_contrast_t2_env_sat_nz_environment_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_env_sat_nz_environment_z_1

# bind individual tables
# tab_envir <- rbind(
#   out_tab_contrast_t2_env_climate_chg_real_z,
#   out_tab_contrast_t2_env_climate_chg_cause_z,
#   out_tab_contrast_t2_env_climate_chg_concern_z,
#   out_tab_contrast_t2_env_sat_nz_environment_z
# )
# t2_env_climate_chg_real_z
# out_tab_contrast_t2_env_climate_chg_cause_z
# # make group table
# group_tab_envir<- group_tab(tab_envir  , type = "RD")
# 
# # save
# here_save(group_tab_envir, "group_tab_envir")
# 
# # read
# group_tab_envir <- here_read("group_tab_envir")


# second analysis
tab_envir_1 <- rbind(
  out_tab_contrast_t2_env_climate_chg_real_z_1,
  out_tab_contrast_t2_env_climate_chg_cause_z_1,
  out_tab_contrast_t2_env_climate_chg_concern_z_1,
  out_tab_contrast_t2_env_sat_nz_environment_z_1
)


# make group table
group_tab_envir_1<- group_tab(tab_envir_1  , type = "RD")

# save
here_save(group_tab_envir_1, "group_tab_envir_1")

# read
group_tab_envir_1 <- here_read("group_tab_envir_1")

# create plots -------------------------------------------------------------
# check N
# N
# sub_title = "Trust in science (place in society): shift those below average to average, otherwise do not shift: N = 32737"
# 
# 
# # graph health
# plot_group_tab_envir <- margot_plot(
#   group_tab_envir,
#   type = "RD",
#   title = "Environmental attitudes",
#   subtitle = sub_title,
#   xlab = "",
#   ylab = "",
#   estimate_scale = 1,
#   base_size = 12,
#   text_size = 3.0,
#   point_size = .5,
#   title_size = 15,
#   subtitle_size = 11,
#   legend_text_size = 8,
#   legend_title_size = 10,
#   x_offset = -1,
#   x_lim_lo = -1,
#   x_lim_hi =  .5
# )
# plot_group_tab_envir
# dev.off()
# # save graph
# ggsave(
#   plot_group_tab_envir,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "plot_group_tab_envir.png",
#   device = 'png',
#   limitsize = FALSE,
#   dpi = 600
# )

# second analysis Graph
# check N
N
sub_title_1 = "Trust in science (place in society): shift everyone by 1x point to max of 7, N = 32737"


# graph health
plot_group_tab_envir_1 <- margot_plot(
  group_tab_envir_1,
  type = "RD",
  title = "Environmental attitudes",
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
plot_group_tab_envir_1
dev.off()
# save graph
ggsave(
  plot_group_tab_envir_1,
  path = here::here(here::here(push_mods, "figs")),
  width =12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_envir_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

