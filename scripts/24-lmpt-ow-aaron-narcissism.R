#lmpt-ow-aaron-psychopath.R
# 2024 01 16 
# joseph bulbulia : joseph.bulbulia@gmail.com
# outcome-wide-analysis-psychopathy

# aaron's study # NARCICISM 
# preliminaries -----------------------------------------------------------


source("/Users/joseph/GIT/templates/functions/libs2.R")

# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/funs.R")

# experimental functions
source(
  "/Users/joseph/GIT/templates/functions/experimental_funs.R"
)
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

# read data: note that you need use the arrow package in R
dat <- arrow::read_parquet(pull_path)

### WARNING: THIS PATH WILL NOT WORK FOR YOU. PLEASE SET A PATH TO YOUR OWN COMPUTER!! ###
### WARNING: FOR EACH NEW STUDY SET UP A DIFFERENT PATH OTHERWISE YOU WILL WRITE OVER YOUR MODELS
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/nzvs_mods/24/aaron-narcissism"
  )

# check path:is this correct?  check so you know you are not overwriting other directors
push_mods

# set exposure here
#nzavs_exposure <- "XXXX"


# define exposures --------------------------------------------------------
# define exposure
#A <- "t1_XXXX"


A <- "t1_aaron_narcissism"

# define exposure
nzavs_exposure <- "aaron_narcissism"
exposure_var = c( "aaron_narcissism", "not_lost") #

# set exposure variable, can be both the continuous and the coarsened, if needed
#exposure_var = c("XXXX", "not_lost") #

# see second function below

# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 10

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
seed <- 0112358
set.seed(seed)

# set cores for estimation
library(future)
plan(multisession)
n_cores <- parallel::detectCores()

# super learner libraries
sl_lib <- c("SL.randomForest",
            "SL.ranger")
SL_folds

# superlearner libraries
library(SuperLearner)
library(ranger)
library(randomForest)

# check options
listWrappers()

# Parent analysis is in: 
# https://github.com/go-bayes/models/blob/main/scripts/24-lmpt-ow-aaron-psychopath.R
# here we perform all the pre-processing

## read data
dat_final_dyadic <- readRDS("/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/lmtp/24/aaron_psychopathy/dat_final_dyadic")



dat_long <- dat_final_dyadic |> 
  # ungroup
  select(
    "wave",
    "year_measured",
    "id",
    "aaron_psychopathy_combined", #aaron
    "aaron_antagonism",#aaron
    "aaron_emotional_stability",#aaron
    "aaron_disinhibition", #aaron
    "aaron_narcissism",#aaron
    "aaron_psychopathy_combined",#aaron
    "sat_relationship", # Relationship satisfication
    "sample_origin_names_combined",
    # Sample origin names combined
    #"alert_level_combined_lead",  not needed because all receive all levels by the point the outcome is measured
    # covid alert levels -> 2019-2020
    "education_level_coarsen",
    # Ordinal-Rank 0-10 NZREG codes (with overseas school quals coded as Level 3, and all other ancillary categories coded as missing)  Combined highschool levels See:https://www.nzqa.govt.nz/assets/Studying-in-NZ/New-Zealand-Qualification-Framework/requirements-nzqf.pdf
    "male",
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
    "religion_church",
    "w_gend_age_euro",
    # sample_weights.
    "modesty",
    "hours_exercise",
    "kessler6_sum",
    # depression constructs,
    "kessler_latent_depression",
    "kessler_latent_anxiety",
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
    "self_esteem",
    "selfesteem_satself",
    #  On the whole am satisfied with myself.
    "selfesteem_postiveself",
    # Take a positive attitude toward myself
    "selfesteem_failure_reversed",
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
    "sat_relationship",
    "conflict_in_relationship",
    "kessler_latent_anxiety",
    "kessler_latent_depression",
    "alert_level_combined_lead",
    "rel_num_l",
  ) |>
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
  dplyr::mutate(
    meets_criteria_baseline = ifelse(year_measured == 1 &
                                       !is.na(!!sym(nzavs_exposure)), 1, 0)) |>  # using R lang
  dplyr::mutate(sample_origin = as.factor( sample_origin_names_combined)) |>  #shorter name
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
    # friends_money = ifelse(friends_money < 0, 0, friends_money),
    # someone gave neg number
    # household_inc_log = log(household_inc + 1),
    #  hours_children_log = log(hours_children + 1),
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1)#,
    # hours_exercise_log = log(hours_exercise + 1)
  ) |>
  dplyr::rename(sample_weights = w_gend_age_euro) |>
  dplyr::mutate(sample_origin = sample_origin_names_combined) |>  #shorter name
  arrange(id, wave) |>
  droplevels() |>
  select(-h_18, -k_18, -h_19, -k_19) |>
  droplevels() |>
  ungroup() %>%
  mutate(wave = as.numeric(wave)) |>
  arrange(id, wave) |>
  mutate(
    religion_church_coarsen = cut(
      religion_church,
      breaks = c(-Inf, 0, 1, 3.99, Inf),
      labels = c("zero", "one", "less_four", "four_up"),
      include.lowest = TRUE,
      right = TRUE,
      ordered = TRUE
    )) %>%
  mutate(
    religion_church_coarsen_n = as.numeric(religion_church_coarsen) - 1,
  ) |>
  mutate(
    # eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban),
    # education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  select(-religion_church_coarsen, -sample_origin) |> 
  droplevels() |>
  arrange(id, wave) |>
  data.frame()


# check dyads
singleton_dyads <- dat_long %>% 
  group_by(rel_num_l) %>% 
  summarise(n_id = n_distinct(id)) %>% 
  filter(n_id != 2)

final_dat_clean <- dat_long %>% 
  anti_join(singleton_dyads, by = "rel_num_l")

n_unique(final_dat_clean$id)
n_unique(final_dat_clean$rel_num_l)


# check sample 
N_participants <-n_unique(final_dat_clean$id) #514 couples
N_participants



# save for paper
here_save(N_participants, "N_participants")


# 1 x person had completely missing information on narciccism
dat_long<-  final_dat_clean


# eyeball distribution
# table(dat_long$wave)
dt_19 <- dat_long |>
  filter(year_measured == 1 & wave == 2)

here_save_arrow(dt_19, "dt_19")

hist(dat_long$aaron_narcissism)
min_score <- min(dt_19$aaron_narcissism, na.rm = TRUE)
min_score

max_score <- max(dt_19$aaron_narcissism, na.rm = TRUE)
max_score

# sd_exposure <- sd(dt_19$aaron_emotional_stability,
#                   na.rm = TRUE)
# sd_exposure
# 
# one_point_in_sd_units <- 1/sd_exposure
# one_point_in_sd_units

# half_sd <- sd_exposure / 2
# half_sd


# Decrease by one point (raw scores)
f <- function(data, trt) {
  ifelse(data[[trt]] >= min_score + 1, data[[trt]] - 1,  min_score)
}



#  Increase everyone by one point, contrasted with what they would be anyway.
# only use this function for raw scores

f_1 <- function(data, trt) {
  ifelse(data[[trt]] <= max_score - 1, data[[trt]] + 1,  max_score)
}

# check function logic
max_score - 1
min_score + 1

#check missing
#naniar::vis_miss(dat_long, warn_large_data = FALSE)
dev.off()



# check sample 
N <-n_unique(dat_long$id) #1070 
N

# double check path
push_mods

here_save(N, "N")

# check col names
colnames(dat)

dev.off()
# check
dt_check_exposure <- dat_long |> filter(wave == 1| wave == 2)

# makes sure all is false
table (is.na(dt_check_exposure$aaron_narcissism))


# make
dt_18 <- dat_long |>
  filter(wave == 1 )



dt_positivity_full <- dt_check_exposure |>
  filter(wave == 1 | wave == 2) |>
  select(wave, id, aaron_narcissism) |> 
  mutate(aaron_narcissism_round = round(aaron_narcissism, 0))


# test positivity
out <-
  msm::statetable.msm(aaron_narcissism_round, id, data = dt_positivity_full)

# transition table
t_tab <- transition_table(out, state_names = NULL)
t_tab

here_save(t_tab, "t_tab")


# 
standard_deviation_exposure <-
  coloured_histogram_sd(dt_19, col_name = "aaron_narcissism", binwidth = .1)

standard_deviation_exposure


# 
#here_save( standard_deviation_exposure, "standard_deviation_exposure")
# 
# ggsave(
#   standard_deviation_exposure,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "standard_deviation_exposure.jpeg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )


# 
# # generate bar plot
# graph_density_of_exposure <- coloured_histogram(dt_19, col_name = "aaron_emotional_stability", scale_min = 1, scale_max = 7)
# 
# graph_density_of_exposure
# 
# 
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




# set variables for baseline exposure and outcome -------------------------



baseline_vars = c(
  "male",
  "age",
  "education_level_coarsen",
  # factors
  "eth_cat",
  "parent",
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  #"bigger_doms", #religious denomination
  # "sample_origin",
  "nz_dep2018",
  # "nzsei13",
  # "born_nz",
  "kessler6_sum",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  # social belonging
  # "household_inc_log",
  # added: measured with error but OK for imputations
  # "partner",
  # "parent",  # newly changed - have information in child number
  "political_conservative",
  "religion_church_coarsen_n",
  #Please rate how politically liberal versus conservative you see yourself as being.
  # Sample origin names combined
  "urban",
  "parent",
  "hours_housework_log",
  "hours_work_log",
  "sample_weights",
  "alert_level_combined_lead",
  "rel_num_l"
) 

# check
baseline_vars

# check
exposure_var
dat_long$kessler6_sum
# outcomes
outcome_vars = c(
  "sat_relationship",
  "conflict_in_relationship",
  "kessler6_sum",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  #  "pwb_your_health",
  # #Your health.
  #  "pwb_your_relationships",
  # #Your personal relationships.
  #  "pwb_your_future_security",
  # #Your future security.
  #  "pwb_standard_living",
  "lifesat",
  "pwi",
  "self_esteem"
)
# impute baseline data (we use censoring for the outcomes)
#colnames(dat_long)
# function imputes only baseline not outcome

# check
dat_long |> 
  filter(wave ==2) |> 
  count(is.na(aaron_narcissism))           

tst <- dat_long |> 
  filter(wave ==1)
  levels(tst$alert_level_combined_lead)   

  table(tst$have_siblings)
str(dat_long)
# make data wide and impute baseline missing values -----------------------

# get rid of haven labels
dat_long <- as.data.frame(dat_long)
dat_long <- haven::zap_formats(dat_long)
dat_long <- haven::zap_label(dat_long)
dat_long <- haven::zap_widths(dat_long)

# get rid of id level that should have been dropped
dat_long <- dat_long |> droplevels()

n_unique(dat_long$id)
# vis missing
naniar::vis_miss(dat_long)

str(dat_long)

prep_coop_all <- margot_wide_impute_baseline(
  dat_long,
  baseline_vars = baseline_vars,
  exposure_var = exposure_var,
  outcome_vars = outcome_vars
)



# # check mi model
# outlist <-
#   row.names(prep_coop_all)[prep_coop_all$outflux < 0.5]
# length(outlist)
# 
# # checks. We do not impute with weights: area of current research
# head(prep_coop_all$loggedEvents, 10)

push_mods

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
colnames(prep_coop_all)

prep_coop_all <- as.data.frame(prep_coop_all)

## dyads response

library(magrittr)
prep_coop_all_1 <- prep_coop_all %>%
  group_by(id, t0_rel_num_l) %>%
  mutate_all(rev) %>%
  ungroup() %>%
  select(-id, -t0_rel_num_l) %>%
  set_colnames(paste0('partner_', colnames(.)))

prep_coop_all_1
prep_coop_all_use  <- cbind(prep_coop_all, prep_coop_all_1)

colnames(prep_coop_all_use)


#
n_unique(prep_coop_all_use$t0_rel_num_l)

# spit and shine:
# load required libraries
library(dplyr)
library(stringr)

# extract column names
col_names <- colnames(prep_coop_all_use)

# identify columns that start with 'partner_'
partner_cols <- str_detect(col_names, "^partner_")

# replace and rename columns
new_col_names <- col_names

# rename operation
new_col_names[partner_cols] <-
  gsub("partner_(t\\d+)_(.*)", "\\1_partner_\\2", col_names[partner_cols])

# apply new column names to dataframe
colnames(prep_coop_all_use) <- new_col_names

# check
colnames(prep_coop_all_use)

# extract column names
col_names <- colnames(prep_coop_all_use)

# extract time prefix and sort based on it
sorted_indices <- order(gsub(".*(t\\d+).*", "\\1", col_names))

# get sorted column names
sorted_col_names <- col_names[sorted_indices]

# use relocate to rearrange the columns
prep_coop_all_use_1 <-
  prep_coop_all_use %>% relocate(all_of(sorted_col_names))

# remove
colnames(prep_coop_all_use_1)


# save function -- will save to your "push_mod" directory
here_save(prep_coop_all_use_1, "prep_coop_all_use_1_backup")

# read function
prep_coop_all_use_1 <- here_read("prep_coop_all_use_1_backup")

colnames(prep_coop_all_use_1)
naniar::vis_miss(prep_coop_all_use_1, warn_large_data = FALSE)
dev.off()



#check must be a dataframe
str(prep_coop_all_use_1)
nrow(prep_coop_all_use_1)
colnames(prep_coop_all_use_1)

prep_coop_all_use_1 <- as.data.frame(prep_coop_all_use_1)

# arrange data for analysis -----------------------------------------------
# spit and shine
df_wide_censored <-
  prep_coop_all_use_1 |>
  mutate(
    t0_education_level_coarsen = as.factor(t0_education_level_coarsen),
    t0_eth_cat = as.factor(t0_eth_cat)
  ) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  relocate("t1_partner_not_lost", .before = starts_with("t2_"))

#check
head(df_wide_censored)
dim(df_wide_censored)
str(df_wide_censored)

# save
here_save(df_wide_censored, "df_wide_censored")
df_wide_censored <- here_read("df_wide_censored")


# define exposure
nzavs_exposure

# arrange data for analysis -----------------------------------------------
# spit and shine
df_clean <- df_wide_censored %>%
  mutate(t2_na_flag = rowSums(is.na(select(
    ., starts_with("t2_")
  ))) > 0) %>%
  mutate(t1_not_lost = ifelse(t2_na_flag, 0, t1_not_lost)) %>%
  mutate(t1_partner_not_lost = ifelse(t2_na_flag, 0, t1_partner_not_lost)) %>%
  # select(-t2_na_flag) %>%
  filter(!rowSums(is.na(select(
    ., starts_with("t0_")
  )))) |>
  dplyr::mutate(
    across(
      where(is.numeric) &
        !t0_not_lost &
        !t1_not_lost &
        !t0_partner_not_lost &
        !t1_partner_not_lost &
        !t0_sample_weights &
        !t0_rel_num_l &
        !t1_aaron_narcissism,
      ~ scale(.x),
      .names = "{col}_z"
    )
  ) |>
  select(
    where(is.factor),
    t0_not_lost,
    t0_partner_not_lost,
    t0_sample_weights,
    t0_rel_num_l,
    t1_aaron_narcissism,
    t1_not_lost,
    t1_partner_not_lost,
    ends_with("_z")
  ) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
  relocate(starts_with("t2_"), .after = starts_with("t1_"))  %>%
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t0_partner_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  relocate("t1_partner_not_lost", .before = starts_with("t2_")) |>
  mutate(t0_sample_weights = as.numeric(t0_sample_weights)) |>
  data.frame()

dim(df_clean)
naniar::vis_miss(df_clean, warn_large_data = FALSE)
dev.off()

colnames(df_clean)
# again check path
push_mods
# save
here_save(df_clean, "df_clean")

# read if needed
df_clean <- here_read("df_clean")

str(df_clean)
df_clean <- as.data.frame(df_clean)

#check n
nrow(df_clean)

colnames(df_clean)
# get names
names_base <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     -t0_not_lost,
                     - t0_partner_sample_weights_z,
                     -t0_partner_alert_level_combined_lead,
                     -id) |> colnames()

names_base

names_outcomes <-
  df_clean |> select(starts_with("t2")) |> colnames()

names_outcomes

colnames(df_clean)

# set variables for models ------------------------------------------------

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

f_1
# make test data (if needed)
# df_clean_test <- df_clean |>
#   slice_head(n = 2000)

# "SL.earth" refers to a wrapper for the 'earth' function from the 'earth' R package in the SuperLearner library. This function implements Multivariate Adaptive Regression Splines (MARS), a non-parametric regression method that extends linear models by allowing for interactions and non-linear relationships between variables.
# MARS models can handle high-dimensional data well and can be a useful tool for capturing complex patterns in the data. They work by fitting piecewise linear models to the data, which allows for flexible and potentially non-linear relationships between predictors and the outcome.


# models -----------------------------------------------------------


# smoker binary
#Do you currently smoke?

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
names_outcomes

n_cores
sl_lib

t2_partner_sat_relationship_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_sat_relationship_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
  # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_partner_sat_relationship_z
here_save(t2_partner_sat_relationship_z, "t2_partner_sat_relationship_z")

t2_partner_sat_relationship_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_sat_relationship_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
here_save(t2_partner_sat_relationship_z_1, "t2_partner_sat_relationship_z_1")
t2_partner_sat_relationship_z_1

t2_partner_sat_relationship_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_sat_relationship_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
  # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
here_save(t2_partner_sat_relationship_z_null, "t2_partner_sat_relationship_z_null")

t2_partner_sat_relationship_z
t2_partner_sat_relationship_z_null
t2_partner_sat_relationship_z_1



### CONFLICT
t2_partner_conflict_in_relationship_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_conflict_in_relationship_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
  #  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_partner_conflict_in_relationship_z
here_save(t2_partner_conflict_in_relationship_z, "t2_partner_conflict_in_relationship_z")

t2_partner_conflict_in_relationship_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_conflict_in_relationship_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
  #  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
here_save(t2_partner_conflict_in_relationship_z_1, "t2_partner_conflict_in_relationship_z_1")


t2_partner_conflict_in_relationship_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_conflict_in_relationship_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
  #  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
here_save(t2_partner_conflict_in_relationship_z_null, "t2_partner_conflict_in_relationship_z_null")

t2_partner_conflict_in_relationship_z
t2_partner_conflict_in_relationship_z_null
t2_partner_conflict_in_relationship_z_1


##
# names_base_t2_kessler_latent_anxiety_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_kessler_latent_anxiety_z")

# During the last 30 days, how often did.... you feel that everything was an effort?
# During the last 30 days, how often did.... you feel nervous?
# During the last 30 days, how often did.... you feel restless or fidgety?

### KESSLER
t2_partner_kessler6_sum_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler6_sum_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
  #  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_partner_kessler6_sum_z
here_save(t2_partner_kessler6_sum_z, "t2_partner_kessler6_sum_z")

t2_partner_kessler6_sum_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler6_sum_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
  # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
here_save(t2_partner_kessler6_sum_z_1, "t2_partner_kessler6_sum_z_1")
t2_partner_kessler6_sum_z_1

t2_partner_kessler6_sum_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler6_sum_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
  #  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
here_save(t2_partner_kessler6_sum_z_null, "t2_partner_kessler6_sum_z_null")

t2_partner_kessler6_sum_z
t2_partner_kessler6_sum_z_null
t2_partner_kessler6_sum_z_1



## anxiety
t2_partner_kessler_latent_anxiety_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_anxiety_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  #  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_kessler_latent_anxiety_z
here_save(t2_partner_kessler_latent_anxiety_z, "t2_partner_kessler_latent_anxiety_z")




t2_partner_kessler_latent_anxiety_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_anxiety_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


here_save(t2_partner_kessler_latent_anxiety_z_1, "t2_partner_kessler_latent_anxiety_z_1")



t2_partner_kessler_latent_anxiety_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_anxiety_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

# test
here_save(t2_partner_kessler_latent_anxiety_z_null,
          "t2_partner_kessler_latent_anxiety_z_null")


t2_partner_kessler_latent_depression_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_depression_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


here_save(t2_partner_kessler_latent_depression_z,
          "t2_partner_kessler_latent_depression_z")


t2_partner_kessler_latent_depression_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_depression_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_partner_kessler_latent_depression_z_1
here_save(t2_partner_kessler_latent_depression_z_1,
          "t2_partner_kessler_latent_depression_z_1")



t2_partner_kessler_latent_depression_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_depression_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_kessler_latent_depression_z_null
here_save(t2_partner_kessler_latent_depression_z_null,
          "t2_partner_kessler_latent_depression_z_null")


# SELF ESTEEM 
t2_partner_self_esteem_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_self_esteem_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_self_esteem_z
here_save(t2_partner_self_esteem_z, "t2_partner_self_esteem_z")



t2_partner_self_esteem_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_self_esteem_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_self_esteem_z_1
here_save(t2_partner_self_esteem_z_1, "t2_partner_self_esteem_z_1")





t2_partner_self_esteem_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_self_esteem_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_self_esteem_z_null
here_save(t2_partner_self_esteem_z_null, "t2_partner_self_esteem_z_null")

t2_partner_self_esteem_z
t2_partner_self_esteem_z_1
t2_partner_self_esteem_z_null

# PWI
t2_partner_pwi_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_pwi_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_pwi_z
here_save(t2_partner_pwi_z, "t2_partner_pwi_z")



# Your health.
t2_partner_pwi_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_pwi_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_pwi_z_1
here_save(t2_partner_pwi_z_1, "t2_partner_pwi_z_1")

t2_partner_pwi_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_pwi_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_partner_pwi_z_null
here_save(t2_partner_pwi_z_null, "t2_partner_pwi_z_null")

## life sat
# I am satisfied with my life.
# In most ways my life is close to ideal.
t2_partner_lifesat_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_lifesat_z",
  cens = C,
  shift = f,
  mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_lifesat_z
here_save(t2_partner_lifesat_z, "t2_partner_lifesat_z")


t2_partner_lifesat_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_lifesat_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_lifesat_z_1
here_save(t2_partner_lifesat_z_1, "t2_partner_lifesat_z_1")

# I am satisfied with my life.
# In most ways my life is close to ideal.
t2_partner_lifesat_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_lifesat_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
  folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_lifesat_z_null
here_save(t2_partner_lifesat_z_null, "t2_partner_lifesat_z_null")

# contrasts ---------------------------------------------------------------
push_mods
# sat relationships
t2_partner_sat_relationship_z <- here_read("t2_partner_sat_relationship_z")
t2_partner_sat_relationship_z_1 <- here_read("t2_partner_sat_relationship_z_1")
t2_partner_sat_relationship_z_null <-
  here_read("t2_partner_sat_relationship_z_null")

# first contrast 
contrast_t2_partner_sat_relationship_z <-
  lmtp_contrast(t2_partner_sat_relationship_z,
                ref = t2_partner_sat_relationship_z_null,
                type = "additive")

tab_contrast_t2_partner_sat_relationship_z <-
  margot_tab_lmtp(contrast_t2_partner_sat_relationship_z,
                  scale = "RD",
                  new_name = "Gain psychopathy narcissism: partner relationship satisfaction")


out_tab_contrast_t2_partner_sat_relationship_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_sat_relationship_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_sat_relationship_z

# second contrast
contrast_t2_partner_sat_relationship_z_1 <-
  lmtp_contrast(t2_partner_sat_relationship_z_1,
                ref = t2_partner_sat_relationship_z_null,
                type = "additive")


tab_contrast_t2_partner_sat_relationship_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_sat_relationship_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy narcissism: partner relationship satisfacton")


out_tab_contrast_t2_partner_sat_relationship_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_sat_relationship_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_sat_relationship_z_1


# conflict
t2_partner_conflict_in_relationship_z<- here_read("t2_partner_conflict_in_relationship_z")
t2_partner_conflict_in_relationship_z_null<- here_read("t2_partner_conflict_in_relationship_z_null")
t2_partner_conflict_in_relationship_z_1<- here_read("t2_partner_conflict_in_relationship_z_1")

t2_partner_conflict_in_relationship_z
t2_partner_conflict_in_relationship_z_null
t2_partner_conflict_in_relationship_z_1



# first contrast 
contrast_t2_partner_conflict_in_relationship_z <-
  lmtp_contrast(t2_partner_conflict_in_relationship_z,
                ref = t2_partner_conflict_in_relationship_z_null,
                type = "additive")

tab_contrast_t2_partner_conflict_in_relationship_z <-
  margot_tab_lmtp(contrast_t2_partner_conflict_in_relationship_z,
                  scale = "RD",
                  new_name = "Gain psychopathy narcissism: partner conflict in relationship")


out_tab_contrast_t2_partner_conflict_in_relationship_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_conflict_in_relationship_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_conflict_in_relationship_z

# second contrast
contrast_t2_partner_conflict_in_relationship_z_1 <-
  lmtp_contrast(t2_partner_conflict_in_relationship_z_1,
                ref = t2_partner_conflict_in_relationship_z_null,
                type = "additive")

tab_contrast_t2_partner_conflict_in_relationship_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_conflict_in_relationship_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy narcissism: partner conflict in relationship")


out_tab_contrast_t2_partner_conflict_in_relationship_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_conflict_in_relationship_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_conflict_in_relationship_z_1


# kessler 6
t2_partner_kessler6_sum_z <- here_read("t2_partner_kessler6_sum_z")
t2_partner_kessler6_sum_z_null <- here_read("t2_partner_kessler6_sum_z_null")
t2_partner_kessler6_sum_z_1 <- here_read("t2_partner_kessler6_sum_z_1")


contrast_t2_partner_kessler6_sum_z <-
  lmtp_contrast(t2_partner_kessler6_sum_z,
                ref = t2_partner_kessler6_sum_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler6_sum_z<-
  margot_tab_lmtp(contrast_t2_partner_kessler6_sum_z,
                  scale = "RD",
                  new_name = "Gain psychopathy narcissism: partner Kessler 6 distress")


out_tab_contrast_t2_partner_kessler6_sum_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler6_sum_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler6_sum_z


# loss

contrast_t2_partner_kessler6_sum_z_1 <-
  lmtp_contrast(t2_partner_kessler6_sum_z_1,
                ref = t2_partner_kessler6_sum_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler6_sum_z_1<-
  margot_tab_lmtp(contrast_t2_partner_kessler6_sum_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy narcissism: partner Kessler 6 distress")


out_tab_contrast_t2_partner_kessler6_sum_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler6_sum_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler6_sum_z_1


# depression
t2_partner_kessler_latent_depression_z <- here_read("t2_partner_kessler_latent_depression_z")
t2_partner_kessler_latent_depression_z_null <- here_read("t2_partner_kessler_latent_depression_z_null")
t2_partner_kessler_latent_depression_z_1 <- here_read("t2_partner_kessler_latent_depression_z_1")



# first contrast
contrast_t2_partner_kessler_latent_depression_z <-
  lmtp_contrast(t2_partner_kessler_latent_depression_z,
                ref = t2_partner_kessler_latent_depression_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler_latent_depression_z <-
  margot_tab_lmtp(contrast_t2_partner_kessler_latent_depression_z,
                  scale = "RD",
                  new_name = "Gain psychopathy narcissism: partner Kessler 6 depression")


out_tab_contrast_t2_partner_kessler_latent_depression_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler_latent_depression_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler_latent_depression_z


# second contrast
contrast_t2_partner_kessler_latent_depression_z_1 <-
  lmtp_contrast(t2_partner_kessler_latent_depression_z_1,
                ref = t2_partner_kessler_latent_depression_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler_latent_depression_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_kessler_latent_depression_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy narcissism: partner Kessler 6 depression")


out_tab_contrast_t2_partner_kessler_latent_depression_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler_latent_depression_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler_latent_depression_z_1


#anxiety
t2_partner_kessler_latent_anxiety_z<- here_read("t2_partner_kessler_latent_anxiety_z")
t2_partner_kessler_latent_anxiety_z_1 <- here_read("t2_partner_kessler_latent_anxiety_z_1")
t2_partner_kessler_latent_anxiety_z_null <- here_read("t2_partner_kessler_latent_anxiety_z_null")


# first contrast
contrast_t2_partner_kessler_latent_anxiety_z <-
  lmtp_contrast(t2_partner_kessler_latent_anxiety_z,
                ref = t2_partner_kessler_latent_anxiety_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler_latent_anxiety_z <-
  margot_tab_lmtp(contrast_t2_partner_kessler_latent_anxiety_z,
                  scale = "RD",
                  new_name = "Gain psychopathy narcissism: partner Kessler 6 anxiety")


out_tab_contrast_t2_partner_kessler_latent_anxiety_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler_latent_anxiety_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler_latent_anxiety_z

# second contrast
contrast_t2_partner_kessler_latent_anxiety_z_1 <-
  lmtp_contrast(t2_partner_kessler_latent_anxiety_z_1,
                ref = t2_partner_kessler_latent_anxiety_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler_latent_anxiety_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_kessler_latent_anxiety_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy narcissism: partner Kessler 6 anxiety")


out_tab_contrast_t2_partner_kessler_latent_anxiety_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler_latent_anxiety_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler_latent_anxiety_z_1


# self esteem
t2_partner_self_esteem_z<- here_read("t2_partner_self_esteem_z")
t2_partner_self_esteem_z_1<- here_read("t2_partner_self_esteem_z_1")
t2_partner_self_esteem_z_null<- here_read("t2_partner_self_esteem_z_null")


# first contrast
contrast_t2_partner_self_esteem_z <-
  lmtp_contrast(t2_partner_self_esteem_z,
                ref = t2_partner_self_esteem_z_null,
                type = "additive")


tab_contrast_t2_partner_self_esteem_z <-
  margot_tab_lmtp(contrast_t2_partner_self_esteem_z,
                  scale = "RD",
                  new_name = "Gain psychopathy narcissism: partner self-esteem")


out_tab_contrast_t2_partner_self_esteem_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_self_esteem_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_self_esteem_z



# second contrast
contrast_t2_partner_self_esteem_z_1 <-
  lmtp_contrast(t2_partner_self_esteem_z_1,
                ref = t2_partner_self_esteem_z_null,
                type = "additive")

tab_contrast_t2_partner_self_esteem_z_1<-
  margot_tab_lmtp(contrast_t2_partner_self_esteem_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy narcissism: partner self-esteem")

out_tab_contrast_t2_partner_self_esteem_z_1<-
  lmtp_evalue_tab(tab_contrast_t2_partner_self_esteem_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_self_esteem_z_1

# pwi
t2_partner_pwi_z<- here_read("t2_partner_pwi_z")
t2_partner_pwi_z_1<- here_read("t2_partner_pwi_z_1")
t2_partner_pwi_z_null<- here_read("t2_partner_pwi_z_null")


# first contrast
contrast_t2_partner_pwi_z <-
  lmtp_contrast(t2_partner_pwi_z,
                ref = t2_partner_pwi_z_null,
                type = "additive")

tab_contrast_t2_partner_pwi_z <-
  margot_tab_lmtp(contrast_t2_partner_pwi_z,
                  scale = "RD",
                  new_name = "Gain psychopathy narcissism: partner pers. wellbeing")


out_tab_contrast_t2_partner_pwi_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_pwi_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_pwi_z

# second contrast
contrast_t2_partner_pwi_z_1 <-
  lmtp_contrast(t2_partner_pwi_z_1,
                ref = t2_partner_pwi_z_null,
                type = "additive")

tab_contrast_t2_partner_pwi_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_pwi_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy narcissism: partner pers. wellbeing")


out_tab_contrast_t2_partner_pwi_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_pwi_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_pwi_z_1

# lifesat
t2_partner_lifesat_z <- here_read("t2_partner_lifesat_z")
t2_partner_lifesat_z_1<- here_read("t2_partner_lifesat_z_1")
t2_partner_lifesat_z_null<- here_read("t2_partner_lifesat_z_null")




# first contrast
contrast_t2_partner_lifesat_z <- lmtp_contrast(t2_partner_lifesat_z,
                                               ref = t2_partner_lifesat_z_null,
                                               type = "additive")

tab_contrast_t2_partner_lifesat_z <-
  margot_tab_lmtp(contrast_t2_partner_lifesat_z, scale = "RD", 
                  new_name = "Gain psychopathy narcissism: partner life satisfaction")


out_tab_contrast_t2_partner_lifesat_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_lifesat_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_lifesat_z

# second contrast
contrast_t2_partner_lifesat_z_1 <- lmtp_contrast(t2_partner_lifesat_z_1,
                                                 ref = t2_partner_lifesat_z_null,
                                                 type = "additive")

tab_contrast_t2_partner_lifesat_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_lifesat_z_1, scale = "RD", 
                  new_name = "Loss psychopathy narcissism: partner life satisfaction")


out_tab_contrast_t2_partner_lifesat_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_lifesat_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_lifesat_z_1

names_base

# make tables -------------------------------------------------------------

# bind individual tables
tab_outcomes_gain <- rbind(
  out_tab_contrast_t2_partner_conflict_in_relationship_z,  
  out_tab_contrast_t2_partner_sat_relationship_z,
  out_tab_contrast_t2_partner_kessler6_sum_z,
  out_tab_contrast_t2_partner_kessler_latent_depression_z,
  out_tab_contrast_t2_partner_kessler_latent_anxiety_z,
  out_tab_contrast_t2_partner_self_esteem_z,
  out_tab_contrast_t2_partner_pwi_z,
  out_tab_contrast_t2_partner_lifesat_z
)

here_save(tab_outcomes_gain,"tab_outcomes_gain")
tab_outcomes_gain

# make group table
group_tab_outcomes_gain <- group_tab(tab_outcomes_gain, type = "RD")

group_tab_outcomes_gain
here_save(group_tab_outcomes_gain,"group_tab_outcomes_gain")




# bind individual tables
tab_outcomes_loss <- rbind(
  out_tab_contrast_t2_partner_conflict_in_relationship_z_1,  
  out_tab_contrast_t2_partner_sat_relationship_z_1,
  out_tab_contrast_t2_partner_kessler6_sum_z_1,
  out_tab_contrast_t2_partner_kessler_latent_depression_z_1,
  out_tab_contrast_t2_partner_kessler_latent_anxiety_z_1,
  out_tab_contrast_t2_partner_self_esteem_z_1,
  out_tab_contrast_t2_partner_pwi_z_1,
  out_tab_contrast_t2_partner_lifesat_z_1
)
tab_outcomes_loss
here_save(tab_outcomes_loss,"tab_outcomes_loss")

# make group table
group_tab_outcomes_loss <- group_tab(tab_outcomes_loss , type = "RD")

# save
here_save(group_tab_outcomes_loss, "group_tab_outcomes_loss")


# RECALL
group_tab_outcomes_loss <- here_read("group_tab_outcomes_loss")
group_tab_outcomes_gain <- here_read("group_tab_outcomes_gain")



# create plots -------------------------------------------------------------

# check N
N = 1068
sub_title = ""

conflicts_prefer(ggplot2::margin)
# graph health
plot_group_tab_gain <- margot_plot(
  group_tab_outcomes_gain,
  type = "RD",
  title = "Gain +1 Psychopathy Narcissism: N=1070",
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
plot_group_tab_gain
dev.off()
# save graph
ggsave(
  plot_group_tab_gain,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_gain.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



# graph body
plot_group_tab_loss <- margot_plot(
  group_tab_outcomes_loss,
  type = "RD",
  title = "Loss +1 Psychopathy Narcissism: N=1070",
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
plot_group_tab_loss

ggsave(
  plot_group_tab_loss,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_loss.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)


plot_group_tab_loss

