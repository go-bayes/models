## Daryl VT's study investigating the effects of religious loss on political orientation

## Estimand
# intervention: loss of religion as defined as disaffiliation
# outcome: one year-effect on political orientation

## Shift function

A <- c("A_0", "A_1")
C <- c("C_0", "C_1") 
L <- c("L_0")
# This shift function returns trt - 1, if trt - 1 >= 1
# otherwise it returns trt

f_make_base <- function(x) {
  ifelse(x == 0,x == 1, x)
} # simulate everyone as religious at baseline

f_make_base_plus_1 <- function(x) {
  ifelse(x == 1,x == 0, x)
}  # simulate everyone as none at baseline + 1



# inclusion criteria:
# baseline exposure: political orientation measured.

# import data

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

### WARNING: FOR EACH NEW STUDY SET UP A DIFFERENT PATH OTHERWISE YOU WILL WRITE OVER YOUR MODELS
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/nzvs_mods/24/daryl-dones"
  )

# check path:is this correct?  check so you know you are not overwriting other directors
push_mods


levels(dat$religion_bigger_denominations)
# set up lmtp -------------------------------------------------------------


# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 10

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(0112358)

# set cores for estimation 
library(future)
plan(multisession)
n_cores <- parallel::detectCores()

# super learner libraries
sl_lib <- c("SL.randomForest",
            "Sl.xgboost",
            "SL.ranger")



# read data ---------------------------------------------------------------
dat <- arrow::read_parquet(pull_path)

# set exposure here
nzavs_exposure <- "religion_religious"

# get ids
ids_2018 <- dat %>%
  filter(year_measured == 1, wave == 2018 & !is.na(!!sym(nzavs_exposure))) |> # criteria, no missing 
  pull(id)

# filter the original dataset for these IDs three waves
dat_long <- dat %>%
  dplyr::filter(id %in% ids_2018 & wave %in% c(2018, 2019, 2020)) %>% 
  arrange(id, wave) %>% 
  dplyr::select(
    "id",
    "wave",
    "year_measured",
    "sample_origin_names_combined",
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
    "nzsei_13_l",
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
    #"total_siblings",
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
    "religion_religious",   # Do you identify with a religion and/or spiritual group?
    "w_gend_age_euro",
    "alcohol_frequency",
    #"How often do you have a drink containing alcohol?"
    "alcohol_intensity",
    # How many drinks containing alcohol do you have on a typical day when drinking?
    "hlth_bmi",
    #The current income gap between New Zealand Europeans and other ethnic groups would be very hard to change.
    "neighbourhood_community",
    #I feel a sense of community with others in my local neighbourhood.
    "support",
    "belong",
    "belong_beliefs",# Know that people around me share my attitudes and beliefs.
    "charity_donate", #
    "hours_charity", #,#Hours spent in activities/Hours spent … voluntary/charitable work
    "rural_gch_2018_l",
    "alert_level_combined_lead"
  ) %>% 
  mutate(
    #initialize 'censored' 
    censored = ifelse(lead(year_measured) == 1, 1, 0),
    
    # modify 'censored' based on the condition; no need to check for NA here as 'censored' is already defined in the previous step
    censored =  ifelse(is.na(censored) & year_measured == 1, 1, censored),
  
    # Apply the case_when condition for setting 'censored' based on 'wave' and the dynamic column specified by 'nzavs_exposure'
    censored = case_when(
      # Add this condition to keep previous modifications unless the specific condition is met
      !is.na(censored) ~ censored,
      
      # Then check if 'wave' is 2019 and the specified exposure is NA, adjusting the condition to reflect the accurate logic
      wave == 2019 & !is.na(!!sym(nzavs_exposure)) ~ 1,
      
      # Default case if none of the above apply; might not be necessary if all possibilities are covered
      TRUE ~ 0
    )
  ) %>% mutate( censored = as.factor(censored))


  
colnames(dat_long)
# check 
table1::table1( ~religion_religious + censored | wave, data = dat_long)

