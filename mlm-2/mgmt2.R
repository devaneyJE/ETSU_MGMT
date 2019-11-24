library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

#reading---------------------------------------
mgmt <- read.csv("mgmt2.csv", header = T, stringsAsFactors = F)
#str(mgmt)
#View(mgmt)

#variable string and day count extraction------------
non.id <- names(subset(mgmt, select = -Assessment.ID))#take out ID variable from names; creating list of surveys

surveys <- non.id %>% 
  str_split("Day") %>% unlist() %>% 
  map_chr(str_extract, "^[:alpha:]+") %>% 
  unique() %>% na.omit() %>% as.character()

num_days <- mgmt %>%
  names() %>%
  str_extract("[:digit:]{1,}") %>%
  as.numeric() %>%
  max(na.rm = T)

#reverse scoring
#x is variable/survey, y is question number to reverse score, n is max possible likert value for survey
rev_score <- function(x, y, n){
  for(i in 1:num_days){
    mgmt[, which(
      names(mgmt) == paste0(
        deparse(substitute(x)),
        deparse(substitute(y)),
        "Day",
        i))] <<- mgmt[, which(
                        names(mgmt) == paste0(
                          deparse(substitute(x)),
                          deparse(substitute(y)),
                          "Day",
                          i))] %>%
      map_dbl(~ (n + 1) - .x)
  }
}

# item_pull <- function(x, y, z){
#   x[, which(
#     names(x) == paste0(
#       deparse(substitute(y)),
#       deparse(substitute(z)),
#       "Day",
#       i))]
# }
# 
# rev_score <- function(x, nQ){
#   map_dbl(x, ~ (nQ + 1) - .x)
# }

#long format----------------------------------
mgmt.long <- mgmt %>% pivot_longer(
   cols = names(subset(mgmt, select = -Assessment.ID)),
   names_to = "survey",
   values_to = "q.scores"
   )
View(mgmt.long)

#survey separation/scoring --------------------
list_arrange <- function(df){ #x = surveys, df = mgmt
  assign("data_list_unscored", list(), .GlobalEnv)
  for(i in 1:length(surveys)){
    assign(paste0(surveys[i], "_responses"),
           df %>% select(matches(paste0("^", surveys[i])))
    ) ->> data_list_unscored[[i]]
  }
}
list_arrange(mgmt)
names(data_list_unscored) <- surveys

#enter survey name to view df with survey-specific responses
unscored_survey_df <- function(i){
  View(data_list_unscored[[deparse(substitute(i))]])
}
scored_survey_df <- function(i){
  View(data_list_scored[[deparse(substitute(i))]])
}

#scoring---------------------------------

assign("data_list_scored", list(), .GlobalEnv)

for(i in 1:length(surveys)){
  data_list_scored[[i]] <- data.frame(
    matrix(
      data = rep(NA, times = nrow(data_list_unscored[[i]])*num_days),
      ncol = num_days, nrow = nrow(data_list_unscored[[i]])))
  data_list_scored[[i]] <- data_list_scored[[i]] %>% 
    setNames(., c(paste0("Day", 1:num_days, "_score")))
}

for(i in 1:length(surveys)){
  for(j in 1:num_days){
    data_list_scored[[i]][, paste0("Day", j, "_score")] <- data_list_unscored[[i]][, str_subset(names(data_list_unscored[[i]]), paste0("Day", j, "$"))] %>%
    as.data.frame() %>% apply(1, mean)
      #apply mean
      #how many values involved in mean
  }
}
names(data_list_scored) <- surveys

#tryCatch
#after expand grid