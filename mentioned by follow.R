

library(readr)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(purrr))
library(tidyr)
library(patchwork)

set1 = "../fprog/canadian_racists"
set2 = "../fprog/mol_racist"
set3 = "../fprog/South_racist"
set4 = "../fprog/recent_direct"
set5 = "../fprog/recent_direct_regather600"
f1 <- paste0(set1, "/", list.files(set1, pattern=".csv$"))
f2 <- paste0(set2, "/", list.files(set2, pattern=".csv$"))
f3 <- paste0(set3, "/", list.files(set3, pattern=".csv$"))
f4 <- paste0(set4, "/", list.files(set4, pattern=".csv$"))
f5 <- paste0(set5, "/", list.files(set5, pattern=".csv$"))
full_list = c(f1,f2,f3,f4,f5)

v_for_visitor <- bind_rows(lapply(full_list, read_csv, col_types=cols(
    .default = col_character()))) %>% distinct()
# 666092 tweets

m_for_mentions <- v_for_visitor %>% select(created_at, user_id, mentions_user_id) %>%
    mutate(mentions = map(mentions_user_id, function(x){unlist(strsplit(x, split=" "))})) %>%
    unnest(mentions) %>% 
    mutate(NZ_time = with_tz(ymd_hms(created_at), tz="Pacific/Auckland"),
           step1 = floor_date(NZ_time,unit = "hour"))

have_data_on <- list.dirs("../fsc_acc", full.names=FALSE)
have_data_on <- have_data_on[have_data_on != ""]
favs <- paste0("../fsc_acc/", have_data_on, "/fav_", have_data_on, ".csv")
frns <- paste0("../fsc_acc/", have_data_on, "/frn_", have_data_on, ".csv")

read_frns <- function(x) {
    if(file.exists(x)){
        filinf <- file.info(x)
        if(filinf$size < 8){
            return(NULL)
        }
        stored_frns <- read.csv(x, colClasses = "character")
        stored_frns %>%
            mutate(friends_id = gsub("x","",user_id)) %>%
            select(user, friends_id) %>%
            return()
    } else {
        return(NULL)
    }
}


frnset <- bind_rows(lapply(frns, read_frns))

on_or_offshore <-
    read_csv(
        "~/Syncplicity Folders/support_files/assignment.csv",
        col_types = cols(.default = col_character())
    ) %>% mutate(withx = paste0("x", user_id)) %>% select(-user_id)

frnset <- frnset %>%
    mutate(lookup = paste(paste0("x",user), paste0("x",friends_id)))
n_for_network <- m_for_mentions %>% 
    inner_join(on_or_offshore, by=c("mentions" = "withx")) %>%
    mutate(close = !paste(mentions, user_id) %in% frnset$lookup) %>%
    filter(locat=="NZ")
s_for_summarised <- n_for_network %>% group_by(mentions, step1) %>% 
    summarise(og = sum(!close), ogthres = og > 50) %>%
    group_by(step1) %>% summarise(mean_outgroup = sum(og)/1505, thres=sum(og)) 
ggplot(s_for_summarised, aes(x=step1, y=thres)) + geom_line() + theme_minimal() +
    ggtitle("Of 1505 NZ accounts, the number of accounts in a given hour \ngettting more than 50 notifications from outside of thier friend groups") +
    xlab("Date") + ylab("Number of accounts") +
    geom_vline(xintercept = ISOdatetime(2018,8,2,19,44,00, tz="Pacific/Auckland"), 
               colour="blue", lwd=1.1) +
    annotate("text", x = ISOdatetime(2018,8,3,9,44,00, tz="Pacific/Auckland"),
             y = 1000, label="visit", colour="blue")

    