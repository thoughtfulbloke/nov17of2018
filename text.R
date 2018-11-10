# checking text features:

library(readr)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(purrr))
library(tidyr)
library(stringr)

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
on_or_offshore <-
    read_csv(
        "~/Syncplicity Folders/support_files/assignment.csv",
        col_types = cols(.default = col_character())
    ) %>% mutate(withx = paste0("x", user_id))

m_for_mentions <- v_for_visitor %>% 
    filter(is.na(quoted_status_id)) %>% # no quote tweets as multiple authors
    select(created_at, text, user_id, mentions_user_id) %>%
    mutate(mentions = map(mentions_user_id, function(x){unlist(strsplit(x, split=" "))})) %>%
    unnest(mentions)
a_for_all <- m_for_mentions %>% 
    left_join(on_or_offshore, by=c("user_id" = "withx")) %>%
    rename(from_where = locat) %>%
    left_join(on_or_offshore, by=c("mentions" = "withx")) %>%
    rename(to_where = locat)
nz <- a_for_all %>% filter(from_where == "NZ")
us <- a_for_all %>% filter(from_where == "US")


nz %>% summarise(sum(str_detect(text, fixed("you ")))) / nrow(nz)
us %>% summarise(sum(str_detect(text, fixed("you ")))) / nrow(us)
nz %>% summarise(sum(str_detect(text, fixed("I ", ignore_case = FALSE)))) / nrow(nz)
us %>% summarise(sum(str_detect(text, fixed("I ", ignore_case = FALSE)))) / nrow(us)
nz %>% summarise(sum(str_detect(text, fixed("evidence", ignore_case = TRUE)))) / nrow(nz)
us %>% summarise(sum(str_detect(text, fixed("evidence", ignore_case = TRUE)))) / nrow(us)
nz %>% summarise(sum(str_detect(text, fixed("fact", ignore_case = TRUE)))) / nrow(nz)
us %>% summarise(sum(str_detect(text, fixed("fact", ignore_case = TRUE)))) / nrow(us)
nz %>% summarise(sum(str_detect(text, fixed("freedom", ignore_case = TRUE)))) / nrow(nz)
us %>% summarise(sum(str_detect(text, fixed("freedom", ignore_case = TRUE)))) / nrow(us)
nz %>% summarise(sum(str_detect(text, fixed("truth", ignore_case = TRUE)))) / nrow(nz)
us %>% summarise(sum(str_detect(text, fixed("truth", ignore_case = TRUE)))) / nrow(us)
nz %>% summarise(sum(str_detect(text, fixed("value", ignore_case = TRUE)))) / nrow(nz)
us %>% summarise(sum(str_detect(text, fixed("value", ignore_case = TRUE)))) / nrow(us)
nz %>% summarise(sum(str_detect(text, fixed("like", ignore_case = TRUE)))) / nrow(nz)
us %>% summarise(sum(str_detect(text, fixed("like", ignore_case = TRUE)))) / nrow(us)
nz %>% summarise(sum(str_detect(text, fixed(" as ", ignore_case = TRUE)))) / nrow(nz)
us %>% summarise(sum(str_detect(text, fixed(" as ", ignore_case = TRUE)))) / nrow(us)

library(tidytext)
nz_words <- nzus %>%
    unnest_tokens(word, text, token = "tweets") %>%
    anti_join(get_stopwords()) %>%
    count(word, sort = TRUE) %>%
    filter(!str_detect(word,fixed("@"))) %>% 
    mutate(nz_prop = n/nrow(nzus)) %>%
    select(word, nz_prop)

usnz %>%
    unnest_tokens(word, text, token = "tweets") %>%
    anti_join(get_stopwords()) %>%
    count(word, sort = TRUE) %>%
    filter(!str_detect(word,fixed("@"))) %>% 
    mutate(us_prop = n/nrow(usnz)) %>%
    select(word, us_prop) %>%
    full_join(nz_words) %>%
    mutate(nz_prop = ifelse(is.na(nz_prop), 0,nz_prop)) %>%
    mutate(us_prop = ifelse(is.na(us_prop), 0,us_prop)) %>%
    mutate(ndiffu = nz_prop - us_prop) %>% 
    arrange(desc(ndiffu)) %>% slice(1:31) %>%
    select(word) %>% write_lines(path="nzout.txt")

usnz %>%
    unnest_tokens(word, text, token = "tweets") %>%
    anti_join(get_stopwords()) %>%
    count(word, sort = TRUE) %>%
    filter(!str_detect(word,fixed("@"))) %>% 
    mutate(us_prop = n/nrow(usnz)) %>%
    select(word, us_prop) %>%
    full_join(nz_words) %>%
    mutate(nz_prop = ifelse(is.na(nz_prop), 0,nz_prop)) %>%
    mutate(us_prop = ifelse(is.na(us_prop), 0,us_prop)) %>%
    mutate(ndiffu = nz_prop - us_prop) %>% 
    arrange(ndiffu) %>% slice(1:31) %>%
    select(word) %>% write_lines(path="usout.txt")

nz_wordsu <- nzus %>%
    unnest_tokens(word, text, token = "tweets") %>%
    count(word, sort = TRUE) %>%
    filter(!str_detect(word,fixed("@"))) %>% 
    mutate(nz_prop = n/nrow(nzus)) %>%
    select(word, nz_prop)

usnz %>%
    unnest_tokens(word, text, token = "tweets") %>%
    count(word, sort = TRUE) %>%
    filter(!str_detect(word,fixed("@"))) %>% 
    mutate(us_prop = n/nrow(usnz)) %>%
    select(word, us_prop) %>%
    full_join(nz_wordsu) %>%
    mutate(nz_prop = ifelse(is.na(nz_prop), 0,nz_prop)) %>%
    mutate(us_prop = ifelse(is.na(us_prop), 0,us_prop)) %>%
    mutate(ndiffu = nz_prop - us_prop) %>% 
    View()
