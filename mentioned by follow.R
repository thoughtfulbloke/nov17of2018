library(readr)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(purrr))
library(tidyr)
library(ggraph)
library(igraph)
library(ggthemes)
library(gganimate)

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

# generate graph data
nzm <- read.csv("~/Syncplicity Folders/support_files/nzmatches.csv",
                colClasses = "character")


# mentions

m_for_mentions <- v_for_visitor %>% select(created_at, user_id, mentions_user_id) %>%
    mutate(mentions = map(mentions_user_id, function(x){unlist(strsplit(x, split=" "))})) %>%
    unnest(mentions) %>% 
    mutate(NZ_time = with_tz(ymd_hms(created_at), tz="Pacific/Auckland"),
           step1 = floor_date(NZ_time,unit = "hour")) %>%
