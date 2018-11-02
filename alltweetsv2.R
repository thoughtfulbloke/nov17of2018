library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
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

terrible_twos <- v_for_visitor %>%
    filter(tolower(reply_to_screen_name) == "lauren_southern" |
               tolower(reply_to_screen_name) == "stefanmolyneux" ) %>%
    select(reply_to_user_id, reply_to_screen_name) %>% distinct()

replies1 <- v_for_visitor %>% 
    filter(reply_to_screen_name %in% terrible_twos$reply_to_screen_name) %>%
    mutate(was = "replied_to")
quotes1 <- v_for_visitor %>% 
    filter(quoted_screen_name %in% terrible_twos$reply_to_screen_name) %>%
    mutate(was = "quoted")
retweets1 <- v_for_visitor %>% 
    filter(retweet_screen_name %in% terrible_twos$reply_to_screen_name) %>%
    mutate(was = "retweeted")

contact_points <- bind_rows(replies1, quotes1, retweets1)

nations <- read.csv("~/Syncplicity Folders/support_files/assignment.csv", colClasses = "character") %>%
    mutate(user_id = paste0("x", user_id)) %>% select(-screen_name)

contact_points %>% left_join(nations) %>% 
    mutate(ID_NZ = case_when(
        locat == "NZ" ~ "is NZ", 
        TRUE ~ "not NZ")) %>%
    count(was, ID_NZ) %>% 
    group_by(ID_NZ) %>%
    mutate(prop= n/sum(n)) %>%
    ggplot(aes(x=ID_NZ, fill=ID_NZ, y=prop)) + geom_col(position = "dodge") +
    facet_wrap(~ was, ncol=3) + scale_fill_viridis_d(end=0.95) + theme_dark() +
    ggtitle("Account actions in direct response to Canuckleheads") +
    xlab("account location") + ylab("Proportion of respondents")
    
                                                 