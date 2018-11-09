# NZ only data for graph

# based on friendships

library(rtweet)
library(dplyr, quietly = TRUE)
library(ggplot2)
library(tidyr)
library(knitr)
library(stringr)
library(ggraph)
library(igraph)

have_data_on <- list.dirs("../fsc_acc", full.names=FALSE)
have_data_on <- have_data_on[have_data_on != ""]
favs <- paste0("../fsc_acc/", have_data_on, "/fav_", have_data_on, ".csv")
profiles <- paste0("../fsc_acc/", have_data_on, "/who_", have_data_on, ".csv")
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

NZ <- read.csv("~/Syncplicity Folders/support_files/assignment.csv",
                  colClasses = "character") %>% filter(locat == "NZ")

nzs <- frnset %>% filter(user %in% NZ$user_id, friends_id %in% NZ$user_id) %>%
    mutate(anticombo = paste(friends_id, user))
nzmatches <- nzs %>% 
    mutate(combo = paste(user, friends_id)) %>%
    select(-anticombo) %>%
    rename(prime=user, secondary=friends_id) %>%
    left_join(nzs, by=c("combo" = "anticombo")) %>%
    mutate(strength = 2 - as.numeric(!is.na(user))) %>%
    select(prime, secondary, strength)

graph <- graph_from_data_frame(nzmatches)
layout <- create_layout(graph, layout = 'fr')
# like fr or dh 
# ggraph(layout) + 
#    geom_edge_link(aes(colour = factor(strength))) + 
#    geom_node_point()
# 
# # Not specifying the layout - defaults to "auto"
# ggraph(graph) + 
#     geom_edge_link(aes(colour = factor(year))) + 
#     geom_node_point()

write.csv(nzmatches, "~/Syncplicity Folders/support_files/nzmatches.csv", row.names = FALSE)
