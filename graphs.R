library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

demention <- function(df){
    return(data.frame(sent_to = unlist(strsplit(df$mentions_user_id, split=" ")),
                   stringsAsFactors = FALSE))
}

longform <- v_for_visitor %>%
    filter(!is.na(mentions_user_id)) %>%
    select(user_id, status_id, created_at, text, mentions_user_id) %>%
    group_by(user_id, status_id, created_at, text) %>%
    nest() %>%
    mutate(unread_instructions = map(data, demention)) %>%
    unnest(unread_instructions) 

auth <- 
    o_for_offshore %>%
    inner_join(a_for_authoritarian, by="user_id") %>%
    mutate(af = nett_nz * (locat == "NZ") + nett_au * (locat == "AU") +
                   nett_gb * (locat == "GB") + nett_us * (locat == "US"),
               af0 = nz0 * (locat == "NZ") + au0 * (locat == "AU") +
                   gb0 * (locat == "GB") + us0 * (locat == "US"),
               balance = fa0-af0, interest = (af + nett_fa)/allike) %>%
    select(user_id, locat, balance)

bymention <- longform %>%
    left_join(auth, by="user_id") %>%
    rename(sender_location = locat, sender_balance = balance) %>%
    left_join(auth, by=c("sent_to"="user_id")) %>%
    rename(sendto_location = locat, sendto_balance = balance)

from <- bymention %>% 
    mutate(send_nz = case_when(
        sender_location == "NZ" ~  "NZ", 
        TRUE ~ "other"),
        sent_to_nz = case_when(
            sendto_location == "NZ" ~  "NZ", 
            TRUE ~ "other")
        ) %>%
    filter(send_nz != sent_to_nz) %>% # 115465 of the 3065316
    count(user_id, send_nz) %>%
    rename(ui = user_id, loc_out = send_nz, n_out = n)

to <- bymention %>% 
    mutate(send_nz = case_when(
        sender_location == "NZ" ~  "NZ", 
        TRUE ~ "other"),
        sent_to_nz = case_when(
            sendto_location == "NZ" ~  "NZ", 
            TRUE ~ "other")
    ) %>%
    filter(send_nz != sent_to_nz) %>% # 115465 of the 3065316
    count(sent_to, sent_to_nz) %>%
    rename(ui = sent_to, n_in = n, loc_in = sent_to_nz)

 from %>% full_join(to) %>%
     mutate(n_out = ifelse(is.na(n_out), 0, n_out),
            n_in = ifelse(is.na(n_in), 0, n_in),
            loc = ifelse(is.na(loc_out),loc_in, loc_out)) %>%
     ggplot(aes(x = log2(n_out), y=log2(n_in), colour=loc)) + geom_point(alpha=0.3) +coord_equal() +
     facet_wrap(~ loc, nrow=1) + geom_abline(slope=1, intercept = 0) +theme_minimal()
 
 
 from %>% full_join(to) %>%
     mutate(n_out = ifelse(is.na(n_out), 0, n_out),
            n_in = ifelse(is.na(n_in), 0, n_in),
            loc = ifelse(is.na(loc_out),loc_in, loc_out)) %>%
     filter(n_out > 0) %>%
     mutate(ratio = n_in/n_out) %>%
     ggplot(aes(x = ratio, colour=loc)) + geom_density(alpha=0.3) +
         theme_minimal() + coord_cartesian(xlim=c(0,100))
 