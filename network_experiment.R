library(dplyr, quietly = TRUE)
library(ggplot2)
library(tidyr)
library(knitr)
library(stringr)
library(ggraph)
library(igraph)
library(ggthemes)
nzm <- read.csv("~/Syncplicity Folders/support_files/nzmatches.csv", colClasses = "character")

graph <- graph_from_data_frame(nzm)
layout <- create_layout(graph, layout = 'fr') %>% select(x,y,name) %>%
    mutate(name = as.character(name))
connects <- nzm %>% inner_join(layout, by=c("prime" = "name")) %>%
    rename(x1 = x, y1= y) %>%
    inner_join(layout, by=c("secondary" = "name")) %>%
    rename(x2 = x, y2= y)
ggplot(connects, aes(x=x1,y=y1, xend=x2, yend=y2)) + 
    geom_point(size=0.2) + theme_tufte() +
    geom_segment(alpha=0.05)

ggplot(layout, aes(x=x,y=y)) + geom_point(size=0.1)
ggraph(layout) + 
    geom_edge_link(aes(colour = factor(strength))) + 
    geom_node_point()

layout <- create_layout(graph, layout = 'fr')
ggplot(layout, aes(x=x,y=y)) + geom_point(size=0.1)
ggraph(layout) + 
    geom_edge_link(aes(colour = factor(strength))) + 
    geom_node_point()
igraph_layouts <- c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
                    'randomly', 'fr', 'kk', 'drl', 'lgl')
lapply(igraph_layouts, function(x){
    ggplot(create_layout(graph, layout = x), aes(x=x,y=y)) + 
        geom_point(size=0.1) + ggtitle(x)})
# like fr
