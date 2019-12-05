#***************************************
#name: pq_collate_allocated.R
#purpose: collate all allocated PQs
#written by: Graeme 21/11/19

#info: this takes all the allocated PQs
#combines into one object
#saves as a master list
#can visualize allocations/questions
#***************************************

#***************************************
#load packages####
#***************************************

library(glue)
library(tidyverse)
library(tidylog)
library(RColorBrewer)
library(lubridate)

#***************************************
#get list of files####
#***************************************

#getting list of allocated questions
directory_to_check <- "allocated/"

#get list of files
allocated_files <- list.files(directory_to_check,
                              full.names = TRUE,
                              pattern = ".csv")

#remove the master list
allocated_files <- allocated_files[!allocated_files == glue("{directory_to_check}all_allocated_pqs.csv")]

#combine to one object
pq <- allocated_files %>%
      map_df(~read_csv(.))

#get rid of questions not allocated
pq <- pq %>% filter(!is.na(topic_area))

#***************************************
#fix encoding issues
#***************************************

pq <- pq %>% mutate(item_text = iconv(item_text, to = "Latin1", from = "UTF-8"))

#run this to check encoding - all should be "unknown"
#unknown means uses system default, find by calling Sys.getlocale("LC_CTYPE")
#if any are UTF-8 you might get weird encoding in the email
#check that all item text is ok
pq %>% mutate(encd = Encoding(item_text)) %>% select(event_id,encd, item_text) %>% count(encd)

#***************************************
#output - overwrites file
#***************************************

write_csv(pq,
          glue("{directory_to_check}/all_allocated_pqs.csv"))

#***************************************
#plot for fun
#***************************************

#number per area
pq %>% 
count(topic_area) %>% 
mutate(topic_area = fct_reorder(topic_area, n, .desc = FALSE)) %>% 
ggplot(aes(topic_area, n)) +
  geom_col(aes(fill = topic_area),color = "black") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  xlab("topic area") +
  ylab("PQs assigned") +
  ggtitle(glue("Number of PQ assigned per area at: {Sys.Date()}")) +
  theme_bw()

#as heatmap
pq %>%
mutate(yr_mnth = glue("{year(approved_date)}_{month(approved_date)}")) %>%  
group_by(yr_mnth) %>%
count(topic_area) %>%
ggplot(aes(yr_mnth, topic_area)) +
  geom_tile(aes(fill = n), color = "black") +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues")) +
  ggtitle(glue("Number of PQ assigned per area at: {Sys.Date()}")) +
  xlab("year/month approved") +
  ylab("topic area") +
  theme_bw()

#by mp
pq %>% 
count(name) %>% 
mutate(name = fct_reorder(name, n, .desc = FALSE)) %>% 
  ggplot(aes(name, n)) +
  geom_col(aes(fill = name),color = "black") +
  coord_flip() +
  xlab("MP") +
  ylab("PQs assigned") +
  ggtitle(glue("PQs assigned - by MPs at: {Sys.Date()}")) +
  theme_bw()

#by party
pq %>% 
  count(party) %>% 
  mutate(party = fct_reorder(party, n, .desc = FALSE)) %>% 
  ggplot(aes(party, n)) +
  geom_col(aes(fill = party),color = "black") +
  coord_flip() +
  xlab("party") +
  ylab("PQs assigned") +
  ggtitle(glue("PQs assigned - by party at: {Sys.Date()}")) +
  theme_bw()
