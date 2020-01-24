#************************************
#pq_tfidf_classing.R
#written by Graeme Jan 2020
#reads previously allocated questions
#calculates tf_idf for new PQ
#matches to predicted topic

#called by pq_scrape.R
#************************************

#************************************
#load packages####
#************************************
library(tidyverse)
library(tidytext)
library(glue)

#************************************
#read assigned questions####
#************************************

#read assigned questions from drive
q_path <- "/conf/linkage/output/IR2020_PQ/pq_allocation/"

assigned <- read_csv(glue("{q_path}/allocated/all_allocated_pqs.csv"), 
                     locale = locale(encoding = "windows-1252")) %>% 
            select(event_id, item_text, topic_area) %>%
            bind_rows(read_csv(glue("{q_path}/data/dummy_pqs.csv"),
                      locale = locale(encoding = "windows-1252")))


txt_to_remove <- paste0("to_ask_the_scottish_government_",
                        "to_ask_the_scottish_parliamentary_",
                        "to_ask_the_first_minister_",
                        collapse = "|")

#clean up questions
assigned <- assigned %>% 
  mutate(item_text = janitor::make_clean_names(item_text),
         item_text = str_replace_all(item_text, txt_to_remove,""),
         item_text = str_replace_all(item_text, "_", " "),
         item_text = gsub("\\p{Nd}+", "", item_text, perl = TRUE)) #remove numbers

#define common words
data(stop_words)
rm_words <- c("nhs", "people", "scotland", "boards", month.name)

#split to words > remove common > stem
#count incidences, calculate tf_idf
assigned <- assigned %>% 
            unnest_tokens(word, item_text, token = "words") %>%
            filter(!word %in% rm_words) %>%
            anti_join(stop_words) %>%
            mutate(word = SnowballC::wordStem(word)) %>%
            filter(!nchar(word) == 1) %>%
            count(topic_area, word) %>% arrange(desc(n)) %>%
            bind_tf_idf(term = word, topic_area, n)

#************************************
#process new questions####
#************************************

#clean up new questions
pq_topic <- new_pq %>% 
            select(event_id, item_text) %>%
            mutate(item_text = janitor::make_clean_names(item_text),
                  item_text = str_replace_all(item_text, txt_to_remove,""),
                  item_text = str_replace_all(item_text, "_", " "),
                  item_text = gsub("\\p{Nd}+", "", item_text, perl = TRUE))

#use the sum of tf_idfs across words to predict topic
#may lose questions if no topics
pq_topic <- 
  pq_topic %>% 
  unnest_tokens(word, item_text) %>% #split to words
  filter(!word %in% rm_words) %>% #remove stop words
  anti_join(stop_words) %>% #remove stop words
  mutate(word = SnowballC::wordStem(word)) %>% #stem
  filter(!nchar(word) == 1) %>% #remove single characters
  left_join(assigned, by = "word") %>% #join to tf_idf data
  filter(!is.na(topic_area)) %>%  #remove blank topics
  group_by(event_id, topic_area) %>% #calculate by question/topic
  summarize(total_tfidf = sum(tf_idf)) %>% #get totals
  slice(which.max(total_tfidf)) #take maximum 

#join back to new pqs, remove prediction for not flagged
new_pq <- new_pq %>%  
          left_join(pq_topic, by = "event_id") %>% 
          select(event_id,expected_answer_date, meeting_date,
                 approved_date, name, party, mp_area, item_text,
                 flag, topic_area, action, notes) %>% 
          mutate(topic_area = case_when(flag == FALSE ~ "", TRUE ~ topic_area))