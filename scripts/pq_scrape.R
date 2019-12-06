#************************************
#pq_scrape.R
#written by Graeme Nov 2019
#reads data from PQ website
#looks for new questions
#checks for flags
#saves output for archive/processing

#called by pq_sched.R
#run by cronR package on RStudio Server
#************************************

#************************************
#load packages####
#************************************
library(tidyverse)
library(janitor)
library(lubridate)
library(glue)
library(jsonlite)

#*******************************
#read flagged/archive
#*******************************
#this takes the archive already saved and gets IDs
#takes it from network
#this path won't work on desktop, but it runs through server
archive <- readRDS(glue("/conf/linkage/output/IR2019_PQ/pq_allocation/data/pq_archive_{year(now())}.RDS"))
archive <- select(archive, unique_id)

#************************************
#scrape PQ website####
#************************************

#get current year questions
pq_urls <- paste0("https://data.parliament.scot/api/motionsquestionsanswersquestions?year=", year(now()))

#get current PQs
pqs <- as_tibble(fromJSON(txt = url(pq_urls), simplifyDataFrame = TRUE))
pqs <- clean_names(pqs)

#get MSPs
msp_url <- "https://data.parliament.scot/api/members/json"
msp <- as_tibble(fromJSON(txt = url(msp_url), simplifyDataFrame = TRUE))
msp <- clean_names(msp)

#tidy up
msp <- msp %>%
      separate(parliamentary_name, into = c("surname", "firstname"), sep = ", ", extra = "merge", remove = TRUE) %>%
      mutate(name = paste(preferred_name, surname)) %>%
      select(person_id, name) %>%
      rename(mspid = person_id)


#get constituency
constituency_url <- "https://data.parliament.scot/api/constituencies/json"
constituency <- as_tibble(fromJSON(txt = url(constituency_url), simplifyDataFrame = TRUE))
constituency <- clean_names(constituency)

constituency <- constituency %>%
                rename(constituency_id = id, constituency_name = name) %>%           
                select(-region_id, -valid_from_date, -valid_until_date, -short_name)

#get region
region_url <- "https://data.parliament.scot/api/regions/json"
region <- as_tibble(fromJSON(txt = url(region_url), simplifyDataFrame = TRUE))
region <- clean_names(region)

region <- region %>%
          rename(region_name = name, region_id = id) %>% 
          select(-start_date,-end_date)

#event ID
event_subtypes_url <- "https://data.parliament.scot/api/motionsquestionsanswerseventsubtypes/json"
event_subtypes <- as_tibble(fromJSON(txt = url(event_subtypes_url), simplifyDataFrame = TRUE))
event_subtypes <- clean_names(event_subtypes)

#clean up
event_subtypes <- event_subtypes %>%
                  select(-intro_text, -event_type_id) %>%
                  filter(str_detect(event_sub_type, fixed("question", ignore_case=TRUE)))

#merge into one data set
df <- pqs %>% left_join(msp, by = "mspid") 
df <- df %>% left_join(region, by = "region_id")
df <- df %>% left_join(constituency, by = "constituency_id")
df <- df %>% left_join(event_subtypes, by = "event_sub_type_id")

#*******************************
#remove those checked already
#*******************************
new_pq <- df %>% filter(!unique_id %in% archive$unique_id)

#*******************************
#define keywords to check
#*******************************

#keywords to check for
flag_strings <- tolower(
                c("nhs", "health", "hospital", "infirmary", "disease", "condition ", "conditions", 
                "procedure", "surgery", "surgical", "operations", "operation", "diagnos", 
                "inpatient", "outpatient", "nurse ", "nurses", "nursing", "dentist", "doctor", 
                "heart", " liver", "kidney", "renal", "ophthalmic", "bowel", "alcohol", "drug", "prescribed", "prescription",
                "osteo", "ovarian", "treated", "beds", "isd", "smoking", "cancer", "cervical", "medicine", "treatment", 
                "workforce", "suicide", "pharmacy", "pharmacies", "pharmacist", "therapy", "deaths",
                "surgeon", "optometrist", "therapist", "dental", "emergency","general practice", "gp",
                "community care", "maternity", "birth", "midwife", "midwives", "eye", 
                "mental health", "public health", "public health scotland",
                "quality indicators", "quality measurement framework", "healthcare audits",
                "sexual health", "stroke", "waiting times", "dying", "babies", "born", "pregnancies",
                "neonatal", "maternal", "breast", "pancreatic", "wellbeing","chronic pain",
                "sickness", "outbreak", "medical", "infection", "AIDS","neurological", "parkinson",
                " flu ", "immunisation", "vaccination", "vaccine", "radiologist", "diabetes", "obstetrician", "neurologist",
                "teenage pregnancy", "pregnancy", "care homes", "patient", "treating", "psychology"))

#these were taken from the topic list on HPS website
hps_keywords <- tolower(
                c("antimicrobial", "bacteraemia", "Botulism", "Brucella", "Campylobacter", "Chickenpox",
                  "Chlamydia", "Clostridioides difficile", "Creutzfeldt-Jakob disease", "CJD",
                  "Cryptosporidium", "Cyclospora", "Diphtheria", "Escherichia coli", "E. coli", "Giardia",
                  "Gonorrhoea", "Haemophilus influenzae", "Healthcare Associated Infection",
                  "Hepatitis", "HIV", "Human papillomavirus", "HPV", "Influenza", "Legionella",
                  "Leptospirosis", "Listeria", "Lyme Disease", "Malaria", "Measles", "Meningococcal", "coronavirus",
                  "Mumps", "Norovirus", "Pneumococcal", "Polio", "Rabies", "Rotavirus", "Rubella", "Salmonella",
                  "Sexually transmitted infection", "Shigella", "Shingles", "Staphylococcus aureus", "Streptococcal",
                  "Syphilis", "Tetanus", "Toxoplasma", "Tuberculosis", "Whooping cough", "Yellow fever", "Yersinia",
                  "Zika", "Zoonoses"))

flag_strings <- c(flag_strings, hps_keywords)

#*******************************
#format columns as dates
#*******************************

new_pq <- new_pq %>%
          mutate(expected_answer_date =  ymd_hms(expected_answer_date), 
          approved_date =  ymd_hms(approved_date),
          meeting_date = ymd_hms(meeting_date))

#*******************************
#add flag and filter
#*******************************
  
#add flag to df if the string is detected
#generate region to display based on either region/constituency
#select columns to keep
new_pq <- new_pq %>%
          mutate(flag = str_detect(tolower(item_text), paste(flag_strings, collapse = "|"))) %>% 
          arrange(desc(flag)) %>%
          mutate(mp_area = if_else(is.na(region_name), 
                           as.character(constituency_name),
                           as.character(region_name))) %>%
          select(event_id, expected_answer_date,
                 meeting_date, approved_date, name,
                 party, mp_area, item_text, flag)

#add blank column header for area
#will remind users to add later
new_pq$topic_area <- ""
new_pq$action <- ""
new_pq$notes <- ""

#get rid of whitespace - stops some encoding issues
new_pq <- new_pq %>% 
          mutate(item_text = str_squish(item_text))

#*******************************
#add suggested contact team
#*******************************

#add code here to check for certain keywords

#will still go to mailbox for review

#*******************************
#save log of new questions
#*******************************

#generate save name
date_to_use <- format(now(), "%Y_%m_%d")
save_name <- glue("{date_to_use}_new_pqs.csv")

#if new PQs then save as csv to server/drive
if (dim(new_pq)[1] > 0) {

write_csv(new_pq, 
          path = glue("pq_auto_emails/data/new_pq/{save_name}"))
  
write_csv(new_pq, 
          path = glue("/conf/linkage/output/IR2019_PQ/pq_allocation/to_be_allocated/{save_name}"))
}

#*******************************
#overwrite archive
#*******************************
#save copy to server for backup
saveRDS(df, file = glue("pq_auto_emails/data/pq_archive_{year(now())}.RDS"))

#save copy to drive
saveRDS(df, file = glue("/conf/linkage/output/IR2019_PQ/pq_allocation/data/pq_archive_{year(now())}.RDS"))
