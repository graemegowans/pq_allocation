#************************************
#pq_sched.R
#written by Graeme Nov 2019
#calls pq_scrape.R to look for new questions

#run by cronR package on RStudio Server
#************************************

#************************************
#load packages####
#************************************
#called in source so not
#strictly necessary
library(gmailr)
library(glue)
library(tidyverse)
library(lubridate)

#************************************
#generate data####
#************************************

#from server
#source("pq_auto_emails/scripts/pq_scrape.R")

#from network
source("/conf/linkage/output/IR2019_PQ/pq_allocation/scripts/pq_scrape.R")

#get email addresses
#saved in new file so addresses aren't shared
emails <- read_csv("/conf/linkage/output/IR2019_PQ/pq_allocation/email_addresses.csv")
gmail_acct <- filter(emails, contact == "gmail_account")
stats_gov <- filter(emails, contact == "stats_gov")

#************************************
#format message####
#can delete this once file version
#is running and approved
#************************************
if(dim(new_pq)[1] > 0){

#format dates
#add a column of HTML formatted text for new PQs
new_pq <- new_pq %>%
          mutate(expected_answer_date = format(expected_answer_date, "%d/%m/%y"),
                 approved_date = format(approved_date, "%d/%m/%y"),
                 meeting_date = format(meeting_date, "%d/%m/%y")) %>%
          mutate(message = glue(
            "<strong>{event_id}</strong><br>",
            "Answer expected: <strong>{expected_answer_date}</strong><br>",
            "Meeting date (if applicable): <strong>{meeting_date}</strong><br>",
            "Question approved: {approved_date}<br>",
            "Asked by: {name}, {party}, {mp_area}<br>",
            "<strong>Question: </strong>{item_text}<br><br>"))

#generate HTML string of message
flagged <- new_pq %>% filter(flag == TRUE)
flagged <- str_flatten(flagged$message)

not_flagged <- new_pq %>% filter(flag == FALSE)
not_flagged <- str_flatten(not_flagged$message)

msg <- paste0("<h3>Flagged Questions</h3>",
              flagged,
              "<br>",
              "**********************",
              "**********************",
              "<h3>Not Flagged</h3>",
              "**********************",
              "**********************",
              "<br>",
              not_flagged)

#************************************
#send email####
#************************************

#authorize
#gm_auth_configure(path = "credentials.json")
  gm_auth_configure(path = "/conf/linkage/output/IR2019_PQ/pq_allocation/credentials.json")
  gm_auth(email = gmail_acct$info)
  
  #generate email
  test_email <-
    gm_mime() %>%
    gm_to(stats_gov$info) %>%
    gm_from(gmail_acct$info) %>%
    gm_subject(paste(Sys.time(),"flagged parliamentary questions")) %>%
  gm_html_body(msg) #add html to email

#send email
gm_send_message(test_email)

}

#************************************
#send links to file
#************************************

if(dim(new_pq)[1] > 0){
  
  #************************************
  #send email####
  #************************************
  
  #get counts
  num_flagged <- new_pq %>% filter(flag == TRUE)
  num_flagged <- dim(num_flagged)[1]
  num_not_flagged <- dim(new_pq)[1]-num_flagged
  
  #generate intro text
  msg <- glue(
    "New PQs ({num_flagged} flagged, {num_not_flagged} not flagged) have been added at: ", 
    "\\\\stats\\cl-out\\IR2019_PQ\\pq_allocation\\to_be_allocated\\{save_name}",
    "<br><br>",
    "They need a topic area assigned. Options (in bold) are:",
    "<ul>",
      "<li><strong>pop_health</strong></li>",
      "<li><strong>quality_indicators</strong></li>",
      "<li><strong>prescribing</strong></li>",
      "<li><strong>nes_wf</strong> - NES workforce</li>",
      "<li><strong>nes_mh_wf</strong> - NES mental health workforce (CAMHS, Psychology)</li>",
      "<li><strong>nes_parenting</strong> - NES Psychology of Parenting</li>",
      "<li><strong>costs_nrac</strong> - Costs or National Resource Allocation Formula</li>",
      "<li><strong>waiting_times</strong> - acute/secondary WT, 18 wk RTT</li>",
      "<li><strong>mhsa</strong> - Mental Health and Service Access</li>",
      "<li><strong>hsc</strong> - Health and Social Care</li>",
      "<li><strong>primary_care_gp</strong></li>",
      "<li><strong>primary_care_other</strong> - eye and dental</li>",
      "<li><strong>shc_audits</strong> - Scottish Healthcare Audits</li>",
      "<li><strong>hps</strong> - Health Protection Scotland</li>",
      "<li><strong>mesh</strong> - Mesh Consultancy</li>",
    "</ul>",
    "Notes: if you want to assign one question to multiple areas, then create a duplicate row for the question and fill in different areas ",
    "These will then be emailed to both areas.",
    "<br><br>",
    "They also need an action assigned:",
    "<ul>",
      "<li>review</li>",
      "<li>information</li>",
    "</ul>",
    "You can also add any notes to be included - e.g. other questions that are referred to.",
    "<br><br>",
    "Once all are assigned, save the file to: ",
    "\\\\stats\\cl-out\\IR2019_PQ\\pq_allocation\\allocated",
    "<br><br>",
    "Using the name format: <strong>YYYY_MM_DD_new_pqs_allocated.csv</strong>"
  )
  
  #authorize
  #gm_auth_configure(path = "credentials.json")
  gm_auth_configure(path = "/conf/linkage/output/IR2019_PQ/pq_allocation/credentials.json")
  gm_auth(email = gmail_acct$info)
  
  #generate email
  test_email <-
    gm_mime() %>%
    gm_to(stats_gov$info) %>%
    gm_from(gmail_acct$info) %>%
    gm_subject(glue("{Sys.time()} - new PQs added")) %>%
    gm_html_body(msg)
  
  #send email
  gm_send_message(test_email)
  
}
