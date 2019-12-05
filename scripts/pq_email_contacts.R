#***************************************
#name: pq_email_contacts.R
#purpose: automatically send PQ emails
#written by: Graeme 19/11/19

#info: this will take the allocated PQ
#add contact info, pool PQs by area,
#generate and send emails
#***************************************

#***************************************
#install packages####
#***************************************
#RDCOMClient won't install on linux server
#install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
#install.packages("tidyverse")
#install.packages("glue")
#install.packages("tidylog)

#***************************************
#load packages####
#***************************************
library(RDCOMClient)
library(tidyverse)
library(glue)
library(tidylog)

#***************************************
#file to use####
#***************************************

#format usually YYYY_MM_DD_new_pqs_allocated.csv
pq_dated <- "2019_11_28"

#read allocated files from network
pq <- read_csv(glue("allocated/{pq_dated}_new_pqs_allocated.csv"))

#***************************************
#get email addresses####
#***************************************

emails <- read_csv("email_addresses.csv")
phi_pq <- filter(emails, contact == "phi_pq")

#***************************************
# email intro text####
#***************************************

#this will be added to the top of the email
email_intro <- glue(
  "Dear all,<br><br>",
  "You have been allocated the following PQs today to answer.",
  " Please read the instructions below and get in touch with your policy contact to agree a deadline.<br><br>",
  "<strong>Instructions:</strong><br><br>",
  "<strong>For Information</strong> - This indicates that the question is highlighted as of interest to your service area. There is no current expectation of formal input to be provided by you to the Scottish Government's work on their answer to the question. However, you should be prepared in case a SG policy contact decides to make contact with you to help them with some background information.<br><br>",
  "<strong>For Review</strong> - This indicates that the question requires you to consider taking action. You need to consider what information and input you could give to the external Scottish Government's work on an answer. There is an expectation that you make contact with the SG to agree a deadline, if relevant support is required.<br><br>",
  "<strong>Policy contacts at the Scottish Government: </strong>",
  "The Executive Team Programme Hub within the Scottish Government handles PQ allocations. The number to use to check if support is needed on a Parliamentary Question in the Scottish Government is now: 0131 244 4347.  If this number is busy, you should use the secondary number 0131 244 0144 or email ETFMQPQ@scotland.gsi.gov.uk<br><br>",
  "<strong>Expected answer dates:</strong> the dates included below show the deadline for the Scottish Government's answer to appear <em>in Parliament</em>. <strong>Your deadline</strong>, therefore, for any support will be ahead of that."
)

#***************************************
# load contact info/process####
#***************************************

#read contact info for topic areas
contact <- read_csv("contact_info.csv",
                    skip_empty_rows = TRUE,
                    trim_ws = TRUE)


#clean up topic area in case entered incorrectly
#make lower case, replace spaces with underscore, remove whitespace
contact <- contact %>% 
           mutate(topic_area = str_to_lower(topic_area),
                  topic_area = str_replace_all(topic_area, pattern = " ","_"),
                  topic_area = str_squish(topic_area))

#collapse emails and remove blanks
#group by full name to preserve column for using in subject
contact <-  contact %>%
            group_by(topic_area, full_name) %>% 
            summarize(send_to = glue_collapse(contact_email, sep = "; ")) %>% 
            na.omit()


#***************************************
#load allocated questions/process####
#***************************************

#get rid of not allocated
pq <- pq %>% filter(!is.na(topic_area))

#arrange so 'for review' is first
pq <- pq %>% arrange(topic_area, desc(action))

#clean up topic area in case entered incorrectly
#make lower case, replace spaces with underscore, remove whitespace
pq <- pq %>% 
      mutate(topic_area = str_to_lower(topic_area),
             topic_area = str_replace_all(topic_area, pattern = " ","_"),
             topic_area = str_squish(topic_area))

#clean up encoding problems
#server encodes as UTF-8, windows uses Latin1
pq <- pq %>% mutate(item_text = iconv(item_text, to = "Latin1", from = "UTF-8"))

#run this to check encoding - all should be "unknown"
#unknown means uses system default, find by calling Sys.getlocale("LC_CTYPE")
#if any are UTF-8 you might get weird encoding in the email
#check that all item text is ok
pq %>% mutate(encd = Encoding(item_text)) %>% select(event_id,encd, item_text)

#generate message per question
pq <- pq %>%
      mutate(expected_answer_date = format(expected_answer_date, "%d/%m/%y"),
             approved_date = format(approved_date, "%d/%m/%y"),
             meeting_date = format(meeting_date, "%d/%m/%y")) %>%
      mutate(action = glue("<span style='color:blue'>For {action}</span><br>"),
             id = glue("<strong>{event_id}</strong><br>"),
             expct = glue("Answer expected: <strong>{expected_answer_date}</strong><br>"),
             mtg = glue("Meeting date (if applicable): <strong>{meeting_date}</strong><br>"),
             apprvd = glue("Question approved: {approved_date}<br>"),
             askd = glue("Asked by: {name}, {party}, {mp_area}<br>"),
             qstn = glue("<strong>Question: </strong>{item_text}<br>"),
             nts = glue("<strong>Notes:</strong> {notes}<br>")) %>%
      mutate(message = if_else(is.na(notes),
                           glue("{action}{id}{expct}{mtg}{apprvd}{askd}{qstn}<br>"),
                           glue("{action}{id}{expct}{mtg}{apprvd}{askd}{qstn}{nts}<br>")
                           ))
  
#group questions by topic and collapse message
pq <- pq %>% 
      group_by(topic_area) %>%
      select(topic_area, message) %>%
      summarize(pooled = glue_collapse(message))
  

#***************************************
#join contact info to questions####
#***************************************

pq <- left_join(pq, 
                contact, 
                by = "topic_area")

#***************************************
#generate/send emails####
#***************************************

#get topics that have questions allocated
topics <- na.omit(pq$topic_area)
  
#loop across topics and send email
for (i in topics) {
    
  #extract topic info
  x <- pq %>% filter(topic_area == i)
  
  #extract info for emails
  send_to <- str_squish(x$send_to)
  subject <- paste0(Sys.Date()," new PQs - ", x$full_name)
  msg_body <- paste0(email_intro, "<br><br>", x$pooled)
      
  #create an email
  OutApp <- COMCreate("Outlook.Application")
  outMail = OutApp$CreateItem(0)

  #*************************************
  #send test emails to yourself first 
  #to check that nothing looks weird
  #*************************************
  
  #configure email parameters
  #outMail[["To"]] = "your.name@nhs.net"
  outMail[["To"]] = send_to
  
  outMail[["SentOnBehalfOfName"]] = phi_pq$email
  outMail[["subject"]] = subject
  outMail[["htmlbody"]] = msg_body

  #send it
  outMail$Send()
}
