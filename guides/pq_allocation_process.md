pq allocation process
================

### getting new questions

On Tuesdays/Thursdays, `pq_sched.R` runs from Graeme's login on the RStudio Server using `cronR`. This script:

-   runs at 8 am
-   calls `pq_scrape.R` to
    -   download the PQs for this year from website
    -   check these against our archive of questions for new PQs
    -   check these new questions against a list of keywords to add a flag
    -   save all the new questions as a csv file (with date) at: `pq_allocation\to_be_allocated\YYYY_MM_DD_new_pqs.csv`
    -   overwrites the archive of questions `pq_allocation\data`
-   emails the statsgov mailbox to say there are new questions and links to the file

These scripts are saved at: `pq_allocation\scripts`

### how to allocate questions

-   open link to file in email
-   check if questions are relevant
-   assign each question to an area
-   add action (review or information)
-   add notes (e.g. if the question refers to another you can include here)

The email has the codes for each area but they are:

-   **pop\_health** - population health (inc. cancer, heart, stroke, births, sexual health, immunisation)
-   **quality\_indicators** - inc. hospital admission/discharge patient data, beds, HSMR,
-   **prescribing** - inc. prescriptions, medicines, pharmacy
-   **nes\_wf** - NES workforce (inc. staff in post, vacancies, turnover, absences, staff types)
-   **nes\_mh\_wf** - NES mental health workforce (as above but for psychology & CAMHS)
-   **nes\_parenting** - NES Psychology of Parenting
-   **costs\_nrac** - Costs or National Resource Allocation Formula,
-   **waiting\_times** - inc. acute/secondary WT, 18 wk RTT
-   **mhsa** - Mental Health and Service Access (inc. suicides, WT for drug/alcohol/UC/A&E/MH, unintentional injuries)
-   **hsc** - Health and Social Care (inc. drugs/alcohol/smoking, delayed discharge, care homes, end of life care, dementia)
-   **primary\_care\_gp** - inc. practices, consultations
-   **primary\_care\_other** - inc. dental and ophthalmic
-   **shc\_audits** - Scottish Healthcare Audits (inc. trauma, stroke, renal, ECT, musculoskeletal)
-   **hps** - Health Protection Scotland (inc. BBV, STI, HAI, GI, respiratory, flu, international)
-   **mesh** - Mesh Consultancy

Further info about where to allocate is in: `pq_allocation\guides\pq_allocation.docx`

To allocate questions to more than one area, duplicate the row and add different areas to each. If you leave the area blank then it won't be emailed anywhere, so you don't need to delete irrelevant ones.

Then save this as `YYYY_MM_DD_new_pqs_allocated.csv` at: `pq_allocation\allocated`

### emailing questions to teams

To send emails, run `pq_email_contacts.R`. This can't be automated on the server as the email package only works on Windows - so this script must be run locally by someone with access to the cl-out area and the PHI pq mailbox. Good idea to open the project file (`pq_allocation.Rproj`) so that working directory is set. This script:

-   loads allocated PQs from `pq_allocation\allocated`
-   loads contact details from `pq_allocation\contact_info.csv`
-   loads email accounts from `pq_allocation\email_addresses.csv`
-   pools questions by topic area
-   matches contact info to topic area
-   generate one email per area with the list of questions
-   sends emails from PHI PQ mailbox

It is good to send test emails to yourself to make sure that everything looks ok before sending to teams:

``` r
  #send to yourself
  outMail[["To"]] = "your.name@nhs.net"
  #outMail[["To"]] = send_to
```

### compiling all allocated questions

It might be useful to keep a running list of where we allocate questions, rather than having to search through emails. The script `pq_collate_allocated.R` takes all files in `pq_allocation\allocated`, removes those not assigned and saves as a master list: `all_allocated_pqs.csv`.

You can see how many are assigned per area:

``` r
library(tidyverse)
library(glue)

#get data
pq <- read_csv("path_to_pqs.csv")

#number per area
pq %>% 
count(topic_area) %>% 
mutate(topic_area = fct_reorder(topic_area, n, .desc = FALSE)) %>% 
ggplot(aes(topic_area, n)) +
  geom_col(color = "black", fill = "#e7298a") +
  coord_flip() +
  xlab("topic area") +
  ylab("PQs assigned") +
  ggtitle(glue("Number of PQ assigned per area at: {Sys.Date()}")) +
  theme_bw()
```

### setting up cron jobs

R scripts can be scheduled to run at defined intervals on the <a href="http://spsssrv02.csa.scot.nhs.uk:8787" target="_blank">RStudio Server Pro</a> using <a href="https://cran.r-project.org/web/packages/cronR/vignettes/cronR.html" target="_blank">cronR</a>

``` r
install.packages("cronR")
```

Create a command to execute R script:

``` r
#log_append = FALSE overwrites the log each time
cmd <- cronR::cron_rscript("/path/to/script.R", 
                          log_append = FALSE)
```

Generate cron job at defined schedule. This example would run at 8 am on Tuesdays and Thursdays:

``` r
cronR::cron_add(cmd, 
                frequency = "daily",
                at = "08:00", 
                days_of_week = c(2, 4),
                id = "name_of_job")
```

For scheduling days of the week:

    0 - Sun      Sunday
    1 - Mon      Monday
    2 - Tue      Tuesday
    3 - Wed      Wednesday
    4 - Thu      Thursday
    5 - Fri      Friday
    6 - Sat      Saturday
    7 - Sun      Sunday

For complicated schedules frequency can be added as:

    ┌────────── minute (0 - 59)
    │ ┌──────── hour (0 - 23)
    │ │ ┌────── day of month (1 - 31)
    │ │ │ ┌──── month (1 - 12)
    │ │ │ │ ┌── day of week (0 - 6 => Sunday - Saturday, or
    │ │ │ │ │                1 - 7 => Monday - Sunday)
    ↓ ↓ ↓ ↓ ↓
    * * * * * command to be executed

This example would run every 3 hours on weekdays:

``` r
cronR::cron_add(command = cmd, 
                frequency = "0 */3 * * *",
                days_of_week = c(1, 2, 3, 4, 5),
                id = "name_of_job")
```

Useful commands:

``` r
#number of jobs
cronR::cron_njobs()

#more infor about running jobs
cronR::cron_ls()

#remove a job
cronR::cron_rm("name_of_job")
```

### one time set up of gmail account for `gmailR`

Taken from <a href="https://github.com/r-lib/gmailr/blob/master/README.md" target="_blank">gmailR</a>

-   Make new gmail account
-   Go to <a href="https://developers.google.com/gmail/api/quickstart/python" target="_blank">quickstart</a> to make new Google project
-   Click *Enable the Gmail API*
-   In resulting dialog click *download client configuration*, save the file as `credentials.json`
-   In your script, add:

``` r
gm_auth_configure(path = "path/to/credentials.json")
gm_auth()
```

The first time, this will take you to the browser where you will allow the project access to your gmail. It will say it is an untrusted application, but that’s because it is the one you just made. Click *advanced* to proceed. Then include the code above in your script to authorize each time without having to go through the browser steps. This will work on the RStudio server, through the guest wifi but not through the NSS network (firewall issues). Running on the server seems to be the easiest way.
