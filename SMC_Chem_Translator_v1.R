library(tidyverse)

load("Data/SMC_Data_Download_081820.Rdata")
head(chem_batch.df)
head(chem_results.df)

CEDEN_chem_labbatch<-chem_batch.df %>%
  # filter() %>% #Eventually add code to filter out entries we don't want to deal with
  transmute(LabBatch=labbatch,
            LabAgencyCode=labagencycode,
            LabSubmissionCode="",
            BatchVerificationCode="",
            SubmittingAgencyCode="",
            LabBatchComments=""
            )

CEDEN_chem_chemresults<-chem_results.df %>%
  # filter() %>% #Eventually add code to filter out entries we don't want to deal with
  transmute(StationCode=stationcode,
             SampleDate=sampledate,
             ProjectCode=paste0("SMC_",login_owner, login_year)
             )