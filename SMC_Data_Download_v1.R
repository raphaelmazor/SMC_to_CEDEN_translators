library(tidyverse)
library(DBI) # needed to connect to data.dfbase
library(dbplyr) # needed to connect to data.dfbase
library(RPostgreSQL) # needed to connect to our data.dfbase
library(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
library(tidyverse)

####Create the connection####

# con is short for connection
# Create connection to the data.dfbase
con <- dbConnect(
  PostgreSQL(),
  host = "192.168.1.17",
  dbname = 'smc',
  user = 'smcread',
  password = '1969$Harbor' # if we post to github, we might want to do rstudioapi::askForPassword()
)

####Create the SMC tables####

#We don't want/need "unified" tables. Those are incomplete wrt the data CEDEN needs.
chem_batch.df<-dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.tbl_chemistrybatch
                            ') 
chem_results.df<-dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.tbl_chemistryresults
                            ') 

tox_batch.df<-dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.tbl_toxicitybatch
                            ') 
tox_results.df<-dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.tbl_toxicityresults
                            ') 
tox_summary.df<-dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.tbl_toxicitysummary
                            ') 


###
#We need handle updated data
#1. Identify records that have been updated in sde.tbl_chemistryresults
#2. Download the relevant records from sde.unified_chemistry
#3. Overwrite the data in chem_results.df with the updated data
# chem_unified.df<-dbGetQuery(con, ' 
#                       SELECT
#                 sde.unified_chemistry.stationcode, 
#                 sde.unified_chemistry.sampledate, 
#                 sde.unified_chemistry.last_edited_user, 
#                 sde.unified_chemistry.last_edited_date, 
#                 sde.unified_chemistry.login_email, 
#                 sde.unified_chemistry.login_agency, 
#                 sde.unified_chemistry.login_owner, 
#                 sde.unified_chemistry.login_year, 
#                 sde.unified_chemistry.login_project, 
#                 sde.unified_chemistry.created_date, 
#                 sde.unified_chemistry.created_user, 
#                 sde.unified_chemistry.submissionid, 
#                 sde.unified_chemistry.objectid, 
#                 sde.unified_chemistry.sampletypecode, 
#                 sde.unified_chemistry.matrixname, 
#                 sde.unified_chemistry.fieldreplicate, 
#                 sde.unified_chemistry.labreplicate, 
#                 sde.unified_chemistry.methodname, 
#                 sde.unified_chemistry.analytename, 
#                 sde.unified_chemistry.fractionname, 
#                 sde.unified_chemistry.unit, 
#                 sde.unified_chemistry."result", 
#                 sde.unified_chemistry.resqualcode, 
#                 sde.unified_chemistry.mdl, 
#                 sde.unified_chemistry.rl, 
#                 sde.unified_chemistry.dilutionfactor, 
#                 sde.unified_chemistry.qacode, 
#                 sde.unified_chemistry.labresultcomments, 
#                 sde.unified_chemistry.labagencycode, 
#                 sde.unified_chemistry.projectcode, 
#                 sde.unified_chemistry.record_origin, 
#                 sde.unified_chemistry.origin_lastupdatedate, 
#                 sde.unified_chemistry.record_publish, 
#                 sde.unified_chemistry.originalid
# FROM
#                 sde.unified_chemistry
# WHERE
#                 record_origin = "SMC" AND
#                 last_edited_date <> created_date;
#                             ') 
# 


lu_station.df <- dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.lu_stations
                            ') 

eval.df <- dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.unified_siteeval
                            ') 
evalsites<-lu_station.df <- dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.lu_evalstations
                            ') 

eval.df2<-eval.df %>%
  select(-createdate, -modifieddate, -objectid) %>%
  inner_join(evalsites                )
write.csv(eval.df2, file="Data/eval.csv", row.names = F)

save.image("Data/SMC_Data_Download_081820.Rdata")

#####


csci_core.df<- dbGetQuery(con, '
                     SELECT * FROM
                     sde.analysis_csci_core')

csci_suppl1_mmi.df<- dbGetQuery(con, '
                     SELECT * FROM
                     sde.analysis_csci_suppl1_mmi')

csci_suppl1_grps.df<- dbGetQuery(con, '
                     SELECT * FROM
                     sde.analysis_csci_suppl1_grps')

csci_suppl1_oe.df<- dbGetQuery(con, '
                     SELECT * FROM
                     sde.analysis_csci_suppl1_oe')

bmi_tax_results.df<-dbGetQuery(con, ' 
                      SELECT * FROM
                       sde.tbl_taxonomyresults
                               ') 
bmi_tax_sampleinfo.df<-dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.tbl_taxonomysampleinfo
                            ') 
bmi_tax_unified.df<-dbGetQuery(con, ' 
                      SELECT * FROM
                       sde.unified_taxonomy
                               ')
save(bmi_tax_unified.df, file="Data/bmi_tax_unified.df.csv")

bmi_csci<-list(lu_station.df, bmi_tax_results.df, bmi_tax_sampleinfo.df,csci_core.df, csci_suppl1_grps.df, csci_suppl1_mmi.df, csci_suppl1_oe.df)
save(bmi_csci, file="Data/bmi_csci.Rdata")
# load("Data/bmi_csci.Rdata")
##################################################
asci.df<- dbGetQuery(con, '
                     SELECT * FROM
                     sde.analysis_asci')

gis.df<-dbGetQuery(con, '
                     SELECT * FROM
                     sde.tblgismetrics')

phab.df<-dbGetQuery(con, '
                     SELECT * FROM
                     sde.unified_phab')

phab_ipi.df<-dbGetQuery(con, '
                     SELECT * FROM
                     sde.analysis_phab_ipi')

phab_mets.df<-dbGetQuery(con, '
                     SELECT * FROM
                     sde.analysis_phab_metrics')

phab_data<-list( phab.df, phab_ipi.df,phab_mets.df)
save(phab_data, file="Data/phab_data.Rdata")
