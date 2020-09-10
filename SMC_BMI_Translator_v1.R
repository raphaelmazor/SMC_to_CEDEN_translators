library(tidyverse)


# loading csci and bmi data

load("Data/bmi_csci.Rdata")
# list(lu_station.df, bmi_tax_results.df, csci_core.df, csci_suppl1_grps.df, csci_suppl1_mmi.df, csci_suppl1_oe.df)
lu_station.df<-bmi_csci[[1]]
bmi_tax_results.df<-bmi_csci[[2]]
bmi_tax_sampleinfo.df<-bmi_csci[[3]]
csci_core.df<-bmi_csci[[4]]
csci_suppl1_grps.df<-bmi_csci[[5]]
csci_suppl1_mmi.df<-bmi_csci[[6]]
csci_suppl1_oe.df<-bmi_csci[[7]]

# getting phab data for sample information

library(DBI) # needed to connect to database
library(dbplyr) # needed to connect to database
library(RPostgreSQL) # needed to connect to our database
library(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
library(tidyverse)
library(lubridate)
library(readxl)

# con is short for connection
# Create connection to the database
con <- dbConnect(
  PostgreSQL(),
  host = "192.168.1.17",
  dbname = 'smc',
  user = 'smcread',
  password = '1969$Harbor' # if we post to github, we might want to do rstudioapi::askForPassword()
)


# this is info from unified phab... should I be looking at raw phab (tbl_phab?)
phab_query <- "select * from sde.unified_phab"
tbl_phab <- tbl(con, sql(phab_query))
phab.1 <- as.data.frame(tbl_phab)
phab_stationinfo <- phab.1 %>% 
  select(stationcode, actual_latitude, actual_longitude,sampleagencycode) %>% 
  distinct(stationcode, .keep_all = TRUE)
 





CEDEN_benthic_locations<-bmi_tax_sampleinfo.df %>%
  # filter() %>%
  inner_join(lu_station.df %>%
               select(stationcode=stationid, latitude,longitude)) %>% #to get target latitude and longitude
  left_join(phab_stationinfo) %>% #to get actual latitude and longitude, and sampleagencycode
  transmute(StationCode=stationcode,
            SampleDate=sampledate,
            # ProjectCode=paste0("SMC_",login_owner, login_year))
            ProjectCode="SMC", #Maybe one project for all?)
            EventCode="BA",
            ProtocolCode="SWAMP_2016_WS",
            AgencyCode="SCCWRP",
            SampleComments="",
            LocationCode="Thalweg",
            GeometryShape="Point",
            CoordinateNumber=1,
            ActualLatitude=actual_latitude, #use actual latitude from phab? not as complete as latitude from lu stations, but that's target?
            ActualLongitude=actual_longitude,  #use actual latitude from phab? not as complete as latitude
            Datum="WGS84",
            CoordinateSource="GPS", #If we find the actuals...
            Elevation="",
            UnitElevation="",
            StationDetailVerBy="",
            StationDetailVerDate="",
            StationDetailComments=""
  )


CEDEN_benthic_benthicresults<-bmi_tax_sampleinfo.df %>%
  # filter() %>%
  inner_join(bmi_tax_results.df,
             by=c("stationcode","sampledate","fieldreplicate","fieldsampleid")) %>%
  # inner_join(lu_station.df %>%               select(stationcode=stationid, latitude,longitude) )%>%
  transmute(
    StationCode=stationcode,
    SampleDate=sampledate,
    ProjectCode="SMC",
    EventCode="BA",
    ProtocolCode="SWAMP_2016_WS",
    AgencyCode=agencycode_labeffort.y, #Really should be from phab
    SampleComments="",
    LocationCode="X",
    GeometryShape="Point",
    CollectionTime="00:00",
    CollectionMethodCode=collectionmethodcode,
    SampleTypeCode="Integrated",
    Replicate=fieldreplicate,
    CollectionDeviceName="D-Frame Kick Net",
    CollectionDepth=-88,
    UnitCollectionDepth="m",
    SieveSize="0.5mm",
    SampleID="fieldsampleid",
    BenthicCollectionComments=benthiccollectioncomments,
    GrabSize=.99,
    UnitGrabSize="m2",
    ReplicateName=replicatename,
    ReplicateCollectionDate="",
    NumberJars=numberjars,
    BenthicCollectionDetailComments=benthiccollectiondetailcomments,
    AgencyCode_LabEffort=agencycode_labeffort.y,
    PersonnelCode_LabEffort=personnelcode_labeffort,
    PercentSampleCounted=percentsamplecounted,
    TotalGrids=totalgrids,
    GridsAnalyzed=gridsanalyzed,
    GridsVolumeAnalyzed=gridsvolumeanalyzed,
    TargetOrganismCount=targetorganismcount,
    ActualOrganismCount=actualorganismcount,
    ExtraOrganismCount=extraorganismcount,
    QCOrganismCount=qcorganismcount,
    DiscardedOrganismCount=discardedorganismcount,
    EffortQACode=qacode.x,
    BenthicLabEffortComments=benthiclabeffortcomments,
    FinalID=finalid,
    LifeStageCode=lifestagecode,
    Distinct=distinctcode,
    BAResult=baresult,
    Result=result,
    UnitName=unit,
    ResQualCode=resqualcode,
    QACode=qacode.y,
    ComplianceCode="",
    BatchVerificationCode="",
    TaxonomicQualifier=taxonomicqualifier,
    ExcludedTaxa=excludedtaxa,
    PersonnelCode_Result=personnelcode_results,
    LabSampleID=labsampleid,
    EnterDate="",
    BenthicResultComments=benthicresultscomments
  )




#CEDEN habitat templates

#Many sample information fields require manual filling. These are left as blank ("") for manual entry in Excel.

# this function contains the same steps for CEDEN_habitat_csci_part1, but was continued to include addition sample information 
# some updates may still need to be made to the sample information fields, including additions from the BMI dataset and PHAB

# running function with input data (load below CEDENify_CSCI function below before running with this input data)
# output is the "xdf" in the function, which is assigned below to CEDEN_habitat_csci
CEDEN_habitat_csci <- CEDENify_CSCI(core=csci_core.df, Suppl1_mmi=csci_suppl1_mmi.df,Suppl1_grps = csci_suppl1_grps.df,
                                   Suppl1_OE = csci_suppl1_oe.df)



CEDENify_CSCI<-function(core= my_csci_core.df, Suppl1_mmi= my_csci_suppl1_mmi.df,Suppl1_grps = my_csci_suppl1_grps.df,
                        Suppl1_OE = my_csci_suppl1_oe.df){
  
  # making csci_suppl1_oe wide format to join with csci core, mmi, grps, then pivot long after
  # this will give us sampledate, collectionmethodcode, and field replicate information from csci_core for all csci data
  csci_suppl1_oe_wide.df <- Suppl1_OE %>% 
    select(sampleid, otu, captureprob) %>% 
    mutate(captureprob = if_else(condition = is.na(captureprob), true = 0, false = captureprob)) %>%  #all NA data should be 0 
    filter(otu != "Unambiguous_NotAtRefCal") %>% #removing 
    mutate(otu=paste0("Pc_",otu)) %>% 
    pivot_wider(names_from = "otu", values_from = "captureprob", values_fill = 0) %>%  #make sure any NAs are 0 
    rename(SampleID = sampleid)
  
  
  #Core, Suppl1_mmi, Suppl1_grps, oe
  all_csci<-core %>%
    filter(record_origin=="SMC") %>%
    select(sampleid, stationcode, sampledate, collectionmethodcode, fieldreplicate, count, number_of_mmi_iterations,
           number_of_oe_iterations, pcnt_ambiguous_individuals, pcnt_ambiguous_taxa,
           e, mean_o, oovere, mmi, csci, csci_percentile) %>% 
    left_join(Suppl1_mmi %>% select(sampleid, clinger_percenttaxa, clinger_percenttaxa_predicted, clinger_percenttaxa_score, coleoptera_percenttaxa,
                                 coleoptera_percenttaxa_predict, coleoptera_percenttaxa_score, taxonomic_richness, taxonomic_richness_predicted,
                                 taxonomic_richness_score, ept_percenttaxa, ept_percenttaxa_predicted, ept_percenttaxa_score, shredder_taxa,
                                 shredder_taxa_predicted, shredder_taxa_score, intolerant_percent, intolerant_percent_predicted, intolerant_percent_score), 
              by="sampleid") %>%
    left_join(Suppl1_grps%>% select(stationcode, pgroup1, pgroup2, pgroup3, pgroup4, pgroup5, pgroup6, pgroup7, pgroup8, pgroup9, pgroup10,pgroup11), 
              by="stationcode") %>%
    # select(-MMI_Percentile, -OoverE_Percentile, -MMI_Score) %>%
    # names() %>%dput()
    rename(SampleID=tolower("SampleID"),StationCode=tolower("StationCode"), SampleDate=tolower("SampleDate"),CollectionMethodCode=tolower("CollectionMethodCode"),
           FieldReplicate=tolower("FieldReplicate"),Count=tolower("Count"), Number_of_MMI_Iterations=tolower("Number_of_MMI_Iterations"),Number_of_OE_Iterations=tolower("Number_of_OE_Iterations"), 
           Pcnt_Ambiguous_Individuals=tolower("Pcnt_Ambiguous_Individuals"),Pcnt_Ambiguous_Taxa=tolower("Pcnt_Ambiguous_Taxa"),E="e",Mean_O=tolower("Mean_O"),OoverE=tolower("OoverE"),MMI = "mmi",CSCI="csci",CSCI_Percentile=tolower("CSCI_Percentile"),
           Clinger_PercentTaxa=tolower("Clinger_PercentTaxa"), Clinger_PercentTaxa_predicted=tolower("Clinger_PercentTaxa_predicted"), Clinger_PercentTaxa_score=tolower("Clinger_PercentTaxa_score"), 
           Coleoptera_PercentTaxa=tolower("Coleoptera_PercentTaxa"), Coleoptera_PercentTaxa_predicted=tolower("Coleoptera_PercentTaxa_predict"), Coleoptera_PercentTaxa_score=tolower("Coleoptera_PercentTaxa_score"), 
           Taxonomic_Richness=tolower("Taxonomic_Richness"), Taxonomic_Richness_predicted=tolower("Taxonomic_Richness_predicted"), Taxonomic_Richness_score=tolower("Taxonomic_Richness_score"), 
           EPT_PercentTaxa=tolower("EPT_PercentTaxa"), EPT_PercentTaxa_predicted=tolower("EPT_PercentTaxa_predicted"), EPT_PercentTaxa_score=tolower("EPT_PercentTaxa_score"), 
           Shredder_Taxa=tolower("Shredder_Taxa"), Shredder_Taxa_predicted=tolower("Shredder_Taxa_predicted"), Shredder_Taxa_score=tolower("Shredder_Taxa_score"), 
           Intolerant_Percent=tolower("Intolerant_Percent"), Intolerant_Percent_predicted=tolower("Intolerant_Percent_predicted"), Intolerant_Percent_score=tolower("Intolerant_Percent_score"),
           pGroup1 = pgroup1, pGroup2 = pgroup2, pGroup3 = pgroup3, pGroup4 = pgroup4, pGroup5 = pgroup5, pGroup6 = pgroup6,
           pGroup7 = pgroup7, pGroup8 = pgroup8, pGroup9 = pgroup9, pGroup10 = pgroup10, pGroup11 = pgroup11
           ) %>% 
    inner_join(csci_suppl1_oe_wide.df, by = "SampleID") %>% #how I decided to join oe data, instead of dealing with it separately 
    pivot_longer(cols = c(-SampleID, -StationCode, -SampleDate,-CollectionMethodCode,-FieldReplicate),
                 names_to = "AnalyteName",
                 values_to = "Result") %>% 
    # note that I can designate which columns not to pivot wide above, instead of listing all the columns
    mutate(AnalyteName = case_when(AnalyteName=="CSCI_Percentile"~"CSCI_Percentile", #The only variable that doesn't follow this naming convention
                                   AnalyteName=="CSCI"~"CSCI", #The only variable that doesn't follow this naming convention
                                   T~paste0("CSCI_", AnalyteName)))
 
  #add other SWAMP fields, and re-order
  # still need to get some information from phab and perhaps bmi
  
  xdf<-all_csci %>% 
    mutate(SampleDate=SampleDate,#from csci core
           ProjectCode="SMC",
           EventCode="BA",
           ProtocolCode="SWAMP_2016_WS", #always "SWAMP_2016_WS"?
           AgencyCode="SCCWRP", #or get from bmi_tax_sampleinfo?? phab??... is this sampleagencycode?
           SampleComments="",
           LocationCode="X",
           GeometryShape="Point",
           CollectionTime="00:00",
           CollectionMethodCode=CollectionMethodCode, #from csci core, should be same info as in bmi_tax_sampleinfo
           Replicate=FieldReplicate, #from csci core
           HabitatCollectionComments="", #not sure where to find this information 
           MatrixName="benthic",
           MethodName = "CSCI_software_v1.x",
           # MethodName=paste0("CSCI_software_v",strsplit(packageVersion("CSCI") %>% as.character(),split=".")[[1]][1],".x"),
           FractionName="None",
           UnitName="none",
           VariableResult="",
           ResQualCode="=",
           QACode="None", #get from bmi_tax_sampleinfo?? leave as none for now
           ComplianceCode="Pend",
           BatchVerificationCode="NR",
           CollectionDeviceName="D-Frame Kick Net", #Or do we want to leave blank?
           HabitatResultComments="" #not sure where to find this information
           
    ) %>%
    select(SampleID, StationCode, SampleDate, 
           ProjectCode, EventCode, ProtocolCode, AgencyCode, SampleComments, 
           LocationCode, GeometryShape, CollectionTime, CollectionMethodCode, 
           Replicate, HabitatCollectionComments, MatrixName, MethodName, AnalyteName,
           FractionName, UnitName, VariableResult, Result, ResQualCode, 
           QACode, ComplianceCode, BatchVerificationCode, CollectionDeviceName, 
           HabitatResultComments)%>%
    mutate(UnitName = case_when(AnalyteName %in% c("CSCI_Pcnt_Ambiguous_Individuals","CSCI_Pcnt_Ambiguous_Taxa","CSCI_Clinger_PercentTaxa",
                                                   "CSCI_Clinger_PercentTaxa_predicted","CSCI_Coleoptera_PercentTaxa","CSCI_Coleoptera_PercentTaxa_predicted",
                                                   "CSCI_EPT_PercentTaxa","CSCI_EPT_PercentTaxa_predicted","CSCI_Intolerant_Percent",
                                                   "CSCI_Intolerant_Percent_predicted")~"%",
                                AnalyteName %in% c("CSCI_Count","CSCI_Number_of_MMI_Iterations","CSCI_Number_of_OE_Iterations",
                                                   "CSCI_Taxonomic_Richness","CSCI_Taxonomic_Richness_predicted",
                                                   "CSCI_Shredder_Taxa","CSCI_Shredder_Taxa_predicted","CSCI_Mean_O")~"count",
                                AnalyteName %in% c("CSCI_OoverE","CSCI_MMI","CSCI",
                                                   "CSCI_Clinger_PercentTaxa_score","CSCI_Coleoptera_PercentTaxa_score","CSCI_Taxonomic_Richness_score",
                                                   "CSCI_EPT_PercentTaxa_score","CSCI_Shredder_Taxa_score","CSCI_Intolerant_Percent_score")~"score",
                                AnalyteName %in% c("CSCI_Percentile","CSCI_E")~"none",
                                grepl("CSCI_Pc_",AnalyteName)~"none",
                                grepl("CSCI_pGroup",AnalyteName)~"none",
                                T~"error"))
  
  # making sure function returns only xdf, not intermediates
  return(xdf)
}







# CEDEN habitat templates, part1 below merges all csci tables but doesn't add sample information like in function above
# this is mostly for troubleshooting table merging before finalizing in function

CEDEN_habitat_csci_part1<-
  csci_core.df %>%
  filter(record_origin=="SMC") %>%
  select(sampleid, stationcode, sampledate, collectionmethodcode, fieldreplicate, count, number_of_mmi_iterations,
         number_of_oe_iterations, pcnt_ambiguous_individuals, pcnt_ambiguous_taxa,
         e, mean_o, oovere, mmi, csci, csci_percentile) %>% 
  inner_join(csci_suppl1_mmi.df %>% select(sampleid, clinger_percenttaxa, clinger_percenttaxa_predicted, clinger_percenttaxa_score, coleoptera_percenttaxa, coleoptera_percenttaxa_predict, coleoptera_percenttaxa_score, taxonomic_richness, taxonomic_richness_predicted, taxonomic_richness_score, ept_percenttaxa, ept_percenttaxa_predicted, ept_percenttaxa_score, shredder_taxa, shredder_taxa_predicted, shredder_taxa_score, intolerant_percent, intolerant_percent_predicted, intolerant_percent_score), 
             by="sampleid") %>%
  inner_join(csci_suppl1_grps.df %>% select(stationcode, pgroup1, pgroup2, pgroup3, pgroup4, pgroup5, pgroup6, pgroup7, pgroup8, pgroup9, pgroup10,pgroup11), 
             by="stationcode") %>% 
  # select(-mmi_percentile, -oovere_percentile, -mmi_score) %>% 
  rename(SampleID=tolower("SampleID"),StationCode=tolower("StationCode"), SampleDate=tolower("SampleDate"),CollectionMethodCode=tolower("CollectionMethodCode"),
         FieldReplicate=tolower("FieldReplicate"),Count=tolower("Count"), Number_of_MMI_Iterations=tolower("Number_of_MMI_Iterations"),Number_of_OE_Iterations=tolower("Number_of_OE_Iterations"), 
         Pcnt_Ambiguous_Individuals=tolower("Pcnt_Ambiguous_Individuals"),Pcnt_Ambiguous_Taxa=tolower("Pcnt_Ambiguous_Taxa"),E="e",Mean_O=tolower("Mean_O"),OoverE=tolower("OoverE"),MMI = "mmi",CSCI="csci",CSCI_Percentile=tolower("CSCI_Percentile"),
         Clinger_PercentTaxa=tolower("Clinger_PercentTaxa"), Clinger_PercentTaxa_predicted=tolower("Clinger_PercentTaxa_predicted"), Clinger_PercentTaxa_score=tolower("Clinger_PercentTaxa_score"), 
         Coleoptera_PercentTaxa=tolower("Coleoptera_PercentTaxa"), Coleoptera_PercentTaxa_predicted=tolower("Coleoptera_PercentTaxa_predict"), Coleoptera_PercentTaxa_score=tolower("Coleoptera_PercentTaxa_score"), 
         Taxonomic_Richness=tolower("Taxonomic_Richness"), Taxonomic_Richness_predicted=tolower("Taxonomic_Richness_predicted"), Taxonomic_Richness_score=tolower("Taxonomic_Richness_score"), 
         EPT_PercentTaxa=tolower("EPT_PercentTaxa"), EPT_PercentTaxa_predicted=tolower("EPT_PercentTaxa_predicted"), EPT_PercentTaxa_score=tolower("EPT_PercentTaxa_score"), 
         Shredder_Taxa=tolower("Shredder_Taxa"), Shredder_Taxa_predicted=tolower("Shredder_Taxa_predicted"), Shredder_Taxa_score=tolower("Shredder_Taxa_score"), 
         Intolerant_Percent=tolower("Intolerant_Percent"), Intolerant_Percent_predicted=tolower("Intolerant_Percent_predicted"), Intolerant_Percent_score=tolower("Intolerant_Percent_score"),
         
         
  ) %>% 
  rename(pGroup1 = pgroup1, pGroup2 = pgroup2, pGroup3 = pgroup3, pGroup4 = pgroup4, pGroup5 = pgroup5, pGroup6 = pgroup6,
         pGroup7 = pgroup7, pGroup8 = pgroup8, pGroup9 = pgroup9, pGroup10 = pgroup10, pGroup11 = pgroup11) %>% 
  inner_join(csci_suppl1_oe_wide.df, by = "SampleID") %>% #how I decided to join oe data, instead of dealing with it separately 
  
  pivot_longer(cols = c(-SampleID, -StationCode, -SampleDate,-CollectionMethodCode,-FieldReplicate),
               names_to = "AnalyteName",
               values_to = "Result") %>% 
  # previous version of pivot longer below, no longer using this because also added oe data before the pivot
  # note that I can designate which columns not to pivot wide above, instead of listing all the columns like below method
  
  # pivot_longer(cols = c(
  #   #From core report
  #   Count,Number_of_MMI_Iterations,Number_of_OE_Iterations, Pcnt_Ambiguous_Individuals,Pcnt_Ambiguous_Taxa,
  #   E,Mean_O,OoverE,CSCI,CSCI_Percentile,
  #   #From Suppl1_mmi
  #   Clinger_PercentTaxa, Clinger_PercentTaxa_predicted, Clinger_PercentTaxa_score, 
  #   Coleoptera_PercentTaxa, Coleoptera_PercentTaxa_predicted, Coleoptera_PercentTaxa_score, 
  #   Taxonomic_Richness, Taxonomic_Richness_predicted, Taxonomic_Richness_score, 
#   EPT_PercentTaxa, EPT_PercentTaxa_predicted, EPT_PercentTaxa_score, 
#   Shredder_Taxa, Shredder_Taxa_predicted, Shredder_Taxa_score, 
#   Intolerant_Percent, Intolerant_Percent_predicted, Intolerant_Percent_score,
#   #From Suppl1_grps
#   pGroup1, pGroup2, pGroup3, pGroup4, pGroup5, pGroup6, pGroup7, pGroup8, pGroup9, pGroup10, pGroup11),
#   names_to = "AnalyteName",
#   values_to = "Result") %>% 

mutate(AnalyteName = case_when(AnalyteName=="CSCI_Percentile"~"CSCI_Percentile", #The only variable that doesn't follow this naming convention
                               AnalyteName=="CSCI"~"CSCI", #The only variable that doesn't follow this naming convention
                               T~paste0("CSCI_", AnalyteName)))












StationCode	SampleDate	ProjectCode	EventCode	ProtocolCode	AgencyCode	SampleComments	LocationCode	GeometryShape	CollectionTime	CollectionMethodCode	Replicate	HabitatCollectionComments	MatrixName	MethodName	AnalyteName	FractionName	UnitName	VariableResult	Result	ResQualCode	QACode	ComplianceCode	BatchVerificationCode	CollectionDeviceName	HabitatResultComments



head(bmi_tax_sampleinfo.df)
head(chem_results.df)

