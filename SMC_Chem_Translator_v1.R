library(tidyverse)

load("Data/SMC_Data_Download_081820.Rdata")
head(chem_batch.df)
head(chem_results.df)

chem_results.df %>% 
  select(analytename) %>%
  unique() %>%
  arrange(analytename)

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
  # filter() %>% #Eventually add code to filter out entries we don't want to deal withleft_join(chem_batch.df %>%
  inner_join(chem_batch.df %>%
              select(labbatch,preparationpreservationname,preparationpreservationdate,digestextractmethod)) %>%
    transmute(StationCode=stationcode,
             SampleDate=sampledate,
             # ProjectCode=paste0("SMC_",login_owner, login_year),
            ProjectCode="SMC", #Maybe one project for all?
            EventCode="BA",
            ProtocolCode="SWAMP_2016_WS",
            AgencyCode=labagencycode,
            SampleComments=labresultcomments,
            LocationCode="Bank",
            GeometryShape="Point",
            CollectionTime="00:00", #Value in guidance for unknown
            CollectionMethodCode=case_when(matrixname %in% c("samplewater","samplewater, <1.2 um")~"Water_Grab",
                                           matrixname %in% c("sediment")~"Sed_Grab",
                                           matrixname %in% c("labwater","blankwater","blankmatrix")~"Not Applicable",
                                           matrixname %in% c("benthic")~"Algae_SWAMP",
                                           T~"FLAG"), #Probably "Not Applicable"
            SampleTypeCode=sampletypecode,
            Replicate=fieldreplicate,
            CollectionDeviceName="Not Recorded",
            CollectionDepth= -88,
            UnitCollectionDepth="m",
            PositionWaterColumn="",
            LabCollectionComments="",
            LabBatch=labbatch,
            AnalysisDate=analysisdate,
            MatrixName=matrixname,
            MethodName=methodname,
            AnalyteName=analytename,
            FractionName=fractionname,
            UnitName=unit,
            LabReplicate=labreplicate,
            Result=result,
            ResQualCode=resqualcode,
            MDL=mdl,	
            RL=rl,
            QACode=qacode,
            ComplianceCode="",
            DilutionFactor=dilfactor,
            ExpectedValue=expectedvalue,
            PrepPreservationName=preparationpreservationname,
            PrepPreservationDate=preparationpreservationdate,
            DigestExtractMethod=digestextractmethod,
            DigestExtractDate="01/Jan/1950 00:00",
            SampleID="",
            LabSampleID=labsampleid,
            LabResultComments=labresultcomments
          )
            		
             
