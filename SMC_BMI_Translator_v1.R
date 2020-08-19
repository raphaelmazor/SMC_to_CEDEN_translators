library(tidyverse)

load("Data/bmi_csci.Rdata")
# list(lu_station.df, bmi_tax_results.df, csci_core.df, csci_suppl1_grps.df, csci_suppl1_mmi.df, csci_suppl1_oe.df)
lu_station.df<-bmi_csci[[1]]
bmi_tax_results.df<-bmi_csci[[2]]
bmi_tax_sampleinfo.df<-bmi_csci[[3]]
csci_core.df<-bmi_csci[[4]]
csci_suppl1_grps.df<-bmi_csci[[5]]
csci_suppl1_mmi.df<-bmi_csci[[6]]
csci_suppl1_oe.df<-bmi_csci[[7]]


CEDEN_benthic_locations<-bmi_tax_sampleinfo.df %>%
  # filter() %>%
  inner_join(lu_station.df %>%
              select(stationcode=stationid, latitude,longitude) )%>%
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
            ActualLatitude=latitude, #This is target. Where is actual?
            ActualLongitude=longitude, #This is target. Where is actual?
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
  







head(bmi_tax_sampleinfo.df)
head(chem_results.df)