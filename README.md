# Surveillance of equine strangles in the United Kingdom between 2015 and 2019 based on laboratory detection of *Streptococcus equi*: Datasets and analysis code
[![DOI](https://zenodo.org/badge/290019559.svg)](https://zenodo.org/badge/latestdoi/290019559)

## Introduction
This repository contains datasets and code used for the manuscript as titled above. All details regarding the data source and considerations that should be noted are discussed in the manuscript; these are important to understand before embarking on any analysis of these data - in particular the differentiation between a strangles diagnosis and a positive sampling event - a single diagnosis can be made up of multiple sample events.

## R Code  
The descriptive analysis for the manuscript was performed using R. The full code is available in the *manuscriptcode.R* file. Links to the csv datasest used are included in the code as well as the required libraries. The code published here is in a format that may assist entry-level R users in navigating through the outcomes. Piping using _dplyr_ was used throughout to create a step-for-step code base.

## Datasets  
### CSV files  
-  **_dataset_diagnosesgen_all.csv_** is the foundational dataset where every row relates to a single Strangles diagnosis
    -  `strangleslogid` denotes the unique identifier for a single strangles diagnosis
    -  `eventdate` denotes the earliest date associated with each diagnosis
    -  `submittingvet_prim` denotes the primary veterinary practice submitting samples for each diagnosis
    -  `nuts3gid` denotes the NUTS3 gid identifier to link to the NUTS3 spatial polygon dataset _(see the **GIS** section below)_
    -  `breedgen` denotes the general breed of the affected horse
    -  `sexgen` denotes the sex of the affected horse
    -  `premesiscategory` denotes the premises category where the affected horse was present at diagnosis
    -  `age` denotes the age (in years) of the affected horse
    -  `submittinglab_prim` denotes the primary laboratory submitting data relating to the diagnosis
    -  `result` denotes the distinct aggregated test methods used for _S.equi_ positive results for each diagnosis  

-  **_dataset_diagnosessampletype.csv_** represents the sample types (`sampletype`) and sample locations (horse - `samplelocation`) for each diagnosis (`strangleslogid`) where multiple lines may represent a single diagnosis - i.e. multiple sample types and from multiple locations were registered per diagnosis.  

-  **_dataset_clinicalsigns.csv_** represents the clinical signs noted for each strangles diagnosis. As with the _dataset_diagnosessampletype_ dataset, multiple clinical signs (`cx`) are associated with a distinct diagnosis (`strangleslogid`).

-  **_dataset_samplereason.csv_** represents the reason for sampling (`samplereason`) with multiple rows constituting a strangles diagnosis (`strangleslogid`).

-  **_dataset_sampledecisions_type.csv_** represents the aggregated reason for sampling (`samplereasons2`) with a single rows constituting a strangles diagnosis (`strangleslogid`), and in this case diagnoses are ommitted where *undefined* was the only associated reason for sampling.
-  **_dataset_sampletestoutcome.csv_** 
-  **_dataset_nuts3_base.csv_** 


### GIS (Spatial) datasets
