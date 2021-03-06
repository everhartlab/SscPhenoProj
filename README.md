# _Sclerotinia sclerotiorum_ Phenotyping Project

> Explore this analysis online: [![Binder](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/everhartlab/SscPhenoProj/master?urlpath=rstudio)

This project contains the data and code used for analysis in our preprint:

> Miorini TJJ, Kamvar ZN, Higgins R, Raetano CG, Steadman JR, Everhart SE. (2018) 
> Variation in pathogen aggression and cultivar performance against _Sclerotinia 
> sclerotiorum_ in soybean and dry bean from Brazil and the U.S. *PeerJ Preprints* 
> **6**:e26622v1 doi: [10.7287/peerj.preprints.26622v1](https://doi.org/10.7287/peerj.preprints.26622v1)

If you use any of the scripts or data, please cite the project on the Open Science Framework:

> Miorini TJJ, Kamvar ZN, Higgins R, Raetano CG, Steadman JR, Everhart SE (2018) 
> Data and analysis for Variation in pathogen aggression and cultivar performance 
> against *Sclerotinia sclerotiorum* in soybean and dry bean from the U.S. and Brazil. 
> doi: [10.17605/OSF.IO/2X7FC](https://doi.org/10.17605/OSF.IO/2X7FC).

# Analysis

## Overview

The analysis of variance calculations and plots are generated within the R
script [`Analysis.R`](Analysis.R). This script makes a direct conversion between
the Microsoft Excel file and csv files stored in the `clean_data` folder and
creates the [Analsysis.Rout](Analysis.Rout) file.

## Running the Analyses

#### On your computer

All package versions are locked to 2018-02-23 with the
[checkpoint](https://CRAN.R-project.org/package=checkpoint) package, which will
automatically install all of the required packages locally before the analysis
is run. It is assumed you have R version 3.4.3. To run this analysis, you can
open it in RStudio and click "source", or, from the command line, you can use:

```sh
$ R -f Analysis.R &> Analysis.Rout
```


#### On the cloud

Alternatively, you can launch an interactive version of this analysis on
https://mybinder.org by clicking on this button: [![Binder](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/everhartlab/SscPhenoProj/master?urlpath=rstudio)

This will launch and RStudio instance on https://mybinder.org with all the
necessary packages installed. From there, you can open `Analysis.R` and run
the analysis. Please note, however, that this integration is in Beta and may
change in the future. Be aware that it may take some time to build the image
before you can inspect the data and analyses.

# Data

The data within this repository exist across nine sheets within a Microsoft
Excel file. Below, we will give details about the data contained within these
sheets. We are including data in this repository that was collected during the
project, but not used in this publication.

All the raw data are stored in the top level of this repository. Processed data
are placed in the folder called `clean_data`. The data are cleaned in the first
part of the R script and track changes should confirm that no data were changed
in the cleaning process.

## Mensure and score in different days_straw test.csv

This sheet contains straw test scores for 3, 6, and 8 days after inoculation
for the detached leaf bioassay of dry bean cultivar G122. The area under the
mycelial progress curve was calculated from these data.

This is a companion file to Sheet B in `Brazilian agressiveness_raw_data-final2.xlsx`


| Column Name      | Description                                                                                 |
|:-----------------|:--------------------------------------------------------------------------------------------|
| X1 (Blank name)  | TJM isolate number                                                                          |
| Block            | Experimental Block                                                                          |
| 3 dai            | Lesion length in centimeters 3 days after inoculation                                       |
| 6 dai            | Lesion length in centimeters 6 days after inoculation                                       |
| 8 dai            | Lesion length in centimeters 8 days after inoculation                                       |
| AUDPC            | Area under the disease progress curve from evaluations at 3, 6 and 8 days after inoculation |
| After first node | lesion length in centimeters after first node                                               |

> Corresponding clean data: [B_ST_DryBean_G122.csv](clean_data/B_ST_DryBean_G122.csv)

## Brazilian agressiveness_raw_data-final2.xlsx


This file contains 10 sheets, the first being a coded summary of each subsequent
sheet, which are labeled A-I.

### Notes

 - Some isolate numbers have been abbreviated in recording the data by
eliminating the prefix "97". For example, isolates named 2A, 5B, 4D are actually
972A, 975B, and 974D.
 - Isolates with prefix 972 do not exist in the Steadman collection.
 - Missing data are formatted as `.`, `#VALUE!`, or a blank cell.


### Sheet A

> Corresponding clean data: [A_DLB_SoyBean_Dassel.csv](clean_data/A_DLB_SoyBean_Dassel.csv)

Detached Leaf Bioassay: 70 isolates vs Dassel on soybean.

 - 2100 Rows
 - 6 Columns
 - 70 isolates
 - 10 replicates
 - 3 collection times
 - 4 sections

| Column Name    | Description                                                         |
|:---------------|:--------------------------------------------------------------------|
| Number         | Row Number                                                          |
| Section        | An experimental section denoting simultaneous evaluations           |
| Isolate number | Numeric isolate number relative to this analysis                    |
| Isolate        | TJM isolate number                                                  |
| Collection     | Collection time: first=21, second=28, third=35 days after emergence |
| Area           | Lesion Area (cm^2^) 48 hours after inoculation                      |

Isolate 1\* = from isolate 1 (P). This isolate has \* because TJM took sclerotia
after he exposed this isolate in dicarboximide fungicide. It grew well in the
discriminatory dose used.


### Sheet B

> Corresponding clean data: [B_ST_DryBean_G122.csv](clean_data/B_ST_DryBean_G122.csv)

Straw test of 32 isolates on dry bean cultivar G122

 - 384 Rows
 - 6 Columns
 - 32 isolates
 - 12 replicates


| Column Name      | Description                                                                                 |
|:-----------------|:--------------------------------------------------------------------------------------------|
| Isolate_number   | Row Number                                                                                  |
| Isolate          | TJM isolate number                                                                          |
| 8 dai (cm)       | Lesion length in centimeters 8 days after inoculation                                       |
| AUDPC            | Area under the disease progress curve from evaluations at 3, 6 and 8 days after inoculation |
| After first node | lesion length in centimeters after first node                                               |
| Score            | Petzoldt & Dickson Straw Test Score (range: 1--9)                                           |

This sheet has a companion sheet called
"Mensure and score in different days_straw test.csv" that contains data for each
day after inoculation.

### Sheet C

Detached Leaf Bioassay: 29 isolates vs IAC-Alvorada

> Corresponding clean data: [C_DLB_DryBean_IAC-Alvorada.csv](clean_data/C_DLB_DryBean_IAC-Alvorada.csv)

 - 870 Rows
 - 11 Columns
 - 29 isolates
 - 25 blocks

| Column Name    | Description                                                         |
|:---------------|:--------------------------------------------------------------------|
| Section        | An experimental section denoting simultaneous evaluations           |
| Block          | Experimental block                                                  |
| Isolate_number | Experiment-specific number                                          |
| Isolate        | TJM isolate number                                                  |
| Collection     | Collection time: first=21, second=28, third=35 days after emergence |
| 24 horas       | Lesion area in square centimeters after 24 hours                    |
| 30 horas       | Lesion area in square centimeters after 30 hours                    |
| 36 horas       | Lesion area in square centimeters after 36 hours                    |
| 48 horas       | Lesion area in square centimeters after 48 hours                    |
| AUMPD          | Area Under the Mycelial Progress Curve                              |

### Sheet D

Straw test: 28 isolates vs. IAC-Alvorada Brazil

> Corresponding clean data: [D_ST_DryBean_IAC-Alvorada.csv](clean_data/D_ST_DryBean_IAC-Alvorada.csv)

 - 308 Rows
 - 4 Columns
 - 28 isolates
 - 11 replicates

| Column Name    | Description                                       |
|:---------------|:--------------------------------------------------|
| Isolate_number | Experiment-specific number                        |
| Isolate        | TJM isolate number                                |
| Rep            | Experimental replicate                            |
| Score          | Petzoldt & Dickson Straw Test Score (range: 1--9) |

### Sheet E

Detached Leaf Bioassay: Soybean cultivars

> Corresponding clean data: [E_DLB_Soybean_Cultivars.csv](clean_data/E_DLB_Soybean_Cultivars.csv)

 - 220 Rows
 - 5 columns
 - 11 varieties
 - 10 biological replicates
 - 2 experimental replicates

| Column Name | Description                                       |
|:------------|:--------------------------------------------------|
| Exp_rep     | Experimental replicate                            |
| Variety     | Numerical designator for soybean variety          |
| Name        | Soybean variety name                              |
| Rep         | Biological replicate                              |
| Area        | Lesion Area (cm^2^) 48 hours after inoculation    |

### Sheet F

Detached Leaf Bioassay: First experimental replicate for dry bean cultivars

> Corresponding clean data: [F_DLB_DryBean_Cultivars-1.csv](clean_data/F_DLB_DryBean_Cultivars-1.csv)

This experiment was to determine which isolate was to be used for the DLB 972B
or 972D. These were evaluated at different time points:

Isolate 2B: up to 48 hpi
Isolate 2D: up to 66 hpi

 - 276 Rows
 - 14 Columns
 - 23 cultivars
 - 12 replicates
 - 69 blocks

| Column Name     | Description                                                             |
|:----------------|:------------------------------------------------------------------------|
| Block           | Experimental block                                                      |
| Isolate         | TJM Isolate identifier                                                  |
| Cultivar_number | Numerical desginator for dry bean variety                               |
| Cultivar_name   | Dry bean cultivar name                                                  |
| 24              | lesion area in centimeters 24 hours post inoculation                    |
| 30              | lesion area in centimeters 30 hours post inoculation                    |
| 36              | lesion area in centimeters 36 hours post inoculation                    |
| 42              | lesion area in centimeters 42 hours post inoculation                    |
| 48              | lesion area in centimeters 48 hours post inoculation                    |
| 54              | lesion area in centimeters 54 hours post inoculation                    |
| 60              | lesion area in centimeters 60 hours post inoculation                    |
| 66              | lesion area in centimeters 66 hours post inoculation                    |
| AUMPC (48)      | Area under the mycelial progress curve up to 48 hours post inoculation. |
| AUMPC (66h)     | Area under the mycelial progress curve up to 66 hours post inoculation. |

### Sheet G

Detached Leaf Bioassay: Second experimental replicate for dry bean cultivars with isolate 972B

> Corresponding clean data: [G_DLB_DryBean_Cultivars-2.csv](clean_data/G_DLB_DryBean_Cultivars-2.csv)

 - 276 Rows
 - 9 Columns
 - 23 cultivars
 - 56 blocks
 - 12 replicates

| Column Name     | Description                                          |
|:----------------|:-----------------------------------------------------|
| Block           | Experimental block                                   |
| Cultivar_number | Numerical designator for dry bean variety            |
| Cultivar_name   | Dry bean cultivar name                               |
| 24              | lesion area in centimeters 24 hours post inoculation |
| 30              | lesion area in centimeters 30 hours post inoculation |
| 36              | lesion area in centimeters 36 hours post inoculation |
| 42              | lesion area in centimeters 42 hours post inoculation |
| 48              | lesion area in centimeters 48 hours post inoculation |
| AUMPC           | Area under the mycelial progress curve               |

### Sheet H

Straw Test: First experimental replicate for dry bean cultivars

> Corresponding clean data: [H_ST_DryBean_Cultivars-1.csv](clean_data/H_ST_DryBean_Cultivars-1.csv)

This experiment was to determine which isolate was to be used for the Straw
Test: 972B or 972D.

Note: Because 972B was very aggressive, TJJM had additionally scored some of the
isolates higher than 9 according to a Modified Petzoldt and Dickson Scale. 
However, when reporting the results of these data, numbers higher than 9 were
converted to 9.

 - 322 Rows
 - 5 Columns
 - 23 cultivars
 - 7 replicates
 - 2 isolates
 - 13 blocks

| Column Name | Description                                       |
|:------------|:--------------------------------------------------|
| Number      | Numerical designator for dry bean cultivar        |
| Cultivar    | Dry bean cultivar name                            |
| Rep         | Experimental replicate                            |
| Isolate     | TJM Isolate identifier                            |
| Score       | Petzoldt & Dickson Straw Test Score (range: 1--9) |

### Sheet I

Straw Test: Second experimental replicate for dry bean cultivars with isolate 972D

> Corresponding clean data: [I_ST_DryBean_Cultivars-2.csv](clean_data/I_ST_DryBean_Cultivars-2.csv)

 - 285 Rows
 - 4 columns
 - 23 cultivars
 - 15 replicates

| Column Name | Description                                       |
|:------------|:--------------------------------------------------|
| Number      | Numerical designator for dry bean cultivar        |
| Cultivar    | Dry bean cultivar name                            |
| Rep         | Experimental replicate                            |
| Score       | Petzoldt & Dickson Straw Test Score (range: 1--9) |
