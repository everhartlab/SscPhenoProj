# SscPhenoProj

Project characterizing Ssc phenotypes and cultivar performance

# Data

The data within this repository exist across nine sheets within a Microsoft
Excel file. Below, we will give details about the data contained within these
sheets.

## Brazillian agressiveness_raw_data-final2.xlsx

### Notes

 - Some isolate numbers have been abbreviated in recording the data by
eliminating the prefix "97". For example, isolates named 2A, 5B, 4D are actually
972A, 975B, and 974D.
 - Isolates with prefix 972 do not exist in the Steadman collection.
 - Missing data are formatted as `.`, `#VALUE!`, or a blank cell.






### Sheet A

Detached Leaf Bioassay: 70 isolates vs Dassel on soybean.

 - 2100 Rows
 - 6 Columns
 - 70 isolates
 - 10 replicates
 - 3 collection times
 - 4 sections?

| Column Name    | Description                                                         |
|:---------------|:--------------------------------------------------------------------|
| Number         | Row Number                                                          |
| Section        | An experimental section denoting simultaneous evaluations           |
| Isolate number | Numeric isolate number relative to this analysis                    |
| Isolate        | TJM isolate number                                                  |
| Collection     | Collection time: first=21, second=28, third=35 days after emergence |
| Area           | Lesion Area (cm^2^)                                                 |

Isolate 1\* = from isolate 1 (P). This isolate has \* because TJM took sclerotia
after he exposed this isolate in dicarboximide fungicide. It grew well in the
discriminatory dose used.


### Sheet B

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
| 24 horas       | Lesion area in centimeters after 24 hours                           |
| 30 horas       | Lesion area in centimeters after 30 hours                           |
| 36 horas       | Lesion area in centimeters after 36 hours                           |
| 48 horas       | Lesion area in centimeters after 48 hours                           |
| AUMPD          | Area Under the Mycelial Progress Curve                              |

### Sheet D

Straw test: 28 isolates vs. IAC-Alvorada Brazil

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

 - 220 Rows
 - 5 columns
 - 11 varieties
 - 10 biological replicates
 - 2 experimental replicates

| Column Name | Description                                       |
|:------------|:--------------------------------------------------|
| Exp_rep     | Experimental replicate                            |
| Variety     | Numerical designator for soybean variety          |
| Rep         | Biological replicate                              |
| Score       | Petzoldt & Dickson Straw Test Score (range: 1--9) |
| Area        | Lesion Area (cm^2^)                               |

### Sheet F

Detached Leaf Bioassay: First experimental replicate for dry bean cultivars

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

This experiment was to determine which isolate was to be used for the Straw
Test: 972B or 972D.

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

Straw Test: Second experimental replicate for dry bean cultivars with isolate 2D

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
