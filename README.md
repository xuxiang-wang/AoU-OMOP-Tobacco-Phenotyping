# All of Us (AoU) OMOP Phenotyping Pipeline

This repository contains the R code for a reproducible phenotyping pipeline designed to identify tobacco use status from the [All of Us Research Program's Curated Data Repository (CDR) v8](https://support.researchallofus.org/hc/en-us/articles/30294451486356-Curated-Data-Repository-CDR-version-8-Release-Notes). The pipeline is structured to be modular and scalable for future phenotyping projects (e.g., alcohol use).

## Project Structure

The project is organized into several key directories and scripts to separate concerns and improve maintainability.

```
aou-omop-phenotyping/
│
├── run_tobacco.R               # Main script to run ONLY the tobacco pipeline
├── (run_alcohol.R)             # A placeholder for the future alcohol pipeline script
├── 00_setup.R                  # SHARED setup for all pipelines (libraries, functions)
│
├── R/
│   ├── tobacco_phenotype/      # All R code specific to the tobacco analysis
│   │   ├── 01_load_data.R
│   │   ├── 02_process_and_combine.R
│   │   ├── 03_create_analysis_datasets.R
│   │   ├── 04_validation_analysis.R
│   │   └── 05_generate_tobacco_report.Rmd # R Markdown file to create the final HTML
│   │
│   └── alcohol_phenotype/      # RESERVED for the future alcohol analysis
│
└── Concepts/                   # Contains all necessary data and concept definition files
    ├── tobacco/
    │   ├── Medication Cessation.csv
    │   ├── Nicotine_for_Cessation.csv
    │   ├── Smoking Status.csv
    │   └── Tobacco Counseling.csv
    │
    └── alcohol/

```

## Requirements

-   Access to the All of Us (AoU) Research Program Controlled Tier dataset.
-   A provisioned workspace within the AoU Researcher Workbench with RStudio.

## Setup and Installation

This project is designed to be version-controlled locally using Git but executed within the secure All of Us Researcher Workbench.

1.  **Clone the Repository (Local Machine)**
    Clone this repository to your local machine to manage code changes with Git.
    ```bash
    git clone [your-repository-url]
    ```

2.  **Prepare Project for Upload**
    To run the pipeline in the AoU Workbench, you will need to upload the necessary scripts and data files.
    -   On your local machine, create a single `.zip` compressed archive containing the following folders:
        -   The `R` folder (containing all analysis scripts and the main runner)
        -   The `Concepts` folder (containing all concept definition CSV files)
    -   Name the file something descriptive, like `tobacco_pipeline_upload.zip`.

3.  **Upload to All of Us Workbench**
    -   In AoU Workbench workspace, navigate to the "Files" area.
    -   Click the "Upload" button and select the `tobacco_pipeline_upload.zip` file you just created.
    -   **The All of Us environment will automatically unzip the file**, recreating the correct folder structure (e.g., `R/tobacco_phenotype/` and `Concepts/tobacco/`) within workspace.

## Usage

Once the files are uploaded and unzipped in AoU workspace, follow these steps to run the analysis:

1.  **Install Packages**
    Install packages before running the `run_tobacco.R` script located inside the `R` folder.
    ```R
    install.packages("tidyverse")  # Restart Rstudio needed when install this package
    install.packages("gtsummary")  # Restart Rstudio needed when install this package
    install.packages("rmarkdown")  # Restart Rstudio needed when install this package
    install.packages("broom")
    install.packages("bigrquery")
    install.packages("dplyr")
    install.packages("lubridate")
    install.packages("stringr")
    install.packages("data.table")
    install.packages("purrr")
    install.packages("irr")
    install.packages("ggplot2")
    install.packages("ggsci")
    install.packages("scales")
    ```

2.  **Execute the Pipeline**
    The entire pipeline is orchestrated by the `run_tobacco.R` script located inside the `R` folder. To run the full analysis, open the RStudio **Console** and execute:
    ```R
    source("R/run_tobacco.R")
    ```

## Expected Output

The script will print progress messages to the console as it completes each step. Upon successful completion, the pipeline will generate two primary types of outputs: **Data Products** and a **Validation Report**.

### 1. Data Products (Data Frames)

The main outputs of the phenotyping pipeline are two R data frames created in RStudio environment:

-   **`long_tobacco_EHR_data`**: An event-level table containing every tobacco-related EHR record for the final cohort, enriched with phenotype indicator variables.
-   **`wide_tobacco_EHR_data`**: A patient-level table with one row per person, summarizing their tobacco use history (e.g., "ever-smoker") and details from their most recent EHR event.

After the pipeline run, you can save these data frames to `.csv` or `.RData` files for further use with commands like `write.csv(...)`.

### 2. Validation Report (HTML File)

The pipeline will also generate a final, comprehensive report inside the **`R/tobacco_phenotype/`** folder named **`tobacco_validation_report.html`**.

You can view this report by:
-   Navigating to the file in the RStudio "Files" pane.
-   Clicking on it and selecting "View in Web Browser".

This report contains all the descriptive tables, agreement metrics (Kappa, ICC), and plots used to validate the phenotype against survey data.

## Reproducibility and Environment

This analysis was performed in the All of Us Researcher Workbench using the R version and package versions listed below. This information is provided to ensure full reproducibility of the results.

<details>
<summary><b>Click to view Session Info</b></summary>

```
R version 4.4.0 (2024-04-24)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 22.04.3 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.20.so;  LAPACK version 3.10.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

time zone: Etc/UTC
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] rmarkdown_2.29    scales_1.4.0      ggsci_4.0.0       broom_1.0.10     
 [5] irr_0.84.1        lpSolve_5.6.23    gtsummary_2.4.0   data.table_1.17.8
 [9] bigrquery_1.6.1   lubridate_1.9.4   forcats_1.0.0     stringr_1.5.2    
[13] dplyr_1.1.4       purrr_1.1.0       readr_2.1.5       tidyr_1.3.1      
[17] tibble_3.2.1      ggplot2_4.0.0     tidyverse_2.0.0  

loaded via a namespace (and not attached):
 [1] gtable_0.3.6       bslib_0.7.0        xfun_0.53         
 [4] clock_0.7.0        gargle_1.5.2       tzdb_0.4.0        
 [7] vctrs_0.6.5        tools_4.4.0        generics_0.1.3    
[10] curl_5.2.1         fansi_1.0.6        highr_0.10        
[13] pkgconfig_2.0.3    RColorBrewer_1.1-3 S7_0.2.0          
[16] gt_1.1.0           lifecycle_1.0.4    compiler_4.4.0    
[19] farver_2.1.1       brio_1.1.5         litedown_0.7      
[22] sass_0.4.9         htmltools_0.5.8.1  yaml_2.3.8        
[25] jquerylib_0.1.4    pillar_1.9.0       crayon_1.5.2      
[28] cachem_1.0.8       commonmark_2.0.0   tidyselect_1.2.1  
[31] digest_0.6.35      stringi_1.8.3      labeling_0.4.3    
[34] fastmap_1.1.1      grid_4.4.0         cli_3.6.5         
[37] magrittr_2.0.3     cards_0.7.0        dichromat_2.0-0.1 
[40] utf8_1.2.4         withr_3.0.0        prettyunits_1.2.0 
[43] backports_1.4.1    bit64_4.0.5        cardx_0.3.0       
[46] timechange_0.3.0   httr_1.4.7         bit_4.0.5         
[49] hms_1.1.3          evaluate_0.23      knitr_1.46        
[52] markdown_2.0       rlang_1.1.6        glue_1.8.0        
[55] DBI_1.2.2          xml2_1.3.6         rstudioapi_0.16.0 
[58] vroom_1.6.5        jsonlite_1.8.8     R6_2.5.1          
[61] fs_1.6.4
```

</details>

## Citing This Work

* *[Placeholder for manuscript citation]*

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.