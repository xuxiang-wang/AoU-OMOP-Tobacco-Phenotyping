# Code Availability

This repository contains the R code for a reproducible phenotyping pipeline designed to identify tobacco use status from the All of Us Research Program's Curated Data Repository (CDR) v8.

## Execution Order

The analysis pipeline is modularized into sequential scripts. To replicate the study findings or generate the validation tables, please execute the code in the following order:

- [00_setup.R](https://github.com/xuxiang-wang/AoU-OMOP-Tobacco-Phenotyping/tree/main/Code/R/00_setup.R): Installs necessary dependencies, loads R libraries, and sets up the project directory structure.
- [01_load_data.R](https://github.com/xuxiang-wang/AoU-OMOP-Tobacco-Phenotyping/tree/main/Code/R/01_load_data.R): Connects to the CDR database, extracts cohort demographics, and retrieves raw tobacco-related concepts (observations, procedures, drugs) using SQL.
- [02_process_and_combine.R](https://github.com/xuxiang-wang/AoU-OMOP-Tobacco-Phenotyping/tree/main/Code/R/02_process_and_combine.R): Cleans the raw data, applies the point-in-time logic, and combines longitudinal event streams into a unified dataset.
- [03_create_analysis_datasets.R](https://github.com/xuxiang-wang/AoU-OMOP-Tobacco-Phenotyping/tree/main/Code/R/03_create_analysis_datasets.R): Generates the final analytical data frames:
    - `long_tobacco_EHR_data`: Event-level history.
    - `wide_tobacco_EHR_data`: Patient-level summary with "Ever Smoker" classifications.
- [04_validation_analysis.R](https://github.com/xuxiang-wang/AoU-OMOP-Tobacco-Phenotyping/tree/main/Code/R/04_validation_analysis.R): Performs the statistical validation (Sensitivity, Specificity, Kappa, ICC) against survey data and runs the sensitivity analyses for temporal windows and time lags.
- [05_generate_tobacco_report.Rmd](https://github.com/xuxiang-wang/AoU-OMOP-Tobacco-Phenotyping/tree/main/Code/R/05_generate_tobacco_report.Rmd): Compiles all results, tables, and figures into a final HTML document (tobacco_validation_report.html) for easy viewing.

Alternative Execution: The entire pipeline can also be orchestrated by running the master script directly:
- [run_tobacco.R](https://github.com/xuxiang-wang/AoU-OMOP-Tobacco-Phenotyping/tree/main/Code/R/run_tobacco.R)


## Expected Output

The scripts will print progress messages to the console as they complete each step. Upon successful completion, the pipeline will generate two primary types of outputs: Data Products and a Validation Report.

1. Data Products (Data Frames)
   The main outputs of the phenotyping pipeline are two R data frames created in the RStudio environment:
   - `long_tobacco_EHR_data`: An event-level table containing every tobacco-related EHR record for the final cohort, enriched with phenotype indicator variables.
   - `wide_tobacco_EHR_data`: A patient-level table with one row per person, summarizing their tobacco use history and details from their most recent EHR event.

2. Validation Report (HTML File)
   The pipeline generates a final, comprehensive report named tobacco_validation_report.html. This report contains all descriptive tables, agreement metrics (Kappa, ICC), and plots used to validate the phenotype against survey data.

## Environment

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
