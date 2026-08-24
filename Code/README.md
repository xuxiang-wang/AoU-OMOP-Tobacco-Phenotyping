# Tobacco Phenotype Pipeline

R code that identifies smoking status (current, former, or non-smoker) from the *All of Us* CDR v9 (OMOP CDM) and validates it against survey data.

## How to run

Run everything with one script:

```r
source("Code/R/run_tobacco.R")
```

If that does not work, run the scripts in `Code/R/` one at a time, in order:

```r
source("Code/R/00_setup.R")                # libraries, settings, helpers
source("Code/R/01_load_data.R")            # pull data from the CDR
source("Code/R/02_classify_ehr.R")         # build the EHR phenotype
source("Code/R/03_compare.R")              # build the cohort, match to surveys
source("Code/R/04_validation_analysis.R")  # tables and figures
rmarkdown::render("Code/R/05_generate_tobacco_report.Rmd")  # HTML report
```

Keep this order — each step uses the output of the one before it.

## What you get

Everything is written to the `Results/` folder:

- `Results/tables/` — every table as a CSV
- `Results/figures/` — every figure as PDF and PNG
- `Results/tobacco_validation_report.html` — one HTML file with all tables and figures

## Before you start

- Run this inside the *All of Us* Researcher Workbench (it reads the CDR).
- The concept sets are in the `Concept Sets/` folder.
- If packages are missing, install them first (see the `install.packages(...)` line at the top of `run_tobacco.R`).

## Environment

R version 4.5.3, *All of Us* Researcher Workbench.

<details>
<summary><b>Session Info</b></summary>

```
R version 4.5.3 (2026-03-11)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 24.04.4 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

time zone: Etc/UTC
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] knitr_1.51          gt_1.3.0            rmarkdown_2.30      scales_1.4.0        ggsci_5.1.0         broom_1.0.12        irr_0.85            lpSolve_5.6.23      gtsummary_2.5.1    
[10] data.table_1.18.2.1 bigrquery_1.6.2     lubridate_1.9.5     forcats_1.0.1       stringr_1.6.0       dplyr_1.2.0         purrr_1.2.1         readr_2.2.0         tidyr_1.3.2        
[19] tibble_3.3.1        ggplot2_4.0.2       tidyverse_2.0.0    

loaded via a namespace (and not attached):
 [1] gtable_0.3.6       xfun_0.56          gargle_1.6.1       tzdb_0.5.0         vctrs_0.7.1        tools_4.5.3        generics_0.1.4     curl_7.0.0         parallel_4.5.3    
[10] pkgconfig_2.0.3    RColorBrewer_1.1-3 S7_0.2.1           lifecycle_1.0.5    compiler_4.5.3     farver_2.1.2       textshaping_1.0.5  brio_1.1.5         litedown_0.9      
[19] sass_0.4.10        htmltools_0.5.9    yaml_2.3.12        pillar_1.11.1      crayon_1.5.3       commonmark_2.0.0   tidyselect_1.2.1   digest_0.6.39      stringi_1.8.7     
[28] labeling_0.4.3     fastmap_1.2.0      grid_4.5.3         cli_3.6.5          magrittr_2.0.4     cards_0.8.0        withr_3.0.2        prettyunits_1.2.0  backports_1.5.0   
[37] bit64_4.6.0-1      cardx_0.3.3        timechange_0.4.0   httr_1.4.8         bit_4.6.0          otel_0.2.0         ragg_1.5.1         hms_1.1.4          evaluate_1.0.5    
[46] markdown_2.0       rlang_1.1.7        glue_1.8.0         DBI_1.3.0          xml2_1.5.2         rstudioapi_0.18.0  vroom_1.7.0        jsonlite_2.0.0     R6_2.6.1          
[55] systemfonts_1.3.2  fs_1.6.7       
```

</details>