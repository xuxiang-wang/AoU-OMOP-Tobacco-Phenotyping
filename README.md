# All of Us (AoU) OMOP Tobacco Phenotyping

## Authors
<a href="https://orcid.org/0009-0006-2953-0576">Xuxiang Wang <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>, <a href="https://orcid.org/0000-0002-1947-1393">Megan L. Rolfzen <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>, <a href="https://orcid.org/0000-0003-2990-9042">Kristina Bailey <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>, <a href="https://orcid.org/0000-0002-5118-591X">Corrine K. Hanson <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>, <a href="https://orcid.org/0000-0002-3732-2789">Ran Dai <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>, <a href="https://orcid.org/0000-0002-3212-7845">A. Jerrod Anzalone <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>

## Abstract
**Background:** Existing phenotyping algorithms are often limited by their design for specific electronic health record (EHR) systems. A validated, reproducible tobacco phenotype for the widely used Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM) is needed to enable scalable, multi-site research. This study aimed to develop and validate a standard-based, rule-driven, point-in-time computable phenotype for tobacco use within the OMOP CDM.
**Methods:** Using EHR and self-reported survey data from 130,409 participants in the All of Us (AoU) Research Program, we developed a phenotype based on OMOP-standardized concept sets and a recency-based algorithm to classify smoking status. The EHR-based phenotype was validated against participant-reported survey data by calculating Cohen’s kappa, sensitivity, specificity, and intraclass correlation coefficients (ICC).
**Results:** Agreement between the EHR-based phenotype and survey-reported records was moderate. For current smokers, Cohen’s kappa was 0.447 (95% CI: 0.440-0.453), with a sensitivity of 0.618 and specificity of 0.859. For current or former smokers, kappa was 0.466 (95% CI: 0.460-0.473), with a sensitivity of 0.613 and a specificity of 0.865.  Among continuous measures, agreement for smoking duration was strong (ICC = 0.83), whereas agreement for daily cigarettes was poor (ICC = 0.09).
**Conclusion:** This study presents a validated, standards-based, reproducible tobacco phenotype within the OMOP CDM. By providing public, versioned concept sets and an auditable pipeline, this study addresses the critical need for a portable and transparent method for tobacco use assessment across large-scale research networks.


## Repository Usage

This repository is broken down into two sections: 

[Concept Sets](https://github.com/xuxiang-wang/AoU-OMOP-Tobacco-Phenotyping/tree/main/Concept%20Sets)

[Code](https://github.com/xuxiang-wang/AoU-OMOP-Tobacco-Phenotyping/tree/main/Code)

## License
This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License. See the `LICENSE` file for details.