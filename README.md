# All of Us (AoU) OMOP Tobacco Phenotyping

## Authors
<a href="https://orcid.org/0009-0006-2953-0576">Xuxiang Wang <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>, <a href="https://orcid.org/0000-0002-1947-1393">Megan L. Rolfzen <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>, <a href="https://orcid.org/0000-0003-2990-9042">Kristina Bailey <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>, <a href="https://orcid.org/0000-0002-5118-591X">Corrine K. Hanson <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>, <a href="https://orcid.org/0000-0002-1040-8750">Jana K. Ponce <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>, <a href="https://orcid.org/0000-0002-3732-2789">Ran Dai <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>, <a href="https://orcid.org/0000-0002-3212-7845">A. Jerrod Anzalone <img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16" /></a>

## Abstract
**Background**: Existing smoking phenotyping algorithms are often designed for specific electronic health record (EHR) systems, which limits their reuse across health systems. A validated, reproducible tobacco phenotype for the Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM) is needed to support scalable, multisite research. This study developed and validated a standards-based, point-in-time computable phenotype for tobacco use within the OMOP CDM.
**Methods**: EHR and self-reported survey data were drawn from 120,624 participants identified from 747,029 participants in the All of Us Research Program. The phenotype was built from OMOP-standardized concept sets and a point-in-time algorithm that assigns smoking status (current, former, or non-smoker) as of a chosen reference date. Using the survey date as the reference, the EHR-based classification was compared with survey responses using Cohen's kappa, sensitivity, and specificity for current and current or former smoking, and intraclass correlation coefficients (ICCs) for cigarettes per day and smoking duration.
**Results**: Agreement was moderate. For current smokers, kappa was 0.451 (95% CI 0.446-0.457; sensitivity 0.605, specificity 0.882, PPV 0.506). For current or former smokers, kappa was 0.492 (95% CI 0.486-0.497; sensitivity 0.606, specificity 0.884, PPV 0.838). Agreement was good for smoking duration (ICC = 0.78) and poor for cigarettes per day (ICC = 0.19).
**Conclusion**: This study presents an openly specified, standards-based, reproducible tobacco phenotype within the OMOP CDM, showing moderate agreement with self-reported survey data. Public concept sets and an auditable pipeline provide a transparent implementation designed for reuse in other research settings in the OMOP CDM.




## Repository Usage

This repository is broken down into two sections: 

[Concept Sets](https://github.com/xuxiang-wang/AoU-OMOP-Tobacco-Phenotyping/tree/main/Concept%20Sets)

[Code](https://github.com/xuxiang-wang/AoU-OMOP-Tobacco-Phenotyping/tree/main/Code)

## License
This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License. See the `LICENSE` file for details.