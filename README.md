# Burnout among physician assistants/physician associates meta-analysis code

[![DOI](https://zenodo.org/badge/DOI/10.31234/osf.io/xgc5s_v1.svg)](https://doi.org/10.31234/osf.io/xgc5s_v1)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-4.1.0-blue.svg)](https://cran.r-project.org/)

## Version 1.0.8
**Updated:** October 2026

**v1.0.8:** Figure formatting updated for JAAPA Supplemental Digital Content (Appendices C–E): study labels as "Author et al, Year", AMA-style *P* values, revised axis labels and titles, and risk-of-bias note placed beneath the forest plot. The statistical analysis is unchanged from v1.0.7.

## Authors
- Anna Bock¹, Alec Knight*², Ricardo Twumasi³
- ¹Physician Assistant Program, Northwestern University, Chicago, Illinois, United States
- ²Faculty of Life Sciences & Medicine, King's College London, London, United Kingdom
- ³Institute of Psychiatry, Psychology & Neuroscience, King's College London, London, United Kingdom
- *Corresponding author: ricardo.twumasi@kcl.ac.uk
  
Full R code for reproducing our meta-analysis of the prevalance of burnout in PAs. This repository contains the code used in our systematic review and meta-analysis (Bock et al., in preparation) 

## Registration  
**PROSPERO Registration:** [CRD42025644630](https://www.crd.york.ac.uk/PROSPERO/view/CRD42025644630)

## Overview

This repository contains the complete code and data for a systematic review and meta-analysis examining burnout prevalence among physician assistants/associates. The analysis includes 18 studies capturing data from 47,098 PAs published between 2018-2026.

## 💻 Requirements

- R (≥ 4.1.0)
- Required R packages:
  ```R
  tidyverse
  metafor
  ```

## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/ricardotwumasi/physician-assistants-burnout-meta-analysis.git
   ```

2. Install required R packages:
   ```R
   required_packages <- c("tidyverse", "metafor")
   install.packages(required_packages)
   ```
## Quick Start

```r
# Set working directory to the R_code folder
setwd("R_code/")

# Run main analysis (Version 1.0.8)
source("01_PA_burnout_main_analysis_CLEAN.R")
```

## 🤖 AI Statement

This repo was vibe coded with the assistance of Claude Opus 4 (Anthropic, San Francisco: CA)

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 📚 Citations

Key methodological references:

```bibtex
@book{harrer2021doing,
      title     = {Doing Meta-Analysis With {R}: A Hands-On Guide},
      author    = {Harrer, Mathias and Cuijpers, Pim and Furukawa Toshi A and Ebert, David D},
      year      = {2021},
      publisher = {Chapman & Hall/CRC Press},
      address   = {Boca Raton, FL and London},
      isbn      = {9780367610074},
      edition   = {1st}
    }

@book{borenstein2021,
	title = {Introduction to {Meta}-{Analysis}},
	isbn = {978-1-119-55835-4},
	url = {https://books.google.co.uk/books?id=2oYmEAAAQBAJ},
	publisher = {Wiley},
	author = {Borenstein, M. and Hedges, L.V. and Higgins, J.P.T. and Rothstein, H.R.},
	year = {2021},
}


@article{viechtbauer2010,
  title={Conducting meta-analyses in R with the metafor package},
  author={Viechtbauer, Wolfgang},
  journal={Journal of Statistical Software},
  volume={36},
  number={3},
  pages={1--48},
  year={2010}
}
```

[Citation](#citation) 
For citing this repository, please use:

<details>
<summary>BibTeX</summary>
<pre><code>@article{bock2025,
  title={Burnout among physician assistants/physician associates: A systematic review and meta-analysis},
  author={Bock, Anna; Knight, Alec; Twumasi, Ricardo},
  journal={PsyArXiv},
  year={2026},
  publisher={tbc},
  doi={10.31234/osf.io/xgc5s_v1}
}
</code></pre>
</details>
<details>
<summary>APA</summary>
<pre><code>Bock, A., Knight, A. & Twumasi, R. (2025). Burnout among physician assistants/physician associates: A systematic review and meta-analysis. [PsyArXiv] [2026]* </code></pre>
</details>
<details>
<summary>Vancouver</summary>
<pre><code>Bock A, Knight A, Twumasi R. Burnout among physician assistants/physician associates: A systematic review and meta-analysis. [Journal] [Year]</code></pre>
</details>

---
Contributors: Ricardo Twumasi, Anna Bock, Alec Knight
