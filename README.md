# Repository: Climate-Resilient Groundnut Varieties

This repository contains the code and supplementary materials for the research paper titled "Adoption of Climate-Resilient Groundnut Varieties Increases Agricultural Production and Smallholder Commercialization in West Africa."

## Paper Abstract
Adoption of climate-resilient crop varieties has the potential to build farmers’ climate resilience but could also induce agricultural transformation in developing nations. We investigate the relationship between adoption of climate-resilient groundnut varieties and production, consumption, and smallholder commercialization using panel data from Ghana, Mali, and Nigeria. We find adoption of climate-resilient groundnut varieties to increase smallholder production, consumption, and commercialization. The biggest adoption impact gains are observed under the sustained use of these climate-resilient varieties. We show that adoption benefits all households at different quantiles of the conditional distribution of commercialization, but the biggest gains are found among smaller producers, suggesting that adoption is inclusive. Furthermore, we provide suggestive evidence that yield increases could explain commercialization, although household consumption also matters. We conclude that adoption of climate-resilient groundnut varieties can at least partially reduce production constraints and promote smallholder consumption and commercialization, with implications for agricultural transformation.

## Repository Structure
The repository is organized as follows:

```
|-- 1_Main_Analysis.do
|-- 2_coefficent_plots.Rmd
|-- 3_supplementary_material.Rmd
|-- 3_Supplementary_Tables.Rmd
|-- 3_Supplementary-Tables.pdf
|-- 3_Supplementary-Tables.tex
|-- 4_Supplementary_Data.Rmd
|-- 4_Supplementary-Data.pdf
|-- Code_Base.Rproj
|-- HelperUtilities/
|   |-- Utils.R
|   |-- global_vars_and_helperFunctions.R
|   |-- header.html
|   |-- my_css.css
|   |-- my_templet.tex
|-- LICENSE.md
|-- README.md
|-- Source_Data.xlsx
|-- data/
|   |-- AdoptHistory_2.dta
|   |-- Groundnut.dta
|   |-- HomeConsumption.dta
|   |-- data_for_descriptive.dta
|   |-- geonear.dta
|-- figures/
|   |-- fig_1.pdf
|   |-- fig_2.pdf
|   |-- fig_3.pdf
|   |-- fig_4.pdf
|   |-- fig_5.pdf
|   |-- fig_6.pdf
|   |-- fig_6_new.svg
|   |-- fig_7.pdf
|   |-- fig_8.gph
|   |-- fig_9.pdf
|   |-- fig_SM1.pdf
|   |-- fig_SM2.pdf
|-- stata_outputs/
|   |-- SM_tables/
|   |   |-- Table_SM10.tex
|   |   |-- Table_SM10.txt
|   |   |-- Table_SM11.tex
|   |   |-- Table_SM11.txt
|   |   |-- Table_SM12.tex
|   |   |-- Table_SM12.txt
|   |   |-- Table_SM13.tex
|   |   |-- Table_SM13.txt
|   |   |-- Table_SM14.tex
|   |   |-- Table_SM14.txt
|   |   |-- Table_SM2.tex
|   |   |-- Table_SM2.txt
|   |   |-- Table_SM3.tex
|   |   |-- Table_SM3.txt
|   |   |-- Table_SM4.tex
|   |   |-- Table_SM4.txt
|   |   |-- Table_SM5.tex
|   |   |-- Table_SM5.txt
|   |   |-- Table_SM6.tex
|   |   |-- Table_SM6.txt
|   |   |-- Table_SM7.tex
|   |   |-- Table_SM7.txt
|   |   |-- Table_SM8.tex
|   |   |-- Table_SM8.txt
|   |   |-- Table_SM9.tex
|   |   └── Table_SM9.txt
|   |-- figures_table/
|       |-- Figure3.txt
|       |-- Figure4.txt
|       |-- Figure5.txt
|       |-- Figure6.txt
|       |-- Figure7.txt
|       |-- Figure_S1.txt
|       |-- Figure_S2.txt
|       |-- coefficent_plot_table.csv
|       └── coefficent_plot_table.xlsx

```


### `1_Main_Analysis.do`
This file contains the main analysis code written in Stata. It  includes data loading, data manipulation, econometric estimation, and the generation of results used in the  paper. To run the main analysis, open the file in Stata and follow the instructions within the code. Ensure that the required datasets are available in the `data/` directory.

### `2_coefficent_plots.Rmd`
The `2_coefficent_plots.Rmd` file is an R Markdown document. It includes code for data visualization and generation of graphs displaying the coefficients and their significance in the regression analysis. To use this file, make sure to have the required R packages installed, and then run the R Markdown document to generate the coefficient plots.


### `3_Supplementary_Tables.Rmd`, `3_Supplementary-Tables.pdf`, `3_Supplementary-Tables.tex`
These files contain supplementary tables to complement the research paper. The R Markdown document (`3_Supplementary_Tables.Rmd`)  includes code to generate additional tables that were not included in the main paper. The PDF and TeX files (`3_Supplementary-Tables.pdf` and `3_Supplementary-Tables.tex`) are the compiled versions of these supplementary tables. To access the supplementary tables, open the R Markdown file and knit it to generate the PDF and TeX versions.

### `4_Supplementary_Data.Rmd`, `4_Supplementary-Data.pdf`
The R Markdown document `4_Supplementary_Data.Rmd`  contains code to process and analyze supplementary data that accompanies the research paper. The `4_Supplementary-Data.pdf` file is the compiled version of this supplementary data analysis. 

### `HelperUtilities/`
This directory contains utility scripts and files that are used to assist the main analysis and other processes in the repository. These utility files  include functions, helper scripts, custom CSS styles, and LaTeX templates used for formatting and generating the supplementary materials.


### `Source_Data.xlsx`
The `Source_Data.xlsx` file contains the source data used for generating figures in the paper. Researchers can access this data for transparency and reproducibility.

### `data/`
The `data/` directory stores various data files used in the analysis. The listed `.dta` files contain datasets in Stata format, and they are  the cleaned and processed versions of the raw data. These datasets are  used as inputs for the main analysis and supplementary analyses.

### `figures/`
The `figures/` directory contains the generated figures in PDF and SVG formats. The figures represent visualizations, graphs, or charts that are included in the research paper or supplementary materials. 

### `stata_outputs/`
The `stata_outputs/` directory contains tables generated by Stata during the analysis. The `SM_tables/` subdirectory  contains tables produced as output, while the `figures_table/` subdirectory may include tables or datasets associated with figures in the paper or supplementary materials. 


## Usage
To use the code in this repository, follow these steps:

1. Clone the repository to your local machine using the following command:
   ```
   git clone git@github.com:bsrthyle/Adoption-of-Climate-Resilient-Groundnut-Varieties.git
   ```

2. Install the required dependencies and libraries mentioned in the code files.

3. Run the `1_Main_Analysis.do` do file to clean, statistical analysis, generate visualizations, and reproduce the results presented in the research paper.

4. Open the `2_coefficent_plots.Rmd` notebook in Rstudio or any compatible environment to generate visualizations, and reproduce the figures presented in the research paper.



## License
The contents of this repository are licensed under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) (GPL-3.0). Please refer to the [LICENSE](LICENSE) file for more details.







