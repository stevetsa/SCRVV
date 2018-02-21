# ViRGo: Variant Report Generator 

View a live demonstration of our application [here](https://hsiaoyi0504.shinyapps.io/virgo/)!

## Introduction   
This project is mainly based on combining and then exploring output files generated from [SC3](https://github.com/NCBI-Hackathons/SC3), which is a previous product of the NCBI Hackathons. SC3 was built upon [PSST](https://github.com/NCBI-Hackathons/PSST), which was also developed in prior NCBI Hackathons. The two input parameters of SC3 are a [NCBI BioProject accession number](https://www.ncbi.nlm.nih.gov/bioproject/) and a gene or disease of interest. With this information, SC3 has the functionality to map each [SRA experiment](https://www.ncbi.nlm.nih.gov/sra) in that BioProject to the SNP information gathered from [NCBI ClinVar](https://www.ncbi.nlm.nih.gov/clinvar/). We implemented a customized version of [SC3](https://github.com/hsiaoyi0504/SC3) to allow it to operate on Ubuntu without the SLURM workload manager.

In this project, we mapped the single cell RNA-sequencing data (NCBI BioProject accession PRJEB15401 and EMBL-EBI ArrayExpress E-MTAB-5061) using SC3. This [dataset](https://www.ncbi.nlm.nih.gov/bioproject/?term=PRJEB15401) consisted of transcription profiles of pancreatic tissues obtained from six healthy controls and four deceased donors with type 2 diabetes.

## What's the problem?
Single nucleotide polymorphisms (SNPs) are the most frequent type of genetic variation among human beings. Each SNP designates a difference in a single DNA building block (nucleotide) and occurs on average in about one out of every 300 nucleotides. In other words, there are about 10 million SNPs in the human genome. These genetic differences sometimes mean that the instructions for protein generation are different, which can cause phenotypic biological variation. While most SNPs have no known effects on health, there is growing evidence that SNPs can influence response to drugs, pathogens, chemicals, vaccines, toxicities, and many more agents. Hence, SNPs are critically relevant to personalized medicine. SNPs are also known to be associated with certain diseases, such as sickle-cell anemia and cystic fibrosis. There is growing evidence that SNPs in a given individual can influence disease susceptibility in complex ways. As a result, research studies will continue to identify associations between SNPs and complex diseases such as cancer, heart disease, diabetes, and Alzheimer's disease. Given the largeness of the SNP information for each individual and the complexity of such diseases, it is difficult to elicit SNPs with causal influence on diseases. Generating open-source software that allows researchers to quickly and interactively view SNP information alongside variables of interest is one small approach toward solving this problem.

## What is ViRGo?
Variant Report Generator (ViRGo) is a reporting and variant browser tool that aggregates information from an RNA-sequecing variant calling pipeline and provides summary and statistics of attributes including the following:

- Organism (Example: Humans)
- Organism part (Example: Pancreas)
- Individual sample (Example: Patient 1)
- Quality of SNP call
- Cell type (Example: Ductal cell)
- Sex of sample
- Disease (Example: Diabetes)

Interactively exploring how these variables are correlated to SNPs can aid in the assessment of clinically relevant variants. Our interactive application is mainly built upon an awesome R package [Shiny](https://shiny.rstudio.com/).
  
## How to use ViRGo
1. Download this Repository: `git clone --recursive https://github.com/NCBI-Hackathons/ViRGo`
2. Collect output files generated beforehand: `python3 collect_output.py`
3. Follow the steps in [MergeTwoFiles.R](MergeTwoFiles.R) to generate the file [/data/merged.Rds](/data/merged.Rds)
4. Install dependencies in R (from R prompt).
```{r}
#ggplot was not available for my version of R, ggplot2 used instead
install.packages(c("plotly", "shiny", "tidyr", "ggplot2", "dplyr", "DT"), repos = "https://cloud.r-project.org")
```
5. Run the Shiny application [ViRGo_app.R](ViRGo_app.R) to interact with the merged data.
```{r}
runApp("ViRGo/ViRGo_app.R")
```

![Flowchart](https://github.com/NCBI-Hackathons/ViRGo/blob/master/flowcharts/flowChart.png)

## Dependencies
* Python 3
* R (>= 3.4.1)
  * Packages:
    * shiny (>= 1.0.5)
    * tidyr (>= 0.7.2)
    * ggplot2 (>= 2.2.1)
    * plotly (>= 4.7.1)
    * dplyr (>= 0.7.4)
    * DT (>= 0.2)
    
## Presentation
* [January 22 Hackathon Slides](./presentation/Presentation_012218.pptx)
* [January 23 Hackathon Slides](https://docs.google.com/presentation/d/1YjBH5frG3v0PLQ3x3KwyDh3pNva85L7tBzYqLTyb7h0/edit#slide=id.p)
  
## Notes
* [data/E-MTAB-5061.sdrf.txt](./data/E-MTAB-5061.sdrf.txt) is from [ArrayExpress](https://www.ebi.ac.uk/arrayexpress/experiments/E-MTAB-5061/).
* Based on output files of a [customized version](https://github.com/hsiaoyi0504/SC3) of [SC3](https://github.com/NCBI-Hackathons/SC3), which is a product of previous NCBI Hackathons.
* [collect_output.py](collect_output.py) is for merging files generated from [SC3]((https://github.com/hsiaoyi0504/SC3)).
* [data/merged_sub.Rds](./data/merged_sub.Rds) and [data/merged.Rds](./data/merged.Rds) are files generated by the script [MergeTwoFiles.R](MergeTwoFiles.R)
