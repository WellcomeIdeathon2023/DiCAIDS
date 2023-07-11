# DiCAIDS {DIgital and Computational Approaches to Infectious Diseases Study}


## Introduction 

The DiCAIDS discovery platform built on the R-shiny framework, provides a user-friendly, click-and-drop interface for aggregating connected datasets, visualising patterns in the dataset and identifying potential correlates of protection.

## The Prototype

The app has five distinctive panels:

<img src="images/dicaids_app.png" align="right" width="40%"/>

- The Browse Panel lets users upload, aggregate, process, and explore single/multiple connected study datasets.

- The Visualise Panel lets users explore simple descriptive patterns in the datasets, including the prevalence of infection and antibody seroconversion and the geometric mean of antibody titers, among others. Users also have the option to select which assay result and virus strain they want to explore for specific visualisations.

- The Analyse Panel lets users apply classical statistical or machine learning methods to identify immune correlates of protection. We also created three separate sub-panels, building on the framework for assessing the immunological correlates of protection in vaccine trials by Qin et al.5. 

- The Help and About panels under development are intended to provide additional resources about statistical models, assumptions underpinning all analyses covered on the platform. We will also use this panel to continually engage with the health data science community and other relevant stakeholders to refine the platform, our models, assumptions, and strategies.

## Methodology

This is your private repository for working on the challenges in the Wellcome Data Science Ideathon.
This repository is maintained and monitored by Wellcome staff and will be made public after July 13 2023.
Feel free to create additional folders in this repository but please use the existing ones as follows:

* `data` - Any data that is loaded from your scripts (excluding data scraped/downloaded from the web) should be uploaded to this folder. Simulated data should be reproducible.
* `code` - All code used as part of your solution should be uploaded this folder and is expected to be reproducible.
* `results` - Final results, including presented slides and other content, should be uploaded to this folder.



# Visualizing Patterns of Health Information Seeking Behaviour during the Novel Coronavirus Disease Outbreak


## Abstract

Previous studies about the timing and intensity of health information-seeking behaviour have presented mixed findings and concentrated in developed countries. We used heatmap visualizations to uncover intricate patterns of health information seeking during the pandemic in 23 sub-Sahara African countries. The visualization is based on Google Trends data for keywords related to coronavirus symptoms and the reported number of confirmed Covid-19 cases and deaths from December 2019 to December 2020. The data’s main insight is that a surge in the search interest for coronavirus symptoms in several African countries followed national case trends and usually occurred after a national announcement of an outbreak rather than global trends. In most countries, search interest significantly peaked between the 12th and 13th week of 2020. Additional insight from the heatmap visualization is the potential to summarize a large amount of data and compare trends in health information-seeking across countries.

 
  <img align="centre" src="Output/Search_CasesDeaths_CoVID_19.png">

  
## Reproducibility Materials :: How To

- Fork this repository.

- Using RStudio open "SearchTrends_CoVID_19.Rproj" file in the main project directory.

- Run the "R/00_master.R" file. Wait. That's it. 

  - The downloaded data are stored in the sub-directory "Data" and the figure is saved in "Output".

## Reproducibility Logic

The whole process is split into four parts, which is reflected in the structure of R scripts. First, the packages required for reproducibility are installed. Second, the figure theme is configured while all data manipulation steps are performed in step 3. Finally, the figures are built. The names of the scripts are quite indicative, and each script is reasonably commented.



# LICENCE

The code in this repository is licenced under a permissive [MIT licence](https://opensource.org/licenses/MIT). All other content is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). This means you may use any content in this repository as long as you credit the authors.
