# WVS Data Analysis
This project is an assignment for Statistics and Methodology course in Master of Data Science and Society program, Tilburg University. In this project, we perform numerous data wrangling task as well as statistical inference and prediction on Wave 6 of the World Values Survey data. The data could be downloaded directly from [WVS website](http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp "Wave 6 WVS data"). We include the preporcessed data in [data](https://github.com/miftahulridwan/WVS-Data-Analysis/tree/master/data "data directory") directory.

## Table of Directory

1. [Code](https://github.com/miftahulridwan/WVS-Data-Analysis/tree/master/code)
2. [Data](https://github.com/miftahulridwan/WVS-Data-Analysis/tree/master/data)
3. [Figure](https://github.com/miftahulridwan/WVS-Data-Analysis/tree/master/figs)
4. [Report](https://github.com/miftahulridwan/WVS-Data-Analysis/tree/master/docs)
 


## Project Summary
The aim of this project is to investigate the effect of particular country on _feelings of happiness_ of its citizen - before and after controlling for health condition, and try to predict satisfaction with financial situation using plausible variable in the dataset. Our result is dependent to the dataset we are using. 
<br>

Before controlling for health condition, we found that the model with country variable explains 4% of the feeling of happiness. We also found that US is the country with the happiest citizen. Furthermore, we gain more statistical power after controlling for health condition: the R squared of our model increases to 20.6%. The country with happiest citizen after controlling for health also change to India. 
<br>

Moreover, we also build 3 models to predict statisfaction with financial situatuon. Details of each model are as follows:
<br>

Model | Variable | Rationale | Cross-Validation Error (CVE)
---- | ---- | ---- | ----
**1** | **_Satisfaction with Life_, _Country_, _Sex_** | **Men and women have different perceptions of financial satisfaction according to their country and to their perceived satisfaction with life. (e.g. unsatisfied woman in Germany vs. satisfied man in India)** | **3.536**
2 | _Number of Children_, _Marital Status_, _Sex_ | Men and women have different perceptions of financial satisfaction at the presence of children and given their marital status. (e.g. single man with no children vs. divorced woman with several children) | 5.213
3 | _Age_, _Level of Education_, _Sex_ | Men and women have different perceptions of financial satisfaction according to their age and to their education level. (e.g. older highly educated man vs. younger uneducated woman) | 5.262

After 10-fold cross-validation, our experiment shows that model with individual perception of life, the country in which the respondent resides, and the biological sex has the lowest croess-validation error with CVE = 3.536. Full report of this project is in [here](https://github.com/miftahulridwan/WVS-Data-Analysis/blob/master/docs/group20_report.pdf).

## Environment and Library
We deploy our [code](https://github.com/miftahulridwan/WVS-Data-Analysis/blob/master/code/group20_code.R) in R using several libraries, such as:
1. MASS
2. MLmetrics

We also use [Student Function](https://github.com/miftahulridwan/WVS-Data-Analysis/blob/master/code/studentFunctions.R) provided by the teacher.
