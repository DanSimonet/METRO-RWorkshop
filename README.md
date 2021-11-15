# METRO-Workshop
by Dan Simonet
R Orientation Workshop for I/O Psychology

-----
:spiral_calendar: November 18th, 2021\
:alarm_clock:     06:30 - 07:30  
:writing_hand:    [METRO Workshop](https://www.metroapppsych.com/)

-----

# Overview
This is an hour-long, hands-on workshop designed for those who are brand new to R & RStudio, or those who would like a refresher in it, and learn best by doing. You will learn the basics of R and data science by practicing on examples within the RStudio IDE (integrated development environment). We'll discuss several basic and intermediate operations including data transformation and tidying (`dplyr`), data visualization (`ggplot2`), statistical analyses and reports (`lm`, `afex`, `markdown`), and text modeling with twitter (`rtweet`, `tidytext`). Two HR datasets will be provided, one on employee attrition and another on HR twitter trends. The  aim is to introduce students and practitioners to the power of R to carry out basic and advanced analyses along with capabilities to visualize and report findings. 

## Learning objectives

Overview of R along with introduction to popular packages for doing data science in the tidyverse, including data transformation, data visualization, statistical reports, and text mininng. Particular aims include:

- **R Syntax and R Studio**: Introducing objects, functions, and vectorization along with different panels of the R Studio interface.
- **Data Manipulation**: How to import, describe, and manipulate data using the `dplyr` verbs including selecting columns, filtering cases, calculating new variables, rearranging rows, and computing summary statistics. 
-  **Basic Visualization**: Learn the grammar of graphics (in `ggplot2`) which affords immense flexibility in the way you visually represent trends, differences, and associations.
-  **Statistical Reporting**: Run multiple regression and ANOVA. Convert model output into tables and present alongisde text in a report using markdown.
-  **New Frontiers - Text Mining**: Learn how to extract, pre-process, simplify, and analyze text data in terms of frequencies and bigrams

Given the aim of the workshop is to *expose* novices to R the emphasis will be showcasing what R can do as opposed to a deep dive into mechanics, language, or package operations. Hence little time will be devoted to installation, programming, data structures, management, interpretation, and errors. 

## Prework

You'll be using [RStudio Cloud](https://rstudio.cloud/), a cloud-based version of R and RStudio available through your web browser. So (all going well) on the day of the workshop all you'll need is **a laptop that can access the internet**. Please do [sign up for an account on RStudio Cloud](https://client.login.rstudio.cloud/oauth/register?redirect=https%3A%2F%2Fclient.login.rstudio.cloud%2Foauth%2Flogin%3Fshow_auth%3D0%26show_login%3D1%26show_setup%3D1) before the workshop. You can make an account directly on RStudio Cloud, or use single-sign-on with a service like GitHub or Google. 

In the unlikely event that there are problems with the conference internet connection, you may want to have a local installation on your computer as a backup. If you'd like, install the following:
 
1. A recent version of R (~4.1.1), which is available for free at [cran.r-project.org](http://www.cran.r-project.org)
2. A recent version of RStudio IDE (~1.4.1717-3), available for free at [www.rstudio.com/download](http://www.rstudio.com/download)
3. The set of relevant R packages, which you can install by connecting to the internet, opening RStudio, and running:  
 
    install.packages(c("afex", "corrplot", "tidyverse", "emmeans", "ggraph",
              "igraph", "kable", "kableExtra", "modelsummary",
              "reshape2", "rtweet", "sjPlot", "tidytext", 
              "topicmodels", "viridis", "wordcloud", "xaringan")) 

## Instructor

Dan Simonet is an Associate Professor in the Psychology Department at Montclair State University (NJ). He is Director of the I/O PhD and MA program and teaches courses in Data Science, Psychometrics, and Multivariate Statistics. His research focuses on personality and individual differences at work. 
