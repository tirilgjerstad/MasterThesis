
#### Load the required packages ####
# if packages are not installed already,
# install them using function install.packages(" ")

library(shiny) # shiny features
library(shinydashboard) # shinydashboard functions
library(shinydashboardPlus)
library(shinycssloaders) # to add a loader while graph is populating
library(shinyWidgets)
library(shinyBS)
library(shinyjs)


library(DT)  # for DT tables
library(dplyr)  # for pipe operator & data manipulations
library(plotly) # for data visualization and plots using plotly 
library(ggplot2) # for data visualization & plots using ggplot2
library(ggtext) # beautifying text on top of ggplot
library(ggcorrplot) # for correlation plot
library(gplots)
library(gt)

library(BiocManager)
options(repos = BiocManager::repositories())

library(openxlsx)
library(pheatmap)
library(heatmaply)
library(NMF)
library(survival)
library(ggsurvfit)
library(devtools)
library(graphics)
library(clusterSim)


# Read the random generated dataset from files. 
random_generated_M = read.table('synthetic_M.txt', header = TRUE, row.names = 1, sep = '')
random_generated_clinical = read.table('synthetic_clinical.txt', header = TRUE, sep = '')

# Fix column names of M. 
categories = scan('synthetic_M.txt', what = 'character', sep = " ", nlines = 1)
names(random_generated_M) = categories
  

