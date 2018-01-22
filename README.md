These repository includes examples of scripts for making different type of graphs
such as bar plot, scatterplot and error bars plot.
The scripts were done for my Master thesis project and the results had to be
represented graphically showing the above ground biomass (AGB) of two palm tree species in Peru.

First, the original files were excel files with the data of AGB for the 20 samples
used. These data had values of height, wood density, steam volume and AGB
and the results have to be compared between palm tree species, comparison of wood
density and tree height taken in field and obtained from Quantitative structure
modelling, and make comparisons with Rosa Goodman et. al. (2008) results regarding
AGB in the same species.

First, the csv files are saved as data frames in order to make plots, all the data
were managed by species to crate graphs. Afterwards, depending on the type of graph these data had to be process to obtain the plots.

The libraries used were:
library(plotly)
library(dplyr)
library(ggplot2)
library(reshape2)
library(cowplot)
library(gridExtra)

Author: Careli Caballero
MSc Geo-information Science

