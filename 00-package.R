pacman::p_load(readr,dplyr,stringr,tidyr,ggplot2,plotly,readxl,purrr,corrplot,psych,heatmaply)

# sf package: two way to download
# install.packages("sf", 
#   configure.args = "--with-proj-include=/proj_home/include --with-proj-lib=/proj_home/lib --with-proj-api=no"
# );
library(sf)

# tmap package: two way to download
# install.packages(
#   "https://cran.rstudio.com/bin/macosx/contrib/4.0/tmap_3.3-2.tgz", 
#   repos = NULL, type = "source"
# );
library(tmap)

pacman::p_load(GGally,ggbiplot)

library(devtools)
install_github("vqv/ggbiplot")

