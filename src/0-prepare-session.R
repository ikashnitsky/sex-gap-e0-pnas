#===============================================================================
# 2021-03-11 -- sex gap e0
# Prepare data
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================  

options(scipen = 9999)

library(tidyverse)
library(magrittr)
library(fs); library(here); library(glue)
library(hrbrthemes)
library(patchwork)
library(cowplot)
library(paletteer)
library(prismatic)
library(rcartocolor)
library(DemoDecomp)
library(countrycode)

# functions to read local HMD directories
devtools::source_gist("0f93062f2b67eeac69949554027fa84f")

# current working directory
wd <- here::here()

# a small function to localize paths
lp <- function(path) {
    require(here)
    require(magrittr)
    require(glue)
    wd <- here::here()
    glue(paste0("{wd}/", path))
}