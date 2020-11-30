source("./data2report.R",encoding = "UTF-8")
configFile <- "./config.txt"
library(tidyverse)
library(readxl)
library(rmarkdown)
library(xfun)
library(showtext)
library(readr)
config <- read_tsv(configFile,col_names = c("name","value"),
                   col_types = cols(name = col_character(),value = col_character())) %>%
  pivot_wider(names_from = "name",values_from = "value")
referenceIndicatorFile <- config %>% select(referenceIndicatorFile) %>% pull() %>% normalizePath()
translationFile <- config %>% select(translationFile) %>% pull() %>% normalizePath()
indicatorResultFile <- config %>% select(indicatorResultFile) %>% pull() %>% normalizePath()
clinicalInfoFile <- config %>% select(clinicalInfoFile) %>% pull() %>% normalizePath()
rowNumber <- config %>% select(rowNumber) %>% pull() %>% as.numeric()
sampleID <- config %>% select(sampleID) %>% pull() %>% as.character()
institution <- config %>% select(institution) %>% pull() %>% as.character()
institutionAddress <- config %>% select(institutionAddress) %>% pull() %>% as.character()
outputDir <- config %>% select(outputDir) %>% pull() %>% normalizePath()

render_to_pdf_report(referenceIndicatorFile = referenceIndicatorFile, 
                                 rowNumber = rowNumber,
                                 translationFile = translationFile,
                                 indicatorResultFile = indicatorResultFile,
                                 sampleID = sampleID,
                                 clinicalInfoFile = clinicalInfoFile,
                                 institution = institution,
                                 institutionAddress = institutionAddress,
                                 outputDir = outputDir)
 
