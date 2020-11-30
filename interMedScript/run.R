thisFile <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'dvia R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
}
source(paste0(dirname(thisFile()), "/source.R"),encoding = "UTF-8")
.libPaths(paste0(dirname(thisFile()), "/App/R-Portable/library"))
#args<-commandArgs(T)
#configFile <- normalizePath(args[1])
configFile <- paste0(dirname(dirname(thisFile())), "/config.txt")
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(stringr)
config <- read_tsv(configFile,col_names = c("name","value"),
                   col_types = cols(name = col_character(),value = col_character())) %>%
  pivot_wider(names_from = "name",values_from = "value")

msDataFile <- config %>% select(msDataFile) %>% pull() %>% normalizePath()
referFile <- config %>% select(referFile) %>% pull() %>% normalizePath()
yearInfoFile <- config %>% select(yearInfoFile) %>% pull() %>% normalizePath()
dataType <- config %>% select(dataType) %>% pull() %>% as.character()
digits <- config %>% select(digits) %>% pull() %>% as.numeric()
saveDir <- config %>% select(saveDir) %>% pull() %>% normalizePath()

t <- intermedReturn(msDataFile = msDataFile,
                    referFile = referFile,
                    yearInfoFile = yearInfoFile,
                    dataType = dataType,
                    digits = digits,
                    saveDir = saveDir)

write_excel_csv(t$newBorn_return,paste(saveDir,"newborn.csv",sep = ""))
write_excel_csv(t$unNewBorn_return,paste(saveDir,"nonnewborn.csv",sep = ""))