#'.bornResult
#'@description generate newborn or non-newborn result
#'@export
#'@importFrom select arrange mutate group_by distinct filter
#'@importFrom pivot_longer pivot_wider
#'@importFrom left_join
#'@param born dataframe contains newborn samples or non-newborn samples
#'@param msData dataframe contains values
#'@param rangeData dataframe contains newborn samples or non-newborn range
#'@param up_down up,down,all,non
#'@return dataframe contains intermed result
.bornResult <- function(born,msData,rangeData,up_down) {
  
  ref <- rangeData  %>% arrange(desc(Index)) %>%
    select(item,Index) %>%
    pivot_wider(names_from = "item",values_from = "Index")

  msData <- msData %>% select(sampleId,colnames(ref))
  #new born
  bornData <- born %>%
    left_join(msData,by = c("样本编码" = "sampleId")) %>%
    select(-age) %>%
    pivot_longer(cols = -`样本编码`,names_to = "item",values_to = "value") %>%
    left_join(rangeData,by = "item")
    # mutate(result = map(.x = value,
    #                     .f = ~if_else(str_detect(value,"-"),"",
    #                                              if_else(as.numeric(value) > up,"指标升高",
    #                                                                               ifelse(as.numeric(value) < low,"指标降低","pass")))))
    # mutate(result = if_else(is.infinite(value),"",
    #                         if_else(value > up,"指标升高",if_else(value < low,"指标降低","pass")))) %>%
    # mutate(value = if_else(is.infinite(value),"-",as.character(value)))
    # mutate(value = if_else(result == "指标升高",str_c("↑",as.character(value),sep = ""),
    #                        if_else(result == "指标降低",str_c("↓",as.character(value),sep = ""),as.character(value)))) %>%
    # #mutate(result = str_c(item,result,sep = "")) %>%
    # select(`样本编码`,item,value,result)
  #up_down <- up_down
  if (up_down == 'up') {
    bornData <- bornData %>% 
      mutate(result = if_else(is.infinite(value),"pass",
                              if_else(value > up,"指标升高","pass"))) %>%
      mutate(value = if_else(is.infinite(value),"-",as.character(value))) %>%
      mutate(value = if_else(result == "指标升高",str_c("↑",as.character(value),sep = ""),as.character(value))) %>%
      select(`样本编码`,item,value,result)
  }else if (up_down == "down") {
    bornData <- bornData %>% 
      mutate(result = if_else(is.infinite(value),"pass",
                              if_else(value < low,"指标降低","pass"))) %>%
      mutate(value = if_else(is.infinite(value),"-",as.character(value))) %>%
      mutate(value = if_else(result == "指标降低",str_c("↓",as.character(value),sep = ""),as.character(value))) %>%
      select(`样本编码`,item,value,result)
  }else if (up_down == "all") {
    bornData <- bornData %>% 
      mutate(result = if_else(is.infinite(value),"pass",
                              if_else(value > up,"指标升高",if_else(value < low,"指标降低","pass")))) %>%
      mutate(value = if_else(is.infinite(value),"-",as.character(value))) %>%
      mutate(value = if_else(result == "指标升高",str_c("↑",as.character(value),sep = ""),
                             if_else(result == "指标降低",str_c("↓",as.character(value),sep = ""),as.character(value)))) %>%
      select(`样本编码`,item,value,result)
  }else if (up_down == "non") {
    bornData <- bornData %>% 
      mutate(result = if_else(is.infinite(value),"pass","pass")) %>%
      mutate(value = if_else(is.infinite(value),"-",as.character(value))) %>%
      mutate(value = as.character(value)) %>%
      select(`样本编码`,item,value,result)
  }else {
    stop("illegal up_down")
  }
  
  born_result <- bornData %>% 
    select(-value) %>% 
    group_by(`样本编码`,result) %>% 
    mutate(`Result` = str_c(item,collapse = ",")) %>%
    ungroup() %>% 
    select(-item) %>%
    distinct() %>%
    filter(result != "pass") %>%
    mutate(`Result` = str_c(`Result`,result,sep = "")) %>%
    group_by(`样本编码`) %>%
    mutate(`结果` = str_c(Result,collapse = ",")) %>% ungroup() %>% 
    select(-result,-Result) %>% distinct()
  
  bornTotal <- bornData %>% select(-result) %>%
    pivot_wider(names_from = "item",values_from = "value") %>%
    left_join(born_result,by = "样本编码") %>%
    # mutate(`分析` = if_else(is.na(`结果`),"本次检测结果未见明显异常","")) %>%
    mutate(`分析` = "本次检测结果未见明显异常") %>%
    mutate(`结果` = if_else(is.na(`结果`),"本次检测结果未见明显异常",`结果`))
  
  addBornRange <- rangeData %>% 
    select(-Index) %>%
    pivot_longer(cols = -item,names_to = "样本编码",values_to = "value") %>%
    mutate(value = as.character(value)) %>%
    pivot_wider(names_from = "item",values_from = "value") %>%
    mutate(`结果` = "",`分析` = '') %>% 
    select(colnames(bornTotal))
  
  born_return <- addBornRange %>% bind_rows(bornTotal)
  return(born_return)
}

#' intermedResult
#' @description generate both newborn and non-newborn intermed result
#' @export
#' @importFrom read_excel read_tsv
#' @importFrom select filter mutate rename
#' @importFrom pivot_longer pivot_wider
#' @importFrom left_join
#' @param msDataFile path and file name contains ms data
#' @param referFle path and file name contains reference range
#' @param yearInfoFile path and file name contains samples' year info
#' @param dataType waters ,daojin ,sciex
#' @param digits significant digit of values
#' @param up_down up,down,all,non
#' @param saveDir where to save the files
intermedReturn <- function(msDataFile,
                           referFile,
                           yearInfoFile,
                           dataType,
                           digits,
                           up_down,
                           saveDir) {
  
  #read ms data
  if (dataType == "daojin") {
    msData <- read_excel(path = msDataFile,sheet = 1) %>%
      rename(c("sampleId" = "...1")) %>% 
      select(-`Date Acquired`,-`Tray:Vial`) %>%
      filter(!str_detect(sampleId,"Criteria")) %>%
      mutate_at(.vars = -1,as.numeric)
  }else if (dataType == "sciex") {
    msData <- read_excel(path = msDataFile,sheet = 1,col_names = T,skip = 2) %>% 
      filter(!is.na(`Sample Name`)) %>%
      select(-`File Name`,-`Sample Index`,-`Sample Type`,-`Comment`,-`Failed Tests`) %>%
      rename(c("sampleId" = "Sample Name")) %>% 
      mutate_if(.predicate = is.numeric,.funs = as.character) %>%
      pivot_longer(cols = -sampleId,names_to = "item",values_to = "value") %>%
      mutate(value = ifelse(value == "No data",NA,value)) %>%
      mutate(item = str_replace(item,"HD-","")) %>%
      mutate(item = str_replace(item,"\\(Import 2\\)","")) %>%
      mutate(value = as.numeric(value)) %>%
      pivot_wider(names_from = "item",values_from = "value")
  }else if (dataType == "waters") {
    msData<-read_tsv(file = msDataFile) %>% 
      rename(c("sampleId"="X1")) %>% 
      mutate_at(.vars = -1,as.numeric)
    # msData <- read_excel(path = msDataFile) %>%
    #   select(`File Name`,`Test Name`,`Calculated Conc`) %>%
    #   rename(c("sampleId" = "File Name","item" = "Test Name","value" = "Calculated Conc")) %>%
    #   filter(!is.na(sampleId)) %>%
    #   pivot_wider(names_from = "item",values_from = "value") %>%
    #   mutate_at(.vars = -1,as.numeric)
  }else{
    stop("illegal dataType")
  }
  #read reference data
  reference <- read_excel(path = referFile,sheet = 1,skip = 3,
                          col_names = c("Index","item","newBornLow","newBornUp","unNewBornLow","unNewBornUp")) %>%
    mutate(item = if_else(item == "C18:2-OH","C18:2OH",
                          if_else(item == "C18:1-OH","C18:1OH",
                                  if_else(item == "C16:1-OH","C16:1OH",item))))
  totalItem <- reference %>% select(item)
  detectItem <- totalItem %>% filter(!str_detect(string = item,pattern = "/"))
  #read sample year info
  yearInfo <- read_excel(path = yearInfoFile,sheet = 1) %>%
    filter(!is.na(`样本编码`),!is.na(`出生日期`),!is.na(`采血日期`)) %>%
    mutate(age = difftime(as.Date(`采血日期`),as.Date(`出生日期`),units = "days")) %>%
    select(`样本编码`,age)
  #calculate ratio
  # if (is.integer(digits)) {
  #   
  # }
  msData <- msData %>%
    # mutate(`C18:2-OH` = `C18:2OH`,`C18:1-OH` = `C18:1OH`,`C16:1-OH` = `C16:1OH`) %>%
    # select(-`C18:2OH`,-`C18:1OH`,-`C16:1OH`) %>% 
    pivot_longer(cols = -sampleId,names_to = "item",values_to = "value") %>%
    mutate(item = if_else(item == "C18:2-OH","C18:2OH",
                          if_else(item == "C18:1-OH","C18:1OH",
                                  if_else(item == "C16:1-OH","C16:1OH",item)))) %>%
    mutate(item = str_replace(item,"( )+","")) %>%
    left_join(detectItem,.,by = "item") %>% 
    #filter(!is.na(value)) %>% 
    mutate(value = round(as.numeric(value),digits)) %>%
    pivot_wider(names_from = "item",values_from = "value") %>%
    mutate(`(C16+C18:1)/C2` = round((`C16` + `C18:1`)/`C2`,digits)) %>%
    mutate(`C0/(C16+C18)` = round(`C0`/(`C16`+`C18`),digits)) %>%
    mutate(`C14:1/C8:1` = round(`C14:1`/`C8:1`,digits),
           `C8/C2` = round(C8/C2,digits),
           `C5OH/C8`= round(C5OH/C8,digits),
           `C5OH/C3` = round(C5OH/C3,digits),
           `C5OH/C2` = round(C5OH/C2,digits),
           `C5DC/C8` = round(C5DC/C8,digits),
           `C5/C2` = round(C5/C2,digits),
           `C4/C2` = round(C4/C2,digits),
           `C3/C2` = round(C3/C2,digits),
           `C3/C0` = round(C3/C0,digits),
           `Phe/Tyr` = round(Phe/Tyr,digits),
           `Orn/Cit` = round(Orn/Cit,digits),
           `Met/Phe` = round(Met/Phe,digits),
           `Leu/Phe` = round(Leu/Phe,digits),
           `Gly/Phe` = round(Gly/Phe,digits),
           `Gly/Ala` = round(Gly/Ala,digits),
           `Cit/Arg` = round(Cit/Arg,digits)) 
  #generate intermediate csv
  newBornRange <- reference %>% 
    select(Index,item,newBornLow,newBornUp) %>% 
    rename(c("low" = "newBornLow","up" = "newBornUp"))
  unNewBornRange <- reference %>% 
    select(Index,item,unNewBornLow,unNewBornUp) %>%
    rename(c("low" = "unNewBornLow","up" = "unNewBornUp"))
  newborn <- yearInfo %>% filter(age>=0,age<=28)
  unNewBorn <- yearInfo %>% filter(age>28)
  
  #write csv
  up_down = up_down
  if (nrow(newborn) == 0) {
    newBorn_return <- NULL
  } else {
    newBorn_return <- .bornResult(born = newborn,msData = msData,rangeData = newBornRange,up_down = up_down)
  }
  if (nrow(unNewBorn) == 0) {
    unNewBorn_return <- NULL
  } else {
    unNewBorn_return <- .bornResult(born = unNewBorn,msData = msData,rangeData = unNewBornRange,up_down = up_down)
  }
  # write_excel_csv(newBorn_return,path = paste(saveDir,"newborn.csv",sep = ""))
  # write_excel_csv(unNewBorn_return,path = paste(saveDir,"nonnewborn.csv",sep = ""))
  return(intermedResult = list(newBorn_return = newBorn_return,unNewBorn_return = unNewBorn_return))
}
