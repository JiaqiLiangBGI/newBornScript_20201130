.bornResult <- function(born,msData,rangeData) {
  
  ref <- rangeData  %>% arrange(desc(Index)) %>% 
    select(item,Index) %>% 
    pivot_wider(names_from = "item",values_from = "Index")
  
  msData <- msData %>% select(sampleId,colnames(ref))
  #new born
  bornData <- born %>%
    left_join(msData,by = c("样本编码" = "sampleId")) %>%
    select(-age) %>%
    pivot_longer(cols = -`样本编码`,names_to = "item",values_to = "value") %>%
    left_join(rangeData,by = "item") %>%
    mutate(result = if_else(value > up,"指标升高",ifelse(value < low,"指标降低","pass"))) %>%
    mutate(value = if_else(result == "指标升高",str_c("↑",as.character(value),sep = ""),
                           if_else(result == "指标降低",str_c("↓",as.character(value),sep = ""),as.character(value)))) %>%
    #mutate(result = str_c(item,result,sep = "")) %>%
    select(`样本编码`,item,value,result)
  
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
    mutate(`分析` = if_else(is.na(`结果`),"本次检测结果未见明显遗传代谢病异常。",""))
  
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

intermedReturn <- function(msDataFile,
                           referFile,
                           yearInfoFile,
                           dataType,
                           digits,
                           saveDir) {
  
  #read ms data
  if (dataType == "daojin") {
    msData <- read_excel(path = msDataFile,sheet = 1) %>%
      rename(c("sampleId" = "...1")) %>% 
      select(-`Date Acquired`,-`Tray:Vial`) %>%
      filter(!str_detect(sampleId,"Criteria")) %>%
      mutate_at(.vars = -1,as.numeric)
  }else if (dataType == "scix") {
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
    msData <- read_excel(path = msDataFile) %>%
      select(`File Name`,`Test Name`,`Calculated Conc`) %>%
      rename(c("sampleId" = "File Name","item" = "Test Name","value" = "Calculated Conc")) %>%
      filter(!is.na(sampleId)) %>%
      pivot_wider(names_from = "item",values_from = "value") %>%
      mutate_at(.vars = -1,as.numeric)
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
  newborn <- yearInfo %>% filter(age>0,age<=28)
  unNewBorn <- yearInfo %>% filter(age>28)
  
  #write csv
  newBorn_return <- .bornResult(born = newborn,msData = msData,rangeData = newBornRange)
  unNewBorn_return <- .bornResult(born = unNewBorn,msData = msData,rangeData = unNewBornRange)
  # write_excel_csv(newBorn_return,path = paste(saveDir,"newborn.csv",sep = ""))
  # write_excel_csv(unNewBorn_return,path = paste(saveDir,"nonnewborn.csv",sep = ""))
  return(intermedResult = list(newBorn_return = newBorn_return,unNewBorn_return = unNewBorn_return))
}