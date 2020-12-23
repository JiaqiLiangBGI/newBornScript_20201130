library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)
#
tranferFormat <- function(file,date) {
  
  #date <- str_replace(as.character(date),"-","")
  tki_data <- read_tsv(file,
                       na = "N/A",
                       col_types = cols(Index = col_double(),
                                        `Sample Name` = col_character(),
                                        `Sample ID` = col_logical(),
                                        `Sample Type` = col_character(),
                                        IS = col_logical(),
                                        `Component Name` = col_character(),
                                        `IS Name` = col_character(),
                                        `Component Group Name` = col_character(),
                                        `Outlier Reasons` = col_character(),
                                        `Actual Concentration` = col_character(),
                                        Area = col_character(),
                                        `IS Area` = col_character(),
                                        Height = col_character(),
                                        `Retention Time` = col_character(),
                                        `Width at 50%` = col_character(),
                                        Used = col_logical(),
                                        `Calculated Concentration` = col_character(),
                                        Accuracy = col_character()))
  #generate first 4 rows
  title <-
    tibble(
      `index` = c(
        "Quantify Compound Summary Report",
        "",
        "Printed Wed Mar 20 08:47:54 2019",
        ""
      ),
      Name = rep("", 4),
      `Sample Text` = rep("", 4),
      `Std.Conc` = rep('', 4),
      RT = rep("", 4),
      Area = rep("", 4),
      `IS Area` = rep("", 4),
      Response = rep("", 4),
      `Conc.` = rep("", 4),
      `%Dev` = rep('', 4)
    )
  
  #total TKI items
  totalItems <- data.frame(compoundId = c("Compound 1","Compound 2",'Compound 3',"Compound 4","Compound 5","Compound 6",'Compound 7',"Compound 8","Compound 9"),
                           items = c('IMA','SUN','SU1226','CRIZ','PAZ','GEF','NIL','AXIT','VEM'),stringsAsFactors = F)
  #select existing items in tki_data
  totalSample <- tki_data %>% filter(!is.na(`IS Name`),`IS Name` != "(No IS)",str_detect(`Sample Name`,"[0-9]{7}"))
  existingItems <- totalItems %>% 
    mutate(existing = str_c(unique(totalSample$`Component Name`),collapse = ";")) %>% 
    filter(str_detect(existing,items))
  
  #generate all items in existingItems dataframe
  generateTotalItems <- existingItems %>%
    mutate(data = map2(.x = items,
                       .y = compoundId,
                       .f = ~generateSingleItem(item = .x,
                                                date = date,
                                                firCol = str_c(.y,.x,sep = ":"),
                                                title = title,
                                                tki_data = tki_data)))
  #bind rows
  finalTKI <- title
  for(i in 1:nrow(generateTotalItems)) {
    finalTKI <- bind_rows(finalTKI,generateTotalItems$data[i])
  }
  return(finalTKI)
}

#write_csv(finalTKI,"./writetest.csv",col_names = FALSE)
#generate single item 
generateSingleItem <- function(item,date,firCol,tki_data,title) {
  
  #generate sample 
  singleItem_sample <- tki_data %>%  
    filter(str_detect(`Component Name`,item), !is.na(`IS Name`),`IS Name` != "(No IS)",str_detect(`Sample Name`,"[0-9]{7}")) %>%
    arrange(`Sample Name`)
  #generate QC 
  singleItem_qc <- tki_data %>% 
    filter(str_detect(`Component Name`,item),
           `Sample Type` == "Quality Control",
           str_detect(`Sample Name`,"QC")) %>%  
    mutate(qcType = str_extract(`Sample Name`,pattern = "[:alpha:]+")) %>%
    filter(!is.na(Accuracy)) %>%
    mutate(selectAccu = abs(as.numeric(Accuracy) - 100)) %>%
    group_by(qcType) %>% 
    filter(selectAccu == min(selectAccu)) %>% ungroup() %>% select(-qcType,-selectAccu) %>% 
    mutate(`Sample Name` = if_else(str_detect(`Sample Name`,"LQC"),"94",
                                   if_else(str_detect(`Sample Name`,"MQC"),"95","96"))) %>% arrange(`Sample Name`)
  #combine sample and QC 
  singleItem <- bind_rows(singleItem_sample,singleItem_qc) %>%
    select(`Sample Name`,`Retention Time`,`Area`,`IS Area`,`Calculated Concentration`) %>% 
    #rename(c("Sample Name" = "Name","Retention Time" = "RT","Calculated Concentration" = "Conc.")) %>%
    rename(c('Name' = "Sample Name","RT" = "Retention Time","Conc." = "Calculated Concentration")) %>%
    mutate(Name = str_c(date,Name,sep = "-"),
           `Sample Text` =  "",`Std.Conc` = "",`Response` = "",`%Dev` = "",
           `index` = as.character(1:(nrow(singleItem_sample) + nrow(singleItem_qc)))) %>%
    select(`index`,Name,`Sample Text`,`Std.Conc`,RT,Area,`IS Area`,Response,`Conc.`,`%Dev`)
  #add first three rows and last one row
  first2Rows <- title[c(1,2),]
  first2Rows[1,1] <- firCol
  thirdRow <- data.frame(index = '',
                         Name = "Name",
                         `Sample Text` = "Sample Text",
                         `Std.Conc` = "Std.Conc",
                         RT = "RT",
                         Area = "Area",
                         `IS Area` = "IS Area",
                         Response = "Response",
                         `Conc.` = "Conc.",
                         `%Dev` = "%Dev",stringsAsFactors = F,check.names = F)
  singleEnd <- title[2,]
  singleTotal <- bind_rows(first2Rows,thirdRow) %>% bind_rows(singleItem) %>% bind_rows(singleEnd)
  
  return(singleTotal)
}
#shiny ui
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "For TKI Data"),
  dashboardSidebar(fileInput(inputId = "TKI_file",label = "Choose a file",accept = c(".txt")),
                   dateInput("date",label = 'Choose a date',
                             startview = "month",format = 'yyyymmdd',
                             language = "zh-CN",autoclose = FALSE),
                   checkboxInput("show","Show Original Data",value = FALSE),
                   checkboxInput("showResult","Show Finished Data",value = FALSE)
                   #downloadButton(outputId = "downloadData",label = "Download")

                   ),
  dashboardBody(fluidRow(box(width = 12,
                             solidHeader = T,
                             status = "primary",downloadButton(outputId = "downloadData",label = "Download")))
                ,fluidRow(box(title = "Data Display Box",
                    status = "primary",
                    width = 12,
                    #height = 600,
                    solidHeader = T,
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('data')))

                    #DT::dataTableOutput("origin")),
                # box(title = "after",
                #     status = "primary",
                #     width = 6,
                #     height = 600,
                #     solidHeader = T,
                #     div(style = 'overflow-x: scroll', DT::dataTableOutput('TKI'))))
                    #DT::dataTableOutput("TKI")))
)))

#shiny server
server <- function(input,output) {
  #inFile <- input$TKI_file
  #output$tempdate <- renderText({format(input$date,"%Y%m%d")})
  output$data <- renderDataTable({
    if (is.null(input$TKI_file)) {
      return(NULL)
    } else if (isTRUE(input$show) && !isTRUE(input$showResult)){
      originData <- read_tsv(input$TKI_file$datapath,
                           na = "N/A",
                           col_types = cols(Index = col_double(),
                                            `Sample Name` = col_character(),
                                            `Sample ID` = col_logical(),
                                            `Sample Type` = col_character(),
                                            IS = col_logical(),
                                            `Component Name` = col_character(),
                                            `IS Name` = col_character(),
                                            `Component Group Name` = col_character(),
                                            `Outlier Reasons` = col_character(),
                                            `Actual Concentration` = col_character(),
                                            Area = col_character(),
                                            `IS Area` = col_character(),
                                            Height = col_character(),
                                            `Retention Time` = col_character(),
                                            `Width at 50%` = col_character(),
                                            Used = col_logical(),
                                            `Calculated Concentration` = col_character(),
                                            Accuracy = col_character()))
      return(originData)
    } else if (!isTRUE(input$show) && isTRUE(input$showResult)) {
      result <- tranferFormat(file = input$TKI_file$datapath,date = format(input$date,"%Y%m%d"))
      return(result)
    } else {
      return(NULL)
    }
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("TKI", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(tranferFormat(file = input$TKI_file$datapath,date = input$date), file, col_names = FALSE)
      #write_csv(result, file, col_names = FALSE)
    }
  )
}
shinyApp(ui,server)