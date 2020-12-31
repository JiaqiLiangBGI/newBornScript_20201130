# library(tidyverse)
# library(readxl)
# library(rmarkdown)
# library(xfun)
# 
# setwd("D:\\newbornpdf\\report\\scr")
# reference <- 'D:\\Users\\wtbxs\\Documents\\work\\NewBornScreening\\NBSreport\\prepare\\range_annotation_202011.xlsx'
# translation_table <- "./prepare/translation.xlsx"
# calculate_data <- "./prepare/newborn.csv"
# clinical_info <- "./prepare/clinical_info.xlsx"

# set default param
# referenceIndicatorFile = "D:\\newbornpdf\\report\\prepare\\range_annotation_202011.xlsx"
# rowNumber = 36
# translationFile = "D:\\newbornpdf\\report\\prepare\\translation.xlsx"
# indicatorResultFile = "D:\\newbornpdf\\report\\prepare\\newborn.csv"
# sampleID = 'all'
# clinicalInfoFile = "D:\\newbornpdf\\report\\prepare\\clinical_info.xlsx"
# institution = '贵阳市新生儿疾病筛查中心'
# institutionAddress = '报告单位：贵阳市新生儿疾病筛查中心，地址：贵阳市瑞金南路63号贵阳市妇幼保健院医技楼8楼。'
# outputDir = 'D:\\newbornpdf\\report\\prepare\\'

#' render_to_pdf_report
#' render data to pdf report
#' @import tidyverse
#' @import readxl
#' @import rmarkdown
#' @import xfun
#' @param referenceIndicatorFile the reference indicator file with ordered indicator list
#' @param rowNumber the row number of each column
#' @param translationFile the Chinese-English reference file
#' @param indicatorResultFile the indicator result
#' @param sampleID the sample id to generate pdf report, default all
#' @param institution the institution name string, default GuiYang
#' @param institutionAddress the institution address string, default GuiYang
#' @param outputDir the output directory, default ./
#' @return 1 if normally end
render_to_pdf_report <- function(referenceIndicatorFile = reference, 
                                 rowNumber = 36,
                                 translationFile = translation_table,
                                 indicatorResultFile = calculate_data,
                                 sampleID = 'all',
                                 clinicalInfoFile = clinical_info,
                                 institution = '贵阳市新生儿疾病筛查中心',
                                 institutionAddress = '报告单位：贵阳市新生儿疾病筛查中心，地址：贵阳市瑞金南路63号贵阳市妇幼保健院医技楼8楼。',
                                 outputDir = './',
                                 detectorFile,
                                 assesserFile){
  
  # read reference to get indicators list (and their order)
  reference_tibble <- read_xlsx(path=referenceIndicatorFile, sheet = 1, col_names = F, skip = 3) %>%
    as_tibble() 
  
  # reverse the order
  reference_indicator <- reference_tibble %>% pull(...2) %>% rev()
  # cut into 2 column
  reference_indicator_column1 <- reference_indicator[seq(1,rowNumber)]
  reference_indicator_column2 <- reference_indicator[seq(rowNumber,length(reference_indicator))]
  
  # read Chinese-English ref
  translation_info <- read_xlsx(translationFile, sheet = 1) %>%
    as_tibble() %>%
    mutate_at("unit", ~if_else(is.na(.), "", .))
  
  # combine Chinese-English ref and indicators list
  reference_indicator_column1_final <- translation_info %>%
    filter(indicator %in% reference_indicator_column1)
  reference_indicator_column2_final <- translation_info %>%
    filter(indicator %in% reference_indicator_column2)
  
  # calculate_data <- "./prepare/imresult2.csv"
  totalData <- read_csv(indicatorResultFile,
                        col_types = cols(.default = col_character())) %>%
    as_tibble() %>%
    mutate_all(~if_else(is.na(.), "", .))
  
  rangeData <- totalData %>% head(2) %>%
    pivot_longer(names_to = "indicator",
                 values_to = "values",
                 -`样本编码`) %>% filter(!(indicator %in% c("结果", "分析"))) %>%
    pivot_wider(names_from = `样本编码`,values_from = "values") %>%
    mutate(`参考范围` = str_c(low,up,sep = "-")) %>% 
    select(indicator,`参考范围`)
  # rangeData <- totalData %>% head(2) %>%
  #   pivot_longer(names_to = "indicator",
  #                values_to = "values",
  #                -`样本编码`) %>%
  #   select(-`样本编码`) %>%
  #   filter(!(indicator %in% c("结果", "分析"))) %>%
  #   group_by(indicator) %>%
  #   summarise(`参考范围`=str_c(sort(`values`),collapse = "-"), .groups = "drop")
    
  sampleData <- totalData %>% tail(-2) %>%
    pivot_longer(names_to = "indicator",
                 values_to = "values",-`样本编码`) %>%
    filter(!(indicator %in% c("结果", "分析"))) %>%
    mutate(`样本编码` = map_chr(.x = `样本编码`,.f = ~if_else(str_detect(.x,"-"),str_split(.x,"-")[[1]][2],.x)))
    
  column1_final <- left_join(reference_indicator_column1_final, sampleData, by = "indicator") %>%
    left_join(., rangeData, by = c("indicator"))
  column2_final <- left_join(reference_indicator_column2_final, sampleData, by = "indicator") %>%
    left_join(., rangeData, by = c("indicator")) 
  
  analysisData <- sampleData <- totalData %>% tail(-2) %>%
    pivot_longer(names_to = "indicator",
                 values_to = "values",-`样本编码`) %>%
    filter(indicator %in% c("结果", "分析"))
  
  # read in clinical info
  clinicalInfo <- read_xlsx(clinicalInfoFile, sheet = 1, skip = 1,
                            col_names = c("样本编码","母亲姓名","受检者姓名",
                                          "受检者性别","出生日期","采血日期",
                                          "检测日期","联系电话","样本类型",
                                          "送检医院","住院号","送检科室",
                                          "实验编号","临床症状",
                                          "初筛样本编码","初筛结果","初筛结论"),
                            col_types = "text") %>%
    as_tibble() %>%
    pivot_longer(names_to = "clinicalInfo",
                 values_to = "values",
                 -1) %>%
    mutate(`样本编码` = map_chr(.x = `样本编码`,.f = ~if_else(str_detect(.x,"-"),str_split(.x,"-")[[1]][2],.x)))
  
  # data prepare ends
  if(sampleID=='all'){
    for(select_sampleID in unique(clinicalInfo$`样本编码`)){
      select_clinicalData <- clinicalInfo %>%
        filter(`样本编码`==select_sampleID) %>%
        mutate(`样本编码` = str_replace(`样本编码`,"_","\\\\_"),
               `values` = str_replace(`values`,"_","\\\\_")) %>%
        pivot_wider(names_from = "clinicalInfo",
                    values_from = "values") %>%
        mutate(`实验编号` = map_chr(.x = `实验编号`,.f = ~if_else(str_detect(.x,"-"),str_split(.x,"-")[[1]][2],.x)))

      select_column1Data <- column1_final %>%
        filter(`样本编码` == select_sampleID) %>%
        select(-indicator,-`样本编码`) %>%
        mutate(
          result = map_chr(values, ~ str_c(str_extract_all(.x, "[0-9.-]")[[1]],collapse = "")),
          mark = map_chr(values, ~ if_else(str_detect(.x, "[^0-9.-]"),
                                           str_extract(.x, "[^0-9.-]"), ""))
        ) %>% 
        select(-values)
      select_column2Data <- column2_final %>%
        filter(`样本编码` == select_sampleID) %>%
        select(-indicator,-`样本编码`) %>%
        mutate(
          result = map_chr(values, ~ str_c(str_extract_all(.x, "[0-9.-]")[[1]],collapse = "")),
          mark = map_chr(values, ~ if_else(str_detect(.x, "[^0-9.-]"),
                                           str_extract(.x, "[^0-9.-]"), ""))
        ) %>% 
        select(-values) %>%
        add_row(
          name = rep("", rowNumber - nrow(.)),
          unit = rep("", rowNumber - nrow(.)),
          result = rep("", rowNumber - nrow(.)),
          mark = rep("", rowNumber - nrow(.)),
          `参考范围` = rep("", rowNumber - nrow(.))
        )
      
      select_analysisData <- analysisData %>%
        filter(`样本编码`==select_sampleID) %>%
        mutate(`样本编码` = str_replace(`样本编码`,"_","\\\\_")) %>%
        pivot_wider(names_from = "indicator",
                    values_from = "values")
        # mutate(`结果` = map_chr(.x = `结果`,.f = ~if_else(nchar(.x) < 45,str_pad(.x,width = 1,side = "right",pad = "\n "),.x)),
        #        `分析` = map_chr(.x = `分析`,.f = ~if_else(nchar(.x) < 45,str_pad(.x,width = 1,side = "right",pad = "\n "),.x)))
     resultWidth <- nchar(select_analysisData$`结果`,type = "width")
     analysisWidth <- nchar(select_analysisData$`分析`,type = "width")
     if (resultWidth > 174) {
       if (analysisWidth > 84) {
         warning(paste0(paste(select_clinicalData$母亲姓名,select_sampleID,sep = "-"),",beyond the page"))
       }
     } else if (resultWidth > 84) {
       if (analysisWidth > 174) {
         warning(paste0(paste(select_clinicalData$母亲姓名,select_sampleID,sep = "-"),",beyond the page"))
       } else if (analysisWidth <= 84) {
         select_analysisData$`分析` <- paste0(select_analysisData$`分析`,"\\")
       }
     } else {
       if (analysisWidth <= 84) {
         select_analysisData$`分析` <- paste0(select_analysisData$`分析`,"\\")
         select_analysisData$`结果` <- paste0(select_analysisData$`结果`,"\\")
       } else if (analysisWidth <= 174) {
         select_analysisData$`结果` <- paste0(select_analysisData$`结果`,"\\")
       }
     }
       render_single_pdf(select_clinicalData,
                        select_column1Data,
                        select_column2Data,
                        select_analysisData,
                        institution,
                        institutionAddress,
                        paste0(paste(select_clinicalData$母亲姓名,select_sampleID,sep = "-"), ".pdf"),
                        outputDir,detectorFile,assesserFile)
    }
  }else if(sampleID %in% clinicalInfo$`样本编码`){
    # sampleID <- '1-A-1-2'
    select_clinicalData <- clinicalInfo %>%
      filter(`样本编码`==sampleID) %>%
      mutate(`样本编码` = str_replace(`样本编码`,"_","\\\\_"),
             `values` = str_replace(`values`,"_","\\\\_")) %>%
      pivot_wider(names_from = "clinicalInfo",
                  values_from = "values") %>%
      mutate(`实验编号` = map_chr(.x = `实验编号`,.f = ~if_else(str_detect(.x,"-"),str_split(.x,"-")[[1]][2],.x)))
    
    select_column1Data <- column1_final %>%
      filter(`样本编码` == sampleID) %>%
      select(-indicator,-`样本编码`) %>%
      mutate(
        result = map_chr(values, ~ str_c(str_extract_all(.x, "[0-9.-]")[[1]],collapse = "")),
        mark = map_chr(values, ~ if_else(str_detect(.x, "[^0-9.-]"),
                                         str_extract(.x, "[^0-9.-]"), ""))
      ) %>% 
      select(-values)
    select_column2Data <- column2_final %>%
      filter(`样本编码` == sampleID) %>%
      select(-indicator,-`样本编码`) %>%
      mutate(
        result = map_chr(values, ~ str_c(str_extract_all(.x, "[0-9.]")[[1]],collapse = "")),
        mark = map_chr(values, ~ if_else(str_detect(.x, "[^0-9.]"),
                                         str_extract(.x, "[^0-9.]"), ""))
      ) %>% 
      select(-values) %>%
      add_row(
        name = rep("", rowNumber - nrow(.)),
        unit = rep("", rowNumber - nrow(.)),
        result = rep("", rowNumber - nrow(.)),
        mark = rep("", rowNumber - nrow(.)),
        `参考范围` = rep("", rowNumber - nrow(.))
      )

    select_analysisData <- analysisData %>%
      filter(`样本编码`==sampleID) %>%
      mutate(`样本编码` = str_replace(`样本编码`,"_","\\\\_")) %>%
      pivot_wider(names_from = "indicator",
                  values_from = "values") 
      # mutate(`结果` = map_chr(.x = `结果`,.f = ~if_else(nchar(.x) < 45,str_pad(.x,1,side = "right",pad = "\n "),.x)),
      #        `分析` = map_chr(.x = `分析`,.f = ~if_else(nchar(.x) < 45,str_pad(.x,1,side = "right",pad = "\n "),.x)))
    resultWidth <- nchar(select_analysisData$`结果`,type = "width")
    analysisWidth <- nchar(select_analysisData$`分析`,type = "width")
    if (resultWidth > 174) {
      if (analysisWidth > 84) {
        warning(paste0(paste(select_clinicalData$母亲姓名,sampleID,sep = "-"),",beyond the page"))
      }
    } else if (resultWidth > 84) {
      if (analysisWidth > 174) {
        warning(paste0(paste(select_clinicalData$母亲姓名,sampleID,sep = "-"),",beyond the page"))
      } else if (analysisWidth <= 84) {
        select_analysisData$`分析` <- paste0(select_analysisData$`分析`,"\\")
      }
    } else {
      if (analysisWidth <= 84) {
        select_analysisData$`分析` <- paste0(select_analysisData$`分析`,"\\")
        select_analysisData$`结果` <- paste0(select_analysisData$`结果`,"\\")
      } else if (analysisWidth <= 174) {
        select_analysisData$`结果` <- paste0(select_analysisData$`结果`,"\\")
      }
    }
    render_single_pdf(select_clinicalData,
                      select_column1Data,
                      select_column2Data,
                      select_analysisData,
                      institution,
                      institutionAddress,
                      paste0(paste(select_clinicalData$母亲姓名,sampleID,sep = "-"), ".pdf"),
                      #paste0(sampleID, ".pdf"),
                      outputDir,detectorFile,assesserFile)
  }else{
    stop(paste0("没有找到",sampleID,"的数据！"))
  }
}
  
#' render_single_pdf
#' render data to pdf report
#' @import tidyverse
#' @import readxl
#' @import rmarkdown
#' @import xfun
#' @param select_clinicalData the clinical information tibble
#' @param select_column1Data the data in the first column
#' @param select_column2Data the data in the second column
#' @param select_analysisData the data of analysis result
#' @param institution the institution name string, default GuiYang
#' @param institutionAddress the institution address string, default GuiYang
#' @param fileName the output file name
#' @param outputDir the output directory, default ./
#' @return 1 if normally end
render_single_pdf <- function(select_clinicalData,
                              select_column1Data,
                              select_column2Data,
                              select_analysisData,
                              institution,
                              institutionAddress,
                              fileName,
                              outputDir,detectorFile,assesserFile,
                              format = 'pdf_document'){
  # rangeData <- totalData %>% head(2)
  # sampleData <- totalData %>% tail(-2) %>% head(1)
  # clinicalData <- clinical_info %>% filter(`样本编码`== sampleData$`样本编码`)
  # totalData <- left_join(sampleData, clinicalData, by=c("样本编码"))
  # institution <- '贵阳市新生儿疾病筛查中心'
  # institution_address <- '报告单位：贵阳市新生儿疾病筛查中心，地址：贵阳市瑞金南路63号贵阳市妇幼保健院医技楼8楼。'
  # pandoc_latex_engine_args("xelatex")
  # render("./rmarkdown2pdf_cn.Rmd", "pdf_document", params = pandoc_latex_engine_args("xelatex"))
  ne <- new.env()
  ne$select_clinicalData <- select_clinicalData
  ne$select_column1Data <- select_column1Data
  ne$select_column2Data <- select_column2Data
  ne$select_analysisData <- select_analysisData
  ne$institution <- institution
  ne$institutionAddress <- institutionAddress
  ne$detectorFile <- detectorFile
  ne$assesserFile <-assesserFile
  Rscript_call(
    rmarkdown::render,
    list(input = "./rmarkdown2pdf_cn.Rmd", 
         output_format = format,
         output_dir = outputDir,
         output_file = fileName,
         envir = ne)
  )
}
  
# ne <- new.env()
# ne$select_clinicalData <- select_clinicalData
# ne$select_column1Data <- select_column1Data
# ne$select_column2Data <- select_column2Data
# ne$select_analysisData <- select_analysisData
# ne$institution <- institution
# ne$institutionAddress <- institutionAddress
# 
# save(select_clinicalData,
#      select_column1Data,
#      select_column2Data,
#      select_analysisData,
#      institution,
#      institutionAddress,
#      file = "temp_env.Rdata")






