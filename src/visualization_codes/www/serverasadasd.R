
library(tidyverse)
library(ggplot2)
library(stringr)
library(reshape2)
library(shiny)
library(htmlwidgets)
library(highcharter)


# carrega scripts
source("explorar_acuracia.R", encoding = "UTF-8")
# função para criar o nome do arquivo com a partir das partes
getdata = function(fase, nivel, bioma) {
  fs = switch(fase, "classificacao" = "cls", "filtro" = "flt", "integracao" = "int") 
  fname = file.path(fase, nivel, paste("acc_",fs, "_", bioma, ".csv", sep = ""))
  con = file(fname, open = "r", encoding = "ISO-8859-1")
  dat = readLines(con)
  close(con)
  return(dat)
}

server <- function(input, output) {
  output$html = renderText(as.character(includeHTML(path = "template.html")))
  data = eventReactive(input$dataButton, {
    b = input$bioma
    f = "integracao"
    n = input$nivel
    getdata(f, input$nivel, input$bioma)
  })
  
  output$userui = renderUI({
    output$user = renderHighchart({plot.confusion(data(), year = input$select_ano, type = "user")})
    highchartOutput("user")
  })
  
  output$producerui = renderUI({
    output$producer = renderHighchart({plot.confusion(data(), year = input$select_ano, type = "producer")})
    highchartOutput("producer")
  })
  
  output$acctotal = renderHighchart2({dat = data(); plot.accuracy(data())})
  
  output$accvb = reactive({
    acc = getconfmat(data(), "Todos") %>% 
      filter(` ` == "total") %>% .$`user's accuracy` %>% as.numeric()
    acc =   sprintf("%1.1f%%", acc*100)
    acc
  })
  
  output$allocvb = reactive({
    acc = getconfmat(data(), "Todos") %>% 
      filter(` ` == "alloc dis") %>% .$total %>% as.numeric()
    acc =   sprintf("%1.1f%%", acc*100)
    acc
  })
  
  output$areavb = reactive({
    acc = getconfmat(data(), "Todos") %>% 
      filter(` ` == "total") %>% .$`area dis` %>% as.numeric()
    acc =   sprintf("%1.1f%%", acc*100)
    acc
  })
  
  output$select_classe_ctlr = renderUI({
    cls = getconfmat(data(), "Todos")
    #which.hasdata = c(which(cls$total>0 | !is.na(cls$total)), which(cls[cls$` `=="total",]>0 | !is.na(cls[cls$` `=="total",]))) %>% unique()
    which.hasdata = which(cls$total>0 | !is.na(cls$total)) %>% unique()
    which.hasdata = which.hasdata[which.hasdata<which(cls$` `=="total")]
    cls = str_trim(cls$` `[which.hasdata])
    return(selectInput("select_classe", "Classe", choices = cls))
  })
  
  output$cl_user = renderHighchart({plot.class.accuracy(data(), class =input$select_classe, type = "user")})
  output$cl_producer = renderHighchart({plot.class.accuracy(data(), class = input$select_classe, type = "producer")})
}