        library(tidyverse)
library(ggplot2)
library(stringr)
library(reshape2)
library(shiny)
library(htmlwidgets)
library(highcharter)

#setwd('I:\\Mapeamento\\MapBiomas\\ACURACIA_GERAL\\Shinny\\Pt')        
        
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

template = htmlTemplate("template.html", 
                        acctotal = highchartOutput("acctotal"),
                        user = highchartOutput("user"),
                        producer = highchartOutput("producer"),
                        cl_user = highchartOutput("cl_user"),
                        cl_producer = highchartOutput("cl_producer"))

server <- function(input, output) {
  data = eventReactive(input$dataButton, {
    b = input$bioma
    f = "integracao"
    n = input$nivel
    getdata(f, input$nivel, input$bioma)
  })
  
  output$user = renderHighchart({plot.confusion(data(), year = input$select_ano, type = "user")})
  output$producer = renderHighchart({plot.confusion(data(), year = input$select_ano, type = "producer")})
  output$acctotal = renderHighchart({dat = data(); plot.accuracy(data())})
  
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
  
  observe({input$select_classe_ctlr;
    if (!is.null(input$select_classe)) {
      output$cl_user = renderHighchart({plot.class.accuracy((data()), class =input$select_classe, type = "user")})
      output$cl_producer = renderHighchart({plot.class.accuracy((data()), class = input$select_classe, type = "producer")})
    } 
  })
 
}

shinyApp(ui = template, server = server)
