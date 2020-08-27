

# identifica arquivo para carregar dependendo da fase, bioma, nível e ano
load.data = function(folder, fase=3, bioma="Global", nivel=3) {
  # listas de biomas
  biomas = c("Amazonia", "Caatinga", "Cerrado", "Mata Atlantica", "Pantanal", "Pampa", "Global")
  
  # solta erro caso não encontrado
  if (!(bioma %in% biomas)) {
    stop(paste(c("Bioma inválido. Vaores permitidos são:", 
                 paste(biomas, sep = ";")), collapse = " "))
  }
  
  # fase
  abr_fase = switch(fase,"1" = "cls", "2" = "flt", "3" = "int")
  fase = switch(fase,"1" = "classificação", "2" = "filtro", "3" = "integracao")
  
  if (is.null(fase)) {
    stop("fase deve ser 1 (classification), 2 (classification + t-filter) ou 3 (integration)")
  }
  
  # nível
  nivel = switch(nivel,"1" = "lv1", "2" = "lv2", "3" = "lv3")
  if (is.null(fase)) {
    stop("nivel deve ser 1, 2 ou 3")
  }
  
  # carrega os dados
  name = ifelse(bioma == "Global", "\\%s\\%s\\acc_%s_%s.csv", "\\%s\\%s\\acc_%s_%s.csv")
  
  con = file(file.path(folder, sprintf(name, fase, nivel, abr_fase, bioma)), open = "r")
  dat = readLines(con)
  close(con)
  return(dat)
}

# transforma uma linha em vetor de texto
line2textvec = function(txt) {
  
  l = txt %>% strsplit(";") %>% unlist
  # se tem apenas um elemento, retorna erro
  if (length(l) <= 1) {
    stop("separator not found")
  }
  # formata cada elemento e transforma em vetor
  fun = function(x) {x %>% parse(text = .[]) %>% eval}
  # retorna resultados
  l %>% lapply(FUN = fun) %>% unlist
}

# localiza a linha de um determinado ano
findtime =  function(txtlist, year) {
  found = txtlist %>%
    lapply(FUN = function(x) str_count(x, paste("ANO: ", year, sep=""))==1) %>% 
    unlist
  found = which(found == TRUE)
  found+1
} 

# obtém a matrix de confusão a partir da linha inicial
getconfmat = function(txtlist, year) {
  start.line = findtime(txtlist, year)
  # obtém primeira linha
  l = line2textvec(txtlist[[start.line]])
  # número de linhas
  nlines = length(l)
  # constroi matriz de resultados
  fun = function(x) {line2textvec(txtlist[[x]])}
  mat = start.line:(start.line+nlines-1) %>% lapply(FUN = fun)  %>% unlist %>% matrix(nrow = nlines, ncol=nlines, byrow=TRUE)
  tab = mat[2:nlines, 1:nlines] %>% as.tibble
  tab[,2:nlines] = apply(tab[,2:nlines], 2, as.numeric)
  colnames(tab) <- mat[1,1:nlines]
  tab
}

# Obtém o offset a partir da primeira linha para uma classe ou estatística
getclassoffset = function(txtlist, class) {
  # primeira linha ma matriz do primeiro ano
  first.confmat = txtlist %>% getconfmat("1985")
  
  # encontra a linha da classe
  class.row = which(first.confmat[,1] == class)
  if (length(class.row) == 0){
    stop("Class not found.")
  }
  class.col = which(names(first.confmat)==class)
  if (length(class.row)==0){
    stop("Class not found.")
  }
  c(row=class.row, col=class.col)
}

# obtém o offset de linha ou coluna de uma estatística
getstatoffset = function(txtlist, stat) {
  # primeira linha ma matriz do primeiro ano
  first.confmat = txtlist %>% getconfmat("1985")
  
  # encontra a linha da classe
  class.row = which(first.confmat[,1]==stat)
  # encontra coluna da estatística
  class.col = which(names(first.confmat)==stat)
  if ((length(class.row)+length(class.col))==0){
    stop("stat not found")
  }
  c(row=class.row, col=class.col)
}

# transforma uma matriz de confusão em uma tabela,
# concatenando as informações de linha e coluna da matriz em um dataframe
confmat2tab = function(confmat) {
  # obtém alguns dados
  nclass = which(confmat$` ` == "total")
  nvar = nrow(confmat) - nclass
  classes = confmat$` `[1:nclass]
  
  # extrai as estatísticas de linha e coluna
  coldata = confmat[(nclass + 1):(nclass + nvar), 2:(nclass + 1)] %>%
    as.matrix %>% t
  rowdata = confmat[1:nclass, (nclass + 1 + 1):(nclass + 1 + nvar)] %>% as.matrix
  dat = cbind(rowdata, coldata)
  
  # cria nomes de colunas com bases nos nomes de linha e coluna originais
  colnm = unlist(confmat[(nclass + 1):(nclass + nvar),1])
  colnm[which(colnm == "total")] = "total_validated"
  colnm[which(colnm == "n")] = "n_validated"
  
  rownm = names(confmat)[(nclass + 1 + 1):(nclass+1+nvar)]
  rownm[which(rownm == "total")] = "total_mapped"
  rownm[which(rownm == "n")] = "n_mapped"
  
  # aplica nomes de colunas
  colnames(dat) = c(rownm,colnm)
  dat = data.frame(classes, dat) %>% as.tibble
  return(dat)
}

# obtém as estimativas de frequências populacionais. ùtil para construir os gráficos de barras da acurácua do usuário e do produtor
get_fractions = function(txtlist, year) {
  tab = txtlist %>% 
    getconfmat(year)
  nclass = which(tab[,1] == "total") - 1
  S = as.matrix(tab[1:nclass, "population"])
  tab = tab[1:nclass, 1:(nclass + 1)]
  Nmat = tab[,2:(nclass + 1)]
  for (i in 1:nclass) {
    Nmat[i,] = as.list(as.numeric(Nmat[i,])*S[i]/sum(Nmat[i,], na.rm = TRUE))
  }
  Nmat[is.nan(as.matrix(Nmat))] = NA
  Nmat = Nmat/sum(Nmat, na.rm = TRUE)
  Nmat = cbind(tab[,1], Nmat)
  names(Nmat)[2:ncol(Nmat)] = str_trim(names(Nmat)[2:ncol(Nmat)])
  Nmat[,1] = str_trim(Nmat[,1])
  return(Nmat)
} 

## obtem dados de acurácia para vários anos no formato de tabela de registros
acc.stats = function(txtlist, years) {
  fun = function(year_x) {
    cbind(year = year_x, getconfmat(txtlist, year_x) %>% confmat2tab())
  }
  years %>% lapply(fun) %>% bind_rows()
}

# gráfico: diagrama de acurácia do usuário e do produtor
plot.confusion = function(txtlist, year = "Todos", type = "user") {
  cores = read.csv("CORES por CLASSE.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
  cores = cores[!(is.na(cores$Classe) | cores$Classe == ""),]
  col = as.character(cores[,2])
  names(col) = cores[,1]
  
  props = get_fractions(txtlist, year) 
  stats = getconfmat(txtlist, year)
  classes = unique(props[,1])
  errorBarsData = data.frame(acc = stats$`user's accuracy`, err = stats$`user stderr`, n = stats$total)
  
  acc_labels = c("xlabel" = "Classe Mapeada", "blabel" = "\" da área classificada como '\" + this.key + \"' é '\" + this.series.name + \"'\"") 
  
  if (type == "producer") {
    props[1:nrow(props), 2:ncol(props)] = t(props[1:nrow(props), 2:ncol(props)])
    acc_label = "Acurácia do Produtor"
    err_label = "Erros de Omissão"
    errorBarsData=data.frame(acc = t(stats[which(stats[,1] == "producer's accuracy"),2:ncol(stats)]),
                             err = t(stats[which(stats[,1] == "prod stdErr"),2:ncol(stats)]),
                             n = t(stats[which(stats[,1] == "total"),2:ncol(stats)]))
    acc_labels = c("xlabel" = "Classe Mapeada", "blabel" = "\" da área de '\" + this.key + \"' foi classificada como '\" + this.series.name + \"'\"")
  }
  errorBarsData =  errorBarsData[1:length(classes),]
  errorBarsData = cbind(class = props[1:length(classes),1], errorBarsData)
  
  for (i in 1:nrow(props)) {
    props[i,2:ncol(props)] = props[i,2:ncol(props)]/sum(props[i,2:ncol(props)], na.rm=TRUE)
  }
  
  props = props %>% melt()
  props = props %>% left_join(errorBarsData, by = c(" "="class"))
  
  props[,1] = factor(props[,1], levels = cores$Classe[seq(length( cores$Classe),1,-1)])
  props[,2] = factor(props[,2], levels = cores$Classe)
  
  props[which(props[,1]!=props[,2]),"value"] = props[which(props[,1]!=props[,2]),"value"]*-1
  
  # cores
  col = data.frame(cls = levels(factor(props$variable))) %>% left_join(cores, by = c("cls"="Classe"))
  
  # tooltip function
  txttooltip =sprintf('function() {return "<strong>" + this.key + "<strong> <br>" +  Math.abs(parseFloat(this.y*100).toFixed(2)) + "%%" + %s}', acc_labels["blabel"])
  txtlabel =  'function() {if (this.value<0) {return \'<span style="fill:red">\' + Math.abs(this.value) + \'</span>\'} else { if (this.value>0) {return \'<span style="fill:blue">\' + this.value + \'</span>\'} else {return this.value} }} '  
  
  # labels
  title.x = ifelse(type == "user", "Classes Mapeadas", "Classes Reais")
  
  p = hchart(props, "bar", hcaes(x = ` `,y = value, group = variable )) %>%
    hc_plotOptions(bar = list(stacking = "normal", fillOpacity = 0),
                   series=list(pointPadding = 0, groupPadding=0),
                   title = list(text = "Acurácia Global")) %>%
    hc_yAxis(tickPositions = seq(-1,1,0.1), 
             labels = list(formatter = JS(txtlabel)),
             plotLines = list(list(label = ",asdalksd", color = "red", width=2, 
                                   value=0,zIndex = 1000, dashStyle = "shortDash"))) %>%
    hc_xAxis(title = list(text = title.x)) %>%
    hc_yAxis(title = list(text = "Valor")) %>%
    hc_tooltip(formatter = JS(txttooltip)) %>%
    hc_colors(as.character(col$cor)) %>%
    hc_exporting(enabled = TRUE)
  
  return(p)
}

plot.confusion.ggplot2 = function(txtlist, year = "Todos", type = "user") {
  cores = read.csv("CORES por CLASSE.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
  col = as.character(cores[,2])
  names(col) = cores[,1]
  
  props = get_fractions(txtlist, year) 
  stats = getconfmat(txtlist, year)
  classes = unique(props[,1])
  errorBarsData = data.frame(acc = stats$`user's accuracy`, err = stats$`user stderr`, n = stats$total)
  
  acc_label = "Acurácia do Usuário"
  err_label = "Erros de Inclusão"
  if (type == "producer") {
    props[1:nrow(props), 2:ncol(props)] = t(props[1:nrow(props), 2:ncol(props)])
    acc_label = "Acurácia do Produtor"
    err_label = "Erros de Omissão"
    errorBarsData=data.frame(acc = t(stats[which(stats[,1] == "producer's accuracy"),2:ncol(stats)]),
                             err = t(stats[which(stats[,1] == "prod stdErr"),2:ncol(stats)]),
                             n = t(stats[which(stats[,1] == "total"),2:ncol(stats)]))
  }
  errorBarsData =  errorBarsData[1:length(classes),]
  errorBarsData = cbind(class = props[1:length(classes),1], errorBarsData)
  
  for (i in 1:nrow(props)) {
    props[i,2:ncol(props)] = props[i,2:ncol(props)]/sum(props[i,2:ncol(props)], na.rm=TRUE)
  }
  
  props = props %>% melt()
  props = props %>% left_join(errorBarsData, by = c(" "="class"))
  props[(length(classes) + 1):nrow(props), c("acc", "err", "n")] = NA
  props[,1] = factor(props[,1], levels = cores$Classe[seq(length( cores$Classe),1,-1)])
  props[,2] = factor(props[,2], levels = cores$Classe)
  
  props[which(props[,1]!=props[,2]),"value"] = props[which(props[,1]!=props[,2]),"value"]*-1
  p = props %>%
    ggplot(aes(x = ` `, y = value, fill = variable)) + 
    geom_col(width = 0.5, color = "white", size = 1) + 
    coord_flip() + 
    scale_y_continuous(breaks = seq(-1,1,0.1), labels = abs(seq(-1,1,0.1))) +
    scale_x_discrete(labels = paste(props[seq(length(classes),1,-1),1],"(n=",props[seq(length(classes),1,-1),"n"], ")", sep= "")) + 
    scale_fill_manual(name = "Classe", values = col) +
    geom_hline(yintercept=0, color = "red", linetype = 2) +
    geom_errorbar(aes(ymin = acc-qnorm(0.975)*err, ymax = acc+qnorm(0.975)*err), color = "gray") + 
    theme(axis.text.x=element_text(colour = c(rep("red", 10), "black", rep("blue", 10)))) + 
    annotation_custom(grob = grid::textGrob(label = err_label, 
                                            gp = grid::gpar(col = "red", cex = 0.8)),
                      xmin = 0.5, xmax = 0.6, ymin = -0.5, ymax = -0.5) + 
    annotation_custom(grob = grid::textGrob(label = acc_label, 
                                            gp = grid::gpar(col = "blue", cex = 0.8)),
                      xmin = 0.5, xmax = 0.6, ymin = 0.5, ymax = 0.5)
  return(p)
}




# plota series temporais
plot.accuracy = function(txtlist, years = 1985:2018) {
  # converte a matriz de confusão para o formato de tabela - função confmat2tab
  fun = function(x) {
    tb = txtlist %>% 
      getconfmat(year = as.character(x)) %>%
      confmat2tab
    
    # adiciona uma coluna com o ano
    cbind(year = x, tb)
  }
  namostras = years %>% lapply(function(x) getconfmat(txtlist, x) %>% 
                                 .[.$` ` == "total", "total"] %>% 
                                 as.numeric() %>% data.frame(n = .,year = x )) %>% bind_rows
  # obtém a matriz de confusão para todos os anos e concatena
  tab = lapply(years, fun) %>% bind_rows
  
  # plota acurácia total e discordâncias
  sub = tab %>% 
    filter(classes=="total") %>% 
    select(year, user.s.accuracy, area.dis, alloc.dis) %>% 
    melt(id.vars = c("year"))
  
  sub2 = sub
  sub2[(sub2$variable == "alloc.dis") | (sub2$variable == "area.dis"),"value" ] = 
    sub2[(sub2$variable == "alloc.dis") | (sub2$variable == "area.dis"),"value" ]*-1
  
  # coluna extra para os tooltips
  sub2 = sub2 %>% left_join(namostras, by = c("year" = "year"))
  
  # english
  enTb = tibble(pt = c("user.s.accuracy", "area.dis", "alloc.dis"),
                en = c("Acurácia", "Discordância de Área", "Discordância de Alocação"))
  sub2 = sub2 %>% left_join(enTb, by = c("variable"="pt")) %>%
    transmute(year, variable = factor(en, levels = enTb$en), score = value, n)
  
  # tooltips string
  txttooltip = 'function() {return "Total # of Samples: " + this.point.n + "<br>" + this.series.name + " :" + Math.abs(parseFloat(this.y).toFixed(3))}'
  
  # highchart
  p = hchart(sub2, "column", hcaes(x = year,y = score,group = variable )) %>%
    hc_plotOptions(column = list(stacking = "normal", fillOpacity = 0),
                   series=list(pointPadding = 0, groupPadding=0)) %>%
    hc_tooltip(formatter = JS(txttooltip)) %>%
    hc_yAxis(title = list(text = "Valor"), tickPositions = seq(-1,1,0.1), 
             labels = list(formatter = JS('function() {return Math.abs(this.value)}'))) %>%
    hc_xAxis(title = list(text = "Ano")) %>%
    hc_colors(c("rgba(27, 147, 91, 0.8)",  "rgba(240, 0, 0, 0.81)", "rgba(235, 227, 5, 0.8)")) %>%
    hc_exporting(enabled = TRUE)
  return(p)
}

###
plot.accuracy.ggplot = function(txtlist, years = 1985:2018) {
  # converte a matriz de confusão para o formato de tabela - função confmat2tab
  fun = function(x) {
    tb = txtlist %>% 
      getconfmat(year = as.character(x)) %>%
      confmat2tab
    
    # adiciona uma coluna com o ano
    cbind(year = x, tb)
  }
  
  # obtém a matriz de confusão para todos os anos e concatena
  tab = lapply(years, fun) %>% bind_rows
  
  # plota acurácia total e discordâncias
  sub = tab %>% 
    filter(classes=="total") %>% 
    select(year, user.s.accuracy, area.dis, alloc.dis) %>% 
    melt(id.vars = c("year"))
  
  sub2 = sub
  sub2[(sub2$variable == "alloc.dis") | (sub2$variable == "area.dis"),"value" ] = 
    sub2[(sub2$variable == "alloc.dis") | (sub2$variable == "area.dis"),"value" ]*-1
  
  p = ggplot(data = sub2, aes(x = year, y = value, fill = variable)) + 
    geom_col(width = 1) + 
    scale_y_continuous(breaks = seq(-1,1,0.1), labels = abs(seq(-1,1,0.1))) + 
    scale_x_continuous(breaks = years) +
    scale_fill_manual(name = "Component", values = c("#39B80B", "#EB4626", "#F7F702")) + 
    theme_bw()+
    expand_limits(y = c(-0.5,1)) + 
    labs(x="Ano", y = "discordância/acurácia")
  return(p)
}

# plota confusões para uma classe em múltiplos anos
plot.class.accuracy = function(txtlist, years = 1985:2018, class, type="user") {
  try({
    fun = function(x) {
      tb = get_fractions(txtlist, year = x) %>% as.tibble()
      nclasses = nrow(tb)
      if (type == "user") {
        tb = tb[,1] %>% cbind(p = t(tb[which(tb[,1] == class), 2:(nclasses + 1)]))
      } else {
        tb = tb[,1] %>%  cbind(p = unlist(tb[1:nclasses, class]))
      }
      # adiciona uma coluna com o ano
      tb$p = tb$p/sum(tb$p, na.rm = TRUE)
      cbind(year = x, tb)
    }
    
    # obtém os dados para todos os anos
    tab = lapply(years, fun) %>% bind_rows
    names(tab)[2] <- "class"
    tab[which(tab$class != class), "p"] = -tab[which(tab$class != class), "p"]
    
    # cores
    cores = read.csv("CORES por CLASSE.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
    col = as.character(cores[,2])
    names(col) = cores[,1]
    
    # cores
    col = data.frame(cls = levels(factor(tab$class))) %>% 
      left_join(cores, by = c("cls"="Classe"))
    
    # labels dependendo se usuário ou produtor
    if (type == "user") {
      acc_labels = c("xlabel" = "Classe Mapeada", "blabel" = sprintf("\" da área mapeada como \'%s\' é \'\" + this.series.name + \"\'\"", class))     
      title = paste("Diagrama da acurácia do Usuário - ", class, sep="")
      ylab = "Erro de Inclusão/Acurácia do Usuário"
    } else {
      acc_labels = c("xlabel" = "Classe Real", "blabel" = sprintf("\" da área de \'%s\' foi classificada como \'\" + this.series.name + \"\'\"", class))     
      title = paste("Diagrama da acurácia do Produtor - ", class, sep="")
      ylab = "Erro de Omissão/Acurácia do Produtor"
    }
    
    # tooltip function
    txttooltip = sprintf('function() {return "<strong>" + this.x + "</strong>: " + Math.abs(parseFloat(this.y*100).toFixed(2)) + "%%" + %s }', acc_labels["blabel"])
    
    # plot
    p = hchart(tab, "column", hcaes(x = year,y = p, group = class )) %>%
      hc_plotOptions(column = list(stacking = "normal", fillOpacity = 0),
                     series=list(pointPadding = 0, groupPadding=0),
                     title = list(text = "Acurácia Global")) %>%
      hc_tooltip(formatter = JS(txttooltip)) %>%
      hc_xAxis(title = list(text = "Ano")) %>%
      hc_yAxis(title = list(text = "Valor"), tickPositions = seq(-1,1,0.1), 
               labels = list(formatter = JS('function() {return Math.abs(this.value)}'))) %>%
      hc_colors(as.character(col$cor)) %>%
      hc_title(text = class) %>%
      hc_exporting(enabled = TRUE)
    
    # p = ggplot(data = tab, aes(x = year, y = p, fill = class)) + 
    #   geom_col(width = 1) + 
    #   scale_y_continuous(breaks = seq(-1,1,0.1), labels = abs(seq(-1,1,0.1))) + 
    #   scale_x_continuous(breaks = years) +
    #   scale_fill_manual(name = "Classes", values = col) +
    #   theme(axis.text.y=element_text(colour = c(rep("red", 10), "black", rep("blue", 10)))) +
    #   expand_limits(y = c(-1,1)) +
    #   labs(x = "Ano", y = ylab, title = title)
    return(p) })
}

plot.class.accuracy.ggplo2 = function(txtlist, years = 1985:2018, class, type="user") {
  fun = function(x) {
    tb = get_fractions(txtlist, year = x) %>% as.tibble()
    nclasses = nrow(tb)
    if (type == "user") {
      tb = tb[,1] %>% cbind(p = t(tb[which(tb[,1] == class), 2:(nclasses + 1)]))
    } else {
      tb = tb[,1] %>%  cbind(p = unlist(tb[1:nclasses, class]))
    }
    # adiciona uma coluna com o ano
    tb$p = tb$p/sum(tb$p, na.rm = TRUE)
    cbind(year = x, tb)
  }
  
  # obtém os dados para todos os anos
  tab = lapply(years, fun) %>% bind_rows
  names(tab)[2] <- "class"
  tab[which(tab$class != class), "p"] = -tab[which(tab$class != class), "p"]
  
  # cores
  cores = read.csv("CORES por CLASSE.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
  col = as.character(cores[,2])
  names(col) = cores[,1]
  
  # labels dependendo se usuário ou produtor
  if (type == "user") {
    title = paste("Diagrama da acurácia do Usuário - ", class, sep="")
    ylab = "Erro de Inclusão/Acurácia do Usuário"
  } else {
    title = paste("Diagrama da acurácia do Produtor - ", class, sep="")
    ylab = "Erro de Omissão/Acurácia do Produtor"
  }
  
  # plot
  p = ggplot(data = tab, aes(x = year, y = p, fill = class)) + 
    geom_col(width = 1) + 
    scale_y_continuous(breaks = seq(-1,1,0.1), labels = abs(seq(-1,1,0.1))) + 
    scale_x_continuous(breaks = years) +
    scale_fill_manual(name = "Classes", values = col) +
    theme(axis.text.y=element_text(colour = c(rep("red", 10), "black", rep("blue", 10)))) +
    expand_limits(y = c(-1,1)) +
    labs(x = "Ano", y = ylab, title = title)
  return(p)
}

# plota série temporal de áreas
plot.areas = function(txtlist, years) {
  tab = txtlist %>% acc.stats(years = years)
  
  # plot
  p = ggplot(data = tab, aes(x = year, y = population)) + 
    geom_col(width = 1) + 
    scale_y_continuous(breaks = seq(-1,1,0.1), labels = abs(seq(-1,1,0.1))) + 
    scale_x_continuous(breaks = years) +
    scale_fill_manual(name = "Classes", values = col) +
    theme(axis.text.y=element_text(colour = c(rep("red", 10), "black", rep("blue", 10)))) +
    expand_limits(y = c(-1,1)) +
    labs(x = "Ano", y = ylab, title = title)
}




