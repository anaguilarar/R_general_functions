# Title     : TODO
# Objective : TODO
# Created by: AAguilar
# Created on: 1/30/2020
library(ggplot2)
library(rcompanion)
library(agricolae)
library(multcompView)
theme_Publication = function(base_size=14, base_family="helvetica", xangle = 0, xhjust = 0.5, 
                             legendposition = "bottom", legendsize = 10,legendtitlesize = 12, legenddirection = "horizontal") {
  library(grid)
  library(ggthemes)
  
  (theme_foundation(base_size=base_size,
                    base_family=base_family)+ theme(
                      plot.title = element_text(face = "bold",
                                                size = rel(1.2), hjust = 0.5),
                      text = element_text(),
                      panel.background = element_rect(colour = NA),
                      plot.background = element_rect(colour = NA),
                      panel.border = element_rect(colour = NA),
                      axis.title = element_text(face = "bold",size = rel(1)),
                      axis.title.y = element_text(angle=90,vjust =2),
                      axis.title.x = element_text(vjust = -0.2),
                      axis.text.x = element_text(angle = xangle,hjust =xhjust),
                      
                      axis.line = element_line(colour="black"),
                      axis.ticks = element_line(),
                      panel.grid.major = element_line(colour="#f0f0f0"),
                      panel.grid.minor = element_blank(),
                      legend.key = element_rect(colour = NA),
                      legend.position = legendposition,
                      legend.direction = legenddirection,
                      legend.key.size= unit(0.2, "cm"),
                      legend.margin = unit(0, "cm"),
                      legend.title = element_text(face="italic",size = legendtitlesize),
                      legend.text = element_text(size = legendsize),
                      plot.margin=unit(c(10,5,5,5),"mm"),
                      strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                      strip.text = element_text(face="bold")
                    ))
  
}


grafica_Numerica = function(eventos , x_variable , y_variable,
         name_x_axes="",name_y_axes ="", colorby = NA, xangle = 0, xhjust = 0.5, linear = T){
  ## asignar el nombre de los labels
  if (name_x_axes == "")
    name_x_axes = x_variable
  if (name_y_axes == "")
    name_y_axes = y_variable
  if (!is.na(colorby)){
    ggplot(eventos, aes(eventos[,x_variable],eventos[,y_variable]))+geom_point(aes(col = eventos[,colorby]))+
      geom_smooth(se = FALSE)+
      labs(y = name_y_axes , x = name_x_axes)+theme_Publication()
  }else if(linear){
    ms = caret::R2(obs = eventos[,y_variable], pred = eventos[,x_variable])

    grap = ggplot(eventos, aes(eventos[,x_variable],eventos[,y_variable]))+
      geom_point()+
      geom_smooth(se = FALSE, method = "lm") + 
      labs(title = paste0("R square: ",round(ms, 2)),
           y = name_y_axes , 
           x = name_x_axes)+
      theme_Publication()
    
  }else{
    grap = ggplot(eventos, aes(eventos[,x_variable],eventos[,y_variable]))+geom_point()+
      geom_smooth(se = FALSE)+
      labs(y = name_y_axes , x = name_x_axes)+theme_Publication()
  }
  return(grap)
  
}


grafica_Categorica = function(eventos , x_variable , y_variable,
         name_x_axes="",name_y_axes ="" , kruskal = T, xangle = 90, xhjust = 0, boxwidth = 1){
  
  labels_diff_ = NULL
  
  if(length(levels(factor(as.character(eventos[,x_variable]))))>1 & kruskal){
    
    if (length(levels(factor(as.character(eventos[,x_variable]))))==2){
      
      map_signif_level <- c("****"=0.0001, "***"=0.001, "**"=0.01,  "*"=0.05,"ns" = 1)
      m = wilcox.test(eventos[,y_variable]~factor(as.character(eventos[,x_variable])))
      
      labalhyp = "****"
      
      test = m$p.value
      for(i in 2:length(map_signif_level))
        if(test>map_signif_level[i-1])
          labalhyp = names(map_signif_level)[i]
      
      labels_diff_ = data.frame(cluster = levels(factor(as.character(eventos[,x_variable])))[1] ,
                                label = paste0("p = ",signif(test,digits = 5),"\n",labalhyp))
      
      
    }else{
      eventos = eventos[!is.na(eventos[,x_variable]),]
      
      PT = pairwise.wilcox.test(eventos[,y_variable] , factor(as.character(eventos[,x_variable]))  , p.adjust.method = "BH")
      
      
      PT1 = fullPTable(PT$p.value)
      if(!T %in% is.na(PT1)){
        labels_diff = multcompLetters(PT1,
                                      compare="<",
                                      threshold=0.05,
                                      Letters=letters,
                                      reversed = FALSE)
        
        labels_diff_ = data.frame(cluster = names(labels_diff$monospacedLetters) ,
                                  label = as.character(labels_diff$monospacedLetters))
        print(labels_diff_)
      }else{
        labels_diff_ = NULL
      }
      
    }
    
  }
  xlabs <- paste(levels(factor(eventos[,x_variable])),"\n(N=",table(factor(eventos[,x_variable])),")",sep="")
  
  ## calcular el valor m?ximo de los datos para colocar la letra
  maxs = eventos %>% group_by(eval(parse(text = x_variable))) %>% 
    dplyr::summarize(maxs = max(eval(parse(text = y_variable)), na.rm = T),
                     sdval = sd(eval(parse(text = y_variable)), na.rm = T))
  
  
  graph = ggplot(eventos, aes(eventos[,x_variable],eventos[,y_variable]))+geom_boxplot(width = boxwidth)
  ## asignar el nombre de los labels
  if (is(name_x_axes)[1]=="expression"){
    
  }else{
    if (name_x_axes == ""){
      name_x_axes = x_variable
    }
    if (name_y_axes == ""){
      name_y_axes = y_variable
    }
  }
  graph = graph + labs(y = name_y_axes , x = name_x_axes)+
    scale_x_discrete(labels=xlabs)+theme_Publication(xangle = 0, xhjust = 0.5)
  if(! is.null(labels_diff_)){
    if(length(unique(as.character(labels_diff_$label)))>1){
      
      graph = graph + geom_text(data=data.frame(),
                                aes(label = labels_diff_$label ,
                                    x = labels_diff_$cluster,y=(maxs$maxs+0.5*maxs$sdval)), hjust=0.5)
    }else if(length(unique(as.character(labels_diff_$label))) == 1){
      
      graph = graph + geom_text(data=data.frame(),
                                aes(label = labels_diff_$label ,
                                    x = labels_diff_$cluster,
                                    y=(maxs[maxs[,1][[1]]%in%labels_diff_$cluster,]$maxs + 0.5*maxs[maxs[,1][[1]]%in%labels_diff_$cluster,]$sdval)), hjust=0.5)
    }
  }
  
  return(graph)
}



grafica_rendimiento = function(eventos, y_variable,name_y_axes){
  ggplot(eventos, aes(eventos[,y_variable]))+geom_histogram()+
    labs(x = name_y_axes )+theme_Publication()
}

plot_multiplevariables <- function(data, featureslist, yfeature , ylabel = NA,colorby = NA, outputfolder = "plots"){
  
  for(feature in featureslist){
    dir.create(paste0(outputfolder,"/"), showWarnings = FALSE)
    tipovar = class(data[,feature])
    if (feature == yfeature){
      graphic = grafica_rendimiento(data[!is.na(data[,feature]),],feature,ylabel)
    }else if(tipovar == "factor" | tipovar == "character"){
      graphic = grafica_Categorica(eventos = data[!is.na(data[,feature]),],
                                   x_variable = feature,
                                   y_variable = yfeature,
                                   name_y_axes = ylabel)
    }else if(tipovar == "numeric"|tipovar=="integer"){
      graphic =grafica_Numerica(data[!is.na(data[,feature]),],
                                feature,
                                yfeature,
                                name_y_axes = ylabel,
                                colorby = colorby)
    }
    graphic_name = paste0(paste0(outputfolder,"/"), feature,".jpg")
    
    ggsave(plot = graphic,filename = graphic_name,
           width = 25, height = 12, units = "cm")
    print(paste0(graphic_name, " was created/n"))
    
  }
  
  
}


##### Corplot


significance_diference_labels = function(pvalue, 
                                         labels_sign_values = c("***"=0.001, "**"=0.01,  "*"=0.05,"ns" = 1)){
  
  map_signif_level <- labels_sign_values
  labalhyp = " "
  
  for(i in 1:length(map_signif_level))
    if(pvalue <= map_signif_level[i]){
      labalhyp = names(map_signif_level)[i]
      break
    }
      
  
  return(labalhyp)
  
}


theme_corrplot = function (base_size = 15, base_family = "", flip = FALSE) 
{
  
  res <- theme_grey(base_size = base_size, base_family = base_family) + 
    theme(panel.background = element_rect(fill = "white"), 
          legend.background = element_rect(fill = "white"), 
          legend.position = "right")
  if (flip) {
    res <- res + theme(panel.grid.major.x = element_line(linetype = "dotted", 
                                                         color = "white"), axis.line.y = element_line(color = "black"))
  }
  else {
    res <- res + theme(panel.grid.major.y = element_line(linetype = "dotted", 
                                                         color = "white"))
  }
  res
}


get_lower_tri<-function(cormat, showdiagonal = FALSE){
  cormat[upper.tri(cormat)] <- NA
  if(!showdiagonal){
    for(i in 1:nrow(cormat)){
      cormat[i,i] <- 0
    }
  }
  
  return(cormat)
}

corplotpaper = function(data, 
         method = "square",
         legend.title = "Pearson's\nCorrelation",
         outline.color = alpha('black',0.01),
         colors = scales::alpha(colorRampPalette(c( "black","white", "black"))(3), 0.8),
         tl.cex = 12,
         tl.col = "black",
         ylabelcol = "gray40",
         tl.srt = 0,
         angleaxis = 0,
         fontsizelabels = 4,
         hjust = 1,
         hjustx = 0,
         sizesigni = 2,
         marginleft=1,
         legendtitlesize=15,
         legendsize = 12,
         add_signficance = TRUE){
  library(scales)
  library(caret)
  
  preProcValues <- preProcess(data, method = c("center", "scale"))
  
  trdata = predict(preProcValues, data)
  
  #corr <- get_lower_tri(m)
  corrdata = cor(trdata)
  mplot = reshape2::melt( get_lower_tri(corrdata), na.rm = TRUE)
  
  
  
  p <- ggplot2::ggplot(data = mplot, mapping = ggplot2::aes_string(x = "Var1", 
                                                                   y = "Var2", fill = "value"))
  
  if (method == "square") {
    p <- p + ggplot2::geom_tile(color = outline.color)
  }
  
  p = p + ggplot2::scale_fill_gradient2(low = colors[1], 
                                        high = colors[3], mid = colors[2], 
                                        midpoint = 0, 
                                        limit = c(-1,1), 
                                        space = "Lab", name = legend.title)
  
  
  tl.cex = 12
  tl.col = "black"
  tl.srt = 0
  p = p + ggplot2::theme(axis.text.y = ggplot2::element_text(size = tl.cex)) + 
    ggplot2::coord_fixed()
  
  
  p = p+ theme_corrplot() + #scale_color_manual(colors) + 
    labs(x = "", y = '')+theme(#axis.text.x=element_blank(), #remove x axis labels
      #axis.ticks.x=element_blank(), #remove x axis ticks
      axis.text.y=element_blank(),  #remove y axis labels
      axis.ticks.y=element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_text(face="italic",size = legendtitlesize),
      legend.text = element_text(size = legendsize),
      axis.text.x = element_text(size = fontsizelabels*3,
                                 color = ylabelcol,
                                 angle = angleaxis,hjust = hjustx, vjust = 1))
  
  
  for(i in 1:(length(unique(as.character(mplot$Var2))))-1){
    labele = unique(as.character(mplot$Var2))[i]
    p = p + annotate("text", x = labele, y = labele, label = paste0(labele," "), color = ylabelcol, 
                     size = fontsizelabels*1.1, hjust=hjust,
    ) 
  }
  
  label <- round(x = mplot[, "value"], digits = 2)
  testlabel = str_replace(as.character(label),'0','')
  testlabel = as.numeric(testlabel)
  
  if(add_signficance){
    
    library(Hmisc)
    res <- rcorr(as.matrix(trdata))
    
    ressig = reshape2::melt( res$P, na.rm = TRUE)
    ressig = ressig%>%mutate(label = sapply(value, significance_diference_labels))
    
    
    m = mplot %>% mutate(id = paste0(Var1, Var2)) %>% 
      left_join(ressig%>% 
                  mutate(id = paste0(Var1, Var2))%>%
                  dplyr::select(id,value,label), by = 'id') %>%
      mutate(label = str_replace(as.character(paste0(label,'\n')), 'NA', ''))
    
    newlabel = str_replace(paste0(m$label,as.numeric(testlabel)), 'NA', '')
    p = p + ggplot2::geom_text(mapping = ggplot2::aes_string(x = "Var1",y = "Var2"), 
                               label = newlabel, color = "black", size =sizesigni)
    
    
  }else{
    p = p+ 
      ggplot2::geom_text(mapping = ggplot2::aes_string(x = "Var1",y = "Var2"), 
                         label = testlabel, color = "black", size =sizesigni)
  }
  
  
  return(p+
           coord_cartesian( # This focuses the x-axis on the range of interest
             clip = 'off')  +   # This keeps the labels from disappearing
           theme(plot.margin =  margin(l=marginleft,unit= "cm")))
  
}



get_signif_diff = function(df,y_variable,x_variable){
  map_signif_level <- c("****"=0.0001, "***"=0.001, "**"=0.01,  "*"=0.05,"ns" = 1)
  m = wilcox.test(df[,y_variable]~factor(as.character(df[,x_variable])))

  labalhyp = "****"

  test = m$p.value
  for(i in 2:length(map_signif_level))
    if(test>map_signif_level[i-1])
      labalhyp = names(map_signif_level)[i]

  labels_diff_ = data.frame(cluster = levels(factor(as.character(df[,x_variable])))[1] ,
                            label = paste0("p = ",signif(test,digits = 5),"\n",labalhyp))
  return(labels_diff_)
}

get_diff_label = function(df,y_variable,x_variable, p.adjust.method = "BH"){
  
  PT = pairwise.wilcox.test(df[,y_variable] , 
                            factor(as.character(df[,x_variable]))  , p.adjust.method = p.adjust.method)
  
  
  PT1 = fullPTable(PT$p.value)
  if(!T %in% is.na(PT1)){
    labels_diff = multcompLetters(PT1,
                                  compare="<",
                                  threshold=0.05,
                                  Letters=letters,
                                  reversed = FALSE)
    
    labels_diff_ = data.frame(cluster = names(labels_diff$monospacedLetters) ,
                              label = as.character(labels_diff$monospacedLetters))
    if(dim(labels_diff_)[1] == 0){
      uniquexvar = unique(df[,x_variable])
      labels_diff_ = data.frame(label = '')%>%
        mutate(cluster= uniquexvar[1])%>%data.frame()
      if(length(uniquexvar)>1){
        for(i in 2:length(uniquexvar)){
          labels_diff_ = labels_diff_%>%
            rbind(data.frame(label = '')%>%
                    mutate(cluster= uniquexvar[i]))%>%data.frame()
        }
      }
      labels_diff_ = labels_diff_%>%
        dplyr::select(cluster, label)
    }
    
  }else{
    uniquexvar = unique(df[,x_variable])
    labels_diff_ = data.frame(label = '')%>%
      mutate(!!groupby:= uniquexvar[1])%>%data.frame()
    if(length(uniquexvar)>1){
      for(i in 2:length(uniquexvar)){
        labels_diff_ = labels_diff_%>%
          rbind(data.frame(label = '')%>%
                  mutate(!!groupby:= uniquexvar[i]))%>%data.frame()
      }
    }
    labels_diff_ = labels_diff_%>%
      dplyr::select(cluster, label)
  }
  return(labels_diff_)
}


grafica_Categorica_Grupos = function(eventos, x_variable, y_variable,
                                     groupby = NULL,
                                     name_x_axes="",name_y_axes ="", 
                                     kruskal = T, xangle = 90, xhjust = 0,
                                     boxwidth = 1, textsposfactor = 0.5){
  
  labels_diff_ = NULL
  
  if(length(levels(factor(as.character(eventos[,x_variable]))))>1 & kruskal){
    
    if (length(levels(factor(as.character(eventos[,groupby]))))==2){
      
      
      labels_diff_ = eventos%>%
        group_by(!!(sym(x_variable)))%>%
        group_modify(~ get_signif_diff(.x%>%data.frame(),y_variable,groupby))%>%
        data.frame()%>%
        rename(!!groupby:= cluster)
      
      
      
    }else{
      #x_variable = 'feature'
      #df = eventos%>%
      #  filter(feature == 'blue')%>%data.frame()
      eventos = eventos[!is.na(eventos[,x_variable]),]
      labels_diff_ = eventos%>%
        group_by(!!(sym(x_variable)))%>%
        group_modify(~ get_diff_label(.x%>%data.frame(),y_variable,groupby))%>%
        data.frame()%>%
        rename(!!groupby:= cluster)
      
    }
    
  }
  xlabs = eventos%>%
    group_by(!!(sym(x_variable)))%>%
    group_modify(~  data.frame(label = paste(levels(factor(.x%>%pull(groupby)))," (N=",table(factor(.x%>%pull(groupby))),")",sep="")%>%
                                 c()%>%
                                 paste0(collapse = '\n')))%>%
    mutate(label = paste0(!!(sym(x_variable)),'\n',label ))%>%
    pull(label)
  
  #xlabs <- paste(levels(factor(eventos[,x_variable])),"\n(N=",table(factor(eventos[,groupby])),")",sep="")
  
  ## calcular el valor m?ximo de los datos para colocar la letra
  if(length(levels(factor(as.character(eventos[,groupby]))))!=2){
    maxs = eventos %>% group_by(eval(parse(text = x_variable)), eval(parse(text = groupby))) %>% 
      dplyr::summarize(maxs = max(eval(parse(text = y_variable)), na.rm = T),
                       sdval = sd(eval(parse(text = y_variable)), na.rm = T))
    
    names(maxs)[1:2] = c(x_variable,groupby)
    #maxs = maxs%>%
    #  rename(!!x_variable:=names(maxs)[1])
    
  }else{
    maxs = eventos %>% group_by(eval(parse(text = x_variable))) %>% 
      dplyr::summarize(maxs = max(eval(parse(text = y_variable)), na.rm = T),
                       sdval = sd(eval(parse(text = y_variable)), na.rm = T))
  }
  
  graph = eventos%>%
    ggplot(aes(!!(sym(x_variable)), !!(sym(y_variable)), color = !!(sym(groupby))))+geom_boxplot(width = boxwidth)
  ## asignar el nombre de los labels
  if (is(name_x_axes)[1]=="expression"){
    
  }else{
    if (name_x_axes == ""){
      name_x_axes = x_variable
    }
    if (name_y_axes == ""){
      name_y_axes = y_variable
    }
  }
  graph = graph + labs(y = name_y_axes , x = name_x_axes)+
    scale_x_discrete(labels=xlabs)+theme_Publication(xangle = 0, xhjust = 0.5)
  if(! is.null(labels_diff_)){
    print(labels_diff_)
    if(length(levels(factor(as.character(eventos[,groupby]))))!=2){
      labels_diff_[,groupby] = factor(labels_diff_[,groupby] , levels = levels(factor(eventos[,groupby])))
      labels_diff_ = labels_diff_%>%
        left_join(maxs, by = c(x_variable,groupby))
      graph = graph + geom_text(data=labels_diff_,
                                aes(label = label ,
                                    x = !!(sym(x_variable)),y=(maxs+textsposfactor*sdval)), 
                                hjust=0.5,show.legend = FALSE, position = position_dodge(width = 0.8))
    }else{
      
      graph = graph + geom_text(data=labels_diff_,
                                aes(label = label ,
                                    x = !!(sym(x_variable)),y=(maxs$maxs+textsposfactor*maxs$sdval)), 
                                hjust=0.5,show.legend = FALSE, position = position_dodge(width = 0.8))
    }
    
    
    
  }
  
  return(graph)
}
