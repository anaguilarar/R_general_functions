# Title     : TODO
# Objective : TODO
# Created by: AAguilar
# Created on: 1/30/2020
library(ggplot2)
library(rcompanion)
library(agricolae)
library(multcompView)
theme_Publication <- function(base_size=14, base_family="helvetica", xangle = 0, xhjust = 0.5, 
                              legendposition = "bottom", legendsize = 10) {
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
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
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
    ggplot(eventos, aes(eventos[,x_variable],eventos[,y_variable]))+
      geom_point()+
      geom_smooth(se = FALSE, method = "lm") + 
      labs(title = paste0("R square: ",round(ms$r.squared, 2)),
           y = name_y_axes , 
           x = name_x_axes)+
      theme_Publication()
    
  }else{
    ggplot(eventos, aes(eventos[,x_variable],eventos[,y_variable]))+geom_point()+
      geom_smooth(se = FALSE)+
      labs(y = name_y_axes , x = name_x_axes)+theme_Publication()
  }
  
}

grafica_Categorica = function(eventos , x_variable , y_variable,
                              name_x_axes="",name_y_axes ="" , kruskal = T, xangle = 90, xhjust = 0){
  
  
  if(length(levels(factor(eventos[,x_variable])))>1 & kruskal){

    eventos = eventos[!is.na(eventos[,x_variable]),]
    
    PT = pairwise.wilcox.test(eventos[,y_variable] , factor(eventos[,x_variable])  , p.adjust.method = "BH")
    
    
    PT1 = fullPTable(PT$p.value)
    labels_diff = multcompLetters(PT1,
                                  compare="<",
                                  threshold=0.05,
                                  Letters=letters,
                                  reversed = FALSE)
    
    labels_diff_ = data.frame(cluster = names(labels_diff$monospacedLetters) ,
                              label = as.character(labels_diff$monospacedLetters))
    print(labels_diff_)
    
  }else{
    labels_diff = list(Letters = 1)
  }
  xlabs <- paste(levels(factor(eventos[,x_variable])),"\n(N=",table(factor(eventos[,x_variable])),")",sep="")
  
  ## calcular el valor m?ximo de los datos para colocar la letra
  maxs = eventos %>% group_by(eval(parse(text = x_variable))) %>% 
    summarize(maxs = max(eval(parse(text = y_variable)), na.rm = T))
  
  
  graph = ggplot(eventos, aes(eventos[,x_variable],eventos[,y_variable]))+geom_boxplot()
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
  if(length(unique(as.character(labels_diff$Letters)))>1){
    graph = graph + geom_text(data=data.frame(),
                              aes(label = labels_diff_$label ,
                                  x = labels_diff_$cluster,y=(maxs$maxs+.2)), hjust=0.5)
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
