require(car)# para recode
library(reshape)#no usado, but i´ll be apply for data for barplot here
library(ggplot2)
library(dplyr)
library(scales)
library(dplyr)
load("tornasol.RData")

et<-matrix(c("Hipertenso", "No Hipertenso","Dislipidemico", 
             "No Dislipidemico","Diabetico", "No Diabetico",
             "Fumador", "No Fumador","Obeso", "No obeso"),2,5)

shinyServer(function(input, output) {
    
    blank_theme <- theme_minimal()+
        theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            plot.title=element_text(size=14, face="bold")
        )
    
    pv2<-function(input1=1,input2=1,input3=0){
        #"Tornasol I"
        #"set1 suppor the subsetof midata     
        set1 <-input1
        etiqet <-recode(input2, "1='Hipertension'; 2='Dislipidemia';
                        3='Diabetes';4='Tabaquismo';5='Obesidad'")
        set2 <-recode(input2, "1='htaxxx_x_100'; 2='cholestt_dico_x_100';
                      3='diabet_dico_x_100';4='fums_dico_x_100';5='imc_dico_x_100'")
        set3<-recode(input3, "1='Costa'; 2='Sierra';3='Selva'")
        
        if(input3==0){
            midata<-subset(au, estudio==set1)
        }
        else {
            midata<-subset(au, estudio==set1& region==set3)   
        }
        p<-mean(midata[,set2],na.rm=TRUE)
        dg1<-c(p,100-p)
        df <- data.frame(
            group = et[,input2],
            value = round(dg1,1))
        bp<- ggplot(df, aes(x="", y=value, fill=group))+
            geom_bar(width = 1, stat = "identity")
        pie <- bp + coord_polar("y", start=0)
        
        ploty<-pie + scale_fill_brewer("Leyenda") + blank_theme + ggtitle(etiqet)+
            theme(axis.text.x=element_blank())+
            geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                          label = percent(value/100)), size=5)
        
        return(ploty)
    }
    
#AQUI TENGO EL PROPBLEMA PARA  ENVIAR DOS ARGUMENTOS A LA FUNCTION PV2
# HERE THE PROBLEM
#HELP ME HELP ME 
#HELP ME HELP ME 
#HELP ME HELP ME 
#HELP ME HELP ME 
#HELP ME HELP ME 
#HELP ME HELP ME 
     dataInput <- reactive({
            pv2(input$dataset
                  #input$pato),  
                  #input$region)                   
                )    
    })
#HELP ME HELP ME 
    output$plot <- renderPlot({    
                (dataInput())
        
 
    #output$plot <- renderPlot({    
     #   chartSeries(dataInput(), theme = chartTheme("white"), 
      #              type = "line", log.scale = input$log, TA = NULL)
 
    })
})