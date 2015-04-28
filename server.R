require(car)# para recode
library(reshape)#no usado, but i´ll be apply for data for barplot here
library(ggplot2)
library(dplyr)
library(scales)
library(dplyr)
load("tornasol.RData")


shinyServer(function(input, output) {

et<-matrix(c("Hipertenso", "No Hipertenso","Dislipidemico", 
                 "No Dislipidemico","Diabetico", "No Diabetico",
                 "Fumador", "No Fumador","Obeso", "No obeso"),2,5)
    
    
    blank_theme <- theme_minimal()+
        theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            plot.title=element_text(size=14, face="bold")
        )
    
    pv2<-function(input1=1,input2="Hipertension",input3="Nacional"){
        #"Tornasol I"
        #"set1 suppor the subsetof midata     
        set1 <-input1
        #Va entrar puro
        etiqet <-c(input2)        
        #Va INPUT2 ingresar como texto y se convertido a numero.
        input22 <-recode(input2, "'Hipertension'=1; 'Dislipidemia'=2;
                        'Diabetes'=3 ;'Tabaquismo'=4;'Obesidad'=5")
       
        input33<-recode(input3,"'Nacional'=0;'Costa'=1;'Sierra'=2;'Selva'=3")
        
        set2 <-recode(input22, "1='htaxxx_x_100'; 2='cholestt_dico_x_100';
                      3='diabet_dico_x_100';4='fums_dico_x_100';5='imc_dico_x_100'")
                
        if(input33==0){
            midata<-subset(au, estudio==set1)
        }
        else {
            midata<-subset(au, estudio==set1 & region==input3)   
        }
        p<-mean(midata[,set2],na.rm=TRUE)
        dg1<-c(p,100-p)
        df <- data.frame(
            group = et[,input22],
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
    
pv3<-function(input1="Tornasol I",input2="Hipertension",input3="Nacional"){
    #"Tornasol I"
    #"set1 suppor the subsetof midata     
    set1 <-input1
    #Va entrar puro
    etiqet <-c(input2)        
    #Va INPUT2 ingresar como texto y se convertido a numero.
    input22 <-recode(input2, "'Hipertension'=1; 'Dislipidemia'=2;
                     'Diabetes'=3 ;'Tabaquismo'=4;'Obesidad'=5")
    
    input33<-recode(input3,"'Nacional'=0;'Costa'=1;'Sierra'=2;'Selva'=3")
    
    set2 <-recode(input22, "1='htaxxx_x_100'; 2='cholestt_dico_x_100';
                  3='diabet_dico_x_100';4='fums_dico_x_100';5='imc_dico_x_100'")
    
    if(input33==0){
        midata<-subset(au, estudio==set1)
    }
    else {
        midata<-subset(au, estudio==set1 & region==input3)   
    }
    incidents<-aggregate(midata[,set2], list(midata$etario), mean,na.rm=TRUE)
    
    incidents$x<-round(incidents$x,1)    
    
    
    ploty<-ggplot(data=incidents, aes(x=Group.1, y=x)) +
        geom_bar(stat="identity", fill="steelblue", width=0.75)+ xlab("Grupos de Edad")+
        ylab("Prevalencia (%)") + ggtitle("Prevalencia por grupos de edad")+
        geom_text(aes(label=prettyNum(x, big.mark=",")), vjust=2, color="white")+
        scale_y_continuous(expand = c(0, 1))
    
    return(ploty)
}


   
    dataInput <- reactive({
        pv2(c(input$dataset),c(input$pato),c(input$region)  
            #input$region)                   
        )    
    })
   
    dataInput2 <- reactive({
     pv3(c(input$dataset),c(input$pato),c(input$region)  
        #input$region)                   
     )    
})
#HELP ME HELP ME me falta hacer pestañas para multiple outpu
    output$plot <- renderPlot({    
                (dataInput())      
                             })

    output$plut <- renderPlot({    
                (dataInput2())      
                             })

})