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
                 "Fumador", "No Fumador","Obeso", "No obeso","Sedentario","No sedentario"),2,6)
    
    
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
                        'Diabetes'=3 ;'Tabaquismo'=4;'Obesidad'=5;'Sedentarismo'=6")
       
        input33<-recode(input3,"'Nacional'=0;'Costa'=1;'Sierra'=2;'Selva'=3")
        
        set2 <-recode(input22, "1='htaxxx_x_100'; 2='cholestt_dico_x_100';
                      3='diabet_dico_x_100';4='fums_dico_x_100';5='imc_dico_x_100';6='sedentarismo'")
                
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
                     'Diabetes'=3 ;'Tabaquismo'=4;'Obesidad'=5;'Sedentarismo'=6")
    
    input33<-recode(input3,"'Nacional'=0;'Costa'=1;'Sierra'=2;'Selva'=3")
    
    set2 <-recode(input22, "1='htaxxx_x_100'; 2='cholestt_dico_x_100';
                  3='diabet_dico_x_100';4='fums_dico_x_100';5='imc_dico_x_100';6='sedentarismo'")
    
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

pv4<-function(input1="Tornasol I",input2="Hipertension",input3="Nacional"){
    #"Tornasol I"
    #"set1 suppor the subsetof midata     
    set1 <-input1
    #Va entrar puro
    etiqet <-c(input2)        
    #Va INPUT2 ingresar como texto y se convertido a numero.
    input22 <-recode(input2, "'Hipertension'=1; 'Dislipidemia'=2;
                     'Diabetes'=3 ;'Tabaquismo'=4;'Obesidad'=5;'Sedentarismo'=6")
    
    input33<-recode(input3,"'Nacional'=0;'Costa'=1;'Sierra'=2;'Selva'=3")
    
    set2 <-recode(input22, "1='htaxxx_x_100'; 2='cholestt_dico_x_100';
                  3='diabet_dico_x_100';4='fums_dico_x_100';5='imc_dico_x_100';6='sedentarismo'")
    
    if(input33==0){
        midata<-subset(au, estudio==set1)
    }
    else {
        midata<-subset(au, estudio==set1 & region==input3)   
    }
    incidents<-aggregate(midata[,set2], list(midata$sexo), mean,na.rm=TRUE)
    
    incidents$x<-round(incidents$x,1)    
    ploty<-ggplot(data=incidents, aes(x=Group.1, y=x)) +
        geom_bar(stat="identity", fill=c("#E69F00", "#0072B2"), width=0.75)+ xlab("")+
        ylab("Prevalencia (%)") + ggtitle("Prevalencia por sexo")+
        geom_text(aes(label=prettyNum(x, big.mark=",")), vjust=4.7, color="white")+
        scale_y_continuous(expand = c(0, 1))+
        theme(axis.text=element_text(size=14), 
              axis.title=element_text(size=14,face="bold"))
    
    return(ploty)
}

pv5<-function(input1="Tornasol I",input2="Hipertension",input3="Nacional"){
    #"Tornasol I"
    #"set1 suppor the subsetof midata     
    set1 <-input1
    #Va entrar puro
    etiqet <-c(input2)        
    #Va INPUT2 ingresar como texto y se convertido a numero.
    input22 <-recode(input2, "'Hipertension'=1; 'Dislipidemia'=2;
                     'Diabetes'=3 ;'Tabaquismo'=4;'Obesidad'=5;'Sedentarismo'=6")
    
    input33<-recode(input3,"'Nacional'=0;'Costa'=1;'Sierra'=2;'Selva'=3")
    
    set2 <-recode(input22, "1='htaxxx_x_100'; 2='cholestt_dico_x_100';
                  3='diabet_dico_x_100';4='fums_dico_x_100';5='imc_dico_x_100'; 6='sedentarismo'")
    
    if(input33==0){
        midata<-subset(au, estudio==set1)
    }
    else {
        midata<-subset(au, estudio==set1 & region==input3)   
    }
    incidents<-aggregate(midata[,set2], list(midata$ciudad2), mean)
       
    incidents$x<-round(incidents$x,1)    
    ploty<-ggplot(data=incidents, aes(x=Group.1, y=x)) +
        geom_bar(stat="identity", fill="lightblue", width=0.75)+ xlab("")+
        ylab("Prevalencia (%)") + ggtitle("Prevalencia por ciudad")+
        geom_text(aes(label=prettyNum(x, big.mark=",")), vjust=3, color="black", size=5)+
        scale_y_continuous(expand = c(0, 1))+
        theme_bw() + theme(axis.title.x = element_text(face="bold", colour="#990000", size=4),
                           axis.text.x  = element_text(angle=45,hjust=1,vjust=1.1, size=13))
    return(ploty)
}


pv6<-function(input1="Tornasol I",input2="Hipertension",input3="Nacional"){
    #"Tornasol I"
    #"set1 suppor the subsetof midata     
    set1 <-input1
    #Va entrar puro
    etiqet <-c(input2)        
    #Va INPUT2 ingresar como texto y se convertido a numero.
    input22 <-recode(input2, "'Hipertension'=1; 'Dislipidemia'=2;
                     'Diabetes'=3 ;'Tabaquismo'=4;'Obesidad'=5;'Sedentarismo'=6")
    
    input33<-recode(input3,"'Nacional'=0;'Costa'=1;'Sierra'=2;'Selva'=3")
    
    set2 <-recode(input22, "1='htaxxx_x_100'; 2='cholestt_dico_x_100';
                  3='diabet_dico_x_100';4='fums_dico_x_100';5='imc_dico_x_100';6='sedentarismo'")
    
    if(input33==0){
        midata<-subset(au, estudio==set1)
    }
    else {
        midata<-subset(au, estudio==set1 & region==input3)   
    }
    incidents<-aggregate(midata[,set2], list(midata$EDUCACION), mean,na.rm=TRUE)
    
    incidents$x<-round(incidents$x,1)    
    
    
    ploty<-ggplot(data=incidents, aes(x=Group.1, y=x)) +
        geom_bar(stat="identity", fill=c("#999999", "#E69F00", "#56B4E9", "#009E73"), width=0.75)+ xlab("")+
        ylab("Prevalencia (%)") + ggtitle("Prevalencia por nivel educativo")+
        geom_text(aes(label=prettyNum(x, big.mark=",")), vjust=2, color="white")+
        scale_y_continuous(expand = c(0, 1))+
        theme(axis.text.x  = element_text(angle=45,hjust=1,vjust=1.1, size=13))
    
    return(ploty)
}


pv7<-function(input1="Tornasol I",input2="Hipertension",input3="Nacional"){
    #"Tornasol I"
    #"set1 suppor the subsetof midata     
    set1 <-input1
    #Va entrar puro
    etiqet <-c(input2)        
    #Va INPUT2 ingresar como texto y se convertido a numero.
    input22 <-recode(input2, "'Hipertension'=1; 'Dislipidemia'=2;
                     'Diabetes'=3 ;'Tabaquismo'=4;'Obesidad'=5;'Sedentarismo'=6")
    
    input33<-recode(input3,"'Nacional'=0;'Costa'=1;'Sierra'=2;'Selva'=3")
    
    set2 <-recode(input22, "1='htaxxx_x_100'; 2='cholestt_dico_x_100';
                  3='diabet_dico_x_100';4='fums_dico_x_100';5='imc_dico_x_100';6='sedentarismo'")
    
    if(input33==0){
        midata<-subset(au, estudio==set1)
    }
    else {
        midata<-subset(au, estudio==set1 & region==input3)   
    }
    incidents<-aggregate(midata[,set2], list(midata$clase_soc), mean,na.rm=TRUE)
    
    incidents$x<-round(incidents$x,1)    
    
    
    ploty<-ggplot(data=incidents, aes(x=Group.1, y=x)) +
        geom_bar(stat="identity", fill=c("#F0E442", "#0072B2", "#D55E00"), width=0.75)+ xlab("")+
        ylab("Prevalencia (%)") + ggtitle("Prevalencia por nivel educativo")+
        geom_text(aes(label=prettyNum(x, big.mark=",")), vjust=2, color="white")+
        scale_y_continuous(expand = c(0, 1))+
        theme(axis.text.x  = element_text(hjust=1,vjust=1.1, size=13))
    
    return(ploty)
}

pv8<-function(input1="Tornasol I",input2="Hipertension",input3="Nacional"){
    #"Tornasol I"
    #"set1 suppor the subsetof midata     
    set1 <-input1
    #Va entrar puro
    etiqet <-c(input2)        
    #Va INPUT2 ingresar como texto y se convertido a numero.
    input22 <-recode(input2, "'Hipertension'=1; 'Dislipidemia'=2;
                     'Diabetes'=3 ;'Tabaquismo'=4;'Obesidad'=5;'Sedentarismo'=6")
    
    input33<-recode(input3,"'Nacional'=0;'Costa'=1;'Sierra'=2;'Selva'=3")
    
    set2 <-recode(input22, "1='htaxxx_x_100'; 2='cholestt_dico_x_100';
                  3='diabet_dico_x_100';4='fums_dico_x_100';5='imc_dico_x_100';6='sedentarismo'")
    
    if(input33==0){
        midata<-subset(au, estudio==set1)
    }
    else {
        midata<-subset(au, estudio==set1 & region==input3)   
    }
    incidents<-aggregate(midata[,set2], list(midata$etario,midata$sexo), mean,na.rm=TRUE)
    
   ploty<-ggplot(data=incidents, aes(x=Group.1, y=x, group=Group.2, colour=Group.2)) +
        geom_line() + geom_point()+xlab("")+
        ylab("Prevalencia (%)") + ggtitle("Prevalencia por edad y sexo")+
        scale_y_continuous(breaks = round(seq(min(0), max(incidents$x), by = 5),1))+
        theme_bw()
    
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

    dataInput3 <- reactive({
    pv4(c(input$dataset),c(input$pato),c(input$region)  
        #input$region)                   
    )    
    })

    dataInput5 <- reactive({
    pv6(c(input$dataset),c(input$pato),c(input$region)  
        #input$region)                   
    )    
    })


    dataInput6 <- reactive({
    pv7(c(input$dataset),c(input$pato),c(input$region)  
        #input$region)                   
    )    
    })

    dataInput7 <- reactive({
    pv8(c(input$dataset),c(input$pato),c(input$region)  
                           
    )    
    })


dataInput4 <- reactive({
    pv5(c(input$dataset),c(input$pato),c(input$region)  
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
    output$plit <- renderPlot({    
    (dataInput4())      
    })

    output$mabel <- renderPlot({    
    (dataInput5())      
    })
    output$ysamar <- renderPlot({    
    (dataInput6())      
    })
    output$francis <- renderPlot({    
    (dataInput7())      
    })

    output$plat <- renderPlot({    
    (dataInput3())      
                            })
})



