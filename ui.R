require(car)# para recode
library(shiny)
library(ggplot2)
library(reshape)
library(scales)
library(dplyr)
load("tornasol.RData")
et<-matrix(c("Hipertenso", "No Hipertenso","Dislipidemico", 
             "No Dislipidemico","Diabetico", "No Diabetico",
             "Fumador", "No Fumador","Obeso", "No obeso"),2,5)

shinyUI(fluidPage(
    titlePanel("TORNASOL: National Peruvian Data -  
                     Cross Sectional Study of Cardiovascular  Diseases "),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Data Scientist: Mg. Wilmer Fuentes"),
            selectInput("dataset", label = "Choose a Study:", 
                        choices = c("Tornasol I","Tornasol II")),
            selectInput("pato", label = "Choose a Disease:", 
                        choices = c("Hipertension","Dislipidemia","Diabetes","Tabaquismo","Obesidad", "Sedentarismo")),
            selectInput("region",label =  "Choose a Region:", 
                        choices = c("Nacional","Costa","Sierra","Selva")),
                    
            br(),
            helpText("Sponsored:", a("www.datascienceperu.com", href="http://www.datascienceperu.com")),
            helpText("Contact: Phone: +51997498295 "),
            helpText( a("analista@datascienceperu.com", href="mailto:analista@datascienceperu.com")),
            br()
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("General", plotOutput("plot")),
                        tabPanel("Sex", plotOutput("plat") ),
                        tabPanel("Age Group", plotOutput("plut")),
                        tabPanel("City", plotOutput("plit")),
                        tabPanel("Education", plotOutput("mabel")),
                        tabPanel("Socioeconomic", plotOutput("ysamar")),
                        tabPanel("Sex vs Age", plotOutput("francis")),
                        tabPanel("Credits",                  
                                h4('The app is intended to show the prevalence of Cardiovascular Risk Factor in Citizen Peruvians- Studies TORNASOL'),
                                 p('The decision makers in Public Health needs a tools to know the magnitud of burden disease. 
                                boxes execute a calculation for dinamic plot presented'),
                                 p(
                                    'Fuente: Factores de Riesgo de las Enfermedades Cardiovasculares en el 
                                    Perú II. Estudio TORNASOL II comparado con TORNASOL I 
                                    después de cinco años.Luis Segura Vega, Regulo Agusti, Enrique Ruiz, Wilmer Fuentes Neira et al
                                    Revista Peruana de Cardiología 2011; 39 (1): 5-59 .
                                  '),
                                img(src='logo1.jpg', align = "center"),
                                br(),
                                p(),
                                img(src='logo2.png', align = "center")
                                )
                                   
            # h2("Prevalence of Cardiovascular Risk Factor"),
            
            
    )
))))