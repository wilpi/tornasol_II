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
    titlePanel("Mg. Wilmer Fuentes"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("TORNASOL II, National Peruvian Data -  
                     Cross Sectional Study of Cardiovascular  Diseases"),
            selectInput("dataset", label = "Choose a Study:", 
                        choices = c("Tornasol I","Tornasol II")),
            selectInput("pato", label = "Choose a Disease:", 
                        choices = c("Hipertension","Dislipidemia","Diabetes","Tabaquismo","Obesidad")),
            selectInput("region",label =  "Choose a Region:", 
                        choices = c("Nacional","Costa","Sierra","Selva")),
                    
            br(),
            br()
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("General", plotOutput("plot")),
                        tabPanel("Por Sexo", "En construccion" ),
                        tabPanel("Grupo de Edad", plotOutput("plut") )
                                   
            # h2("Prevalence of Cardiovascular Risk Factor"),
            #h5("Draft of work - i have a problem to pass ALL argument(diseases,region) to my function plot pv2, pv2 work fine out of shiny  "),
            #plotOutput("plot"),
            #h5("Coming soon, here barplot of prevalence by age group"))
            
    )
))))