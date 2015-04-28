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
                        choices = c("Hipertension","Dislipidemia","Diabetes","Tabaquismo","Obesidad", "Sedentarismo")),
            selectInput("region",label =  "Choose a Region:", 
                        choices = c("Nacional","Costa","Sierra","Selva")),
                    
            br(),
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
                                h4('The app is intended to show a prediction for a childs height based on the heights of the parents.'),
                                 p('The user needs to add in the height of the father and height of the mother and check the box for
                                if they want to know the predicted height of a son, a daughter or both. The app will run a simple
                                calculation (note. this is not based of any real data of childrens heights but just a simple sum
                                to shows the functions are working) and show the average expected height of a child. If the check
                                boxes are ticked a calculation for a son and/or a daughter will also appear'),
                                 p('The ui.R and server.R files can be found at the following github site: https://github.com/BDFace/DDP_Project_Shiny')
                           )
                                   
            # h2("Prevalence of Cardiovascular Risk Factor"),
            #h5("Draft of work - i have a problem to pass ALL argument(diseases,region) to my function plot pv2, pv2 work fine out of shiny  "),
            #plotOutput("plot"),
            #h5("Coming soon, here barplot of prevalence by age group"))
            
    )
))))