senareoAutre <- function(outpt, inpt) {
  outpt$CapitalChoixuiAutre = renderUI({
    CapitalUi(modalnew = "modalnewAutre",
              cHoixCapitalUser = inpt$CapitalChoixAutre,
              capitalfixe = "capitalfixeAutre",foissalaire = "foissalaireAutre",
              AjoutConditionsCapital = "AjoutConditionsCapitalAutre",
              DataconditionsCapital = "DataconditionsCapitalAutre",
              whatconditionCapital = "whatconditionCapitalAutre")
  })
  
  outpt$DataconditionsCapitalAutre = renderDataTable({
    data.frame(`Anciennete` = c("15-19","20-24","25-29","30-65"),
               Grades = NA,
               capital = paste0(c(2,3,4,6)," X Salaire Mensuelle")) %>% 
      select(inpt$whatconditionCapitalAutre, capital) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxactualisationuiAutre = renderUI({
    TauxUi(modalnew = "modalnewactAutre",
           CHoixTauxUser = inpt$tauxactualisationchoixAutre,
           whatconditionTaux = "whatconditionTauxactAutre",
           DataconditionsTaux = "DataconditionsTauactxAutre",
           AjoutConditionsTaux = "AjoutConditionsTauxactAutre",
           TauxFixe = "TauxactFixeAutre", value0 = 5)
  })
  
  outpt$DataconditionsTauactxAutre = renderDataTable({
    data.frame(Maturite = 1:60, ZC = (seq(3.7,5,length.out = 60)/100) %>% round(digits = 4)) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxaevolutionsalireuiAutre = renderUI({
    TauxUi(modalnew = "modalnewSalaireAutre",
           CHoixTauxUser = inpt$tauxevolutionsalirechoixAutre,
           whatconditionTaux = "whatconditionTauxSalaireAutre",
           DataconditionsTaux = "DataconditionsTauxSalaireAutre",
           AjoutConditionsTaux = "AjoutConditionsTauxSalaireAutre",
           TauxFixe = "TauxsalaireFixeAutre",
           Tauxact = F, value0 = 3.5)
  })
  
  outpt$DataconditionsTauxSalaireAutre = renderDataTable({
    data.frame(`Anciennete` = NA, Grades = NA, Taux = NA) %>% 
      select(inpt$whatconditionTauxSalaireAutre, Taux) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxchargementsociauxuiAutre = renderUI({
    TauxUi(modalnew = "modalnewchargsociauAutre",
           CHoixTauxUser = inpt$tauxchargementsociauxchoixAutre,
           whatconditionTaux = "whatconditionTauxchargsociauAutre",
           DataconditionsTaux = "DataconditionsTauxchargsociauAutre",
           AjoutConditionsTaux = "AjoutConditionsTauxchargsociauAutre",
           TauxFixe = "TauxchargsociauFixeAutre",
           Tauxact = F, value0 = 2.4)
  })
  
  outpt$DataconditionsTauxchargsociauAutre = renderDataTable({
    data.frame(`Anciennete` = NA, Grades = NA, Taux = NA) %>% 
      select(inpt$whatconditionTauxchargsociauAutre, Taux) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxturnoveruiAutre = renderUI({
    TauxUi(modalnew = "modalnewturnoverAutre",
           CHoixTauxUser = inpt$turnoverchoixAutre,
           whatconditionTaux = "whatconditionTauxturnoverAutre",
           DataconditionsTaux = "DataconditionsTauxturnoverAutre",
           AjoutConditionsTaux = "AjoutConditionsTauxturnoverAutre",
           TauxFixe = "TauxturnoverFixeAutre",
           Tauxact = F,value0 = 2.5)
  })
  
  outpt$DataconditionsTauxturnoverAutre = renderDataTable({
    data.frame(`Anciennete` = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59"),
               Grades = NA,
               Taux = c(2.26,1.76,2.81,1.74,0.56,.22,.05,0)/100) %>%
      select(inpt$whatconditionTauxturnoverAutre, Taux) %>%
      datatable(rownames = F,editable = T)
  })
}

bbbbb = function(dateinventairAutre, tablemortalitAutre, ageretraitAutre, CapitalChoiAutre, tauxactualisationchoiAutre, tauxevolutionsalirechoiAutre, tauxchargementsociauxchoiAutre, turnoverchoiAutre, 
                 tauxactualisationuAutre, CapitaluAutre, tauxaevolutionsalireuAutre, tauxchargementsociauxuAutre, tauxturnoveruAutre){
  fluidRow(
    column(width = 3,p("Date d'evaluation")),
    column(width = 7,dateInput(inputId = dateinventairAutre,label = NULL, language = "fr",weekstart = 1)),
    
    column(width = 3,p("Table de mortalite")),
    column(width = 7,pickerInput(inputId = tablemortalitAutre,label = NULL, choices = c("TV 88-90","TD 88-90"),selected = "TV 88-90")),
    
    column(width = 3,p("Age retraite")),
    column(width = 7,numericInputIcon(inputId = ageretraitAutre,label = NULL,value = 65,min = 40,max = 80)),
    
    column(width = 3,p("Montant IFC")),
    column(width = 5,prettyRadioButtons(inputId = CapitalChoiAutre,label = NULL, choices = c("Capital Fixe", "Fonction de Salaire", "Table"),inline = TRUE, status = "primary",fill = TRUE,selected = "Table")),
    column(width = 4),
    column(width = 4,offset = 3,uiOutput(CapitaluAutre)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Taux d'actualisation")),
    column(width = 5,prettyRadioButtons(inputId = tauxactualisationchoiAutre,label = NULL, choices = c("Taux fixe","Courbe des Taux","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxactualisationuAutre)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Taux Evolution de Salaire")),
    column(width = 5,prettyRadioButtons(inputId = tauxevolutionsalirechoiAutre,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxaevolutionsalireuAutre)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Taux de chargement sociaux")),
    
    column(width = 5,prettyRadioButtons(inputId = tauxchargementsociauxchoiAutre,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxchargementsociauxuAutre)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Rotation des personnel")),
    column(width = 5,prettyRadioButtons(inputId = turnoverchoiAutre,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE,selected = "Table")),
    column(width = 4,uiOutput(tauxturnoveruAutre)),
  )
}









aaaa = function(tabNamee, avantage, dateinventaire, tablemortalite, ageretraite, CapitalChoix, 
                tauxactualisationchoix, tauxevolutionsalirechoix, tauxchargementsociauxchoix, turnoverchoix, 
                tauxactualisationui, Capitalui, tauxaevolutionsalireui, tauxchargementsociauxui, tauxturnoverui){
  fluidRow(
    column(width = 3,p("Date d'evaluation")),
    column(width = 7,dateInput(inputId = dateinventaire,label = NULL,language = "fr",weekstart = 1)),
    
    column(width = 3,p("Table de mortalite")),
    column(width = 7,pickerInput(inputId = tablemortalite,label = NULL, choices = c("TV 88-90","TD 88-90"),selected = "TV 88-90")),

    column(width = 3,p("Age retraite")),
    column(width = 7,numericInputIcon(inputId = ageretraite,label = NULL,value = 65,min = 40,max = 80)),

    column(width = 3,p("Montant IFC")),
    column(width = 5,prettyRadioButtons(inputId = CapitalChoix,label = NULL, choices = c("Capital Fixe", "Fonction de Salaire", "Table"),inline = TRUE, status = "primary",fill = TRUE,selected = "Table")),
    column(width = 4),
    column(width = 4,offset = 3,uiOutput(Capitalui)),

    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),

    column(width = 3,p("Taux d'actualisation")),
    column(width = 5,prettyRadioButtons(inputId = tauxactualisationchoix,label = NULL, choices = c("Taux fixe","Courbe des Taux","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxactualisationui)),
    #
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    #
    column(width = 3,p("Taux Evolution de Salaire")),
    column(width = 5,prettyRadioButtons(inputId = tauxevolutionsalirechoix,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxaevolutionsalireui)),
    #
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    #
    column(width = 3,p("Taux de chargement sociaux")),
    #
    column(width = 5,prettyRadioButtons(inputId = tauxchargementsociauxchoix,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxchargementsociauxui)),
    #
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),

    column(width = 3,p("Rotation des personnel")),
    column(width = 5,prettyRadioButtons(inputId = turnoverchoix,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE,selected = "Table")),
    column(width = 4,uiOutput(tauxturnoverui)),
  )
}




library(shinymanager)
library(dashboardthemes)
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(keyring)
library(shinyjs)
library(magrittr)
library(dplyr)
library(DT)
library(readxl)
library(lubridate)
library(shinyWidgets)
library(skimr)
library(tidyverse)
library(highcharter)
library(shinyBS)
library(rhandsontable)
library(shinydashboardPlus)
library(naniar)


# shinyAppDir(appDir = "C:/Users/lamna/Desktop/test/New/aap", enableBookmarking = "server")
anotherone = T
parstore = reactiveValues()
parstore$CapitalChoixIFC = 1000

Loadproject = function(session, state){
  # project = projects[as.numeric(whichproject),]
  # state <- readRDS(project[1,4])
  # restore data
  # data(state[["data"]])
  updateAwesomeCheckboxGroup(session, "IFCcheck", selected = state[["avantageIFC"]])
  updateAwesomeCheckboxGroup(session, "MALADIEcheck", selected = state[["avantageMALADIE"]])
  updateAwesomeCheckboxGroup(session, "MedailledeTravailcheck", selected = state[["avantageMedailledeTravail"]])
  updateAwesomeCheckboxGroup(session, "CaisseInternedeRetraitecheck", selected = state[["avantageCaisseInternedeRetraite"]])
  updateDateInput(session, "dateinventaire", value = state[["dateinventaire"]])
  updatePickerInput(session, "tablemortalite", selected = state[["tablemortalite"]])
  updateNumericInputIcon(session, "ageretraite", value = state[["ageretraite"]])
  updatePrettyRadioButtons(session, "CapitalChoixIFC", selected = state[["CapitalChoix"]])
  updatePrettyRadioButtons(session, "turnoverchoixIFC", selected = state[["turnoverchoix"]])
  updatePrettyRadioButtons(session, "tauxchargementsociauxchoixIFC", selected = state[["tauxchargementsociauxchoix"]])
  updatePrettyRadioButtons(session, "tauxactualisationchoixIFC", selected = state[["tauxactualisationchoix"]])
  updatePrettyRadioButtons(session, "tauxevolutionsalirechoixIFC", selected = state[["tauxevolutionsalirechoix"]])
  updateAutonumericInput(session, "capitalfixeIFC", value = state[["capitalfixe"]])
  updateNumericInputIcon(session, "foissalaireIFC", value = state[["foissalaire"]])
  updateNumericInputIcon(session, "TauxactFixeIFC", value = state[["TauxactFixeIFC"]])
  updateNumericInputIcon(session, "TauxsalaireFixeIFC", value = state[["TauxsalaireFixeIFC"]])
  updateNumericInputIcon(session, "TauxchargsociauFixeIFC", value = state[["TauxchargsociauFixeIFC"]])
  updateNumericInputIcon(session, "TauxturnoverFixeIFC", value = state[["TauxturnoverFixeIFC"]])
  
  
  # nameheader$projname = project[1,1]
  
  # return(project[1,1])
  
}

connected = function(session, projects, whichproject, nameheader){
  project = projects[as.numeric(whichproject),]
  state <- readRDS(project[1,4])
  # restore data
  # data(state[["data"]])
  # updateAwesomeCheckboxGroup(session, "IFCcheck", selected = state[["avantageIFC"]])
  # updateAwesomeCheckboxGroup(session, "MALADIEcheck", selected = state[["avantageMALADIE"]])
  # updateAwesomeCheckboxGroup(session, "MedailledeTravailcheck", selected = state[["avantageMedailledeTravail"]])
  # updateAwesomeCheckboxGroup(session, "CaisseInternedeRetraitecheck", selected = state[["avantageCaisseInternedeRetraite"]])
  updateDateInput(session, "dateinventaire", value = state[["dateinventaire"]])
  updatePickerInput(session, "tablemortalite", selected = state[["tablemortalite"]])
  updateNumericInputIcon(session, "ageretraite", value = state[["ageretraite"]])
  updatePrettyRadioButtons(session, "CapitalChoixIFC", selected = state[["CapitalChoix"]])
  updatePrettyRadioButtons(session, "turnoverchoixIFC", selected = state[["turnoverchoix"]])
  updatePrettyRadioButtons(session, "tauxchargementsociauxchoixIFC", selected = state[["tauxchargementsociauxchoix"]])
  updatePrettyRadioButtons(session, "tauxactualisationchoixIFC", selected = state[["tauxactualisationchoix"]])
  updatePrettyRadioButtons(session, "tauxevolutionsalirechoixIFC", selected = state[["tauxevolutionsalirechoix"]])
  updateAutonumericInput(session, "capitalfixeIFC", value = state[["capitalfixe"]])
  updateNumericInputIcon(session, "foissalaireIFC", value = state[["foissalaire"]])
  updateNumericInputIcon(session, "TauxactFixeIFC", value = state[["TauxactFixeIFC"]])
  updateNumericInputIcon(session, "TauxsalaireFixeIFC", value = state[["TauxsalaireFixeIFC"]])
  updateNumericInputIcon(session, "TauxchargsociauFixeIFC", value = state[["TauxchargsociauFixeIFC"]])
  updateNumericInputIcon(session, "TauxturnoverFixeIFC", value = state[["TauxturnoverFixeIFC"]])
#   
#   
#   # nameheader$projname = project[1,1]
#   
#   return(project[1,1])
#   
  }

`%AND%` <- function (x, y) {
  if (!is.null(x) && !anyNA(x))
    if (!is.null(y) && !anyNA(y))
      return(y)
  return(NULL)
}

passwordInputAddon <- function (inputId, label, value = "", placeholder = NULL, addon, width = NULL)
{
  value <- shiny::restoreInput(id = inputId, default = value)
  htmltools::tags$div(
    class = "form-group shiny-input-container",
    label %AND% htmltools::tags$label(label, `for` = inputId),
    style = if (!is.null(width)) paste0("width: ", htmltools::validateCssUnit(width), ";"),
    htmltools::tags$div(
      style = "margin-bottom: 5px;", class="input-group",
      addon %AND% htmltools::tags$span(class="input-group-addon", addon),
      htmltools::tags$input(
        id = inputId, type = "password", class = "form-control",
        value = value, placeholder = placeholder
      )
    )
  )
}

can.be.numeric = Vectorize(function(x) {
  stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
  numNAs <- sum(is.na(x))
  numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
  return(numNAs_new == numNAs)
})

assssdate = function(x){
  options(warn = -1)
  if (can.be.numeric(x)) {
    a = as.Date(x = as.double(x),origin = "1900-01-01") - 2
  }
  options(warn = 0)
  return(a)
}

SidebarAvantages = function(AvantageName, AvantageId, Subitem1Id, Subitem2Id, Subitem3Id){
  # req(checkifselected)
  # if (checkifselected == AvantageName) {
    menuItem(AvantageName, tabName = AvantageId,
             menuSubItem("DONNEES",tabName = Subitem1Id),
             menuSubItem("PARAMETRAGE",tabName = Subitem2Id),
             menuSubItem("RESULTATS",tabName = Subitem3Id))
    
  # }
}


add_panel_donnes = function(tabNamee, avantage, btnimportid, filepathid, datatemplatescreen, 
                            datashowui, datashowanalyse, datashowinvalide, datastatistique, 
                            plotstatage1, plotstatage2, 
                            plotstatanciennete1, plotstatanciennete2,
                            plotstatsalaire1, plotstatsalaire2) {
  if (avantage == "avantageIFC") {
    tabItem(tabName = tabNamee,
            tabsetPanel(
              tabPanel(title = "Table", icon = icon(name = "database"),
                       br(),
                       actionBttn(inputId = btnimportid,label = "Importation", style = "gradient",color = "primary",icon = icon("upload")),
                       # actionBttn(inputId = "sssss",label = "test"),
                       bsModal("modalExample", "Aide", btnimportid, size = "large",
                               fileInput(inputId = filepathid, label = NULL,buttonLabel = "Parcourir",multiple = F),
                               h2("Format des donnees requise :"),
                               uiOutput(outputId = datatemplatescreen)),
                       fluidRow(column(width = 12,)),
                       fluidRow(column(width = 12,)),
                       fluidRow(column(width = 12,)),
                       rHandsontableOutput(outputId = datashowui)),
              tabPanel(title = "Validation", icon = icon(name = "check"),
                       tabsetPanel(
                       tabPanel("Analyse",icon = icon("list"),
                                br(),
                                fluidRow(
                                  column(width = 7,
                                         box(title = "Criteres",width = 10,fluidRow(
                                           column(width = 6,p("Date d'evaluation")),
                                           column(width = 6,dateInput(inputId = "dateevaluation",label = NULL,language = "fr",weekstart = 1)),
                                           
                                           column(width = 6,p("Salaire Mensuelle Maximal")),
                                           column(width = 6,autonumericInput(inputId = "SalaireMax",currencySymbol = "  MAD",
                                                                             label = NULL, value = 100000,
                                                                             currencySymbolPlacement = "s",decimalPlaces = 0,
                                                                             digitGroupSeparator = ",")),
                                           
                                           column(width = 6,p("Salaire Mensuelle Minimal")),
                                           column(width = 6,autonumericInput(inputId = "SalaireMin",currencySymbol = "  MAD",
                                                                             label = NULL, value = 3000,
                                                                             currencySymbolPlacement = "s",decimalPlaces = 0,
                                                                             digitGroupSeparator = ",")),
                                           
                                           column(width = 6,p("Age Maximal")),
                                           column(width = 6,numericInputIcon(inputId = "AgeMax",label = NULL,value = 60,min = 15,max = 100)),
                                           
                                           column(width = 6,p("Age Minimal")),
                                           column(width = 6,numericInputIcon(inputId = "AgeMin",label = NULL,value = 15,min = 10,max = 100)),
                                           
                                           column(width = 6,p("Age minimal d'entre en entreprise")),
                                           column(width = 6,numericInputIcon(inputId = "AgeentreeMin",label = NULL,value = 15,min = 10,max = 100)),
                                         ))),
                                  column(width = 4,box(title = "Diagnostique",width = 12,tableOutput(outputId = datashowanalyse)))
                                )),
                       tabPanel("Donnees Invalides",icon = icon("times"),
                                                       br(),
                                                       fluidRow(column(width = 12,)),
                                                       fluidRow(column(width = 12,)),
                                                       fluidRow(column(width = 12,)),
                                                       # fluidRow(column(offset = 5,width = 4,downloadButton(outputId = "downloadInvalides",label = "Telecharger"))),
                                                       fluidRow(column(width = 12,)),
                                                       fluidRow(column(width = 12,)),
                                                       fluidRow(column(width = 12,box(width = 12,rHandsontableOutput(datashowinvalide))))),
                       tabPanel("Donnees Valides",icon = icon("check"),
                                br(),
                                fluidRow(column(width = 12,)),
                                fluidRow(column(width = 12,)),
                                fluidRow(column(width = 12,)),
                                # fluidRow(column(offset = 5,width = 4,downloadButton(outputId = "downloadvalides",label = "Telecharger"))),
                                fluidRow(column(width = 12,)),
                                fluidRow(column(width = 12,)),
                                fluidRow(column(width = 10,offset = 2,box(width = 8,rHandsontableOutput(outputId = "datashowvalide")))))

                       )),
              tabPanel(title = "Statistique", icon = icon(name = "chart-bar"),
                       column(width = 6,offset = 6,uiOutput(outputId = "plotstatby")),
                       tabsetPanel(
                         tabPanel(title = "Age",
                                  fluidRow(column(width = 6,highchartOutput(outputId = plotstatage1)),
                                           column(width = 6,highchartOutput(outputId = plotstatage2)))),
                         tabPanel(title = "Anciennete",
                                  fluidRow(column(width = 6,highchartOutput(outputId = plotstatanciennete1)),
                                           column(width = 6,highchartOutput(outputId = plotstatanciennete2))))),
                       br(),
                       # fluidRow(column(width = 12,)),
                       # fluidRow(column(width = 12,)),
                       # fluidRow(column(width = 12,)),
                       # dataTableOutput(outputId = datastatistique),
                       fluidRow(column(width = 12,))
              )
            ))
  }
}


add_panel_parametrage = function(tabNamee, avantage, dateinventaire, tablemortalite, ageretraite, CapitalChoix, 
                                 tauxactualisationchoix, tauxevolutionsalirechoix, tauxchargementsociauxchoix, turnoverchoix, 
                                 tauxactualisationui, Capitalui, tauxaevolutionsalireui, tauxchargementsociauxui, tauxturnoverui){
  if (avantage == "avantageIFC") {
    tabItem(tabName = tabNamee,
            tabsetPanel(id = "runnallscen",
              tabPanel(title = "Scenario de base", icon = icon(name = "user-cog"),
                       column(width = 12,),
                       fluidRow(
                         column(width = 3,p("Date d'evaluation")),
                         column(width = 7,dateInput(inputId = dateinventaire,label = NULL,language = "fr",weekstart = 1)),
                         
                         column(width = 3,p("Table de mortalite")),
                         column(width = 7,pickerInput(inputId = tablemortalite,label = NULL, choices = c("TV 88-90","TD 88-90"),selected = "TV 88-90")),
                         
                         column(width = 3,p("Age retraite")),
                         column(width = 7,numericInputIcon(inputId = ageretraite,label = NULL,value = 65,min = 40,max = 80)),
                         
                         column(width = 3,p("Montant IFC")),
                         column(width = 5,prettyRadioButtons(inputId = CapitalChoix,label = NULL, choices = c("Capital Fixe", "Fonction de Salaire", "Table"),inline = TRUE, status = "primary",fill = TRUE,selected = "Table")),
                         column(width = 4),
                         column(width = 4,offset = 3,uiOutput(Capitalui)),
                         
                         column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
                         
                         column(width = 3,p("Taux d'actualisation")),
                         column(width = 5,prettyRadioButtons(inputId = tauxactualisationchoix,label = NULL, choices = c("Taux fixe","Courbe des Taux","Table"),inline = TRUE, status = "primary",fill = TRUE)),
                         column(width = 4,uiOutput(tauxactualisationui)),
                         
                         column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
                         
                         column(width = 3,p("Taux Evolution de Salaire")),
                         column(width = 5,prettyRadioButtons(inputId = tauxevolutionsalirechoix,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE)),
                         column(width = 4,uiOutput(tauxaevolutionsalireui)),
                         
                         column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
                         
                         column(width = 3,p("Taux de chargement sociaux")),
                         
                         column(width = 5,prettyRadioButtons(inputId = tauxchargementsociauxchoix,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE)),
                         column(width = 4,uiOutput(tauxchargementsociauxui)),
                         
                         column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
                         
                         column(width = 3,p("Rotation des personnel")),
                         column(width = 5,prettyRadioButtons(inputId = turnoverchoix,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE,selected = "Table")),
                         column(width = 4,uiOutput(tauxturnoverui)),
                       )),
              tabPanel(title = "Scenario 2", icon = icon(name = "tasks"),
                       uiOutput(outputId = "senareoIFCui2")),
              tabPanel(title = "Scenario 3", icon = icon(name = "tasks"),
                       uiOutput(outputId = "senareoIFCui3"))
            ))
  }
  
}


add_panel_Resultat = function(tabNamee, avantage, Tableresultattete, DataAnalysesensibilite1, DataAnalysesensibilite2) {
  if (avantage == "avantageIFC") {
    tabItem(tabName = tabNamee,
            tabsetPanel(id = "re",
              tabPanel(title = "Calcul Par tete",value = "CalculPartete", icon = icon(name = "users"),
                       fluidRow(column(width = 12,)),
                       fluidRow(column(width = 12,)),
                       fluidRow(column(width = 12,)),
                       dataTableOutput(outputId = Tableresultattete)
              ),
              tabPanel(title = "Flux Futures", icon = icon(name = "chart-line"),
                       br(),
                       fluidRow(column(width = 12,)),
                       fluidRow(column(width = 12,)),
                       highchartOutput(outputId = "fluxfuture")
              ),
              tabPanel(title = "Simulations", icon = icon(name = "calculator"),
                       br(),
                       fluidRow(column(width = 12,)),
                       fluidRow(column(width = 12,)),
                       dataTableOutput(outputId = DataAnalysesensibilite1),
                       dataTableOutput(outputId = DataAnalysesensibilite2)
              )
              
            )
    )
  }
}


senareoIFC22 = function(dateinventaire2, tablemortalite2, ageretraite2, CapitalChoix2, tauxactualisationchoix2, tauxevolutionsalirechoix2, tauxchargementsociauxchoix2, turnoverchoix2, 
                        tauxactualisationui2, Capitalui2, tauxaevolutionsalireui2, tauxchargementsociauxui2, tauxturnoverui2){
  fluidRow(
    column(width = 3,p("Date d'evaluation")),
    column(width = 7,dateInput(inputId = dateinventaire2,label = NULL, language = "fr",weekstart = 1)),
    
    column(width = 3,p("Table de mortalite")),
    column(width = 7,pickerInput(inputId = tablemortalite2,label = NULL, choices = c("TV 88-90","TD 88-90"),selected = "TV 88-90")),
    
    column(width = 3,p("Age retraite")),
    column(width = 7,numericInputIcon(inputId = ageretraite2,label = NULL,value = 65,min = 40,max = 80)),
    
    column(width = 3,p("Montant IFC")),
    column(width = 5,prettyRadioButtons(inputId = CapitalChoix2,label = NULL, choices = c("Capital Fixe", "Fonction de Salaire", "Table"),inline = TRUE, status = "primary",fill = TRUE,selected = "Table")),
    column(width = 4),
    column(width = 4,offset = 3,uiOutput(Capitalui2)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Taux d'actualisation")),
    column(width = 5,prettyRadioButtons(inputId = tauxactualisationchoix2,label = NULL, choices = c("Taux fixe","Courbe des Taux","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxactualisationui2)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Taux Evolution de Salaire")),
    column(width = 5,prettyRadioButtons(inputId = tauxevolutionsalirechoix2,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxaevolutionsalireui2)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Taux de chargement sociaux")),
    
    column(width = 5,prettyRadioButtons(inputId = tauxchargementsociauxchoix2,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxchargementsociauxui2)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Rotation des personnel")),
    column(width = 5,prettyRadioButtons(inputId = turnoverchoix2,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE,selected = "Table")),
    column(width = 4,uiOutput(tauxturnoverui2)),
  )
}

senareoIFC2 <- function(outpt, inpt) {
  outpt$CapitalChoixuiIFC2 = renderUI({
    CapitalUi(modalnew = "modalnewIFC2",
              cHoixCapitalUser = inpt$CapitalChoixIFC2,
              capitalfixe = "capitalfixeIFC2",foissalaire = "foissalaireIFC2",
              AjoutConditionsCapital = "AjoutConditionsCapitalIFC2",
              DataconditionsCapital = "DataconditionsCapitalIFC2",
              whatconditionCapital = "whatconditionCapitalIFC2")
  })
  
  outpt$DataconditionsCapitalIFC2 = renderDataTable({
    data.frame(`Anciennete` = c("15-19","20-24","25-29","30-65"),
               Grades = NA,
               capital = paste0(c(2,3,4,6)," X Salaire Mensuelle")) %>% 
      select(inpt$whatconditionCapitalIFC2, capital) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxactualisationuiIFC2 = renderUI({
    TauxUi(modalnew = "modalnewactIFC2",
           CHoixTauxUser = inpt$tauxactualisationchoixIFC2,
           whatconditionTaux = "whatconditionTauxactIFC2",
           DataconditionsTaux = "DataconditionsTauactxIFC2",
           AjoutConditionsTaux = "AjoutConditionsTauxactIFC2",
           TauxFixe = "TauxactFixeIFC2", value0 = 4.4)
  })
  
  outpt$DataconditionsTauactxIFC2 = renderDataTable({
    data.frame(Maturite = 1:60, ZC = (seq(3.7,5,length.out = 60)/100) %>% round(digits = 4)) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxaevolutionsalireuiIFC2 = renderUI({
    TauxUi(modalnew = "modalnewSalaireIFC2",
           CHoixTauxUser = inpt$tauxevolutionsalirechoixIFC2,
           whatconditionTaux = "whatconditionTauxSalaireIFC2",
           DataconditionsTaux = "DataconditionsTauxSalaireIFC2",
           AjoutConditionsTaux = "AjoutConditionsTauxSalaireIFC2",
           TauxFixe = "TauxsalaireFixeIFC2",
           Tauxact = F, value0 = 3)
  })
  
  outpt$DataconditionsTauxSalaireIFC2 = renderDataTable({
    data.frame(`Anciennete` = NA, Grades = NA, Taux = NA) %>% 
      select(inpt$whatconditionTauxSalaireIFC2, Taux) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxchargementsociauxuiIFC2 = renderUI({
    TauxUi(modalnew = "modalnewchargsociauIFC2",
           CHoixTauxUser = inpt$tauxchargementsociauxchoixIFC2,
           whatconditionTaux = "whatconditionTauxchargsociauIFC2",
           DataconditionsTaux = "DataconditionsTauxchargsociauIFC2",
           AjoutConditionsTaux = "AjoutConditionsTauxchargsociauIFC2",
           TauxFixe = "TauxchargsociauFixeIFC2",
           Tauxact = F, value0 = 1.4)
  })
  
  outpt$DataconditionsTauxchargsociauIFC2 = renderDataTable({
    data.frame(`Anciennete` = NA, Grades = NA, Taux = NA) %>% 
      select(inpt$whatconditionTauxchargsociauIFC2, Taux) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxturnoveruiIFC2 = renderUI({
    TauxUi(modalnew = "modalnewturnoverIFC2",
           CHoixTauxUser = inpt$turnoverchoixIFC2,
           whatconditionTaux = "whatconditionTauxturnoverIFC2",
           DataconditionsTaux = "DataconditionsTauxturnoverIFC2",
           AjoutConditionsTaux = "AjoutConditionsTauxturnoverIFC2",
           TauxFixe = "TauxturnoverFixeIFC2",
           Tauxact = F,value0 = 2.5)
  })
  
  outpt$DataconditionsTauxturnoverIFC2 = renderDataTable({
    data.frame(`Anciennete` = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59"),
               Grades = NA,
               Taux = c(2.26,1.76,2.81,1.74,0.56,.22,.05,0)/100) %>%
      select(inpt$whatconditionTauxturnoverIFC2, Taux) %>%
      datatable(rownames = F,editable = T)
  })
}



senareoIFC3 <- function(outpt, inpt) {
  outpt$CapitalChoixuiIFC3 = renderUI({
    CapitalUi(modalnew = "modalnewIFC3",
              cHoixCapitalUser = inpt$CapitalChoixIFC3,
              capitalfixe = "capitalfixeIFC3",foissalaire = "foissalaireIFC3",
              AjoutConditionsCapital = "AjoutConditionsCapitalIFC3",
              DataconditionsCapital = "DataconditionsCapitalIFC3",
              whatconditionCapital = "whatconditionCapitalIFC3")
  })
  
  outpt$DataconditionsCapitalIFC3 = renderDataTable({
    data.frame(`Anciennete` = c("15-19","20-24","25-29","30-65"),
               Grades = NA,
               capital = paste0(c(2,3,4,6)," X Salaire Mensuelle")) %>% 
      select(inpt$whatconditionCapitalIFC3, capital) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxactualisationuiIFC3 = renderUI({
    TauxUi(modalnew = "modalnewactIFC3",
           CHoixTauxUser = inpt$tauxactualisationchoixIFC3,
           whatconditionTaux = "whatconditionTauxactIFC3",
           DataconditionsTaux = "DataconditionsTauactxIFC3",
           AjoutConditionsTaux = "AjoutConditionsTauxactIFC3",
           TauxFixe = "TauxactFixeIFC3", value0 = 5)
  })
  
  outpt$DataconditionsTauactxIFC3 = renderDataTable({
    data.frame(Maturite = 1:60, ZC = (seq(3.7,5,length.out = 60)/100) %>% round(digits = 4)) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxaevolutionsalireuiIFC3 = renderUI({
    TauxUi(modalnew = "modalnewSalaireIFC3",
           CHoixTauxUser = inpt$tauxevolutionsalirechoixIFC3,
           whatconditionTaux = "whatconditionTauxSalaireIFC3",
           DataconditionsTaux = "DataconditionsTauxSalaireIFC3",
           AjoutConditionsTaux = "AjoutConditionsTauxSalaireIFC3",
           TauxFixe = "TauxsalaireFixeIFC3",
           Tauxact = F, value0 = 3.5)
  })
  
  outpt$DataconditionsTauxSalaireIFC3 = renderDataTable({
    data.frame(`Anciennete` = NA, Grades = NA, Taux = NA) %>% 
      select(inpt$whatconditionTauxSalaireIFC3, Taux) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxchargementsociauxuiIFC3 = renderUI({
    TauxUi(modalnew = "modalnewchargsociauIFC3",
           CHoixTauxUser = inpt$tauxchargementsociauxchoixIFC3,
           whatconditionTaux = "whatconditionTauxchargsociauIFC3",
           DataconditionsTaux = "DataconditionsTauxchargsociauIFC3",
           AjoutConditionsTaux = "AjoutConditionsTauxchargsociauIFC3",
           TauxFixe = "TauxchargsociauFixeIFC3",
           Tauxact = F, value0 = 2.4)
  })
  
  outpt$DataconditionsTauxchargsociauIFC3 = renderDataTable({
    data.frame(`Anciennete` = NA, Grades = NA, Taux = NA) %>% 
      select(inpt$whatconditionTauxchargsociauIFC3, Taux) %>% 
      datatable(rownames = F,editable = T)
  })
  
  outpt$tauxturnoveruiIFC3 = renderUI({
    TauxUi(modalnew = "modalnewturnoverIFC3",
           CHoixTauxUser = inpt$turnoverchoixIFC3,
           whatconditionTaux = "whatconditionTauxturnoverIFC3",
           DataconditionsTaux = "DataconditionsTauxturnoverIFC3",
           AjoutConditionsTaux = "AjoutConditionsTauxturnoverIFC3",
           TauxFixe = "TauxturnoverFixeIFC3",
           Tauxact = F,value0 = 2.5)
  })
  
  outpt$DataconditionsTauxturnoverIFC3 = renderDataTable({
    data.frame(`Anciennete` = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59"),
               Grades = NA,
               Taux = c(2.26,1.76,2.81,1.74,0.56,.22,.05,0)/100) %>%
      select(inpt$whatconditionTauxturnoverIFC3, Taux) %>%
      datatable(rownames = F,editable = T)
  })
}

senareoIFC33 = function(dateinventaire3, tablemortalite3, ageretraite3, CapitalChoix3, tauxactualisationchoix3, tauxevolutionsalirechoix3, tauxchargementsociauxchoix3, turnoverchoix3, 
                        tauxactualisationui3, Capitalui3, tauxaevolutionsalireui3, tauxchargementsociauxui3, tauxturnoverui3){
  fluidRow(
    column(width = 3,p("Date d'evaluation")),
    column(width = 7,dateInput(inputId = dateinventaire3,label = NULL, language = "fr",weekstart = 1)),
    
    column(width = 3,p("Table de mortalite")),
    column(width = 7,pickerInput(inputId = tablemortalite3,label = NULL, choices = c("TV 88-90","TD 88-90"),selected = "TV 88-90")),
    
    column(width = 3,p("Age retraite")),
    column(width = 7,numericInputIcon(inputId = ageretraite3,label = NULL,value = 65,min = 40,max = 80)),
    
    column(width = 3,p("Montant IFC")),
    column(width = 5,prettyRadioButtons(inputId = CapitalChoix3,label = NULL, choices = c("Capital Fixe", "Fonction de Salaire", "Table"),inline = TRUE, status = "primary",fill = TRUE,selected = "Table")),
    column(width = 4),
    column(width = 4,offset = 3,uiOutput(Capitalui3)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Taux d'actualisation")),
    column(width = 5,prettyRadioButtons(inputId = tauxactualisationchoix3,label = NULL, choices = c("Taux fixe","Courbe des Taux","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxactualisationui3)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Taux Evolution de Salaire")),
    column(width = 5,prettyRadioButtons(inputId = tauxevolutionsalirechoix3,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxaevolutionsalireui3)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Taux de chargement sociaux")),
    
    column(width = 5,prettyRadioButtons(inputId = tauxchargementsociauxchoix3,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE)),
    column(width = 4,uiOutput(tauxchargementsociauxui3)),
    
    column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),column(width = 12,p("")),
    
    column(width = 3,p("Rotation des personnel")),
    column(width = 5,prettyRadioButtons(inputId = turnoverchoix3,label = NULL, choices = c("Taux fixe","Table"),inline = TRUE, status = "primary",fill = TRUE,selected = "Table")),
    column(width = 4,uiOutput(tauxturnoverui3)),
  )
}



CapitalUi = function(valuecap, modalnew, cHoixCapitalUser, capitalfixe, foissalaire, AjoutConditionsCapital, DataconditionsCapital, whatconditionCapital) {
  if (cHoixCapitalUser == "Capital Fixe") {
    autonumericInput(inputId = capitalfixe,currencySymbol = "  MAD",
                     label = NULL, value = valuecap,
                     currencySymbolPlacement = "s",
                     decimalPlaces = 0,
                     digitGroupSeparator = ","
    )
  }else if(cHoixCapitalUser == "Fonction de Salaire"){
    fluidRow(column(width = 5,numericInputIcon(inputId = foissalaire, label = NULL,value = 3,min = 1,max = 60,
                                               icon = list(NULL, icon("times")),help_text = '0 < ... < 60')),
             column(width = 5,p("Salaire Mensuelle")))
    
  }else{
    fluidRow(
      actionBttn(inputId = AjoutConditionsCapital,label = "Ajout Des Conditions", style = "gradient",color = "primary",icon = icon("plus"),size = "xs"),
      bsModal(modalnew, "Greetings", AjoutConditionsCapital, size = "large",
              selectInput(inputId = whatconditionCapital,label = "L'augmentation de salaire sera en fonction de :",choices = c("Anciennete","Grades"),selected = "Anciennete",multiple = TRUE),
              DT::dataTableOutput(outputId = DataconditionsCapital)
      ))
    
  }}

TauxUi = function(modalnew, value0 = 2, DataconditionsTaux, AjoutConditionsTaux, CHoixTauxUser, TauxFixe, Tauxact = T, whatconditionTaux){
  if (CHoixTauxUser == "Taux fixe") {
    numericInputIcon(
      inputId = TauxFixe,
      label = NULL,
      value = value0,
      min = 0,max = 100,step = .01,
      icon = list(NULL, icon("percent")))
  }else if(CHoixTauxUser == "Courbe des Taux"){
    p('En cours ...')
    
  }else{
    
    if (!Tauxact) {
      fluidRow(
        actionBttn(inputId = AjoutConditionsTaux,label = "Ajout Des Conditions", style = "gradient",color = "primary",icon = icon("plus"),size = "xs"),
        bsModal(modalnew, NULL, AjoutConditionsTaux, size = "large",
                selectInput(inputId = whatconditionTaux,width = "100%", label = "L'augmentation de salaire sera en fonction de :",choices = c("Anciennete","Grades"),selected = "Anciennete",multiple = TRUE),
                DT::dataTableOutput(outputId = DataconditionsTaux)
        ))
    }else{
      fluidRow(
        actionBttn(inputId = AjoutConditionsTaux,label = "Ajout Des Conditions", style = "gradient",color = "primary",icon = icon("plus"),size = "xs"),
        bsModal(modalnew, NULL, AjoutConditionsTaux, size = "large",
                DT::dataTableOutput(outputId = DataconditionsTaux)
        ))
    }
    
  }}




