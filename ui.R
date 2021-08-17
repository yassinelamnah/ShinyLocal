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

My_body_MALADIE = function(){
  navbarPage('', id = "inTabset_MALADIE",
             tabPanel(title = "DEFINITION", value = "1", p("???")),
             tabPanel(title = "PARAMETRAGE", value = "2",p("???")),
             tabPanel(title = "IMPORTATION ET ANALYSE DE DONNEES INDIVIDIELLES", value = "3",p("???")),
             tabPanel(title = "RESULTAS", value = "4",p("???")),
             useShinyjs(),
             tags$head(tags$style(HTML('.navbar-nav a {cursor: default}')))
  )
}



ui <- dashboardPage(
  
  ### ui header
  dashboardHeader(
    titleWidth = 500,
    
    ### changing logo
    title = logo_blue_gradient(boldText = "FRS",
                               mainText = "Evaluation des engagements sociaux selon IAS 19",
                               badgeText = "v1.1"
    ),
    leftUi = tagList(
      uiOutput(outputId = "ProjectName")
    ),
    # tags$li(a(onclick = "openTab('main')",
    #           href = NULL,
    #           icon("download"),
    #           title = "Homepage",
    #           style = "cursor: pointer;"),
    #         class = "dropdown",
    #         tags$script(HTML("
    #                                var openTab = function(tabName){
    #                                $('a', $('.sidebar')).each(function() {
    #                                if(this.getAttribute('data-value') == tabName) {
    #                                this.click()
    #                                };
    #                                });
    #                                }")))
    tags$li(class = "dropdown",
            actionBttn(inputId = "saveState",
                       label = "Sauvegarder",
                       icon = icon("download"),
                       style = "gradient",size = "sm",
                       color = "success")
    )
  ),
  dashboardSidebar(
    tags$head(tags$style(HTML('.content-wrapper { height: 1000px !important;}'))),
    uiOutput("box1"),
    # tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
    sidebarMenu(
      id = "sidebar",
      menuItem(text = "Accueil",tabName = "main"),
      menuItemOutput("IFC"),
      menuItemOutput("MALADIE"),
      menuItemOutput("MedailledeTravail"),
      menuItemOutput("CaisseInternedeRetraite"),
      menuItem(text = "Synthese",tabName = "Synthese")
    ),
    sidebarMenuOutput(outputId = "dy_menu")),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    useShinyjs(),
    # tags$div(id="C",class='shiny-input-radiogroup',DT::dataTableOutput('foo')),
    tabItems(
      tabItem(tabName = "main",
              # tags$div(id="C",class='shiny-input-radiogroup',DT::dataTableOutput('foo')),
              # downloadButton("saveState", "Save state"),
              # fileInput("rds","Restore state",accept = ".rds"),
              box(title = "Definition & Reglementation Des Avantages Sociaux",status = "primary",
                  width = 12,collapsible = T,
                  fluidRow(column(width = 6,pickerInput(inputId = "Aidewhat",label = NULL, 
                                                        choices = c("Indemnite Fin Carriere (IFC)", 
                                                                    "Couverture Medicale",
                                                                    "Medaille de Travail", 
                                                                    "Caisse Interne de Retraite"))),
                           column(width = 3,
                                  actionBttn(inputId = "AideReg",size = "sm",style = "unite",
                                             label = NULL, icon = icon("question")))
                  ),
                  uiOutput(outputId = "AdminorNot")),
              box(title = "Liste Des Anciens Projets",status = "primary",
                  width = 12,collapsible = T,
                  fluidRow(
                    column(width = 1,dropdownButton(
                      tags$h4("Pour des raisons de confidentialite, Certains informations seront masques, sauf si vous montrez le statut des ressources humaines."),
                      br(),br(),
                      fluidRow(column(width = 5,p("Etes vous un responsable RH ?")),
                               column(width = 4,prettyToggle(inputId = "RHorNo",label_on = "Oui", icon_on = icon("check"),status_on = "info",status_off = "warning", label_off = "Non",icon_off = icon("remove")))
                      ),
                      fluidRow(
                        column(8, align = "center", offset = 2,uiOutput(outputId = "mesageerror"))
                      ),
                      fluidRow(
                        column(8, align = "center", offset = 2,uiOutput(outputId = "passtrufalse"))
                      ),
                      fluidRow(
                        column(8, align = "center", offset = 2,actionBttn(inputId = "entreprojet",label = "Ok.",size = "sm"))
                      ),
                      circle = TRUE, status = "primary",size = "sm",
                      icon = icon("eye"), width = "700px",
                      tooltip = tooltipOptions(title = "Cliquez pour Voir un projet")
                    ))
                    ,column(width = 1,dropdownButton(
                      textInput(label = "Libelle du projet",inputId = "nameprojectchoix"),
                      dateInput(inputId = "DateProjetVolu", value = Sys.Date(),label = "Date du Projet"),
                      selectInput(inputId = "whatuserneed",multiple = T,
                                  choices = c("Indemnite Fin Carriere (IFC)","Couverture Medicale",
                                              "Medaille de Travail","Caisse Interne de Retraite"),
                                  label = "Avantages"),
                      passwordInputAddon("PasswordFirst", label = NULL, placeholder = "Mot De Passe", addon = icon("key"),width = "45%"),
                      passwordInputAddon("PasswordSecond",label = NULL , placeholder = "Confirmation du Mot De Passe", addon = icon("key"),width = "45%"),
                      column(width = 12, align = "center",uiOutput(outputId = "AddError")),
                      column(width = 12, align = "center",actionBttn(inputId = "Addfinish", label = "Ajouter",style = "unite")),
                      circle = TRUE, status = "warning",size = "sm",
                      icon = icon("plus"), width = "700px",
                      tooltip = tooltipOptions(title = "Cliquez pour ajouter un projet")
                    )),
                    column(width = 1,dropdownButton(
                      passwordInputAddon("PasswordDelete",label = NULL , 
                                         placeholder = "Saisir Le Code Du Projet", 
                                         addon = icon("key"),width = "45%"),
                      column(width = 12, align = "center",actionBttn(inputId = "checktodelete", label = "Supprimer!",style = "unite")),
                      circle = TRUE, status = "danger",size = "sm",
                      icon = icon("trash-alt"), width = "700px",
                      tooltip = tooltipOptions(title = "Cliquez pour supprimer un projet")
                    )),
                    column(width = 10,),
                    column(width = 12,p('')),column(width = 12,p('')),column(width = 12,p('')),
                    column(width = 12,
                           tags$div(id="C",class='shiny-input-radiogroup',DT::dataTableOutput('foo')),
                           # style = "height:300px; overflow-y: scroll;overflow-x: scroll;"
                    ))
              )
      ),
      add_panel_donnes(tabNamee = "subMenuDonneesIFC",
                       avantage = "avantageIFC",
                       btnimportid = "btnimportidIFC",
                       filepathid = "filepathidIFC",
                       datatemplatescreen = "datatemplatescreenIFC",
                       datashowui = "datashowuiIFC",
                       datashowanalyse = "datashowanalyseIFC",
                       datashowinvalide = "datashowinvalideIFC",
                       datastatistique = "datastatistiqueIFC",
                       plotstatage1 = "plotstatage1IFC",
                       plotstatage2 = "plotstatage2IFC",
                       plotstatanciennete1 = "plotstatanciennete1IFC",
                       plotstatanciennete2 = "plotstatanciennete2IFC",
                       plotstatsalaire1 = "plotstatsalaire1IFC",
                       plotstatsalaire2 = "plotstatsalaire2IFC"),
      add_panel_parametrage(tabNamee = "subMenuParametrageIFC",
                            dateinventaire = "dateinventaire",
                            tablemortalite = "tablemortalite",
                            ageretraite = "ageretraite",
                            avantage = "avantageIFC",
                            CapitalChoix = "CapitalChoixIFC",
                            Capitalui = "CapitalChoixuiIFC",
                            tauxactualisationchoix = "tauxactualisationchoixIFC",
                            tauxactualisationui = "tauxactualisationuiIFC",
                            tauxevolutionsalirechoix = "tauxevolutionsalirechoixIFC",
                            tauxaevolutionsalireui = "tauxaevolutionsalireuiIFC",
                            tauxchargementsociauxchoix = "tauxchargementsociauxchoixIFC",
                            turnoverchoix = "turnoverchoixIFC",
                            tauxchargementsociauxui = "tauxchargementsociauxuiIFC",
                            tauxturnoverui = "tauxturnoveruiIFC"),
      add_panel_Resultat(tabNamee = "subMenuResultatIFC", 
                         avantage = "avantageIFC",
                         Tableresultattete = "TableresultatteteIFC",
                         DataAnalysesensibilite1 = "DataAnalysesensibilite1IFC",
                         DataAnalysesensibilite2 = "DataAnalysesensibilite2IFC"),
      tabItem(tabName = "IFC", 
              navbarPage('', id = "inTabset",
                         tabPanel(title = "DEFINITION", value = "1",
                                  p("")),
                         tabPanel(title = "PARAMETRAGE", value = "2",
                                  uiOutput(outputId = "par_ui_IFC")
                                  # column(width = 8,offset = 2,
                                  #        uiOutput(outputId = "par_ui_IFC")
                                  # )
                         ),
                         tabPanel(title = "IMPORTATION ET ANALYSE DE DONNEES INDIVIDIELLES", value = "3",
                                  fileInput(inputId = "ind_input_IFC", label = NULL,buttonLabel = "Parcourir",multiple = F),
                                  fluidRow(column(width = 8,box(title = "Base de donneees Valideees (filtrees)",width = 12,
                                                                DT::dataTableOutput(outputId = "data_IFC"))),
                                           column(width = 4,actionBttn(inputId = "Analysededonnees", color = "primary",label = "Analyse de donnees",style = "unite")),
                                           br(),br(),
                                           column(width = 4,uiOutput(outputId = "dataanalyseIFC"))
                                  )),
                         tabPanel(title = "RESULTAS", value = "4",
                                  fluidRow(column(width = 4,offset = 5,
                                                  downloadButton(outputId = "download_IFC",label = "Telecharger Excel")),
                                           highchartOutput(outputId = "testplot")),
                                  dataTableOutput(outputId = "table_output_IFC")),
                         useShinyjs(),
                         tags$script(
                           HTML("var header = $('.navbar > .container-fluid');
                                            header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"Next\" type=\"button\" class=\"btn btn-primary action-button\" onclick=\"signIn()\">>></button></div>')")
                         ),
                         tags$script(
                           HTML("var header = $('.navbar > .container-fluid');
                                            header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"Previous\" type=\"button\" class=\"btn btn-primary action-button\" onclick=\"signIn()\"><<</button></div>')")
                         ),
                         tags$head(tags$style(HTML('.navbar-nav a {cursor: default}')))
              )
      ),
      tabItem(tabName = "MALADIE", My_body_MALADIE()),
      tabItem(tabName = "Avantage3", p("???")),
      tabItem(tabName = "Avantage4", p("???")),
      tabItem(tabName = "Synthese", 
              downloadButton(outputId = "downloader",label = "Synthese (.word)"),
              actionBttn(inputId = "Pourdestest",label = "poir tester"),
              fluidRow(column(width = 10,p("...")),
                       column(width = 10,uiOutput(outputId = "uiAutre")))))
  ))




# ui <- secure_app(ui,
#                  tags_bottom = tags$div(
#                    tags$p(
#                      "Pour des questions, Veuillez contacter ",
#                      tags$a(
#                        href = "mailto:yassine.lamnah@gmail.com?Subject=Demande%20Aide%20IAS19%20Shiny",
#                        target="_top", "l'administrateur"
#                      )
#                    )
#                  ),
#                  language = "fr",enable_admin = T,
#                  background  = "url(/bbb.gif) 50% 97% no-repeat,
#                             url(/image.png) center;")