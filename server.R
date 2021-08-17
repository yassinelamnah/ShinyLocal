tableconditions2 = data.frame(`Anciennete` = NA,
                              Grades = NA,
                              Taux = NA)

tableconditions = data.frame(`Anciennete` = c("15-19","20-24","25-29","30-65"),
                             Grades = NA,
                             capital = paste0(c(2,3,4,6)," X Salaire Mensuelle"))

tableconditions3 = data.frame(`Anciennete` = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59"),
                              Grades = NA,
                              Taux = c(2.26,1.76,2.81,1.74,0.56,.22,.05,0)/100)



Table_Capital_0 = data.frame(`Anciennete` = c("15-19","20-24","25-29","30-65"),
                             Sexe = NA,
                             capital = paste0(c(2,3,4,6)," X Salaire Mensuelle"))

Table_evolutionsalire_0 = data.frame(`Anciennete` = NA, Sexe = NA, Taux = NA)

Table_turnover_0 = data.frame(`Anciennete` = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59"),
                              Sexe = NA,
                              Taux = c(2.26,1.76,2.81,1.74,0.56,.22,.05,0)/100)


Capital_garantie = Table_Capital_0 %>%
  separate(col = Anciennete ,into = c("start","fin")) %>%
  mutate(multiplicateur = stringr::str_sub(string = capital,end = 1))

turn_over = Table_turnover_0 %>%
  separate(col = Anciennete ,into = c("start","fin")) %>%
  mutate(TO = Taux)

calcul_multiplucateur = Vectorize(function(age){
  mult = Capital_garantie$multiplicateur[Capital_garantie$start <= age & Capital_garantie$fin >= age] %>% 
    as.double()
  if (length(mult) == 0) {
    mult = 0
  }
  return(mult)
})

calcul_turnover = Vectorize(function(age){
  TO_x = turn_over$TO[turn_over$start <= age & turn_over$fin >= age] %>% 
    as.double()
  if (length(TO_x) == 0) {
    TO_x = 0
  }
  return(TO_x)
}
)

salaire_annuelle = F
# token <- readRDS("droptoken.rds")

detect_duplicat <- function(x){ 
  if (sum(dup <- duplicated(x))==0) 
    return(dup) 
  if (class(x) %in% c("data.frame","matrix")) 
    duplicated(rbind(x[dup,],x))[-(1:sum(dup))] 
  else duplicated(c(x[dup],x))[-(1:sum(dup))] 
}

save_to_store = function(name_file, Avantages_sociau, parametre_IFC){
  wb = openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb,sheetName = "Avantages")
  openxlsx::writeData(wb,sheet = "Avantages",x = Avantages_sociau,startCol = 1,startRow = 1)
  
  openxlsx::addWorksheet(wb,sheetName = "IFC")
  openxlsx::writeData(wb,sheet = "IFC",x = parametre_IFC,startCol = 1,startRow = 1)
  
  openxlsx::saveWorkbook(wb,file = name_file,overwrite = T)
}

save_excel = function(name_excel,  df, sheet1, df2, sheet2=NULL, sheet3,evo_sal,i_act,charg_sociau,date_inventaire) {
  
  wb = openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,sheetName = sheet1)
  openxlsx::mergeCells(wb, sheet = sheet1, cols = 2:5, rows = 2)
  openxlsx::writeData(wb, sheet = sheet1,startCol = 2,startRow = 2,
                      x = "Evaluation de la provisions de L'indemnite de fin de carriere selon la norme IAS 19")
  
  openxlsx::writeData(wb, sheet = sheet1,startCol = 3,startRow = 4,
                      x = "Hypotheses et parametres")
  
  openxlsx::writeData(wb, sheet = sheet1,startCol = 3,startRow = 6:9,
                      x = c("Date d'evaluation","Taux d'actualisation",
                            "Taux d'evolution des salaires","Taux de chargement sociaux"))
  
  openxlsx::writeData(wb, sheet = sheet1,startCol = 4,startRow = 6,x = date_inventaire)
  openxlsx::writeData(wb, sheet = sheet1,startCol = 4,startRow = 7,x = i_act)
  openxlsx::writeData(wb, sheet = sheet1,startCol = 4,startRow = 8,x = evo_sal)
  openxlsx::writeData(wb, sheet = sheet1,startCol = 4,startRow = 9,x = charg_sociau)
  
  openxlsx::addStyle(wb, sheet = sheet1,
                     openxlsx::createStyle(fontSize = 13,numFmt = "DATE"),
                     rows = 6, cols = 4)
  openxlsx::addStyle(wb, sheet = sheet1,
                     openxlsx::createStyle(fontSize = 13,numFmt = "PERCENTAGE"),
                     rows = 7:9, cols = 4)
  
  openxlsx::mergeCells(wb, sheet = sheet1, cols = 2:5, rows = 12)
  openxlsx::writeData(wb, sheet = sheet1,startCol = 2,startRow = 12,
                      x = "Cout de l'indemnite de fin de carriere accordee aux salaries")
  
  openxlsx::writeData(wb, sheet = sheet1,startCol = 3,startRow = 14:15,
                      x = c("IFC","PBO"))
  
  openxlsx::writeData(wb, sheet = sheet1,startCol = 4,startRow = 14:15,
                      x = c(sum(df$IFC,na.rm = T),sum(df$PBO,na.rm = T)))
  openxlsx::addStyle(wb, sheet = sheet1,
                     openxlsx::createStyle(fontSize = 13,numFmt = "#,##0"),
                     rows = 14:15, cols = 4)
  
  openxlsx::addStyle(wb, sheet = sheet1, 
                     openxlsx::createStyle(fgFill =  "#ccaf2f",fontColour = "white",fontSize = 18),
                     rows = 2, cols = 2)
  
  openxlsx::addStyle(wb, sheet = sheet1, 
                     openxlsx::createStyle(fgFill =  "#ccaf2f",fontColour = "white",fontSize = 18),
                     rows = 12, cols = 2)
  
  openxlsx::addStyle(wb, sheet = sheet1, 
                     openxlsx::createStyle(fgFill =  "#b2d99a",fontSize = 13),
                     rows = 6:9, cols = 3)
  
  openxlsx::addStyle(wb, sheet = sheet1, 
                     openxlsx::createStyle(fgFill =  "#b2d99a",fontSize = 13),
                     rows = 14:15, cols = 3)
  
  openxlsx::setColWidths(wb, sheet = sheet1, cols = 3,widths = 30)
  openxlsx::setColWidths(wb, sheet = sheet1, cols = 4,widths = 20)
  openxlsx::setColWidths(wb, sheet = sheet1, cols = 5,widths = 55)
  
  
  # tete par tete
  openxlsx::addWorksheet(wb,sheetName = sheet3)
  openxlsx::writeData(wb,sheet = sheet3,x = df,startCol = 1,startRow = 1)
  
  openxlsx::addStyle(wb, sheet = sheet3,gridExpand = T,
                     openxlsx::createStyle(numFmt = "#,##0"),
                     rows = 1:nrow(df),cols = c(4,6:8))
  
  openxlsx::addStyle(wb, sheet = sheet3, 
                     openxlsx::createStyle(fgFill =  "#ccaf2f",
                                           fontColour = "white"),
                     rows = 1, cols = 1:8)
  
  # tete par tete
  openxlsx::addWorksheet(wb,sheetName = sheet2)
  openxlsx::writeData(wb,sheet = sheet2,x = df2,startCol = 1,startRow = 1)
  
  openxlsx::addStyle(wb, sheet = sheet2,gridExpand = T,
                     openxlsx::createStyle(numFmt = "#,##0"),
                     rows = 1:(nrow(df2)+100),cols = c(4,6:8))
  
  openxlsx::addStyle(wb, sheet = sheet2, 
                     openxlsx::createStyle(fgFill =  "#ccaf2f",
                                           fontColour = "white"),
                     rows = 1, cols = 1:8)
  
  
  openxlsx::saveWorkbook(wb, file = name_excel, overwrite = TRUE)
  
  
}




Date_retraite = function(naissance,age_retraite){
  make_date(year = year(naissance) + age_retraite,
            month = month(naissance),
            day = day(naissance))
}


calcul_age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

calcul_Salaire_finale_mensuelle = function(salaire_x){
  if (salaire_annuelle) {
    return(salaire_x / 12)
  }else{
    return(salaire_x)
  }
  
}




server <- function(input, output, session) {
    res_auth <- secure_server(
        check_credentials = check_credentials("database.sqlite")
    )
    
    output$box1 <- renderUI({
      fluidRow(column(width = 12,sidebarUserPanel(res_auth$user,
                                       subtitle = a(href = "#", 
                                                    icon("circle", class = "text-success"),
                                                    "En ligne"),
                                       image = res_auth$pic)),
               column(width = 8,offset = 4,
                      actionBttn(inputId = "logout",label = "Deconnexion", style = "gradient",color = "danger",size = "xs",icon = icon("sign-out")))
               )
        
    })
    
    #------------------------ add remove avantege
    
    output[["IFC"]] <- renderMenu({
        SidebarAvantages(
          # checkifselected = "IFC" %in% AvantageToShow$data,
                         AvantageName = "Indemnite Fin Carriere (IFC)",
                         AvantageId = "IFC",
                         Subitem1Id = "subMenuDonneesIFC",
                         Subitem2Id = "subMenuParametrageIFC",
                         Subitem3Id = "subMenuResultatIFC")
    })
    
    output[["MALADIE"]] <- renderMenu({
        SidebarAvantages(
          # checkifselected = input$MALADIEcheck,
                         AvantageName = "Couverture Medicale",
                         AvantageId = "MALADIE",
                         Subitem1Id = "subMenuDonneesMALADIE",
                         Subitem2Id = "subMenuParametrageMALADIE",
                         Subitem3Id = "subMenuResultatMALADIE")
    })
    
    output[["MedailledeTravail"]] <- renderMenu({
      SidebarAvantages(
        # checkifselected = input$MedailledeTravailcheck,
                       AvantageName = "Medaille de Travail",
                       AvantageId = "MedailledeTravail",
                       Subitem1Id = "subMenuDonneesMedailledeTravail",
                       Subitem2Id = "subMenuParametrageMedailledeTravail",
                       Subitem3Id = "subMenuResultatMedailledeTravail")
      })
    
    output[["CaisseInternedeRetraite"]] <- renderMenu({
      SidebarAvantages(
        # checkifselected = input$CaisseInternedeRetraitecheck,
                       AvantageName = "Caisse Interne de Retraite",
                       AvantageId = "MedailledeTravail",
                       Subitem1Id = "subMenuDonneesCaisseInternedeRetraite",
                       Subitem2Id = "subMenuParametrageCaisseInternedeRetraite",
                       Subitem3Id = "subMenuResultatCaisseInternedeRetraite")
    })
    
    AvantageToShow = reactiveValues()
    AvantageToShow$data = ""
    
    observe({toggleElement(id = "IFC",condition = "Indemnite Fin Carriere (IFC)" %in% AvantageToShow$data)})
    observe({toggleElement(id = "MALADIE",condition = "Couverture Medicale" %in% AvantageToShow$data)})
    observe({toggleElement(id = "MedailledeTravail",condition = "Medaille de Travail" %in% AvantageToShow$data)})
    observe({toggleElement(id = "CaisseInternedeRetraite",condition = "Caisse Interne de Retraite" %in% AvantageToShow$data)})
    
    #------------------------ next prevuouz
    
    # Previous_Button_IFC=tags$div(actionButton("Prev_Tab_IFC",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
    #                                                               ')))
    # Next_Button_IFC=div(actionButton("Next_Tab_IFC",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>')))
    # 
    # 
    # 
    # Previous_Button_MALADIE=tags$div(actionButton("Prev_Tab_MALADIE",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
    #                                                               ')))
    # Next_Button_MALADIE=div(actionButton("Next_Tab_MALADIE",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>')))
    
    
    
  
    
    


    
    
    
    table_mortalite = reactive({
      if (input$tablemortalite == "TD 88-90") {
        readxl::read_excel("Hypotheses.xlsx",sheet = "Table Mortalite") %>% 
          dplyr::select(1:2) %>% 
          `colnames<-`(c("age","lx"))
      }else{
         readxl::read_excel("Hypotheses.xlsx",sheet = "Table Mortalite") %>% 
          dplyr::select(1,3) %>% 
          `colnames<-`(c("age","lx"))
      }
    })
    
    
    Exn = Vectorize(function(x, n, i_act){
      Exn = ((1+i_act)^(-n)) * 
        table_mortalite()$lx[table_mortalite()$age == x + n] / table_mortalite()$lx[table_mortalite()$age == x]
      return(Exn)
    })
    
    
    
    
    
    
    
    values <- reactiveValues(df_data = NULL)
    values$df_data = tableconditions
    values2 <- reactiveValues(df_data = NULL)
    values2$df_data = tableconditions2
    values3 <- reactiveValues(df_data = NULL)
    values3$df_data = tableconditions3
    
    observeEvent(input$Ajouterconditioncapital, {
      aaa <<- "cond1"
      tableconditions = data.frame(`Anciennete` = NA,
                                   Sexe = NA,
                                   capital = NA)
      popdialog(whatvalid = "done",
                whatcondition = "whatcondition",
                conditions = "conditions",
                howcapital = "howcapital",
                uitable = "uicapital",
                ajoutcondition = "ajoutcondition",
                suppcondition = "suppcondition",
                tableconditions = "tableconditions")
    })
    
    observeEvent(input$Ajouterconditionevolutionsalire, {
      aaa <<- "cond2"
      tableconditions2 = data.frame(`Anciennete` = NA,
                                    Sexe = NA,
                                    Taux = NA)
      
      popdialog(whatvalid = "done",
                whatcondition = "whatcondition",
                conditions = "conditions",
                howcapital = "howcapital",
                uitable = "uitaux",
                ajoutcondition = "ajoutcondition",
                suppcondition = "suppcondition",
                tableconditions = "tableconditions2")
    })
    
    observeEvent(input$Ajouterconditionturnover, {
      aaa <<- "cond3"
      tableconditions3 = data.frame(`Anciennete` = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59"),
                                    Sexe = NA,
                                    Taux = c(2.26,1.76,2.81,1.74,0.56,.22,.05,0)/100)
      
      popdialog(whatvalid = "done",
                whatcondition = "whatcondition",
                conditions = "conditions",
                howcapital = "howcapital",
                uitable = "uitaux",
                ajoutcondition = "ajoutcondition",
                suppcondition = "suppcondition",
                tableconditions = "tableconditions3")
    })
    
    output$uitaux = renderUI({
      numericInputIcon(inputId = "ttaux",label = NULL,step = .01,value = 3.5,min = 0,max = 10,icon = list(NULL, icon("percent")))
    })
    
    
    observeEvent(input$whatcondition,{
      if (aaa == "cond1") {values$df_data = tableconditions}
      if (aaa == "cond2") {values2$df_data = tableconditions2}
      if (aaa == "cond3") {values3$df_data = tableconditions3}
    })
    
    
    
    output$tableconditions = DT::renderDataTable({
      
      conditonshow = values$df_data[,c(input$whatcondition,"capital")]
      conditonshow %>% 
        DT::datatable(rownames = F)
    })
    
    output$tableconditions2 = DT::renderDataTable({
      
      conditonshow = values2$df_data[,c(input$whatcondition,"Taux")]
      conditonshow %>% 
        DT::datatable(rownames = F)
    })
    
    output$tableconditions3 = DT::renderDataTable({
      
      conditonshow = values3$df_data[,c(input$whatcondition,"Taux")]
      conditonshow %>% 
        DT::datatable(rownames = F)
    })
    
    ajoutcondition = function(what){
      if (!is.na(what)) {
        if (what == "Sexe") {
          return(fluidRow(column(offset = 1,width = 4,radioGroupButtons(inputId = "Sexecondition",label = "Sexe",choices = c("M", "F"),justified = TRUE))))
        }
        if (what == "Anciennete") {
          return(fluidRow(column(offset = 1,width = 9,sliderTextInput(inputId = "Anciennetecondition",label = "Anciennete",choices = seq(0,input$ageretraite - 15,by = 1),selected = c(5,10)))))
        }
      }
    }
    
    output$conditions = renderUI({
      fluidRow(ajoutcondition(input$whatcondition[1]),
               ajoutcondition(input$whatcondition[2]))
    })
    
    
    
    
    
    observeEvent(input$close,{
      show_alert(
        session = session,
        btn_labels = NA,
        title = "Etes vous sÃ»r ?",
        text = tags$div(h5(""), 
                        actionButton("yes", "Oui"),
                        actionButton("no", "Non")
        ),
        type = "warning"
      )
    })
    
    observeEvent(input$yes,{
      closeSweetAlert()
      removeModal()
      removeModal()
    })
    
    observeEvent(input$no,{
      closeSweetAlert()
    })
    
    Capital_garantie = reactiveValues(data = NULL)
    turn_over = reactiveValues(data = NULL)
    
    observeEvent(input$done,{
      if (aaa == "cond1") {Capital_garantie$data <<- values$df_data[,c(input$whatcondition,"capital")]}
      if (aaa == "cond2") {data2 <<- values2$df_data[,c(input$whatcondition,"Taux")]}
      if (aaa == "cond3") {turn_over$data <<- values3$df_data[,c(input$whatcondition,"Taux")]}
      removeModal()
      removeModal()
    })
    
    
    
    
    
    
    
    
    
    
    

    # output$download_IFC <- downloadHandler(
    #   filename = function() {
    #     paste("data_output", Sys.time() %>% gsub(pattern = ":" ,replacement = "."), ".xlsx", sep="")
    #   },
    #   content = function(file) {
    #     save_excel(name_excel = file,
    #                sheet1 = "Calcul Engagement",
    #                sheet2 = "Donnees Invalides",
    #                sheet3 = "Resultats tete par tete",
    #                df = calcul_tete_tete(),
    #                df2 = test()[[3]],
    #                evo_sal = input$prof_carr/100,
    #                i_act = input$taux_dact/100,
    #                charg_sociau = input$taux_charg_socio/100,
    #                date_inventaire = as.Date(input$dateinventaire)
    #                )
    #   }
    # )
    
    shinyjs::disable(selector = '.navbar-nav a')
    
    observeEvent(input$Next, {
      if (as.numeric(input$inTabset) < 4) {
        
        # if (input$inTabset == 3 & sum(test()[[1]]$n[c(2,4:6)],na.rm = T)>0) {
        #   sendSweetAlert(
        #     session = session,
        #     title = "L'engagement IFC va ÃÂÃÂÃÂÃÂªtre calculÃÂÃÂÃÂÃÂ© que pour les donnÃÂÃÂÃÂÃÂ©es validÃÂÃÂÃÂÃÂ©es",
        #     text = NULL,
        #     type = "warning"
        #   )
        # }
        
        updateTabsetPanel(session, "inTabset",
                          selected = as.character(as.numeric(input$inTabset)+1))
      }
      
    })
    
    observeEvent(input$Previous, {
      if (as.numeric(input$inTabset) > 1) {
        updateTabsetPanel(session, "inTabset",
                          selected = as.character(as.numeric(input$inTabset)-1))
      }
      
    })
    
    
    # output$downloader <-
    #   downloadHandler(
    #     "synthese.docx",
    #     content =
    #       function(file)
    #       {
    #         rmarkdown::render(
    #           input = "rapport.Rmd",
    #           output_file = "built_report.docx",
    #           params = list(plot = test()[[1]])
    #         )
    #         readBin(con = "built_report.docx",
    #                 what = "raw",
    #                 n = file.info("built_report.docx")[, "size"]) %>%
    #           writeBin(con = file)
    #       }
    #   )
    
    # observe({
    #   req(input$dateinventaire,
    #       input$ageretraite,
    #       input$taux_dact,
    #       input$prof_carr,
    #       input$taux_charg_socio,
    #       res_auth$user)
    #   # try({
    #     para_IFC = data.frame(parametre = c("Date d'ÃÂÃÂ©valuation",
    #                                         "Age retraite",
    #                                         "Taux d'actualisation",
    #                                         "Evolution de Salaire",
    #                                         "Taux de chargement sociaux"),
    #                           valeur = c(as.character(input$dateinventaire),
    #                                      input$ageretraite,
    #                                      input$taux_dact,
    #                                      input$prof_carr,
    #                                      input$taux_charg_socio))
    #     # },silent = T)
    #   
    #     pathhhh = paste0(res_auth$user, ".xlsx")
    #     
    #     save_to_store(name_file = pathhhh,
    #                   Avantages_sociau = input$show_optional_sidebar_item,
    #                   parametre_IFC = para_IFC)
    #     
    #     if (drop_exists(pathhhh,dtoken = token)) {
    #       drop_delete(pathhhh,dtoken = token)
    #     }
    #     
    #     drop_upload(pathhhh,dtoken = token)
    #     
    #   
    #   
    #   
    # })
    
    output$par_ui_IFC = renderUI({

      # pathhhh = paste0(res_auth$user, ".xlsx")
      # if (drop_exists(pathhhh,dtoken = token)) {
      #   drop_download(pathhhh,dtoken = token,overwrite = T)
      #   last_par = readxl::read_excel(pathhhh,sheet = "IFC")
      #   function_par_IFC(val1 = as.Date(last_par$valeur[1]),
      #                    val2 = as.double(last_par$valeur[2]),
      #                    val3 = as.double(last_par$valeur[3]),
      #                    val4 = as.double(last_par$valeur[4]),
      #                    val5 = as.double(last_par$valeur[5]))
      # 
      # } else {
        function_par_IFC()
      # }

      })
    

      
      
      
    # })
    
    # ============================================================
    #   ============================================================
    #   ============================================================
    #   ============================================================
    #   ============================================================
    #   ============================================================
      
    
    output$uitauxactualisation = renderUI({
      if (input$choixtauxactualisation == "Taux fixe") {
        numericInputIcon(inputId = "valeur_taux_actualisation",label = NULL,step = .01,value = 3.5,min = 0,max = 10,icon = list(NULL, icon("percent")))
      }else{
        p("Pas encore")
      }
    })
    
    output$howcapital2 = renderUI({
      if (input$Parcategorie) {
        actionBttn(inputId = "Ajouterconditioncapital",label = "Ajouter des conditions",size = "xs")
      
        }else{
        fluidRow(column(width = 1,switchInput(inputId = "howcapital",label = "Fixe",value = T,onLabel = "Oui",offLabel = "Non")),
                 column(width = 9,offset = 3,uiOutput("uicapital")))
      }
    })
    # 
    # 
    output$uicapital = renderUI({
      if (input$howcapital == T) {
        fluidRow(column(
          width = 10,
          offset = 1,
          autonumericInput(
            inputId = "capitalfixe",
            currencySymbol = "  MAD",
            label = NULL,
            value = 100000,
            currencySymbolPlacement = "s",
            decimalPlaces = 0,
            digitGroupSeparator = ",",
          )
          ))

      }else{
        fluidRow(column(width = 6,numericInputIcon(inputId = "foissalaire",label = NULL,value = 3,min = 1,max = 10,
                                                   icon = list(NULL, icon("times"))
                                                   )),
                 column(width = 6,pickerInput(inputId = "periodicitesalaire",label = NULL, choices = c("Salaire Mensuelle", "Salaire Annuelle"),width = "fit"))
        )
      }


    })
    # 
    # output$cond1 = renderUI({
    #   if ("Anciennete" %in% input$whatconditioncapital) {
    #     sliderTextInput(
    #       inputId = "Anciennetecondition",
    #       label = "Anciennete",
    #       choices = seq(0,input$ageretraite - 15,by = 1),
    #       selected = c(5,10))
    #   }
    # })
    # 
    # output$cond2 = renderUI({
    #   if ("Sexe" %in% input$whatconditioncapital) {
    #     radioGroupButtons(
    #       inputId = "Sexecondition",
    #       label = "Sexe",
    #       choices = c("M", "F"),
    #       justified = TRUE
    #     )
    #   }
    # })
    # 
    # values <- reactiveValues(df_data = NULL)
    # values$df_data = Table_Capital_0
    # 
    # 
    # observeEvent(input$whatconditioncapital,{
    #   values$df_data = Table_Capital_0
    # })
    # 
    # observeEvent(input$ajoutconditioncapital,{
    #   categoriecapital <- values$df_data
    #   sex = anc = amount = NA
    #   if ("Anciennete" %in% input$whatconditioncapital) {anc = paste0(input$Anciennetecondition,collapse = "-")}
    #   if ("Sexe" %in% input$whatconditioncapital) {sex = input$Sexecondition}
    #   if (input$howcapital) {amount = input$capitalfixe}else{amount = paste0(input$foissalaire ," X ", input$periodicitesalaire)}
    #   categoriecapital = rbind(categoriecapital, c(anc, sex,amount)) %>% 
    #     filter(!is.na(capital))
    #   i_unique = duplicated(categoriecapital[,-ncol(categoriecapital)])
    #   values$df_data = categoriecapital[!i_unique,]
    #   output$tablecapital = DT::renderDataTable({
    #     
    #     Capital_garantie = values$df_data[,c(input$whatconditioncapital,"capital")]
    #     Capital_garantie %>% 
    #       DT::datatable(rownames = F)
    #   })
    # })
    # 
    # 
    # observeEvent(input$suppconditioncapital,{
    #   values$df_data = values$df_data[-nrow(values$df_data),]
    #   if (nrow(values$df_data)==0) {
    #     values$df_data = Table_Capital_0
    #   }
    # })



    ############## evolution salaire ##############

    output$uitauxaevolutionsalire = renderUI({
      if (input$choixtauxevolutionsalire == "Taux fixe") {
        numericInputIcon(inputId = "valeur_taux_evolutionsalire",label = NULL,step = .01,value = 3.5,min = 0,max = 10,icon = list(NULL, icon("percent")))
      }else{
        actionBttn(inputId = "Ajouterconditionevolutionsalire",label = "Ajouter des conditions",size = "xs")
      }
    })
    
    output$uitauxturnover = renderUI({
      if (input$choixturnover == "Taux fixe") {
        numericInputIcon(inputId = "valeur_taux_turnover",label = NULL,step = .01,value = 3.5,min = 0,max = 10,icon = list(NULL, icon("percent")))
      }else{
        actionBttn(inputId = "Ajouterconditionturnover",label = "Ajouter des conditions",size = "xs")
      }
    })
    
    # #
    # output$evolutionsalire_cond1 = renderUI({
    #   if ("Anciennete" %in% input$whatconditionevolutionsalire) {
    #     sliderTextInput(
    #       inputId = "Ancienneteconditionevolutionsalire",
    #       label = "Anciennete",
    #       choices = seq(0,input$ageretraite - 15,by = 1),
    #       selected = c(5,10))
    #   }
    # })
    # 
    # output$evolutionsalire_cond2 = renderUI({
    #   if ("Sexe" %in% input$whatconditionevolutionsalire) {
    #     radioGroupButtons(
    #       inputId = "Sexeconditionevolutionsalire",
    #       label = "Sexe",
    #       choices = c("M", "F"),
    #       justified = TRUE
    #     )
    #   }
    # })
    # #
    # valuesevolutionsalire <- reactiveValues(df_data = NULL)
    # valuesevolutionsalire$df_data = Table_evolutionsalire_0
    # 
    # observeEvent(input$whatconditionevolutionsalire,{
    #   valuesevolutionsalire$df_data = Table_evolutionsalire_0
    # })
    # 
    # observeEvent(input$ajoutconditionevolutionsalire,{
    #   evolutionsalire <- valuesevolutionsalire$df_data
    #   print(evolutionsalire)
    #   sex = anc = amount = NA
    #   if ("Anciennete" %in% input$whatconditionevolutionsalire) {anc = paste0(input$Ancienneteconditionevolutionsalire,collapse = "-")}
    #   if ("Sexe" %in% input$whatconditionevolutionsalire) {sex = input$Sexeconditionevolutionsalire}
    #   if (input$howcapital) {amount = input$valeur_taux_evolutionsalire2}
    #   evolutionsalire = rbind(evolutionsalire, c(anc, sex,amount)) %>%
    #     filter(!is.na(Taux))
    #   i_uniqueevolutionsalire = duplicated(evolutionsalire[,-ncol(evolutionsalire)])
    #   valuesevolutionsalire$df_data = evolutionsalire[!i_uniqueevolutionsalire,]
    #   output$table_evolutionsalire = DT::renderDataTable({
    #     valuesevolutionsalire$df_data[,c(input$whatconditionevolutionsalire,"Taux")] %>%
    #       DT::datatable(rownames = F)
    #   })
    # })
    # 
    # 
    # observeEvent(input$suppconditionevolutionsalire,{
    #   valuesevolutionsalire$df_data = valuesevolutionsalire$df_data[-nrow(valuesevolutionsalire$df_data),]
    #   if (nrow(valuesevolutionsalire$df_data)==0) {
    #     valuesevolutionsalire$df_data = Table_evolutionsalire_0
    #   }
    # })
    # #
    # #
    # # ############## Turnover ##############
    # #
    # output$uitauxturnover = renderUI({
    #   if (input$choixturnover == "Taux fixe") {
    #     numericInputIcon(inputId = "valeur_taux_turnover",label = NULL,step = .01,value = 3.5,min = 0,max = 10,icon = list(NULL, icon("percent")))
    #   }else{
    #     dropdownButton(right = T,
    #                    selectInput(inputId = 'whatconditionturnover',label = "L'augmentation de salaire sera en fonction de :",choices = c("Anciennete","Sexe"),selected = "Anciennete",multiple = TRUE),
    #                    br(),p("Si, "),
    #                    fluidRow(column(width = 5,uiOutput(outputId = "turn_cond1")),
    #                             column(width = 2,uiOutput(outputId = "turn_cond2"))),
    # 
    #                    br(),p("Alors, le taux sera :"),
    #                    fluidRow(column(width = 7,numericInputIcon(inputId = "valeur_taux_turnover2",label = NULL,step = .01,value = 3.5,min = 0,max = 10,icon = list(NULL, icon("percent"))))),
    #                    fluidRow(column(width = 1,actionBttn(inputId = "ajoutconditionturnover","Ajouter",color = "success",style = "material-flat",size = "xs")),
    #                             column(width = 1,actionBttn(inputId = "suppconditionturnover","Supprimer",color = "danger",style = "material-flat",size = "xs"))),
    #                    br(),
    #                    DT::dataTableOutput(outputId = "table_turnover"),
    # 
    #                    circle = TRUE, status = "primary",size = "xs",
    #                    icon = icon("plus"), width = "700px",
    #                    tooltip = tooltipOptions(title = "Ajouter des conditions")
    #     )
    #   }
    # })
    # 
    # output$turn_cond1 = renderUI({
    #   if ("Anciennete" %in% input$whatconditionturnover) {
    #     sliderTextInput(
    #       inputId = "Ancienneteconditioneturnover",
    #       label = "Anciennete",
    #       choices = seq(0,input$ageretraite - 15,by = 1),
    #       selected = c(5,10))
    #   }
    # })
    # 
    # output$turn_cond2 = renderUI({
    #   if ("Sexe" %in% input$whatconditionturnover) {
    #     radioGroupButtons(
    #       inputId = "Sexeconditionturnover",
    #       label = "Sexe",
    #       choices = c("M", "F"),
    #       justified = TRUE
    #     )
    #   }
    # })
    # 
    # valuesevolutionturnover <- reactiveValues(df_data = NULL)
    # valuesevolutionturnover$df_data = Table_turnover_0
    # 
    # observeEvent(input$whatconditionturnover,{
    #   valuesevolutionturnover$df_data = Table_turnover_0
    # })
    # 
    # observeEvent(input$ajoutconditionturnover,{
    #   evolutionsalire <- valuesevolutionturnover$df_data
    #   print(evolutionsalire)
    #   sex = anc = amount = NA
    #   if ("Anciennete" %in% input$whatconditionturnover) {anc = paste0(input$Ancienneteconditioneturnover,collapse = "-")}
    #   if ("Sexe" %in% input$whatconditionturnover) {sex = input$Sexeconditionturnover}
    #   if (input$howcapital) {amount = input$valeur_taux_turnover2}
    #   evolutionsalire = rbind(evolutionsalire, c(anc, sex,amount)) %>%
    #     filter(!is.na(Taux))
    #   i_uniqueevolutionsalire = duplicated(evolutionsalire[,-ncol(evolutionsalire)])
    #   valuesevolutionturnover$df_data = evolutionsalire[!i_uniqueevolutionsalire,]
    #   output$table_turnover = DT::renderDataTable({
    #     
    #     turn_over = valuesevolutionturnover$df_data[,c(input$whatconditionturnover,"Taux")]
    #     turn_over %>%
    #       DT::datatable(rownames = F)
    #   })
    # })
    # 
    # 
    # observeEvent(input$suppconditionturnover,{
    #   valuesevolutionturnover$df_data = valuesevolutionturnover$df_data[-nrow(valuesevolutionturnover$df_data),]
    #   if (nrow(valuesevolutionturnover$df_data)==0) {
    #     valuesevolutionturnover$df_data = Table_turnover_0
    #   }
    # })
    # 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    data = reactive({
      readxl::read_excel("Donnees Indiv.xlsx",
                         col_names = c("Matricule","Naissance","Entree","Salaire","Grades","Sexe"),
                         col_types = c("text","text","text","text","text","text"),
                         skip = 1)
    })
    
    
    test = reactive({
      
      ncol = dim(data())[2]
      
      options(warn = -1)
      
      datatest = data()
      datatest[,"doublons"] = detect_duplicat(datatest[,1] %>% pull)
      datatest = datatest %>% 
        naniar::add_n_miss() %>% 
        mutate(Remarque = "") %>% 
        mutate(Remarque = if_else(n_miss_all>0,true = "Valeures Manquants",false = Remarque,missing = Remarque)) %>% 
        mutate(Remarque = if_else(doublons & !is.na(Matricule),true = "Matricule en doublons",false = Remarque,missing = Remarque)) %>% 
        mutate(Remarque = if_else(can.be.numeric(Naissance),false = paste(Remarque,"Naissance Invalides",sep = ", "),true = Remarque,missing = Remarque)) %>%
        mutate(Remarque = if_else(can.be.numeric(Entree),false = paste(Remarque,"Date d entree Invalides",sep = ", "),true = Remarque,missing = Remarque)) %>%
        mutate(Remarque = if_else(can.be.numeric(Salaire),false = paste(Remarque,"Salaire Invalides",sep = ", "),true = Remarque,missing = Remarque)) %>%
        mutate(Entree2 = assssdate(Entree), Naissance2 = assssdate(Naissance), Salaire2 = as.double(Salaire)) %>%
        mutate(age = calcul_age(Naissance2, as.Date(input$dateevaluation)),
               anciennete = calcul_age(Entree2, as.Date(input$dateevaluation)),
               agetravail = calcul_age(Naissance2, Entree2)) %>%
        mutate(Remarque = if_else(age > input$AgeMax,true = paste(Remarque,paste("Age >", input$AgeMax),sep = ", "),false = Remarque,missing = Remarque)) %>%
        mutate(Remarque = if_else(age < input$AgeMin,true = paste(Remarque,paste("Age <", input$AgeMin),sep = ", "),false = Remarque,missing = Remarque)) %>%
        mutate(Remarque = if_else(agetravail < input$AgeentreeMin,true = paste(Remarque,paste("Age travail <", input$AgeentreeMin),sep = ", "),false = Remarque,missing = Remarque)) %>%
        mutate(Remarque = if_else(anciennete < 0,true = paste(Remarque,"Date d'entre > Date d'evaluation",sep = ", "),false = Remarque,missing = Remarque)) %>%
        mutate(Remarque = if_else(Salaire2 > input$SalaireMax,true = paste(Remarque,paste("Salaire >", input$SalaireMax),sep = ", "),false = Remarque,missing = Remarque)) %>%
        mutate(Remarque = if_else(Salaire2 < input$SalaireMin,true = paste(Remarque,paste("Salaire <", input$SalaireMin),sep = ", "),false = Remarque,missing = Remarque)) %>% 
        mutate(Naissance= if_else(is.na(Naissance2),Naissance,as.character(Naissance2)),
               Entree= if_else(is.na(Entree2),Entree,as.character(Entree2)))
      
      
      data_good = datatest %>% 
        filter(Remarque == "") %>% 
        mutate(Naissance = Naissance2, Entree = Entree2, Salaire = Salaire2) %>% 
        select(1:ncol)
      
      data_bad = datatest %>% 
        filter(Remarque != "") %>% 
        select(1:ncol, Remarque)
      
      Analyse_donnees = data.frame(`.` = c("Nombre d'observation Total","Nombre d'observation invalides",
                                           "Doublons", "Valeurs manquantes","Valeurs erronees", 
                                           paste("Age <", input$AgeMin),
                                           paste("Age >", input$AgeMax),
                                           paste("Age travail <", input$AgeentreeMin),
                                           "Date d'entre > Date d'evaluation",
                                           paste("Salaire <", input$SalaireMin),
                                           paste("Salaire >", input$SalaireMax)
                                           ),
                                   n = rep(0,11))
      
      Analyse_donnees[,2][1] = nrow(data())
      Analyse_donnees[,2][2] = nrow(data_bad)
      Analyse_donnees[,2][3] = datatest$Remarque %>% stringr::str_count("doublons") %>% sum(na.rm = T)
      Analyse_donnees[,2][4] = datatest$Remarque %>% stringr::str_count("Manquants") %>% sum(na.rm = T)
      Analyse_donnees[,2][5] = datatest$Remarque %>% stringr::str_count("Invalides") %>% sum(na.rm = T)
      Analyse_donnees[,2][6] = datatest$Remarque %>% stringr::str_count("Age <") %>% sum(na.rm = T)
      Analyse_donnees[,2][7] = datatest$Remarque %>% stringr::str_count("Age >") %>% sum(na.rm = T)
      Analyse_donnees[,2][8] = datatest$Remarque %>% stringr::str_count("Age travail <") %>% sum(na.rm = T)
      Analyse_donnees[,2][9] = datatest$Remarque %>% stringr::str_count("evaluation") %>% sum(na.rm = T)
      Analyse_donnees[,2][10] = datatest$Remarque %>% stringr::str_count("Salaire <") %>% sum(na.rm = T)
      Analyse_donnees[,2][11] = datatest$Remarque %>% stringr::str_count("Salaire >") %>% sum(na.rm = T)
      
      
      options(warn = 0)

      return(list(Analyse_donnees,data_good,data_bad))
      
    })
    
    output$datatemplatescreenIFC <- renderUI({
      HTML('<center><img src="/data_template.png"></center>')
      })
    
    observeEvent(input$sssss,{
      saveRDS(hot_to_r(input$datashowuiIFC),"Projects.rds")
    })
    
    output$datashowuiIFC = renderRHandsontable({
      
      if (flagRH$value) {
        data() %>% 
          rhandsontable(height = 500,language = "fr-FR")
      }else{
        data() %>% 
          rhandsontable(height = 500,language = "fr-FR") %>% 
          hot_col("Salaire", "password")
      }
    })
    
    
    # output$datashowinvalideIFC = renderDataTable({
    #   test()[[3]] %>% 
    #     datatable(rownames = F,editable = T,
    #               options = list(columnDefs =
    #                                list(list(className = 'dt-right',
    #                                          targets = "_all"))))
    #   })
    
    output$datashowinvalideIFC = renderRHandsontable({
      if (flagRH$value) {
        rhandsontable(test()[[3]],height = 400,language = "fr-FR")
      }else{
        rhandsontable(test()[[3]],height = 400,language = "fr-FR") %>% 
          hot_col("Salaire", "password")
      }
      })
    
    output$datashowvalide = renderRHandsontable({
      if (flagRH$value) {
        rhandsontable(test()[[2]],height = 400,language = "fr-FR")
      }else{
        rhandsontable(test()[[2]],height = 400,language = "fr-FR") %>% 
          hot_col("Salaire", "password")
      }
      
      # test()[[2]] %>% 
      #   datatable(rownames = F,editable = T,
      #             options = list(columnDefs =
      #                              list(list(className = 'dt-right',
      #                                        targets = "_all"))))
    })
      
    output$datashowanalyseIFC = renderTable({
      test()[[1]] %>% 
        mutate_at(2,scales::number,accuracy = 1)
      })
    
    
    
    Statistique = reactive({
      test()[[2]] %>% 
        mutate(age = calcul_age(Naissance,input$dateevaluation),
               Anciennete = calcul_age(Entree,input$dateevaluation))
    })
    
    output$datastatistiqueIFC = DT::renderDataTable({
      Statistique() %>%
        group_by(Grades) %>%
        summarise(n = n(),
                  `Age Moyenne` = mean(age),
                  `Anciennete Moyenne` = mean(Anciennete),
                  `Salaire Moyenne` = mean(Salaire)) %>%
        mutate_at(2:5,scales::number,accuracy = 1) %>%
        datatable(rownames = F,editable = T,
                  options = list(columnDefs =
                                   list(list(className = 'dt-right',
                                             targets = "_all"))))
    })
    
    
    output$plotstatby = renderUI({
      pickerInput(
        inputId = "plotstatbycheck",
        label = "Par groupe", 
        choices = skim_without_charts(data())$skim_variable[skim_without_charts(data())$character.n_unique <= 5]
        
      )
    })
    
    output$plotstatage1IFC = renderHighchart({
      Statistique() %>% 
        count(age) %>% 
        hchart('area', hcaes(x = 'age', y = 'n'))
    })
    
    output$plotstatage2IFC = renderHighchart({
      req(input$plotstatbycheck)
      dataaashow = Statistique() %>%
        select(age, input$plotstatbycheck) %>% 
        group_by_all() %>% 
        count()
      colnames(dataaashow)[2] = "groupby"
      dataaashow %>% 
        hchart('area', hcaes(x = 'age', y = 'n', group = groupby))
    })
    
    output$plotstatanciennete1IFC = renderHighchart({
      Statistique() %>% 
        count(Anciennete) %>% 
        hchart('area', hcaes(x = 'Anciennete', y = 'n'))
    })
    
    output$plotstatanciennete2IFC = renderHighchart({
      req(input$plotstatbycheck)
      dataaashow = Statistique() %>%
        select(Anciennete, input$plotstatbycheck) %>% 
        group_by_all() %>% 
        count()
      colnames(dataaashow)[2] = "groupby"
      dataaashow %>% 
        hchart('area', hcaes(x = 'Anciennete', y = 'n', group = groupby))
    })
    
    # output$plotstatsalaire1IFC = renderHighchart({
    #   hchart(
    #     density(Statistique()$Salaire,
    #             from = min(Statistique()$Salaire), 
    #             to = max(Statistique()$Salaire)),
    #     type = "area", name = "Salaire"
    #   )
    # })
    # 
    # output$plotstatsalaire2IFC = renderHighchart({
    #   req(input$plotstatbycheck)
    #   df = Statistique()
    #   ds = map(levels(as.factor(df[,input$plotstatbycheck] %>% pull())), function(x){
    #     dt = density(df$Salaire[df[,input$plotstatbycheck] == x], 
    #                  from = min(df$Salaire[df[,input$plotstatbycheck] == x]), 
    #                  to = max(df$Salaire[df[,input$plotstatbycheck] == x]))
    #     dt = dt[1:2]
    #     dt = list_parse2(as.data.frame(dt))
    #     list(data = dt, name = x)
    #   })
      
    #   highchart() %>% 
    #     hc_add_series_list(ds)
    # })
    
    
    # parstore = reactiveValues()
    # parstore$CapitalChoixIFC = 1000
    
      
    
    # observeEvent(input$CapitalChoixIFC,{
    #   parstore$CapitalChoixIFC = input$capitalfixeIFC
    # })
    
    
    
    output$CapitalChoixuiIFC = renderUI({
      CapitalUi(valuecap = 10000,
                modalnew = "modalnewIFC",
                cHoixCapitalUser = input$CapitalChoixIFC,
                capitalfixe = "capitalfixeIFC",
                foissalaire = "foissalaireIFC",
                AjoutConditionsCapital = "AjoutConditionsCapitalIFC",
                DataconditionsCapital = "DataconditionsCapitalIFC",
                whatconditionCapital = "whatconditionCapitalIFC")
    })
    
    output$DataconditionsCapitalIFC = renderDataTable({
      data.frame(`Anciennete` = c("15-19","20-24","25-29","30-65"),
                 Grades = NA,
                 capital = paste0(c(2,3,4,6)," X Salaire Mensuelle")) %>% 
        select(input$whatconditionCapitalIFC, capital) %>% 
        datatable(rownames = F,editable = T)
    })
    
    output$tauxactualisationuiIFC = renderUI({
      TauxUi(modalnew = "modalnewactIFC",
             CHoixTauxUser = input$tauxactualisationchoixIFC,
             whatconditionTaux = "whatconditionTauxactIFC",
             DataconditionsTaux = "DataconditionsTauactxIFC",
             AjoutConditionsTaux = "AjoutConditionsTauxactIFC",
             TauxFixe = "TauxactFixeIFC", 
             # value0 = 3.5
             )
    })
    
    output$DataconditionsTauactxIFC = renderDataTable({
      data.frame(Maturite = 1:60, ZC = (seq(3.7,5,length.out = 60)/100) %>% round(digits = 4)) %>% 
        datatable(rownames = F,editable = T)
    })
    
    output$tauxaevolutionsalireuiIFC = renderUI({
      TauxUi(modalnew = "modalnewSalaireIFC",
             CHoixTauxUser = input$tauxevolutionsalirechoixIFC,
             whatconditionTaux = "whatconditionTauxSalaireIFC",
             DataconditionsTaux = "DataconditionsTauxSalaireIFC",
             AjoutConditionsTaux = "AjoutConditionsTauxSalaireIFC",
             TauxFixe = "TauxsalaireFixeIFC",
             Tauxact = F#,  
             # value0 = 3.5
             )
    })
    
    output$DataconditionsTauxSalaireIFC = renderDataTable({
      data.frame(`Anciennete` = NA, Grades = NA, Taux = NA) %>% 
        select(input$whatconditionTauxSalaireIFC, Taux) %>% 
        datatable(rownames = F,editable = T)
    })
    
    output$tauxchargementsociauxuiIFC = renderUI({
      TauxUi(modalnew = "modalnewchargsociauIFC",
             CHoixTauxUser = input$tauxchargementsociauxchoixIFC,
             whatconditionTaux = "whatconditionTauxchargsociauIFC",
             DataconditionsTaux = "DataconditionsTauxchargsociauIFC",
             AjoutConditionsTaux = "AjoutConditionsTauxchargsociauIFC",
             TauxFixe = "TauxchargsociauFixeIFC",
             Tauxact = F#, value0 = 0
             )
    })
    
    output$DataconditionsTauxchargsociauIFC = renderDataTable({
      data.frame(`Anciennete` = NA, Grades = NA, Taux = NA) %>% 
        select(input$whatconditionTauxchargsociauIFC, Taux) %>% 
        datatable(rownames = F,editable = T)
    })
    
    output$tauxturnoveruiIFC = renderUI({
      TauxUi(modalnew = "modalnewturnoverIFC",
             CHoixTauxUser = input$turnoverchoixIFC,
             whatconditionTaux = "whatconditionTauxturnoverIFC",
             DataconditionsTaux = "DataconditionsTauxturnoverIFC",
             AjoutConditionsTaux = "AjoutConditionsTauxturnoverIFC",
             TauxFixe = "TauxturnoverFixeIFC",
             Tauxact = F#,value0 = 2.5
             )
    })

    output$DataconditionsTauxturnoverIFC = renderDataTable({
      data.frame(`Anciennete` = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59"),
                 Grades = NA,
                 Taux = c(2.26,1.76,2.81,1.74,0.56,.22,.05,0)/100) %>%
        select(input$whatconditionTauxturnoverIFC, Taux) %>%
        datatable(rownames = F,editable = T)
    })
    
    output$senareoIFCui2 = renderUI({
      senareoIFC22(dateinventaire2 = "dateinventaire2",
                   tablemortalite2 = "tablemortalite2",
                   ageretraite2 = "ageretraite2",
                   CapitalChoix2 = "CapitalChoixIFC2",
                   Capitalui2 = "CapitalChoixuiIFC2",
                   tauxactualisationchoix2 = "tauxactualisationchoixIFC2",
                   tauxactualisationui2 = "tauxactualisationuiIFC2",
                   tauxevolutionsalirechoix2 = "tauxevolutionsalirechoixIFC2",
                   tauxaevolutionsalireui2 = "tauxaevolutionsalireuiIFC2",
                   tauxchargementsociauxchoix2 = "tauxchargementsociauxchoixIFC2",
                   turnoverchoix2 = "turnoverchoixIFC2",
                   tauxchargementsociauxui2 = "tauxchargementsociauxuiIFC2",
                   tauxturnoverui2 = "tauxturnoveruiIFC2")
      })
    
    output$senareoIFCui3 = renderUI({
      senareoIFC33(dateinventaire3 = "dateinventaire3",
                   tablemortalite3 = "tablemortalite3",
                   ageretraite3 = "ageretraite3",
                   CapitalChoix3 = "CapitalChoixIFC3",
                   Capitalui3 = "CapitalChoixuiIFC3",
                   tauxactualisationchoix3 = "tauxactualisationchoixIFC3",
                   tauxactualisationui3 = "tauxactualisationuiIFC3",
                   tauxevolutionsalirechoix3 = "tauxevolutionsalirechoixIFC3",
                   tauxaevolutionsalireui3 = "tauxaevolutionsalireuiIFC3",
                   tauxchargementsociauxchoix3 = "tauxchargementsociauxchoixIFC3",
                   turnoverchoix3 = "turnoverchoixIFC3",
                   tauxchargementsociauxui3 = "tauxchargementsociauxuiIFC3",
                   tauxturnoverui3 = "tauxturnoveruiIFC3")
    })
    
    senareoIFC2(outpt = output, inpt = input)
    senareoIFC3(outpt = output, inpt = input)
    senareoAutre(outpt = output, inpt = input)
    
    
    Resultat = function(df,evo_sal, i_act, charg_sociau,date_inventaire,age_retraite) {
      df %>% 
        mutate(Naissance = as.Date(Naissance),
               Entree = as.Date(Entree),
               age = calcul_age(Naissance,date_inventaire),
               date_retraite = Date_retraite(Naissance,age_retraite),
               Anc_Actuelle = as.double((date_inventaire - Entree)) / 365.25,
               Anc_Retraite = as.double(date_retraite - Entree) / 365.25,
               SFC = calcul_Salaire_finale_mensuelle(Salaire) * ((1 + evo_sal)^(age_retraite - age)),
               IFC = SFC * calcul_multiplucateur(age),
               PBO = IFC * Exn(age,age_retraite - age, i_act) *
                 ((1 - calcul_turnover(age))^(age_retraite - 1 - age)) *
                 (Anc_Actuelle / Anc_Retraite) *
                 (1 + charg_sociau)
        )
    }
    
    
    calcul_tete_tete = reactive({
      Resultat(df = test()[[2]],
               evo_sal = input$TauxsalaireFixeIFC/100,
               i_act = input$TauxactFixeIFC/100,
               charg_sociau = input$TauxchargsociauFixeIFC/100,
               date_inventaire = as.Date(input$dateinventaire),
               age_retraite = input$ageretraite)
    })
    
    output$TableresultatteteIFC =  renderDT({
      calcul_tete_tete() %>%
        select(Matricule,Naissance,Entree,Salaire,age,SFC,IFC,PBO) %>% 
        mutate_at(c(4,6:8),scales::number,accuracy = 1) %>%
        datatable(rownames = F,options = list(columnDefs =
                                                list(list(className = 'dt-right',
                                                          targets = "_all"))))
    })
    
    output$fluxfuture = renderHighchart({
      
      validate(
        need( !is.null(input$dateinventaire) ,
              "Veuillez Verifier vous hypothese de la fenetre 'PARAMETRAGE'")
      )
      
      aaaa = calcul_tete_tete() %>%
        group_by(dateret = lubridate::year(Naissance)) %>%
        mutate(dateret = dateret + input$ageretraite) %>%
        summarise(sum = round(sum(PBO)), n = n())
      
      
      highchart() %>%
        hc_yAxis_multiples(
          list(lineWidth = 3, title = list(text = "n")),
          list(opposite = TRUE, title = list(text = "MAD"))
        ) %>%
        hc_add_series(data = aaaa$n,type = "column" , name = "Depart annuel") %>%
        hc_add_series(data = aaaa$sum, type = "column", yAxis = 1, name = "Cout annuelle") %>%
        hc_xAxis(categories = aaaa$dateret)
      
    })
    
    output$DataAnalysesensibilite1IFC = renderDT({
      
      validate(
        need( !is.null(input$dateinventaire2) &  !is.null(input$dateinventaire3) ,
             "Veuillez Verifier vous hypothese de la fenetre 'PARAMETRAGE'")
      )

      tablee1 = calcul_tete_tete() %>%
        select(Matricule,Naissance,Entree,Salaire,age,SFC,IFC,PBO)
      tablee2 = Resultat(df = test()[[2]],
                        evo_sal = input$TauxsalaireFixeIFC2/100,
                        i_act = input$TauxactFixeIFC2/100,
                        charg_sociau = input$TauxchargsociauFixeIFC2/100,
                        date_inventaire = as.Date(input$dateinventaire2),
                        age_retraite = input$ageretraite2) %>%
        select(Matricule,Naissance,Entree,Salaire,age,SFC,IFC,PBO)
      tablee3 = Resultat(df = test()[[2]],
                        evo_sal = input$TauxsalaireFixeIFC3/100,
                        i_act = input$TauxactFixeIFC3/100,
                        charg_sociau = input$TauxchargsociauFixeIFC3/100,
                        date_inventaire = as.Date(input$dateinventaire3),
                        age_retraite = input$ageretraite3) %>%
        select(Matricule,Naissance,Entree,Salaire,age,SFC,IFC,PBO)

      tablee = data.frame(Scenario = c("scenario1","scenario2","scenario3"),
                          PBO = c(sum(tablee1$PBO,na.rm = T),
                                  sum(tablee2$PBO,na.rm = T),
                                  sum(tablee3$PBO,na.rm = T))) %>% 
        mutate_at(2,scales::number,accuracy = 1) 
      
      tablef1 = data.frame(scenario = "scenario1",
                           evo_sal = input$TauxsalaireFixeIFC/100,
                           i_act = input$TauxactFixeIFC/100,
                           charg_sociau = input$TauxchargsociauFixeIFC/100,
                           date_inventaire = as.Date(input$dateinventaire),
                           age_retraite = input$ageretraite)
      
      tablef2 = data.frame(scenario = "scenario2",
                           evo_sal = input$TauxsalaireFixeIFC2/100,
                           i_act = input$TauxactFixeIFC2/100,
                           charg_sociau = input$TauxchargsociauFixeIFC2/100,
                           date_inventaire = as.Date(input$dateinventaire2),
                           age_retraite = input$ageretraite2)
      
      tablef3 = data.frame(scenario = "scenario3",
                           evo_sal = input$TauxsalaireFixeIFC3/100,
                           i_act = input$TauxactFixeIFC3/100,
                           charg_sociau = input$TauxchargsociauFixeIFC3/100,
                           date_inventaire = as.Date(input$dateinventaire3),
                           age_retraite = input$ageretraite3)
      
      
      tablef = rbind(tablef1,tablef2,tablef3)
      
      cbind(tablef,PBO = tablee[,2]) %>% 
        `colnames<-`(c("Scenario","Evolution de Salaire","Taux actualisation",
                       "charge sociau","date d'inventaire","age retraite","PBO")) %>% 
        mutate_at(c(2:4),scales::percent,accuracy = .01) %>% 
        datatable(rownames = F,options = list(columnDefs =
                                                list(list(className = 'dt-right',
                                                          targets = "_all"))))
      
      
    })
    
    output$DataAnalysesensibilite2IFC = renderDT({})
    
    # observeEvent(input$Save,{
    #   a = list()
    #   a[[1]] = list(dateinventaire = input$dateinventaire,
    #                 tablemortalite = input$tablemortalite,
    #                 ageretraite = input$ageretraite,
    #                 CapitalChoix = input$CapitalChoixIFC,
    #                 tauxchargementsociauxchoix = input$tauxchargementsociauxchoixIFC,
    #                 turnoverchoix = input$turnoverchoixIFC,
    #                 tauxactualisationchoix = input$tauxactualisationchoixIFC,
    #                 tauxevolutionsalirechoix = input$tauxevolutionsalirechoixIFC,
    #                 capitalfixe = input$capitalfixeIFC,
    #                 foissalaire = input$foissalaireIFC,
    #                 TauxFixe = input$TauxactFixeIFC, 
    #                 TauxFixe = input$TauxsalaireFixeIFC,
    #                 TauxFixe = input$TauxchargsociauFixeIFC,
    #                 TauxFixe = input$TauxturnoverFixeIFC)
    #   
    #   a[[2]] = list(dateinventaire2 = input$dateinventaire2,
    #                 tablemortalite2 = input$tablemortalite2,
    #                 ageretraite2 = input$ageretraite2,
    #                 CapitalChoix2 = input$CapitalChoixIFC2,
    #                 tauxchargementsociauxchoix2 = input$tauxchargementsociauxchoixIFC2,
    #                 turnoverchoix2 = input$turnoverchoixIFC2,
    #                 tauxactualisationchoix2 = input$tauxactualisationchoixIFC2,
    #                 tauxevolutionsalirechoix2 = input$tauxevolutionsalirechoixIFC2,
    #                 capitalfixe = input$capitalfixeIFC2,
    #                 foissalaire = input$foissalaireIFC2,
    #                 TauxFixe = input$TauxactFixeIFC2, 
    #                 TauxFixe = input$TauxsalaireFixeIFC2,
    #                 TauxFixe = input$TauxchargsociauFixeIFC2,
    #                 TauxFixe = input$TauxturnoverFixeIFC2)
    #   
    #   a[[3]] = list(dateinventaire3 = input$dateinventaire3,
    #                 tablemortalite3 = input$tablemortalite3,
    #                 ageretraite3 = input$ageretraite3,
    #                 CapitalChoix3 = input$CapitalChoixIFC3,
    #                 tauxchargementsociauxchoix3 = input$tauxchargementsociauxchoixIFC3,
    #                 turnoverchoix3 = input$turnoverchoixIFC3,
    #                 tauxactualisationchoix3 = input$tauxactualisationchoixIFC3,
    #                 tauxevolutionsalirechoix3 = input$tauxevolutionsalirechoixIFC3,
    #                 capitalfixe = input$capitalfixeIFC3,
    #                 foissalaire = input$foissalaireIFC3,
    #                 TauxFixe = input$TauxactFixeIFC3, 
    #                 TauxFixe = input$TauxsalaireFixeIFC3,
    #                 TauxFixe = input$TauxchargsociauFixeIFC3,
    #                 TauxFixe = input$TauxturnoverFixeIFC3)
    #   
    #   a %>% 
    #     saveRDS("teeeeeeeeeeeeest.rds")
    #   
    # })
    # 
    # observeEvent(input$get,{
    #   b = readRDS("teeeeeeeeeeeeest.rds")
    #   updatePickerInput(session = session, inputId = "tablemortalite",
    #                     choices = c("A","V"))
    #   
    #   
    #   c(input$dateinventaire,
    #     input$tablemortalite,
    #     input$ageretraite,
    #     input$CapitalChoixIFC,
    #     input$tauxchargementsociauxchoixIFC,
    #     input$turnoverchoixIFC,
    #     input$tauxactualisationchoixIFC,
    #     input$tauxevolutionsalirechoixIFC,
    #     input$capitalfixeIFC,
    #     input$foissalaireIFC,
    #     input$TauxactFixeIFC, 
    #     input$TauxsalaireFixeIFC,
    #     input$TauxchargsociauFixeIFC,
    #     input$TauxturnoverFixeIFC) = b[[1]] %>% unlist()
    # })
    
    
    
    
    
    
    
    # base <- reactiveVal() # to store the uploaded data

    # observeEvent(input[["csv"]], { # read and store the uploaded data
    #   csv <- input[["csv"]][["datapath"]]
    #   data(read.csv(csv))
    # })
    # 
    # output[["uploaded"]] <- reactive({ # indicator data uploaded
    #   !is.null(data())
    # })
    # outputOptions(output, "uploaded", suspendWhenHidden = FALSE)
    # 
    # output[["uiX"]] <- renderUI({ # the widget for selecting a variable
    #   req(data())
    #   selectInput(
    #     "X", 
    #     "Select variable",
    #     choices = colnames(data())
    #   )
    # })
    # 
    # Xloggable <- reactiveVal(FALSE) # indicates whether log-transform is possible 
    # observeEvent(input[["X"]], {    
    #   loggable <- all(data()[[input[["X"]]]] > 0, na.rm = TRUE)
    #   Xloggable(loggable)
    # })
    # 
    # logX <- reactive({ # indicates whether to log-transform the selected variable
    #   Xloggable() && input[["log10"]]
    # })
    # 
    # observeEvent(list(input[["X"]], input[["log10"]]), { # prevents log-transform
    #   req(input[["X"]])                                  # if not possible
    #   if(input[["log10"]] && !Xloggable()){
    #     showNotification("The selected variable cannot be log-transformed.")
    #     updateCheckboxInput(session, "log10", value = FALSE)
    #   }
    # })
    # 
    # output[["plot"]] <- renderPlot({ # the plot
    #   req(input[["X"]])
    #   x <- data()[[input[["X"]]]]
    #   if(logX()){
    #     plot(log10(x), pch = 19)
    #   }else{
    #     plot(x, pch = 19)
    #   }
    # })
    # 
    
    
    NomProjet <- reactiveValues()
    listproject = reactiveValues()
    
    
    
    
    observeEvent(input$saveState,{
      
      
      state <- list(
        # data  = data(),
        # X     = input[["X"]],
        # log10 = input[["log10"]]
        data = data(),
        avantageIFC = "Indemnite Fin Carriere (IFC)" %in% AvantageToShow$data,
        avantageMALADIE = input$MALADIEcheck,
        avantageMedailledeTravail = input$MedailledeTravailcheck,
        avantageCaisseInternedeRetraite = input$CaisseInternedeRetraitecheck,
        dateinventaire = input$dateinventaire,
        tablemortalite = input$tablemortalite,
        ageretraite = input$ageretraite,
        CapitalChoix = input$CapitalChoixIFC,
        tauxchargementsociauxchoix = input$tauxchargementsociauxchoixIFC,
        turnoverchoix = input$turnoverchoixIFC,
        tauxactualisationchoix = input$tauxactualisationchoixIFC,
        tauxevolutionsalirechoix = input$tauxevolutionsalirechoixIFC,
        capitalfixe = input$capitalfixeIFC,
        foissalaire = input$foissalaireIFC,
        TauxactFixeIFC = input$TauxactFixeIFC,
        TauxsalaireFixeIFC = input$TauxsalaireFixeIFC,
        TauxchargsociauFixeIFC = input$TauxchargsociauFixeIFC,
        TauxturnoverFixeIFC = input$TauxturnoverFixeIFC)
      
      
      a = readRDS("RMA/ListeProjetcts.rds")
      
      selected = input$whatuserneed
      selected = paste0(selected,collapse = "<br/>")
      
      store_state = paste0("RMA/",NomProjet$projname,".rds")
      
      a %>% 
        rbind(list(NomProjet$projname,Sys.Date(),selected,store_state)) %>% 
        saveRDS("RMA/ListeProjetcts.rds")
      
      saveRDS(state, store_state)
      
      listproject$data = listprojectformat()
      
      sendSweetAlert(
        session = session,
        title = "Le sauvegard a ete fait avec succes",
        type = "success"
      )
      
    })
    # output"saveState"]] <- downloadHandler( # bookmarking
    #   
    #   content = function(file){
    #     
    #     
    #   }
    # )
    
    # NomProjet$projname = "a"
    
    aaaaa = reactiveValues()
    
    
    # 
    # observeEvent(input$LoadProjet, {
    #   # read the saved state
    #   if (is.null(input$C)) {
    #     sendSweetAlert(
    #       session = session,
    #       title = "Aucun projet n'a ete selectionne",
    #       type = "warning"
    #     )
    #   }else{
    #     
    #     observeEvent(input$RHorNo,{
    #       if (input$RHorNo) {
    #         
    #         output$mesageerror = renderUI({
    #           passwordInputAddon("password", label = "", placeholder = "Password", addon = icon("key"),width = "45%")
    #           # tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center;font-size: 30px; display: block;}")
    #         })
    #       }else{
    #         output$mesageerror = renderUI({
    #           p("")
    #         })
    #         }
    #     })
    #     
    #     observeEvent(input$AccesRH,{
    #       
    #       # anotherone = T
    #       
    #       # if (anotherone) {
    #         
    #         if (input$RHorNo) {
    #           if (input$password == "oui") {
    #             
    #             # anotherone = F
    #             # AvantageToShow$data = listproject$data$Avantage[input$C] %>% str_split("<br/>",simplify = F)
    #             NomProjet$projname = connected(session = session,
    #                                            projects = listproject$data,
    #                                            whichproject = input$C,
    #                                            nameheader = NomProjet)
    #             
    #             output$ProjectName = renderUI({
    #               isolate(p(paste0("Nom du projet :",isolate(NomProjet$projname))))
    #             })
    #             
    #             removeModal()
    #             
    #             flagRH$value = T
    #             
    #           }else{
    #             # sendSweetAlert(
    #             #   session = session,
    #             #   title = "Ce que vous avez entre ne correspond pas au Mot de passe du projet.",
    #             #   type = "error"
    #             # )
    #           }
    #         }
    #         else{
    #           
    #           NomProjet$projname = connected(session = session,
    #                                          projects = listproject$data,
    #                                          whichproject = input$C,
    #                                          nameheader = NomProjet)
    #           
    #           removeModal()
    #           
    #           # sendSweetAlert(
    #           #   session = session,
    #           #   title = paste0("Vous etes maintenant sur le projet intitule '", NomProjet$projname, "'", "(Version confidentielle)"),
    #           #   type = "warning"
    #           # )
    #           flagRH$value = F
    #           
    #           
    #           
    #           output$ProjectName = renderUI({
    #             isolate(p(paste0("Nom du projet :",isolate(NomProjet$projname))))
    #           })
    #           
    #           
    #         }
    #         
    #       # }
    #       
    #       # anotherone = F
    #       
    #       
    #       
    #     })
    #     
    #     
    #     
    #     # NomProjet$projname = project[1,1]
    #     
    #   }
    #   
    # })
    # 
    # 
    
    
    
    
    
    listprojectformat = reactive({
      m = readRDS("RMA/ListeProjetcts.rds")
      j = ncol(m)+1
      for (i in seq_len(nrow(m))) {
        m[i, j] = sprintf(
          if_else(i == 1,
                  '<input type="radio" name="%s" value="%s"/>',
                  '<input type="radio" name="%s" value="%s"/>'),
          "C", i
        )
      }
      colnames(m)[j] = '.'
      return(m)
    })
    
    m = readRDS("RMA/ListeProjetcts.rds")
    j = ncol(m)+1
    for (i in seq_len(nrow(m))) {
      m[i, j] = sprintf(
        if_else(i == 1,
                '<input type="radio" name="%s" value="%s"/>',
                '<input type="radio" name="%s" value="%s"/>'),
        "C", i
      )
    }
    colnames(m)[j] = '.'
    listproject$data = m
    
    
    # listproject$data = listprojectformat()
    
    output$foo = DT::renderDataTable(
      listproject$data[,c(5,1:3)][nrow(listproject$data):1,], 
      escape = FALSE, selection = 'none', server = FALSE,
      rownames = F,options = list(scrollY = "300px"))
    
    
    observeEvent(input$Addfinish,{
      # NomProjet$projname = input$nameprojectchoix
      
      projects = readRDS("RMA/ListeProjetcts.rds")
      cond1 = input$nameprojectchoix %in% projects$Libele
      cond2 = (input$PasswordFirst == input$PasswordSecond) & (input$PasswordSecond != "")
      cond2 = !cond2
      if (cond1 & cond2) {
        output$AddError = renderUI({ 
          fluidRow(p(code("Le Libele est deja utilise")),
                   p(code("Mot de passe Incorrect")))
        })
        
      }else if(cond2){
        output$AddError = renderUI({ p(code("Mot de passe Incorrect")) })
      }
      
      else if(cond1){
        output$AddError = renderUI({ p(code("Le Libele est deja utilise")) })
      }else{
        output$ProjectName = renderUI({isolate(p(strong(paste0("Nom du projet : ",input$nameprojectchoix))))})
        AvantageToShow$data = input$whatuserneed
        PassCurrentProjet <<- input$PasswordSecond
        removeModal()
        
        
        selected = input$whatuserneed
        selected = paste0(selected,collapse = "<br/>")
        
        store_state = paste0("RMA/",input$nameprojectchoix,".rds")
        
        readRDS(file = "RMA/ListeProjetcts.rds") %>% 
          rbind(list(input$nameprojectchoix,input$DateProjetVolu,selected,store_state)) %>% 
          saveRDS("RMA/ListeProjetcts.rds")
        
        
        listproject$data = listprojectformat()
        
        sendSweetAlert(
          session = session,
          title = paste0("Le projet '", input$nameprojectchoix ,"' a ete cree"),
          type = "success"
          )
        
        }
      
    })
      
      
    
    
    
    observeEvent(input$RHorNo,{
      if (input$RHorNo) {
        output$mesageerror = renderUI({
          passwordInputAddon("password", label = "", placeholder = "Password", addon = icon("key"),width = "45%")
          # tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center;font-size: 30px; display: block;}")
        })
      }else{
        output$mesageerror = renderUI({
          p("")
        })
      }
    })
    
    flagRH = reactiveValues()
    
    observeEvent(input$entreprojet,{
      
      if (is.null(input$C)) {
        sendSweetAlert(
          session = session,
          title = "Veuillez tout d'abord selectionner un projet.",
          type = "warning"
        )
      }else{
        
        if (input$RHorNo) {
          if (input$password == "oui") {

            projects = readRDS("RMA/ListeProjetcts.rds")
            project = projects[as.numeric(input$C),]
            state <- readRDS(project[1,4])
            AvantageToShow$data = project$Avantage[1] %>% str_split("<br/>") %>% unlist()
            output$ProjectName = renderUI({isolate(p(strong(paste0("Nom du projet : ",project[1,1]))))})
            Loadproject(session,state)
            
            flagRH$value = T
            showTab(inputId = "re", target = "CalculPartete")
            sendSweetAlert(
              session = session,
              title = paste0("Le projet intitule '", project[1,1], "'", " est ouvert."),
              type = "success"
            )
          }else{
            output$passtrufalse = renderUI({p(code("Mot de passe incorrect"))})
          }
        }else{
          flagRH$value = F
          hideTab(inputId = "re", target = "CalculPartete")
          
          projects = readRDS("RMA/ListeProjetcts.rds")
          project = projects[as.numeric(input$C),]
          state <- readRDS(project[1,4])
          AvantageToShow$data = project$Avantage[1] %>% str_split("<br/>") %>% unlist()
          output$ProjectName = renderUI({isolate(p(strong(paste0("Nom du projet : ",project[1,1]))))})
          Loadproject(session,state)
          
          sendSweetAlert(
            session = session,
            title = paste0("Le projet intitule '", project[1,1], "'", "(Version confidentielle) est ouvert"),
            type = "warning"
          )
        }
        }
      
    })
    
    
    # observeEvent(input$Pourdestest, {
    #   hideTab(inputId = "re", target = "CalculPartete")
    # })
    
    observeEvent(input$logout, {
      session$reload()
    })
    
    observeEvent(input$checktodelete, {
      if (is.null(input$C)) {
        sendSweetAlert(
          session = session,
          title = "Veuillez tout d'abord selectionner Le projet.",
          type = "warning"
        )
      }else{
        if (input$PasswordDelete == "oui") {
          projects = readRDS("RMA/ListeProjetcts.rds")
          projects[-as.numeric(input$C),] %>% 
            saveRDS(file = "RMA/ListeProjetcts.rds")
          
          listproject$data = listprojectformat()
        }else{
          sendSweetAlert(
            session = session,
            title = "Mot de passe incorrect",
            type = "error"
          )
        }
      }
      })
    
    observeEvent(input$Pourdestest,{
      output$uiAutre = renderUI({
        # tabsetPanel(
        #   tabPanel("tab 1", "contents"),
        #   tabPanel("tab 2", "contents"),
        #   tabPanel("tab 3", "contents"))
        # aaaa(dateinventaire = "dateinventaireAutre",
        #      tablemortalite = "tablemortaliteAutre",
        #      ageretraite = "ageretraiteAutre",
        #      CapitalChoix = "CapitalChoixAutre",
        #      Capitalui = "CapitaluiAutre", 
        #      tauxactualisationchoix = "tauxactualisationchoixAutre",
        #      tauxactualisationui = "tauxactualisationuiAutre")
        
        # aaaa(tabNamee = "subMenuParametrageAutre",
        #                            dateinventaire = "dateinventaireAutre",
        #                            tablemortalite = "tablemortaliteAutre",
        #                            ageretraite = "ageretraiteAutre",
        #                            avantage = "Autre",
        #                            CapitalChoix = "CapitalChoixAutre",
        #                            Capitalui = "CapitalChoixuiAutre",
        #                            tauxactualisationchoix = "tauxactualisationchoixAutre",
        #                            tauxactualisationui = "tauxactualisationuiAutre",
        #                            tauxevolutionsalirechoix = "tauxevolutionsalirechoixAutre",
        #                            tauxaevolutionsalireui = "tauxaevolutionsalireuiAutre",
        #                            tauxchargementsociauxchoix = "tauxchargementsociauxchoixAutre",
        #                            turnoverchoix = "turnoverchoixAutre",
        #                            tauxchargementsociauxui = "tauxchargementsociauxuiAutre",
        #                            tauxturnoverui = "tauxturnoveruiAutre")
        
        bbbbb(dateinventairAutre = "dateinventairAutre",
              tablemortalitAutre = "tablemortalitAutre",
              ageretraitAutre = "ageretraitAutre",
              CapitalChoiAutre = "CapitalChoixAutre",
              CapitaluAutre = "CapitalChoixuiAutre",
              tauxactualisationchoiAutre = "tauxactualisationchoixAutre",
              tauxactualisationuAutre = "tauxactualisationuiAutre",
              tauxevolutionsalirechoiAutre = "tauxevolutionsalirechoixAutre",
              tauxaevolutionsalireuAutre = "tauxaevolutionsalireuiAutre",
              tauxchargementsociauxchoiAutre = "tauxchargementsociauxchoixAutre",
              turnoverchoiAutre = "turnoverchoixAutre",
              tauxchargementsociauxuAutre = "tauxchargementsociauxuiAutre",
              tauxturnoveruAutre = "tauxturnoveruiAutre")
        
      })
    })
    
    menu_vals = reactiveValues(menu_list = NULL)
    counter <- reactiveValues(counter_value = 0L)
    
    output$dy_menu <- renderMenu({
      menu_list <- list(
        menuItem("Add Menu Items", tabName = "main", selected = TRUE),
        menu_vals$menu_list)
      sidebarMenu(.list = menu_list)
    })
    
    observeEvent(eventExpr = input$add_menu,
                 handlerExpr = {
                   counter$counter_value <- counter$counter_value + 1L
                   menu_vals$menu_list[[length(menu_vals$menu_list) + 1]] <- menuItem(paste("Menu", counter$counter_value),
                                                                                      tabName = paste0("Menu", counter$counter_value)) 
                   
                 })
    
    if (TRUE) {
      output$AdminorNot = renderUI({actionButton('add_menu', "Ajout d'avantage")})
    }else{
      output$AdminorNot = renderUI({p("")})
    }
    
    
}

