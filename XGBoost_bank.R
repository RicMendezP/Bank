#############################################################
# XG Boost - DEMO for web site                              #
#############################################################

library(shiny)
library(shinydashboard)
# library("SHAPforxgboost")
# library(fastDummies)
# library(caret)
# library(e1071)
# library(xgboost)
# library(doParallel) 
# library(caTools)
library(dplyr)

#######################################################
# DATA SOURCES                                        #
#######################################################


Data <- read.csv("DataSample.csv")



######################################################
#  FUNCTIONS                                         #
######################################################






#######################################################
# USER INTERFACE                                      #
#######################################################

ui <- dashboardPage(
      dashboardHeader(title = h3("BANK LOAN"),
                       titleWidth = 300,
                       disable = F),
       dashboardSidebar(
         img(src = "logo-vertical-2022_388x300.jpg",height = 388, width = 300, align = "center"),
         hr(),
         h5(HTML("DATA UPLOAD AND CLEANING"), style="text-align:center"),
         hr(),
         disable = F,
         width = 300,
         collapsed = F,
         actionButton("case_story"," 1.Read: Case Story"),
         actionButton("Data"," 2.Read: data source"),
         actionButton(inputId = "upload",
                      label = " 3.Data upload ",
                      icon =  icon("refresh")),
         hr(),
         h5(HTML("THE XGBOOST ALGORITHM"), style="text-align:center"),
         hr(),
         actionButton("xgboost"," 4.Read: XGBoost - R code detail"),
         sliderInput(
           inputId = "pagepng",
           label= "Select page number",
           min = 1,
           max = 29,
           value = 1),
         actionButton(inputId = "upload_2",
                      label = " 5.Document upload ",
                      icon =  icon("refresh")),
         hr(),
         h5(HTML("SHAP VALUE ANALYSIS"), style="text-align:center"),
         hr(),
         actionButton("charts"," 6. Chart drivers"),
         selectInput("drivers","7. Select summary chart",
                     choices = list(
                       "Summary Plot" = "summary",
                       "Group main features" = "group",
                       "Observation range 500 - 650" = "feature1",
                       "Observation range 1500 - 1650" = "feature2",
                       "Observation range 3500 - 3650" = "feature3",
                       "Observation range 5500 - 5650" = "feature4",
                       "Observation range 8500 - 8650" = "feature5"),
                       selected = "Summary Plot"),
         actionButton("plot1","Plot selected chart", icon("option-vertical",lib ="glyphicon")),
         selectInput("mdrivers","8. Select main drivers",
                     choices = list(
                       "Duration" = "duration",
                       "Unknown contact" = "contact",
                       "Month:May" = "May",
                       "Housing" = "housing",
                       "Past days" = "day",
                       "Month:July" = "July",
                       "Balance" = "balance",
                       "Outcome" ="outcome",
                       "Age" = "age",
                       "Month:August" = "august"),
                     selected = "duration"),
         actionButton("plot2","Plot selected chart", icon("option-vertical",lib ="glyphicon")),
         selectInput("modrivers","9. Select other months graph",
                     choices = list(
                       "January" = "jan",
                       "February" = "feb",
                       "March" = "mar",
                       "June" = "jun",
                       "September" = "sep",
                       "November" = "nov",
                       "December" = "dec"),
                     selected = "jan"),
         actionButton("plot3","Plot selected chart", icon("option-vertical",lib ="glyphicon")),
         selectInput("jdrivers","10. Select job and activity graph",
                     choices = list(
                       "Unemployed" = "unemployed",
                       "Blue collar" = "bluecollar",
                       "Entrepreneur" = "entrepreneur",
                       "Housemaid" = "housemaid",
                       "Management" = "management",
                       "Self employed" = "selfemployed",
                       "Technician" = "technician",
                       "Unknown" = "unknown",
                       "Student" = "student",
                       "Retired" = "retired",
                       "Services" = "services"),
                     selected = "unemployed"),
         actionButton("plot4","Plot selected chart", icon("option-vertical",lib ="glyphicon")),
         hr(),
         tags$div(class="header", checked=NA,
                  tags$p("¿DO YOU WANT TO TELL US ABOUT YOUR BUSINESS CASE?", align = "center"),
                  tags$a(href="https://www.gssg.com.co" , h5("CONTACT US HERE", align = "center", target = "_blank"))
         ),
         hr(),
         tags$div(class="header", checked=NA,
                  tags$a(href="https://inisghtdiscovery.shinyapps.io/web_page_dashboards/", h5("GO TO MAIN PAGE", align = "center", target = "_blank"))
         ),
         hr(),
         actionButton("refresh","Refresh", icon("refresh")),
         hr()
        
       ),
      
      dashboardBody(
                    box(
                      title = "Bank marketing data",
                      status = "info",
                      width = 12,
                      height = 600,
                      solidHeader = T,
                      DT::dataTableOutput("Data")
                        ),
                    box(
                      title = "R code and documentation",
                      status = "info",
                      width = 12,
                      height = 750,
                      solidHeader = T,
                      uiOutput("PNG_XGBoost")
                    ),
                    box(
                      title = "Summary charts and plots",
                      status = "info",
                      width = 12,
                      height = 770,
                      solidHeader = T,
                      uiOutput("summary")
                    ),
                    
                
      )
     
 )



#######################################################
# server                                              #
#######################################################

server <- function( input, output, session){
  
  observeEvent(input$case_story,{
    
    showModal(modalDialog(
      
      title = "INTRODUCTION BANK LOAN STORY CASE - BANKING INDUSTRY ",
      HTML("
      
      This is a bank loan case with a binary (yes / no) answer option to accept or reject the loan <br>
      in a tele marketing campaign. The detail description of the campaign and its data source is <br>
      as follows:<br><br>
      
      Relevant Information:<br><br>

      The data is related with direct marketing campaigns of a Portuguese banking institution.<br>
      The marketing campaigns were based on phone calls. Often, more than one contact to the <br>
      same client was required, in order to access if the product (bank term deposit) would be (or not)<br>
      subscribed.<br><BR>
      key Question<br><br>
      The positive answers rate are 12% in the real scenario; ¿ Which variable combination will reduce <br>
      the negative answers (reduce the negative predictive value)? and which is the importance and direction (positive or negative ) influence on the decision ?<br><br>
      <b> THE XGBOOST ALGORITHM WILL BE USED TO ANSWER THIS QUESTION </br> 
           "),
      #footer = "Esto es un pie de página", 
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", #tamaño de la ventana
      easyClose = T,
      fade = T #Efecto
      
    ))
    
  })
  
  
  observeEvent(input$Data,{
    
    showModal(modalDialog(
      
      title = "DATA SOURCE ",
      HTML("  This dataset is public available for research. The details are described in [Moro et al., 2011].<br> 
      Please include this citation if you plan to use this database: <br><br>

     [Moro et al., 2011] S. Moro, R. Laureano and P. Cortez. Using Data Mining for Bank Direct Marketing:<br> 
     An Application of the CRISP-DM Methodology.<br> 
     In P. Novais et al. (Eds.), Proceedings of the European Simulation and Modelling Conference - <br>
     ESM'2011, pp. 117-121, Guimarães, Portugal, October, 2011. EUROSIS.<br><br>

  Available at: [pdf] http://hdl.handle.net/1822/14838 <br>
                [bib] http://www3.dsi.uminho.pt/pcortez/bib/2011-esm-1.txt <br><br>

1. Title: Bank Marketing <br>

2. Sources<br>
   Created by: Paulo Cortez (Univ. Minho) and Sérgio Moro (ISCTE-IUL) @ 2012<br>
    
3. Past Usage:<br>

  The full dataset was described and analyzed in:<br> 

  S. Moro, R. Laureano and P. Cortez. Using Data Mining for Bank Direct Marketing: An Application of <br>
  the CRISP-DM Methodology.<br> 
  In P. Novais et al. (Eds.), Proceedings of the European Simulation and Modelling Conference - ESM'2011,<br> 
  pp. 117-121, Guimarães, Portugal, October, 2011. EUROSIS.<br><br>

   There are two datasets:<br><br>
      1) bank-full.csv with all examples, ordered by date (from May 2008 to November 2010).<br>
      2) DataSample.csv with 10% of the examples (4521), randomly selected from bank-full.csv.<br><br>
      
      <b> THE ORIGINAL DATA SET HAS 45,211 OBSERVATIONS AND 17 VARIABLES; 10% WILL BE UPLOADED TO </b>
      <b> UNDERSTAND THE DATA STRUCTURE. </b>"),
      #footer = "Esto es un pie de página",
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", #tamaño de la ventana
      easyClose = T,
      fade = T #Efecto
      
    ))
    
  })
  
 
  
  observeEvent(input$upload,{
    updateActionButton(                                  
      session = session,
      inputId = "upload",
      label = "3. Loaded data",
      icon = icon("ok",lib ="glyphicon")
    )
    output$Data <- 
      output$results <- DT::renderDataTable(
        Data,
        options = list(scrollX = TRUE)
      )
  })
  
  observeEvent(input$xgboost,{
    
    showModal(modalDialog(
      
      title = " XG BOOST ALGORITHM R CODE DESCRIPTION  ",
      HTML("
      
      The running time of the algorithm would be long,therefore the following document will<br>
      review the steps to code with R the XGBoost algorithm to solve the key question. <br><br>
      <b> THE XGBOOST ALGORITHM IS A SUPERVISED ALGORITHM THAT WORK WITH THE DECISION TREE ENSAMBLES </b>
      <b> MODEL --- LEARN MORE HERE: https://xgboost.readthedocs.io/en/latest/tutorials/model.html --- </B>
      "),
      #footer = "Esto es un pie de página", 
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", #tamaño de la ventana
      easyClose = T,
      fade = T #Efecto
      
    ))
    
  })
  
  
  observeEvent(input$upload_2,{
    updateActionButton(                                  
      session = session,
      inputId = "upload_2",
      label = "5. Select page and load document",
      icon = icon("ok",lib ="glyphicon")
    )
   
    p <-as.numeric(input$pagepng)
    
    if (p==1) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="1.png")})} 
    if (p==2) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="2.png")})}
    if (p==3) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="3.png")})}
    if (p==4) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="4.png")})} 
    if (p==5) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="5.png")})}
    if (p==6) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="6.png")})}
    if (p==7) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="7.png")})} 
    if (p==8) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="8.png")})}
    if (p==9) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="9.png")})}
    if (p==10) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="10.png")})} 
    if (p==11) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="11.png")})}
    if (p==12) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="12.png")})}
    if (p==13) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="13.png")})} 
    if (p==14) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="14.png")})}
    if (p==15) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="15.png")})}
    if (p==16) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="16.png")})} 
    if (p==17) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="17.png")})}
    if (p==18) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="18.png")})}
    if (p==19) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="19.png")})} 
    if (p==20) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="20.png")})}
    if (p==21) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="21.png")})}
    if (p==22) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="22.png")})} 
    if (p==23) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="23.png")})}
    if (p==24) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="24.png")})}
    if (p==25) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="25.png")})}
    if (p==26) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="26.png")})}
    if (p==27) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="27.png")})} 
    if (p==28) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="28.png")})}
    if (p==29) {output$PNG_XGBoost <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="29.png")})}
    
  })
  
  
  observeEvent(input$charts,{
    
    showModal(modalDialog(
      
      title = "CHARTS PLOTS AND CONCLUSIONS",
      HTML("Key insights will be analyzed using the SHAP values in the following steps: <br><br>
      - Summary charts and detalied ranges of main features and its related observations.<br> 
      The summary graph will easily describe the <b>importance of each feature and its direction</b>; 
      if its effect is <b>positive or negative</b> to drive the customer decision get a loan. <br><br>
      This graph will help to answer the stated key question at the introduction section .<br><br>
      <b>Conclusions will be detailed on page 29 of step 4</b><br><br>
      Other charts are:<br><br>
      - Main variables charts to check its detailed positive or negative impact <br><br> 
      - Other month variables  to check its detailed positive or negative impact<br><br>
      - Main job and activities variables to check its detailed positive or negative impact<br><br>
      ----- learn about shap values: https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/"),
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", #tamaño de la ventana
      easyClose = T,
      fade = T #Efecto
      
    ))
    
  })
  
  
  
  observeEvent(input$plot1,{
    
    updateSelectInput(
      session = session,
      inputId = "drivers",
      label = "7. Selected summary analysis",
      choices = list(
        "Summary Plot" = "summary",
        "Group main features" = "group",
        "Observation range 500 - 650" = "feature1",
        "Observation range 1500 - 1650" = "feature2",
        "Observation range 3500 - 3650" = "feature3",
        "Observation range 5500 - 5650" = "feature4",
        "Observation range 8500 - 8650" = "feature5"),
      selected = "summary")
   
    updateActionButton(                                   
      session = session,
      inputId = "plot1",
      label = " Select and Plot",
      icon = icon("option-vertical",lib ="glyphicon"))
   
    driver <- as.character(input$drivers)
    
    if (driver=="summary")  {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_summary.png")})}
    if (driver=="group")    {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_by_group.png")})}
    if (driver=="feature1") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_feature_500.png")})} 
    if (driver=="feature2") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_feature_1500.png")})}
    if (driver=="feature3") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_feature_3500.png")})}
    if (driver=="feature4") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_feature_5500.png")})} 
    if (driver=="feature5") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_feature_8500.png")})}
   
  })
  
  
  observeEvent(input$plot2,{
    
    updateSelectInput(
      session = session,
      inputId = "mdrivers",
      label = "8.Selected main drivers analysis",
      choices = list(
        "Duration" = "duration",
        "Unknown contact" = "contact",
        "Month:May" = "may",
        "Housing" = "housing",
        "Past days" = "day",
        "Month:July" = "july",
        "Balance" = "balance",
        "Outcome" ="outcome",
        "Age" = "age",
        "Month:August" = "august"),
      selected = "duration")
    
    
    updateActionButton(                                   
      session = session,
      inputId = "plot2",
      label = " Select and Plot",
      icon = icon("option-vertical",lib ="glyphicon"))
    
    mdriver <- as.character(input$mdrivers)
    
    if (mdriver=="duration") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_duration.png")})}
    if (mdriver=="contact")  {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_contact_unknown.png")})}
    if (mdriver=="may") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_may.png")})}
    if (mdriver=="housing") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_housing_yes.png")})} 
    if (mdriver=="Paid days") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_value_pdays.png")})}
    if (mdriver=="july") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_jul.png")})} 
    if (mdriver=="balance") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_balance.png")})}
    if (mdriver=="outcome") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_poutcome_success.png")})}
    if (mdriver=="age") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_age.png")})}
    if (mdriver=="august") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_value_august.png")})}
    
    
    
  })
  
  
  observeEvent(input$plot3,{
    
    updateSelectInput(
      session = session,
      inputId = "modrivers",
      label = "9. Selected month analysis",
      choices = list(
        "January" = "jan",
        "February" = "feb",
        "March" = "mar",
        "June" = "jun",
        "September" = "sep",
        "November" = "nov",
        "December" = "dec"),
      selected = "jan")
       
    
    updateActionButton(                                   
      session = session,
      inputId = "plot3",
      label = " Select and Plot",
      icon = icon("option-vertical",lib ="glyphicon"))
    
    modriver <- as.character(input$modrivers)
    
    if (modriver=="jan") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_jan.png")})}
    if (modriver=="feb")  {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shape_plot_feb.png")})}
    if (modriver=="mar") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_march.png")})}
    if (modriver=="jun") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_june.png")})} 
    if (modriver=="sep") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_sept.png")})}
    if (modriver=="nov") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shapplot_november.png")})} 
    if (modriver=="dec") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_dec.png")})}
   
    
    
  })
  
  
  observeEvent(input$plot4,{
    
    updateSelectInput(
      session = session,
      inputId = "jdrivers",
      label = "10. Selected Job and Activity analysis",
      choices = list(
        "Unemployed" = "unemployed",
        "Blue collar" = "bluecollar",
        "Entrepreneur" = "entrepreneur",
        "Housemaid" = "housemaid",
        "Management" = "management",
        "Self employed" = "selfemployed",
        "Technician" = "technician",
        "Unknown" = "unknown",
        "Student" = "student",
        "Retired" = "retired",
        "Services" = "services"),
      selected = "unemployed")
    
    
    updateActionButton(                                   
      session = session,
      inputId = "plot4",
      label = " Select and Plot",
      icon = icon("option-vertical",lib ="glyphicon"))
    
    jdriver <- as.character(input$jdrivers)
    
    if (jdriver=="unemployed") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_unemployed.png")})}
    if (jdriver=="bluecollar")  {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_blue_collar.png")})}
    if (jdriver=="entrepreneur") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_entrepreneur.png")})}
    if (jdriver=="housemaid") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_housemaid.png")})} 
    if (jdriver=="management") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_management.png")})}
    if (jdriver=="selfemployed") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_self_employed.png")})}
    if (jdriver=="technician") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_technician.png")})} 
    if (jdriver=="unknown") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_job_unknown.png")})}
    if (jdriver=="student") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_student.png")})}
    if (jdriver=="services") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_job_services.png")})}
    if (jdriver=="retired") {output$summary <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="shap_plot_retired.png")})}
    
    
    
  })
  
  
  
  
  
  
  observeEvent(input$refresh,{
    updateActionButton(                                  
      session = session,
      inputId = "refresh",
      label = "refresh",
      icon = icon("ok",lib ="glyphicon")
    )
    
    session$reload()
    
  })
  
  
  
  
  
    
}

#######################################################
# LAUNCH                                              #
#######################################################

shinyApp( ui = ui , server = server)  

