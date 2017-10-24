library(shiny)
library(DT)
options(encoding="UTF-8")
Sys.setlocale("LC_ALL","English")
#Load Data
Data<-read.csv("./data/clean_data.csv", header =T, sep =";", check.names =F)
Data<-Data[!apply(is.na(Data) | Data == "", 1, all),]
Cultivation<-Data
Nontarget<-Data
Soil<-Data

shinyServer(function(input, output) {
# Generates the three tables
  #Cultivation Table
  output$mytable1 <- DT::renderDataTable({
    #First check if input is All or not, then subset based off input Year etc. 
    if(input$Year != "All" & input$Country != "All" & input$CRY != "All" & input$Host != "All")
      DT::datatable(Cultivation[Cultivation$Year == input$Year 
                                & Cultivation$Country == input$Country
                                & Cultivation$CRY == input$CRY
                                & Cultivation$Hose == input$Host
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All" & input$Country != "All" & input$CRY != "All")
      DT::datatable(Cultivation[Cultivation$Year == input$Year 
                                & Cultivation$Country == input$Country
                                & Cultivation$CRY == input$CRY
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))  
    else if (input$Host != "All" & input$Country != "All" & input$Year != "All")
      DT::datatable(Cultivation[Cultivation$Host == input$Host 
                                & Cultivation$Country == input$Country
                                & Cultivation$Year == input$Year
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Host != "All" & input$Country != "All" & input$CRY != "All")
      DT::datatable(Cultivation[Cultivation$Host == input$Host 
                                & Cultivation$Country == input$Country
                                & Cultivation$CRY == input$CRY
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Host != "All" & input$Year != "All" & input$CRY != "All")
      DT::datatable(Cultivation[Cultivation$Host == input$Host 
                                & Cultivation$Year == input$Year
                                & Cultivation$CRY == input$CRY
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))   
    else if (input$Year != "All" & input$Country != "All")
      DT::datatable(Cultivation[Cultivation$Year == input$Year 
                                & Cultivation$Country == input$Country
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All" & input$CRY != "All")
      DT::datatable(Cultivation[Cultivation$Year == input$Year 
                                & Cultivation$CRY == input$CRY
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All" & input$Host != "All")
      DT::datatable(Cultivation[Cultivation$Year == input$Year 
                                & Cultivation$Host == input$Host
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$CRY != "All" & input$Country != "All")
      DT::datatable(Cultivation[Cultivation$CRY == input$CRY 
                                & Cultivation$Country == input$Country
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Host != "All" & input$Country != "All")
      DT::datatable(Cultivation[Cultivation$Host == input$Host 
                                & Cultivation$Country == input$Country
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$CRY != "All" & input$Host != "All")
      DT::datatable(Cultivation[Cultivation$CRY == input$CRY 
                                & Cultivation$Host == input$Host
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All")
      DT::datatable(Cultivation[Cultivation$Year == input$Year 
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))   
    else if (input$Host != "All")
      DT::datatable(Cultivation[Cultivation$Host == input$Host 
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Country != "All")
      DT::datatable(Cultivation[Cultivation$Country == input$Country 
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))   
    else if (input$Year != "All")
      DT::datatable(Cultivation[Cultivation$CRY == input$CRY 
                                , input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else
      DT::datatable(Cultivation[, input$show_vars_cult, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))
  })
  
  output$mytable2 <- DT::renderDataTable({
    #First check if input is All or not, then subset based off input Year etc. 
    if(input$Year != "All" & input$Country != "All" & input$CRY != "All" & input$Host != "All")
      DT::datatable(Nontarget[Nontarget$Year == input$Year 
                                & Nontarget$Country == input$Country
                                & Nontarget$CRY == input$CRY
                                & Nontarget$Hose == input$Host
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All" & input$Country != "All" & input$CRY != "All")
      DT::datatable(Nontarget[Nontarget$Year == input$Year 
                                & Nontarget$Country == input$Country
                                & Nontarget$CRY == input$CRY
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))  
    else if (input$Host != "All" & input$Country != "All" & input$Year != "All")
      DT::datatable(Nontarget[Nontarget$Host == input$Host 
                                & Nontarget$Country == input$Country
                                & Nontarget$Year == input$Year
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Host != "All" & input$Country != "All" & input$CRY != "All")
      DT::datatable(Nontarget[Nontarget$Host == input$Host 
                                & Nontarget$Country == input$Country
                                & Nontarget$CRY == input$CRY
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Host != "All" & input$Year != "All" & input$CRY != "All")
      DT::datatable(Nontarget[Nontarget$Host == input$Host 
                                & Nontarget$Year == input$Year
                                & Nontarget$CRY == input$CRY
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))   
    else if (input$Year != "All" & input$Country != "All")
      DT::datatable(Nontarget[Nontarget$Year == input$Year 
                                & Nontarget$Country == input$Country
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All" & input$CRY != "All")
      DT::datatable(Nontarget[Nontarget$Year == input$Year 
                                & Nontarget$CRY == input$CRY
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All" & input$Host != "All")
      DT::datatable(Nontarget[Nontarget$Year == input$Year 
                                & Nontarget$Host == input$Host
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$CRY != "All" & input$Country != "All")
      DT::datatable(Nontarget[Nontarget$CRY == input$CRY 
                                & Nontarget$Country == input$Country
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Host != "All" & input$Country != "All")
      DT::datatable(Nontarget[Nontarget$Host == input$Host 
                                & Nontarget$Country == input$Country
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$CRY != "All" & input$Host != "All")
      DT::datatable(Nontarget[Nontarget$CRY == input$CRY 
                                & Nontarget$Host == input$Host
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All")
      DT::datatable(Nontarget[Nontarget$Year == input$Year 
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))   
    else if (input$Host != "All")
      DT::datatable(Nontarget[Nontarget$Host == input$Host 
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Country != "All")
      DT::datatable(Nontarget[Nontarget$Country == input$Country 
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))   
    else if (input$Year != "All")
      DT::datatable(Nontarget[Nontarget$CRY == input$CRY 
                                , input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else
      DT::datatable(Nontarget[, input$show_vars_non, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))
  })
  
  output$mytable3 <- DT::renderDataTable({
    #First check if input is All or not, then subset based off input Year etc. 
    if(input$Year != "All" & input$Country != "All" & input$CRY != "All" & input$Host != "All")
      DT::datatable(Soil[Soil$Year == input$Year 
                                & Soil$Country == input$Country
                                & Soil$CRY == input$CRY
                                & Soil$Hose == input$Host
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All" & input$Country != "All" & input$CRY != "All")
      DT::datatable(Soil[Soil$Year == input$Year 
                                & Soil$Country == input$Country
                                & Soil$CRY == input$CRY
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))  
    else if (input$Host != "All" & input$Country != "All" & input$Year != "All")
      DT::datatable(Soil[Soil$Host == input$Host 
                                & Soil$Country == input$Country
                                & Soil$Year == input$Year
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Host != "All" & input$Country != "All" & input$CRY != "All")
      DT::datatable(Soil[Soil$Host == input$Host 
                                & Soil$Country == input$Country
                                & Soil$CRY == input$CRY
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Host != "All" & input$Year != "All" & input$CRY != "All")
      DT::datatable(Soil[Soil$Host == input$Host 
                                & Soil$Year == input$Year
                                & Soil$CRY == input$CRY
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))   
    else if (input$Year != "All" & input$Country != "All")
      DT::datatable(Soil[Soil$Year == input$Year 
                                & Soil$Country == input$Country
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All" & input$CRY != "All")
      DT::datatable(Soil[Soil$Year == input$Year 
                                & Soil$CRY == input$CRY
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All" & input$Host != "All")
      DT::datatable(Soil[Soil$Year == input$Year 
                                & Soil$Host == input$Host
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$CRY != "All" & input$Country != "All")
      DT::datatable(Soil[Soil$CRY == input$CRY 
                                & Soil$Country == input$Country
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Host != "All" & input$Country != "All")
      DT::datatable(Soil[Soil$Host == input$Host 
                                & Soil$Country == input$Country
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$CRY != "All" & input$Host != "All")
      DT::datatable(Soil[Soil$CRY == input$CRY 
                                & Soil$Host == input$Host
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Year != "All")
      DT::datatable(Soil[Soil$Year == input$Year 
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))   
    else if (input$Host != "All")
      DT::datatable(Soil[Soil$Host == input$Host 
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else if (input$Country != "All")
      DT::datatable(Soil[Soil$Country == input$Country 
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))   
    else if (input$Year != "All")
      DT::datatable(Soil[Soil$CRY == input$CRY 
                                , input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))    
    else
      DT::datatable(Soil[, input$show_vars_soil, drop = FALSE], options = list(lengthMenu=list(c( -1, 25, 50), c('All', '25', '50')), pageLength = 5,columnDefs = list(list(className = 'dt-right', targets = "_all"))))
  })
  
})

