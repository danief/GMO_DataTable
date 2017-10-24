# shinyUI()

Data<-read.csv("./data/clean_data.csv", header =T, sep =";", check.names =F)
Data<-Data[!apply(is.na(Data) | Data == "", 1, all),]
Cultivation<-Data
Nontarget<-Data
Soil<-Data
options(encoding="UTF-8")
Sys.setlocale("LC_ALL","English")
######### ui ##################
shinyUI(fluidPage(#theme="bootstrap.css",
  titlePanel(title=div(img(src="logo.jpg", align="left", height= 90, width=140), "GMO DataTable", align="center")),
  fluidRow(
    column(2, selectInput("Year", "Year:",
    c("All", unique(as.character(Cultivation$Year))))),
    
    column(2, selectInput("Country","Country:",
    c("All", unique(as.character(Cultivation$Country))))),
    
    column(2, selectInput("CRY","CRY:",
    c("All", unique(as.character(Cultivation$CRY))))),
    
    column(2, selectInput("Host","Host:",
    c("All", unique(as.character(Cultivation$Host))))),
    
    fluidRow(DT::dataTableOutput("table"))
  ),
  
  sidebarLayout(fluid=F, position="left",
  sidebarPanel(width=2,
  
  conditionalPanel('input.dataset === "Cultivation"',
  checkboxGroupInput("show_vars_cult", "Columns in Cultivation to show:",
  names(Cultivation), selected = names(Cultivation))),
  
  conditionalPanel('input.dataset === "Nontarget"',
  checkboxGroupInput("show_vars_non", "Columns in Cultivation to show:",
  names(Nontarget), selected = names(Nontarget))),
  
  conditionalPanel('input.dataset === "Soil"',
  checkboxGroupInput("show_vars_soil", "Columns in Cultivation to show:",
  names(Soil), selected = names(Soil)))),
                
  mainPanel( 
  tabsetPanel(
  id = 'dataset',
  tabPanel("Cultivation", DT::dataTableOutput("mytable1")),
  tabPanel("Nontarget", DT::dataTableOutput("mytable2")),
  tabPanel("Soil", DT::dataTableOutput("mytable3"))
                  ))
  ))
)
