##########################################################################################

# projectPISA App
# ISSS608 G2
# App File

###########################################################################################


#==============================#
#~~~~~ Importing Packages~~~~~#
#==============================#

pacman::p_load("shiny", "fresh", "shinydashboard", "shinyWidgets", "shinythemes",
               "tidyverse",
               "DT",
               "ExPanDaR", "kableExtra","plotly", "scales")

#==============================#
#~~~~~ Data Manipulation ~~~~~#
#==============================#

stu <- read_csv("data/stu_SG_rcd.csv")
varlist <- read_csv("data/var.csv")


Math <- round(summary(stu$Math),1)
Reading <- round(summary(stu$Reading),1)
Science <- round(summary(stu$Science),1)

db_summarystats <- as.data.frame(rbind(Math, Reading, Science))

# Dashboard Ternary Plot Tooltips
stu_tt <- stu %>% 
  mutate(rank_Math = round(percent_rank(Math)*100, 0),
         rank_Reading = round(percent_rank(Reading)*100, 0),
         rank_Science = round(percent_rank(Science)*100, 0),
         tooltip = paste0("Math: ", round(Math,0), " | Percentile: ", rank_Math,
                          "\nReading: ", round(Reading,0), " | Percentile: ", rank_Reading,
                          "\nScience: ", round(Science),  " | Percentile: ", rank_Science))

#========================#
#~~~~~ Theme ~~~~~#
#========================#

# Create dashboard theme with bootstrap/fresh ----------------------------------------------------
mytheme <- create_theme(
  theme = "default",
  adminlte_color(
    light_blue = "#2A2A21",
    purple = "#B2ABB4",
    green = "#b1bdb5",
    yellow = "#d6ac5e",
    blue = "#4E7880",
    red = "#b3907A"
  ),
  adminlte_sidebar(
    width = "180px",
    dark_bg = "#f5f5eb",
    dark_hover_bg = "#DDAFA1",
    dark_color = "#f5f5eb",
    dark_hover_color = "#d6ac5e",
    dark_submenu_bg = "#DDAFA1",
    dark_submenu_color = "#f5f5eb",
    dark_submenu_hover_color = "#DDAFA1"
  ),
  adminlte_global(
    content_bg = "#f5f5eb",
    box_bg = "#FFF", 
    info_box_bg = "#b1bdb5"
  ),
  adminlte_vars("body-bg" = "#FFF"),
  output_file = NULL
)

# Title Logo ----------------------------------------------------
pplogo <- tags$a(
  href = "https://projectpisa.netlify.app/",
  tags$img(
    src="Logo.PNG",
    height = '95',
    width = '80'))

#========================#
###### Shiny UI ######
#========================#

# Dashboard Header ----------------------------------------------------

## Application title
header <- dashboardHeader(
  tags$li(class = "dropdown",
          tags$style(".main-header {max-height: 100px}"),
          tags$style(".main-header .logo {height: 100px; margin-top: 1em; margin-bottom: 1em}"),
          tags$style(".sidebar-toggle {height: 100px;margin-top: 1em; margin-bottom: 1em; padding-top:1em; padding-botton:em;}")
  ),
  title = div(pplogo,
              style = "position: relative; margin:-3px 0px 0px 5px; display:right-align;"),
  titleWidth = 100)

## Dashboard Sidebar ----------------------------------------------------
sidebar <- dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 120px}"),
  sidebarMenu(
    id = "tabs",
    menuItem("Home", tabName = "tab_home", icon = icon("house")),
    menuItem("Data Analysis", tabName = "tab_eda", icon = icon("magnifying-glass-chart") #startExpanded = TRUE,
             #menuSubItem("Scores", tabName = "tab_target"),
             #menuSubItem("Explanatory Variables", tabName = "tab_variable"),
             #menuSubItem("Country Comparisons", tabName = "tab_country_compare")
    ),
    
    menuItem("Cluster Analysis", tabName = "tab_cluster", icon = icon("users-viewfinder"),
             menuSubItem("Heatmap", tabName = "tab_heatmap"),
             menuSubItem("Parallel Plot", tabName = "tab_parallelplot")
             ),
    
    menuItem("Regression Models", tabName = "tab_mod", icon = icon("tree"),
             menuSubItem("Decision Tree", tabName = "tab_dt"),             
             menuSubItem("Random Forest", tabName = "tab_rf"),
             menuSubItem("Gradient Boosting", tabName = "tab_gb")
    )
  )
)



# Content Body  ----------------------------------------------------

body <- dashboardBody(
  ## Setting theme  ----------------------------------------------------
  use_theme(mytheme),
  
  ## CSS style  ----------------------------------------------------
  ## Header  ----------------------------------------------------
  tags$head(tags$style(HTML('

        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #f5f5eb;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #f5f5eb;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #f5f5eb;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #f5f5eb;
                              
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #f5f5eb ;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #f5f5eb;
                              color: #394434; #text colour of links
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #f5f5eb;
         }
        
        /* The toggle lines to collapse menu bar   */                 
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              color: #4E7880;
                              background-color: #394434;
         }
        .navbar-custom-menu {
                              position: absolute;
                              display: inline-block;
                              margin-top: 5px;
        }
        .navbar-custom-menu li {
                                margin-left: 15px;
                                }
        
         .myClass { 
              font-size: 200%;
              line-height: 50px;
              text-align: left;
              font-family: "Playfair Display", serif;
              font-weight: bold;
              padding: 0 10px;
              overflow: hidden;
              color: white;
            }
                              '))),
  
  ## Text Styles  ----------------------------------------------------
  tags$style("h2 { font-family: 'Noto Sans', sans-serif; font-weight: bold; }"),
  tags$style("h3 { font-family:'Noto Sans', sans-serif; font-weight: bold; }"),
  tags$style(".small-box.bg-aqua { color: #2A2D34 !important; }"),
  tags$style(".small-box.bg-yellow { color: #2A2D34 !important; }"),
  tags$style(".box-header h3.box-title{ color: #7C6D62; font-weight: bold }"),
  tags$style(".box { font-size: 90%}"),
  tags$style(".box.bg-aqua { color: #2A2D34 !important; }"),
  tags$style(".nav-tabs-custom .nav-tabs li.active { border-top-color: #E9D758 !important; }"),
  tags$style(".nav-tabs-custom .nav-tabs li { font-weight: bold !important; }"),
  
  ## Body Tabs  ----------------------------------------------------
  tabItems(
    ### Dashboard  ----------------------------------------------------
    tabItem(tabName = "tab_home",
            fluidRow(
              
              #### Dashboard First Column  ----------------------------------------------------
              
              column(width = 6,
                     div(style = "padding = 0em; margin-left: 2em; margin-top: 2em; height: 100% ",
                         fluidRow(
                           box(
                             title = "Welcome to Project PISA", #tags$p("", style = "color: #b3907A; font-weight: bold; font-size: 80%;"),
                            status = "primary",
                            #solidHeader = TRUE,
                            collapsible = FALSE,
                            width = 12,
                            tags$div("According to the latest OECD's Programme for International Student Assessment (PISA) 2022, which measures 15-year-olds' ability to use their reading, mathematics, and science knowledge and skills to meet real-life challenges, socioeconomic status accounted for 17% of the variation in mathematics performance in Singapore (compared to 15% on average across OECD countries).",
                                    "Clearly, Singapore's success does not translate to success for every student. Why then do some students outperform others? And is socioeconomic status the only factor for success?",
                                    tags$br(), tags$br(),
                                    "Our team believes that knowledge is power. While causality cannot and should not be easily drawn between the various forces of influence and academic performance, a more detailed and nuanced understanding of these factors would highlight potential areas to focus on when engaging parents and students as well as when developing education and socioeconomic policies for a more inclusive and equitable society.")
                            )
                           ),
                         
                         fluidRow(
                           box(
                             title = tags$p("Relationship between subject scores", style = "font-weight: bold;"),
                             plotlyOutput("db_ternplot_",
                                          height = "44vh",
                                          width = "100%"),
                             status = "primary",
                             width = 12
                           )
                         )
                         ),
                     

                     ),
            
            #### Dashboard Second Column  ----------------------------------------------------
            column(width = 6,
                   
                   #### Dashboard Survey Stats Value Boxes  ----------------------------------------------------
                   div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       fluidRow(
                          valueBoxOutput("home_studnum_", width = 6),
                          valueBoxOutput("home_schnum_", width = 6)
                          ),
                       
                       div(style = "padding = 0em; margin-left: 0em; margin-right: 0em",
                          fluidRow(
                            box(
                              title = "Summary Statistics",
                              status = "primary",
                              width = 12,
                              collapsible = FALSE,
                              DT::dataTableOutput("db_summarystats_")
                              )
                            )
                          ),
                       div(style = "padding = 0em; margin-left: 0em; margin-right: 0em; height: 100% ",
                           fluidRow(
                             box(title = "Variable List", #tags$p("", style = "color: #b3907A; font-weight: bold; font-size: 80%;"),
                                 status = "primary",
                                 #solidHeader = TRUE,
                                 collapsible = FALSE,
                                 width = 12,
                                 DT::dataTableOutput("db_varlist_"))
                             )
                           )
                       )
                   )
                )
            ),
    
    ### Dashboard  ----------------------------------------------------
    tabItem(tabName = "tab_dt",
            fluidRow(
              column(width = 3,
                     div(style = "padding = 0em; margin-right: -0.5em",
                         box(title = tags$p("Variable Selection", style = "color: #FFF; font-weight: bold; font-size: 80%;"),
                             status = "primary")
                         )
                     )
            ))
    )
  )


# User Interface  ----------------------------------------------------
ui <- dashboardPage(title="Project PISA", header, sidebar, body)

#========================#
#~~~~~ Shiny Server ~~~~~#
#========================#

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Add Title  ----------------------------------------------------
  observeEvent(input$tabs, {
    header <- switch(input$tabs,
                     tab_home = "Home",
                     tab_eda = "Exploratory & Confirmatory Data Analysis",
                     tab_cluster = "Cluster Analysis",
                     tab_heatmap = "Country Comparison",
                     tab_cluster = "Cluster Heatmap",
                     tab_parallelplot = "Parallel Plot",
                     tab_dt = "Decision Tree",
                     tab_rf = "Random Forest",
                     tab_gb = "Gradient Boosting"
    )
    
    # you can use any other dynamic content you like
    shinyjs::html("pageHeader", header)
  })  

  
  # Dashboard Server  ----------------------------------------------------
  output$home_studnum_ <- renderValueBox({
    valueBox(
      value = tags$p(nrow(stu), style = "font-size: 60%;"),
      subtitle = tags$p(paste0("Participanting Students"), style = "font-size: 80%;"), 
      icon = icon("children"),
      color = "yellow"
    )
  })
  
  output$home_schnum_ <- renderValueBox({
    valueBox(
      value = tags$p(n_distinct(stu$CNTSCHID), style = "font-size: 60%;"), 
      subtitle = tags$p("Participanting Schools", style = "font-size: 80%;"), 
      icon = icon("building-columns"),
      color = "yellow"
    )
  })
  
  output$db_summarystats_ <- DT::renderDataTable({
      datatable(db_summarystats,
                options = list(dom = 't'),
                class = "compact")
  })
  
  output$db_varlist_ <- DT::renderDataTable({
    datatable(varlist,
              class = "compact",
              options = list(hover = TRUE))
  })
  

  output$db_ternplot_ <- renderPlotly({
    plot_ly(stu_tt,
            type = "scatterternary",
            mode = 'markers',
            a = stu_tt$rank_Math,
            b = stu_tt$rank_Reading,
            c = stu_tt$rank_Science,
            text = stu_tt$tooltip,
            opacity = 0.6,
            marker = list(color = '#4E7880',
                          opacity = 0.6,
                          size = 6,
                          line = list('width' = 1, color = '#FFFFFF')
            )
    ) %>%
      layout(
        ternary = list(
          xsum = 100,
          aaxis = list(title = 'Math'),
          baxis = list(title ='Reading'),
          caxis = list(title ='Science')
        ),
        autosize = TRUE,
        automargin = FALSE,
        margin = list(l=30,r=30,b=50,t=50,pad=4)
        #width = 600, 
        #height = 380
      )
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
