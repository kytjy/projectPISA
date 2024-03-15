##########################################################################################

# projectPISA App
# ISSS608 G2
# App File

###########################################################################################


#==============================#
#~~~~~ Importing Packages~~~~~#
#==============================#

pacman::p_load("shiny", "fresh", "shinydashboard", "shinydashboardPlus", "shinyWidgets", "shinythemes", "shinyjs",
               "tidyverse",
               "DT",
               "ExPanDaR", "kableExtra","plotly", "scales")

#==============================#
#~~~~~ Data Manipulation ~~~~~#
#==============================#

# Loading files
stu <- read_csv("data/stu_SG_rcd.csv")
varlist <- read_csv("data/var.csv")

# Dashboard - Summary Statistics
Math <- round(summary(stu$Math),1)
Reading <- round(summary(stu$Reading),1)
Science <- round(summary(stu$Science),1)

db_summarystats <- as.data.frame(rbind(Math, Reading, Science))

# Dashboard - Ternary Plot Tooltips
stu_tt <- stu %>% 
  mutate(rank_Math = round(percent_rank(Math)*100, 0),
         rank_Reading = round(percent_rank(Reading)*100, 0),
         rank_Science = round(percent_rank(Science)*100, 0),
         tooltip = paste0("Math: ", round(Math,0), " | Percentile: ", rank_Math,
                          "\nReading: ", round(Reading,0), " | Percentile: ", rank_Reading,
                          "\nScience: ", round(Science),  " | Percentile: ", rank_Science))

# Regression Model
stu_mb <- stu %>% 
  select(2:26)

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
    aqua = "#4E7880",
    red = "#b3907A"
  ),
  adminlte_sidebar(
    width = "200px",
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
          tags$style(".sidebar-toggle {height: 100px;margin-top: 1em; margin-bottom: 1em;}")
  ),
  title = div(pplogo,
              style = "position: relative; margin:-3px 0px 0px 5px; display:right-align;"),
  titleWidth = 80)

## Dashboard Sidebar ----------------------------------------------------
sidebar <- dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 150px; margin-left: 0px;}"),
  width = 80,
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
    
    menuItem("Regression", tabName = "tab_mod", icon = icon("tree"),
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
  tags$head(
    tags$style(HTML('
        
        h1 {
        
        }
        '))),
        
  tags$head(
    tags$style(HTML('                
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
                              color: #DDAFA1;
                              background-color: #f5f5eb;
         }
        .navbar-custom-menu {
                              position: absolute;
                              display: inline-block;
                              margin-top: 5px;
        }
        .navbar-custom-menu li {
                                margin-top: 5px;
                                margin-left: 10px;
        }
        
        .sidebar-toggle {
        }
        
         .myClass { 
              font-size: 200%;
              line-height: 50px;
              text-align: left;
              font-family: "Playfair Display", serif;
              font-weight: bold;
              padding: 0px;
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
            #fluidRow(
              
              #### Dashboard Left Column  ----------------------------------------------------
              
              column(width = 5,
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         fluidRow(
                           box(
                             title = "Welcome to Project PISA", #tags$p("", style = "color: #b3907A; font-weight: bold; font-size: 80%;"),
                            status = "primary",
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
                                          height = "40vh",
                                          width = "100%"),
                             status = "primary",
                             width = 12
                           )
                         )
                         ),
                     

                     ),
            
            #### Dashboard Right Column  ----------------------------------------------------
            column(width = 6,
                   
                   #### Dashboard Survey Stats Value Boxes  ----------------------------------------------------
                   div(style = "padding = 0em; margin-left: 1em; margin-top: 3em; height: 100% ",
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
                #)
            ),
    
    ### Decision Tree  ----------------------------------------------------
    tabItem(tabName = "tab_dt",
            fluidRow(
              column(width = 2,
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p("Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = FALSE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em",
                                 awesomeRadio(
                                   inputId = "mb_target_",
                                   label = "Response Variable",
                                   choices = c("Math", "Reading", "Science"),
                                   selected = "Math",
                                   status = "primary"
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em",
                                 pickerInput(
                                   inputId = "mb_vars_",
                                   label = "Explanatory Variables",
                                   choices = list(
                                     `School Environment` = list("School Type" = "SchoolType",
                                                                 "Loneliness" = "Loneliness",
                                                                 "Classroom Safety" = "ClassroomSafety",
                                                                 "Teacher Support" = "TeacherSupport"),
                                     `Personal` = list("Gender" = "Gender",
                                                       "Math Homework Time" = "Homework_Math",
                                                       "Reading Homework Time" = "Homework_Reading",
                                                       "Science Homework Time" = "Homework_Science",
                                                       "Preference for Math" = "Preference_Math",
                                                       "Preference for Reading" = "Preference_Reading",
                                                       "Preference for Science" = "Preference_Science",
                                                       "Exercise" = "Exercise"),
                                     `Socioeconomic` = list("Parents' Education" = "ParentsEducation",
                                                            "Immigration" = "Immigration",
                                                            "Home Language" = "HomeLanguage",
                                                            "Sibling" = "Sibling",
                                                            "Aircon" = "Aircon",
                                                            "Helper" = "Helper",
                                                            "Vehicle" = "Vehicle",
                                                            "Books" = "Books",
                                                            "Own Room" = "OwnRoom",
                                                            "Family Commitment" = "FamilyCommitment")),
                                   selected = colnames(stu_mb[1:24]),
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE),
                                   choicesOpt = NULL,
                                   inline = FALSE,
                                   width = NULL
                                 ))
                             )
                         ),
                     ),
              column(width = 2,
                     div(
                       style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       box(title = tags$p(span(icon("tree"), "Decision Tree"), style = "font-weight: bold;"),
                           closable = FALSE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,
                           div(style = "padding = 0em; margin-top: -0.5em",
                               chooseSliderSkin("Flat"),
                               sliderInput(inputId = "dt_partition_",
                                           label = "Train-Test Partition:",
                                           min = 0.05,
                                           max = 0.95,
                                           value = c(0.8))),
                           div(style = "padding = 0em; margin-top: -0.5em",
                               tags$p("Select proportion of data used to train model", style = "font-style: italic;")),
                           div(style = "padding = 0em; margin-top: 0.5em",
                               numericInput(inputId = "dt_tunelength_",
                                            label = "Initial Tune Length:",
                                            value = 4,
                                            min = 1,
                                            max = 10,
                                            step = 1
                                            )),
                           div(style = "padding = 0em; margin-top: 0.5em",
                               selectInput(inputId = "dt_resamplingmethod_",
                                           label = "Resampling Method:",
                                           choices = c("Bootstrap" = "dt_bootstrap",
                                                       "Cross Validation" = "dt_cvkfold",
                                                       "Repeated Cross-Validation" = "dt_repeatkfold"),
                                           selected = "dt_bootstrap"
                                           )),
                           hidden(div(style = "padding = 0em; margin-top: 0.5em",
                                      numericInput(inputId = "dt_cvkfold_number",
                                                   label = "K-fold:",
                                                   min = 3,
                                                   max = 50,
                                                   step = 1,
                                                   value = 10))),
                           hidden(div(style = "padding = 0em; margin-top: 0.5em",
                                      numericInput(inputId = "dt_repeatkfold_number",
                                                   label = "K-fold:",
                                                   min = 3,
                                                   max = 50,
                                                   step = 1,
                                                   value = 10))),  
                           hidden(div(style = "padding = 0em; margin-top: 0.5em",
                                      numericInput(inputId = "dt_repeatkfold_repeat", 
                                                   label = "Number of Repeats:",
                                                   min = 3,
                                                   max = 10,
                                                   step = 1,
                                                   value = 3))), 
                           div(style = "padding = 0em; margin-top: 2em",
                               actionButton(inputId = "dt_action_",
                                            label ="Run Analysis",
                                            icon = icon("wrench")),
                               align = "center"
                           ),
                           # Box Sidebar for Model Tuning Parameters
                           sidebar = boxSidebar(
                             id = "dt_tuneparameters",
                             label = "Tuning Parameters",
                             width = 100,
                             numericInput(inputId = "dt_cp", 
                                          label = "Complexity Parameter:",
                                          min = 0.01,
                                          max = 1,
                                          step = 0.001,
                                          value = 0.02),
                             actionButton(inputId = "dt_tunemodel",
                                          label = "Tune Model",
                                          icon = icon("scissors"))
                             )
                       )
                     )

                     ),
              column(width = 8,
                     div(
                       style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       tabBox(title = tags$p("Results", style = "font-weight: bold;"),
                              id = "dt_resultstb",
                              width= 12,
                              side = "right",
                              selected = "Fit Assessment",
                              tabPanel("Best Tune"),
                              tabPanel("Decision Tree"),
                              tabPanel("Fit Assessment")) 
                       )
                     )
              )
                     
                     
    )
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
