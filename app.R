##########################################################################################

# projectPISA App
# ISSS608 G2
# App File

###########################################################################################


#==============================#
#~~~~~ Importing Packages~~~~~#
#==============================#

pacman::p_load("shiny", "fresh", "shinydashboard", "shinydashboardPlus", "shinyWidgets", "shinythemes", "shinyjs",
               "tidyverse", "DT", "kableExtra", "plotly", "scales", "gt",
               "ranger", "vip", "rpart.plot", "caret", "tidymodels", "gbm",
               "waiter"
               )

#==============================#
#~~~~~ Data Manipulation ~~~~~#
#==============================#

# Loading files
#stu <- read_csv("data/stu_SG_rcd.csv")

stu <- read_rds("data/stu_SG_rcd.rds")
varlist <- read_csv("data/var.csv")

# Dashboard - Summary Statistics
Math <- round(summary(stu$Math),1)
Reading <- round(summary(stu$Reading),1)
Science <- round(summary(stu$Science),1)

db_summarystats <- as.data.frame(rbind(Math, Reading, Science))

# # Dashboard - Ternary Plot Tooltips
# stu_tt <- stu %>% 
#   mutate(rank_Math = round(percent_rank(Math)*100, 0),
#          rank_Reading = round(percent_rank(Reading)*100, 0),
#          rank_Science = round(percent_rank(Science)*100, 0),
#          tooltip = paste0("Math: ", round(Math,0), " | Percentile: ", rank_Math,
#                           "\nReading: ", round(Reading,0), " | Percentile: ", rank_Reading,
#                           "\nScience: ", round(Science),  " | Percentile: ", rank_Science))

# Regression Model
stu_mb <- stu %>% 
  select(2:26) %>% 
  na.omit()

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
    height = '85',
    width = '70'))

#========================#
###### Shiny UI ######
#========================#

# Dashboard Header ----------------------------------------------------

## Application title
header <- dashboardHeader(
  tags$li(class = "dropdown",
          tags$style(".main-header {max-height: 80px; font-size: 10px;}"),
          tags$style(".main-header .logo {height: 80px; margin-top: 1em; margin-bottom: 0em; font-size: 10px;}"),
          tags$style(".sidebar-toggle {height: 80px;margin-top: 1em; margin-bottom: 0em; font-size: 10px;}"),
          tags$style(".navbar {min-height:20px !important}")
  ),
  title = div(pplogo,
              style = "position: relative; margin:0px 0px 0px 0px; display:right-align;"),
  titleWidth = 80)

## Dashboard Sidebar ----------------------------------------------------
sidebar <- dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 120px; padding-right: 0px; margin-left: 0px; margin-right: 0px; font-size: 10px;}"),
  width = 80,
  minified = TRUE,
  sidebarMenu(
    id = "tabs",
    menuItem("Home", tabName = "tab_home", icon = icon("house")),
    menuItem("Data Analysis", tabName = "tab_eda", 
             icon = icon("magnifying-glass-chart")
             ),
    
    menuItem("Cluster Analysis", tabName = "tab_cluster", 
             icon = icon("users-viewfinder"),
             startExpanded = TRUE,
             menuSubItem("Heatmap", tabName = "tab_heatmap"),
             menuSubItem("Parallel Plot", tabName = "tab_parallelplot")
             ),
    
    menuItem("Regression", tabName = "tab_mod", 
             icon = icon("tree"),
             startExpanded = TRUE,
             menuSubItem("Decision Tree", tabName = "tab_dt"),             
             menuSubItem("Random Forest", tabName = "tab_rf"),
             menuSubItem("Gradient Boosting", tabName = "tab_gb")
    )
  )
)



# Content Body  ----------------------------------------------------

body <- dashboardBody(
  ## Setting theme  ----------------------------------------------------
  use_googlefont("Noto Sans"),
  
  use_theme(mytheme),
  
  useShinyjs(),
  
  #shinyjs::inlineCSS("body > div > header > nav > a {visibility: hidden}"),
  
  autoWaiter(),

  
  ## CSS style  ----------------------------------------------------
  tags$head(
    tags$style(HTML(' 
        
        .p  {font-size: 10px;} 

                    
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
                              font-size: 10px;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #f5f5eb;
                              font-size: 10px;.
                              
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #f5f5eb ;
                              font-size: 10px;
        }
        
        .sidebar-menu li a{
                              background-color: #f5f5eb ;
                              font-size: 10px;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #f5f5eb;
                              color: #394434; #text colour of links
                              font-size: 10px;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #f5f5eb;
                              font-size: 10px;
         }
        
        /* The toggle lines to collapse menu bar   */                 
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              color: #DDAFA1;
                              background-color: #f5f5eb;
                              font-size: 10px;
         }
        .navbar-custom-menu {
                              position: absolute;
                              display: inline-block;
                              margin-top: 5px;
                              font-size: 10px;
        }
        .navbar-custom-menu li {
                                margin-top: 5px;
                                margin-left: 10px;
                                font-size: 10px;
        }
        
        .sidebar-toggle { 
                          font-size: 10px;
        }
        
        # .selectize-input.focus {
        # border-color: #DDAFA1;
        # outline: 0;
        # -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.075), 0 0 8px rgba(102, 175, 233, 0.6);
        # box-shadow: inset 0 1px 1px rgba(0,0,0,.075), 0 0 8px rgba(221, 175, 161, 1);
        # }
        
        .box {font-size: 10px;}
        
        .dropdown-item:hover {
         color: #000000 ;
         background-color: #DDAFA1 !important;
        }
        
        .optgroup-header {
                font-size: 10px !important;
        }
        
        # .cbcontainer {
        # display: inline-block;
        # }
        # 
        # .checkbox {
        #        text-align: left;
        #        display: inline-block;
        # }
        # .checkbox input {
        #        float: left;
        #        position: relative !important;
        #        margin-right: 0px !important;
        #        padding-right: 0px !important;
        # }
        # 
        # .checkbox label {
        #         padding-left: 0px !important;
        #         padding-right: 0px !important;
        #      }
        
        input[type="number"] {
        font-size: 10px;}

        
         .myClass { 
              font-size: 10px;
              #line-height: 50px;
              #text-align: left;
              #font-family: "Playfair Display", serif;
              #font-weight: bold;
              #padding: 0px;
              #overflow: hidden;
              #color: white;
         }
        
        .shiny-input-container{padding:0px !important;}
                              '))),
  
  ## Text Styles  ----------------------------------------------------
  tags$style("h2 { font-family: 'Noto Sans', sans-serif; font-weight: bold; }"),
  tags$style("h3 { font-family:'Noto Sans', sans-serif; font-weight: bold; }"),
  tags$style(".small-box.bg-aqua { color: #2A2D34 !important; }"),
  tags$style(".small-box.bg-yellow { color: #2A2D34 !important; }"),
  tags$style(".box-header h3.box-title{ color: #7C6D62; font-weight: bold; font-size: 12px; }"),
  tags$style(".box {font-size: 10px;}"),
  tags$style(".tabBox {font-size: 10px}"),  
  tags$style(".tabBox-header h3.box-title{ color: #7C6D62; font-weight: bold; font-size: 12px; }"),
  tags$style(".box.bg-aqua { color: #2A2D34 !important; }"),
  tags$style(".nav-tabs-custom .nav-tabs li.active { border-top-color: #E9D758 !important;font-size: 10px; }"),
  tags$style(".nav-tabs-custom .nav-tabs li { font-weight: bold !important; font-size: 10px;}"),
  
  ## Body Tabs  ----------------------------------------------------
  tabItems(
    ### Dashboard  ----------------------------------------------------
    tabItem(tabName = "tab_home",
            #fluidRow(
              
              #### Dashboard Left Column  ----------------------------------------------------
              
              column(width = 5,
                     div(style = "padding = -2em; margin-left: 0em; margin-top: 3em; height: 100% ",
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

                         )
                         ),
                     div(style = "padding = 0em; margin-left: 0em; margin-right: 0em; height: 100% ",
                         fluidRow(
                           box(title = "Variable List", #tags$p("", style = "color: #b3907A; font-weight: bold; font-size: 80%;"),
                               status = "primary",
                               collapsible = FALSE,
                               width = 12,
                               DT::dataTableOutput("db_varlist_"))
                           )
                         )
                     ),
            
            #### Dashboard Right Column  ----------------------------------------------------
            column(width = 7,
                   
                   #### Dashboard Survey Stats Value Boxes  ----------------------------------------------------
                   div(style = "padding = 0em; margin-left: 1em; margin-top: 3em; height: 100%; ",
                       fluidRow(
                          valueBoxOutput("home_studnum_", width = 6),
                          valueBoxOutput("home_schnum_", width = 6)
                          ),
                       div(style = "padding = 0em; margin-left: 0em; margin-right: 0em",
                           fluidRow(
                           tabBox(
                           title = tags$p("Performance & Variable Distribution", style = "color: #7C6D62; font-weight: bold; font-size: 12px;"),
                           width = 12,
                           side = "left",
                           selected = "Subject",
                           tabPanel("Subject",
                                    div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                        pickerInput(
                                          inputId = "db_hist_subject",
                                          label = "Select Subject:",
                                          choices = c("Math", "Reading", "Science"), 
                                          selected = "Math",
                                          multiple = FALSE,
                                          options = list(style = "myClass"), 
                                          choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                          inline = TRUE,
                                          width = NULL
                                        ),
                                        align = "center"
                                    ),
                                    div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                        plotlyOutput("db_hist_scores_",
                                                     height = "40vh",
                                                     width = "90%"),
                                        tags$div(tags$br()),
                                        DT::dataTableOutput("db_summarystats_"),
                                        align = "center")
                                    ),
                           tabPanel("Frequency of Predictor Response",
                                    div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                        pickerInput(
                                          inputId = "db_bar_var",
                                          label = "Select Variable: ",
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
                                          selected = "SchoolType",
                                          multiple = FALSE,
                                          options = list(style = "myClass", `actions-box` = TRUE),
                                          choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                                          inline = TRUE,
                                          width = NULL
                                        ), 
                                        align = "center"
                                    ),
                                    div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                        plotlyOutput("db_bar",
                                                     height = "40vh",
                                                     width = "90%"),
                                        align = "center")
                           ),
                           tabPanel("Scores by Predictor",
                                    div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                        pickerInput(
                                          inputId = "db_boxviolin_target",
                                          label = "Select Subject:",
                                          choices = c("Math", "Reading", "Science"), 
                                          selected = "Math",
                                          multiple = FALSE,
                                          options = list(style = "myClass"), 
                                          choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                          inline = TRUE,
                                          width = NULL
                                        ),
                                        pickerInput(
                                          inputId = "db_boxviolin_var",
                                          label = "Select Variable: ",
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
                                          selected = "SchoolType",
                                          multiple = FALSE,
                                          options = list(style = "myClass", `actions-box` = TRUE),
                                          choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                                          inline = TRUE,
                                          width = NULL
                                        ), 
                                        align = "center"
                                    ),
                                    div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                        plotlyOutput("db_boxviolin",
                                                     height = "40vh",
                                                     width = "90%"),
                                        align = "center")
                                    )
                           )
                           )
                       )
                       )
                   )
                #)
            ),
    
    ### Data Analysis **ARIEL**   ----------------------------------------------------
    tabItem(tabName = "tab_eda",
            
            #### CDA Tab's Side Toggles ----------------------------------------------------
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p(span(icon("magnifying-glass"), "Data Analysis"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = FALSE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "Provide a short description of the module here, or maybe description of the statistical tests."
                             )
                         )
                     ),
                     
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p("Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = FALSE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 
                                 ## ARIEL YOUR SIDE BAR TOGGLES CAN PUT HERE
                                 pickerInput(
                                   inputId = "cda_target_", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Scores",
                                   choices = c("Math", "Reading", "Science"), 
                                   selected = "Math",
                                   multiple = FALSE,
                                   options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "cda_vars_", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Variables",
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
                                   multiple = FALSE,
                                   options = list(style = "myClass", `actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                                   inline = FALSE,
                                   width = NULL
                                 ))
                             )
                         )
                     )
                   ),
            column(width = 10,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p("Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = FALSE,
                             width = 12 # ARIEL ADD COMMA HERE 
                             ## ARIEL YOUR PLOTS CAN PUT HERE
                             ))
                   )
                   )
            
            ),
    ### Decision Tree  ----------------------------------------------------
    tabItem(tabName = "tab_dt",
            
            #### Decision Tree Toggle Column  ----------------------------------------------------
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p(span(icon("tree"), "Decision Tree"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = FALSE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "A Decision Tree is a machine learning algorithm that partitions the data into subsets. The partitioning process starts with a binary split and continues until no further splits can be made. Various branches of variable length are formed. The goal of a decision tree is to encapsulate the training data in the smallest possible tree, i.e. simplest possible explanation for the variation in scores."
                                 )
                             )
                         ),
                         
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p("Step 1: Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = FALSE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "dt_target_",
                                   label = "Scores",
                                   choices = c("Math", "Reading", "Science"), 
                                   selected = "Math",
                                   multiple = FALSE,
                                   options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "dt_vars_",
                                   label = "Variables",
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
                                   options = list(style = "myClass", `actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                                   inline = FALSE,
                                   width = NULL
                                 ))
                             )
                         ),
                     div(
                       style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       box(title = tags$p("Step 2: Data Splitting", style = "font-weight: bold;"),
                           collapsible = TRUE,
                           collapsed = TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               chooseSliderSkin("Flat"),
                               sliderInput(inputId = "dt_partition_",
                                           label = "Train-Test Partition:",
                                           min = 0.05,
                                           max = 0.95,
                                           value = c(0.8)
                                           )),
                           div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                               actionButton(inputId = "dt_action_",
                                            label = "Run Analysis",
                                            icon = icon("wrench"),
                                            style = 'padding: 4px; font-size: 10px'),
                               align = "center"
                           )                      
                           )
                     ),
                       
                      # Box Sidebar for Model Tuning Parameters
                       hidden(div(id = "dt_kfold_group",
                                           style = "padding = 0em; margin-right: -0.5em; margin-top: -1em",
                                           box(
                                             title = tags$p("Step 3: Tuning Parameters", style = "font-weight: bold;"),
                                             status = "primary",
                                             collapsible = TRUE,
                                             collapsed = FALSE,
                                             width = 12,
                                             div(style = "padding = 0em; margin-top: -0.5em",
                                                 numericInput(inputId = "dt_rkfold_number",
                                                              label = "K-Fold:",
                                                              min = 2,
                                                              max = 50,
                                                              step = 1,
                                                              value = 10),
                                                 div(style = "padding = 0em; margin-top: -0.8em",
                                                     numericInput(inputId = "dt_rkfold_repeat",
                                                                  label = "Repeat Count:",
                                                                  min = 1,
                                                                  max = 10,
                                                                  step = 1,
                                                                  value = 3)
                                                     ),
                                                 div(style = "padding = 0em; margin-top: 0em",
                                                     materialSwitch(inputId = "dt_bestcp",
                                                                  label = "Model with the best complexity parameter  ",
                                                                  value = TRUE,
                                                                  status = "warning")
                                                 ),
                                                 hidden(
                                                   div(id = "dt_cp_opts",
                                                     style = "padding = 0em; margin-top: -0.8em",
                                                     numericInput(inputId = "dt_cp",
                                                                  label = "Complexity Parameter:",
                                                                  min = 0.00001,
                                                                  max = 1,
                                                                  step = 0.00001,
                                                                  value = 0.02)
                                                     )
                                                   ),
                                                 div(style = "padding = 0em; margin-top: 2em; font-size: 10px;",
                                                     actionButton(inputId = "dt_tunemodel_",
                                                                  label = "Tune Model",
                                                                  icon = icon("scissors"),
                                                                  style = 'padding: 4px; font-size: 10px'
                                                                  ),
                                                     align = "center"
                                                     )
                                                 )
                                             )
                                  )
                              )

                   )
            ),
            
            #### Decision Tree Results Column 1 ----------------------------------------------------
          
            column(width = 5,
                   fluidRow(
                     div(
                       style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       box(title = tags$p("Fit Assessment", style = "font-weight: bold;"),
                           closable = FALSE,
                           width = 12,
                           status = "primary",

                           #### Decision Tree - Actual vs Predicted ----------------------------------------------------
                             column(width = 6, 
                                    plotOutput("dt_plot_predvsactual_",
                                               width = "100%",
                                               height = "30vh")),
                                    
                             
                             #### Decision Tree - Actual vs Residuals ----------------------------------------------------
                             column(width = 6, 
                                    plotOutput("dt_plot_residvsactual_",
                                               width = "100%",
                                               height = "30vh"))
                             
                           )
                       )
                     )
                     ,
                   fluidRow(div(
                     style = "padding = 0em; margin-left: 0em; margin-top: 0.5em; height: 100% ",
                     box(title = tags$p("Best Tune", style = "font-weight: bold;"),
                         closable = FALSE,
                         width = 12,
                         status = "primary",
                         plotOutput("dt_cp_plot_",
                                    width = "100%",
                                    height = "30vh"),
                         valueBoxOutput("dt_showcp_", width =12)
                         )
                     )
                     )
                   ),
            column(width = 5,
                   fluidRow(
                     style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                     div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                         valueBoxOutput("dt_R2_", width = 4),
                         valueBoxOutput("dt_RMSE_", width = 4),
                         valueBoxOutput("dt_MAE_", width = 4)
                         )
                     ),
                     div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                         box(title = tags$p("Decision Tree", style = "font-weight: bold;"),
                             closable = FALSE,
                             width = 12,
                             status = "primary",
                             plotOutput("dt_rpartplot_")
                             )
                     )
                   )
            ),
    
    ### Random Forest  ----------------------------------------------------
    tabItem(tabName = "tab_rf",
            
            #### Random Forest Toggle Column  ----------------------------------------------------
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p(span(icon("tree"), "Random Forest"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = FALSE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "A Random Forest is like a group decision-making team in machine learning. It combines the opinions of many “trees” (individual models) to make better predictions, creating a more robust and accurate overall model. This randomness introduces variability among individual trees, reducing the risk of overfitting and improving overall prediction performance."
                             )
                         )
                     ),
                     
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p("Step 1: Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = FALSE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "rf_target_",
                                   label = "Scores",
                                   choices = c("Math", "Reading", "Science"), 
                                   selected = "Math",
                                   multiple = FALSE,
                                   options = list(style = "myClass"),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "rf_vars_",
                                   label = "Variables",
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
                                   options = list(style = "myClass", `actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                                   inline = FALSE,
                                   width = NULL)
                                 )
                         )
                     ),
                     div(
                       style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       box(title = tags$p("Step 2: Data Splitting", style = "font-weight: bold;"),
                           collapsible = TRUE,
                           collapsed = TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               chooseSliderSkin("Flat"),
                               sliderInput(inputId = "rf_partition_",
                                           label = "Train-Test Partition:",
                                           min = 0.05,
                                           max = 0.95,
                                           value = c(0.8))
                               ),
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               chooseSliderSkin("Flat"),
                               sliderInput(inputId = "rf_treenum_",
                                           label = "No. of Trees:",
                                           min = 5,
                                           max = 500,
                                           value = c(50))
                               ),
                           div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                               selectInput(inputId = "rf_varimpmode_",
                                           label = "Variable Importance Measure:",
                                           choices = c("Gini Importance" = "impurity",
                                                       "Permutation Importance" = "permutation"),
                                           selected = "impurity")
                               )
                           )
                       ),
                     div(
                       style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       box(title = tags$p("Step 3: Resampling Options", style = "font-weight: bold;"),
                           collapsible = TRUE,
                           collapsed = TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,
                           div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                               selectInput(inputId = "rf_resamplingmethod_",
                                           label = "Resampling Method:",
                                           choices = c("Bootstrap" = "rf_bootstrap",
                                                       "Cross Validation" = "rf_cvkfold",
                                                       "Repeated Cross-Validation" = "rf_repeatkfold"),
                                           selected = "rf_bootstrap")
                               ),
                           hidden(tags$div(id = "rf_cvkfold_group",
                                           numericInput(inputId = "rf_cvkfold_number",
                                                        label = "K-fold:",
                                                        min = 2,
                                                        max = 50,
                                                        step = 1,
                                                        value = 10)
                                           )
                                  ),
                           hidden(tags$div(id = "rf_rcvkfold_group",
                                            numericInput(inputId = "rf_rcvkfold_number",
                                                         label = "K-Fold:",
                                                         min = 2,
                                                         max = 50,
                                                         step = 1,
                                                         value = 10),
                                            div(style = "padding = 0em; margin-top: -0.8em",
                                                numericInput(inputId = "rv_rcvkfold_repeat",
                                                             label = "Repeat Count:",
                                                             min = 1,
                                                             max = 10,
                                                             step = 1,
                                                             value = 3)
                                            )
                                        )
                                  )
                           )
                       ),
                     div(
                       style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       box(title = tags$p("Step 4: Tuning Parameters", style = "font-weight: bold;"),
                           collapsible = TRUE,
                           collapsed = TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,  
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               selectInput(inputId = "rf_splitrule_",
                                       label = "Split Rule:",
                                       choices = c("Variance" = "variance",
                                                    "Extra Trees" = "extratrees",
                                                    "Max Stat" = "maxstat"),
                                       selected = "variance")),

                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               chooseSliderSkin("Flat"),
                               sliderInput(inputId = "rf_minnodesize_",
                                           label = "Minimum Node Size:",
                                           min = 2,
                                           max = 50,
                                           value = 5)
                           ),


                           div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                               actionButton(inputId = "rf_action_",
                                            label = "Run Analysis",
                                            icon = icon("wrench"),
                                            style = 'padding: 4px; font-size: 10px'),
                               align = "center"
                               )
                           )
                       )
                     )
                   ),
            
            #### Random Forest Results Column  ----------------------------------------------------
            
            column(width = 10,
                   fluidRow(
                     style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                     div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                         valueBoxOutput("rf_display_R2_", width = 4),
                         valueBoxOutput("rf_display_RMSE_", width = 4),
                         valueBoxOutput("rf_display_MAE_", width = 4)
                     )
                   ),

                     fluidRow(
                       div(
                         style = "padding = 0em; margin-left: 0em; margin-top: 0em; height: 100% ",
                         box(title = tags$p("Fit Assessment", style = "font-weight: bold;"),
                           closable = FALSE,
                           width = 12,
                           status = "primary",
                           
                           #### Random Forest - Actual vs Predicted ----------------------------------------------------
                           column(width = 6, 
                                  plotOutput("rf_plotpredvsactual_",
                                             width = "100%",
                                             height = "30vh")),
                           
                           
                           #### Random Forest - Actual vs Residuals ----------------------------------------------------
                           column(width = 6, 
                                  plotOutput("rf_plotresidvsactual_",
                                             width = "100%",
                                             height = "30vh"))
                           )
                       ),

                     div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                         box(title = tags$p("Variable Importance", style = "font-weight: bold;"),
                             closable = FALSE,
                             width = 12,
                             status = "primary",
                             div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                               dropdownButton(
                                 numericInput(
                                     inputId = "rf_varimp_varcount",
                                     label = "Number of variables to display (limited to the number of available responses):",
                                     min = 5,
                                     max = 120,
                                     value = 10,
                                     width = "290px",
                                     step = 1),
                                 circle = FALSE,
                                 right = TRUE,
                                 status = "default",
                                 icon = icon("gear"), 
                                 width = "300px"
                                 #tooltip = tooltipOptions(title = "Click for more options")
                                     ),
                               align = "right"),
                             div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                                 plotOutput("rf_varimp_plot_",
                                            width = "100%"))
                             )
                         )
                     )
            )
            ),
    tabItem(tabName = "tab_gb",
            
            #### Gradient Boosting Toggle Column  ----------------------------------------------------
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p(span(icon("tree"), "Gradient Boosting"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = FALSE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "Gradient Boosting is an ensemble machine learning technique that combines the predictions from several models to improve the overall predictive accuracy."
                             )
                         )
                     ),
                     
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p("Step 1: Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = FALSE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "gb_target_",
                                   label = "Scores",
                                   choices = c("Math", "Reading", "Science"), 
                                   selected = "Math",
                                   multiple = FALSE,
                                   options = list(style = "myClass"),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "gb_vars_",
                                   label = "Variables",
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
                                   options = list(style = "myClass", `actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                                   inline = FALSE,
                                   width = NULL
                                 ))
                         )
                     ),
                     div(
                       style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       box(title = tags$p("Step 2: Data Splitting", style = "font-weight: bold;"),
                           collapsible = TRUE,
                           collapsed = TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               chooseSliderSkin("Flat"),
                               sliderInput(inputId = "gb_partition_",
                                           label = "Train-Test Partition:",
                                           min = 0.05,
                                           max = 0.95,
                                           value = c(0.8))
                           )
                           )
                       ),
                     div(
                       style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       box(title = tags$p("Step 3: Resampling Options", style = "font-weight: bold;"),
                           collapsible = TRUE,
                           collapsed = TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,
                           div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                               selectInput(inputId = "gb_resamplingmethod_",
                                           label = "Resampling Method:",
                                           choices = c("Bootstrap" = "gb_bootstrap",
                                                       "Cross Validation" = "gb_cvkfold",
                                                       "Repeated Cross-Validation" = "gb_repeatkfold"),
                                           selected = "gb_bootstrap")
                           ),
                           hidden(tags$div(id = "gb_cvkfold_group",
                                           numericInput(inputId = "gb_cvkfold_number",
                                                        label = "K-fold:",
                                                        min = 2,
                                                        max = 50,
                                                        step = 1,
                                                        value = 10)
                           )
                           ),
                           hidden(tags$div(id = "gb_rcvkfold_group",
                                           numericInput(inputId = "gb_rcvkfold_number",
                                                        label = "K-Fold:",
                                                        min = 2,
                                                        max = 50,
                                                        step = 1,
                                                        value = 10),
                                           div(style = "padding = 0em; margin-top: -0.8em",
                                               numericInput(inputId = "gb_rcvkfold_repeat",
                                                            label = "Repeat Count:",
                                                            min = 1,
                                                            max = 10,
                                                            step = 1,
                                                            value = 3)
                                               )
                                           )
                           )
                       )
                     ),
                   div(
                       style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       box(title = tags$p("Step 4: Tuning Parameters", style = "font-weight: bold;"),
                           collapsible = TRUE,
                           collapsed = TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,  
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               chooseSliderSkin("Flat"),
                               sliderInput(inputId = "gb_minnodesize",
                                           label = "Min. Node Size:",
                                           min = 2,
                                           max = 50,
                                           value = 5)
                           ),
                           hidden(tags$div(id = "gb_cvkfold_tune",
                                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                               sliderInput(inputId = "gb_cvkfold_interactiondepth",
                                                           label = "Max. Tree Depth (select range): ",
                                                           min = 2,
                                                           max = 10,
                                                           step = 1,
                                                           value = c(1,3)
                                                           )
                                               ),
                                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                               sliderInput(inputId = "gb_cvkfold_treenumrange",
                                                           label = "Boosting Iterations (choose 2 for comparison):",
                                                           min = 10,
                                                           max = 500,
                                                           step = 10,
                                                           value = c(10, 50)
                                                           )
                                               )
                                           )
                                  ),
                           hidden(tags$div(id = "gb_bs_tune",
                                           style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                               sliderInput(inputId = "gb_bs_interactiondepth",
                                                           label = "Max. Tree Depth: ",
                                                           min = 2,
                                                           max = 10,
                                                           step = 1,
                                                           value = 1
                                                           )
                                               ),
                                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                               sliderInput(inputId = "gb_bs_treenumrange",
                                                           label = "Boosting Iterations: ",
                                                           min = 10,
                                                           max = 500,
                                                           step = 10,
                                                           value = 10
                                                           )
                                               )
                                           )
                                  ),
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               numericInput(inputId = "gb_shrinkage",
                                            label = "Learning Rate:",
                                            min = 0.01,
                                            max = 1.0,
                                            step = 0.01,
                                            value = c(0.01)
                               )
                           ),
                           div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                               actionButton(inputId = "gb_action_",
                                            label = "Run Analysis",
                                            icon = icon("wrench"),
                                            style = 'padding: 4px; font-size: 10px'
                                            ),
                               align = "center"
                               )
                           )
                       )
                   )
                   ),
            
            (column(width = 5,
                    div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                        box(title = tags$p("Fit Assessment", style = "font-weight: bold;"),
                            status = "info",
                            collapsible = FALSE,
                            width = 12,
                            solidHeader =FALSE,

                                #### Gradient Boosting Actual vs Predicted ----------------------------------------------------
                                column(width = 6, 
                                       plotOutput("gb_plotpredvsactual_",
                                                  width = "100%",
                                                  height = "30vh")),
                                
                                
                                #### Gradient Boosting - Actual vs Residuals ----------------------------------------------------
                                column(width = 6, 
                                       plotOutput("gb_plotresidvsactual_",
                                                  width = "100%",
                                                  height = "30vh"))
                            )),
                    hidden(div(id = "gb_modelplottab",
                               style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                               box(title = tags$p("Resampling Profile", style = "font-weight: bold;"),
                                   status = "info",
                                   collapsible = FALSE,
                                   width = 12,
                                   solidHeader =FALSE,
                                   plotOutput("gb_modelplot_",
                                              width = "100%",
                                              height = "30vh"))
                        )
                        )
                    )

            ),
            (column(width = 5,
                    fluidRow(
                      style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                      div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                          valueBoxOutput("gb_display_R2_", width = 4),
                          valueBoxOutput("gb_display_RMSE_", width = 4),
                          valueBoxOutput("gb_display_MAE_", width = 4)
                      )
                    ),
                    div(style = "padding = 0em; margin-left: 0em; margin-top: 0em; height: 100% ",
                        box(title = tags$p("Variable Importance", style = "font-weight: bold;"),
                            status = "info",
                            collapsible = FALSE,
                            width = 12,
                            solidHeader =FALSE,
                            div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                                dropdownButton(
                                  numericInput(
                                    inputId = "gb_varimp_varcount",
                                    label = "Number of variables to display (limited to the number of available responses):",
                                    min = 5,
                                    max = 120,
                                    value = 10,
                                    width = "290px",
                                    step = 1),
                                  circle = FALSE,
                                  right = TRUE,
                                  status = "default",
                                  icon = icon("gear"), 
                                  width = "300px"
                                  #tooltip = tooltipOptions(title = "Click for more options")
                                ),
                                align = "right"),
                            div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                                plotOutput("gb_varimp_plot_",
                                           width = "100%"))
                            )
                    ),
                    hidden(div(id = "gb_besttuneplottab", 
                               style = "padding = 0em; margin-left: 0em; margin-top: 0em; height: 100% ",
                               box(title = tags$p("Best Tune", style = "font-weight: bold;"),
                                   status = "info",
                                   collapsible = FALSE,
                                   width = 12,
                                   solidHeader =FALSE,
                                   tableOutput("dt_besttune_"))
                        )
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

server <- function(input, output) {
  
  # Sidebar  ----------------------------------------------------
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
    
    shinyjs::html("pageHeader", header)
  })  

  
  # Dashboard ----------------------------------------------------
  output$home_studnum_ <- renderValueBox({
    valueBox(
      value = tags$p(nrow(stu), style = "font-size: 60%;"),
      subtitle = tags$p(paste0("Participating Students"), style = "font-size: 80%;"), 
      icon = tags$i(icon("children"), style="font-size: 60%"),
      color = "yellow"
    )
  })
  
  output$home_schnum_ <- renderValueBox({
    valueBox(
      value = tags$p(n_distinct(stu$SchoolID), style = "font-size: 60%;"), 
      subtitle = tags$p("Participanting Schools", style = "font-size: 80%;"), 
      icon = tags$i(icon("building-columns"), style="font-size: 60%"),
      color = "yellow"
    )
  })
  
  summarystatstable <- reactive({
    db_summarystats[input$db_hist_subject, ]
  })
  
    
  output$db_summarystats_ <- DT::renderDataTable({
      datatable(summarystatstable(),
                options = list(dom = 't'),
                class = "compact")
  })

  
  
  output$db_varlist_ <- DT::renderDataTable({
    datatable(varlist,
              class = "compact",
              options = list(hover = TRUE))
  })
  


  output$db_hist_scores_ <- renderPlotly({
    histog <- 
      plot_ly(stu,
            color = I("#caced8")) %>% 
      add_histogram(x = ~ get(input$db_hist_subject),
                    hoverlabel = list(
                      bgcolor = "black",
                      bordercolor = "#ffffff"),
                    hovertemplate=paste('Score: %{x}<br>',
                                        'Number of Students: %{y}<extra></extra>')
      ) %>% 
      layout(
        autosize = TRUE,
        xaxis = list(title = paste0(input$db_hist_subject, " Score"),   
                     showticklabels = TRUE),
        yaxis = list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = FALSE
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        bargap = 0.1
      )
    
    boxp <- plot_ly(stu,
                    x = ~ get(input$db_hist_subject),
                    color = I("#caced8"),
                    type = "box",
                    boxmean = T,
                    fillcolor = "",
                    line = list(color = "#caced8",
                                width = 1.5),
                    hoverlabel = list(
                      bgcolor = "black",
                      bordercolor = "#ffffff"
                    ),
                    marker = list(color = 'rgb(8,81,156)',
                                  outliercolor = 'rgba(219, 64, 82, 0.6)',
                                  line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                              outlierwidth = 2))
    ) %>% 
      layout(
        xaxis = list(title = "",
                     zeroline = FALSE,
                     showline = FALSE,
                     showticklabels = FALSE,
                     showgrid = FALSE,
                     tickfont = list(size = 8)
                     ),
        yaxis = list(title = "",
                     zeroline = FALSE,
                     showline = FALSE,
                     showticklabels = FALSE,
                     showgrid = FALSE
             ))
    
    subplot(boxp, histog, 
            nrows = 2,
            heights = c(0.2, 0.8),
            #widths = c(0.8, 0.2),
            shareX = TRUE) %>% 
      layout(showlegend = FALSE
      )
    
  })
  
  
  
  output$db_boxviolin <- renderPlotly({
    plot_ly(stu, 
            x = ~ get(input$db_boxviolin_var),
            y = ~ get(input$db_boxviolin_target),
            line = list(width=1),
            type = "violin",
            spanmode = 'hard',
            #marker = list(opacity = 0.5,
            #              line = list(width = 2)),
            box = list(visible = T),
            #points = 'all',
            scalemode = 'count',
            meanline = list(visible = T,
                            color = "red"),
            color = I('#caced8'),
            marker = list(
              line = list(
                width = 2,
                color = '#caced8'
              ),
              symbol = 'line-ns')
            ) %>% 
      layout(yaxis = list(title = input$db_boxviolin_target),
             xaxis = list(title = input$db_boxviolin_var)
             )
  })
  
  output$db_bar <- renderPlotly({
    plot_ly(stu, 
            x = ~ get(input$db_bar_var)
            ) %>% 
      add_histogram(
            textposition = 'auto',
            color = I('#caced8'),
            textfont = list(size =10, 
                            color = 'rgb(158,202,225)'
                          )
            ) %>% 
      layout(yaxis = list(title = "No. of Students"),
             xaxis = list(title = input$db_bar_var)
      )
  })
  
  # DT Data Manipulation  ----------------------------------------------------

  # Combine Selected Variables
  dt_selected_variables <- reactive({
      c(input$dt_target_,
        input$dt_vars_)
      })  
  
  dt_data <- reactive({
      stu_mb %>%
        select(all_of(dt_selected_variables()))
    }
  )
  
  dt_index <- eventReactive(
    input$dt_action_, {
      set.seed(1234)
      caret::createDataPartition(dt_data()[[1]], p = input$dt_partition_, list = FALSE)      
    }
  )
  
  dt_traindata <- eventReactive(
    input$dt_action_, {
      dt_data()[dt_index(),]
    })
  
  dt_testdata <- eventReactive(
    input$dt_action_, {
      dt_data()[-dt_index(),]
    })

  # DT Sidebar Toggles  ----------------------------------------------------
  ## Model Tuning Parameters 

  ### Display Selection after first model run
  observeEvent(input$dt_action_, {
    shinyjs::show("dt_kfold_group")
  })
  
  ## Hide Complexity Parameter when box is checked, ie, model with best CP to be plotted
  observeEvent(input$dt_bestcp, {
    if (input$dt_bestcp == TRUE) {
    shinyjs::hide("dt_cp_opts")
    }
    else {shinyjs::show("dt_cp_opts")
      }
  })
  
  # DT Model  ----------------------------------------------------
  dtmodel <- eventReactive(
    input$dt_action_, {
      if (input$dt_target_ == "Math") {
        set.seed(1234)
        
        train(
          form = Math ~ .,
          data = dt_traindata(),
          method = "rpart",
          trControl = trainControl(method = "repeatedcv",
                                   number = 5,
                                   repeats = 2)
          #   if(input$dt_resamplingmethod_ == "dt_bootstrap") 
          #   {dt_bs_resample()}
          # else if (input$dt_resamplingmethod_ == "dt_cvkfold") 
          # {dt_cvkfold_resample()}
          # else {dt_cvrepeatkfold_resample()}
        )}
      
      else if (input$dt_target_ == "Reading") {
        set.seed(1234)
        
        train(
          form = Reading ~ .,
          data = dt_traindata(),
          method = "rpart",
          trControl = trainControl(method = "repeatedcv",
                                   number = 5,
                                   repeats = 2)
        )}
      
      else {
        set.seed(1234)
        
        train(
          form = Science ~ .,
          data = dt_traindata(),
          method = "rpart",
          trControl = trainControl(method = "repeatedcv",
                                   number = 5,
                                   repeats = 2)
        )}
      })
  
  dtmodel_prune <- eventReactive(
    input$dt_tunemodel_, {
      if (input$dt_target_ == "Math") {
        set.seed(1234)
        
        train(
          form = Math ~ .,
          data = dt_traindata(),
          method = "rpart",
          tuneGrid = expand.grid(cp = if(input$dt_bestcp == TRUE) {dtmodel()$bestTune}
                                 else {input$dt_cp}
                                 ),
          trControl = trainControl(method = "repeatedcv",
                                   number = input$dt_rkfold_number,
                                   repeats = input$dt_rkfold_repeat)
        )}
      
      else if (input$dt_target_ == "Reading") {
        set.seed(1234)
        
        train(
          form = Reading ~ .,
          data = dt_traindata(),
          method = "rpart",
          tuneGrid = expand.grid(cp = input$dt_cp),
          trControl = trainControl(method = "repeatedcv",
                                   number = input$dt_rkfold_number,
                                   repeats = input$dt_rkfold_repeat)
        )}
      
      else {
        set.seed(1234)
        
        train(
          form = Science ~ .,
          data = dt_traindata(),
          method = "rpart",
          tuneGrid = expand.grid(cp = input$dt_cp),
          trControl = trainControl(method = "repeatedcv",
                                   number = input$dt_rkfold_number,
                                   repeats = input$dt_rkfold_repeat)
        )}
    })  
  
  dt_rpartplot <- eventReactive(
    input$dt_action_, {
      rpart.plot(dtmodel()$finalModel,
                 box.palette="GnBu",
                 branch.lty=3, 
                 shadow.col="gray", 
                 nn=TRUE)
    })
  
  dt_rpartplot_prune <- eventReactive(
    input$dt_tunemodel_, {
      rpart.plot(dtmodel_prune()$finalModel,
                 box.palette="GnBu",
                 branch.lty=3, 
                 shadow.col="gray", 
                 nn=TRUE)
    })
  
  ### Function to plot decision tree
  dt_plotrpart = function(modeltype){
    output$dt_rpartplot_ = renderPlot(modeltype)
  }

  observeEvent(input$dt_action_, dt_plotrpart(dt_rpartplot()))
  observeEvent(input$dt_tunemodel_, dt_plotrpart(dt_rpartplot_prune()))

  
  ## Predicting Results
  predictdt_model <- eventReactive(
    input$dt_action_, {
      predict(dtmodel(), 
              newdata = dt_testdata())
        })
  
  predict_dt <- eventReactive(
    input$dt_action_, {
        bind_cols(
          Actual = dt_testdata()[[1]],
          Predicted = predictdt_model(),
          Residuals = predictdt_model() - dt_testdata()[[1]])
      })
  
  predictdt_model_prune <- eventReactive(
    input$dt_tunemodel_, {
      predict(dtmodel_prune(), 
              newdata = dt_testdata())
    })
  
  predict_dt_prune <- eventReactive(
    input$dt_tunemodel_, {
      bind_cols(
        Actual = dt_testdata()[[1]],
        Predicted = predictdt_model_prune(),
        Residuals = predictdt_model_prune() - dt_testdata()[[1]])
    })
  
  ## Plot Predicted vs Actual
  dt_plot_predvsactual <- eventReactive(
    input$dt_action_, {
      ggplot(data = predict_dt(),
             aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.2, color = "grey40") +
        geom_smooth(method = "loess", formula = "y ~ x", color="#dfb2e9") +
        geom_abline(intercept = 0, slope = 1, linetype = 2, color = "#20948b", size = 0.8) +
        labs(title = "Predicted vs Actual")+ 
        theme_minimal()+
        theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              plot.title = element_text(size=12, face="bold"),
              axis.title = element_text(face="bold"),
              axis.text = element_text(face="bold")) 
    })
  
  ## Plot Predicted vs Actual - Pruned Model
  dt_plot_predvsactual_prune <- eventReactive(
    input$dt_tunemodel_, {
      ggplot(data = predict_dt_prune(),
             aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.2, color = "grey40") +
        geom_smooth(method = "loess", formula = "y ~ x", color="#dfb2e9") +
        geom_abline(intercept = 0, slope = 1, linetype = 2, color = "#20948b", size = 0.8) +
        labs(title = "Predicted vs Actual")+ 
        theme_minimal()+
        theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              plot.title = element_text(size=12, face="bold"),
              axis.title = element_text(face="bold"),
              axis.text = element_text(face="bold")) 
    })
  
  
  ### Function to plot Residuals vs Actual
  dt_plotpredvsactual = function(modeltype){
    output$dt_plot_predvsactual_ = renderPlot(modeltype)
  }
  
  observeEvent(input$dt_action_, dt_plotpredvsactual(dt_plot_predvsactual()))
  observeEvent(input$dt_tunemodel_, dt_plotpredvsactual(dt_plot_predvsactual_prune()))
  
  
  ## Plot Residuals vs Actual
  dt_plot_residvsactual <- eventReactive(
    input$dt_action_, {
      ggplot(data = predict_dt(),
             aes(x = Actual, y = Residuals)) +
        geom_point(alpha = 0.2, color = "grey40") +
        geom_smooth(method = "loess", formula = "y ~ x", color="#dfb2e9") +
        geom_hline(yintercept = 0, linetype = 2, color = "#20948b", size = 0.8) +
        labs(title = "Residuals vs Actual")+ 
        theme_minimal()+
        theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              plot.title = element_text(size=12, face="bold"),
              axis.title = element_text(face="bold"),
              axis.text = element_text(face="bold")) 
    })
  
  ## Plot Residuals vs Actual - Pruned Model
  dt_plot_residvsactual_prune <- eventReactive(
    input$dt_tunemodel_, {
      ggplot(data = predict_dt_prune(),
             aes(x = Actual, y = Residuals)) +
        geom_point(alpha = 0.2, color = "grey40") +
        geom_smooth(method = "loess", formula = "y ~ x", color="#dfb2e9") +
        geom_hline(yintercept = 0, linetype = 2, color = "#20948b", size = 0.8) +
        labs(title = "Residuals vs Predicted")+ 
        theme_minimal()+
        theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              plot.title = element_text(size=12, face="bold"),
              axis.title = element_text(face="bold"),
              axis.text = element_text(face="bold")) 
    }) 
  
  ### Function to plot Residuals vs Actual
  dt_plotresidvsactual = function(modeltype){
    output$dt_plot_residvsactual_ = renderPlot(modeltype)
  }
  
  observeEvent(input$dt_action_, dt_plotresidvsactual(dt_plot_residvsactual()))
  observeEvent(input$dt_tunemodel_, dt_plotresidvsactual(dt_plot_residvsactual_prune()))


  
  ## Model Statistic
  ### RMSE
  dt_RMSE <- eventReactive(
    input$dt_action_, {
      round(caret::RMSE(predictdt_model(),
                  dt_testdata()[[1]]),3)
    }
  )
  
  dt_RMSE_prune <- eventReactive(
    input$dt_tunemodel_, {
      round(caret::RMSE(predictdt_model_prune(),
                        dt_testdata()[[1]]),3)
    }
  )
  
  dt_display_RMSE = function(modeltype){
    output$dt_RMSE_ = renderValueBox(
      valueBox(
        value = tags$p(modeltype, style = "font-size: 60%;"), 
        subtitle = tags$p(paste0("RMSE"), style = "font-size: 80%;"), 
        icon = tags$i(icon("calculator"), style="font-size: 60%"),
        color = "yellow"
      )
    )
  }

  observeEvent(input$dt_action_, dt_display_RMSE(dt_RMSE()))
  observeEvent(input$dt_tunemodel_, dt_display_RMSE(dt_RMSE_prune()))
  
  ### R2
  dt_R2 <- eventReactive(
    input$dt_action_, {
      round(caret::R2(predictdt_model(),
                  dt_testdata()[[1]]),3)
    }
  )
  
  dt_R2_prune <- eventReactive(
    input$dt_tunemodel_, {
      round(caret::R2(predictdt_model_prune(),
                       dt_testdata()[[1]]),3)
    }
  )  

  
  dt_display_R2 = function(modeltype){
    output$dt_R2_ = renderValueBox(
      valueBox(
        value = tags$p(modeltype, style = "font-size: 60%;"), 
        subtitle = tags$p(paste0("R-Square"), style = "font-size: 80%;"), 
        icon = tags$i(icon("calculator"), style="font-size: 60%"),
        color = "yellow"
      )
    )
  }
  
  observeEvent(input$dt_action_, dt_display_R2(dt_R2()))
  observeEvent(input$dt_tunemodel_, dt_display_R2(dt_R2_prune()))
  
  ### MAE
  dt_MAE <- eventReactive(
    input$dt_action_, {
      round(caret::MAE(predictdt_model(),
                       dt_testdata()[[1]]),3)
    }
  )  
  
  dt_MAE_prune <- eventReactive(
    input$dt_tunemodel_, {
      round(caret::MAE(predictdt_model_prune(),
                       dt_testdata()[[1]]),3)
    }
  )  


  dt_display_MAE = function(modeltype){
    output$dt_MAE_ = renderValueBox(
      valueBox(
        value = tags$p(modeltype, style = "font-size: 60%;"), 
        subtitle = tags$p(paste0("MAE"), style = "font-size: 80%;"), 
        icon = tags$i(icon("calculator"), style="font-size: 60%"),
        color = "yellow"
      )
    )
  }
  
  observeEvent(input$dt_action_, dt_display_MAE(dt_MAE()))
  observeEvent(input$dt_tunemodel_, dt_display_MAE(dt_MAE_prune()))
  
  
  
  ### CP
  #### CP ValueBox
  dt_cp_plot <- eventReactive(
    input$dt_action_, {
      ggplot(dtmodel()) +
        geom_point(alpha = 0.2, color = "grey40")+
        theme_minimal()+
        labs(y = "RMSE")+
        theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              plot.title = element_text(size=12, face="bold"),
              axis.title = element_text(face="bold"),
              axis.text = element_text(face="bold")) 
    }
  )  

  
  
  ### Function to plot Residuals vs Actual
  dt_plotcpplot = function(modeltype){
    output$dt_cp_plot_ = renderPlot(modeltype)
  }
  
  observeEvent(input$dt_action_, dt_plotcpplot(dt_cp_plot()))
  # observeEvent(input$dt_tunemodel_, dt_plotcpplot(dt_cp_plot_prune()))
  
  
  #### CP Plot
  dt_showcp <- eventReactive(
    input$dt_action_, {
      round(dtmodel()$bestTune,5)
    }
  )  
  

 
  dt_showcpbox = function(modeltype){
    output$dt_showcp_ = renderValueBox(
      valueBox(
        value = tags$p(modeltype, style = "font-size: 60%;"), 
        subtitle = tags$p("Complexity Parameter:  Used to control the size of the decision tree and to select the optimal tree size. The tree will stop dividing nodes when the reduction in relative error is less than a certain value.", 
                          style = "font-size: 100%;"), 
        icon = tags$i(icon("trophy"), style="font-size: 80%"),
        color = "yellow"
      )
    )
  }
  
  observeEvent(input$dt_action_, dt_showcpbox(dt_showcp()))
  # observeEvent(input$dt_tunemodel_, dt_showcpbox(dt_showcp_prune()))
  
  
  # RF Data Manipulation  ----------------------------------------------------
  
  # Combine Selected Variables
  rf_selected_variables <- reactive({
    c(input$rf_target_,
      input$rf_vars_)
  })  
  
  rf_data <- reactive({
    stu_mb %>%
      select(all_of(rf_selected_variables()))
  }
  )
  
  rf_index <- eventReactive(
    input$rf_action_, {
      set.seed(1234)
      caret::createDataPartition(rf_data()[[1]], p = input$rf_partition_, list = FALSE)      
    }
  )
  
  rf_traindata <- eventReactive(
    input$rf_action_, {
      rf_data()[rf_index(),]
    })
  
  rf_testdata <- eventReactive(
    input$rf_action_, {
      rf_data()[-rf_index(),]
    })
  
  # RF Sidebar Toggles  ----------------------------------------------------
  ## Model Tuning Parameters 
  
  # ### Display Selection depending on resampling method
   observeEvent(input$rf_resamplingmethod_, {
    if (input$rf_resamplingmethod_ == "rf_cvkfold") {
      shinyjs::show("rf_cvkfold_group")
    }
     else {
      shinyjs::hide("rf_cvkfold_group")
    }
  })

  observeEvent(input$rf_resamplingmethod_, {
    if (input$rf_resamplingmethod_ == "rf_repeatkfold") {
      shinyjs::show("rf_rcvkfold_group")
    }
    else {
      shinyjs::hide("rf_rcvkfold_group")
    }
  })
  
  # RF Model  ----------------------------------------------------
  
  ## Assign Model Train Control Parameters
  rf_bs_resample <- eventReactive(
    input$rf_action_, {
    trainControl(method = "none"
                 )
      })

  rf_cvkfold_resample  <- eventReactive(
    input$rf_action_, {
      trainControl(method = "cv",
                   number = input$rf_cvkfold_number
      )
    })

  rf_rcvkfold_resample <- eventReactive(
    input$rf_action_, {
      trainControl(method = "repeatedcv",
                   number = input$rf_rcvkfold_number,
                   repeats = input$rv_rcvkfold_repeat
      )
    })
  
  
  rf_tgrid <- eventReactive(
    input$rf_action_, {
      expand.grid(
        mtry = sqrt(ncol(rf_traindata())),
        splitrule = input$rf_splitrule_,
        min.node.size = input$rf_minnodesize_
      )
      
    })
  
  rfmodel <- eventReactive(
    input$rf_action_, {
      if (input$rf_target_ == "Math") {
        set.seed(1234)
        
        train(
          form = Math ~ .,
          data = rf_traindata(),
          method = "ranger",
          num.trees = input$rf_treenum_,
          importance = input$rf_varimpmode_,
          local.importance = TRUE,
          tuneGrid  = rf_tgrid(),
          trControl =
            if(input$rf_resamplingmethod_ == "rf_bootstrap")
              { rf_bs_resample() }
            else if (input$rf_resamplingmethod_ == "rf_cvkfold")
            { rf_cvkfold_resample() }
            else { rf_rcvkfold_resample() }
        )}
      
      else if (input$rf_target_ == "Reading") {
        set.seed(1234)
        
        train(
          form = Reading ~ .,
          data = rf_traindata(),
          method = "ranger",
          num.trees = input$rf_treenum_,
          importance = input$rf_varimpmode_,
          local.importance = TRUE,
          tuneGrid  = rf_tgrid(),
          trControl = 
            if(input$rf_resamplingmethod_ == "rf_bootstrap")
            { rf_bs_resample() }
          else if (input$rf_resamplingmethod_ == "rf_cvkfold")
          { rf_cvkfold_resample() }
          else { rf_rcvkfold_resample() }
        )}
      
      else {
        set.seed(1234)
        
        train(
          form = Science ~ .,
          data = rf_traindata(),
          method = "ranger",
          num.trees = input$rf_treenum_,
          importance = input$rf_varimpmode_,
          local.importance = TRUE,
          tuneGrid  = rf_tgrid(),
          trControl = 
            if(input$rf_resamplingmethod_ == "rf_bootstrap")
            { rf_bs_resample() }
          else if (input$rf_resamplingmethod_ == "rf_cvkfold")
          { rf_cvkfold_resample() }
          else { rf_rcvkfold_resample() }
        )}
    })  
  
  ## Predicting Results
  predictrf_model <- eventReactive(
    input$rf_action_, {
      predict(rfmodel(), 
              newdata = rf_testdata())
    })
  
  predict_rf <- eventReactive(
    input$rf_action_, {
      bind_cols(
        Actual = rf_testdata()[[1]],
        Predicted = predictrf_model(),
        Residuals = predictrf_model() - rf_testdata()[[1]])
    })
  
  ## Plot Predicted vs Actual
  rf_plot_predvsactual <- eventReactive(
    input$rf_action_, {
      ggplot(data = predict_rf(),
             aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.2, color = "grey40") +
        geom_smooth(method = "loess", formula = "y ~ x", color="#dfb2e9") +
        geom_abline(intercept = 0, slope = 1, linetype = 2, color = "#20948b", size = 0.8) +
        labs(title = "Predicted vs Actual")+ 
        theme_minimal()+
        theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              plot.title = element_text(size=12, face="bold"),
              axis.title = element_text(face="bold"),
              axis.text = element_text(face="bold")) 
    })
  
  output$rf_plotpredvsactual_ <- renderPlot({
    rf_plot_predvsactual()
  })

  ## Plot Residuals vs Actual
  rf_plot_residvsactual <- eventReactive(
    input$rf_action_, {
      ggplot(data = predict_rf(),
             aes(x = Actual, y = Residuals)) +
        geom_point(alpha = 0.2, color = "grey40") +
        geom_smooth(method = "loess", formula = "y ~ x", color="#dfb2e9") +
        geom_hline(yintercept = 0, linetype = 2, color = "#20948b", size = 0.8) +
        labs(title = "Residuals vs Actual")+ 
        theme_minimal()+
        theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              plot.title = element_text(size=12, face="bold"),
              axis.title = element_text(face="bold"),
              axis.text = element_text(face="bold")) 
    })
  
  
  output$rf_plotresidvsactual_ <- renderPlot({
    rf_plot_residvsactual()
  })
  

  ## Statistical Analysis
  ### R2
  rf_R2 <- eventReactive(
    input$rf_action_, {
      round(caret::R2(predictrf_model(),
                       rf_testdata()[[1]]),3)
    }
  )
      
  ### RMSE
  rf_RMSE <- eventReactive(
    input$rf_action_, {
      round(caret::RMSE(predictrf_model(),
                        rf_testdata()[[1]]),3)
    }
  )
  
  ### MAE
  rf_MAE <- eventReactive(
    input$rf_action_, {
      round(caret::MAE(predictrf_model(),
                       rf_testdata()[[1]]),3)
    }
  )  
  
  output$rf_display_R2_ <- renderValueBox({
    valueBox(
      value = tags$p(rf_R2(), style = "font-size: 60%;"),
      subtitle = tags$p(paste0("R-Square"), style = "font-size: 80%;"), 
      icon = tags$i(icon("calculator"), style="font-size: 60%"),
      color = "yellow"
    )
  })
  
  output$rf_display_RMSE_ <- renderValueBox({
    valueBox(
      value = tags$p(rf_RMSE(), style = "font-size: 60%;"),
      subtitle = tags$p(paste0("RMSE"), style = "font-size: 80%;"), 
      icon = tags$i(icon("calculator"), style="font-size: 60%"),
      color = "yellow"
    )
  })
  
  output$rf_display_MAE_ <- renderValueBox({
    valueBox(
      value = tags$p(rf_MAE(), style = "font-size: 60%;"),
      subtitle = tags$p(paste0("MAE"), style = "font-size: 80%;"), 
      icon = tags$i(icon("calculator"), style="font-size: 60%"),
      color = "yellow"
    )
  })
  
  ## Random Forest - Variable Importance
  rf_varimp_plot <- reactive(
    if(input$rf_action_) {
      vip::vip(rfmodel(), 
               num_features = input$rf_varimp_varcount, 
               geom = "col",
               jitter = TRUE,
               aesthetics = list(color = "grey40"))
    }
  )  
  
  output$rf_varimp_plot_ <-
    renderPlot({
      rf_varimp_plot()
    })
  
  # GB Data Manipulation  ----------------------------------------------------
  
  # Combine Selected Variables
  gb_selected_variables <- reactive({
    c(input$gb_target_,
      input$gb_vars_)
  })  
  
  gb_data <- reactive({
    stu_mb %>%
      select(all_of(gb_selected_variables()))
  }
  )
  
  gb_index <- eventReactive(
    input$gb_action_, {
      set.seed(1234)
      caret::createDataPartition(gb_data()[[1]], p = input$gb_partition_, list = FALSE)      
    }
  )
  
  gb_traindata <- eventReactive(
    input$gb_action_, {
      gb_data()[gb_index(),]
    })
  
  gb_testdata <- eventReactive(
    input$gb_action_, {
      gb_data()[-gb_index(),]
    })
  
  # RF Sidebar Toggles  ----------------------------------------------------
  ## Model Tuning Parameters 
  
  # ### Display Selection depending on resampling method
  observeEvent(input$gb_resamplingmethod_, {
    if (input$gb_resamplingmethod_ == "gb_cvkfold") {
      shinyjs::show("gb_cvkfold_group")
    }
    else {
      shinyjs::hide("gb_cvkfold_group")
    }
  })
  
  observeEvent(input$gb_resamplingmethod_, {
    if (input$gb_resamplingmethod_ == "gb_repeatkfold") {
      shinyjs::show("gb_rcvkfold_group")
    }
    else {
      shinyjs::hide("gb_rcvkfold_group")
    }
  })
  
  #### Display tuning parameter selection box
  observeEvent(input$gb_resamplingmethod_, {
    if (input$gb_resamplingmethod_ == "gb_bootstrap") {      
      shinyjs::hide("gb_cvkfold_tune")
      shinyjs::show("gb_bs_tune")
    }
    else {

      shinyjs::show("gb_cvkfold_tune")
      shinyjs::hide("gb_bs_tune")
      }
  })

   
  
  # GB Model  ----------------------------------------------------
  
  ## Assign Model Train Control Parameters
  gb_bs_resample <- eventReactive(
    input$gb_action_, {
      trainControl(method = "none"
      )
    })
  
  gb_cvkfold_resample  <- eventReactive(
    input$gb_action_, {
      trainControl(method = "cv",
                   number = input$gb_cvkfold_number
      )
    })
  
  gb_rcvkfold_resample <- eventReactive(
    input$gb_action_, {
      trainControl(method = "repeatedcv",
                   number = input$gb_rcvkfold_number,
                   repeats = input$gb_rcvkfold_repeat
      )
    })
  
  gb_cvkfold_treenumrange_  <- eventReactive(
    input$gb_action_, {
      seq(from = input$gb_cvkfold_treenumrange[1],
          to = input$gb_cvkfold_treenumrange[2],
          length.out = 10)
    }) 
  

  gb_cvkfold_interactiondepth_ <- eventReactive(
    input$gb_action_, {
      seq(from = max(1, input$gb_cvkfold_interactiondepth[1]),
          to = min(10, input$gb_cvkfold_interactiondepth[2]),
          by = 1)
    }) 
  
  gb_cvkfold_shrinkage_ <- eventReactive(
    input$gb_action_, {
      seq(from = max(0.01, input$gb_shrinkage - 0.01),
          to = min(1, input$gb_shrinkage + 0.01),
          length.out = 3)
    }) 
  
  gb_tgrid <- eventReactive(
    input$gb_action_, {
      if (input$gb_resamplingmethod_ == "gb_bootstrap") {
        expand.grid(
          interaction.depth = input$gb_bs_interactiondepth, 
          n.trees = input$gb_bs_treenumrange,
          shrinkage = input$gb_shrinkage, 
          n.minobsinnode = input$gb_minnodesize
        )        
      }
      else (
      expand.grid(
        interaction.depth = gb_cvkfold_interactiondepth_(), 
        n.trees = gb_cvkfold_treenumrange_(),
        shrinkage = gb_cvkfold_shrinkage_(), 
        n.minobsinnode = input$gb_minnodesize
      ))
      
    })
  

  gbmodel <- eventReactive(
    input$gb_action_, {
      if (input$gb_target_ == "Math") {
        set.seed(1234)
        
        train(
          form = Math ~ .,
          data = gb_traindata(),
          method = "gbm",
          verbose = FALSE,
          tuneGrid  = gb_tgrid(),
          trControl =
            if(input$gb_resamplingmethod_ == "gb_bootstrap")
            { gb_bs_resample() }
          else if (input$gb_resamplingmethod_ == "gb_cvkfold")
          { gb_cvkfold_resample() }
          else { gb_rcvkfold_resample() }
        )}
      
      else if (input$gb_target_ == "Reading") {
        set.seed(1234)
        
        train(
          form = Reading ~ .,
          data = gb_traindata(),
          method = "gbm",
          verbose = FALSE,
          tuneGrid  = gb_tgrid(),
          trControl =
            if(input$gb_resamplingmethod_ == "gb_bootstrap")
            { gb_bs_resample() }
          else if (input$gb_resamplingmethod_ == "gb_cvkfold")
          { gb_cvkfold_resample() }
          else { gb_rcvkfold_resample() }
        )}
      
      else {
        set.seed(1234)
        
        train(
          form = Science ~ .,
          data = gb_traindata(),
          method = "gbm",
          verbose = FALSE,
          tuneGrid  = gb_tgrid(),
          trControl =
            if(input$gb_resamplingmethod_ == "gb_bootstrap")
            { gb_bs_resample() }
          else if (input$gb_resamplingmethod_ == "gb_cvkfold")
          { gb_cvkfold_resample() }
          else { gb_rcvkfold_resample() }
        )}
    })  
  
  ## Predicting Results
  predictgb_model <- eventReactive(
    input$gb_action_, {
      predict(gbmodel(), 
              newdata = gb_testdata())
    })
  
  predict_gb <- eventReactive(
    input$gb_action_, {
      bind_cols(
        Actual = gb_testdata()[[1]],
        Predicted = predictgb_model(),
        Residuals = predictgb_model() - gb_testdata()[[1]])
    })
  
  ## Plot Predicted vs Actual
  gb_plot_predvsactual <- eventReactive(
    input$gb_action_, {
      ggplot(data = predict_gb(),
             aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.2, color = "grey40") +
        geom_smooth(method = "loess", formula = "y ~ x", color="#dfb2e9") +
        geom_abline(intercept = 0, slope = 1, linetype = 2, color = "#20948b", size = 0.8) +
        labs(title = "Predicted vs Actual")+ 
        theme_minimal()+
        theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              plot.title = element_text(size=12, face="bold"),
              axis.title = element_text(face="bold"),
              axis.text = element_text(face="bold")) 
    })
  
  output$gb_plotpredvsactual_ <- renderPlot({
    gb_plot_predvsactual()
  })
  
  ## Plot Residuals vs Actual
  gb_plot_residvsactual <- eventReactive(
    input$gb_action_, {
      ggplot(data = predict_gb(),
             aes(x = Actual, y = Residuals)) +
        geom_point(alpha = 0.2, color = "grey40") +
        geom_smooth(method = "loess", formula = "y ~ x", color="#dfb2e9") +
        geom_hline(yintercept = 0, linetype = 2, color = "#20948b", size = 0.8) +
        labs(title = "Residuals vs Actual")+ 
        theme_minimal()+
        theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              plot.title = element_text(size=12, face="bold"),
              axis.title = element_text(face="bold"),
              axis.text = element_text(face="bold")) 
    })
  
  
  output$gb_plotresidvsactual_ <- renderPlot({
    gb_plot_residvsactual()
  })
  
  ## Statistical Analysis
  ### R2
  gb_R2 <- eventReactive(
    input$gb_action_, {
      round(caret::R2(predictgb_model(),
                      gb_testdata()[[1]]),3)
    }
  )
  
  output$gb_display_R2_ <- renderValueBox({
    valueBox(
      value = tags$p(gb_R2(), style = "font-size: 60%;"),
      subtitle = tags$p(paste0("R-Square"), style = "font-size: 80%;"), 
      icon = tags$i(icon("calculator"), style="font-size: 60%"),
      color = "yellow"
    )
  })
  
  ### RMSE
  gb_RMSE <- eventReactive(
    input$gb_action_, {
      round(caret::RMSE(predictgb_model(),
                        gb_testdata()[[1]]),3)
    }
  )
  
  output$gb_display_RMSE_ <- renderValueBox({
    valueBox(
      value = tags$p(gb_RMSE(), style = "font-size: 60%;"),
      subtitle = tags$p(paste0("RMSE"), style = "font-size: 80%;"), 
      icon = tags$i(icon("calculator"), style="font-size: 60%"),
      color = "yellow"
    )
  })
  
  ### MAE
  gb_MAE <- eventReactive(
    input$gb_action_, {
      round(caret::MAE(predictgb_model(),
                       gb_testdata()[[1]]),3)
    }
  )  

  output$gb_display_MAE_ <- renderValueBox({
    valueBox(
      value = tags$p(gb_MAE(), style = "font-size: 60%;"),
      subtitle = tags$p(paste0("MAE"), style = "font-size: 80%;"), 
      icon = tags$i(icon("calculator"), style="font-size: 60%"),
      color = "yellow"
    )
  })
  
  
  #### Best Tune Results
  dt_besttune <- eventReactive(
    input$gb_action_, {
     gbmodel()$bestTune %>% 
        knitr::kable(col.names = c("Number of Trees",
                                   "Max. Tree Depth",
                                   "Learning Rate",
                                   "Min. Node Size")) %>% 
        kable_styling(full_width = F,
                      bootstrap_options = c("hover"))
    }
  ) 

  
  output$dt_besttune_ <- renderText({
    dt_besttune()
  })
  
  
  gb_modelplot <- eventReactive(
    input$gb_action_, {
      plot(gbmodel())
        # theme(plot.title = element_text(size=12, face="bold"),
        #       axis.title = element_text(face="bold"),
        #       axis.text = element_text(face="bold"))

    }
  )  
  
  output$gb_modelplot_ <-
    renderPlot({
      gb_modelplot()
    })
  
  
  observeEvent(input$gb_resamplingmethod_, {
    if (input$gb_resamplingmethod_ == "gb_bootstrap") {
      shinyjs::hide("gb_modelplottab")
      shinyjs::hide("gb_besttuneplottab")
      
    }

    else {
      shinyjs::show("gb_modelplottab")
      shinyjs::show("gb_besttuneplottab")    
      }
  })
  
  #### Variable Importance
  gb_varimp_plot <- reactive(
    if(input$gb_action_) {
      vip::vip(gbmodel(), 
               num_features = input$gb_varimp_varcount, 
               geom = "col",
               jitter = TRUE,
               aesthetics = list(color = "grey40"))
    }
  )  
  
  output$gb_varimp_plot_ <-
    renderPlot({
      gb_varimp_plot()
    })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
