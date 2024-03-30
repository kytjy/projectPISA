##########################################################################################
# projectPISA App
# ISSS608 G2
# App File
###########################################################################################


#==============================#
#~~~~~ Importing Packages~~~~~#
#==============================#

pacman::p_load("shiny", "fresh", "shinydashboard", "shinydashboardPlus", "shinyWidgets", "shinythemes", "shinyjs", "waiter", 
               "tidyverse", "DT", "kableExtra", "plotly", "scales", "gt", "ggpubr", "treemap", "ggridges",
               "ggstatsplot", "nortest",
               "ranger", "rpart", "vip", "visNetwork", "sparkline",
               "caret", "tidymodels", "gbm",
               "dendextend", "heatmaply",
               "parallelPlot", "poLCA", "rlang"
)

#==============================#
#~~~~~ Data Manipulation ~~~~~#
#==============================#

# Loading files
stu <- read_rds("data/stu_SG_rcd.rds")
varlist <- read_csv("data/var.csv")
lcalabels <- read_csv("data/lcalabels.csv")

# Dashboard - Summary Statistics
Math <- round(summary(stu$Math),1)
Reading <- round(summary(stu$Reading),1)
Science <- round(summary(stu$Science),1)

db_summarystats <- as.data.frame(rbind(Math, Reading, Science))

# DB Static Histogram
db_hist <- stu %>%  
  dplyr::select(Math, Reading, Science) %>% 
  pivot_longer(cols = Math:Science,
               names_to = "Subject",
               values_to = "Score")

# CDA - Association Test
assoctest_var <- c("School Type" = "SchoolType",
                   "Loneliness" = "Loneliness",
                   "Classroom Safety" = "ClassroomSafety",
                   "Teacher Support" = "TeacherSupport",
                   "Gender" = "Gender",
                   "Math Homework Time" = "Homework_Math",
                   "Reading Homework Time" = "Homework_Reading",
                   "Science Homework Time" = "Homework_Science",
                   "Preference for Math" = "Preference_Math",
                   "Preference for Reading" = "Preference_Reading",
                   "Preference for Science" = "Preference_Science",
                   "Exercise" = "Exercise",
                   "Parents' Education" = "ParentsEducation",
                   "Immigration" = "Immigration",
                   "Home Language" = "HomeLanguage",
                   "Sibling" = "Sibling",
                   "Aircon" = "Aircon",
                   "Helper" = "Helper",
                   "Vehicle" = "Vehicle",
                   "Books" = "Books",
                   "Own Room" = "OwnRoom",
                   "Family Commitment" = "FamilyCommitment")

# Regression Model - Data Selection
stu_mb <- stu %>% 
  dplyr::select(2:26) %>% 
  na.omit()

# Cluster Analysis - Data Manipulation
stu_ca <- stu

stu_ca_less <- stu_ca %>% 
  dplyr::select(2:26) %>% 
  na.omit()

# Preparation for Sankey Plot
median_math <- median(stu_ca_less$Math)
median_reading <- median(stu_ca_less$Reading)
median_science <- median(stu_ca_less$Science)

stu_sankey <- stu_ca %>%
  mutate(Math = ifelse(Math >= median_math, "Median and Above", "Below Median")) %>%
  mutate(Reading = ifelse(Reading >= median_reading, "Median and Above", "Below Median")) %>%
  mutate(Science = ifelse(Science >= median_science, "Median and Above", "Below Median"))

stu_sankey[-1] <- lapply(stu_sankey[-1], factor)

# Recode for LCA

stu_lca <- stu_ca_less %>%
  mutate(Sibling = recode(Sibling,
                          `0` = "No",
                          `1` = "Yes",
                          `2` = "Yes",
                          `≥3` = "Yes")) %>%
  mutate(Vehicle = recode(Vehicle,
                          `0` = "No",
                          `1` = "Yes",
                          `2` = "Yes",
                          `≥3` = "Yes")) %>%
  mutate(Exercise = recode(Exercise,
                           `0` = "No",
                           `1` = "Yes",
                           `2` = "Yes",
                           `3` = "Yes",
                           `4` = "Yes",
                           `5` = "Yes",
                           `6` = "Yes",
                           `7` = "Yes",
                           `8` = "Yes",
                           `9` = "Yes",
                           `10` = "Yes")) %>%
  mutate(FamilyCommitment = recode(FamilyCommitment,
                                   `0` = "No",
                                   `1` = "Yes",
                                   `2` = "Yes",
                                   `3` = "Yes",
                                   `4` = "Yes",
                                   `5` = "Yes",
                                   `6` = "Yes",
                                   `7` = "Yes",
                                   `8` = "Yes",
                                   `9` = "Yes",
                                   `10` = "Yes")) %>%
  mutate(Math = ifelse(Math >= median_math, "Median and Above", "Below Median")) %>%
  mutate(Reading = ifelse(Reading >= median_reading, "Median and Above", "Below Median")) %>%
  mutate(Science = ifelse(Science >= median_science, "Median and Above", "Below Median"))

stu_lca[-1] <- lapply(stu_lca[-1], factor)

#========================#
#~~~~~ Theme ~~~~~#
#========================#

mycolours <-  c("#C7C2D0","#b8c3c2","#e2d9c5", "#e1bbb0", "#fed8a6","#d8dcce", "#E3C2CB", "#98B0B6", "#e9d7c0", "#ABA58D", 
                "#ABC8CE", "#EDCFC0", "#FEF3D2", "#E4D5E3", "#C3D3D6", "#FFD4D4","#C6B49D", "#D5DBE4","#D4CBB3", "#FFDECD",
                "#D6D6D6", "#FCE5C0", "#C8C681")


# Create dashboard theme with bootstrap/fresh ----------------------------------------------------
mytheme <- create_theme(
  theme = "default",
  adminlte_color(
    light_blue = "#2A2A21",
    purple = "#B2ABB4",
    green = "#b1bdb5",
    yellow = "#EFE095",
    blue = "#4E7880",
    aqua = "#4E7880",
    red = "#b3907A"
  ),
  adminlte_sidebar(
    width = "180px",
    dark_bg = "#f5f5eb",
    dark_hover_bg = "#DDAFA1",
    dark_color = "#2A2A21",
    dark_hover_color = "#d6ac5e",
    dark_submenu_bg = "#DDAFA1",
    dark_submenu_color = "#ffffff",
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
    src="Logo2.PNG",
    height = '40'))


#========================#
###### Shiny UI ######
#========================#

# Dashboard Header ----------------------------------------------------

## Application title
header <- dashboardHeader(
  tags$li(class = "dropdown",
          tags$style(".main-header {max-height: 40px; font-size: 10px;}"),
          tags$style(".main-header .logo {height: 40px; margin-top: 0.5em; margin-bottom: 0em; font-size: 10px;}"),
          tags$style(".sidebar-toggle {height: 40px;margin-top: 0.5em; margin-bottom: 0em; font-size: 10px;}"),
          tags$style(".navbar {min-height:20px !important}")
  ),
  title = div(pplogo,
              style = "position: relative; margin:0px 0px 0px 0px; display:right-align;"),
  titleWidth = 40)

## Dashboard Sidebar ----------------------------------------------------
sidebar <- dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 50px; padding-right: 0px; margin-left: 0px; margin-right: 0px; font-size: 10px;}"),
  width = 40,
  minified = TRUE,
  sidebarMenu(
    id = "tabs",
    menuItem("Home", tabName = "tab_home", icon = icon("house")),
    menuItem("Data Exploration", tabName = "tab_eda", 
             icon = icon("binoculars"),
             startExpanded = FALSE,
             menuSubItem("Variable Overview", tabName = "tab_cdaoverview"),
             menuSubItem("Sankey Diagram", tabName = "tab_parallelplot")
    ),    
    menuItem("Confirmatory Analysis", tabName = "tab_cda", 
             icon = icon("calculator"),
             startExpanded = FALSE,
             menuSubItem("Analysis of Scores", tabName = "tab_cdascores"),
             menuSubItem("Analysis of Predictors", tabName = "tab_cdapredictor")
    ),
    
    menuItem("Cluster Analysis", tabName = "tab_cluster", 
             icon = icon("users-viewfinder"),
             startExpanded = FALSE,
             menuSubItem("Association between Predictors", tabName = "tab_cdaassoc"),
             #menuSubItem("Hierarchical Clustering", tabName = "tab_heatmap"),
             menuSubItem("Latent Class Analysis", tabName = "tab_lca")
    ),
    
    menuItem("Regression Analysis", tabName = "tab_mod", 
             icon = icon("tree"),
             startExpanded = FALSE,
             menuSubItem("Regression Tree", tabName = "tab_dt"),             
             menuSubItem("Random Forest", tabName = "tab_rf"),
             menuSubItem("Gradient Boosting", tabName = "tab_gb")
    ),
    br(),
    conditionalPanel(
      condition = "input.tabs == 'tab_dt' || input.tabs == 'tab_rf' || input.tabs == 'tab_gb'",
      div(style = "padding = 0em; margin-left: 1em; margin-right: 0em; height: 100% ",
          fluidRow(
            box(
              title = tags$p(span("Step 1: Construct Model"), style = "font-weight: bold; color: #FFFFFF"),
              status = "primary",
              solidHeader = TRUE,
              width = 11,
              chooseSliderSkin("Flat"),
              pickerInput(
                inputId = "mb_target_",
                label = "Subject",
                choices = c("Math", "Reading", "Science"), 
                selected = "Math",
                multiple = FALSE,
                options = list(style = "myClass"), #list(`actions-box` = TRUE),
                choicesOpt = list(style = rep_len("font-size: 10px; background: #FFFFFF;", 3)),
                inline = FALSE,
                width = NULL),
              pickerInput(
                inputId = "mb_vars_",
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
                choicesOpt = list(style = rep_len("font-size: 10px; background: #FFFFFF;", 22)),
                inline = FALSE,
                width = NULL),
              sliderInput(inputId = "mb_partition_",
                          label = "Train-Test Partition:",
                          min = 0.05,
                          max = 0.95,
                          value = c(0.8)),
              div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                  align = "center",
                  tags$p(span("Continue to Step 2", icon("angles-right")), style = "font-weight: bold; color: #000000;"))
            )
          )
      )
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
        
        .selectize-input { font-size: 10px;}
        .selectize-dropdown { font-size: 10px;}
        
        
        .box {font-size: 10px;
              color: #000000;
              margin: 5px;}
        
        .dropdown-item:hover {
         color: #000000 ;
         background-color: #DDAFA1 !important;
        }
        
        .optgroup-header {
                font-size: 10px !important;
        }

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
        
        html.shiny-busy .container-fluid {
        cursor: wait;}
        
                              '))
  ),
  
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
                   div(style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
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
                       )
                       
                   ),
                   div(style = "padding = 0em; margin-left: -2em; margin-right: 0em; height: 100% ",
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
                   div(style = "padding = 0em; margin-left: 0em; margin-top: 0em; height: 100%; ",
                       fluidRow(
                         valueBoxOutput("home_studnum_", width = 6),
                         valueBoxOutput("home_schnum_", width = 6)
                       ),
                       fluidRow(
                         valueBoxOutput("home_malestudnum_", width = 3),
                         valueBoxOutput("home_femalestudnum_", width = 3),
                         valueBoxOutput("home_publicschnum_", width = 3),
                         valueBoxOutput("home_privateschnum_", width = 3)
                         
                       ),
                       div(style = "padding = 0em; margin-left: 0em; margin-right: 0em",
                           fluidRow(
                             plotOutput("db_hist_static",
                                        width = "100%")
                             
                           )
                       )
                   )
            )
    ),
    
    ### Data Analysis - Variable Overview Tab ----------------------------------------------------
    tabItem(tabName = "tab_cdaoverview",
            column(width = 12,
                   div(style = "padding = 0em; margin-left: -2em; margin-right: 0em",
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
                                                     height = "60vh",
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
                                        plotOutput("db_treemap",
                                                   height = "70vh",
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
                                                     height = "70vh",
                                                     width = "90%"),
                                        align = "center")
                           )
                         )
                       )
                   )
            )
    ),
    
    ### Data Analysis - One-Sample Test ----------------------------------------------------
    tabItem(tabName = "tab_cdascores",
            
            #### CDA Tab's Side Toggles ----------------------------------------------------
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                         box(title = tags$p(span(icon("magnifying-glass"), "Analysis of Scores"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "The Analysis of Scores section evaluates whether a user-defined value significantly deviates from the population mean. Users can input scores from different studies within the PISA dataset or even the PISA average itself. This enables a statistical test to identify any notable differences from the population mean.", 
                                 tags$br(), tags$br(),
                                 "Central tendency varies with different types of tests: mean for parametric statistics, median for non-parametric statistics, trimmed mean for robust statistics, MAP estimator for Bayesian statistics.",
                                 tags$br(), tags$br(),
                                 "The Test for Normality section enables users to visually and statistically assess if the distribution conforms to a normal distribution. This helps users select the appropriate test type."
                             )
                         )
                     ),
                     
                     
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 1.5em; height: 100% ",
                         box(title = tags$p("Step 1: Subject Selection & Setting Test Score", style = "font-weight: bold;"),
                             status = "primary",
                             collapsed = FALSE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "cda_testscorehist_subject",
                                   label = "Subject",
                                   choices = c("Math", "Reading", "Science"), 
                                   selected = "Math",
                                   multiple = FALSE,
                                   options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 numericInput(
                                   inputId = "cda_testscorehist_ts",
                                   label = "Test Score:",
                                   value = 550,
                                   min = 100,
                                   max = 900,
                                   step = 1)
                             )
                         )
                     ),
                     div(
                       style = "padding = 0em; margin-left: -2em; margin-top: 1.5em; height: 100% ",
                       box(title = tags$p("Step 2: Configurations", style = "font-weight: bold;"),
                           collapsed = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,  
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               numericInput(
                                 inputId = "cda_testscorehist_cl", 
                                 label = "Confidence Level",
                                 min = 0.01,
                                 max = 0.99,
                                 step = 0.01,
                                 value = 0.95,
                                 width = NULL
                               )),
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               sliderInput(
                                 inputId = "cda_testscorehist_binwidth",
                                 label = "Bin Width",
                                 min = 1,
                                 max = 50,
                                 value =20,
                                 width = NULL
                               )),
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               pickerInput(
                                 inputId = "cda_testscorehist_testtype", 
                                 label = "Test Type",
                                 choices = list("Parametric" = "p", 
                                                "Non-parametric" = "np", 
                                                "Robust" = "robust", 
                                                "Bayes Factor" = "bf"), 
                                 selected = "p",
                                 multiple = FALSE,
                                 options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                 choicesOpt = list(style = rep_len("font-size: 10px;", 4)),
                                 inline = FALSE,
                                 width = NULL
                               )),
                           hidden(
                             div(
                               id = "cda_testscorehist_ifparametric",
                               style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               pickerInput(
                                 inputId = "cda_effsizetype",
                                 label = "Type of effect size",
                                 choices = list("Cohen's d" = "d", 
                                                "Hedge's g" = "g"), 
                                 selected = "g",
                                 multiple = FALSE,
                                 options = list(style = "myClass"),
                                 choicesOpt = list(style = rep_len("font-size: 10px;", 2)),
                                 inline = FALSE,
                                 width = NULL)
                             )
                           ),
                           div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                               actionButton(inputId = "action_cda_testscorehist",
                                            label = "Run Analysis",
                                            icon = icon("gear"),
                                            style = 'padding: 4px; font-size: 10px'),
                               align = "center")
                       )
                     )
                   )
            ),
            column(width = 10,
                   fluidRow(div(style = "padding = 0em; margin-left: 0em; margin-top: 0em; height: 100% ",
                                box(title = tags$p("Results", style = "font-weight: bold;"),
                                    status = "primary",
                                    collapsed = FALSE,
                                    width = 12,
                                    div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                                        plotOutput("cda_testscorehist_",
                                                   height = "35vh",
                                                   width = "90%"),
                                        align = "center")
                                )
                   )
                   ),
                   fluidRow(
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 0em; height: 100% ",
                         box(title = tags$p("Test for Normality", style = "font-weight: bold;"),
                             status = "primary",
                             collapsed = FALSE,
                             width = 12,
                             box(title = tags$p("QQ Plot", style = "font-weight: bold"),
                                 width = 6,
                                 plotOutput("cda_testscoreqqplot_",
                                            height = "25vh")),
                             box(title = tags$p("Anderson-Darling Test", style = "font-weight: bold;"),
                                 width = 6,
                                 valueBoxOutput("cda_adtest_stat", width = 12),
                                 valueBoxOutput("cda_adtest_pvalue", width = 12)
                             )
                         )
                     )
                   )
            )
    ),
    ### Data Analysis - ANOVA/2-Sample Test ----------------------------------------------------
    tabItem(tabName = "tab_cdapredictor",
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                         box(title = tags$p(span(icon("magnifying-glass"), "Analysis of Predictors"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "An ANOVA test is a statistical method utilized to ascertain whether there exists a statistically significant distinction among two or more categorical groups by examining differences in means using variance.",
                                 tags$br(), tags$br(),
                                 "Within this section, an ANOVA test will be executed to evaluate the presence of a significant difference in subject scores across diverse categories of a single variable. This test scrutinizes the means of multiple groups to determine if the variance between these groups surpasses that within them, thereby indicating a noteworthy difference in scores across the categories."
                             )
                         )
                     ),
                     
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 1.5em; height: 100% ",
                         box(title = tags$p("Step 1: Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = FALSE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "cda_anova_subject",
                                   label = "Subject",
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
                                   inputId = "cda_anova_var",
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
                                   selected = "SchoolType",
                                   multiple = FALSE,
                                   options = list(style = "myClass"),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                                   inline = FALSE,
                                   width = NULL
                                 ))
                             
                         )
                     ),
                     div(
                       style = "padding = 0em; margin-left: -2em; margin-top: 1.5em; height: 100% ",
                       box(title = tags$p("Step 2: Configurations", style = "font-weight: bold;"),
                           collapsed = TRUE,
                           collapsible =TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,  
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               numericInput(
                                 inputId = "cda_anova_cl", 
                                 label = "Confidence Level",
                                 min = 0.01,
                                 max = 0.99,
                                 step = 0.01,
                                 value = 0.95,
                                 width = NULL
                               )),
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               pickerInput(
                                 inputId = "cda_anova_testtype", 
                                 label = "Test Type",
                                 choices = list("Parametric" = "p", 
                                                "Non-parametric" = "np", 
                                                "Robust" = "robust", 
                                                "Bayes Factor" = "bf"), 
                                 selected = "p",
                                 multiple = FALSE,
                                 options = list(style = "myClass"),
                                 choicesOpt = list(style = rep_len("font-size: 10px;", 4)),
                                 inline = FALSE,
                                 width = NULL
                               )),
                           # div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                           #     pickerInput(
                           #       inputId = "cda_anova_varequal", 
                           #       label = "Equal Variance?",
                           #       choices = list("Yes" = TRUE, 
                           #                      "No" = FALSE), 
                           #       selected = FALSE,
                           #       multiple = FALSE,
                           #       options = list(style = "myClass"),
                           #       choicesOpt = list(style = rep_len("font-size: 10px;", 2)),
                           #       inline = FALSE,
                           #       width = NULL
                           #     )),
                           div(style = "padding = 0em; margin-top: 0em",
                               materialSwitch(inputId = "cda_anova_pwcompare",
                                              label = "Show Pairwise Comparsions?",
                                              value = FALSE,
                                              status = "warning")
                           ),
                           hidden(div(id = "cda_pwdisplaypanel",
                                      style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                      pickerInput(
                                        inputId = "cda_pwdisplay", 
                                        label = "Pairwise Display Type",
                                        choices = list("Show Significant Pairs" = "s", 
                                                       "Show Non-Significant Pairs" = "ns", 
                                                       "All" = "a"), 
                                        selected = "s",
                                        multiple = FALSE,
                                        options = list(style = "myClass"),
                                        choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                        inline = FALSE,
                                        width = NULL)
                           )
                           ),
                           div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                               actionButton(inputId = "action_cda_anova",
                                            label = "Run Analysis",
                                            icon = icon("gear"),
                                            style = 'padding: 4px; font-size: 10px'),
                               align = "center")
                       )
                     )
                   )
            ),
            column(width = 10,
                   fluidRow(div(style = "padding = 0em; margin-left: 0em; margin-top: 0em; height: 100% ",
                                box(title = tags$p("Results", style = "font-weight: bold;"),
                                    status = "primary",
                                    collapsed = FALSE,
                                    width = 12,
                                    div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                        plotOutput("cda_anova_plot",
                                                   height = "80vh",
                                                   width = "100%"),
                                        align = "center")
                                )
                   )
                   )
            )
    ),
    ### Data Analysis - Association Test ----------------------------------------------------
    tabItem(tabName = "tab_cdaassoc",
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                         box(title = tags$p(span(icon("users-viewfinder"), "Association Between Predictors"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "A test of association is a hypothesis test designed to establish and quantify the relationship between two distinct factors. 
                                 It can also be utilized to determine whether the observed difference in any two categorical variables within our dataset is due to chance or indicates an actual relationship between them.",
                                 tags$br(), tags$br(),
                                 "When dealing with categorical variables, multicollinearity can be identified using the chi-square test. This test evaluates the association between two categorical variables. 
                                 If the chi-square test reveals a significant association between two nominal predictors, it may indicate the presence of multicollinearity. 
                                 Multicollinearity can result in less reliable statistical inferences and may lead to skewed or misleading results."
                             )
                         )
                     ),
                     
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 1.5em; height: 100% ",
                         box(title = tags$p("Step 1: Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = FALSE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 selectizeInput(
                                   inputId = "cda_assoc_var1",
                                   label = "Select y-axis Variable ",
                                   choices = assoctest_var,
                                   selected = NULL,
                                   multiple = FALSE,
                                   options = list(maxItems = 1),
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 selectizeInput(
                                   inputId = "cda_assoc_var2",
                                   label = "Select x-axis Variable",
                                   choices = NULL,
                                   selected = NULL,
                                   multiple = FALSE,
                                   options = list(maxItems = 1),
                                   width = NULL
                                 ))
                             
                         )
                     ),
                     div(
                       style = "padding = 0em; margin-left: -2em; margin-top: 1.5em; height: 100% ",
                       box(title = tags$p("Step 2: Configurations", style = "font-weight: bold;"),
                           collapsed = TRUE,
                           collapsible =TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,  
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               numericInput(
                                 inputId = "cda_assoc_cl", 
                                 label = "Confidence Level",
                                 min = 0.01,
                                 max = 0.99,
                                 step = 0.01,
                                 value = 0.95,
                                 width = NULL
                               )),
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               pickerInput(
                                 inputId = "cda_assoc_testtype", 
                                 label = "Test Type",
                                 choices = list("Parametric" = "p", 
                                                "Non-parametric" = "np", 
                                                "Robust" = "robust", 
                                                "Bayes Factor" = "bf"), 
                                 selected = "p",
                                 multiple = FALSE,
                                 options = list(style = "myClass"),
                                 choicesOpt = list(style = rep_len("font-size: 10px;", 4)),
                                 inline = FALSE,
                                 width = NULL
                               )),
                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                               pickerInput(
                                 inputId = "cda_assoc_labeltype", 
                                 label = "Label Type Displayed",
                                 choices = list("Percentage" = "percentage", 
                                                "Count" = "counts", 
                                                "Both" = "both"), 
                                 selected = "percentage",
                                 multiple = FALSE,
                                 options = list(style = "myClass"),
                                 choicesOpt = list(style = rep_len("font-size: 10px;", 4)),
                                 inline = FALSE,
                                 width = NULL
                               )),
                           div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                               actionButton(inputId = "action_cda_assoc",
                                            label = "Run Analysis",
                                            icon = icon("gear"),
                                            style = 'padding: 4px; font-size: 10px'),
                               align = "center")
                       )
                     )
                   )
            ),
            column(width = 10,
                   fluidRow(div(style = "padding = 0em; margin-left: 0em; margin-top: 0em; height: 100% ",
                                box(title = tags$p("Results", style = "font-weight: bold;"),
                                    status = "primary",
                                    collapsed = FALSE,
                                    width = 12,
                                    div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                        plotOutput("cda_assoc_plot",
                                                   height = "80vh",
                                                   width = "90%"),
                                        align = "center")
                                )
                   )
                   )
            )
    ),    
    
 
    ### Parallel Plot  ----------------------------------------------------
    tabItem(tabName = "tab_parallelplot",
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                         box(title = tags$p(span(icon("users-viewfinder"), "Sankey Diagram"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             solidHeader = TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "A Sankey Diagram is used to illustrate the flow of data between different categories or clusters, with the line thickness corresponding to the frequencies of the flows."
                             )
                         )
                     ),
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 1.5em; height: 100% ",
                         box(title = tags$p("Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "scores_sankey",
                                   label = "Scores",
                                   choices = c("Math", "Reading", "Science"),
                                   multiple = FALSE,
                                   options = list(style = "myClass"),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL)
                             ),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "vars_sankey",
                                   label = "Variables (Choose 4)",
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
                                   multiple = TRUE,
                                   options = list(style = "myClass", "max-options" = 4),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                                   inline = FALSE,
                                   width = NULL)
                             ),
                             div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                                 actionButton(inputId = "action_sankey_vars",
                                              label = "Confirm Selection",
                                              icon = icon("filter"),
                                              style = 'padding: 4px; font-size: 10px'),
                                 align = "center")
                         )
                     )
                   )
            ),
            column(width = 10,
                   div(style = "padding = 0em; margin-left: -1em; margin-top: 0em; height: 100%",
                       box(title = tags$p("Plot", style = "font-weight: bold;"), 
                           collapsible = FALSE,
                           status = "primary",
                           width = 12,
                           div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                               plotlyOutput("sankey",
                                            height = "80vh",
                                            width = "90%"),
                               align = "center")
                       )
                   )
            )
    ),
    
    ### Latent Class Analysis  ----------------------------------------------------
    tabItem(tabName = "tab_lca",
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                         box(title = tags$p(span(icon("users-viewfinder"), "Latent Class Analysis"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "A Latent Class Analaysis (LCA) Bar Plot is used to show the distribution of categorical variables within each latent class. LCA is a statistical method used to identify unobserved (latent) classes within a population based on observed categorical variables. Each observation is probabilistically assigned to one of the latent classes. The bar plot helps to identify which categories are most prevalent or distinctive within each class."
                             )
                         )
                     ),
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 1.5em; height: 100% ",
                         box(title = tags$p("Step 1: Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "scores_lca",
                                   label = "Scores",
                                   choices = c("Math", "Reading", "Science"),
                                   multiple = TRUE,
                                   options = list(style = "myClass", `actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),                                 
                                   inline = FALSE,
                                   width = NULL)),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "vars_lca",
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
                                   multiple = TRUE,
                                   options = list(style = "myClass", `actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 22)),                                 
                                   inline = FALSE,
                                   width = NULL)),
                             div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                                 actionButton(inputId = "action_lca_vars",
                                              label = "Confirm Selection",
                                              icon = icon("filter"),
                                              style = 'padding: 4px; font-size: 10px'),
                                 align = "center")
                         ),
                         box(title = tags$p("Step 2: Clustering Parameters", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 sliderInput(inputId = "numclusters_lca", 
                                             label = "No. of Clusters:",
                                             min = 1, 
                                             max = 15, 
                                             value = 5,
                                             step = 1),
                                 sliderInput(inputId = "numrep_lca", 
                                             label = "No. of Repetitions:",
                                             min = 1, 
                                             max = 5, 
                                             value = 3,
                                             step = 1)
                             )
                         ),
                         box(title = tags$p("Step 3: Variable Distribution by Class", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 selectInput(inputId = "groupby_lca", 
                                             label = "Variable to Plot",
                                             choices = " ")),
                             div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                                 actionButton(inputId = "action_lca",
                                              label = "Run Analysis",
                                              icon = icon("gear"),
                                              style = 'padding: 4px; font-size: 10px'),
                                 align = "center")
                         )
                     )
                   )
            ),
            column(width = 10,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 0em; height: 100%",
                         tabBox(title = tags$p("Latent Class Analysis", style = "color: #7C6D62; font-weight: bold; font-size: 12px;"), 
                                width = 12,
                                height = "60vh",
                                tabPanel(title = tags$p("Cluster Proportions", style = "font-weight: bold"),
                                         div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                                             plotlyOutput("lca_propplot_",
                                                          width = "90%",
                                                          height = "50vh"),
                                             align = "center")
                                ),
                                tabPanel(title = tags$p("Variable View", style = "font-weight: bold"),
                                         div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                                             plotlyOutput("lca",
                                                          width = "90%",
                                                          height = "50vh"),
                                             align = "center")
                                ),
                                tabPanel(title = tags$p("Summary View", style = "font-weight: bold"),
                                         div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                                             plotlyOutput("lca_plotsummary_",
                                                          width = "90%",
                                                          height = "50vh"),
                                             align = "center")
                                )
                         )
                     ),
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 0.5em; height: 100%",
                         box(title = tags$p("Supplementary Information", style = "font-weight: bold"), 
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             fluidRow(
                               column(width = 6, 
                                      uiOutput("active_lca_supp1"),
                                      flipBox(id = "fb_lca_supp1",
                                              width = 12,
                                              front = div(#h6("Click for More Information", align = "center"),
                                                          valueBoxOutput("lca_supp1", width =12)),
                                              back = wellPanel(
                                                p("Akaike Information Criterion (AIC) is a measure of the relative quality of a statistical model for a given set of data. It balances the goodness of fit of the model with the complexity of the model, penalising models that are overly complex. Lower AIC values indicate a better balance between model fit and complexity, suggesting a more optimal model for explaining the data.")))),
                               column(width = 6, 
                                      uiOutput("active_lca_supp2"),
                                      flipBox(id = "fb_lca_supp2",
                                              width = 12,
                                              front = div(#h6("Click for More Information", align = "center"),
                                                          valueBoxOutput("lca_supp2", width =12)),
                                              back = wellPanel(
                                                p("Bayesian Information Criterion (BIC) is similar to AIC but places a stronger penalty on models with more parameters, making it more conservative in selecting models. Like AIC, lower BIC values indicate better model fit, but BIC tends to favor simpler models more strongly than AIC."))))),
                             fluidRow(
                               column(width = 6, 
                                      uiOutput("active_lca_supp3"),
                                      flipBox(id = "fb_lca_supp3",
                                              width = 12,
                                              front = div(#h6("Click for More Information", align = "center"),
                                                          valueBoxOutput("lca_supp3", width =12)),
                                              back = wellPanel(
                                                p("Likelihood Ratio/Deviance Statistic (Gsq) is a measure of the goodness of fit of the model. It compares the fit of the model to a model with perfect fit by assessing the difference in deviance between the two models. Lower values indicate better model fit, with values close to zero suggesting that the model fits the data well.")))),
                               column(width = 6, 
                                      uiOutput("active_lca_supp4"),
                                      flipBox(id = "fb_lca_supp4",
                                              width = 12,
                                              front = div(#h6("Click for More Information", align = "center"),
                                                          valueBoxOutput("lca_supp4", width =12)),
                                              back = wellPanel(
                                                p("Entropy quantifies the uncertainty or randomness in the distribution of observations across different classes. A high entropy indicates greater uncertainty, while low entropy suggests more predictability or orderliness.Higher entropy values indicate less distinct class separation, while lower entropy values suggest clearer class distinctions."))))
                               
                             )
                         )
                     )
                   )
            )
    ),
    
    ### Regression tree  ----------------------------------------------------
    tabItem(tabName = "tab_dt",
            
            #### Regression tree Toggle Column  ----------------------------------------------------
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                         box(title = tags$p(span(icon("tree"), "Regression Tree"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "A Regression Tree is a machine learning algorithm that partitions the data into subsets. The partitioning process starts with a binary split and continues until no further splits can be made. Various branches of variable length are formed. The goal of a regression tree is to encapsulate the training data in the smallest possible tree, i.e. simplest possible explanation for the variation in scores."
                             )
                         )
                     ),
                   
                     # div(style = "padding = 0em; margin-left: 0em; margin-top: 0em; height: 100% ",
                     #     box(title = tags$p("Step 1: Variable Selection", style = "font-weight: bold;"),
                     #         status = "primary",
                     #         collapsible = TRUE,
                     #         collapsed = FALSE,
                     #         width = 12,
                     #         div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                     #             pickerInput(
                     #               inputId = "mb_target_",
                     #               label = "Subject",
                     #               choices = c("Math", "Reading", "Science"), 
                     #               selected = "Math",
                     #               multiple = FALSE,
                     #               options = list(style = "myClass"), #list(`actions-box` = TRUE),
                     #               choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                     #               inline = FALSE,
                     #               width = NULL
                     #             )),
                     #         div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                     #             pickerInput(
                     #               inputId = "mb_vars_",
                     #               label = "Variables",
                     #               choices = list(
                     #                 `School Environment` = list("School Type" = "SchoolType",
                     #                                             "Loneliness" = "Loneliness",
                     #                                             "Classroom Safety" = "ClassroomSafety",
                     #                                             "Teacher Support" = "TeacherSupport"),
                     #                 `Personal` = list("Gender" = "Gender",
                     #                                   "Math Homework Time" = "Homework_Math",
                     #                                   "Reading Homework Time" = "Homework_Reading",
                     #                                   "Science Homework Time" = "Homework_Science",
                     #                                   "Preference for Math" = "Preference_Math",
                     #                                   "Preference for Reading" = "Preference_Reading",
                     #                                   "Preference for Science" = "Preference_Science",
                     #                                   "Exercise" = "Exercise"),
                     #                 `Socioeconomic` = list("Parents' Education" = "ParentsEducation",
                     #                                        "Immigration" = "Immigration",
                     #                                        "Home Language" = "HomeLanguage",
                     #                                        "Sibling" = "Sibling",
                     #                                        "Aircon" = "Aircon",
                     #                                        "Helper" = "Helper",
                     #                                        "Vehicle" = "Vehicle",
                     #                                        "Books" = "Books",
                     #                                        "Own Room" = "OwnRoom",
                     #                                        "Family Commitment" = "FamilyCommitment")),
                     #               selected = colnames(stu_mb[1:24]),
                     #               multiple = TRUE,
                     #               options = list(style = "myClass", `actions-box` = TRUE),
                     #               choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                     #               inline = FALSE,
                     #               width = NULL
                     #             ))
                     #     )
                     # ),
                     # div(
                     #   style = "padding = 0em; margin-left: 0em; margin-top: 1.5em; height: 100% ",
                     #   box(title = tags$p("Step 2: Data Splitting", style = "font-weight: bold;"),
                     #       collapsible = TRUE,
                     #       collapsed = TRUE,
                     #       width = 12,
                     #       status = "primary",
                     #       solidHeader = FALSE,
                     #       div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                     #           chooseSliderSkin("Flat"),
                     #           sliderInput(inputId = "mb_partition_",
                     #                       label = "Train-Test Partition:",
                     #                       min = 0.05,
                     #                       max = 0.95,
                     #                       value = c(0.8)
                     #           )),
                     #       div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                     #           actionButton(inputId = "dt_action_",
                     #                        label = "Run Analysis",
                     #                        icon = icon("wrench"),
                     #                        style = 'padding: 4px; font-size: 10px'),
                     #           align = "center"
                     #       )                      
                     #   )
                     # ),

                     div(
                       style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                       box(title = tags$p("Step 2: Model Initiation", style = "font-weight: bold;"),
                           collapsible = TRUE,
                           collapsed = TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,
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
                     hidden(div(id = "dt_modeltune_group",
                                style = "padding = 0em; margin-left: -2em; margin-top: -1em",
                                box(
                                  title = tags$p("Step 3: Tuning Parameters", style = "font-weight: bold;"),
                                  status = "primary",
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  width = 12,
                                  div(style = "padding = 0em; margin-top: -0.5em",
                                      sliderInput(inputId = "dt_minsplit",
                                                  label = "Minimum Split:",
                                                  min = 5,
                                                  max = 20,
                                                  step = 1,
                                                  value = 5),
                                      div(style = "padding = 0em; margin-top: -0.8em",
                                          sliderInput(inputId = "dt_maxdepth",
                                                      label = "Maximum Depth:",
                                                      min = 5,
                                                      max = 20,
                                                      step = 1,
                                                      value = 10)
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
                                                         value = 0.001)
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
            
            #### Regression Tree Results Column 1 ----------------------------------------------------
            
            column(width = 5,
                   fluidRow(
                     div(
                       style = "padding = 0em; margin-left: -1em; margin-top: 0em; height: 100% ",
                       box(title = tags$p("Fit Assessment", style = "font-weight: bold;"),
                           closable = FALSE,
                           width = 12,
                           status = "primary",
                           
                           #### Regression Tree - Actual vs Predicted ----------------------------------------------------
                           column(width = 6, 
                                  plotOutput("dt_plot_predvsactual_",
                                             width = "100%",
                                             height = "30vh")),
                           
                           
                           #### Regression Tree - Actual vs Residuals ----------------------------------------------------
                           column(width = 6, 
                                  plotOutput("dt_plot_residvsactual_",
                                             width = "100%",
                                             height = "30vh"))
                           
                       )
                     )
                   )
                   ,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: -1em; margin-top: 0em; height: 100% ",
                         #height = "100%",
                         box(title = tags$p("Complexity Parameter", style = "font-weight: bold;"),
                             closable = FALSE,
                             width = 12,
                             status = "primary",    
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 plotOutput("dt_cp_plot_",
                                            width = "90%",
                                            height = "30vh"),
                                 align = "center"),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 flipBox(id = "dt_cp_info",
                                         width = 12,
                                         front = valueBoxOutput("dt_showcp_", width = 12),
                                         back = tags$p("Complexity Parameter is used to control the size of the regression tree and to select the optimal tree size. The tree will stop dividing nodes when the reduction in relative error is less than a certain value.")
                                 )
                             )
                         )
                         # tabPanel(title = "CP Table",
                         #          height = "100%",
                         #          div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                         #              DT::dataTableOutput("dt_cptable_")
                         #              )
                         #          )
                         
                         
                     )
                   )
                   
            ),
            column(width = 5,
                   fluidRow(
                     style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                     div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                         valueBoxOutput("dt_R2_", width = 4),
                         valueBoxOutput("dt_RMSE_", width = 4),
                         valueBoxOutput("dt_MAE_", width = 4)
                     )
                   ),
                   div(style = "padding = 0em; margin-top: 0em; margin-left: -2em;",
                       tabBox(
                         #closable = FALSE,
                         width = 12,
                         #status = "primary",
                         tabPanel(
                           title = tags$p("Regression Tree", style = "color: #7C6D62; font-weight: bold; font-size: 12px;"),
                           visNetworkOutput("dt_rpartplot_",
                                            width = "100%",
                                            height = "70vh")),
                         tabPanel(
                           title = tags$p("Variable Importance", style = "color: #7C6D62; font-weight: bold; font-size: 12px;"),
                           div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                               dropdownButton(
                                 numericInput(
                                   inputId = "dt_varimp_varcount",
                                   label = "Configure number of variables to display (limited to the number of available responses):",
                                   min = 5,
                                   max = 120,
                                   value = 10,
                                   width = "290px",
                                   step = 1),
                                 circle = FALSE,
                                 right = TRUE,
                                 status = "default",
                                 icon = icon("gear"), 
                                 width = "300px"),
                               align = "right"),
                           div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                               plotOutput("dt_varimp_plot_",
                                          height = "62vh",
                                          width = "100%")
                           )
                         )
                         
                       )
                   )
            )
    ),
    
    ### Random Forest  ----------------------------------------------------
    tabItem(tabName = "tab_rf",
            
            #### Random Forest Toggle Column  ----------------------------------------------------
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                         box(title = tags$p(span(icon("tree"), "Random Forest"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "A Random Forest is like a group decision-making team in machine learning. It combines the opinions of many “trees” (individual models) to make better predictions, creating a more robust and accurate overall model. This randomness introduces variability among individual trees, reducing the risk of overfitting and improving overall prediction performance."
                             )
                         )
                     ),
                     
                     div(
                         style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                         box(id = "rf_modeltune_group", 
                             title = tags$p("Step 2: Tuning Parameters", style = "font-weight: bold;"),
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             status = "primary",
                             solidHeader = FALSE,
                           # div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                           #     chooseSliderSkin("Flat"),
                           #     sliderInput(inputId = "mb_partition_",
                           #                 label = "Train-Test Partition:",
                           #                 min = 0.05,
                           #                 max = 0.95,
                           #                 value = c(0.8))
                           # ),
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
                           ),
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
                           )
                       )
                     ),
                     div(
                       style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
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
                                           ))
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
                     style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                     div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                         valueBoxOutput("rf_display_R2_", width = 4),
                         valueBoxOutput("rf_display_RMSE_", width = 4),
                         valueBoxOutput("rf_display_MAE_", width = 4)
                     )
                   ),
                   
                   fluidRow(column(width = 5,
                                   div(
                                     style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                                     box(title = tags$p("Fit Assessment", style = "font-weight: bold;"),
                                         closable = FALSE,
                                         width = 12,
                                         status = "primary",
                                         
                                         #### Random Forest - Actual vs Predicted ----------------------------------------------------
                                         plotOutput("rf_plotpredvsactual_",
                                                    width = "100%",
                                                    height = "32vh"),
                                         
                                         #### Random Forest - Actual vs Residuals ----------------------------------------------------
                                         plotOutput("rf_plotresidvsactual_",
                                                    width = "100%",
                                                    height = "32vh")
                                     )
                                   )
                   ),
                   
                   column(width = 7,
                          div(style = "padding = 0em; margin-top: 0em; margin-left: -3em;",
                              box(title = tags$p("Variable Importance", style = "font-weight: bold;"),
                                  closable = FALSE,
                                  width = 12,
                                  status = "primary",
                                  div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                                      dropdownButton(
                                        numericInput(
                                          inputId = "rf_varimp_varcount",
                                          label = "Configure number of variables to display (limited to the number of available responses):",
                                          min = 5,
                                          max = 120,
                                          value = 10,
                                          width = "290px",
                                          step = 1),
                                        circle = FALSE,
                                        right = TRUE,
                                        status = "default",
                                        icon = icon("gear"), 
                                        width = "300px"),
                                      align = "right"),
                                  div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                                      plotOutput("rf_varimp_plot_",
                                                 height = "62vh",
                                                 width = "100%"))
                              )
                          )
                   )
                   )
            )
    ),
    tabItem(tabName = "tab_gb",
            
            #### Gradient Boosting Toggle Column  ----------------------------------------------------
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                         box(title = tags$p(span(icon("tree"), "Gradient Boosting"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsed = TRUE,
                             collapsible = TRUE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "Gradient Boosting is an ensemble machine learning technique that combines the predictions from several models to improve the overall predictive accuracy."
                             )
                         )
                     ),
                     
                     div(
                       style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                       box(title = tags$p("Step 2: Resampling Options", style = "font-weight: bold;"),
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
                       style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                       box(title = tags$p("Step 3: Tuning Parameters", style = "font-weight: bold;"),
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
                                                           value = c(2,3)
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
                                           style = "padding = 0em; margin-left: 0em; margin-top: 0em; height: 100% ",
                                           div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                               sliderInput(inputId = "gb_bs_interactiondepth",
                                                           label = "Max. Tree Depth: ",
                                                           min = 2,
                                                           max = 10,
                                                           step = 1,
                                                           value =2
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
            (column(width = 10,
                    fluidRow(
                      style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                      div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                          valueBoxOutput("gb_display_R2_", width = 4),
                          valueBoxOutput("gb_display_RMSE_", width = 4),
                          valueBoxOutput("gb_display_MAE_", width = 4)
                      )
                    ),
                    fluidRow(
                      column(width = 5,
                             div(style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                                 box(title = tags$p("Fit Assessment", style = "font-weight: bold;"),
                                     status = "info",
                                     collapsible = FALSE,
                                     width = 12,
                                     solidHeader =FALSE,
                                     
                                     #### Gradient Boosting Actual vs Predicted ----------------------------------------------------
                                     
                                     plotOutput("gb_plotpredvsactual_",
                                                width = "100%",
                                                height = "32vh"),
                                     
                                     
                                     plotOutput("gb_plotresidvsactual_",
                                                width = "100%",
                                                height = "32vh")
                                 )
                             ),
                             hidden(div(id = "gb_besttuneplottab", 
                                        style = "padding = 0em; margin-left: -2em; margin-top: 0em; height: 100% ",
                                        box(title = tags$p("Best Tune", style = "font-weight: bold;"),
                                            status = "info",
                                            collapsible = FALSE,
                                            width = 12,
                                            solidHeader =FALSE,
                                            tableOutput("gb_besttune_"))
                             )
                             )
                      ),
                      (column(width = 7,
                              div(style = "padding = 0em; margin-left: -3em; margin-top: 0em; height: 100% ",
                                  box(title = tags$p("Variable Importance", style = "font-weight: bold;"),
                                      status = "info",
                                      collapsible = FALSE,
                                      width = 12,
                                      solidHeader =FALSE,
                                      div(style = "padding = 0em; margin-top: 0em; margin-left: 0em;",
                                          dropdownButton(
                                            numericInput(
                                              inputId = "gb_varimp_varcount",
                                              label = "Configure number of variables to display (limited to the number of available responses):",
                                              min = 5,
                                              max = 120,
                                              value = 10,
                                              width = "290px",
                                              step = 1),
                                            circle = FALSE,
                                            right = TRUE,
                                            status = "default",
                                            icon = icon("gear"), 
                                            width = "300px"),
                                          align = "right"),
                                      div(style = "padding = 0em; margin-top: 0.5em; font-size: 10px;",
                                          plotOutput("gb_varimp_plot_",
                                                     height = "45vh",
                                                     width = "100%"))
                                  )
                              ),
                              hidden(div(id = "gb_modelplottab",
                                         style = "padding = 0em; margin-left: -3em; margin-top: 0em; height: 100% ",
                                         box(title = tags$p("Resampling Profile", style = "font-weight: bold;"),
                                             status = "info",
                                             collapsible = FALSE,
                                             width = 12,
                                             solidHeader =FALSE,
                                             plotOutput("gb_modelplot_"
                                                        #height = "100%"
                                             ))
                              )
                              )
                      )
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
                     tab_cda = "Exploratory & Confirmatory Data Analysis",
                     tab_cdascores = "Analysis of Scores",
                     tab_cdapredictor = "Analysis of Predictors",
                     tab_cdaassoc = "Association between Predictors",
                     tab_cluster = "Cluster Analysis",
                     tab_heatmap = "Country Comparison",
                     tab_cluster = "Cluster Heatmap",
                     tab_parallelplot = "Parallel Plot",
                     tab_dt = "Regression Tree",
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
  
  output$home_malestudnum_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(sum(stu$Gender == "Male"), ", ", round((sum(stu$Gender == "Male")/nrow(stu))*100,0), "%"), style = "font-size: 40%;"),
      subtitle = tags$p(paste0("Male Students"), style = "font-size: 60%;"), 
      icon = tags$i(icon("child"), style="font-size: 40%"),
      color = "yellow"
    )
  })
  
  output$home_femalestudnum_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(sum(stu$Gender == "Female"), ", ", round((sum(stu$Gender == "Female")/nrow(stu))*100,0), "%"), style = "font-size: 40%;"),
      subtitle = tags$p(paste0("Female Students"), style = "font-size: 60%;"), 
      icon = tags$i(icon("child-dress"), style="font-size: 40%"),
      color = "yellow"
    )
  })
  
  output$home_schnum_ <- renderValueBox({
    valueBox(
      value = tags$p(n_distinct(stu$SchoolID), style = "font-size: 60%;"), 
      subtitle = tags$p("Participating Schools", style = "font-size: 80%;"), 
      icon = tags$i(icon("building-columns"), style="font-size: 60%"),
      color = "yellow"
    )
  })
  
  output$home_privateschnum_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(sum(stu$SchoolType == "Private"), ", ", round((sum(stu$SchoolType == "Private")/nrow(stu))*100,0), "%"), style = "font-size: 40%;"),
      subtitle = tags$p(paste0("Private School"), style = "font-size: 60%;"), 
      icon = tags$i(icon("building-columns"), style="font-size: 40%"),
      color = "yellow"
    )
  })
  
  output$home_publicschnum_ <- renderValueBox({
    valueBox(
      value = tags$p(paste0(sum(stu$SchoolType == "Public"), ", ", round((sum(stu$SchoolType == "Public")/nrow(stu))*100,0), "%"), style = "font-size: 40%;"),
      subtitle = tags$p(paste0("Public School"), style = "font-size: 60%;"), 
      icon = tags$i(icon("building-columns"), style="font-size: 40%"),
      color = "yellow"
    )
  })
  
  summarystatstable <- reactive({
    db_summarystats[input$db_hist_subject, ]
  })
  
  
  output$db_summarystats_ <- DT::renderDataTable({
    datatable(summarystatstable(),
              options = list(dom = 't'))
  })
  
  
  
  output$db_varlist_ <- DT::renderDataTable({
    datatable(varlist,
              class = "compact",
              options = list(hover = TRUE))
  })
  
  output$db_hist_static <- renderPlot({
    ggplot(db_hist, 
           aes(x = Score, 
               y = Subject)) +
      geom_density_ridges(
        scale = 1.5,
        rel_min_height = 0.01,
        #bandwidth = 10,
        fill = "#C7C2D0",
        color = "white"
      ) +
      scale_x_continuous(
        name = "Scores",
        expand = c(0, 0)
      ) +
      scale_y_discrete(name = NULL, expand = expansion(add = c(0.2, 2.6))) +
      labs(title = "Distribution of Subject Scores")+
      theme_ridges() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
        axis.line.y = element_blank(),
        axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
        plot.title = element_text(size=16, face="bold", hjust = 0.5),
        axis.title = element_text(hjust = 0.5)
        #axis.text = element_text(face="bold")
      ) 
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
  
  db_bar_varlabel <- reactive(
    {switch(input$db_bar_var,
            "SchoolType" = "School Type",
            "Loneliness" = "Loneliness",
            "ClassroomSafety" = "Classroom Safety",
            "TeacherSupport" = "Teacher Support",
            "Gender" = "Gender",
            "Homework_Math" = "Math Homework Time",
            "Homework_Reading" = "Reading Homework Time",
            "Homework_Science" = "Science Homework Time",
            "Preference_Math" = "Preference for Math",
            "Preference_Reading" = "Preference for Reading",
            "Preference_Science" = "Preference for Science",
            "Exercise" = "Exercise",
            "ParentsEducation" = "Parents' Education",
            "Immigration" = "Immigration",
            "HomeLanguage" = "Home Language",
            "Sibling" = "Sibling",
            "Aircon" = "Aircon",
            "Helper" = "Helper",
            "Vehicle" = "Vehicle",
            "Books" = "Books",
            "OwnRoom" = "Own Room",
            "FamilyCommitment" = "Family Commitment")}
  )
  
  output$db_treemap <- renderPlot({
    stu %>%
      count(.[[input$db_bar_var]]) %>%
      mutate( pct = paste0(round(n/nrow(stu)*100,2),"%"),
              label = paste(.[['.[[input$db_bar_var]]']], n, pct, sep = "\n")) %>%
      
      treemap(index = "label",  # Categorical variable
              vSize = "n",   # Dependent variable
              title = paste0("Frequency of ",db_bar_varlabel()),
              fontsize.title = 14
      )
  })
  
  # output$db_bar <- renderPlotly({
  #   plot_ly(stu, 
  #           x = ~ get(input$db_bar_var)
  #   ) %>% 
  #     add_histogram(
  #       textposition = 'auto',
  #       color = I('#caced8'),
  #       textfont = list(size =10, 
  #                       color = 'rgb(158,202,225)'
  #       )
  #     ) %>% 
  #     layout(yaxis = list(title = "No. of Students"),
  #            xaxis = list(title = input$db_bar_var)
  #     )
  # })
  
  # EDA/CDA  ----------------------------------------------------
  
  ## One-Sample Test - gghistostats   ----------------------------------------------------
  
  ### Test Score Histogram Reactive Sidebar Toggles
  observeEvent(input$cda_testscorehist_testtype, {
    if (input$cda_testscorehist_testtype == "p") {
      shinyjs::show("cda_testscorehist_ifparametric")
    }
    else {shinyjs::hide("cda_testscorehist_ifparametric")
    }
  }) 
  
  cda_testscorehist <- eventReactive(
    input$action_cda_testscorehist, {
      if(input$cda_testscorehist_testtype == "p") {
        
        set.seed(1234)
        
        gghistostats(
          data = stu,
          x = !!sym(input$cda_testscorehist_subject),
          test.value = as.numeric(input$cda_testscorehist_ts),
          type = "p",
          centrality.plotting = TRUE,
          conf.level = as.numeric(input$cda_testscorehist_cl),
          centrality.line.args = list(color = "#20948b", linetype = "dashed", linewidth = 1),
          binwidth = as.numeric(input$cda_testscorehist_binwidth),
          bin.args = list(color = "white", fill = "grey40", alpha = 0.5),
          effsize.type = input$cda_effsizetype, #if parametric
          normal.curve = TRUE,
          normal.curve.args = list(color = "#dfb2e9", linewidth = 1.5),
          ggplot.component = list(theme(text = element_text(size = 15),
                                        axis.title = element_text(size = 14)))
        )}
      else {
        set.seed(1234)
        
        gghistostats(
          data = stu,
          x = !!sym(input$cda_testscorehist_subject),
          test.value = as.numeric(input$cda_testscorehist_ts),
          type = input$cda_testscorehist_testtype,
          centrality.plotting = TRUE,
          conf.level = as.numeric(input$cda_testscorehist_cl),
          centrality.line.args = list(color = "#20948b", linetype = "dashed", linewidth = 1),
          binwidth = as.numeric(input$cda_testscorehist_binwidth),
          bin.args = list(color = "white", fill = "grey40", alpha = 0.5),
          normal.curve = TRUE,
          normal.curve.args = list(color = "#dfb2e9", linewidth = 1.5),
          ggplot.component = list(theme(text = element_text(size = 15),
                                        axis.title = element_text(size = 14, face="bold"),
                                        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                                        axis.line.y = element_blank(),
                                        axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                                        plot.title = element_text(size=12, face="bold")
          )
          )
        )
      }
    }
  )
  
  output$cda_testscorehist_ <- renderPlot({
    cda_testscorehist()
  })
  
  
  ### QQ-Plot
  
  cda_testscoreqqplot <- eventReactive(
    input$action_cda_testscorehist, {
      
      ggqqplot(stu,
               x = input$cda_testscorehist_subject,
               color = "grey40") +
        labs(x = "Theoretical Quantiles",
             y = input$cda_testscorehist_subject) +
        theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              plot.title = element_text(size=12, face="bold"),
              axis.title = element_text(face="bold"))
      #axis.text = element_text(face="bold")
    })
  
  output$cda_testscoreqqplot_ <- renderPlot({
    cda_testscoreqqplot()
  })
  
  
  ### Normality test
  
  cda_testscore_adtest <- eventReactive(
    input$action_cda_testscorehist, {
      nortest::ad.test(stu[[input$cda_testscorehist_subject]])
    })
  
  output$cda_adtest_stat <- renderValueBox(
    valueBox(
      value = tags$p(round(cda_testscore_adtest()$statistic,3), style = "font-size: 60%;"), 
      subtitle = tags$p("Anderson-Darling statistic", style = "font-style: italic; font-size: 80%"),
      icon = tags$i(icon("calculator"), style="font-size: 60%"),
      color = "yellow"
    )
  )
  
  output$cda_adtest_pvalue = renderValueBox(
    valueBox(
      value = tags$p(cda_testscore_adtest()$p.value, style = "font-size: 60%;"), 
      subtitle = tags$p("p-Value of Anderson-Darling Statistic", style = "font-style: italic; font-size: 80%"),
      icon = tags$i(icon("calculator"), style="font-size: 60%"),
      color = "yellow"
    )
  )
  
  ## 2-Sample Test/ANOVA   ----------------------------------------------------
  ## Reactive Sidebar Toggles
  observeEvent(input$cda_anova_pwcompare, {
    if (input$cda_anova_pwcompare == TRUE) {
      shinyjs::show("cda_pwdisplaypanel")
    }
    else {shinyjs::hide("cda_pwdisplaypanel")
    }
  })  
  
  var_xlabels <- reactive(
    {switch(input$cda_anova_var,
            "SchoolType" = "School Type",
            "Loneliness" = "Loneliness",
            "ClassroomSafety" = "Classroom Safety",
            "TeacherSupport" = "Teacher Support",
            "Gender" = "Gender",
            "Homework_Math" = "Math Homework Time",
            "Homework_Reading" = "Reading Homework Time",
            "Homework_Science" = "Science Homework Time",
            "Preference_Math" = "Preference for Math",
            "Preference_Reading" = "Preference for Reading",
            "Preference_Science" = "Preference for Science",
            "Exercise" = "Exercise",
            "ParentsEducation" = "Parents' Education",
            "Immigration" = "Immigration",
            "HomeLanguage" = "Home Language",
            "Sibling" = "Sibling",
            "Aircon" = "Aircon",
            "Helper" = "Helper",
            "Vehicle" = "Vehicle",
            "Books" = "Books",
            "OwnRoom" = "Own Room",
            "FamilyCommitment" = "Family Commitment")}
  )
  
  cda_anova_ <- eventReactive(
    input$action_cda_anova, { 
      if (input$cda_anova_pwcompare == TRUE) {
        ggbetweenstats(
          data = stu,
          x = !!sym(input$cda_anova_var), 
          y = !!sym(input$cda_anova_subject),
          type = input$cda_anova_testtype,
          conf.level = as.numeric(input$cda_anova_cl), 
          #var.equal = input$cda_anova_varequal,
          pairwise.display = input$cda_pwdisplay,
          #p.adjust.method = "fdr",
          xlab = var_xlabels(),
          title = paste0("Distribution of " , input$cda_anova_subject, " score across ", var_xlabels()),
          centrality.label.args = list(size = 4, nudge_x = 0.4, segment.linetype = 4),
          ggsignif.args = list(textsize = 4, tip_length = 0.03),
          ggplot.component = list(theme(text = element_text(size = 15),
                                        axis.title = element_text(size = 14, face="bold"),
                                        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                                        axis.line.y = element_blank(),
                                        axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                                        plot.title = element_text(size=16, face="bold")),
                                  scale_color_manual(values = mycolours))
        )
      }
      else {
        ggbetweenstats(
          data = stu,
          x = !!sym(input$cda_anova_var), 
          y = !!sym(input$cda_anova_subject),
          type = input$cda_anova_testtype,
          conf.level = as.numeric(input$cda_anova_cl), 
          #var.equal = input$cda_anova_varequal,
          pairwise.display = "none",
          #p.adjust.method = "fdr",
          centrality.plotting = TRUE,
          xlab = var_xlabels(),
          title = paste0("Distribution of " , input$cda_anova_subject, " scores across ", var_xlabels()),
          centrality.label.args = list(size = 5, nudge_x = 0.4, segment.linetype = 4),
          ggsignif.args = list(size = 0, textsize = 0, tip_length = 0),
          ggplot.component = list(theme(text = element_text(size = 15),
                                        axis.title = element_text(size = 14, face="bold"),
                                        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                                        axis.line.y = element_blank(),
                                        axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                                        plot.title = element_text(size=16, face="bold")),
                                  scale_color_manual(values = mycolours))
          
        )
      }
      
    })
  
  output$cda_anova_plot <- renderPlot({
    cda_anova_()
  })
  
  
  ## Association Test   ----------------------------------------------------
  
  ## Reactive Sidebar Toggles  
  
  observeEvent(input$cda_assoc_var1, {
    updateSelectizeInput(session = getDefaultReactiveDomain(),
                         inputId = "cda_assoc_var2",
                         choices = assoctest_var[!(assoctest_var %in% input$cda_assoc_var1)],
                         selected = NULL)
  })
  
  assoc_var_xlabels <- reactive(
    {switch(input$cda_assoc_var1,
            "SchoolType" = "School Type",
            "Loneliness" = "Loneliness",
            "ClassroomSafety" = "Classroom Safety",
            "TeacherSupport" = "Teacher Support",
            "Gender" = "Gender",
            "Homework_Math" = "Math Homework Time",
            "Homework_Reading" = "Reading Homework Time",
            "Homework_Science" = "Science Homework Time",
            "Preference_Math" = "Preference for Math",
            "Preference_Reading" = "Preference for Reading",
            "Preference_Science" = "Preference for Science",
            "Exercise" = "Exercise",
            "ParentsEducation" = "Parents' Education",
            "Immigration" = "Immigration",
            "HomeLanguage" = "Home Language",
            "Sibling" = "Sibling",
            "Aircon" = "Aircon",
            "Helper" = "Helper",
            "Vehicle" = "Vehicle",
            "Books" = "Books",
            "OwnRoom" = "Own Room",
            "FamilyCommitment" = "Family Commitment")}
  )
  
  assoc_var_ylabels <- reactive(
    {switch(input$cda_assoc_var2,
            "SchoolType" = "School Type",
            "Loneliness" = "Loneliness",
            "ClassroomSafety" = "Classroom Safety",
            "TeacherSupport" = "Teacher Support",
            "Gender" = "Gender",
            "Homework_Math" = "Math Homework Time",
            "Homework_Reading" = "Reading Homework Time",
            "Homework_Science" = "Science Homework Time",
            "Preference_Math" = "Preference for Math",
            "Preference_Reading" = "Preference for Reading",
            "Preference_Science" = "Preference for Science",
            "Exercise" = "Exercise",
            "ParentsEducation" = "Parents' Education",
            "Immigration" = "Immigration",
            "HomeLanguage" = "Home Language",
            "Sibling" = "Sibling",
            "Aircon" = "Aircon",
            "Helper" = "Helper",
            "Vehicle" = "Vehicle",
            "Books" = "Books",
            "OwnRoom" = "Own Room",
            "FamilyCommitment" = "Family Commitment")}
  )
  
  
  cda_assoc_ <- eventReactive(
    input$action_cda_assoc, {
      ggbarstats(
        stu, 
        x = !!sym(input$cda_assoc_var1), 
        y = !!sym(input$cda_assoc_var2),
        type = input$cda_assoc_testtype,
        conf.level = as.numeric(input$cda_assoc_cl),
        label = input$cda_assoc_labeltype,
        label.args = list(size =5, alpha = 1, fill = "white"),
        legend.title =assoc_var_xlabels(),
        ylab = assoc_var_xlabels(), 
        xlab = assoc_var_ylabels(),
        ggplot.component = list(theme(text = element_text(size = 15),
                                      plot.subtitle = element_text(size = 15),
                                      legend.title = element_text(size = 15),
                                      legend.text = element_text(size = 14),
                                      axis.title = element_text(size = 14, face="bold"),
                                      axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                                      axis.line.y = element_blank(),
                                      axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                                      plot.title = element_text(size=16, face="bold")),
                                scale_fill_manual(values = mycolours)
        )
        
      )
    }
  )
  
  output$cda_assoc_plot <- renderPlot({
    cda_assoc_()
  })
  
  
  # Clustering  ----------------------------------------------------


  # Update "group_by" Option with Selected Variables
  vars_names_heatmap <- c("SchoolID" = "School ID",
                          "SchoolType" = "School Type",
                          "ClassroomSafety" = "Classroom Safety",
                          "TeacherSupport" = "Teacher Support",
                          "Homework_Math" = "Math Homework Time",
                          "Homework_Reading" = "Reading Homework Time",
                          "Homework_Science" = "Science Homework Time",
                          "Preference_Math" = "Preference for Math",
                          "Preference_Reading" = "Preference for Reading",
                          "Preference_Science" = "Preference for Science",
                          "ParentsEducation" = "Parents' Education",
                          "HomeLanguage" = "Home Language",
                          "OwnRoom" = "Own Room",
                          "FamilyCommitment" = "Family Commitment",
                          "Math" = "Math",
                          "Reading" ="Reading",
                          "Science" = "Science",
                          "Gender" = "Gender",
                          "Exercise" = "Exercise",
                          "Immigration" = "Immigration",
                          "Sibling" = "Sibling",
                          "Aircon" = "Aircon",
                          "Helper" = "Helper",
                          "Vehicle" = "Vehicle",
                          "Books" = "Books")

  vars_names_lca <- vars_names_heatmap
  vars_names_sankey <- vars_names_heatmap

  ### Sankey Diagram  ----------------------------------------------------
  
  # Combine Selected Variables and SchoolID
  selected_variables_sankey <- eventReactive(
    input$action_sankey_vars, {
      c(input$scores_sankey,
        input$vars_sankey)
    }
  )
  
  # Filter Dataset using Selected Variables
  data_filtered_sankey <- eventReactive(
    input$action_sankey_vars, {
      data_filtered_sankey <- stu_sankey %>% 
        dplyr::select(all_of(selected_variables_sankey()))
    }
  )
  
  # Sankey Table of Proportions
  freq_table_sankey <- eventReactive(
    input$action_sankey_vars, {
      data_filtered_sankey() %>% 
        dplyr::group_by_at(vars(selected_variables_sankey())) %>% 
        summarise(n = n())
    })
  
  nodes_sankey <- eventReactive(
    input$action_sankey_vars, {
      nodes_sankey <- data.frame(name = unique(c(as.character(unlist(freq_table_sankey()[,1])),
                                                 as.character(unlist(freq_table_sankey()[,2])),
                                                 as.character(unlist(freq_table_sankey()[,3])),
                                                 as.character(unlist(freq_table_sankey()[,4])),
                                                 as.character(unlist(freq_table_sankey()[,5]))
      )))
    })
  
  # Create Links for Sankey
  links_sankey <- eventReactive(
    input$action_sankey_vars, {
      links_sankey <- data.frame(source = numeric(0), target = numeric(0), value = numeric(0))
      for (i in 1:(ncol(freq_table_sankey()) - 1)) {
        source_col <- unlist(freq_table_sankey()[, i])
        target_col <- unlist(freq_table_sankey()[, i + 1])
        value_col <- freq_table_sankey()$n
        links_sankey <- rbind(links_sankey, data.frame(source = match(source_col, nodes_sankey()$name) - 1,
                                                       target = match(target_col, nodes_sankey()$name) - 1,
                                                       value = value_col,
                                                       stringsAsFactors = FALSE))
      }
      links_sankey
    }
  )
  
  # Generate Sankey Diagram
  title_names_sankey <- eventReactive(
    input$action_sankey_vars, {
      choices_original_sankey <- selected_variables_sankey()
      choices_display_sankey <- choices_original_sankey
      
      for (var in names(vars_names_sankey)) {
        index_sankey <- which(choices_original_sankey == var)
        if (length(index_sankey) > 0) {
          choices_display_sankey[index_sankey] <- vars_names_sankey[var]
        }
      }
      
      return(choices_display_sankey)
    })
  
  sankey_plot <- eventReactive(
    input$action_sankey_vars, {
      plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(pad = 15,
                    thickness = 20,
                    line = list(color = "black", width = 0.5),
                    label = nodes_sankey()$name),
        link = list(source = links_sankey()$source,
                    target = links_sankey()$target,
                    value = links_sankey()$value)) %>%
        layout(
          title = list(text = paste("Sankey Diagram for", paste(title_names_sankey(), collapse = ", ")),
                       font = list(size = 12))
          
          #paste("Sankey Diagram:", paste(colnames(freq_table_sankey()[1:5]), collapse = ", "))
        )
    })
  
  # Function to Render Sankey Diagram
  cluster_sankey = function(modeltype){
    output$sankey = renderPlotly(modeltype)}
  
  observeEvent(input$action_sankey_vars, cluster_sankey(sankey_plot()))
  
  
  
  ### Latent Class Analysis  ----------------------------------------------------
  
  # Define Selected Variables
  selected_variables_lca <- eventReactive(
    input$action_lca_vars, {
      c(input$scores_lca,
        input$vars_lca)
    }
  )
  
  # Update "group_by" Option with Selected Variables
  observeEvent(input$action_lca_vars, {
    choices_original_lca <- selected_variables_lca()
    choices_display_lca <- choices_original_lca
    
    for (var in names(vars_names_lca)) {
      index_lca <- which(choices_original_lca == var)
      if (length(index_lca) > 0) {
        choices_display_lca[index_lca] <- vars_names_lca[var]
      }
    }
    
    updateSelectInput(
      session = getDefaultReactiveDomain(), 
      inputId = "groupby_lca", 
      choices = setNames(choices_original_lca, choices_display_lca)
    )
  }
  )
  
  # Filter Dataset using Selected Variables
  data_filtered_lca <- eventReactive(
    input$action_lca_vars, {
      data_filtered_lca <- stu_lca %>%
        dplyr::select(all_of(selected_variables_lca()))
    }
  )
  
  # Create Formula for LCA
  model_lca <- eventReactive(
    input$action_lca, {
      formula_string <- paste(selected_variables_lca(), collapse = ", ")
      f <- as.formula(paste("cbind(", formula_string, ") ~ 1"))
      model_lca <- poLCA(f, data_filtered_lca(), 
                         nclass= input$numclusters_lca, 
                         nrep = input$numrep_lca, 
                         maxiter=5000)    }
  )
  
  model_lcaprobs <- eventReactive(
    input$action_lca, {
      reshape2::melt(model_lca()$probs) %>% 
        rename(Class = Var1,
               Factor_Level = Var2,
               Prop = value,
               Category = L1) %>%
        mutate(Prop = round(Prop*100,2))
      
    })
  
  model_lcawlabels <- eventReactive(
    input$action_lca, {
      left_join(model_lcaprobs(), lcalabels,
                by = c("Factor_Level" = "Factor_Level",
                       "Category" = "Category"))
      
    })
  
  ## LCA Summary Plot
  lca_plotsummary <- eventReactive(
    input$action_lca, {
      ggplot(model_lcawlabels(),
             aes(x = Class, y = Prop, group = desc(Factor_Level))) + 
        geom_bar(stat = "identity", position = "stack", 
                 aes(fill = Factor_Level,
                     text = paste0("Proportion: ", Prop,"%","\n",
                                   "Category: ", Category, "\n",
                                   "Level: ", Factor_Level))) + 
        facet_wrap(~ Category, nrow = 1) + 
        coord_flip() +
        labs(fill = "Factor Level") +
        scale_y_continuous("Proportion", expand = c(0, 0)) +
        theme_minimal()+
        scale_fill_manual(values = mycolours)+
        theme(legend.position="none")
    })
  
  output$lca_plotsummary_ <- renderPlotly({
    ggplotly(lca_plotsummary(), tooltip = c("text", "x")) 
  })
  
  
  ## LCA Statistics
  aic_lca <- eventReactive(
    input$action_lca, {
      return(model_lca()$aic)
    }
  )
  
  aic_lca_display = function(){
    output$lca_supp1 = renderValueBox(
      valueBox(
        value = tags$p(scales::comma(round(aic_lca(),2)), style = "font-size: 60%;"), 
        subtitle = tags$p("AIC (Click for More Information)", style = "font-size: 80%;"), 
        icon = tags$i(icon("calculator"), style = "font-size: 60%;"), 
        width = 12,
        color = "yellow"
      )
    )
  }
  
  observeEvent(input$action_lca, aic_lca_display())
  
  
  bic_lca <- eventReactive(
    input$action_lca, {
      return(model_lca()$bic)
    }
  )
  
  bic_lca_display = function(){
    output$lca_supp2 = renderValueBox(
      valueBox(
        value = tags$p(scales::comma(round(bic_lca(),2)), style = "font-size: 60%;"), 
        subtitle = tags$p("BIC (Click for More Information)", style = "font-size: 80%;"), 
        icon = tags$i(icon("calculator"), style = "font-size: 60%;"), 
        width = 12,
        color = "yellow"
      )
    )
  }
  
  observeEvent(input$action_lca, bic_lca_display())
  
  gsq_lca <- eventReactive(
    input$action_lca, {
      return(model_lca()$Gsq)
    }
  )
  
  gsq_lca_display = function(){
    output$lca_supp3 = renderValueBox(
      valueBox(
        value = tags$p(scales::comma(round(gsq_lca(),2)), style = "font-size: 60%;"), 
        subtitle = tags$p("Likelihood Ratio (Click for More Information)", style = "font-size: 80%;"), 
        icon = tags$i(icon("calculator"), style = "font-size: 60%;"), 
        width = 12,
        color = "yellow"
      )
    )
  }
  
  observeEvent(input$action_lca, gsq_lca_display())
  
  ent_lca <- eventReactive(
    input$action_lca, {
      return(poLCA.entropy(model_lca()))
    }
  )
  
  ent_lca_display = function(){
    output$lca_supp4 = renderValueBox(
      valueBox(
        value = tags$p(scales::comma(round(ent_lca(),2)), style = "font-size: 60%;"), 
        subtitle = tags$p("Entropy (Click for More Information)", style = "font-size: 80%;"), 
        icon = tags$i(icon("calculator"), style = "font-size: 60%;"), 
        width = 12,
        color = "yellow"
      )
    )
  }
  
  observeEvent(input$action_lca, ent_lca_display()) 
  
  
  ## Generate LCA Bar Plot - By variable
  lca_plot <- eventReactive(
    input$action_lca, {
      model_lca <- model_lca()
      data_filtered_lca_plot <- data_filtered_lca()
      data_filtered_lca_plot$class <- model_lca$predclass
      data_filtered_lca_plot$class <- factor(data_filtered_lca_plot$class)
      
      selected_var <- as.symbol(input$groupby_lca)
      
      plot_table <- data_filtered_lca_plot %>%
        dplyr::group_by(!!selected_var, class) %>% 
        summarise(proportion = n()) %>% 
        dplyr::ungroup()
      
      p <- ggplot(plot_table, 
                  aes(fill = !!selected_var, 
                      y = proportion, 
                      x = class)) +
        geom_bar(position = "fill", stat = "identity")+
        theme_minimal()+
        labs(x = "Class",
             y = "Proportion") +
        coord_flip() +
        scale_y_continuous("Proportion", expand = c(0, 0)) + 
        scale_fill_manual(values = mycolours) +
        theme(legend.position="none")
      
      ggplotly(p)
    }
  )
  
  lca_withmod <- eventReactive(
    input$action_lca, {
      data_filtered_lca() %>%
        mutate(class = model_lca()$predclass) %>%
        mutate(class = as.factor(class))
  })

  # Function to Plot LCA Bar Plot
  cluster_lca = function(modeltype){
    output$lca = renderPlotly(modeltype)}
  
  observeEvent(input$action_lca, cluster_lca(lca_plot()))
  
  cluster_lca = function(modeltype){
    output$lca = renderPlotly(modeltype)}
  
  observeEvent(input$action_lca, cluster_lca(lca_plot()))
  
  ## Data Manipulation for cluster proportion
  lca_proportion <- eventReactive(
    input$action_lca, {
      lca_withmod() %>%
        group_by(class) %>%
        summarise(counts = n()) %>%
        mutate(class_pct = round(counts/ sum(counts), 2)) %>%
        ungroup()
    })
  
  ## Cluster Proportion
  lca_propplot <- eventReactive(
    input$action_lca, {
      ggplot(lca_proportion(), 
             aes(y = class, x = class_pct)) + 
        geom_bar(aes(text = paste0("Proportion: ",round(class_pct, 2)*100,"%")), 
                 stat = "identity",
                 fill = "#C7C2D0") +
        labs(x = "Proportion of Records in Cluster", y = "Cluster") +
        scale_x_continuous(labels = scales::percent) +
        theme_minimal() +
        theme(legend.position="none")
    })
  
  output$lca_propplot_ <- renderPlotly({
    ggplotly(lca_propplot(), tooltip = c("text")) %>%
      layout(autosize = TRUE)
  })
  
  
  # Regression Analysis ----------------------------------------------------
  
  # Shared Model Construction Panel  ----------------------------------------------------

  # Define reactiveValues object to store selected options for each tab

  
  # DT Data Manipulation  ----------------------------------------------------
  
  # Combine Selected Variables
  dt_selected_variables <- reactive({
    c(input$mb_target_,
      input$mb_vars_)
  })  
  
  dt_data <- reactive({
    stu_mb %>%
      dplyr::select(all_of(dt_selected_variables()))
  }
  )
  
  dt_index <- eventReactive(
    input$dt_action_, {
      set.seed(1234)
      caret::createDataPartition(dt_data()[[1]], p = input$mb_partition_, list = FALSE)      
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
    shinyjs::show("dt_modeltune_group")
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
      if (input$mb_target_ == "Math") {
        set.seed(1234)
        
        rpart(formula = Math ~.,
              data = dt_traindata(),
              method = "anova",
              control = rpart.control(minsplit = 5, cp = 0.001, maxdepth = 10)
        )}
      
      else if (input$mb_target_ == "Reading") {
        set.seed(1234)
        
        rpart(formula = Reading ~.,
              data = dt_traindata(),
              method = "anova",
              control = rpart.control(minsplit = 5, cp = 0.001, maxdepth = 10)
        )}
      
      else {
        set.seed(1234)
        
        rpart(formula = Science ~.,
              data = dt_traindata(),
              method = "anova",
              control = rpart.control(minsplit = 5, cp = 0.001, maxdepth = 10)
        )}
    })
  
  dtmodel_prune <- eventReactive(
    input$dt_tunemodel_, {
      if (input$mb_target_ == "Math") {
        if (input$dt_bestcp == TRUE) {
          set.seed(1234)
          
          rpart(formula = Math ~.,
                data = dt_traindata(),
                method = "anova",
                control = rpart.control(minsplit = input$dt_minsplit, 
                                        maxdepth = input$dt_maxdepth,
                                        cp = dt_bestcp()))
        }
        else {
          set.seed(1234)
          
          rpart(formula = Math ~.,
                data = dt_traindata(),
                method = "anova",
                control = rpart.control(minsplit = input$dt_minsplit, 
                                        maxdepth = input$dt_maxdepth,
                                        cp = input$dt_cp))
        } }
      
      else if (input$mb_target_ == "Reading") {
        if (input$dt_bestcp == TRUE) {
          set.seed(1234)
          
          rpart(formula = Reading ~.,
                data = dt_traindata(),
                method = "anova",
                control = rpart.control(minsplit = input$dt_minsplit, 
                                        maxdepth = input$dt_maxdepth,
                                        cp = dt_bestcp()))
        }
        else {
          set.seed(1234)
          
          rpart(formula = Reading ~.,
                data = dt_traindata(),
                method = "anova",
                control = rpart.control(minsplit = input$dt_minsplit, 
                                        maxdepth = input$dt_maxdepth,
                                        cp = input$dt_cp))
        }
      }
      
      else {
        if (input$dt_bestcp == TRUE) {
          set.seed(1234)
          
          rpart(formula = Science ~.,
                data = dt_traindata(),
                method = "anova",
                control = rpart.control(minsplit = input$dt_minsplit, 
                                        maxdepth = input$dt_maxdepth,
                                        cp = dt_bestcp()))
        }
        else {
          set.seed(1234)
          
          rpart(formula = Science ~.,
                data = dt_traindata(),
                method = "anova",
                control = rpart.control(minsplit = input$dt_minsplit, 
                                        maxdepth = input$dt_maxdepth,
                                        cp = input$dt_cp))
        }
      }
    })
  
  
  
  dt_rpartplot <- eventReactive(
    input$dt_action_, {
      visTree(dtmodel(), 
              edgesFontSize = 12, 
              nodesFontSize = 14,
              legend = FALSE)
    })
  
  dt_rpartplot_prune <- eventReactive(
    input$dt_tunemodel_, {
      visTree(dtmodel_prune(), 
              edgesFontSize = 12, 
              nodesFontSize = 14,
              legend = FALSE)
    })
  
  ### Function to plot regression tree
  dt_plotrpart = function(modeltype){
    output$dt_rpartplot_ = renderVisNetwork(modeltype)
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
  #### CP Plot
  dt_bestcp <- eventReactive(
    input$dt_action_, {
      round(dtmodel()$cptable[which.min(dtmodel()$cptable[,"xerror"]),"CP"],5)
    }
  )  
  
  
  
  dt_showcpbox = function(modeltype){
    output$dt_showcp_ = renderValueBox(
      valueBox(
        value = tags$p(modeltype, style = "font-size: 60%;"), 
        subtitle = tags$p("Best Complexity Parameter (Click for more)", style = "font-size: 100%"),
        icon = tags$i(icon("trophy"), style="font-size: 70%"),
        color = "yellow"
      )
    )
  }
  
  observeEvent(input$dt_action_, dt_showcpbox(dt_bestcp()))
  # observeEvent(input$dt_tunemodel_, dt_showcpbox(dt_showcp_prune()))
  
  #### CP ValueBox
  dt_cp_plot <- eventReactive(
    input$dt_action_, {
      plotcp(dtmodel(), upper = "splits")
    }
  )  
  
  
  
  #### Function to plot Residuals vs Actual
  dt_plotcpplot = function(modeltype){
    output$dt_cp_plot_ = renderPlot(modeltype)
  }
  
  observeEvent(input$dt_action_, dt_plotcpplot(dt_cp_plot()))
  # observeEvent(input$dt_tunemodel_, dt_plotcpplot(dt_cp_plot_prune()))
  
  #### CP Table
  dt_cptable <- eventReactive(
    input$dt_action_, {
      dtmodel()$cptable %>%
        data.frame() %>%
        mutate(
          min_idx = which.min(dtmodel()$cptable[, "xerror"]),
          rownum = row_number(),
          xerror_cap = dtmodel()$cptable[min_idx, "xerror"] + 
            dtmodel()$cptable[min_idx, "xstd"],
          eval = case_when(rownum == min_idx ~ "Min. xerror",
                           xerror < xerror_cap ~ "Under cap",
                           TRUE ~ "")) %>%
        dplyr::select(-rownum, -min_idx) 
    })
  
  output$dt_cptable_ <- DT::renderDataTable({
    datatable(dt_cptable(),
              options = list(dom = 't',
                             autoWidth = TRUE))
  })
  
  
  
  ## DT Variable Importance
  dt_varimp_plot_tune <- reactive(
    if(input$dt_action_) {
      vip::vip(dtmodel(), 
               num_features = input$dt_varimp_varcount, 
               geom = "col",
               aesthetics = list(color = "grey40"))
    }
  ) 
  
  ## DT Variable Importance
  dt_varimp_plot_prune <- reactive(
    if(input$dt_tunemodel_) {
      vip::vip(dtmodel_prune(), 
               num_features = input$dt_varimp_varcount, 
               geom = "col",
               aesthetics = list(color = "grey40"))
    }
  ) 
  
  
  ### Function to plot Residuals vs Actual
  dt_varimp_plot = function(modeltype){
    output$dt_varimp_plot_ = renderPlot(modeltype)
  }
  
  observeEvent(input$dt_action_, dt_varimp_plot(dt_varimp_plot_tune()))
  observeEvent(input$dt_tunemodel_, dt_varimp_plot(dt_varimp_plot_prune()))
  
  # RF Data Manipulation  ----------------------------------------------------
  
  # Combine Selected Variables
  rf_selected_variables <- reactive({
    c(input$mb_target_,
      input$mb_vars_)
  })  
  
  rf_data <- reactive({
    stu_mb %>%
      dplyr::select(all_of(rf_selected_variables()))
  }
  )
  
  rf_index <- eventReactive(
    input$rf_action_, {
      set.seed(1234)
      caret::createDataPartition(rf_data()[[1]], p = input$mb_partition_, list = FALSE)      
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

  ### Display Selection depending on resampling method
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
      if (input$mb_target_ == "Math") {
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
      
      else if (input$mb_target_ == "Reading") {
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
    c(input$mb_target_,
      input$mb_vars_)
  })  
  
  gb_data <- reactive({
    stu_mb %>%
      dplyr::select(all_of(gb_selected_variables()))
  }
  )
  
  gb_index <- eventReactive(
    input$gb_action_, {
      set.seed(1234)
      caret::createDataPartition(gb_data()[[1]], p = input$mb_partition_, list = FALSE)      
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
  
  # GB Sidebar Toggles  ----------------------------------------------------
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
      seq(from = max(2, input$gb_cvkfold_interactiondepth[1]),
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
      if (input$mb_target_ == "Math") {
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
      
      else if (input$mb_target_ == "Reading") {
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
  gb_besttune <- eventReactive(
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
  
  
  output$gb_besttune_ <- renderText({
    gb_besttune()
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