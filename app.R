##########################################################################################

# projectPISA App
# ISSS608 G2
# App File

###########################################################################################


#==============================#
#~~~~~ Importing Packages~~~~~#
#==============================#

pacman::p_load("shiny", "fresh", "shinydashboard", "shinydashboardPlus", "shinyWidgets", "shinythemes", "shinyjs", "waiter",
               "tidyverse", "DT", "kableExtra", "plotly", "scales", "gt",
               "ggstatsplot",
               "ranger", "vip", "rpart.plot", "caret", "tidymodels", "gbm",
               "dendextend", "heatmaply",
               "parallelPlot", "poLCA", "rlang"
               )

#==============================#
#~~~~~ Data Manipulation ~~~~~#
#==============================#

# Loading files
stu <- read_rds("data/stu_SG_rcd.rds")
varlist <- read_csv("data/var.csv")

# Dashboard - Summary Statistics
Math <- round(summary(stu$Math),1)
Reading <- round(summary(stu$Reading),1)
Science <- round(summary(stu$Science),1)

db_summarystats <- as.data.frame(rbind(Math, Reading, Science))

# Regression Model - Data Selection
stu_mb <- stu %>% 
  dplyr::select(2:26) %>% 
  na.omit()

# Cluster Analysis - Data Manipulation
stu_ca_less <- stu %>% 
  dplyr::select(2:26) %>% 
  na.omit()

cat <- list(Math = NULL, Reading = NULL, Science = NULL, 
            SchoolType = c("Public",
                           "Private"),
            Loneliness = c("Strongly Agree",
                           "Agree",
                           "Disagree",
                           "Strongly Disagree"),
            ClassroomSafety = c("Strongly Disagree",
                                "Disagree",
                                "Agree",
                                "Strongly Agree"),
            TeacherSupport = c("Never or almost never",
                               "Some lessons",
                               "Most lesson",
                               "Every lesson"),
            Gender = c("Female",
                       "Male"),
            Homework_Math = c("≤ 0.5hr",
                              "0.5hr - 1hr",
                              "1hr - 2hr",
                              "2hr - 3hr",
                              "3 - 4 hr",
                              "> 4hr"),
            Homework_Reading = c("≤ 0.5hr",
                                 "0.5hr - 1hr",
                                 "1hr - 2hr",
                                 "2hr - 3hr",
                                 "3 - 4 hr",
                                 "> 4hr"),
            Homework_Science = c("≤ 0.5hr",
                                 "0.5hr - 1hr",
                                 "1hr - 2hr",
                                 "2hr - 3hr",
                                 "3 - 4 hr",
                                 "> 4hr"),
            Preference_Math = c("Strongly Disagree",
                                "Disagree",
                                "Agree",
                                "Strongly Agree"),
            Preference_Reading = c("Strongly Disagree",
                                   "Disagree",
                                   "Agree",
                                   "Strongly Agree"),
            Preference_Science = c("Strongly Disagree",
                                   "Disagree",
                                   "Agree",
                                   "Strongly Agree"),
            Exercise = c("0",
                         "1", 
                         "2",
                         "3",
                         "4",
                         "5",
                         "6",
                         "7",
                         "8",
                         "9",
                         "10"),
            ParentsEducation = c("Pre-Primary",
                                 "Primary", 
                                 "Secondary",
                                 "Post-Secondary",
                                 "Tertiary"),
            Immigration = c("Native",
                            "2nd Generation",
                            "3rd Generation"),
            HomeLanguage = c("English",
                             "Others"),
            Sibling = c("0",
                        "1",
                        "2",
                        "≥3"),
            Books = c("0",
                      "1 - 10",
                      "11 - 25",
                      "26 - 100",
                      "101 - 200",
                      "201-500",
                      ">500"),
            Aircon = c("No", "Yes"),
            Helper = c("No", "Yes"),
            Vehicle = c("0",
                        "1",
                        "2",
                        "≥3"),
            OwnRoom = c("No", 
                        "Yes"),
            FamilyCommitment = c("0",
                                 "1", 
                                 "2",
                                 "3",
                                 "4",
                                 "5",
                                 "6",
                                 "7",
                                 "8",
                                 "9",
                                 "10"))


# Recode for LCA
median_math <- median(stu_ca_less$Math)
median_reading <- median(stu_ca_less$Reading)
median_science <- median(stu_ca_less$Science)

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
             menuSubItem("Hierarchical Clustering", tabName = "tab_heatmap"),
             menuSubItem("K-means Clustering", tabName = "tab_parallelplot"),
             menuSubItem("Latent Class Analysis", tabName = "tab_lca")
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
        
        html.shiny-busy .container-fluid {
        cursor: wait;}
        
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
                                 "Confirmatory data analysis rigorously tests pre-existing hypotheses using statistical methods.
                                 This section wll conduct normality test, association test, ANOVA test to verify our hypothesis "                             )
                         )
                     ),
                     
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p("Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = FALSE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "cda_Subject", ## ARIEL CAN CHANGE THIS NAME
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
                                 pickerInput(
                                   inputId = "cda_Variables", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Select First Variable",
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
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "cda_Variables_2", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Select Second Variable",
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
                                   options = list(style = "myClass"),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                                   inline = FALSE,
                                   width = NULL)
                                 )
                             )
                         ),
                     div(
                       style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                       box(title = tags$p("Step 2: Configure Options", style = "font-weight: bold;"),
                           collapsible = TRUE,
                           collapsed = TRUE,
                           width = 12,
                           status = "primary",
                           solidHeader = FALSE,  
                         div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 
                                 ## ARIEL YOUR SIDE BAR TOGGLES CAN PUT HERE
                                 sliderInput(
                                   inputId = "cda_slider_bin", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Number of Bins",
                                   min = 1,
                                   max = 50,
                                   value =20,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 
                                 ## ARIEL YOUR SIDE BAR TOGGLES CAN PUT HERE
                                 pickerInput(
                                   inputId = "cda_effsize.type", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Effect Size",
                                   choices = c("d", "g"), 
                                   selected = "d",
                                   multiple = FALSE,
                                   options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                   #choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 
                                 ## ARIEL YOUR SIDE BAR TOGGLES CAN PUT HERE
                                 pickerInput(
                                   inputId = "cda_Normal.curve", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Show distribution curve?",
                                   choices = c(TRUE, FALSE), 
                                   selected = 'TRUE',
                                   multiple = FALSE,
                                   #options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                   #choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 
                                 ## ARIEL YOUR SIDE BAR TOGGLES CAN PUT HERE
                                 pickerInput(
                                   inputId = "cda_Centrality.plotting", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Show central tendency measure",
                                   choices = c(TRUE, FALSE), 
                                   selected = 'TRUE',
                                   multiple = FALSE,
                                   options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 
                                 ## ARIEL YOUR SIDE BAR TOGGLES CAN PUT HERE
                                 pickerInput(
                                   inputId = "cda_test_type", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Test Type",
                                   choices = c("parametric", "Non-parametric", "robust", "Bayes Factor"), 
                                   selected = 'parametric',
                                   multiple = FALSE,
                                   options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 
                                 ## ARIEL YOUR SIDE BAR TOGGLES CAN PUT HERE
                                 sliderInput(
                                   inputId = "cda_slider", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Confidence Level",
                                   min = 0,
                                   max = 1,
                                   value =0.95,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 
                                 ## ARIEL YOUR SIDE BAR TOGGLES CAN PUT HERE
                                 pickerInput(
                                   inputId = "cda_Pairwise_Comparsions", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Show Pairwise Comparsions",
                                   choices = c("TRUE", "FALSE"), 
                                   selected = 'TRUE',
                                   multiple = FALSE,
                                   options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 
                                 ## ARIEL YOUR SIDE BAR TOGGLES CAN PUT HERE
                                 pickerInput(
                                   inputId = "cda_Pairwise_Display", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Show Pairwise Display",
                                   choices = c("significant", "non-significant", "all"), 
                                   selected = 'significant',
                                   multiple = FALSE,
                                   options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL
                                 )),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 
                                 ## ARIEL YOUR SIDE BAR TOGGLES CAN PUT HERE
                                 pickerInput(
                                   inputId = "cda_P.adjust_method", ## ARIEL CAN CHANGE THIS NAME
                                   label = "Choose P.Adjust Method",
                                   choices = c("fdr", "BY"), 
                                   selected = 'fdr',
                                   multiple = FALSE,
                                   options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
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
                             width = 12, # ARIEL ADD COMMA HERE 
                             plotOutput("EDA_Plot1"),
                             plotOutput("EDA_Plot2"),
                             plotOutput("CDA_Plot1")## ARIEL YOUR PLOTS CAN PUT HERE
                             ))
                   )
                   )
            
            ),
    ### Cluster Heatmap  ----------------------------------------------------
    tabItem(tabName = "tab_heatmap",
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p(span(icon("users-viewfinder"), "Cluster Heatmap"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = TRUE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "A Cluster Heatmap is a heatmap with hierarchical clustering. The dendrograms on the side of the heatmap organises the rows and columns based on their similarities and differences, creating a hierachical tree structure. The clustering helps identify patterns and relationships within the data."
                             )
                         )
                     ),
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100%",
                         box(title = tags$p("Step 1: Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = FALSE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "scores_heatmap",
                                   label = "Subject",
                                   choices = c("Math", "Reading", "Science"),
                                   multiple = TRUE,
                                   options = list(style = "myClass"), #list(`actions-box` = TRUE),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL)
                             ),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "vars_heatmap",
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
                                   options = list(style = "myClass"),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                                   inline = FALSE,
                                   width = NULL)
                             ),
                             div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                                 actionButton(inputId = "action_heatmap_vars",
                                              label = "Confirm Selection",
                                              icon = icon("filter"),
                                              style = 'padding: 4px; font-size: 10px'),
                                 align = "center")
                         ),
                         box(title = tags$p("Step 2: Data Sampling", style = "font-weight: bold;"),
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             status = "primary",
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 numericInput(inputId = "seed_heatmap", 
                                              label = "Seed:", 
                                              value = 123, 
                                              step = 1),
                                 selectInput(inputId = "groupby_heatmap", 
                                             label = "Group by:", 
                                             choices = " "),
                                 sliderInput(inputId = "prop_heatmap", 
                                             label = "Proportion:",
                                             min = 0.1, 
                                             max = 1.0, 
                                             value = 0.1, 
                                             step = 0.1),
                                 selectInput(inputId = "replace_heatmap", 
                                             label = "Replace?",
                                             choices = c("False", "True")
                                 )
                             )
                         ),
                         box(title = tags$p("Step 3: Data Transformation", style = "font-weight: bold;"),
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             status = "primary",
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 radioButtons(inputId = "data_transformation_heatmap", 
                                              label = " ",
                                              choices = c("Scale by Column", 
                                                          "Scale by Row",
                                                          "Normalise", 
                                                          "Percentise"),
                                              selected = "Normalise")
                             )
                         ),
                         box(title = tags$p("Step 4: Clustering Algorithm", style = "font-weight: bold;"),
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             status = "primary",
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 selectInput(inputId = "dist_method_heatmap", 
                                             label = "Distance Metric:",
                                             choices = c("euclidean", 
                                                         "maximum", 
                                                         "manhattan",
                                                         "canberra", 
                                                         "binary", 
                                                         "minkowski")),
                                 selectInput(inputId = "hclust_heatmap", 
                                             label = "Hierarchical Clustering Method:",
                                             choices = c("complete", 
                                                         "ward.D",
                                                         "ward.D2", 
                                                         "single", 
                                                         "average", 
                                                         "mcquitty", 
                                                         "median", 
                                                         "centroid")),
                                 selectInput(inputId = "seriation_heatmap", 
                                             label = "Seriation:",
                                             choices = c("none",
                                                         "OLO", 
                                                         "mean", 
                                                         "GW")),
                                 radioButtons(inputId = "clusters_heatmap",
                                              label = "No. of Clusters",
                                              choices = c("Auto", 
                                                          "Manual"),
                                              selected = "Auto"),
                                 conditionalPanel(
                                   condition = "input.clusters_heatmap == 'Manual'",
                                   sliderInput(inputId = "numclusters_heatmap", 
                                               label = "Manual Selection for No. of Clusters:",
                                               min = 1, 
                                               max = 15, 
                                               value = 5,
                                               step = 1)
                                 ),
                                 div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                                     actionButton(inputId = "action_heatmap",
                                                  label = "Run Analysis",
                                                  icon = icon("gear"),
                                                  style = 'padding: 4px; font-size: 10px'),
                                     align = "center")
                             )
                         )
                     )
                     )
              ),
              column(width = 10,
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100%",
                         box(title = tags$p("Plot", style = "font-weight: bold;"), 
                             collapsible = FALSE,
                             status = "primary",
                             width = 12,
                             plotlyOutput("cluster_heatmap", height = "800px"),
                             box(title = tags$p("Supplementary Information", style = "font-weight: bold"), 
                                 collapsible = TRUE,
                                 collapsed = TRUE,
                                 width = 12,
                                 fluidRow(
                                   column(width = 4, 
                                          uiOutput("active_heatmap_supp1"),
                                          flipBox(id = "fb_heatmap_supp1",
                                                  width = 12,
                                                  front = wellPanel(
                                                    style = "background: white",
                                                    h5(strong("Best Clustering Method"), 
                                                       align = "center"),
                                                    h6("Click for More Information"),
                                                    tableOutput("cluster_supp1")),
                                                  back = wellPanel(
                                                    p("The dend_expend() function in the dendextend package is used to determine the recommended clustering method to be used. The method with the highest optimum value is recommended.")))),
                                   column(width = 8, 
                                          uiOutput("active_heatmap_supp2"),
                                          flipBox(id = "fb_heatmap_supp2",
                                                  width = 12,
                                                  front = wellPanel(
                                                    style = "background: white",
                                                    h5(strong("Optimal No. of Clusters"),
                                                       align = "center"),
                                                    h6("Click for More Information"),
                                                    plotOutput("cluster_supp2")),
                                                  back = wellPanel(
                                                    p("The find_k() function in the dendextend package is used to determine the optimal number of clusters. Note that there could be discrepancy between the default number of clusters decided by the heatmaply() function used to plot the heatmap and the optimal number of clusters found by the find_k() function because different methods are used for determining the number of clusters. The heatmaply() function uses its internal algorithm, while the find_k function involves silhouette analysis.")
                                                  )
                                          )
                                   )
                                 )
                             )
                         )
                     )
              )
    ),
    
    ### Parallel Plot  ----------------------------------------------------
    tabItem(tabName = "tab_parallelplot",
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p(span(icon("users-viewfinder"), "Parallel Coordinates Plot"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = TRUE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "A Parallel Coordinates Plot is used to explore and understand clusters identified by the k-means algorithm, showing the similarities and differences within and between clusters."
                             )
                         )
                     ),
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p("Step 1: Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "scores_parallel",
                                   label = "Subject",
                                   choices = c("Math", "Reading", "Science"),
                                   multiple = TRUE,
                                   options = list(style = "myClass"),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 3)),
                                   inline = FALSE,
                                   width = NULL)
                             ),
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "vars_parallel",
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
                                   options = list(style = "myClass"),
                                   choicesOpt = list(style = rep_len("font-size: 10px;", 22)),
                                   inline = FALSE,
                                   width = NULL)
                             ),
                             div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                                 actionButton(inputId = "action_parallel_vars",
                                              label = "Confirm Selection",
                                              icon = icon("filter"),
                                              style = 'padding: 4px; font-size: 10px'),
                                 align = "center")
                         ),
                         box(title = tags$p("Step 2: Data Sampling", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 numericInput(inputId = "seed_parallel", 
                                              label = "Seed:", 
                                              value = 123,
                                              step = 1),
                                 selectInput(inputId = "groupby_parallel", 
                                             label = "Group by:", 
                                             choices = " "),
                                 sliderInput(inputId = "prop_parallel", 
                                             label = "Proportion:",
                                             min = 0.1, 
                                             max = 1.0, 
                                             value = 0.1, 
                                             step = 0.1),
                                 selectInput(inputId = "replace_parallel", 
                                             label = "Replace?",
                                             choices = c("False", "True")
                                 )
                             )
                         ),
                         box(title = tags$p("Step 3: Aesthetics", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 selectInput(inputId = "ref_col_parallel", 
                                             label = "Reference Column:",
                                             choices = " "),
                                 selectInput(inputId = "colscale_cont_parallel", 
                                             label = "Colour Scale (Continuous Data):",
                                             choices = c("Viridis", 
                                                         "Inferno", 
                                                         "Magma", 
                                                         "Plasma", 
                                                         "Warm", 
                                                         "Cool", 
                                                         "Rainbow", 
                                                         "CubehelixDefault",
                                                         "Blues", 
                                                         "Greens", 
                                                         "Greys", 
                                                         "Oranges",
                                                         "Purples", 
                                                         "Reds", 
                                                         "BuGn", 
                                                         "BuPu", 
                                                         "GnBu",
                                                         "OrRd",
                                                         "PuBuGn", 
                                                         "PuBu", 
                                                         "PuRd", 
                                                         "RdBu",
                                                         "RdPu", 
                                                         "YlGnBu", 
                                                         "YlGn", 
                                                         "YlOrBr", 
                                                         "YlOrRd")
                                 ),
                                 selectInput(inputId = "colscale_cat_parallel", 
                                             label = "Colour Scale (Categorical Data):",
                                             choices = c("Category10", 
                                                         "Accent", 
                                                         "Dark2", 
                                                         "Paired",
                                                         "Set1")
                                 )
                             ),
                             div(style = "padding = 0em; margin-top: 0em; font-size: 10px;",
                                 actionButton(inputId = "action_parallel",
                                              label = "Run Analysis",
                                              icon = icon("gear"),
                                              style = 'padding: 4px; font-size: 10px'),
                                 align = "center")
                         )
                     )
                   )
              ),
              column(width = 10,
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100%",
                         box(title = tags$p("Plot", style = "font-weight: bold;"), 
                             collapsible = FALSE,
                             status = "primary",
                             width = 12,
                             parallelPlotOutput("parallelplot")
                         )
                     )
              )
            ),
    
    ### Latent Class Analysis  ----------------------------------------------------
    tabItem(tabName = "tab_lca",
            column(width = 2,
                   fluidRow(
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p(span(icon("users-viewfinder"), "Latent Class Analysis Bar Plot"), style = "font-weight: bold; color: #FFFFFF"),
                             status = "info",
                             collapsible = TRUE,
                             width = 12,
                             solidHeader =TRUE,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 "A Latent Class Analaysis (LCA) Bar Plot is used to show the distribution of categorical variables within each latent class. LCA is a statistical method used to identify unobserved (latent) classes within a population based on observed categorical variables. Each observation is probabilistically assigned to one of the latent classes. The bar plot helps to identify which categories are most prevalent or distinctive within each class."
                             )
                         )
                     ),
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100% ",
                         box(title = tags$p("Step 1: Variable Selection", style = "font-weight: bold;"),
                             status = "primary",
                             collapsible = TRUE,
                             width = 12,
                             div(style = "padding = 0em; margin-top: -0.5em; font-size: 10px;",
                                 pickerInput(
                                   inputId = "Subject_lca",
                                   label = "Subject",
                                   choices = c("Math", "Reading", "Science"),
                                   multiple = TRUE,
                                   options = list(style = "myClass"),
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
                                   options = list(style = "myClass"),
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
                     div(style = "padding = 0em; margin-left: 0em; margin-top: 3em; height: 100%",
                         box(title = tags$p("Latent Class Analysis", style = "font-weight: bold;"), 
                             collapsible = FALSE,
                             status = "primary",
                             width = 12,
                             plotlyOutput("lca")
                         )
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
                             collapsible = TRUE,
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
                         div(style = "padding = 0em; margin-top: 1em; margin-left: 0em;",
                             flipBox(id = "dt_cp_info",
                                     width = 12,
                                     front = valueBoxOutput("dt_showcp_", width =12),
                                     back = tags$p("Complexity Parameter is used to control the size of the decision tree and to select the optimal tree size. The tree will stop dividing nodes when the reduction in relative error is less than a certain value.")
                             ))
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
                             collapsible = TRUE,
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
                             collapsible = TRUE,
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
  
  ### EDA/CDA  ----------------------------------------------------
  
    output$EDA_Plot1 <- renderPlot({
      selected_subject <- input$cda_Subject
      
      dataset <- stu %>%
        dplyr::select(c(selected_subject)) %>%
        rename(var1= selected_subject)
      
      set.seed(1234)
      
      gghistostats(
        data= dataset,
        x = var1,
        mean.ci = input$`cda_slider`,
        xlab =  paste0("Acdemic performance of", input$cda_Subject),
        centrality.type = input$`cda_test_type`,
        binwidth = input$`cda_slider_bin`,
        normal.curve = input$`cda_Normal.curve`,
        effsize.type = input$`cda_effsize.type`,
        centrality.plotting = input$`cda_Centrality.plotting`
    )
    
    
  })
  
  output$EDA_Plot2 <- renderPlot({
    ggbarstats(
      stu, 
      x = !!sym(input$cda_Variables), ## HOW TO ENSURE USERS DONT CLICK BOTH SAME VARIABLE
      y = !!sym(input$cda_Variables_2)
    )
    
    
  })
  

  output$CDA_Plot1 <- renderPlot({

    var <- input$cda_Variables
    subject <- input$cda_Subject
    
    dataset <- stu %>%
      dplyr::select(c(var,subject)) %>%
      rename(variable = var,
             score = subject)
    
    ggbetweenstats(
      data = dataset,
      x = variable, 
      y = score,
      type = input$`cda_test_type`,
      mean.ci = input$`cda_slider`, 
      pairwise.comparisons = input$`cda_Pairwise_Comparsions`, 
      pairwise.display = input$`cda_Pairwise_Display`,
      p.adjust.method = input$`cda_P.adjust_method`,
      messages = FALSE,
      title = paste0("Differece of ",input$cda_Variables, "on Academic Performance")
    )
    
  })
  
  
  ### Cluster Heatmap  ----------------------------------------------------
  
  # Combine Selected Variables and SchoolID
  selected_variables_heatmap <- eventReactive(
    input$action_heatmap_vars, {
      c('SchoolID', 
        input$scores_heatmap,
        input$vars_heatmap)
    }
  )
  
  # Combine Selected Variables (excl. Scores) and SchoolID
  selected_variables_heatmap_less <- eventReactive(
    input$action_heatmap_vars, {
      c('SchoolID', 
        input$vars_heatmap)
    }
  )
  
  # Update "group_by" Option with Selected Variables
  vars_names <- c("SchoolID" = "School ID",
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
  
  observeEvent(input$action_heatmap_vars, {
    choices_original <- selected_variables_heatmap_less()
    choices_display <- choices_original
    
    for (var in names(vars_names)) {
      index <- which(choices_original == var)
      if (length(index) > 0) {
        choices_display[index] <- vars_names[var]
      }
    }
    
    updateSelectInput(
      session = getDefaultReactiveDomain(), 
      inputId = "groupby_heatmap", 
      choices = setNames(choices_original, choices_display)
    )
  })
  
  # Filter Dataset using Selected Variables
  data_filtered_heatmap <- eventReactive(
    input$action_heatmap_vars, {
      data_filtered_heatmap <- stu %>% 
        dplyr::select(all_of(selected_variables_heatmap()))})
  
  # Data Sampling
  data_sample_heatmap <- eventReactive(
    input$action_heatmap, {
      set.seed(input$seed_heatmap)
      data_filtered_heatmap() %>%
        dplyr::group_by_at(vars(input$groupby_heatmap)) %>% 
        slice_sample(prop = input$prop_heatmap, 
                     replace = (if (input$replace_heatmap == "False") {FALSE} 
                                else {TRUE})) %>%
        dplyr::ungroup()
    }
  )
  
  # Convert Sampled Dataset to Matrix
  heatmap_data <- eventReactive(
    input$action_heatmap, {
      data.matrix(data_sample_heatmap())
    }
  )
  
  # Generate Plot: Cluster Heatmap
  heatmap_plot <- eventReactive(
    input$action_heatmap, {
      if (input$data_transformation_heatmap == "Scale by Column") {
        heatmaply(heatmap_data(),
                  scale = "column",
                  dist_method = input$dist_method_heatmap,
                  hclust_method = input$hclust_heatmap,
                  k_row = (if (input$clusters_heatmap == "Auto") {NA}
                           else {input$numclusters_heatmap}),
                  seriate = input$seriation_heatmap)
      } 
      else if (input$data_transformation_heatmap == "Scale by Row") {
        heatmaply(heatmap_data(),
                  scale = "row",
                  dist_method = input$dist_method_heatmap,
                  hclust_method = input$hclust_heatmap,
                  k_row = (if (input$clusters_heatmap == "Auto") {NA}
                           else {input$numclusters_heatmap}),
                  seriate = input$seriation_heatmap)
      } 
      else if (input$data_transformation_heatmap == "Normalise") {
        heatmaply(normalize(heatmap_data()),
                  scale = "none",
                  dist_method = input$dist_method_heatmap,
                  hclust_method = input$hclust_heatmap,
                  k_row = (if (input$clusters_heatmap == "Auto") {NA} 
                           else {input$numclusters_heatmap}),
                  seriate = input$seriation_heatmap) 
      } 
      else if (input$data_transformation_heatmap == "Percentise") {
        heatmaply(percentize(heatmap_data()),
                  scale = "none",
                  dist_method = input$dist_method_heatmap,
                  hclust_method = input$hclust_heatmap,
                  k_row = (if (input$clusters_heatmap == "Auto") {NA} 
                           else {input$numclusters_heatmap}),
                  seriate = input$seriation_heatmap)
      }
    }
  )
  
  # Function to Render Cluster Heatmap
  cluster_heatmap = function(modeltype){
    output$cluster_heatmap = renderPlotly(modeltype)}
  
  observeEvent(input$action_heatmap, cluster_heatmap(heatmap_plot()))
  
  # FlipBox: Heatmap Supplementary Visualisation 1
  output$active_heatmap <- renderUI({
    side <- if (input$fb_heatmap_supp1) " " else " "})
  
  # Heatmap Supplementary Visualisation 1
  heatmap_supp1_plot <- eventReactive(
    input$action_heatmap, {
      heatmap_data <- heatmap_data()
      if (input$data_transformation_heatmap == "Scale by Column") {
        dplyr::select(dend_expend(dist(heatmap_data),
                                  scale = "column",
                                  method = input$dist_method_heatmap)[[3]], 2:3) 
      } 
      else if (input$data_transformation_heatmap == "Scale by Row") {
        dplyr::select(dend_expend(dist(heatmap_data),
                                  scale = "row",
                                  method = input$dist_method_heatmap)[[3]], 2:3)
      } 
      else if (input$data_transformation_heatmap == "Normalise") {
        dplyr::select(dend_expend(dist(normalize(heatmap_data)),
                                  method = input$dist_method_heatmap)[[3]], 2:3)
      } 
      else if (input$data_transformation_heatmap == "Percentise") {
        dplyr::select(dend_expend(dist(percentize(heatmap_data)),
                                  method = input$dist_method_heatmap)[[3]], 2:3)
      }
    }
  )
  
  # Function to Render Heatmap Supplementary Visualisation 1
  cluster_supp1 = function(modeltype){
    output$cluster_supp1 = renderTable(modeltype)}
  
  observeEvent(input$action_heatmap, cluster_supp1(heatmap_supp1_plot()))
  
  # FlipBox: Heatmap Supplementary Visualisation 2
  output$active_heatmap <- renderUI({
    side <- if (input$fb_heatmap_supp2) " " else " "})
  
  # Heatmap Supplementary Visualisation 2
  heatmap_supp2_plot <- eventReactive(
    input$action_heatmap, {
      heatmap_data <- heatmap_data()
      if (input$data_transformation_heatmap == "Scale by Column") {
        plot(find_k(hclust(dist((heatmap_data),
                                method = input$dist_method_heatmap),
                           method = input$hclust_heatmap)))
      } 
      else if (input$data_transformation_heatmap == "Scale by Row") {
        plot(find_k(hclust(dist((heatmap_data),
                                method = input$dist_method_heatmap),
                           method = input$hclust_heatmap)))
      } 
      else if (input$data_transformation_heatmap == "Normalise") {
        plot(find_k(hclust(dist(normalize(heatmap_data),
                                method = input$dist_method_heatmap),
                           method = input$hclust_heatmap)))
      } 
      else if (input$data_transformation_heatmap == "Percentise") {
        plot(find_k(hclust(dist(percentize(heatmap_data),
                                method = input$dist_method_heatmap),
                           method = input$hclust_heatmap)))
      }
    }
  )
  
  # Function to Render Heatmap Supplementary Visualisation 2
  cluster_supp2 = function(modeltype){
    output$cluster_supp2 = renderPlot(modeltype)}
  
  observeEvent(input$action_heatmap, cluster_supp2(heatmap_supp2_plot()))
  
  ### Parallel Plot  ----------------------------------------------------
  
  # Combine Selected Variables and SchoolID
  selected_variables_parallel1 <- eventReactive(
    input$action_parallel_vars, {
      c('SchoolID', 
        input$scores_parallel,
        input$vars_parallel)
    }
  )
  
  # Combine Selected Variables (excl. Scores) and SchoolID
  selected_variables_parallel1_less <- eventReactive(
    input$action_parallel_vars, {
      c('SchoolID', 
        input$vars_parallel)
    }
  )
  
  # Define Selected Variables (without SchoolID)
  selected_variables_parallel2 <- eventReactive(
    input$action_parallel_vars, {
      c(input$scores_parallel,
        input$vars_parallel)
    }
  )
  
  # Update "group_by" Option with Selected Variables
  observeEvent(input$action_parallel_vars, {
    choices_original <- selected_variables_parallel1_less()
    choices_display <- choices_original
    
    for (var in names(vars_names)) {
      index <- which(choices_original == var)
      if (length(index) > 0) {
        choices_display[index] <- vars_names[var]
      }
    }
    
    updateSelectInput(
      session = getDefaultReactiveDomain(), 
      inputId = "groupby_parallel", 
      choices = setNames(choices_original, choices_display)
    )
  }
  )
  
  # Update "Reference Column" Option with Selected Variables
  observeEvent(input$action_parallel_vars, {
    choices_original <- selected_variables_parallel2()
    choices_display <- choices_original
    
    for (var in names(vars_names)) {
      index <- which(choices_original == var)
      if (length(index) > 0) {
        choices_display[index] <- vars_names[var]
      }
    }
    
    updateSelectInput(
      session = getDefaultReactiveDomain(), 
      inputId = "ref_col_parallel", 
      choices = setNames(choices_original, choices_display)
    )
  }
  )
  
  # Filter Dataset using Selected Variables
  data_filtered_parallel <- eventReactive(
    input$action_parallel_vars, {
      data_filtered_parallel <- stu %>% 
        dplyr::select(all_of(selected_variables_parallel1()))
    }
  )
  
  # Data Sampling
  data_sample_parallel <- eventReactive(
    input$action_parallel, {
      set.seed(input$seed_parallel)
      data_filtered_parallel() %>%
        dplyr::group_by_at(vars(input$groupby_parallel)) %>% 
        slice_sample(prop = input$prop_parallel, 
                     replace = (if (input$replace_parallel == "False") {FALSE} 
                                else {TRUE})) %>%
        dplyr::ungroup() %>%
        dplyr::select(-"SchoolID")
    }
  )
  
  # Create List of Lists of Selected Variables from "cat" List 
  lst <- eventReactive(
    input$action_parallel, {
      cols <- colnames(data_sample_parallel())
      lst <- lapply(cols, function(col_name) {
        if (col_name %in% names(cat)) {
          cat[[col_name]]
        } 
        else {
          NULL
        }
      }
      )
    }
  )
  
  # Generate Plot: Parallel Plot
  parallelplot_plot <- eventReactive(
    input$action_parallel, {
      parallelPlot(data_sample_parallel(),
                   categorical = lst(),
                   rotateTitle = TRUE,
                   refColumnDim = input$ref_col_parallel,
                   continuousCS = input$colscale_cont_parallel,
                   categoricalCS = input$colscale_cat_parallel)
    }
  )
  
  # Function to Render Parallel Plot
  cluster_parallelplot = function(modeltype){
    output$parallelplot = renderParallelPlot(modeltype)}
  
  observeEvent(input$action_parallel, cluster_parallelplot(parallelplot_plot()))
  
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
    choices_original <- selected_variables_lca()
    choices_display <- choices_original
    
    for (var in names(vars_names)) {
      index <- which(choices_original == var)
      if (length(index) > 0) {
        choices_display[index] <- vars_names[var]
      }
    }
    
    updateSelectInput(
      session = getDefaultReactiveDomain(), 
      inputId = "groupby_lca", 
      choices = setNames(choices_original, choices_display)
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
      model_lca <- poLCA(f, 
                         data_filtered_lca(), 
                         nclass=input$numclusters_lca, 
                         nrep= input$numrep_lca, 
                         maxiter=5000)
      bic_lca <- model_lca$bic
      aic_lca <- model_lca$aic
      entropy <- function(p) sum(-p*log(p))
      error_prior <- entropy(model_lca$P)
      error_post <- mean(apply(model_lca$posterior, c(1,2), entropy), na.rm=T)
      entropy_lca <- (error_prior - error_post) / error_prior
      return(model_lca)
    }
  )
  
  
  #bic_lca <- model_lca$bic
  #aic_lca <- model_lca$aic
  #entropy <- function(p) sum(-p*log(p))
  #error_prior <- entropy(model_lca$P)
  #error_post <- mean(apply(model_lca$posterior, c(1,2), entropy), na.rm=T)
  #entropy_lca <- (error_prior - error_post) / error_prior
  
  # Generate Plot Parallel Plot
  lca_plot <- eventReactive(
    input$action_lca, {
      model_lca <- model_lca()
      data_filtered_lca_plot <- data_filtered_lca()
      data_filtered_lca_plot$class <- model_lca$predclass
      data_filtered_lca_plot$class <- factor(data_filtered_lca_plot$class)
      
      selected_var <- as.symbol(input$groupby_lca)
      
      plot_table <- data_filtered_lca_plot %>%
        dplyr::group_by(!!selected_var, class) %>% 
        summarise(counts = n()) %>% 
        dplyr::ungroup()
      
      p <- ggplot(plot_table, 
                  aes(fill = !!selected_var, 
                      y = counts, 
                      x = class)) +
        geom_bar(position = "fill", stat = "identity") +
        labs(y = "Proportion", x = "Class")+ 
        theme_minimal()+
        theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              axis.line.y = element_blank(),
              axis.ticks.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
              plot.title = element_text(size=12, face="bold"),
              axis.title = element_text(face="bold"),
              axis.text = element_text(face="bold")) 
      
      ggplotly(p)
    }
  )
  
  ### Function to Plot Cluster Heatmap
  cluster_lca = function(modeltype){
    output$lca = renderPlotly(modeltype)}
  
  observeEvent(input$action_lca, cluster_lca(lca_plot()))
  
  
  # Regression Analysis ----------------------------------------------------
    # DT Data Manipulation  ----------------------------------------------------

  # Combine Selected Variables
  dt_selected_variables <- reactive({
      c(input$dt_target_,
        input$dt_vars_)
      })  
  
  dt_data <- reactive({
      stu_mb %>%
        dplyr::select(all_of(dt_selected_variables()))
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
        subtitle = tags$p("Best Complexity Parameter (Click for more)", style = "font-style: italic; font-size: 80%"),
        icon = tags$i(icon("trophy"), style="font-size: 70%"),
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
      dplyr::select(all_of(rf_selected_variables()))
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
      dplyr::select(all_of(gb_selected_variables()))
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
