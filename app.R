## app.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(formattable)
library(dplyr)
library(readr)
library(shinyjs)

foradmission <- read_csv("foradmission.csv")
formajor <- read_csv("formajor.csv")
campuses <- list('Berkeley' = c(`Computer Science, B.S.` = 'bcsbs'), 
                 'Davis' = c(`Computer Science, B.S.` = 'dcsbs'),
                 'Irvine' = c(`Computer Science, B.S.` = 'icsbs'),
                 'Los Angeles' = c(`Computer Science, B.S.` = 'lacsbs', 
                                   `Computer Science & Engineering, B.S.` = 'lacsebs'),
                 'Merced' = c(`Computer Science, B.S.` = 'mcsbs'),
                 'Riverside' = c(`Computer Science, B.S.` = 'rcsbs'),
                 'San Diego' = c(`Computer Science, B.S.` = 'sdcsbs', 
                                 `Computer Science, B.A.` = 'sdcsba'),
                 'Santa Barbara' = c(`Computer Science, B.S.` = 'sbcsbs'),
                 'Santa Cruz' = c(`Computer Science, B.S.` = 'sacsbs'))

ui <- dashboardPage(
  
dashboardHeader(title = "UC Transfer Pathways"),
  
dashboardSidebar(
  
  shinyjs::useShinyjs(),
  
  inlineCSS(list(.red = "font-weight: normal; padding-left: 20px; padding-right: 20px",
                 .blue = "font-weight: normal; padding-left: 12px",
                 .green = "font-weight: normal", 
                 .orange = "font-weight: normal; padding-left: 19px")),
  
  sidebarMenu(
    menuItem("Computer Science", tabName = "compsci", icon = icon("desktop")),
    fluidRow(div(id = "myapp",
        div(id = "program",
          selectizeInput("programInput", 
            HTML("<i>As you decide which campuses to apply to, please use this tool
                  to tailor your coursework to the specific admission requirements
                  at those UC campuses.<br><br>

                  Use the dropdown menu below to select ALL the campuses and
                  programs you're interested in.</i><br><br>"), 
            choices = campuses, selected = "", multiple = TRUE,
            options = list(placeholder = "Make selections here"))),
        
          column(3, div(id = "subButton", submitButton("Update"))), 
          column(3, div(id = "action", actionButton("reset", "Reset")))
    ))
)),
  
  #Dashboard Body Outputs
  dashboardBody(
    
    # FormatTable Outputs 
    fluidRow(
      column(width = 6,
        box(title = "Shared Transfer Pathway for Admission", status = "primary",
            width = NULL, solidHeader = TRUE,
            formattableOutput("requiredadmissionOutput"))),
      
      column(width = 6,
         box(title = "Additional Lower Division Courses Required for Majors", status = "info", 
            width = NULL, solidHeader = TRUE,
            formattableOutput("requiredmajorOutput")))
    ),
      
    #infoBox: Make the Most of Your Time
    #infoBox: General Education Requirements
    #infoBox: Tools to Help You Prepare
    fluidRow(
        infoBox(tags$b("Make the Most of Your Time"), 
              tags$h5("All CS programs require calculus, so consider using a calculus
                     course to fulfill the mathematical concepts and quantitative
                     requirement."),
              color = ("light-blue"),
              fill = TRUE,
              icon = icon("clock-o")),
      
        infoBox(tags$b("General Education Requirements"), 
             tags$h5("Use this tool to evaluate whether you should prioritize
             IGETC or other general education courses in choosing your
             tranfer prepration coursework."),
             color = ("olive"),
             fill = TRUE,
             icon = icon("university")),
             
         infoBox(tags$b("Tools to Help You Prepare"), 
             tags$h5("Use ASSIST to look for courses to make the most of your time
                     and the Transfer Admission Planner to track and plan your coursework."),
             color = ("light-blue"),
             fill = TRUE,
             icon = icon("wrench"))
        
    ), #fluidRow end bracket
    
    #InfoBox: Links to Program Websites
    #InfoBox: Print CS Transfer Pathway
    fluidRow(
      infoBox(tags$b("Links to Program Websites"), 
              selectizeInput("linkInput", NA, choices = campuses,
              multiple = TRUE, 
              options = list(
                placeholder = 'Select your programs'
              )
              ),
              color = ("light-blue"),
              fill = FALSE,
              icon = icon("link")),
      
      infoBox(tags$b("Print the CS Transfer Pathway"),
              tags$head(tags$script(src = "message-handler.js")),
              actionButton("pring", "Print", width = NULL, align = 'center'),
              color = ("olive"),
              fill = FALSE,
              icon = icon("print"))
    )
  )
)

#Shiny Apps Server
server <- function(input, output) {
  
  #Subset of foradmission based on user input
  filtered_foradmission <- reactive({
    foradmission %>% 
      filter(ID == input$programInput) %>%
      arrange(Rank) %>%
      distinct(Courses)
  })
  
  #Subset of formajor based on user input
  filtered_formajor <- reactive({
    formajor %>% 
      filter(ID == input$programInput) %>%
      arrange(Rank)
  })
  
  output$requiredadmissionOutput <- renderFormattable({
    
    formattable(filtered_foradmission() [1:9,-c(1,2,3,4)], list(
      
      Courses = formatter("span", 
          style = ~ style(color = ifelse(Requirements == "TRUE", style(display = "block",
            border.radius = "4px", padding.right = "4px", padding.left = "4px",
            background.color = "orange", color = "white", font.weight = "bold"), NA))),
      
      Requirements = formatter("span",
           style = x ~ style(color = ifelse(x,"green","gray"), align = "l"),
           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Required for Admission", "Not Required for Admission")))
      
      ))
  })
  
  output$requiredmajorOutput <- renderFormattable({
    
    formattable(filtered_formajor() [1:9, -c(1,2,3,4)], list(
      
      Courses = formatter("span", 
          style = ~ style(color = ifelse(Requirements == "TRUE", style(display = "block", 
            border.radius = "4px", padding.right = "4px", padding.left = "4px",
            background.color = "orange", color = "white", font.weight = "bold"), NA))),
      
      Requirements = formatter("span",
         style = x ~ style(color = ifelse(x,"green","gray")),
         x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Required for Major", "Not Required for Major")))
    ))
  })
  
  #Berkeley
  shinyjs::onclick("berkeleyAdvanced",
    shinyjs::toggle(id = "berkeley", anim = TRUE))
  shinyjs::addClass("berkeleyAdvanced", "red")
  shinyjs::addClass("berkeley", "blue")
  shinyjs::addClass("myapp", "red")
  shinyjs::addClass("instructions", "red")
  shinyjs::addClass("subButton", "green")
  shinyjs::addClass("action", "orange")

  observe({
    toggleState("update", !is.null(input$programInput) && input$programInput != "")
  })
  
  observeEvent(input$programInput, {
    reset("myapp")
  })
}

shinyApp(ui, server)