## app.R ##
# Author: Vinh Luong
# University of California Transfer Pathways for Computer Science
# Students will use the input

library(shiny)
library(shinydashboard)
library(DT)
library(formattable)
library(dplyr)
library(readr)
library(materializeR)
library(htmltools)
library(shinyjs)

foradmission <- read_csv("foradmission.csv")
formajor <- read_csv("formajor.csv")

campuses <- list('UC Berkeley' = c(`Berkeley,Computer Science, B.S.` = 'bcsbs'), 
                 'UC Davis'    = c(`Davis, Computer Science, B.S.` = 'dcsbs'),
                 'UC Irvine'   = c(`Irvine, Computer Science, B.S.` = 'icsbs'),
                 'UC Merced'   = c(`Merced, Computer Science, B.S.` = 'mcsbs'),
                 'UC Riverside'     = c(`Riverside, Computer Science, B.S.` = 'rcsbs'),
                 'UC Santa Barbara' = c(`Santa Barbara, Computer Science, B.S.` = 'sbcsbs'),
                 'UC Santa Cruz'    = c(`Santa Cruz, Computer Science, B.S.` = 'sacsbs'),
                 'UC San Diego'     = c(`San Diego, Computer Science, B.S.` = 'sdcsbs', 
                                        `San Diego, Computer Science, B.A.` = 'sdcsba'),
                 'UC Los Angeles'   = c(`Los Angeles, Computer Science, B.S.` = 'lacsbs', 
                                        `Los Angeles, Computer Science & Engineering, B.S.` = 'lacsebs'))

ui <- dashboardPage(
  
  dashboardHeader(title = "UC Transfer Pathways"),
    
  dashboardSidebar(
      
    shinyjs::useShinyjs(),
    
    inlineCSS(list(.red    = "font-weight: normal; padding-left: 20px; padding-right: 20px",
                   .blue   = "font-weight: normal; padding-left: 12px",
                   .green  = "font-weight: normal", 
                   .orange = "font-weight: normal; padding-left: 19px")),
    
    sidebarMenu(
      menuItem("Computer Science", tabName = "compsci", icon = icon("desktop")),
      fluidRow(div(id = "myapp",
        div(id = "program",
          selectizeInput("programInput", 
            HTML("<i>A transfer pathway is available for Computer Science since it
                  is one of the UC's top majors. Use this tool to tailor your coursework
                  to the specific admission requirements to each of 
                  UC's nine undergraduate campuses.<br><br>
    
                  The dropdown menu below allows you to make multiple selections.
                  Select ALL the programs you're interested in.</i><br><br>"), 
            choices = campuses, selected = "icsbs", multiple = TRUE,
            options = list(placeholder = "Make selections here"))),
        
          column(3, div(id = "submit.button", submitButton("Update"))), 
          column(3, div(id = "reset.button", actionButton("reset", "Reset"))) 
      ))
  )),
    
  dashboardBody(
    
    fluidRow(
      column(width = 6,
        box(title  = "Shared Transfer Pathway for Admission", 
            status = "primary", width  = NULL, solidHeader = TRUE,
            formattableOutput("admission.output"))),
      
      column(width = 6,
         box(title = "Additional Lower Division Courses Required for Majors", 
             status = "primary", width = NULL, solidHeader = TRUE,
             formattableOutput("major.output")))
    ),
      
    fluidRow(
      infoBox(
        tags$b("Make the Most of Your Time"), 
        tags$h5("All CS programs require calculus, so consider 
                using a calculus course to fulfill the mathematical 
                concepts and quantitative requirement."),
        color = ("light-blue"), fill = TRUE, icon = icon("clock-o")),
    
      infoBox(
        tags$b("General Education Requirements"), 
        tags$h5("Use this tool to evaluate whether you should prioritize
                IGETC or other general education courses in choosing your
                tranfer prepration coursework."),
        color = ("olive"), fill = TRUE, icon = icon("university")),
           
      infoBox(
        tags$b("Tools to Help You Prepare"), 
        tags$h5("Use ASSIST to look for courses to make the most of your time
                 and the Transfer Admission Planner to track and plan your coursework."),
        color = ("light-blue"), fill = TRUE, icon = icon("wrench"))
    ), 
    
    fluidRow(
      infoBox(
        tags$b("Links to Program Websites"), 
        selectizeInput("linkInput", NA, choices = campuses,
        multiple = TRUE, options = list(placeholder = 'Select your programs')),
        color = ("light-blue"), fill = FALSE, icon = icon("link")),
      
      infoBox(
        tags$b("Print the CS Transfer Pathway"),
        tags$head(tags$script(src = "message-handler.js")),
        actionButton("pring", "Print", width = NULL, align = 'center'),
        color = ("olive"), fill = FALSE, icon = icon("print")), 
      
      box(
        HTML("Testing")
      )
    
    )
))

server <- function(input, output) {
  
  subset.foradmission <- reactive({ # Reactive is used because of programInput
    tbl_df(foradmission) %>% 
      filter(ID %in% input$programInput) %>%
      group_by(Order, Courses) %>%
      summarize(Requirements = min(Toggle))
  })

  subset.formajor <- reactive({
    tbl_df(formajor) %>% 
      filter(ID %in% input$programInput) %>%
      group_by(Order, Courses) %>%
      summarize(Requirements = min(Toggle))
  })
  
  output$admission.output <- renderFormattable({
    
    formattable(subset.foradmission() [1:9, -1], list(
      
      Courses = formatter("span", 
          style = ~ style(color = ifelse(Requirements == "0", style(display = "block",
            border.radius = "4px", padding.right = "4px", padding.left = "4px",
            background.color = "orange", color = "white", font.weight = "bold"), NA))),
      
      Requirements = formatter("span",
           style = x ~ style(color = ifelse(x,"gray","green"), align = "l"),
           x ~ icontext(ifelse(x, "remove", "ok"), ifelse(x, "Not Required for Admission", "Required for Admission")))
      
      ))
  })
  
  output$major.output <- renderFormattable({
    
    formattable(subset.formajor() [1:9, -1], list(
      
      Courses = formatter("span", 
          style = ~ style(color = ifelse(Requirements == "0", style(display = "block", 
            border.radius = "4px", padding.right = "4px", padding.left = "4px",
            background.color = "orange", color = "white", font.weight = "bold"), NA))),
      
      Requirements = formatter("span",
         style = x ~ style(color = ifelse(x,"gray","green")),
         x ~ icontext(ifelse(x, "remove", "ok"), ifelse(x, "Not Required for Major", "Required for Major")))
    ))
  })
  
  shinyjs::addClass("myapp", "red")
  shinyjs::addClass("submit.button", "green")
  shinyjs::addClass("reset.button", "orange")

  #Update button toggles on if selections are made
  observe({
    toggleState("submit.button", !is.null(input$programInput) && input$programInput != "")
  })
  
  observeEvent(input$programInput, {
    reset("program")
  })
}

shinyApp(ui, server)