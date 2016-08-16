## app.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(formattable)
library(dplyr)
library(shinyjs)

foradmission <- read.csv("foradmission.csv", stringsAsFactors = FALSE)
formajor <- read.csv("formajor.csv", stringsAsFactors = FALSE)

ui <- dashboardPage(
  
dashboardHeader(title = "UC Transfer Pathways"),
  
dashboardSidebar(
    
    shinyjs::useShinyjs(),
    inlineCSS(list(.red = "font-weight: normal; padding-left: 20px ; padding-right: 20px",
                   .blue= "font-weight: normal; padding-left: 12px")),

    sidebarMenu(
      menuItem("Computer Science", tabName = "compsci", icon = icon("desktop")),
      menuItem(HTML("As you decide which campuses<br>
                 to apply to, please use this tool<br>
                 to tailor your coursework to the<br>
                 specific admission requirements<br>
                 at those UC campuses.")),
      
      fluidRow(
        div(id = "myapp",
          div(id = "program",
              selectizeInput("programInput", HTML("Use the dropdown menu below
                                               to select ALL the campuses and
                                               programs you're interested in:<br><br>"), 
                     choices = list('Berkeley' = c(`Computer Science, B.S.` = 'bcsbs'), 
                                    'Davis' = c(`Computer Science, B.S.` = 'dcsbs'),
                                    'Irvine' = c(`Computer Science, B.S.` = 'icsbs'),
                                    'Los Angeles' = c(`Computer Science, B.S.` = 'lacsbs', 
                                                      `Computer Science & Engineering, B.S.` = 'lacsebs'),
                                    'Merced' = c(`Computer Science, B.S.` = 'mcsbs'),
                                    'Riverside' = c(`Computer Science, B.S.` = 'rcsbs'),
                                    'San Diego' = c(`Computer Science, B.S.` = 'sdcsbs', 
                                                    `Computer Science, B.A.` = 'sdcsba'),
                                    'Santa Barbara' = c(`Computer Science, B.S.` = 'sbcsbs'),
                                    'Santa Cruz' = c(`Computer Science, B.S.` = 'sacsbs')),
                    selected = "bcsbs", 
                    multiple = TRUE,
                    options = list(placeholder = "Select program(s) here")
              )
          ),
          div(id = "buttons",
            actionButton("update", "Update"),
            actionButton("reset", "Clear Selections")
          )
        )
      )
      
  )),
  
  #Dashboard Body Outputs
  dashboardBody(
    
    # FormatTable Outputs 
    fluidRow(
      column(width = 6,
        box(
          title = "Shared Transfer Pathway for Admission", status = "primary",
          width = NULL, solidHeader = TRUE,
          formattableOutput("requiredadmissionOutput"))
      ),
      
      column(width = 6,
         box(
           title = "Additional Lower Division Courses Required for Majors", status = "info", 
           width = NULL, solidHeader = TRUE,
           formattableOutput("requiredmajorOutput"))
      )
    ), #fluidrow end bracket
      
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
              selectizeInput("linkInput", NA, choices = list(
                'Berkeley' = c(`Computer Science, B.S.` = 'BCSBS', `Computer Science & Engineering, B.S.` = 'BCSEBS'),
                'Davis' = c(`Computer Science, B.S.` = 'DCSBS'),
                'Irvine' = c(`Computer Science, B.S.` = 'ICSBS'),
                'Los Angeles' = c(`Computer Science, B.S.` = 'LACSBS', `Computer Science & Engineering, B.S.` = 'LACSEBS'),
                'Merced' = c(`Computer Science, B.S.` = 'MCSBS'),
                'Riverside' = c(`Computer Science, B.S.` = 'RCSBS'),
                'San Diego' = c(`Computer Science, B.S.` = 'SDACSBS', `Computer Science, B.A.` = 'SDCSBA'),
                'Santa Barbara' = c(`Computer Science, B.S.` = 'SBCSBS'),
                'Santa Cruz' = c(`Computer Science, B.S.` = 'SCCSBS')),
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
  shinyjs::addClass("buttons", "blue")

  observe({
    toggleState("update", !is.null(input$programInput) && input$programInput != "")
  })
  
  observeEvent(input$reset, {
    reset("myapp")
  })
}

shinyApp(ui, server)