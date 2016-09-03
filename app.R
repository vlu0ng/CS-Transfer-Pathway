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
library(shinyjs)

foradmission <- read_csv("foradmission.csv")
formajor <- read_csv("formajor.csv")

campuses <- list('Click to select'= "", #Placeholder prompt
                 'UC Berkeley' = c(`Berkeley - CS, B.S.` = 'bcsbs'), 
                 'UC Davis'    = c(`Davis - CS, B.S.` = 'dcsbs'),
                 'UC Irvine'   = c(`Irvine - CS, B.S.` = 'icsbs'),
                 'UC Merced'   = c(`Merced - CS, B.S.` = 'mcsbs'),
                 'UC Riverside'     = c(`Riverside - CS, B.S.` = 'rcsbs'),
                 'UC Santa Barbara' = c(`Santa Barbara - CS, B.S.` = 'sbcsbs'),
                 'UC Santa Cruz'    = c(`Santa Cruz - CS, B.S.` = 'sacsbs'),
                 'UC San Diego'     = c(`San Diego - CS, B.S.` = 'sdcsbs', 
                                        `San Diego - CS, B.A.` = 'sdcsba'),
                 'UC Los Angeles'   = c(`Los Angeles - CS, B.S.` = 'lacsbs', 
                                        `Los Angeles - CSE, B.S.` = 'lacsebs'))


ui <- dashboardPage(
  
  dashboardHeader(title = "UC Transfer Pathways"),
    
  dashboardSidebar(
    
    shinyjs::useShinyjs(),
    
    inlineCSS( #Inline CSS for buttons and inputs
      list(.red    = "font-weight: normal; padding-left: 1px",
           .orange = "font-weight: normal; padding-left: 42px")),
    
    sidebarMenu(
      
      menuItem("COMPUTER SCIENCE", tabName = "compsci", icon = icon("desktop")),
      
      id = "sidebarInput",
        selectInput("programInput", 
          HTML("<i>A Transfer Pathway is available for Computer Science because it
               is one of the UC's top majors. The pathway is a single set of courses students
               can take to prepare for admission to any of UC's nine undergraduate
               campuses.<br><br>
               
               Use this tool to tailor your coursework to the specific
               admission requirements for the programs and campuses you are interested in.
               The dropdown menu below allows you to make multiple selections.
               </i><br><br>"), 
          choices = campuses, selected = "", multiple = TRUE),
      
        column(3, div(id = "submit.button", 
          actionButton("submitInput","Submit", icon = icon("send"), width = '96px'))),
        
        column(3, div(id = "reset.button", 
          actionButton("resetInput", "Reset", icon = icon("refresh"), width = '96px')))
      
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
        tags$b("Tools to Help You Prepare"), 
        HTML("<h5>Use <a href='http://www.assist.org/web-assist/welcome.html'>
             <b>ASSIST</b></a> to find the specific courses classes offered at 
             your community colleges that will satisfy the expected coursework.<br><br>
             In addition to the specific coursework above, you will need to fulfill 
             <a href = 'http://bit.ly/2bxe4Li'><b>minimum requirements</b></a> 
             expected of all transfer applicants to UC.</h5>"),
        color = ("light-blue"), fill = FALSE, icon = icon("wrench")),
    
      infoBox(
        tags$b("Associate Degree for Transfer"), 
        HTML("<h5>If you're working on an Associate Degree for Transfer (ADT) 
             in computer science at your community college with the goal of 
             applying to CSU as well as UC, there's a lot of overlap in coursework. 
             However, the UC's expect computer science transfer students to take 
             multivariable calculus, linear algebra, and differential equations
             while the CSU's dont’t.</h5>"),
        color = ("olive"), fill = FALSE, icon = icon("exchange")),
      
      infoBox(
        tags$b("Campus GPA Requirements"), 
        HTML("<h5>Admission to different UC campuses and majors varies in competitiveness 
             depending on how many students apply and how many slots are available. As a 
             result, the minimum GPA and grade requirements for particular courses varies 
             from campus to campus. Please look on the <a href = 'http://bit.ly/1GIdBn6'>
             <b>campus admissions</b></a> websites to find minimum expected GPA.</h5>"),
        color = ("light-blue"), fill = FALSE, icon = icon("mortar-board"))
    ), 
    
    fluidRow(
      
      infoBox(
        tags$b("Make the Most of Your Time"), 
        HTML("<h5>All CS programs require calculus, so consider 
             using a calculus course to fulfill the mathematical 
             concepts and quantitative requirement.</h5>"),
        color = ("light-blue"), fill = TRUE, icon = icon("clock-o")),
      
      infoBox(
        tags$b("General Education Requirements"), 
        HTML("<h5>Use this tool to evaluate whether you should prioritize
             IGETC or other general education courses in choosing your
             tranfer prepration coursework.</h5>"),
        color = ("olive"), fill = TRUE, icon = icon("university")),
      
      infoBox(
        tags$b("Programming Languages"),
        HTML("<h5>Specific programming languages are more 
             important at some campuses than others. See individual 
             campus websites for guidance.</h5>"),
        color = ("light-blue"), fill = TRUE, icon = icon("code"))
    )
))

server <- function(input, output) {
  
  subset.foradmission <- eventReactive(input$submitInput, { 
    tbl_df(foradmission) %>% 
    filter(ID %in% input$programInput) %>%
    group_by(Order, Courses) %>%
    summarize(Requirements = min(Toggle))
    
  })

  subset.formajor <- eventReactive(input$submitInput, {
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
         style = x ~ style(color = ifelse(x,"gray","green"), align = "l"),
         x ~ icontext(ifelse(x, "remove", "ok"), ifelse(x, "Not Required for Major", "Required for Major")))
    ))
  })
  
  observeEvent(input$resetInput, {
    shinyjs::reset("sidebarInput")
  })
  
  observe({
    toggleState("submitInput", !is.null(input$programInput) && input$programInput != "")
  })
  
  shinyjs::addClass("submit.button", "red")
  shinyjs::addClass("reset.button", "orange")

}

shinyApp(ui, server)