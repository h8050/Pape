#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyauthr)
library(RSQLite)
library(DBI)
library(tidyverse)
library(shinydashboardPlus)
library(shinyWidgets)
library(fresh)
library(shinyjs)
library(colourpicker)

conn <- dbConnect(SQLite(), "users.sqlite")
credenciales <- tibble::tibble(
  user = dbGetQuery(conn, "select username from users")[[1]],
  # password = sapply(
  #   dbGetQuery(conn, "select password from users")[[1]],
  #   sodium::password_store
  # ),
  password = dbGetQuery(conn, "select password from users")[[1]],
  name = paste0(rep("User "), rep(1:length(user)))
)

mydb <- dbConnect(SQLite(), dbname="data_test.db")

dbListTables(mydb)

data_test <- fetch(dbSendQuery(mydb, "select * from data"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  creds <- shinyauthr::loginServer(
    id = "login",
    data = credenciales,
    user_col = user,
    pwd_col = password,
    # sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # logout status managed by shinyauthr module and stored here
  logout_init <- shinyauthr::logoutServer("logout",
                                          reactive((creds()$user_auth)))
  # callModule(shinyauthr::logoutServer, "logout", reactive(creds()$user_auth))
  
  # this opens or closes the sidebar on login/logout
  observe({
    if (creds()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  # Connect to database
  mydb <- reactive({
    mydb = dbConnect(SQLite(), dbname="cashiering_db.db")
  })
  
  # data Table
  data_test <- reactive({
    data_test = fetch(dbSendQuery(mydb(), "select * from items"))
  })
  
  # data Table
  data_test1 <- reactive({
    data_test1 = fetch(dbSendQuery(mydb(), "select * from product_list"))
  })
  
  # data Table
  data_test2 <- reactive({
    data_test2 = fetch(dbSendQuery(mydb(), "select * from sqlite_sequence"))
  })
  
  # data Table
  data_test3 <- reactive({
    data_test3 = fetch(dbSendQuery(mydb(), "select * from transaction_list"))
  })
  
  # data Table
  data_test4 <- reactive({
    data_test4 = fetch(dbSendQuery(mydb(), "select * from user_list"))
  })
  
  # only when credentials()$user_auth is TRUE, render your desired sidebar menu
  output$sidebar <- renderMenu({
    req(creds()$user_auth)
    sidebarMenu(
      id = "tabs",
      menuItem(text = div(
        tags$img(src = 'HP_2.png', width = "66%"), style = "text-align: center"
      )),
      menuItem("items", tabName = "tab0"),
      menuItem("product_list", tabName = "tab1"),
      menuItem("sqlite_sequence", tabName = "tab2"),
      menuItem("transaction_list", tabName = "tab3"),
      menuItem("user_list", tabName = "tab4"),
      menuItem("venta", tabName = "tab5")
    )
  })
  
  # tab 0 UI and output ----------------------------------------
  output$tab0_ui <- renderUI({
    req(creds()$user_auth)
    DT::DTOutput("table0")
  })
  
  output$table0 <- DT::renderDT({
    DT::datatable(data_test(), options = list(scrollX = TRUE))
  })
  
  # tab 1 UI and output ----------------------------------------
  output$tab1_ui <- renderUI({
    req(creds()$user_auth)
    DT::DTOutput("table1")
  })
  
  output$table1 <- DT::renderDT({
    DT::datatable(data_test1(), options = list(scrollX = TRUE))
  })
  
  # tab 2 UI and output ----------------------------------------
  output$tab2_ui <- renderUI({
    req(creds()$user_auth)
    DT::DTOutput("table2")
  })
  
  output$table2 <- DT::renderDT({
    DT::datatable(data_test2(), options = list(scrollX = TRUE))
  })
  # tab 3 UI and output ----------------------------------------
  output$tab3_ui <- renderUI({
    req(creds()$user_auth)
    DT::DTOutput("table3")
  })
  
  output$table3 <- DT::renderDT({
    DT::datatable(data_test3(), options = list(scrollX = TRUE))
  })
  
  # tab 4 UI and output ----------------------------------------
  output$tab4_ui <- renderUI({
    req(creds()$user_auth)
    DT::DTOutput("table4")
  })
  
  output$table4 <- DT::renderDT({
    DT::datatable(data_test4(), options = list(scrollX = TRUE))
  })
  
  # tab 5 UI and output ----------------------------------------
  output$tab5_ui <- renderUI({
    req(creds()$user_auth)
    # DT::DTOutput("table5")
  })
  
  # output$table4 <- DT::renderDT({
  #   DT::datatable(data_test4(), options = list(scrollX = TRUE))
  # })
  
  output$tiempito <- renderText({
    # EL req() se puede usar para mostrarlo después de iniciar sesión o siempre
    # req(creds()$user_auth)
    invalidateLater(60000, session)
    paste(format(Sys.time(), "%R"))
  })
  
})
