#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

dropdownActionMenu <-
  function (...,
            title = NULL,
            icon = NULL,
            .list = NULL,
            header = NULL) {
    items <- c(list(...), .list)
    lapply(items, shinydashboard:::tagAssert, type = "li")
    type <- "notifications" # TODO créer action + CSS
    dropdownClass <- paste0("dropdown ", type, "-menu")
    tags$li(
      class = dropdownClass,
      a(
        href = "#",
        class = "dropdown-toggle",
        `data-toggle` = "",
        icon,
        title
      ),
      tags$ul(
        class = "dropdown-menu",
        if (!is.null(header))
          tags$li(class = "header", header),
        tags$li(tags$ul(class = "menu", items))
      )
      # ,
      # style = "padding: 5px;"
    )
  }

actionItem = function (inputId,
                       text,
                       icon = NULL,
                       tabSelect = FALSE) {
  if (!is.null(icon)) {
    shinydashboard:::tagAssert(icon, type = "i")
    icon <-
      tagAppendAttributes(icon, class = paste0("text-", "success"))
  }
  if (tabSelect) {
    tags$li(a(onclick = paste0("shinyjs.tabSelect('", inputId, "')"), icon, text))
  } else {
    tags$li(actionLink(inputId, text, icon))
  }
}

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    # useShinyjs(),
    # extendShinyjs(text="shinyjs.tabSelect=function(tabName){$('a[data-value='+tabName+']').click();}"),
    # put the shinyauthr logout ui module in here
    dashboardHeader(
      title = span(tags$img(src = "HP_2.png", height = '100%')),
      
      #   tags$a(
      #   "!",
      # tags$img(src='HP_2.png', height='35')),
      # dropdownActionMenu(title = strong(
      #   uiOutput("dd1")
      # )
      # ),
      # dropdownActionMenu(title = strong(
      #   uiOutput("dd2")
      # )
      # ),
      # dropdownActionMenu(title = strong(
      #   uiOutput("dd3")
      # )
      # ),
      # dropdownActionMenu(title = strong(
      #   uiOutput("dd4")
      # )
      # ),
      # tags$li(
      #   a(
      #     strong("Tab Title 1"),
      #     height = 30, 
      #     title = "",
      #   ),
      #   class = "dropdown"
      # ),
      # tags$li(
      #   a(
      #     strong("Tab Title 2"),
      #     height = 30, 
      #     title = "",
      #     target = "_blank"
      #   ),
      #   class = "dropdown"
      # ),
      # tags$li(
      #   a(
      #     strong("Tab Title 3"),
      #     height = 30, 
      #     title = "",
      #     target = "_blank"
      #   ),
      #   class = "dropdown"
      # ),
      # tags$li(
      #   a(
      #     strong("Tab Title 4"),
      #     height = 30, 
      #     title = "",
      #     target = "_blank"
      #   ),
      #   class = "dropdown"
      # ),
      # tags$li(
      #   div(
      #     href = 'http://highperformance.mx/',
      #     img(
      #       src = 'HP_2.png',
      #       title = "HP",
      #       height = "50"
      #     )
      #     # ,
      #     # style = "padding: 5px;"
      #   ),
      #   class = "dropdown"
      # )
      tags$li(
        class = "dropdown",
        # style = "padding: 5px;",
        shinyauthr::logoutUI("logout",
                             label = "Cerrar sesión",
                             style = "color: #b20b02")
      ),
      dropdownActionMenu(title=em(textOutput('tiempito'))
      )
    ),
    
    # setup a sidebar menu to be rendered server-side
    dashboardSidebar(# width = 100,
      collapsed = TRUE, sidebarMenuOutput("sidebar")),
    
    
    dashboardBody(
      #' tags$style(
      #'   type = "text/css",
      #'   "
      #' /*    Move everything below the header */
      #'     .content-wrapper {
      #'         margin-top: 0px;
      #'     }
      #'     .content {
      #'         padding-top: 140px;
      #'     }
      #' /*    Format the title/subtitle text */
      #'     .title-box {
      #'         position: absolute;
      #'         text-align: center;
      #'         top: 50%;
      #'         left: 50%;
      #'         transform:translate(-50%, -50%);
      #'     }
      #'     @media (max-width: 590px) {
      #'         .title-box {
      #'             position: absolute;
      #'             text-align: center;
      #'             top: 10%;
      #'             left: 10%;
      #'             transform:translate(-5%, -5%);
      #'         }
      #'     }
      #'     @media (max-width: 767px) {
      #'         .primary-title {
      #'             font-size: 1.1em;
      #'         }
      #'         .primary-subtitle {
      #'             font-size: 1em;
      #'         }
      #'     }
      #' /*    Make the image taller */
      #'     .main-header .logo {
      #'         height: 100px;
      #'     }
      #' /*    Override the default media-specific settings */
      #'     @media (max-width: 5000px) {
      #'         .main-header {
      #'             padding: 0 0;
      #'             position: relative;
      #'         }
      #'         .main-header .logo,
      #'         .main-header .navbar {
      #'             width: 100%;
      #'             float: none;
      #'         }
      #'         .main-header .navbar {
      #'             margin: 0;
      #'         }
      #'         .main-header .navbar-custom-menu {
      #'             float: right;
      #'         }
      #'     }
      #' /*    Move the sidebar down */
      #'     .main-sidebar {
      #'         position: absolute;
      #'     }
      #'     .left-side, .main-sidebar {
      #'         padding-top: 175px;
      #'     }"
      #' ),
      shinyjs::useShinyjs(),
      
      # put the shinyauthr login ui module here
      shinyauthr::loginUI(
        "login",
        title = "Acceso al sistema",
        user_title = "Usuario",
        pass_title = "Contraseña",
        login_title = "Iniciar sesión",
        error_message = "Verifica tus credenciales",
        tags$head(tags$style(
          HTML(
            ".btn-primary {
                  color: #ffffff;
                  background-color: #b20b02;
                  border-color: #b20b02;
              }
              .panel-primary {
                  border-color: #b20b02;
              }"
          )
        ))
      ),
      
      # setup any tab pages you want after login here with uiOutputs
      tabItems(
        tabItem("tab0", uiOutput("tab0_ui")),
        tabItem("tab1", uiOutput("tab1_ui")),
        tabItem("tab2", uiOutput("tab2_ui")),
        tabItem("tab3", uiOutput("tab3_ui")),
        tabItem("tab4", uiOutput("tab4_ui")),
        tabItem("tab5", uiOutput("tab5_ui"))
      )
    ),
    md = TRUE,
    skin = "black"
  )
)