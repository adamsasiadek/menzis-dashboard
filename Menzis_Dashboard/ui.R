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
library(plotly)
library(leaflet)
library(DT)

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title = "Menzis Dashboard"),
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Globaal",
            tabName = "geodata",
            icon = icon("globe-europe")
        ),
        menuItem(
            "Gezondheidsmonitor",
            tabName = "gzmon",
            icon = icon("medkit")
        ),
        menuItem(
            "Postcode",
            tabName = "postcode",
            icon = icon("microscope")
        ),
        menuItem(
            "Exploratie",
            tabName = "explore",
            icon = icon("wpexplorer")
        )
    )),
    dashboardBody(withMathJax(),
                  tabItems(
        tabItem(tabName = 'geodata',
                fluidRow(
                    box(leafletOutput(
                        'geo_gemeenten', width = "100%", height = 800
                    )),
                    box(
                        title ="",
                        sliderInput(
                            label = h3("Jaar"),
                            inputId = 'geo_JAAR',
                            min = 2015,
                            max = 2018,
                            value = 2015,
                            step = 1,
                            sep = "",
                            ticks = TRUE
                            
                        ),
                        checkboxGroupInput(
                            'geo_GESLACHT',
                            h3("Geslacht"),
                            choices = list('Man' = 'M',
                                           'Vrouw' = 'V'),
                            selected = 'M'
                        ),
                        checkboxGroupInput(
                            'geo_kostentype',
                            h3("Type Kosten"),
                            choices = list(
                                'Relatieve Kosten Generalistische GGZ' = "RELATIEVE_GENERALISTISCHE_BASIS_GGZ",
                                'Relatieve Kosten Specialistische GGZ' = "RELATIEVE_SPECIALISTISCHE_GGZ",
                                'Relatieve Kosten Langdurige GGZ' = "RELATIEVE_LANGDURIGE_GGZ"
                            ),
                            selected = "RELATIEVE_GENERALISTISCHE_BASIS_GGZ"
                        )
                        
                        
                    )
                ),
                fluidRow(box(
                    plotlyOutput('geo_plotly')
                ))),
        tabItem(tabName = 'gzmon',
                fluidRow(box(
                    title = "",
                    plotlyOutput('gzmon_plotly_corr')
                )),
                box(title = "",
                    plotlyOutput('gzmon_mse_plot'))),
        tabItem(tabName = 'postcode',
                fluidRow(
                    box("",
                        plotlyOutput('pp3_corr_plot')),
                    box("",
                        DT::dataTableOutput('pp3_coef_table'))
                ),
                fluidRow(
                    box("",
                        plotlyOutput('pp3_effect_plot')),
                    box("",
                        plotlyOutput('pp3_effect_plot2'))
                ),
        fluidRow(box(
            "",
            DT::dataTableOutput('pp3_diff_pred_real_table')
        ),
        box(
         h4(uiOutput('pp3_reg_text_formule'), align = 'center'),
         h4(uiOutput('pp3_reg_text_getallen'), align = 'center'),
          sliderInput(
            label = 'P_vrouw',
            inputId = 'P_Vrouw',
            min = 0,
            max = 100,
            value = 50,
            step = 5,
            sep = "",
            ticks = TRUE
            
          ),
          sliderInput(
            label = 'P_tothh_eenp',
            inputId = 'P_Tothh_eenp',
            min = 0,
            max = 100,
            value = 50,
            step = 5,
            sep = "",
            ticks = TRUE
            
          ),
          sliderInput(
            label = 'P_uitkminaow',
            inputId = 'P_Uitkminaow',
            min = 0,
            max = 100,
            value = 50,
            step = 5,
            sep = "",
            ticks = TRUE
            
          ),
          sliderInput(
            label = 'P_link_hh',
            inputId = 'P_Link_hh',
            min = 0,
            max = 100,
            value = 50,
            step = 5,
            sep = "",
            ticks = TRUE
            
          ),
        )
        )),
        tabItem(tabName = 'explore',
                fluidRow(box(
                    "",
                    plotlyOutput('exp_park_plantsoen')
                ),
                box("", plotlyOutput('exp_sportter')),
                box("", plotlyOutput('exp_10_kwal_analyse_plot'))))
    )))

