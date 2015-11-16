# Data Products - Course Project: Population projections & pyramids
# RScript ui.R: Ian, 16 November, 2015
#
# Produces an interactive population pyramid that displays
# the results of a cohort-component population projection.
# Uses rCharts (with the HighCharts library) within shiny.
#

require(shiny)
require(rCharts)
lnk =
  "www.ons.gov.uk/ons/rel/npp/national-population-projections/index.html"

shinyUI(fluidPage(

  # Application title
   titlePanel("Population projections for the United Kingdom"),
  # Sidebar with a sliders to input total fertility and a dropdown box
   sidebarLayout(
      sidebarPanel(
         h4("Total fertility (children):"),
         sliderInput("TFR2015",
            h5("2015-20:"), min = 0.5, max = 4, value = 1.9, step=0.05),
         sliderInput("TFR2025",
            h5("2025-30:"), min = 0.5, max = 4, value = 1.9, step=0.05),
         sliderInput("TFR2035",
            h5("2035-40:"), min = 0.5, max = 4, value = 1.9, step=0.05),
          sliderInput("TFR2045",
            h5("2045-50:"), min = 0.5, max = 4, value = 1.9, step=0.05),
         selectInput("Year", label = h4("Draw pyramid for:"),
                     choices = list("2015" = 2015, "2020" = 2020,
                                    "2025" = 2025, "2030" = 2030,
                                    "2035" = 2035, "2040" = 2040,
                                    "2045" = 2045, "2050" = 2050),
                                    selected = 2015)
      ),
      # Display rubric, total population & the population pyramid in html
      mainPanel(
         tabsetPanel(tabPanel("Results",
            p("These projections forecast the UK population for the period
               2015 to 2050. They use official ", tags$a(href=lnk,
               "Office for National Statistics"), " assumptions for
               mortality and international migration."),
            p("By adjusting the sliders you can examine
               how higher or lower ", em("total fertility")," than the
               official assumption of an average of 1.9 children per woman
               affects the size and age structure of the population. You
               can view a population pyramid for the initial or middle year
               of each decade - select a year using the drop down box."),
            p("Placing the cursor over the bar for a particular age group
               in the pyramid will cause the projected size of that age
               group to appear in a pop-up box."),
            p("Higher fertility results in a larger population and a more
               broadly-based population pyramid. Lower fertility leads to
               less growth and undercutting of the population pyramid."),
            h3(textOutput("Year")),
            showOutput("pyramid", "highcharts")
         ),
         # More detailed documentation on a separate tab
         tabPanel("Technical notes",
            p("This application performs what is termed in demography a
              cohort-component population projection. It uses the
              principal projected population for 2015 in the official
              2014-based projections as a base population but takes its
              assumptions from the 2012-based projections as ONS had yet to
              publish those that it used for the 2014-based projections
              early in November 2015."),
            p("The principal simplification is that the projection is
              undertake for 5-year age groups and steps, not single-year
              ones, as in the official projections. Also, the TFR can only
              be altered for alternate quinqennia and is interpolated for
              the intermediate ones to keep the interface simpler."),
            p("The assumptions about migration and age-specific fertility
              are held constant throughout the projection although the
              official projections run the current rates into these values
              during the first few years of the projection. Despite these
              differences in methods and assumptions, the output from these
              projections closely appoximates to that from the official
              ones."),
            p("The app is coded in the R package shiny. The population
              pyramid was drawn using rCharts. The code is available
              on Github at this address:", tags$a(href=
              "https://github.com/BugBunny/ProjectionsProject",
              "https://github.com/BugBunny/ProjectionsProject")),
            p("A five page presentation motivating the project is
               available on git.io:", tags$a(href=
              "http://bugbunny.github.io/ProjectionsProject.html",
              "http://bugbunny.github.io/ProjectionsProject.html")),
            p("Thanks to Kyle Walker for his ",
              tags$a(href="http://walkerke.github.io/2014/06/rcharts-pyramids/",
                  "article"), " on producing interactive population pyramids in
              rCharts."),
            p("ProjectionsProject. Copyright (C) 2015 - Ian Timaeus.
              This program comes with ABSOLUTELY NO WARRANTY; this is free
              software, and you are welcome to redistribute it under certain
              conditions; see ", tags$a(href=
              "http://www.gnu.org/licenses/gpl.html",
              "http://www.gnu.org/licenses/gpl.html"))
         ))
      )
   )
))
# ProjectionsProject. Copyright (C) 2015 - Ian Timaeus.
# This program comes with ABSOLUTELY NO WARRANTY;
# this is free software, and you are welcome to redistribute it
# under certain conditions; see http://www.gnu.org/licenses/gpl.html

