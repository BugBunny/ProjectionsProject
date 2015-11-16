# Data Products - Course Project: Population projections & pyramids
# RScript: server.R - Ian, 16 November 2015
#
# Performs a population projection that depends on assumptions about
# future total fertility determined by the user and then produces
# an interactive population pyramid for a year chosen by them.
#
# Requires shiny and rCharts (HighCharts library) anmd reshape2.
#
# The formatting of the pyramid uses code developed by Kyle Walker:
# http://walkerke.github.io/2014/06/rcharts-pyramids/)
#

library(shiny)
require(rCharts)
require(reshape2)

# asfr - function to split the TFR into 7 age-specific fertility rates
asfr <- function(tfr) tfr * c(0.043, 0.168, 0.269, 0.304,
                              0.174, 0.037, 0.003)

# project_step - function to project a population 5 years forward
#   inPop - initial population for 20 five-year age groups by sex
#   Px - five-year surviviorship ratios for the projection interval
#   fx - 5-year age-specific fertility rates for the projection interval
#   migrants - number of net in-migrants during the projection interval
project_step <- function(inpop, Px, fx, migrants) {
   outpop <- inpop
   # Survive those aged <90 at the outset and add in the migrants
   outpop[2:19, ] <- inpop[1:18, ] * Px[2:19, ] + migrants[2:19, ]
   # Add the surviving centenarians back into the 95+ age group
   outpop[20, ] <- (inpop[19, ] + inpop[20, ])*Px[20, ] + migrants[20, ]
   # Calculate female births assuming a sex ratio at birth of 1.06
   outpop[1, 2] <- sum(fx * (inpop[4:10, 2] + outpop[4:10, 2]) /2) /2.06
   # and the baby boys
   outpop[1, 1] <- outpop[1, 2] * 1.06
   # Survive the births into the 0-4 age group anmd add m,igranmts
   outpop[1, ] <- outpop[1, ] * Px[1, ] + migrants[1, ]
   return(outpop)
}
# Define the initial population and dynamics (Input data from
# www.ons.gov.uk/ons/rel/npp/national-population-projections/index.html)
Men <- c(2059, 2021, 1813, 1962, 2203, 2240, 2179, 2029, 2124, 2280,
         2249, 1949, 1713, 1754, 1295, 992, 679, 368, 137, 26)
Women <- c(1959, 1928, 1730, 1858, 2117, 2215, 2203, 2046, 2169,
           2346, 2312, 1999, 1785, 1858, 1428, 1170, 905, 603, 304, 80)
initPop <- data.frame(cbind(Men, Women))
Survivorship <- read.csv("survivorship.csv")
NetMigrants = c(23.0, 13.4, 15.5, 88.8, 183.4, 93.2, 26.1, -1.1, -13.5,
                6.4, 1.0, -3.9, 0.4, 0.8, 0.6, 0.8, 0.4, 0.2, 0.0, 0.0,
                18.5, 13.9, 9.6, 87.9, 164.8, 51.3, 21.7, 1.8, 0.9, 3.0,
                4.4, 1.7, 3.0, 3.0, 2.0, 1.2, 0.5, 0.2, 0.0, 0.0)
dim(NetMigrants) <- c(20,2)

shinyServer(function(input, output) {
   # The projection is done using reactive functions so that it is only
   # recalculated when total fertility is changed, not every time the
   # population pyramid is redrawn

   # Assemble 35-years worth of TFRs from input sliders and interpolation
   tfr <- reactive({
            c(as.numeric(input$TFR2015),
            (as.numeric(input$TFR2015) + as.numeric(input$TFR2025)) / 2,
            as.numeric(input$TFR2025),
            (as.numeric(input$TFR2025) + as.numeric(input$TFR2035)) / 2,
            as.numeric(input$TFR2035),
            (as.numeric(input$TFR2035) + as.numeric(input$TFR2045)) / 2,
            as.numeric(input$TFR2045))
   })
   # Do the 35-year population projection using the user-defined ASFRs
   projPop1 <- reactive({project_step(initPop, Survivorship[, 1:2],
      asfr(tfr()[1]), NetMigrants)})
#  Either the dumb programmer or shiny cannot do this in a loop. This is
#  probably because the reactive functions for each i get nested and 8
#  levels of nesting is too much for shiny. Or it may be that I'm dumb.
#     for (i in 2:8) {
#       assign(paste0("projPop", i), reactive({
#          project_step(eval(as.symbol(paste0("projPop", i-1)))(),
#             Survivorship, asfr(tfr()[i]), NetMigrants)
#          })
#       )
#    }
   projPop2 <- reactive({project_step(projPop1(), Survivorship[, 3:4],
      asfr(tfr()[2]), NetMigrants)})
   projPop3 <- reactive({project_step(projPop2(), Survivorship[, 5:6],
      asfr(tfr()[3]), NetMigrants)})
   projPop4 <- reactive({project_step(projPop3(), Survivorship[, 7:8],
      asfr(tfr()[4]), NetMigrants)})
   projPop5 <- reactive({project_step(projPop4(), Survivorship[, 9:10],
      asfr(tfr()[5]), NetMigrants)})
   projPop6 <- reactive({project_step(projPop5(), Survivorship[, 11:12],
      asfr(tfr()[6]), NetMigrants)})
   projPop7 <- reactive({project_step(projPop6(), Survivorship[, 13:14],
      asfr(tfr()[7]), NetMigrants)})

   # Render textual output line
   output$Year <- renderText({
      i <- match(input$Year, c("2015", "2020", "2025", "2030",
                               "2035", "2040", "2045", "2050"))
      totalPop <- round(sum(if (i==1) initPop
         else eval(as.symbol(paste0("projPop", i-1)))())/1000, 1)
      paste0("Selected year = ", input$Year, ". Total population = ",
            totalPop, " million")})
   # Render population pyramid using rCharts
   output$pyramid <- renderChart2({
      # Set up data for the pyramid for the year selected by user
      ageGp <- c(" 0- 4", " 5- 9", "10-14", "15-19", "20-24", "25-29",
                 "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                 "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                 "90-94", "95+")
      # rev as in reverse to get the kids at the bottom of the pyramid
      Age <- factor(ageGp, levels = rev(ageGp), labels = rev(ageGp))
      i <- match(input$Year, c("2015", "2020", "2025", "2030",
                               "2035", "2040", "2045", "2050"))
      chosenPop <- if (i==1) initPop
         else eval(as.symbol(paste0("projPop", i-1)))()
      # Melt data frame for selected year
      plotData <- melt(cbind(Age, chosenPop),
         variable.name = "Sex", id.vars = "Age",
         value.name = "Population")
      # Make values for men negative so that they plot to the left
      plotData$Population[plotData$Sex == "Men"] <-
         -plotData$Population[plotData$Sex == "Men"]
      # Draw plot using hPlot (Highcharts library)
      h <- hPlot(y = 'Population', x = 'Age', type = 'bar',
         data = plotData, group = 'Sex')
      h$plotOptions(series = list(stacking = 'normal',
         pointPadding = 0, borderWidth = 0))
      h$yAxis(labels = list(formatter = "#! function()
            {return(Math.abs(this.value));} !#"),
         title = list(enabled = TRUE, text = "Population ('000s)"))
      h$colors("steelblue", "plum")
      h$tooltip(formatter = "#! function() {return'<b>'+
         this.series.name +', Age '+
         this.point.category +'</b><br/>'+'<b>Population: </b>'+
         Highcharts.numberFormat(Math.abs(this.point.y), 0);
         } !#")
      return(h)
   })
})
# ProjectionsProject. Copyright (C) 2015 - Ian Timaeus.
# This program comes with ABSOLUTELY NO WARRANTY;
# this is free software, and you are welcome to redistribute it
# under certain conditions; see http://www.gnu.org/licenses/gpl.html
