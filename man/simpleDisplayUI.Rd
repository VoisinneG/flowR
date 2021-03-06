% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simpleDisplay_module.R
\name{simpleDisplayUI}
\alias{simpleDisplayUI}
\alias{simpleDisplay}
\title{A shiny Module to display and save plots}
\usage{
simpleDisplayUI(id)

simpleDisplay(
  input,
  output,
  session,
  plot_list,
  params = reactiveValues(),
  save = TRUE,
  multirow = FALSE
)
}
\arguments{
\item{id}{shiny id}

\item{input}{shiny input}

\item{output}{shiny output}

\item{session}{shiny session}

\item{plot_list}{a reactivevalues object containing a plot or a list of plots}

\item{params}{reactivevalues object used to initialize plot parameters
with the following elements (not mandatory):
\describe{
  \item{use_plotly}{: use plotly library to render an interactive plot}
  \item{top}{: main title}
 }}

\item{save}{logical. Add buttons to save plot and plot data?}

\item{multirow}{logical. Allow plots to be displayed on multiple rows?}
}
\value{
a reactivevalues object with:
\describe{
  \item{plot}{: the plots displayed}
  \item{params}{: input parameters. These include events describing user interaction 
  with the plot such as:
    \describe{
      \item{plot_brush}{: plot brush events}
      \item{plot_click}{: plot click events}
      \item{plot_dblclick}{: plot double click events}
    }
  }
}
}
\description{
A shiny Module to display and save plots

display server function
}
\examples{
\dontrun{
library(shiny)
library(shinydashboard)
library(gridExtra)
library(ggplot2)
library(plotly)

if (interactive()){

  ui <- dashboardPage(
    dashboardHeader(title = "simpleDisplay"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      fluidRow(
        column(12, box(width = NULL, simpleDisplayUI("simple_display_module")))
      )
    )
  )

  server <- function(input, output, session) {

    params <- reactiveValues(top = "Iris", use_plotly = FALSE)

    plot_list <- reactive({

      plist <- list()

      plist[[1]] <- ggplot(iris, aes(x=Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point(alpha = 0.5) +
        facet_wrap(~Species)

       plist[[2]] <- ggplot(iris, aes(x=Species, y = Sepal.Length, fill = Species)) +
         geom_col(alpha = 0.5)

      return(plist)

    })

    callModule(simpleDisplay, "simple_display_module", 
               plot_list = plot_list, 
               params = params,
               size = 500)

  }

  shinyApp(ui, server)

}
}
}
