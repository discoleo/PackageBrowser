

### UI Functions

init.theme = function() {
    bs_theme(
        bg = "#101010", 
        fg = "#FDF7F7", 
        primary = "#ED79F9"
    );
}

### Shiny functions
# Layout:
sidebarLayout = function(...) shiny::sidebarLayout(...);
fluidRow = function(...) shiny::fluidRow(...);
column = function(...) shiny::column(...);
# Panels:
tabPanel = function(...) shiny::tabPanel(...);
mainPanel = function(...) shiny::mainPanel(...);
titlePanel = function(...) shiny::titlePanel(...);
sidebarPanel = function(...) shiny::sidebarPanel(...);
# Input:
sliderInput = function(...) shiny::sliderInput(...);
selectInput = function(...) shiny::selectInput(...);
checkboxInput = function(...) shiny::checkboxInput(...);


getUI = function() {
	shiny::shinyUI(
	shiny::fluidPage(
	shiny::navbarPage("Package-Browser", id="menu.top",
		# theme = init.theme(),
		tabPanel("Data", # icon = icon("upload file"),
				fluidRow(DT::DTOutput("tblData")),
				fluidRow(
					"Load from CRAN: ",
					actionButton("downloadPkgs", "All"), # All CRAN
					"  Open selection: ",
					actionButton("openPkgs", "Open"),    # Open Selection
					"  Filter: ",
					actionButton("fltToday", "Today"),   # Filter Today
					actionButton("fltWeek", "1 Week") ), # Filter 1 Week
				fluidRow(
					"Data Table: ",
					downloadButton("savePkgs", "Save"), # Save List of Packages
					checkboxInput("chkRegex", "Regex Search", value = TRUE),
					checkboxInput("chkCase", "Case Insensitive", value = TRUE)
				)
		),
		tabPanel("Words", # icon = icon("Words"),
				fluidRow(DT::DTOutput("tblWords"))
		),
		tabPanel("History", # icon = icon("History"),
				fluidRow(DT::DTOutput("tblFltHistory"))
		),
		tabPanel("Archived", # icon = icon("Archived"),
				fluidRow(DT::DTOutput("tblArchived"))
		),
		tabPanel("Help", # icon = icon("Help"),
				uiOutput("txtHelp")
		)
	)))
}
