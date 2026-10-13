

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

text.html = function(x, col, pad = 12) {
	if(! is.null(pad)) {
		if(is.numeric(pad)) pad = paste0(pad, "px");
		pad = paste0(pad, collapse = " ");
		pad = paste0("padding: ", pad, ";");
	}
	html = paste0("<span style=\"color:", col, ";",
		pad, "\">", x, "</span>");
	HTML(html);
}
# Inline DIV:
divi = function(x, width = 160, pad.l = 30) {
	div(x, style = paste0("display: inline-block; ",
		"padding-left: ", pad.l, "px;",
		"width: ", width, "px;"));
}

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
					text.html(" |  Open selection: ", col = "#FF0000"),
					actionButton("openPkgs", "Open"),    # Open Selection
					actionButton("btnReverse", "Rev Dep"), # Reverse Dependencies
					text.html(" |  Filter: ", col = "#0000FF"),
					actionButton("fltToday", "Today"),   # Filter Today
					actionButton("fltWeek", "1 Week"),   # Filter 1 Week
					actionButton("fltMonth", "1 Month"), # Filter 1 Month
					# GoTo:
					text.html(" | Word/Page: ", col = "#A0A032"),
					divi(textInput("inGOTO", "", width = "80px"), width = 90, 10),
					actionButton("btnGOTO", "GoTo"),   # GoTo: Page / Text
				),
			fluidRow(
					"Data Table: ",
					downloadButton("savePkgs", "Save"),  # Save List of Packages
					divi(checkboxInput("chkRegex", "Regex Search", value = TRUE)),
					divi(checkboxInput("chkCase", "Case Insensitive", value = TRUE)),
			),
			fluidRow(DT::DTOutput("tblReverse"))
		),	
		tabPanel("Installed", # icon = icon("Installed"),
			fluidRow(style = "padding-bottom:14px; padding-left:20px;",
			column(2,
			actionButton("btnUpdatePkgs", "Update"),    # Update Installed Packages
			), column(10,
			checkboxInput("chkFltNewer", "Newer", value = TRUE), # Newer
			),
			fluidRow(DT::DTOutput("tblPkgsInstalled")),
			fluidRow(style = "padding-bottom:12px;"),
			h2("Table with Installed Packages:"),
		)),
		tabPanel("Search", # icon = icon("Search"),
			fluidRow(
				column(5, textAreaInput("inputSearch",
					" Insert multiple Search terms: 1 term per line! ", width = "100%", rows = 3)),
				column(2, fluidRow(HTML("&nbsp;")),
					fluidRow(actionButton("btnSearch", "Search")), # Advanced Search
					downloadButton("savePkgsAdv", "Save"), # Save List of Packages
					"Save Table",
					actionButton("openPkgsAdv", "Open"), # Open Selection
					"Open selection"
				),
				column(5, "TODO")
			),
			fluidRow(DT::DTOutput("tblSearch"))
		),
		### Words in Title
		tabPanel("Words", # icon = icon("Words"),
			fluidRow(
			column(6, fluidRow(DT::DTOutput("tblWords"))),
			column(6,
			# Padding: <top, right, bottom, left>
			HTML("<div style='padding: 0px 0px 10px 20px;'>"),
			fluidRow(
				textInput("inPageTblWords", "GoTo Word/Page:"),
			),
			fluidRow(
				actionButton("btnGotoWord", "Page"),
				actionButton("btnViewByWord", "View"),
				actionButton("btnOpenPkgWord", "Open"),
			),
			# Packages containing selected word;
			fluidRow(DT::DTOutput("tblPkgsWords")),
			HTML("</div>")
			),
			),
		),
		tabPanel("History", # icon = icon("History"),
			fluidRow(DT::DTOutput("tblFltHistory"))
		),
		tabPanel("Archived", # icon = icon("Archived"),
				fluidRow(DT::DTOutput("tblArchived")),
				fluidRow(
					"Open selected: ",
					actionButton("openPkgsArch", "Open")),   # Open Selection
				fluidRow("Note: the page with packages is downloaded automatically,
					but it may take a few seconds!")
		),
		tabPanel("Help", # icon = icon("Help"),
				uiOutput("txtHelp")
		)
	)))
}
