

getServer = function(x) {
	shiny::shinyServer(server.app)
}

server.app = function(input, output, session) {
	# Global Options
	options = list(
		url = NULL
	);
	
	# Dynamic variable
	values = reactiveValues(
		Active    = "Not",  # Active Tab
		fullData  = NULL,   # Initial Data
		fltData   = NULL,   # Filtered Data
		flt       = NULL,   # Active Filter
		fltHist   = as.filter(filter.regex)   # Filter History
	);
	
	# Reset Filters on Data table
	hasData = function() { ! is.null(values$fullData); }
	reset.tab = function() {
		if(values$Active != "Data" && hasData()) {
			values$Active  = "Data";
		}
	}
	
	observeEvent(input$menu.top, {
		if(values$Active == "Data") {
			print("Switched away from Data!");
			filter.byTable();
			values$Active = "Other";
			values$flt    = input$tblData_search_columns;;
		}
	})
	filter.byTable = function() {
		id = input$tblData_rows_all;
		if(is.null(id)) {
			values$fltData = values$fullData;
		} else {
			values$fltData = values$fullData[id, ];
		}
	}
	#
	option.regex = function(x, varia = NULL, caseInsens = TRUE) {
		opt = list(search = list(regex = x, caseInsensitive = caseInsens),
			searchHighlight = TRUE);
		if( ! is.null(varia)) opt = c(opt, varia);
		return(opt);
	}
	
	### Web Download
	observeEvent(input$downloadPkgs, {
		values$fullData = read.html(url = options$url);
		values$flt      = NULL;
		# filter.byTable();
		values$fltData = values$fullData;
	})
	### Open Package Page
	observeEvent(input$openPkgs, {
		id = input$tblData_rows_selected;
		if(is.null(id)) {
			showModal(modalDialog(
				title = "Nothing selected! Please select first a package.",
				easyClose = TRUE, footer = NULL
			));
			return();
		}
		browseURL(url.cran(values$fullData$Package[id][1]));
	})
	### Today
	observeEvent(input$fltToday, {
		values$flt = input$tblData_search_columns;
		dt = as.Date(Sys.time());
		dt = c(dt, dt);
		output$tblData = DT::renderDT(dataTable(date = dt));
	})
	
	### Options: Data
	observeEvent(input$chkRegex, {
		isReg = input$chkRegex;
		values$reg.Data = isReg;
		values$flt = input$tblData_search_columns;
		output$tblData = DT::renderDT(dataTable());
	})
	### Filters
	observeEvent(input$tblData_search_columns[[2]], {
		flt = input$tblData_search_columns[[2]];
		# print(input$tblData_search_columns[[2]]);
		if(flt == "") return();
		fltLast = values$fltHist$Flt;
		fltLast = tail(fltLast, 1);
		if(flt == fltLast) return();
		values$fltHist = rbind(values$fltHist, as.filter(flt));
	})
	
	### Tables
	
	# Data
	dataTable = function(date = NULL) ({
		reset.tab();
		flt = as.filter.tbl(values$flt, date=date);
		DT::datatable(values$fullData, filter = 'top',
			options = option.regex(values$reg.Data, varia = flt));
	})
	
	# Words
	output$tblWords = DT::renderDT({
		x = values$fltData$Title;
		cat("Rows: ", nrow(x), "\n");
		sW = as.words(x);
		DT::datatable(sW, filter = 'top',
			options = option.regex(values$reg.Data));
	})
	
	# Filter History
	output$tblFltHistory = DT::renderDT({
		DT::datatable(values$fltHist, filter = 'top',
			options = option.regex(values$reg.Data));
	})
	
	### Help
	output$txtHelp <- renderUI({
		HTML("<div>
			<h2>All Packages on CRAN</h2>
			<p>The titles of all packages available on CRAN are downloaded by pressing the <b>All</b> button.
				The computer must be connected to the internet!</p>
			<h2>Open a Specific Package</h2>
			<p>Select 1 package from the table by clicking on the respective row.
				Click the <b?Open</> button and the specific CRAN web page will be opened.</p>
			</div>"
		)
	})
}
