

getServer = function(x) {
	shiny::shinyServer(server.app)
}

server.app = function(input, output, session) {
	# Global Options
	options = list(
		url = NULL,
		urlArchive = "https://cran.r-project.org/src/contrib/Archive/"
	);
	
	# Dynamic variable
	values = reactiveValues(
		Active    = "Not",  # Active Tab
		fullData  = NULL,   # Initial Data
		fltData   = NULL,   # Filtered Data
		flt       = NULL,   # Active Filter
		fltHist   = as.filter(filter.regex),   # Filter History
		fltRegex  = TRUE,
		fltCaseInsens = TRUE  # Filter: Case Insensitive
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
	filter.byTable0 = function() {
		id = input$tblData_rows_all;
		if(is.null(id)) {
			values$fullData;
		} else {
			values$fullData[id, ];
		}
	}
	filter.byTable = function() {
		values$fltData = filter.byTable0();
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
		len = length(id); # use last selected item;
		if(len > 1) print("Multiple selection!");
		browseURL(url.cran(values$fullData$Package[id][len]));
	})
	### Filter: Today
	observeEvent(input$fltToday, {
		values$flt = input$tblData_search_columns;
		dt = as.Date(Sys.time());
		dt = c(dt, dt);
		output$tblData = DT::renderDT(dataTable(date = dt));
	})
	### Filter: 1 Week
	observeEvent(input$fltWeek, {
		values$flt = input$tblData_search_columns;
		dt = as.Date(Sys.time());
		dt = c(dt - 7, dt);
		output$tblData = DT::renderDT(dataTable(date = dt));
	})
	
	### Options: Data
	observeEvent(input$chkRegex, {
		isRegex = input$chkRegex;
		values$fltRegex = isRegex;
		values$flt = input$tblData_search_columns;
		output$tblData = DT::renderDT(dataTable());
	})
	observeEvent(input$chkCase, {
		isCaseIns = input$chkCase;
		values$fltCaseInsens = isCaseIns;
		values$flt = input$tblData_search_columns;
		output$tblData = DT::renderDT(dataTable());
	})
	### Filters
	observeEvent(input$tblData_search_columns[[2]], {
		flt = input$tblData_search_columns[[2]];
		# print(input$tblData_search_columns[[2]]);
		if(flt == "") return();
		fltLast = values$fltHist$Flt;
		len = length(fltLast);
		if(len == 0) {
			values$fltHist = as.filter(flt);
			return();
		}
		fltLast = fltLast[len];
		# if(flt == fltLast) return();
		if( ! is.na(pmatch(strip.filter(fltLast), flt))) {
			# does NOT catch all variants of newly typed input!
			values$fltHist[len, ] = as.filter(flt);
		} else {
			values$fltHist = rbind(values$fltHist, as.filter(flt));
		}
		# Reset Selection:
		selectRows(dataTableProxy('tblData'), NULL);
	})
	
	### Tables
	
	# Data
	dataTable = function(date = NULL) ({
		reset.tab();
		flt = as.filter.tbl(values$flt, date=date);
		DT::datatable(values$fullData, filter = 'top',
			options = option.regex(values$fltRegex, varia = flt,
				caseInsens = values$fltCaseInsens));
	})
	
	# Words
	output$tblWords = DT::renderDT({
		x = values$fltData$Title;
		cat("Rows: ", nrow(x), "\n");
		sW = as.words(x);
		DT::datatable(sW, filter = 'top',
			options = option.regex(values$fltRegex));
	})
	
	# Filter History
	output$tblFltHistory = DT::renderDT({
		DT::datatable(values$fltHist, filter = 'top',
			options = option.regex(values$fltRegex));
	})
	
	# Archived Packages
	output$tblArchived = DT::renderDT({
		if(is.null(values$fullData)) return();
		x = read.html2(options$urlArchive, idCols = c(2,3));
		x$Name = substr(x$Name, 1, nchar(x$Name) - 1);
		nms = setdiff(x$Name, values$fullData$Package);
		ids = match(nms, x$Name);
		x   = x[ids, ];
		DT::datatable(x, filter = 'top',
			options = option.regex(values$fltRegex));
	})
	
	### Save Packages
	output$savePkgs = downloadHandler(
		filename = function() {
			paste("Packages.", as.Date(Sys.time()), ".csv", sep = "");
		},
		content = function(file) {
			x = filter.byTable0();
			if(is.null(x)) return(NULL);
			write.csv(x, file, row.names = FALSE);
		}
	)
	
	### Help
	output$txtHelp <- renderUI({
		help.Pkg();
	})
}
