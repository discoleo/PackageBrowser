

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
		fullData  = NULL,   # initial Data
		fltData   = NULL,
		flt       = NULL,   # Filters
		fltHist   = NULL    # Filter History
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
	observeEvent(input$openPkgs, {
		id = input$tblData_rows_selected;
		if(is.null(id)) return();
		browseURL(paste0("https://cran.r-project.org/web/packages/",
			values$fullData$Package[id][1], "/index.html"))
	})
	
	### Options: Data
	observeEvent(input$chkRegex, {
		isReg = input$chkRegex;
		values$reg.Data = isReg;
		values$flt = input$tblData_search_columns;
		output$tblData <- DT::renderDT(dataTable());
	})
	
	### Tables
	
	# Data
	dataTable = function() ({
		reset.tab();
		flt = values$flt;
		if( ! is.null(flt)) {
			flt = c("", flt); # Row ID
			flt = lapply(flt, function(x) if(x == "") NULL else list(search = x));
			isN = all(is.null(unlist(flt)));
			flt = if(isN) NULL else list(searchCols = flt);
		}
		DT::datatable(values$fullData, filter = 'top',
			options = option.regex(values$reg.Data, varia = flt));
	})
	
	# Words
	output$tblWords = DT::renderDT({
		x = values$fltData$Title;
		cat("Rows: ", nrow(x), "\n");
		sW = strsplit(x, "[ ,\t\n'\"()/]+");
		sW = table(unlist(sW));
		sW = as.data.frame(sW, stringsAsFactors = FALSE);
		names(sW)[1] = "Word";
		sW$Len = nchar(sW$Word);
		DT::datatable(sW, filter = 'top',
			options = option.regex(values$reg.Data));
	})
}
