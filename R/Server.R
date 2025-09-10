

getServer = function(x) {
	shiny::shinyServer(server.app)
}

server.app = function(input, output, session) {
	# Global Options
	options = list(
		url = NULL,  # use/override url hardcoded in function read.html();
		urlArchive = "https://cran.r-project.org/src/contrib/Archive/",
		title.stripWords = c("^A +"),
		fltRegCaseInsens_Words = TRUE,
		NULLARG = NULL
	);
	
	# Dynamic variable
	values = reactiveValues(
		ActiveTab  = "Not", # Active Tab
		fullData   = NULL,  # Initial Data
		dfFltData  = NULL,  # Filtered Data
		dfSearch   = NULL,  # Data filtered by Advanced Search
		dfWords    = NULL,  # Words in Titles
		dfPkgWords = NULL,  # Packages containing specific Word;
		dfReverse  = NULL,  # Reverse Dependencies
		dfArchived = NULL,  # Archived Packages
		fltMain    = NULL,  # Active Filter
		fltHist    = as.filter.df(filter.regex),   # Filter History
		fltRegex   = TRUE,
		fltCaseInsens = TRUE,  # Filter: Case Insensitive
		fltCaseInsensAdv = TRUE,
		pageWord   = 0,
		NULLARG = NULL
	);
	
	asDT = function(x, filter = "top", dom = NULL, regex = NULL) {
		# Options:
		varia = NULL;
		if(! is.null(dom)) varia = list(dom = dom);
		if(is.null(regex)) regex = values$fltRegex;
		opt = option.regex(regex, varia = varia);
		if(is.null(filter)) {
			DT::datatable(x, options = opt);
		} else {
			DT::datatable(x, filter = filter, options = opt);
		}
	}
	setPage = function(pg, tbl) {
		proxy = dataTableProxy(tbl);
		selectPage(proxy, pg);
	}
	getSelected = function(nameTable) {
		if( ! is.character(nameTable)) warning("Wrong table name!");
		id = input[[paste0(nameTable, "_rows_selected")]];
	}
	data.MessageStart = function() {
		data.frame(Package = "Press the *All* button below to download the table of packages from CRAN.");
	}
	tblMessageDownload = function() {
		DT::datatable(data.MessageStart(),
			options = option.regex(TRUE, varia = list(dom = "t")));
	}
	
	# Reset Filters on Data table
	hasData = function() { ! is.null(values$fullData); }
	reset.tab = function() {
		if(values$ActiveTab != "Data" && hasData()) {
			values$ActiveTab  = "Data";
		}
	}
	
	observeEvent(input$menu.top, {
		if(values$ActiveTab == "Data") {
			print("Switched away from Data!");
			filter.byTable();
			values$ActiveTab = "Other";
			values$fltMain   = input$tblData_search_columns;;
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
	filter.byTableSearch = function() {
		id = input$tblSearch_rows_all;
		if(is.null(id)) {
			values$dfSearch;
		} else {
			values$dfSearch[id, ];
		}
	}
	filter.byTable = function() {
		values$dfFltData = filter.byTable0();
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
		values$fullData = read.html(
			url   = options$url,
			strip = options$title.stripWords);
		values$fltMain   = NULL;
		values$dfFltData = values$fullData;
		reset.tab();
	})
	### Open Package Page
	observeEvent(input$openPkgs, {
		id = input$tblData_rows_selected;
		openPackage(id, values$fullData);
	})
	observeEvent(input$openPkgsAdv, {
		id = input$tblSearch_rows_selected;
		openPackage(id, values$dfSearch);
	})
	
	### Filter: Today
	observeEvent(input$fltToday, {
		values$fltMain = input$tblData_search_columns;
		dt = as.Date(Sys.time());
		dt = c(dt, dt);
		output$tblData = DT::renderDT(dataTable(date = dt));
	})
	### Filter: 1 Week
	observeEvent(input$fltWeek, {
		values$fltMain = input$tblData_search_columns;
		dt = as.Date(Sys.time());
		dt = c(dt - 7, dt);
		output$tblData = DT::renderDT(dataTable(date = dt));
	})
	
	### Options: Data
	observeEvent(input$chkRegex, {
		isRegex = input$chkRegex;
		values$fltRegex = isRegex;
		values$fltMain = input$tblData_search_columns;
		output$tblData = DT::renderDT(dataTable());
	})
	observeEvent(input$chkCase, {
		isCaseIns = input$chkCase;
		values$fltCaseInsens = isCaseIns;
		values$fltMain = input$tblData_search_columns;
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
			values$fltHist = as.filter(flt, isRegex = values$fltRegex);
			return();
		}
		fltLast = fltLast[len];
		fltNew  = as.filter(flt, isRegex = values$fltRegex);
		# if(flt == fltLast) return();
		# TODO: strip only if ! ML;
		if( ! is.na(pmatch(strip.filter(fltLast), flt))) {
			# does NOT catch all variants of newly typed input!
			values$fltHist[len, ] = fltNew;
		} else {
			values$fltHist = rbind(values$fltHist, fltNew);
		}
		# Reset Selection:
		selectRows(dataTableProxy('tblData'), NULL);
	})
	
	### Reverse Dependencies
	observeEvent(input$btnReverse, {
		id = getSelected("tblData");
		if(is.null(id)) {
			values$dfReverse = NULL;
			return();
		}
		# Last Selected
		len = length(id);
		id  = id[len];
		pkg = values$fullData$Package[id];
		#
		x = read.reverseDependencies(pkg);
		values$dfReverse  = x;
		output$tblReverse = DT::renderDT(
			DT::datatable(x, filter = 'top',
				options = option.regex(values$fltRegex)));
	})
	
	### Advanced Search
	# Note: currently reuses values$fltRegex;
	observeEvent(input$btnSearch, {
		txtRegex = input$inputSearch;
		x = filter.tbl(txtRegex, values$dfFltData, values$fltCaseInsensAdv);
		fltNew = as.filter(txtRegex, isRegex = values$fltRegex, isML = TRUE);
		values$fltHist = rbind(values$fltHist, fltNew);
		values$dfSearch  = x;
		output$tblSearch = DT::renderDT(
			DT::datatable(x, filter = 'top',
				options = option.regex(values$fltRegex, varia = list(dom = "tipl"))));
	})
	
	### Tables
	
	# Data
	dataTable = function(date = NULL) ({
		if(is.null(values$fullData)) {
			return(tblMessageDownload());
		}
		flt = as.filter.tbl(values$fltMain, date=date);
		DT::datatable(values$fullData, filter = 'top',
			options = option.regex(values$fltRegex, varia = flt,
				caseInsens = values$fltCaseInsens));
	})
	
	### Words
	output$tblWords = DT::renderDT({
		x = values$dfFltData$Title;
		cat("Rows: ", length(x), "\n");
		sW = as.words(x);
		values$dfWords = sW;
		DT::datatable(sW, filter = 'top',
			options = option.regex(values$fltRegex));
	})
	
	getWordPage = function() {
		val = input$inPageTblWords;
		if(is.null(val) || nchar(val) == 0) return(0);
		isNum = grepl("^[0-9]", val);
		if(isNum) {
			pg = as.integer(val);
			if(is.na(pg)) return(0);
		} else {
			if(options$fltRegCaseInsens_Words) {
				val = paste0("(?i)", val);
			}
			val = grepl(val, values$dfWords$Word);
			pg  = which(val);
			if(length(pg) == 0) return(0);
			pg = pg[1];
			pg = (pg - 1) %/% 10 + 1;
		}
		values$pageWord = pg;
	}
	observeEvent(input$btnGotoWord, {
		pg = getWordPage();
		values$pageWord = pg;
		if(is.null(pg) || pg == 0) return();
		setPage(pg, 'tblWords');
	})
	
	observeEvent(input$btnViewByWord, {
		x  = values$dfFltData;
		if(is.null(x)) return();
		id = input$tblWords_rows_selected;
		if(length(id) == 0) return();
		sWords = values$dfWords$Word[id];
		idPkg  = getPkgsByWord(sWords, x);
		x = x[idPkg, ];
		values$dfPkgWords = x;
	})
	output$tblPkgsWords = DT::renderDT({
		x = values$dfPkgWords;
		if(is.null(x)) return();
		asDT(x, dom = "tip")
	});
	
	# Open Selected Package
	observeEvent(input$btnOpenPkgWord, {
		x  = values$dfPkgWords;
		if(is.null(x)) return();
		id = input$tblPkgsWords_rows_selected;
		openPackage(id, x);
	})
	
	getPkgsByWord = function(w, x) {
		# TODO: better quoting;
		w = paste0("\\Q", w, "\\E");
		w = paste0(w, collapse = "|");
		w = paste0("(?i)", w);
		id = grepl(w, x$Title);
		id = which(id);
		return(id);
	}
	
	# Filter History
	output$tblFltHistory = DT::renderDT({
		DT::datatable(values$fltHist, filter = 'top',
			options = option.regex(values$fltRegex));
	})
	
	# Archived Packages
	readArchive = function() {
		if(is.null(values$fullData)) return();
		print("Reading Archive");
		x = read.html2(options$urlArchive, idCols = c(2,3));
		names(x)[1] = "Package";
		x$Package = substr(x$Package, 1, nchar(x$Package) - 1);
		# nms = setdiff(x$Package, values$fullData$Package);
		# ids = match(nms, x$Package);
		ids = match(x$Package, values$fullData$Package, 0L) == 0L;
		x   = x[ids, ];
		values$dfArchived = x;
	}
	
	# Archived Packages
	output$tblArchived = DT::renderDT({
		readArchive();
		x = values$dfArchived;
		if(is.null(x)) return();
		DT::datatable(x, filter = 'top',
			options = option.regex(values$fltRegex,
				varia = list(order = list(2, "desc"))));
	})
	observeEvent(input$openPkgsArch, {
		id = input$tblArchived_rows_selected;
		openPackage(id, values$dfArchived);
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
	output$savePkgsAdv = downloadHandler(
		filename = function() {
			paste("Packages.", as.Date(Sys.time()), ".csv", sep = "");
		},
		content = function(file) {
			x = filter.byTableSearch();
			if(is.null(x)) return(NULL);
			write.csv(x, file, row.names = FALSE);
		}
	)
	
	### Help
	output$txtHelp <- renderUI({
		help.Pkg();
	})
}
