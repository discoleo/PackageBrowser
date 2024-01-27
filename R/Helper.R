
read.html = function(url = NULL) {
	if(is.null(url)) {
		url = "https://cran.r-project.org/web/packages/available_packages_by_date.html";
	}
	doc = rvest::read_html(url);
	
	x  = doc |> rvest::html_node("table") |> rvest::html_table();
	dt = as.POSIXlt(x$Date, tz = "GMT");
	x$Date = NULL;
	x$Date = as.Date(dt);
	# x$H    = dt$hour;
	return(x);
}
url.cran = function(x) {
	paste0("https://cran.r-project.org/web/packages/",
			x, "/index.html");
}

### Filters
as.filter = function(x, expand = TRUE, isRegex = TRUE) {
	if(is.null(x)) return(x);
	if(expand && isRegex) x = paste0(x, "[a-z]*")
	data.frame(Flt = x, Regex = isRegex, Date = Sys.time());
}

as.filter.tbl = function(x, date = NULL) {
	flt = x;
	if( ! is.null(flt)) {
		flt = c("", flt); # Row ID
		flt = lapply(flt, function(x) if(x == "") NULL else list(search = x));
		isN = all(is.null(unlist(flt)));
		flt = if(isN) NULL else list(searchCols = flt);
	}
	if( ! is.null(date)) {
		dt = paste0(format(date, format = "%Y-%m-%d"), collapse = "...");
		dt = list(search = dt);
		if(is.null(flt)) {
			flt = list(searchCols = list(NULL, NULL, NULL, dt));
		} else {
			flt[[1]][[4]] = dt; # id hardcoded
		}
		# print(flt)
	}
	return(flt);
}

as.words = function(x) {
	sW = strsplit(x, "[ ,\t\n'\"()/]+");
	sW = table(unlist(sW));
	sW = as.data.frame(sW, stringsAsFactors = FALSE);
	names(sW)[1] = "Word";
	# Stop-words
	isStop = sW$Word %in% wordsStop;
	sW = sW[ ! isStop, ];
	#
	sW$Len = nchar(sW$Word);
	return(sW);
}
