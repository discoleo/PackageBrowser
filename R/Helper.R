
### Helper Functions

# TODO:
# - evaluate biterm topics:
#   https://cran.r-project.org/web/packages/BTM/index.html


### Read HTML
read.html = function(url = NULL, strip = NULL) {
	if(is.null(url)) {
		url = "https://cran.r-project.org/web/packages/available_packages_by_date.html";
	}
	doc = rvest::read_html(url);
	x   = doc |> rvest::html_element("table") |> rvest::html_table();
	print("Finished downloading.");
	# Date:
	dt = as.POSIXlt(x$Date, tz = "GMT", format = "%Y-%m-%d");
	x$Date = NULL;
	x$Date = as.Date(dt);
	# x$H    = dt$hour;
	if(! is.null(strip) && length(strip) > 0) {
		if(length(strip) > 1) {
			warning("Strip multiple tokens: NOT yet implemented!");
		}
		x$Title = gsub(strip, "", x$Title);
	}
	return(x);
}
read.html2 = function(url = NULL, name = "Last modified", idCols = NULL, filter = TRUE) {
	doc = rvest::read_html(url);
	x = doc |> rvest::html_element("table") |> rvest::html_table();
	print("Finished downloading.");
	x = as.data.frame(x); # as.POSIXlt fails with tibble;
	if(! is.null(idCols)) x = x[, idCols];
	# Date:
	id = match(name, names(x));
	if(is.na(id)) {
		warning("Invalid column name!");
		print(str(x));
	}
	if(filter) {
		idF = which(is.na(x[, id]) | x[, id] == "");
		cat("Filtered: ", idF, "\n");
		x = x[ - idF, ]; # skip "Parent Directory"
	}
	dt = as.POSIXlt(x[, id], tz = "GMT", format = "%Y-%m-%d");
	x[, id] = NULL;
	x$Date  = as.Date(dt);
	# x$H   = dt$hour;
	return(x);
}
url.cran = function(x) {
	paste0("https://cran.r-project.org/web/packages/",
			x, "/index.html");
}

read.reverseDependencies = function(pkg) {
	url = url.cran(pkg);
	doc = rvest::read_html(url);
	x   = doc |> rvest::html_elements(
			xpath = "//table[tr/td/text()[contains(., 'Reverse')]]");
	# print(as.character(x));
	x = rvest::html_table(x);
	x = as.data.frame(x);
	return(x);
}

### Package CRAN page
# - Open in new window
openPackage = function(idRows, tbl) {
	if(is.null(idRows)) {
		showModal(modalDialog(
			title = "Nothing selected! Please select first a package.",
			easyClose = TRUE, footer = NULL
		));
		return();
	}
	len = length(idRows); # use last selected item;
	if(len > 1) print("Multiple selection!");
	browseURL(url.cran(tbl$Package[idRows][len]));
}

### Filters

### Filter History
as.filter.df = function(x) {
	x$Flt[ ! x$ML] = paste0(x$Flt[ ! x$ML], "[a-z]*");
	data.frame(Flt = x$Flt, ML = x$ML,
		Regex = x$Regex, Date = Sys.time());
}
# ML = multi-line filter;
as.filter = function(x, expand = TRUE, isRegex = TRUE, isML = FALSE) {
	if(is.null(x)) return(x);
	if(expand && isRegex && ! isML) {
		x = paste0(x, "[a-z]*");
	}
	data.frame(Flt = x, ML = isML,
		Regex = isRegex, Date = Sys.time());
}
strip.filter = function(x) {
	# "[a-z]*"
	n = nchar(x);
	n = pmax(0, n - 6);
	substr(x, 1, n);
}

### Table Filters

filter.tbl = function(pattern, data, isCaseInsens = TRUE) {
	txt = pattern;
	if(txt == "" || nrow(data) == 0) {
			# TODO
			print("Nothing to search!");
			return();
		}
		txt = as.regex(txt, isCaseInsens);
		# print(txt); # Debug
		#
		isT = TRUE;
		for(id in seq_along(txt$Regex)) {
			isR = grepl(txt$Regex[id], data$Title, perl = TRUE);
			if(txt$Neg[id]) isR = ! isR;
			isT = isT & isR;
		}
		data[isT, , drop = FALSE];
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

as.regex = function(x, isCaseInsens = TRUE) {
	txt = unlist(strsplit(x, "\n+"));
	# Empty lines:
	len = nchar(txt);
	idEmpty = which(len == 0);
	if(length(idEmpty) > 0) txt = txt[ - idEmpty];
	if(length(txt) == 0) return(list(Regex = "", Neg = FALSE));
	# Negation:
	isNeg = substr(txt, 1, 1) == "!";
	idNeg = which(isNeg);
	txt[idNeg] = substr(txt[idNeg], 2, len[idNeg]);
	# Case:
	if(isCaseInsens) txt = paste0("(?i)", txt);
	return(list(Regex = txt, Neg = isNeg));
}
