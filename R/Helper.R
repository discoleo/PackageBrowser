
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

as.filter = function(x, expand = TRUE) {
	if(is.null(x)) return(x);
	if(expand) x = paste0(x, "[a-z]*")
	data.frame(Flt = x, Regex = TRUE, Date = Sys.time());
}

filter.regex = c(
	"meta",
	"model",
	"solve",
	"prot(?!o|ec)"
)
