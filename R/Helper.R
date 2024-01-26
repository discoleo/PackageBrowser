
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