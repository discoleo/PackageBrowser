


as.words = function(x) {
	# TODO: doi;
	# Web Links:
	patHREF = "(?i)https?+://[^ \t\n>,()\"']++";
	href = extract.regex(x, patHREF);
	isHREF = sapply(href, function(x) nchar(x[1]) > 0);
	href = href[isHREF];
	# Clean the strings:
	x[isHREF] = gsub(patHREF, " ", x[isHREF], perl = TRUE);
	# ". " fails in Microbiology;
	# "\.(?<!\...)" e.g. "U.S.";
	sW = strsplit(x, "[ ,\t\n'\"()/]+|[.:] +|\\.$(?<!\\...)", perl = TRUE);
	sW = c(unlist(sW), unlist(href));
	sW = table(sW);
	sW = as.data.frame(sW, stringsAsFactors = FALSE);
	names(sW)[1] = "Word";
	# Stop-words
	isStop = sW$Word %in% wordsStop;
	sW = sW[ ! isStop, ];
	#
	sW$Len = nchar(sW$Word);
	return(sW);
}


# rm.match = remove match;
extract.regex = function(x, pattern, perl=TRUE, rm.match=TRUE,
		simplify=FALSE, verbose=FALSE) {
	if(inherits(x, "data.frame")) stop("x should be an array!");
	r = gregexpr(pattern, x, perl=perl);
	if(verbose) cat("Finished Regex.\nStarting extraction.\n");
	s = lapply(seq(length(x)), function(id) {
		tmp = r[[id]];
		if(tmp[1] == -1) return("");
		nStart = tmp;
		substring(x[id], nStart, nStart - 1 + attr(tmp, "match.length"));
	});
	if(simplify) s = unlist(s);
	if(rm.match) {
		# TODO
	}
	return(s)
}
