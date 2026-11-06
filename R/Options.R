

### Stop-Words
wordsStop = c(
	"a", "A", "an", "and", "are", "as",
	"by",
	"for", "from",
	"i.e.", "in", "is", "its", "Its",
	"of", "on", "or",
	"that", "the", "The", "these", "this", "to",
	"using", "with"
)


### Filters
getFiltersDefault = function() {
	flt = c(
		"meta",
		"solve",
		# mining|mine / ! determine|minimization|luminescence
		"min(?<!deter...)(?:i(?![-ms ])|e(?!sc))",
		"pubmed",
		"vac(?<!pri...)", # vaccine vs privacy
		"anti(?![ltcf])",
		# TODO: What else is there?
		# "model" + ! "mix" + ! "linear"
		"model\n!mix\n!linear",
		"adaptive\ndynamic",
		"prot(?!o|ec)", # Protein|proteic
		"(?<!mis)cell", # Cells: ! miscellaneous
		"model", # Any model
		NULL
	);
}

### Filters
filter.regex = data.frame(
	Flt = getFiltersDefault(),
	# Multi-Line:
	ML = c(rep(FALSE, 6), TRUE, TRUE,
		rep(FALSE, 3)),
	Regex = TRUE
)

### Interesting Packages:

# 1. adapt3: Adaptive Dynamics and Community Matrix Model Projections

