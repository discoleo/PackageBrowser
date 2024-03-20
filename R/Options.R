

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
filter.regex = data.frame(
	Flt = c(
		"meta",
		"model",
		"solve",
		"prot(?!o|ec)",
		"(?<!mis)cell", # Cells
		# mining|mine / ! determine|minimization|luminescence
		"min(?<!deter...)(?:i(?![-ms ])|e(?!sc))",
		"pubmed",
		"vac(?<!pri...)", # vaccine vs privacy
		"anti(?![ltcf])",
		# TODO: What else is there?
		# "model" + ! "mix" + ! "linear"
		"model\n!mix\n!linear"
	),
	# Multi-Line:
	ML = c(rep(FALSE, 9), TRUE),
	Regex = TRUE
)

