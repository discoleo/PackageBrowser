

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
filter.regex = c(
	"meta",
	"model",
	"solve",
	"prot(?!o|ec)",
	"min(?<!deter...)(?:i(?![-ms ])|e(?!sc))", # mining|mine
	"pubmed",
	"vac(?<!pri...)", # vaccine vs privacy
	"anti(?![ltcf])"
)

