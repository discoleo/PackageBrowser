# PackageBrowser

Browse list of  R Packages:
- downloads list of active packages from CRAN;
- enables advanced searching using regular expressions;
- use Search tab for advanced searching: multiple expressions & negation of any of the expressions;

## Installation

Install the package directly from GitHub or download the source files.

## Example

Load the package and start the app:

```R
### Load library

# if installed;
library(PackageBrowser)

# otherwise:
path = "..."
devtools::document(path)
devtools::load_all(path)

### Start app
runApp()
```

Click the **All** button: the list of all packages is downloaded from the CRAN server.
