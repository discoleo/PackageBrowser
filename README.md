# PackageBrowser

Browse list of R Packages:
- Downloads list of active packages from CRAN;
- Enables searches using regular expressions;
- Advanced searching possible using the Search tab: enables searches using multiple expressions & negation of the individual expressions in the list of expressions;


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


### Start App
runApp()
```

In the **Data** tab click the **All** button: this will download the list of all active packages from the CRAN server.

The packages are displayed in a DataTable. It is possible to filter the list of packages using various filters, including regular expressions. More advanced filters are available in the **Search** tab.

The **Open** button opens the specific CRAN web page for the currently selected package.

The **Reverse** button loads the list of reverse dependencies for the current package.

The **Archived** tab displays the list of packages that were archived on CRAN. The list is loaded automatically.
> Note: it may take 5-10 s to load the data.


## Other Packages

- Package pkgdepR: Statically Determine Function Dependencies Between Packages
  > https://cran.r-project.org/web/packages/pkgdepR/index.html
- CRAN incoming dashboard
  > https://r-hub.github.io/cransays/articles/dashboard.html
- Package ciw: Watch the CRAN Incoming Directories
  > https://cran.r-project.org/web/packages/ciw/index.html
- Package foghorn: Summarize CRAN Check Results in the Terminal
  > https://cran.r-project.org/web/packages/foghorn/index.html
