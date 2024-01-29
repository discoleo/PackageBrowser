
help.Pkg = function() {
	HTML(
		"<div>
			<h2>Data Tab</h2>
			<h3>All Packages on CRAN</h2>
			<p>The titles of all packages available on CRAN are downloaded by pressing the <b>All</b> button.
				The computer must be connected to the internet!</p>
			<h3>Open a Specific Package</h2>
			<p>Select 1 package from the table by clicking on the respective row.
				Click the <b>Open</b> button and the specific CRAN web page will be opened.</p>
			<h3>Save Table</h2>
			<p>Saves the filtered table as a csv file.</p>
		
			<h2>Words Tab</h2>
			<p>Tabulates the words in the package titles. Various stop-words are removed.</p>
		
			<h2>History Tab</h2>
			<p>Displays the Search history. Some predefined search terms are included.</p>
		</div>"
	);
}

