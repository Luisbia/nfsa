# nfsa 0.0.2

* Bug in nfsa_validation().
* Adding new folders in nfsa_folders().

# nfsa 0.0.3

* New function nfsa_internal_consistency() based on the work by Estela Kaja.

# nfsa 0.0.4

* Proper completeness report according to regulation.
* New function nfsa_comext() to download data from ITGS from Eurobase.
* New convenience functions for reading matis, xml, xlsx and csv files.
* nfsa_comext() now checks there are three months before calculating the quarter.
* nfsa_seas_level1() only checks X13, no TS in order to be consistent between level1 and level2/3.

# nfsa 0.0.5

* Removed euro area requirements for non-EA countries in completeness_regulation().
* Added /internal to nfsa_folders().
* Output an excel file for all revisions.

# nfsa 0.0.6

* You can specify the starting quarter for nfsa_seas_levelx() functions
* Better alignment of codes with the ones in the VR tool.
* Fix missing / in the output of conf/obs status.
* Internal consistency T0800/T0801 using Conval rules (which I think need to be reviewed!).
* New functions to read (not from Famex!) and write excel templates.


# nfsa 0.0.7
* Internal consistency T0800/T0801 now handles the case when transactions are missing.
* Delete some not so useful functions (nfsa_completeness_regulation).

# nfsa 0.0.8
* Default data directory is M:
* Added argument to plots to choose the line colours.
* Possibility to choose in revisions vintage or version.
* GFS SA now checked with internal files from dir D.
* New function to convert euro fixed to euro.
* New function to copy locally parquet files from server.

# nfsa 0.0.9
* Adding some code suggestions from Gemini.
* Option to have revision of versions in sequence of accounts.
* New function to delete content of output folders.
* Most output sent to temporary excel files on the screen to avoid searching for files.
* New function to plot together T0801/BOP.

# nfsa 1.0.0
* GR converted to EL in output data.
* New function to calculate data per capita.
* Fixed bug in nfsa_seas_level1 when rows where not ordered by period.
* Added argument to nfsa_get_data*() which allows to collect all columns. 

