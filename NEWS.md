# nfsa 0.0.2

* Bug in nfsa_validation().
* Adding new folders in nfsa_folders().

# nfsa 0.0.3

* New function nfsa_internal_consistency() based on the work by Estela Kaja.

# nfsa 0.0.4

* Proper completeness report according to regulation
* new function nfsa_comext() to download data from ITGS from Eurobase
* new convenience functions for reading matis,xml and xlsx.csv files
* nfsa_comext() now checks there are three months before calculating the quarter.
* nfsa_seas_level1() only checks X13, no TS in order to be consistent between leve1 and level2/3

# nfsa 0.0.5

* Removed euro area requirements for non-EA countries in completeness_regulation()
* Added /internal to nfsa_folders()
* Output an excel file for all revisions

# nfsa 0.0.6

* You can specify the staring quarter for nfsa_seas_levelx() functions
* Better alignment of codes with the ones in the VR tool.
* Fix missing / in the output of conf/obs status.
* Internal consistency T0800/T0801 using Conval rules (whichj I think need to be reviewed!).
* New functions to read (not from Famex!) and write excel templates. 


# nfsa 0.0.7
* Internal consistency T0800/T0801 now handles the case when transactions are missing.
* Delete some not so useful functions (nfsa_transmissions,nfsa_completeness_regulation)

# nfsa 0.0.8
* Default data directory is M:
* Added argument to plots to choose the line colours.
