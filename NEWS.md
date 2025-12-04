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
