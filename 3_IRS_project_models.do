clear
set more off
cd "/Users/adrienne/Box Sync/UCSF Research Projects/UMSP/IRS project/"

*NOTE: Models are run in Stata due to model convergence issues with MLMs in R.


import delimited "/Users/adrienne/Box Sync/UCSF Research Projects/UMSP/IRS project/IRS stopped 3 sites.csv", encoding(ISO-8859-1)
menbreg cases i.mths_since_stop precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.mths_since_stop precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.time_cat precip_lag proportion_rdt notsuspect i.month || site:, irr

Clear
import delimited "/Users/adrienne/Box Sync/UCSF Research Projects/UMSP/IRS project/IRS restarted 9 sites.csv", encoding(ISO-8859-1)

menbreg cases i.mths_since_irs precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.mths_since_irs precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.time_cat precip_lag proportion_rdt notsuspect i.month || site:, irr

clear
import delimited "/Users/adrienne/Box Sync/UCSF Research Projects/UMSP/IRS project/IRS started 5 sites.csv", encoding(ISO-8859-1)

menbreg cases i.mths_since_irs_continuous precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.mths_since_irs_continuous precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.time_cat precip_lag proportion_rdt notsuspect i.month || site:, irr
