clear
set more off

*NOTE: Models are run in Stata due to model convergence issues with MLMs in R.


import delimited "./IRS stopped 3 sites.csv", encoding(ISO-8859-1)
menbreg cases i.mths_since_stop precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.mths_since_stop precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.time_cat precip_lag proportion_rdt notsuspect i.month || site:, irr

Clear
import delimited "./IRS restarted 9 sites.csv", encoding(ISO-8859-1)

menbreg cases i.mths_since_irs precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.mths_since_irs precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.time_cat precip_lag proportion_rdt notsuspect i.month || site:, irr

clear
import delimited "./IRS started 5 sites.csv", encoding(ISO-8859-1)

menbreg cases i.mths_since_irs_continuous precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.mths_since_irs_continuous precip_lag proportion_rdt notsuspect i.month || site:, irr
menbreg cases_adjusted i.time_cat precip_lag proportion_rdt notsuspect i.month || site:, irr
