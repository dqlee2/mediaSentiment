** Created by: Do Lee
** Order of commands below. 

clear all
set more off
set matsize 10000 
set maxvar 30000
macro drop _all
sysdir set PLUS "..\ado"

********************************************************************************
*** Stata Modules ***
********************************************************************************

* For merging cross-country data from multiple sources by Rafal Raciborski
net install dm0038, replace from(http://www.stata-journal.com/software/sj8-3)

* Update to dm0038
net install dm0038_1, replace from(http://www.stata-journal.com/software/sj10-4/)

* Module to winsorize data
ssc install winsor2, replace

* Module to make regression tables
ssc install estout, replace

* Module to generate statistics using observations within range
ssc install rangestat, replace

* Modules for managing value and variable labels. Will use the labmask command. 
ssc install labutil, replace

* Module to implement Hodrick-Prescott fitler for timeseries data
ssc install hprescott, replace

* Module to extend newey (HAC covariance estimation)
ssc install newey2, replace

* Module to provide alternatives to common Stata commands optimized for large datasets
ssc install ftools, replace

* Module (Mata) to provide various functions (to compute quantiles)
ssc install moremata, replace

* Module to perform linear or instrumental-variable regression
* absorbing any number of high-dimensional fixed effects
ssc install reghdfe, replace

* Module to calculate robust standard errors for panels with cross-sectional dependence
ssc install xtscc, replace

********************************************************************************
*** Global Macros ***
********************************************************************************

* Number of lags
global Nlags = 8

* Projection horizon
global horizon = 20
global US_horizon = 15
global horizonFlows = 30

* Specify significance level
scalar sig1 = 0.05
scalar sig2 = 0.3

********************************************************************************
*** Order of Commands ***
********************************************************************************

do news.do
do data.do
do epfr.do
do dfm.do
do clean.do

do table_2.do
do table_a1.do
do table_a3.do
do table_a4.do
do figure_1.do
do figure_2.do
do figure_3.do
do figure_4.do
do figure_5.do
do figure_6.do
do figure_7.do
do figure_8.do
do figure_a1.do
do figure_a3.do
do figure_a4.do
do figure_a5.do
do figure_a6.do
do figure_a7.do
do figure_a8.do
do figure_a9.do
do figure_a10.do
do figure_a11.do

*** End of File ***
