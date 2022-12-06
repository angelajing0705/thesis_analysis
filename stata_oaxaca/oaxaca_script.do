net set ado "\\Client\C$\Users\jingyuqi\Documents\econ191\analysis\oaxaca"
*ssc install oaxaca
*ssc install outreg2

*import delimited using oaxaca_data_R.csv

oaxaca cl014_348 normalize(case3 case1 case2 case4 case5) normalize(educ_c_14 educ_c_8 educ_c_12 educ_c_16 educ_c_18 educ_c_20) normalize(hhi_c_40 hhi_c_0 hhi_c_20 hhi_c_60 hhi_c_80) normalize(age3 age1 age2 age4) hisp anative black asian pislander race_6mixed unemp_exp, by(imm) pooled relax 

/* oaxaca cl014_348 case1 case2 case4 case5 educ_c_8 educ_c_12 educ_c_16 educ_c_18 educ_c_20 hhi_c_0 hhi_c_20 hhi_c_60 hhi_c_80 age1 age2 age4 hisp anative black asian pislander race_6mixed unemp_exp, by(imm) pooled relax*/

estimates store test
estimates table, star(.1 .05 .01)

global path "\\Client\C$\Users\jingyuqi\Documents\econ191\analysis\oaxaca"
global tables3 "$path/Tables3"
mkdir $tables3
cd $path
outreg2 using $tables3/oaxaca.doc
