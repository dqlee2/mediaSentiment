* Maps countries to IMF's advanced economies classification from 2016 weo
cap drop AE
gen byte AE = .
replace AE = 0 if weo==213 // Argentina
replace AE = 0 if weo==223 // Brazil
replace AE = 0 if weo==228 // Chile
replace AE = 0 if weo==924 // China
replace AE = 1 if weo==134 // Germany
replace AE = 1 if weo==184 // Spain
replace AE = 1 if weo==132 // France
replace AE = 1 if weo==174 // Greece
replace AE = 0 if weo==536 // Indonesia
replace AE = 1 if weo==178 // Ireland
replace AE = 0 if weo==534 // India
replace AE = 1 if weo==136 // Italy
replace AE = 1 if weo==158 // Japan
replace AE = 0 if weo==542 // Korea
replace AE = 0 if weo==273 // Mexico
replace AE = 0 if weo==548 // Malaysia
replace AE = 0 if weo==293 // Peru
replace AE = 0 if weo==566 // Philippines
replace AE = 0 if weo==964 // Poland
replace AE = 1 if weo==182 // Portugal
replace AE = 0 if weo==922 // Russia
replace AE = 0 if weo==578 // Thailand
replace AE = 0 if weo==186 // Turkey
replace AE = 1 if weo==111 // United States
replace AE = 0 if weo==199 // South Africa
lab var AE "Advanced economy based on IMF 2016 weo classification"
tab AE, m
