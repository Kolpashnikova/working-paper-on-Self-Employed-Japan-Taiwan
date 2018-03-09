use "...\tscs122.dta", clear

recode c3e (1/2=3)(3/4=2)(5/9=0)(10/99=.), gen(Employment)

recode a1 (1=0)(2=1), gen(Female)

gen Age = 2012 -(a2y+1911)

recode a15 (98=.), gen(nchild)

recode b1 (1/2=0)(3=6)(4/5=9)(6/9=12)(10=17)(11=14)(12=15)(15/19=17)(20/21=19), gen(EduInYears)

gen Married=0
replace Married=1 if a10==3

recode b9 (1/2=0)(3=6)(4/5=9)(6/9=12)(10=17)(11=14)(12=15)(15/19=17)(20/21=19)(97/99=.), gen(EduInYearsSP)

recode k3 (1=0)(2=5)(3=15)(4=25)(5=35)(6=45)(7=55)(8=65) ///
	(9=75)(10=85)(11=95)(12=105)(13=115)(14=125)(15=135) ///
	(16=145)(17=155)(18=165)(19=175)(20=185)(21=195)(22=250) ///
	(23=300)(97/98=.), gen(PersInc)


recode k3 (1/2=0)(3=5)(4=15)(5=25)(6=35)(7=45)(8=55)(9=65) ///
	(10=75)(11=85)(12=95)(13=105)(14=115)(15=125)(16=135) ///
	(17=145)(18=155)(19=165)(20=175)(21=185)(22=195)(23=250) ///
	(24=300)(97/99=.), gen(PartInc)

	*****
	
mlogit Employment i.Female Age nchild EduInYears ///
	EduInYearsSP i.Married PersInc PartInc, b(0)

mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc, b(0)

margins Female, at(Age=(20(1)50)) atmeans over(Married)  predict(outcome(2)) noesample

marginsplot, yline(0) ///
	ylabel(0(0.05)0.2) ///
	ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
	xtitle("Age", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Non-Married Men" 2 "Married Men" 3 "Non-Married Women" 4 "Married Women"))

margins Female

mtable, at(Female = (0 1))

margins, dydx(Female)

margins Female, predict(outcome(2)) noesample


*******************************************************************
*** calculating margins for self-employed
*******************************************************************

mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc, b(0)

margins Female, at(EduInYears=(0(1)20)) atmeans over(Married)  predict(outcome(2)) noesample

marginsplot, yline(0) ///
	ylabel(0(0.05)0.2) ///
	ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
	xtitle("Age", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Non-Married Men" 2 "Married Men" 3 "Non-Married Women" 4 "Married Women"))

	
mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc, b(0)

margins Female, at(EduInYears=(0(1)20)) atmeans predict(outcome(2)) noesample

marginsplot, yline(0) ///
	ylabel(0(0.05)0.2) ///
	ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
	xtitle("Education", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Men" 2 "Women"))
	
	
mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc, b(0)

margins Female, at(PersInc=(5(10)120)) atmeans predict(outcome(2)) noesample

marginsplot, yline(0) ///
	ylabel(0(0.05)0.2) ///
	ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
	xtitle("Income", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Men" 2 "Women"))
	
	
mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc, b(0)

margins Female, at(nchild=(0(1)10)) atmeans predict(outcome(2)) noesample

marginsplot, yline(0) ///
	ylabel(0(0.05)0.2) ///
	ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
	xtitle("Number of Children", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Men" 2 "Women"))
	
recode a3 (5/97=.), gen(Ethnicity)
label values Ethnicity a3

tab Employment Ethnicity if Ethnicity!=3 & Employment !=2, col chi
tab Employment Ethnicity if Ethnicity!=3 & Employment !=2 & Married==0, col chi
tab Employment Ethnicity if Ethnicity!=3 & Employment !=2 & Married==1, col chi

tab Employment Ethnicity if Ethnicity!=3 & Female==1 & Employment !=2, col chi
tab Employment Ethnicity if Ethnicity!=3 & Female==1 & Employment !=2 & Married==0, col chi
tab Employment Ethnicity if Ethnicity!=3 & Female==1 & Employment !=2 & Married==1, col chi

tab Employment Ethnicity if Ethnicity!=3 & Female==0 & Employment !=2, col chi
tab Employment Ethnicity if Ethnicity!=3 & Female==0 & Employment !=2 & Married==0, col chi
tab Employment Ethnicity if Ethnicity!=3 & Female==0 & Employment !=2 & Married==1, col chi

gen FukieneseHakka = 0
replace FukieneseHakka=1 if Ethnicity ==1 | Ethnicity ==2

mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc i.FukieneseHakka, b(0)

margins FukieneseHakka, at(EduInYears=(0(1)20)) over(Female) atmeans predict(outcome(2)) noesample
marginsplot, yline(0) ///
	ylabel(0(0.05)0.2) ///
	ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
	xtitle("Education", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))

	
*************************** education in family business********************************
mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc i.FukieneseHakka ///
	i.Female##i.FukieneseHakka, b(0) r
margins FukieneseHakka, at(EduInYears=(0(1)20)) over(Female) atmeans predict(outcome(2)) noesample
marginsplot, yline(0) ///
	ylabel(0(0.05)0.2) ///
	ytitle("Likelihood of Working in Family Business", height(10) size(medium)) ///
	xtitle("Education", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))

	/*mlogit Employment i.Female Age nchild EduInYears ///
		i.Married PersInc i.FukieneseHakka, b(0) r
	margins FukieneseHakka, at(EduInYears=(0(1)20)) over(Female) atmeans predict(outcome(2)) noesample
	marginsplot, yline(0) ///
		ylabel(0(0.05)0.2) ///
		ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
		xtitle("Education", height(5)) ///
		title("Taiwan") ///
		legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))
	*/

*************************** Age in family business ********************************
mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc i.FukieneseHakka ///
	i.Female##i.FukieneseHakka, b(0) r
margins FukieneseHakka, at(Age=(20(5)60)) over(Female) atmeans predict(outcome(2)) noesample
marginsplot, yline(0) ///
	ylabel(0(0.05)0.2) ///
	ytitle("Likelihood of Working in Family Busines", height(10) size(medium)) ///
	xtitle("Age", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))

	/*mlogit Employment i.Female Age nchild EduInYears ///
		i.Married PersInc i.FukieneseHakka, b(0) r
	margins FukieneseHakka, at(Age=(20(5)60)) over(Female) atmeans predict(outcome(2))
	marginsplot, yline(0) ///
		ylabel(0(0.05)0.2) ///
		ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
		xtitle("Education", height(5)) ///
		title("Taiwan") ///
		legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))
	*/
	
*************************** education in selfemployed********************************
mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc i.FukieneseHakka ///
	i.Female##i.FukieneseHakka, b(0) r
margins FukieneseHakka, at(EduInYears=(0(1)20)) over(Female) atmeans predict(outcome(3)) noesample
marginsplot, yline(0) ///
	ylabel(0(0.1)0.5) ///
	ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
	xtitle("Education", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))

	/*mlogit Employment i.Female Age nchild EduInYears ///
		i.Married PersInc i.FukieneseHakka, b(0) r
	margins FukieneseHakka, at(EduInYears=(0(1)20)) over(Female) atmeans predict(outcome(3)) noesample
	marginsplot, yline(0) ///
		ylabel(0(0.05)0.2) ///
		ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
		xtitle("Education", height(5)) ///
		title("Taiwan") ///
		legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))
	*/
*************************** Age in selfemployed ********************************
mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc i.FukieneseHakka ///
	i.Female##i.FukieneseHakka, b(0) r
margins FukieneseHakka, at(Age=(20(5)60)) over(Female) atmeans predict(outcome(3)) noesample
marginsplot, yline(0) ///
	ylabel(0(0.1)0.5) ///
	ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
	xtitle("Age", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))

	/*mlogit Employment i.Female Age nchild EduInYears ///
		i.Married PersInc i.FukieneseHakka, b(0) r
	margins FukieneseHakka, at(Age=(20(5)60)) over(Female) atmeans predict(outcome(2))
	marginsplot, yline(0) ///
		ylabel(0(0.05)0.2) ///
		ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
		xtitle("Education", height(5)) ///
		title("Taiwan") ///
		legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))
	*/

	
*************************** education in employed********************************
mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc i.FukieneseHakka ///
	i.Female##i.FukieneseHakka, b(0) r
margins FukieneseHakka, at(EduInYears=(0(1)20)) over(Female) atmeans predict(outcome(0)) noesample
marginsplot, yline(0) ///
	ylabel(0.3(0.1)1) ///
	ytitle("Likelihood of Being Employed", height(10) size(medium)) ///
	xtitle("Education", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))

	/*mlogit Employment i.Female Age nchild EduInYears ///
		i.Married PersInc i.FukieneseHakka, b(0) r
	margins FukieneseHakka, at(EduInYears=(0(1)20)) over(Female) atmeans predict(outcome(3)) noesample
	marginsplot, yline(0) ///
		ylabel(0(0.05)0.2) ///
		ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
		xtitle("Education", height(5)) ///
		title("Taiwan") ///
		legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))
	*/
*************************** Age in employed ********************************
mlogit Employment i.Female Age nchild EduInYears ///
	i.Married PersInc i.FukieneseHakka ///
	i.Female##i.FukieneseHakka, b(0) r
margins FukieneseHakka, at(Age=(20(5)60)) over(Female) atmeans predict(outcome(0)) noesample
marginsplot, yline(0) ///
	ylabel(0.3(0.1)1) ///
	ytitle("Likelihood of Being Employed", height(10) size(medium)) ///
	xtitle("Age", height(5)) ///
	title("Taiwan") ///
	legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))

	/*mlogit Employment i.Female Age nchild EduInYears ///
		i.Married PersInc i.FukieneseHakka, b(0) r
	margins FukieneseHakka, at(Age=(20(5)60)) over(Female) atmeans predict(outcome(2))
	marginsplot, yline(0) ///
		ylabel(0(0.05)0.2) ///
		ytitle("Likelihood of Being SelfEmployed", height(10) size(medium)) ///
		xtitle("Education", height(5)) ///
		title("Taiwan") ///
		legend(order(1 "Other Men" 2 "Other Women" 3 "Fukienese Men" 4 "Fukienese Women"))
	*/

	******************descriptive data
	
	
tab Employment Ethnicity if Ethnicity!=3, col chi
tab Employment Ethnicity if Ethnicity!=3 & Married==0, col chi
tab Employment Ethnicity if Ethnicity!=3 & Married==1, col chi

tab Employment Ethnicity if Ethnicity!=3 & Female==1, col chi
tab Employment Ethnicity if Ethnicity!=3 & Female==1 & Married==0, col chi
tab Employment Ethnicity if Ethnicity!=3 & Female==1 & Married==1, col chi

tab Employment Ethnicity if Ethnicity!=3 & Female==0, col chi
tab Employment Ethnicity if Ethnicity!=3 & Female==0 & Married==0, col chi
tab Employment Ethnicity if Ethnicity!=3 & Female==0 & Married==1, col chi
