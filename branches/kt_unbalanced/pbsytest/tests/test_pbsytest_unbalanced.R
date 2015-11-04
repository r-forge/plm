# test pbsytest() - unbalanced and balanced version



################### Bera, Sosa-Escudero and Yoon (2001) and joint test of Baltagi/Li (1991) ###############
# see Baltagi (2005), Econometric Analysis of Panel Data, 3rd edition, pp. 96-97
#  or Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, pp. 108:
# "For the Grunfeld data, we computed LM_mu = 798.162 in Table 4.2 using the xttest0 command
# in Stata. Using TSP, LM_p = 143.523, LM*_mu = 664.948, LM*_p = 10.310 and the joint LM1 statistic
# in (5.36) is 808.471.
# The joint test rejects the null of no first-order serial correlation and no random firm effects.
# The one-directional tests LM_p and LM*_p reject the null of no first-order serial correlation,
# while the one-directional tests LM_mu and LM*_mu reject the null of no random firm effects."


library(plm)
data("Grunfeld")
Grunfeldpdata <- pdata.frame(Grunfeld, index = c("firm", "year"), drop.index = FALSE, row.names = TRUE)
form_gunfeld <- formula(inv ~ value + capital)
pool_grunfeld <- plm(form_gunfeld, data=Grunfeldpdata, model="pooling")

plm::pbsytest(pool_grunfeld, test = "ar") # chisq = 10.31    => LM*_p  in Baltagi's book
plm::pbsytest(pool_grunfeld, test = "re") # chisq = 664.9694 => LM*_mu in Baltagi's book [sqrt(chisq) = z = 25.787 in plm v1.4-0 as on CRAN]
plm::pbsytest(pool_grunfeld, test = "j")  # chisq = 808.47   => LM1    in Baltagi's book

# formula interface
plm::pbsytest(form_gunfeld, data=Grunfeld, test = "ar")
plm::pbsytest(form_gunfeld, data=Grunfeld, test = "re")
plm::pbsytest(form_gunfeld, data=Grunfeld, test = "j")


# each should result in an error
plm::pbsytest(pool_grunfeld_half, test = "ar", normal = T)
plm::pbsytest(pool_grunfeld_half, test = "j",  normal = T)
plm::pbsytest(form_gunfeld, data=Grunfeld, test = "ar", normal = T)
plm::pbsytest(form_gunfeld, data=Grunfeld, test = "j",  normal = T)


############### balanced version ###################
### Results from Bera et al. (2001), p. 13:

## Bera/Sosa-Escudero/Yoon (2001), Tests for the error component model in the presence of local misspecifcation,
##                                 Journal of Econometrics 101 (2001), pp. 1-23.

# To replicate, a special version of the Grunfeld data set is needed: only 5 selected firms (total of 100 obs)
# from http://pages.stern.nyu.edu/~wgreene/Text/tables/TableF13-1.txt
# or   http://statmath.wu.ac.at/~zeileis/grunfeld/TableF13-1.txt

Grunfeld_greene_5firms <- read.csv("http://pages.stern.nyu.edu/~wgreene/Text/tables/TableF13-1.txt", sep="")
# Grunfeld_greene_5firms <- read.csv("http://statmath.wu.ac.at/~zeileis/grunfeld/TableF13-1.txt", sep="") # alternative source

# Matching to Grunfeld data set in plm
# Grunfeld[c(1:20, 41:60), 3:5] == Grunfeld_greene_5firms[c(1:20, 41:60), 3:5]
# Grunfeld[61:80, 3:5]          == Grunfeld_greene_5firms[21:40, 3:5]
# Grunfeld[141:160, 3:5]        == Grunfeld_greene_5firms[61:80, 3:5]
# Grunfeld[21:40, 3:5]          == Grunfeld_greene_5firms[81:100, 3:5] # almost all equal, 3 values differ

pGrunfeld_greene_5firms <- pdata.frame(Grunfeld_greene_5firms, index = c("Firm", "Year"), drop.index = FALSE, row.names = TRUE)
form_gunfeld_half <- formula(I ~ F + C)
pool_grunfeld_half <- plm(form_gunfeld_half, data=pGrunfeld_greene_5firms, model="pooling")
re_grunfeld_half   <- plm(form_gunfeld_half, data=pGrunfeld_greene_5firms, model="random")

plm::pbsytest(pool_grunfeld_half, test = "ar")             # chisq = 3.7125         => RS*_p in Bera et al. (2001), p. 13
plm::pbsytest(pool_grunfeld_half, test = "re")             # chisq = 384.183        => RS*_u  [sqrt(chisq) = z = 19.601 in plm v1.4-0 as on CRAN]
plm::pbsytest(pool_grunfeld_half, test = "re", normal = T) # normal = 19.601; p = 0 => RSO*_u (tiny diff due to rounding)
plm::pbsytest(pool_grunfeld_half, test = "j")              # chisq = 457.53         => RS_up

# plmtest's statistic is also mentioned in paper
plm::plmtest(pool_grunfeld_half, type = "bp")              # chisq = 453.82   => RS_u   in Bera et al. (2001), p. 13
plm::plmtest(pool_grunfeld_half, type = "honda")           # normal = 21.3031 => RSO_u

## RS_p in Bera et al (2001), p. 9 (formula 19) is not implemented
## it's origin is in Baltagi/Li (1991), but there is is just a side result
## in terms of n, t, b of pbsystest it is: (n*t^2*(B^2)) / (t-1)

# formula interface
plm::pbsytest(form_gunfeld_half, data=pGrunfeld_greene_5firms, test = "ar")          
plm::pbsytest(form_gunfeld_half, data=pGrunfeld_greene_5firms, test = "re")
plm::pbsytest(form_gunfeld_half, data=pGrunfeld_greene_5firms, test = "re", normal = T)
plm::pbsytest(form_gunfeld_half, data=pGrunfeld_greene_5firms, test = "j")

plm::plmtest(form_gunfeld_half, data=pGrunfeld_greene_5firms, type = "bp")


############ Replicate tests from original paper Sosa-Escudero/Bera (2008) ####################
############ unbalanced panel                                              ####################
##
## data set for test from Sosa-Escudero/Bera (2008), pp. 75-77
## available as STATA .dta file at http://www.stata-journal.com/software/sj8-1/sg164_1/ginipanel5.dta
##
## Sosa-Escudero/Bera (2008), Tests for unbalanced error-components models under local misspecification,
##                            The Stata Journal (2008), Vol. 8, Number 1, pp. 68-78.

library(haven)
ginipanel5 <- read_dta("http://www.stata-journal.com/software/sj8-1/sg164_1/ginipanel5.dta")
pginipanel5 <- pdata.frame(ginipanel5, index = c("naglo", "ano"), drop.index = FALSE, row.names = TRUE)

# STATA command for RE model: xtreg gini ie ie2 indus adpubedsal desempleo tactiv invipib apertura pyas4 e64 supc tamfam, re
# use pooling model in R:
formula_gini <- formula(gini ~ ie + ie2 + indus + adpubedsal + desempleo + tactiv + invipib + apertura + pyas4 + e64 + supc + tamfam)
pool_gini <- plm(formula_gini, data=pginipanel5, model="pooling")

pdim(pool_gini) # Unbalanced Panel: n=17, T=6-8, N=128

# STATA's Output of xttest1, unadjusted (Sosa-Escudero/Bera (2008), p. 77):
#
# Random Effects, Two Sided:
#   LM(Var(u)=0) = 13.50 Pr>chi2(1) = 0.0002
#  ALM(Var(u)=0) = 6.03  Pr>chi2(1) = 0.0141              # test="re"
#
# Random Effects, One Sided:
#   LM(Var(u)=0) = 3.67 Pr>N(0,1) = 0.0001
#  ALM(Var(u)=0) = 2.46 Pr>N(0,1) = 0.0070                # test="re", normal = T
#
# Serial Correlation:
#   LM(lambda=0) = 9.32 Pr>chi2(1) = 0.0023
#  ALM(lambda=0) = 1.86 Pr>chi2(1) = 0.1732               # test="ar"
#
# Joint Test:
#   LM(Var(u)=0,lambda=0) = 15.35 Pr>chi2(2) = 0.0005     # test="j"


plm::pbsytest(pool_gini, test = "re")             # chisq = 6.0288793,  df = 1, p-value = 0.01407367
plm::pbsytest(pool_gini, test = "re", normal = T) # normal = 2.4553776,         p-value = 0.007036833
plm::pbsytest(pool_gini, test = "ar")             # chisq = 1.8550073,  df = 1, p-value = 0.1732021
plm::pbsytest(pool_gini, test = "j")              # chisq = 15.352307,  df = 2, p-value = 0.0004637552

# formula interface
plm::pbsytest(formula_gini, data=pginipanel5, test = "re")             # chisq = 6.0288793,  df = 1, p-value = 0.01407367
plm::pbsytest(formula_gini, data=pginipanel5, test = "re", normal = T) # normal = 2.4553776,   n/a   p-value = 0.007036833
plm::pbsytest(formula_gini, data=pginipanel5, test = "ar")             # chisq = 1.8550073,  df = 1, p-value = 0.1732021
plm::pbsytest(formula_gini, data=pginipanel5, test = "j")              # chisq = 15.352307,  df = 2, p-value = 0.0004637552
