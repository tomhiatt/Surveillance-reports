# -------------------------------------------------
# Tables in the global report
# Tom Hiatt
# 6 July 2012, updated 23 June 2014
# -------------------------------------------------

source('d:/users/hiattt/Dropbox/Code/Surveillance reports/Setup.r')

# tb_burden -------------------------------------------------------------------

# Numbers

tag <- read.csv("D:/Users/hiattt/Google Drive/Work files/Global TB report/Tables and Figures/From others/PG/Code needs to access/tab2_1.csv")
# tagvars <- c("e_pop_num", "e_mort_exc_tbhiv_num", "e_mort_exc_tbhiv_num_lo", "e_mort_exc_tbhiv_num_hi", "e_mort_tbhiv_num", "e_mort_tbhiv_num_lo", "e_mort_tbhiv_num_hi", "e_prev_num", "e_prev_num_lo", "e_prev_num_hi", "e_inc_num", "e_inc_num_lo", "e_inc_num_hi", "e_inc_tbhiv_num", "e_inc_tbhiv_num_lo", "e_inc_tbhiv_num_hi")
# tag <- subset(tb, g_hbc22=='high' & year==thisyear-1, c('country', tagvars))
# 
# names(tag)[1] <- 'group_name'
# tag <- rbind(tag, subset(araw, group_type %in% c('global', 'g_whoregion'), c('group_name', tagvars)))
# 
# names(tag)[1] <- 'rowname'

tag <- .shortnames(tag, col="rowname")
# tag <- .shortnames(tag, col="country", ord='hbc')
# tag[1:22,] <- tag[order(tag[1:22, 'rowname']),]
# tag[24:29,] <- tag[order(tag[23:28, 'rowname']),]

tag[2] <- rounder(tag[[2]]/1000)

for(col in 3:ncol(tag)){
  tag[col] <- ifelse(is.na(tag[[col]]), "–", frmt(tag[[col]]/1000))
} 

tag[tag$rowname=="India", "rowname"] <- "India(d)"
# tag[tag$country=="India", "country"] <- "India(d)"

cat(paste("<font size=5><b>Estimated epidemiological burden of TB, ", thisyear-1, ".</b> Numbers in thousands.<sup>a</sup></font>", sep=""), file=glue("Tables/burden_num", Sys.Date(), ".htm"))

tai <- xtable(tag)
print(tai, type="html", file=glue("Tables/burden_num", Sys.Date(), ".htm"), include.rownames=F, include.colnames=F, append=T,
      html.table.attributes="border=0 rules=rows width=1100",
      add.to.row=list(pos=list(0,30), command=c("<TR width=600> <TH colspan=2></TH> <TH colspan=3>MORTALITY<sup>b</sup></TH> <TH colspan=3>HIV-POSITIVE <br>TB MORTALITY</TH> 
	<TH colspan=3>PREVALENCE</TH> <TH colspan=3>INCIDENCE</TH> 
<TH colspan=3>HIV-POSITIVE <br>INCIDENT TB CASES</TH> </TR>
<TR> <TH></TH> <TH>POPULATION</TH> <TH>BEST<sup>c</sup></TH> <TH>LOW</TH> <TH>HIGH</TH> <TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH> <TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH> <TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH> <TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH> </TR>", 
"<TR> <TD colspan=14><sup>a</sup> Numbers for mortality, prevalence and incidence shown to two significant figures. Totals (HBCs, regional and global) are computed prior to rounding. <br><sup>b</sup> Mortality excludes deaths among HIV-positive TB cases. Deaths among HIV-positive TB cases are classified as HIV deaths according to ICD-10 and are shown separately in this table. <br><sup>c</sup> Best, low and high indicate the point estimate and lower and upper bounds of the 95% uncertainty interval. <br><sup>d</sup> Estimates for India have not yet been officially approved by the Ministry of Health & Family Welfare, Government of India, and should therefore be considered provisional.</TD></TR>")))

tablecopy("burden_num")

# Rates

# tah <- read.csv("D:/Users/hiattt/Google Drive/Work files/Global TB report/Tables and Figures/From others/PG/Code needs to access/tab2_2.csv")
tahvars <- c("e_pop_num", "e_mort_exc_tbhiv_100k", "e_mort_exc_tbhiv_100k_lo", "e_mort_exc_tbhiv_100k_hi", "e_mort_tbhiv_100k", "e_mort_tbhiv_100k_lo", "e_mort_tbhiv_100k_hi", "e_prev_100k", "e_prev_100k_lo", "e_prev_100k_hi", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_tbhiv_prct", "e_tbhiv_prct_lo", "e_tbhiv_prct_hi")
tai <- subset(tb, g_hbc22=='high' & year==thisyear-1, c('country', tahvars))


# import PG's last 2 rows to get raw values (until db fixed)
tbhivwa <- read.csv("D:/Users/hiattt/Google Drive/Work files/Global TB report/Tables and Figures/From others/PG/Code needs to access/tab2_2.csv")
names(tbhivwa)[1] <- 'country'
tai <- merge(tai, tbhivwa[1:22,c('country', "tbhiv", "tbhiv.lo", "tbhiv.hi")], all.x=TRUE)

tai[c("e_tbhiv_prct", "e_tbhiv_prct_lo", "e_tbhiv_prct_hi")] <- tai[c("tbhiv", "tbhiv.lo", "tbhiv.hi")] * 100

tai <- tai[c('country', tahvars)]

warning('Raw values not yet in DB. If I\'m wrong, then remove this part.')



# AF and BG fix
if(is.na(tai[tai$country=='Afghanistan', 'e_tbhiv_prct_lo'])){
  tai[1:2, 'e_tbhiv_prct_lo'] <- tai[1:2, 'e_tbhiv_prct'] * 0.75
  tai[1:2, 'e_tbhiv_prct_hi'] <- tai[1:2, 'e_tbhiv_prct'] * 1.25
  warning('Afghanistan and Bangladesh TB/HIV ranges have been guesstimated.')
}

names(tai)[1] <- 'group_name'
tah <- rbind(tai, subset(araw, group_type %in% c('global', 'g_whoregion') & year==thisyear-1, c('group_name', tahvars)))
tah[nrow(tah)+1,'group_name'] <- 'High-burden countries'

for(est in c("e_mort_exc_tbhiv_100k", "e_mort_tbhiv_100k", "e_prev_100k", "e_inc_100k", "e_tbhiv_prct")){
  
  tah[tah$group_name=='High-burden countries', c(est, glue(est, '_lo'), glue(est, '_hi'))] <- subset(add.rv(tai[est]/1e5, tai[glue(est, '_lo')]/1e5, tai[glue(est, '_hi')]/1e5, weights=tai$e_pop_num), select=c(r, r.lo, r.hi)) * 1e5
}

tah[tah$group_name=='High-burden countries','e_pop_num'] <- sum(tai$e_pop_num)

# for(est in c("e_tbhiv_prct")){ # Currently dropping Afghanistan
#   
#   tah[tah$group_name=='High-burden countries', c(est, glue(est, '_lo'), glue(est, '_hi'))] <- subset(add.rv(tai[2:22,est]/100, tai[2:22,glue(est, '_lo')]/100, tai[2:22,glue(est, '_hi')]/100, weights=tai[2:22,'e_pop_num']), select=c(r, r.lo, r.hi)) * 100
#   
# } # This ain't workin so I'll have to workaround in the meantime.

# tbhivwa <- read.csv("D:/Users/hiattt/Google Drive/Work files/Global TB report/Tables and Figures/From others/PG/Code needs to access/tab2_2.csv")
# tah[tah$group_name=='High-burden countries', c('e_pop_num', "e_tbhiv_prct", "e_tbhiv_prct_lo", "e_tbhiv_prct_hi")] <- c(sum(tai$e_pop_num), subset(tbhivwa, rowname=='High-burden countries', c("tbhiv", "tbhiv.lo", "tbhiv.hi"))*100)


names(tah)[1] <- 'rowname'

tah <- .shortnames(tah, col="rowname", ord='hbc')

# tah[1:22,] <- tah[order(tah[1:22, 'rowname']),]

tah[2] <- rounder(tah[[2]]/1000)

for(col in 3:14){
  tah[col] <- frmt(tah[[col]], rates=TRUE)
} 

for(col in 15:17){
  tah[col] <- ifelse(tah[[col]] < 0.1, "<0.1", ifelse(tah[[col]] < 1, signif(tah[[col]],2), signif(tah[[col]],3) ))
} 

# for(col in 12:14){
#   tah[col] <- ifelse(is.na(tah[[col]]), "–", frmt(tah[[col]] * 100))
# } 

tah[tah$rowname=="India", "rowname"] <- "India(c)"

cat(paste("<font size=5><b>Estimated epidemiological burden of TB, ", thisyear-1, ".</b> Rates per 100 000 population except where indicated.<sup>a</sup></font>", sep=""), file=glue("Tables/burden_rt", Sys.Date(), ".htm"))

taj <- xtable(tah)
print(taj, type="html", file=glue("Tables/burden_rt", Sys.Date(), ".htm"), include.rownames=F, include.colnames=F, append=T,
      html.table.attributes="border=0 rules=rows width=1100",
      add.to.row=list(pos=list(0,30), command=c("<TR width=600> <TH colspan=2></TH> <TH colspan=3>MORTALITY<sup>a</sup></TH> 
  <TH colspan=3>HIV-POSITIVE TB MORTALITY</TH> <TH colspan=3>PREVALENCE</TH> <TH colspan=3>INCIDENCE</TH> 
  <TH colspan=3>HIV PREVALENCE IN <br>INCIDENT TB CASES (%)</TH> </TR>
  <TR> <TH></TH> <TH>POPULATION<br>(THOUSANDS)</TH> <TH>BEST<sup>b</sup></TH> <TH>LOW</TH> <TH>HIGH</TH> <TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH> <TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH> <TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH> <TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH> </TR>", 
"<TR> <TD colspan=17><sup>a</sup> Mortality excludes deaths among HIV-positive TB cases. Deaths among HIV-positive TB cases are classified as HIV deaths according to ICD-10 and are shown separately in this table. <br><sup>b</sup> Best, low and high indicate the point estimate and lower and upper bounds of the 95% uncertainty interval. <br><sup>c</sup> Estimates for India have not yet been officially approved by the Ministry of Health & Family Welfare, Government of India, and should therefore be considered provisional.</TD></TR>")))

tablecopy("burden_rt")

# for country profiles

taca <- subset(e.t, g_hbc22=='high' & year==thisyear-1, select=c('country', 'year', "e_mort_exc_tbhiv_num", "e_mort_exc_tbhiv_num_lo", "e_mort_exc_tbhiv_num_hi", "e_mort_exc_tbhiv_100k", "e_mort_exc_tbhiv_100k_lo", "e_mort_exc_tbhiv_100k_hi", "e_prev_num", "e_prev_num_lo", "e_prev_num_hi", "e_prev_100k", "e_prev_100k_lo", "e_prev_100k_hi", "e_inc_num", "e_inc_num_lo", "e_inc_num_hi", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_num", "e_inc_tbhiv_num_lo", "e_inc_tbhiv_num_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "c_cdr", "c_cdr_lo", "c_cdr_hi"))

nums <- names(taca)[grep("num", names(taca))]
rates <- names(taca)[grep("100k", names(taca))]

tada <- taca

for(var in nums){
  tada[var] <- frmt(taca[[var]] / 1000, thou=TRUE)
}
for(var in rates){
  tada[var] <- frmt(tada[[var]], rates=TRUE)
}

# tada <- cbind(glue(taca$country, taca$year), taca)

write.csv(tada, file=glue("Tables/burden_cp", Sys.Date(), ".csv"), row.names=FALSE)

# notif -------------------------------------------------------------------

tbb <- subset(n, year==thisyear-1, select=c('country', 'g_whoregion', 'g_hbc22', 'c_notified', 'c_newinc', "new_labconf", "new_clindx", "new_ep", "ret_rel_labconf", "ret_rel_clindx", "ret_rel_ep", "ret_nrel", "notif_foreign"))

tbb$new.pulm <- .rowsums(tbb[c("new_labconf", "new_clindx", "ret_rel_labconf", "ret_rel_clindx")])

tbb <- tbb[order(tbb$country),]
names(tbb)[names(tbb)=='country'] <- 'area'

# make aggregate rows
tbbh <- aggregate(tbb[4:ncol(tbb)], by=list(area=tbb$g_hbc22), FUN=sum, na.rm=TRUE)
tbbh <- tbbh[tbbh$area=='high',]
tbbh$area <- 'High-burden countries'
tbbr <- aggregate(tbb[4:ncol(tbb)], by=list(area=tbb$g_whoregion), FUN=sum, na.rm=TRUE)
tbbg <- tbb; tbbg$area <- 'Global'
tbbga <- aggregate(tbbg[4:ncol(tbb)], by=list(area=tbbg$area), FUN=sum, na.rm=TRUE)

# combine together
tbc <- rbind(tbb[tbb$g_hbc22=='high', c(1, 4:ncol(tbb))], tbbh, tbbr, tbbga)

# Fill in data for countries not reporting lab confirmed (see below too)
# tbc$new_labconf2 <- ifelse(is.na(tbc$new_labconf), tbc$new_sp, tbc$new_labconf)

# calculate and format vars
# tbc$labconf <- rowSums(tbc[c('new_labconf2', 'ret_labconf')]) 
# tbc$notif_lab <- tbc$labconf / tbc$c_notified_lab * 100
# tbc$ret_nrel <- tbc$c_ret - tbc$ret_rel
tbc$nrpulm_lab_pct <- tbc$new_labconf / tbc$new.pulm * 100

tbc$notif_foreign_pct <- tbc$notif_foreign / tbc$c_notified * 100


for(var in 2:ncol(tbc)){
  tbc[var] <- rounder(tbc[[var]])
}

tbc[is.na(tbc$nrpulm_lab_pct), 'nrpulm_lab_pct'] <- "–"
tbc[is.na(tbc$notif_foreign_pct), 'notif_foreign_pct'] <- "–"

# rename
tbc <- .shortnames(tbc, col='area')

# Add footnote for countries not reporting Lab confirmed (Thailand)
# footnote.b <- ifelse(any(is.na(tbc$new_labconf)), paste('(b) LABORATORY CONFIRMED data for', paste(subset(tbc, is.na(new_labconf), 'area'), collapse=', '), 'refer to smear-positive cases only. Data on cases that were laboratory confirmed using other methods were not reported.'), "")
# tbc$new_labconf2 <- ifelse(is.na(tbc$new_labconf), glue(tbc$new_sp, "(b)"), tbc$new_labconf2)

# Add to file
  
tbm <- xtable(tbc[c("area", "c_notified", "notif_foreign_pct", 'c_newinc', "new_labconf", "new_clindx", "new_ep", "ret_rel_labconf", "ret_rel_clindx", "ret_rel_ep", "nrpulm_lab_pct", "ret_nrel")])

digits(tbm) <- 0

cat(paste("<h3>Case notifications,", thisyear-1, "</h3>"), file=glue("Tables/notif", Sys.Date(), ".htm"))

print(tbm, type="html", file=glue("Tables/notif", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, html.table.attributes="border=0 rules=rows width=1100 cellpadding=5", append=T, add.to.row=list(pos=list(0, nrow(tbm)), command=c(
"<TR> <TD colspan=4></TD> 
	<TH colspan=3>NEW OR PREVIOUS TREATMENT HISTORY UNKNOWN</TH> 
  <TH colspan=3>RELAPSE</TH> 
  <TD colspan=2></TD> </TR> 
  <TR> <TD></TD> <TD>TOTAL NOTIFIED</TD> 
  <TD>PERCENT FOREIGN-BORN</TD> 
  <TD>NEW AND RELAPSE(a)</TD> 
  <TD>PULMONARY BACTERIOLOGICALLY CONFIRMED</TD>
  <TD>PULMONARY CLINICALLY DIAGNOSED</TD> 
  <TD>EXTRAPULMONARY</TD>   
  <TD>PULMONARY BACTERIOLOGICALLY CONFIRMED</TD>
  <TD>PULMONARY CLINICALLY DIAGNOSED</TD> 
  <TD>EXTRAPULMONARY</TD> 
  <TD>PERCENTAGE OF PULMONARY CASES LABORATORY CONFIRMED</TD> 
  <TD>RETREATMENT EXCL. RELAPSE</TD> 
 </TR>", 
	"<TR> <TD colspan=12>Blank cells indicate data not reported.<br>
(a) NEW AND RELAPSE includes cases for which the treatment history is unknown.</TD></TR>")))

tablecopy("notif")

# tsr new_sp and all

# tsr_sp -------------------------------------------------------------------

# Get country tsr and cohort size
tcb <- subset(o, g_hbc22=='high' & year >= 1995 & year<thisyear-1, select=c('country', 'year', 'c_new_sp_tsr', 'new_sp_coh'))

tcb <- .shortnames(tcb)
tcb <- tcb[order(tcb$country),]

# reshape to wide
tcc <- cast(tcb, country~year, value='c_new_sp_tsr')
tcd <- cast(tcb, country~year, value='new_sp_coh')

# HBC and regional aggregates (note this is just a straight up sum)
tca <- c('year', 'c_new_sp_tsr', 'new_sp_coh', "new_sp_cur", "new_sp_cmplt")

tce <- subset(o, year >= 1995 & year < thisyear-1, select=c('g_whoregion', 'g_hbc22', tca))

tcf <- aggregate(tce[4:7], by=list(tce$g_whoregion, tce$year), FUN=sum, na.rm=T)
tcg <- aggregate(tce[4:7], by=list(tce$g_hbc22, tce$year), FUN=sum, na.rm=T)
tch <- tcg
tch[1] <- 'Global'
tci <- aggregate(tch[3:6], by=list(tch$Group.1, tch$Group.2), FUN=sum, na.rm=T)

# Combine aggregate rows
tcj <- tcg[tcg$Group.1=="high",]
tcj <- merge(tcj, tcf, all=T)
tcj <- merge(tcj, tci, all=T)
tcj[tcj$Group.1=="high",1] <- 'High-burden countries'

# calculate tsr
tcj <- within(tcj, c_new_sp_tsr <- round((new_sp_cur + new_sp_cmplt) / new_sp_coh *100, 0))
names(tcj)[1:2] <- c('country', 'year')
tcj <- .shortnames(tcj)

# reshape to wide
tck <- cast(tcj, country~year, value='c_new_sp_tsr')
tck <- tck[order(c(2:5, 8, 1, 6:7)),]

tcl <- cast(tcj, country~year, value='new_sp_coh')
tcl <- tcl[order(c(2:5, 8, 1, 6:7)),]

# Combine all (country and aggregate)
tc_tsr <- tcc
tc_tsr[23:30,] <- tck

tc_coh <- rbind(tcd, tcl)

# Change coh to thousands
for(col in 2:ncol(tc_coh)){
  tc_coh[col] <- rounder(tc_coh[[col]] / 1000, decimals=TRUE)
}

# remove country column names
names(tc_tsr)[1] <- names(tc_coh)[1] <- ""

# fill in – for missing tsr

for(var in 2:ncol(tc_tsr)){
  tc_tsr[var] <- ifelse(is.na(tc_tsr[[var]]), '–', frmt(tc_tsr[[var]]))
}

# Add to file

cat(paste("<h3>Treatment success for new smear-positive cases (%) and cohort size (thousands), 1995–", thisyear-2, "</h3>a. Treatment success (%)", sep=""), file=glue("Tables/sp_tsr", Sys.Date(), ".htm"))

tc_tsra <- xtable(tc_tsr)
digits(tc_tsra) <- 0
print(tc_tsra, type="html", file=glue("Tables/sp_tsr", Sys.Date(), ".htm"),include.rownames=F, include.colnames=T, append=T, 
      html.table.attributes="border=0 rules=rows width=900")

cat("<br>b. Cohort size (thousands)", file=glue("Tables/sp_tsr", Sys.Date(), ".htm"), append=T)

tc_coha <- xtable(tc_coh)

print(tc_coha, type="html", file=glue("Tables/sp_tsr", Sys.Date(), ".htm"),include.rownames=F, include.colnames=T, append=T,
      html.table.attributes="border=0 rules=rows width=900",
      add.to.row=list(pos=list(30), command=c(paste("<TR> <TD colspan=", thisyear-1995, ">Blank cells indicate data not reported. – indicates values that cannot be calculated.</TD></TR>", sep=""))))

tablecopy("sp_tsr")


# All (smear pos, extrapulm, ret)
# allnew_tsr -------------------------------------------------------------------

tcbb <- subset(o, year>=1995 & year<thisyear-1, select=c('country', 'year', 'g_hbc22', 'g_whoregion', 'new_sp_coh', 'new_sp_cur', 'new_sp_cmplt', 'new_snep_coh', 'new_snep_cmplt'))

tcbb <- .shortnames(tcbb)
tcbb <- tcbb[order(tcbb$country),]

tcbb$succ <- .rowsums(tcbb[c('new_sp_cur', 'new_sp_cmplt', 'new_snep_cmplt')])
tcbb$coh <- .rowsums(tcbb[c('new_sp_coh', 'new_snep_coh')])

tcbbc <- tcbb[tcbb$g_hbc22=='high', c(1:2, 10:11)]
names(tcbbc)[names(tcbbc)=="country"] <- 'area'
tcbbh <- aggregate(tcbb[10:11], by=list(area=tcbb$g_hbc22, year=tcbb$year), FUN=sum, na.rm=TRUE)
tcbbh <- tcbbh[tcbbh$area=='high',]
tcbbh$area <- 'High-burden countries'
tcbbr <- aggregate(tcbb[10:11], by=list(area=tcbb$g_whoregion, year=tcbb$year), FUN=sum, na.rm=TRUE)
tcbbg <- aggregate(tcbb[10:11], by=list(year=tcbb$year), FUN=sum, na.rm=TRUE)
tcbbg$area <- 'Global'

tcba <- rbind(tcbbc, tcbbh, tcbbr, tcbbg)
tcba$tsr <- tcba$succ / tcba$coh * 100
tcba <- .shortnames(tcba, col='area')
order <- unique(tcba$area)

tcbc <- cast(tcba, area~year, value='tsr')
tcbd <- cast(tcba, area~year, value='coh')

tcbc$area <- factor(tcbc$area, levels=order)
tcbc <- tcbc[order(tcbc$area),]

tcbd$area <- factor(tcbd$area, levels=order)
tcbd <- tcbd[order(tcbd$area),]

tcb_coh <- tcbd

for(col in 2:ncol(tcb_coh)){
  tcb_coh[col] <- rounder(tcb_coh[[col]] / 1000, decimals=TRUE)
}

names(tcbc)[1] <- names(tcb_coh)[1] <- ""

# fill in – for missing tsr

for(var in 2:ncol(tcbc)){
  tcbc[var] <- ifelse(is.na(tcbc[[var]]), '–', frmt(tcbc[[var]]))
}

# Add to file

cat(paste("<h3>Treatment success for all new cases (%) and cohort size (thousands), 1995–", thisyear-2, "</h3>a. Treatment success (%)", sep=""), file=glue("Tables/allnew_tsr", Sys.Date(), ".htm"))

tcb_tsra <- xtable(tcbc)
digits(tcb_tsra) <- 0
print(tcb_tsra, type="html", file=glue("Tables/allnew_tsr", Sys.Date(), ".htm"),include.rownames=F, include.colnames=T, append=T, html.table.attributes="border=0 rules=rows width=900")

cat("<br>b. Cohort size (thousands)", file=glue("Tables/allnew_tsr", Sys.Date(), ".htm"), append=T)

tcb_coha <- xtable(tcb_coh)

print(tcb_coha, type="html", file=glue("Tables/allnew_tsr", Sys.Date(), ".htm"),include.rownames=F, include.colnames=T, append=T, html.table.attributes="border=0 rules=rows width=900", add.to.row=list(pos=list(30), command=c(paste("<TR> <TD colspan=", thisyear-1995, ">Blank cells indicate data not reported. – indicates values that cannot be calculated.</TD></TR>", sep=""))))

tablecopy("allnew_tsr")

#-------------------------------------------------------------------
# cdr
#-------------------------------------------------------------------
if(!identical(e.t, e)) warning('These estimates are currently phony!')

# get country rows
tda <- subset(e.t, g_hbc22=='high' & year %in% c(1995, 2000, 2005, 2010, thisyear-1), select=c('country', 'year', 'g_hbc22', 'g_whoregion', 'c_cdr_lo', 'c_cdr', 'c_cdr_hi'))
tda$group_name <- tda$country

# get HBC aggregate row (requires PG's add.rv estimates aggregator)
tda1 <- merge(n.t, e.t)
tdhb <- subset(tda1, g_hbc22=='high' & year %in% c(1995, 2000, 2005, 2010, thisyear-1), select=c('country', 'year', 'g_hbc22', 'g_whoregion', 'e_inc_100k_lo', 'e_inc_100k', 'e_inc_100k_hi', 'e_pop_num', 'c_newinc'))

tdhb2 <- aggregate(tdhb['c_newinc'], by=list(year=tdhb$year), FUN=sum, na.rm=TRUE)
tdhb2[c('e_inc_num_lo', 'e_inc_num', 'e_inc_num_hi')] <- NA

for(yea in unique(tdhb$year)){
  tdhb3 <- subset(tdhb, year==yea)
  tdhb4 <- add.rv(tdhb3$e_inc_100k/1e5, tdhb3$e_inc_100k_lo/1e5, tdhb3$e_inc_100k_hi/1e5, weights=tdhb3$e_pop_num)
  tdhb2[tdhb2$year==yea, c('e_inc_num_lo', 'e_inc_num', 'e_inc_num_hi')] <- tdhb4[c('r.lo.num', 'r.num', 'r.hi.num')]
}

tdhb2 <- within(tdhb2, {
  group_name <- 'High-burden countries'
  c_cdr_lo <- round(c_newinc / e_inc_num_hi *100)
  c_cdr <- round(c_newinc / e_inc_num *100)
  c_cdr_hi <- round(c_newinc / e_inc_num_lo *100)
}) 

# get aggregates for Regions and global
tdr <- subset(a.t, group_type %in% c('g_whoregion', 'global') & year %in% c(1995, 2000, 2005, 2010, thisyear-1), select=c('group_name', 'year', 'c_cdr_lo', 'c_cdr', 'c_cdr_hi'))

# Merge and reshape 
tdb <- merge(tda, tdhb2, all=TRUE)
tdb <- merge(tdb, tdr, all=TRUE)
tdc <- melt(tdb, measure.vars=c('c_cdr', 'c_cdr_lo', 'c_cdr_hi'), id=c('group_name', 'year')) 

tdd <- cast(tdc, group_name~...)

# Order table, fix names
ordrd <- c(sort(unique(tda$group_name)), 'High-burden countries', sort.int(unique(tdr[tdr$group_name != 'global', 'group_name'])), 'global')
tdd <- tdd[match(ordrd, tdd$group_name),]

tde <- .shortnames(tdd, col='group_name') 


# 
# tde[1:22,] <- tde[order(tde[1:22,'rowname']),]
# 
# tde[1:22,] <- tde[1:22, c("rowname", "cdr.1995", "cdr.hi.1995", "cdr.lo.1995", "cdr.2000", "cdr.hi.2000", "cdr.lo.2000", "cdr.2005", "cdr.hi.2005", "cdr.lo.2005", "cdr.2010", "cdr.hi.2010", "cdr.lo.2010", "cdr.2011", "cdr.hi.2011", "cdr.lo.2011")]

# format missings
for(var in 2:ncol(tde)){
  tde[var] <- ifelse(is.na(tde[[var]]), '–', frmt(tde[[var]]))
}

cat(paste("<font size=5><b>Estimates of the case detection rate for new and relapse cases (%), 1995–", thisyear-1, "</b><sup>a</sup></font>", sep=""), file=glue("Tables/cdr", Sys.Date(), ".htm"))

tdf <- xtable(tde)
digits(tdf) <- 1
print(tdf, type="html", file=glue("Tables/cdr", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, append=T,
      html.table.attributes="border=0 rules=rows width=900", add.to.row=list(pos=list(0,30), 
                                                                             command=c(paste("<TR> <TH></TH> <TH colspan=3>1995</TH> <TH colspan=3>2000</TH> <TH colspan=3>2005</TH> <TH colspan=3>2010</TH> <TH colspan=3>", thisyear-1, "</TH> </TR>
<TR> <TH></TH> <TH>BEST<sup>b</sup></TH> <TH>LOW</TH> <TH>HIGH</TH> 
<TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH> 
<TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH> 
<TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH> 
<TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH>
</TR>", sep=""), "<TR> <TD colspan=16>– indicates values that cannot be calculated.<br><sup>a</sup> Estimates for all years are recalculated as new information becomes available and techniques are refined, so they may differ from those published previously. <br><sup>b</sup> Best, low and high indicate best estimates followed by lower and upper bounds. The lower and upper bounds are defined as the 2.5th and 97.5th centiles of outcome distributions produced in simulations.</TD></TR>")))

tablecopy('cdr')

# tbhiv -------------------------------------------------------------------

tea <- merge(n[c('country', 'year', "g_whoregion", 'g_hbhiv41', "hiv_tbscr", "hiv_ipt")], tbhiv[c('country', 'year', "hivtest", "hivtest_pct_denominator", "hivtest_pos_pct_denominator", "hivtest_pos_pct_numerator", "hiv_cpt_pct_numerator", "hiv_cpt_pct_denominator", "hiv_art_pct_numerator", "hiv_art_pct_denominator", 'hivtest_pct_numerator', 'hivtest_pos', 'hiv_cpt', 'hiv_art')])

tea <- merge(tea, e[c('country', 'year', 'e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')], all.x=T)

tea <- subset(tea, year==thisyear-1)

# Get TB/HIV high burden countries only
te1 <- tea[tea$g_hbhiv41=='high', c(1, 5:ncol(tea))]
te1 <- .shortnames(te1, ord='tbhiv')
te1 <- te1[order(te1$country),]

# Get total of high tbhiv burden countries
te2 <- te1
te2['country'] <- 'High TB/HIV burden countries'
te2 <- aggregate(te2[2:ncol(te2)], by=list(country=te2$country), FUN=sum, na.rm=T)

# Get global totals
teb <- aggregate(tea[5:ncol(tea)], by=list(country=tea$g_whoregion), FUN=sum, na.rm=T)
tec <- teb
tec[1] <- 'Global'
tec <- aggregate(tec[2:ncol(tec)], by=list(country=tec$country), FUN=sum, na.rm=T)

# create country/agg flag ("type")
teda <- te1
teda$type <- 'country'

tedb <- rbind(te2, teb, tec)
tedb$type <- 'agg'

# Combine all together
ted <- rbind(teda, tedb)

# Format table columns WHY AM I DOING IT DIFFERENT FOR COUNTRY?!! Need to come back to this and the ART map. And the HIV test map.
ted <- within(ted, {
  hivtest1000 <- rounder(ifelse(type=='country', hivtest, hivtest_pct_numerator) / 1000, decimals=TRUE)
  hivtest_prct <- rounder(ifelse(type=='country', hivtest, hivtest_pct_numerator) / hivtest_pct_denominator * 100, decimals=TRUE)
  hivtest_pos_prct <- rounder(ifelse(type=='country', hivtest_pos / hivtest,hivtest_pos_pct_numerator / hivtest_pos_pct_denominator) * 100, decimals=TRUE)
  hiv_cpt_prct <- rounder(ifelse(type=='country', hiv_cpt / hivtest_pos, hiv_cpt_pct_numerator / hiv_cpt_pct_denominator) * 100, decimals=TRUE)
  hiv_art_prct <- rounder(ifelse(type=='country', hiv_art / hivtest_pos, hiv_art_pct_numerator / hiv_art_pct_denominator) * 100, decimals=TRUE)
  hiv_tbscr2 <- rounder(hiv_tbscr / 1000, decimals=TRUE)
  hiv_ipt2 <- rounder(hiv_ipt / 1000, decimals=TRUE)
})

# replace aggregate estimates with properly aggregated numbers
ted1 <- e.t[e.t$year==thisyear-1 & !is.na(e.t$e_inc_tbhiv_100k) & e.t$g_hbhiv41=='high' , c('iso3', 'e_inc_tbhiv_100k', 'e_inc_tbhiv_100k_lo', 'e_inc_tbhiv_100k_hi', 'e_pop_num')] 
ted1[2:4] <- ted1[2:4] / 100000
ted1$e_pop_num <- as.numeric(ted1$e_pop_num)
ted2 <- add.rv(ted1$e_inc_tbhiv_100k, ted1$e_inc_tbhiv_100k_lo, ted1$e_inc_tbhiv_100k_hi, weights=ted1$e_pop_num)

ted[ted$country=="High TB/HIV burden countries", c('e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')] <- signif(ted2[c("r.num", "r.lo.num", "r.hi.num")], 2)

ted3 <- a.t[a.t$year==thisyear-1 & a.t$group_type %in% c('g_whoregion', 'global'), c('group_name', 'group_type', 'e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')]
ted3 <- ted3[order(ted3$group_type, ted3$group_name),]

ted[43:nrow(ted), c('e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')] <- ted3[c('e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')]

ted <- .shortnames(ted, ord='tbhiv')

# ted[c('e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')] <- frmt(ted[c('e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')] / 1000
ted$e_inc_tbhiv_num <- frmt(ted$e_inc_tbhiv_num / 1000, thou=TRUE)
ted$e_inc_tbhiv_num_lo <- frmt(ted$e_inc_tbhiv_num_lo / 1000, thou=TRUE)
ted$e_inc_tbhiv_num_hi <- frmt(ted$e_inc_tbhiv_num_hi / 1000, thou=TRUE)

# Fix and footnote for Russian Federation
ted[ted$country=='Russian Federation', 'hivtest1000'] <- glue(ted[ted$country=='Russian Federation', 'hivtest1000'], '(a)')

ted[ted$country=='Russian Federation', c('hivtest_prct', 'hivtest_pos_prct')] <- NA

# ted[ted$country=='Russian Federation', 'hivtest_prct'] <- glue(rounder(subset(n, iso2=='RU' & year==thisyear-1, hivtest_p) / subset(n, iso2=='RU' & year==thisyear-1, c_new) * 100, decimals=TRUE))

# ted[ted$country=='Russian Federation', 'country'] <- 'Russian Federation(a)'

warning("Russian Federation modification for the TB/HIV table is still in place. Delete this message when no longer applicable.")
                                                                              
tee <- xtable(ted[c("country", 'e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi', "hivtest1000", "hivtest_prct", "hivtest_pos_prct", "hiv_cpt_prct", "hiv_art_prct", "hiv_tbscr2", "hiv_ipt2")], align=c('l', 'l', rep('c',10)))
# digits(tee) <- 1

cat(glue("<font size=3 align=\"left\"><b>HIV testing, treatment for HIV-positive TB patients and prevention of TB among people living with <b>HIV</b>, 41 high TB/HIV burden countries and WHO regions, ", thisyear-1, ".</b> Numbers in thousands except where indicated.</font><br><br>"), file=glue("Tables/tbhiv", Sys.Date(), ".htm"))

print(tee, type="html", file=glue("Tables/tbhiv", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, append=T,
      html.table.attributes="border=0 rules=rows width=1100", add.to.row=list(pos=list(0, nrow(tee)), command=c("
<TR> <TH></TH> <TH colspan=3>ESTIMATED HIV-POSTIVE INCIDENT TB CASES</TH> 
<TH>NUMBER OF TB PATIENTS WITH  KNOWN HIV STATUS</TH> 
<TH>% OF NOTIFIED TB PATIENTS TESTED FOR HIV</TH> 
<TH>% OF TESTED TB PATIENTS HIV- POSITIVE</TH> 
<TH>% OF IDENTIFIED HIV-POSITIVE TB PATIENTS STARTED ON CPT</TH> 
<TH>% OF IDENTIFIED HIV-POSITIVE TB PATIENTS STARTED ON ART</TH> 
<TH>NUMBER OF HIV- POSITIVE PEOPLE SCREENED FOR TB</TH> 
<TH>NUMBER OF HIV- POSITIVE PEOPLE PROVIDED WITH IPT</TH>  </TR>
<TR> <TH></TH> <TH>BEST</TH> <TH>LOW</TH> <TH>HIGH</TH>",
"<TR> <TD colspan=11>Blank cells indicate data not reported.<br>
<sup>a</sup> Data for the Russian Federation exclude retreatment cases and cases from prisons.<TD colspan=8></TD> </TR>")))

tablecopy("tbhiv")


# lab_capac & lab_policy --------------------------------------------------------

# 2013 fix
# if(thisyear==2013) load('d:/users/hiattt/Dropbox/Tom_global_report/view_TME_master_strategy_2013-07-22.Rdata')

# NOTE: Non reporters not included in aggregates

tfa <- subset(s, year==thisyear-1, select=c(country, year, g_whoregion, g_hbc22, g_hbmdr27, c_lab_sm_100k, c_lab_cul_5m, c_lab_dst_5m, lab_sm_f, lab_sm_led, lab_cul_f, lab_dst_f, lab_lpa_f, lab_xpert, dst_in_guide, lc_rst_in_guide, sp_case_dfn_in_guide, lpa_in_guide, dx_alg_tbhiv_in_guide, xpert_in_guide, xpert_in_guide_TBHIV, xpert_in_guide_MDR))

tfb <- merge(tfa, p[c('country', 'year', 'e_pop_num')], all.x=T)

# get pops for data reported 
# Non-reporters are left out of aggregation
tfb <- within(tfb, {
  e_pop_num <- as.numeric(e_pop_num)
  pop_sm <- ifelse(is.na(lab_sm_f), NA, e_pop_num)
  pop_cul <- ifelse(is.na(lab_cul_f), NA, e_pop_num)
  pop_dst <- ifelse(is.na(lab_dst_f), NA, e_pop_num)
  pop_lpa <- ifelse(is.na(lab_lpa_f), NA, e_pop_num)
})  

for(poli in c("dst_in_guide", "lc_rst_in_guide", "lpa_in_guide", "sp_case_dfn_in_guide", "dx_alg_tbhiv_in_guide", "xpert_in_guide", "xpert_in_guide_TBHIV", "xpert_in_guide_MDR")){
  tfb[glue(poli, '_n')] <- ifelse(is.na(tfb[poli]), 0, 1)
}

tfhbc22 <- aggregate(tfb[9:ncol(tfb)], by=list(country=tfb$g_hbc22), FUN=sum, na.rm=T)
tfhbc22 <- tfhbc22[tfhbc22$country=='high',]
tfhbc22$country <- 'High-burden countries'

tfhbmdr27 <- aggregate(tfb[9:ncol(tfb)], by=list(country=tfb$g_hbmdr27), FUN=sum, na.rm=T)
tfhbmdr27 <- tfhbmdr27[tfhbmdr27$country=='high',]
tfhbmdr27$country <- 'High MDR-TB burden countries'

tfreg <- aggregate(tfb[9:ncol(tfb)], by=list(country=tfb$g_whoregion), FUN=sum, na.rm=T)

tfglob <- aggregate(tfb[9:ncol(tfb)], by=list(country=tfb$year), FUN=sum, na.rm=T)
tfglob$country <- 'Global'

tfc <- rbind(tfb[tfb$g_hbc22=='high' | tfb$g_hbmdr27=='high', c(1, 9:ncol(tfb))], tfhbc22, tfhbmdr27, tfreg, tfglob)
tfc$ord <- 1:nrow(tfc)
tfd <- merge(tfc, tfa[c('country', 'g_hbc22', 'g_hbmdr27')], all.x=T)
tfd <- tfd[order(tfd$ord),]
tfd <- .shortnames(tfd, ord='hbc.mdr')

for(poli in c("dst_in_guide", "lc_rst_in_guide", "lpa_in_guide", "sp_case_dfn_in_guide", "dx_alg_tbhiv_in_guide", "xpert_in_guide", "xpert_in_guide_TBHIV", "xpert_in_guide_MDR")) {
  tfd[is.na(tfd$g_hbc22), poli] <- glue(round(tfd[is.na(tfd$g_hbc22), poli] / tfd[is.na(tfd$g_hbc22), glue(poli, '_n')] * 100, 0) , "%")
  tfd[!is.na(tfd$g_hbc22), poli] <- ifelse(tfd[!is.na(tfd$g_hbc22), poli]=='1', 'Y', tfd[!is.na(tfd$g_hbc22), poli])
  tfd[!is.na(tfd$g_hbc22), poli] <- ifelse(tfd[!is.na(tfd$g_hbc22), poli]=='0', 'N', tfd[!is.na(tfd$g_hbc22), poli])
  tfd[!is.na(tfd$g_hbc22), poli] <- ifelse(is.na(tfd[!is.na(tfd$g_hbc22), poli]), '', tfd[!is.na(tfd$g_hbc22), poli])
}

tfd <- within(tfd, {
  c_sm_100k <- ifelse(is.na(lab_sm_f), '–', round.conv(lab_sm_f / pop_sm * 100000))
  c_cul_5m <- ifelse(is.na(lab_cul_f), '–', round.conv(lab_cul_f / pop_cul * 5000000))
  c_dst_5m <- ifelse(is.na(lab_dst_f), '–', round.conv(lab_dst_f / pop_dst * 5000000))
  c_lpa_5m <- ifelse(is.na(lab_lpa_f), '–', round.conv(lab_lpa_f / pop_lpa * 5000000))
  
  g_hbc22 <- ifelse(g_hbc22=='high', 'X', NA)
  g_hbmdr27 <- ifelse(g_hbmdr27=='high', 'X', NA)
  
  lab_sm_led_pct <- ifelse(is.na(lab_sm_led) | is.na(lab_sm_f), '–', rounder(lab_sm_led / lab_sm_f * 100))
  lab_sm_f <- rounder(lab_sm_f)
  lab_cul_f <- rounder(lab_cul_f)  
  lab_dst_f <- rounder(lab_dst_f)  
  lab_lpa_f <- rounder(lab_lpa_f)  
  lab_xpert <- rounder(lab_xpert)
})

# Remove aggregates for sums 
tfd[37:45, c("lab_sm_f", "lab_sm_led", "lab_cul_f", "lab_dst_f", "lab_lpa_f", "lab_xpert")] <- '–'

# lab_capac ####

tfe <- xtable(tfd[c("country", "g_hbc22", "g_hbmdr27", "lab_sm_f", "c_sm_100k", "lab_sm_led_pct", "lab_cul_f", "c_cul_5m", "lab_dst_f", "c_dst_5m", "lab_lpa_f", "c_lpa_5m", "lab_xpert")], align=c('l', 'l', rep('c',12)))

print(tfe, type="html", file=glue("Tables/lab_capac", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, #sanitize.text.function=identity, #this makes <0.1 disappear in word even if not in a browser. 
      html.table.attributes="border=0 rules=rows width=1100 cellpadding=5", add.to.row=list(pos=list(0, nrow(tfe)), command=c(glue("<h2 align=\"left\" >Laboratory capacity, ", thisyear-1, "<sup>a</sup></h2>
  <TR> <TH></TH> <TH></TH> <TH></TH> <TH colspan=3>SMEAR MICROSCOPY</TH> 
<TH colspan=2>CULTURE</TH>
<TH colspan=2>DRUG SUSCEPTIBILITY TESTING</TH>
<TH colspan=2>LINE PROBE ASSAY</TH>
<TH colspan=1>XPERT MTB/RIF</TH>
</TR>
<TR> <TD></TD> <TD>HIGH TB BURDEN</TD>
<TD>HIGH MDR-TB BURDEN</TD>
<TD>NUMBER OF LABORATORIES</TD>
<TD>LABORATORIES PER 100 000 POPULATION</TD>
<TD>PERCENTAGE OF LABORATORIES USING LED MICROSCOPES</TD>
<TD>NUMBER OF LABORATORIES</TD>
<TD>LABORATORIES PER 5 MILLION POPULATION</TD>
<TD>NUMBER OF LABORATORIES</TD>
<TD>LABORATORIES PER 5 MILLION POPULATION</TD>
<TD>NUMBER OF LABORATORIES</TD>
<TD>LABORATORIES PER 5 MILLION POPULATION</TD>
<TD>NUMBER OF SITES</TD>
</TR>"),
	"<TR> <TD colspan=11>Blank cells indicate data not reported.<br>
– indicates values that cannot be calculated.<br>
<sup>a</sup> The regional and global figures are aggregates of data reported by low- and middle-income countries and territories. Data for the variables shown in the table are not requested from high-income countries in the WHO data collection form. <TD colspan=8></TD> </TR>")))

tablecopy("lab_capac")

# lab_policy ####

tff <- xtable(tfd[c("country", "g_hbc22", "g_hbmdr27", "dst_in_guide", "lc_rst_in_guide", "lpa_in_guide", "dx_alg_tbhiv_in_guide", "xpert_in_guide_TBHIV", "xpert_in_guide_MDR")], align=c('l', 'l', rep('c',8)))

print(tff, type="html", file=glue("Tables/lab_policy", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, #sanitize.text.function=identity, 
      html.table.attributes="border=0 rules=rows width=1100 cellpadding=0", add.to.row=list(pos=list(0, nrow(tff)), command=c(glue("<h2 align=\"left\">Incorporation of WHO policy guidance for diagnosis of TB, ", thisyear-1, "<sup>a</sup></h2>
	<TR> <TH></TH> <TH>HIGH TB BURDEN</TH> <TH>HIGH MDR-TB BURDEN</TH>
<TH>CONVENTIONAL DRUG SUSCEPTIBILITY TESTING (DST)</TH>
<TH>LIQUID CULTURE AND RAPID SPECIATION TEST </TH>
<TH>LINE-PROBE ASSAY FOR DETECTING RESISTANCE TO RIFAMPICIN</TH>
<TH>ALGORITHM FOR THE DIAGNOSIS OF TB IN PEOPLE LIVING WITH HIV</TH>
<TH>XPERT MTB/RIF FOR DIAGNOSIS OF TB IN PERSONS AT RISK OF HIV-ASSOCIATED TB</TH>
<TH>XPERT MTB/RIF FOR DIAGNOSIS OF DRUG-RESISTANT TB IN PERSONS AT RISK</TH> </TR>"),
"<TR> <TD colspan=8>Blank cells indicate data not reported.<br>
<sup>a</sup> The regional and global figures are aggregates of data reported by low- and middle-income countries and territories. Data for the variables shown in the table are not requested from high-income countries in the WHO data collection form. 
<TD colspan=8></TD> </TR>")))

tablecopy("lab_policy")

# agesex -------------------------------------------------------------------

# Get country data

tiasb <- subset(n.t, year == thisyear-1, select=c("iso3", "country", "year", "g_whoregion", "g_hbc22", "newrel_m04", "newrel_m514", "newrel_m014", "newrel_m1524", "newrel_m2534", "newrel_m3544", "newrel_m4554", "newrel_m5564", "newrel_m65", "newrel_mu", "newrel_f04", "newrel_f514", "newrel_f014", "newrel_f1524", "newrel_f2534", "newrel_f3544", "newrel_f4554", "newrel_f5564", "newrel_f65", "newrel_fu", 'newrel_m15plus', 'newrel_f15plus', 'newrel_sexunk014', 'newrel_sexunk15plus'))

# Order by country
# tiasb <- tiasb[order(tiasb$country),]

# for alternate report age or sex reported (measured by total)
tiasb$snep.age.alt <- .rowsums(tiasb[c("newrel_m014", "newrel_f014", 'newrel_sexunk014', "newrel_m15plus", "newrel_f15plus", 'newrel_sexunk15plus')])  #excludes vars where age is unknown

tiasb$snep.sex.alt <- .rowsums(tiasb[c("newrel_m014", "newrel_m15plus", "newrel_mu", "newrel_f014", "newrel_f15plus", "newrel_fu")]) #excludes vars where sex is unknown

# combine all vars
tiasb$all.014 <- .rowsums(tiasb[c("newrel_m014", "newrel_f014", 'newrel_sexunk014')]) # (unknowns will be removed for this var later, but for now they are needed for age % and aggregates)

tiasb$all.1544 <- .rowsums(tiasb[c("newrel_m1524",  "newrel_m2534", "newrel_m3544", "newrel_f1524",  "newrel_f2534", "newrel_f3544")])
tiasb$all.4564 <- .rowsums(tiasb[c("newrel_m4554", "newrel_m5564", "newrel_f4554", "newrel_f5564")])
tiasb$all.65 <- .rowsums(tiasb[c("newrel_m65", "newrel_f65")])
tiasb$all.totalage <- .rowsums(tiasb[c('all.014', 'newrel_m15plus', 'newrel_f15plus', 'newrel_sexunk15plus')]) 

tiasb$all.male <- .rowsums(tiasb[c("newrel_m014", 'newrel_m15plus', "newrel_mu")])
tiasb$all.female <- .rowsums(tiasb[c("newrel_f014", 'newrel_f15plus', "newrel_fu")])

# Assemble aggregates
# tiasr <- aggregate(tiasb[6:ncol(tiasb)], by=list(group_name=tiasb$g_whoregion, year=tiasb$year), FUN='sum', na.rm=T)
# 
# tiash <- aggregate(tiasb[6:ncol(tiasb)], by=list(hbc=tiasb$g_hbc22, year=tiasb$year), FUN='sum', na.rm=T)
# tiash$group_name <- 'High-burden countries'
# tiash <- tiash[tiash$hbc=='high',-1]
# 
# tiasg <- aggregate(tiasb[6:ncol(tiasb)], by=list(year=tiasb$year), FUN='sum', na.rm=T)
# tiasg$group_name <- 'Global'
# 
# tiasj <- rbind(tiash, tiasr, tiasg)
# # tiasj$type <- "aggs"
# tiasj$iso3 <- "aggs"
# tiasj$g_whoregion <- tiasj$group_name
# 
# # tiasb$type <- 'countries'
# names(tiasb)[names(tiasb)=="country"] <- "group_name"

tiaa <- glb.rpt.table(df = tiasb, column.nums = 6:ncol(tiasb), country.col = 2)

tiaa1 <- .shortnames(subset(tb, rel_in_agesex_flg==0 & g_hbc22=="high" & year==thisyear-1, country))
tiaa$area <- ifelse(tiaa$area %in% tiaa1$country, paste0(tiaa$area, "*"), tiaa$area)

tikeep <- c("area", "newrel_m014", "newrel_f014", "all.014", "all.1544", "all.4564", "all.65", "all.totalage", "all.male", "all.female") #"sn.ep.totalage",

tiaa <- tiaa[tikeep]

# tiaa <- rbind(tiasb[tiasb$g_hbc22=='high', tikeep], tiasj[tikeep])
# tiaa$order <- 1:nrow(tiaa)

# Calculate % child. (Removed for all new where countries didn't report)
# tiaa$new_sp.child.pct <- ifelse(is.na(tiaa$new_sp.totalage), "–", frmt(tiaa$new_sp.014 / tiaa$new_sp.totalage * 100))
tiaa$all.child.pct <- ifelse(is.na(tiaa$all.totalage) , "–", frmt(tiaa$all.014 / tiaa$all.totalage * 100))

# Calculate male female ratio. (Removed for all new where countries didn't report)
# tiaa$new_sp.mf.ratio <- ifelse(is.na(tiaa$new_sp.male) | is.na(tiaa$new_sp.female) | tiaa$new_sp.male==0 | tiaa$new_sp.female==0, "–", frmt(tiaa$new_sp.male / tiaa$new_sp.female))
tiaa$all.mf.ratio <- ifelse(is.na(tiaa$all.male) | is.na(tiaa$all.female) | tiaa$all.male==0 | tiaa$all.female==0, "–", frmt(tiaa$all.male / tiaa$all.female))

for(var in tikeep[2:8]){
  tiaa[var] <- rounder(tiaa[[var]])
}

# Remove values for countries not reporting full sn ep.
# for(var in tikeep[15:18]){
#   tiaa[var] <- ifelse(tiaa$sn.ep.report==4 | tiaa$iso3=="aggs", tiaa[[var]], "")
# }

# tiaa <- tiaa[order(tiaa$order),]
tid <- xtable(subset(tiaa, select=c("area", "all.014", "all.1544", "all.4564", "all.65", "all.child.pct", "all.mf.ratio")))

# Add to file

cat(glue("<h2>Notifications of new and relapse TB cases by age and sex, ", thisyear-1, '</h2>'), file=glue("Tables/agesex", Sys.Date(), ".htm"))

print(tid, type="html", file=glue("Tables/agesex", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, append=TRUE, add.to.row=list(pos=list(0,nrow(tid)), command=c(
        " 
        <TR> <TD></TD> 
        <TD>0–14 YEARS</TD>
        <TD>15–44 YEARS</TD>
        <TD>45–64 YEARS</TD>
        <TD>&#8805;65 YEARS</TD>
        <TD>% AGED &#60 15 YEARS</TD>
        <TD>MALE/FEMALE RATIO</TD>
        </TR>", 
	"<TR> <TD colspan=7>Blank cells indicate data that could not be reported for the age categories shown.<br>– indicates values that cannot be calculated.<br>* New cases only.</TD> </TR>")))

tablecopy("agesex")

#==========================
# Pieces of the 'top 10 countries' table

tma <- subset(e.t, year==thisyear-1, c(country, e_mort_100k, e_pop_num, e_mort_exc_tbhiv_num))

tma$e_mort_num <- tma$e_mort_100k / 1e5 * tma$e_pop_num

.maxplus <- function(df, vect, num.rows){
  df1 <- df[order(df[vect], decreasing=TRUE),]
  head(df1,num.rows)
}

tmb <- .maxplus(tma, 'e_mort_num', 10)
tmb$e_mort_num <- signif(tmb$e_mort_num, 2)
write.cb(tmb[c('country', 'e_mort_num')])

tmc <- .maxplus(tma, 'e_mort_100k', 10)
write.cb(tmc[c('country', 'e_mort_100k')])


subset(tma, e_mort_num %in% .maxplus(tma$e_mort_num,10), c(country, e_mort_num))


# END ===================================================