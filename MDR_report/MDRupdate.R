###################################################
# Tom Hiatt
# Updated: 01 September 2014
# Regional MDR-TB annual analysis
# This is the data munging script to be called from the associated Rmd file. Changes to data crunching are done here and changes to the text are done on the google doc file, then copied to the Rmd file, and then run.
###################################################
# title: "Drug resistant tuberculosis in the WHO Western Pacific Region"

# Script found at: https://docs.google.com/document/d/1QGiWYD68Y6w1lo8d2or4889kSxHrzcNkMK3lhPQOvCQ/edit

## Change this information to customize for your own data.

# Year of TB notification data
yr <- 2013
# 

require("reshape")
require("ggplot2")
require("grid")
require("scales")
require("xtable")
require("stringr")
require("timeSeries")
require("ggthemes")
require("Gmisc")

##################
# Knitr settings #
##################
require(knitr) 
knitr::opts_chunk$set(warning=FALSE,
                      message=FALSE,
                      echo=FALSE,
                      dpi=96,
                      fig.width=4, fig.height=4, # Default figure widths
                      dev="png", dev.args=list(type="cairo"), # The png device
                      # Change to dev="postscript" if you want the EPS-files
                      # for submitting. Also remove the dev.args() as the postscript
                      # doesn't accept the type="cairo" argument.
                      error=FALSE)

# Reproducible research process inspired by: http://gforge.se/2014/01/fast-track-publishing-using-knitr-part-ii/ and http://gforge.se/2014/07/fast-track-publishing-using-rmarkdown/

# Citation directions here: http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html#citations and here: http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html

# Create regional report folder if needed.
# dir.create("./Regional_report") # run this if error below
# dir.create("./figure data") # run this if error below
if(!any(grep("MDR_report", getwd()))) setwd("./MDR_report")

# Theme for plots
theme_report <- function(base_size=12, base_family="") {
colors <- ggthemes_data$few
gray <- colors$medium['gray']
black <- colors$dark['black'] # I don't know why these last 3 parts are needed, but they are. List is here: http://docs.ggplot2.org/0.9.2.1/theme.html
theme_bw(base_size=base_size, base_family=base_family) +
theme(
line = element_line(colour = gray),
rect = element_rect(fill = "white", colour = NA),
text = element_text(colour = black),
axis.ticks.x = element_line(colour = gray),
axis.ticks.y = element_blank(),
legend.key = element_rect(colour = NA),
## Examples do not use grid lines
panel.border = element_rect(colour = gray),
panel.grid.major.x = element_blank(),
panel.grid.minor = element_blank(),
strip.background = element_rect(fill="white", colour=NA),
strip.text = element_text(hjust=0)
)
}

# Functions for data formatting ##########################

# Simple rounder that just adds in the thousands separator 
rounder <- function(x, decimals=FALSE) {
if(decimals==TRUE){
ifelse(is.na(x), NA, ifelse(x==0, 0, ifelse(x < 0.01, "<0.01", ifelse(round(x,2) < 0.1, formatC(round(x,2), format='f', digits=2), ifelse(round(x,1) < 10, formatC(round(x,1), format='f', digits=1), formatC(round(x,0), big.mark=" ", format='d') )))))
}
else ifelse(is.na(x), NA, ifelse(x==0, 0, ifelse(x < 1, "< 1", formatC(round(x,0), big.mark=" ", format='d'))))
}

.rowsums <- function(x) { 
# This function sums rows ignoring NAs unless all are NA
# use it like this
# t3c$snu <- .rowsums(t3c[c('new_sn', 'new_su')])
tosum <- as.matrix(x)
summed <- rowMeans((tosum), na.rm=T) * rowSums(!is.na((tosum)))
return(summed)
}

# Change names to WPSAR convention
.WPSARnames <- function(d, col='country', ord='no order'){
d[col] <- as.character(d[[col]])
d[col] <- ifelse(d[[col]]=='China, Hong Kong SAR', 'Hong Kong Special Administrative Region (China)', 
 ifelse(d[[col]]=='China, Macao SAR', 'Macao Special Administrative Region (China)', 
ifelse(d[[col]]=='Micronesia (Federated States of)', 'Micronesia, Federated States of', 
 ifelse(d[[col]]=='WPR', 'Western Pacific Region
', d[[col]]))))

if(!ord %in% c('wpr')) warning('Not ordering.')
if(ord=='wpr')d <- d[match(c("Afghanistan", "Bangladesh", "Brazil", "Cambodia", "China", "DR Congo", "Ethiopia", "India", "Indonesia", "Kenya", "Mozambique","Myanmar", "Nigeria", "Pakistan", "Philippines", "Russian Federation", "South Africa", "Thailand", "Uganda", "UR Tanzania", "Viet Nam", "Zimbabwe", "High-burden countries", "AFR", "AMR", "EMR", "EUR", "SEAR", "WPR", "Global"), d[[col]]),]

return(d)
}

# Functions for maps ####################################

source("./MapFunctions.r")
load("gparts.Rdata")

# Tables and figure counts ##############################
# Functions to assist with tables and figure in markdown document (from here: http://rmflight.github.io/posts/2012/10/papersinRmd.html)
########################################################

incCount <- function(inObj, useName) {
nObj <- length(inObj)
useNum <- max(inObj) + 1
inObj <- c(inObj, useNum)
names(inObj)[nObj + 1] <- useName
inObj
}
figCount <- c(`_` = 0)
tableCount <- c(`_` = 0)

tableCount

pasteLabel <- function(preText, inObj, objName, insLink = TRUE, sepper = " ") {
objNum <- inObj[objName]

useText <- paste(preText, objNum, sep = sepper)
if (insLink) {
useText <- paste("[", useText, "](#", objName, ")", sep = " ")
}
useText
}

tableCat <- function(inFrame) {
outText <- paste(names(inFrame), collapse = " | ")
outText <- c(outText, paste(rep("---", ncol(inFrame)), collapse = " | "))
invisible(apply(inFrame, 1, function(inRow) {
outText <<- c(outText, paste(inRow, collapse = " | "))
}))
return(outText)
}

# Create directory and Get data #################

external.data <- FALSE

if(!external.data){
source("D:/Users/hiattt/Dropbox/Code/R/.Rprofile")
runprofile()
} 

if(external.data){
if(!"data" %in% dir() | length(dir("./data"))==0){
dir.create(paste("./data"))
stop("Download the notification and treatment outcomes data from 'http://who.int/tb/country/data/download/en/' to './data'.")
}

data1 <- dir("./data", full.names=TRUE)
tb <- NA
for(i in data1){
data3 <- read.csv(i)
tb <- merge(tb, data3, all=TRUE)
}

# Note: to use your own data, make a flat data file with each row corresponding to the lower reporting unit and a aggregating variable. EG by country with aggregating variable of region.

# Add needed variables
tb$g_hbc22 <- ifelse(tb$iso3 %in% c("AFG", "BGD", "BRA", "CHN", "COD", "ETH", "IDN", "IND", "KEN", "KHM", "MMR", "MOZ", "NGA", "PAK", "PHL", "RUS", "THA", "TZA", "UGA", "VNM", "ZAF", "ZWE"), "high", "low")

tb$c_new <- .rowsums(tb[c("new_sp", "new_sn", "new_su", "new_ep", "new_oth")])
tb$c_newinc <- .rowsums(tb[c("c_new", "ret_rel", "newret_oth")])
tb$c_ret <- .rowsums(tb[c("ret_rel", "ret_taf", "ret_tad", "ret_oth")])
tb$c_notified <- .rowsums(tb[c("c_new", "c_ret", "newret_oth")])

tb$c_tot_newrel_100k <- tb$tot_newrel / tb$e_pop_num * 1e5

# Get population data (available from UN population division. http://esa.un.org/unpd/wpp/unpp/panel_population.htm) and formatted TB/HIV data and estimates for the WHO Western Pacific Region.

} # End of external data part


# Common data munging 

whbc <- subset(tb, g_whoregion=="WPR" & year==yr & mdr>=1, c(iso3))
whbc <- whbc$iso3

#c("KHM", "CHN", "JPN", "MYS", "MNG", "PNG", "PHL", "KOR", "VNM")

tb$country2 <- ifelse(tb$iso3 %in% whbc, as.character(tb$iso3), "Other") 
tb$country2 <- factor(tb$country2, c("KHM", "CHN", "JPN", "MYS", "MNG", "PNG", "PHL", "KOR", "VNM", "Other"), c("Cambodia", "China", "Japan", "Malaysia", "Mongolia", "Papua New Guinea", "Philippines", "Republic of Korea", "Viet Nam", "Other countries"))

# End of setup


# m-coverage -----------------------------------------------
figCount <- incCount(figCount, "m-coverage")

coa <- read.table(textConnection("
iso3 coverage year
AUS Nationwide	2013
BRN	Nationwide	2013
KHM	Nationwide	2007
CHN	Nationwide	2014
HKG	Nationwide	2012
MAC	Nationwide	2013
COK	Nationwide	2013
FJI	Nationwide	2006
PYF	Nationwide	2013
GUM	Nationwide	2012
JPN	Nationwide	2002
MYS	Subnational	1997
MHL	Nationwide	2013
MNG	Nationwide	2007
NCL	Nationwide	2012
NZL	Nationwide	2012
MNP	Nationwide	2012
PLW	Nationwide	2013
PNG	Subnational	2014
PHL	Nationwide	2012
KOR	Nationwide	2004
WSM	Nationwide	2013
SGP	Nationwide	2013
SLB	Nationwide	2004
VUT	Nationwide	2006
VNM	Nationwide	2012
"), header = TRUE, as.is = TRUE)
# closeAllConnections() # This breaks stuff if left in there.

coa$cat <- cut(coa$year, c(0, 2005, 2010, 2014, Inf), c('1997–2004', '2005–2009', '2010–2013', 'Ongoing survey in 2014'), right=FALSE)
# 
mc1 <- WHOmap.print(coa, legend.title= "Year of most \nrecent data", copyright=FALSE, show=FALSE, zoom="WPR")

write.csv(coa, file=paste0(pasteLabel("./figure_data/table", tableCount, "m-coverage", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")

# t-drestnotif ----------------------------------------------------
tableCount <- incCount(tableCount, "t-drestnotif")

# Notification table

oa <- subset(tb, year==yr & g_whoregion=="WPR", 
select=c(iso3, g_whoregion, country, 
e_new_mdr_prop, e_new_mdr_prop_lo, e_new_mdr_prop_hi,
e_ret_mdr_prop, e_ret_mdr_prop_lo, e_ret_mdr_prop_hi,
e_mdr_num, e_mdr_num_lo, e_mdr_num_hi,
c_newunk, c_ret, 
dst_rlt_new, dst_rlt_ret, dst_rlt_unk, 
mdr_new,mdr_ret,mdr, rapid_dx_dr_r, 
conf_mdr_tx, unconf_mdr_tx))

oa <- oa[order(oa$country),]
names(oa)[names(oa)=='country'] <- 'area'
estvars <- c("e_new_mdr_prop", "e_new_mdr_prop_lo", "e_new_mdr_prop_hi", "e_ret_mdr_prop", "e_ret_mdr_prop_lo", "e_ret_mdr_prop_hi", "e_mdr_num", "e_mdr_num_lo", "e_mdr_num_hi")

# make aggregate rows
ob <- oa[oa$iso3 %in% whbc, ]
obh <- aggregate(ob[4:ncol(ob)], by=list(area=ob$g_whoregion), FUN=sum, na.rm=TRUE)
obh$area <- "Total"
obh[estvars] <- NA # Not the proper way to sum bounds. Maybe to replace this later.

obw <- aggregate(oa[4:ncol(oa)], by=list(area=oa$g_whoregion), FUN=sum, na.rm=TRUE)
obw[estvars] <- subset(emdra, year==yr & group_name=="WPR", estvars) # Proper sums for aggregates.

# combine together
oc <- rbind(ob[3:ncol(ob)], obh, obw) 

# calculate and format vars

for(stima in c("new", "ret")){ # change proportional estimates to percent
for(bound in c("", "_lo", "_hi")){
oc[paste0("e_", stima, "_mdr_pct", bound)] <- oc[paste0("e_", stima, "_mdr_prop", bound)] * 100
}
}

oc$e_new_mdr_pct_range <- paste0("(", rounder(oc$e_new_mdr_pct_lo), "&#8211;", rounder(oc$e_new_mdr_pct_hi), ")")
oc$e_ret_mdr_pct_range <- paste0("(", rounder(oc$e_ret_mdr_pct_lo), "&#8211;", rounder(oc$e_ret_mdr_pct_hi), ")")
oc$e_mdr_num_range <- paste0("(", rounder(oc$e_mdr_num_lo), "&#8211;", rounder(oc$e_mdr_num_hi), ")")

oc$dst_rlt_new_pct <- rounder(oc$dst_rlt_new / oc$c_newunk * 100)
oc$dst_rlt_ret_pct <- rounder(oc$dst_rlt_ret / oc$c_ret * 100)

oc$mdr_new_pct <- rounder(oc$mdr_new / oc$c_newunk * 100)
oc$mdr_ret_pct <- rounder(oc$mdr_ret / oc$c_ret * 100)

oc$mdr_tx <- .rowsums(oc[c('conf_mdr_tx', 'unconf_mdr_tx')])

# Format
for(var in c("e_new_mdr_pct", "e_ret_mdr_pct", "e_mdr_num", "mdr", "rapid_dx_dr_r", "conf_mdr_tx", "mdr_tx")){
oc[var] <- ifelse(is.na(oc[[var]]), "&#8211;", rounder(oc[[var]]))
}
for(var in c("mdr_new_pct", "mdr_ret_pct")){
oc[var] <- ifelse(is.na(oc[[var]]), "&#8211;", oc[[var]])
}

# Rename countries
od <- .WPSARnames(oc, col="area")

rownames(od) <- od$area

oe <- subset(od, select=c("mdr_new_pct", "e_new_mdr_pct", "e_new_mdr_pct_range", "mdr_ret_pct", "e_ret_mdr_pct", "e_ret_mdr_pct_range", "mdr", "e_mdr_num", "e_mdr_num_range", "rapid_dx_dr_r", "conf_mdr_tx", "mdr_tx"))


write.csv(oe, file=paste0(pasteLabel("./figure_data/table", tableCount, "t-drestnotif", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")

# t-drnotif -------------------------------------------------------------
tableCount <- incCount(tableCount, "t-drnotif")

# Notification table

nb <- subset(tb, year==yr & g_whoregion=="WPR", 
select=c(iso3, g_whoregion, country,
new_labconf, new_clindx, 
ret_rel_labconf, ret_rel_clindx, ret_nrel, 
cullpa_new, cullpa_ret, cullpa_unk, 
dst_rlt_new, dst_rlt_ret, dst_rlt_unk, 
dr_h_nr_new,dr_h_nr_ret,dr_h_nr_unk, 
dr_r_nh_new,dr_r_nh_ret,dr_r_nh_unk, 
mdr_new,mdr_ret,mdr_unk,
xpert_new, xpert_ret, xpert_unk, 
xpert_dr_r_new, xpert_dr_r_ret, xpert_dr_r_unk))

nb <- nb[order(nb$country),]
names(nb)[names(nb)=='country'] <- 'area'

# make aggregate rows
nbc <- nb[nb$iso3 %in% whbc, ]
nbch <- aggregate(nbc[4:ncol(nbc)], by=list(area=nbc$g_whoregion), FUN=sum, na.rm=TRUE)
nbch$area <- "Total"

nbcw <- aggregate(nb[4:ncol(nb)], by=list(area=nb$g_whoregion), FUN=sum, na.rm=TRUE)

# combine together
nbd <- rbind(nbc[3:ncol(nbc)], nbch, nbcw) # , tbbh, tbbga

# calculate and format vars
nbd$dstx_rlt_new <- .rowsums(nbd[c('dst_rlt_new', 'xpert_new')])
nbd$dstx_rlt_ret <- .rowsums(nbd[c('dst_rlt_ret', 'xpert_ret')])
nbd$dstx_rlt <- .rowsums(nbd[c('dstx_rlt_new', 'dstx_rlt_ret', 'dst_rlt_unk', 'xpert_unk')])

nbd$pulm_new <- .rowsums(nbd[c('new_labconf', 'new_clindx')])
nbd$pulm_ret <- .rowsums(nbd[c('ret_rel_labconf', 'ret_rel_clindx', 'ret_nrel')])
nbd$pulm_all <- .rowsums(nbd[c('pulm_new', 'pulm_ret')])

nbd$dstx_rlt_new_pct <- nbd$dstx_rlt_new / nbd$pulm_new * 100
nbd$dstx_rlt_ret_pct <- nbd$dstx_rlt_ret / nbd$pulm_ret * 100
# nbd$dst_rlt <- .rowsums(nbd[c('dst_rlt_new', 'dst_rlt_ret', 'dst_rlt_unk')])
nbd$dstx_rlt_pct <- nbd$dstx_rlt /nbd$pulm_all * 100

nbd$dst_rlt <- .rowsums(nbd[c('dst_rlt_new', 'dst_rlt_ret', 'dst_rlt_unk')])
nbd$dr_h_nr_new_pct <- nbd$dr_h_nr_new / nbd$dst_rlt_new * 100
nbd$dr_h_nr_ret_pct <- nbd$dr_h_nr_ret / nbd$dst_rlt_ret * 100
nbd$dr_h_nr_pct <- .rowsums(nbd[c('dr_h_nr_new', 'dr_h_nr_ret', 'dr_h_nr_unk')]) /nbd$dst_rlt * 100 # Note: denominator doesn't include Xpert

nbd$dr_r_nh_new_pct <- nbd$dr_r_nh_new / nbd$dst_rlt_new * 100
nbd$dr_r_nh_ret_pct <- nbd$dr_r_nh_ret / nbd$dst_rlt_ret * 100
nbd$dr_r_nh_pct <- .rowsums(nbd[c('dr_r_nh_new', 'dr_r_nh_ret', 'dr_r_nh_unk')]) /nbd$dst_rlt * 100 # Note: denominator doesn't include Xpert

nbd$mdrr_new <- .rowsums(nbd[c('mdr_new', 'dr_r_nh_new', 'xpert_dr_r_new')])
nbd$mdrr_ret <- .rowsums(nbd[c('mdr_ret', 'dr_r_nh_ret', 'xpert_dr_r_ret')])
nbd$mdrr <- .rowsums(nbd[c('mdrr_new', 'mdrr_ret', 'dr_r_nh_unk', 'xpert_dr_r_unk')])

nbd$mdrr_new_pct <- nbd$mdrr_new / nbd$dstx_rlt_new * 100
nbd$mdrr_ret_pct <- nbd$mdrr_ret / nbd$dstx_rlt_ret * 100
nbd$mdrr_pct <- nbd$mdrr / nbd$dstx_rlt * 100


nbd$xpert_dr_r <- .rowsums(nbd[c('xpert_dr_r_new', 'xpert_dr_r_ret', 'xpert_dr_r_unk')])

nbd$xpert_dr_r_new_pct <- nbd$xpert_dr_r_new / nbd$mdrr_new * 100
nbd$xpert_dr_r_ret_pct <- nbd$xpert_dr_r_ret / nbd$mdrr_ret * 100
nbd$xpert_dr_r_pct <- nbd$xpert_dr_r /nbd$mdrr * 100

# Format
for(var in 2:ncol(nbd)){
nbd[var] <- ifelse(is.na(nbd[[var]]), "&#8211;", rounder(nbd[[var]]))
}


# Rename countries
nbe <- .WPSARnames(nbd, col="area")

# Consider adding a column for put on treatment and/or including those from the other MDR table.

rownames(nbe) <- nbe$area

nbf <- subset(nbe, select=c("dstx_rlt_new", "dstx_rlt_new_pct", "dstx_rlt_ret", "dstx_rlt_ret_pct", "dstx_rlt", "dstx_rlt_pct", "mdrr_new", "mdrr_new_pct", "mdrr_ret", "mdrr_ret_pct", "mdrr", "mdrr_pct", "xpert_dr_r_new", "xpert_dr_r_new_pct", "xpert_dr_r_ret", "xpert_dr_r_ret_pct", "xpert_dr_r", "xpert_dr_r_pct"))

names(nbf) <- rep(c("n", "%"), 9)

# Make that table
t_drnotif <- htmlTable(nbf, caption = "", rowlabel = "", cgroup = rbind(c("Notified cases with DST results", "MDR-TB and RR-TB<sup>*</sup> among cases with DST results", "Cases confirmed by Xpert among MDR-TB and RR-TB<sup>*</sup>", rep(NA,6)), rep(c("New", "Ret.", "Total"),3)), n.cgroup = rbind(c(rep(3,3), rep(NA,6)), rep(2,9)), ctable = TRUE, tfoot = "<sup>*</sup> Rif-resistant only cases are included whether confirmed by DST or Xpert.")

write.csv(nbf, file=paste0(pasteLabel("./figure_data/table", tableCount, "t-drnotif", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")

# DST coverage trend --------------------------------------------------


# f_alignment ------------------------------------------------------
figCount <- incCount(figCount, "f_alignment")

tea <- subset(tb, year %in% 2007:yr & g_whoregion=="WPR", select=c(iso3, country, year, e_mdr_num, e_mdr_num_lo, e_mdr_num_hi, mdr, rapid_dx_dr_r, conf_mdr_tx, unconf_mdr_tx))

teb1 <- aggregate(tea[4:ncol(tea)], by=list(year=tea$year), FUN=sum, na.rm=TRUE)
teb1$country <- teb1$iso3 <- "WPR"

# Separate hbcs
teb3 <- tea[tea$iso3 %in% whbc,] 
# teb2 <- aggregate(teb3[4:ncol(tea)], by=list(year=teb3$year, area=teb3$country), FUN=sum, na.rm=TRUE)


#combine
teb <- rbind(teb1, teb3)

# Calculate new vars
teb$`Cases confirmed` <- .rowsums(teb[c('mdr', 'rapid_dx_dr_r')])
teb$`Patients enrolled on treatment` <- .rowsums(teb[c('conf_mdr_tx', 'unconf_mdr_tx')])


tec <- melt(teb[c("country", "year", "Cases confirmed", "Patients enrolled on treatment")], id=1:2)

ted <- ggplot(tec, aes(year, value, color=variable)) + geom_point(alpha=.5) + geom_line(size=1, alpha=.5) + facet_wrap(~country, scales="free_y", ncol=4) + theme_report() + scale_color_brewer("", type="qual", palette=6) + scale_x_continuous("") + scale_y_continuous("MDR-TB cases")+ guides(fill = guide_legend(reverse = TRUE)) + theme(legend.position = "bottom")

write.csv(tec, file=paste0(pasteLabel("./figure_data/figure", figCount, "f-align-bar", insLink=FALSE), ".csv"), row.names=FALSE)


# t.est.enroll ------------------------------------------------------------
tableCount <- incCount(tableCount, "t.est.enroll")

# Notification table

oa <- subset(tb, year==yr & g_whoregion=="WPR", 
 select=c(iso3, g_whoregion, country, 
e_mdr_num, e_mdr_num_lo, e_mdr_num_hi,
mdr, rapid_dx_dr_r, 
conf_mdr_tx, unconf_mdr_tx))

oa <- oa[order(oa$country),]
names(oa)[names(oa)=='country'] <- 'area'
estvars <- c("e_mdr_num", "e_mdr_num_lo", "e_mdr_num_hi")

# make aggregate rows
ob <- oa[oa$iso3 %in% whbc, ]
obh <- aggregate(ob[4:ncol(ob)], by=list(area=ob$g_whoregion), FUN=sum, na.rm=TRUE)
obh$area <- "Total"
obh[estvars] <- NA # Not the proper way to sum bounds. Maybe to replace this later.

obw <- aggregate(oa[4:ncol(oa)], by=list(area=oa$g_whoregion), FUN=sum, na.rm=TRUE)
obw[estvars] <- subset(emdra, year==yr & group_name=="WPR", estvars) # Proper sums for aggregates.

# combine together
oc <- rbind(ob[3:ncol(ob)], obh, obw) 

# calculate and format vars

oc$e_mdr_num_range <- paste0("(", rounder(oc$e_mdr_num_lo), "&#8211;", rounder(oc$e_mdr_num_hi), ")")

oc$pmdr <- .rowsums(oc[c('mdr', 'rapid_dx_dr_r')])
oc$mdr_tx <- .rowsums(oc[c('conf_mdr_tx', 'unconf_mdr_tx')])

oc$pmdr_pct <- rounder(oc$pmdr / oc$e_mdr_num * 100)
oc$pmdr_pct_range <- paste0("(", rounder(oc$pmdr / oc$e_mdr_num_hi * 100), "&#8211;", rounder(oc$pmdr / oc$e_mdr_num_lo * 100), ")")

oc$mdr_tx_pct <- rounder(oc$mdr_tx / oc$pmdr * 100)

# Format
oc$e_mdr_num <- rounder(oc$e_mdr_num)

for(var in 2:ncol(oc)){
oc[var] <- ifelse(is.na(oc[[var]]), "&#8211;", oc[[var]])
}

# Rename countries
od <- .WPSARnames(oc, col="area")

rownames(od) <- od$area

oe <- subset(od, select=c("e_mdr_num", "e_mdr_num_range", "pmdr", "pmdr_pct", "pmdr_pct_range", "mdr_tx", "mdr_tx_pct"))

# Make that table
t.est.enroll <- htmlTable(oe, caption = "", rowlabel = "", cgroup = rbind(c("MDR-TB cases<sup>*</sup>", "Enrolled on treatment", rep(NA,3)), c("Estimated", "Detected", "%", "Number", "%")), n.cgroup = rbind(c(3,2, rep(NA,3)), c(2,1,2,1,1)), align=c(rep(c('r','l','r'),2), 'r'), ctable = TRUE, tfoot = "<sup>*</sup> All columns except estimates include Rif-resistant cases confirmed by Xpert only.", headings = NA )

write.csv(oe, file=paste0(pasteLabel("./figure_data/table", tableCount, "t.est.enroll", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")
# f.tx.out --------------------------------------------
figCount <- incCount(figCount, "f.tx.out")

tra <- subset(tb, year %in% 2006:(yr-2) & g_whoregion=="WPR", select=c(iso3, country, year, mdr_coh, mdr_cur, mdr_cmplt, mdr_died, mdr_fail, mdr_def, mdr_succ))

# Fill in 0s for NAs
tra[is.na(tra)] <- 0

trb1 <- aggregate(tra[4:ncol(tra)], by=list(year=tra$year), FUN=sum, na.rm=TRUE)
trb1$area <- trb1$iso3 <- "WPR"

# Separate hbcs
trb2 <- subset(tra, iso3 %in% whbc & mdr_coh > 0) 
trb2 <- rename(trb2, c(country="area"))
# trb2 <- aggregate(tra[4:ncol(tra)], by=list(year=tra$year, area=tra$country), FUN=sum, na.rm=TRUE)

#combine
trb <- rbind(trb1, trb2)

trb$mdr_succ <- ifelse(trb$year>=2011, trb$mdr_succ, trb$mdr_cur + trb$mdr_cmplt)
trb$Success <- trb$mdr_succ / trb$mdr_coh * 100
trb$Died <- trb$mdr_died / trb$mdr_coh * 100
trb$Failed <- trb$mdr_fail / trb$mdr_coh * 100
trb$Defaulted <- trb$mdr_def / trb$mdr_coh * 100
trb$`Not evaluated` <- (trb$mdr_coh - (trb$mdr_succ + trb$mdr_died + trb$mdr_fail + trb$mdr_def)) / trb$mdr_coh * 100

trc <- melt(trb[c("area", "year", "Success", "Died", "Failed", "Defaulted", "Not evaluated")], id=1:2)

f.tx.out <- ggplot(trc, aes(year, value, fill=variable)) + geom_bar(stat="identity", position="stack") + facet_wrap(~area) + theme_report() + scale_fill_brewer('Outcome', type="qual", palette=6) + scale_x_continuous("") + scale_y_continuous("Percent of cohort") + coord_cartesian(ylim=c(0,100)) + guides(fill = guide_legend(reverse = TRUE))

write.csv(trc, file=paste0(pasteLabel("./figure_data/figure", figCount, "f-mtxout-bar", insLink=FALSE), ".csv"), row.names=FALSE)


# t.xdr -----------------------------------------------------------
tableCount <- incCount(tableCount, "t.xdr")

# Notification table

xa <- subset(tb, year==yr & g_whoregion=="WPR", 
 select=c(iso3, g_whoregion, country, 
mdr, mdr_dst_rlt, mdr_ds_fq2li,
mdr_dr_fq, mdr_dr_2li, xdr))

xa <- xa[order(xa$country),]
names(xa)[names(xa)=='country'] <- 'area'

# make aggregate rows
xb <- xa[xa$iso3 %in% whbc, ]
xc <- aggregate(xb[4:ncol(xb)], by=list(area=xb$g_whoregion), FUN=sum, na.rm=TRUE)
xc$area <- "Total"

xd <- aggregate(xa[4:ncol(xa)], by=list(area=xa$g_whoregion), FUN=sum, na.rm=TRUE)

# combine together
xe <- rbind(xb[3:ncol(xb)], xc, xd) # , tbbh, tbbga

# calculate and format vars
xe$xdst_pct <- xe$mdr_dst_rlt / xe$mdr * 100
xe$fq_pct <- xe$mdr_dr_fq / xe$mdr * 100
xe$xli_pct <- xe$mdr_dr_2li / xe$mdr * 100
xe$xdr_pct <- xe$xdr / xe$mdr * 100

# Format
for(var in 2:ncol(xe)){
xe[var] <- ifelse(is.na(xe[[var]]), "&#8211;", rounder(xe[[var]]))
}

# Rename countries
xf <- .WPSARnames(xe, col="area")

rownames(xf) <- xf$area

xg <- subset(xf, select=c("mdr", "xdst_pct", "mdr_dr_fq", "fq_pct", "mdr_dr_2li", "xli_pct", "xdr", "xdr_pct"))

names(xg) <- c("MDR-TB", "% 2<sup>nd</sup>-line DST", rep(c("Number", "%"),3))

# Make that table
t.xdr <- htmlTable(xg, caption = "", rowlabel = "", cgroup = c("", "Resistance to FQ", "Resistance to 2<sup>nd</sup>-line injectible", "XDR-TB"), n.cgroup = c(2,2,2,2), ctable = TRUE, tfoot = "")

write.csv(xf, file=paste0(pasteLabel("./figure_data/table", tableCount, "t-drnotif", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")


##### GRAPH ON SLD EXPENDITURE

# age.sex.hiv.exploration -----------------------------------------------------
tableCount <- incCount(tableCount, "age.sex.hiv.exploration")

# Explore the association between MDR adn other things

oa <- subset(tb, year==yr & g_whoregion=="WPR", 
select=c(iso3, g_whoregion, country, 
mdr_hivpos, mdr_hivneg, mdr_hivunk, 
nmdr_hivpos, nmdr_hivneg, nmdr_hivunk, 
mdr_f, mdr_m, mdr_sexunk, 
nmdr_f, nmdr_m, nmdr_sexunk, 
mdr_014, mdr_15plus, mdr_ageunk, 
nmdr_014, nmdr_15plus, nmdr_ageunk))

oa <- oa[order(oa$country),]
names(oa)[names(oa)=='country'] <- 'area'


# make aggregate rows
ob <- oa[oa$iso3 %in% whbc, ]
obh <- aggregate(ob[4:ncol(ob)], by=list(area=ob$g_whoregion), FUN=sum, na.rm=TRUE)
obh$area <- "Total"


obw <- aggregate(oa[4:ncol(oa)], by=list(area=oa$g_whoregion), FUN=sum, na.rm=TRUE)


# combine together
oc <- rbind(ob[3:ncol(ob)], obh, obw) 

# calculate and format vars

vars <- data.frame(left=c("hivpos", "f", "014"), middle=c("hivneg", "m", "15plus"), right=c("hiv", "sex", "age"))

for(type in 1:3){
oc[paste0(vars[type,3], "_rr")] <- (oc[paste0("mdr_", vars[type,1])] /
.rowsums(oc[c(paste0("mdr_", vars[type,1]), paste0("mdr_", vars[type,2]), paste0("mdr_", vars[type,3], "unk"))])) / 
(oc[paste0("nmdr_", vars[type,1])] /
.rowsums(oc[c(paste0("nmdr_", vars[type,1]), paste0("nmdr_", vars[type,2]), paste0("nmdr_", vars[type,3], "unk"))]))
}


# Format
for(var in c("hiv_rr", "sex_rr", "age_rr")){
oc[var] <- ifelse(is.na(oc[[var]]), "&#8211;", signif(oc[[var]], 3))
}


# Rename countries
od <- .WPSARnames(oc, col="area")

rownames(od) <- od$area

oe <- subset(od, select=c("hiv_rr", "sex_rr", "age_rr"))

# Make that table
age.sex.hiv.exploration <- htmlTable(oe, caption = "Risk ratios for MDR-TB")


write.csv(oe, file=paste0(pasteLabel("./figure_data/table", tableCount, "age.sex.hiv.exploration", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")

# **************************************************************
# Dumping ground ----------------------------------------------
# **************************************************************

# drestnotif2 -------------------------------------------
tableCount <- incCount(tableCount, "t-drestnotif2")

# Notification table

oa <- subset(tb, year==yr & g_whoregion=="WPR", 
select=c(iso3, g_whoregion, country, 
e_new_mdr_prop, e_new_mdr_prop_lo, e_new_mdr_prop_hi,
e_ret_mdr_prop, e_ret_mdr_prop_lo, e_ret_mdr_prop_hi,
e_mdr_num, e_mdr_num_lo, e_mdr_num_hi,
c_newunk, c_ret, 
dst_rlt_new, dst_rlt_ret, dst_rlt_unk, 
mdr_new,mdr_ret,mdr, rapid_dx_dr_r, 
conf_mdr_tx, unconf_mdr_tx))

oa <- oa[order(oa$country),]
names(oa)[names(oa)=='country'] <- 'area'
estvars <- c("e_new_mdr_prop", "e_new_mdr_prop_lo", "e_new_mdr_prop_hi", "e_ret_mdr_prop", "e_ret_mdr_prop_lo", "e_ret_mdr_prop_hi", "e_mdr_num", "e_mdr_num_lo", "e_mdr_num_hi")

# make aggregate rows
ob <- oa[oa$iso3 %in% whbc, ]
obh <- aggregate(ob[4:ncol(ob)], by=list(area=ob$g_whoregion), FUN=sum, na.rm=TRUE)
obh$area <- "Total"
obh[estvars] <- NA # Not the proper way to sum bounds. Maybe to replace this later.

obw <- aggregate(oa[4:ncol(oa)], by=list(area=oa$g_whoregion), FUN=sum, na.rm=TRUE)
obw[estvars] <- subset(emdra, year==yr & group_name=="WPR", estvars) # Proper sums for aggregates.

# combine together
oc <- rbind(ob[3:ncol(ob)], obh, obw) 

# calculate and format vars

for(stima in c("new", "ret")){ # change proportional estimates to percent
for(bound in c("", "_lo", "_hi")){
oc[paste0("e_", stima, "_mdr_pct", bound)] <- oc[paste0("e_", stima, "_mdr_prop", bound)] * 100
}
}

oc$e_new_mdr_pct_range <- paste0("(", rounder(oc$e_new_mdr_pct_lo), "&#8211;", rounder(oc$e_new_mdr_pct_hi), ")")
oc$e_ret_mdr_pct_range <- paste0("(", rounder(oc$e_ret_mdr_pct_lo), "&#8211;", rounder(oc$e_ret_mdr_pct_hi), ")")
oc$e_mdr_num_range <- paste0("(", rounder(oc$e_mdr_num_lo), "&#8211;", rounder(oc$e_mdr_num_hi), ")")

oc$dst_rlt_new_pct <- rounder(oc$dst_rlt_new / oc$c_newunk * 100)
oc$dst_rlt_ret_pct <- rounder(oc$dst_rlt_ret / oc$c_ret * 100)

oc$mdr_new_pct <- rounder(oc$mdr_new / oc$c_newunk * 100)
oc$mdr_ret_pct <- rounder(oc$mdr_ret / oc$c_ret * 100)

oc$mdr_tx <- .rowsums(oc[c('conf_mdr_tx', 'unconf_mdr_tx')])

# Format
for(var in c("e_new_mdr_pct", "e_ret_mdr_pct", "e_mdr_num", "mdr", "rapid_dx_dr_r", "conf_mdr_tx", "mdr_tx")){
oc[var] <- ifelse(is.na(oc[[var]]), "&#8211;", rounder(oc[[var]]))
}
for(var in c("mdr_new_pct", "mdr_ret_pct")){
oc[var] <- ifelse(is.na(oc[[var]]), "&#8211;", oc[[var]])
}

# Rename countries
od <- .WPSARnames(oc, col="area")

rownames(od) <- od$area

oe <- subset(od, select=c("mdr_new_pct", "e_new_mdr_pct", "e_new_mdr_pct_range", "mdr_ret_pct", "e_ret_mdr_pct", "e_ret_mdr_pct_range", "mdr", "e_mdr_num", "e_mdr_num_range", "rapid_dx_dr_r", "conf_mdr_tx", "mdr_tx"))

##### `r I(pasteLabel("Table", tableCount, "t-drestnotif", insLink=FALSE))`. Estimated drug resistant cases compared with notified cases and cases put on treatment, `r yr`

drestnotif2 <- htmlTable(oe, caption = "", rowlabel = "", cgroup = rbind(c("% MDR-TB among new", "% MDR-TB among ret.", "MDR-TB cases", "", rep(NA,5)), c(rep(c("Detected", "Estimated"),2), "Detected", "Estimated <br>___________________", "RR cases<sup>*</sup>", "Confirmed MDR-TB on treatment", "Total MDR-TB on treatment")), n.cgroup = rbind(c(3,3,3,3, rep(NA,5)), c(1,2,1,2,1,2,1,1,1)), align=c(rep(c('c','r', 'l'),3), rep("c",3) ), ctable = TRUE, tfoot = "<sup>*</sup> Additional Rifampicine resistant cases detected by Xpert.", headings = NA )

write.csv(oe, file=paste0(pasteLabel("./figure_data/table", tableCount, "drestnotif2", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")

# Bit to copy html file over
# file.copy("D:/Users/hiattt/Dropbox/Code/Surveillance reports/MDR_report/MDRupdate.html", "D:/Users/hiattt/Dropbox/STB-WPRO/MDR article/MDRupdate.html", overwrite = TRUE)
