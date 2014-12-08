###################################################
# Tom Hiatt
# Updated: 01 September 2014
# Regional MDR-TB annual analysis
# This is the data munging script to be called from the associated Rmd file. Changes to data crunching are done here and changes to the text are done on the google doc file, then copied to the Rmd file, and then run. 
# The bulk of data munging for tables and figures goes here. In-paragraph code still largely is processed there in order to avoid mistakes.
###################################################
# title: "Drug resistant tuberculosis in the WHO Western Pacific Region"

# Script found at: https://docs.google.com/document/d/1QGiWYD68Y6w1lo8d2or4889kSxHrzcNkMK3lhPQOvCQ/edit

# How to reproduce this analysis.
# - Download and open in R the "MDRWPRO2014.Rdata" data and scripts. (If you are seeing this you have completed this step.)
# - Save the four script files to your computer using 
### cat(script1, file = "script1.Rmd")
### cat(script2, file = "script2.R")
### cat(script3, file = "springer-vancouver.csl")
### cat(script4, file = "MDRupdate.bib")
# - Open script1.Rmd in Rstudio and run.

# TO DELETE BEFORE PUBLICATION
# setwd("D:/Users/hiattt/Dropbox/Code/Surveillance reports/MDR_report")

# script1 <- readChar("./MDRupdate.Rmd", file.info("./MDRupdate.Rmd")$size)
# script2 <- readChar("./MDRupdate.R", file.info("./MDRupdate.R")$size)
# script3 <- readChar("./springer-vancouver.csl", file.info("./springer-vancouver.csl")$size)
# script4 <- readChar("./MDRupdate.bib", file.info("./MDRupdate.bib")$size)


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
require("directlabels")
require("dplyr")

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

# theme_report <- theme_bw() +
#     theme(
# #       line = element_line(colour = gray),
#       rect = element_rect(fill = "white", colour = NA),
# #       text = element_text(colour = black),
#       axis.ticks.x = element_line(colour = gray),
#       axis.ticks.y = element_blank(),
#       legend.key = element_rect(colour = NA),
#       ## Examples do not use grid lines
#       panel.border = element_rect(colour = gray),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank(),
#       strip.background = element_rect(fill="white", colour=NA),
#       strip.text = element_text(hjust=0)
#     )


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
.WPSARnames <- function(d, col='country'){
  d[col] <- as.character(d[[col]])
  d[col] <- ifelse(d[[col]]=='Lao People\'s Democratic Republic', 'Lao People\'s \nDemocratic \nRepublic', 
                   ifelse(d[[col]]=='China, Macao SAR', 'China, Macao SAR', 
                          ifelse(d[[col]]=='Micronesia (Federated States of)', 'Micronesia, Federated States of', 
                                 ifelse(d[[col]]=='WPR', 'Western Pacific Region
', d[[col]]))))
  
  return(d)
}

# For adding an x-axis to orphaned plots -----------------------------
facetAdjust <- function(x, pos = c("up", "down"))
{
  pos <- match.arg(pos)
  p <- ggplot_build(x)
  gtable <- ggplot_gtable(p); dev.off()
  dims <- apply(p$panel$layout[2:3], 2, max)
  nrow <- dims[1]
  ncol <- dims[2]
  panels <- sum(grepl("panel", names(gtable$grobs)))
  space <- ncol * nrow
  n <- space - panels
  if(panels != space){
    idx <- (space - ncol - n + 1):(space - ncol)
    gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
    if(pos == "down"){
      rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"), 
                   gtable$layout$name)
      lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
      gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
    }
  }
  class(gtable) <- c("facetAdjust", "gtable", "ggplot"); gtable
}
# The function for printing which differs only by few lines from ggplot2:::print.ggplot:
print.facetAdjust <- function(x, newpage = is.null(vp), vp = NULL) {
  if(newpage)
    grid.newpage()
  if(is.null(vp)){
    grid.draw(x)
  } else {
    if (is.character(vp)) 
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(x)
    upViewport()
  }
  invisible(x)
}

# Functions for maps ####################################

# source("./MapFunctions.r")
# load("gparts.Rdata")

# Tables and figure counts ##############################
# Functions to assist with tables and figure in markdown document (from here: http://rmflight.github.io/posts/2012/10/papersinRmd.html)

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
# NOTE: After this is nearly final I will replace this portion with a subset of only the data used which will be embedded in the final HTML file.

# runprofile()
# mdat1 <- subset(tb, g_whoregion=="WPR" & year %in% 2007:yr, c("iso3", "g_whoregion", "country", "e_new_mdr_num", "e_new_mdr_num_lo", 
#  "e_new_mdr_num_hi", "e_ret_mdr_num", "e_ret_mdr_num_lo", "e_ret_mdr_num_hi", 
#  "e_mdr_num", "e_mdr_num_lo", "e_mdr_num_hi", "mdr_new", "mdr_ret", 
#  "mdr_unk", "dr_r_nh_new", "dr_r_nh_ret", "dr_r_nh_unk", "xpert_dr_r_new", 
#  "xpert_dr_r_ret", "xpert_dr_r_unk", "mdr", "rapid_dx_dr_r", "conf_mdr_tx", 
#  "unconf_mdr_tx", "e_new_mdr_prop", "e_new_mdr_prop_lo", "e_new_mdr_prop_hi", 
#  "e_ret_mdr_prop", "e_ret_mdr_prop_lo", "e_ret_mdr_prop_hi", "c_newunk", 
#  "c_ret", "dst_rlt_new", "dst_rlt_ret", "dst_rlt_unk", "year", 
#  "mdr_dst_rlt", "mdr_ds_fq2li", "mdr_dr_fq", 
#  "mdr_dr_2li", "xdr", "mdr_coh", "mdr_cur", "mdr_cmplt", "mdr_died", 
#  "mdr_fail", "mdr_def", "mdr_succ", "mdr_hivpos", "mdr_hivneg", 
#  "mdr_hivunk", "nmdr_hivpos", "nmdr_hivneg", "nmdr_hivunk", "mdr_f", 
#  "mdr_m", "mdr_sexunk", "nmdr_f", "nmdr_m", "nmdr_sexunk", "mdr_014", 
#  "mdr_15plus", "mdr_ageunk", "nmdr_014", "nmdr_15plus", "nmdr_ageunk", 
#  "c_notified", "new_ep", "ret_rel_ep", "xpert_new", "xpert_ret", 
#  "xpert_unk", "new_labconf", "new_clindx", "ret_rel_labconf", 
#  "ret_rel_clindx", "ret_nrel", "cullpa_new", "cullpa_ret", "cullpa_unk", 
#  "dr_h_nr_new", "dr_h_nr_ret", "dr_h_nr_unk", "e_pop_num"))
# 
# mdat2 <- subset(f, year %in% 2007:yr & g_whoregion=="WPR", select=c(iso3, country, year, exp_sld, exp_mdrmgt, exp_tot, rcvd_sld_gov, rcvd_sld_loan, rcvd_sld_gf, rcvd_sld_grnt, rcvd_mdrmgt_gov, rcvd_mdrmgt_loan, rcvd_mdrmgt_gf, rcvd_mdrmgt_grnt))
# 
# mdat <- merge(mdat1, mdat2)
# 
# madat1 <- subset(emdra,year==yr&group_name=="WPR",c("year", "group_name", "e_new_mdr_num","e_new_mdr_num_lo","e_new_mdr_num_hi","e_ret_mdr_num",
# "e_ret_mdr_num_lo","e_ret_mdr_num_hi","e_new_mdr_prop","e_new_mdr_prop_lo",
# "e_new_mdr_prop_hi","e_ret_mdr_prop","e_ret_mdr_prop_lo","e_ret_mdr_prop_hi",
# "e_mdr_num","e_mdr_num_lo","e_mdr_num_hi"))
# 
# madat2 <- subset(a, year==yr & group_name=="WPR", c("year", "group_name", "e_inc_num", "e_mort_exc_tbhiv_num"))
# 
# madat <- merge(madat1, madat2)
# 
# save(mdat, madat, script1, script2, script3, script4, WHOmap.print, centres, gline, gpoly, gworld, pieces, file="MDRWPRO2014.Rdata")
load("MDRWPRO2014.Rdata")


# external.data <- FALSE
# 
# if(!external.data){
#   source("D:/Users/hiattt/Dropbox/Code/R/.Rprofile")
#   runprofile()
# } 
# 
# if(external.data){
#   if(!"data" %in% dir() | length(dir("./data"))==0){
#     dir.create(paste("./data"))
#     stop("Download the notification and treatment outcomes data from 'http://who.int/tb/country/data/download/en/' to './data'.")
#   }
#   
#   data1 <- dir("./data", full.names=TRUE)
#   tb <- NA
#   for(i in data1){
#     data3 <- read.csv(i)
#     tb <- merge(tb, data3, all=TRUE)
#   }
#   
#   # Note: to use your own data, make a flat data file with each row corresponding to the lower reporting unit and a aggregating variable. EG by country with aggregating variable of region.
#   
#   # Add needed variables
#   tb$g_hbc22 <- ifelse(tb$iso3 %in% c("AFG", "BGD", "BRA", "CHN", "COD", "ETH", "IDN", "IND", "KEN", "KHM", "MMR", "MOZ", "NGA", "PAK", "PHL", "RUS", "THA", "TZA", "UGA", "VNM", "ZAF", "ZWE"), "high", "low")
#   
#   tb$c_new <- .rowsums(tb[c("new_sp", "new_sn", "new_su", "new_ep", "new_oth")])
#   tb$c_newinc <- .rowsums(tb[c("c_new", "ret_rel", "newret_oth")])
#   tb$c_ret <- .rowsums(tb[c("ret_rel", "ret_taf", "ret_tad", "ret_oth")])
#   tb$c_notified <- .rowsums(tb[c("c_new", "c_ret", "newret_oth")])
#   
#   tb$c_tot_newrel_100k <- tb$tot_newrel / tb$e_pop_num * 1e5
#   
#   # Get population data (available from UN population division. http://esa.un.org/unpd/wpp/unpp/panel_population.htm) and formatted TB/HIV data and estimates for the WHO Western Pacific Region.
#   
# } # End of external data part


# Common data munging 

whbc <- subset(mdat, g_whoregion=="WPR" & year==yr & mdr>=1, c(iso3))
whbc <- whbc$iso3

whbc10 <- subset(mdat, g_whoregion=="WPR" & year==yr & mdr>=10, c(iso3))
whbc10 <- whbc10$iso3

mdat$country2 <- ifelse(mdat$iso3 %in% whbc, as.character(mdat$iso3), "Other") 
mdat$country2 <- factor(mdat$country2, c("KHM", "CHN", "JPN", "MYS", "MNG", "PNG", "PHL", "KOR", "VNM", "Other"), c("Cambodia", "China", "Japan", "Malaysia", "Mongolia", "Papua New Guinea", "Philippines", "Republic of Korea", "Viet Nam", "Other countries"))

# Embed data -----------------------------------------

# The function below, when called, will print HTML code containing a link to an encoded version of an RData file. From here: http://bayesfactor.blogspot.com/2014/09/embedding-rdata-files-in-rmarkdown.html

setDownloadURI = function(list, filename = stop("'filename' must be specified"), textHTML = "Click here to download the data.", fileext = "RData", envir = parent.frame()){
  require(base64enc,quietly = TRUE)
  divname = paste(sample(LETTERS),collapse="")
  tf = tempfile(pattern=filename, fileext = fileext)
  save(list = list, file = tf, envir = envir)
  filenameWithExt = paste(filename,fileext,sep=".")
  
  uri = dataURI(file = tf, mime = "application/octet-stream", encoding = "base64")
  cat("<a style='text-decoration: none' id='",divname,"'></a>
    <script>
    var a = document.createElement('a');
    var div = document.getElementById('",divname,"');
    div.appendChild(a);
    a.setAttribute('href', '",uri,"');
    a.innerHTML = '",textHTML,"' + ' (",filenameWithExt,")';
    if (typeof a.download != 'undefined') {
      a.setAttribute('download', '",filenameWithExt,"');
    }else{
      a.setAttribute('onclick', 'confirm(\"Your browser does not support the download HTML5 attribute. You must rename the file to ",filenameWithExt," after downloading it (or use Chrome/Firefox/Opera). \")');
    }
    </script>",
      sep="")
}

# End of setup


# m.coverage -----------------------------------------------
figCount <- incCount(figCount, "m.coverage")

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

# coa$cat <- cut(coa$year, c(0, 2005, 2010, 2014, Inf), c('1997\u20132004', '2005\u20132009', '2010\u20132013', 'Ongoing survey in 2014'), right=FALSE)

cob <- merge(coa, subset(mdat, year==yr, iso3), all.y=TRUE)
cob$cat <- cut(cob$year, c(0, 2005, 2010, 2014, Inf), c('1997\u20132004', '2005\u20132009', '2010\u20132013', 'Ongoing survey in 2014'), right=FALSE)

# levels(cob$cat) <- c(levels(cob$cat), "No data")
# cob[is.na(cob$cat), "cat"] <- "No data"

m.coverage <- WHOmap.print(cob, legend.title= "Year of most \nrecent data", na.label = "No recent data", copyright=FALSE, show=FALSE, zoom="WPR")

write.csv(coa, file=paste0(pasteLabel("./figure_data/figure", figCount, "m.coverage", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")

# t.drestnotif ----------------------------------------------------
tableCount <- incCount(tableCount, "t.drestnotif")

# Notification table

oa1 <- subset(mdat, year==yr & g_whoregion=="WPR", 
             select=c(iso3, g_whoregion, country, 
                      e_new_mdr_num, e_new_mdr_num_lo, e_new_mdr_num_hi,
                      e_ret_mdr_num, e_ret_mdr_num_lo, e_ret_mdr_num_hi,
                      e_new_mdr_prop, e_new_mdr_prop_lo, e_new_mdr_prop_hi,
                      e_ret_mdr_prop, e_ret_mdr_prop_lo, e_ret_mdr_prop_hi,
                      e_mdr_num, e_mdr_num_lo, e_mdr_num_hi,
                      c_newunk, c_ret, 
                      dst_rlt_new, dst_rlt_ret, dst_rlt_unk, 
                      mdr_new,mdr_ret,mdr, rapid_dx_dr_r, 
                      conf_mdr_tx, unconf_mdr_tx))

oa1 <- oa1[order(oa1$country),]
names(oa1)[names(oa1)=='country'] <- 'area'
estvars <- c("e_new_mdr_num", "e_new_mdr_num_lo", "e_new_mdr_num_hi", "e_ret_mdr_num", "e_ret_mdr_num_lo", "e_ret_mdr_num_hi", "e_new_mdr_prop", "e_new_mdr_prop_lo", "e_new_mdr_prop_hi", "e_ret_mdr_prop", "e_ret_mdr_prop_lo", "e_ret_mdr_prop_hi", "e_mdr_num", "e_mdr_num_lo", "e_mdr_num_hi")

# make aggregate rows
ob <- oa1[oa1$iso3 %in% whbc, ]
# obh <- aggregate(ob[4:ncol(ob)], by=list(area=ob$g_whoregion), FUN=sum, na.rm=TRUE)
# obh$area <- "Total"
# obh[estvars] <- NA # Not the proper way to sum bounds. Maybe to replace this later.

obw <- aggregate(oa1[4:ncol(oa1)], by=list(area=oa1$g_whoregion), FUN=sum, na.rm=TRUE)
obw[estvars] <- subset(madat, year==yr & group_name=="WPR", estvars) # Proper sums for aggregates.

# combine together
oc <- rbind(ob[3:ncol(ob)], obw) # Leaving out total for now  obh, 

# calculate and format vars

for(stima in c("new", "ret")){ # change proportional estimates to percent
  for(bound in c("", "_lo", "_hi")){
    oc[paste0("e_", stima, "_mdr_pct", bound)] <- oc[paste0("e_", stima, "_mdr_prop", bound)] * 100
  }
}

oc$e_new_mdr_num_range <- paste0("(", rounder(oc$e_new_mdr_num_lo), "&#8211;", rounder(oc$e_new_mdr_num_hi), ")")
oc$e_new_mdr_pct_range <- paste0("(", rounder(oc$e_new_mdr_pct_lo), "&#8211;", rounder(oc$e_new_mdr_pct_hi), ")")

oc$e_ret_mdr_num_range <- paste0("(", rounder(oc$e_ret_mdr_num_lo), "&#8211;", rounder(oc$e_ret_mdr_num_hi), ")")
oc$e_ret_mdr_pct_range <- paste0("(", rounder(oc$e_ret_mdr_pct_lo), "&#8211;", rounder(oc$e_ret_mdr_pct_hi), ")")

oc$e_mdr_num_range <- paste0("(", rounder(oc$e_mdr_num_lo), "&#8211;", rounder(oc$e_mdr_num_hi), ")")

# oc$dst_rlt_new_pct <- rounder(oc$dst_rlt_new / oc$c_newunk * 100)
# oc$dst_rlt_ret_pct <- rounder(oc$dst_rlt_ret / oc$c_ret * 100)
# 
# oc$mdr_new_pct <- rounder(oc$mdr_new / oc$c_newunk * 100)
# oc$mdr_ret_pct <- rounder(oc$mdr_ret / oc$c_ret * 100)
# 
# oc$mdr_tx <- .rowsums(oc[c('conf_mdr_tx', 'unconf_mdr_tx')])

# Format
for(these in c("e_new_mdr_num", "e_ret_mdr_num", "e_new_mdr_pct", "e_ret_mdr_pct", "e_mdr_num")){
  oc[these] <- ifelse(is.na(oc[[these]]), "&#8211;", rounder(oc[[these]]))
}
# for(var in c("mdr_new_pct", "mdr_ret_pct")){
#   oc[var] <- ifelse(is.na(oc[[var]]), "&#8211;", oc[[var]])
# }

# Rename countries
od <- .WPSARnames(oc, col="area")

rownames(od) <- od$area

oe <- subset(od, select=c("e_new_mdr_num", "e_new_mdr_num_range", "e_new_mdr_pct", "e_new_mdr_pct_range", "e_ret_mdr_num", "e_ret_mdr_num_range", "e_ret_mdr_pct", "e_ret_mdr_pct_range", "e_mdr_num", "e_mdr_num_range"))

# Make that table
t.drestnotif <- htmlTable(oe, caption = "", rowlabel = "", cgroup = rbind(c("MDR-TB among new", "MDR-TB among retreatment", "Total MDR-TB cases", rep(NA,2)), c(rep(c("n", "%"),2), "n")), n.cgroup = rbind(c(4,4,2, rep(NA,2)), rep(2,5)), align=rep(c('r', 'l'),10), ctable = TRUE, headings = NA )

# make text stop wrapping
t.drestnotif <- gsub('<td', '<td nowrap="nowrap"; ', t.drestnotif)

write.csv(oe, file=paste0(pasteLabel("./figure_data/table", tableCount, "t.drestnotif", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")

# f.estxy ----------------------------------------------------
figCount <- incCount(figCount, "f.estxy")

ea <- subset(mdat, year==yr & g_whoregion=="WPR", 
              select=c(country, 
                       e_new_mdr_prop, e_new_mdr_prop_lo, e_new_mdr_prop_hi,
                       e_ret_mdr_prop, e_ret_mdr_prop_lo, e_ret_mdr_prop_hi
                       ))

names(ea)[names(ea)=='country'] <- 'area'

eb <- subset(madat, year==yr & group_name=="WPR", c(group_name, e_new_mdr_prop, e_new_mdr_prop_lo, e_new_mdr_prop_hi, e_ret_mdr_prop, e_ret_mdr_prop_lo, e_ret_mdr_prop_hi))

names(eb)[names(eb)=='group_name'] <- 'area'

# combine together
ec <- .WPSARnames(rbind(ea, eb))

# ec$mdr.sum <- ec$e_new_mdr_prop + ec$e_ret_mdr_prop
# 
# ec$dupe <- duplicated(ec$mdr.sum)
# ec$counter <- 1
# ed <- aggregate(ec["counter"], by=list(mdr.sum=ec$mdr.sum), FUN=sum, na.rm=TRUE)
# 
# ee <- merge(ec[-ncol(ec)], ed)
# ee$area2 <- ifelse(ee$counter==1, ee$area, paste(ee$counter, "countries"))

# Remove estimates that only use the regional average
ec$dupe <- ifelse(signif(ec$e_new_mdr_prop,2) %in% c(0, 0.044, 0.045) | signif(ec$e_ret_mdr_prop,2) %in% c(0, 0.222), TRUE, FALSE)
# 

ef <- subset(ec, !dupe)

#####
# # This is currently a dirty hack. I should try and fix it to more systematic.
# library(dplyr)
# ef <- ef[!grepl("countries", ef$area2),] %>%
#   filter(area2 %nin% c("Brunei Darussalam", "Solomon Islands", "Marshall Islands", "Vanuatu", "WPR", "Malaysia") )
# #   ef[!grepl("Brunei", ef$area2),] %>%
# #   ef[!grepl("Solomon", ef$area2),] %>%
# #   ef[!grepl("Marshall", ef$area2),] %>%
# #   ef[!grepl("Vanu", ef$area2),] %>%
#   ef[!grepl("WPR", ef$area2),]



# library(dplyr)
mod <- lm(ef$e_ret_mdr_prop ~ ef$e_new_mdr_prop) %>%
  summary()



p <- ggplot(ef, aes(e_new_mdr_prop, e_ret_mdr_prop, color=area)) + geom_point() + geom_smooth(aes(group=1), method="lm", se=TRUE, fullrange=TRUE) + theme_report() + theme(legend.position = "none") + labs(x="Proportion of MDR-TB among new cases", y="Proportion of MDR-TB among retreatment cases") + expand_limits(x=0, y=0)
f.estxy <- direct.label(p) + annotate("text", x = .04, y = .4, label = paste0("r^2", "==", signif(mod$r.squared,3)), parse=TRUE) 
f.estxy

# geom_pointrange gives you bars.I need to ask PG how he does them on the x axis as well. These ranges are kind of dumb looking here. , ymin=e_ret_mdr_prop_lo, ymax=e_ret_mdr_prop_hi

write.csv(ef, file=paste0(pasteLabel("./figure_data/figure", figCount, "f.estxy", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE)

# f.estnotif ----------------------------------------------------
figCount <- incCount(figCount, "f.estnotif")


# t.drnotif -------------------------------------------------------------
tableCount <- incCount(tableCount, "t.drnotif")

# Notification table

nb <- subset(mdat, year==yr & g_whoregion=="WPR", 
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

# nbf <- apply(nbf, 2, gsub, patt=" ", replace="&nbsp;")

for(clm in 1:ncol(nbf)){
  nbf[,clm] <- gsub(' ', '&nbsp;', nbf[,clm])
}

names(nbf) <- rep(c("n", "%"), 9)

# Make that table
t.drnotif <- htmlTable(nbf, caption = "", rowlabel = "", cgroup = rbind(c("Notified cases with DST results", "MDR-TB and RR-TB<sup>*</sup> among cases with DST results", "Cases confirmed by Xpert among MDR-TB and RR-TB<sup>*</sup>", rep(NA,6)), rep(c("New", "Ret.", "Total"),3)), n.cgroup = rbind(c(rep(3,3), rep(NA,6)), rep(2,9)), ctable = TRUE, tfoot = "<sup>*</sup> Rif-resistant only cases are included whether confirmed by DST or Xpert.")

# tmp <- gsub( '<td', '<td nowrap="nowrap"; ', t.drnotif)

write.csv(nbf, file=paste0(pasteLabel("./figure_data/table", tableCount, "t.drnotif", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")

# f.dst.trend --------------------------------------------------
figCount <- incCount(figCount, "f.dst.trend")


dsa <- subset(mdat, year %in% 2007:yr & g_whoregion=="WPR", 
             select=c(iso3, g_whoregion, country, year, 
                      c_notified, c_ret, new_ep, ret_rel_ep, 
                      dst_rlt_new, dst_rlt_ret, dst_rlt_unk,
                      dr_r_nh_ret, dr_r_nh_new, dr_r_nh_unk, 
                      mdr_new,mdr_ret,mdr_unk,
                      xpert_new, xpert_ret, xpert_unk, 
                      xpert_dr_r_new, xpert_dr_r_ret, xpert_dr_r_unk))

dsb <- dsa[order(dsa$country),]
names(dsb)[names(dsb)=='country'] <- 'area'

# Treat all missings as 0s.
# dsb[is.na(dsb)] <- 0

# calculate numerators and denominators for aggregates
  # Add up vars where needed
dsb$dstx <- .rowsums(dsb[c("dst_rlt_ret", "xpert_ret")])
dsb$mdrr <- .rowsums(dsb[c( 'mdr_ret', 'dr_r_nh_ret', 'xpert_dr_r_ret')])

  # Only include country-years where the num and denom are both present
dsb$dstx.numerator <- ifelse(is.na(dsb$c_ret), NA, dsb$dstx)
dsb$dstx.denominator <- ifelse(is.na(dsb$dstx), NA, dsb$c_ret)

dsb$mdrr.numerator <- ifelse(is.na(dsb$dstx), NA, dsb$mdrr)
dsb$mdrr.denominator <- ifelse(is.na(dsb$mdrr), NA, dsb$dstx)

# make aggregate rows
dsbc <- dsb[dsb$iso3 %in% whbc10, ]
# nbch <- aggregate(nbc[4:ncol(nbc)], by=list(area=nbc$g_whoregion), FUN=sum, na.rm=TRUE)
# nbch$area <- "Total"

dsbcw <- aggregate(dsb[5:ncol(dsb)], by=list(area=dsb$g_whoregion, year=dsb$year), FUN=sum, na.rm=TRUE)
# dsbcw$iso3 <- dsbcw$g_whoregion <- "WPR"

# combine together
dsd <- rbind(dsbc[3:ncol(dsbc)], dsbcw) # nbch, 

# calculate PERCENTAGE vars
dsd$dstx_pct <- dsd$dstx.numerator / dsd$dstx.denominator * 100 

dsd$mdrr_pct <- dsd$mdrr.numerator / dsd$mdrr.denominator * 100 


# Rename countries
dse <- .WPSARnames(dsd, col="area")
  

dsf <- melt(subset(dse, select=c(area, year, dstx_pct, mdrr_pct)), id=1:2)
dsf <- subset(dsf, !is.na(value))

f.dst.trend <- facetAdjust(ggplot(dsf, aes(year, value, color=variable)) + geom_point(alpha=.5) + geom_line(size=1, alpha=.5) + facet_wrap(~area, scales="free_y") + theme_report() + scale_color_brewer("", type="qual", palette=6, breaks=c("dstx_pct", "mdrr_pct"), labels=c(expression("% DST among pulmonary cases"^a), "% MDR-TB or RR-TB among tested")) + scale_x_continuous("", breaks=seq(min(dsf$year), max(dsf$year),2)) + scale_y_continuous("Percent")+ guides(fill = guide_legend(reverse = TRUE)) + theme(legend.position = "bottom") + expand_limits(y=0))

write.csv(dsf, file=paste0(pasteLabel("./figure_data/figure", figCount, "f.dst.trend", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE)



# t.est.enroll ------------------------------------------------------------
tableCount <- incCount(tableCount, "t.est.enroll")

# Notification table

oa2 <- subset(mdat, year==yr & g_whoregion=="WPR", 
             select=c(iso3, g_whoregion, country, 
                      e_new_mdr_num, e_new_mdr_num_lo, e_new_mdr_num_hi, 
                      e_ret_mdr_num, e_ret_mdr_num_lo, e_ret_mdr_num_hi,
                      e_mdr_num, e_mdr_num_lo, e_mdr_num_hi,
                      mdr_new, mdr_ret, mdr_unk, 
                      dr_r_nh_new, dr_r_nh_ret, dr_r_nh_unk, 
                      xpert_dr_r_new, xpert_dr_r_ret, xpert_dr_r_unk, 
                      mdr, rapid_dx_dr_r, 
                      conf_mdr_tx, unconf_mdr_tx))

oa2 <- oa2[order(oa2$country),]
names(oa2)[names(oa2)=='country'] <- 'area'
estvars <- c("e_new_mdr_num", "e_new_mdr_num_lo", "e_new_mdr_num_hi", "e_ret_mdr_num", "e_ret_mdr_num_lo", "e_ret_mdr_num_hi", "e_mdr_num", "e_mdr_num_lo", "e_mdr_num_hi")

# caclulate pre-aggregating variables. (For this table, non-reporters are assumed to be 0.)
oa2$mdrr.restricted <- .rowsums(oa2[c("mdr_new", "mdr_ret", "mdr_unk", "dr_r_nh_new", "dr_r_nh_ret", "dr_r_nh_unk", "xpert_dr_r_new", "xpert_dr_r_ret", "xpert_dr_r_unk")])
oa2$mdrr.unrestricted <- .rowsums(oa2[c('mdr', 'rapid_dx_dr_r')])

oa2$mdrr.best <- ifelse(oa2$mdrr.restricted > oa2$mdrr.unrestricted & !is.na(oa2$mdrr.restricted), oa2$mdrr.restricted, oa2$mdrr.unrestricted)

oa2$mdrr.new <- .rowsums(oa2[c("mdr_new", "dr_r_nh_new","xpert_dr_r_new")])
oa2$mdrr.ret <- .rowsums(oa2[c("mdr_ret", "dr_r_nh_ret","xpert_dr_r_ret")])

# make aggregate rows
ob <- oa2[oa2$iso3 %in% whbc, ]
# obh <- aggregate(ob[4:ncol(ob)], by=list(area=ob$g_whoregion), FUN=sum, na.rm=TRUE)
# obh$area <- "Total"
# obh[estvars] <- NA # Not the proper way to sum bounds. Maybe to replace this later.

obw <- aggregate(oa2[4:ncol(oa2)], by=list(area=oa2$g_whoregion), FUN=sum, na.rm=TRUE)
obw[estvars] <- subset(madat, year==yr & group_name=="WPR", estvars) # Proper sums for aggregates.

# combine together
oc <- rbind(ob[3:ncol(ob)], obw) # obh, 

# calculate and format vars

oc$e_mdr_num_range <- paste0("(", rounder(oc$e_mdr_num_lo), "&#8211;", rounder(oc$e_mdr_num_hi), ")")
oc$e_new_mdr_num_range <- paste0("(", rounder(oc$e_mdr_num_lo), "&#8211;", rounder(oc$e_new_mdr_num_hi), ")")
oc$e_ret_mdr_num_range <- paste0("(", rounder(oc$e_ret_mdr_num_lo), "&#8211;", rounder(oc$e_ret_mdr_num_hi), ")")

oc$pmdr <- oc$mdrr.best
oc$mdr_tx <- .rowsums(oc[c('conf_mdr_tx', 'unconf_mdr_tx')])

oc$pmdr_pct <- rounder(oc$pmdr / oc$e_mdr_num * 100)
oc$pmdr_pct_range <- paste0("(", rounder(oc$pmdr / oc$e_mdr_num_hi * 100), "&#8211;", rounder(oc$pmdr / oc$e_mdr_num_lo * 100), ")")

oc$pmdr_new_pct <- rounder(oc$mdrr.new / oc$e_new_mdr_num * 100)
oc$pmdr_new_pct_range <- paste0("(", rounder(oc$mdrr.new / oc$e_new_mdr_num_hi * 100), "&#8211;", rounder(oc$mdrr.new / oc$e_new_mdr_num_lo * 100), ")")

oc$pmdr_ret_pct <- rounder(oc$mdrr.ret / oc$e_ret_mdr_num * 100)
oc$pmdr_ret_pct_range <- paste0("(", rounder(oc$mdrr.ret / oc$e_ret_mdr_num_hi * 100), "&#8211;", rounder(oc$mdrr.ret / oc$e_ret_mdr_num_lo * 100), ")")

oc$mdr_tx_pct <- rounder(oc$mdr_tx / oc$pmdr * 100)

# Format
oc$e_mdr_num <- rounder(oc$e_mdr_num)

for(var in 2:ncol(oc)){
  oc[var] <- ifelse(is.na(oc[[var]]), "&#8211;", oc[[var]])
}

# Rename countries
od <- .WPSARnames(oc, col="area")

rownames(od) <- od$area

oe <- subset(od, select=c("e_new_mdr_num", "e_new_mdr_num_range", "e_ret_mdr_num", "e_ret_mdr_num_range", "e_mdr_num", "e_mdr_num_range", "mdrr.new", "mdrr.ret", "pmdr", "pmdr_new_pct", "pmdr_new_pct_range", "pmdr_ret_pct", "pmdr_ret_pct_range", "pmdr_pct", "pmdr_pct_range", "mdr_tx", "mdr_tx_pct"))

# Make that table
t.est.enroll <- htmlTable(oe, caption = "", rowlabel = "", cgroup = rbind(c("Estimated", "Notified<sup>a</sup>", "% notified among estimated", "Enrolled on treatment", rep(NA,7)), c(rep(c("New", "Ret.", "Total"),3), "n", "% among detected")), n.cgroup = rbind(c(6,3,6,2, rep(NA,7)), c(rep(2,3),rep(1,3),rep(2,3),1,1)), align=c(rep(c('r','l'),3), rep('r',3), rep(c('r','l'),3), rep('r',2)), ctable = TRUE, tfoot = "<sup>a</sup> All columns except estimates include Rif-resistant cases confirmed by Xpert only. Total MDR-TB cases detected include cases with history unknown, amongst extrapulmonary and from samples taken more than 2 weeks after start of treatment.<br>", headings = NA )

write.csv(oe, file=paste0(pasteLabel("./figure_data/table", tableCount, "t.est.enroll", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")


# f.alignment ------------------------------------------------------
figCount <- incCount(figCount, "f.alignment")

tea <- subset(mdat, year %in% 2007:yr & g_whoregion=="WPR", select=c(iso3, country, year, e_mdr_num, e_mdr_num_lo, e_mdr_num_hi, mdr, rapid_dx_dr_r, conf_mdr_tx, unconf_mdr_tx))

# Calculate new vars
tea$`Cases confirmed` <- .rowsums(tea[c('mdr', 'rapid_dx_dr_r')])
tea$`Patients enrolled on treatment` <- .rowsums(tea[c('conf_mdr_tx', 'unconf_mdr_tx')])

tea$agg.case.conf <- ifelse(is.na(tea$`Patients enrolled on treatment`), NA, tea$`Cases confirmed`)
tea$agg.pts.enr <- ifelse(is.na(tea$`Cases confirmed`), NA, tea$`Patients enrolled on treatment`)

teb1 <- aggregate(tea[4:ncol(tea)], by=list(year=tea$year), FUN=sum, na.rm=TRUE)
teb1$country <- teb1$iso3 <- "WPR"

# Separate hbcs
teb3 <- tea[tea$iso3 %in% whbc10,] 
# teb2 <- aggregate(teb3[4:ncol(tea)], by=list(year=teb3$year, area=teb3$country), FUN=sum, na.rm=TRUE)


#combine
teb <- rbind(teb1, teb3)

# replace agg row with reporters only.
teb[teb$country=="WPR", "Cases confirmed"] <- teb[teb$country=="WPR", "agg.case.conf"]
teb[teb$country=="WPR", "Patients enrolled on treatment"] <- teb[teb$country=="WPR", "agg.pts.enr"]

tec <- melt(teb[c("country", "year", "Cases confirmed", "Patients enrolled on treatment")], id=1:2)

f.alignment <- ggplot(tec, aes(year, value, color=variable)) + geom_point(alpha=.5) + geom_line(size=1, alpha=.5) + facet_wrap(~country, scales="free_y", ncol=4) + theme_report() + scale_color_brewer("", type="qual", palette=6) + scale_x_continuous("", breaks=seq(min(tec$year), max(tec$year),2)) + scale_y_continuous("MDR-TB cases")+ guides(fill = guide_legend(reverse = TRUE)) + theme(legend.position = "bottom") + expand_limits(y=0)

write.csv(tec, file=paste0(pasteLabel("./figure_data/figure", figCount, "f.alignment", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE)


# f.tx.out --------------------------------------------
figCount <- incCount(figCount, "f.tx.out")

tra <- subset(mdat, year %in% 2006:(yr-2) & g_whoregion=="WPR", select=c(iso3, country, year, mdr_coh, mdr_cur, mdr_cmplt, mdr_died, mdr_fail, mdr_def, mdr_succ))

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

trb <- .WPSARnames(trb, col = "area")

trb$mdr_succ <- ifelse(trb$year>=2011, trb$mdr_succ, trb$mdr_cur + trb$mdr_cmplt)
trb$Success <- trb$mdr_succ / trb$mdr_coh * 100
trb$Died <- trb$mdr_died / trb$mdr_coh * 100
trb$Failed <- trb$mdr_fail / trb$mdr_coh * 100
trb$`Lost to follow-up` <- trb$mdr_def / trb$mdr_coh * 100
trb$`Not evaluated` <- (trb$mdr_coh - (trb$mdr_succ + trb$mdr_died + trb$mdr_fail + trb$mdr_def)) / trb$mdr_coh * 100

trc <- melt(trb[c("area", "year", "Success", "Died", "Failed", "Lost to follow-up", "Not evaluated")], id=1:2)

f.tx.out <- ggplot(trc, aes(year, value, fill=variable)) + geom_bar(stat="identity", position="stack") + facet_wrap(~area) + theme_report() + scale_fill_brewer('Outcome', type="qual", palette=6) + scale_x_continuous("", breaks=seq(min(trc$year), max(trc$year),2)) + scale_y_continuous("Percent of cohort") + coord_cartesian(ylim=c(0,100)) + guides(fill = guide_legend(reverse = TRUE))

write.csv(trc, file=paste0(pasteLabel("./figure_data/figure", figCount, "f.tx.out", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE)


# t.xdr -----------------------------------------------------------
tableCount <- incCount(tableCount, "t.xdr")

# Notification table

xa <- subset(mdat, year==yr & g_whoregion=="WPR", 
             select=c(iso3, g_whoregion, country, 
                      unconf_mdr_tx, conf_mdr_tx, mdr_dst_rlt, mdr_ds_fq2li,
                      mdr_dr_fq, mdr_dr_2li, xdr))

xa <- xa[order(xa$country),]
names(xa)[names(xa)=='country'] <- 'area'

# make aggregate rows
xb <- xa[xa$iso3 %in% whbc, ]
# xc <- aggregate(xb[4:ncol(xb)], by=list(area=xb$g_whoregion), FUN=sum, na.rm=TRUE)
# xc$area <- "Total"

xd <- aggregate(xa[4:ncol(xa)], by=list(area=xa$g_whoregion), FUN=sum, na.rm=TRUE)

# combine together
xe <- rbind(xb[3:ncol(xb)], xd) # xc, , tbbh, tbbga

# calculate and format vars
xe$mdr_tx <- .rowsums(xe[c('conf_mdr_tx', 'unconf_mdr_tx')])

xe$xdst_pct <- xe$mdr_dst_rlt / xe$mdr_tx * 100
xe$fq_pct <- xe$mdr_dr_fq / xe$mdr_dst_rlt * 100
xe$xli_pct <- xe$mdr_dr_2li / xe$mdr_dst_rlt * 100
xe$xdr_pct <- xe$xdr / xe$mdr_dst_rlt * 100

# Format
for(var in 2:ncol(xe)){
  xe[var] <- ifelse(is.na(xe[[var]]), "&#8211;", rounder(xe[[var]]))
}

# Rename countries
xf <- .WPSARnames(xe, col="area")

rownames(xf) <- xf$area

xg <- subset(xf, select=c("mdr_tx", "mdr_dst_rlt", "xdst_pct", "mdr_dr_fq", "fq_pct", "mdr_dr_2li", "xli_pct", "xdr", "xdr_pct"))

names(xg) <- c("Enrolled on 2<sup>nd</sup>-line treatment", "number", "% of enrolled", rep(c("number", "% of tested"),3))

# Make that table
t.xdr <- htmlTable(xg, caption = "", rowlabel = "", cgroup = c("", "2<sup>nd</sup>-line DST", "Resistance to FQ", "Resistance to 2<sup>nd</sup>-line injectible", "XDR-TB"), n.cgroup = c(1,2,2,2,2), ctable = TRUE, tfoot = "")

write.csv(xf, file=paste0(pasteLabel("./figure_data/table", tableCount, "t.xdr", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")


# f.expend ------------------------------------------------------
figCount <- incCount(figCount, "f.expend")

tfa <- subset(mdat, year %in% 2007:yr & g_whoregion=="WPR", select=c(iso3, country, year, exp_sld, exp_mdrmgt, rcvd_sld_gov, rcvd_mdrmgt_gov, rcvd_sld_loan, rcvd_mdrmgt_loan, rcvd_sld_gf, rcvd_mdrmgt_gf, rcvd_sld_grnt, rcvd_mdrmgt_grnt))

tfb1 <- aggregate(tfa[4:ncol(tfa)], by=list(year=tfa$year), FUN=sum, na.rm=TRUE)
tfb1$country <- tfb1$iso3 <- "WPR"

# Separate hbcs
tfb3 <- tfa[tfa$iso3 %in% whbc10,] 
# tfb2 <- aggregate(tfb3[4:ncol(tfa)], by=list(year=tfb3$year, area=tfb3$country), FUN=sum, na.rm=TRUE)


#combine
tfb <- rbind(tfb1, tfb3)

# Calculate new vars
tfb$exp_pmdt <- .rowsums(tfb[c('exp_sld', 'exp_mdrmgt')]) / 1000

tfd <- melt(tfb[grep("rcvd|year|country", names(tfb))], id=c("country", "year"))

tfd$`Funding source` <- ifelse(grepl("gov", tfd$variable), "Government", "External")

tfd$value <- tfd$value / 1000

tfd <- subset(tfd, !is.na(value))

tfc <- subset(tfb, !is.na(exp_pmdt))


# tfc <- melt(tfb[c("country", "year", "Cases confirmed", "Patients enrolled on treatment")], id=1:2)

f.expend <- ggplot(tfc, aes(year, exp_pmdt)) + geom_bar(data=tfd, aes(year, value, fill=`Funding source`), position = "stack", stat="identity") + geom_point(alpha=.5) + geom_line(size=1, alpha=.5) + facet_wrap(~country, scales="free_y") + theme_report() + scale_x_continuous("", breaks=seq(min(tfc$year), max(tfc$year),2)) + scale_y_continuous("US$ (thousands)") + guides(fill = guide_legend(reverse = TRUE)) + expand_limits(y=0) + theme(legend.position = "bottom") + scale_fill_brewer()

write.csv(tfc, file=paste0(pasteLabel("./figure_data/figure", figCount, "f.expend", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE)


# age.sex.hiv.exploration -----------------------------------------------------
tableCount <- incCount(tableCount, "age.sex.hiv.exploration")

# Explore the association between MDR adn other things

oa3 <- subset(mdat, year==yr & g_whoregion=="WPR", 
             select=c(iso3, g_whoregion, country, 
                      mdr_hivpos, mdr_hivneg, mdr_hivunk, 
                      nmdr_hivpos, nmdr_hivneg, nmdr_hivunk, 
                      mdr_f, mdr_m, mdr_sexunk, 
                      nmdr_f, nmdr_m, nmdr_sexunk, 
                      mdr_014, mdr_15plus, mdr_ageunk, 
                      nmdr_014, nmdr_15plus, nmdr_ageunk))

oa3 <- oa3[order(oa3$country),]
names(oa3)[names(oa3)=='country'] <- 'area'


# make aggregate rows
ob <- oa3[oa3$iso3 %in% whbc, ]
obh <- aggregate(ob[4:ncol(ob)], by=list(area=ob$g_whoregion), FUN=sum, na.rm=TRUE)
obh$area <- "Total"


obw <- aggregate(oa3[4:ncol(oa3)], by=list(area=oa3$g_whoregion), FUN=sum, na.rm=TRUE)


# combine together
oc <- rbind(ob[3:ncol(ob)], obh, obw) 

# calculate and format Risk ratios (the increased risk of having MDR if you're male vs female, etc.) I'm ignoring unkowns here.

vars <- data.frame(left=c("hivpos", "f", "014"), middle=c("hivneg", "m", "15plus"), right=c("hiv", "sex", "age"))

for(type in 1:3){ # mdr kids over all kids divided by mdr adults over all adults
  oc[paste0(vars[type,3], "_rr", vars[type,1], "vs", vars[type,2])] <- (oc[paste0("mdr_", vars[type,1])] /
                                        .rowsums(oc[c(paste0("mdr_", vars[type,1]), paste0("nmdr_", vars[type,1]))])) /
    
    (oc[paste0("mdr_", vars[type,2])] /
       .rowsums(oc[c(paste0("mdr_", vars[type,2]), paste0("nmdr_", vars[type,2]))]))
}


# Format
for(var in c("hiv_rrhivposvshivneg", "sex_rrfvsm", "age_rr014vs15plus")){
  oc[var] <- ifelse(is.na(oc[[var]]), "&#8211;", signif(oc[[var]], 3))
}

# for(type in 1:3){
#   oc[paste0(vars[type,3], "_rr")] <- (oc[paste0("mdr_", vars[type,1])] /
#                                         .rowsums(oc[c(paste0("mdr_", vars[type,1]), paste0("mdr_", vars[type,2]), paste0("mdr_", vars[type,3], "unk"))])) / 
#     (oc[paste0("nmdr_", vars[type,1])] /
#        .rowsums(oc[c(paste0("nmdr_", vars[type,1]), paste0("nmdr_", vars[type,2]), paste0("nmdr_", vars[type,3], "unk"))]))
# }
# 
# 
# # Format
# for(var in c("hiv_rr", "sex_rr", "age_rr")){
#   oc[var] <- ifelse(is.na(oc[[var]]), "&#8211;", signif(oc[[var]], 3))
# }


# Rename countries
od <- .WPSARnames(oc, col="area")

rownames(od) <- od$area

oe <- subset(od, select=c("hiv_rrhivposvshivneg", "sex_rrfvsm", "age_rr014vs15plus"))

# Make that table
age.sex.hiv.exploration <- htmlTable(oe, caption = "Risk ratios for MDR-TB")
# View(od)

write.csv(oe, file=paste0(pasteLabel("./figure_data/table", tableCount, "age.sex.hiv.exploration", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")

# **************************************************************
# Dumping ground ----------------------------------------------
# **************************************************************

# drestnotif2 -------------------------------------------
tableCount <- incCount(tableCount, "t-drestnotif2")

# Notification table

oa4 <- subset(mdat, year==yr & g_whoregion=="WPR", 
             select=c(iso3, g_whoregion, country, 
                      e_new_mdr_prop, e_new_mdr_prop_lo, e_new_mdr_prop_hi,
                      e_ret_mdr_prop, e_ret_mdr_prop_lo, e_ret_mdr_prop_hi,
                      e_mdr_num, e_mdr_num_lo, e_mdr_num_hi,
                      c_newunk, c_ret, 
                      dst_rlt_new, dst_rlt_ret, dst_rlt_unk, 
                      mdr_new,mdr_ret,mdr, rapid_dx_dr_r, 
                      conf_mdr_tx, unconf_mdr_tx))

oa4 <- oa4[order(oa4$country),]
names(oa4)[names(oa4)=='country'] <- 'area'
estvars <- c("e_new_mdr_prop", "e_new_mdr_prop_lo", "e_new_mdr_prop_hi", "e_ret_mdr_prop", "e_ret_mdr_prop_lo", "e_ret_mdr_prop_hi", "e_mdr_num", "e_mdr_num_lo", "e_mdr_num_hi")

# make aggregate rows
ob <- oa4[oa4$iso3 %in% whbc, ]
obh <- aggregate(ob[4:ncol(ob)], by=list(area=ob$g_whoregion), FUN=sum, na.rm=TRUE)
obh$area <- "Total"
obh[estvars] <- NA # Not the proper way to sum bounds. Maybe to replace this later.

obw <- aggregate(oa4[4:ncol(oa4)], by=list(area=oa4$g_whoregion), FUN=sum, na.rm=TRUE)
obw[estvars] <- subset(madat, year==yr & group_name=="WPR", estvars) # Proper sums for aggregates.

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

##### `r I(pasteLabel("Table", tableCount, "t-drestnotif", insLink=FALSE, sepper=""))`. Estimated drug resistant cases compared with notified cases and cases put on treatment, `r yr`

drestnotif2 <- htmlTable(oe, caption = "", rowlabel = "", cgroup = rbind(c("% MDR-TB among new", "% MDR-TB among ret.", "MDR-TB cases", "", rep(NA,5)), c(rep(c("Detected", "Estimated"),2), "Detected", "Estimated <br>___________________", "RR cases<sup>*</sup>", "Confirmed MDR-TB on treatment", "Total MDR-TB on treatment")), n.cgroup = rbind(c(3,3,3,3, rep(NA,5)), c(1,2,1,2,1,2,1,1,1)), align=c(rep(c('c','r', 'l'),3), rep("c",3) ), ctable = TRUE, tfoot = "<sup>*</sup> Additional Rifampicine resistant cases detected by Xpert.", headings = NA )

write.csv(oe, file=paste0(pasteLabel("./figure_data/table", tableCount, "t-drestnotif2", insLink=FALSE, sepper=""), ".csv"), row.names=FALSE, na="")

# Bit to copy html file over
# file.copy("D:/Users/hiattt/Dropbox/Code/Surveillance reports/MDR_report/MDRupdate.html", "D:/Users/hiattt/Dropbox/STB-WPRO/MDR article/MDRupdate.html", overwrite = TRUE)
