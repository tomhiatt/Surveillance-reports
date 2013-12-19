# -------------------------------------------------
# Maps in the global report
# Tom Hiatt
# 6 July 2012
# -------------------------------------------------

source('d:/users/hiattt/Dropbox/Code/Global TB control Reports/Tables and Figures/2013/Setup.r')

# if map make is still broken, run this.
# source('c:/delete/Functions.r')

setwd(outfolder)

# -------------------------------------------------
# HIVtest_map
# -------------------------------------------------

mc <- subset(tbhiv, year==thisyear-1, select=c(country, iso2, iso3, g_whoregion, hivtest_pct_numerator, hivtest_pct_denominator))
mc$hivtest_prct <- round(mc$hivtest_pct_numerator/mc$hivtest_pct_denominator * 100)

# Fix and footnote for Russian Federation
mc[mc$country=='Russian Federation', 'hivtest_prct'] <- round(subset(n, iso2=='RU' & year==thisyear-1, hivtest_p) / subset(n, iso2=='RU' & year==thisyear-1, c_new) * 100)

warning("Russian Federation modification for the HIV test map is still in place. Delete this message when no longer applicable.")


mc$cat <- NA  
mc$cat[mc$hivtest_prct >= 75] <- ">=75"
mc$cat[mc$hivtest_prct < 75] <- '50–74'
mc$cat[mc$hivtest_prct < 50] <- '15–49'
mc$cat[mc$hivtest_prct < 15] <- '0–14'

mc$cat <- as.factor(mc$cat)
mc$cat <- factor(mc$cat, levels = c("0–14", "15–49",  "50–74", ">=75"))

# HIV testing map (ugly colors are for easier design)

mc1 <- WHOmap.print(mc, paste("Percentage of patients with known HIV status by country,", thisyear-1, "(a)"), "Percentage of \nnotified TB patients", copyright=FALSE, colors=c('red', 'blue', 'orange', 'green'), show=FALSE)

figsave(mc1, mc, "HIVtest_map")


# -------------------------------------------------
# HIVart_map
# -------------------------------------------------

mca <- subset(tbhiv, year==thisyear-1, select=c(country, iso2, iso3, g_whoregion, hiv_art_pct_numerator, hiv_art_pct_denominator))
mca$hivart_prct <- round(mca$hiv_art_pct_numerator/mca$hiv_art_pct_denominator * 100)

mca$cat <- cut(mca$hivart_prct, c(0, 25, 50, 75, Inf), c('0–24', '25–49', '50–74', '75–100'), right=FALSE)

# map

mca1 <- WHOmap.print(mca, paste("Percentage of HIV-positive TB patients enrolled on antiretroviral therapy (ART),", thisyear-1), "Percentage of \nHIV-positive \nTB patients", copyright=FALSE, colors=c('red', 'blue', 'orange', 'green'), show=FALSE)

figsave(mca1, mca, "HIVart_map")


# -------------------------------------------------
# HIVipt_map
# -------------------------------------------------

mca <- subset(n, year==thisyear-1 & hiv_reg_new < hiv_reg, select=c(country, iso2, iso3, g_whoregion, hiv_ipt, hiv_reg_new, hiv_reg))

mca$hivipt_prct <- mca$hiv_ipt/mca$hiv_reg_new * 100

mca$cat <- cut(mca$hivipt_prct, c(0, 10, 30, 50, Inf), c('0–9', '10–29', '30–49', '50–100'), right=FALSE)

# map

mca1 <- WHOmap.print(mca, paste("Percentage of people living with HIV enrolled on Isoniazid preventive therapy (IPT),", thisyear-1), "Percentage of \npeople living  \nwith HIV on IPT", copyright=FALSE)
#, show=FALSE)

figsave(mca1, mca, "HIVipt_map")

# Version 2 with 50% target

mca$hivipt_prct <- mca$hiv_ipt/(mca$hiv_reg_new / 2) * 100

mca$cat <- cut(mca$hivipt_prct, c(0, 25, 50, 75, Inf), c('0–24', '25–49', '50–74', '>=75'), right=FALSE)

# map

mca2 <- WHOmap.print(mca, paste("Percentage of elligible people newly enrolled on HIV care \nand provied isoniazid preventive therapy (IPT),", thisyear-1), "Percentage of \npeople living  \nwith HIV on IPT", copyright=FALSE)
#, show=FALSE)

figsave(mca2, mca, "HIVipt_map2")

# -------------------------------------------------
# Xpert_map
# -------------------------------------------------

library(XLConnect)

mda <- loadWorkbook("D:/Users/hiattt/Google Drive/Work files/Global TB report/Tables and Figures/From others/GeneXpert cartridges for Tom.xlsx")
mdb <- readWorksheet(mda, "Modules for Tom")

mdb$carts <- as.numeric(mdb$cartridges)

# qplot(mdb[mdb$carts<500, 'carts'])

mdb$cat <- cut(mdb$carts, c(0, 1, 2000, 10000, 40000, 100000, Inf), c('0', '1–1 999', '2 000–9 999', '10 000–39 999', '40 0000–99 999', '>=100 000'), right=FALSE)

# map
mdc <- WHOmap.print(mdb, paste("Progress in the roll-out of Xpert MTB/RIF, by July", thisyear), "Xpert MTB/RIF \ncartridges ordered", copyright=FALSE, na.label="Not eligible for \npreferential pricing", colors=c('red', 'blue', 'orange', 'green', 'brown', 'purple'), show=FALSE)

figsave(mdc, mdb, "Xpert_map")

# -------------------------------------------------
# Estimates sources maps
# -------------------------------------------------

mea <- merge(read.csv("https://dl.dropbox.com/u/454007/annex1_data_sources.csv"), subset(e, year==thisyear-1, select=c('country', 'iso3')))

# Mortality with VR data

meb <- subset(mea, source.mort=='VR', select=c("g.whoregion", "country", "iso3", "source.mort"))

meb$cat <- factor("Estimated with \nVR data")

# map
mec <- WHOmap.print(meb, paste("Countries for which TB mortality is directly measured from vital registration systems \nand/or mortality surveys,", thisyear-1), "", low.color="dark red", copyright=FALSE)
#, show=FALSE)

figsave(mec, meb, "mort_src_map")


# -------------------------------------------------
# Incidence rates
# -------------------------------------------------

mfa <- subset(e.t, year==thisyear-1, select=c('country', 'iso3', 'e_inc_100k'))

mfa$cat <- cut(round(mfa$e_inc_100k), c(0,10,20,50,125,300,500,Inf), c('0–9.9', '10–19', '20–49', '50–124', '125–299', '300–499', '>=300'), right=FALSE)

# map
mfc <- WHOmap.print(mfa, paste("Estimated TB incidence rates,", thisyear-1), "Estimated new TB \ncases (all forms) per \n100 000 population per year", na.label="No estimate", copyright=FALSE, colors=c('red', 'blue', 'orange', 'green', 'purple', 'violet', 'sienna'), show=FALSE)
dev.off()

figsave(mfc, mfa, "inc_map")

# -------------------------------------------------
# Mortality rates
# -------------------------------------------------

mia <- subset(e.t, year==thisyear-1, select=c('country', 'iso3', 'e_mort_exc_tbhiv_100k'))

mia$cat <- cut(mia$e_mort_exc_tbhiv_100k, c(0,1,4,10,20,40,Inf), c('0–0.9', '1–3.9', '4–9.9', '10–19', '20–39', '>=40'), right=FALSE)

# map
mic <- WHOmap.print(mia, paste("Estimated TB mortality rates,", thisyear-1), "Estimated TB \ndeaths per \n100 000 population", na.label="No estimate", copyright=FALSE, colors=c('red', 'blue', 'orange', 'green', 'purple', 'brown'), show=FALSE)
dev.off()

figsave(mic, mia, "mort_map")

# -------------------------------------------------
# HIV prevalence in new cases
# -------------------------------------------------

mga <- subset(e.t, year==thisyear-1, select=c('country', 'iso3', 'e_tbhiv_prct'))


mga$cat <- cut(mga$e_tbhiv_prct, c(0,5,20,50,Inf), c('0–4', '5–19', '20–49', '>=50'), right=FALSE)

# map
mgc <- WHOmap.print(mga, paste("Estimated HIV prevalence in new TB cases,", thisyear-1), 'HIV prevalence \nin new TB cases, \nall ages (%)', na.label="No estimate", copyright=FALSE, colors=c('red', 'blue', 'orange', 'green'))
#, show=FALSE)
dev.off()

figsave(mgc, mga, "hivprev_map")


# -------------------------------------------------
# Electronic recording and reporting
# -------------------------------------------------

mha <- subset(s, year %in% (thisyear-1):(thisyear-2), select=c('country', 'year', 'iso3', 'caseb_err_nat'))

# Take last year's answer if unreported
for(cnty in unique(mha$country)) {
  if(is.na(mha[mha$country==cnty & mha$year==thisyear-1, 'caseb_err_nat'])) mha[mha$country==cnty & mha$year==thisyear-1, 'caseb_err_nat'] <- mha[mha$country==cnty & mha$year==thisyear-2, 'caseb_err_nat']
}

mhb <- subset(mha, year==thisyear-1)

# Western europe fixes
if(thisyear==2013){
  mhb[mhb$iso3 %in% c('AUT', 'CHE', 'CYP', 'DNK', 'GRC', 'GRL', 'ITA', 'LUX', 'UZB', 'KOR'), 'caseb_err_nat'] <- 42
  mhb[mhb$iso3 %in% c('KGZ'), 'caseb_err_nat'] <- 0
  mhb[mhb$iso3 %in% c('NPL'), 'caseb_err_nat'] <- 43
} 


mhb$cat <- factor(mhb$caseb_err_nat, levels=c(42, 43, 0), labels=c('All TB patients', 'MDR-TB patients only', 'None' ))

# map
mhc <- WHOmap.print(mhb, paste("Availability of national electronic case-based databases of TB patients,", thisyear-1), '', colors=c('dark green', 'light green', 'white'), copyright=FALSE)
#, show=FALSE)
dev.off()

figsave(mhc, mhb, "err_map")

# A map for the back of the 2013 report

mia <- subset(e, year %in% c(2012))

mib <- subset(e, year %in% c(1990))
mib$cat <- factor('blank')

mic <- WHOmap.print(mib, paste("Mortality", thisyear-1), '', colors=c('white'), copyright=FALSE)
#, show=FALSE)

library(whomap)
source("D:/Users/hiattt/Dropbox/Code/Maps/Global map_post2011.r")
toplot9 <- merge(centres, mib, by.x = "id", by.y = "iso3", all.x=TRUE) 
# toplot9 <- toplot9[order(toplot9$order), ]

toplot2 <- merge(centres, mia, by.x = "id", by.y = "iso3", all.x=TRUE) 
toplot2 <- toplot2[order(toplot2$order), ]

mic + geom_point(aes(long, lat, size=(e_mort_exc_tbhiv_num)), color='blue', fill='red', toplot9) + geom_point(aes(long, lat, size=(e_mort_exc_tbhiv_num), color='red', fill=NA), toplot2)

toplot2 <- toplot2[order(toplot2$e_mort_exc_tbhiv_num, decreasing=TRUE),]
ggplot(toplot2, aes(long, lat, size=(e_mort_exc_tbhiv_num), color=rnorm(nrow(toplot2)))) + geom_point() + theme(line=element_blank()) + scale_size(range=c(1,75))



dev.off()


# ======================
