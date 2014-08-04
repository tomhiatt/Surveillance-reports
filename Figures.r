# -------------------------------------------------
# Figures in the global report
# Tom Hiatt
# 10 July 2012, revised 23 June 2014
# -------------------------------------------------


# HT: run the Setup.r manually first  ...
# source('D:/TMEData/TomsCode/Global TB control Reports/Tables and figures/Setup.r')


# Link -------------------------------------------------
# Link GTBR2014 figure numbers to code sections
# 
# 4.5 Treatment outcomes for new and relapse cases: 4_5_txsucc
# (box 4.5): Outcomes of TB treatment by HIV status: B4_5_hiv_ts_d
# 
# 
# 7.1 Percentage of TB patients with known HIV status: 7_1_hivtest_graph
#
# 7.3 Number of HIV-positive TB patients enrolled on CPT and ART: 7_3_cpt_art_hiv_graph
# 7.4 % TB patients HIV+, and % HIV+ TB patients on CPT and ART: 7_4_hivprog_graph_all
#
# 7.7 IPT for PLHIV without active TB : 7_7_hiv_ipt_graph
# -------------------------------------------------






#flag for whether or not to produce estimates figures
flg_show_estimates <- FALSE



# Bits I'm going to implement:
#   - Always show 0 (and not 0.0)
#   - Always expand for a comfy fit
#   - Labels be in the color of the lines where applicable
#   - Titles left justified
#   - label range always includes all data
#   - All use same color palette

if(flg_show_estimates){
  
  # Global rates of incidence, prevalence and mortality - est_global
  # fig_global ----------------------------------------------------
  
  eza <- subset(araw.t, group_type=='global')
  eza$forecast <- ifelse(eza$year >= thisyear, "forecast", "current")
  
  p1 <- qplot(year, e_inc_100k, data=eza, geom='line', colour=I('#00FF33')) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('#00FF33'), alpha=0.4) +
    #   geom_line(aes(year, newrel_100k)) + 
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I('red')) +      
    geom_ribbon(aes(year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi), fill=I('red'), alpha=0.4) +
    #   facet_wrap(~g_whoregion, scales='free_y') +
    scale_x_continuous('', breaks=c(seq(1990, 2005, 5), thisyear-1)) + ylab('Rate per 100 000 population') +
    expand_limits(y=c(0, max(pretty(c(eza$e_inc_100k_hi, max(eza$e_inc_100k_hi) * (1.20)))))) + theme_glb.rpt() + theme(legend.position='none', plot.title = element_text(hjust = 0)) + ggtitle('Incidence')
  
  p2 <- qplot(year, e_mort_exc_tbhiv_100k, data=eza, geom='line', colour=I('blue'), linetype=forecast) +
    geom_ribbon(aes(year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi), fill=I('blue'), alpha=0.4) +
    geom_hline(aes(yintercept=eza[eza$year==1990, "e_mort_exc_tbhiv_100k"] / 2), linetype=2) +
    ylab('') + xlab('') +
    expand_limits(y=0) +
    theme_glb.rpt() +
    theme(legend.position='none') + ggtitle('Mortality')
  
  p3 <- qplot(year, e_prev_100k, data=eza, geom='line', colour=I('blue'), linetype=forecast) +
    geom_ribbon(aes(year, ymin=e_prev_100k_lo, ymax=e_prev_100k_hi), fill=I('blue'), alpha=0.4) +
    geom_hline(aes(yintercept=eza[eza$year==1990, "e_prev_100k"] / 2), linetype=2) +
    ylab('') + xlab('') +
    expand_limits(y=0) +
    theme_glb.rpt() +
    theme(legend.position='none') + ggtitle('Prevalence')
  
  pdf(width=8, height=4, file='Figs/fig_global.pdf', 
      title='Global trends in case notification rates and estimated rates of incidence, morality and prevalence.') 
  
  ## next commented out by Hazim 1 July 2014 because grid.arrange was not recognised
  #grid.arrange(p1, p2, p3, nrow=1)
  dev.off()
  
  write.csv(merge(global[c('year', 'inc', 'inc.lo', 'inc.hi', 'inc.h', 'inc.h.lo', 'inc.h.hi', 'newrel')], 
                  global.ff[c('year', 'mort.nh', 'mort.nh.lo', 'mort.nh.hi', 'prev', 'prev.lo', 'prev.hi')], all=T), 
            file=paste(outfolder, "/FigData/", "fig_global", ".csv", sep=""), row.names=F, na="")
  
  # Global rates of incidence, and notifications
  # inc_notif_glo ----------------------------------------------------
  
  eha <- subset(araw.t, group_type=="global")
  
  ehb <- merge(subset(eha, select=c("group_name", "year", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_pop_num")), aggregate(n.t['c_newinc'], by=list(year=n.t$year), FUN=sum, na.rm=TRUE))
  
  ehb$newrel_100k <- ehb$c_newinc / ehb$e_pop_num * 100000
  
  ehc <- qplot(year, e_inc_100k, data=ehb, geom='line', colour=I('#00FF33')) + geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('#00FF33'), alpha=0.4) + geom_line(aes(year, newrel_100k)) + scale_x_continuous('', breaks=c(seq(1990, 2005, 5), thisyear-1)) + ylab('Rate per 100 000 population per year') + expand_limits(y=c(0, max(pretty(c(ehb$e_inc_100k_hi, max(ehb$e_inc_100k_hi) * (1.20)))))) + theme_glb.rpt() + ggtitle(paste('Global trends in case notification (black) and estimated TB \nincidence (green) rates, 1990-', thisyear-1, sep="")) 
  
  figsave(ehc, ehb, "inc_notif_glo", width=6, height=6)
  
  # Incidence only
  # inc_glo -----------------------------------------------------
  
  ehd <- qplot(year, e_inc_100k, data=ehb, geom='line', colour=I('#00FF33')) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('#00FF33'), alpha=0.4) +
    #   geom_line(aes(year, newrel_100k)) + 
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I('red')) +
    geom_ribbon(aes(year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi), 
                fill=I('red'), alpha=0.4) +
    #   facet_wrap(~g_whoregion, scales='free_y') +
    scale_x_continuous('', breaks=c(seq(1990, 2005, 5), thisyear-1)) + ylab('Rate per 100 000 population per year') +
    expand_limits(y=c(0, max(pretty(c(ehb$e_inc_100k_hi, max(ehb$e_inc_100k_hi) * (1.20)))))) + theme_glb.rpt() + 
    ggtitle(paste('Global trends in case notification (black) and estimated TB \nincidence (green) rates, 1990-', thisyear-1, '.', sep="")) 
  
  figsave(ehd, ehb, "inc_glo", width=7)
  
  
  # Regional rates of incidence, and notifications
  # inc_notif_reg ----------------------------------------------------
  
  efa1 <- subset(araw.t, group_type=="g_whoregion" & year < thisyear)
  
  # names(regional) <- gsub ('_', '\\.', names (regional))
  efa1$g_whoregion <- factor(efa1$group_name, labels=c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific"))
  
  efa <- merge(subset(efa1, select=c("group_name", "g_whoregion", "year", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_pop_num")), aggregate(n.t['c_newinc'], by=list(group_name=n.t$g_whoregion, year=n.t$year), FUN=sum, na.rm=TRUE))
  
  efa$newrel_100k <- efa$c_newinc / efa$e_pop_num * 100000
  
  # a fudging to get values on top
  topper <- function(dat){
    dat$top <- max(pretty(c(dat$e_inc_100k_hi, max(dat$e_inc_100k_hi) * (1.1))))
    return(dat)
  }
  efc <- ddply(efa, "g_whoregion", topper)
  
  efb <- qplot(year, e_inc_100k, data=efc, geom='line', colour=I('#00FF33')) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('#00FF33'), alpha=0.4) +
    geom_line(aes(year, newrel_100k)) + 
    # geom_line(aes(year, inc.h), colour=I('red')) +      
    # geom_ribbon(aes(year, ymin=inc.h.lo, ymax=inc.h.hi), 
    # fill=I('red'), alpha=0.4) +
    facet_wrap(~g_whoregion, scales='free_y') +
    scale_x_continuous('', breaks=c(seq(1990, 2005, 5), thisyear-1)) + ylab('Rate per 100 000 population per year') +
    expand_limits(y=0) + geom_point(aes(year, top), alpha=0) +
    theme_glb.rpt() + 
    ggtitle(paste('Case notification and estimated TB incidence rates by WHO region, 1990-', thisyear-1, '.', sep="")) 
  
  figsave(efb, efa, "inc_notif_reg")
  
  # Incidence only
  # inc_reg ------------------------------------------------------
  
  efd <- qplot(year, e_inc_100k, data=efc, geom='line', colour=I('green')) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('green'), alpha=0.4) +
    #   geom_line(aes(year, newrel_100k)) + 
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I('red')) +
    geom_ribbon(aes(year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi), 
                fill=I('red'), alpha=0.4) +
    facet_wrap(~g_whoregion, scales='free_y') +
    scale_x_continuous('', breaks=c(seq(1990, 2005, 5), thisyear-1)) + ylab('Rate per 100 000 population per year') +
    expand_limits(y=0) + geom_point(aes(year, top), alpha=0) +
    theme_glb.rpt() + 
    ggtitle(paste('Estimated TB incidence rates by WHO region, 1990-', thisyear-1, '.', sep="")) 
  
  figsave(efd, efa, "inc_reg")
  
  # HBC rates of incidence, and notifications
  # inc_notif_hbc ----------------------------------------------------
  
  ega <- subset(merge(eraw.t, e.t[e.t$year==thisyear-1,c("country", "g_hbc22")]), g_hbc22=="high" & year < thisyear) # This hack is until we add g_hbc22 to eraw.
  
  egb <- .shortnames(merge(subset(ega, select=c("country", "g_whoregion", "year", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_pop_num")), subset(n.t, select=c('country', 'year', 'c_newinc'))), ord='multiyear')
  
  egb$newrel_100k <- egb$c_newinc / egb$e_pop_num * 100000
  
  # a fudging to get values on top
  # topper <- function(dat){
  #   dat$top <- max(pretty(c(dat$e_inc_100k_hi, max(dat$e_inc_100k_hi) * (1.1))))
  #   return(dat)
  # }
  # egc <- ddply(egb, "country", topper)
  
  egd <- qplot(year, e_inc_100k, data=egb, geom='line', colour=I('green')) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('green'), alpha=0.4) +
    geom_line(aes(year, newrel_100k)) + 
    facet_wrap(~country, scales='free_y') +
    scale_x_continuous('', breaks=c(seq(1990, 2005, 5), thisyear-1)) + ylab('Rate per 100 000 population per year') +
    expand_limits(y=0) + # geom_point(aes(year, top), alpha=0) +
    theme_glb.rpt() + 
    ggtitle(paste('Case notification and estimated TB incidence rates, 22 high-burden countries, 1990-', thisyear-1, '.', sep="")) 
  
  figsave(egd, egb, "inc_notif_hbc")
  
  # Incidence only
  # inc_hbc -----------------------------------------------------
  
  ege <- qplot(year, e_inc_100k, data=egb, geom='line', colour=I('green')) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('green'), alpha=0.4) +
    #   geom_line(aes(year, newrel_100k)) + 
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I('red')) +
    geom_ribbon(aes(year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi), 
                fill=I('red'), alpha=0.4) +
    facet_wrap(~country, scales='free_y') +
    scale_x_continuous('', breaks=c(seq(1990, 2005, 5), thisyear-1)) + ylab('Rate per 100 000 population per year') +
    expand_limits(y=0) + # geom_point(aes(year, top), alpha=0) +
    theme_glb.rpt() + 
    ggtitle(paste('Estimated TB incidence rates, 22 high-burden countries, 1990-', thisyear-1, '.', sep="")) 
  
  figsave(ege, egb, "inc_hbc")
  
  # Global rates of TB prevalence
  # prev_glo ----------------------------------------------------
  # This graph seems redundant and unnecessary.
  
  # epb <- subset(araw.t, group_type=="global", select=c("group_name", "year", "e_prev_100k", "e_prev_100k_lo", "e_prev_100k_hi"))
  # 
  # epc <- qplot(year, e_prev_100k, data=epb, geom='line', colour=I('orange')) +
  #   geom_ribbon(aes(year, ymin=e_prev_100k_lo, ymax=e_prev_100k_hi), fill=I('orange'), alpha=0.4) + scale_x_continuous('') + ylab('Rate per 100 000 population') + theme_glb.rpt() + 
  #   ggtitle(glue('Global trends in estimated TB prevalence rates, 1990-', thisyear-1, " \nand forecast TB prevalence rates ", thisyear, "-2015, by WHO region.")) 
  # 
  # figsave(epc, epb, "prev_glo", width=6, height=6)
  
  # Regional rates of TB prevalence and mortality
  # prev_reg ----------------------------------------------------
  # This currently doesn't have all the components, but it doesn't error. Same with Mort below.
  
  regional <- subset(araw.t, group_type=="g_whoregion")
  regional$forecast <- ifelse(regional$year >= thisyear, "forecast", "current")
  
  regional$g_whoregion <- factor(regional$group_name, labels=c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific"))
  
  efb <- ggplot(regional, aes(year, e_prev_100k, linetype=forecast)) + geom_line(colour=I('blue')) +
    geom_ribbon(aes(year, ymin=e_prev_100k_lo, ymax=e_prev_100k_hi), fill=I('blue'), alpha=0.4) + 
    #   geom_hline(data=subset(regional, year==1990, e_prev_100k), aes(yintercept=e_prev_100k/2), linetype=2) +
    facet_wrap(~g_whoregion, scales='free_y') +
    scale_x_continuous('', breaks=c(seq(1990, 2005, 5), thisyear-1)) + ylab('Rate per 100 000 population') +
    expand_limits(y=0) + 
    theme_glb.rpt() + theme(legend.position="none") +
    ggtitle(paste('Estimated prevalence rates by WHO region, 1990-', thisyear-1, '.', sep="")) 
  
  figsave(efb, regional, "prev_reg")
  
  # mort_reg ----------------------------------------------------
  
  efg <- ggplot(regional, aes(year, e_mort_exc_tbhiv_100k, linetype=forecast)) + geom_line(colour=I('orange')) +
    geom_ribbon(aes(year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi), fill=I('orange'), alpha=0.4) + 
    #   geom_hline(data=subset(regional, year==1990, e_mort_exc_tbhiv_100k), aes(yintercept=e_prev_100k/2), linetype=2) +
    facet_wrap(~g_whoregion, scales='free_y') +
    scale_x_continuous('', breaks=c(seq(1990, 2005, 5), thisyear-1)) + ylab('Rate per 100 000 population') +
    expand_limits(y=0) + 
    theme_glb.rpt() + theme(legend.position="none") +
    ggtitle(paste('Estimated mortality rates by WHO region, 1990-', thisyear-1, '.', sep="")) 
  
  figsave(efg, regional, "mort_reg")
  
  #----------------------------------------------------
  # Country and regional Profiles - Incidence, prevalence and mortality in 22 HBCs
  #----------------------------------------------------
  
  cpfd1 <- merge(n, eraw,)
  rpfd <- aggregate(cpfd1['c_newinc'], by=list(group_name=cpfd1$g_whoregion, year=cpfd1$year), FUN=sum, na.rm=TRUE)
  rpfd <- merge(rpfd, araw)
  cpfd1$group_name <- cpfd1$country
  cpfd <- merge(cpfd1, rpfd, all=T)
  
  # Incidence 
  
  hest <- subset(cpfd, g_hbc22=='high' | group_name %in% rpfd$group_name)
  levels(hest$group_name)[match('Democratic Republic of the Congo', levels(hest$group_name))] <- 'DR Congo'
  levels(hest$group_name)[match('United Republic of Tanzania', levels(hest$group_name))] <- 'UR Tanzania'
  hest$c_newinc_100k <- hest$c_newinc / hest$e_pop_num * 1e5
  
  pdf(width=14, height=9.5, file='CPFigs/hbc_cp_inc.pdf')
  # windows(14,9.5) windows(3, 0.87)
  qplot(year, e_inc_100k, data=hest, geom='line', colour=I('green')) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('green'), alpha=0.4) +
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I('red')) +
    geom_line(aes(year, c_newinc_100k)) + 
    geom_ribbon(aes(year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi), 
                fill=I('red'), alpha=0.4) +
    facet_wrap(~group_name, scales='free', ncol=4) +
    scale_y_continuous(name = "") +
    scale_x_continuous(name="", expand = c(0, 0)) + 
    expand_limits(y=0) +
    theme_glb.rpt(base_size=6) + theme(legend.position='none', panel.grid.minor = element_blank()) +  ggtitle('Incidence')
  dev.off()
  
  # Prevalence
  
  pdf(width=14, height=9.5, file='CPFigs/hbc_cp_prev.pdf')
  p1 <- qplot(year, e_prev_100k, data=hest, geom='line', colour=I('blue')) +
    geom_ribbon(aes(year, ymin=e_prev_100k_lo, ymax=e_prev_100k_hi), fill=I('blue'), alpha=0.4) +
    # geom_hline(aes(yintercept=target.prev), linetype=2) +
    facet_wrap(~group_name, scales='free', ncol=4) +
    scale_y_continuous(name = "") +
    scale_x_continuous(name="", expand = c(0, 0)) + 
    expand_limits(y=0) +
    theme_glb.rpt(base_size=6) +
    theme(legend.position='none', panel.grid.minor = element_blank()) + ggtitle('Prevalence')
  print(p1) 
  dev.off()
  
  # Mortality
  
  pdf(width=14, height=9.5, file='CPFigs/hbc_cp_mort.pdf')
  p2 <- qplot(year, e_mort_exc_tbhiv_100k, data=hest, geom='line', colour=I('orange')) +
    geom_ribbon(aes(year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi), fill=I('orange'), alpha=0.4) +
    # geom_hline(aes(yintercept=target.mort), linetype=2) +
    facet_wrap(~group_name, scales='free', ncol=4) +
    scale_y_continuous(name = "") +
    scale_x_continuous(name="", expand = c(0, 0)) + 
    expand_limits(y=0) +
    theme_glb.rpt(base_size=6) +
    theme(legend.position='none', panel.grid.minor = element_blank()) + ggtitle('Mortality')
  print(p2)
  dev.off()
}


# 7_1_hivtest_graph -------------------------------------------------------------------

gaa <- subset(tbhiv, year>=2004, select=c('g_whoregion', 'year', 'hivtest_pct_denominator', 'hivtest_pct_numerator'))

gaa$regional <- ifelse(gaa$g_whoregion=='AFR', 'African region', 'Regions outside \n  Africa')

gabr <- aggregate(gaa[c('hivtest_pct_denominator', 'hivtest_pct_numerator')], by=list(area=gaa$regional, year=gaa$year), FUN=sum, na.rm=TRUE)
gabg <- aggregate(gaa[c('hivtest_pct_denominator', 'hivtest_pct_numerator')], by=list(year=gaa$year), FUN=sum, na.rm=TRUE)
gabg$area <- 'Global'

gac <- rbind(gabr, gabg)

gac$hivtest_pct <- gac$hivtest_pct_numerator / gac$hivtest_pct_denominator * 100

gadstart <- 2004

gad <- ggplot(subset(gac, year >= gadstart), aes(year, hivtest_pct, colour=area)) +   geom_line(size=1.5) + geom_text(data=subset(gac, year==max(gac$year)), aes(label = area), hjust=-.1, vjust=0, size=5) + scale_y_continuous(name = "Percentage of notified TB patients", limits=c(0,100), expand=c(0,0)) + scale_x_continuous("", labels=gadstart:(thisyear-1), breaks=gadstart:(thisyear-1)) + scale_color_brewer(name="WHO region", palette="Dark2") + expand_limits(x=c(gadstart, thisyear+0.5)) + ggtitle(glue('Percentage of notified TB patients with known HIV status, ', gadstart, '-', thisyear-1)) + theme_glb.rpt() + theme(legend.position="none")

# windows(11, 7); gad; dev.off()
figsave(gad, gac, "7_1_hivtest_graph")


# 7_xxxx_hivtest_num -------------------------------------------------------------------

# Dropped from 2014 report

gja <- subset(tbhiv, year>=2004, select=c('country', 'g_whoregion', 'year', 'hivtest', 'hivtest_pos'))

# Some assumptions 
cat(glue('There are ', nrow(subset(gja, is.na(hivtest)&!is.na(hivtest_pos)&hivtest_pos!=0)), ' records that reported hivtest_pos, but didn\'t report hivtest. These come up to ', rounder(sum(subset(gja, is.na(hivtest)&!is.na(hivtest_pos)&hivtest_pos!=0, select='hivtest_pos'))), ' cases in hivtest_pos across all years (', rounder(sum(subset(gja, year==thisyear-1 & is.na(hivtest)&!is.na(hivtest_pos)&hivtest_pos!=0, select='hivtest_pos'))),' in ', thisyear-1,'). These cases are added to hivtest total.'))

cat(glue('There are ', nrow(subset(gja, is.na(hivtest_pos)&!is.na(hivtest)&hivtest!=0)), ' records that reported hivtest, but didn\'t report hivtest_pos. These come up to ', rounder(sum(subset(gja, is.na(hivtest_pos)&!is.na(hivtest)&hivtest!=0, select='hivtest'))), ' cases in hivtest across all years (', rounder(sum(subset(gja, year==thisyear-1 & is.na(hivtest_pos)&!is.na(hivtest)&hivtest!=0, select='hivtest'))),' in ', thisyear-1,'). There is no adjustment made for these. i.e. it appears as if all tested negative.'))


gja$hivtest2 <- ifelse(is.na(gja$hivtest), gja$hivtest_pos, gja$hivtest)
gja$neg <- gja$hivtest2 - gja$hivtest_pos
gja$pos <- gja$hivtest_pos

# gja$unk <- gja$hivtest2 - rowSums(cbind(gja$pos, gja$neg), na.rm=TRUE)

gjb <- aggregate(gja[4:ncol(gja)], by=list(year=gja$year), FUN=sum, na.rm=TRUE)

gje <- melt(gjb,id.vars=c('year'), measure.vars=c('pos', 'neg'))
gje$value2 <- gje$value/1000

gjf <- ggplot(gje, aes(factor(year), value2, fill=variable)) + geom_bar(width=0.6, stat='identity') + scale_y_continuous(name = "TB patients (thousands)", expand=c(0,0), limits=c(0,3100)) + scale_x_discrete("") + scale_fill_brewer(name="HIV status", palette="Dark2", breaks=rev(levels(gje$variable)), labels=c('Negative', 'Positive')) + theme_glb.rpt() + ggtitle(glue('Number of TB patients with known HIV status, ', min(gje$year), '-', max(gje$year))) + theme(legend.position=c(0.15, 0.75))

# windows(11, 7); gjf; dev.off()
figsave(gjf, gje, "7_xxxx_hivtest_num")

# 7_3_cpt_art_hiv_graph -------------------------------------------------------------------

gca <- subset(tbhiv, year>=2004, select=c('iso3', 'year', 'hivtest_pos', 'hiv_cpt', 'hiv_art'))

gcb <- aggregate(gca[3:ncol(gca)], by=list(year=gca$year), FUN=sum, na.rm=T)

gcc <- melt(gcb, id=1)
gcc$variable <- factor(gcc$variable, levels = c("hivtest_pos", "hiv_cpt", "hiv_art"), labels = c('HIV-positive','CPT','ART'))

gcc$value <- gcc$value/1000

gcd <- ggplot(gcc, aes(year, value, color=variable)) + geom_line(size=1) + scale_y_continuous("Number of TB patients (thousands)") + theme_glb.rpt() + scale_x_continuous(name="", breaks=2004:(thisyear-1)) +  scale_colour_brewer(name="Data provided", palette="Dark2") + geom_text(data=gcc[gcc$year==thisyear-2, ], aes(label=variable), vjust=3, hjust=1)  + ggtitle(paste("Number of notified HIV-positive TB patients enrolled on co-trimoxazole preventive therapy (CPT) \nand antiretroviral therapy (ART), 2004", thisyear-1, sep="-")) + expand_limits(y=c(min(pretty(c(gcc$value, min(gcc$value) * (0.95)))), max(pretty(c(gcc$value, max(gcc$value) * (1.05)))))) + theme(legend.position="none")

# windows (10,7); gcd; dev.off()
figsave(gcd, gcb, "7_3_cpt_art_hiv_graph")

# 
# # cpt_graph delete -------------------------------------------------------------------
# 
# gda <- subset(tbhiv, year>=2005, select=c('iso3', 'year', 'hiv_cpt_pct_numerator', 'hivtest_pos_pct_denominator'))
# 
# # countries with full time series
# gdb <- aggregate(gda[3:ncol(gda)], by=list(iso3=gda$iso3), FUN=sum, na.rm=F)
# gdb_full <- gdb[!is.na(gdb$hiv_cpt_pct_numerator), 'iso3'] # this is right (59)
# 
# # count reporting countries
# gdc <- gda[!is.na(gda$hiv_cpt_pct_numerator),]
# gdc$rep <- 1
# gdc_rep <- aggregate(gdc['rep'], by=list(year=gdc$year), FUN=sum) # Now this works, but why such inflation?
# 
# # Pick your plot. Only full time series or all
# # gdd <- gda[gda$iso3 %in% gdb_full, ] ; gdd_num <- paste(',', length(gdb_full), 'reporting countries')# Full time series only
# gdd <- gda ; gdd_num <- "" # All countries
# 
# gdd <- aggregate(gdd[3:ncol(gdd)], by=list(year=gdd$year), FUN=sum, na.rm=T)
# 
# gdd$pcnt <- gdd$hiv_cpt_pct_numerator / gdd$hivtest_pos_pct_denominator
# 
# gde <- ggplot(gdd, aes(year, pcnt)) + geom_line(size=1) + scale_y_continuous("Percentage of HIV-positive TB patients", limits = c(0, 1), expand = c(0, 0), labels=percent) + theme_glb.rpt() + scale_x_continuous(name="") + geom_text(data=gdd[gdd$year==thisyear-1, ], aes(label='CPT'), vjust=3, hjust=2) + theme(legend.position="none", panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), title=paste("Co-trimoxazole preventive therapy for HIV-positive TB patients, 2005-", thisyear-1, gdd_num, sep="")) 
# 
# # windows (10,7); gde; dev.off()
# figsave(gde, gdd, "cpt_graph")
# 
# 
# gdf <- ggplot(gdd, aes(year, pcnt)) + geom_line(size=1) +
#   scale_y_continuous("Percentage of HIV-positive TB patients", limits = c(0, 1), 
#                      expand = c(0, 0), labels=percent) + theme_glb.rpt() +
#                        scale_x_continuous(name="") +  
#                        geom_text(data=gdd[gdd$year==thisyear-1, ], aes(label='CPT'), vjust=3, hjust=2, size=6) +
#                        opts(legend.position="none", panel.grid.major = theme_blank(),
#                             panel.grid.minor = theme_blank(), 
#                             title=paste("Co-trimoxazole preventive therapy for HIV-positive 
#   TB patients, 2005-", thisyear-1, gdd_num, sep=""), 
# 	plot.title=theme_text(size=22), axis.title.y=theme_text(size=15, angle=90),
#                             axis.text.y=theme_text(size=15), axis.text.x=theme_text(size=15)) 
# 
# # windows (10,7); gdf; dev.off()
# 
# ggsave(file = paste(outfolder, "/Slides/", "cpt_graph", ".wmf", sep=""), gdf, width=10, height=7) 
# 
# write.csv(gdd, file=paste(outfolder, "/FigData/", "cpt_graph", Sys.Date(), ".csv", sep=""), row.names=F, na="")
# 
# #-------------------------------------------------------------------
# # art_graph delete
# #-------------------------------------------------------------------
# 
# 
# gde <- subset(tbhiv, year>=2005, select=c('iso3', 'year', 'hiv_art_pct_numerator', 'hiv_art_pct_denominator'))
# 
# # countries with full time series
# gdf <- aggregate(gde[3:ncol(gde)], by=list(iso3=gde$iso3), FUN=sum, na.rm=F)
# gdf_full <- gdf[!is.na(gdf$hiv_art_pct_numerator), 'iso3'] # this is right (59)
# 
# # count reporting countries
# gdg <- gde[!is.na(gde$hiv_art_pct_numerator),]
# gdg$rep <- 1
# gdg_rep <- aggregate(gdg['rep'], by=list(year=gdg$year), FUN=sum) # Now this works, but why such inflation?
# 
# # Pick your plot. Only full time series or all
# # gdh <- gde[gde$iso3 %in% gdf_full, ] ; gdh_num <- paste(',', length(gdf_full), 'reporting countries')# Full time series only
# gdh <- gde ; gdh_num <- "" # All countries
# 
# gdh <- aggregate(gdh[3:ncol(gdh)], by=list(year=gdh$year), FUN=sum, na.rm=T)
# 
# gdh$pcnt <- gdh$hiv_art_pct_numerator / gdh$hiv_art_pct_denominator
# 
# gdi <- ggplot(gdh, aes(year, pcnt)) + geom_line(size=1) +
#   scale_y_continuous("Percentage of HIV-positive TB patients", limits = c(0, 1), 
#                      expand = c(0, 0), labels=percent) + theme_glb.rpt() +
#                        scale_x_continuous(name="") +  
#                        geom_text(data=gdh[gdh$year==thisyear-1, ], aes(label='ART'), vjust=3, hjust=2) +
#                        opts(legend.position="none", # panel.grid.major = theme_blank(),
#                             panel.grid.minor = theme_blank(), 
#                             title=paste("Antiretroviral therapy for HIV-positive TB patients, 2005-", thisyear-1, gdd_num, sep="")) 
# 
# # windows (10,7); gdi; dev.off()
# figsave(gdi, gdh, "art_graph")
# 
# 
# 
# 
# gdj <- ggplot(gdh, aes(year, pcnt)) + geom_line(size=1) +
#   scale_y_continuous("Percentage of HIV-positive TB patients", limits = c(0, 1), 
#                      expand = c(0, 0), labels=percent) + theme_glb.rpt() +
#                        scale_x_continuous(name="") +  
#                        geom_text(data=gdh[gdh$year==thisyear-1, ], aes(label='ART'), vjust=3, hjust=2, size=6) +
#                        opts(legend.position="none", panel.grid.major = theme_blank(),
#                             panel.grid.minor = theme_blank(), 
#                             title=paste("Antiretroviral therapy for HIV-positive TB patients, 
# 	2005-", thisyear-1, gdd_num, sep=""), 
# 	plot.title=theme_text(size=22), axis.title.y=theme_text(size=15, angle=90),
#                             axis.text.y=theme_text(size=15), axis.text.x=theme_text(size=15)) 
# 
# # windows (10,7); gdj; dev.off()
# ggsave(file = paste(outfolder, "/Slides/", "art_graph", ".wmf", sep=""), gdj, width=10, height=7) 
# 
# write.csv(gdh, file=paste(outfolder, "/FigData/", "art_graph", Sys.Date(), ".csv", sep=""), row.names=F, na="")



# 7_xxxx_tbscr_graph -------------------------------------------------------------------

# Dropped from 2014 global report!

gea <- merge(subset(n, year>=2005, select=c('iso3', 'year', 'hiv_tbscr')), subset(p, select=c('iso3', 'year', 'e_pop_num')))

gea$area <- ifelse(gea$iso3 %in% c("IND"), "India", "Rest of the world")

geb <- aggregate(gea['hiv_tbscr'], by=list(year=gea$year, area=gea$area), FUN=sum, na.rm=T)


geb$tbscr2 <- geb$hiv_tbscr / 1000000

fmt <- function(){
  function(x) ifelse(x==0, 0, format(x,nsmall = 1,scientific = FALSE))
}
geb$area <- factor(geb$area, levels=c("Rest of the world", "India" ))

ged <- ggplot(geb, aes(year, tbscr2, fill=area)) + geom_area() + scale_y_continuous("Number of people screened (millions)") + theme_glb.rpt() + aes(ymin=0) + scale_x_continuous(name="") + ggtitle(paste("Intensified TB case-finding among people living with HIV, 2005", thisyear-1, sep="-")) + scale_fill_brewer("" )


# windows (10,7); ged; dev.off()
figsave(ged, geb, "7_xxxx_tbscr_graph")



# ggsave(file = paste(outfolder, "/Figs/", "tbscr_graph", Sys.Date(), ".pdf", sep=""), ged, width=10, height=7) 
# 
# gee <- ggplot(geb, aes(year, value)) + geom_line(size=1) +
#   scale_y_continuous("Number of people screened (millions)", limits = c(0, 2.5), expand = c(0, 0)) + theme_glb.rpt() +
#   scale_x_continuous(name="") +  scale_color_brewer(name="Data provided", palette="Dark2") +
#   # scale_fill_manual(values=c("grey50", "blue")) + 
#   # geom_text(data=gec[gec$year==thisyear-1, ], aes(label=c('Tested HIV-positive','CPT','ART')), 
#   # vjust=3, hjust=2, colour="black", size=6) +
#   opts(legend.position="none", panel.grid.major = theme_blank(),
#        panel.grid.minor = theme_blank(), 
#        title=paste("Intensified TB case-finding among people living with HIV, 
# 	2005", thisyear-1, sep="-"), 
# 	plot.title=theme_text(size=22), axis.title.y=theme_text(size=15, angle=90),
#        axis.text.y=theme_text(size=15), axis.text.x=theme_text(size=15)) 
# 
# # windows (10,7); gee; dev.off()
# ggsave(file = paste(outfolder, "/Slides/", "tbscr_graph", ".wmf", sep=""), gee, width=10, height=7) 
# 
# write.csv(geb, file=paste(outfolder, "/FigData/", "tbscr_graph", Sys.Date(), ".csv", sep=""), row.names=F, na="")

# 7_7_hiv_ipt_graph -------------------------------------------------------------------

gfa <- subset(n, year>=2005, select=c('iso3', "g_whoregion", 'year', 'hiv_ipt'))

gfa$area <- ifelse(gfa$iso3 %in% c("ZAF"), "1.South Africa", ifelse(gfa$g_whoregion=="AFR", "2.Rest of AFR", "3.Rest of the world"))


gfb <- aggregate(gfa[4], by=list(year=gfa$year, area=gfa$area), FUN=sum, na.rm=T)

gfc <- melt(gfb, id=1:2)

gfc$value <- gfc$value/1000

gfc$area <- factor(gfc$area, levels=c("1.South Africa", "2.Rest of AFR", "3.Rest of the world" ))

gfd <- 
  ggplot(gfc, aes(year, value, fill=area)) + geom_bar(width=0.7, stat='identity') + scale_y_continuous("Number of HIV-positive people without active TB (thousands)") + theme_glb.rpt() + scale_x_continuous(name="", breaks=c(min(gfc$year):max(gfc$year))) +  scale_fill_brewer(name="Data provided", palette="Dark2") + 
  guides(fill = guide_legend(reverse = TRUE)) + ggtitle(paste("Provision of isoniazid preventive therapy (IPT) to people living with HIV without active TB, 2005", thisyear-1, sep="-")) 

# windows (10,7); gfd; dev.off()
figsave(gfd, gfb, "7_7_hiv_ipt_graph")




# gfe <- ggplot(gfc, aes(year, value)) + geom_line(size=1) +
#   scale_y_continuous("Number of HIV-positive people without active TB (thousands)", 
#                      limits = c(0, 420), expand = c(0, 0)) + theme_glb.rpt() +
#                        scale_x_continuous(name="") +  scale_color_brewer(name="Data provided", palette="Dark2") +
#                        # scale_fill_manual(values=c("grey50", "blue")) + 
#                        # geom_text(data=gec[gec$year==thisyear-1, ], aes(label=c('Tested HIV-positive','CPT','ART')), 
#                        # vjust=3, hjust=2, colour="black", size=6) +
#                        opts(legend.position="none", panel.grid.major = theme_blank(),
#                             panel.grid.minor = theme_blank(), 
#                             title=paste("IPT provision among HIV-positive people, 2005", thisyear-1, sep="-"), 
#                             plot.title=theme_text(size=22), axis.title.y=theme_text(size=15, angle=90),
#                             axis.text.y=theme_text(size=15), axis.text.x=theme_text(size=15)) 
# 
# # windows (10,7); gfe; dev.off()
# ggsave(file = paste(outfolder, "/Slides/", "hiv_ipt_graph", ".wmf", sep=""), gfe, width=10, height=7) 
# 
# write.csv(gfb, file=paste(outfolder, "/FigData/", "hiv_ipt_graph", Sys.Date(), ".csv", sep=""), row.names=F, na="")


# TB/HIV graphs for showing burden distribution. 
# hivdist_graph -------------------------------------------------------------------

gna <- subset(tbhiv, year>=2003, select=c("country", "year", "g_whoregion", "hiv_art"))
gna <- .shortnames(gna)

highs <- unique(gna[gna$year==thisyear-1 & gna$hiv_art>20000 & !is.na(gna$hiv_art),"country"])

gna$group <- 'All other countries'
gna$group <- ifelse(gna$g_whoregion=='AFR', 'AFR other countries', gna$group)
gna$group <- ifelse(gna$country %in% highs, gna$country, gna$group)


gnb <- aggregate(gna['hiv_art'], by=list(year=gna$year, group=gna$group), FUN=sum, na.rm=T)
gnb$hiv_art_1k <- gnb$hiv_art / 1000


gnc <- ggplot(gnb, aes(year, hiv_art_1k, color=group)) + geom_line(size=1.5, alpha=.5) + geom_text(data=gnb[gnb$year==thisyear-1,], aes(label = group, color=group), hjust=-.1, vjust=0, size=3) + scale_y_continuous(name = "Patients (thousands)", expand=c(0.1, 0)) + scale_x_continuous("", breaks=2003:(thisyear-1)) + scale_color_brewer(palette="Dark2") + theme_glb.rpt() + expand_limits(x=c(2003, thisyear + 0.1)) + theme(legend.position="none") + ggtitle('Antiretroviral therapy for HIV-positive TB patients by WHO region and selected countries, 2003-2010')

# windows(10, 7); gnc; dev.off()
figsave(gnc, gnb, "hivdist_graph")


# gnd <- ggplot(gnb[gnb$group %in% highs,], aes(year, hiv_art_1k, group=group)) + 
#   geom_line(size=1.5, color="light blue") + geom_text(data=gnb[gnb$group %in% highs&gnb$year==thisyear-1,],
#                                                       aes(label = group), hjust=-.1, vjust=0, size=5) +
#                                                         geom_line(data=gnb[!gnb$group %in% highs,], aes(year, hiv_art_1k, color=group), size=1.5, alpha=.5) +
#                                                         scale_y_continuous(name = "Patients (thousands)") +
#                                                         scale_x_continuous("") + scale_color_brewer(name="WHO region", palette="Dark2") +
#                                                         expand_limits(x=c(2003, 2011.5)) + theme_glb.rpt() +
#                                                         opts(title='Antiretroviral therapy for HIV-positive TB patients by WHO region and selected countries, 2003-2010', 
#                                                              plot.title=theme_text(size=22), axis.title.y=theme_text(size=15, angle=90),
#                                                              axis.text.y=theme_text(size=15), axis.text.x=theme_text(size=15), 
#                                                              legend.title=theme_text(size=14, hjust=0), legend.text=theme_text(size=12))
# 
# # windows(11, 7); gnd; dev.off()
# ggsave(paste(outfolder, "/Slides/", "hivdist_graph", ".wmf", sep=""), gnd, width=11, height=7) 
# 
# write.csv(gnb, file=paste(outfolder, "/FigData/", "hivdist_graph", Sys.Date(), ".csv", sep=""), row.names=F, na="")

# 7_4_hivprog_graph_all -------------------------------------------------------------------

gg <- subset(tbhiv, year>=2003, select=c('iso3', 'year', 'g_hbhiv63', 'hivtest', 'hivtest_pos', 'hiv_cpt', 'hiv_art', 'hiv_cpt_pct_numerator', 'hiv_cpt_pct_denominator', 'hiv_art_pct_numerator', 'hiv_art_pct_denominator', 'c_notified', 'hivtest_pos_pct_denominator', 'hivtest_pos_pct_numerator'))

# replace denominators with interpolated rates across years

gga <- merge(e.t[c('iso3', 'year', 'e_pop_num')], gg, all.y=T)

ggb <- melt(gga, id=1:4)

ggb$rate <- ggb$value / ggb$e_pop_num

ghe <- gga

for(var in c('c_notified', 'hivtest', 'hivtest_pos')) {
  
  gha <- cast(ggb[ggb$variable==var & !is.na(ggb$rate),c('iso3', 'year', 'rate')], year~iso3, value='rate')
  ghb <- timeSeries(as.matrix(as.matrix.cast_df(gha)))
  ghc <- na.omit(ghb, method="ie")
  ghc$year <- 2003:(thisyear-1)
  ghd <- melt(as.data.frame(ghc), id='year', variable='iso3')
  names(ghd)[3] <- paste(var, "ir", sep="_")
  ghe <- merge(ghe, ghd, all.x=T)
}

ghe$c_notified_m <- ghe$c_notified_ir * ghe$e_pop_num
ghe$hivtest_m <- ghe$hivtest_ir * ghe$e_pop_num
ghe$hivtest_pos_m <- ghe$hivtest_pos_ir * ghe$e_pop_num

ggf <- ghe

gl <- within(ggf, {
  c_notified_hnm <- ifelse(!is.na(c_notified) & !is.na(hivtest) & 
                             c_notified >= hivtest, c_notified, NA)
  hivtest_hnm <- ifelse(!is.na(c_notified) & !is.na(hivtest) & 
                          c_notified >= hivtest, hivtest, NA)
  
  hivtest_lo <- ifelse(!is.na(c_notified_m) & is.na(hivtest), 0, hivtest)
  hivtest_hi <- ifelse(!is.na(c_notified_m) & is.na(hivtest), c_notified_m, hivtest)
  
  hivtest_pos_lo <- ifelse(!is.na(hivtest_m) & is.na(hivtest_pos), 0, hivtest_pos)
  hivtest_pos_hi <- ifelse(!is.na(hivtest_m) & is.na(hivtest_pos), hivtest_m, hivtest_pos)
  
  hiv_cpt_lo <- ifelse(!is.na(hivtest_pos_m) & is.na(hiv_cpt), 0, hiv_cpt)
  hiv_cpt_hi <- ifelse(!is.na(hivtest_pos_m) & is.na(hiv_cpt), hivtest_pos_m, hiv_cpt)
  
  hiv_art_lo <- ifelse(!is.na(hivtest_pos_m) & is.na(hiv_art), 0, hiv_art)
  hiv_art_hi <- ifelse(!is.na(hivtest_pos_m) & is.na(hiv_art), hivtest_pos_m, hiv_art)
  
})

table(gl[!is.na(gl$c_notified_m),'year']) # 214 / 216 countries have reported in at least 1 year
table(gl[!is.na(gl$hivtest_m),'year']) 		# 203 / 216 countries
table(gl[!is.na(gl$hivtest_pos_m),'year'])	# 202 / 216 countries (somehow I lost a country)

unique(e.t[!e.t$iso3  %in% unique(gl[!is.na(gl$c_notified_m),'iso3']),'country'])
unique(e.t[!e.t$iso3  %in% unique(gl[!is.na(gl$hivtest_m),'iso3']),'country'])
unique(e.t[!e.t$iso3  %in% unique(gl[!is.na(gl$hivtest_pos_m),'iso3']),'country'])
# table(tbhiv[!is.na(tbhiv$hivtest),'year'])

gk <- aggregate(gl[6:ncol(gl)], by=list(year=gl$year), FUN=sum, na.rm=T)

gah <- within(gk, {
  pht_best <- hivtest_hnm / c_notified_hnm
  pht_lo <- hivtest_lo / c_notified_m
  pht_hi <- hivtest_hi / c_notified_m
  
  phtp_best <- hivtest_pos_pct_numerator / hivtest_pos_pct_denominator
  phtp_lo <- hivtest_pos_lo / hivtest_m
  phtp_hi <- hivtest_pos_hi / hivtest_m
  
  pcpt_best <- hiv_cpt_pct_numerator / hiv_cpt_pct_denominator
  pcpt_lo <- hiv_cpt_lo / hivtest_pos_m
  pcpt_hi <- hiv_cpt_hi / hivtest_pos_m
  
  part_best <- hiv_art_pct_numerator / hiv_art_pct_denominator
  part_lo <- hiv_art_lo / hivtest_pos_m
  part_hi <- hiv_art_hi / hivtest_pos_m
})

gai <- melt(gah[c('year', "part_hi", "part_lo", "part_best", "pcpt_hi", "pcpt_lo", "pcpt_best", "phtp_hi", "phtp_lo", "phtp_best", "pht_hi", "pht_lo", "pht_best")], id=1)

for(ro in 1:nrow(gai)){
  
  both <- as.data.frame(str_split(gai[ro,'variable'], "_"))
  gai[ro, 'var'] <- both[1,]
  gai[ro, 'hilo'] <- both[2,]
  
}

gaj <- cast(gai, year+var~hilo)

gaj['Percentages'] <- factor(gaj$var, levels=c("pht", "phtp", "pcpt", "part"), labels=c('% of notified TB patients with known HIV status', '% of notified TB patients with known HIV status \nwho were HIV-positive', '% of notified HIV-positive TB patients\n started on CPT', '% of notified HIV-positive TB patients\nstarted on ART'))

gak <- gaj[gaj$year>=2007 & gaj$Percentages!='% of notified TB patients with known HIV status',] 

gak[c("best", "hi", "lo")] <- gak[c("best", "hi", "lo")] * 100

gal <- ggplot(gak, aes(year, best)) + geom_line(size=1, alpha=.5) + geom_ribbon(aes (year, best, ymin=lo, ymax=hi), alpha=0.2) + facet_wrap(~Percentages, ncol=3) + scale_y_continuous(limits = c(0, 100), name = "Percentage of notified TB patients", breaks=c(0, 20, 40, 60, 80, 100)) + scale_x_continuous("") + theme_glb.rpt()  + ggtitle(paste0('Percentage of notified TB patients with known HIV status who were HIV positive, and percentage of notified \nHIV-positive TB patients enrolled on co-trimoxazole preventive therapy (CPT) and antiretroviral therapy (ART),\n ', min(gak$year), '-', max(gak$year), '(a)'))

# windows(13,6); gal
figsave(gal, gak, "7_4_hivprog_graph_all")




# Title: Proportion of TB patients tested for HIV, tested patients found positive and 
# HIV-positive TB patients on CPT and ART, 2003-2010.
# Footnote: Lines indicate percentage from countries who have reported both the 
# numerator and denominator for the percentage. Ranges indicate 0% and 100% for 
# countries reporting the denominator only. Countries reporting neither numerator
# nor denominator for the time series are excluded and account for <1% of the estimated
# Global TB/HIV burden. Missing denominator data were estimated using interpolation and
# continuation of rates outside of the range of reported data.

# Intervals indicate scenarios where non-reporting countries report 0% or 100%.

# 4_5_txsucc -------------------------------------------------------------------

ha1 <- subset(tb, year==thisyear-2, select=c('country', 'g_whoregion', 'g_hbc22', "rel_with_new_flg", "newrel_coh", "newrel_succ", "newrel_fail", "newrel_died", "newrel_lost", "c_newrel_neval"))

# Aggregate and reassemple

haa <- glb.rpt.table(ha1, 5:ncol(ha1))

ha2 <- .shortnames(subset(ha1, rel_with_new_flg==0 & g_hbc22=="high", country))
haa$area <- ifelse(haa$area %in% ha2$country, paste0(haa$area, "*"), haa$area)

haa$area <- factor(haa$area, levels=rev(haa$area))

haa$`Treatment success` <- haa$newrel_succ / haa$newrel_coh * 100
haa$Failure <- haa$newrel_fail / haa$newrel_coh * 100
haa$Died <- haa$newrel_died / haa$newrel_coh * 100
haa$`Lost to follow-up` <- haa$newrel_lost / haa$newrel_coh * 100
haa$`Not evaluated` <- haa$c_newrel_neval / haa$newrel_coh * 100


# Plot

hab <- melt(haa[c(1, 8:12)], id=1)

hac <- ggplot(hab, aes(area, value, fill=variable)) + geom_bar(stat="identity", position="stack") + geom_hline(yintercept=85, color="grey70") + geom_text(data=subset(hab, variable=="Treatment success"), aes(label=round(value,0)), hjust=1.25, vjust=0.3, size=4, color="white") + theme_glb.rpt() + coord_flip() +   scale_fill_brewer("", type = "qual", palette = 8) + labs(x="", y="Percentage of cohort") + theme(legend.position="bottom", panel.grid=element_blank()) + expand_limits(c(0,0))

figsave(hac, hab, "4_5_txsucc")

# B4_6_hiv_ts_d ---------------------------------------------------

hma <- subset(tb, year==thisyear-2 & !is.na(tbhiv_coh) & !is.na(newrel_coh), c(iso3, newrel_coh, newrel_succ, newrel_fail, newrel_died, newrel_lost, c_newrel_neval, ret_nrel_coh, ret_nrel_succ, ret_nrel_fail, ret_nrel_died, ret_nrel_lost, c_ret_nrel_neval, tbhiv_coh, tbhiv_succ, tbhiv_fail, tbhiv_died, tbhiv_lost, c_tbhiv_neval)) 

if(thisyear==2014){
  hma <- subset(hma, iso3 %nin% c('COD', "MOZ"))
  warning("DRC and Mozambique numbers removed!!!")
}



hma[1] <- "global"

hmb <- aggregate(hma[2:ncol(hma)], by=list(area=hma$iso3), FUN=sum, na.rm=TRUE)

hmc <- melt(hmb, id=1)
hmc$type <- str_extract(hmc$variable, "tbhiv|newrel|ret_nrel")
hmc$out <- str_extract(hmc$variable, "coh|succ|fail|died|lost|neval")

# Combine newrel and ret_nrel

hmc$type <- ifelse(hmc$type=="tbhiv", "tbhiv", "all")
hmc1 <- aggregate(hmc[3], by=list(type=hmc$type, out=hmc$out), FUN=sum, na.rm=TRUE)

hmd <- cast(hmc1, out~type)

hmd$`HIV-` <- hmd$all - hmd$tbhiv
hmd <- rename(hmd, c(tbhiv="HIV+"))

hme <- melt(as.data.frame(hmd[-2]), id=1, variable_name = "type")

hmf <- cast(hme, ...~out)

hmf$`Treatment success` <- hmf$succ / hmf$coh * 100
hmf$Failed <- hmf$fail / hmf$coh * 100
hmf$Died <- hmf$died / hmf$coh * 100
hmf$`Lost to follow-up` <- hmf$lost / hmf$coh * 100
hmf$`Not evaluated` <- hmf$neval / hmf$coh * 100

hmg <- melt(as.data.frame(hmf[c(1,8:ncol(hmf))]), id=1) # It's a melt and cast fiesta!

hmh <- ggplot(hmg, aes(type, value, fill=variable)) + geom_bar(stat="identity", position="stack", width=0.5) + theme_glb.rpt() + labs(x="") + scale_y_continuous("Percentage of cohort") + scale_fill_brewer("", type = "qual", palette=6) + ggtitle(paste("Outcomes of TB treatment by HIV status,", thisyear-2, sep=" "))

hmi <- hmg; hmi$duh <- ""

# hmj <- ggplot(hmi, aes(duh, value, fill=type)) + geom_bar(stat="identity", position="dodge", width=0.5) + facet_wrap(~variable, scales="free") + theme_glb.rpt() + labs(x="") + scale_y_continuous("Percentage of cohort") + scale_fill_brewer("", type = "qual", palette=6)

hmj <- ggplot(hmi, aes(variable, value, fill=type)) + geom_bar(stat="identity", position="dodge", width=0.5)  + theme_glb.rpt() + labs(x="") + scale_y_continuous("Percentage of cohort", limits=c(0,100)) + scale_fill_brewer("", type = "qual", palette=6) + ggtitle(paste("Outcomes of TB treatment by HIV status,", thisyear-2, sep=" "))

# figsave(hmh, hmg, "hiv_ts_d_oldversion")
figsave(hmj, hmg, "B4_6_hiv_ts_d")


if(flg_show_estimates){
  
  #-------------------------------------------------------------------
  # SLIDE - absolute incidence and mortality (all and hiv-neg)
  #-------------------------------------------------------------------
  
  sga <- subset(global, select=c("year", "inc.num", "inc.lo.num", "inc.hi.num",
                                 "mort.num", "mort.lo.num", "mort.hi.num",
                                 "mort.nh.num", "mort.nh.lo.num", "mort.nh.hi.num"))
  
  sgb <- sga	
  sga[,5:10] <- NA
  sga$im <- 'Incidence'
  
  sgb[,2:4] <- NA
  sgb$im <- 'Mortality (HIV-negative in blue)'
  
  sgc <- rbind(sga, sgb)
  sgc[2:10] <- sgc[2:10] / 1000000
  
  sga1 <- ggplot(sgc, aes(x=year, y=inc.num)) + geom_line(colour="green") + 
    geom_ribbon(aes (year, ymin=inc.lo.num, ymax=inc.hi.num), fill=I('green'), alpha=I(0.2)) +
    geom_line(aes(year, mort.num), color='purple') +
    geom_ribbon(aes (year, ymin=mort.lo.num, ymax=mort.hi.num), fill=I('purple'), alpha=I(0.2)) +
    geom_line(aes(year, mort.nh.num), color='blue') +
    geom_ribbon(aes (year, ymin=mort.nh.lo.num, ymax=mort.nh.hi.num), fill=I('blue'), alpha=I(0.2)) +
    facet_wrap(~ im, scales='free') + 
    geom_point(aes(year, inc.num*0), alpha=0) + geom_point(aes(year, mort.num*0), alpha=0) +
    
    scale_y_continuous( name = "Millions of people" ) + theme_glb.rpt() +
    scale_x_continuous(name="") +  
    opts(title=paste("Global incidence and mortality, 1990", thisyear-1, sep="-"),
         plot.title=theme_text(size=22), axis.title.y=theme_text(size=15, angle=90),
         axis.text.y=theme_text(size=15), axis.text.x=theme_text(size=15)) 
  
  # windows(11, 7); sga1; dev.off()
  ggsave(paste(outfolder, "/Slides/", "inc_mort", ".png", sep=""), sga1, width=11, height=7) 	
  
  #-------------------------------------------------------------------
  # just incidence with notifications
  #-------------------------------------------------------------------
  
  sgd <- subset(global, select=c("year", "inc.num", "inc.lo.num", "inc.hi.num"))
  sge <- aggregate(n['c_newinc'], by=list(year=n$year), FUN=sum, na.rm=T)
  sgf <- merge(sgd, sge)
  sgf[2:5] <- sgf[2:5] / 1000000
  
  sga2 <- ggplot(sgf, aes(x=year, y=inc.num)) + geom_line(colour="green", size=1) + 
    geom_ribbon(aes (year, ymin=inc.lo.num, ymax=inc.hi.num), fill=I('green'), alpha=I(0.2)) +
    geom_line(aes(year, c_newinc), size=1) +
    scale_y_continuous(name = "Cases (millions)") + theme_glb.rpt() +
    scale_x_continuous(name="") +  
    expand_limits(y=c(0,9.9)) +
    theme_glb.rpt(base_size=16) +
    opts(legend.position='none', title=
           paste("Global incidence and \nnotifications, 1990", thisyear-1, sep="-"), 
         plot.title = theme_text(hjust=0.3, size=20))
  
  # windows(11, 7); sga1; dev.off()
  ggsave(paste(outfolder, "/Slides/", "global_incnotif_num", ".png", sep=""), sga2, width=5, height=6) 	
  
  #-------------------------------------------------------------------
  # Revamping Christopher's Global plan plot
  #-------------------------------------------------------------------
  
  library(stringr)
  
  setwd('J:/DivData/TME - monitoring & evaluation/M&S/1docs (Annual TB Report)/11/Tables and Figures/For Christopher')
  
  cfa <- read.csv('fin_fig10.csv')
  
  names(cfa) <- c("Region", "year", "DOTS_hi", "DOTS_lo", "DOTS_all", "MDR-TB_hi", 
                  "MDR-TB_lo", "MDR-TB_all", "TB/HIV_hi", "TB/HIV_lo", 
                  "TB/HIV_all", "Laboratories_hi", "Laboratories_lo", "Laboratories_all", "Total_hi", 
                  "Total_lo", "Total_all")
  cfb <- melt(cfa, id=1:2)
  
  for(ro in 1:nrow(cfb)){
    
    both <- as.data.frame(str_split(cfb[ro,'variable'], "_"))
    cfb[ro, 'var'] <- both[1,]
    cfb[ro, 'hilo'] <- both[2,]
    
  }
  
  cfc <- cast(cfb, Region+year+var~hilo)
  
  cfc$var <- factor(cfc$var, levels=c("DOTS", "MDR-TB", "Total", "TB/HIV", "Laboratories"))
  cfc$Region <- factor(cfc$Region, labels=c('Europe', 'Rest of World', 'World'))
  cfc['Needed'] <- cfc$Region
  
  alph <- .5
  
  cfd <- ggplot(cfc, aes(year, all, color=Region)) + geom_line(size=1, alpha=alph) + facet_wrap(~var, scales="free_y") +
    geom_point(alpha=alph) + geom_ribbon(aes(year, ymin=lo, ymax=hi, fill=Region), color=NA, alpha=alph) +
    geom_hline(yintercept=0, alpha=0) + scale_y_continuous(name = "US$ millions (nominal)") + 
    scale_x_continuous("") + #scale_color_brewer(name="Funding\n \nAvailable", palette="Dark2") +
    opts(title='Funding available (lines) for TB control, 2010-2012 and funding needed (bands) according
         to the Global Plan, 2011-2015') + theme_glb.rpt()
  
  # windows(10,7); cfd
  ggsave(paste("fin_fig10", Sys.Date(), ".pdf", sep=""), cfd, width=10, height=7) 
}

#-------------------------------------------------------------------
# hiv_txout _rep and _tsr
#-------------------------------------------------------------------

h <- o

h[20:ncol(h)] <- ifelse(is.na(h[20:ncol(h)]), NA, 1) # Holy smoke it worked!

gia <- h

gia$count <- 1
gib <- aggregate(gia[20:ncol(gia)], by=list(year=gia$year), FUN=sum, na.rm=T)

gic <- melt(gib[c('year', 'new_sp_coh', 'hiv_new_sp_coh', 'hiv_new_snep_coh', 'hiv_ret_coh', 'count')], id=1)
gid <- qplot(year, value, data=gic, geom='line', color=variable)

gidout <- names(gia[c(1:20)])
gidcsv <- merge(eraw[c('iso3', 'year', 'e_inc_tbhiv_num')], 
                gia[c(gidout, 'new_sp_coh', 'hiv_new_sp_coh', 'hiv_new_snep_coh', 'hiv_ret_coh')], all.y=T)
# csv(gidcsv)

#-------------------------------------------------------------------
# hiv_tsr_graph
#-------------------------------------------------------------------

gie <- o[o$year >= 2007, c('country', 'iso3', 'year', 'new_sp_coh', 'new_sp_cur', 'new_sp_cmplt', 'hiv_new_sp_coh', 
                           'hiv_new_sp_cur', 'hiv_new_sp_cmplt', 'new_sp_died', 'hiv_new_sp_died')]

gif <- aggregate(gie[4:ncol(gie)], by=list(iso3=gie$iso3), FUN=sum, na.rm=F)
gif_full <- gif[!is.na(gif$hiv_new_sp_coh), 'iso3'] # this is right (25)
gif_full <- gif_full[-19] # I'm taking out Romania for weird data in 2008

gig <- gie[gie$iso3 %in% gif_full, ] #; gdd_num <- paste(',', length(gdb_full), 'reporting countries')# Full time series only

gih <- aggregate(gig[3:ncol(gig)], by=list(year=gig$year), FUN=sum, na.rm=T)

gih$nhiv_tsr <- (gih$new_sp_cur + gih$new_sp_cmplt - gih$hiv_new_sp_cur - gih$hiv_new_sp_cmplt) / 
  (gih$new_sp_coh - gih$hiv_new_sp_coh)

gih$hiv_tsr <- (gih$hiv_new_sp_cur + gih$hiv_new_sp_cmplt) / gih$hiv_new_sp_coh

gih$nhiv_dr <- (gih$new_sp_died - gih$hiv_new_sp_died) / 
  (gih$new_sp_coh - gih$hiv_new_sp_coh)

gih$hiv_dr <- gih$hiv_new_sp_died / gih$hiv_new_sp_coh
gii <- melt(gih[c('year', 'nhiv_tsr', 'hiv_tsr', 'nhiv_dr', 'hiv_dr')], id=1)
gij <- qplot(year, value, data=gii, geom='line', color=variable, labels=percent, ylim=c(0,1)) 

gii$hiv <- ifelse(gii$variable=='nhiv_tsr' | gii$variable=='nhiv_dr', 'negative', 'positive')

# o[o$iso3 %in% gif_full & (o$year==2008 | o$year==2009), c('country', 'c_tsr')]
levels(gii$variable) <- c("HIV-negative treatment success", "HIV-positive treatment success", 
                          "HIV-negative died", "HIV-positive died")

gik <-  ggplot(gii, aes(year, value, group=variable, color=hiv)) + 
  geom_line(size=1.5) + geom_text(data=gii[gii$year==thisyear-2,],
                                  aes(label = variable), hjust=-.1, vjust=0, size=3) +
  scale_y_continuous(name = "Percent of cohort", labels=percent, expand=c(0,0)) +
  scale_x_continuous("", breaks=2004:2009, labels=2004:2009, expand=c(0,0)) + 
  theme_glb.rpt() + expand_limits(y=c(0, 1), x=c(2004, 2010.6)) + scale_color_brewer(palette="Dark2") +
  opts(legend.position="none", panel.grid.minor=theme_blank(),
       title=paste('Percentage of treatment cohort successfully treated and died 
                   by HIV status in', length(gif_full), 'selected countries, 2004-2009'))

# windows(10, 7); gik; dev.off()
ggsave(paste(outfolder, "/Figs/", "hiv_tsr_graph", Sys.Date(), ".pdf", sep=""), gik, width=10, height=7) 

gil <- ggplot(gii, aes(year, value, group=variable, color=hiv)) + 
  geom_line(size=1.5) + geom_text(data=gii[gii$year==thisyear-2,],
                                  aes(label = variable), hjust=-.1, vjust=0, size=5) +
  scale_y_continuous(name = "Percent of cohort", labels=percent, expand=c(0,0)) +
  scale_x_continuous("", breaks=2004:2009, labels=2004:2009, expand=c(0,0)) + 
  theme_glb.rpt() + expand_limits(y=c(0, 1), x=c(2004, 2011.9)) + scale_color_brewer(palette="Dark2") +
  opts(legend.position="none", panel.grid.minor=theme_blank(),
       title=paste('Percentage of treatment cohort successfully treated and died 
                   by HIV status in', length(gif_full), 'selected countries, 2004-2009'), 
       plot.title=theme_text(size=22), axis.title.y=theme_text(size=15, angle=90),
       axis.text.y=theme_text(size=15), axis.text.x=theme_text(size=15), 
       legend.title=theme_text(size=14, hjust=0), legend.text=theme_text(size=12))

# windows(11, 7); gil; dev.off()
ggsave(paste(outfolder, "/Slides/", "hiv_tsr_graph", ".wmf", sep=""), gil, width=11, height=7) 

write.csv(gii, file=paste(outfolder, "/FigData/", "hiv_tsr_graph", Sys.Date(), ".csv", sep=""), row.names=F, na="")

#-------------------------------------------------------------------
# hiv_txout
#-------------------------------------------------------------------

gha <- subset(o, year==thisyear-2, select=c('iso2', 'country', 'g_hbhiv63', 'year', 'hiv_new_sp_coh', 'hiv_new_sp_cur', 'hiv_new_sp_cmplt', 
                                            'hiv_new_sp_died', 'hiv_new_sp_fail', 'hiv_new_sp_def', 'new_sp_coh', 'new_sp_cur', 
                                            'new_sp_cmplt', 'new_sp_died', 'new_sp_fail', 'new_sp_def', 'hiv_new_snep_coh', 
                                            'hiv_new_snep_cmplt', 'hiv_new_snep_died', 'hiv_new_snep_fail', 'hiv_new_snep_def', 
                                            'new_snep_coh', 'new_snep_cmplt', 'new_snep_died', 'new_snep_fail', 
                                            'new_snep_def', 'hiv_ret_coh', 'hiv_ret_cur', 'hiv_ret_cmplt', 'hiv_ret_died', 
                                            'hiv_ret_fail', 'hiv_ret_def', 'ret_coh', 'ret_cur', 'ret_cmplt', 'ret_died', 'ret_fail', 
                                            'ret_def'))

gha$count <- 1

# Generate tx success and not evaluated vars
gha <- within(gha, {
  new_sp_suc <- rowSums(cbind(new_sp_cur, new_sp_cmplt), na.rm=T)
  hiv_new_sp_suc <- rowSums(cbind(hiv_new_sp_cur, hiv_new_sp_cmplt), na.rm=T)
  
  new_snep_suc <- new_snep_cmplt
  hiv_new_snep_suc <- hiv_new_snep_cmplt
  
  ret_suc <- rowSums(cbind(ret_cur, ret_cmplt), na.rm=T)
  hiv_ret_suc <- rowSums(cbind(hiv_ret_cur, hiv_ret_cmplt), na.rm=T)
  
  new_sp_neval <- new_sp_coh - rowSums(cbind(new_sp_cur, new_sp_cmplt, new_sp_died, new_sp_fail, new_sp_def), na.rm=T)
  hiv_new_sp_neval <- hiv_new_sp_coh - rowSums(cbind(hiv_new_sp_cur, hiv_new_sp_cmplt, hiv_new_sp_died, 
                                                     hiv_new_sp_fail, hiv_new_sp_def), na.rm=T)
  
  new_snep_neval <- new_snep_coh - rowSums(cbind(new_snep_cmplt, new_snep_died, new_snep_fail, new_snep_def), na.rm=T)
  hiv_new_snep_neval <- hiv_new_snep_coh - rowSums(cbind(hiv_new_snep_cmplt, hiv_new_snep_died, 
                                                         hiv_new_snep_fail, hiv_new_snep_def), na.rm=T)
  
  ret_neval <- ret_coh - rowSums(cbind(ret_cur, ret_cmplt, ret_died, ret_fail, ret_def), na.rm=T)
  hiv_ret_neval <- hiv_ret_coh - rowSums(cbind(hiv_ret_cur, hiv_ret_cmplt, hiv_ret_died, 
                                               hiv_ret_fail, hiv_ret_def), na.rm=T)
})

# Remove countries who didn't report HIV outcomes

gha$new_sp_rep <- ifelse(is.na(gha$new_sp_coh) | gha$hiv_new_sp_coh==0 | is.na(gha$hiv_new_sp_coh), 0, 1)
gha$new_snep_rep <- ifelse(is.na(gha$new_snep_coh) | gha$hiv_new_snep_coh==0 | is.na(gha$hiv_new_snep_coh), 0, 1)
gha$ret_rep <- ifelse(is.na(gha$ret_coh) | gha$hiv_ret_coh==0 | is.na(gha$hiv_ret_coh), 0, 1)
gha$all_rep <- ifelse(is.na(gha$new_sp_coh) | gha$hiv_new_sp_coh==0 | is.na(gha$hiv_new_sp_coh) |
                        is.na(gha$new_snep_coh) | gha$hiv_new_snep_coh==0 | is.na(gha$hiv_new_snep_coh) |
                        is.na(gha$ret_coh) | gha$hiv_ret_coh==0 | is.na(gha$hiv_ret_coh), 0, 1)
gha$any_rep <- ifelse(gha$new_sp_rep==1 | gha$new_snep_rep==1 | gha$ret_rep==1 , 1, 0)

ghb <- gha[gha$new_sp_rep==1, ]
ghb <- aggregate(ghb[5:ncol(ghb)], by=list(year=ghb$year), FUN=sum, na.rm=T)

ghc <- gha[gha$new_snep_rep==1, ]
ghc <- aggregate(ghc[5:ncol(ghc)], by=list(year=ghc$year), FUN=sum, na.rm=T)

ghd <- gha[gha$ret_rep==1, ]
ghd <- aggregate(ghd[5:ncol(ghd)], by=list(year=ghd$year), FUN=sum, na.rm=T)

ghe <- data.frame(type=c('New smear-positive', 'New smear-positive', 'New smear-negative/ extrapulmonary', 
                         'New smear-negative/ extrapulmonary', 'Retreatment', 'Retreatment'), 
                  hiv=c('HIV +', 'HIV -', 'HIV +', 'HIV -', 'HIV +', 'HIV -'))

ghe['Successfully treated'] <- c(
  ghb$hiv_new_sp_suc , 
  (ghb$new_sp_suc - ghb$hiv_new_sp_suc) , 
  ghc$hiv_new_snep_suc , 
  (ghc$new_snep_suc - ghc$hiv_new_snep_suc) , 
  ghd$hiv_ret_suc , 
  (ghd$ret_suc - ghd$hiv_ret_suc) )

ghe['Cohort'] <- c(
  ghb$hiv_new_sp_coh, 
  (ghb$new_sp_coh - ghb$hiv_new_sp_coh), 
  ghc$hiv_new_snep_coh, 
  (ghc$new_snep_coh - ghc$hiv_new_snep_coh), 
  ghd$hiv_ret_coh, 
  (ghd$ret_coh - ghd$hiv_ret_coh))

ghe['Percent successfully treated'] <- c(
  ghb$hiv_new_sp_suc / ghb$hiv_new_sp_coh, 
  (ghb$new_sp_suc - ghb$hiv_new_sp_suc) / (ghb$new_sp_coh - ghb$hiv_new_sp_coh), 
  ghc$hiv_new_snep_suc / ghc$hiv_new_snep_coh, 
  (ghc$new_snep_suc - ghc$hiv_new_snep_suc) / (ghc$new_snep_coh - ghc$hiv_new_snep_coh), 
  ghd$hiv_ret_suc / ghd$hiv_ret_coh, 
  (ghd$ret_suc - ghd$hiv_ret_suc) / (ghd$ret_coh - ghd$hiv_ret_coh))

ghe['Died'] <- c(
  (ghb$hiv_new_sp_died + ghb$hiv_new_sp_def) , 
  (ghb$new_sp_died - ghb$hiv_new_sp_died) , 
  (ghc$hiv_new_snep_died + ghc$hiv_new_snep_def) , 
  (ghc$new_snep_died - ghc$hiv_new_snep_died) , 
  (ghd$hiv_ret_died + ghd$hiv_ret_def) , 
  (ghd$ret_died - ghd$hiv_ret_died)  ) 

ghe['Evaluated cohort'] <- c(
  (ghb$hiv_new_sp_coh - ghb$hiv_new_sp_neval), 
  ((ghb$new_sp_coh - ghb$hiv_new_sp_coh) - (ghb$new_sp_neval - ghb$hiv_new_sp_neval)), 
  (ghc$hiv_new_snep_coh - ghc$hiv_new_snep_neval), 
  ((ghc$new_snep_coh - ghc$hiv_new_snep_coh) - (ghc$new_snep_neval - ghc$hiv_new_snep_neval)), 
  (ghd$hiv_ret_coh - ghd$hiv_ret_neval), 
  ((ghd$ret_coh - ghd$hiv_ret_coh) - (ghd$ret_neval - ghd$hiv_ret_neval))) 


ghe['Percent died'] <- c(
  (ghb$hiv_new_sp_died + ghb$hiv_new_sp_def) / (ghb$hiv_new_sp_coh - ghb$hiv_new_sp_neval), 
  (ghb$new_sp_died - ghb$hiv_new_sp_died) / 
    ((ghb$new_sp_coh - ghb$hiv_new_sp_coh) - (ghb$new_sp_neval - ghb$hiv_new_sp_neval)), 
  (ghc$hiv_new_snep_died + ghc$hiv_new_snep_def) / (ghc$hiv_new_snep_coh - ghc$hiv_new_snep_neval), 
  (ghc$new_snep_died - ghc$hiv_new_snep_died) / 
    ((ghc$new_snep_coh - ghc$hiv_new_snep_coh) - (ghc$new_snep_neval - ghc$hiv_new_snep_neval)), 
  (ghd$hiv_ret_died + ghd$hiv_ret_def) / (ghd$hiv_ret_coh - ghd$hiv_ret_neval), 
  (ghd$ret_died - ghd$hiv_ret_died)  / 
    ((ghd$ret_coh - ghd$hiv_ret_coh) - (ghd$ret_neval - ghd$hiv_ret_neval))) 

ghe['Countries'] <- c(
  ghb$count, ghb$count, 
  ghc$count, ghc$count, 
  ghd$count, ghd$count)

ghe7 <- c('All', 'HIV +', sum(ghe[c(1,3,5), 3]), sum(ghe[c(1,3,5), 4]), sum(ghe[c(1,3,5), 3])/ sum(ghe[c(1,3,5), 4]),
          sum(ghe[c(1,3,5), 6]), sum(ghe[c(1,3,5), 7]), sum(ghe[c(1,3,5), 6]) / sum(ghe[c(1,3,5), 7]), sum(gha$any_rep))

ghe8 <- c('All', 'HIV -', sum(ghe[c(2,4,6), 3]), sum(ghe[c(2,4,6), 4]), sum(ghe[c(2,4,6), 3])/ sum(ghe[c(2,4,6), 4]),
          sum(ghe[c(2,4,6), 6]), sum(ghe[c(2,4,6), 7]), sum(ghe[c(2,4,6), 6]) / sum(ghe[c(2,4,6), 7]), sum(gha$any_rep))

ghe[7, ] <- ghe7
ghe[8, ] <- ghe8

write.csv(ghe, file=paste(outfolder, "/FigData/", "hiv_txout", Sys.Date(), ".csv", sep=""), row.names=F, na="")

#-------------------------------------------------------------------
# txout_reg
#-------------------------------------------------------------------

gia <- subset(o, year==thisyear-2, select=c('iso2', 'country', 'g_whoregion', 'year', 'new_sp_coh', 'new_sp_cur', 'new_sp_cmplt', 'new_sp_died', 'new_sp_fail', 'new_sp_def', 'c_new_sp_neval', 'new_snep_coh', 'new_snep_cmplt', 'new_snep_died', 'new_snep_fail', 'new_snep_def', 'c_new_snep_neval', 'ret_coh', 'ret_cur', 'ret_cmplt', 'ret_died', 'ret_fail', 'ret_def', 'c_ret_neval'))

gia$count <- 1

# Remove non-reporting countries for total countries reporting
gib <- subset(gia, !is.na(new_sp_coh) | !is.na(new_snep_coh) | !is.na(ret_coh))

# Aggregate by region and global

gi.reg <- aggregate(gib[5:ncol(gib)], by=list(area=gib$g_whoregion), FUN=sum, na.rm=T)

gi.glo <- aggregate(gib[5:ncol(gib)], by=list(area=gib$year), FUN=sum, na.rm=T)
gi.glo$area <- 'Global'

gic <- .shortnames(rbind(gi.reg, gi.glo), 'area')
gic$area <- factor(gic$area, levels=rev(gic$area), labels=rev(gic$area))

### Smear-positive cases only
gic1 <- within(gic, {
  Cases <- new_sp_coh# + new_snep_coh  + ret_coh
  Success <- (new_sp_cur + new_sp_cmplt) / Cases # + new_snep_cmplt + ret_cur + ret_cmplt
  Died <- (new_sp_died) / Cases # + new_snep_died + ret_died
  Failed <- (new_sp_fail) / Cases # + new_snep_fail + ret_fail
  Defaulted <- (new_sp_def) / Cases # + new_snep_def + ret_def
  Unevaluated <- (c_new_sp_neval) / Cases # + c_new_snep_neval + c_ret_neval
})

# reshape for the plot
gid1 <- melt(gic1[,c('area', 'Success', 'Died', 'Failed', 'Defaulted', 'Unevaluated')], id=1)
gid1$variable <- factor(gid1$variable, levels=c("Success", "Died", "Failed", "Defaulted", "Unevaluated"), labels=c("Successfully treated", "Died", "Failed", "Defaulted", "Not evaluated"))

gie1 <- ggplot(gid1, aes(area, value, fill = variable) ) + geom_bar(stat='identity', position='fill' ) + scale_y_continuous("", breaks=seq(0, 1, 0.1), labels=percent_format()) + scale_fill_manual("", values=c('goldenrod2', 'firebrick3', 'olivedrab', 'navy', 'orangered')) + coord_flip() + theme_glb.rpt() + labs(x="", title=paste('a. New smear-positive cases')) + theme(legend.position='bottom', plot.title = element_text(hjust = 0))

figsave(gie1, gid1, 'txout_reg1')

### New cases only
gic2 <- within(gic, {
  Cases <- new_sp_coh + new_snep_coh  #+ ret_coh
  Success <- (new_sp_cur + new_sp_cmplt + new_snep_cmplt ) / Cases #+ ret_cur + ret_cmplt
  Died <- (new_sp_died + new_snep_died) / Cases # + ret_died
  Failed <- (new_sp_fail + new_snep_fail) / Cases # + ret_fail
  Defaulted <- (new_sp_def + new_snep_def) / Cases # + ret_def
  Unevaluated <- (c_new_sp_neval + c_new_snep_neval) / Cases # + c_ret_neval
})

# reshape for the plot
gid2 <- melt(gic2[,c('area', 'Success', 'Died', 'Failed', 'Defaulted', 'Unevaluated')], id=1)
gid2$variable <- factor(gid2$variable, levels=c("Success", "Died", "Failed", "Defaulted", "Unevaluated"), labels=c("Successfully treated", "Died", "Failed", "Defaulted", "Not evaluated"))

gie2 <- ggplot(gid2, aes(area, value, fill = variable) ) + geom_bar(stat='identity', position='fill' ) + scale_y_continuous("", breaks=seq(0, 1, 0.1), labels=percent_format()) + scale_fill_manual("", values=c('goldenrod2', 'firebrick3', 'olivedrab', 'navy', 'orangered')) + coord_flip() + theme_glb.rpt() + labs(x="", title=paste('b. All new cases')) + theme(legend.position='bottom', plot.title = element_text(hjust = 0))

figsave(gie2, gid2, 'txout_reg2')

#==========================
# Pieces of the 'top 10 countries' figure (previously a table)

tma <- subset(e.t,e_inc_num > 1e3 & year==thisyear-1)

tma$e_mort_num <- tma$e_mort_100k / 1e5 * tma$e_pop_num
tma$e_mort_num_lo <- tma$e_mort_100k_lo / 1e5 * tma$e_pop_num
tma$e_mort_num_hi <- tma$e_mort_100k_hi / 1e5 * tma$e_pop_num

.maxplus <- function(df, vect, num.rows){
  df1 <- df[order(df[vect], decreasing=TRUE),]
  df1 <- .shortnames(df1[1:num.rows, c('country', vect, glue(vect, '_lo'), glue(vect, '_hi'))])
  df1$var <- vect
  names(df1) <- c("country", 'best', 'lo', 'hi', 'var')
  (df1)
}

tm1 <- .maxplus(tma, 'e_inc_num', 10)
tm2 <- .maxplus(tma, 'e_inc_100k', 10)
tm3 <- .maxplus(tma, 'e_inc_tbhiv_num', 10)
tm4 <- .maxplus(tma, 'e_mort_exc_tbhiv_num', 10)
tm5 <- .maxplus(tma, 'e_mort_exc_tbhiv_100k', 10)
tm6 <- .maxplus(tma, 'e_mort_num', 10)
tm7 <- .maxplus(tma, 'e_mort_100k', 10)

tm.nhiv <- rbind(tm1, tm2, tm3, tm4, tm5, tm6, tm7)
tm.nhiv$var <- factor(tm.nhiv$var, levels=c("e_inc_num", "e_inc_100k",  "e_inc_tbhiv_num", "e_mort_exc_tbhiv_num", "e_mort_exc_tbhiv_100k", 'e_mort_num', 'e_mort_100k'), labels=c("Incidence: absolute numbers", "Incidence: rate per 100 000 population",  "TB/HIV incidence", "Mortality (excluding TB/HIV)", "Mortality per 100 000 (excluding TB/HIV)", "Mortality (including TB/HIV)", "Mortality per 100 000 (including TB/HIV)"))

# ggplot(tm.nhiv, aes(best, country)) + geom_point()  + facet_wrap(~var, scales='free') 
# + geom_errorbar(aes(ymin='lo', ymax='hi'))+ coord_flip()

for(pn in c(1,4,6)){
  vr <- levels(tm.nhiv$var)[pn]
  tn <- subset(tm.nhiv, var==vr)
  tn$country <- factor(tn$country, levels=rev(tn$country))
  tn1 <- ggplot(tn, aes(best/1e6, country, xmin=lo/1e6, xmax=hi/1e6)) + geom_point()  + geom_errorbarh(height=.25) +  theme_glb.rpt() + labs(y="", x='Cases per year (millions)', title=vr) + theme(plot.title = element_text(hjust = 0)) 
  
  figsave(tn1, tn, glue('topten_', pn), width=5, height=4) 
}

for(pn in c(2,5,7)){
  vr <- levels(tm.nhiv$var)[pn]
  tn <- subset(tm.nhiv, var==vr)
  tn$country <- factor(tn$country, levels=rev(tn$country))
  tn1 <- ggplot(tn, aes(best, country, xmin=lo, xmax=hi)) + geom_point()  + geom_errorbarh(height=.25) +  theme_glb.rpt() + labs(y="", x='Rate per 100 000 per year', title=vr) + theme(plot.title = element_text(hjust = 0)) 
  
  figsave(tn1, tn, glue('topten_', pn), width=5, height=4) 
}






## END ++=================================++
dmod <- lm(price ~ cut, data=diamonds)
cuts <- data.frame(cut=unique(diamonds$cut), predict(dmod, data.frame(cut = unique(diamonds$cut)), se=TRUE)[c("fit","se.fit")])

se <- ggplot(cuts, aes(cut, fit, ymin = 3400, ymax=fit + se.fit, colour = cut))
se + geom_linerange()


