# Epidemiological Situation of Tuberculosis in the Western Pacific towards the End TB Strategy Targets: analysis of case notification data in `r yr <- 2013; yr`
Tom Hiatt<sup>&dagger;</sup> and Nobuyuki Nishikiori<sup>&dagger;</sup>  



[TH: targets in the title is maybe not the best idea, since we have no baseline and catastrophic expenditure hasn't even been concluded how to measure.]

<sup>&dagger;</sup> Stop TB and Leprosy Elimination, Division of Communicable Diseases, World Health Organization Regional Office for the Western Pacific, Manila, Philippines.

--------------------------------------------------------

Correspondence to Tom Hiatt (e-mail: hiattt@wpro.who.int).

--------------------------------------------------------

To cite this article: 



--------------------------------------------------------

Abstract 
--------------------------------------------------------


Tuberculosis (TB) control in the World Health Organization (WHO) Western Pacific Region has seen substantial progress in the last decade with a 36 reduction in prevalent TB cases since 2000. The burden remains immense however, and national TB programmes must evolve and adapt to build upon these gains. Through routine surveillance, countries and areas in the Region reported 1.4 million TB cases in 2013. The case notification rate increased in the early 2000s, appears to have stabilized in recent years and is in decline for all forms and laboratory-confirmed cases. The age and sex breakdown for laboratory-confirmed TB case rates by country shows generally higher rates with increased age and declining rates over time for all age groups. Treatment success remains high in the Region with 16 countries reaching or maintaining an 85% success rate. HIV testing among TB patients has increased gradually along with a slow decline in the number of HIV-positive patients found.

The trend of TB notification is heavily influenced by programmatic improvements in many countries and rapidly changing demographics. It appears that cases are being found earlier as reflected in declining rates of laboratory-confirmed TB with steady rates of TB in all forms. WHO estimates depict a decline in TB incidence in the Region. HIV testing, while still low, has increased substantially in recent years with essential TB/HIV services expanding in many countries.

TB surveillance data, within inherent limitations, is an important source of programmatic and epidemiological information. Careful interpretation of these findings can provide useful insight for programmatic decision-making.

--------------------------------------------------------

Introduction
--------------------------------------------------------


Significant progress has been made in tuberculosis (TB) control in the World Health Organization (WHO) Western Pacific Region especially in since 2000. The number of prevalent TB patients in the Region fell from 3.5 million in 2000 to 2.3 million in 2013.[@WorldHealthOrganization2014] During the same period, 16.7 million patients were diagnosed and treated, and an estimated 800 000 deaths were averted between 2000 and 2010.[@WorldHealthOrganization2011] According to the latest WHO estimates, the Region is on track for achieving the TB-related Millennium Development Goals (MDGs) and other international targets by 2015. However, with 1.4 million TB patients notified annually in the Region and several countries with a persistent substantial disease burden, TB control policies and strategies require continuous evolution to adopt new tools and approaches as well as to address emerging challenges faced by national TB control programmes. In light of the MDG target date approaching, WHO has developed a new Global TB Strategy for after 2015.[@WorldHealthOrganization2014a] Particularly at this critical period of strategy renewal, thorough analysis of surveillance data provides valuable information on the current epidemiological situation, programmatic progress and future directions.

This article represents our second regional analysis of TB notification data as an article in the Western Pacific Surveillance and Response Journal. We plan to continue reporting an annual regional analysis in this way punctuated by complementary reports focused on various topics such as subnational data analysis and utilization, the situation of drug-resistant TB, contact investigation and other forms of TB screening activities to stimulate the utilization of surveillance data for informed programme decision-making.

Methods
--------------------------------------------------------


### Data
Every year, 36 countries and areas in the Region are requested to report TB surveillance data to WHO using a standardized data collection form. Since 2009, a web-based online system has been used for data submission and validation. Collected data covers the following areas: TB case notifications and treatment outcomes, diagnostic and treatment services, drug management, surveillance and surveys of drug-resistance, information on TB/HIV co-infection, infection control, engagement of all care providers and budgets and expenditures for TB control. The full description of methods is available in the Global Tuberculosis Report 2014[@WorldHealthOrganization2014] and the data sets are available from the WHO global TB database (www.who.int/tb/data). Case definitions for TB can be found in the 4th edition of the TB treatment guidelines.[@WorldHealthOrganization2006] For 2013, 32 countries and areas of the Western Pacific Region reported data representing more than 99.9% of the total population. This report describes the epidemiological situation and progress in programmatic response with a focus on seven countries with a high burden of TB: Cambodia, China, the Lao People's Democratic Republic, Mongolia, Papua New Guinea, the Philippines and Viet Nam. The remaining countries in the Region have been split into 20 Pacific Island countries and areas ("Pacific") and 9 other countries and areas ("Other").

### Analysis and reproducibility
Analysis was conducted by the statistical package R (ver. 3.1.2, R Core Team, 2014, Vienna, Austria, www.R-project.org). Due to calls for transparent and reproducible research,[@Peng2006; @Groves2012] we have published programme code to generate the entire contents of this article including all figures and tables by using R with the knitr package (ver. 1.9, Yihui Xie, 2015). Readers can download the code (see supplement material) and reproduce all figures and tables under an appropriate personal computing environment. For non-commercial purposes, readers may modify the code to produce figures and tables that are not presented in this article. For instance, readers may wish to produce tables and figures for countries or regions other than the WHO Western Pacific Region.

# Questions this analysis is trying to answer
How much TB still exists in WPR?
Is the amount of TB going up, coming down, or staying stable?
Where is TB clustering in the region?
Is the clustering staying the same or changing, if changing how?
where is the amount of TB coming down and at what rate? Is it going up anywhere?
Are child cases being found? What about among women? How is the disease progressing with aging countries?
What do we know about these populations and how is TB reflecting or being affected?
Where is TB still spreading and where is it only reactivation? Where are chunks of the population likely not infected?
What is the relationship between wealth (GDP) and TB motality at the national level? What are the macro associations (causes) for TB?
What is the relationship between Migrant stock and TB?
Which programmes are ahead of the game in finding cases?
Which programmes treat the best? If you get TB, which country do you hope you live in?

Structure
table of latest year
map of latest year
trend by group

HIGH BURDEN
relationship between GDP and mort
aging plot
notif trend panel plot
treatment success
financing?
HIV?

PACIFIC
scatter of notif rate by abs num 
MDR in the pacific

OTHER
scatter of rate and abs num
scatter of rate and migrant stock, maybe include foreign born var
aging plot


Results
--------------------------------------------------------


### Case notification






In 2013, countries and areas in the Region reported 1.4 million people with TB disease ([Table 1](#t-notif)). This accounts for 23% of the global burden. Of these cases, 97.6% (1 342 404) were new episodes of TB disease (either new or relapse cases). Within the Region China accounts for 62% (855 241) of the caseload with the Philippines and Viet Nam following with 18% (244 392) and 7.4% (102 196), respectively. TB notification rates, expressed as cases per 100 000 population, vary substantially in the Region with the highest rates found in Kiribati, Papua New Guinea, Marshall Islands, Cambodia and the Philippines  with 398, 309, 283, 249 and 234 per 100 000 population, respectively ([Table 1](#t-notif), [Figure 1](#m-notif)). 

--------------------------------------------------------

<a id="t-notif"></a> 
**Table 1. Tuberculosis case notification from countries and areas of the Western Pacific Region, 2013**  
<!-- html table generated in R 3.1.2 by xtable 1.7-4 package -->
<!-- Wed Apr 22 16:21:29 2015 -->
<table border=0 rules=rows width=900 cellpadding=5>
  <TR> 
  <TD colspan='4' style='border-right: solid 2px black;'></TD> 
  <TH colspan='3' style='border-right: solid 2px black;'>NEW OR PREVIOUS TREATMENT HISTORY UNKNOWN</TH> 
  <TH colspan='3' style='border-right: solid 2px black;'>RELAPSE</TH> 
  <TD></TD> 
  </TR> 
  <TR> 
  <TD></TD> 
  <TD>TOTAL NOTIFIED</TD> 
  <TD>NEW AND RELAPSE <sup>a</sup></TD> 
  <TD style='border-right: solid 2px black;'>RETREATMENT EXCLUDING RELAPSE</TD> 
  <TD>PULMONARY BACTERIOLOGICALLY CONFIRMED</TD>
  <TD>PULMONARY CLINICALLY DIAGNOSED</TD> 
  <TD style='border-right: solid 2px black;'>EXTRAPULMONARY</TD> 
  <TD>PULMONARY BACTERIOLOGICALLY CONFIRMED</TD>
  <TD>PULMONARY CLINICALLY DIAGNOSED</TD> 
  <TD style='border-right: solid 2px black;'>EXTRAPULMONARY</TD> 
  <TD>PERCENTAGE OF PULMONARY CASES BACTERIOLOGICALLY CONFIRMED</TD> 
  </TR> 
  <TR> <tr> <td> American Samoa </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td> – </td> </tr>
  <tr> <td> Australia </td> <td> 1 256 </td> <td> 1 250 </td> <td> 6 </td> <td> 612 </td> <td> 86 </td> <td> 512 </td> <td> 26 </td> <td> 1 </td> <td> 13 </td> <td> 88 </td> </tr>
  <tr> <td> Brunei Darussalam </td> <td> 212 </td> <td> 212 </td> <td> 0 </td> <td> 149 </td> <td> 9 </td> <td> 48 </td> <td> 6 </td> <td> 0 </td> <td> 0 </td> <td> 95 </td> </tr>
  <tr> <td> Cambodia </td> <td> 39 055 </td> <td> 37 743 </td> <td> 1 312 </td> <td> 14 082 </td> <td> 9 069 </td> <td> 14 203 </td> <td> 389 </td> <td> 0 </td> <td> 0 </td> <td> 61 </td> </tr>
  <tr> <td> China </td> <td> 855 241 </td> <td> 847 176 </td> <td> 8 065 </td> <td> 275 915 </td> <td> 512 469 </td> <td> 30 767 </td> <td> 28 025 </td> <td>  </td> <td>  </td> <td> 37 </td> </tr>
  <tr> <td> Hong Kong Special Administrative Region (China) </td> <td> 4 773 </td> <td> 4 744 </td> <td> 29 </td> <td> 2 439 </td> <td> 1 033 </td> <td> 893 </td> <td> 259 </td> <td> 66 </td> <td> 54 </td> <td> 71 </td> </tr>
  <tr> <td> Macao Special Administrative Region (China) </td> <td> 437 </td> <td> 433 </td> <td> 4 </td> <td> 286 </td> <td> 61 </td> <td> 57 </td> <td> 25 </td> <td> 3 </td> <td> 1 </td> <td> 83 </td> </tr>
  <tr> <td> Cook Islands </td> <td> 2 </td> <td> 2 </td> <td> 0 </td> <td> 1 </td> <td> 0 </td> <td> 1 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 100 </td> </tr>
  <tr> <td> Fiji </td> <td> 269 </td> <td> 254 </td> <td> 15 </td> <td> 106 </td> <td> 74 </td> <td> 71 </td> <td> 3 </td> <td> 0 </td> <td> 0 </td> <td> 60 </td> </tr>
  <tr> <td> French Polynesia </td> <td> 61 </td> <td> 52 </td> <td> 9 </td> <td> 29 </td> <td> 8 </td> <td> 6 </td> <td> 7 </td> <td> 1 </td> <td> 1 </td> <td> 80 </td> </tr>
  <tr> <td> Guam </td> <td> 48 </td> <td> 48 </td> <td> 0 </td> <td> 22 </td> <td> 22 </td> <td> 4 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 50 </td> </tr>
  <tr> <td> Japan </td> <td> 20 495 </td> <td> 20 495 </td> <td>  </td> <td> 12 758 </td> <td> 2 201 </td> <td> 4 274 </td> <td> 831 </td> <td> 182 </td> <td> 249 </td> <td> 85 </td> </tr>
  <tr> <td> Kiribati </td> <td> 420 </td> <td> 407 </td> <td> 13 </td> <td> 128 </td> <td> 159 </td> <td> 85 </td> <td> 22 </td> <td> 10 </td> <td> 3 </td> <td> 47 </td> </tr>
  <tr> <td> Lao People's Democratic Republic </td> <td> 4 214 </td> <td> 4 130 </td> <td> 84 </td> <td> 3 004 </td> <td> 541 </td> <td> 392 </td> <td> 193 </td> <td> 0 </td> <td> 0 </td> <td> 86 </td> </tr>
  <tr> <td> Malaysia </td> <td> 24 071 </td> <td> 23 417 </td> <td> 654 </td> <td> 13 641 </td> <td> 5 640 </td> <td> 3 046 </td> <td> 676 </td> <td> 307 </td> <td> 107 </td> <td> 71 </td> </tr>
  <tr> <td> Marshall Islands </td> <td> 153 </td> <td> 149 </td> <td> 4 </td> <td> 70 </td> <td> 48 </td> <td> 28 </td> <td> 3 </td> <td> 0 </td> <td> 0 </td> <td> 60 </td> </tr>
  <tr> <td> Micronesia, Federated States of </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td> – </td> </tr>
  <tr> <td> Mongolia </td> <td> 4 733 </td> <td> 4 331 </td> <td> 402 </td> <td> 1 622 </td> <td> 733 </td> <td> 1 756 </td> <td> 220 </td> <td> 0 </td> <td> 0 </td> <td> 72 </td> </tr>
  <tr> <td> Nauru </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td> – </td> </tr>
  <tr> <td> New Caledonia </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td>  </td> <td> – </td> </tr>
  <tr> <td> New Zealand </td> <td> 277 </td> <td> 272 </td> <td> 5 </td> <td> 125 </td> <td> 15 </td> <td> 125 </td> <td> 6 </td> <td> 0 </td> <td> 1 </td> <td> 90 </td> </tr>
  <tr> <td> Niue </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> – </td> </tr>
  <tr> <td> Northern Mariana Islands </td> <td> 33 </td> <td> 33 </td> <td> 0 </td> <td> 15 </td> <td> 12 </td> <td> 1 </td> <td> 5 </td> <td> 0 </td> <td> 0 </td> <td> 62 </td> </tr>
  <tr> <td> Palau </td> <td> 8 </td> <td> 8 </td> <td> 0 </td> <td> 7 </td> <td> 0 </td> <td> 1 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 100 </td> </tr>
  <tr> <td> Papua New Guinea </td> <td> 24 860 </td> <td> 22 657 </td> <td> 2 203 </td> <td> 3 150 </td> <td> 9 390 </td> <td> 9 919 </td> <td> 198 </td> <td>  </td> <td>  </td> <td> 26 </td> </tr>
  <tr> <td> Philippines </td> <td> 244 392 </td> <td> 229 918 </td> <td> 14 474 </td> <td> 97 353 </td> <td> 123 510 </td> <td> 4 386 </td> <td> 4 669 </td> <td> 0 </td> <td> 0 </td> <td> 45 </td> </tr>
  <tr> <td> Republic of Korea </td> <td> 45 292 </td> <td> 41 579 </td> <td> 3 713 </td> <td> 18 860 </td> <td> 9 860 </td> <td> 7 369 </td> <td> 2 957 </td> <td> 1 699 </td> <td> 834 </td> <td> 65 </td> </tr>
  <tr> <td> Samoa </td> <td> 22 </td> <td> 22 </td> <td> 0 </td> <td> 11 </td> <td> 4 </td> <td> 7 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 73 </td> </tr>
  <tr> <td> Singapore </td> <td> 2 172 </td> <td> 2 164 </td> <td> 8 </td> <td> 1 103 </td> <td> 647 </td> <td> 278 </td> <td> 92 </td> <td> 28 </td> <td> 16 </td> <td> 64 </td> </tr>
  <tr> <td> Solomon Islands </td> <td> 368 </td> <td> 360 </td> <td> 8 </td> <td> 136 </td> <td> 105 </td> <td> 114 </td> <td> 5 </td> <td> 0 </td> <td> 0 </td> <td> 57 </td> </tr>
  <tr> <td> Tokelau </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> – </td> </tr>
  <tr> <td> Tonga </td> <td> 10 </td> <td> 10 </td> <td> 0 </td> <td> 8 </td> <td> 0 </td> <td> 2 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 100 </td> </tr>
  <tr> <td> Tuvalu </td> <td> 18 </td> <td> 18 </td> <td> 0 </td> <td> 10 </td> <td> 6 </td> <td> 2 </td> <td>  </td> <td>  </td> <td>  </td> <td> 62 </td> </tr>
  <tr> <td> Vanuatu </td> <td> 123 </td> <td> 123 </td> <td> 0 </td> <td> 42 </td> <td> 24 </td> <td> 56 </td> <td> 1 </td> <td> 0 </td> <td> 0 </td> <td> 64 </td> </tr>
  <tr> <td> Viet Nam </td> <td> 102 196 </td> <td> 100 395 </td> <td> 1 801 </td> <td> 50 607 </td> <td> 24 403 </td> <td> 18 326 </td> <td> 7 059 </td> <td>  </td> <td>  </td> <td> 70 </td> </tr>
  <tr> <td> Wallis and Futuna Islands </td> <td> 2 </td> <td> 2 </td> <td> 0 </td> <td> 2 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 0 </td> <td> 100 </td> </tr>
  <tr> <td> Western Pacific Region
                                         </td> <td> 1 375 213 </td> <td> 1 342 404 </td> <td> 32 809 </td> <td> 496 293 </td> <td> 700 129 </td> <td> 96 729 </td> <td> 45 677 </td> <td> 2 297 </td> <td> 1 279 </td> <td> 44 </td> </tr>
   <TR> <TD colspan=11>Blank cells indicate data not reported.<br>
  <sup>a</sup> NEW AND RELAPSE includes cases for which the treatment history is unknown.</TD></TR> </table>


--------------------------------------------------------

<a id="m-notif"></a> 
**Figure 1 Tuberculosis case notification rate (new and relapse) per 100 000 population in countries and areas of the Western Pacific Region, 2013**  
![](RegTBfigsTables_files/figure-html/m-notif-1.png) 

<font size="1">The boundaries shown and the designations used on this map do not imply the expression of any opinion whatsoever on the part of the World Health Organization concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. White lines on maps represent approximate border lines for which there may not yet be full agreement.</font>

--------------------------------------------------------



Case notification rates in high-burden countries increased dramatically for six years starting in 2002 peaking at 83 cases per 100 000 population. A smaller increase in rate of all forms of TB occurred between 2010 and 2012 ([Figure 2](#f-notif-trend)). The overall trend for the Region follows that of the high-burden countries. TB in the Pacific fluctuated between 44 and 54 for all forms of TB and NA and NA for smear-positive cases with no distinct trend. Other countries in the Region have held more steady rates with the rate of all forms at 38.


--------------------------------------------------------

<a id="f-notif-trend"></a> 
**Figure 2 Tuberculosis case notification rate (all forms and new laboratory-confirmed) per 100 000 population in the Western Pacific Region, 2000--2013**  
![](RegTBfigsTables_files/figure-html/f-notif-trend-1.png) 

--------------------------------------------------------

### High-burden countries




The seven high-burden countries of the Region account for 93% of the estimated caseload. All seven countries have experienced a decrease in TB mortality since 2000 as well as an overall increasing trend in gross domestic product (GDP) ([Figure 3](#f-gdp-hb)).

--------------------------------------------------------

<a id="f-gdp-hb"></a> 
**Figure 3 TB mortality^a^ per 100 000 population compared with gross domestic product (GDP) per capita in seven high-burden countries of the Western Pacific Region, 2000--2013**  
![](RegTBfigsTables_files/figure-html/f-gdp-hb-1.png) 

^a^ TB mortality excludes deaths due to HIV in accordance with ICD-10.




--------------------------------------------------------

### Distribution by age and sex




[Figure 4](#f-agesex-bar) shows age group and sex-specific case notification rates of new laboratory-confirmed cases for the countries with a high burden of TB in the Region with available data (note that the scale of the vertical axis is different for each country). Many countries follow a typical pattern for cross-sectional observations with increasing notification rates towards older populations except in Mongolia. In general, males are more affected than females with male-to-female TB ratios as high as three as seen in Viet Nam.

--------------------------------------------------------

<a id="f-agesex-bar"></a> 
**Figure 4 Age group- and sex-specific notification rates (per 100 000 population) of new and relapse TB cases in countries with a high-burden of TB in the Western Pacific Region, 2013**  
![](RegTBfigsTables_files/figure-html/f-agesex-bar-1.png) 

--------------------------------------------------------

[Figure 5](#f-agesex) shows trends of notification rates of new laboratory-confirmed cases of age- and sex-specific groups in the seven countries with a high-burden of TB from 2000–2013. Some countries demonstrated a declining trend of case notification for almost all age- and sex- groups, such as Cambodia, China and Viet Nam while others showed a less apparent trend except Papua New Guinea with a sharply increasing trend.

--------------------------------------------------------

<a id="f-agesex"></a> 
**Figure 5 Trend of age group- and sex-specific notification rates (per 100 000 population) of new and relapse^a^ tuberculosis cases in seven countries with a high-burden of TB in the Western Pacific Region, 2000--2013**  
![](RegTBfigsTables_files/figure-html/f-agesex-1.png) 

^a^ Rates prior to 2013 include new cases only.

--------------------------------------------------------

### Treatment outcomes



The Region continued observing treatment success rates beyond the target of 85% ([Figure 6](#f-txout-bar)), and the rate has been at 85% or higher over the past several years. Across the Region, 16 countries and areas reached or maintained the 85% treatment success target. Among the countries with a high burden of TB, the treatment success rate was highest in China (95%) followed by Cambodia (94%), Viet Nam (91%), the Lao People's Democratic Republic (90%), Mongolia (88%) and the Philippines (88%). The treatment success rate of Papua New Guinea was the lowest at 68% with approximately a quarter of the 2012 cohort either lost to follow-up or un-evaluated.

--------------------------------------------------------

<a id="f-txout-bar"></a> 
**Figure 6 Trend of treatment outcome expressed as a proportion among new laboratory-confirmed cases in the Western Pacific Region, 2000--2012**  
![](RegTBfigsTables_files/figure-html/f-txout-bar-1.png) 

--------------------------------------------------------

### TB/HIV co-infection and collaborative activities



There has been some progress in reporting of information on TB/HIV co-infection and collaborative activities in the last several years. [Figure 7](#f-tbhiv) summarizes four basic indicators (HIV testing, HIV positivity rate, co-trimoxazole preventive therapy (CPT) coverage and  antiretroviral 
therapy (ART) coverage) for the seven countries with a high burden of TB. Cambodia reported the most comprehensive data completeness and programmatic progress. The coverage of HIV testing, CPT and ART progressively increased with a steady decrease in the proportion of HIV positive individuals among TB patients.

--------------------------------------------------------

<a id="f-tbhiv"></a> 
**Figure 7 Progress in TB/HIV activities in seven countries in the Western Pacific Region with a high burden of TB, 2005--2013**  
![](RegTBfigsTables_files/figure-html/f-tbhiv-1.png) 

--------------------------------------------------------

### Pacific Island Countries





The seven high-burden countries of the Region account for 93% of the estimated caseload. All seven countries have experienced a decrease in TB mortality since 2000 as well as an overall increasing trend in gross domestic product (GDP) ([Figure 3](#f-gdp-hb)).

--------------------------------------------------------

<a id="f-notif-pac"></a> 
**Figure 8 New and relapse TB cases per 100 000 population compared with absolute number of cases in 14 Pacific Island countries^a^ of the Western Pacific Region, 2013**  
![](RegTBfigsTables_files/figure-html/f-notif-pac-1.png) 

^a^ Niue and Tokelau reported 0 TB cases and American Samoa, Micronesia (Federated States of), Nauru, and New Caledonia did not report data for 2013. 

--------------------------------------------------------





--------------------------------------------------------


### Other countries




The seven high-burden countries of the Region account for 93% of the estimated caseload. All seven countries have experienced a decrease in TB mortality since 2000 as well as an overall increasing trend in gross domestic product (GDP) ([Figure 3](#f-gdp-hb)).

--------------------------------------------------------

<a id="f-notif-oth"></a> 
**Figure 9 New and relapse TB cases per 100 000 population compared with absolute number of cases in 126 countries of the Western Pacific Region, 2000--2013**  
![](RegTBfigsTables_files/figure-html/f-notif-oth-1.png) ![](RegTBfigsTables_files/figure-html/f-notif-oth-2.png) 

^a^ Niue and Tokelau reported 0 TB cases and American Samoa, Micronesia (Federated States of), Nauru, and New Caledonia did not report data for 2013. 

--------------------------------------------------------




The seven high-burden countries of the Region account for 93% of the estimated caseload. All seven countries have experienced a decrease in TB mortality since 2000 as well as an overall increasing trend in gross domestic product (GDP) ([Figure 10](#f-migrant-other)).

--------------------------------------------------------

<a id="f-migrant-other"></a> 
**Figure 10 Foreign-born TB cases and international migrant stock in nine countries of the Western Pacific Region, 2007--2013**  
![](RegTBfigsTables_files/figure-html/f-migrant-other-1.png) ![](RegTBfigsTables_files/figure-html/f-migrant-other-2.png) 

International migrant stock data from World Development Indicators, World Bank.

--------------------------------------------------------





[Figure 11](#f-agesex-oth-bar) shows age group and sex-specific case notification rates of new laboratory-confirmed cases for the countries with a high burden of TB in the Region with available data (note that the scale of the vertical axis is different for each country). Many countries follow a typical pattern for cross-sectional observations with increasing notification rates towards older populations except in Mongolia. In general, males are more affected than females with male-to-female TB ratios as high as three as seen in Viet Nam.

--------------------------------------------------------

<a id="f-agesex-oth-bar"></a> 
**Figure 11 Age group- and sex-specific notification rates (per 100 000 population) of new and relapse TB cases in countries with a high-burden of TB in the Western Pacific Region, 2013**  
![](RegTBfigsTables_files/figure-html/f-agesex-oth-bar-1.png) 


--------------------------------------------------------

<a id="f-agesex-oth"></a> 
**Figure 12 Trend of age group- and sex-specific notification rates (per 100 000 population) of new and relapse^a^ tuberculosis cases in seven countries with a high-burden of TB in the Western Pacific Region, 2000--2013**  
![](RegTBfigsTables_files/figure-html/f-agesex-oth-1.png) 

^a^ Rates prior to 2013 include new cases only.

![](RegTBfigsTables_files/figure-html/f-agesex-oth2-1.png) 



Discussion
--------------------------------------------------------



Overall, in 2013, countries and areas of the Western Pacific Region reported 1.3 million new and relapse TB cases (all forms) and a case notification rate of 72 per 100 000 population, a level similar to the past several years.

It has been known that the rapid increase in case notification between 2002 and 2007 was due to several positive programmatic developments in many countries in the Region such as completion and consolidation of the WHO DOTS strategy expansion; improvement in case reporting, including electronic reporting systems; and efforts to engage all health care providers.[@VanMaaren2007] Particularly, renewal of infectious disease-related legislation and the establishment of an internet-based disease notification system in China made a substantial contribution to the progress.[@Wang2007]

Although the case notification for all forms of TB appears to be flat since 2007, it is important to note that the new laboratory-confirmed case notification rates demonstrate a clear declining trend ([Figure 2](#f-notif-trend)). A possible interpretation is that the true TB incidence has been declining while overall case detection is static because intensified programmatic efforts by national TB programmes for early and increased case detection include smear-negative and extra pulmonary TB. The latest WHO estimates support this explanation with estimated incidence rates showing a consistent, rapidly-declining trend[@WorldHealthOrganization2014] ([Figure 13](#f-est)). 

--------------------------------------------------------

<a id="f-est"></a> 
**Figure 13 Tuberculosis case notification rate per 100 000 population, estimated incidence^a^, prevalence and mortality in the Western Pacific Region, 1990--2013**  
![](RegTBfigsTables_files/figure-html/f-est-1.png) 

^a^ Red line with uncertainty band: incidence, black line: notified new and relapse cases.

--------------------------------------------------------

In any country where a rapid demographic change is underway, overall notification rates may not reflect the true disease trend in communities. For instance, an overall case notification trend may appear to be stable because decreasing incidences can be cancelled out by a rapidly increasing proportion of the older population. For this reason, the examination of age- and sex- specific case notification rates is more informative and provides insights for understanding the underlying epidemiological process in a given setting.

The typical pattern of linear increase of notification towards the older populations (such as shown in some countries in [Figure 4](#f-agesex-bar)) has been explained as a widely observed phenomena under a stable TB control situation,[@Rieder1999] reflecting a high annual risk of TB infection in the past when the older population was young. Atypical patterns shown for Papua New Guinea and Mongolia, particularly relatively high notification rates among young and female groups, warrant further investigation. Time trend analysis for age- and sex- notification rates ([Figure 5](#f-agesex)) is useful to detect any specific subgroups among which TB transmission and/or disease progression is particularly active.

One of the critical shortcomings of these analyses is a gross lack of morbidity information among small children because the data is limited to laboratory-confirmed cases only. Since the 2006 revision of WHO recording and reporting forms,[@WorldHealthOrganization2006] the number of countries reporting age- and sex-disaggregated data for smear-negative and extra pulmonary cases is increasing and will enable a better assessment of the TB burden among children in future analysis.

HIV infection fuels the TB epidemic, particularly in countries and areas with a high burden of TB. The overall percentage of TB patients tested for HIV in the Region still remained low. However, the figure has substantially increased in the last several years particularly in Cambodia, Viet Nam and the Lao People's Democratic Republic. Essential services such as co-trimoxazole prophylaxis and isoniazid preventive therapy have also expanded in many countries in the Region.

This report provides a snapshot of the epidemiological and programmatic situation of TB in the Western Pacific Region based on the case notification data in 2013. As for any disease surveillance system, the analysis of surveillance data has inherent limitations. TB surveillance covers populations served by care providers linked with the national TB programme. Ideally this would include all known cases in the country; in practice the proportion of cases diagnosed outside of the TB programme and included in national reporting varies depending on the legal framework in the country. The WHO TB Impact Measurement Task Force recommends that countries continuously improve surveillance systems until reported cases can be considered a reliable proxy for incidence.[@WorldHealthOrganization2009; @WorldHealthOrganization2014b] A careful assessment is needed of programmatic progress in the country and the quality of surveillance data when interpreting these findings.

TB surveillance continues to be an important source of information for assessing the situation and measuring the progress for decision-making. WHO Regional Office for the Western Pacific will continue to conduct regional analyses on various topics related to TB epidemiology and programmatic progress, as well as provide support to countries to conduct epidemiological and programmatic assessment at national and subnational levels.

--------------------------------------------------------

**<em>Funding</em>**

None.

--------------------------------------------------------

**<em>Conflict of interest</em>**

None declared.

--------------------------------------------------------

**<em>Acknowledgement</em>**

The authors are very grateful to the national tuberculosis control programmes of the countries of the Western Pacific Region.

--------------------------------------------------------

**<em>References:</em>**



