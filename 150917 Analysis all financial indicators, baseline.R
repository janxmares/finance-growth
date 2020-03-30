### Jan Mares, Finance and Growth: BMA Evidence
### September 2015
### Jan Mares
### Financial indicators 1960-2011, means;

### Packages 
library(BMS)
library(data.table)
library(xtable)
library(here)
library(ggplot2)

#
#
#

### Load data
hhm_data <- data.table(read.table(file=here("data/hhm_data.csv"), sep=",", header=T))

# Read the data on total credit (BIS)
total.credit <- read.csv(here("data/BIS total credit final.csv"), stringsAsFactors = F, header = T)

# Merge original data with total credit
hhm_data <- merge(hhm_data,total.credit, by = c("iso3c"), all.x = T)

# Removing vars with a lot of missing observations, text columns & variables not used in the analysis
hhm_data_baseline <- hhm_data[, c("Accounts1000","MarketCap.WOtop10","MarketVol","Country",
                         "iso3c","iso2c","findex","finrev","Lrev","sum_cr","max_cr",
                         "legor_uk","legor_ge","legor_sc","legor_fr","group","fst",
                         "statehist","ATMs1000","BankBranches","totalcredit"):=NULL]

#
#
#

# Run BMA, baseline
bma_finind <- bms(hhm_data_baseline, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd", user.int= F)

#
#

# Estimation with subset
hhm_data_subset <- hhm_data[, j = .(GDP60, Mining, RFEXDist, Confucian, LifeExp,
                                    Buddha, NetInterestMargin, EquipInv, BankZscore,
                                    MarketTurn, Privatecredit, MarketCap, totalcredit)]

# Keep only the complete cases
hhm_data_subset <- hhm_data_subset[complete.cases(hhm_data_subset),]

# Estimation
bma_finind_tc <- bms(hhm_data_subset, mprior = "uniform", g = "hyper")

# Plot - private credit vs. total credit
cor(hhm_data_subset$Privatecredit, hhm_data_subset$totalcredit)
ggplot(data=hhm_data_subset, aes(x=Privatecredit, y=totalcredit)) + geom_smooth(method='lm') + geom_point()

# fit linear regression of total credit on bank privatecredit
pc_tc <- lm(totalcredit ~ Privatecredit, data = hhm_data_subset)

# fit the values for the original data
# hhm_data[is.na(totalcredit), totalcredit := predict(pc_tc, hhm_data[is.na(totalcredit),])]
hhm_data[, totalcredit := predict(pc_tc, hhm_data)]

# Reestimate using the total credit
hhm_data_tc <- hhm_data[, c("Accounts1000","MarketCap.WOtop10","MarketVol","Country",
                         "iso3c","iso2c","findex","finrev","Lrev","sum_cr","max_cr",
                         "legor_uk","legor_ge","legor_sc","legor_fr","group","fst",
                         "statehist","ATMs1000","BankBranches","Privatecredit"):=NULL]

# Run BMA
bma_finind <- bms(hhm_data_tc, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd", user.int= F)

# Results
summary(bma_finind)
coef(bma_finind, exact = T)

#
#

# BMA, random model prior
# bma_finind <- bms(hhm_data, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
#                             nmodel=5000, mcmc="bd", user.int= F)

# BMA, baseline, reverse jump
bma_finind_revjump <- bms(hhm_data, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="rev.jump", user.int= F)


# Run BMA, UIP
bma_finind_uip <- bms(hhm_data, burn=1000000, iter=3000000, mprior = "uniform", g = "UIP",
                      nmodel=5000, mcmc="bd", user.int= F)

# BMA, UIP, reverse jump
bma_finind_uip_revjump <- bms(hhm_data, iter=3000000, burn=1000000, mprior = "uniform", g = "UIP",
                            nmodel=5000, mcmc="rev.jump", user.int= F)

# Results, coefficients
summary(bma_finind, exact=T)
coef(bma_finind, exact=T)

# Results, coefficients
summary(bma_finind_uip, exact=T)
coef(bma_finind_uip, exact=T)

# Results, coefficients
summary(bma_finind_revjump, exact=T)
coef(bma_finind_revjump, exact=T)

# Save results
save(bma_finind, file=paste0(resultsdir, "Means 1960-2011, all fin. ind baseline.RData"))
save(bma_finind_uip, file=paste0(resultsdir, "Means 1960-2011, all fin. ind uip.RData"))

# Reverse jump results
save(bma_finind_revjump, file=paste0(resultsdir, "Means 1960-2011, all fin. ind baseline reverse jump.RData"))
save(bma_finind_uip_revjump, file=paste0(resultsdir, "Means 1960-2011, all fin. ind uip reverse jump.RData"))


#load(file=paste0(resultsdir,"Means 1960-2011, all fin. ind baseline.RData"))
#
#
#

# Creating ouput tables

# Variable names setup
explvarshort <- c("GDP60","Mining","Confucian","LifeExp","BlMktPm","RFEXDist","SubSahara",
                  "stdBMP","EquipInv","Buddha","LabForce","French","Muslim","English",
                  "NequipInv","LatAmerica","RuleofLaw","Hindu","EthnoL","Abslat","Foreign",
                  "Catholic","Brit","WorkPop","PublEdupct","Privatecredit","YrsOpen","Spanish",
                  "Jewish","PrScEnroll","Protestants","EcoOrg","Age","OutwarOr","HighEnroll",
                  "Area","RevnCoup","CivlLib","WarDummy","PrExports","Popg","PolRights",
                  "MarketCap","MarketTurn","BankZscore","NetInterestMargin","Privatecredit#Privatecredit",
                  "NetInterestMargin#NetInterestMargin","MarketTurn#MarketTurn","BankZscore#BankZscore",
                  "MarketCap#MarketCap","Privatecredit#NetInterestMargin","BankBranches")

explvarnames <- c("GDP level in 1960","Fraction GDP in mining","Fraction Confucian","Life expectancy","Black market premium",
                  "Exchange rate distortions", "Sub-Sahara dummy","SD of black market premium",
                  "Equipment investment","Fraction Buddhist","Size of labour force","French colony dummy",
                  "Fraction Muslim","Fraction of pop. speaking English", "Non-equipment investment",
                  "Latin America dummy", "Rule of law","Fraction Hindu","Ethnolinguistic fractionalization",
                  "Absolute latitude","Fraction speaking foreign language","Fraction Catholic",
                  "British colony dummy","Ratio of workers to population","Public education share",
                  "Private credit","Number of years of open economy","Spanish colony dummy",
                  "Fraction Jewish","Primary school enrolment","Fraction Protestants","Degree of capitalism",
                  "Age","Outward orientation","High school enrolment","Area","Revolutions and coups",
                  "Civil liberties","War dummy","Primary exports","Population growth","Political rights",
                  "Market capitalization","Market turnover","Bank Z-score","Net interest margin",
                  "Private credit sq.","Net interest margin sq.","Market turnover sq.","Bank Z-score sq",
                  "Market capitalization sq.", "Private credit*Net interest margin","Bank branches/1000 inh.")

# Bind short and long variables names together
varnames <- cbind(explvarshort, explvarnames)  

# Results hyper prior
results_all <- coef(bma_finind, exact=T)

# List of variables sorted by PIP 
varlist <- data.frame(explvarshort=row.names(results_all))

# Creating concordance table
concordance <- merge(varlist, varnames, by="explvarshort", sort=F)

# Inputt variable names
row.names(results_all) <- concordance$explvarnames

# LaTeX output with proper varnames
xtable(results_all[,1:3], digits=c(0,2,5,5))

# PART FOR COMAPRISON FIGURE, WITH REVERSE JUMP AND UIP SAMPLER!

# load(file=paste0(resultsdir,"Means 1960-2011, all fin. ind baseline.RData"))
# load(file=paste0(resultsdir,"Means 1960-2011, all fin. ind uip.RData"))
# load(file=paste0(resultsdir, "Means 1960-2011, all fin. ind baseline reverse jump.RData"))
# load(file=paste0(resultsdir, "Means 1960-2011, all fin. ind uip reverse jump.RData"))

# #rename long reg.names
# bma_finind$reg.names <- replace(bma_finind$reg.names,bma_finind$reg.names=="NetInterestMargin", "IntMargin")
# bma_finind_uip$reg.names <- replace(bma_finind_uip$reg.names,bma_finind_uip$reg.names=="NetInterestMargin", "IntMargin")
# bma_finind_revjump$reg.names <- replace(bma_finind_revjump$reg.names,bma_finind_revjump$reg.names=="NetInterestMargin", "IntMargin")
# bma_finind_uip_revjump$reg.names <- replace(bma_finind_uip_revjump$reg.names,bma_finind_uip_revjump$reg.names=="NetInterestMargin", "IntMargin")


# # Writing EMF file,
# emf(file = paste0(folderfig,"plotCompall6011_sampler.emf"), width = 9, height = 5, bg = "transparent", fg = "black", family = "Arial", custom.lty = FALSE)
# plotComp(bma_finind, bma_finind_uip, bma_finind_revjump,bma_finind_uip_revjump,
#          lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = TRUE,
#          add.grid = F, do.par = F, cex.axis=0.8, cex.xaxis=0.8)
# dev.off()


