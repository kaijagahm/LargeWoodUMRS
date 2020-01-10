################################################################################################################################
# 
# Edited version of "descriptive_stats_clean.R" by Kaija Gahm, 11 Aug 2018 
# Deprecated as of January 2020: shifted over to descriptive_stats.Rmd.
#
# output: 
#
################################################################################################################################

# Source libraries and functions
source("Analysis/R_scripts/libraries.R")
source("Analysis/R_scripts/ownfunctions.R")

# Load data (combined upper/lower river)
load("Analysis/data/outputs/sites_upperlower.Rda")
load("Analysis/data/outputs/sites_ur.Rda")
load("Analysis/data/outputs/sites_lr.Rda")

# Data Cleaning
## Remove the river mile column
sites_upperlower <- sites_upperlower %>% select(-river_mile)

## Exclude strata: BWC-O and MCB-W
sites_upperlower <- sites_upperlower %>% filter(stratum %notin% c("BWC-O", "MCB-W")) %>% droplevels()

## Exclude sites NA for snag and/or pool
sites_upperlower <- sites_upperlower %>% filter(!is.na(snag), !is.na(pool))

# Summaries
## Overall
summary_all <- sites_upperlower %>% summarize(npoints = n(),
                                              wood = sum(snag == 1),
                                              nowood = sum(snag == 0),
                                              propwood = wood/npoints)



## By pool
summary_pool <- sites_upperlower %>% 
  group_by(pool) %>%
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood =  sum(snag == 0),
            propwood = wood/npoints)

## Test for significant differences in wood proportion
chisq.test(summary_pool[,c("wood", "nowood")]) # yes, these are significantly different

## Pairwise comparisons
pairwise.prop.test(x = summary_pool$wood, n = summary_pool$npoints, 
                   p.adjust.method = "bonferroni")



## By year and pool
summary_yearpool <- sites_upperlower %>% group_by(pool, year) %>%
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood =  sum(snag == 0),
            propwood = wood/npoints)

### plot
summary_yearpool %>% ggplot(aes(x = year, y = propwood, col = pool, group = pool))+
  geom_line()


# Summarize by stratum
summary_stratum <- sites_upperlower %>% group_by(stratum) %>% 
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood = sum(snag == 0),
            propwood = wood/npoints)

# Calculate confidence intervals for these proportions
cis <- BinomCI(summary_stratum$wood, n = summary_stratum$npoints, conf.level = 0.95)
summary_stratum <- cbind(summary_stratum, cis[,2:3])

# Test for significant differences
chisq.test(summary_stratum[,2:3])

# Pairwise comparisons
pairwise.prop.test(x = summary_stratum$wood, n = summary_stratum$npoints, p.adjust.method = "bonferroni")

# Plot of proportions and confidence intervals by stratum
ggplot(data = summary_stratum, aes(x = stratum, y = propwood))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = lwr.ci, 
                    ymax = upr.ci), 
                width = 0,
                size = 1.2)+
  theme_bw() +
  theme(text = element_text(size = 16))+
  ylab("Proportion of points with wood")+
  xlab("Habitat Stratum")+
  ggtitle("Wood proportion by stratum")

# Summarize by stratum and pool
summary_stratumpool <- sites_upperlower %>% group_by(pool, stratum) %>%
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood = sum(snag == 0),
            propwood = wood/npoints) %>% as.data.frame()

# Test for significant differences
chisq.test(summary_stratumpool[,3:4])

# Calculate confidence intervals for the proportions
cis <- BinomCI(summary_stratumpool$wood, 
               n = summary_stratumpool$npoints, 
               conf.level = 0.95) %>% 
  as.data.frame() %>% select(lwr.ci, upr.ci)
summary_stratumpool <- cbind(summary_stratumpool, cis[,1:2])

# Plot of proportions and confidence intervals by stratum and pool
summary_stratumpool %>% ggplot(aes(x = stratum, y = propwood, col = pool))+
  geom_point(size = 3, position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = lwr.ci, ymax = upr.ci), 
                width = 0,
                size = 1.2,
                position = position_dodge(width = 0.3))+
  theme(text = element_text(size = 16))+
  ylab("Proportion of points with wood")+
  xlab("Habitat Stratum")

# examine trends in stratum through time, by pool
summary_poolyearstratum <- sites_upperlower %>% 
  group_by(pool, year, stratum) %>% 
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood = sum(snag == 0),
            propwood = wood/npoints) %>%
  as.data.frame()

# Plot of proportions and confidence intervals by stratum and pool
summary_poolyearstratum %>% ggplot(aes(x = year, y = propwood))+
  geom_line(size = 1.5, aes(linetype = stratum))+
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "twodash", "dotdash", "solid"))+
  ylim(0,1)+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion of points with wood")+
  theme(text = element_text(size = 20))+
  facet_wrap(~pool)

summary_poolyearstratum %>% ggplot(aes(x = year, y = propwood, group = stratum))+ #,color=stratum))
  geom_line(size = 0.65, aes(linetype = stratum))+ #c("solid","twodash","longdash","dotted","dotdash"))+
  ylim(0,1)+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion of points with wood")+
  #scale_color_manual(name = "Stratum", 
  #                 values = c("black", "black", "black","black", "black"))+
  #geom_line(aes(linetype = stratum ))+
  #scale_linetype_manual(values = c("solid","dotted","dashed","twodash","dotdash"))+
  theme(text = element_text(size = 20))+
  facet_wrap(~pool)


#subset summary_poolyearstratum to only include two most common strata
# Plot of proportions by stratum and pool, only the two most common strata
summary_poolyearstratum %>% filter(stratum %in% c("MCB-U", "SCB")) %>%
  ggplot(aes(x = year, y = propwood, group = stratum))+
  geom_line(size = .65, aes(linetype = stratum))+
  theme_bw()+
  xlab("Year")+
  ylab("Proportion of points with wood")+
  facet_wrap(~pool)


#Violin plots of annual variation by stratum and pool 
# All observations, by stratum and pool
summary_poolyearstratum %>% ggplot(aes(x = stratum, y = propwood))+
  geom_violin()+
  geom_boxplot(width = 0.1,
               fill = "black",
               outlier.color = NA)+
  stat_summary(fun.y = median,
               geom = "point",
               fill = "white",
               shape = 21,
               size = 2.5)+
  facet_wrap(~pool)

# Pools, including those with > 9 observations
summary_poolyearstratum %>% filter(npoints > 9) %>%
  ggplot(aes(x = pool, y = propwood))+
  geom_violin()+
  geom_boxplot(width = 0.1,
               fill = "black",
               outlier.color = NA)+
  stat_summary(fun.y = median,
               geom = "point",
               fill = "white",
               shape = 21,
               size = 2.5)

# Stratum and pool, greater than 10 observations
summary_poolyearstratum %>% filter(npoints > 9) %>%
  ggplot(aes(x = stratum, y = propwood))+
  geom_violin()+
  geom_boxplot(width=0.1,
               fill="black",
               outlier.colour=NA)+
  stat_summary(fun.y=median, 
               geom="point",
               fill="white",
               shape=21,
               size=2.5)+
  facet_wrap(~pool)

# Save data
save(summary_all, file = "Analysis/data/outputs/summary_stats/summary_all.Rda")
save(summary_pool, file = "Analysis/data/outputs/summary_stats/summary_pool.Rda")
save(summary_poolyearstratum, file = "Analysis/data/outputs/summary_stats/summary_poolyearstratum.Rda")
save(summary_stratum, file = "Analysis/data/outputs/summary_stats/summary_stratum.Rda")
save(summary_stratumpool, file = "Analysis/data/outputs/summary_stats/summary_stratumpool.Rda")
save(summary_yearpool, file = "Analysis/data/outputs/summary_stats/summary_yearpool.Rda")

##########################  
# Examine as time series
library(segmented)
library(cpm)
library(trend)
library(ggplot2)

levels(summary_yearpool$pool) <- c("P4","P8","P13","P26","LG","OR")
ggplot(summary_yearpool,aes(x=year,y=propwood)) + 
  geom_point() + 
  stat_smooth(method="loess",col="red") + 
  theme_bw() + 
  theme(text = element_text(size = 20))+
  facet_wrap(summary_yearpool$pool)       

p4 <- summary_yearpool[which(summary_yearpool$pool %in% "P4"),c(2,6)] 
plot(p4[,2]~p4[,1],pch=19)
x <- p4[,1]
y <- p4[,2]
lin.mod <- lm(y~x)
seg.mod <- segmented(lin.mod, seg.Z=~x, psi=1999)
plot(y~x,pch=19,cex=1.5, xlab="Year",ylab="Proportion with LW",main="Pool 4")
plot(seg.mod,add=T)
summary(seg.mod)

p4b <- p4[which(p4[,1] >= 1999),]
x <- p4b[,1]
y <- p4b[,2]
lin.mod <- lm(y~x)
plot(y~x,pch=19,cex=1.5, xlab="Year",ylab="Proportion with LW",main="Pool 4")
abline(lin.mod)
ggplot(p4b,aes(x=year,y=propwood)) + 
  geom_point() + 
  stat_smooth(method="lm",col="red") + 
  theme_bw()

pettitt.test(p4[,2])  #first 6 points is separate
mk.test(p4[,2]) #monotonic trend? 

ylims <- c(0,1)

p4 <- summary_yearpool[which(summary_yearpool$pool %in% "P4"),c(2,6)] 
plot(p4[,2]~p4[,1], pch=19,cex=1.5, ylim=ylims,
     xlab="Year",ylab="Proportion with LW",main="Pool 4",cex.lab=1.5,cex.axis=1.25)
abline(v=1998.5,col="red",lwd=3,lty=2)
lm1 <- lm(p4[1:6,2]~p4[1:6,1])
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=1998.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
summary(lm1)
mean(p4[1:6,2])
segments(x0,mean(p4[1:6,2]),x1,mean(p4[1:6,2]),lwd=3,lty=2,col="darkgrey")
text(1995,0.25,labels="mean = 0.487",cex=1.5)

lm2 <- lm(p4[7:nrow(p4),2]~p4[7:nrow(p4),1])
x0=1998.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
segments(x0,mean(p4[7:nrow(p4),2]),x1,mean(p4[7:nrow(p4),2]),lwd=3,lty=2,col="darkgrey")
text(2009,0.45,labels="mean = 0.730",cex=1.5 )

points(p4[,2]~p4[,1], pch=19,cex=1.75)

mean(p4[7:nrow(p4),2])
summary(lm2)


p8 <- summary_yearpool[which(summary_yearpool$pool %in% "P8"),c(2,6)]   
plot(p8[,2]~p8[,1],pch=19)
x <- p8[,1]
y <- p8[,2]
lin.mod <- lm(y~x)
seg.mod <- segmented(lin.mod, seg.Z=~x, psi=2003)
plot(y~x,pch=19,cex=1.5, xlab="Year",ylab="Proportion with LW",main="Pool 8",
     cex.lab=1.5,cex.axis=1.25)
plot(seg.mod,add=T)
summary(seg.mod)
pettitt.test(p8[,2])  #first 6 points is separate
mk.test(p8[,2]) #monotonic trend? yes, highly significant trend


plot(p8[,2]~p8[,1],pch=19,cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Pool 8",cex.lab=1.5,cex.axis=1.25,ylim=ylims)
abline(v=2003.5,col="red",lwd=3,lty=2)
lm1 <- lm(p8[1:10,2]~p8[1:10,1])
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2003.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
segments(x0,y0,x1,y1,lty=1,lwd=3,col="black")
summary(lm1)
mean(p8[1:10,2])
# text(1996.5,0.9,labels="*",cex=8,col="black")
text(1999,0.94,labels="slope = -0.029",cex=1.5,col="black")
text(1999,0.87,labels="p = 0.020",cex=1.5,col="black")
text(1999,0.8,bquote(r^2 == 0.525),cex=1.5)

lm2 <- lm(p8[11:nrow(p8),2]~p8[11:nrow(p8),1])
x0=2003.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
segments(x0,mean(p8[11:nrow(p8),2]),x1,mean(p8[11:nrow(p8),2]),col="darkgrey",lwd=3,lty=2)
text(2011,0.75,labels="mean = 0.430",cex=1.5,col="black")

lm3 <- lm(p8[,2]~p8[,1])
abline(lm3,col="black",lwd=1.5)
text(1994,0.5,labels="slope = -0.144",cex=1.25)
text(1994,0.44,labels="p < 0.001",cex=1.25)
text(1994,0.38,bquote(r^2 == 0.594),cex=1.25) #labels="r2 = 0.525",cex=1.25)

points(p8[,2]~p8[,1], pch=19,cex=1.75)



p13 <- summary_yearpool[which(summary_yearpool$pool %in% "P13"),c(2,6)]   
plot(p13[,2]~p13[,1],pch=19)
x <- p13[,1]
y <- p13[,2]
lin.mod <- lm(y~x)
seg.mod <- segmented(lin.mod, seg.Z=~x, psi=2005)
plot(y~x,pch=19,cex=1.5, xlab="Year",ylab="Proportion with LW",main="Pool 13")
plot(seg.mod,add=T)
summary(seg.mod)


pettitt.test(p13[,2])  #first 12 points is separate
mk.test(p13[,2]) #monotonic trend? 


plot(p13[,2]~p13[,1],pch=19,cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Pool 13",cex.lab=1.5,cex.axis=1.25,ylim=ylims)
abline(v=2004.5,col="red",lwd=3,lty=2)
lm1 <- lm(p13[1:12,2]~p13[1:12,1])
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2004.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
segments(x0,mean(p13[1:10,2]),x1,mean(p13[1:10,2]),lty=2,lwd=3,col="darkgrey")
summary(lm1)
mean(p13[1:10,2])
text(1998,0.55,labels="mean = 0.758",cex=1.5)

lm2 <- lm(p13[13:nrow(p13),2]~p13[13:nrow(p13),1])
x0=2004.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
segments(x0,mean(p13[13:nrow(p13),2]),x1,mean(p13[13:nrow(p13),2]),lty=2,lwd=3,col="darkgrey")
text(2012,0.45,labels="mean = 0.666",cex=1.5)

points(p13[,2]~p13[,1], pch=19,cex=1.75)

mean(p13[13:nrow(p13),2])
summary(lm2)





p26 <- summary_yearpool[which(summary_yearpool$pool %in% "P26"),c(2,6)] 
plot(p26[,2]~p26[,1],pch=19)
x <- p26[,1]
y <- p26[,2]
lin.mod <- lm(y~x)
seg.mod <- segmented(lin.mod, seg.Z=~x, psi=2000)
plot(y~x,pch=19,cex=1.5, xlab="Year",ylab="Proportion with LW",main="Pool 26")
plot(seg.mod,add=T)
summary(seg.mod)
summary(lin.mod)
slope(seg.mod)


pettitt.test(p26[,2])  #break in mean? NO
mk.test(p26[,2]) #monotonic trend? NO
(mean1 <- mean(p26[,2]))  #No trends/breaks whatsoever
plot(p26[,2]~p26[,1],pch=19,cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Pool 26",cex.lab=1.5,cex.axis=1.25,ylim=ylims)
abline(h=mean(p26[,2]),col="darkgrey",lwd=2,lty=2)
points(p26[,2]~lg[,1], pch=19,cex=1.75)
text(2015,0.9,labels="mean = 0.594",cex=1.5)


lg <- summary_yearpool[which(summary_yearpool$pool %in% "LG"),c(2,6)]  
plot(lg[,2]~lg[,1],pch=19)
x <- lg[,1]
y <-lg[,2]
lin.mod <- lm(y~x)
seg.mod <- segmented(lin.mod, seg.Z=~x, psi=2004)
plot(y~x,pch=19,cex=1.5, xlab="Year",ylab="Proportion with LW",main="La Grange")
plot(seg.mod,add=T)
summary(seg.mod)
slope(seg.mod)

pettitt.test(lg[,2])  #first 6 points is separate
mk.test(lg[,2]) #monotonic trend? 

plot(lg[,2]~lg[,1],pch=19,cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="La Grange",cex.lab=1.5,cex.axis=1.25,ylim=ylims)
abline(v=2010.5,col="red",lwd=3,lty=2)
lm1 <- lm(lg[1:18,2]~lg[1:18,1])
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2010.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
# segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
summary(lm1)
mean(lg[1:18,2])
segments(x0,mean(lg[1:10,2]),x1,mean(lg[1:10,2]),lty=2,lwd=3,col="darkgrey")
text(2002,0.8,labels="mean = 0.568",cex=1.5)

lm2 <- lm(lg[19:nrow(lg),2]~lg[19:nrow(lg),1])
x0=2010.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
# segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
segments(x0,mean(lg[19:nrow(lg),2]),x1,mean(lg[19:nrow(lg),2]),lty=2,lwd=3,col="darkgrey")
text(2014,0.6,labels="mean = 0.336",cex=1.5)

mean(lg[19:nrow(lg),2])
summary(lm2)

lm3 <- lm(lg[,2]~lg[,1])
summary(lm3)
abline(lm3,col="black",lwd=1)
text(1995,0.4,labels="slope = -0.013",cex=1.25)
text(1995,0.32,labels="p < 0.001",cex=1.25)
text(1995,0.26,bquote(r^2 == 0.545),cex=1.25) 


points(lg[,2]~lg[,1], pch=19,cex=1.75)





or <- summary_yearpool[which(summary_yearpool$pool %in% "OR"),c(2,6)]   
plot(or[,2]~or[,1],pch=19)  
x <- or[,1]
y <-or[,2]
lin.mod <- lm(y~x)
seg.mod <- segmented(lin.mod, seg.Z=~x, psi=2004)
plot(y~x,pch=19,cex=1.5, xlab="Year",ylab="Proportion with LW",main="Open River")
plot(seg.mod,add=T)
summary(seg.mod)
slope(seg.mod)

pettitt.test(or[,2])  #first 6 points is separate
mk.test(or[,2]) #monotonic trend? 
mean(or[,2])

plot(or[,2]~or[,1],pch=19,cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Open River",cex.lab=1.5,cex.axis=1.25,ylim=ylims)
segments(1993,mean(or[,2]),2017,mean(or[,2]),lty=2,lwd=3,col="darkgrey")
text(2004,0.8,labels="mean = 0.442",cex=1.5)
points(or[,2]~or[,1], pch=19,cex=1.75)


################################################################  
# Examine as time series - aquatic areas by pool
################################################################     

#keep pch & lty codes consistent for habitat types: 
# BWC:  pch=3,lty=2,col=grey (plus sign, dashed grey line)
# MCB:  pch=19,lty=1,col=black (solid dot, solid black line)
# SCB: pch=1,lty=1,col=grey (open circle, solid grey line)
# IMP: pch=4,lty=3,col=black (X, dotted black line)


p4 <- summary_poolyearstratum[which(summary_poolyearstratum$pool %in% "P4"),c(2,3,7)] #select year, stratum and propwood
#exploratory first...
ggplot(p4,aes(x=year,y=propwood)) + 
  geom_point() + 
  stat_smooth(method="loess",col="red") + 
  theme_bw() + 
  theme(text = element_text(size = 20))+
  facet_wrap(p4$stratum)       

#examine habitats separately
p4.bwc <- p4[which(p4$stratum %in% "BWC"),]
pettitt.test(p4.bwc[,3])  #first 6 points is separate
mk.test(p4.bwc[,3]) #monotonic trend? 

p4.mcb <- p4[which(p4$stratum %in% "MCB-U"),]
pettitt.test(p4.mcb[,3])  #first 6 points is separate
mk.test(p4.mcb[,3]) #monotonic trend? 

p4.scb <- p4[which(p4$stratum %in% "SCB"),]
pettitt.test(p4.scb[,3])  #first 6 points is separate
mk.test(p4.scb[,3]) #monotonic trend? 

#Plot for export
ah.pch <- c(NA,3,NA,NA,NA,19,NA,1)[as.numeric(p4$stratum)]
plot(p4[,3]~p4[,1],cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Pool 4",cex.lab=1.5,cex.axis=1.25,ylim=ylims,pch=ah.pch)

abline(v=1998.5,col="black",lty=1,lwd=1) #MCB  - or lwd=3?
abline(v=1998.5,col="grey",lty=2,lwd=1) #BWC
abline(v=2000.5,col="grey",lty=1,lwd=1) #SCB
points(p4[,3]~p4[,1],cex=1.75,pch=ah.pch)

#bwc segments
lm1 <- lm(p4.bwc[1:6,3]~p4.bwc[1:6,1])
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=1998.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
summary(lm1)
mean(p4.bwc[1:6,3])
segments(x0,mean(p4.bwc[1:6,3]),x1,mean(p4.bwc[1:6,3]),lwd=1,lty=2,col="grey")

lm2 <- lm(p4.bwc[7:nrow(p4.bwc),3]~p4.bwc[7:nrow(p4.bwc),1])
x0=1998.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
segments(x0,y0,x1,y1,lty=2,lwd=1,col="grey")
#segments(x0,mean(p4.bwc[7:nrow(p4.bwc),3]),x1,mean(p4.bwc[7:nrow(p4.bwc),3]),lwd=1,lty=2,col="grey")
mean(p4.bwc[7:nrow(p4.bwc),3])

#mcb segments
lm1 <- lm(p4.mcb[1:6,3]~p4.mcb[1:6,1])
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=1998.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
summary(lm1)
mean(p4.mcb[1:6,3])
segments(x0,mean(p4.mcb[1:6,3]),x1,mean(p4.mcb[1:6,3]),lwd=1,lty=1,col="black")

lm2 <- lm(p4.mcb[7:nrow(p4.mcb),3]~p4.mcb[7:nrow(p4.mcb),1])
x0=1998.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
segments(x0,mean(p4.mcb[7:nrow(p4.mcb),3]),x1,mean(p4.mcb[7:nrow(p4.mcb),3]),lwd=1,lty=1,col="black")
mean(p4.mcb[7:nrow(p4.mcb),3])

#scb segments
lm1 <- lm(p4.scb[1:8,3]~p4.scb[1:8,1])
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2000.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
summary(lm1)
mean(p4.mcb[1:8,3])
segments(x0,mean(p4.scb[1:8,3]),x1,mean(p4.scb[1:8,3]),lwd=1,lty=1,col="grey")

lm2 <- lm(p4.scb[9:nrow(p4.scb),3]~p4.scb[9:nrow(p4.scb),1])
x0=2000.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
segments(x0,mean(p4.scb[9:nrow(p4.scb),3]),x1,mean(p4.scb[9:nrow(p4.scb),3]),lwd=1,lty=1,col="grey")
mean(p4.scb[9:nrow(p4.scb),3])

#legend("bottomright",c("MCB","SCB","BWC","IMP"),pch=c(19,1,4,3),col=c("black","black","black","black"),
#      bty="n")
#legend(2013.75,0.35,c("","","",""),lty=c(1,1,2,3),col=c("black","grey","grey","black"),bty="n")

legend("bottomright",c("MCB","SCB"),pch=c(19,1),col=c("black","black"),
       bty="n")
legend(2013.75,0.2,c("",""),lty=c(1,1),col=c("black","grey"),bty="n")

legend(2011,0.2,c("BWC","IMP"),pch=c(3,4),col=c("black","black"),
       bty="n")
legend(2009.5,0.2,c("",""),lty=c(2,3),col=c("grey","black"),bty="n")

#Second Plot for export - no means
ah.pch <- c(NA,3,NA,NA,NA,19,NA,1)[as.numeric(p4$stratum)]
plot(p4[,3]~p4[,1],cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Pool 4",cex.lab=1.5,cex.axis=1.25,ylim=ylims,pch=ah.pch)

abline(v=1998.5,col="black",lty=1,lwd=2) #MCB  - or lwd=3?
abline(v=1998.5,col="grey",lty=2,lwd=2) #BWC
abline(v=2000.5,col="grey",lty=1,lwd=2) #SCB
points(p4[,3]~p4[,1],cex=1.75,pch=ah.pch)
lm2 <- lm(p4.bwc[7:nrow(p4.bwc),3]~p4.bwc[7:nrow(p4.bwc),1])
x0=1998.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
segments(x0,y0,x1,y1,lty=2,lwd=2,col="grey")
legend("bottomright",c("MCB","SCB"),pch=c(19,1),col=c("black","black"),
       bty="n",cex=1.2)
legend(2013.0,0.23,c("",""),lty=c(1,1),col=c("black","grey"),bty="n",cex=1.2,lwd=c(2,2))

legend(2010,0.23,c("BWC","IMP"),pch=c(3,4),col=c("black","black"),cex=1.2,
       bty="n")
legend(2008,0.23,c("",""),lty=c(2,3),col=c("grey","black"),bty="n",cex=1.2,lwd=c(2,2))
points(p4[,3]~p4[,1],cex=1.75,pch=ah.pch)



mean(p4.bwc[,3])
mean(p4.mcb[,3])
summary(lm(p4.mcb[,3]~p4.mcb[,1]))
mean(p4.scb[,3])
summary(lm(p4.scb[,3]~p4.scb[,1]))






p8 <- summary_poolyearstratum[which(summary_poolyearstratum$pool %in% "P8"),c(2,3,7)] #select year, stratum and propwood
#exploratory first...
ggplot(p8,aes(x=year,y=propwood)) + 
  geom_point() + 
  stat_smooth(method="loess",col="red") + 
  theme_bw() + 
  theme(text = element_text(size = 20))+
  facet_wrap(p8$stratum)   

#examine habitats separately
p8.bwc <- p8[which(p8$stratum %in% "BWC"),]
pettitt.test(p8.bwc[,3])  #first 6 points is separate
mk.test(p8.bwc[,3]) #monotonic trend? 

p8.mcb <- p8[which(p8$stratum %in% "MCB-U"),]
pettitt.test(p8.mcb[,3])  #first 6 points is separate
mk.test(p8.mcb[,3]) #monotonic trend? 

p8.scb <- p8[which(p8$stratum %in% "SCB"),]
pettitt.test(p8.scb[,3])  #first 6 points is separate
mk.test(p8.scb[,3]) #monotonic trend? 

p8.imp <- p8[which(p8$stratum %in% "IMP"),]
pettitt.test(p8.imp[,3])  #first 6 points is separate
mk.test(p8.imp[,3]) #monotonic trend? 

#Plot for export
ah.pch <- c(NA,3,NA,NA,4,19,NA,1)[as.numeric(p8$stratum)]
plot(p8[,3]~p8[,1],cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Pool 8",cex.lab=1.5,cex.axis=1.25,ylim=ylims,pch=ah.pch)

#abline(v=1998.5,col="black",lty=1,lwd=3) #MCB - no breakpoint, no trend
abline(v=2001.5,col="grey",lty=2,lwd=2) #BWC - breakpoint, trend
abline(v=2002.5,col="grey",lty=1,lwd=2) #SCB - breakpoint, trend
abline(v=2000.5,col="black",lty=3,lwd=2) #IMP - breakpoint, trend
points(p8[,3]~p8[,1],cex=1.75,pch=ah.pch)

#mcb - just mean
abline(h=mean(p8.mcb[,3]),col="black",lty=1,lwd=1) #MCB - no breakpoint, no trend

#bwc segments
lm1 <- lm(p8.bwc[1:9,3]~p8.bwc[1:9,1])
summary(lm1)
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2001.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
segments(x0,y0,x1,y1,lty=2,lwd=2,col="grey")
mean(p8.bwc[1:9,3])
#segments(x0,mean(p8.bwc[1:9,3]),x1,mean(p8.bwc[1:9,3]),lwd=1,lty=2,col="grey")


lm2 <- lm(p8.bwc[10:nrow(p8.bwc),3]~p8.bwc[10:nrow(p8.bwc),1])
summary(lm2)
x0=2001.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
segments(x0,y0,x1,y1,lty=2,lwd=2,col="grey")
#segments(x0,mean(p8.bwc[10:nrow(p8.bwc),3]),x1,mean(p8.bwc[10:nrow(p8.bwc),3]),lwd=1,lty=2,col="grey")
mean(p8.bwc[10:nrow(p8.bwc),3])

#scb segments
lm1 <- lm(p8.scb[1:10,3]~p8.scb[1:10,1])
summary(lm1)
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2002.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
mean(p8.scb[1:10,3])
segments(x0,mean(p8.scb[1:10,3]),x1,mean(p8.scb[1:10,3]),lwd=1,lty=1,col="grey")

lm2 <- lm(p8.scb[11:nrow(p8.scb),3]~p8.scb[11:nrow(p8.scb),1])
summary(lm2)
x0=2002.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
segments(x0,mean(p8.scb[11:nrow(p8.scb),3]),x1,mean(p8.scb[11:nrow(p8.scb),3]),lwd=1,lty=1,col="grey")
mean(p8.scb[11:nrow(p8.scb),3])

#imp segments
lm1 <- lm(p8.imp[1:8,3]~p8.imp[1:8,1])
summary(lm1)
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2000.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
segments(x0,y0,x1,y1,lty=3,lwd=2,col="black")
mean(p8.imp[1:8,3])
#segments(x0,mean(p8.imp[1:8,3]),x1,mean(p8.imp[1:8,3]),lwd=1,lty=3,col="black")

lm2 <- lm(p8.imp[9:nrow(p8.imp),3]~p8.imp[9:nrow(p8.imp),1])
summary(lm2)
x0=2000.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=3,lwd=1,col="black")
segments(x0,mean(p8.imp[9:nrow(p8.imp),3]),x1,mean(p8.imp[9:nrow(p8.imp),3]),lwd=1,lty=3,col="black")
mean(p8.imp[9:nrow(p8.imp),3])

mean(p8.bwc[,3])
summary(lm(p8.bwc[,3]~p8.bwc[,1]))
mean(p8.mcb[,3])
mean(p8.scb[,3])
summary(lm(p8.scb[,3]~p8.scb[,1]))
mean(p8.imp[,3])
summary(lm(p8.imp[,3]~p8.imp[,1]))

#Second Plot for export - simple
ah.pch <- c(NA,3,NA,NA,4,19,NA,1)[as.numeric(p8$stratum)]
plot(p8[,3]~p8[,1],cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Pool 8",cex.lab=1.5,cex.axis=1.25,ylim=ylims,pch=ah.pch)

#abline(v=1998.5,col="black",lty=1,lwd=3) #MCB - no breakpoint, no trend
abline(v=2001.5,col="grey",lty=2,lwd=2) #BWC - breakpoint, trend
abline(v=2002.5,col="grey",lty=1,lwd=2) #SCB - breakpoint, trend
abline(v=2000.5,col="black",lty=3,lwd=2) #IMP - breakpoint, trend
points(p8[,3]~p8[,1],cex=1.75,pch=ah.pch)

#imp segments
lm1 <- lm(p8.imp[1:8,3]~p8.imp[1:8,1])
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2000.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
segments(x0,y0,x1,y1,lty=3,lwd=2,col="black")
#bwc segments
lm1 <- lm(p8.bwc[1:9,3]~p8.bwc[1:9,1])
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2001.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
segments(x0,y0,x1,y1,lty=2,lwd=2,col="grey")

lm2 <- lm(p8.bwc[10:nrow(p8.bwc),3]~p8.bwc[10:nrow(p8.bwc),1])
x0=2001.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
segments(x0,y0,x1,y1,lty=2,lwd=2,col="grey")





p13 <- summary_poolyearstratum[which(summary_poolyearstratum$pool %in% "P13"),c(2,3,7)] #select year, stratum and propwood
#exploratory first...
ggplot(p13,aes(x=year,y=propwood)) + 
  geom_point() + 
  stat_smooth(method="loess",col="red") + 
  theme_bw() + 
  theme(text = element_text(size = 20))+
  facet_wrap(p13$stratum)      

#examine habitats separately
p13.bwc <- p13[which(p13$stratum %in% "BWC"),]
pettitt.test(p13.bwc[,3])  #first 6 points is separate
mk.test(p13.bwc[,3]) #monotonic trend? 

p13.mcb <- p13[which(p13$stratum %in% "MCB-U"),]
pettitt.test(p13.mcb[,3])  #first 6 points is separate
mk.test(p13.mcb[,3]) #monotonic trend? 

p13.scb <- p13[which(p13$stratum %in% "SCB"),]
pettitt.test(p13.scb[,3])  #first 6 points is separate
mk.test(p13.scb[,3]) #monotonic trend? 

p13.imp <- p13[which(p13$stratum %in% "IMP"),]
pettitt.test(p13.imp[,3])  #first 6 points is separate
mk.test(p13.imp[,3]) #monotonic trend? 

#Plot for export
ah.pch <- c(NA,3,NA,NA,4,19,NA,1)[as.numeric(p13$stratum)]
plot(p13[,3]~p13[,1],cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Pool 13",cex.lab=1.5,cex.axis=1.25,ylim=ylims,pch=ah.pch)
#no trends/breakpoints detected for aq habs so just plot means
#abline(h=mean(p13.mcb[,3]),col="black",lty=1,lwd=1) #MCB - no breakpoint, yes trend
lm1 <- lm(p13.mcb[9:nrow(p13.mcb),3]~p13.mcb[9:nrow(p13.mcb),1])
summary(lm1)
x0=1993
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2017
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
segments(x0,y0,x1,y1,lty=1,lwd=1,col="black")
abline(h=mean(p13.bwc[,3]),col="grey",lty=2,lwd=1) #BWC - breakpoint, trend
abline(h=mean(p13.scb[,3]),col="grey",lty=1,lwd=1) #SCB - breakpoint, trend
abline(h=mean(p13.imp[,3]),col="black",lty=3,lwd=1) #IMP - breakpoint, trend
points(p13[,3]~p13[,1],cex=1.75,pch=ah.pch)

mean(p13.bwc[,3])
mean(p13.mcb[,3])
summary(lm(p13.mcb[,3]~p13.mcb[,1]))
mean(p13.scb[,3])
mean(p13.imp[,3])

# Second plot for export - simple
ah.pch <- c(NA,3,NA,NA,4,19,NA,1)[as.numeric(p13$stratum)]
plot(p13[,3]~p13[,1],cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Pool 13",cex.lab=1.5,cex.axis=1.25,ylim=ylims,pch=ah.pch)
# lm1 <- lm(p13.mcb[,3]~p13.mcb[,1]) #not statistically significant even though monotonic trend detected
# x0=1993
# y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
# x1=2017
# y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
# segments(x0,y0,x1,y1,lty=1,lwd=2,col="black")
# 

p26 <- summary_poolyearstratum[which(summary_poolyearstratum$pool %in% "P26"),c(2,3,7)] #select year, stratum and propwood
#exploratory first...
ggplot(p26,aes(x=year,y=propwood)) + 
  geom_point() + 
  stat_smooth(method="loess",col="red") + 
  theme_bw() + 
  theme(text = element_text(size = 20))+
  facet_wrap(p26$stratum)            

#examine habitats separately
p26.bwc <- p26[which(p26$stratum %in% "BWC"),]
pettitt.test(p26.bwc[,3])  #first 6 points is separate
mk.test(p26.bwc[,3]) #monotonic trend? 

p26.mcb <- p26[which(p26$stratum %in% "MCB-U"),]
pettitt.test(p26.mcb[,3])  #first 6 points is separate
mk.test(p26.mcb[,3]) #monotonic trend? 

p26.scb <- p26[which(p26$stratum %in% "SCB"),]
pettitt.test(p26.scb[,3])  #first 6 points is separate
mk.test(p26.scb[,3]) #monotonic trend? 

p26.imp <- p26[which(p26$stratum %in% "IMP"),]
pettitt.test(p26.imp[,3])  #first 6 points is separate
mk.test(p26.imp[,3]) #monotonic trend? 

#Plot for export
ah.pch <- c(NA,3,NA,NA,4,19,NA,1)[as.numeric(p26$stratum)]
plot(p26[,3]~p26[,1],cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Pool 26",cex.lab=1.5,cex.axis=1.25,ylim=ylims,pch=ah.pch)
#no trends/breakpoints detected for aq habs so just plot means
abline(h=mean(p26.mcb[,3]),col="black",lty=1,lwd=1) #MCB - no breakpoint, no trend
abline(h=mean(p26.bwc[,3]),col="grey",lty=2,lwd=1) #BWC - breakpoint, trend
abline(h=mean(p26.scb[,3]),col="grey",lty=1,lwd=1) #SCB - breakpoint, trend
abline(h=mean(p26.imp[,3]),col="black",lty=3,lwd=1) #IMP - breakpoint, trend
points(p26[,3]~p26[,1],cex=1.75,pch=ah.pch)

#Second Plot for export - simple
ah.pch <- c(NA,3,NA,NA,4,19,NA,1)[as.numeric(p26$stratum)]
plot(p26[,3]~p26[,1],cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Pool 26",cex.lab=1.5,cex.axis=1.25,ylim=ylims,pch=ah.pch)


mean(p26.bwc[,3])
mean(p26.mcb[,3])
mean(p26.scb[,3])
mean(p26.imp[,3])

lg <- summary_poolyearstratum[which(summary_poolyearstratum$pool %in% "LG"),c(2,3,7)] #select year, stratum and propwood
#exploratory first...
ggplot(lg,aes(x=year,y=propwood)) + 
  geom_point() + 
  stat_smooth(method="loess",col="red") + 
  theme_bw() + 
  theme(text = element_text(size = 20))+
  facet_wrap(lg$stratum)     

#examine habitats separately
lg.bwc <- lg[which(lg$stratum %in% "BWC"),]
pettitt.test(lg.bwc[,3])  #first 6 points is separate
mk.test(lg.bwc[,3]) #monotonic trend? 
mean(lg.bwc[,3])

lg.mcb <- lg[which(lg$stratum %in% "MCB-U"),]
pettitt.test(lg.mcb[,3])  #first 6 points is separate
mk.test(lg.mcb[,3]) #monotonic trend? 
mean(lg.mcb[,3])

lg.scb <- lg[which(lg$stratum %in% "SCB"),]
pettitt.test(lg.scb[,3])  #first 6 points is separate
mk.test(lg.scb[,3]) #monotonic trend? 
mean(lg.scb[,3])

#Plot for export
ah.pch <- c(NA,3,NA,NA,4,19,NA,1)[as.numeric(lg$stratum)]
plot(lg[,3]~lg[,1],cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="La Grange",cex.lab=1.5,cex.axis=1.25,ylim=ylims,pch=ah.pch)

#mcb segments
abline(v=2010.5,lty=1,lwd=1,col="black")
lm1 <- lm(lg.bwc[1:18,3]~lg.bwc[1:18,1])
summary(lm1)
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2010.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=1,col="black")
mean(lg.bwc[1:18,3])
segments(x0,mean(lg.bwc[1:18,3]),x1,mean(lg.bwc[1:18,3]),lwd=1,lty=1,col="black")

mean(lg.bwc[19:nrow(lg.bwc),3])
segments(x1,mean(lg.bwc[19:nrow(lg.bwc),3]),2017,mean(lg.bwc[19:nrow(lg.bwc),3]),lwd=1,lty=1,col="black")

#bwc segments
abline(v=2003.5,lwd=1,lty=2,col="grey")
lm1 <- lm(lg.bwc[1:11,3]~lg.bwc[1:11,1])
summary(lm1)
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2003.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=2,lwd=1,col="grey")
mean(lg.bwc[1:11,3])
segments(x0,mean(lg.bwc[1:11,3]),x1,mean(lg.bwc[1:11,3]),lwd=1,lty=2,col="grey")

lm2 <- lm(lg.bwc[12:nrow(lg.bwc),3]~lg.bwc[12:nrow(lg.bwc),1])
summary(lm2)
x0=2003.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=2,lwd=1,col="grey")
segments(x0,mean(lg.bwc[12:nrow(lg.bwc),3]),x1,mean(lg.bwc[12:nrow(lg.bwc),3]),lwd=1,lty=2,col="grey")
mean(lg.bwc[12:nrow(lg.bwc),3])


#scb segments
abline(v=2007.5,lty=1,lwd=1,col="grey")
lm1 <- lm(lg.scb[1:14,3]~lg.scb[1:14,1])
summary(lm1)
x0=1992.5
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
x1=2007.5
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=1,col="darkgrey")
mean(lg.scb[1:14,3])
segments(x0,mean(lg.scb[1:14,3]),x1,mean(lg.scb[1:14,3]),lwd=1,lty=1,col="grey")

lm2 <- lm(lg.scb[15:nrow(lg.scb),3]~lg.scb[15:nrow(lg.scb),1])
summary(lm2)
x0=2007.5
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
x1=2017.5
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
#segments(x0,y0,x1,y1,lty=1,lwd=3,col="darkgrey")
segments(x0,mean(lg.scb[15:nrow(lg.scb),3]),x1,mean(lg.scb[15:nrow(lg.scb),3]),lwd=1,lty=1,col="grey")
mean(lg.scb[15:nrow(lg.scb),3])

#Second Plot for export - with trends
ah.pch <- c(NA,3,NA,NA,4,19,NA,1)[as.numeric(lg$stratum)]
plot(lg[,3]~lg[,1],cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="La Grange",cex.lab=1.5,cex.axis=1.25,ylim=ylims,pch=ah.pch)
#sbc
lm2 <- lm(lg.scb[,3]~lg.scb[,1])
summary(lm2)
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
segments(1992.5,y0,2017.5,y1,lty=1,lwd=2,col="grey")
#bwc
lm2 <- lm(lg.bwc[,3]~lg.bwc[,1])
summary(lm2)
y0=(x0 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
y1=(x1 * lm2$coefficients[[2]])+lm2$coefficients[[1]]
segments(1992.5,y0,2017.5,y1,lty=2,lwd=2,col="grey")
#mcb
lm1 <- lm(lg.mcb[,3]~lg.mcb[,1])
summary(lm1)
y0=(x0 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
y1=(x1 * lm1$coefficients[[2]])+lm1$coefficients[[1]]
segments(1992.5,y0,2017.5,y1,lty=1,lwd=2,col="black")

# plot vertical break lines on top of lms 
abline(v=2010.5,lty=1,lwd=2,col="black")
abline(v=2003.5,lwd=2,lty=2,col="grey")
abline(v=2007.5,lty=1,lwd=2,col="grey")


or <- summary_poolyearstratum[which(summary_poolyearstratum$pool %in% "OR"),c(2,3,7)] #select year, stratum and propwood
#exploratory first...
ggplot(or,aes(x=year,y=propwood)) + 
  geom_point() + 
  stat_smooth(method="loess",col="red") + 
  theme_bw() + 
  theme(text = element_text(size = 20))+
  facet_wrap(or$stratum)            

#examine habitats separately
or.mcb <- or[which(or$stratum %in% "MCB-U"),]
pettitt.test(or.mcb[,3])  #first 6 points is separate
mk.test(or.mcb[,3]) #monotonic trend? 

or.scb <- or[which(or$stratum %in% "SCB"),]
pettitt.test(or.scb[,3])  #first 6 points is separate
mk.test(or.scb[,3]) #monotonic trend? 


mean(or.mcb[,3])
mean(or.scb[,3])

# Plot for export - with trends
ah.pch <- c(NA,3,NA,NA,4,19,NA,1)[as.numeric(or$stratum)]
plot(or[,3]~or[,1],cex=1.75,xlab="Year",
     ylab="Proportion with LW",main="Open River",cex.lab=1.5,cex.axis=1.25,ylim=ylims,pch=ah.pch)
abline(h=mean(or.mcb[,3]),lty=1,lwd=1,col="black")
abline(h=mean(or.scb[,3]),lty=1,lwd=1,col="grey")




# p4.ts <- ts(p4[,2],start=c(1993),end=c(2017),frequency=1)      
#   (adf.test(p4.ts))
#   plot(p4.ts)
#   p4.sma <- SMA(p4.ts,n=3) #smooth with a simple moving average to see trends
#   plot.ts(p4.sma)
#   #
#   p4.bf <- bfast(p4.ts,h=0.15,season="none",max.iter=1)
#   plot(p4.bf)
#   p4.bf
#   #
# p8.ts <- ts(p8[,2],start=c(1993),end=c(2017),frequency=1)      
#   (adf.test(p8.ts))
#   plot(p8.ts)
#   p8.sma <- SMA(p8.ts,n=4)
#   plot.ts(p8.sma)
#   #
#   p8.bf <- bfast(p8.ts,h=0.15,season="none",max.iter=1)
#   plot(p8.bf)
#   p8.bf
#   #
# p13.ts <- ts(p13[,2],start=c(1993),end=c(2017),frequency=1)      
#   (adf.test(p13.ts))
#   plot(p13.ts)
#   p13.sma <- SMA(p13.ts,n=4)
#   plot.ts(p13.sma)
#   #
#   p13.bf <- bfast(p13.ts,h=0.15,season="none",max.iter=1)
#   plot(p13.bf)
#   p13.bf
#   #
# p26.ts <- ts(p26[,2],start=c(1993),end=c(2017),frequency=1)      
#   (adf.test(p26.ts))
#   plot(p26.ts)
#   p26.sma <- SMA(p26.ts,n=3)
#   plot.ts(p26.sma)
#   #
#   p26.bf <- bfast(p26.ts,h=0.15,season="none",max.iter=1)
#   plot(p26.bf)
#   p26.bf
#   #
# lg.ts <- ts(lg[,2],start=c(1993),end=c(2017),frequency=1)      
#   (adf.test(lg.ts))
#   plot(lg.ts)
#   #
#   lg.bf <- bfast(lg.ts,h=0.15,season="none",max.iter=1)
#   plot(lg.bf)
#   lg.bf
#   #
# or.ts <- ts(or[,2],start=c(1993),end=c(2017),frequency=1)      
#   (adf.test(or.ts))
#   plot(or.ts)
#   #
#   or.bf <- bfast(or.ts,h=0.15,season="none",max.iter=1)
#   plot(or.bf)
#   or.bf
#   #
