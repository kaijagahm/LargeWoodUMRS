# Week 4 sandbox, 6/25/18 - 6/29/18
source("libraries.R")
source("color_schemes.R")
load("data/new.ef.Rda")
load("data/pool8.barcodes.Rda")

#Map electrofishing points by stratum type
(mbs <- ggplot(data = new.ef, aes(x = lon, y = lat, color = stratum_name))+
  geom_point(size = 0.7)+
  coord_fixed(1.2)+
  scale_color_manual(name = "Aquatic Habitat Stratum", 
                     values = strataColors_distinct)+
  guides(color = guide_legend(override.aes = list(size=6)))+
  theme(text = element_text(size=18))+
  ggtitle("Sampling Points by Aquatic Habitat Stratum")+
  scale_x_continuous(limits = c(-91.35, -91.15))
)
#***#add outline of pool 8 

  #By year
  (mbs_y <- mbs + 
      facet_wrap(~year)
  )
#***#add outline of pool 8 

# Bar Chart of Number of Sampling Points by Stratum
nss <- new.ef %>% filter(!is.na(snag)) %>% filter(!is.na(stratum_name)) %>% ggplot(aes(x = stratum_name))+
  geom_bar(aes(fill = stratum_name))+
  scale_fill_manual(name = "Aquatic Habitat Stratum", 
                    labels = names(strataColors_distinct),
                    values = strataColors_distinct)+
  ylab("Number of Sampling Events")+
  xlab("Aquatic Habitat Stratum")+
  ggtitle("Number of Points Sampled in each Aquatic Habitat Stratum")+
  theme(axis.text.x = element_blank())
nss + theme(text = element_text(size=20))

  #by snag
  (nss_s <- nss+
    facet_wrap(~snagyn)+
    theme(text = element_text(size=20))
  )

  #by year
  (nss_y <- nss+
    facet_wrap(~year)
  )
  
  #Table by year
  (by_year <- as.data.frame(new.ef %>% 
                                   group_by(year) %>% 
                                   summarize(totpoints = n(), 
                                             CWDpoints = sum(snag),
                                             noCWDpoints = n() - sum(snag),
                                             propCWD = round(sum(snag/n()), 4))
  )
  )
  
  #Table by stratum and year
  (stratum.year <- as.data.frame(new.ef %>% 
                                   group_by(year, stratum_name) %>% 
                                   summarize(totpoints = n(), 
                                             CWDpoints = sum(snag),
                                             noCWDpoints = n() - sum(snag),
                                             propCWD = round(sum(snag/n()), 4))
  )
  )
  

#Proportion of points with CWD by year
ggplot(data = by_year, aes(x = year, y = propCWD)) +
  geom_line(size = 1.5)+
  geom_point(size = 3, col = "red")+
  ggtitle("Proportion of Sites with CWD by Sampling Year")+
  xlab("Year of Sampling") +
  ylab("Proportion of Sites with CWD")+
  theme(text = element_text(size=20))

#Number of points sampled by year
ggplot(data = by_year, aes(x = year, y = totpoints)) +
  geom_line(size = 1.5)+
  geom_point(size = 3, col = "red")+
  ggtitle("Number of Points by Sampling Year")+
  xlab("Year of Sampling") +
  ylab("Number of Points Sampled")+
  theme(text = element_text(size=20))

#CWD points and total points by year
ggplot(data = by_year, aes(x = year))+
  geom_line(aes(y = totpoints), size = 1.5)+
  geom_line(aes(y = CWDpoints), size = 1.5, color = "red")+
  ggtitle("# CWD Points and # Points Sampled by Year")+
  xlab("Year")+
  ylab("Number of Points")+
  theme(text = element_text(size=20))
#not sure how to make a legend for this one

#CWD points by total points sampled
ggplot(data = by_year, aes(x = totpoints, y = CWDpoints))+
  geom_point(size = 3, pch = 8)+
  ggtitle("CWD Points vs. Total Points Sampled")+
  theme_bw()+
  theme(text = element_text(size=20))+
  xlab("# Points Sampled (by year)")+
  ylab("# Points with CWD")
  
#Number of points by stratum and year
(npsy <- ggplot(data = stratum.year, aes(x = year, y = totpoints, color = stratum_name))+
  geom_line(size = 1.5)+
  scale_color_manual(name = "Aquatic Habitat Stratum", 
                     values = strataColors_distinct)+
  ggtitle("Number of Points Sampled by Stratum Over Time")+
  ylab("Number of Sampling Points")+
  xlab("Year")+
  theme_bw()+
  theme(text = element_text(size=20))
)

#Proportion of CWD by Stratum and Year
(prop.cwd_sy <- ggplot(data = stratum.year[stratum.year$stratum_name != "Impounded--Offshore",], 
       aes(x = year, 
           y = propCWD, 
           color = stratum_name))+
  geom_line(size = 1.5)+
  scale_color_manual(name = "Aquatic Habitat Stratum", 
                     values = strataColors_distinct)+
  ggtitle("Proportion of Points with CWD by Year")+
  ylab("Proportion of Points with CWD")+
  xlab("Year")+
  theme_bw()+
  theme(text = element_text(size=20))
)
      
      #same thing but smoothed
      (prop.cwd_sy_s <- ggplot(data = stratum.year[stratum.year$stratum_name != "Impounded--Offshore",], 
             aes(x = year, 
                 y = propCWD, 
                 color = stratum_name))+
        geom_smooth(size = 1.5, se = FALSE)+
        scale_color_manual(name = "Aquatic Habitat Stratum", 
                           values = strataColors_distinct)+
        ggtitle("Proportion of Points with CWD by Year")+
        ylab("Proportion of Points with CWD")+
        xlab("Year")+
        theme_bw()+
        theme(text = element_text(size=20))
      )

# How does proportion of CWD change over time at the fixed sites?
  # Just the fixed sites:
  ggplot(data = new.ef %>% filter(sitetype == 2), aes(x = lon, y = lat))+
    geom_point()
  # There seem to be only two fixed sites. 
  # So why are there 80 rows marked as being at a fixed site?
  table(new.ef$sitetype)
  table(new.ef[new.ef$sitetype == 2,]$barcode)
  table(new.ef[new.ef$sitetype == 2,]$year)
  table(droplevels(new.ef[new.ef$sitetype == 2,]$lcode))
  # only two location codes
  length(unique(new.ef[new.ef$sitetype == 2,]$barcode))
  # it appears that each location was sampled 40 times.
  length(unique(new.ef[new.ef$lcode == "M692.7R" & 
                         new.ef$sitetype == 2,]$barcode))
  length(unique(new.ef[new.ef$lcode == "M701.8C" & 
                         new.ef$sitetype == 2,]$barcode))
  
  #plot CWD over time for each fixed site
  s1 <- new.ef[new.ef$lcode == "M692.7R" & new.ef$sitetype == 2,]
  s1_s <- s1 %>% 
    group_by(year) %>% 
    summarize(prop.cwd = sum(snag)/n(),
              site = factor("1"))
  
  s2 <- new.ef[new.ef$lcode == "M701.8C" & new.ef$sitetype == 2,]
  s2_s <- s2 %>%
    group_by(year) %>%
    summarize(prop.cwd = sum(snag)/n(),
              site = factor("2"))

  fixed <- rbind(s1_s, s2_s)
  
# CWD presence at fixed sites over time
  ggplot(data = fixed, aes(x = year, y = prop.cwd, col = site, linetype = site))+
    geom_line(size = 1)+
    geom_point(size = 2)+
    scale_y_continuous(limits = c(0, 1))+
    ggtitle("CWD Presence at Fixed Sites Over Time")
    
  
# Stratum and Landcover Type
  library(reshape2)
table_all <- as.data.frame.matrix(with(new.ef, 
                                       table(stratum_name, landcover_short)))
table_prop_all <- table_all/nrow(new.ef)
table_prop_all$stratum <- row.names(table_prop_all)
row.names(table_prop_all) <- NULL
ma <- melt(table_prop_all, id.vars = "stratum")

table_cwd <- as.data.frame.matrix(with(new.ef %>% filter(snag == 1), 
                                       table(stratum_name, landcover_short)))
table_prop_cwd <- table_cwd/nrow(new.ef %>% filter(snag == 1))
table_prop_cwd$stratum <- row.names(table_prop_cwd)
row.names(table_prop_cwd) <- NULL
mc <- melt(table_prop_cwd, id.vars = "stratum")

ggplot(data = ma, aes(variable, stratum))+
  geom_tile(aes(fill = value))+
  scale_fill_gradient("Proportion of all \n points \n", low = "lightblue", high = "black") +
  theme_bw()+
  ggtitle("Proportion plot for all points")+
  xlab("Nearest Land Cover Type")+
  ylab("Aquatic Stratum")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        text = element_text(size = 16))

ggplot(data = mc, aes(variable, stratum))+
  geom_tile(aes(fill = value))+
  scale_fill_gradient("Proportion of all \n CWD points \n", low = "lightblue", high = "black") +
  theme_bw()+
  ggtitle("Proportion plot for CWD points")+
  xlab("Nearest Land Cover Type")+
  ylab("Aquatic Stratum")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        text = element_text(size = 16))

# Distance to nearest landcover, by snag presence
ggplot(data = new.ef %>% filter(dist_landcover > 0), 
       aes(x = dist_landcover, 
           color = snagyn))+
  geom_line(stat = "density", size = 2)+
  scale_color_manual(name = "Coarse Woody Debris", 
                       values = c("black", "red"))+
  guides(color = guide_legend(override.aes = list(size=6)))+
  theme(text = element_text(size=18))+
  ggtitle("Distribution of Distance to Nearest Landcover")+
  xlab("Distance to Nearest Landcover")+
  ylab("Density")

# Percent aquatic veg
ggplot(data = new.ef, 
       aes(x = pct_aqveg, 
           color = snagyn))+
  geom_line(stat = "density", size = 2)+
  scale_color_manual(name = "Coarse Woody Debris", 
                     values = c("black", "red"))+
  guides(color = guide_legend(override.aes = list(size=6)))+
  theme(text = element_text(size=18))+
  ggtitle("Distribution of Percent Aquatic Veg.")+
  xlab("Percent Aquatic Vegetation")+
  ylab("Density")

# Percent open water
ggplot(data = new.ef,
       aes(x = pct_opwat, 
           color = snagyn))+
  geom_line(stat = "density", size = 2)+
  scale_color_manual(name = "Coarse Woody Debris", 
                     values = c("black", "red"))+
  guides(color = guide_legend(override.aes = list(size=6)))+
  theme(text = element_text(size=18))+
  ggtitle("Distribution of Percent Open Water")+
  xlab("Percent Open Water")+
  ylab("Density")

# Want to do two-sample Kolmogorov-Smirnov tests to test for equality of distributions
d1 <- density(new.ef[new.ef$snag == 1,]$pct_opwat)
d0 <- density(new.ef[new.ef$snag == 0,]$pct_opwat)

ks.test(d1$y, d0$y, alternative = "two.sided")

# Chi-squared tests
# H0: snag presence is independent of aquatic habitat type
# Ha: snag presence is not independent of aquatic habitat type.
t <- table(new.ef$snagyn, new.ef$stratum)
chisq.test(t)  
# p < 0.001, X-squared = 310.28, df = 6
# Reject the null that the two variables are independent. Conclude that snag presence is not independent of aquatic habitat type. 

# H0: snag presence is independent of landcover type
# Ha: snag presence is not independent of landcover type.
lc <- table(new.ef$snagyn, new.ef$landcover_abbr)
chisq.test(lc)
# p < 0.001, X-squared = 57.468, df = 16
# Reject the null that the two variables are independent. Conclude that snag presence is not independent of nearest landcover type. 

# What if we split landcover into fewer classifications? Go back to the data cleaning document for this. 
temp <- droplevels(new.ef %>% 
                     filter(landcover_lumped != "Aquatic veg", 
                            landcover_lumped != "Sand")
)
lcl <- table(temp$snagyn, temp$landcover_lumped)
chisq.test(lcl)

#if you reduce nearest landcover classifications down to forest, developed, and grassland/meadow, there is no longer a detectable difference in the distribution for CWD vs. not

#how broad/narrow do we want these categories to be?
#plot number of points, faceted by CWD presence.
temp %>% filter(!is.na(snag)) %>% ggplot(aes(x = landcover_lumped))+
  geom_bar(aes(fill = landcover_lumped))+
  scale_fill_manual(name = "Nearest landcover type", 
                    labels = levels(temp$landcover_lumped),
                    values = lcColors_lumped)+
  ylab("Number of Sampling Events")+
  xlab("Nearest Landcover Type")+
  ggtitle("Landcover types by CWD Presence")+
  facet_wrap(~snagyn)+
  theme(axis.text.x = element_blank())+ 
  theme(text = element_text(size=20))

#Problems
# Aquatic veg is one of the possible "nearest landcover" types. Weird.
# Some areas are listed as having zero distance to the nearest landcover.
# Are these the same areas?

av_as_land <- new.ef[new.ef$landcover_lumped == "Aquatic veg",]
hist(av_as_land$dist_landcover)
#nope, some of these are still listed as being far away from land.

onland <- new.ef[new.ef$dist_landcover == 0,]
table(onland$landcover_lumped)
#that isn't particularly helpful either.
sum(onland$barcode %in% av_as_land$barcode)/nrow(onland)
# 48% of the "onland" points also have aquatic vegetation as nearest landcover
sum(av_as_land$barcode %in% onland$barcode)/nrow(av_as_land)
# 64% of the "av_as_land" points are also listed as being on land.

#Beginning to explore connectivity metrics
# Is there more CWD if more of the perimeter borders on terrestrial areas?
new.ef %>% ggplot(aes(x = snagyn, 
                      y = pct_terr, 
                      fill = snagyn))+
  geom_boxplot()+
  coord_flip()

#density plot
ggplot(data = new.ef, 
       aes(x = pct_terr, 
           color = snagyn))+
  geom_line(stat = "density", size = 2)+
  scale_color_manual(name = "Coarse Woody Debris", 
                     values = c("black", "red"))+
  guides(color = guide_legend(override.aes = list(size=6)))+
  theme(text = element_text(size=18))+
  ggtitle("Distribution of Pct Terrestrial Shoreline")+
  xlab("Pct Terrestrial Shoreline")+
  ylab("Density")
# this looks promising!

  mod_pct_terr <- glm(snag~pct_terr, data = new.ef, family = "binomial")
  summary(mod_pct_terr)
  #ooh wow look at that coefficient and that p-value!!

    #generate data to plot model with line
    pct_terr <- data.frame(pct_terr = seq(from = 0, to = 100, by = .1))
    predictions <- as.data.frame(predict(mod_pct_terr, 
                                         newdata = pct_terr, 
                                         type = 'response', 
                                         se.fit = T))
    generated_data <- cbind(pct_terr, predictions)
    #plot model
    generated_data %>% ggplot(aes(x = pct_terr, 
                                  y = fit)) + 
      geom_ribbon(aes(ymin = fit-se.fit, ymax = fit+se.fit, 
                      x = pct_terr), fill = rgb(0, 0, 0, 0.2))+
      geom_line(aes(y = fit), 
                col = "blue")+
      labs(x="% Terrestrial shoreline perimeter", 
           y="Probability of CWD presence", 
           title=paste("CWD by Percent Terrestrial Shoreline")) +
      scale_y_continuous(limits = c(0,1))+
      theme_bw()+
      theme(text = element_text(size=20))

    