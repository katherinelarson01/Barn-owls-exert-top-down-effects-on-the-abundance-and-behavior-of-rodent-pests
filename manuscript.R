#Load packages
library(tidyr)
library(dplyr)
library(glmmTMB)
library(MuMIn)
library(ggeffects)
library(ggplot2)
library(purrr)
library(MuMIn)


####################### Table of Contents #######################
#################################################################

#Chew Blocks: Index of Rodent Abundance
#1. Vegetation cover variable
#2. Choose distribution
#3. Candidate model set
#4. Assumptions
#5. Figures

#GUD: Rodent Perceived Predation Risk
#1. Vegetation cover variable
#2. Choose distribution
#3. Candidate model set
#4. Assumptions
#5. Figures

#Cameras: Rodent Activity
#1. Vegetation cover variable
#2. Choose distribution
#3. Candidate model set
#4. Assumptions
#5. Figures



############# Chew Block: Index of Rodent Abundance #############
#################################################################

#Variable Definitions====
#two sessions: b=sampled in feb-mar, d = sampling in may-june
#pressure= barn owl hunting pressure
#illumination= lunar illumination
#total cover= final index of vegetation cover variable
#prop_chewed= used the proportion of blocks on a grid that showed evidence of chewing
#_vr= vine row
#_cc= cover crop row



#Create index of vegetation cover variable====

#Load vegetation data (file path from personal laptop)

chew_veg <- read.csv("C:/Users/kathe/OneDrive/Desktop/OwlProject/manuscript/journals/ecological_applications/for_submission/data/chewblock_veg.csv")
attach(chew_veg)

#combine vineyard and grid into one column
chew_veg<-unite(chew_veg, vineyard_grid, c(vineyard, grid), remove=FALSE)

chew_veg$vineyard_grid<-as.factor(chew_veg$vineyard_grid)
chew_veg$grid<-as.factor(chew_veg$grid)
chew_veg$vineyard<-as.factor(chew_veg$vineyard)
chew_veg$session<-as.factor(chew_veg$session)

#Calculate median percent cover of dry vine debris in cover crop and vine row for each vineyard_grid and session ID
chew_veg<- chew_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_dry_vine_cc = median(dry_vine_debris_cc))

chew_veg<- chew_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_dry_vine_vr = median(dry_vine_debris_vr))


#Calculate median height of dry and live vegetation in cover crop and vine row for each vineyard_grid and session ID

chew_veg<- chew_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_h_live_vr = median(height_live_veg_vr))

chew_veg<- chew_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_h_dry_vr = median(height_dry_veg_vr))

chew_veg<- chew_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_h_live_cc = median(height_live_veg_cc))

chew_veg<- chew_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_h_dry_cc = median(height_dry_veg_cc))

#Calculate median vine canopy cover in vine row for each vineyard_grid and session ID

chew_veg<- chew_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_canopy_cover_vr = median(canopy_cover_vr))

chew_veg <- chew_veg %>%
  mutate(session = recode(session, b = '1', d = '2'))

chew_veg$session<-as.numeric(chew_veg$session)

# Create a new chew_veg df with only median values of variables at grids
cols_to_remove <- c(10:16)  # List of column indices to remove
chew_veg <- chew_veg[, -cols_to_remove] 

#take the maximum height of veg in cover crop row
chew_veg$max_h_cc <- pmax(chew_veg$median_h_live_cc, chew_veg$median_h_dry_cc)

#take the maximum height of veg in vine row
chew_veg$max_h_vr <- pmax(chew_veg$median_h_live_vr, chew_veg$median_h_dry_vr)

#sum percent of ground covered with dry vine debris in cover crop and vine row
#chew_veg$sum_dry_vine<- chew_veg$median_dry_vine_vr + chew_veg$median_dry_vine_cc

#Remove duplicate rows
chew_veg <- chew_veg %>%
  distinct(vineyard_grid, session, .keep_all = TRUE)

#merge vegetation data with chew block and weather data

#Remove unnecessary variables
chew_veg <- chew_veg[c("vineyard_grid", "session", "median_dry_vine_vr", "median_dry_vine_cc", "median_canopy_cover_vr", "max_h_cc", "max_h_vr")]

#Load chew block data (file path from personal laptop)
chew_data<-read.csv("C:/Users/kathe/OneDrive/Desktop/OwlProject/manuscript/journals/ecological_applications/for_submission/data/chewblock_data.csv")

#merge
chew_data<- merge(chew_data, chew_veg, by =c("vineyard_grid", "session"), all.x=TRUE)


# Manually scale veg cover columns between 0 and 1
chew_data <- data.frame(
  sum_precip=chew_data$sum_precip,
  grid=chew_data$grid,
  grams_removed=chew_data$grams_removed,
  count=chew_data$count,
  pressure=chew_data$pressure,
  chewed_binary=chew_data$chewed_binary,
  vineyard=chew_data$vineyard,
  session=chew_data$session,
  prop_chewed=chew_data$prop_chewed,
  illumination=chew_data$illumination,
  max_temp=chew_data$max_temp,
  max_wind=chew_data$max_wind,
  median_canopy_cover_vr = (chew_data$median_canopy_cover_vr - min(chew_data$median_canopy_cover_vr)) / (max(chew_data$median_canopy_cover_vr) - min(chew_data$median_canopy_cover_vr)),
  median_dry_vine_cc = (chew_data$median_dry_vine_cc - min(chew_data$median_dry_vine_cc)) / (max(chew_data$median_dry_vine_cc) - min(chew_data$median_dry_vine_cc)),
  median_dry_vine_vr = (chew_data$median_dry_vine_vr - min(chew_data$median_dry_vine_vr)) / (max(chew_data$median_dry_vine_vr) - min(chew_data$median_dry_vine_vr)),
  max_h_cc = (chew_data$max_h_cc - min(chew_data$max_h_cc)) / (max(chew_data$max_h_cc) - min(chew_data$max_h_cc)),
  max_h_vr = (chew_data$max_h_vr - min(chew_data$max_h_vr)) / (max(chew_data$max_h_vr) - min(chew_data$max_h_vr))
)

#Create total cover variable 
chew_data['total_cover'] = chew_data['median_canopy_cover_vr'] + chew_data['median_dry_vine_cc'] + chew_data['median_dry_vine_vr'] + chew_data['max_h_vr'] + chew_data['max_h_cc']

#center and scale total_cover variable
chew_data$total_cover<-standardize(chew_data$total_cover)



#Choose appropriate distribution (based on r2 goodness of fit)====

#Full model with a Binomial Distribution (Binary Response Variable)
full_binomial<-glmmTMB(chewed_binary  ~ sum_precip + max_wind*pressure  +  total_cover*pressure +pressure*illumination + (1|vineyard/grid)
                       , family=binomial, weights=count, data=chew_data)
r2(full_binomial)

#Full model with a Gaussian Distribution (Continuous Response Variable)
full_gauss_logit<-glmmTMB(prop_chewed  ~ sum_precip +   max_wind*pressure +  total_cover*pressure +pressure*illumination + (1|vineyard/grid)
                          , family=gaussian, data=chew_data)
r2(full_gauss_logit)

#Remove unnecessary variables
cols_to_remove <- c("grams_removed", "chewed_binary")
chew_data <- chew_data[, !(names(chew_data) %in% cols_to_remove)]

#Remove duplicate rows:
chew_data<-unique(chew_data)

#Full model with a Gaussian Distribution and Logit Link (Binary Response Variable)
full_gauss_logit<-glmmTMB(prop_chewed  ~ sum_precip +   max_wind*pressure + illumination +  total_cover*pressure +pressure*illumination + (1|vineyard)
                          , family=gaussian (link="logit"),weights=count, data=chew_data)
r2(full_gauss_logit)

#Full model with a Beta Distribution (Binary Response Variable)
full_beta<-glmmTMB(prop_chewed  ~ sum_precip +   max_wind*pressure + illumination +  total_cover*pressure +pressure*illumination + (1|vineyard)
                   , family=beta_family,weights=count, data=chew_data)
r2(full_beta)

#Check full beta model assumptions====

check_model(full_beta)
check_predictions(full_beta)


#Candidate model set====

m1<-glmmTMB(prop_chewed  ~ sum_precip + 1 + (1|vineyard)
            , family=beta_family,weights=count, data=chew_data)

m2<-glmmTMB(prop_chewed  ~ sum_precip + illumination + max_wind + (1|vineyard)
            , family=beta_family,weights=count, data=chew_data)

m3<-glmmTMB(prop_chewed  ~ sum_precip + illumination + max_wind + pressure + (1|vineyard)
            , family=beta_family,weights=count, data=chew_data)

m4<-glmmTMB(prop_chewed  ~ sum_precip +   max_wind + illumination + total_cover + (1|vineyard)
            , family=beta_family,weights=count, data=chew_data)

m5<-glmmTMB(prop_chewed  ~ sum_precip + max_wind  + illumination + pressure +  total_cover + (1|vineyard)
            , family=beta_family,weights=count, data=chew_data)

m6<-glmmTMB(prop_chewed  ~ sum_precip +  max_wind + illumination + pressure*max_wind + (1|vineyard)
            , family=beta_family,weights=count, data=chew_data)

m7<-glmmTMB(prop_chewed  ~ sum_precip + max_wind + illumination +  total_cover*pressure + (1|vineyard)
            , family=beta_family,weights=count, data=chew_data)

m8<-glmmTMB(prop_chewed  ~ sum_precip +   max_wind + illumination*pressure + (1|vineyard)
            , family=beta_family,weights=count, data=chew_data)

chewAICc <- model.sel(m1,m2,m3,m4,m5,m6,m7,m8)
summary(m3)

#Reassess Assumptions / Determine Beta Coefficients, SEs, CIs====
check_convergence(m3)
check_model(m3)
confint(m3)


#Figures====


#Figure 1===

mydf<- ggpredict(m3, terms=c("pressure"))

attach(mydf)

pressure<-ggplot(mydf, aes(x,predicted)) + 
  geom_line(color='black', linewidth = 1) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5, fill="#CC9999") + 
  labs(colour = "Box")+
  labs(
    x = "Hunting Pressure",
    y = "Proportion of Chewed Blocks",
    title = ""
  )+
  theme_bw()+
  theme(axis.line = element_line(color='black', size = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text( color="black", size=18),
        axis.title = element_text( color="black", size=18),
        axis.text = element_text(color="black", size=18),
        axis.ticks = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.5, "cm"),
        plot.title = element_text(size=27))

pressure


#Appendix S7===

data <- (data.frame(y = c(-0.54, 1.00, -3.25, -0.17, -0.69, -1.38, 0.76, 1.13,     -0.44, 0.67, -1.59, 0.18, -0.05, 0.02, 0.03,  -0.18),
                    x = c("Precipitation", "Wind", "Hunting Pressure", "Lunar Illumination", "Vegetation Cover", "Wind*Hunting Pressure", "Vegetation Cover*Hunting Pressure", "Lunar Illumination*Hunting Pressure", "Precipitation", "Wind", "Hunting Pressure", "Lunar Illumination", "Vegetation Cover", "Wind*Hunting Pressure", "Vegetation Cover*Hunting Pressure", "Lunar Illumination*Hunting Pressure"),
                    lower = c(-0.81, 0.67, -4.14, -0.49,  -0.95, -2.77, -0.23, 0.26,     -0.51, 0.58, -1.85, 0.09, -0.13, -0.34, -0.24, -0.45),
                    upper = c(-0.27, 1.32, -2.37, 0.15, -0.43, 0.01, 1.75, 2.00,     -0.37, 0.76, -1.32, 0.28, -0.02, 0.37, 0.30, 0.08),
                    Model=c("gaussian", "gaussian", "gaussian", "gaussian", "gaussian", "gaussian", "gaussian", "gaussian", "beta", "beta", "beta", "beta", "beta", "beta", "beta", "beta")))

#Turn your 'x' column into a character vector
data$x <- as.character(data$x)
data$Model <- as.character(data$Model)
#Then turn it back into a factor with the levels in the correct order
data$x <- factor(data$x, levels=unique(data$x))

p<-ggplot(data, aes(x=x, y=y, color=Model)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  coord_flip()+
  theme_bw()+                                         
  labs(
    x = "",
    y = "Coefficient Estimates with 95% Confidence Intervals",
    title = ""
  ) +
  scale_y_continuous(breaks = seq(min(-2), max(3), by = 0.3))+
  geom_hline(yintercept=0, linetype='dotted', size=1)+
  theme(axis.line = element_line(color='black', size = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(color="black", size=18),
        axis.title = element_text(color="black", size=18),
        axis.text = element_text(color="black", size=18),
        axis.ticks = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.5, "cm"),
        legend.position="right",
        legend.title=element_text(size=16), 
        legend.text=element_text(size=14))

p+ scale_y_continuous(breaks = seq(-4.2, 2.0, by = .4))


#Appendix S9===


mydf<- ggpredict(m5, terms=c("total_cover"))
attach(mydf)

chew_cover<-ggplot(mydf, aes(x,predicted)) + 
  geom_line(color='black', linewidth = 1) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5, fill="#CC9999") + 
  labs(colour = "Box")+
  labs(
    x = "Vegetation Cover",
    y = "Proportion of Chewed Blocks",
    title = ""
  )+
  theme_bw()+
  theme(axis.line = element_line(color='black', size = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text( color="black", size=18),
        axis.title = element_text( color="black", size=16),
        axis.text = element_text(color="black", size=18),
        axis.ticks = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.5, "cm"),
        plot.title = element_text(size=27))


#Chew Block:
#Marginal effect of pressure at differing levels of illumination
range(chew_scaled$illumination)

cover_.59<- chew_scaled

illu_.59$illumination=illu_.59$illumination+.59


m8<-glmmTMB(prop_chewed  ~ sum_precip +   max_wind + illumination*pressure + (1|vineyard)
            , family=beta_family,weights=count, data=illu_.59)
summary(m8)
confint(m8)



df <- data.frame(
  beta_pressure = c(-1.38, -1.52, -1.65, -1.79, -1.92),
  illu = c(-1.31, -0.59, 0.13, 0.85, 1.56),
  low=c(-1.75, -1.76,-1.87, -2.10, -2.38),
  high=c(-1.02, -1.28, -1.44, -1.48, -1.47),
  se = c(0.18, 0.12, 0.11, 0.16, 0.23)
)

eb <- aes(ymax = beta_pressure + se, ymin = beta_pressure - se)

# Standard error of the mean
g<-ggplot(df, aes(x=illu, y=beta_pressure)) +                                      
  labs(
    x = "Lunar Illumination",
    y = "Marginal Effect of Owl Hunting Pressure",
    title = ""
  )+
  theme_bw()+
  theme(axis.line = element_line(color='black', size = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text( color="black", size=18),
        axis.title = element_text( color="black", size=16),
        axis.text = element_text(color="black", size=18),
        axis.ticks = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.5, "cm"),
        plot.title = element_text(size=27))+
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5, fill = "#CC9999") +
  geom_hline(yintercept=0, linetype='dotted', size=.6)

chew_marg<-(g+
              scale_x_continuous(n.breaks=8) +
              scale_y_continuous(n.breaks=8))


#GUD:
#Marginal effect of pressure at differing levels of cover

df <- data.frame(
  beta_pressure = c(0.51, 0.43, 0.36, 0.28, 0.2),
  cover = c(-1.73, -0.76, 0.21, 1.18, 2.15),
  low=c(0.19, 0.21, 0.18, 0.07, -0.09),
  high=c(0.83, 0.66, 0.54, 0.49, 0.50),
  se = c(0.16, 0.12, 0.091, 0.11, 0.15)
)

eb <- aes(ymax = beta_pressure + se, ymin = beta_pressure - se)

# Standard error of the mean
gg<-ggplot(df, aes(x=cover, y=beta_pressure)) +                                      
  labs(
    x = "Vegetation Cover",
    y = "Marginal Effect of Owl Hunting Pressure",
    title = ""
  )+
  theme_bw()+
  theme(axis.line = element_line(color='black', size = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text( color="black", size=18),
        axis.title = element_text( color="black", size=16),
        axis.text = element_text(color="black", size=18),
        axis.ticks = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.5, "cm"),
        plot.title = element_text(size=27))+
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5, fill = "#CC9999") +
  geom_hline(yintercept=0, linetype='dotted', size=.6)

gud_marg<-(gg+
             scale_x_continuous(n.breaks=8) +
             scale_y_continuous(n.breaks=8))



#Activity Model:

df <- data.frame(
  beta_pressure = c(0.08, 0.037, -0.0055, -0.049, -0.092),
  cover = c(-1.91, -0.48, .95, 2.38, 3.82),
  low=c(-0.02, -0.03, -0.07, -0.15, -0.24),
  high=c(0.18, 0.10, 0.06, 0.05, 0.06),
  se = c(.1, .11, .12, .14, .15)
)

eb <- aes(ymax = beta_pressure + se, ymin = beta_pressure - se)

# Standard error of the mean
ggg<-ggplot(df, aes(x=cover, y=beta_pressure)) +                                     
  labs(
    x = "Vegetation Cover",
    y = "Marginal Effect of Owl Hunting Pressure",
    title = ""
  )+
  theme_bw()+
  theme(axis.line = element_line(color='black', size = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text( color="black", size=18),
        axis.title = element_text( color="black", size=16),
        axis.text = element_text(color="black", size=18),
        axis.ticks = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.5, "cm"),
        plot.title = element_text(size=27))+
  geom_line() +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5, fill = "#CC9999") +
  geom_hline(yintercept=0, linetype='dotted', size=.6)

activity_marg<-(ggg+
                  scale_x_continuous(n.breaks=8) +
                  scale_y_continuous(n.breaks=6))



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(chew_cover, gud_marg, chew_marg, activity_marg, cols=2)







############# GUD: Rodent Perceived Predation Risk #############
########################################################

#Create index of vegetation cover variable====

#Load vegetation data (file path from personal laptop)

gud_veg <- read.csv("C:/Users/kathe/OneDrive/Desktop/OwlProject/manuscript/journals/ecological_applications/for_submission/data/gud_veg.csv")
attach(gud_veg)


#Variable Definitions====
#GUD grids were comprised of 2 rows with 6 bins: _a3 is one row, _b4 is row 2
#_vr= vine row
#_cc= cover crop row
#two sessions: b=sampled in feb-mar, d = sampling in may-june
#pressure= barn owl hunting pressure
#illumination= lunar illumination
#proportion= average proportion of grams of seeds remaining per GUD grid
#total cover= final index of vegetation cover variable



#combine vineyard and grid into one column
gud_veg<-unite(gud_veg, vineyard_grid, c(vineyard, grid), remove=FALSE)

gud_veg$vineyard_grid<-as.factor(gud_veg$vineyard_grid)
gud_veg$grid<-as.factor(gud_veg$grid)
gud_veg$vineyard<-as.factor(gud_veg$vineyard)
gud_veg$session<-as.factor(gud_veg$session)
gud_veg$site<-as.factor(gud_veg$site)

#Calculate median percent cover of dry vine debris in cover crop and vine row for each vineyard_grid, session ID, and site ID
gud_veg<- gud_veg %>%
  group_by(vineyard_grid, session, site)%>%
  mutate(median_dry_vine_cc = median(dry_vine_debris_cc))

gud_veg<- gud_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_dry_vine_vr = median(dry_vine_debris_vr))


#Calculate median height of dry and live vegetation in cover crop and vine row for each vineyard_grid and session ID

gud_veg<- gud_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_h_live_vr = median(height_live_veg_vr))

gud_veg<- gud_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_h_dry_vr = median(height_dry_veg_vr))

gud_veg<- gud_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_h_live_cc = median(height_live_veg_cc))

gud_veg<- gud_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_h_dry_cc = median(height_dry_veg_cc))

#Calculate median vine canopy cover in vine row for each vineyard_grid and session ID

gud_veg<- gud_veg %>%
  group_by(vineyard_grid, session)%>%
  mutate(median_canopy_cover_vr = median(canopy_cover_vr))

gud_veg <- gud_veg %>%
  mutate(session = recode(session, b = '1', d = '2'))
gud_veg$session<-as.numeric(gud_veg$session)

# Create a new gud_veg df with only median values of variables at grids
cols_to_remove <- c(9:15)  # List of column indices to remove
gud_veg <- gud_veg[, -cols_to_remove] 

#take the maximum height of veg in vine row
gud_veg$max_h_vr <- pmax(gud_veg$median_h_live_vr, gud_veg$median_h_dry_vr)

#Remove duplicate rows
gud_veg <- gud_veg %>%
  distinct(vineyard_grid, session, site, .keep_all = TRUE)



#Pivot data frame so there is one column for row 1 (a3) and another column for row 2 (b4)
median_dry_vine_cc <- gud_veg %>%
  pivot_wider(names_from = site, values_from = median_dry_vine_cc, names_prefix = "median_dry_vine_") %>%
  group_by(vineyard_grid,session) %>%
  summarize(
    median_dry_vine_a3 = first(na.omit(median_dry_vine_a3)),
    median_dry_vine_b4 = first(na.omit(median_dry_vine_b4))
  )

median_h_live <- gud_veg %>%
  pivot_wider(names_from = site, values_from = median_h_live_cc, names_prefix = "median_h_live_") %>%
  group_by(vineyard_grid,session) %>%
  summarize(
    median_h_live_a3 = first(na.omit(median_h_live_a3)),
    median_h_live_b4 = first(na.omit(median_h_live_b4))
  )

median_h_dry <- gud_veg %>%
  pivot_wider(names_from = site, values_from = median_h_dry_cc, names_prefix = "median_h_dry_") %>%
  group_by(vineyard_grid,session) %>%
  summarize(
    median_h_dry_a3 = first(na.omit(median_h_dry_a3)),
    median_h_dry_b4 = first(na.omit(median_h_dry_b4))
  )

canopy_cover <- gud_veg[c("vineyard_grid", "session", "median_canopy_cover_vr")]
max_h_vr <- gud_veg[c("vineyard_grid", "session", "max_h_vr")]
median_dry_vine_vr <- gud_veg[c("vineyard_grid", "session", "median_dry_vine_vr")]

dataframes <- list(median_h_dry, median_h_live, median_dry_vine_cc, canopy_cover, max_h_vr, median_dry_vine_vr)
common_columns <- c("vineyard_grid", "session")
gud_veg <- reduce(dataframes, function(x, y) merge(x, y, by = common_columns, all.x = TRUE))

gud_veg$max_h_cc <- pmax(gud_veg$median_h_live_a3, gud_veg$median_h_dry_a3, gud_veg$median_h_dry_b4, gud_veg$median_h_live_b4)
gud_veg$sum_dry_vine<- gud_veg$median_dry_vine_a3 + gud_veg$median_dry_vine_b4 + gud_veg$median_dry_vine_vr 


#merge vegetation data with gud weights and weather data
#Load gud data (file path from personal laptop)
gud_data<-read.csv("C:/Users/kathe/OneDrive/Desktop/OwlProject/manuscript/journals/ecological_applications/for_submission/data/gud_data.csv")


#merge
gud_data<- merge(gud_data, gud_veg, by =c("vineyard_grid", "session"), all.x=TRUE)

#Remove unnecessary variables
cols_to_remove <- c("weight_remaining", "site")
gud_data <- gud_data[, !(names(gud_data) %in% cols_to_remove)]

#Remove duplicate rows:
gud_data<-unique(gud_data)


# Manually scale veg cover columns between 0 and 1
gud_data <- data.frame(
  sum_precip=gud_data$sum_precip,
  proportion=gud_data$proportion,
  pressure=gud_data$pressure,
  vineyard=gud_data$vineyard,
  grid=gud_data$grid,
  session=gud_data$session,
  rep=gud_data$rep,
  illumination=gud_data$illumination,
  max_temp=gud_data$max_temp,
  max_wind=gud_data$max_wind,
  median_canopy_cover_vr = (gud_data$median_canopy_cover_vr - min(gud_data$median_canopy_cover_vr)) / (max(gud_data$median_canopy_cover_vr) - min(gud_data$median_canopy_cover_vr)),
  sum_dry_vine = (gud_data$sum_dry_vine - min(gud_data$sum_dry_vine)) / (max(gud_data$sum_dry_vine) - min(gud_data$sum_dry_vine)),
  max_h_cc = (gud_data$max_h_cc - min(gud_data$max_h_cc)) / (max(gud_data$max_h_cc) - min(gud_data$max_h_cc)),
  max_h_vr = (gud_data$max_h_vr - min(gud_data$max_h_vr)) / (max(gud_data$max_h_vr) - min(gud_data$max_h_vr))
)


#Create total cover variable 
gud_data['total_cover'] = gud_data['median_canopy_cover_vr'] + gud_data['sum_dry_vine'] + gud_data['max_h_vr'] + gud_data['max_h_cc']

#center and scale total_cover variable
gud_data$total_cover<-standardize(gud_data$total_cover)




#Choose appropriate distribution (based on r2 goodness of fit)====

full_beta<-glmmTMB(proportion ~ rep +  max_temp + max_wind*pressure + illumination*pressure + total_cover*pressure +
                     (1|vineyard), family=beta_family, data = gud_data)
r2(full_beta)

full_gauss_logit<-glmmTMB(proportion ~ rep +  max_temp + max_wind*pressure + illumination*pressure + total_cover*pressure +
                            (1|vineyard), family=gaussian(link="logit"), data = gud_data)
r2(full_gauss_logit)

#Check full beta model assumptions====

check_model(full_beta)
check_predictions(full_beta)


#Candidate Model Set====

m1<-glmmTMB(proportion ~ 1 + 
              (1|vineyard/grid), family=beta_family, data = gud_data)

m2<-glmmTMB(proportion ~  rep +  max_temp + illumination + max_wind +
              (1|vineyard/grid), family=beta_family, data = gud_data)

m3<-glmmTMB(proportion ~  rep +  max_temp + illumination + max_wind + pressure +
              (1|vineyard/grid), family=beta_family, data = gud_data)

m4<-glmmTMB(proportion ~ rep +  max_temp + max_wind + illumination + total_cover + 
              (1|vineyard/grid), family=beta_family, data = gud_data)

m5<-glmmTMB(proportion ~ rep +   max_temp + max_wind  + illumination + pressure + total_cover +
              (1|vineyard/grid), family=beta_family, data = gud_data)

m6<-glmmTMB(proportion ~  rep + max_temp + illumination + pressure*max_wind + 
              (1|vineyard/grid), family=beta_family, data = gud_data)

m7<-glmmTMB(proportion ~ rep +  max_temp + max_wind + illumination + total_cover*pressure +
              (1|vineyard/grid), family=beta_family(link="logit"), data = gud_data)

m8<-glmmTMB(proportion ~ rep +  max_temp + max_wind + illumination*pressure +
              (1|vineyard/grid), family=beta_family, data = gud_data)

gudAICc <- model.sel(m1,m2,m3,m4,m5,m6,m7,m8)

summary(m5)

#Reassess Assumptions / Determine Beta Coefficients, SEs, CIs====
check_convergence(m5)
check_model(m5)
confint(m5)



#Figures====


#Figure 2===

mydf<- ggpredict(m5, terms=c("pressure"))
attach(mydf)

pressure<-ggplot(mydf, aes(x,predicted)) + 
  geom_line(color='black', linewidth = 1) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5, fill="#CC9999") + 
  labs(colour = "Box")+
  labs(
    x = "Hunting Pressure",
    y = "Proportion of Seeds Remaining",
    title = "Top Ecological Predictors of Rodent Perceived Predation Risk"
  )+
  theme_bw()+
  theme(axis.line = element_line(color='black', size = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text( color="black", size=18),
        axis.title = element_text( color="black", size=18),
        axis.text = element_text(color="black", size=18),
        axis.ticks = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.5, "cm"),
        plot.title = element_text(size=27))

mydf<- ggpredict(m5, terms=c("total_cover"))
attach(mydf)

veg<-ggplot(mydf, aes(x,predicted)) + 
  geom_line(color='black', linewidth = 1) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5, fill="#CC9999") + 
  labs(colour = "Box")+
  labs(
    x = "Vegetation Cover",
    y = "",
    title = ""
  )+
  theme_bw()+
  theme(axis.line = element_line(color='black', size = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text( color="black", size=18),
        axis.title = element_text( color="black", size=18),
        axis.text = element_text(color="black", size=18),
        axis.ticks = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.5, "cm"),
        plot.title = element_text(size=27))

mydf<- ggpredict(m5, terms=c("illumination"))
attach(mydf)

illu<-ggplot(mydf, aes(x,predicted)) + 
  geom_line(color='black', linewidth = 1) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5, fill="#CC9999") + 
  labs(colour = "Box")+
  labs(
    x = "Lunar Illumination",
    y = "Proportion of Seeds Remaining",
    title = ""
  )+
  theme_bw()+
  theme(axis.line = element_line(color='black', size = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text( color="black", size=18),
        axis.title = element_text( color="black", size=18),
        axis.text = element_text(color="black", size=18),
        axis.ticks = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.5, "cm"),
        plot.title = element_text(size=27))

mydf<- ggpredict(m5, terms=c("max_wind"))
attach(mydf)

wind<-ggplot(mydf, aes(x,predicted)) + 
  geom_line(color='black', linewidth = 1) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5, fill="#CC9999") + 
  labs(colour = "Box")+
  labs(
    x = "Wind",
    y = "",
    title = ""
  )+
  theme_bw()+
  theme(axis.line = element_line(color='black', size = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text( color="black", size=18),
        axis.title = element_text( color="black", size=18),
        axis.text = element_text(color="black", size=18),
        axis.ticks = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.5, "cm"),
        plot.title = element_text(size=27))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(pressure,illu,veg, wind, cols=2)



############### Cameras: Rodent Activity ###############
########################################################

#Variable Definitions====
#series= approximately 2 week time between recording vegetation at and re-baiting cameras
#count= nightly count of independent detections of mice and voles 
#max_event_group_size= the number of rodents in the photo with the largest group size per event
#bait_age= number of days since bait was deployed
#pressure= barn owl hunting pressure
#illumination= lunar illumination
#vegetation was measured in the cover crop row on either side on the camera site, so _cc= first cover crop row, _adj= second cover crop row
#total cover= final index of vegetation cover variable
#sum_thatch_cc= thatch thickness in the cover crop rows 


#Create index of vegetation cover variable====

#Load vegetation data (file path from personal laptop)

camera_veg <- read.csv("C:/Users/kathe/OneDrive/Desktop/OwlProject/manuscript/journals/ecological_applications/for_submission/data/camera_veg.csv")
attach(camera_veg)

#take the maximum height of veg from both cover crop rows and vine row 
camera_veg$max_h_cc <- pmax(camera_veg$height_live_veg_cc, camera_veg$height_dry_veg_cc, camera_veg$height_dry_veg_adj, camera_veg$height_live_veg_adj)
camera_veg$max_h_vr <- pmax(camera_veg$height_live_veg_vr, camera_veg$height_dry_veg_vr)

#sum percent of ground covered with dry vine debris from both cover crop rows
camera_veg$sum_vine_cc<- camera_veg$dry_vine_debris_cc + camera_veg$dry_vine_debris_adj 

#sum thatch cover thickness from both cover crop rows
camera_veg$sum_thatch_cc<-camera_veg$thatch_cc +camera_veg$thatch_adj


selected_columns <- camera_veg[, c("series", "vineyard", "site", "canopy_cover_vr", "sum_vine_cc", "max_h_vr", "max_h_cc", "dry_vine_debris_vr", "sum_thatch_cc")]


# Manually scale columns between 0 and 1
df_scaled <- data.frame(
  series=selected_columns$series,
  vineyard=selected_columns$vineyard,
  site=selected_columns$site,
  canopy_cover_vr = (selected_columns$canopy_cover_vr - min(selected_columns$canopy_cover_vr)) / (max(selected_columns$canopy_cover_vr) - min(selected_columns$canopy_cover_vr)),
  sum_vine_cc = (selected_columns$sum_vine_cc - min(selected_columns$sum_vine_cc)) / (max(selected_columns$sum_vine_cc) - min(selected_columns$sum_vine_cc)),
  max_h_cc = (selected_columns$max_h_cc - min(selected_columns$max_h_cc)) / (max(selected_columns$max_h_cc) - min(selected_columns$max_h_cc)),
  max_h_vr = (selected_columns$max_h_vr - min(selected_columns$max_h_vr)) / (max(selected_columns$max_h_vr) - min(selected_columns$max_h_vr)),
  dry_vine_vr = (selected_columns$dry_vine_debris_vr - min(selected_columns$dry_vine_debris_vr)) / (max(selected_columns$dry_vine_debris_vr) - min(selected_columns$dry_vine_debris_vr)),
  sum_thatch_cc = (selected_columns$sum_thatch_cc - min(selected_columns$sum_thatch_cc)) / (max(selected_columns$sum_thatch_cc) - min(selected_columns$sum_thatch_cc))
)

#Create final index of vegetation cover variable
df_scaled['total_cover'] = df_scaled['canopy_cover_vr'] + df_scaled['dry_vine_vr'] + df_scaled['max_h_vr'] + df_scaled['sum_vine_cc'] + df_scaled['max_h_cc'] 

#Center and scale final vegetation variables
df_scaled$total_cover<-standardize(df_scaled$total_cover)
df_scaled$sum_thatch_cc<-standardize(df_scaled$sum_thatch_cc)

#Remove duplicate rows
df_scaled <- df_scaled %>%
  distinct(vineyard, site, series, .keep_all = TRUE)

#merge vegetation data with nights camera counts and weather data

#Load camera data (file path from personal laptop)
camera_data<-read.csv("C:/Users/kathe/OneDrive/Desktop/OwlProject/manuscript/journals/ecological_applications/for_submission/data/camera_data.csv")

#data set has already been cleaned by:
#1. looping through raw detections to create independent detections based on a 10-minutue threshold
#2. removing rows from nights when camera died
#3. subsetting data set to only include camera hits taken two nights before, the night of, and two nights after we measured vegetation variables at the camera sites (~every two weeks) 
#4. obtaining the number of independent events per night (nightly count of independent detections of mice and voles)

#merge
camera_data<- merge(camera_data, df_scaled, by =c("series","vineyard", "site"), all.x=TRUE)


#Choose appropriate distribution (based on r2 goodness of fit)====

#Full model with a poisson distribution
full_poison<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind*pressure + sum_precip + illumination*pressure + total_cover*pressure + sum_thatch_cc*pressure +
                       (1|vineyard/site), family=poisson,   data=camera_data)
r2(full_poison)

#Full model with a negative binomial distribution
full_nbinom<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind*pressure + sum_precip + illumination*pressure + total_cover*pressure + sum_thatch_cc*pressure +
                       (1|vineyard/site), family=nbinom2(),   data=camera_data)
r2(full_nbinom)

#Full model with a zero inflated poisson distribution
full_zipois<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind*pressure + sum_precip + illumination*pressure + total_cover*pressure + sum_thatch_cc*pressure +
                       (1|vineyard/site), family=poisson(), ziformula=~1,   data=camera_data)
r2(full_zipois)

#Full model with a zero-inflated negative binomial distribution
full_zibinom<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind*pressure + sum_precip + illumination*pressure + total_cover*pressure + sum_thatch_cc*pressure +
                        (1|vineyard/site), family=nbinom2(), ziformula=~1,   data=camera_data)
r2(full_zibinom)

#Check full beta model assumptions====

check_model(full_nbinom, check=c("vif", "normality", "reqq"))
plot(check_overdispersion(full_nbinom))
check_overdispersion(full_nbinom)
plot(check_normality(full_nbinom))
plot(check_colinearity(full_nbinom))


#Candidate Model Set====

m1<-glmmTMB(count ~ 1 + 
              (1|vineyard/site), family=nbinom2(),   data=camera_data)

m2<-glmmTMB(count ~  bait_age + max_event_groupsize + max_temp + sum_precip + illumination + max_wind +
              (1|vineyard/site), family=nbinom2(),   data=camera_data)

m3<-glmmTMB(count ~  bait_age + max_event_groupsize + max_temp + sum_precip + illumination + max_wind + pressure +
              (1|vineyard/site), family=nbinom2(),   data=camera_data)

m4<-glmmTMB(count ~  bait_age + max_event_groupsize + max_temp + sum_precip + illumination + pressure*max_wind + 
              (1|vineyard/site), family=nbinom2(),   data=camera_data)

m5<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind + sum_precip + illumination +  total_cover*pressure +
              (1|vineyard/site), family=nbinom2(),   data=camera_data)

m6<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind + sum_precip + illumination +  sum_thatch_cc*pressure +
              (1|vineyard/site), family=nbinom2(),   data=camera_data)

m7<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind + sum_precip + illumination +  total_cover*pressure + sum_thatch_cc*pressure +
              (1|vineyard/site), family=nbinom2(),   data=camera_data)

m8<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind + sum_precip + illumination*pressure +
              (1|vineyard/site), family=nbinom2(),   data=camera_data)

m9<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind + sum_precip + illumination + total_cover + 
              (1|vineyard/site), family=nbinom2(),   data=camera_data)

m10<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind + sum_precip + illumination + sum_thatch_cc +
               (1|vineyard/site), family=nbinom2(),   data=camera_data)

m11<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind + sum_precip + illumination + total_cover + sum_thatch_cc +
               (1|vineyard/site), family=nbinom2(),   data=camera_data)

m12<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind + sum_precip  + illumination + pressure +  total_cover +
               (1|vineyard/site), family=nbinom2(),   data=camera_data)

m13<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind + sum_precip  + illumination + pressure + sum_thatch_cc +
               (1|vineyard/site), family=nbinom2(),   data=camera_data)

m14<-glmmTMB(count ~ bait_age + max_event_groupsize + max_temp + max_wind + sum_precip  + illumination + pressure +  total_cover + sum_thatch_cc +
               (1|vineyard/site), family=nbinom2(),   data=camera_data)

cameraAICc <- model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)

summary(m7)

#Reassess Assumptions / Determine Beta Coefficients, SEs, CIs====
check_model(m7, check=c("vif", "normality", "reqq"))
plot(check_overdispersion(m7))
check_overdispersion(m7)
plot(check_normality(m7))
plot(check_colinearity(m7))

confint(m7)


#Figures====


#Figure 3===

df <- data.frame(
  thatch = c(-0.62, 0.86, 2.34, 3.81, 5.28),
  beta_pressure = c(0.10, -0.085, -0.27, -0.45, -0.64),
  low= c(0.04, -0.16, -0.42, -0.68, -0.94),
  high= c(0.16, -0.01, -0.12, -0.23, -0.33),
  eb = c(.03, 0.04, 0.08, 0.12, 0.16)
)


# Standard error of the mean
g <- ggplot(df, aes(x = thatch, y = beta_pressure)) +                                     
  labs(
    x = "Thatch Thickness",
    y = "Marginal Effect of Barn Owl Hunting Pressure",
    title = ""
  ) +
  theme_bw() +
  theme(
    axis.line = element_line(color = 'black', linewidth = 1),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x = element_text(color = "black", size = 22),
    axis.title = element_text(color = "black", size = 22),
    axis.text = element_text(color = "black", size = 22),
    axis.ticks = element_line(linewidth = 1, color = "black"),
    axis.ticks.length = unit(.5, "cm"),
    plot.title = element_text(size = 27)
  ) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5, fill = "#CC9999") +
  geom_hline(yintercept = 0, linetype = 'dotted', linewidth = .6)

g+
  scale_x_continuous(n.breaks=8) +
  scale_y_continuous(n.breaks=6)
