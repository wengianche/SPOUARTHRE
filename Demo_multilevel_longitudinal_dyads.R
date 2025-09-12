#####Author: WENG IAN CHE
#####Date_create: 2025-09-11
#####Date_update: 2025-09-11

#####Reference: https://quantdev.ssri.psu.edu/tutorials/apa-ati-intensive-longitudinal-data-session-m-dyadic-multilevel-model


#Outline
######################################################
#APA-ATI Intensive Longitudinal Data: Session M - Dyadic Multilevel Model

######################################################
######################################################
#APA-ATI Intensive Longitudinal Data: Session M - Dyadic Multilevel Model
######################################################
#####Research questions
####1. Is the number of daily work stressors associated with end-of-day relationship satisfaction?
###1.1.  What is the extent of association for the typical male partner and for the typical female partner (i.e., “actor” fixed effects)?
###1.2.  What is the extent of influence of the typical male partner on the typical female partner, and vice versa (i.e., “partner” fixed effects)?
###1.3.  Is there heterogeneity in the strength of the association across male partners in couples and across female partners in couples (random effects)?
  
#Notice that, so far, the questions are stated separately for the two types of dyad members (males and females). The dyadic longitudinal model provides for another type of question - questions related to non-independence.

####2. Are dyad members’ relationship satisfactions on any given day related, after accounting for other explanatory variables?

#Here, these relations manifest as correlations/covariances between intercepts, slopes, and residuals.

####Load libraries
library(pacman)
pacman::p_load(dplyr, #for data manipulation
               reshape2, #for data manipulation
               ggplot2, #for plotting
               nlme, #for mlm analysis
               psych #for descriptives
) 

####Load data
#Read in the .csv file using the url() function
BLdyads_long <- read.csv(file="BLdyads_long.csv",header=TRUE) #2100 couples


####Reformatting the data (to stacked data format)
#To analyze in the multilevel framework, the data need to be melted so that the outcome variable for females (f_reldis) and males (m_reldis) are combined into a single variable along with two dummy indicators (female and male) that indicate which dyad member’s score is in each row of data. The data thus become “double-entry” or “stacked”: two records for each observation. The data file is twice as long, and we structure the model to “turn on” and “turn off” the double records to invoke parameter estimation for each variable.

#Create double-entry data (note that the outcome variables to be combined are commented out of the id.vars list)
BLdyads_melt <- melt(data = BLdyads_long,
                     id.vars = c("coupleid","time","time7c",
                                 "f_personid",#"f_reldis",
                                 "f_wrkstrs","f_wrkstrsc","f_wrkstrsc_b","f_wrkstrsc_w",
                                 "m_personid",#"m_reldis",
                                 "m_wrkstrs","m_wrkstrsc","m_wrkstrsc_b","m_wrkstrsc_w"),
                     na.rm = FALSE, 
                     value.name = "reldis") #naming new variable

#Reorder rows for convenience 
BLdyads_melt <- BLdyads_melt[order(BLdyads_melt$coupleid, BLdyads_melt$time, BLdyads_melt$var), ]

#Add the two dummy indicators 
BLdyads_melt$female <- ifelse(BLdyads_melt$variable == "f_reldis", 1, 0)
BLdyads_melt$male <- ifelse(BLdyads_melt$variable == "m_reldis", 1, 0)

#Add another gender indicator
BLdyads_melt$gender <- as.numeric(factor(BLdyads_melt$variable)) #1==Female; 2=Male

#Create integrated personid variable
BLdyads_melt$personid <- ifelse(BLdyads_melt$female == 1, BLdyads_melt$f_personid, BLdyads_melt$m_personid)

#Subset and organize into a new data set for easy viewing and efficiency
#dput(names(BLdyads_melt)) #to obtain variable list for easy cut&paste
BLdyads_doubleentry <- BLdyads_melt[ ,c("coupleid", "personid","time", "time7c",
                                        "gender", "reldis","female", "male",
                                        "f_wrkstrs", "f_wrkstrsc", "f_wrkstrsc_b", "f_wrkstrsc_w",
                                        "m_wrkstrs", "m_wrkstrsc", "m_wrkstrsc_b", "m_wrkstrsc_w")]

####Plotting the data
#We start with examining the distribution of our outcome variable end-of-day relationship dissatisfaction, reldis. Let’s look at the histogram by gender.
ggplot(data = BLdyads_doubleentry, aes(x = reldis)) +
  geom_histogram(fill = "white", color = "black") + 
  labs(x = "Relationship Dissatisfaction") +
  facet_grid(. ~ gender) # creating a separate plot for each gender

#Next, let’s plot a few dyads’ reports of relationship dissatisfaction through the course of the study. Since the the predictor variable has already been split into time-varying (state) and time-invariant (trait) components, we use the time-varying predictor wrkstrsc_w as the “background” context variable.
ggplot(data = subset(BLdyads_doubleentry, coupleid <= 9), aes(x = time, group = personid), legend = FALSE) +
  geom_rect(mapping = aes(xmin = time-.5, xmax = time+.5, ymin = 0, ymax = 5, fill = f_wrkstrsc_w), alpha = 0.6) + # creating rectangles in the background of the plot colored by female work stressors
  geom_rect(mapping = aes(xmin = time-.5, xmax = time+.5, ymin = 5, ymax = 10, fill = m_wrkstrsc_w), alpha = 0.6) + # creating rectangles in the background of the plot colored by male work stressors
  geom_point(aes(x = time, y = reldis, color = factor(gender)), shape = 17, size = 3) + # creating a different colored point for each gender
  geom_line(aes(x = time, y = reldis, color = factor(gender)), lty = 1, size=1) + # creating a different colored line for each gender
  xlab("Time") + 
  ylab("Relationship Dissatisfaction") + ylim(0, 10) +
  scale_x_continuous(breaks=seq(0, 20, by = 5)) + 
  facet_wrap( ~ coupleid) +# creating a separate plot for each dyad
  theme(legend.position = "none")

#Finally, we examine a histogram of the dyad-level (between-dyad) time-invariant variables f_wrkstrsc_b and m_wrkstrsc_b.
#between-person differences
#females
ggplot(data = subset(BLdyads_doubleentry, time == 0 & female == 1), 
       aes(x = f_wrkstrsc_b)) +
  geom_histogram(fill = "white", color = "black") + 
  labs(x = "Female Work Stress (dyad-level centered)")

#males
ggplot(data = subset(BLdyads_doubleentry, time == 0 & male == 1), aes(x = f_wrkstrsc_b)) +
  geom_histogram(fill = "white", color = "black") + 
  labs(x = "Male Work Stress (dyad-level centered)")

####The Dyadic Multilevel Model
#We’ll construct a model looking at the within-person and between-person associations of relationship dissatisfaction (reldis) with female and male work stressors (f_wrkstrs and m_wrkstrs - which have been centered and separated into within-person and between-person components (f_wrkstrsc_w,m_wrkstrsc_w and f_wrkdstrsc_b,m_wrkdstrsc_b respectively).
#We use the two dummy variables (male and female) to turn on and off the parameters. The parameters invoked with 𝑓𝑒𝑚𝑎𝑙𝑒 are associated with the female member of the dyad’s relationship dissatisfaction (within reldis), and parameters invoked with 𝑚𝑎𝑙𝑒 are associated with the male member of the dyad’s relationship dissatisfaction (within reldis).
#Fitting the full model with both between-dyad and within-dyad actor and partner effects
#Model 1: 
model1 <- lme(fixed = reldis ~ -1 + 
                female + #intercept
                female:f_wrkstrsc_b + female:f_wrkstrsc_w + #actor-effects
                female:m_wrkstrsc_b + female:m_wrkstrsc_w + #partner-effects
                male + #intercept
                male:m_wrkstrsc_b + male:m_wrkstrsc_w + #actor-effects
                male:f_wrkstrsc_b + male:f_wrkstrsc_w, #partner-effects 
              random = ~ -1 + 
                female + male + #intercepts
                female:f_wrkstrsc_w + male:m_wrkstrsc_w + #actor-effects
                female:m_wrkstrsc_w + male:f_wrkstrsc_w | coupleid, #partner-effects
              weights=varIdent(form = ~1 | gender),        # invokes separate sigma^{2}_{e} for each gender
              corr=corCompSymm(form = ~1 | coupleid/time), # invokes off-diagonal sigma_{e1e2} 
              data=BLdyads_doubleentry,
              control=lmeControl(maxIter=10000, opt="optim")) 

summary(model1)

#Model 2
model2 <- lme(fixed = reldis ~ -1 + 
                female + #intercept
                female:f_wrkstrsc_b + female:f_wrkstrsc_w + #actor-effects
                female:m_wrkstrsc_b + female:m_wrkstrsc_w + #partner-effects
                male + #intercept
                male:m_wrkstrsc_b + male:m_wrkstrsc_w + #actor-effects
                male:f_wrkstrsc_b + male:f_wrkstrsc_w, #partner-effects 
              random = ~ -1 + 
                female + male + #intercepts
                female:f_wrkstrsc_w + male:m_wrkstrsc_w + #actor-effects
                female:m_wrkstrsc_w + male:f_wrkstrsc_w | coupleid, #partner-effects
              weights=varIdent(form = ~1 | gender),       #this invokes separate sigma^{2}_{e} for each gender
              corr=corAR1(form = ~1 | coupleid/gender/time), #this invokes an AR structure
              data=BLdyads_doubleentry,
              control=list(maxIter=1000, opt="optim")) 

summary(model2)
######################################################
