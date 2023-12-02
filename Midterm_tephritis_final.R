# load packages and remove objects in environment
pacman::p_load(openxlsx,MuMIn, ggplot2,tidyverse,dplyr,gt,knitr, GGally, Hmisc,MASS,ggiraphExtra,viridis)
rm(list=ls())

###################################################### Read and manipulate data
data = read.table("tephritis.txt")
#give names to the columns
names(data) = data[1,]
#remove first row with the column names 
data = data[-c(1),]


# turn columns into numeric type
for (i in c(4:10)){
  data[,i] = as.numeric(data[,i])
}

############################################################## Explore ----

#plot data against each other
pairs(data[4:10])

#plot data, color based on hostplant
ggpairs(data, columns = 4:10, ggplot2::aes(colour=Hostplant, alpha = 0.5))

#plot data, color based on sex
ggpairs(data, columns = 4:10, ggplot2::aes(colour=Sex, alpha = 0.5))

#plot data, color based on geography
ggpairs(data, columns = 4:10, ggplot2::aes(colour=Baltic, alpha = 0.5))

#plot data, color based on geography and sex
data$merge = paste(data$Sex, data$Baltic)
ggpairs(data, columns = 4:10, ggplot2::aes(colour=merge, alpha = 0.4))

#plot data, color based on sympatry
ggpairs(data, columns = 4:10, ggplot2::aes(colour=Patry, alpha = 0.5))

#plot data, color based on geography and hostplant
data$merge2 = paste(data$Hostplant, data$Patry)
ggpairs(data, columns = 4:10, ggplot2::aes(colour=merge2, alpha = 0.4))

#plot data, color based on geography and sex
data$merge3 = paste(data$Sex, data$Patry)
ggpairs(data, columns = 4:10, ggplot2::aes(colour=merge3, alpha = 0.4))

data$OL_ratio = data$OL / data$BL
#plot data, color based on OL/BL ration
ggpairs(data, columns = 13, ggplot2::aes(colour=Baltic, alpha = 0.5))


summary(fdata)
#how many of each level Patry do we have?
mean(fdata[fdata$Patry == "Allopatry",5])
mean(fdata[fdata$Patry == "Sympatry",5])


######################################################## Allometry ----

# female dataset
fdata = data[data$Sex == "Female" & !is.na(data$OL),]

###################################################### * Heterophyllum ----

# create heterophyllum dataset
hdata = fdata[data$Hostplant == "Heterophyllum",]

#fit model, explore results
m = lm(log(hdata$OL) ~ log(hdata$BL))
summary(m)

############################################################ * Oleraceum ----

#create oleraceum dataset, don't include outliers
odata = data[fdata$Hostplant == "Oleraceum" & fdata$BL > 3.6,]

# fit model, explore results
m = lm(log(odata$OL) ~ log(odata$BL))
summary(m)


######################### plot both on the same graph, don't include outliers
allo_data = fdata[fdata$BL > 3.6,]

# create new dataset to plot
plot_this = data.frame(OL = log(allo_data $OL), 
                       BL = log(allo_data $BL), 
                       Hostplant = allo_data $Hostplant)

#plot results
ggplot(data = plot_this, aes(x = BL, y = OL, col = Hostplant)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  labs(x = "Body length (mm)", y = "Ovipositor length (mm) ", title = "") +
  theme_bw() + 
  theme(legend.position = "top", 
        aspect.ratio=1, 
        axis.title.y = element_text(margin = margin(t = 0, r = 16, b = 0, l = 0))) +
  scale_color_manual(values=c("#21918c", "#440154")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))


################################################################## ANCOVA----
##################################################################*Ovipositor----

#make correct dataset
fdata = subset(data, Sex == "Female"& !is.na(data$OL) & BL > 3.6,)

# fit multiple models for model selection
m1 = glm(OL ~ Patry * Hostplant, data = fdata)

m2 = glm(OL ~ Patry + Hostplant, data = fdata)

m3 = glm(OL ~ Patry, data = fdata)

m4 = glm(OL ~ Hostplant , data = fdata)


# construct model selection table
mlist = c(m1, m2, m3, m4)
AICTab = AIC(m1, m2, m3, m4)
AICTab$logLik[1] = logLik(m1)
AICTab$logLik[2] = logLik(m2)
AICTab$logLik[3] = logLik(m3)
AICTab$logLik[3] = logLik(m4)
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

# explore selected model 
summary(m1)

#make dataframe for barchart
data_model = data.frame(
  Hostplant = factor(c("Heterophyllum", "Heterophyllum", "Oleraceum",  "Oleraceum")),
  Patry = factor(c("Allopatry",  "Sympatry","Allopatry", "Sympatry")),
  OL = c(1.79966,  1.70821, 1.67034, 1.64146)
)

#extract standard error
se <- summary(m1)$coef[, "Std. Error"]

#plot results in barchart
ggplot(data_model, aes(x = Hostplant, y = OL, fill = Patry)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.7) +
  labs(title = "",
       x = "",
       y = "Ovipositor length (mm)") +
  theme_minimal() + 
  theme(legend.position = "top", 
        aspect.ratio=1, 
        axis.title.y = element_text(margin = margin(t = 0, r = 16, b = 0, l = 0))) +
  geom_errorbar(aes(ymax = OL + se, ymin = OL- se),
                position = position_dodge(width = 0.8), width = 0.25) + 
  coord_cartesian(ylim = c(1, 1.8)) +
  scale_fill_manual(values = c("Allopatry" = "#5ec962", "Sympatry" = "#3b528b"))
                
  
##########################################################  * Bodylength----
# make correct dataset
fdata = subset(data, Sex == "Female"& !is.na(data$OL))

#plot distribution
p = ggplot(fdata, aes(x=Hostplant, y = BL)) + 
  geom_boxplot(aes(fill = Hostplant)) + 
  scale_fill_viridis(discrete = T, option = "B") 

#remove outliers
fdata = subset(data, Sex == "Female"& !is.na(data$OL) & BL > 3.6,)


# fit multiple models for model selection
m1 = glm(BL ~ Patry * Hostplant, data = fdata)

m2 = glm(BL ~ Patry + Hostplant, data = fdata)

m3 = glm(BL ~ Patry, data = fdata)

m4 = glm(BL ~ Hostplant , data = fdata)

# make table for model selection 
mlist = c(m1, m2, m3, m4)
AICTab = AIC(m1, m2, m3, m4)
AICTab$logLik[1] = logLik(m1)
AICTab$logLik[2] = logLik(m2)
AICTab$logLik[3] = logLik(m3)
AICTab$logLik[3] = logLik(m4)
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

# explore selected model
summary(m1)

# construct dataframe with relevant data
data_model = data.frame(
  Hostplant = factor(c("Heterophyllum", "Heterophyllum", "Oleraceum",  "Oleraceum")),
  Patry = factor(c("Allopatry",  "Sympatry","Allopatry", "Sympatry")),
  BL = c(4.50494,  4.62357, 4.52707, 4.43659)
)

# extract standard error
se <- summary(m1)$coef[, "Std. Error"]

# plot results in barchart
ggplot(data_model, aes(x = Hostplant, y = BL, fill = Patry)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.7) +
  labs(title = "",
       x = "",
       y = "Body length (mm)") +
  theme_minimal() + 
  theme(legend.position = "top", 
        aspect.ratio=1, 
        axis.title.y = element_text(margin = margin(t = 0, r = 16, b = 0, l = 0))) +
  geom_errorbar(aes(ymax = BL + se, ymin = BL- se),
                position = position_dodge(width = 0.8), width = 0.25) +
  coord_cartesian(ylim = c(3, 4.7)) +
  scale_fill_manual(values = c("Allopatry" = "#5ec962", "Sympatry" = "#3b528b"))


##########################################################  * OL/BL ratio----

# calculate ovipositor to bodylength ratio
data$OL_ratio = data$OL / data$BL
#m correct dataset
fdata = subset(data, Sex == "Female"& !is.na(data$OL) & BL > 3.6,)


# fit multiple models for model selection
m1 = glm(OL_ratio ~ Patry * Hostplant, data = fdata)

m2 = glm(OL_ratio ~ Patry + Hostplant, data = fdata)

m3 = glm(OL_ratio ~ Patry, data = fdata)

m4 = glm(OL_ratio ~ Hostplant , data = fdata)


# construct table for model selection
mlist = c(m1, m2, m3, m4)
AICTab = AIC(m1, m2, m3, m4)
AICTab$logLik[1] = logLik(m1)
AICTab$logLik[2] = logLik(m2)
AICTab$logLik[3] = logLik(m3)
AICTab$logLik[3] = logLik(m4)
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

# explore result of model selection
summary(m1)


# construct dataframe with results
data_model = data.frame(
  Hostplant = factor(c("Heterophyllum", "Heterophyllum", "Oleraceum",  "Oleraceum")),
  Patry = factor(c("Allopatry",  "Sympatry","Allopatry", "Sympatry")),
  OL = c(0.400357,  0.370374, 0.368086, 0.402672)
)

#extract standard error from model
se <- summary(m1)$coef[, "Std. Error"]

# plot data in barchart
ggplot(data_model, aes(x = Hostplant, y = OL, fill = Patry)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.7) +
  labs(title = "",
       x = "",
       y = "Ovipositor length ratio") +
  theme_minimal() + 
  theme(legend.position = "top", aspect.ratio=1, axis.title.y = element_text(margin = margin(t = 0, r = 16, b = 0, l = 0))) +
  geom_errorbar(aes(ymax = OL + se, ymin = OL- se),
                position = position_dodge(width = 0.8), width = 0.25) + 
  coord_cartesian(ylim = c(0.2, 0.42)) +
  scale_fill_manual(values = c("Allopatry" = "#5ec962", "Sympatry" = "#3b528b"))






