### PCA CODE EXAMPLE ###

# DR
# R version: 4.2.2

### Code below was used for the PCA analysis in Romualdi et al.(2023) MPB-fire scoping review ###

df <- read_excel("MPB_effects_on_wildfire_lit_review_June_2022_v002.xlsx", sheet = 7, skip = 1) # read in your data frame

#################################### Preparing the Data Frame ##########################################

### Response must be a single variable ###

# The fire response post-MPB in this example has three categorical levels: negative, neutral, and positive

# Making multiple response columns numeric
r_num <- lapply(df[1:24,4:6], as.numeric)
r_num

# Even if there is one response column in the df, it must be numeric

response <- (-1*(r_num$Negative)+0*(r_num$Neutral))+r_num$Positive
response
# 1 = -1 (negative response)
# 2 = 0 (neutral response)
# 3 = 1 (positive response)


### Create a df with all the selected indicators/predictors ###

df2 <- data.frame(response,df[,c(-4,-5,-6)])
colnames(df2)

GLOBAL <- df2[,c(2, 5:51)] # excluding ecoregion column 55
colnames(GLOBAL)
GLOBAL_resp <- df2[,c(1:4, 5:51)]
GLOBAL_resp

for (i in 1:ncol(GLOBAL)){
  GLOBAL[,i] = as.numeric(GLOBAL[,i]) # This loop was used to fix a technical issue that needed all variables in PCA to be numeric
  i = i+1
}

############################################
### Helligner transforming the GLOBAL df ###
############################################

# https://www.davidzeleny.net/anadat-r/doku.php/en:confusions#:~:text=Ad%20b)%20Hellinger%20transformation%20converts,species%20abundances%2C%20but%20relative%20abundances.

# PCA is sensitive to double-zero issue
# So Hellinger transforming data gets rid of the double-zero issue in the data (i.e., any samples that are all zero)

# vegan
GLOBAL2<- decostand(GLOBAL[,-1], method="hellinger") # Hellinger transformation - see above link
GLOBAL2 <- GLOBAL2[,apply(GLOBAL2, 2, var, na.rm=TRUE) != 0] # removes variable columns with zero variance - Hellinger transformed
colnames(GLOBAL2)
# 46 total variables


#################################### Broken-Stick & PCA Analyses ##########################################

########################
### CLEAN GLOBAL PCA ###
########################

### GGBIPLOT ###
library(ggplot2)
install.packages("devtools", repo="http://cran.us.r-project.org")
library(devtools)
remotes::install_github('vqv/ggbiplot')
library(ggbiplot)

### Hellinger transformed - GLOBAL2
GLOBAL2 <- GLOBAL2[,apply(GLOBAL2, 2, var, na.rm=TRUE) != 0] # removes variable columns with zero variance - Hellinger transformed (repeated from above for clarity)
global.pca2 <- prcomp(GLOBAL2, scale. = TRUE)
global.pca2
summary(global.pca2)


plot(global.pca2)
# PC1 - explains 20.95%
# PC2 - explains 13.38%
# top 2 PCA - explains 34.33% total variation in data
# top 5 PCA - explains 63.02% total variation in data


###  Broken stick model (MacArthur 1957)
ev = global.pca2$sdev^2 # Hellinger transformed (as seen above)

evplot = function(ev) {
  n = length(ev)
  bsm = data.frame(j=seq(1:n), p=0)
  bsm$p[1] = 1/n
  for (i in 2:n) bsm$p[i] = bsm$p[i-1] + (1/(n + 1 - i))
  bsm$p = 100*bsm$p/n
  # Plot eigenvalues and % of variation for each axis
  op = par(mfrow=c(1,1),omi=c(0.1,0.3,0.1,0.1), mar=c(1, 1, 1, 1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("center", "Average eigenvalue", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
          main="Broken Stick Distribution", col=c("bisque",2), las=2)
  legend("center", c("Percent (%) Observed Variance", "Percent (%) Broken Stick Model Variance"), 
         pch=15, col=c("bisque",2), bty="n")
  par(op)
}

evplot(ev)
# first plot tells us to choose PC with eignevalues > avergae eigenvalues - 8 PC
# second plot tell us to choose PC when % eigenvalue variance is greater than expected variance under BS model - PC 1-4, 6
ev

# Finding loadings
global_loading2 <- scores(global.pca2, choices = 1:2, display = "species", scaling = 0) # Hellinger transformed
global_loading2
global_loading2 <- as.data.frame(global_loading2)
# Square loadings
g_load2_hel <- (global_loading2)^2
g_load2_hel
g_load2_hel <- as.data.frame(g_load2_hel)

# Writing to excel
library(writexl)
write_xlsx(g_load2_hel,"/Users/doria/Documents/Literature Review/MPB_FIRE_review_paper_CLEAN_FILTERED_GLOBAL_loadings2_hel_PC1PC2.xlsx")

# Calculating a reference broken stick distribution - omitting indicators will all 0s
nVars<- 46 # this variable represents the number of variables you are assessing (make nVars equal to the number of predictors you are analysing)
a<-brokenStick(1:nVars, nVars)  # actual function - the variable 'a' here contains the vector to compare with your loadings
a
a <- as.data.frame(a)
a
write_xlsx(a,"/Users/doria/Documents/Literature Review/MPB_FIRE_review_paper_CLEAN_FILTERED_GLOBAL_BS_loadings_52.xlsx")
sum(a) # note what the sum of the probabilities is



### PRCOMP PCA BIPLOT - HELLINGER TRANSFORMED ###
library(ggplot2)
library(ggrepel)

ggbiplot(global.pca2) #basic biplot using ggbiplot

#gg <- ggbiplot(global.pca2, obs.scale = 1, var.scale = 1, repel = TRUE,
               # groups = as.factor(GLOBAL_resp$response), ellipse = TRUE) +
 # scale_color_manual(name = 'Fire Severity Response', values = c("#3399ff", "#ffbf00", "#ff0000")) +
 #theme(legend.direction = 'horizontal', legend.position = 'top')

gg <- ggbiplot(global.pca2, obs.scale = 1, var.scale = 1, repel = TRUE,
  theme(legend.direction = 'horizontal', legend.position = 'top')
# must make response a factor (as.factor)
gg

gg2 <- gg + ggtitle("Clean & Filtered Global PCA") + 
  xlab("PC1* (20.95% variance explained)") +
  ylab("PC2* (13.38% variance explained)")
gg2 

gg3 <- gg2 + theme(plot.title = element_text(family = "sans", face = "bold", colour = "black", size = "15", hjust = 0.5,1),
                   axis.title.x = element_text(family = "sans", face = "plain", colour = "black", size = "12"),
                   axis.title.y = element_text(family = "sans", face = "plain", colour = "black", size = "12")) +
  expand_limits(x = c(-2, 2), y = c(-2, 1))
gg3

gg4 <- gg3 + theme(panel.background = element_rect(fill = "white", colour = "grey", size = 1))
gg4

gg5 <- gg4 + geom_point(size = 2) +
  geom_label_repel(aes(label = GLOBAL$ID_1),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')
gg5  # label with response colours and increased point size in inksape 

