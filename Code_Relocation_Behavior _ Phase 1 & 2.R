library(datasets)
library(randomForest)
library(ggplot2, warn.conflicts = FALSE)
library(pdp)
library(ggplot2)
library(caret)
library(beepr)
library(knitr)
library(ROSE)
library(e1071)
library(DMwR2)
library(smotefamily)
library(dplyr)
library(scutr)
library(extrafont)



library(tictoc)
library(ggpubr)
library(scales)
library(magrittr)
library(dplyr)
#***  clearing environment console and remove variables
rm(list=ls())

# options(max.print=5000)

#*** getting the directory of Scripts and dataset:

current_directory = dirname(rstudioapi::getSourceEditorContext()$path)


#*** setting the current directory of the files as the active direcory

setwd(current_directory)
#*** creating a folder names "results" to save the results of each step.
dir.create("results", showWarnings = TRUE, recursive = FALSE, mode = "0777")


CALIB <- read.csv("data/a2021_1000.csv", header = TRUE)

Year<- as.factor(CALIB$Year)
mtry <- as.factor(CALIB[which(CALIB$Year == 2021), "m"])
fig2<- ggplot( subset(CALIB, Year %in% c(2021)), mapping= aes(x = n,
                               y = e, color = mtry)) +coord_cartesian(xlim=c(250,300))+
  geom_point()+
  geom_line()+
  labs(title="RandomForest OOB Error Vs ntree",
       x="Number of trees (n)", y= "OOB Error") +
  theme(text = element_text(family="Times New Roman", face="bold",size=12), plot.title = element_text(( hjust=0.5)))

dev.off()
fig2
ggsave("results/3.png", plot = fig2, device = "png" )




#Data preparation 2019

dts19 <- read.csv("data/2019R.csv", header = TRUE)
# row.names(dts19) <- dts19[,1]

# nrow(dts19)
#Missing Values_Filter out
# miss19 <- filter(dts19, dts19$RELOC != 1 & (is.na(dts19$LVAL) | is.na(dts19$VAL_SQ)  ))

# dt19 <- anti_join(dts19,miss19)

# dt19$SQ2 <- na_if(dt19$SQ2, 0)
# dt19_missVAL <- subset(dt19,select = c(LAT, LON, LVAL, SQ2, VAL_SQ))
# nrow(dt19_missVAL)
# nrow(dt19)
# #Missing_IMputation_ KNNImputation
# knnoutput <- knnImputation(dt19_missVAL, k = 3)
# 
# dt19$LVAL <- knnoutput$LVAL
# dt19$SQ2 <- knnoutput$SQ2
# dt19$VAL_SQ <- knnoutput$VAL_SQ
# 
# 
# #Set row names and response variable
# str(dt19)
# 
# dt19$RELOC <- as.character(dt19$RELOC)
# dt19$RELOC <- as.factor(dt19$RELOC)
# 


#add id colum
# d19<-mutate(d19, id = row_number())

# write.csv(d19, file = "data/2019_PRPD1BH.csv")



#Data preparation 2020

dts20 <- read.csv("data/2020R.csv", header = TRUE)
# row.names(dts20) <- dts20[,1]

# nrow(dts20)
#Missing Values_Filter out
# miss20 <- filter(dts20, dts20$RELOC != 1 & (is.na(dts20$LVAL) | is.na(dts20$VAL_SQ)  ))

# dt20 <- anti_join(dts20,miss20)

# dt20$SQ2 <- na_if(dt20$SQ2, 0)
# dt20_missVAL <- subset(dt20,select = c(LAT, LON, LVAL, SQ2, VAL_SQ))
# nrow(dt20_missVAL)
# nrow(dt20)
# #Missing_IMputation_ KNNImputation
# knnoutput <- knnImputation(dt20_missVAL, k = 3)
# 
# dt20$LVAL <- knnoutput$LVAL
# dt20$SQ2 <- knnoutput$SQ2
# dt20$VAL_SQ <- knnoutput$VAL_SQ

#Set row names and response variable


# dt20$RELOC <- as.character(dt20$RELOC)
# dt20$RELOC <- as.factor(dt20$RELOC)
# dt20$POPDENS <- as.numeric(dt20$POPDENS)
# dt20$RENT <- as.numeric(dt20$RENT)
#Feature Selection



#add id colum
# d20<-mutate(d20, id = row_number())

# write.csv(d20, file = "data/2020_PRPD1BH.csv")





#Data preparation 2021

dts21 <- read.csv("data/2021R.csv", header = TRUE)
 # row.names(dts21) <- dts21[,1]
# 
# nrow(dts21)
# dts21
#Missing Values_Filter out
# miss21 <- filter(dts21, dts21$RELOC != 1 & (is.na(dts21$LVAL) | is.na(dts21$VAL_SQ)  ))
# 
# dt21 <- anti_join(dts21,miss21)

# dt21$SQ2 <- na_if(dt21$SQ2, 0)
# dt21_missVAL <- subset(dt21,select = c(LAT, LON, LVAL, SQ2, VAL_SQ))
# nrow(dt21_missVAL)
# nrow(dt21)
# #Missing_IMputation_ KNNImputation
# knnoutput <- knnImputation(dt21_missVAL, k = 3)
# 
# dt21$LVAL <- knnoutput$LVAL
# dt21$SQ2 <- knnoutput$SQ2
# dt21$VAL_SQ <- knnoutput$VAL_SQ
# 
# #Set row names and response variable


# dt21$RELOC <- as.character(dt21$RELOC)
# dt21$RELOC <- as.factor(dt21$RELOC)
# dt21$POPDENS <- as.numeric(dt21$POPDENS)
# dt21$RENT <- as.numeric(dt21$RENT)
# #Feature Selection
# d21<-subset(dt21,select=-c(INFOUSA, LAT, LON, HELP,RELOC_DIST,
#                            GIS, NAICS, NAICS2,INDIFIRM,FOWN,FIRM,INDVD
#                            ,ASSET, SQ,TRUE_FRANC,HIGHTECH,SMALL,MEDIUM, BIG,L_LEAS,L_OWN,NEW_BUSI,LVAL,MTENANT
# ))
'
d19bl$EXACT <- as.integer(d19$EXACT)
d20$EXACT <- as.integer(d20$EXACT)
d21$EXACT <- as.integer(d21$EXACT)

d19$YEAR <- as.integer(d19$YEAR)
d20$YEAR <- as.integer(d20$YEAR)
d21$YEAR <- as.integer(d21$YEAR)

d19$HIGHINC <- as.integer(d19$HIGHINC)
d20$HIGHINC <- as.integer(d20$HIGHINC)
d21$HIGHINC <- as.integer(d21$HIGHINC)

d19$SMALL <- as.integer(d19$SMALL)
d20$SMALL <- as.integer(d20$SMALL)
d21$SMALL <- as.integer(d21$SMALL)

d19$EMP <- as.integer(d19$EMP)
d20$EMP <- as.integer(d20$EMP)
d21$EMP <- as.integer(d21$EMP)

d19$CREDIT <- as.integer(d19$CREDIT)
d20$CREDIT <- as.integer(d20$CREDIT)
d21$CREDIT <- as.integer(d21$CREDIT)
'



# #Feature Selection


d19<-subset(dts19,select=-c(ï..INFOUSA, NEW_DIST, NAICS, NAICS2,SQ2,LVAL,VAL_SQ,CREDIT,
                             LVAL_IMP,EMP,YEAR,SMALL,MEDIUM,FOWN,BIG,FRANC,HIGHTECH,FIRM,HIGHINC,CHANGE,HOME,METRO))

d19

d20<-subset(dts20,select=-c(ï..INFOUSA, NEW_DIST, NAICS, NAICS2,SQ2,LVAL,VAL_SQ,CREDIT,
                             LVAL_IMP,EMP,YEAR,SMALL,MEDIUM,FOWN,BIG,FRANC,HIGHTECH,FIRM,HIGHINC,CHANGE,HOME,METRO))


d21<-subset(dts21,select=-c(ï..INFOUSA, NEW_DIST, NAICS, NAICS2,SQ_2,LVAL,VAL_SQ,CREDIT,
                             LVAL_IMP,EMP,YEAR,SMALL,MEDIUM,FOWN,BIG,TRUE_FRANC,HIGHTECH,FIRM,HIGHINC,CHANGE,HOME,METRO))



write.csv(d21, file = "data/2021_PRPD1BH.csv")

d19$POPDENS <- as.numeric(d19$POPDENS)
d19$RENT <- as.numeric(d19$RENT)

d19[c(which(is.na(d19))),]
d19 <- na.omit(d19)


d20[c(which(is.na(d20))),]
d19<- na.omit(d19)

# d19 <- d19 %>% mutate_at(c("SQ_IMP", "LVAL_IMP", "LUMIX", "POPDENS","RENT","INTER",
#                            "INTER","HIGH","MAJOR","SAME"), ~(scale(.) %>% as.vector))
# 
# d20 <- d20 %>% mutate_at(c("SQ_IMP", "LVAL_IMP", "LUMIX", "POPDENS","RENT","INTER",
#                            "INTER","HIGH","MAJOR","SAME"), ~(scale(.) %>% as.vector))
# d21 <- d21 %>% mutate_at(c("SQ_IMP", "LVAL_IMP", "LUMIX", "POPDENS","RENT","INTER",
#                            "INTER","HIGH","MAJOR","SAME"), ~(scale(.) %>% as.vector))
#standardize Variables










d19$EXACT <- as.integer(d19$EXACT)
d20$EXACT <- as.integer(d20$EXACT)
d21$EXACT <- as.integer(d21$EXACT)

d19$YEAR <- as.integer(d19$YEAR)
d20$YEAR <- as.integer(d20$YEAR)
d21$YEAR <- as.integer(d21$YEAR)

d19$HIGHINC <- as.integer(d19$HIGHINC)
d20$HIGHINC <- as.integer(d20$HIGHINC)
d21$HIGHINC <- as.integer(d21$HIGHINC)

d19$SMALL <- as.integer(d19$SMALL)
d20$SMALL <- as.integer(d20$SMALL)
d21$SMALL <- as.integer(d21$SMALL)


d19$EMP <- as.integer(d19$EMP)
d20$EMP <- as.integer(d20$EMP)
d21$EMP <- as.integer(d21$EMP)

d19$CREDIT <- as.integer(d19$CREDIT)
d20$CREDIT <- as.integer(d20$CREDIT)
d21$CREDIT <- as.integer(d21$CREDIT)

d19$SAME <- as.integer(d19$SAME)
d20$SAME <- as.integer(d20$SAME)
d21$SAME <- as.integer(d21$SAME)


d19$FRANC <- as.integer(d19$FRANC)
d20$FRANC <- as.integer(d20$FRANC)
d21$TRUE_FRANC <- as.integer(d21$TRUE_FRANC)

d19$FOWN <- as.integer(d19$FOWN)
d20$FOWN <- as.integer(d20$FOWN)
d21$FOWN <- as.integer(d21$FOWN)

d19$BIG <- as.integer(d19$BIG)
d20$BIG <- as.integer(d20$BIG)
d21$BIG <- as.integer(d21$BIG)


d19$MEDIUM <- as.integer(d19$MEDIUM)
d20$MEDIUM <- as.integer(d20$MEDIUM)
d21$MEDIUM <- as.integer(d21$MEDIUM)

d19$FIRM <- as.integer(d19$FIRM)
d20$FIRM <- as.integer(d20$FIRM)
d21$FIRM <- as.integer(d21$FIRM)

d19$METRO <- as.integer(d19$METRO)
d20$METRO <- as.integer(d20$METRO)
d21$METRO <- as.integer(d21$METRO)

d19$HOME <- as.integer(d19$HOME)
d20$HOME <- as.integer(d20$HOME)
d21$HOME <- as.integer(d21$HOME)

d19$FIRM <- as.integer(d19$FIRM)
d20$FIRM <- as.integer(d20$FIRM)
d21$FIRM <- as.integer(d21$FIRM)

d19$MEDIUM <- as.integer(d19$MEDIUM)
d20$MEDIUM <- as.integer(d20$MEDIUM)
d21$MEDIUM <- as.integer(d21$MEDIUM)


d19$CHANGE <- as.integer(d19$CHANGE)
d20$CHANGE <- as.integer(d20$CHANGE)
d21$CHANGE <- as.integer(d21$CHANGE)

d19$CHANGE <- as.integer(d19$CHANGE)
d20$CHANGE <- as.integer(d20$CHANGE)
d21$CHANGE <- as.integer(d21$CHANGE)


d19$Current <- as.character(d19$Current)
d19$Current <- as.factor(d19$Current)

d20$Current <- as.character(d20$Current)
d20$Current <- as.factor(d20$Current)

d21$Current <- as.character(d21$Current)
d21$Current <- as.factor(d21$Current)

# db20 <- na.omit(db20)
# db21 <- na.omit(db21)

#' 
#' #SCUT: SMOTE Over sampling and Cluster Based Undersampling
#' 
#' #' Validate a dataset for resampling.
#' 
#' #' @param data Dataframe to validate.
#' #' @param cls_col Column with class information.
#' #'
#' #' @return NA
#' validate_dataset <- function(data, cls_col) {
#'   if (!(cls_col %in% names(data))) {
#'     stop("Column not found in data: ", cls_col)
#'   }
#'   if (sum(!unlist(lapply(data, is.numeric))) == is.numeric(data[[cls_col]])) {
#'     stop("Data frame must be only numeric besides the class column.")
#'   }
#'   if (any(is.na(data))) {
#'     stop("Data frame cannot contain NAs.")
#'   }
#' }
#' 
#' 
#' SCUT <- function(data, cls_col, oversample = oversample_smote,
#'                  undersample = undersample_kmeans, osamp_opts = list(), usamp_opts = list(),t,y) {
#'   validate_dataset(data, cls_col)
#'   
#'   # target number of observations per class
#'   m <- round(nrow(data) / length(unique(data[[cls_col]])))
#'   u<-data[[cls_col]] == 1
#'   k <- length(u[u==TRUE])
#'   ma <- y*k
#'   mi <- t*k
#'   # bulid skeleton of output
#'   ret <- as.data.frame(matrix(nrow = 0, ncol = ncol(data)), col.names = names(data))
#'   
#'   for (cls in unique(data[[cls_col]])) {
#'     n <- sum(data[[cls_col]] == cls)
#'     if (n < m) {
#'       d_prime <- do.call(
#'         oversample,
#'         c(
#'           list(
#'             data = data,
#'             cls_col = cls_col,
#'             cls = cls,
#'             m = mi
#'           ),
#'           osamp_opts
#'         )
#'       )
#'       
#'       ret <- rbind(ret, d_prime)
#'     }
#'     else if (n > m) {
#'       d_prime <- do.call(
#'         undersample,
#'         c(
#'           list(
#'             data = data,
#'             cls_col = cls_col,
#'             cls = cls,
#'             m = ma
#'           ),
#'           usamp_opts
#'         )
#'       )
#'       
#'       ret <- rbind(ret, d_prime)
#'     }
#'     else {
#'       # this class is already balanced
#'       ret <- rbind(ret, data[data[[cls_col]] == cls, ])
#'     }
#'   }
#'   
#'   rownames(ret) <- NULL
#'   return(ret)
#' }
#' 
#' 

# # Loading package
# library(fpc)
# 
# # Remove label form dataset
# db19db<- subset(db19,select = -c(RELOC))
# db19db<-  db19db[sample(nrow(db19db), 2000, replace = FALSE, prob = NULL),]


# table(db19$RELOC)
# table(db20$RELOC)
# table(db21$RELOC)
# 
# db19 <- subset(db19, select = -c(HIGHTECH,FOWN,TRUE_FRANC))
# db20 <- subset(db20, select = -c(HIGHTECH,FOWN,TRUE_FRANC))
# db21 <- subset(db21, select = -c(HIGHTECH,FOWN,TRUE_FRANC))

# db19
# 
# str(db19)
# str(db20)
# str(db21)
# db19bl <- SCUT(db19, "RELOC" ,  usamp_opts = list(k=6104),t=7,y=7)
# db20bl <- SCUT(db20, "RELOC" ,  usamp_opts = list(k=5043 ),t=8,y=8)
# db21bl <- SCUT(db21, "RELOC" ,  usamp_opts = list(k=15713 ),t=5,y=5)
# 
# table(db19bl$RELOC)
# table(db20bl$RELOC)
# table(db21bl$RELOC)
# write.csv(db19bl, file = "data/db19bl1.csv")
# write.csv(db20bl, file = "data/db20bl1.csv")
# write.csv(db21bl, file = "data/db21bl1.csv")
# 
# db19bl <- read.csv("data/db19bl1.csv", header = TRUE)
# db19bl <- subset(db19bl, select = -c(X))
# db20bl <- read.csv("data/db20bl1.csv", header = TRUE)
# db20bl <- subset(db20bl, select = -c(X ))
# db21bl <- read.csv("data/db21bl1.csv", header = TRUE)
# db21bl <- subset(db21bl, select = -c(X ))
# 
# db19bl$RELOC <- as.character(db19bl$RELOC)
# db19bl$RELOC <- as.factor(db19bl$RELOC)
# 
# db20bl$RELOC <- as.character(db20bl$RELOC)
# db20bl$RELOC <- as.factor(db20bl$RELOC)
# 
# db21bl$RELOC <- as.character(db21bl$RELOC)
# db21bl$RELOC <- as.factor(db21bl$RELOC)
# 





computeOOBErrEst <- function (x)
{
  cm <- x$confusion
  cm <- cm[, -ncol(cm)]
  1 - sum(diag(cm)) / sum(cm)
}

computrecall <- function (x)
{
  cm <- x$confusion
  cm <- cm[, -ncol(cm)]
  cm[2,2] / sum(cm[2,2],cm[2,1])
}



trainControl(method = "cv", number = 10)

rfp2<-train(Current~., d19, method = "rf", metric= "Accuracy", trControl = trainControl(), tuneGrid = NULL)

rf2p2<- randomForest( RELOC~ ., data = db19bl, ntree = 300, mtry = 12, 
                    importance = TRUE)
rf2







#model Calibration
#n


a <- as.data.frame(matrix(nrow = 0, ncol = 4))
colnames(a) <- c('n','m','e','t')
a
# ret <- rbind(ret, d_prime)
# interger optimization could be added

for (n in seq(5,495,5)) {
  tic()
  rf19<- randomForest( Current~ ., data = d19, ntree = n, mtry = 6, importance = TRUE)
  tc<-toc(quiet = T)
  t<- tc$toc-tc$tic
  e=computeOOBErrEst(rf19)
  
  a <- rbind(a, c(n,2019,e,t))
  
  
  
}
write.csv(a, file = "202019_PH2_12.csv")
for (n in seq(500,1000,10)) {
  
  tic()
  rf19<- randomForest( Current~ ., data = d19, ntree = n, mtry = 6, importance = TRUE)
  tc<-toc(quiet = T)
  t<- tc$toc-tc$tic
  e=computeOOBErrEst(rf19)
  
  a <- rbind(a, c(n,2019,e,t))
  
  
  
  
}

write.csv(a, file = "202019_PH2_1.csv")


for (n in seq(5,495,5)) {
  
  rf19<- randomForest( Current~ ., data = d19, ntree = n, mtry = 6, importance = TRUE)
  e=computeOOBErrEst(rf19)
  r=computrecall(rf19)
  a <- rbind(a, c(n,2019,e,r))
  
  
  
}

for (n in seq(500,1000,10)) {
  
  rf19<- randomForest( Current~ ., data = d19, ntree = n, mtry =6, importance = TRUE)
  e=computeOOBErrEst(rf19)
  r=computrecall(rf19)
  a <- rbind(a, c(n,2019,e,r))
  
  
  
}

write.csv(a, file = "2021_PH2_2.csv")
colnames(a) <- c('n','m','e','r')
a
Year<- as.factor(a$m)
mtry <- as.factor(CALIB[which(CALIB$Year == 2021), "m"])
fig2<- ggplot( a, mapping= aes(x = n, y = e, color = Year)) +
  geom_point()+
  geom_line()+
  labs(title="RandomForest OOB Error Vs ntree",
       x="Number of trees (n)", y= "OOB Error") +
  theme(text = element_text(family="Times New Roman", face="bold",size=12), plot.title = element_text(( hjust=0.5)))

dev.off()
fig2
ggsave("results/3.png", plot = fig2, device = "png" )


for (n in seq(10,100,10)) {
  
  rf21<- randomForest( as.factor(RELOC)~ ., data = db21bl, ntree = n, mtry = 5, importance = TRUE)
  e=computeOOBErrEst(rf2)
  r=computrecall(rf2)
  a <- rbind(a, c(n,21,e,r))
  
  
  
}
colnames(a) <- c('n','year','e','r')
a


Year<- as.factor(a$year)
fig2<- ggplot( a, mapping= aes(x = n,
                               y = e, color = Year)) +
  geom_point()+
  geom_line()+
  labs(title="RandomForest OOB Error Vs ntree",
       x="Number of trees (n)", y= "OOB Error") +
  theme(text = element_text(family="Times New Roman", face="bold",size=12), plot.title = element_text(( hjust=0.5)))

dev.off()
fig2
ggsave("results/3.png", plot = fig2, device = "png" )


#m,d

b <- as.data.frame(matrix(nrow = 0, ncol = 5))
colnames(b) <- c('year','m','d','e','r')




for (d in seq(25,4000,25)) {
  for (m in seq(4,12,4)){
    rf21<- randomForest( Current~ ., data = d21, ntree = 300, mtry = m, 
                        importance = TRUE, maxnodes=d)
    e=computeOOBErrEst(rf21)
    r=computrecall(rf21)
    b <- rbind(b, c(21,m,d,e,r))



  }}
write.csv(b, file = "2021_PH2_dm.csv")


for (d in seq(20,3000,20)) {
  for (m in seq(4,12,4)){
    rf19<- randomForest( Current~ ., data = d19, ntree = 300, mtry = m, 
                         importance = TRUE, maxnodes=d)
    e=computeOOBErrEst(rf19)
    r=computrecall(rf19)
    b <- rbind(b, c(19,m,d,e,r))
    
    
    
  }}


M<- as.factor(a$m)
fig1<- ggplot( a, mapping= aes(x = n,
                                      y = e, color = M)) +
  geom_point()+
  geom_line()+
  labs(title="RandomForest OOB Error Vs ntree",
       x="Number of trees (n)", y= "OOB Error") +
  theme(text = element_text(family="Times New Roman", face="bold",size=12), plot.title = element_text(( hjust=0.5)))

dev.off()
fig1
ggsave("results/1.png", plot = fig1, device = "png" )



x <- as.data.frame(matrix(nrow = 0, ncol = 4))
colnames(x) <- c('m','d','e','r')
x
for (d in seq(1000,3000,500)) {
  for (m in seq(2,8,2)){
    
    rf19b<- randomForest( as.factor(RELOC)~ ., data = db19bl, ntree = 50, mtry = m, importance = TRUE, maxnodes = d )
    e=computeOOBErrEst(rf19b)
    r=computrecall(rf19b)
    x <- rbind(x, c(m,d,e,r))
    
    
    
  }} 
beep()
x
colnames(x) <- c('m','d','e','r')

M<- as.factor(x$m)


Year<- as.factor(a$year)
fig2<- ggplot( x, mapping= aes(x = d,
                               y = e, color= M)) +
  geom_point()+
  geom_line()+
  labs(title="RandomForest OOB Error Vs ntree",
       x="Nodesize(d)", y= "OOB Error") +
  theme(text = element_text(family="Times New Roman", face="bold",size=12), plot.title = element_text(( hjust=0.5)))

dev.off()
fig2
ggsave("results/m&d2.png", plot = fig2, device = "png" )


#Model Calibrated, Variable Importance
# 
# db19bl1 <- subset(db19bl, select = -c())
# db20bl1 <- subset(db20bl, select = -c())
# db21bl1 <- subset(db21bl, select = -c())
d19 <- subset(d19, select = -c(FOWN,FRANC,BIG,SMALL))
d20 <- subset(d20,select = -c(HIGHINC))
d21 <- subset(d21, select = -c(YEAR,EMP,FIRM,HIGHINC,CREDIT,CHANGE,HOME,METRO,LVAL_IMP))

str(db21bl)

rf19c <- randomForest( Current~ ., data = d19, ntree = 300, 
                       mtry = 6, importance = TRUE )
rf20c <- randomForest( Current~ ., data = d20, ntree = 300, 
                       mtry = 6, importance = TRUE)
rf21c <-randomForest( Current~ ., data = d21, ntree = 300,
                      mtry = 6, importance = TRUE )


rf19c
rf20c
rf21c



imp_df19 <- data.frame(importance(rf19c, scale = FALSE, type = 1))
imp_df20 <- data.frame(importance(rf20c, scale = FALSE, type = 1))

imp_df21 <- data.frame(importance(rf21c, scale = FALSE, type = 1))
imp_df21
# imp_df19

imp_df19 <- imp_df19 %>% mutate(names = rownames(imp_df19)) %>%
  arrange(desc(MeanDecreaseAccuracy))
imp_df20 <- imp_df20 %>% mutate(names = rownames(imp_df20)) %>%
  arrange(desc(MeanDecreaseAccuracy))
imp_df21 <- imp_df21 %>% mutate(names = rownames(imp_df21)) %>%
  arrange(desc(MeanDecreaseAccuracy))



imp_df19 <-mutate(imp_df19, rank_2019 = row_number())
imp_df20 <-mutate(imp_df20, rank_2020 = row_number())
imp_df21 <-mutate(imp_df21, rank_2021 = row_number())

imp_df19$PERCENTAGE <-(format(round(100*((imp_df19$MeanDecreaseAccuracy)/sum(imp_df19$MeanDecreaseAccuracy)),1),nsmall=1))
imp_df20$PERCENTAGE <-(format(round(100*((imp_df20$MeanDecreaseAccuracy)/sum(imp_df20$MeanDecreaseAccuracy)),1),nsmall=1))
imp_df21$PERCENTAGE <-(format(round(100*((imp_df21$MeanDecreaseAccuracy)/sum(imp_df21$MeanDecreaseAccuracy)),1),nsmall=1))

imp_df19
imp_df20
imp_df21
write.csv(imp_df19, file = "data/imp_df19_1.csv")
write.csv(imp_df20, file = "data/imp_df20_1.csv")
write.csv(imp_df21, file = "data/imp_df21_1.csv")

t <- as.data.frame(matrix(nrow = 0, ncol = 7))
colnames(t) <- c('VAr_name','rank2019','rank2020','rank2021')


for (i in (1:length(imp_df19$names))) {
  t <- rbind(t, c(imp_df19[i,"names"],
                  imp_df19[which(imp_df19$names == imp_df19[i,"names"] ),"rank_2019"],
                  imp_df20[which(imp_df20$names == imp_df19[i,"names"] ),"rank_2020"],
                  imp_df21[which(imp_df21$names == imp_df19[i,"names"] ),"rank_2021"],
                  (format(round(100*((imp_df19[which(imp_df19$names == imp_df19[i,"names"] ),"MeanDecreaseAccuracy"])/sum(imp_df19$MeanDecreaseAccuracy)),1),nsmall=1)),
                  format(round(100*((imp_df20[which(imp_df20$names == imp_df19[i,"names"] ),"MeanDecreaseAccuracy"])/sum(imp_df20$MeanDecreaseAccuracy)),1),nsmall=1),
                  format(round(100*((imp_df21[which(imp_df21$names == imp_df19[i,"names"] ),"MeanDecreaseAccuracy"])/sum(imp_df21$MeanDecreaseAccuracy)), 1),nsmall = 1))
             
             )
  
}
colnames(t) <- c('VAr_name','rank19','rank2020','rank2021')


# c(computeOOBErrEst(rf19c), computrecall(rf19c))
# c(computeOOBErrEst(rf20c), computrecall(rf20c))
# c(computeOOBErrEst(rf21c), computrecall(rf21c))
# rf19c
# rf20c

library(DALEX)
explnr_rf19c <- explain(rf19c, data = d19, y= "Current" , label = "2019", verbose = FALSE)
explnr_rf20c <- explain(rf20c, data = d20, y= "Current" , label = "2020", verbose = FALSE)
explnr_rf21c <- explain(rf21c, data = d21, y= "Current" , label = "2021", verbose = FALSE)

PDP_PH2 <- as.data.frame(matrix(nrow = 0, ncol = 5))

PDP_PH2


#1_HIGH

pdp_rf19 <- model_profile(explnr_rf19c, variables ="HIGH", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="HIGH", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="HIGH", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)


PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)

#2_MAJOR

pdp_rf19 <- model_profile(explnr_rf19c, variables ="MAJOR", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="MAJOR", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="MAJOR", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)


PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)






#3_INTER

pdp_rf19 <- model_profile(explnr_rf19c, variables ="INTER", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="INTER", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="INTER", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)


PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)






#4_LUMIX

pdp_rf19 <- model_profile(explnr_rf19c, variables ="LUMIX", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="LUMIX", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="LUMIX", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)


PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)






#5_SQ_IMP

pdp_rf19 <- model_profile(explnr_rf19c, variables ="SQ_IMP", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="SQ_IMP", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="SQ_IMP", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)


PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)




#6_SAME

pdp_rf19 <- model_profile(explnr_rf19c, variables ="SAME", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="SAME", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="SAME", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)


PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)



#7_EXACT

pdp_rf19 <- model_profile(explnr_rf19c, variables ="EXACT", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="EXACT", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="EXACT", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)


PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)


#8_RENT

pdp_rf19 <- model_profile(explnr_rf19c, variables ="RENT", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="RENT", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="RENT", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)


PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)



#9_POPDENS

pdp_rf19 <- model_profile(explnr_rf19c, variables ="POPDENS", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="POPDENS", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="POPDENS", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)


PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)



#10_VAL_SQ_IMP

pdp_rf19 <- model_profile(explnr_rf19c, variables ="VAL_SQ_IMP", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="VAL_SQ_IMP", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="VAL_SQ_IMP", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)


PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)



colnames(PDP_PH2) <- c('Variable','Lable','VARIABLE','Variable Effect','v5')

PDP_PH2
write.csv(PDP_PH2, file = "results/PDP_PH2_1.csv")


# 
# #10_METRO
# 
# pdp_rf19 <- model_profile(explnr_rf19c, variables ="METRO", type = "partial")
# pdp_rf20 <- model_profile(explnr_rf20c, variables ="METRO", type = "partial")
# pdp_rf21 <- model_profile(explnr_rf21c, variables ="METRO", type = "partial")
# 
# pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
# pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
# pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)
# 
# 
# PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)
# 
# 
# #11_HOME
# 
# pdp_rf19 <- model_profile(explnr_rf19c, variables ="HOME", type = "partial")
# pdp_rf20 <- model_profile(explnr_rf20c, variables ="HOME", type = "partial")
# pdp_rf21 <- model_profile(explnr_rf21c, variables ="HOME", type = "partial")
# 
# pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
# pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
# pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)
# 
# 
# PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)
# 
# 
# #12_CHANGE
# 
# pdp_rf19 <- model_profile(explnr_rf19c, variables ="CHANGE", type = "partial")
# pdp_rf20 <- model_profile(explnr_rf20c, variables ="CHANGE", type = "partial")
# pdp_rf21 <- model_profile(explnr_rf21c, variables ="CHANGE", type = "partial")
# 
# pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
# pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
# pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)
# 
# 
# PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)
# 
# 
# #13_CREDIT
# 
# pdp_rf19 <- model_profile(explnr_rf19c, variables ="CREDIT", type = "partial")
# pdp_rf20 <- model_profile(explnr_rf20c, variables ="CREDIT", type = "partial")
# pdp_rf21 <- model_profile(explnr_rf21c, variables ="CREDIT", type = "partial")
# 
# pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
# pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
# pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)
# 
# 
# PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)
# 
# #14_FIRM
# 
# pdp_rf19 <- model_profile(explnr_rf19c, variables ="FIRM", type = "partial")
# pdp_rf20 <- model_profile(explnr_rf20c, variables ="FIRM", type = "partial")
# pdp_rf21 <- model_profile(explnr_rf21c, variables ="FIRM", type = "partial")
# 
# pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
# pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
# pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

# 
# PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf20,pdp_rf21)
# 
# 
# 
# #15_FIRM
# 
# pdp_rf19 <- model_profile(explnr_rf19c, variables ="LVAL_IMP", type = "partial")
# # pdp_rf20 <- model_profile(explnr_rf20c, variables ="LVAL_IMP", type = "partial")
# pdp_rf21 <- model_profile(explnr_rf21c, variables ="LVAL_IMP", type = "partial")
# 
# pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
# # pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
# pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)
# 
# 
# PDP_PH2 <- rbind(PDP_PH2,pdp_rf19,pdp_rf21)


colnames(PDP_PH2) <- c('Variable','Lable','VARIABLE','Variable Effect','v5')


write.csv(PDP_PH2, file = "results/PDP_PH2.csv")



'

#1_VAL_SQ_IMP

pdp_rf19 <- model_profile(explnr_rf19c, variables ="LVAL_IMP", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="LVAL_IMP", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="LVAL_IMP", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

pdp_rf21

PDP_PH2 <- rbind(PDP_PH2, c(pdp_rf19,pdp_rf20,pdp_rf21))


colnames(pdp_rf19) <- c('VAr_name','Lable','VAL_SQ_IMP','Relocatoin', 'V5')
colnames(pdp_rf20) <- c('VAr_name','Lable','VAL_SQ_IMP','Relocatoin', 'V5')
colnames(pdp_rf21) <- c('VAr_name','Lable','VAL_SQ_IMP','Relocatoin', 'V5')


fig1<- ggplot(NULL, aes(VAL_SQ_IMP ,Relocatoin)) +  coord_cartesian(xlim=c(-500,500))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06,linetype = "solid")+
  geom_line(data= pdp_rf20,color="Red",size=1.06,linetype = "longdash")+
  geom_line(data= pdp_rf21,color="Green",size=1.06,linetype = "dotdash")+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.5)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +2))

dev.off()
fig1  
ggsave("results/firg_VAL_SQ_IMP1.png", plot = fig1, device = "png")

#1_LVAL_IMP

pdp_rf19 <- model_profile(explnr_rf19c, variables ="LVAL_IMP", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="LVAL_IMP", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="LVAL_IMP", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','LVAL_IMP','Relocatoin', 'V5')
colnames(pdp_rf20) <- c('VAr_name','Lable','LVAL_IMP','Relocatoin', 'V5')
colnames(pdp_rf21) <- c('VAr_name','Lable','LVAL_IMP','Relocatoin', 'V5')


fig1<- ggplot(NULL, aes(LVAL_IMP ,Relocatoin)) +  coord_cartesian(xlim=c(-100,100))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06,linetype = "solid")+
  geom_line(data= pdp_rf20,color="Red",size=1.06,linetype = "longdash")+
  geom_line(data= pdp_rf21,color="Green",size=1.06,linetype = "dotdash")+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.5)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +2))

dev.off()
fig1  
ggsave("results/firg_LVAL_IMP1.png", plot = fig1, device = "png")


#2_CREDIT

pdp_rf19 <- model_profile(explnr_rf19c, variables ="CREDIT", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="CREDIT", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="CREDIT", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','CREDIT','Relocatoin', 'V5')
colnames(pdp_rf20) <- c('VAr_name','Lable','CREDIT','Relocatoin', 'V5')
colnames(pdp_rf21) <- c('VAr_name','Lable','CREDIT','Relocatoin', 'V5')


fig1<- ggplot(NULL, aes(CREDIT ,Relocatoin)) + coord_cartesian(xlim=c(-15,15))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06,linetype = "solid")+
  geom_line(data= pdp_rf20,color="Red",size=1.06,linetype = "longdash")+
  geom_line(data= pdp_rf21,color="Green",size=1.06,linetype = "dotdash")+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.5)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +2))

dev.off()
fig1  
ggsave("results/firg_CREDIT.png", plot = fig1, device = "png")


#3_EXACT

pdp_rf19 <- model_profile(explnr_rf19c, variables ="EXACT", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="EXACT", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="EXACT", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','EXACT','Relocatoin', 'V5')
colnames(pdp_rf20) <- c('VAr_name','Lable','EXACT','Relocatoin', 'V5')
colnames(pdp_rf21) <- c('VAr_name','Lable','EXACT','Relocatoin', 'V5')


fig1<- ggplot(NULL, aes(EXACT ,Relocatoin)) + coord_cartesian(xlim=c(-10,10))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06,linetype = "solid")+
  geom_line(data= pdp_rf20,color="Red",size=1.06,linetype = "longdash")+
  geom_line(data= pdp_rf21,color="Green",size=1.06,linetype = "dotdash")+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.5)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +2))

dev.off()
fig1  
ggsave("results/firg_EXACT.png", plot = fig1, device = "png")



#4_VAL_SQ_IMP

pdp_rf19 <- model_profile(explnr_rf19c, variables ="VAL_SQ_IMP", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="VAL_SQ_IMP", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="VAL_SQ_IMP", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','VAL_SQ_IMP','Relocatoin', 'V5')
colnames(pdp_rf20) <- c('VAr_name','Lable','VAL_SQ_IMP','Relocatoin', 'V5')
colnames(pdp_rf21) <- c('VAr_name','Lable','VAL_SQ_IMP','Relocatoin', 'V5')


fig1<- ggplot(NULL, aes(VAL_SQ_IMP ,Relocatoin)) + coord_cartesian(xlim=c(-10,10))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06,linetype = "solid")+
  geom_line(data= pdp_rf20,color="Red",size=1.06,linetype = "longdash")+
  geom_line(data= pdp_rf21,color="Green",size=1.06,linetype = "dotdash")+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.5)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +2))

dev.off()
fig1  
ggsave("results/firg_VAL_SQ_IMP1.png", plot = fig1, device = "png")


#5_MAJOR

pdp_rf19 <- model_profile(explnr_rf19c, variables ="MAJOR", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="MAJOR", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="MAJOR", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','MAJOR','Relocatoin', 'V5')
colnames(pdp_rf20) <- c('VAr_name','Lable','MAJOR','Relocatoin', 'V5')
colnames(pdp_rf21) <- c('VAr_name','Lable','MAJOR','Relocatoin', 'V5')


fig1<- ggplot(NULL, aes(MAJOR ,Relocatoin)) +coord_cartesian(xlim=c(-10,10))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06,linetype = "solid")+
  geom_line(data= pdp_rf20,color="Red",size=1.06,linetype = "longdash")+
  geom_line(data= pdp_rf21,color="Green",size=1.06,linetype = "dotdash")+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.5)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +2))

dev.off()
fig1  
ggsave("results/firg_MAJOR1.png", plot = fig1, device = "png")




#6_HIGHINC

pdp_rf19 <- model_profile(explnr_rf19c, variables ="HIGHINC", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="HIGHINC", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="HIGHINC", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','HIGHINC','Relocatoin', 'V5')
colnames(pdp_rf20) <- c('VAr_name','Lable','HIGHINC','Relocatoin', 'V5')
colnames(pdp_rf21) <- c('VAr_name','Lable','HIGHINC','Relocatoin', 'V5')
pdp_rf19


fig1<- ggplot(NULL, aes(HIGHINC ,Relocatoin)) +
  geom_line(data= pdp_rf19,color="Blue",size=1.06,linetype = "solid")+
  geom_line(data= pdp_rf20,color="Red",size=1.06,linetype = "longdash")+
  geom_line(data= pdp_rf21,color="Green",size=1.06,linetype = "dotdash")+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.5)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +2))
  
dev.off()
fig1  
ggsave("results/firg_HIGHINC.png", plot = fig1, device = "png")

  
  
  
#7_INTER

pdp_rf19 <- model_profile(explnr_rf19c, variables ="INTER", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="INTER", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="INTER", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','INTER','Relocatoin', 'V5')
colnames(pdp_rf20) <- c('VAr_name','Lable','INTER','Relocatoin', 'V5')
colnames(pdp_rf21) <- c('VAr_name','Lable','INTER','Relocatoin', 'V5')


fig1<- ggplot(NULL, aes(INTER ,Relocatoin)) +  coord_cartesian(xlim=c(-30,30))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06,linetype = "solid")+
  geom_line(data= pdp_rf20,color="Red",size=1.06,linetype = "longdash")+
  geom_line(data= pdp_rf21,color="Green",size=1.06,linetype = "dotdash")+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.5)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +2))

dev.off()
fig1  
ggsave("results/firg_INTER1.png", plot = fig1, device = "png")

  
  
#8_EMP

pdp_rf19 <- model_profile(explnr_rf19c, variables ="EMP", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="EMP", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="EMP", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','EMP','Relocatoin', 'V5')
colnames(pdp_rf20) <- c('VAr_name','Lable','EMP','Relocatoin', 'V5')
colnames(pdp_rf21) <- c('VAr_name','Lable','EMP','Relocatoin', 'V5')


fig1<- ggplot(NULL, aes(EMP ,Relocatoin)) +  coord_cartesian(xlim=c(-8,8))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06,linetype = "solid")+
  geom_line(data= pdp_rf20,color="Red",size=1.06,linetype = "longdash")+
  geom_line(data= pdp_rf21,color="Green",size=1.06,linetype = "dotdash")+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.5)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +2))+ scale_x_continuous(breaks = pretty)

dev.off()
fig1  
ggsave("results/firg_EMP.png", plot = fig1, device = "png")




#9_POPDENS

pdp_rf19 <- model_profile(explnr_rf19c, variables ="POPDENS", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="POPDENS", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="POPDENS", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','POPDENS','Relocatoin', 'V5')
colnames(pdp_rf20) <- c('VAr_name','Lable','POPDENS','Relocatoin', 'V5')
colnames(pdp_rf21) <- c('VAr_name','Lable','POPDENS','Relocatoin', 'V5')



fig3<- ggplot(NULL, aes(POPDENS ,Relocatoin)) + coord_cartesian(xlim=c(-30000,30000))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06)+
  geom_line(data= pdp_rf20,color="Red",size=1.06)+
  geom_line(data= pdp_rf21,color="Green",size=1.06)+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.5)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +2))


dev.off()
fig3  
ggsave("results/firg3_POPDENS.png", plot = fig3, device = "png")


#10_RENT

pdp_rf19 <- model_profile(explnr_rf19c, variables ="RENT", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="RENT", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="RENT", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','RENT','Relocatoin', 'V5')
colnames(pdp_rf20) <- c('VAr_name','Lable','RENT','Relocatoin', 'V5')
colnames(pdp_rf21) <- c('VAr_name','Lable','RENT','Relocatoin', 'V5')



fig5<- ggplot(NULL, aes(RENT ,Relocatoin)) + coord_cartesian(xlim=c(-1500,1500))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06)+
  geom_line(data= pdp_rf20,color="Red",size=1.06)+
  geom_line(data= pdp_rf21,color="Green",size=1.06)+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.5)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +2))

dev.off()
fig5  
ggsave("results/firg5_RENT.png", plot = fig5, device = "png")



#10_SAME

pdp_rf19 <- model_profile(explnr_rf19c, variables ="SAME", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="SAME", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="SAME", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','SAME','Relocatoin', 'V7')
colnames(pdp_rf20) <- c('VAr_name','Lable','SAME','Relocatoin', 'V7')
colnames(pdp_rf21) <- c('VAr_name','Lable','SAME','Relocatoin', 'V7')



fig7<- ggplot(NULL, aes(SAME ,Relocatoin)) + coord_cartesian(xlim=c(-20,20))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06)+
  geom_line(data= pdp_rf20,color="Red",size=1.06)+
  geom_line(data= pdp_rf21,color="Green",size=1.06)+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.6)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +3))+ scale_x_continuous(breaks= pretty_breaks())

dev.off()
fig7  
ggsave("results/firg7_SAME.png", plot = fig7, device = "png")


#10_HIGH

pdp_rf19 <- model_profile(explnr_rf19c, variables ="HIGH", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="HIGH", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="HIGH", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','HIGH','Relocatoin', 'V7')
colnames(pdp_rf20) <- c('VAr_name','Lable','HIGH','Relocatoin', 'V7')
colnames(pdp_rf21) <- c('VAr_name','Lable','HIGH','Relocatoin', 'V7')



fig7<- ggplot(NULL, aes(HIGH ,Relocatoin)) +coord_cartesian(xlim=c(-10,10))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06)+
  geom_line(data= pdp_rf20,color="Red",size=1.06)+
  geom_line(data= pdp_rf21,color="Green",size=1.06)+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.6)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +3))+ scale_x_continuous(breaks= pretty_breaks())

dev.off()
fig7  
ggsave("results/firg7_HIGH.png", plot = fig7, device = "png")


#10_CREDIT

pdp_rf19 <- model_profile(explnr_rf19c, variables ="CREDIT", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="CREDIT", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="CREDIT", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','CREDIT','Relocatoin', 'V7')
colnames(pdp_rf20) <- c('VAr_name','Lable','CREDIT','Relocatoin', 'V7')
colnames(pdp_rf21) <- c('VAr_name','Lable','CREDIT','Relocatoin', 'V7')

pdp_rf19

fig7<- ggplot(NULL, aes(CREDIT ,Relocatoin)) + coord_cartesian(xlim=c(-16,18))+
  geom_line(data= pdp_rf19,color="Blue",size=1.06)+
  geom_line(data= pdp_rf20,color="Red",size=1.06)+
  geom_line(data= pdp_rf21,color="Green",size=1.06)+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.6)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +3))+ scale_x_continuous(breaks= pretty_breaks())

dev.off()
fig7  
ggsave("results/firg7_CREDIT.png", plot = fig7, device = "png")




#10_RENT

pdp_rf19 <- model_profile(explnr_rf19c, variables ="RENT", type = "partial")
pdp_rf20 <- model_profile(explnr_rf20c, variables ="RENT", type = "partial")
pdp_rf21 <- model_profile(explnr_rf21c, variables ="RENT", type = "partial")

pdp_rf19 <- data.frame(pdp_rf19$agr_profiles)
pdp_rf20 <- data.frame(pdp_rf20$agr_profiles)
pdp_rf21 <- data.frame(pdp_rf21$agr_profiles)

colnames(pdp_rf19) <- c('VAr_name','Lable','RENT','Relocatoin', 'V7')
colnames(pdp_rf20) <- c('VAr_name','Lable','RENT','Relocatoin', 'V7')
colnames(pdp_rf21) <- c('VAr_name','Lable','RENT','Relocatoin', 'V7')

pdp_rf19

fig7<- ggplot(NULL, aes(RENT ,Relocatoin)) + 
  geom_line(data= pdp_rf19,color="Blue",size=1.06)+
  geom_line(data= pdp_rf20,color="Red",size=1.06)+
  geom_line(data= pdp_rf21,color="Green",size=1.06)+
  theme(text = element_text(family="Times New Roman", face="bold",size=12),
        plot.title = element_text(( hjust=0.6)), 
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = +3))+ scale_x_continuous(breaks= pretty_breaks())

dev.off()
fig7  
ggsave("results/firg7_RENT.png", plot = fig7, device = "png")












f<-ggarrange(fig1,fig2,fig3,fig4,fig5,fig6,fig7,fig8,fig9,fig10
          + rremove("y.text"), 
          labels = c("A", "B", "C","D","F","G","H","I","J","K"),
          ncol = 5, nrow = 2)






f









## Looping over variables ranked by importance:
data(airquality)
airquality <- na.omit(airquality)
set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., airquality, importance=TRUE)
imp <- importance(ozone.rf)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
impvar19 <- rownames(imp_df19)[order(imp_df19[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 3))
for (i in seq_along(impvar19)) {
  partialPlot(ozone.rf, airquality, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(30, 70))
}
par(op)  

fig3<- 
  ggplot( pdp_rf19$agr_profiles, mapping= aes(x = "_x_",
                                              y ="_yhat_", color= "Red")) +
  geom_point()+
  geom_line()+
  labs(title="",
       x=")", y= "") +
  theme(text = element_text(family="Times New Roman", face="bold",size=12), plot.title = element_text(( hjust=0.5)))

dev.off()
fig3

