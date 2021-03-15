# ###Install packages###
# source("http://bioconductor.org/biocLite.R")
# biocLite("sva")
# biocLite("curatedOvarianData")
# browseVignettes("curatedOvarianData")
# install.packages("devtools") #Install devtools
# devtools::install_github("AleAviP/BFR.BE")
# install.packages(cate)
# install.packages(caret)

###Set up libraries####
library(BFR.BE) #My code
library(sva) #COMBAT
library(cate) #MLE
library(curatedOvarianData) #OV data
library(caret)
#####Loading data####
seed<-123
set.seed(seed)
#Combine function
combine <- function(X1, X2) {
  fids <- intersect(row.names(exprs(X1)),row.names(exprs(X2)))
  X1 <- exprs(X1)[fids,]
  X2 <- exprs(X2)[fids,]
  cbind(X1,X2)
}

#Data with 2 batches:
#E.MTAB.386_eset Angiogenic mRNA and microRNA gene expression signature predicts a novel subtype of serous ovarian cancer.
data(E.MTAB.386_eset)
#GSE30161_eset: Multi-gene expression predictors of single drug responses to adjuvant chemotherapy in ovarian carcinoma: predicting platinum resistance.
data(GSE30161_eset)

#Setting data in our format
X_ov = combine(E.MTAB.386_eset, GSE30161_eset)
batch_ov = c(rep("EMTAB386",ncol(exprs(E.MTAB.386_eset))),
             rep("GSE30161",ncol(exprs(GSE30161_eset))))
X_ov_exp = t(X_ov)
n_ov = nrow(X_ov_exp) #187
B_ov = matrix(ncol=2, c(ifelse(batch_ov=="EMTAB386",1,0),ifelse(batch_ov=="GSE30161",0,1)))
p_b_ov = ncol(B_ov)
V_ov = matrix(nrow=n_ov,c(E.MTAB.386_eset@phenoData@data$days_to_death,GSE30161_eset@phenoData@data$days_to_death,
                          E.MTAB.386_eset@phenoData@data$tumorstage,GSE30161_eset@phenoData@data$tumorstage,
                          E.MTAB.386_eset@phenoData@data$age_at_initial_pathologic_diagnosis,GSE30161_eset@phenoData@data$age_at_initial_pathologic_diagnosis,
                          ifelse(E.MTAB.386_eset@phenoData@data$vital_status=="living",1,0),ifelse(GSE30161_eset@phenoData@data$vital_status=="living",1,0)))
V_ov_2 = as.matrix(ncol = 1, V_ov[,3])
p_v_ov = ncol(V_ov_2)
#Genes with more variability
var_OV <- data.frame(variance = apply(X_ov_exp,2,var))
index_OV_var <- c(1:ncol(X_ov_exp))[var_OV$variance>quantile(var_OV$variance,.90)]
X_ov_exp_var=X_ov_exp[,index_OV_var]
dim(X_ov_exp_var) #187 1007
#Normalising data
X_var_std = preProcess(X_ov_exp_var, method=c("center", "scale"))
X_var_std <- predict(X_var_std, X_ov_exp_var)

#####Estimation BFR####
##MOM
MOM_100=BFR.BE.EM.CV(x=X_var_std, v=V_ov_2, b=B_ov, q = 100,seed = seed)

##No correction
ev <- eigen(cor(t(X_var_std))) # get eigenvalues
#Number of factors with %covariance
S2 <- cov(X_var_std)
l2 <- eigen(S2)$values
v2 <- eigen(S2)$vector
pc2 <- X_var_std %*% v
Position(function(x) x > .9,cumsum(l2)/sum(l2)) #26
FA_var = fa.em((X_var_std),Position(function(x) x > .9,cumsum(l2)/sum(l2)),tol=0.001, maxiter = 100)

##COMBAT
mod_2 <- model.matrix(~as.factor(V_ov_2), data=data.frame(X_var_std))
combat_edata_2 <- ComBat(dat=t(X_var_std), batch=B_ov[,1], mod=mod_2)
#Number of factors with %covariance
S <- cov(t(combat_edata_2))
l <- eigen(S)$values
v <- eigen(S)$vector
pc <- t(combat_edata_2) %*% v
Position(function(x) x > .9,cumsum(l)/sum(l)) #101
FAov_var_COMBAT =  fa.em(t(combat_edata_2),Position(function(x) x > .9,cumsum(l)/sum(l)),tol=0.001, maxiter = 100)
Position(function(x) x > .7,cumsum(l)/sum(l)) #41
FAov_var_COMBAT_70 =  fa.em(t(combat_edata_2),Position(function(x) x > .7,cumsum(l)/sum(l)),tol=0.001, maxiter = 100)

##### Ploting Factors####
plot(FA_var$Z[,1:2],pch=c("o","+")[as.factor(B_ov[,1])],
     col=c("#2c7bb6","#fc8d59")[as.factor(B_ov[,1])],
     cex=1.8,cex.lab=1.5,cex.main=1.5,
     ann=FALSE, 
     axes=FALSE)
box()

plot(FAov_var_COMBAT$Z[,1:2],pch=c("o","+")[as.factor(B_ov[,1])],
     col=c("#2c7bb6","#fc8d59")[as.factor(B_ov[,1])],
     cex=1.8,cex.lab=1.5,cex.main=1.5,
     ann=FALSE, 
     axes=FALSE)
box()

plot(FA.ov(MOM_100,B_ov)[,order(-apply(MOM_100$M^2,2,sum))[1:2]],pch=c("o","+")[as.factor(B_ov[,1])],
     col=c("black","darkgray")[as.factor(survival_data$days2death<(365*3))],
     cex=1.8,cex.lab=1.5,cex.main=1.5,
     ann=FALSE, 
     axes=FALSE)
box()

plot(MOM_100$Ez[,1:2],pch=c("o","+")[as.factor(B_ov[,1])],
     col=c("black","darkgray")[as.factor(survival_data$days2death<(365*3))],
     cex=1.8,cex.lab=1.5,cex.main=1.5,
     ann=FALSE, 
     axes=FALSE)
box()
