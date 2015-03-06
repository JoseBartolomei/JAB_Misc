
library(survey)
a <- svyglm(currasth ~ ageg2 + sex + educag3 ,
            design = brfss.sd , family = quasibinomial)

a

dvnum <- sort(names(BRFSS_ADS))[c(19, 29)]
dvnum

library(plyr)

# Calculate logistic regresion to the designated dependent varaibles
list.glm <- llply(dvnum, function(i) {
 svyglm(reformulate(c('sex'), # here goes the independent variables
                    response = i), design = brfss.sd,
        family = quasibinomial)})


# Apply the t_model_funct function to beutify the regression result and then select columns
tabl.glm <- llply(list.glm, t_model_funct)[, c("OR", "SE", "p")]

tabl.glm


lapply(1:2, function (i) {
 t(sapply(tabl.glm, "[", i = i, j = "OR"))
})



####################### Test ###########

freq.current.sex <- list("NA", "NA")
freq.current.sex

Dvar <- c("~currasth", "~asthma2")
Dvar

for (i in 1:length(Dvar)){
 # Sex
 x <- as.formula(Dvar[i])
 freq.current.sex[[i]] <-
  svyby(x, ~sex,
        design = brfss.sd, FUN = svytotal, na.rm = TRUE)
}
##########################################################
Dvar <- quote("cvdinfr")

svyglm(reformulate(c('sex',  'educag3', 'sex', 'ageg2', 'incomg',
                     'marital2', 'emplrec2'), # here goes the independent variables
                   response = Dvar), design = brfss.sd,
       family = quasibinomial)


#########################################
