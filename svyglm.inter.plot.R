
###################################################################
#### This function plot interaction plot for a svyglm result.
####  This funct. is created for a binomial response variable
#### and x level predictor variable.
#### In addition was created using a quasibinomial link in the svyglm function
###################################################################

svyglm.inter.plot <- function(svyglm.object)
{
x.names     <- factor(glm.int$xlevels[[1]])
cov.names   <- factor(glm.int$xlevels[[2]])
x.length    <- length(x.names)
cov.length  <- length(cov.names)
coef.length <- length(glm.int$coef)
int.start   <- (x.length + cov.length) -1
Intercept   <- glm.int$coeff[1]
Beta1       <- glm.int$coeff[2:x.length]
Beta2       <- glm.int$coeff[x.length + 1:(cov.length - 1)]

Beta_int    <- glm.int$coeff[x.length + 1  + (cov.length ) : (coef.length - x.length)-1]
Beta_int    <- matrix(Beta_int, x.length-1, cov.length-1)


logodds <- c(c(Intercept, Intercept + Beta2), #Current 
           c(Intercept + Beta1[1], Intercept + Beta1[1] + Beta2 + Beta_int[1,]), #Former
           c(Intercept + Beta1[2], Intercept + Beta1[2] + Beta2 + Beta_int[2,])) # Never


inter.tabl <- data.frame(predictor = sort(rep(x.names, cov.length)),
              covariate = rep(cov.names, x.length),
              logodds = logodds)

## plot the created table
covar <- names(glm.int$xlevel)[2]
pdf(file = paste("G:/ORP_Workspace/BRFSS/QualityOfLife/Plots/Rplots_", covar, ".pdf", sep = ""))

with(inter.tabl, interaction.plot(x.factor = predictor, 
              trace.factor = covariate, response = logodds, fun = mean))
dev.off()

## Create a table with the ORs stratified by the covariate
## This informatin will be used if the interaction are statistical and 
## meaningfully significants

require(Hmisc)

OR.int <- cbind(round(matrix(1/exp(Beta1)),2), round(1/(exp(Beta1 + Beta_int)), 2))

SE.int <-  matrix(SE(glm.int))[c(2:x.length,
          (((length(SE(glm.int)) - (cov.length-1) * (x.length-1) + 1))) : length(SE(glm.int))),]

SE.int <- matrix(SE.int, nrow = x.length-1, ncol = cov.length)

logodds.int <- cbind(round(matrix(Beta1),2), round(Beta1 + Beta_int, 2))

LL    <- round(1/exp(logodds.int + (1.96 * SE.int)), 2)

UL    <- round(1/exp(logodds.int - (1.96 * SE.int)), 2)


show.latex(latex.default(OR.int,
                        title = "Variables",
                        file = "",
                        rowname = as.character(x.names[-1]),
                        colhead = as.character(cov.names),
                        caption = "ORs by a covariate"))


show.latex(latex.default(LL,
                        title = "Variables",
                        file = "",
                        rowname = as.character(x.names[-1]),
                        colhead = as.character(cov.names),
                        caption = "ORs Lower levels by a covariate"))


show.latex(latex.default(UL,
                        title = "Variables",
                        file = "",
                        rowname = as.character(x.names[-1]),
                        colhead = as.character(cov.names),
                        caption = "ORs Upper levels by a covariate"))
}
