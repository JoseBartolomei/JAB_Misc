

## This function create a table with the crude, adjusted and Delta
## from a svyglm to be use fir the display of selected model.

## Is possible to select and combine resutls to create different
## tables with the information.
##
## If you want the output to be a latex table use type = latex.

## If you want the output to be a matrix to manipulate and use the data then
## use type = matrix

Adj_ORs <- function (crude_model, adj_model, type){
  
x.names     <- factor(crude_model$xlevels[[1]])

x.length    <- length(x.names)

coef.length <- length(crude_model$coef)

  
ORc <- round(t(1/exp(crude_model$coeff)[2:x.length]), 2)

LLc <-  1/(exp(coef(crude_model) + 1.96 * (SE(crude_model))))[2:x.length]
ULc <- 1/(exp(coef(crude_model) - 1.96 * (SE(crude_model))))[2:x.length]

former.cic <- paste("(", round(LLc[1], 2), "-", round(ULc[1], 2), ")", sep="")
never.cic <- paste("(", round(LLc[2], 2), "-", round(ULc[2], 2), ")", sep="")

OR.tcrude <- cbind(Outcome = "Crude", "Current vs. Former" = paste(ORc[1], former.cic), 
                    "Current vs. Never" = paste(ORc[2], never.cic))
  
OR.adj <- round(t(1/exp(adj_model$coeff)[2:x.length]), 2)

LLa <-  1/(exp(coef(adj_model) + 1.96 * (SE(adj_model))))[2:x.length]
ULa <- 1/(exp(coef(adj_model) - 1.96 * (SE(adj_model))))[2:x.length]

former.ci <- paste("(", round(LLa[1], 2), "-", round(ULa[1], 2), ")", sep="")
never.ci <- paste("(", round(LLa[2], 2), "-", round(ULa[2], 2), ")", sep="")

OR.tadj <- cbind(Outcome = "adjusted OR", 
                "Current vs. Former" = paste(OR.adj[1], former.ci), 
                "Current vs. Never" = paste(OR.adj[2], never.ci))


# Calculate delta between the crude and the adjusted OR to determine confounding.

Delta <- cbind(Outcome = "Delta", "Current vs. Former" = round(((OR.adj[1] - ORc[1]) / ORc[1]) * 100, 2), "Current vs. Never" = round(((OR.adj[2] - ORc[2]) / ORc[2]) * 100, 2))

#join results using rbind the different results

ORc <- rbind(OR.tcrude, OR.tadj, Delta)

require(xtable)
xt1 <- xtable(ORc,  caption = "Adjusted Odds Ratios")

if(type == "matrix") {
  Type <- "matrix"
}  

if(type == "latex") {
  Type <- "latex"
}

if(Type == "matrix"){
  print(ORc) & return(ORc)
}
  
if(Type == "latex"){ 
  print(xt1) & return(xt1)
}


}
