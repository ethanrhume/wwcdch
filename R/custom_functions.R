compute.measures <- function(table){
  RD <- table[1,1]/(table[1,1]+table[2,1]) - table[1,2]/(table[1,2]+table[2,2])
  RR <- (table[1,1]/(table[1,1]+table[2,1]))/(table[1,2]/(table[1,2]+table[2,2]))
  OR <- (table[1,1]*table[2,2])/(table[1,2]*table[2,1])
  #standard errors
  p.hat <- (table[1,1]+table[1,2])/(table[1,1]+table[1,2]+table[2,1]+table[2,2]) #est proportion of cases
  p1.hat <- table[1,1]/(table[1,1]+table[2,1])
  p0.hat <- table[1,2]/(table[1,2]+table[2,2])
  n1 <- table[1,1]+table[2,1] #total exposed
  n0 <- table[1,2]+table[2,2] #total unexposed
  se.RD <- sqrt((p0.hat*(1-p0.hat)/n0) + (p1.hat*(1-p1.hat)/n1))
  se.logRR <- sqrt((1/table[1,1])-(1/n1)+(1/table[1,2])-(1/n0))
  
  #confidence intervals
  ci.RD <- c(RD-qnorm(.975)*se.RD,RD+qnorm(.975)*se.RD)
  ci.RR <- exp(c(log(RR)-qnorm(.975)*se.logRR,log(RR)+qnorm(.975)*se.logRR)) #exponentiate to get valid CI
  ci.OR <- fisher.test(table)$conf.int #extract confint from the fisher test #return results using a list
  results <- list() #setup a blank list to store results
  results[[1]] <- data.frame(RD=c(RD=RD,ci.RD),RR=c(RR=RR,ci.RR),OR=c(OR=OR,ci.OR)) #create results df
  row.names(results[[1]]) <- c("point estimate","ci low","ci high")
  
  results[[1]] #output all results
}

tosnake <- function(x) {
  # Ensure input is character
  if (!is.character(x)) x <- as.character(x)
  
  x <- tolower(x)                         # Convert to lowercase
  x <- gsub("[^a-z0-9]+", "_", x)        # Replace non-alphanumeric with underscores
  x <- gsub("^_|_$", "", x)              # Trim leading/trailing underscores
  x <- gsub("_+", "_", x)                # Collapse multiple underscores
  return(x)
}