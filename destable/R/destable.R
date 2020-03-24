#####################################################################
#####################################################################
##### Function to get table of descriptive statistics in paper or report
##### Bo Wen
##### PKU
##### 2020.03.23
#####################################################################
#####################################################################

#####################################################################
#####################################################################
##### Detial about the fucntion
##### There are 6 arguments in this fucntion;
##### vars is a vector of all vars to describe;
##### each variable in vars arg should be classified into 3 following
##### arguments;
##### strata should be a factor var
##### function will return a data.frame which can be exported to csv
##### Note: "GBK" is prefered with file-encoding
#####################################################################
#####################################################################

des_table <- function(vars,normalVars=NULL,unnormalVars=NULL,
                      factorVars=NULL,strata,digits){
  tmp <- na.omit(data[,c(vars,strata)])
  result <- data.frame()
  ## normal vars, t-test
  if(!is.null(normalVars)) {
    for(i in 1:length(normalVars)){
      m1 <- tapply(unlist(tmp[,normalVars[i]]),INDEX = unlist(tmp[,strata]),
                   FUN = function(x){
                     paste(format(round(mean(x),digits = digits),nsmall = digits),
                           "±",
                           format(round(sd(x),digits = digits),nsmall = digits))})
      tb_1 <- t(data.frame(m1))
      p1 <- t.test(unlist(tmp[,normalVars[i]])~unlist(tmp[,strata]))

      tb_1 <- data.frame(var=normalVars[i],tb_1,
                         p = format(round(p1$p.value,digits = 3),nsmall = 3))
      row.names(tb_1) <- normalVars[i]
      result <- rbind(result,tb_1)
    }
  }
  ## non-normal vars, wilcox-test
  if (!is.null(unnormalVars)) {
    for(i in 1:length(unnormalVars)){
      m1 <- tapply(unlist(tmp[,unnormalVars[i]]),INDEX = unlist(tmp[,strata]),
                   FUN = function(x){
                     paste(format(round(median(x),digits = digits),nsmall = digits),
                           "(",
                           format(round(quantile(x,0.25),digits = digits),nsmall = digits),
                           ",",
                           format(round(quantile(x,0.75),digits = digits),nsmall = digits),
                           ")")})
      tb_1 <- t(data.frame(m1))
      p1 <- t.test(unlist(tmp[,normalVars[i]])~unlist(tmp[,strata]))
      p1 <- wilcox.test(unlist(tmp[,normalVars[i]])~unlist(tmp[,strata]))

      tb_1 <- data.frame(var=unnormalVars[i],tb_1,
                         p = format(round(p1$p.value,digits = 3),nsmall = 3))
      row.names(tb_1) <- unnormalVars[i]
      result <- rbind(result,tb_1)
    }
  }
  ## factor vars, chi-square
  if (!is.null(factorVars)) {
    for(i in 1:length(factorVars)){
      m1 <- table(unlist(tmp[,factorVars[i]]),unlist(tmp[,strata]))
      m2 <- format(round(prop.table(m1,margin = 2)*100,digits = digits),nsmall = digits)
      n_col <- colnames(m1)

      tb_1 <- data.frame(var=rep(factorVars[i],length(row.names(m1))),
                         row.names = row.names(m1))
      for(j in 1:length(n_col)){
        tb_1[,j+1] <-paste(m1[,j]," (",m2[,j],")",sep = "")
        names(tb_1)[j+1] <- n_col[j]
      }
      p1 <- chisq.test(m1)

      tb_1[,2+length(n_col)] <- format(round(p1$p.value,3),nsmall = 3)
      names(tb_1)[2+length(n_col)] <- "p"
      result <- rbind(result,tb_1)
    }
  }
  return(result)
}
