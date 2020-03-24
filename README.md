# destable
Package to describe table in report
      
    To get table of descriptive statistics in paper or report
    There are 6 arguments in this fucntion;
    vars is a vector of all vars to describe;
    each variable in vars arg should be classified into 3 following arg;
    strata should be a factor var;
    function will return a data.frame which can be exported to csv;
    for normal vars, t-test is applied to get p-value; 
    for non-normal, wilcox-test is used to get p-value;
    for factor vars, chi.test is used to get p-value.
