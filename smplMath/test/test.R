#first set your file to data, preferable with csv, using read.csv.

data <- read.csv("Electricity1970.csv")

train_models_summary(data, independent_var = "cost", dependent_vars = c("labor", "capital", "fuel"))

#Or internalize the  data = read.csv("File Name Here")   set independent_var to independent variable and put all dependent variables in a character vector string

train_models_summary(data = read.csv("Electricity1970.csv"), independent_var = "cost", dependent_vars = c("labor", "capital", "fuel"))

#Data needs to be Int or numeric to work
