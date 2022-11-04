library(readr)
enf_cardio2_0 <- read_delim("enf_cardio2.0.csv", 
                            delim = "\t", escape_double = FALSE, 
                            col_names = FALSE, trim_ws = TRUE)
View(enf_cardio2_0)
#Los datos de la tabla vienen en tantos por 100.000
