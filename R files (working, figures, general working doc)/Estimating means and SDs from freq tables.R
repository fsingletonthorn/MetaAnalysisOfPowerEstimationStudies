# estimating means and sds 

# used to import data, but not required here:
read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

# articles to check: 
dat$id[which(is.na(dat$varMedium) & is.na(dat$IQRMedium))]
# [1]  96  99  73  92  59  62  42  89 
# 112 112 113 121 115 123 
# 119
# [17] 122 124 131
# articles checked 
# 96 (only small avaliable), 99, 73 (no freq tables),92 (no freq tables), 
# 59, 62, 42, 89 (no freq tables), 112 (actually reporst SDs in table directly),
# 122 (no freq tables), 124, 131 
#  113 (no freq tables),121, 115, 123 (not avaliable), 119

# cASHEN, gEIGER 2004 # only small avaliable 
powers <- c(.99, 0.965, .92, .87, .82, .745, .645, .545, .445, .345, .245, .145, 0.07)
ns <- c(12, 1, 1, 1, 0, 0, 1, 1, 2, 10, 10, 29, 9)

# Mazen  Hemmasi & Lewis 1987
# Table 2 # paste(as.character(MAZ$small), collapse=", ")
powers <-  c(0.990, 0.965, 0.920, 0.845, 0.745, 0.645, 0.545, 0.445, 0.345, 0.245, 0.095, 0.065)
small <- c(0, 0, 2, 1, 1, 1, 0, 2, 2, 5, 16, 14)
medium <- c(6, 2, 2, 3, 4, 4, 6, 4, 7, 3, 3, 0)
large <- c(12, 6, 5, 6, 7, 3, 2, 1, 0, 1, 1, 0) 

ns<- large # etc. 

# Mone, M. A., Mueller, G. C. and Mauland, W. 1996

powers <- c(0.995, 0.965, 0.92, 0.845, 0.745, 0.645, 0.545, 0.445, 0.345, 0.245, 0.145, 0.07)
small <- c(9, 3, 1, 5, 1, 6, 7, 9, 15, 43, 75, 36)
medium <- c(49, 15, 21, 24, 20, 19, 21, 18, 8, 11, 3, 1)
large <- c(124, 22, 17, 21, 8, 10, 4, 1, 2, 0, 1, 0)

# Haase 1974 # table 1
powers <- c(0.995, 0.965, 0.92, 0.845, 0.745, 0.645, 0.545, 0.445, 0.345, 0.245, 0.145, 0.07) 
small <- c(2, 1, 1, 1, 1, 2, 6, 1, 1, 6, 16, 30) 
medium <- c(4, 8, 5, 4, 3, 7, 7, 5, 5, 12, 12, 0) 
large <- c(13, 0, 0, 4, 4, 3, 0, 5, 9, 2, 0, 0)


# Woolley, Thomas W.	A comprehensive power-analytic investigation of research in medical education 
# table 2
powers <- c(0.995, 0.965, 0.92, 0.845, 0.745, 0.645, 0.545, 0.445, 0.345, 0.245, 0.145, 0.07) 
small <- c(1, 2, 0, 1, 0, 2, 3, 5, 7, 13, 47, 19) 
medium <- c(17, 7, 9, 11, 7, 11, 13, 8, 8, 6, 3, 0)
large <- c(50, 11, 8, 14, 6, 2, 5, 2, 1, 1, 0, 0)
 
## Borkowski, Susan C. Mary Jeanne Welsh and Zhang, Qinke	An Analysis of Statistical Power in Behavioral Accounting Research. Table 3
powers <- c(0.97, 0.92, 0.845, 0.745, 0.645, 0.545, 0.445, 0.345, 0.245, 0.145, 0.045) 
small <- c(0, 0, 1, 2, 1, 0, 4, 5, 20, 50, 13) 
medium <- c(10, 11, 13, 8, 12, 16, 12, 9, 5, 0, 0) 
large <- c(51, 12, 14, 11, 1, 6, 0, 1, 0, 0, 0)

# Woolley, Thomas W. and Dawson, George O.	A Follow-up Power Analysis of the Statistical Tests Used in the Journal of Research in Science Teaching. Table III
powers <- c(0.995, 0.965, 0.92, 0.845, 0.745, 0.645, 0.545, 0.445, 0.345, 0.245, 0.145, 0.07) 
small <- c(1, 3, 2, 2, 2, 5, 8, 9, 8, 29, 79, 44) 
medium <- c(29, 8, 8, 22, 21, 13, 23, 24, 18, 13, 11, 2) 
large <- c(65, 30, 21, 26, 15, 14, 7, 5, 4, 4, 1, 0)

# Overland, C. T.	Statistical Power in the Journal of Research in Music Education (2000-2010): A Retrospective Power Analysis	Bulletin of the Council for Research in Music Education	2014	
powers <- c(0.965, 0.92, 0.845, 0.745, 0.645, 0.545, 0.445, 0.345, 0.245, 0.145, 0.045) 
small <- c(4, 0, 2, 0, 4, 4, 6, 16, 17, 37, 35) 
medium <- c(29, 3, 14, 16, 8, 12, 11, 9, 8, 11, 3) 
large <- c(70, 9, 11, 12, 5, 4, 3, 2, 2, 5, 2)

# Mazen, A. M. M., Hemmasi, M. and Lewis, M. F	Assessment of statistical power in contemporary strategy research
powers <- c(0.995, 0.965, 0.92, 0.845, 0.745, 0.645, 0.545, 0.445, 0.345, 0.245, 0.145, 0.07) 
small <- c(0, 0, 2, 1, 1, 1, 0, 2, 2, 5, 16, 14) 
medium <- c(6, 2, 2, 3, 4, 4, 6, 4, 7, 3, 3, 0)
large <- c(12, 6, 5, 6, 7, 3, 2, 1, 0, 1, 1, 0)



# used this to print the above, after preprocessing in excel
temp<-read.excel(header = T)
paste0(c("powers <- c(", paste(as.character(temp$Power), collapse=", "), ") \n small <- c(",
        paste(as.character(temp$small), collapse=", "),  ") \n medium <- c(",
        paste(as.character(temp$medium), collapse=", "),  ") \n large <- c(",
        paste(as.character(temp$large), collapse=", "), ")"), collapse = "")

# all estiamted using: 
ns<- medium # etc. 

n <-sum(ns)
estMean <- sum(powers * ns) / n
estVar <-  (sum(ns * powers^2)/n) - estMean^2
estSD <- sqrt(estVar)
estSD


paste(as.character(temp), collapse=", ")