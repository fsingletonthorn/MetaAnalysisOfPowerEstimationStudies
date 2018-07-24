#reanalysis of FraleyVazire2011
# Requires "FraleyVazire2011DataFile" uploaded on the OSF alongside the current file
library(readr)
FZ <- read_delim("REPLACE WITH DATA FILE LOCATION" # 
                 "\t", escape_double = FALSE, trim_ws = TRUE)
View(FZ)

# stopped, as it is too messy to extract numbers reliably from this file
