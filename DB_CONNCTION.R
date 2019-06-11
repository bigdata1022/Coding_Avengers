# How to Install RORacle Packages
# 1. Download Rtools(Current Version)
# 2. Install Rtools
# 3. Install Oracle Client 
# 4. Download Oracle client SDK file
# 5. Copy SDK file and paste in Oracle client folder
# 6. Set OCI_LIB64 system enviroment path (installed Client file path)
# 7. Set tnsname, host, port, service name
# 8. username, password 
# 9. Run function
#############################################
# ROracle Connection 

#install.packages("ROracle")


# ROracle Connection

DB_CON <- function(){
  
  library(ROracle)
  
  host <- "localhost"
  port <- 1521
  svc  <- "orcl"
  
  connect_string <- paste0(
    '(DESCRIPTION=',
    '(ADDRESS=(PROTOCOL=tcp)(HOST=', host, ')(PORT=', port, '))',
    '(CONNECT_DATA=(SERVICE_NAME=', svc, ')))')
  
  
  drv <- dbDriver('Oracle')
  usn <- "SYSTEM"
  pwd <- "bakorea0601"
  
  con <<-  dbConnect(drv
                     , username = usn
                     , password = pwd
                     , connect_string)
  
  
}


