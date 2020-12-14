#Check Usage -----
checks=checkPackageUsage()
runTests(package="sDataTable")
#Dismiss Usage Warnings -----
#  suppressUsageWarnings(checks)
###Dev Setup -----
## INSTALL: CTRL + SHIFT + B
sDevTools::clearEnv() ## CTRL + SHIFT + R
library(sDevTools)
sDevTools::loadUtils()
#Dev -----
new_fn((),#args
 desc=  ,#Function description
 return=  #Function returns
 )

