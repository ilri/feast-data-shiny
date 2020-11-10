Two files are stored here to speed up data processing, namely:
- FEASTdatCache.rdata
- NewDataCheck.rds

These files are checked each instance to see if new data has been added. 
If these files do not exist then they will be created in the first instance. See lines 96-109 of global.R