library(remotes)
install_github("ropenspain/climaemet")
library(climaemet)
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJubXExMDAxQGFsdS51YnUuZXMiLCJqdGkiOiJlOWRmMTc0OC05NjcxLTRiMjctODM2OS01NmYxNTZjNjNjYzIiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTY2NjE2NzExNCwidXNlcklkIjoiZTlkZjE3NDgtOTY3MS00YjI3LTgzNjktNTZmMTU2YzYzY2MyIiwicm9sZSI6IiJ9.mIlOZxnREIqfi6hTeh37A_yMnryWyI0NPJzilMMAMmU", install = TRUE)

stations <- aemet_stations() # Need to have the API Key registered

knitr::kable(head(stations))

