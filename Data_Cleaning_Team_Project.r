#===================================================================================== #  
#                                      Data Viz 6242 Fall '22                          # 
#                                            Team 143                                  # 
#                   Analyzing the Localized Effect of Inflation on Election Outcomes   # 
#                                    Last updated: 10/15/2022                          # 
#===================================================================================== # 

#### set working directory
#paste your file path below
filepath <- "C:/Users/Jessi/Documents/Class Files/06- CSE 6242 Data and Visual Analytics/team project/Data/"
export   <- ""
setwd(filepath)
getwd()

#### Install Packages
install.packages("data.table")
install.packages("fredr")
install.packages("devtools")

#### Libraries
library(readxl)
library(openxlsx)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(splitstackshape)
library(data.table)
library(DataExplorer)
library(glue)
library(purrr)
library(fredr)
library(janitor)



#===================================================================================== # 
#                                       Election Data                                  # 
#                                                                                      # 
#===================================================================================== # 

# data sources
# elections --  MIT Election Data and Science Lab, 2017, "U.S. House 1976–2020", https://doi.org/10.7910/DVN/IG0UN2, Harvard Dataverse, V11, UNF:6:ry6R0P1KRBhWkIfZzKiM8A== [fileUNF]   
# presidents -- https://www.kaggle.com/datasets/harshitagpt/us-presidents?resource=download

# load data
edf <- fread("1976-2020-house.csv")
pres <- fread("us_presidents.csv")

# preview data
View(edf)

# remove unnecessary columns, calculate results, identify winner by >=50%, clean party names
edf2 <- edf %>%
  subset(select = -c(14,15,18,19,20)) %>%
  mutate(result = round((candidatevotes/totalvotes)*100,2)) %>%
  mutate(win = case_when(result >= 50.00 ~ "W", 
                         result < 50.00 ~ "L")) %>%
  select(year,state_po,state_cen, state_ic, office, district, party, stage, special, win) %>%
  filter(win == "W") %>%
  mutate(across('party', str_replace, 'DEMOCRATIC-FARMER-LABOR', 'DEMOCRAT')) %>%
  mutate(across('party', str_replace, 'INDEPENDENT-REPUBLICAN', 'REPUBLICAN'))

# lkp table for prior election winner party
inc <- edf2 %>%
  mutate(n_y = year+2) %>%
  select(n_y,state_po, district, party) %>%
  rename(inc_party = party) %>%
  rename(year = n_y)

edf3 <- left_join(edf2, inc, by = c("year", "state_po", "district"))

# add party change column, 
edf3 <- edf3 %>%
  mutate(edf3, party_change = ifelse((party==inc_party), 0, 1)) %>%
  select(-c(win))


# column for presidential party in election year
# change strings to dates
pres <- pres %>%
  mutate(start = mdy(pres$start, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))) %>%
  mutate(end = mdy(pres$end, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))) %>%
  mutate(st_year = year(start)) %>%
  mutate(end_year = year(end))

# create df to lookup presidential party
lyear <- list(unique(edf3$year))
party <- data.frame(pres$st_year,pres$end_year, pres$party)
party <- filter(party, pres.st_year >= 1974)

# create function to evaluate year and return presidential party
pf <- function(a) {
  for(r in 1:nrow(party)) {
    if((a > party[r,1]) &( a < party[r,2])) {
      return(party[r,3])
    }
  }
}

# create new data set with year and pres party

lkp_party = data.frame(elyear=integer(),pres_party=character())

for(y in lyear[[1]]){
  lkp_party = rbind(lkp_party, data.frame(elyear=y,pres_party=pf(y)))
  } 

# join party to election data
edf4 <- left_join(edf3, lkp_party, by = c("year" = "elyear"))



#===================================================================================== # 
#                                       Inflation Data                                 # 
#                 fredr provides a complete set of R bindings to the Federal           # 
#                      Reserve of Economic Data (FRED) RESTful API,                    # 
#===================================================================================== #

## uncomment on first use - environ file will open in new tab
## paste fred api into file, save and close
## restart R

#usethis::edit_r_environ()
#FRED_API_KEY="7a2a918d67a77798e679dd164c3dbb7a"

#CPIAUCNS: Consumer Price Index for All Urban Consumers: All Items in U.S. City Average
 
cpiaucns <- fredr(series_id = "CPIAUCNS",
               observation_start = as.Date("1975-01-01"),
               observation_end = as.Date("2021-12-31"),
               frequency = "a",
               aggregation_method = "avg",
               units = "lin") %>%
  select(date,value) %>%
  rename(CPI=value)

inf <- purrr::map_dfr(c("CPIAUCNS"
                        ,"CPILFENS"
                        ,"CPIHOSNS"
                        ,"CUUR0000SAH1"
                        ,"CUUR0000SAH2"
                        ,"CPIAPPNS"
                        ,"CPITRNNS"
                        ,"CUUR0000SETA01"
                        ,"CUUR0000SETB01"
                        ,"CPIMEDNS"
                        ,"CUUR0000SEEA"
                        ,"CPIOGSNS"
                        ,"CUUR0000SEGA"
                        ,"CUUR0000SA0R"
                        ,"CUUR0000SAF11"
                        ,"CUUR0000SEFJ"
                        ,"CUUR0000SEFV"
                        ,"CPIUFDNS"
                        ,"CPIFABNS"
                        ), fredr)

params <- list(
  series_id = c("CPIAUCNS"
                ,"CPILFENS"
                ,"CPIHOSNS"
                ,"CUUR0000SAH1"
                ,"CUUR0000SAH2"
                ,"CPIAPPNS"
                ,"CPITRNNS"
                ,"CUUR0000SETA01"
                ,"CUUR0000SETB01"
                ,"CPIMEDNS"
                ,"CUUR0000SEEA"
                ,"CPIOGSNS"
                ,"CUUR0000SEGA"
                ,"CUUR0000SA0R"
                ,"CUUR0000SAF11"
                ,"CUUR0000SEFJ"
                ,"CUUR0000SEFV"
                ,"CPIUFDNS"
                ,"CPIFABNS"
                ),
  frequency = c("a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a"),
  aggregation_method = c("avg","avg","avg","avg","avg","avg","avg","avg","avg","avg","avg","avg","avg","avg","avg","avg","avg","avg","avg"),
  units = c("lin","lin","lin","lin","lin","lin","lin","lin","lin","lin","lin","lin","lin","lin","lin","lin","lin","lin","lin")
)

inf <- pmap_dfr(
  .l = params,
  .f = ~ fredr(series_id = ..1, frequency = ..2, aggregation_method = ..3, units = ..4)
)

inf2 <- inf %>%
  mutate(year = year(date)) %>%
  select(year, series_id, value) %>%
  pivot_wider(names_from = series_id, values_from = value) %>%
  filter(between(year, 1976, 2021)) %>%
  rename(CPI=CPIAUCNS
         ,CPI_Ex=CPILFENS
         ,Housing=CPIHOSNS
         ,Housing_Shelter=CUUR0000SAH1
         ,Housing_Fuel=CUUR0000SAH2
         ,Apparel=CPIAPPNS
         ,Trans=CPITRNNS
         ,Trans_New_Car=CUUR0000SETA01
         ,Trans_Gas=CUUR0000SETB01
         ,Med=CPIMEDNS
         ,Ed=CUUR0000SEEA
         ,Other_Good=CPIOGSNS
         ,Tobacco=CUUR0000SEGA
         ,Purchase_Power=CUUR0000SA0R
         ,Food_Home=CUUR0000SAF11
         ,Food_Dairy=CUUR0000SEFJ
         ,Food_Away=CUUR0000SEFV
         ,Food=CPIUFDNS
         ,Food_Bev=CPIFABNS)
  
##################################################################################################
#                                                                                                #
#                         Combine Election and Inflation Data Sets                               #
#                                                                                                #
##################################################################################################

final <- left_join(edf4, inf2, c("year"))

#final <- final[!(final$year==1976),]
final2 <- na.omit(final)

#sum(is.na(final$inc_party))
#colSums(is.na(final))

##################################################################################################
#                                                                                                #
#                                        Exporting Tables                                        #
#                                                                                                #
##################################################################################################

# Creating workbook
wb <- createWorkbook()

#worksheet for each data set
addWorksheet(wb, "final", gridLines = FALSE)

#add data to worksheets
writeDataTable(wb, "final", x = final2, tableStyle = "TableStyleMedium2", withFilter = TRUE)

# Saving excel file
saveWorkbook(wb, file = paste0(filepath, "final_data.xlsx"), overwrite = TRUE)


