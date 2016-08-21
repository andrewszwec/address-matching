##
##
## This script contains string matching code that will be
## used on the NBN address matching project
##
##
##
##
##
##

setwd("/Users/andrew/Documents/R/Address Matching/")


library(stringdist)

## Load some test data
## DATA PROVIDER
dp <- read.csv("data/au-500.csv", stringsAsFactors = FALSE)

## NBN CO
nbn <- read.csv("data/nbn_data.csv", stringsAsFactors = FALSE)

## Doctor the nbn data a little to make it harder to match
nbn$address_dr <- nbn$address


## Randomly mess up the address
for (i in seq(1,nrow(nbn))){
    dr.method <- as.numeric(row.names(nbn[i,])) %% 5

    if(dr.method == 0){
      # Change oo to o etc
      nbn$address_dr[i] <- gsub('oo', 'o', nbn$address[i], ignore.case = TRUE, perl = TRUE )
      nbn$address_dr[i] <- gsub('ll', 'l', nbn$address[i], ignore.case = TRUE, perl = TRUE )
      nbn$address_dr[i] <- gsub('tt', 't', nbn$address[i], ignore.case = TRUE, perl = TRUE )
      nbn$address_dr[i] <- gsub('dd', 'd', nbn$address[i], ignore.case = TRUE, perl = TRUE )
      nbn$address_dr[i] <- gsub('ff', 'f', nbn$address[i], ignore.case = TRUE, perl = TRUE )
    }
    
    if(dr.method == 1){
      # Do nothing
      nbn$address_dr[i] <- nbn$address[i]
    }
    
    if(dr.method == 2){
      # Turn St to Street
      nbn$address_dr[i] <- gsub('st', 'Street', nbn$address[i], ignore.case = TRUE, perl = TRUE )
    }
    
    if(dr.method == 3){
      # Turn Ave to Avenue
      nbn$address_dr[i] <- gsub('ave', 'Avenue', nbn$address[i], ignore.case = TRUE, perl = TRUE)
    }
    if(dr.method == 4){
      # Turn Ave to Avenue
      nbn$address_dr[i] <- gsub('bvld', 'Boulevard', nbn$address[i], ignore.case = TRUE, perl = TRUE)
    }
    
}



## Randomly mess up the postcode
## Randomly mess up the address
for (i in seq(1,nrow(nbn))){
  dr.method <- as.numeric(row.names(nbn[i,])) %% 10
  
  if(dr.method == 0){
    # Mess up the postcode
    nbn$post_dr[i] <- nbn$post[i] + 1
  }
  if(dr.method == 1){
    # Mess up the postcode
    nbn$post_dr[i] <- nbn$post[i] + 10
  }

  if(dr.method == 2){
    # Mess up the postcode
    nbn$post_dr[i] <- nbn$post[i] + 100
  }
  if(dr.method == 3){
    # Mess up the postcode
    nbn$post_dr[i] <- nbn$post[i] + 1000
  }
  if(dr.method >= 4){
    # Do nothing
    nbn$post_dr[i] <- nbn$post[i] 
  }
}

#write.csv(nbn, file="data/nbn_data_doctored.csv")

nbnx <- nbn[ , c("NBN_ID", "company_name", "address_dr", "post_dr")]

### NOW TRY STRING MATCHING
# Try StringDist Library
maxDist = 2

for (m in seq(1, nrow(nbnx))) {
  row.num <- amatch(nbnx$address_dr[m], dp$address ,maxDist=maxDist) 
  nbnx$match_row[m] <- row.num
  
  #print(paste(nbnx$address_dr[m], "matches with", dp$address[row.num]))
  
}
write.csv(nbnx, file="data/nbn_matched.csv")



# PLAN
# match:
# 1. Street names
# 2. Postcode
# 3. state

## Table of many each NBN record mapped to multiple data partner records
sql("
step1 <-     select nbn_id
          ,dp_id
          ,company_name
          ,address_dr
          ,post_dr 
          ,state
          ,address_2
          ,post_2
          ,state_2
          ,adist(address_dr, address_2) as addr_dist
          ,adist(post_dr, post_2) as post_dist
          ,adist(state, state_2) as state_dist
        from nbnx  a
        left join data.partner b  on   adist(address_dr, address_2) < addr_thr
                                and  adist(post_dr, post_2) < post_thr
                                and  adist(state, state_2) < state_thr
    ")

## Provides a score (smaller is better) -> tap off from here to get other possible matches for Tableau
step2 <- sql("
    select 
            a.*
            ,addr_dist + post_dist + state_dist as score
            ,rank() over(partition by nbn_id order by (addr_dist + post_dist + state_dist) asc) as myRank
    from step1 a
    ")

## Best Matches
step3 <- sql("
             select
                    a.*

             from step3
             where myRank = 1
             ")




