library(rvest)
library(jsonlite)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(broom)
library(MASS)
library(modelr)
library(lattice)
library(hexbin)
library(gridExtra)
library(xtable)
library(splines2)
library(lubridate)
library(arm)



# prod1 <- campaignProdListMorhipo(77015)
# saveCampaignWithName(prod1)
# 
# prod2 <- campaignProdListMorhipo(77016)
# saveCampaignWithName(prod2)


##diffSales <- campaignImport('SavedDiff.csv', 'Test')
##diffSales[c("X", "CampaignID", "DateTime", "campaignID")] <- NULL


##lubridate
##ymd convert

##dummyVars
##dmy <- dummyVars("~PTypeNew", diffSales)
##tp <- data.frame(predict(dmy, newdata = diffSales))


##dateTime from Factor
# my_datetime <- as.POSIXlt("15-10-2017 16:41:00",format = "%d-%m-%Y %H:%M:%S")
# my_datetime$zone <- NULL
# my_datetime

##CorrelationMatrix
##ggcorr(allComb[2:9])




# prod1 <- campaignImport('7593620180925203151.csv','PBF_Male_1')
# prod2 <- campaignImport('7593620181001170118.csv','PBF_Male_1')
# salesMale1 <- createSalesTotals(prod1, prod2)
# pTypesMale1 <- sumCampaignProductTypes(salesMale1)
# 
# prod3 <- campaignImport('7593520180925202503.csv','PBF_Female_1')
# prod4 <- campaignImport('7593520181001170111.csv','PBF_Female_1')
# salesFemale1 <- createSalesTotals(prod3, prod4)
# pTypesFemale1 <- sumCampaignProductTypes(salesFemale1)
# 
# prod5 <- campaignImport('7488620180810215214.csv','PBF_Unisex_1')
# prod6 <- campaignImport('7488620180813164129.csv','PBF_Unisex_1')
# salesUnisex1 <- createSalesTotals(prod5, prod6)
# pTypesUnisex1 <- sumCampaignProductTypes(salesUnisex1)
# 
# prod7 <- campaignImport('7612720181002150421.csv','PBF_Unisex_2')
# prod8 <- campaignImport('7612720181008220657.csv','PBF_Unisex_2')
# salesUnisex2 <- createSalesTotals(prod7, prod8)
# pTypesUnisex2 <- sumCampaignProductTypes(salesUnisex2)
# 
# prod9 <- campaignImport('7630220181010090257.csv','PBF_Female_2')
# prod10 <- campaignImport('7630220181016133443.csv','PBF_Female_2')
# salesFemale2 <- createSalesTotals(prod9, prod10)
# pTypesFemale2 <- sumCampaignProductTypes(salesFemale2)
# 
# prod11 <- campaignImport('7630320181012093434.csv','PBF_Male_2')
# prod12 <- campaignImport('7630320181016133448.csv','PBF_Male_2')
# salesMale2 <- createSalesTotals(prod11, prod12)
# pTypesMale2 <- sumCampaignProductTypes(salesMale2)








# prod13 <- campaignImport('7648020181018170709.csv','PBF_Female_3')
# prod14 <- campaignImport('7648020181023174804.csv','PBF_Female_3')
# salesFemale3 <- createSalesTotals(prod13, prod14)
# pTypesFemale3 <- sumCampaignProductTypes(salesFemale3)


# allCombSmall <- rbind(pTypesFemale2, pTypesMale2, pTypesFemale1, pTypesMale1, pTypesUnisex1, pTypesUnisex2)
# allCombBig <- rbind(salesMale2, salesFemale2, salesMale1, salesFemale1, salesUnisex1, salesUnisex2)

campaignProdListMorhipo <- function(campaignID){
  
  baseURL <- 'https://www.morhipo.com'
  baseCampaign <- 'https://www.morhipo.com/kampanya/liste/'
  baseCampaignAfter <- paste(baseCampaign, as.character(campaignID), sep = '')
  
  campaignPage <- read_html(paste(baseCampaignAfter,'/campaign' , sep = ''))
  campaignPaginationURL <- paste(baseURL, 
                                 str_replace('/Private/ProductListNextPage/_camp_?intent=2&pg=',
                                             '_camp_', as.character(campaignID)), 
                                 sep = '')
  
  campaignProductCountSelector <- '.total_prod_count #total-product-count'
  totalProductCount <- as.numeric(html_node(campaignPage, campaignProductCountSelector) %>%
                                    html_text())
  
  pagesToCall <- (totalProductCount %/% 96) - 1 
  
  campaignProductList <- data.frame(SortID = numeric(),
                                    ModelID = integer(),
                                    VariantID = integer(),
                                    ProductID = integer(),
                                    ProductBrand = character(),
                                    ModelName = character(),
                                    Color = character(),
                                    StickerPrice = numeric(),
                                    DiscountPrice = numeric(),
                                    RStock = integer(),
                                    VStock = integer(),
                                    StockQuantity = integer())
  
  totalCount <- 0
  
  for (i in -1:pagesToCall) {
    ##print(paste(campaignPaginationURL, as.character(i), sep = ''))
    productData <- fromJSON(paste(campaignPaginationURL, as.character(i), sep = ''))
    totalCount <- totalCount + nrow(productData)
    ##print(totalCount)
    midDF <- productData[c("ModelID", "VariantID", "ProductID", 
                           "ProductBrand", "ModelName", "Color", "StickerPrice",
                           "DiscountPrice", "RStock", "VStock", 
                           "StockQuantity")]
    campaignProductList <- rbind(campaignProductList, midDF)
  }
  
  campaignProductList$SortedID <- seq.int(nrow(campaignProductList))
  campaignProductList$DateTime <- format(Sys.time(), "%Y-%m-%d %X")
  campaignProductList$campaignID <- campaignID
  campaignProductList$DiscountRate <- round(campaignProductList$DiscountPrice*100/campaignProductList$StickerPrice, 0)
  
  ##RoundedDiscountRate
  ##round(prod$DiscountPrice * 100 / prod$StickerPrice, 0)
  
  return(campaignProductList)
}

saveCampaignWithName <- function(campaignProducts){
  
  campaignID <- unlist(campaignProducts[c("campaignID")][1,], use.names = FALSE)
  date <- unlist(campaignProducts[c("DateTime")][1,], use.names = FALSE)
  date <- str_replace_all(date, ":", "")
  path <- paste('D:/R/data/WebData/morhipo.com/Campaigns/',
        as.character(campaignID), 
        date, 
        '.csv', 
        sep = '',' ', '')
  path <- str_replace_all(path, " ", "")
  path <- str_replace_all(path, '-', '')
  print(path)
  write.csv(campaignProducts,file =  path)
  
  ##list.files("D:/R/data/WebData/morhipo.com/Campaigns")
}



saveMainPageCampaigns <- function(){
  
  mainPage <- read_html("https://www.morhipo.com/kampanya/alisveris")
  
  campaignBigBanners <- html_nodes(mainPage, '.mh_promotion_container .campaign-banners')
  urlList <- html_attr(campaignBigBanners, "href")
  titleList <- html_attr(campaignBigBanners, "title")
  idList <- html_attr(campaignBigBanners, "data-campaignid")
  
  bigBanners <- data.frame(Links = urlList,
                           Titles = titleList,
                           IDs = idList,
                           Status = "BigBanner")
  
  campaignSmallBanners <- html_children(html_nodes(mainPage, '.shortblocks'))
  urlList <- html_attr(campaignSmallBanners, "href")
  titleList <- html_attr(campaignSmallBanners, "title")
  idList <- html_attr(campaignSmallBanners, "data-campaignid")
  
  smallBanners <- data.frame(Links = urlList,
                             Titles = titleList,
                             IDs = idList,
                             Status = "SmallBanner")
  
  allBanners <- rbind(bigBanners, smallBanners)
  allBanners$DateTime <- format(Sys.time(), "%Y-%m-%d %X")
  allBanners$SortID <- seq.int(nrow(allBanners))
  return(allBanners)
}


createSalesTotals <- function(campaignFirst, campaignSecond){
  
  joined <- inner_join(campaignFirst, campaignSecond, by = "VariantID")
  joined$HoursActive <- as.numeric(round(difftime(joined$DateTime.y, joined$DateTime.x, units = "hours"),0))
  joined <- joined[c("VariantID","ModelName.x" , "Color.x",
                     "StockQuantity.x", "StockQuantity.y",
                     "DiscountPrice.x", "StickerPrice.x", "SortedID.x", "campaignID.x",
                     "HoursActive")]
  joined$Sales <- joined$StockQuantity.x - joined$StockQuantity.y
  colnames(joined) <- c("VariantID","ModelName", "Color", "StockBefore", "StockAfter", 
                           "DiscountPrice", "StickerPrice" , "SortID", "CampaignID", "HoursActive", "Sales")
  joined$DiscountPerc <- round(joined$DiscountPrice*100/joined$StickerPrice, 0)
  
  if (nrow(joined[joined$DiscountPerc == "Inf",]) == 1) {
    joined[joined$DiscountPerc == "Inf",]$DiscountPerc <- 0.0
  }
  
  joined$PType <- trimws(lapply(strsplit(as.character(joined$ModelName), " "), tail, 1))
  joined$Sales[joined[c("Sales")] < 0] <- 0
  joined$StockBefore[joined[c("StockBefore")] == 0] <- 1
  joined <- getTopUsedProductTypes(joined, 10)
  total <- nrow(joined)
  weight <- as.data.frame(table(joined[c("PTypeGrouped")]))
  weight$PPerc <- round(weight$Freq * 100 / total, 2)
  weight$Freq <- NULL
  names(weight) <- c("PTypeGrouped", "PPerc")
  joined <- inner_join(joined, weight, by = "PTypeGrouped")
  joined
}

campaignImport <- function(campaignName, folderName){
  path <- paste('D:/R/data/WebData/morhipo.com/Campaigns/', folderName, sep = '')
  path <- paste(path, '/', sep = '')
  path <- paste(path, campaignName, sep = '')
  read.csv(path)
}


campaignFilesImport <- function(folderName){
  
  basePath <- 'D:/R/data/WebData/morhipo.com/Campaigns/'
  basePath <- paste(basePath, folderName, sep = '')
  basePath <- paste(basePath, '/', sep = '')
  
  ##Dep. Variables
  fileList <- paste(basePath, list.files(basePath), sep = '')
  fileNames <- paste(basePath, folderName, sep = '')
  
  total <- length(fileList)
  
}


getColumnStringFreq <- function(v){
  pastedV <- paste(v, collapse = " ")
  Val <- unlist(strsplit(pastedV, " "))
  df <- as.data.frame(table(Val))
  colnames(df) <- c("Value", "Total")
  df
}


getTopUsedProductTypes <- function(df, n){
  df1 <- getColumnStringFreq(df$PType)
  topTypes <- filter(df1, rank(desc(Total)) <= n)
  df$Value <- df$PType
  df$Value <- as.character(df$Value)
  topTypes$Value <- as.character(topTypes$Value)
  joined <- left_join(df, topTypes, by = "Value")
  joined[is.na(joined$Total), c("Value")] <- 'OtherType'
  joined$Total <- NULL
  names(joined)[names(joined) == "Value"] <- "PTypeGrouped"
  joined$PTypeGrouped <- as.factor(joined$PTypeGrouped)
  joined
}

addDummyVars <- function(df){
  dmy <- dummyVars("~PTypeGrouped", df)
  tp <- data.frame(predict(dmy, newdata = df))
  df <- cbind(df, tp)
  df
}

sumCampaignProductTypes <- function(df){
  df$IsSold <- ifelse(df$Sales > 0, NA , 0)
  df <- df %>% 
    group_by(PTypeGrouped) %>%
    summarise(SortMean = round(mean(SortID), 0), 
              PriceMean = round(mean(DiscountPrice),0),
              DiscountMean = round(mean(DiscountPerc),0),
              PPercMean = round(mean(PPerc),0),
              CountTotal = n(),
              TotalActiveHours = as.numeric(round(mean(HoursActive,0))),
              TotalSales = sum(Sales),
              TotalVariantSold = sum(is.na(IsSold)),
              TotalStock = sum(StockBefore),
              StockByVariant = round(sum(StockBefore)/n(),0)) %>%
    filter(PTypeGrouped != "OtherType")
  df
}



























