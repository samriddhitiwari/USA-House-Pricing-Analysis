View(USA_Housing)
#VIF analysis
linearmodel <- lm(SalePrice~.,data = USA_Housing)
summary(linearmodel)
library(car)
viftable<- vif(linearmodel)
viftable
sorttable <- sort(viftable,decreasing=TRUE)
sorttable
sorttable[sorttable < 10]

#Neural network (continous output) with one hidden layer
#Creating a vector of attributes with VIF less than 10
vifOut <- c("GarageCars", "GarageArea", "TotRmsAbvGrd" ,"TotalBsmtSF", "YearBuilt", "OverallQual", "FullBath", "BsmtFin", "Bedroom",
           "HalfBath", "BsmtFullBath", "Fireplaces", "LotFrontage", "Kitchen", "OverallCond", "LotArea",
           "WoodDeckSF", "BsmtHalfBath", "PoolArea")

#Creating Linear Model with shortlisted attributes
lmOut <- lm(SalePrice ~ GarageCars + GarageArea + TotRmsAbvGrd + TotalBsmtSF + YearBuilt + OverallQual + FullBath + BsmtFin + Bedroom +
              HalfBath + BsmtFullBath + Fireplaces + LotFrontage + Kitchen + OverallCond + LotArea +
              WoodDeckSF + BsmtHalfBath + PoolArea, data = USA_Housing)

summary(lmOut)

#Create new dataset with shortlisted attributes
Housing <- USA_Housing[,c("SalePrice",vifOut)]

#Installing library for Neural Network
install.packages("neuralnet", dependencies = TRUE)
library(neuralnet)


#Define the normalize function 
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}

#Define the denormalize function 
denormalize <- function(y,x){return(y*(max(x)-min(x))+min(x))}


#Normalize the data
HousingNorm <- as.data.frame(lapply(Housing,normalize))

#Separate Dataset into training and testing 
Housing_index <- sample(nrow(Housing), 0.7 * nrow(Housing),replace = FALSE)
Housing_train <- HousingNorm[Housing_index, ]
Housing_test <- HousingNorm[-Housing_index, ]

#Creating First Neural Network (One Layer with loop from 1 to 10 nodes)
cur_max_list <- list()
for (layer_one in 1:10){
  set.seed(4)
  Housing_model <- neuralnet(SalePrice ~ GarageCars + GarageArea + TotRmsAbvGrd + TotalBsmtSF + YearBuilt + OverallQual + FullBath + BsmtFin + Bedroom +
                               HalfBath + BsmtFullBath + Fireplaces + LotFrontage + Kitchen + OverallCond + LotArea +
                               WoodDeckSF + BsmtHalfBath + PoolArea,
                             data=Housing_train, hidden=layer_one, lifesign="minimal", 
                             linear.output=FALSE, threshold=0.1,stepmax=1e7)
  
  Housing_results <- compute(Housing_model, Housing_test[2:20])
  
  Housingdenorm <- denormalize(Housing_results$net.result, 
                               Housing$SalePrice[Housing_index])
  
  actualSalePrice <- Housing$SalePrice[-Housing_index]
  Housingnet_correlation <- cor(Housingdenorm,actualSalePrice)
  
  print(Housingnet_correlation)
  
  cur_max_list[paste(layer_one)] <- Housingnet_correlation
} 

cur_max_list[which.max(sapply(cur_max_list,max))]






#Creating second Neural Network (Two layer network with 10 node in each layer)
cur_max_list <- list()

for (layer_one in 1:10){
  set.seed(4)
  for (layer_two in 1:10){
    Housing_model <- neuralnet(SalePrice ~ GarageCars + GarageArea + TotRmsAbvGrd + TotalBsmtSF + YearBuilt + OverallQual + FullBath + BsmtFin + Bedroom +
                                 HalfBath + BsmtFullBath + Fireplaces + LotFrontage + Kitchen + OverallCond + LotArea +
                                 WoodDeckSF + BsmtHalfBath + PoolArea,
                               data=Housing_train, hidden=c(layer_one,layer_two), lifesign="minimal", 
                               linear.output=FALSE, threshold=0.1,stepmax=1e7)
    
    Housing_results <- compute(Housing_model, Housing_test[2:20])
    
    Housingdenorm <- denormalize(Housing_results$net.result, 
                                 Housing$SalePrice[Housing_index])
    
    actualSalePrice <- Housing$SalePrice[-Housing_index]
    Housingnet_correlation <- cor(Housingdenorm,actualSalePrice)
    
    print(Housingnet_correlation)
    
    key <- paste(layer_one, layer_two, sep='-') 
    
    cur_max_list[key] <- Housingnet_correlation
  }}

cur_max_list[which.max(sapply(cur_max_list,max))]





#Doing Analysis with statistically significant variables only 
vif6 <- c("GarageArea", "TotRmsAbvGrd" ,"TotalBsmtSF", "YearBuilt", "OverallQual", "FullBath", "BsmtFin", "Bedroom",
          "HalfBath", "Fireplaces", "LotFrontage", "Kitchen", "OverallCond", "LotArea", "WoodDeckSF")

#Create new dataset
Housing6 <- Housing[,c("SalePrice",vif6)]


#Normalize the data
Housing6Norm <- as.data.frame(lapply(Housing6,normalize))

#Separate Dataset into training and testing 
Housing6_index <- sample(nrow(Housing6), 0.7 * nrow(Housing6),replace = FALSE)
Housing6_train <- Housing6Norm[Housing6_index, ]
Housing6_test <- Housing6Norm[-Housing6_index, ]


#Creating First Neural Network (One Layer with loop from 1 to 10 nodes)
cur_max_list <- list()
for (layer_one in 1:10){
  set.seed(7)
  Housing6_model <- neuralnet(SalePrice ~ GarageArea + TotRmsAbvGrd + TotalBsmtSF + YearBuilt + OverallQual + FullBath + BsmtFin + Bedroom +
                                HalfBath + Fireplaces + LotFrontage + Kitchen + OverallCond + LotArea +
                                WoodDeckSF,
                              data=Housing6_train, hidden=layer_one, lifesign="minimal", 
                              linear.output=FALSE, threshold=0.1,stepmax=1e7)
  
  Housing6_results <- compute(Housing6_model, Housing6_test[2:16])
  
  Housing6denorm <- denormalize(Housing6_results$net.result, 
                                Housing6$SalePrice[Housing6_index])
  
  actualSalePrice6 <- Housing6$SalePrice[-Housing6_index]
  Housing6net_correlation <- cor(Housing6denorm,actualSalePrice6)
  
  print(Housing6net_correlation)
  
  cur_max_list[paste(layer_one)] <- Housing6net_correlation
} 

cur_max_list[which.max(sapply(cur_max_list,max))]