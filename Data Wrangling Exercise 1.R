#0: Load the data in RStudio
install.packages("Rcpp")
library(Rcpp)
library(dplyr)
library(readxl)
d <- read.csv(file = "refine_original.csv", stringsAsFactors = FALSE)
is.data.frame(d)
#1: Clean up brand names

#First download a package that will give distances between strings. There are many 
#methods to measure the distance between two strings, but basically the distance between
#two strings is a small number if the strings are similar (e.g. "PHILLIPS" and "PHILIPS") and the distance is big if the
#strings are dissimilar (e.g. "AKZO" and "UNILEVER"). We will use string distance functions
#to standardize the brand names
install.packages("stringdist")
library(stringdist)
d$UpperCaseCompany<-toupper(d$company) #Change company names to upper case

#Now we will add 4 columns to the dataframe d, defined as follows:
#   phillips_dist: The Levenstein string distance between "PHILLIPS" and the brand name
#   akzo_dist: The Levenstein string distance between "AKZO" and the brand name
#   van_houten_dist: The Levenstein string distance between VAN HOUTEN" and the brand name
#   unilever_dist: The Levenstein string distance between "UNILEVER" and the brand name

d$phillips_dist<-stringdist(d$UpperCaseCompany,"PHILLIPS",method="lv")
d$akzo_dist<-stringdist(d$UpperCaseCompany,"AKZO",method="lv")
d$van_houten_dist<-stringdist(d$UpperCaseCompany,"VAN HOUTEN",method="lv")
d$unilever_dist<-stringdist(d$UpperCaseCompany,"UNILEVER",method="lv")

#Look at d to check work 
d[,c("UpperCaseCompany","phillips_dist","akzo_dist","van_houten_dist","unilever_dist")]

#When I examine the string distances that I just computed, it appears that the following is a 
#good rule for standardizing the brand names: 
#Let threshold=2
#If the phillips_dist <= threshold, then standardized_brand_name="Phillips"
#else if akzo_dist <= threshold, then standardized_brand_name="Akzo" 
#etc. 

threshold <- 2

#I'm wondering if there's some way to avoid writing the loop below; any suggestions?
for (i in 1:length(d$phillips_dist)){
  if (d$phillips_dist[i]<=threshold) {d$standardized_company_name[i]<-"Phillips"}
  else if (d$akzo_dist[i]<=threshold) {d$standardized_company_name[i]<-"Akzo"}
  else if (d$van_houten_dist[i]<=threshold) {d$standardized_company_name[i]<-"Van Houten"}
  else if (d$unilever_dist[i]<=threshold) {d$standardized_company_name[i]<-"Unilever"}
}

#Look at d to check work 
d[,c("UpperCaseCompany","standardized_company_name")]

#2: Separate product code and number
#Separate the product code and product number into separate columns i.e. add two new columns 
#called product_code and product_number, containing the product code and number respectively

#Below, we use a function called "strtoi" that converts a string to an integer; for example, strtoi("5")=5. 
#If the argument to "strtoi" is not an integer, then strtoi returns NA; for example, strtoi("a")=NA

(split<-strsplit(d$Product.code...number,"-"))
(split_unlist<-unlist(split))
(product_numbers<-strtoi(split_unlist[!is.na(strtoi(split_unlist))]))
(product_codes<-split_unlist[is.na(strtoi(split_unlist))])

d$product_codes<-product_codes
d$product_numbers<-product_numbers

#Look at d to check work 
d[,c("Product.code...number","product_codes","product_numbers")]

#3: Add product categories
#You learn that the product codes actually represent the following product categories:
  #p = Smartphone
  #v = TV
  #x = Laptop
  #q = Tablet
#In order to make the data more readable, add a column with the product category for each record.

#Again, I wonder if there's some way to avoid writing a loop as I did below: 

for (i in 1:length(d$product_code)){
  if (d$product_codes[i]=="p") {d$product_category[i]<-"Smartphone"}
  else if (d$product_codes[i]=="v") {d$product_category[i]<-"TV"}
  else if (d$product_codes[i]=="x") {d$product_category[i]<-"Laptop"}
  else if (d$product_codes[i]=="q") {d$product_category[i]<-"Tablet"}
}

#Look at d to check work 
d[,c("product_codes","product_category")]

#4: Add full address for geocoding
#You'd like to view the customer information on a map. In order to do that, the addresses need 
#to be in a form that can be easily geocoded. Create a new column full_address that concatenates
#the three address fields (address, city, country), separated by commas.

d$full_address<-paste(d$address,d$city,d$country,sep=',')

#Look at d to check work 

d[,c("address","city","country","full_address")]

#5: Create dummy variables for company and product category
#Both the company name and product category are categorical variables i.e. they take only a fixed
#set of values. In order to use them in further analysis you need to create dummy variables. 
#Create dummy binary variables for each of them with the prefix company_ and product_ i.e.
#   Add four binary (1 or 0) columns for company: company_philips, company_akzo, 
#   company_van_houten and company_unilever

#   Add four binary (1 or 0) columns for product category: product_smartphone, product_tv, 
#   product_laptop and product_tablet
d$company_phillips<-1*(d$standardized_company_name=="Phillips")
d$company_akzo<-1*(d$standardized_company_name=="Akzo")
d$company_van_houten<-1*(d$standardized_company_name=="Van Houten")
d$company_unilever<-1*(d$standardized_company_name=="Unilever")
d$product_smartphone<-1*(d$product_category=="Smartphone")
d$product_tv<-1*(d$product_category=="TV")
d$product_laptop<-1*(d$product_category=="Laptop")
d$product_tablet<-1*(d$product_category=="Tablet")

#Now check work: 
d[,c("standardized_name","company_phillips","company_akzo","company_van_houten","company_unilever")]
d[,c("product_category","product_smartphone","product_tv","product_laptop","product_tablet")]

#Now I'll remove the obsolete variables from the data frame d, as well as the auxiliary variables that I created
#(e.g. "UpperCaseCompany", "phillips_dist"...)
keepers<-c("name","standardized_company_name","product_codes","product_numbers","product_category","full_address","company_phillips",
           "company_akzo","company_van_houten","company_unilever","product_smartphone","product_tv","product_laptop","product_tablet")
           
#To keep only some columns: df<-d[,"product_category"]
d2<-d[,keepers]
head(d2)

#6: Submit the project on Github
#Include your code, the original data as a CSV file refine_original.csv, and the cleaned up data as a CSV file called refine_clean.csv.
write.csv(d2, file = "refine_clean.csv")
