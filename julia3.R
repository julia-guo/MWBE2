########
# NTWK #
########

#UPDATED WITH GOOD MODELS AND PROPENSITY SCORE MATCHING

#new, more concise version

#Examine MOCS data
#Jonathan Auerbach
#Julia Guo
#June 2017

#Packages needed:
library(tidyverse)
library(blockmodels)
library(stringr)
library(gridExtra)
library(lubridate)
library(lme4)
library(rstanarm)
library(modelr)
library(devtools)
library(tidybayes)

#----------------------#
#     1. READ DATA     #
#----------------------#

setwd("/Users/jauerbach/Dropbox/DDC/NTWK/MOCS/")
MWBE <- read_csv("2016_10_27/MWBECertifiedList.csv") %>% 
  mutate(ETHNICITY = gsub("Caucasian Female","Non-Minority",ETHNICITY),
         `M/WBE Type` = gsub("/","", `M/WBE Type`),
         female = `M/WBE Type` %in% c("MWBE", "WBE")) %>%
  filter(`M/WBE Type` != "EBE",
         ETHNICITY != "N/A",
         FY != "FY11") %>%
  mutate(`Expiration Date` = parse_date(`Expiration Date`, format = "%d-%b-%y"),
         `Registration Fiscal Date` = parse_date(str_c(substr(`Expiration Date`,6,10),
                                                       "-",
                                                       substr(FY,3,4)),
                                                 format = "%m-%d-%y"),
         `Registration Date` = if_else(parse_number(format(`Registration Fiscal Date`, "%m")) > 6,
                                       `Registration Fiscal Date` - 365,
                                       `Registration Fiscal Date`),
         MReg_Year = year(`Registration Date`),
         MExp_Year = year(`Expiration Date`))

CNST <- read_delim("2016_09_22/CONSTR_RELATED_FLAG.txt",
                   delim = "|", col_types = cols(DOC_ID = col_character()))

MSTR <- read_delim("2016_09_22/qryDUNS_02_KwDUNS_0922.txt",
                   delim = "|", col_types = cols(DOC_ID = col_character()),
                   quote = "")

setwd("/Users/jauerbach/Dropbox/DDC/NTWK/NETS2012_NY/")
RATING <- read_delim("NETS2012_NY_Ratings.txt",
                     "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(DunsNumber,DnBRating06:DnBRating12) %>%
  gather("Year","DnBRating",DnBRating06:DnBRating12, na.rm = TRUE) %>%
  mutate(Year = 2000 + parse_number(str_extract(Year, "[[:digit:]]+"))) %>%
  na.omit() %>% 
  mutate(DnBRating = ifelse(str_length(DnBRating) == 2 |
                            str_sub(DnBRating, 1, 2) == "ER" |
                            str_sub(DnBRating, 1, 3) == "INV", 
                            NA, DnBRating)) %>%
  separate(DnBRating, c("Wealth", "Risk"), sep = 2, convert = TRUE) %>%
  mutate(Wealth = ifelse(str_sub(Wealth, 2, 2) == "R", NA, Wealth))


EMP <- read_delim("NETS2012_NY_Emp.txt",
                     "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(DunsNumber, Emp06, Emp07, Emp08, Emp09, Emp10, Emp11, Emp12) %>%
  gather("Year","Emp",Emp06:Emp12, na.rm = TRUE) %>%
  mutate(Year = 2000 + parse_number(str_extract(Year, "[[:digit:]]+"))) %>%
  na.omit()

SALES <- read_delim("NETS2012_NY_Sales.txt",
                  "\t", escape_double = FALSE, trim_ws = TRUE,
                  col_types = cols(.default = "c")) %>% 
  select(DunsNumber, Sales06, Sales07, Sales08, Sales09, Sales10, Sales11, Sales12) %>%
  gather("Year","Sales",Sales06:Sales12, na.rm = TRUE) %>%
  mutate(Year = 2000 + parse_number(str_extract(Year, "[[:digit:]]+")),
         Sales = parse_number(Sales)) %>%
  na.omit()

substrRight <- function(x, n) {
  x <- gsub("NA", "", x)
  substr(x, nchar(x) - n, nchar(x))
}

NAICS <- read_delim("NETS2012_NY_NAICS.txt",
                    "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  unite(CAT, NAICS90:NAICS12, sep = "") %>% 
  transmute(DunsNumber, 
            NAICS6 = substrRight(CAT,6),
            NAICS2 = str_sub(NAICS6, 1, 2))

#-----------------------#
#     2. MERGE DATA     #
#-----------------------#

CNST_MSTR <- MSTR %>% 
## Q. What is CONSTR_REL_FL? Are these the only construction firms?  
  #filter(DOC_ID %in% CNST$DOC_ID[CNST$CONSTR_REL_FL == 1]) %>%
  filter(DOC_ID %in% CNST$DOC_ID) %>%
  semi_join(NAICS, by = c("DUNS_NO" = "DunsNumber")) %>%
  mutate(Reg_Year = parse_number(format(RegistrationDate, "%Y")))  %>%
  group_by(DUNS_NO, VendorTIN, Reg_Year, Method, Type, DEPT_SH_NM) %>%
  summarise(Contract_Value = sum(CurrentValue))
rm(list = c("CNST", "MSTR"))

Rating_2Years_Later <- RATING %>% transmute(DunsNumber,
                                            Year = Year - 2,
                                            TYLWealth = Wealth,
                                            TYLRisk = Risk)

Sales_2Years_Later <- SALES %>% transmute(DunsNumber,
                                          Year = Year - 2,
                                          TYLSales = Sales)

Emp_2Years_Later <- EMP %>% transmute(DunsNumber,
                                      Year = Year - 2,
                                      TYLEmp = Emp)

EVERYTHING <- CNST_MSTR %>% 
  left_join(NAICS, by = c("DUNS_NO" = "DunsNumber")) 
EVERYTHING <- EVERYTHING %>% 
  left_join(RATING, by = c("DUNS_NO" = "DunsNumber", "Reg_Year" = "Year")) 
EVERYTHING <- EVERYTHING %>% 
  left_join(Rating_2Years_Later, by = c("DUNS_NO" = "DunsNumber", "Reg_Year" = "Year")) 
EVERYTHING <- EVERYTHING %>% 
  left_join(EMP, by = c("DUNS_NO" = "DunsNumber", "Reg_Year" = "Year")) 
EVERYTHING <- EVERYTHING %>% 
  left_join(Emp_2Years_Later, by = c("DUNS_NO" = "DunsNumber", "Reg_Year" = "Year"))
EVERYTHING <- EVERYTHING %>% 
  left_join(SALES, by = c("DUNS_NO" = "DunsNumber", "Reg_Year" = "Year"))
EVERYTHING <- EVERYTHING %>% 
  left_join(Sales_2Years_Later, by = c("DUNS_NO" = "DunsNumber", "Reg_Year" = "Year"))

# We only look at MWBEs with Certification Date
# most.frequent returns the most frequent ethnicity, female, M/WBE type when 
## there are multiple entries

most.frequent <- function(x) names(which.max(table(x)))

TRUE_MWBE <- 
  MWBE %>% 
  mutate(`Certification Date` = parse_date(`Certification Date`,
                                           format = "%d-%b-%y",
                                           na = paste(rep("#",255),collapse = ""))) %>%
  filter(!is.na(`Certification Date`)) %>%
  group_by(tax_id) %>%
  summarise(`Certification Date` = min(`Certification Date`),
            ethnicity = most.frequent(ETHNICITY), 
            female = most.frequent(female), 
            MWBEType = most.frequent(`M/WBE Type`)) %>%
  na.omit() %>%
  mutate(certYear = parse_number(year(`Certification Date`)))

HALF_MWBE <- MWBE %>% 
  filter(is.na(parse_date(`Certification Date`, 
                          format = "%d-%b-%y",
                          na = paste(rep("#",255),collapse = ""))),
         !(tax_id %in% TRUE_MWBE$tax_id))

EVERYTHING <- EVERYTHING %>% anti_join(HALF_MWBE, by = c("VendorTIN" = "tax_id"))
EVERYTHING <- EVERYTHING %>% left_join(TRUE_MWBE, by = c("VendorTIN" = "tax_id"))

#Add MWBE Y/N column
EVERYTHING <- EVERYTHING %>%
  mutate(MWBEStatus = VendorTIN %in% TRUE_MWBE$tax_id)

#-----------------------#
#     3. CLEAN DATA     #
#-----------------------#
#quantile(log(EVERYTHING$Sales + 1, base = 10), na.rm = TRUE)
#table(log(EVERYTHING$Sales + 1, base = 10) > 8)

#quantile(log(EVERYTHING$Emp + 1, base = 10), na.rm = TRUE)
#table(log(EVERYTHING$Emp + 1, base = 10) > 4)

#------------------------------#
#     4. EXPLORE/PLOT DATA     #
#------------------------------#

#only summmary plots

#Race and gender counts over time
ggplot(TRUE_MWBE) +
  geom_histogram(mapping = aes(x = female, fill = factor(certYear)), position = "dodge", stat ="count") +
  facet_wrap(~ ethnicity) +
  labs(title = "Number of MWBE certifications over time", fill = "Year",
       subtitle = "male = \"FALSE\", female = \"TRUE\"",
       x = "Gender")

#Gender counts overall
ggplot(TRUE_MWBE) +
  geom_histogram(mapping = aes(x = female, fill = ethnicity), stat ="count") +
  labs(title = "Gender distribution of certified MWBEs", fill = "Ethnicity",
       subtitle = "male = \"FALSE\", female = \"TRUE\"",
       x = "Gender")

#Compare contract values of MWBEs vs. non-MWBEs
ggplot(EVERYTHING) +
  geom_point(aes(x = MWBEType, y = Contract_Value))

ggplot(EVERYTHING) +
  geom_boxplot(aes(x = MWBEStatus, y = log10(Contract_Value), color = ethnicity)) +
  coord_flip() +
  labs(title = "Contract Values of MWBEs vs non-MWBEs", 
       subtitle = "MWBE = \"TRUE\", Non-MWBE = \"FALSE\"",
       x = "MWBE Status",
       y = "Contract Value (log 10 scale)",
       color = "Ethnicity")

#Compare MWBE distributions by industry type
plot_type <-
  EVERYTHING %>%
  group_by(Type) %>%
  summarise(count = n()) %>%
  filter(count > 400) %>%
  transmute(Type)

EVERYTHING %>% filter(Type %in% plot_type$Type) %>% 
  ggplot() +
  aes(x = log(Contract_Value, base = 10), y = as.numeric(MWBEStatus), color = Type) +
  geom_smooth(method ="glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(color = "Job Category", 
       x = "Contract Value (log 10 scale)", 
       y = "Probability of MWBE Status", 
       title = "Comparing MWBE Percentages", 
       subtitle ="at low and high value contracts and across job categories") +
  coord_cartesian(ylim = c(0, 1.00)) 

#Compare MWBE distributions by contract obtaining methods
plot_method <-
  EVERYTHING %>%
  group_by(Method) %>%
  summarise(count = n()) %>%
  filter(count > 2800) %>%
  transmute(Method)

EVERYTHING %>% filter(Method %in% plot_method$Method) %>% 
  ggplot() +
  aes(log(Contract_Value, base = 10), as.numeric(MWBEStatus), color = Method) +
  geom_smooth(method ="glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(color = "Contract Procurement Method", x = "Contract Value (log 10 scale)", y = "Probability of MWBE Status", title = "Comparing MWBE Percentages", subtitle ="at low and high value contracts and across job procurement methods") +
  coord_cartesian(ylim = c(0, 1.00)) 

#Compare MWBE distributions by contracting dept
plot_dept <-
  EVERYTHING %>%
  group_by(DEPT_SH_NM) %>%
  summarise(count = n()) %>%
  filter(count > 1200) %>%
  transmute(DEPT_SH_NM)
plots <- list()
start <- 1
end <- 5
for(i in 1:(length(plot_dept$DEPT_SH_NM) %/% 5)){
  plots[[i]] <-
    EVERYTHING %>% filter(DEPT_SH_NM %in% plot_dept$DEPT_SH_NM[start:end]) %>%
    ggplot() +
    aes(x = log(Contract_Value, base = 10), y = as.numeric(MWBEStatus),
        color = DEPT_SH_NM) +
    geom_smooth(method ="glm", method.args = list(family = "binomial"), se = TRUE) +
    labs(y = "MWBE Probability", color = "Department", x = "Contract Value (log 10 scale)")
  start = start + 5
  end = end + 5
}

grid.arrange(plots[[1]], plots[[2]], plots [[3]], plots[[4]], top = "Comparing MWBE Percentages")

#MWBE vs NAICS
ggplot(EVERYTHING) +
  geom_histogram(aes(x = NAICS2, fill = factor(MWBEStatus)), stat = "count") +
  labs(title = "MWBE distribution across different industries",
       x = "Industry NAICS code",
       fill = "MWBE Status")

#Contract value across NAICS
ggplot(EVERYTHING) +
  geom_smooth(aes(x = as.numeric(NAICS2), y = log10(Contract_Value), color = factor(MWBEStatus))) +
  labs(title = "Contract value across industries",
       x = "Industry NAICS code",
       color = "MWBE Status",
       y = "Contract Value (log 10 scale)")

#cohort graphs (sub in different y's: Emp, ContractValue, Risk, Sales...)
#mean is taken to account for # of contracts
EVERYTHING %>%
  filter(certYear <= 2012 && certYear >= 2008 | is.na(certYear)) %>%
ggplot() +
  aes(x = Reg_Year, y = Emp, color = ethnicity) +
  stat_summary(fun.y = mean, geom = "line", size = 1) +
  facet_wrap(~ certYear) +
  geom_vline(aes(xintercept = certYear), linetype = "dashed") +
  labs(title = "Employment before and after certification",
       x = "Year", color = "Ethnicity", y = "Employment")
  
#wealth vs ethnicity. plot 1 = by ethnicity, plot 2 = MWBE vs non-MWBE

ggplot(EVERYTHING) +
 aes(x = factor(Wealth), fill = factor(MWBEStatus)) +
 geom_histogram(stat = "count") +
  coord_cartesian(ylim = c(0, 5000))

ggplot(EVERYTHING) +
 aes(x = parse_number(NAICS2), y = Wealth, color = factor(MWBEStatus))+
 stat_summary(geom = "line", fun.y = mean)

#Risk across NAICS. Plot 1 = by eth, plot 2 = MWBE vs non-MWBE
ggplot(EVERYTHING) +
aes(x = parse_number(NAICS2), y = Risk, color = ethnicity) +
  geom_smooth() +
  labs(x = "Industry NAICS code", title ="Risk across industries", 
       color = "Ethnicity")

ggplot(EVERYTHING) +
  aes(x = parse_number(NAICS2), y = Risk, color = factor(MWBEStatus))+
  geom_smooth() +
  labs(x = "Industry NAICS code", title ="Risk across industries", 
       color = "MWBE Status")

#MWBE distribution across naics. Plot 1 = histogram MWBE vs non-MWBE.
#Plot 2 = freqpoly by ethnicity. Plot 3 = histogram, facetted by ethnicity.
ggplot(EVERYTHING) +
  aes(x = NAICS2, fill = MWBEStatus) +
  geom_histogram(stat = "count")

ggplot(EVERYTHING) +
  aes(x = NAICS2, y = ..prop.., group = ethnicity, color = ethnicity) +
  geom_freqpoly(stat = "count")

ggplot(EVERYTHING) +
  aes(x = NAICS2, y = ..prop.., group = ethnicity, fill = ethnicity) +
  geom_histogram(stat = "count") +
  facet_wrap(~ ethnicity) +
#  coord_flip() +
  labs(y = "Proportion of Establishments")

#Look at avg change in risk two years after getting a city contract
EVERYTHING %>%
  filter(Reg_Year >= 2007 && Reg_Year <= 2010) %>%
  group_by(Reg_Year, factor(MWBEStatus)) %>%
  summarise(change = mean(TYLSales - Sales, na.rm = TRUE)) %>%
  ggplot() +
    geom_smooth(aes(x = Reg_Year, y = change, color = `factor(MWBEStatus)`))

#-----------------------#
#     4. MODEL DATA     #
#-----------------------#

#Make a new dataset that will be used with the model
temp <- EVERYTHING
temp$id <- 1:nrow(temp)
temp$EmpF <- cut(temp$Emp, c(0,10,100,500,Inf))
temp$SalesF <- cut(log(EVERYTHING$Sales+1, base = 10), c(0,6,8,10))
temp$WealthF <- factor(temp$Wealth)
temp$RiskF <- factor(temp$Risk)
temp$ethnicity <- ifelse(is.na(temp$ethnicity), "Non-Minority", temp$ethnicity)
temp$female <- ifelse(is.na(temp$female), FALSE, temp$female)
temp$Reg_Year <- factor(temp$Reg_Year)

options(mc.cores = parallel::detectCores())
mixed <- stan_glmer(MWBEStatus ~ EmpF + SalesF + RiskF + Type + Reg_Year +
                      (1 | NAICS2) + (1 | WealthF), 
             temp, family = binomial(link = "logit"), iter = 200)

temp2 <- temp %>% 
  ungroup() %>%
  select(MWBEStatus, EmpF, SalesF, Type, RiskF, Reg_Year, NAICS2, WealthF, id) %>%
  na.omit() %>%
  mutate(fit = apply(posterior_predict(mixed),2,mean),
                resid = residuals(mixed))

aov.out <- aov(mixed)
plot(aov.out)

ggplot(temp2) +
  aes(MWBEStatus, fit) +
  geom_boxplot()

ggplot(temp2) +
  aes(fit, fill = factor(MWBEStatus,labels = c("Not MWBE","MWBE"))) +
  geom_density(alpha = .5, bw =.05) +
  labs(x = "propensity score", y = "density", fill = "") +
  theme(legend.position = "bottom")

draws <- as_tibble(as.matrix(mixed)[,-1])
test <- tibble(Emp = apply(draws[,1:3], 1, sd),
               Sales = apply(draws[,4:5], 1, sd),
               Risk = apply(draws[,6:8], 1, sd),
               Type = apply(draws[,9:37], 1, sd),
               Reg_Year = apply(draws[,38:43], 1, sd),
               NAICS2 = apply(draws[,44:89], 1, sd),
               Wealth = apply(draws[,90:103], 1, sd),
               Error = apply(predictive_error(mixed, draws = 400),1,sd))

tibble(lower = apply(test,2,mean) - apply(test,2,sd),
       lower2 = apply(test,2,mean) - 2*apply(test,2,sd),
       mean = apply(test,2,mean),
       upper = apply(test,2,mean) + apply(test,2,sd),
       upper2 = apply(test,2,mean) + 2*apply(test,2,sd),
       names = colnames(test)) %>%
  ggplot() +
  geom_linerange(aes(x = names, ymin = lower, ymax = upper), size = 2) +
  geom_linerange(aes(x = names, ymin = lower2, ymax = upper2)) +
  coord_flip()
ggsave("imgs/anova.png")
mixed

#------------------------#
#     5. CAUSAL COMP     #
#------------------------#

#NOTE: FIGURE OUT HOW TO ACCOUNT FOR NAs

temp3 <- temp2 %>% left_join(temp)
temp3_mwbe <- temp3 %>% filter(MWBEStatus == TRUE)
temp3_nmwbe <- temp3 %>% filter(MWBEStatus == FALSE)

temp3_mwbe <- temp3_mwbe %>% mutate(match = -1)
for(row in 1:nrow(temp3_mwbe)) {
  temp3_mwbe$match[row] <- which.min((temp3_nmwbe$fit - temp3_mwbe$fit[row])^2)
}

temp3_matches <- temp3_nmwbe[temp3_mwbe$match,]

#DID Sales
mean(temp3_mwbe$TYLSales - temp3_matches$TYLSales, na.rm = TRUE) -
  mean(temp3_mwbe$Sales - temp3_matches$Sales, na.rm = TRUE)

#no matching = effects are exaggerated
(mean(EVERYTHING$TYLSales[EVERYTHING$MWBEStatus == TRUE], na.rm = TRUE) - 
    mean(EVERYTHING$TYLSales[EVERYTHING$MWBEStatus == FALSE], na.rm = TRUE)) -
  (mean(EVERYTHING$Sales[EVERYTHING$MWBEStatus == TRUE], na.rm = TRUE) - 
     mean(EVERYTHING$Sales[EVERYTHING$MWBEStatus == FALSE], na.rm = TRUE))

#DID Risk (incr .3)
mean(temp3_mwbe$TYLRisk - temp3_matches$TYLRisk, na.rm = TRUE) -
  mean(temp3_mwbe$Risk - temp3_matches$Risk, na.rm = TRUE)

#no matching = effects seem positive (-0.01)
(mean(EVERYTHING$TYLRisk[EVERYTHING$MWBEStatus == TRUE], na.rm = TRUE) - 
    mean(EVERYTHING$TYLRisk[EVERYTHING$MWBEStatus == FALSE], na.rm = TRUE)) -
  (mean(EVERYTHING$Risk[EVERYTHING$MWBEStatus == TRUE], na.rm = TRUE) - 
     mean(EVERYTHING$Risk[EVERYTHING$MWBEStatus == FALSE], na.rm = TRUE))

#More MWBEs than non-MWBEs increase their risk
sum(temp3_mwbe$TYLRisk < temp3_mwbe$Risk, na.rm = TRUE)/nrow(temp3_mwbe)
sum(temp3_nmwbe$TYLRisk < temp3_nmwbe$Risk, na.rm = TRUE)/nrow(temp3_nmwbe)

#DID Employment
mean(temp3_mwbe$TYLEmp - temp3_matches$TYLEmp, na.rm = TRUE) -
  mean(temp3_mwbe$Emp - temp3_matches$Emp, na.rm = TRUE)

(sd(temp3_mwbe$TYLEmp - temp3_matches$TYLEmp, na.rm = TRUE) +
  sd(temp3_mwbe$Emp - temp3_matches$Emp, na.rm = TRUE))/sqrt(nrow(temp))

(mean(EVERYTHING$TYLEmp[EVERYTHING$MWBEStatus == TRUE], na.rm = TRUE) - 
       mean(EVERYTHING$TYLEmp[EVERYTHING$MWBEStatus == FALSE], na.rm = TRUE)) -
  (mean(EVERYTHING$Emp[EVERYTHING$MWBEStatus == TRUE], na.rm = TRUE) - 
         mean(EVERYTHING$Emp[EVERYTHING$MWBEStatus == FALSE], na.rm = TRUE))

#------------------------#
#     6. Non Gov Con     #
#------------------------#
mixed_no_type <- stan_glmer(MWBEStatus ~ EmpF + SalesF + RiskF + Reg_Year +
                      (1 | NAICS2) + (1 | WealthF), 
                    temp, family = binomial(link = "logit"), iter = 200)

REST <- RATING %>% anti_join(EVERYTHING, c("DunsNumber" = "DUNS_NO"))
REST <- REST %>% 
  left_join(NAICS, by = c("DunsNumber" = "DunsNumber")) 
REST <- REST %>% 
  left_join(Rating_2Years_Later, by = c("DunsNumber" = "DunsNumber", "Year" = "Year")) 
REST <- REST %>% 
  left_join(EMP, by = c("DunsNumber" = "DunsNumber", "Year" = "Year")) 
REST <- REST %>% 
  left_join(Emp_2Years_Later, by = c("DunsNumber" = "DunsNumber", "Year" = "Year"))
REST <- REST %>% 
  left_join(SALES, by = c("DunsNumber" = "DunsNumber", "Year" = "Year"))
REST <- REST %>% 
  left_join(Sales_2Years_Later, by = c("DunsNumber" = "DunsNumber", "Year" = "Year"))

nongov <- REST
nongov$id <- 1:nrow(nongov)
nongov$EmpF <- cut(nongov$Emp, c(0,10,100,500,Inf))
nongov$SalesF <- cut(log(nongov$Sales+1, base = 10), c(0,6,8,10))
nongov$WealthF <- factor(nongov$Wealth)
nongov$RiskF <- factor(nongov$Risk)
nongov$Reg_Year <- factor(nongov$Year)
nongov <- na.omit(nongov)
nongov$MWBEStatus <- -1

nongov <- nongov %>% 
  ungroup() %>%
  mutate(fit = apply(posterior_predict(mixed_no_type, newdata = nongov),2,mean))

temp3_mwbe <- temp3_mwbe %>% mutate(matchnongov = -1)
for(row in 1:nrow(temp3_mwbe)) {
  temp3_mwbe$matchnongov[row] <- which.min((nongov$fit - temp3_mwbe$fit[row])^2)
}

nongov_matches <- nongov[temp3_mwbe$matchnongov,]

#DID Sales
mean(temp3_mwbe$TYLSales - nongov_matches$TYLSales, na.rm = TRUE) -
  mean(temp3_mwbe$Sales - nongov_matches$Sales, na.rm = TRUE)

mean(temp3_mwbe$TYLRisk - nongov_matches$TYLRisk, na.rm = TRUE) -
  mean(temp3_mwbe$Risk - nongov_matches$Risk, na.rm = TRUE)

#Issue with na.omit is that these are just the people who stayed in business the whole time

##Notes:

#1. Unit of Analysis: Establishment, Reg Year (Dep, Type, Method)
#2. Treatment: MWBE Status
#2. Outcome: Sales/Emp/Wealth/Risk Two Years Later