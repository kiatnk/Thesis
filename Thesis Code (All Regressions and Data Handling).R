library(haven)
library(tidyverse)
library(SocialPosition)
library(nnet)
library(ggplot2)
library(psych)
library(xtable)
library(lme4)
library(nlme)
library(ordinal)
library(extrafont)


#S003 Country
#S006 ID
#x025 Education
#x035_4 Occupation
#x048 Region
#x001 Gender
#x003 Age

#read in, filter 1999 data
data = read_dta("ZA4804_v3-1-0.dta", col_select = c("S002EVS", "S003", "S006",
                                                    "X025", "X035_4", "X048",
                                                    "X001", "X003"))


striplab = function(x) { 
  
  stripped_x = zap_label(zap_labels(x))
  
  return((stripped_x))
}

countries = c(40, 56, 100, 191, 203, 208, 246, 250, 276, 348, 352, 372,
              380, 428, 528, 616, 620, 703, 705, 724)

unused = c(-5, -4, -3, -2, -1)

dat99 = data %>%
  filter(S002EVS == 3) %>%
  filter(S003 %in% countries) %>%
  filter(!X001 %in% unused) %>%
  filter(!X003 %in% unused) %>%
  filter(!X025 %in% unused) %>%
  filter(X025 != 1) %>%
  filter(!X035_4 %in% unused) %>%
  filter(!X048 %in% unused) %>%
  mutate(age = striplab(X003)) %>%
  mutate(gen = striplab(X001))

#recode education 99

dat99 = dat99 %>%
  mutate(edu = striplab(X025)) %>%
  mutate(edu = dplyr::recode(edu,
                      '2' = 1,
                      '3' = 2,
                      '4' = 3,
                      '5' = 3,
                      '6' = 4,
                      '7' = 5,
                      '8' = 6))
#recode occupation 99

dat99 = dat99 %>%
  mutate(isco88 = striplab(X035_4)) %>%
  mutate(isco882d = as.numeric(substr(isco88, 1, 2))) %>%
  filter(isco882d %% 10 != 0)

#recode regions 99 and merge

regions = read.csv("regions.csv")

dat99 = dat99 %>%
  mutate(ISO = striplab(dat99$X048))

dat99 = merge(dat99, regions, by="ISO")

#select necessary columns, make as factors
dat99 = dat99 %>%
  dplyr::select(S006, country, age, gen, edu, isco882d, nuts2016) %>%
  rename(region = nuts2016) %>%
  filter(!is.na(region)) %>%
  rename(idno = S006) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(gen = factor(gen, labels = c('M','F'))) %>%
  mutate(edu = as.factor(edu)) %>%
  mutate(isco882d = as.factor(isco882d)) %>%
  mutate(out = relevel(isco882d, ref = "61")
)

#run multinomial regressions


mulmod = function(df) {
  
  if (length(unique(df$region)) > 1) {
    multinom(out ~ age + gen + edu + region, data = df)
    #fitted(multinom(out ~ age + gen + edu + region, data = df))
    
  } 
  
  else {
    multinom(out ~ age + gen + edu, data = df)
    #fitted(multinom(out ~ age + gen + edu, data = df))
  }
   
}

############################## 2018 DATA ######################################

dat18 = read_sav("ESS9e03_1.sav", col_select = c("idno", "cntry", "agea", "gndr",
                 "eisced", "isco08", "region","gincdif", "ipcrtiv", "impsafe", 
                 "impdiff", "ipfrule", "ipmodst", "impfree", "ipstrgv", 
                 "ipbhprp", "imptrad","hinctnta","rlgdgr"))

nodata = c(55,77,88,99)
missctry = c("CH", "ME", "RS", "CY", "NO", "GB", "SE", 'EE', 'LT')

dat18 = dat18 %>%
  drop_na() %>%
  filter(!cntry %in% missctry) %>%
  filter(gndr != 9) %>%
  filter(agea != 999) %>%
  filter(!eisced %in% nodata & eisced != 0) %>%
  rename(country = cntry) %>%
  rename(age = agea) %>%
  mutate(age = as.numeric(age)) %>%
  rename(gen = gndr) %>%
  mutate(gen = factor(gen, levels = c(1,2), labels = c("M","F"))) %>%
  mutate(edu = striplab(eisced)) %>%
  mutate(edu = dplyr::recode(edu, 
                      '1' = 1,
                      '2' = 2,
                      '3' = 3,
                      '4' = 4,
                      '5' = 4,
                      '6' = 5,
                      '7' = 6)) %>%
  mutate(edu = as.factor(edu)) %>%
  mutate(redinc = as.factor(striplab(gincdif)))
  

#recode occupation 18

cutisco = c(66666, 77777, 88888, 99999, 0, 100, 110, 200, 210, 300, 310)

isco88 = convert_from_ISCO08_to_ISCO88_3d(dat18$isco08, dat18)

dat18 = dat18 %>%
  mutate(isco88 = as.numeric(substr(isco88$ISCO88_3d, 1, 2))) %>%
  filter(!isco08 %in% cutisco) %>%
  filter(isco88 %% 10 != 0)


#recode region

nain99 = c("CZ07", "ES64", "ES63", "FI20","PL11","PL12","PL31","PL32","PL33",
           "PL34", "PL43", "SE11", "SE12","SE21","SE22","SE23","SE31","SE32",
           "SE33")

dat18 = dat18 %>%
  mutate(region = substr(region, 1, 4)) %>%
  mutate(region = dplyr::recode(region, 
                         "DK01"="DK00",
                         "DK02"="DK00",
                         "DK03"="DK00",
                         "DK04"="DK00",
                         "DK05"="DK00",
                         "FR10"="FR1",
                         "FRC1"="FR2",
                         "FRD2"="FR2",
                         "FRH0"="FR5",
                         "FRL0"="FR8",
                         "FRJ2"="FR6",
                         "FRB0"="FR2",
                         "FRE2"="FR2",
                         "FRK2"="FR7",
                         "FRF2"="FR2",
                         "FRI1"="FR6",
                         "FRG0"="FR5",
                         "FRJ1"="FR8",
                         "FRI2"="FR6",
                         "FRE1"="FR3",
                         "FRF1"="FR4",
                         "FRI3"="FR5",
                         "FRF3"="FR4",
                         "FRC2"="FR4",
                         "FRD1"="FR2",
                         "FRK1"="FR7")) %>%
  filter(!region %in% nain99)


####################### factor analysis ############################


facdata = dat18 %>% dplyr::select(impsafe, ipfrule, ipmodst, ipstrgv, ipbhprp, 
                                  imptrad, impfree, ipcrtiv, impdiff) %>%
  mutate(across(c("impsafe":"impdiff"), as.factor)) %>%
  mutate(across(c("impsafe":"impdiff"), dplyr::recode, 
                        `1`= 6, 
                        `2`= 5,
                        `3`= 4,
                        `4`= 3,
                        `5`= 2,
                        `6`= 1))

auth.fa = fa(facdata, factors = 2, rotation = "oblimin")

facscore = factor.scores(facdata, auth.fa)

dat18 = dat18 %>%
  mutate(auth =  facscore$scores[,1])

auth = dat18 %>% dplyr::select(impsafe, ipfrule, ipmodst, ipstrgv, imptrad, 
                               ipbhprp)
sqc = dat18 %>% dplyr::select(ipcrtiv, impdiff, impfree)

psych::alpha(auth)
psych::alpha(sqc)


######################## run multinomial models ###############################

trace("multinom", edit=TRUE)   

multireg = dat99 %>%
  group_by(country) %>%
  filter(n_distinct(region) > 1) %>%
  ungroup(country) %>%
  group_nest(country) %>%
  mutate(model = map(data,
                     ~multinom(out ~ age + gen + edu + region,
                               data = .x))
  ) %>% 
  left_join(dat18 %>% group_nest(country), by = "country") %>%
  mutate(fitted = map2(model,
                       data.y,
                       ~predict(.x, newdata = .y, type = "probs"))) %>%
  dplyr::select(country, data.y, fitted)


singlereg = dat99 %>%
  group_by(country) %>%
  filter(n_distinct(region) <= 1) %>%
  ungroup(country) %>%
  group_nest(country) %>%
  mutate(model = map(data,
                      ~multinom(out ~ age + gen + edu,
                                data = .x))
  ) %>% 
  left_join(dat18 %>% group_nest(country), by = "country") %>%
  mutate(fitted = map2(model,
                       data.y,
                       ~predict(.x, newdata = .y, type = "probs"))) %>%
  dplyr::select(country, data.y, fitted)

unnested = multireg
unnested[17:20,] = singlereg[1:4,]
unnested = unnest(unnested, c(data.y,fitted))

job_ids = colnames(multireg$fitted[[1]])
colnames(unnested$fitted) = job_ids

#Load in probabilities and sum
risk = read.delim("corrtab88-08 - Copy.txt", header = TRUE, sep = "\t")
risk = risk %>%
 group_by(isco882dcode) %>%
  mutate(thetaj = mean(probability))

#Load in operational stock of robots by country
countries = unique(unnested$country)

robots = readxl::read_xlsx("application_ - Operational Stock.xlsx", skip=2)
robots = robots %>%
  rename(country = "...1") %>%
  dplyr::select(country,`2004`:`2018`) %>%
  filter(!is.na(country)) %>%
  separate(country, into=c("country",NA), sep="-", extra="drop") %>%
  filter(country %in% countries) %>%
  mutate(
    deltaR = case_when(
      `2004` > 0 ~ (`2017`-`2004`)/`2004`,
      `2004` == 0 & `2005` != 0 ~ (`2017`-`2005`)/`2005`,
      `2004` == 0 & `2005` == 0 ~ (`2017`-`2006`)/`2006`
    )
  )

######

calc_vulnerability = function(matrix) {
  exposure = list()
  for (row in 1:nrow(matrix)) {
    sum = 0
    for (col in 1:ncol(matrix)) {
      sum = sum + matrix[row, col] * risk$thetaj[risk$isco882dcode == colnames(matrix)[col]][1]
    }
    exposure = c(exposure, sum)
  }
  unlist(exposure)
}

unnested = mutate(unnested, vulnerability = calc_vulnerability(unnested$fitted), .after = isco88)

unnested = merge(unnested, robots, by="country")
unnested = unnested %>% 
  mutate(indexp = deltaR*vulnerability)


############################## add party family ###############################

party18 = read_sav("ESS9e03_1.sav", 
                   col_select = c("idno","cntry", "prtvtcat", "prtvtdbe",
                   "prtvtdbg", "prtvtecz","prtvede2","prtvtddk", "prtvtees",
                   "prtvtdfi", "prtvtdfr","prtvtahr","prtvtfhu", "prtvtcis",
                   "prtvtcit", "prtvtalv","prtvtgnl","prtvtdpl", "prtvtcpt",
                   "prtvtfsi", "prtvtdsk"))


partycode = readxl::read_xlsx("Party Recode.xlsx")


party18 = party18 %>%
  pivot_longer(prtvtcat:prtvtdsk, values_to = "pchoice") %>%
  filter(!is.na(pchoice)) %>%
  dplyr::select(-name) %>%
  group_by(cntry) %>%
  left_join(partycode, by = c("cntry","pchoice")) %>%
  ungroup()%>%
  dplyr::select(-cntry)

finaldata = merge(unnested, party18, by = "idno")


finaldata = finaldata %>%
  filter(family != 0 & family != "No family" & family != "Regionalist" &
           family != "No Family")

#trade exposure control
imports = readxl::read_xlsx("import variable.xlsx")

imports = imports %>%
  dplyr::select(country, tradexp)

finaldata = merge(finaldata, imports, by = "country")


#multilevel variable
mlv = read.csv("country level variable.csv")

mlv = mlv %>%
  group_by(ï..country, year, rout) %>%
  mutate(sum_emp = sum(obs_value)) %>%
  distinct(sum_emp) %>%
  ungroup() %>%
  group_by(ï..country) %>%
  mutate(
    sharemp = case_when(
      year == 0 ~ sum_emp/sum_emp[rout == 3],
      year == 1 ~ sum_emp/sum_emp[rout == 4]
    )
  ) %>%
  filter(rout == 1) %>%
  mutate(deltaemp = sharemp[year == 0] - sharemp[year == 1]) %>%
  distinct(deltaemp) %>%
  rename("country" = "ï..country")

finaldata = merge(finaldata, mlv, "country")


finaldata = finaldata %>%
  mutate(redinc = as.numeric(gincdif)) %>%
  filter(redinc %in% c(1,2,3,4,5)) %>%
  mutate(edu = as.numeric(eisced)) %>%
  mutate(edu = dplyr::recode(edu,
                      `1` = "ISCED 1",
                      `2` = "ISCED 2",
                      `3` = "ISCED 3b",
                      `4` = "ISCED 3a",
                      `5` = "ISCED 4",
                      `6` = "ISCED 5 V1",
                      `7` = "ISCED 5 V2")) %>%
  mutate(edu = as.factor(edu))

finaldata = finaldata %>%
  mutate(relg = as.numeric(rlgdgr)) %>%
  filter(!relg %in% c(77,88,99)) %>%
  mutate(inc = as.numeric(hinctnta)) %>%
  filter(!inc %in% c(77,88,99))
  
#choose baseline
finaldata$choice = relevel(factor(finaldata$family), ref = "Radical Right")

#standardize predictors
finaldata$indexp2 = scale(finaldata$indexp)
finaldata$tradexp2 = scale(finaldata$tradexp)
finaldata$age2 = scale(finaldata$age)
finaldata$deltaemp2 = scale(finaldata$deltaemp)
finaldata$relg2 = scale(finaldata$relg)
finaldata$inc2 = scale(finaldata$inc)
finaldata$edu = factor(finaldata$edu, ordered = TRUE)
finaldata$auth2 = scale(finaldata$auth)
finaldata$red2 = scale(finaldata$redinc)


data = finaldata %>%
  dplyr::select(idno, country, indexp, indexp2, age, age2, gen, eisced, edu, relg, 
                relg2, inc, inc2, deltaemp, deltaemp2, tradexp, tradexp2,
         auth, redinc, family, auth2, red2, isco88) %>%
  mutate(id = row_number())

saveRDS(data, file = "data.RDS") 

################## Check some assumptions ##########################
data = readRDS("data.RDS")

data = data %>%
  mutate(
    pchoice = case_when(
      family == "Radical Right" ~ 1,
      family == "Green" ~ 0,
      family == "Socialist" ~ 0,
      family == "Christian-Democratic" ~ 0,
      family == "Conservatives" ~ 0,
      family == "Radical Left" ~ 0,
      family == "Agrarian/Center" ~ 0,
      family == "Confessional" ~ 0,
      family == "Liberal" ~ 0
    )) %>%
  #mutate(choice = factor(pchoice, labels = c("Other", "Radical Right")))  %>%
  mutate(country = zap_labels(country)) %>%
  #filter(!country %in% c("BG")) %>%
  mutate(country = as.factor(country)) %>%
  mutate(rdemand = dplyr::recode(redinc,
                                 `1` = 5,
                                 `2` = 4,
                                 `3` = 3,
                                 `4` = 2,
                                 `5` = 2)) %>%
  mutate(rdemand = factor(rdemand, ordered = T))

dat18$idno = as.numeric(dat18$idno)

data = merge(data, dat18, "idno")

data = data %>%
  na.omit() %>%
  mutate(isco08num = zap_labels(isco08)) %>%
  mutate(routine = recode(isco08num,
                          `1111`="Nonroutine",
                          `1112`="Nonroutine",
                          `1113`="Nonroutine",
                          `1114`="Nonroutine",
                          `1120`="Nonroutine",
                          `1211`="Nonroutine",
                          `1212`="Nonroutine",
                          `1213`="Nonroutine",
                          `1219`="Nonroutine",
                          `1221`="Nonroutine",
                          `1222`="Nonroutine",
                          `1223`="Nonroutine",
                          `1311`="Nonroutine",
                          `1312`="Nonroutine",
                          `1321`="Nonroutine",
                          `1322`="Nonroutine",
                          `1323`="Nonroutine",
                          `1324`="Nonroutine",
                          `1330`="Nonroutine",
                          `1341`="Nonroutine",
                          `1342`="Nonroutine",
                          `1343`="Nonroutine",
                          `1344`="Nonroutine",
                          `1345`="Nonroutine",
                          `1346`="Nonroutine",
                          `1349`="Nonroutine",
                          `1411`="Nonroutine",
                          `1412`="Nonroutine",
                          `1420`="Nonroutine",
                          `1431`="Nonroutine",
                          `2111`="Nonroutine",
                          `2112`="Nonroutine",
                          `2113`="Nonroutine",
                          `2114`="Nonroutine",
                          `2120`="Nonroutine",
                          `2131`="Nonroutine",
                          `2132`="Nonroutine",
                          `2133`="Nonroutine",
                          `2141`="Nonroutine",
                          `2142`="Nonroutine",
                          `2143`="Nonroutine",
                          `2144`="Nonroutine",
                          `2145`="Nonroutine",
                          `2146`="Nonroutine",
                          `2149`="Nonroutine",
                          `2151`="Nonroutine",
                          `2152`="Nonroutine",
                          `2153`="Nonroutine",
                          `2161`="Nonroutine",
                          `2162`="Nonroutine",
                          `2163`="Nonroutine",
                          `2164`="Nonroutine",
                          `2165`="Nonroutine",
                          `2166`="Nonroutine",
                          `2211`="Nonroutine",
                          `2212`="Nonroutine",
                          `2221`="Nonroutine",
                          `2222`="Nonroutine",
                          `2230`="Nonroutine",
                          `2240`="Nonroutine",
                          `2250`="Nonroutine",
                          `2261`="Nonroutine",
                          `2262`="Nonroutine",
                          `2263`="Nonroutine",
                          `2264`="Nonroutine",
                          `2265`="Nonroutine",
                          `2266`="Nonroutine",
                          `2267`="Nonroutine",
                          `2269`="Nonroutine",
                          `2310`="Nonroutine",
                          `2320`="Nonroutine",
                          `2330`="Nonroutine",
                          `2341`="Nonroutine",
                          `2342`="Nonroutine",
                          `2351`="Nonroutine",
                          `2352`="Nonroutine",
                          `2353`="Nonroutine",
                          `2354`="Nonroutine",
                          `2355`="Nonroutine",
                          `2356`="Nonroutine",
                          `2359`="Nonroutine",
                          `2411`="Nonroutine",
                          `2412`="Nonroutine",
                          `2413`="Nonroutine",
                          `2421`="Nonroutine",
                          `2422`="Nonroutine",
                          `2423`="Nonroutine",
                          `2424`="Nonroutine",
                          `2431`="Nonroutine",
                          `2432`="Nonroutine",
                          `2433`="Nonroutine",
                          `2434`="Nonroutine",
                          `2511`="Nonroutine",
                          `2512`="Nonroutine",
                          `2513`="Nonroutine",
                          `2514`="Nonroutine",
                          `2519`="Nonroutine",
                          `2521`="Nonroutine",
                          `2522`="Nonroutine",
                          `2523`="Nonroutine",
                          `2529`="Nonroutine",
                          `2611`="Nonroutine",
                          `2612`="Nonroutine",
                          `2619`="Nonroutine",
                          `2621`="Nonroutine",
                          `2622`="Nonroutine",
                          `2631`="Nonroutine",
                          `2632`="Nonroutine",
                          `2633`="Nonroutine",
                          `2634`="Nonroutine",
                          `2635`="Nonroutine",
                          `2636`="Nonroutine",
                          `2641`="Nonroutine",
                          `2642`="Nonroutine",
                          `2643`="Nonroutine",
                          `2651`="Nonroutine",
                          `2652`="Nonroutine",
                          `2653`="Nonroutine",
                          `2654`="Nonroutine",
                          `2655`="Nonroutine",
                          `2656`="Nonroutine",
                          `2659`="Nonroutine",
                          `3111`="Routine",
                          `3112`="Nonroutine",
                          `3113`="Nonroutine",
                          `3114`="Nonroutine",
                          `3115`="Nonroutine",
                          `3116`="Nonroutine",
                          `3117`="Nonroutine",
                          `3118`="Nonroutine",
                          `3119`="Nonroutine",
                          `3121`="Nonroutine",
                          `3122`="Nonroutine",
                          `3123`="Nonroutine",
                          `3131`="Routine",
                          `3132`="Routine",
                          `3133`="Routine",
                          `3134`="Routine",
                          `3135`="Routine",
                          `3141`="Nonroutine",
                          `3142`="Nonroutine",
                          `3143`="Nonroutine",
                          `3151`="Nonroutine",
                          `3152`="Nonroutine",
                          `3153`="Nonroutine",
                          `3154`="Nonroutine",
                          `3155`="Nonroutine",
                          `3211`="Nonroutine",
                          `3212`="Nonroutine",
                          `3213`="Routine",
                          `3214`="Nonroutine",
                          `3221`="Nonroutine",
                          `3222`="Nonroutine",
                          `3230`="Nonroutine",
                          `3240`="Nonroutine",
                          `3251`="Nonroutine",
                          `3252`="Routine",
                          `3253`="Nonroutine",
                          `3254`="Nonroutine",
                          `3255`="Nonroutine",
                          `3256`="Nonroutine",
                          `3257`="Nonroutine",
                          `3258`="Nonroutine",
                          `3259`="Nonroutine",
                          `3311`="Nonroutine",
                          `3312`="Routine",
                          `3313`="Routine",
                          `3314`="Nonroutine",
                          `3315`="Nonroutine",
                          `3321`="Nonroutine",
                          `3322`="Nonroutine",
                          `3323`="Nonroutine",
                          `3324`="Nonroutine",
                          `3331`="Routine",
                          `3332`="Nonroutine",
                          `3333`="Nonroutine",
                          `3334`="Nonroutine",
                          `3339`="Nonroutine",
                          `3341`="Nonroutine",
                          `3342`="Routine",
                          `3343`="Routine",
                          `3344`="Routine",
                          `3351`="Nonroutine",
                          `3352`="Nonroutine",
                          `3353`="Routine",
                          `3354`="Routine",
                          `3355`="Nonroutine",
                          `3359`="Nonroutine",
                          `3411`="Routine",
                          `3412`="Nonroutine",
                          `3413`="Nonroutine",
                          `3421`="Nonroutine",
                          `3422`="Nonroutine",
                          `3423`="Nonroutine",
                          `3431`="Routine",
                          `3432`="Nonroutine",
                          `3433`="Nonroutine",
                          `3434`="Nonroutine",
                          `3511`="Routine",
                          `3512`="Nonroutine",
                          `3513`="Routine",
                          `3514`="Nonroutine",
                          `3521`="Routine",
                          `3522`="Nonroutine",
                          `4110`="Routine",
                          `4120`="Routine",
                          `4131`="Routine",
                          `4132`="Routine",
                          `4211`="Routine",
                          `4212`="Nonroutine",
                          `4213`="Routine",
                          `4214`="Nonroutine",
                          `4221`="Routine",
                          `4222`="Routine",
                          `4223`="Routine",
                          `4224`="Routine",
                          `4225`="Routine",
                          `4226`="Routine",
                          `4227`="Routine",
                          `4229`="Nonroutine",
                          `4311`="Routine",
                          `4312`="Routine",
                          `4313`="Routine",
                          `4321`="Routine",
                          `4322`="Routine",
                          `4323`="Nonroutine",
                          `4411`="Routine",
                          `4412`="Routine",
                          `4413`="Routine",
                          `4414`="Nonroutine",
                          `4415`="Routine",
                          `4416`="Routine",
                          `4419`="Routine",
                          `5111`="Nonroutine",
                          `5112`="Nonroutine",
                          `5113`="Nonroutine",
                          `5120`="Nonroutine",
                          `5131`="Nonroutine",
                          `5132`="Nonroutine",
                          `5141`="Nonroutine",
                          `5142`="Nonroutine",
                          `5151`="Nonroutine",
                          `5152`="Nonroutine",
                          `5153`="Nonroutine",
                          `5161`="Nonroutine",
                          `5162`="Nonroutine",
                          `5163`="Nonroutine",
                          `5164`="Nonroutine",
                          `5165`="Nonroutine",
                          `5169`="Nonroutine",
                          `5211`="Nonroutine",
                          `5212`="Nonroutine",
                          `5221`="Nonroutine",
                          `5222`="Routine",
                          `5223`="Nonroutine",
                          `5230`="Routine",
                          `5241`="Nonroutine",
                          `5242`="Nonroutine",
                          `5243`="Nonroutine",
                          `5244`="Routine",
                          `5245`="Nonroutine",
                          `5246`="Nonroutine",
                          `5311`="Nonroutine",
                          `5312`="Nonroutine",
                          `5321`="Nonroutine",
                          `5322`="Nonroutine",
                          `5329`="Nonroutine",
                          `5411`="Nonroutine",
                          `5412`="Nonroutine",
                          `5413`="Nonroutine",
                          `5414`="Nonroutine",
                          `5419`="Nonroutine",
                          `6111`="Nonroutine",
                          `6112`="Nonroutine",
                          `6113`="Nonroutine",
                          `6114`="Nonroutine",
                          `6121`="Nonroutine",
                          `6122`="Nonroutine",
                          `6123`="Nonroutine",
                          `6129`="Nonroutine",
                          `6130`="Nonroutine",
                          `6210`="Nonroutine",
                          `6221`="Nonroutine",
                          `6222`="Nonroutine",
                          `6223`="Nonroutine",
                          `6224`="Nonroutine",
                          `6310`="Nonroutine",
                          `6320`="Nonroutine",
                          `6330`="Nonroutine",
                          `6340`="Nonroutine",
                          `7111`="Nonroutine",
                          `7112`="Nonroutine",
                          `7113`="Nonroutine",
                          `7114`="Nonroutine",
                          `7115`="Nonroutine",
                          `7119`="Nonroutine",
                          `7121`="Nonroutine",
                          `7122`="Nonroutine",
                          `7123`="Nonroutine",
                          `7124`="Nonroutine",
                          `7125`="Nonroutine",
                          `7126`="Nonroutine",
                          `7127`="Nonroutine",
                          `7131`="Nonroutine",
                          `7132`="Nonroutine",
                          `7133`="Nonroutine",
                          `7211`="Routine",
                          `7212`="Routine",
                          `7213`="Routine",
                          `7214`="Nonroutine",
                          `7215`="Nonroutine",
                          `7221`="Routine",
                          `7222`="Nonroutine",
                          `7223`="Routine",
                          `7224`="Routine",
                          `7231`="Nonroutine",
                          `7232`="Nonroutine",
                          `7233`="Nonroutine",
                          `7234`="Nonroutine",
                          `7311`="Nonroutine",
                          `7312`="Nonroutine",
                          `7313`="Nonroutine",
                          `7314`="Nonroutine",
                          `7315`="Routine",
                          `7316`="Nonroutine",
                          `7317`="Nonroutine",
                          `7318`="Routine",
                          `7321`="Routine",
                          `7322`="Routine",
                          `7323`="Routine",
                          `7411`="Nonroutine",
                          `7412`="Nonroutine",
                          `7413`="Nonroutine",
                          `7421`="Nonroutine",
                          `7422`="Nonroutine",
                          `7511`="Routine",
                          `7512`="Routine",
                          `7513`="Routine",
                          `7514`="Routine",
                          `7515`="Routine",
                          `7516`="Routine",
                          `7521`="Routine",
                          `7522`="Nonroutine",
                          `7523`="Routine",
                          `7531`="Nonroutine",
                          `7532`="Routine",
                          `7533`="Nonroutine",
                          `7534`="Nonroutine",
                          `7535`="Routine",
                          `7536`="Nonroutine",
                          `7541`="Nonroutine",
                          `7542`="Nonroutine",
                          `7543`="Routine",
                          `7544`="Nonroutine",
                          `7549`="Routine",
                          `8111`="Routine",
                          `8112`="Routine",
                          `8113`="Nonroutine",
                          `8114`="Routine",
                          `8121`="Routine",
                          `8122`="Routine",
                          `8131`="Routine",
                          `8132`="Routine",
                          `8141`="Routine",
                          `8142`="Routine"
  ))





data %>%
  ggplot(aes(indexp)) + geom_boxplot() +
  facet_wrap(.~country) +
  labs(x = "Individual Exposure") +
  theme_bw()


statistics = data %>%
  select(country, pchoice, indexp, auth, rdemand, inc, relg, tradexp)
ss = describe(statistics)
xtable(ss, type = "latex", file = "sumstats.tex")


data %>%
  filter(family == "Radical Right") %>%
  ggplot(aes(auth)) + geom_histogram() +
  facet_wrap(~country) +
  theme_bw() +
  theme(legend.position="bottom", text = element_text(size = 14, family="LM Roman 10")) +
  labs(x = "Authoritarian Attitudes", y = "Count")


################### Individual Exposure x Authoritarianism ####################


amnull = lmer(auth ~ (1|country), data)

a0 = lm(auth ~ 1, data)

am1 = lmer(auth ~ indexp2 + 
             (1 | country), data)

am2 = lmer(auth ~ indexp2 + 
             (1 + indexp2 | country), data)

am3 = lmer(auth ~ indexp2 + tradexp2 + relg2 + 
             (1 + indexp2 | country), data)

am4 = lmer(auth ~ indexp2 + deltaemp2 + tradexp2 + relg2 + 
             (1 + indexp2 | country), data)

am5 = lmer(auth ~ indexp2*deltaemp2 + tradexp2 + relg2 + 
             (1 + indexp2 | country), data)


################### Individual Exposure x Demand for Redistribution ###########

trimdat = data %>%
  select(rdemand, country, indexp2, inc2, tradexp2)

rm0 = clm(rdemand ~ 1, data=trimdat)

rm1 = clmm2(rdemand ~ 1, random=country, data=trimdat, Hess = T)

rm2 = clmm2(rdemand ~ indexp2, random = country, data = trimdat, Hess=T)

rm3 = clmm2(rdemand ~ indexp2 + inc2 + tradexp2, random = country, data = trimdat, Hess=T)


############################## Mediation Analysis ############################

pdata = data%>%
  select(id, country,pchoice,indexp2,auth2,auth,rdemand) %>%
  pivot_wider(names_from = rdemand, values_from = rdemand) %>%
  replace(is.na(.), 0) %>%
  rename(neut = `2`) %>%
  rename(ref = `1`) %>%
  rename(pref = `3`) %>%
  rename(strpref = `4`) %>%
  mutate(neut = dplyr::recode(neut, `2`=1)) %>%
  mutate(pref = dplyr::recode(pref, `3`=1)) %>%
  mutate(strpref = dplyr::recode(strpref, `4`=1))


p0 = lmer(pchoice ~ indexp2 + (1 |country), data = pdata)
p1 = lmer(pchoice ~ auth2 + (1 |country), data = pdata)
p2 = lmer(pchoice ~ neut + pref + strpref + (1 |country), data = pdata)
p3 = lmer(pchoice ~ indexp2 + auth2 + neut + pref + strpref + (1 |country), data = pdata)
p4 = lmer(pchoice ~ indexp2 + auth2 + (1 |country), data = pdata)
