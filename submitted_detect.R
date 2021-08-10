library(readxl)

#################### FEMP summary #####################

long = read_excel("N:/Gorden_Jiang/FEMP_summary/FEMP_summary28052021.xlsx")

ID = long$ID
address = substr(long$PhysicalAddress,1, 15)
# median(nchar(long$address), na.rm = T)
long = data.frame(ID, address)

##################### FEMP resubmission ###########################

short = read_excel("N:/Gorden_Jiang/FEMP_2021_resubmission/FEMPresub28052021.xlsx")

ID = short$FEMPID
address = substr(short$FarmPhysicalAddress,1, 15)
short = data.frame(ID, address)

##################### FEMP resubmitted (Fuzzy + Hard matching) ##########################

library(fuzzyjoin)
fuz_match = stringdist_join(long, 
                            short, 
                            by = "address", mode = "left", ignore_case = T, method = "jw",max_dist = 3, 
                            distance_col = "dist") %>%  group_by(ID.x) %>% slice(which.min(dist))

fuz_match$match_yes = mapply(function(a, b) grepl(a, b, fixed = T), 
                             substr(fuz_match$address.x,1,5), 
                             substr(fuz_match$address.y,1,5))

print(paste0(sum(fuz_match$match_yes), " detected by fuzzy + hard matching"))

# Compare to hard matching only

long$resubmitted = substr(long$address, 1, 10) %in% substr(short$address, 1, 10)

print(paste0(sum(long$resubmitted), " detected by hard matching only")) 

colnames(fuz_match)[1] = "ID"
compare = merge(x = long, y =  fuz_match, by =  c("ID"), all.x = TRUE)

View(subset(compare, compare$resubmitted == FALSE & compare$match_yes == TRUE))
