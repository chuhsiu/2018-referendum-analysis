############ 六都公投資料匯入 ###############
p = "data/"
town6 = c("臺北市","新北市","桃園市","臺中市","臺南市","高雄市")

readFile <- function(file,n,c) { 
  read_xls(file,
           col_names = FALSE,
           skip = 6,
           sheet=n ) %>% mutate(
             case = n+6,
             county = c) 
}

readTown <- function(path,name){
  pth = paste0(path, name,".xls")
  Reduce(rbind, map(1:10, function(x) readFile(pth,x,name)))
}

ref6 = Reduce(rbind, map(town6, function(x) readTown(p,x)))

# 去除不要的 row / col
ref6 = ref6[!is.na(ref6$...2),c(1:8,12,13,14,15)]

# 重新命名欄位名稱
# town, vill, site(幾個投票所), agree, disagree, valid, invalid, votes, ...9, ...10 ,...11, tickets, rate
names = c("town", "vill", "site", "agree", "disagree", "valid", "invalid", "vote", "ticket", "rate","case","county")
colnames(ref6) = names

# 各里投票所整合
SIX = ref6 %>% group_by(case, county, town, vill) %>% dplyr::summarise(
  sites = n(),
  agree = sum(agree),
  disagree = sum(disagree),
  valid = sum(valid),
  invalid = sum(invalid),
  votes = sum(vote),
  tickets = sum(ticket),
  rate = round((votes/tickets)*100,2)
) %>% ungroup() %>% arrange(county,town, vill)
