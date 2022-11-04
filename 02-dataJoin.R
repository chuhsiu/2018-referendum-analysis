############ 整合 V_ID ###############
load("data/RiTW108.rdata")

a10_6 = dplyr::rename(Age10, V_ID = 村里代碼) %>% 
  filter(縣市名稱 %in% town6, !is.na(V_ID), year==108) %>% 
  mutate(V_ID = str_replace(V_ID,"(05|12)(1|2)-","\\10-"))

# spred各案同意率
ax6 = SIX %>% transmute(
  county = county,
  town=str_remove_all(town,"\\s"), vill=vill,
  case=sprintf("a%02d",case), agree=agree) %>% 
  spread(case, agree) %>% arrange(county,town, vill)

# spred各案投票率
vx6 = SIX %>% transmute(
  county = county,
  town=str_remove_all(town,"\\s"), vill=vill,
  case=sprintf("v%02d",case), valid=valid) %>% 
  spread(case, valid) %>% arrange(county,town, vill)

# spred各案投票人數
tx6 = SIX %>% transmute(
  county = county,
  town=str_remove_all(town,"\\s"), 
  vill=vill, 
  tickets=tickets) %>% 
  distinct(county,town, vill, .keep_all = T) %>% 
  arrange(county,town, vill)

ax6 = ax6 %>% cbind(vx6) %>% cbind(tx6); 
ax6 = ax6[,-c(14:16,27:29)]
# 檢查有沒有錯的村里名稱
# ax6$ok = ifelse(ax6$vill == ax6$vill, TRUE, NA)
# ax6[is.na(ax6$ok),]

# 亂碼更正
correctWord = function(x){
  x$村里名稱 = ifelse(x$縣市名稱 == "高雄市" & x$村里名稱=="山里", "峯山里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "高雄市" & x$村里名稱=="公里", "公舘里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "高雄市" & x$村里名稱=="達卡努瓦", "達卡努瓦里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "高雄市" & x$村里名稱=="埔里", "坔埔里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "高雄市" & x$村里名稱=="■北里", "廍北里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "高雄市" & x$村里名稱=="■南里", "廍南里", x$村里名稱)
  
  x$村里名稱 = ifelse(x$縣市名稱 == "臺北市" & x$村里名稱=="糖■里", "糖廍里", x$村里名稱)
  
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="公■里", "公塭里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="■南里", "塭南里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="田里", "塩田里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="南■里", "南廍里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="石■里", "石𥕢里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="寮■里", "寮廍里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="江里", "晋江里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="埕里", "塩埕里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="玉里", "玉峯里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="■林里", "檨林里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="■拔里", "𦰡拔里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="山里", "山脚里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="?頭港里", "坔頭港里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="?興里", "塩興里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="行里", "塩行里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺南市" & x$村里名稱=="洲里", "塩洲里", x$村里名稱)
  
  x$村里名稱 = ifelse(x$縣市名稱 == "臺中市" & x$鄉鎮市區名稱=="北屯區" & x$村里名稱=="■子里", "廍子里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺中市" & x$村里名稱=="龜■里", "龜壳里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺中市" & x$村里名稱=="蔗■里", "蔗廍里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺中市" & x$村里名稱=="榔里", "槺榔里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺中市" & x$村里名稱=="■子里", "廍子里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺中市" && x$鄉鎮市區名稱=="外埔區" &  x$村里名稱=="■子里", "廍子里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺中市" & x$村里名稱=="榔里", "槺榔里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺中市" & x$村里名稱=="公里", "公舘里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "臺中市" & x$村里名稱=="龍里", "双龍里", x$村里名稱)
  
  x$村里名稱 = ifelse(x$縣市名稱 == "桃園市" & x$村里名稱=="林里", "菓林里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "桃園市" & x$村里名稱=="榔里", "槺榔里", x$村里名稱)
  
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="公里", "公舘里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="石■里", "石𥕢里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="爪里", "爪峯里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="永里", "永舘里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="洞里", "濓洞里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="新里", "濓新里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="■寮里", "獇寮里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="廷里", "峯廷里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="崁里", "崁脚里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="新■里", "新廍里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="五里", "五峯里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="灰■里", "灰磘里", x$村里名稱)
  x$村里名稱 = ifelse(x$縣市名稱 == "新北市" & x$村里名稱=="瓦■里", "瓦磘里", x$村里名稱)
  
  return(x)
}

six_code = correctWord(a10_6) %>% 
  group_by(縣市名稱,鄉鎮市區名稱,村里名稱, V_ID) %>% dplyr::summarise(n=sum()) %>%
  arrange(縣市名稱,鄉鎮市區名稱, 村里名稱) %>% select(縣市名稱,鄉鎮市區名稱, 村里名稱, V_ID) %>% ungroup() %>%
  cbind(ax6) 

# 檢查有沒有錯的村里名稱
# six_code$ok = ifelse(six_code$村里名稱 == six_code$vill, TRUE, NA)
# six_code[is.na(six_code$ok), c(1,2,3,7)]

ref_6 = six_code[,c(-1,-2,-3,-29)]
# save(ref_6, file="ref_6.rdata")

############ 整合 - 社會經濟變數 ###############

i = read.csv("data/socioeconomic.csv" ,fileEncoding = "big5", skip=1)

i = i %>% dplyr::rename(V_ID = 村里代碼) %>% 
  filter(縣市名稱 %in% town6, !is.na(V_ID))

# 變數選擇：戶數7 人口數8 男人口9 女人口10 人口密度13 18:38 45:53碩博 大學專科 高中以下 結婚對數66 離婚對數68 社會增加數70 原住民88  111>113
# 選出要的column 
ii = i[,c(1,2,4,5,7:10,13,14,17,39:41,45:53,66,68,70,80:81,88,111:113,150:158)]

ii[is.na(ii)] <- 0

town_six = ii %>% right_join(ref_6, by=c("V_ID" = "V_ID")) %>% select(-1,-2,-3)
town_six = town_six[,c(1,39:41,2:38,42:62)]

save(town_six, file="town_six.rdata")
