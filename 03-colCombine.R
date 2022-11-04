load("town_six.rdata")

############ 以「里」為單位 #############
df_vill = town_six %>%
  right_join(ref_6) %>% 
  transmute(
    V_ID = V_ID, county = county, town = town, vill=vill,
    戶數 = 戶數,人口總數=人口數/1000, # 千分
    性別比 = round(男性人口數/女性人口數*100,2),人口密度=人口密度,
    扶養比=扶養比,老化指數=老化指數,
    edu_master = round((X15歲以上博士人口數+X15歲以上碩士人口數+X15歲以上大學院校人口數+X15歲以上專科人口數)/人口數*100,2),
    edu_normal = round((X15歲以上高中職人口數+X15歲以上國中初職人口數+X15歲以上小學人口數+X15歲以上自修人口數+X15歲以上不識字人口數)/人口數*100,2),
    結婚率 = 粗結婚率, 離婚率=粗離婚率,
    社會增加人口=round(社會增加人數/人口數*100,2),原住民人口數=round(原住民人口數/人口數*100,2),
    co_life = round(共同生活戶數/戶數*100,2),
    co_work_life = round(共同事業戶數/戶數*100,2),
    alone_lift = round(單獨生活戶數/戶數*100,2),
    income =  round(各類所得金額合計/人口數,2),    # 人均收入
    income_profit = round((各類所得金額營利所得/各類所得金額合計*100),2),    # 營利收入比率
    income_business = round(各類所得金額執行業務所得/各類所得金額合計*100,2),    # 執行業務收入比率
    income_payment = round((各類所得金額薪資所得/各類所得金額合計*100),2),     # 薪資收入比率
    income_interest = round(各類所得金額利息所得/各類所得金額合計*100,2),    # 利息收入比率
    income_rent = round(各類所得金額租賃及權利金/各類所得金額合計*100,2),     # 租賃收入比率
    income_transction = round(各類所得金額財產交易所得/各類所得金額合計*100,2),    # 財產交易收入比率
    income_lottery = round(各類所得金額機會中獎所得/各類所得金額合計*100,2),   # 中獎收入比率
    income_stock = round((各類所得金額股利所得/各類所得金額合計*100),2),    # 股利收入比率
    a07 = round(a07/v07*100,2),
    a08 = round(a08/v08*100,2),
    a09 = round(a09/v09*100,2),
    a10 = round(a10/v10*100,2),
    a11 = round(a11/v11*100,2),
    a12 = round(a12/v12*100,2),
    a13 = round(a13/v13*100,2),
    a14 = round(a14/v14*100,2),
    a15 = round(a15/v15*100,2),
    a16 = round(a16/v16*100,2),
    v07 = round(v07/tickets*100,2),
    v08 = round(v08/tickets*100,2),
    v09 = round(v09/tickets*100,2),
    v10 = round(v10/tickets*100,2),
    v11 = round(v11/tickets*100,2),
    v12 = round(v12/tickets*100,2),
    v13 = round(v13/tickets*100,2),
    v14 = round(v14/tickets*100,2),
    v15 = round(v15/tickets*100,2),
    v16 = round(v16/tickets*100,2)
  )

col = c("V_ID","county","town","vill",
        "戶數","人口數","性別比","人口密度","扶養比","老化指數",
        "教育程度(大專以上)","教育程度(高中以下)",
        "結婚率","離婚率","社會增加率","原住民人口比",
        "共同生活戶數","共同事業戶數","單獨生活戶數",
        "人均收入","營利收入比","執行業務收入比","薪資收入比","利息收入比","租賃收入比",
        "財產交易收入比","中獎收入比","股利收入比",
        "07反火力發電","08反燃煤發電","09反核食進口",
        "10反民法同婚","11反同志教育","12專法同婚",
        "13東奧正名","14挺同婚","15挺同志教育","16以核養綠",
        "v07_反火力發電","v08_反燃煤發電","v09_反核食進口",
        "v10_反民法同婚","v11_反同志教育","v12_專法同婚",
        "v13_東奧正名","v14_挺同婚","v15_挺同志教育","v16_以核養綠")
colnames(df_vill) <- col

############ 以「區」為單位 #############
# 將各vill的資料加權合併 / 整理變數欄位 / 比率單位都是：%

df_town <- town_six %>% group_by(county, town) %>% 
  dplyr::summarise(
    Household = sum(戶數),
    People = sum(人口數),
    M_F_rat = round(sum(男性人口數)/sum(女性人口數)*100,2),
    Density = sum(人口數)/sum(人口數/人口密度),
    Support = round((sum(X0.14歲人口數)+sum(X65歲以上人口數))/sum(X15.64歲人口數)*100,2),
    Ageing = round(sum(X65歲以上人口數)/sum(X0.14歲人口數)*100,2),
    edu_master = round((sum(X15歲以上博士人口數)+sum(X15歲以上碩士人口數)+sum(X15歲以上大學院校人口數)+sum(X15歲以上專科人口數))/People*100,2),
    edu_normal = round((sum(X15歲以上高中職人口數)+sum(X15歲以上國中初職人口數)+sum(X15歲以上小學人口數)+sum(X15歲以上自修人口數)+sum(X15歲以上不識字人口數))/People*100,2),
    marriage_couple = round(sum(結婚對數)/People*1000,2), # 單位是 1/1000
    divorce_couple = round(sum(離婚對數)/People*1000,2), # 單位是 1/1000
    social_add = round(sum(社會增加人數)/People*1000,2), # 單位是 1/1000
    ab_rat = round(sum(原住民人口數)/People*100,2),
    co_life = round(sum(共同生活戶數)/Household*100,2),
    co_work_life = round(sum(共同事業戶數)/Household*100,2),
    alone_lift = round(sum(單獨生活戶數)/Household*100,2),
    Income =  round(sum(各類所得金額合計)/People,2),    # 人均收入，單位再確認
    income_profit = round(sum(各類所得金額營利所得)/sum(各類所得金額合計)*100,2),    # 營利收入比率
    income_business = round(sum(各類所得金額執行業務所得)/sum(各類所得金額合計)*100,2),    # 執行業務收入比率
    income_payment = round(sum(各類所得金額薪資所得)/sum(各類所得金額合計)*100,2),     # 薪資收入比率
    income_interest = round(sum(各類所得金額利息所得)/sum(各類所得金額合計)*100,2),    # 利息收入比率
    income_rent = round(sum(各類所得金額租賃及權利金)/sum(各類所得金額合計)*100,2),     # 租賃收入比率
    income_transction = round(sum(各類所得金額財產交易所得)/sum(各類所得金額合計)*100,2),    # 財產交易收入比率
    income_lottery = round(sum(各類所得金額機會中獎所得)/sum(各類所得金額合計)*100,2),   # 中獎收入比率
    income_stock = round(sum(各類所得金額股利所得)/sum(各類所得金額合計)*100,2),    # 股利收入比率
    a07 = round(sum(a07)/sum(v07)*100,2),
    a08 = round(sum(a08)/sum(v08)*100,2),
    a09 = round(sum(a09)/sum(v09)*100,2),
    a10 = round(sum(a10)/sum(v10)*100,2),
    a11 = round(sum(a11)/sum(v11)*100,2),
    a12 = round(sum(a12)/sum(v12)*100,2),
    a13 = round(sum(a13)/sum(v13)*100,2),
    a14 = round(sum(a14)/sum(v14)*100,2),
    a15 = round(sum(a15)/sum(v15)*100,2),
    a16 = round(sum(a16)/sum(v16)*100,2),
    v07 = round(sum(v07)/sum(tickets)*100,2),
    v08 = round(sum(v08)/sum(tickets)*100,2),
    v09 = round(sum(v09)/sum(tickets)*100,2),
    v10 = round(sum(v10)/sum(tickets)*100,2),
    v11 = round(sum(v11)/sum(tickets)*100,2),
    v12 = round(sum(v12)/sum(tickets)*100,2),
    v13 = round(sum(v13)/sum(tickets)*100,2),
    v14 = round(sum(v14)/sum(tickets)*100,2),
    v15 = round(sum(v15)/sum(tickets)*100,2),
    v16 = round(sum(v16)/sum(tickets)*100,2)
  ) %>% ungroup()

col = c("county","town",
        "戶數","人口數","性別比","人口密度","扶養比","老化指數",
        "教育程度(大專以上)","教育程度(高中以下)",
        "結婚率","離婚率","社會增加率","原住民人口比",
        "共同生活戶數","共同事業戶數","單獨生活戶數",
        "人均收入","營利收入比","執行業務收入比","薪資收入比","利息收入比","租賃收入比",
        "財產交易收入比","中獎收入比","股利收入比",
        "07反火力發電","08反燃煤發電","09反核食進口",
        "10反民法同婚","11反同志教育","12專法同婚",
        "13東奧正名","14挺同婚","15挺同志教育","16以核養綠",
        "v07_反火力發電","v08_反燃煤發電","v09_反核食進口",
        "v10_反民法同婚","v11_反同志教育","v12_專法同婚",
        "v13_東奧正名","v14_挺同婚","v15_挺同志教育","v16_以核養綠")
colnames(df_town) <- col


## na 值整理

# apply(df_vill, 2, function(x) sum(is.na(x)))
# apply(df_town, 2, function(x) sum(is.na(x)))

df_vill[is.na(df_vill)] <- 0
df_town[is.na(df_town)] <- 0

save(town_six, df_town, df_vill, file="refData.rdata")
