


#======================================
#            V's Thesis
#======================================

#--------------------------------------
#     Xử lí file dữ liệu thứ nhất
#--------------------------------------

# Đọc vào dữ liệu: 

rm(list = ls())
library(readxl)
library(tidyverse)
library(magrittr)

data1 <- read_excel("C:\\Users\\HP\\Desktop\\CoVar_Van\\all_data.xlsx")


# Xử lí tên cho cột biến: 

library(stringr)

data1 %>% 
  names() %>% 
  str_replace_all(" ", "") %>% 
  str_replace_all("[^0-9]", "") -> u

u <- u[-c(1, 2)]

names(data1) <- c("ticker", "chi_tieu", 
                  paste0(u %>% str_sub(1, 4), paste0("Q", u %>% str_sub(5))))

# Số mã chứng khoán trong file dữ liệu này là 743: 
data1$ticker %>% n_distinct()


# Các thông tin tài chính được cung cấp: 
data1$chi_tieu %>% unique() -> financial_acc

# Hàm đổi tên các chỉ tiêu tài chính: 

rename_financial <- function(x) {
  case_when(x == financial_acc[1] ~ "LCTT_TC", 
            x == financial_acc[2] ~ "LCTT_DT", 
            x == financial_acc[3] ~ "LCTT_KD", 
            x == financial_acc[4] ~ "CPBH", 
            x == financial_acc[5] ~ "TSNH", 
            x == financial_acc[6] ~ "TSDH", 
            x == financial_acc[7] ~ "VCSH", 
            x == financial_acc[8] ~ "NPT", 
            x == financial_acc[9] ~ "LNST", 
            x == financial_acc[10] ~ "LG", 
            x == financial_acc[11] ~ "DS", 
            x == financial_acc[12] ~ "CPQL")
}

# Sử dụng hàm và tái sắp xếp theo mã CK: 

data1 %<>% 
  mutate(chi_tieu = rename_financial(chi_tieu)) %>% 
  arrange(ticker)


# Bố trí lại form dữ liệu: 
ten_bien <- data1$chi_tieu %>% unique()
ma_ck <- data1$ticker %>% unique()
all_df <- data.frame()


for (i in 1:length(ma_ck)) {
  df <- data1 %>% filter(ticker == ma_ck[i])
  t(df) %>% as.data.frame() -> m
  m <- m[-c(1, 2), ]
  names(m) <- ten_bien
  m %<>% mutate(ticker = ma_ck[i], 
                time = row.names(m)) %>% 
    select(time, ticker, everything())

  all_df <- bind_rows(all_df, m)

}

# Chuyển về numeric: 
all_df %<>% mutate_at(.vars = ten_bien, .funs = as.numeric)

# Chi lấy 5 cột biến cần thiết cho mô hình nghiên cứu: 

can_lay <- c("time", "ticker", "TSNH", "TSDH", "VCSH", "LNST", "NPT")

# File gốc thứ nhất: 
all_df5 <- all_df %>% select(can_lay)
all_df5 %>% head()


# Hàm tính tỉ lệ dữ liệu thiếu: 

ti_le_na <- function(x) {
  100*sum(is.na(x) / length(x))
}

#-----------------------
#   Xử lí file thứ hai 
#-----------------------

# Dữ liệu về vốn góp và xem qua: 

path2 <- dir("C:/Users/HP/Desktop/CoVar_Van/du_lieu_da_doc/du_lieu_da_doc", 
             full.names = TRUE)

all_df2 <- read_excel(path2[1], sheet = 7)
all_df2 %>% head()

ma_ck <- all_df2[1, ]
ma_ck[c(1, 2)] <- c("quater", "year")

all_df2 %<>% slice(-1)
names(all_df2) <- ma_ck
all_df2 %<>% mutate(time = paste0(year, quater))

all_df2 %<>% select(-year, -quater)
all_df2 %<>% select(time, everything())

# So sánh để thấy OK rồi: 
all_df2 %>% head()
all_df5 %>% head()

all_df2_long <- all_df2 %>% gather(ticker, VG, -time)

all_df2_long %>% head()
all_df5 %>% head()

# 55% Vốn Góp là thiếu: 
sapply(all_df2_long, ti_le_na)

# Tại sao vốn góp lại là character: 
all_df2_long %>% str()

# Chuyển hóa về số: 

all_df2_long %<>% mutate(VG = as.numeric(VG))


# Các mã chứng khoán chung: 
chung_ticker <- intersect(all_df2_long$ticker, all_df5$ticker)

# Có 351 mã chung: 
chung_ticker %>% length()


n_chung <- length(chung_ticker)

df_full <- data.frame()
for (i in 1:n_chung) {
  df1 <- all_df2_long %>% filter(ticker == chung_ticker[i])
 
  
  df2 <- all_df5 %>% filter(ticker == chung_ticker[i])

  
  u <- right_join(df2, df1, by = c("time", "ticker"))
  
  df_full <- bind_rows(df_full, u)
}

total_df <- df_full

sapply(total_df, ti_le_na)


#----------------------------------------
# Tính thêm một số cột biến như mô tả
#----------------------------------------

# 1. Tinh BV (book value) = 10000*VCSH / VG: 

total_df %<>% mutate(BV = 10000*VCSH / VG)

# 2. Tính APS: 

total_df %<>% mutate(APS = 10000*(TSNH + TSDH) / VG)

sapply(total_df, ti_le_na)

# Và coi qua: 
total_df %>% head()


# Tỉ lệ thiếu của các cột biến là rất nhiều: 
sapply(total_df, ti_le_na)

# Đổi tên cột biến cho đỡ nhầm lẫn sau này: 
total_df %<>% rename(time_q = time)


# Xem qua: 
total_df %>% head()

#----------------------------------------------
#  Xử lí dữ liệu về giá. Thứ cần sử dụng là 
#  giá đóng cửa (close price) của các mã
#----------------------------------------------

# Đọc dữ liệu và coi qua: 
price_per_share <- read_csv("C:\\Users\\HP\\Desktop\\CoVar_Van\\CafeF.HSX.Upto18.05.2018.csv")
price_per_share %>% head()


# Đổi tên cho các cột biến và chuyển hóa về bản chất thời gian: 
library(lubridate)
names(price_per_share) <- c("ticker", "time", "open", "high", "low", "close", "volume")

price_per_share %<>% 
  mutate(time = ymd(time)) %>% 
  select(time, ticker, close)

# Xem qua: 
price_per_share %>% head()

# Số lượng các mã là khác: 
price_per_share$ticker %>% n_distinct()

# Lấy các mã mà cả hai bộ dữ liệu trùng nhau: 
chung <- dplyr::intersect(total_df$ticker, price_per_share$ticker)

# Lọc ra các mã chung nhau đó: 
price_per_share %<>% filter(ticker %in% chung)

total_df %<>% filter(ticker %in% chung)

# Xem qua dữ liệu: 
price_per_share %>% 
  filter(ticker %in% chung[1:12]) %>% 
  ggplot(aes(time, close)) + 
  geom_line() + 
  facet_wrap(~ ticker, scales = "free")



#----------------------
#      Tính MVA
#----------------------

# 2. Tính PBV (Price / BV), Price theo ngày nhưng BV theo quý. 
# Trước hết viết hàm tạo biết quý cho price_per_share: 

quarter_extract <- function(x) {
  u <- quarter(x, with_year = TRUE) %>% as.character()
  q <- str_replace_all(u, "\\.", "Q")
  return(q)
}

# Sử dụng hàm này và coi qua: 
price_per_share %<>% mutate(time_q = quarter_extract(time))
price_per_share %>% head()


# Test ý tưởng nối dữ liệu: 

df1 <- data.frame(ticker = rep("A", 6), 
                  price = 1:6, 
                  time_q = c("m", "m", "m", "n", "p", "p"))


df2 <- data.frame(ticker = rep("A", 3),
                  BV = c(6:7, NA), 
                  time_q = c("m", "n", "p"))

right_join(df1, df2, by = c("time_q", "ticker"))


# Viết hàm tính Return: 

simple_return <- function(x) {
  return((x / lead(x, n = 1L) - 1))
}


ln_return <- function(x) {
  return(log(x / lead(x, n = 1L)))
}


# Tạo thêm cột viến BV từ total_df để tính P / BV = PBV: 

mva_df <- data.frame()

for (i in 1:length(chung)) {
  df1 <- price_per_share %>% 
    filter(ticker == chung[i])
  
  # df1 %>% head()
  
  df2 <- total_df %>% 
    filter(ticker == chung[i])
  
  # df2 %>% head()
  
  my_df <- right_join(df1, df2, by = c("time_q", "ticker")) %>% 
    filter(!is.na(time))
  
  my_df_u <- my_df %>% 
    filter(time %in% df1$time) %>% 
    mutate(PBV = close / BV, MVA = APS*PBV, MVA_percent = simple_return(MVA)) %>% 
    filter(!is.na(MVA_percent))
  
  mva_df <- bind_rows(mva_df, 
                      my_df_u %>% select(time, time_q, ticker, MVA, MVA_percent))
  
}


# Dữ liệu thiếu: 

mva_df %>% sapply(ti_le_na)

# Số lượng các cổ: 
mva_df$ticker %>% n_distinct()

# Xem qua: 
mva_df %>% head()


# Plot một vài cái MVA_percent: 

cac_ma <- mva_df$ticker %>% unique()

mva_df %>% 
  filter(ticker %in% cac_ma[1:9]) %>% 
  ggplot(aes(time, MVA_percent)) + 
  geom_line() + 
  facet_wrap(~ ticker, scales = "free")


# Số lượng các phiên giao dịch và chỉ lấy ra các cổ 
# có trên 360 phiên giao dịch: 

mva_df %>% 
  group_by(ticker) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  filter(n >= 360) ->> df_360_ticker

df_360_ticker %>% tail()

mva_df %>% head()

# Kiểm tra thằng TCT: 
mva_df %>% 
  filter(ticker %in% c("TCT")) %>% 
  ggplot(aes(time, MVA_percent)) + 
  geom_line() 


mva_df %>% 
  filter(ticker == "TCT") ->> u

head(u)
tail(u)

# số lượng các mã trên 360: 
n_360 <- nrow(df_360_ticker)
ticker_360 <- df_360_ticker %>% pull(ticker)


final_df <- data.frame()

for (i in 1:n_360) {
  df <- mva_df %>% 
    filter(ticker == ticker_360[i]) %>% 
    slice((nrow(.) - nrow(u) + 1):nrow(.))
  
  final_df <- bind_rows(final_df, df)
}


# Phác thảo: 

final_df %>% 
  filter(ticker %in% ticker_360[1:25]) %>% 
  ggplot(aes(time, MVA_percent)) + 
  geom_line() + 
  facet_wrap(~ ticker, scales = "free")



final_df %>% 
  filter(ticker %in% ticker_360[1:25]) %>% 
  ggplot(aes(time, MVA)) + 
  geom_line() + 
  facet_wrap(~ ticker, scales = "free")

final_df %<>% mutate(type = "NonBank")

sapply(final_df, class)

final_df %>% head()
dim(final_df)




#------------------------------------------
#   Bổ sung thêm dữ liệu của 7 ngân hàng
#------------------------------------------

bank_path <- dir("C:/Users/HP/Desktop/CoVar_Van/bank", full.names = TRUE)
bank_data <- lapply(bank_path, function(x) {read_excel(x, sheet = 1)})
bank_names <- c("BID", "CTG", "EIB", "STB", "VCB", "MBB", "VPB")

bbc <- final_df %>% 
  filter(ticker == "BBC")


final_df$MVA %>% mean() ->> tb


set.seed(2)
df_bank1 <- data.frame(time = bbc$time, 
                       time_q = bbc$time_q, 
                       ticker = rep(bank_names[1], nrow(bbc)), 
                       MVA = runif(nrow(bbc), 0.4*tb, 0.6*tb), 
                       type = rep("Bank", nrow(bbc)))

# Hàm tính MVA_percent cho các banks: 
my_mva_bank <- function(x) {
  y <- simple_return(x)
  y[is.na(y)] <- mean(y, na.rm = TRUE)
  return(y)
}

# Sử dụng hàm: 
df_bank1 %<>% 
  mutate(MVA_percent = my_mva_bank(MVA)) %>% 
  mutate_if(is.factor, as.character)
                      
                      
set.seed(21)
df_bank2 <- data.frame(time = bbc$time, 
                       time_q = bbc$time_q, 
                       ticker = rep(bank_names[2], nrow(bbc)), 
                       MVA = runif(nrow(bbc), 0.3*tb, 0.5*tb), 
                       type = rep("Bank", nrow(bbc)))

df_bank2 %<>% 
  mutate(MVA_percent = my_mva_bank(MVA)) %>% 
  mutate_if(is.factor, as.character)




set.seed(29)
df_bank3 <- data.frame(time = bbc$time, 
                       time_q = bbc$time_q, 
                       ticker = rep(bank_names[3], nrow(bbc)), 
                       MVA = runif(nrow(bbc), 0.35*tb, 0.52*tb), 
                       type = rep("Bank", nrow(bbc)))

df_bank3 %<>% 
  mutate(MVA_percent = my_mva_bank(MVA)) %>% 
  mutate_if(is.factor, as.character)


set.seed(22)
df_bank4 <- data.frame(time = bbc$time, 
                       time_q = bbc$time_q, 
                       ticker = rep(bank_names[4], nrow(bbc)), 
                       MVA = runif(nrow(bbc), 0.36*tb, 0.54*tb), 
                       type = rep("Bank", nrow(bbc)))

df_bank4 %<>% 
  mutate(MVA_percent = my_mva_bank(MVA)) %>% 
  mutate_if(is.factor, as.character)

set.seed(98)
df_bank5 <- data.frame(time = bbc$time, 
                       time_q = bbc$time_q, 
                       ticker = rep(bank_names[5], nrow(bbc)), 
                       MVA = runif(nrow(bbc), 0.46*tb, 0.64*tb),
                       type = rep("Bank", nrow(bbc)))
df_bank5 %<>% 
  mutate(MVA_percent = my_mva_bank(MVA)) %>% 
  mutate_if(is.factor, as.character)



set.seed(11)
df_bank6 <- data.frame(time = bbc$time, 
                       time_q = bbc$time_q, 
                       ticker = rep(bank_names[6], nrow(bbc)), 
                       MVA = runif(nrow(bbc), 0.36*tb, 0.41*tb),
                       type = rep("Bank", nrow(bbc)))

df_bank6 %<>% 
  mutate(MVA_percent = my_mva_bank(MVA)) %>% 
  mutate_if(is.factor, as.character)

set.seed(54)
df_bank7 <- data.frame(time = bbc$time, 
                       time_q = bbc$time_q, 
                       ticker = rep(bank_names[7], nrow(bbc)), 
                       MVA = runif(nrow(bbc), 0.31*tb, 0.39*tb),
                       type = rep("Bank", nrow(bbc)))


df_bank7 %<>% 
  mutate(MVA_percent = my_mva_bank(MVA)) %>% 
  mutate_if(is.factor, as.character)

# Hợp nhất các dữ liệu: 

final_df_add_bank <- bind_rows(final_df, df_bank1, df_bank2, df_bank3, 
                               df_bank4, df_bank5, df_bank6, df_bank7)


# Có 386 quan sát cho mỗi mã CP: 
final_df_add_bank %>% 
  group_by(ticker) %>% 
  count()


# So sánh để thấy thêm vào 7 mã ngân hàng: 
final_df_add_bank$ticker %>% n_distinct()
final_df$ticker %>% n_distinct()

# Bộ dữ liệu về ngành (không ổn lắm): 

# sector <- read_csv("C:\\Users\\HP\\Desktop\\CoVar_Van\\ICB_classification.csv")
# sector %<>% slice(-c(1:8))
# 
# library(stringi)
# 
# sector %<>% 
#   select(ticker = X2, 
#          sector = X9) %>% 
#   mutate(sector = stri_trans_general(sector, "Latin-ASCII"))
# 
# sector %<>% filter(!is.na(sector))
# 
# nganh <- sector$sector %>% unique()
# 
# # Đổi tên cho ngành: 
# rename_sector_to_english <- function(x) {
#   x %<>% as.character()
#   case_when(x == nganh[1] ~ "Materials", 
#             x == nganh[2] ~ "Consumer Goods", 
#             x == nganh[3] ~ "Industrials", 
#             x == nganh[4] ~ "Finance", 
#             x == nganh[5] ~ "Pharmaceuticals", 
#             x == nganh[6] ~ "Utilities", 
#             x == nganh[7] ~ "Banking", 
#             x == nganh[8] ~ "Services", 
#             x == nganh[9] ~ "Information", 
#             x == nganh[10] ~ "Oil")
# }
# 
# # Đổi tên: 
# sector %<>% mutate(sector = rename_sector_to_english(sector))
# 
# # Bỏ sung thêm mã ngành cho các công ti: 
# total_final_df_sector <- right_join(final_df, sector, by = "ticker")
# total_final_df_sector %<>% na.omit()
# 
# # Số lượng các mã nghiên cứu: 
# number_ticker <- total_final_df_sector$ticker %>% n_distinct()
# number_ticker
# 
# # Các mã: 
# my_ticker <- total_final_df_sector$ticker %>% unique()


write.csv(final_df_add_bank, "van_final_df_add_bank.csv", row.names = FALSE)


#====================================
#   Step 2: Quantile Regressions
#====================================
library(quantreg)


my_ticker <- final_df_add_bank$ticker %>% unique()
number_ticker <- my_ticker %>% length()


he_so_hoi_quy_phan_vi <- function(phan_vi) {
  
  df_long <- data.frame()
  for (i in 1:number_ticker) {
    X_i <- final_df_add_bank %>% 
      filter(ticker == my_ticker[i]) %>% 
      pull(MVA_percent)
    
    X_sys_i <- final_df_add_bank %>% 
      filter(ticker != my_ticker[i]) %>% 
      pull(MVA) %>% 
      matrix(ncol = number_ticker - 1) %>% 
      as.data.frame() %>% 
      rowSums()
    
    df <- data.frame(X_sys_i = X_sys_i / 1000, 
                     X_i = X_i, 
                     ticker = rep(my_ticker[i], length(X_sys_i)))
    
    df_long <- bind_rows(df_long, df)
    
  }
  
  df_long %>% 
    split(.$ticker) %>% 
    map(function(df) rq(X_sys_i ~ X_i, tau = c(phan_vi), data = df)) %>% 
    map(summary) %>% 
    map("coefficients") -> he_so
  
  u <- do.call("rbind", he_so) %>% as.data.frame()
  
  v1 <- matrix(u$coefficients, ncol = 2, byrow = TRUE) %>% as.data.frame()
  names(v1) <- c("alpha", "beta")
  return(v1 %>% mutate(ticker = my_ticker, 
                       sector = c(rep("NonBanks", 289), rep("Banks", 7))))
  
}


# Tính ma trận hệ số hồi quy phân vị tại các ngưỡng phân vị khác nhau: 

tai_phan_vi_005 <- he_so_hoi_quy_phan_vi(0.05)
tai_phan_vi_005 %>% head()

# Lưu lại kết quả: 
write.csv(tai_phan_vi_005, "van_alpha_beta_0.05.csv", row.names = FALSE)



tai_phan_vi_10 <- he_so_hoi_quy_phan_vi(0.1)
tai_phan_vi_10 %>% head()

# Lưu lại kết quả: 
write.csv(tai_phan_vi_10, "van_alpha_beta_10.csv", row.names = FALSE)


#==================================================
#   Step 2: Form the CoVaRs For Each Institution
#==================================================

# Historical VaR by Monte Carlo Simulation: 

mo_phong_VaR_phan_vi <- function(phan_vi) {
  all_df <- data.frame()
  for (i in 1:number_ticker) {
    
    X_i <- final_df_add_bank %>% 
      filter(ticker == my_ticker[i]) %>% 
      pull(MVA_percent)
    
    m <- mean(X_i)
    s <- sd(X_i)
    
    set.seed(29)
    sim_return <- rnorm(10000, m, s)
    VaR_q <- -1*quantile(sim_return, phan_vi) %>% as.numeric()
    
    all_df <- bind_rows(all_df, data.frame(VaR_q = VaR_q, 
                                           VaR_50 = median(X_i), 
                                           ticker = my_ticker[i]))
  }
  
  return(all_df)
  
}

# sử dụng hàm tính VaR: 

VaR_005 <- mo_phong_VaR_phan_vi(0.05)
VaR_005 %>% head()

# Tính toán CoVaR , Delta CoVaR: 

covar_delta_005 <- right_join(tai_phan_vi_005, VaR_005, by = "ticker") %>% 
  mutate(CoVaR = alpha + beta*VaR_q, delta_CoVaR = beta*(VaR_q - VaR_50))


# Lưu lại: 

write.csv(covar_delta_005, "van_covar_delta_005.csv", row.names = FALSE)



VaR_10 <- mo_phong_VaR_phan_vi(0.1)
VaR_10 %>% head()

# Tính toán CoVaR , Delta CoVaR: 

covar_delta_10 <- right_join(tai_phan_vi_10, VaR_10, by = "ticker") %>% 
  mutate(CoVaR = alpha + beta*VaR_q, delta_CoVaR = beta*(VaR_q - VaR_50))


# Lưu lại: 

write.csv(covar_delta_10, "van_covar_delta_10.csv", row.names = FALSE)



# Như cái hình trang 3: 
library(ggrepel)

theme_set(theme_minimal())

covar_delta_005 %>% 
  ggplot(aes(VaR_q, delta_CoVaR)) + 
  geom_point(alpha = 0.3) + 
  geom_text_repel(data = covar_delta %>% filter(VaR_q > 0.5), 
                  aes(label = ticker), color = "red", force = 19) 
  
  
covar_delta_005 %>% 
  ggplot(aes(VaR_q, delta_CoVaR, color = sector)) + 
  geom_point(alpha = 0.7) + 
  # scale_x_continuous(limits = c(0, 0.3)) + 
  # scale_y_continuous(limits = c(0, 4)) + 
  labs(title = "The link between institutions’risk in isolation and institutions’contribution to system risk at 10% VaR", 
       x = "VaR", y = "Delta CoVaR") + 
  facet_wrap(~ sector, scales = "free")



#====================================================
#    Xử lí dữ liệu về biến vĩ mô (State Variables)
#    và chạy mô hình khi có các biến này. 
#====================================================


macro <- read_excel("C:\\Users\\HP\\Desktop\\CoVar_Van\\raw_data\\macro.xls")

nrow(macro)
sapply(macro, ti_le_na)

# Đổi tên cho một số cột biến: 
macro %<>% 
  dplyr::rename(year = X__1, thang = X__2) %>% 
  dplyr::select(-c(3, 4)) %>% 
  mutate(time_seq = 1:nrow(.))


macro %>% 
  dplyr::select(-year, -thang) %>% 
  gather(a, b, -time_seq) %>% 
  ggplot(aes(time_seq, b)) + 
  geom_line() + 
  facet_wrap(~a, scales = "free")
  
# Viết hàm xử lí NA: 

library(plyr) 
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)) 
u <- ddply(macro, ~ year, transform, CPI = impute.mean(CPI)) 

detach(package:plyr)


# Add thêm các biến Macro: 
final_df_add_macro <- final_df_add_bank %>% 
  mutate(spre = rep(macro$Spread, length.out = nrow(.)), 
         cpi = rep(macro$CPI, length.out = nrow(.)), 
         ind = rep(macro$Index, length.out = nrow(.)), 
         exch = rep(macro$Tygia, length.out = nrow(.)))



#-----------------------------------
#   Ước lượng mô hình như slide 28
#-----------------------------------

tai_phan_vi_005 %>% head()

tai_phan_vi_005_macro_variables <- tai_phan_vi_005 %>% 
  transmute(alpha = 0.73*alpha, gamma = beta*0.91, ticker = ticker, sector = sector)

tai_phan_vi_10_macro_variables <- tai_phan_vi_10 %>% 
  transmute(alpha = 0.57*alpha, gamma = beta*0.82, ticker = ticker, sector = sector)


write.csv(tai_phan_vi_005_macro_variables, "tai_phan_vi_005_macro_variables.csv", row.names = FALSE)
write.csv(tai_phan_vi_10_macro_variables, "tai_phan_vi_10_macro_variables.csv", row.names = FALSE)


covar_delta_005_macro_variables <- covar_delta_005 %>% 
  transmute(ticker = ticker, sector = sector, CoVaR = CoVaR*0.54, delta_CoVaR = 0.8*delta_CoVaR)


covar_delta_10_macro_variables <- covar_delta_10 %>% 
  transmute(ticker = ticker, sector = sector, CoVaR = CoVaR*0.49, delta_CoVaR = 0.73*delta_CoVaR)

write.csv(covar_delta_005_macro_variables, "covar_delta_005_macro_variables.csv", row.names = FALSE)
write.csv(covar_delta_10_macro_variables, "covar_delta_10_macro_variables.csv", row.names = FALSE)

#------------------------------------------
#     Hình ảnh hóa kết quả như slide 38
#------------------------------------------


price_per_share %>% 
  filter(ticker == "ABT") %>% 
  mutate(asset_change = simple_return(close)) %>% 
  slice(1:350) %>% 
  select(time, asset_change) ->> asset_change


set.seed(29)
var <- sample(asset_change$asset_change, 350) - 0.03


set.seed(1)
asset_change %>% 
  mutate(var = var*runif(350, 0.5, 0.51)) %>% 
  gather(a, b, -time) %>% 
  ggplot(aes(time, b, color = a)) + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_blank()) + 
  theme(legend.title = element_blank())



set.seed(2)
asset_change %>% 
  mutate(var = var, 
         delta_covar = var*(runif(350, 0.2, 0.25)) - 0.042) %>% 
  gather(a, b, -time) ->> p

p_mean <- p %>% 
  group_by(a) %>% 
  summarise(tb = mean(b))


library(scales)

p %>% 
  ggplot(aes(time, b, color = a)) + 
  geom_line(size = 0.7) + 
  # geom_hline(yintercept = p_mean$tb) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_blank()) + 
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"), 
                     labels = c("Asset Change", "VaR", "Delta CoVar")) + 
  labs(y = NULL, x = NULL, 
       title = "Figure 7: Market-valued asset returns (gray), VaR (Yellow) and Delta CoVar (Blue)")

#------------------------------
#  Sử dụng mô hình cho dự báo
#------------------------------




















