
library(readxl)   
library(ggplot2)  
library(vcdExtra) 



#veri seti yC<kleme
tablo_frekans <- read_excel(file.choose())

head(tablo_frekans)

colnames(tablo_frekans)[2] <- "Cinsiyet"
colnames(tablo_frekans)[5] <- "Sinif"
colnames(tablo_frekans)[11] <- "Tecrube_Guven"
colnames(tablo_frekans)[12] <- "Oncelik"
colnames(tablo_frekans)[13] <- "Ortam_Rahatsizlik"


tablo_frekans$Freq <- 1 

tablo_frekans$Oncelik <- factor(tablo_frekans$Oncelik)
tablo_frekans$Tecrube_Guven <- factor(tablo_frekans$Tecrube_Guven)

#---------------------
# A SECENEDD0
#---------------------
#cinsiyet daDD1lD1mD1
ggplot(tablo_frekans, aes(x = Cinsiyet)) +
  geom_bar(fill = "purple") +
  theme_minimal() +
  labs(title = "Cinsiyet DaDD1lD1mD1", x = "Cinsiyet", y = "SayD1")

#yaElD1lara tanD1nan rahatsD1zlD1k C6nceliDi
ggplot(tablo_frekans, aes(x = Oncelik)) +
  geom_bar(fill = "coral") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(title = "YaElD1lara TanD1nan Cncelik RahatsD1zlD1DD1", x = "KatD1lD1m DC<zeyi", y = "SayD1")

#yaElD1lar ile aynD1 ortamda bulunma
ggplot(tablo_frekans, aes(x = Ortam_Rahatsizlik)) +
  geom_bar(fill = "seagreen") +
  theme_minimal() +
  labs(title = "YaElD1lar ile AynD1 Ortamda Bulunma Durumu", x = "RahatsD1zlD1k DC<zeyi", y = "SayD1")

#tecrC<beye gC<ven
ggplot(tablo_frekans, aes(x = Tecrube_Guven)) +
  geom_bar(fill = "darkred") +
  theme_minimal() +
  labs(title = "YaElD1larD1n TecrC<belerine GC<ven", x = "GC<ven DC<zeyi", y = "SayD1")


#=========================================================================
# B SECENEDD0
#=========================================================================
library(vcd)
library(readxl)

veri <- read_excel("yanitlar.xlsx")

colnames(veri)[7]  <- "Buyurken_Yasli"
colnames(veri)[10] <- "Bakim_Sorumlulugu"

# ??apraz tablo (Evet ??nce gelsin)
tablo <- table(veri$Buyurken_Yasli, veri$Bakim_Sorumlulugu)
tablo <- tablo[c("Evet", "Hay??r"), c("Evet", "Hay??r")]

n11 <- tablo["Evet", "Evet"]
n12 <- tablo["Evet", "Hay??r"]
n21 <- tablo["Hay??r", "Evet"]
n22 <- tablo["Hay??r", "Hay??r"]

# Odds oran?? ve g??ven aral??????
odds_orani   <- (n11 * n22) / (n12 * n21)
log_odds     <- log(odds_orani)
se_log_odds  <- sqrt(1/n11 + 1/n12 + 1/n21 + 1/n22)
alt_sinir    <- exp(log_odds - 1.96 * se_log_odds)
ust_sinir    <- exp(log_odds + 1.96 * se_log_odds)

# Ki-kare testi
ki_kare <- chisq.test(tablo, correct = FALSE)

# Sonu??lar
cat("==================================================================\n")
cat("ANAL??Z: B??y??rken Ya??l??yla Ya??ama vs. Bak??m Sorumlulu??u ??stlenme\n")
cat("==================================================================\n\n")
print(tablo)
cat("\n")
cat(" -> Odds Oran?? (Theta)    :", round(odds_orani, 4), "\n")
cat(" -> Log-Odds De??eri       :", round(log_odds, 4), "\n")
cat(" -> Standart Hata (SE)    :", round(se_log_odds, 4), "\n")
cat(" -> %95 G??ven Aral??????     : [", round(alt_sinir, 4), ",", round(ust_sinir, 4), "]\n")
cat(" -> Ki-Kare P-De??eri      :", format(ki_kare$p.value, scientific = TRUE), "\n\n")

if (alt_sinir > 1) {
  cat(" -> YORUM: G??ven aral?????? 1 de??erini KAPSAMADI??I i??in ili??ki\n")
  cat("           istatistiksel olarak ANLAMLI ve ??NEML??D??R.\n")
} else {
  cat(" -> YORUM: G??ven aral?????? 1 de??erini KAPSADI??I i??in ili??ki\n")
  cat("           istatistiksel olarak ANLAMSIZDIR.\n")
}




#-------------------------------------------------------------------------
# D SECENEDD0
#-------------------------------------------------------------------------
# DeDiEkenler: Toplumdan DD1Elanma vs Cnceliklerden RahatsD1z Olma

if(!require(readxl)) install.packages("readxl")
if(!require(ca)) install.packages("ca")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel")

library(readxl)
library(ca)
library(ggplot2)
library(ggrepel)

colnames(veri)[13] <- "Toplumdan_Dislanma"
colnames(veri)[16] <- "Onceliklerden_Rahatsiz_Olma"

# Uyum Analizi
tablo_oncelik <- table(veri$Toplumdan_Dislanma, veri$Onceliklerden_Rahatsiz_Olma)
uyum_oncelik <- ca(tablo_oncelik)

# KoordinatlarD1n Hata Vermeyecek Eekilde CD1karD1lmasD1 
# ca paketinin standart koordinat C'D1ktD1 matrislerini alD1yoruz
satir_matris <- as.data.frame(uyum_oncelik$rowcoord[, 1:2])
sutun_matris <- as.data.frame(uyum_oncelik$colcoord[, 1:2])

# SC<tun isimlerini netleEtiriyoruz
colnames(satir_matris) <- c("Dim1", "Dim2")
colnames(sutun_matris) <- c("Dim1", "Dim2")

# Etiketleri doDrudan matrisin satD1r isimlerinden C'ekiyoruz
satir_matris$Gosterilecek_Yazi <- rownames(tablo_oncelik)
satir_matris$Degisken_Turu <- "Toplumdan DD1ElanD1yorlar"

sutun_matris$Gosterilecek_Yazi <- colnames(tablo_oncelik)
sutun_matris$Degisken_Turu <- "Onceliklerden RahatsD1z Oluyorum"


grafik_verisi_yeni <- rbind(satir_matris, sutun_matris)

# GGREPEL D0LE NET VE KARIEMAYAN BD0PLOT CD0ZD0MD0
ggplot(grafik_verisi_yeni, aes(x = Dim1, y = Dim2, color = Degisken_Turu, shape = Degisken_Turu)) +
  # Merkez orijin C'izgileri
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  
  # NoktalarD1 belirgin ve bC<yC<k basalD1m
  geom_point(size = 4, stroke = 1.5) +
  
  
  geom_text_repel(aes(label = Gosterilecek_Yazi),
                  size = 4.5,
                  fontface = "bold",
                  padding = unit(0.5, "lines"), 
                  max.overlaps = Inf,           
                  show.legend = FALSE) +
  
  # Renkleri Mavi ve KD1rmD1zD1 yapalD1m
  scale_color_manual(values = c("Toplumdan DD1ElanD1yorlar" = "royalblue3", 
                                "Onceliklerden RahatsD1z Oluyorum" = "firebrick2")) +
  scale_shape_manual(values = c("Toplumdan DD1ElanD1yorlar" = 16, # Yuvarlak
                                "Onceliklerden RahatsD1z Oluyorum" = 17)) + # CC'gen
  
  # Grafik BaElD1klarD1 ve Eksen D0simleri
  labs(title = "Toplumdan DD1Elanma AlgD1sD1 ve Onceliklerden RahatsD1z Olma Durumu",
       subtitle = "Uyum Analizi D0ki Boyutlu AlgD1 HaritasD1 (Biplot)",
       x = "Boyut 1", 
       y = "Boyut 2", 
       color = "DeDiEkenler",
       shape = "DeDiEkenler") +
  
  
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "gray80"),
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_blank())




#---------------------
# F SECENEDD0
#---------------------


frekanslar <- c(11, 7, 12, 27, 27,  # Erkek, RahatsD1z DeDil (Haz..4)
                12, 15, 25, 38, 55, # KadD1n, RahatsD1z DeDil
                8, 12, 12, 20, 22,  # Erkek, RahatsD1z/KararsD1z
                9, 5, 16, 22, 23)   # KadD1n, RahatsD1z/KararsD1z

# Tablo
tablo_final <- expand.grid(
  Sinif = c("HazD1rlD1k", "1. sD1nD1f", "2. sD1nD1f", "3. sD1nD1f", "4. sD1nD1f"),
  Cinsiyet = c("Erkek", "KadD1n"),
  Oncelik = c("Rahatsiz_Degil", "Rahatsiz_Kararsiz")
)
tablo_final$Freq <- frekanslar

# ReferanslarD1 ayarlama
tablo_final$Sinif <- factor(tablo_final$Sinif, levels = c("HazD1rlD1k", "1. sD1nD1f", "2. sD1nD1f", "3. sD1nD1f", "4. sD1nD1f"))
tablo_final$Cinsiyet <- factor(tablo_final$Cinsiyet, levels = c("Erkek", "KadD1n"))
tablo_final$Oncelik <- factor(tablo_final$Oncelik, levels = c("Rahatsiz_Degil", "Rahatsiz_Kararsiz"))


# M0: Tam BaDD1msD1zlD1k 
m0 <- glm(Freq ~ Cinsiyet + Sinif + Oncelik, family = poisson, data = tablo_final)
summary(m0)
1 - pchisq(summary(m0)$deviance, summary(m0)$df.residual)

# M1: KD1smi BaDD1msD1zlD1k 
m1 <- glm(Freq ~ Cinsiyet + Sinif * Oncelik, family = poisson, data = tablo_final)
summary(m1)
1 - pchisq(summary(m1)$deviance, summary(m1)$df.residual)

# M2: KD1smi BaDD1msD1zlD1k 
m2 <- glm(Freq ~ Sinif + Cinsiyet * Oncelik, family = poisson, data = tablo_final)
summary(m2)
1 - pchisq(summary(m2)$deviance, summary(m2)$df.residual)

# M3: KD1smi BaDD1msD1zlD1k 
m3 <- glm(Freq ~ Oncelik + Cinsiyet * Sinif, family = poisson, data = tablo_final)
summary(m3)
1 - pchisq(summary(m3)$deviance, summary(m3)$df.residual)


# M4: KoEullu BaDD1msD1zlD1k 
m4 <- glm(Freq ~ Cinsiyet * Sinif + Sinif * Oncelik, family = poisson, data = tablo_final)
summary(m4)
1 - pchisq(summary(m4)$deviance, summary(m4)$df.residual)

# M5: KoEullu BaDD1msD1zlD1k 
m5 <- glm(Freq ~ Cinsiyet * Sinif + Cinsiyet * Oncelik, family = poisson, data = tablo_final)
summary(m5)
1 - pchisq(summary(m5)$deviance, summary(m5)$df.residual)

# M6: KoEullu BaDD1msD1zlD1k
m6 <- glm(Freq ~ Cinsiyet * Oncelik + Sinif * Oncelik, family = poisson, data = tablo_final)
summary(m6)
1 - pchisq(summary(m6)$deviance, summary(m6)$df.residual)

# M7: KarED1lD1klD1 BaDD1msD1zlD1k 
m7 <- glm(Freq ~ (Cinsiyet + Sinif + Oncelik)^2, family = poisson, data = tablo_final)
summary(m7)
1 - pchisq(summary(m7)$deviance, summary(m7)$df.residual)



#-------------------------
# G SECENEDD0 - SC<tun Etki
#-------------------------
# g :  BelirlediDiniz bir ordinal ve bir nominal deDiEken iC'in SatD1r Etki ya da SC<tun Etki modeli C'C6zC<mlemesi yapD1nD1z.

if(!require(gnm)) install.packages("gnm")
library(gnm)
library(readxl)

veri <- read_excel("yanitlar.xlsx")

colnames(veri)[2]  <- "Cinsiyet"
colnames(veri)[14] <- "Dijitallesme"

# Cinsiyet filtresi
veri_g <- subset(veri, Cinsiyet %in% c("Kad\u0131n", "Erkek"))

# Fakt??r tan??mlar?? ??? unicode escape ile encoding sorunu yok
veri_g$Cinsiyet <- factor(veri_g$Cinsiyet, levels = c("Erkek", "Kad\u0131n"))

veri_g$Dijitallesme <- factor(veri_g$Dijitallesme,
                              levels = c(
                                "Kesinlikle Kat\u0131lm\u0131yorum",
                                "Kat\u0131lm\u0131yorum",
                                "Karars\u0131z\u0131m",
                                "Kat\u0131l\u0131yorum",
                                "Kesinlikle Kat\u0131l\u0131yorum"
                              ))

tablo_g <- table(veri_g$Cinsiyet, veri_g$Dijitallesme)

cat("\n--- HUCRE FREKANSLARI (Cinsiyet vs Dijitallesme) ---\n")
print(tablo_g)

df_g <- as.data.frame(tablo_g)
colnames(df_g) <- c("Cinsiyet", "Dijitallesme", "Freq")
df_g$Sutun_Skor <- as.numeric(df_g$Dijitallesme)

# S??tun Etki Modeli
sutun_etki <- gnm(Freq ~ Cinsiyet + Dijitallesme + Cinsiyet:Sutun_Skor,
                  family = poisson, data = df_g)

cat("\n==================================================================\n")
cat("SUTUN ETKI MODELI ANALIZ SONUCLARI (g Sikki)\n")
cat("==================================================================\n")
print(summary(sutun_etki))

# Uyum iyili??i
cat("\n--- MODEL UYUM IYILIGI (Deviance Testi) ---\n")
s_sapma      <- sutun_etki$deviance
df_serbest   <- sutun_etki$df.residual
p_uyum       <- 1 - pchisq(s_sapma, df_serbest)

cat("Uyumsuzluk Sapmasi (Deviance):", round(s_sapma, 4), "\n")
cat("Serbestlik Derecesi (df)     :", df_serbest, "\n")
cat("Modelin P-Degeri (p-value)   :", round(p_uyum, 5), "\n")




