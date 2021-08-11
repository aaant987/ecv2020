
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(datapasta)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library("FactoMineR")
library("factoextra")
library("corrplot")
library("PerformanceAnalytics")
library(ggrepel)
library(ggthemes)
library("ggthemr")
packs <- c("png","grid")
lapply(packs, require, character.only = TRUE) 
library(png)
library(hrbrthemes)

personas <- data.frame(
    stringsAsFactors = FALSE,
                            ccaa = c("Total Nacional",
                                     "Andalucía",
                                     "Aragón",
                                     "Asturias",
                                     "Baleares",
                                     "Canarias",
                                     "Cantabria",
                                     "C. y León",
                                     "C. La Mancha",
                                     "Cataluña",
                                     "C. Valenciana",
                                     "Extremadura",
                                     "Galicia",
                                     "Madrid",
                                     "Murcia",
                                     "Navarra",
                                     "País Vasco",
                                     "La Rioja",
                                     "Ceuta",
                                     "Melilla"),
                      vacaciones = c(34.4,45.4,23.6,32.1,31.7,47.4,36.7,28.2,
                                     32.5,30.6,36.1,45,37.4,26.6,45.3,
                                     21.1,18,23.9,49.1,39.2),
                          comida = c(5.4,
                                     7.2,1.2,1.8,7.2,7.8,3.6,2.6,3.1,
                                     2.4,7.2,5.8,14.9,4,8.1,4.4,2.7,0.2,
                                     2.1,10.3),
                     temperatura = c(10.9,11.3,2.8,7.8,19.9,17.5,5.8,6.6,
                                     9.6,9.4,13.6,13.7,9.6,11.5,13.4,10.3,
                                     7.6,6,2.9,18.9),
              gastos_imprevistos = c(35.4,43.9,26,27.6,36.6,63.1,31.5,24.7,
                                     33.1,33.4,36.8,46.8,28.1,30.1,43.1,
                                     20.1,19,24.5,55.9,52.3),
                   retrasos_pago = c(12.2,17,4.9,11.4,12.4,21.6,5.4,6.3,7.3,
                                     12.4,13.1,12.7,6.2,11,13.8,10.4,
                                     8.2,8.2,28.2,19.8),
                       automóvil = c(4.9,
                                     4.4,2.7,3.9,2.8,10.5,2.5,3,1.9,
                                     6.9,6.6,3.2,1.3,5.2,5,2.7,4.2,1.6,7,
                                     9.3),
                       ordenador = c(6.5,
                                     8.3,4.2,5.7,7.7,9.7,4.6,5.4,5.9,
                                     5.2,8.7,7.7,5,5.3,10.2,3.2,2.8,3.9,
                                     14.6,21.9)
            )


rownames(personas) = personas$ccaa

personas
df <- data.frame(personas, row.names = 1)
df = as.data.frame(scale(df))
df




res.pca <- PCA(df,  graph = TRUE)






theme_unique_dark <- function (base_size = 12, base_family = "") {
  ret <- (theme_bw(base_size = base_size, base_family = base_family) +
            theme(text = element_text(colour = "white"),
                  title = element_text(color = "yellow", hjust = 0.5),
                  line = element_line(color = "white"),
                  rect = element_rect(fill = "black", color = "white"),
                  axis.ticks = element_line(color = "#969696"),
                  axis.title = element_text(color = "white"),
                  axis.text = element_text(color = "#eaeaea"),
                  axis.line = element_line(color = "#969696", linetype = 1),
                  legend.background = element_rect(fill = NULL, color = NULL),
                  legend.position = "bottom",
                  legend.key = element_rect(fill = NA, color = NA, linetype = 0),
                  strip.background = element_rect(fill=NA,colour=NA,size=NA,linetype = NULL),
                  strip.text = element_text(color="white",face="bold",vjust=.5,hjust=.5),
                  panel.background = element_rect(fill = "black", color = NULL),
                  panel.border = element_blank(),
                  panel.grid = element_line(color = "#252525"),
                  panel.grid.major = element_line(color = "#353535"),
                  panel.grid.minor = element_line(color = "#101010"),
                  plot.background = element_rect(fill = "black", colour = "black", linetype = 0)))
  ret
}
set.seed(457)
p <- fviz_pca_biplot(res.pca, col.var = "red", col.ind = "yellow", geom = c("point","text"), 
                    repel = T, addEllipses = F, labelsize = 2,
                     title = "Biplot de personas con carencia material por comunidades autónomas",
                     subtitle = "",
                     caption = "") +
  theme(plot.title = element_text(hjust = 0.5, color = "yellow")) +
  theme(plot.subtitle = element_text(hjust = 0.5, color = "grey10")) +
  theme_modern_rc()+
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 10))

p

#----barras+lineas pobreza y renta

data <- data.frame(
                                                                                    
                                                                          ccaa = c("Total Nacional",
                                                                                   "Andalucía",
                                                                                   "Aragón",
                                                                                   "Asturias",
                                                                                   "Baleares",
                                                                                   "Canarias",
                                                                                   "Cantabria",
                                                                                   "C. y León",
                                                                                   "C. La Mancha",
                                                                                   "Cataluña",
                                                                                   "C. Valenciana",
                                                                                   "Extremadura",
                                                                                   "Galicia",
                                                                                   "Madrid",
                                                                                   "Murcia",
                                                                                   "Navarra",
                                                                                   "País Vasco",
                                                                                   "La Rioja",
                                                                                   "Ceuta",
                                                                                   "Melilla"),
                   riesgo = c(21,28.5,16,
                                                                                   22.2,14.1,29.9,18,
                                                                                   15.1,25.1,16.7,
                                                                                   24.6,31.4,22.1,
                                                                                   15.4,25,9.9,10,15,
                                                                                   35.3,36.3),
                                                  renta= c(12292,9990,
                                                                                   13097,12786,
                                                                                   12658,9935,12748,
                                                                                   12697,10.485,1417,
                                                                                   11332,9147,
                                                                                   11469,1458,985,
                                                                                   15094,15813,13504,
                                                                                   9853,11427),
                   carencia_material_severa = c(7,8,1.7,4.5,6.9,10.7,4.4,3.6,3.1,
                                                6.2,11.5,8.8,5.2,6.9,7.6,6.2,5,
                                                2.3,6.4,13.9)
                 )


data <- data %>% 
  mutate(ccaa = fct_reorder(ccaa, -riesgo))






cor.test(data$riesgo, data$carencia_material_severa)

lmtest <- lm(data$carencia_material_severa ~ data$riesgo)
summary(lmtest)
 
p1 <- ggplot(data, aes(x = riesgo, y = carencia_material_severa)) +
  geom_smooth(method = "lm", se = F, color = "green") +
  geom_point(size = 2.1, alpha = 1, color = "red") +
  scale_y_continuous(limits = c(0,15)) +
  scale_x_continuous(limits = c(0,40)) +
  geom_text_repel(aes(label = ccaa), color = "yellow", size = 2.1) +
  labs(x = "% Personas con carencia material severa", y = "Personas en riesgo de pobreza o exclusión social %") +
  labs(title = "Relación entre riesgo de pobreza y carencia material severa",
       subtitle = "",
       caption = "") +
  theme_modern_rc()+
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 10))
p1


library(ggpubr)
figure <- ggarrange(p, p1, ncol  = 2)
figure<- annotate_figure(figure,
                                top = text_grob("Encuesta de condiciones de vida",
                                                color = "white", face = "bold", size = 16),
                                bottom = text_grob("www.ine.es | @dataR_amateur",
                                                   hjust = 1, x = 1, face = "italic", size = 8,
                                                   color = "white")) +
  theme_modern_rc()
figure


residentes <- data.frame(
    stringsAsFactors = FALSE,
                        poblacion = c("Total","Española",
                                      "Extranjera (Unión Europea)","Extranjera (Resto del mundo)"),
          vacaciones = c(34.3, 32.5, 39.3, 57.9),
              comida = c(5.2, 5, 4.8, 9.7),
         temperatura = c(10.8, 9.8, 16.7, 23.5),
  gastos_imprevistos = c(34.8, 32, 48.8, 69.5),
       retrasos_pago = c(11.4, 9.5, 19.8, 35.6),
           automóvil = c(4.8, 3.6, 6.8, 21.6),
           ordenador = c(6, 4.6, 8.8, 25.6)
             )


ggplot(residentes, aes(x= vacaciones, y = poblacion, fill = poblacion)) +
  geom_bar(stat = "identity") +
  ggtitle("Black's 2nd Move") +
  coord_flip()




residentes1 <- data.frame(
  stringsAsFactors = FALSE,
                        nacionalidad = c("Total","Total","Total","Total","Total",
                                      "Total","Total","Española","Española",
                                      "Española","Española","Española",
                                      "Española","Española",
                                      "Extranjera (Unión Europea)","Extranjera (Unión Europea)",
                                      "Extranjera (Unión Europea)",
                                      "Extranjera (Unión Europea)","Extranjera (Unión Europea)",
                                      "Extranjera (Unión Europea)",
                                      "Extranjera (Unión Europea)",
                                      "Extranjera (Resto del mundo)","Extranjera (Resto del mundo)",
                                      "Extranjera (Resto del mundo)",
                                      "Extranjera (Resto del mundo)",
                                      "Extranjera (Resto del mundo)","Extranjera (Resto del mundo)",
                                      "Extranjera (Resto del mundo)"),
                         carencia = c("vacaciones","comida","temperatura",
                                      "gastos_imprevistos","retrasos_pago",
                                      "automóvil","ordenador","vacaciones","comida",
                                      "temperatura","gastos_imprevistos",
                                      "retrasos_pago","automóvil","ordenador",
                                      "vacaciones","comida","temperatura",
                                      "gastos_imprevistos","retrasos_pago",
                                      "automóvil","ordenador","vacaciones","comida",
                                      "temperatura","gastos_imprevistos",
                                      "retrasos_pago","automóvil","ordenador"),
                            valor = c(34.3,5.2,10.8,34.8,11.4,4.8,6,32.5,
                                      5,9.8,32,9.5,3.6,4.6,39.3,4.8,16.7,
                                      48.8,19.8,6.8,8.8,57.9,9.7,23.5,
                                      69.5,35.6,21.6,25.6)
               )



p3 <- ggplot(residentes1, aes(x= carencia, y = valor, fill = nacionalidad)) +
  geom_col(position = "dodge", colour = "black") +
  labs(x = "", y = "% de personas") +
  labs(title = "Personas de 16 y más años con carencia material por nacionalidad",
       subtitle = "",
       caption = "") +
scale_x_discrete(labels=c("gastos_imprevistos" = "gastos imprevistos", 
                          "retrasos_pago" = "retrasos pago")) +
scale_fill_discrete(name = "Nacionalidad") +
  theme_modern_rc()+
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 9, angle = 90))



figure <- ggarrange(p, p1, p3, nrow = 2, ncol = 2)
figure<- annotate_figure(figure,
                         top = text_grob("Encuesta de condiciones de vida",
                                         color = "white", face = "bold", size = 16),
                         bottom = text_grob("www.ine.es | @dataR_amateur",
                                            hjust = 1, x = 1, face = "italic", size = 8,
                                            color = "white")) +
  theme_modern_rc()
figure


figure <- ggarrange(
  p, p3,  p1,
  nrow = 1, common.legend = TRUE, legend="top")


figure<- annotate_figure(figure,
                         top = text_grob("Encuesta de condiciones de vida 2020",
                                         color = "white", face = "bold", size = 20),
                         bottom = text_grob("www.ine.es | @dataR_amateur",
                                            hjust = 1, x = 1, face = "italic", size = 8,
                                            color = "white")) +
  theme_modern_rc()
figure

figure + ggsave("ecv2020.png", width = 13, height = 8.5, dpi = 500)

