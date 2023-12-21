### Data and Package Import

library(pacman)
pacman:::p_load("tidyverse", "BSDA", "EnvStats", "DescTools", "gridExtra", "knitr", "nortest", "readxl", "reshape2", "gridGraphics", "grid")

medidas <- function(data){
  mean = mean(data)
  median = median(data)
  q1 = quantile(data, names=FALSE)[2]
  q3 = quantile(data, names=FALSE)[4]
  
  var = var(data)
  sd = sd(data)
  
  d1 = quantile(data, seq(0, 1, 0.1), names=FALSE)[2]
  d9 = quantile(data, seq(0, 1, 0.1), names=FALSE)[10]
  
  skew = 3*(mean - median)/sd
  kurt = (q3 - q1)/(2*(d9 - d1))
  
  cat("Média:", mean)
  cat("\nMediana:", median)
  cat("\n1º e 3º quartil:", q1, q3)
  cat("\nVariância e Desvio Padrão:", var, sd)
  cat("\nAssimetria:", skew)
  cat("\nCurtose:", kurt)
  

}


setwd(choose.dir())

amostra_200 <- readxl::read_xls("Amostra_g15_Matheus_Ramon.xls", 
                                col_types = c("numeric", "numeric", "text", 
                                              "text", "text", "text", "text", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric"))

# Sample

amostra50 <-  sample(amostra_200$ID, 50)

amostra50 <-   data.frame("ID" = amostra50)

amostra50 <- merge(amostra50, amostra_200, by= "ID")

write_rds(amostra50, "amostra50.xls")

amostra_50 <- read_rds("amostra_50.xls")


# Labels

amostra_50$TAM_MUN <- parse_character(amostra_50$TAM_MUN)

amostra_50$TAM_ESCOLA <- parse_character(amostra_50$TAM_ESCOLA)

amostra_200 <- amostra_200 %>% 
  mutate(LOCAL = fct_recode(LOCAL,
                            "Urbana" = "1",
                            "Rural" = "2"),TAM_MUN = fct_recode(TAM_MUN,
                                                                "20k ou menos" = "1",
                                                                "20k-50k" = "2",
                                                                "50k-100k" = "3",
                                                                "100k- 1m" = "4",
                                                                "1m ou mais" = "5"),ADM = fct_recode(ADM,
                                                                                                     "Estadual" = "2",
                                                                                                     "Municipal" = "3"),TAM_ESCOLA = fct_recode(TAM_ESCOLA,
                                                                                                                                                "25 ou menos" = "1",
                                                                                                                                                "25-49" = "2",
                                                                                                                                                "50-99" = "3",
                                                                                                                                                "100 ou mais" = "4"))

amostra_50 <- amostra_50 %>% 
  mutate(LOCAL = fct_recode(LOCAL,
                            "Urbana" = "1",
                            "Rural" = "2"),TAM_MUN = fct_recode(TAM_MUN,
                                                                "20k ou menos" = "1",
                                                                "20k-50k" = "2",
                                                                "50k-100k" = "3",
                                                                "100k-1m" = "4",
                                                                "1m ou mais" = "5"),ADM = fct_recode(ADM,
                                                                                                     "Estadual" = "2",
                                                                                                     "Municipal" = "3"),TAM_ESCOLA = fct_recode(TAM_ESCOLA,
                                                                                                                                                "25 ou menos" = "1",
                                                                                                                                                "25-49" = "2",
                                                                                                                                                "50-99" = "3",
                                                                                                                                                "100 ou mais" = "4"))

### Exploratory Analysis

# Region
a <- amostra_200 %>% 
  group_by(REG) %>% 
  mutate(Escolas = n(), Densidade = Escolas/200)

b <- amostra_50 %>% 
  group_by(REG) %>% 
  mutate(Escolas = n(), Densidade = Escolas/50)

x <- ggplot(a, aes(x = reorder(REG, Densidade, FUN =  median), y = Densidade, fill = REG))+
  geom_col(show.legend = F, position = "identity")+
  ylim(0, 0.5)+
  xlab("Região do País")+
  ylab("Proporção da Amostra")+
  theme_minimal()+
  ggtitle("Amostra 200")

y <- ggplot(b, aes(x = reorder(REG, Densidade, FUN =  median), y = Densidade, fill = REG))+
  geom_col(show.legend = F, position = "identity")+
  ylim(0, 0.5)+
  xlab("Região do País")+
  ylab("")+
  theme_minimal()+
  ggtitle("Amostra 50")

grid.arrange(x, y, nrow = 1)

t1 = a[c(1,8,12,16,35), c(3,12,13)]

t2 = b[c(14,2 ,29,35,50), c(3,12,13)]

kable(list(t1,t2), caption = "Amostra 200 | Amostra 50")


# Location

a <- amostra_200 %>%
  select(REG, LOCAL) %>% 
  group_by(LOCAL, REG) %>% 
  mutate(freq = length(REG)) %>% 
  unique()


b <- amostra_50 %>%
  select(REG, LOCAL) %>% 
  group_by(LOCAL, REG) %>% 
  mutate(freq = length(REG)) %>% 
  unique() %>% 
  select(LOCAL, freq)


x <- ggplot(a, aes(x ="", y = freq, fill = LOCAL))+ 
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+
  theme_void()+
  scale_fill_discrete(name = "Local da Escola", labels = c("Urbano", "Rural"))+
  ggtitle("Gráfico de Setores da Amostra 200")

y <- ggplot(b, aes(x ="", y = freq, fill = LOCAL))+ 
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+
  theme_void()+
  scale_fill_discrete(name = "Local da Escola", labels = c("Urbano", "Rural"))+
  ggtitle("Gráfico de Setores da Amostra 50")  


grid.arrange(x,y, nrow = 1)

c <- a %>% 
  group_by(LOCAL) %>% 
  mutate(Escolas = sum(freq), Densidade = Escolas / 200) %>% 
  unique()

d <- b %>% 
  group_by(LOCAL) %>% 
  mutate(Escolas = sum(freq), Densidade = Escolas / 50) %>% 
  unique()

t1 = c[1:2, c(2,4,5)]

t2 = d[c(4,1), c(2,4,5)]

kable(list(t1,t2), caption = "Amostra 200 | Amostra 50")


# Municipality Size

a <- amostra_200 %>% 
  group_by(TAM_MUN) %>% 
  mutate(Escolas = n(), Densidade = Escolas/200)

b <- amostra_50 %>% 
  group_by(TAM_MUN) %>% 
  mutate(Escolas = n(), Densidade = Escolas/50)

x <- ggplot(a, aes(x = reorder(TAM_MUN, Densidade, FUN = median), y = Densidade, fill = TAM_MUN))+
  geom_col(position = "identity", show.legend = F)+
  ylim(0, 0.3)+
  xlab("Tamanho do Município")+
  ylab("")+
  theme_minimal()+
  ggtitle("Amostra de 200")+
  coord_flip()


y <- ggplot(b, aes(x = reorder(TAM_MUN, Densidade, FUN = median), y = Densidade, fill = TAM_MUN))+
  geom_col(position = "identity", show.legend = F)+
  ylim(0, 0.3)+
  xlab("Tamanho do Município")+
  ylab("Proporção da Amostra")+
  theme_minimal()+
  ggtitle("Amostra de 50")+
  scale_fill_discrete(name = "Tamanho do Município", labels = c("<20000 hab", "20000 a 49999 hab", "50000 a 99999 hab", "100000 a 999999 hab", "1000000 ou mais"))+
  coord_flip()



grid.arrange(x,y, nrow = 2)

t1 = a[c(1,3,9,11,5), c(5,12,13)]

t2 = b[c(4,5,13,8,47), c(5,12,13)]

kable(list(t1,t2), caption = "Amostra 200 | Amostra 50")


# Administration Type

a <- amostra_200 %>%
  select(REG, ADM) %>%  
  group_by(ADM, REG) %>% 
  mutate(freq = length(REG)) %>% 
  unique()


b <- amostra_50 %>%
  select(REG, ADM) %>% 
  group_by(ADM, REG) %>% 
  mutate(freq = length(REG)) %>% 
  unique()



x <- ggplot(a, aes(x ="", y = freq, fill = ADM))+ 
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+
  theme_void()+
  scale_fill_discrete(name = "Administração da Escola")+
  ggtitle("Gráfico de Setores da Amostra 200")

y <- ggplot(b, aes(x ="", y = freq, fill = ADM))+ 
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+
  theme_void()+
  scale_fill_discrete(name = "Administração da Escola")+
  ggtitle("Gráfico de Setores da Amostra 50")  


grid.arrange(x,y, nrow = 1)

c <- a %>% 
  group_by(ADM) %>% 
  mutate(Escolas = sum(freq), Densidade = Escolas / 200) %>% 
  unique()

d <- b %>% 
  group_by(ADM) %>% 
  mutate(Escolas = sum(freq), Densidade = Escolas / 50) %>% 
  unique()

t1 = c[c(1,10), c(2,4,5)]

t2 = d[c(1,9), c(2,4,5)]

kable(list(t1,t2), caption = "Amostra 200 | Amostra 50")


# "School density"

a <- amostra_200 %>% 
  group_by(TAM_ESCOLA) %>% 
  mutate(Escolas = n(), Densidade = Escolas/200)

b <- amostra_50 %>% 
  group_by(TAM_ESCOLA) %>% 
  mutate(Escolas = n(), Densidade = Escolas/50)

x <- ggplot(a, aes(x = reorder(TAM_ESCOLA, Densidade, FUN = median), y = Densidade, fill = TAM_ESCOLA))+
  geom_col(position = "identity", show.legend = F)+
  ylim(0, 0.4)+
  xlab("Número de Estudantes")+
  ylab("")+
  theme_minimal()+
  ggtitle("Amostra de 200")+
  coord_flip()


y <- ggplot(b, aes(x = reorder(TAM_ESCOLA, Densidade, FUN = median), y = Densidade, fill = TAM_ESCOLA))+
  geom_col(position = "identity", show.legend = F)+
  ylim(0, 0.4)+
  xlab("Tamanho da Escola")+
  ylab("Proporção da Amostra")+
  theme_minimal()+
  ggtitle("Amostra de 50")+
  scale_fill_discrete(name = "Número de Estudantes")+
  coord_flip()



grid.arrange(x,y, nrow = 2)

t1 = a[c(5,1,9,32), c(7,12,13)]

t2 = b[c(4,5,8,16), c(7,12,13)]

kable(list(t1,t2), caption = "Amostra 200 | Amostra 50")


## Number of students

x <- ggplot(amostra_200, aes(x = MATRICULADOS, y = (..density..)*10))+
  geom_histogram(bins = 5, color = 'white', fill = 'black')+
  ylim(0, 0.13)+
  theme_minimal()+
  xlab("Número de Alunos")+
  ylab("Proporção da Amostra")+
  ggtitle("Amostra de 200")


y <- ggplot(amostra_50, aes(x = MATRICULADOS, y = (..density..)*10))+
  geom_histogram(bins = 5, color = 'white', fill = 'black')+
  ylim(0, 0.13)+
  theme_minimal()+
  xlab("Número de Alunos")+
  ylab("")+
  ggtitle("Amostra de 50")

grid.arrange(x,y, nrow = 1)

c <- table(cut(amostra_200$MATRICULADOS,breaks = seq(0,204,25)))

d <- table(cut(amostra_50$MATRICULADOS,breaks = seq(0,204,25)))

kable(list(c,d), caption = "Amostra 200 | Amostra 50")


# Samples Confusion Matrix

medidas(amostra_200$MATRICULADOS)

medidas(amostra_50$MATRICULADOS)


## Evasion percentage

x <- ggplot(amostra_200, aes(x = "", y = PARTICIPACAO))+
  geom_boxplot()+
  theme_minimal()+
  xlab("")+
  ylab("Alunos Participantes (%)")+
  ggtitle("Amostra de 200")

y <- ggplot(amostra_50, aes(x = "", y = PARTICIPACAO))+
  geom_boxplot()+
  theme_minimal()+
  xlab("")+
  ylab("")+
  ggtitle("Amostra de 50")

z <- ggplot(amostra_200, aes(x = PARTICIPACAO, y = (..density..)*10))+
  geom_histogram(binwidth = 10, color = 'white', fill = 'black')+
  ylim(0, 0.5)+
  theme_minimal()+
  xlab("Alunos Participantes (%)")+
  ylab("Proporção da Amostra")

w <- ggplot(amostra_50, aes(x = PARTICIPACAO, y = (..density..)*10))+
  geom_histogram(binwidth = 10, color = 'white', fill = 'black')+
  ylim(0, 0.5)+
  theme_minimal()+
  xlab("Alunos Participantes (%)")+
  ylab("")

grid.arrange(x,y,z,w, nrow = 2)

# Samples confusion matrix

medidas(amostra_200$PARTICIPACAO)

medidas(amostra_50$PARTICIPACAO)

c <- table(cut(amostra_200$PARTICIPACAO,breaks = seq(50,100,10)))

d <- table(cut(amostra_50$PARTICIPACAO,breaks = seq(50,100,10)))

kable(list(c,d), caption = "Amostra 200 | Amostra 50")


## Portuguese Grade

x <- ggplot(amostra_200, aes(x = "", y = NOTA_LP))+
  geom_boxplot()+
  ylim(130, 240)+
  theme_minimal()+
  xlab("")+
  ylab("Nota de Português")+
  ggtitle("Amostra de 200")

y <- ggplot(amostra_50, aes(x = "", y = NOTA_LP))+
  geom_boxplot()+
  ylim(130, 240)+
  theme_minimal()+
  xlab("")+
  ylab("")+
  ggtitle("Amostra de 50")

z <- ggplot(amostra_200, aes(x = NOTA_LP, y = (..density..)*10))+
  geom_histogram(bins = 12, color = 'white', fill = 'black')+
  xlim(130, 240)+
  ylim(0, 0.3)+
  theme_minimal()+
  xlab("Nota de Português")+
  ylab("Proporção da Amostra")

w <- ggplot(amostra_50, aes(x = NOTA_LP, y = (..density..)*10))+
  geom_histogram(bins = 12, color = 'white', fill = 'black')+
  xlim(130, 240)+
  ylim(0, 0.3)+
  theme_minimal()+
  xlab("Nota de Português")+
  ylab("")

grid.arrange(x,y,z,w, nrow = 2)

c <- table(cut(amostra_200$NOTA_LP,breaks = seq(140,240,25)))

d <- table(cut(amostra_50$NOTA_LP,breaks = seq(140,240,25)))

kable(list(c,d), caption = "Amostra 200 | Amostra 50")


medidas(amostra_200$NOTA_LP)

medidas(amostra_50$NOTA_LP)


## Math Grade

x <- ggplot(amostra_200, aes(x = "", y = NOTA_MT))+
  geom_boxplot()+
  ylim(150, 280)+
  theme_minimal()+
  xlab("")+
  ylab("Nota de Matemática")+
  ggtitle("Amostra de 200")

y <- ggplot(amostra_50, aes(x = "", y = NOTA_MT))+
  geom_boxplot()+
  ylim(150, 280)+
  theme_minimal()+
  xlab("")+
  ylab("")+
  ggtitle("Amostra de 50")

z <- ggplot(amostra_200, aes(x = NOTA_MT, y = (..density..)*10))+
  geom_histogram(bins = 12, color = 'white', fill = 'black')+
  xlim(150, 280)+
  ylim(0, 0.25)+
  theme_minimal()+
  xlab("Nota de Matemática")+
  ylab("Proporção da Amostra")

w <- ggplot(amostra_50, aes(x = NOTA_MT, y = (..density..)*10))+
  geom_histogram(bins = 12, color = 'white', fill = 'black')+
  xlim(150, 280)+
  ylim(0, 0.25)+
  theme_minimal()+
  xlab("Nota de Matemática")+
  ylab("")

grid.arrange(x,y,z,w, nrow = 2)

medidas(amostra_200$NOTA_MT)

medidas(amostra_50$NOTA_MT)

c <- table(cut(amostra_200$NOTA_MT,breaks = seq(150,280,25)))

d <- table(cut(amostra_50$NOTA_MT,breaks = seq(150,280,25)))

kable(list(c,d), caption = "Amostra 200 | Amostra 50")


### Estimatives

# Evasion

prop_200 <- amostra_200 %>% 
  filter(PARTICIPACAO < 75)

prop_50 <- amostra_50 %>% 
  filter(PARTICIPACAO < 75)

estimativa_200 <- BinomCI(length(prop_200$PARTICIPACAO), 200)


estimativa_50 <- BinomCI(length(prop_50$PARTICIPACAO), 50)


kable(list(estimativa_200, estimativa_50), caption = "Amostra 200 | Amostra 50")


# Portuguese Grade Mean
t.test(amostra_200$NOTA_LP, mu = 184.3, alternative = "greater")

t.test(amostra_50$NOTA_LP, mu = 184.3, alternative = "greater")

# Math Grade Mean
t.test(amostra_200$NOTA_MT, mu = 204.3, alternative = "greater")

t.test(amostra_50$NOTA_MT, mu = 204.3, alternative = "greater")


### Normality analysis

# Portuguese Grade
attach(amostra_200)
par(mfrow=c(1,2))
hist(amostra_200$NOTA_LP, main = "Distribuição das notas de LP", 
     xlab = "Notas", ylab = "Frequência")
qqnorm(amostra_200$NOTA_LP, main="Quantil-quantil da normal",
       xlab="Quantis teóricos da distribuição normal",
       ylab="Quantis observados da amostra")
qqline(amostra_200$NOTA_LP)

attach(amostra_50)
par(mfrow=c(1,2))
hist(amostra_50$NOTA_LP, main = "Distribuição das notas de LP", 
     xlab = "Notas", ylab = "Frequência")
qqnorm(amostra_50$NOTA_LP, main="Quantil-quantil da normal",
       xlab="Quantis teóricos da distribuição normal",
       ylab="Quantis observados da amostra")
qqline(amostra_50$NOTA_LP)


pearson.test(amostra_200$NOTA_LP) 
pearson.test(amostra_50$NOTA_LP)

# Math Grade
attach(amostra_200)
par(mfrow=c(1,2))
hist(amostra_200$NOTA_MT, main = "Distribuição das notas de MAT", 
     xlab = "Notas", ylab = "Frequência")
qqnorm(amostra_200$NOTA_MT, main="Quantil-quantil da normal",
       xlab="Quantis teóricos da distribuição normal",
       ylab="Quantis observados da amostra")
qqline(amostra_200$NOTA_MT)

attach(amostra_50)
par(mfrow=c(1,2))
hist(amostra_50$NOTA_MT, main = "Distribuição das notas de MT", 
     xlab = "Notas", ylab = "Frequência")
qqnorm(amostra_50$NOTA_MT, main="Quantil-quantil da normal",
       xlab="Quantis teóricos da distribuição normal",
       ylab="Quantis observados da amostra")
qqline(amostra_50$NOTA_MT)

pearson.test(amostra_200$NOTA_MT)

pearson.test(amostra_50$NOTA_MT)


## Multiple variable analysis

# Math Grade X Location

x <- ggplot(amostra_200, aes(LOCAL, NOTA_MT))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Amostra de 200")+
  xlab("Local")+
  ylab("Nota de Matemática")

y <- ggplot(amostra_50, aes(LOCAL, NOTA_MT))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Amostra de 50")+
  xlab("Local")+
  ylab("")

grid.arrange(x,y, nrow = 1)

rural <- amostra_200 %>% 
  filter(LOCAL == "Rural")

urbana <- amostra_200 %>% 
  filter(LOCAL == "Urbana")


var.test(rural$NOTA_MT,
         urbana$NOTA_MT)

rural_50 <- amostra_50 %>% 
  filter(LOCAL == "Rural")

urbana_50 <- amostra_50 %>% 
  filter(LOCAL == "Urbana")


var.test(rural_50$NOTA_MT,
         urbana_50$NOTA_MT)

t.test(rural$NOTA_MT, urbana$NOTA_MT, alternative = "less", var.equal = T)

t.test(rural_50$NOTA_MT, urbana_50$NOTA_MT, alternative = "less", var.equal = T)


# Portuguese Grade X Administration Type
x <- ggplot(amostra_200, aes(ADM, NOTA_LP))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Amostra de 200")+
  xlab("Administração")+
  ylab("Nota de Português")

y <- ggplot(amostra_50, aes(ADM, NOTA_LP))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Amostra de 50")+
  xlab("Administração")+
  ylab("")

grid.arrange(x,y, nrow = 1)

estadual <- amostra_200 %>% 
  filter(ADM == "Estadual")

municipal <- amostra_200 %>% 
  filter(ADM == "Municipal")

var.test(estadual$NOTA_LP,
         municipal$NOTA_LP)

estadual_50 <- amostra_50 %>% 
  filter(ADM == "Estadual")

municipal_50 <- amostra_50 %>% 
  filter(ADM == "Municipal")


var.test(estadual_50$NOTA_LP,
         municipal_50$NOTA_LP)

t.test(estadual$NOTA_LP, municipal$NOTA_LP, var.equal = T)

t.test(estadual_50$NOTA_LP, municipal_50$NOTA_LP, var.equal = T)


# Math Grade X Portuguese Grade difference
notas <- melt(amostra_200, 1:9, variable.name = "Disciplina", value.name = "Nota")

notas_50 <- melt(amostra_50, 1:9, variable.name = "Disciplina", value.name = "Nota")

x <- ggplot(notas)+
  geom_boxplot(aes(x = Disciplina,Nota))+
  theme_minimal()+
  ggtitle("Amostra 200")

y <- ggplot(notas_50)+
  geom_boxplot(aes(x = Disciplina,Nota))+
  theme_minimal()+
  ggtitle("Amostra 50")


grid.arrange(x,y, nrow = 1)

dif <- amostra_200 %>% 
  group_by(ID) %>% 
  mutate(d = NOTA_LP - NOTA_MT)

dif_50 <- amostra_50 %>% 
  group_by(ID) %>% 
  mutate(d = NOTA_LP - NOTA_MT)

t.test(dif$d, alternative = "less")

t.test(dif_50$d, alternative = "less")


# Evasion X Location

Prop_200 <- amostra_200 %>% 
  filter(PARTICIPACAO < 75) %>% 
  group_by(LOCAL) %>% 
  mutate(contagem = length(LOCAL)) %>% 
  select(LOCAL, contagem) %>% 
  unique()




Prop_50 <- amostra_50 %>% 
  filter(PARTICIPACAO < 75) %>% 
  group_by(LOCAL) %>% 
  mutate(contagem = length(LOCAL)) %>% 
  select(LOCAL, contagem) %>% 
  unique()


Local <- matrix(c(7,151,8,34), byrow=TRUE, 
                2, 2, dimnames=list(c("Urbana", "Rural"), c("Sim","Não")))

Local_50 <- matrix(c(4,38,2,6), byrow=TRUE, 
                   2, 2, dimnames=list(c("Urbana", "Rural"), c("Sim","Não")))

kable(list(Local,Local_50), caption = "Amostra 200 | Amostra 50")

chisq.test(Local)
chisq.test(Local_50)


# Evasion X Region
Prop_200r <- amostra_200 %>% 
  filter(PARTICIPACAO < 75) %>% 
  group_by(REG) %>% 
  mutate(contagem = length(REG)) %>% 
  select(REG, contagem) %>% 
  unique()



Prop_50r <- amostra_50 %>% 
  filter(PARTICIPACAO < 75) %>% 
  group_by(REG) %>% 
  mutate(contagem = length(REG)) %>% 
  select(REG, contagem) %>% 
  unique()



Regiao <- matrix(c(2,15, 2, 21,7, 53, 1, 21,3, 75), byrow=TRUE, 
                 5, 2, dimnames=list(c("CO","N", "NE","S", "SE"), c("Sim","Não")))


Regiao_50 <- matrix(c(1,5, 1, 4,2, 12, 0, 2,2, 21), byrow=TRUE, 
                    5, 2, dimnames=list(c("CO","N", "NE","S", "SE"), c("Sim","Não")))

kable(list(Regiao,Regiao_50), caption = "Amostra 200 | Amostra 50")


chisq.test(Regiao)
chisq.test(Regiao_50)


# Region X Administration Type

Prop_200 <- amostra_200 %>% 
  group_by(ADM, REG) %>% 
  mutate(contagem = length(REG)) %>% 
  select(REG, contagem) %>% 
  unique()

Prop_50 <- amostra_50 %>% 
  group_by(ADM, REG) %>% 
  mutate(contagem = length(REG)) %>% 
  select(REG, contagem) %>% 
  unique()

a <- matrix(c(6,11, 4, 19,5, 55, 6, 16,19, 59), byrow=TRUE, 
            5, 2, dimnames=list(c("CO","N", "NE","S", "SE"), c("Estadual","Municipal")))

b <- matrix(c(0,6, 1, 4,2, 12, 1, 1,6, 17), byrow=TRUE, 
            5, 2, dimnames=list(c("CO","N", "NE","S", "SE"), c("Estadual","Municipal")))


kable(list(a,b), caption = "Amostra 200 | Amostra 50")

chisq.test(a)
chisq.test(b)


# School Size X Municipality
Prop_200 <- amostra_200 %>% 
  group_by(TAM_MUN, TAM_ESCOLA) %>% 
  mutate(contagem = length(TAM_MUN)) %>% 
  select(contagem) %>% 
  unique()

Prop_50 <- amostra_50 %>% 
  group_by(TAM_MUN, TAM_ESCOLA) %>% 
  mutate(contagem = length(TAM_MUN)) %>% 
  select(contagem) %>% 
  unique()

a <- matrix(c(10,18, 11, 1,12, 22, 17, 5,4, 20,18,7,1,9,15,6,1,1,12,10), byrow=TRUE, 
            5, 4, dimnames=list(c("20k ou menos","20k-50k", "50k-100k","100k- 1m", "1m ou mais"), c("25 ou menos","25-49", "50-99", "100 ou mais")))

b <- matrix(c(4,4, 2, 1,3, 4, 1, 0,1, 5,2,2,1,4,6,2,0,1,2,5), byrow=TRUE, 
            5, 4, dimnames=list(c("20k ou menos","20k-50k", "50k-100k","100k- 1m", "1m ou mais"), c("25 ou menos","25-49", "50-99", "100 ou mais")))


kable(list(a,b), caption = "Amostra 200 | Amostra 50")

chisq.test(a)
chisq.test(b)


# Math Grade X Portuguese Grade correlation
x <- ggplot(amostra_200)+
  geom_point(aes(NOTA_LP, NOTA_MT))+
  theme_minimal()+
  ggtitle("Amostra 200")+
  xlab("Nota de Português")+
  ylab("Nota de Matemática")

y <- ggplot(amostra_50)+
  geom_point(aes(NOTA_LP, NOTA_MT))+
  theme_minimal()+
  ggtitle("Amostra 50")+
  xlab("Nota de Português")+
  ylab("")


grid.arrange(x,y, nrow = 1)

cor.test(amostra_200$NOTA_LP,amostra_200$NOTA_MT, alternative = "greater")

cor.test(amostra_50$NOTA_LP,amostra_50$NOTA_MT, alternative = "greater")


### Linear Regression Model
resmodelo <- lm(amostra_200$NOTA_LP~amostra_200$NOTA_MT)

summary(resmodelo)

resmodelo2 <- lm(amostra_50$NOTA_LP~amostra_50$NOTA_MT)

summary(resmodelo2)

par(mfrow=c(1,2))
plot(amostra_200$NOTA_LP~amostra_200$NOTA_MT, main = "Amostra 200", xlab = "Nota de Matemática", ylab = "Nota de Português")
abline(resmodelo,lty=2)
plot(amostra_50$NOTA_LP~amostra_50$NOTA_MT, main = "Amostra 50", xlab = "Nota de Matemática", ylab = "")
abline(resmodelo,lty=2)