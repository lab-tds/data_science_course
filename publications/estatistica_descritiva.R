# Código base para a publicação "Metodologia científica utilizando o R: estatística descritiva".
# Autores: Alek Fröhlich, André Bernardes Turcato, Daniel Guimarães Tiezzi.


# instalar o pacote para acesso ao banco de dados
install.packages('medicaldata')
# carregar o banco de dados
data <- medicaldata::blood_storage
# verifica a classe do objeto
class(data)
# [1] "data.frame"
# verifica as dimensões do objeto
dim(data)
# [1] 316  20

# visualizar parte da tabela (4 linhas e 4 colunas)
data[1:4, 1:4]
#   RBC.Age.Group Median.RBC.Age  Age AA
# 1             3             25 72.1  0
# 2             3             25 73.6  0
# 3             3             25 67.5  0
# 4             2             15 65.8  0

# Seleção de variáveis de interesse
data <- data[, c('Age', 'PVol', 'T.Stage', 'PreopPSA', 'Recurrence')]
# visualizar os dados
head(data)
#    Age  PVol T.Stage PreopPSA Recurrence
# 1 72.1  54.0       1    14.08          1
# 2 73.6  43.2       2    10.50          1
# 3 67.5 102.7       1     6.98          0
# 4 65.8  46.0       1     4.40          0
# 5 63.2  60.0       1    21.40          0
# 6 65.4  45.9       1     5.10          0
# Definir o tipo de dado
data$Age <- as.numeric(data$Age)
data$PVol <- as.numeric(data$PVol)
data$T.Stage <- as.factor(data$T.Stage)
data$PreopPSA <- as.numeric(data$PreopPSA)
data$Recurrence <- as.factor(data$Recurrence)
# Pré processamento dos dados com remoção de dados faltantes
any(is.na(data))
# [1] TRUE
data <- data[complete.cases(data), ]
dim(data)
# [1] 293   5

# Contagem de casos por categoria
table(data$T.Stage)
#   1   2
# 260  33
table(data$Recurrence)
#   0   1
# 243  50
# Contagem cruzada por categoria - tabela de contingência
table(data$T.Stage, data$Recurrence)
#       0   1
#   1 224  36
#   2  19  14

# Frequências relativas em porcentagem
round(prop.table(table(data$T.Stage))*100,1)
#    1    2
# 88.7 11.3
round(prop.table(table(data$Recurrence))*100,1)
#    0    1
# 82.9 17.1
round(prop.table(table(data$T.Stage, data$Recurrence))*100,1)
#        0    1
#   1 76.5 12.3
#   2  6.5  4.8

# Medidas de tendência central e de dispersão
summary(data$Age)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   38.40   56.00   61.80   60.97   66.10   79.00
sd(data$Age)
# [1] 7.285216
IQR(data$Age)
# [1] 10.1
summary(data$PVol)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   19.40   41.00   48.50   56.06   64.00  274.00
sd(data$PVol)
# [1] 29.39968
IQR(data$PVol)
# [1] 23
summary(data$PreopPSA)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   1.300   5.000   6.200   8.359   9.100  40.100
sd(data$PreopPSA)
# [1] 6.153984
IQR(data$PreopPSA)
# [1] 4.1

# Histogramas de frequência
png('histogramas.png', height = 5, width = 18, units = 'in', res = 300)
par(mfrow=c(1,3))
hist(data$Age)
hist(data$PVol)
hist(data$PreopPSA)
dev.off()

png('barras.png', height = 5, width = 13, units = 'in', res = 300)
par(mfrow=c(1,2), mar = c(5,2,2,1))
barplot(table(data$Recurrence, data$T.Stage), col = c('navy', 'firebrick'))
par(mar = c(5,1,2,5), xpd=TRUE)
barplot(prop.table(table(data$Recurrence,data$T.Stage),2), col = c('navy', 'firebrick'))
legend('right', legend = c('Não','Sim'), fill = c('navy', 'firebrick') , inset=c(-0.17,0), bty='n', title= 'Recorrência')
dev.off()

png('dispersao.png', height = 5, width = 10, units = 'in', res = 300)
par(mfrow=c(1,2))
plot(data$PVol ~ data$Age, pch = 19, ylab = 'Volume', xlab = 'Idade')
abline(lm(data$PVol ~ data$Age), col = 'red', lty = 2, lwd = 2)
plot(data$PVol ~ data$PreopPSA, pch = 19, ylab = 'Volume', xlab = 'PSA')
abline(lm(data$PVol ~ data$PreopPSA), col = 'red', lty = 2, lwd = 2)
dev.off()

png('boxplot.png', height = 5, width = 10, units = 'in', res = 300)
par(mfrow=c(1,2))
boxplot(data$PreopPSA ~ data$Recurrence, ylab = 'PSA', xlab = 'Recorrência')
boxplot(data$PVol ~ data$Recurrence, ylab = 'Volume', xlab = 'Recorrência')
dev.off()

