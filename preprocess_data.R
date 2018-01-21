# Чтение файла
require(xlsx);
my_dir='d:/Galya/Wilson';
filename <- paste("Wilson_FamilyID", "xlsx", sep=".")
filename <- "Wilson_FamilyID.xlsx"
print(filename)

data <- read.xlsx(file=paste(my_dir,filename,sep="/"), sheetName="Sheet1", endRow=86, header=TRUE)

# Удаление ошибочно прочитанных данных
# Удаляем лишние строки и колонки
#data = head(data,85)
#data = data[,1:24]

c(nrow(data), ncol(data))

# Имена колонок
datnames = c(
  "FIO", "Family", "FamilyID",  
  "Sex", "Height", "Mass", "BMI", 
  "Target",                            # Форма болезни: 1 абдомин, 2 неврологич, 3 смешанная
  "TargetRelativeMax",                 # Наихудшая форма болезни у родственников
  "DebutOrgan", "DebutAge", 
  "Cirrhosis", "ChildPugh", "Advanced", 
  "Activity", 
  "KKF", 
  "F2", "F5", "F7", "F13", "ITGA2", "ITGB3", "PAI_1", "FGB", "MTHFR_677", "MTHFR_1298");

names(data) = datnames

# Имена и фамилии пациентов
data[,"FIO"] = as.character(data[,"FIO"])
data[,"FIO"] = iconv(data[,"FIO"], from="UTF-8", "cp1251")
data[,"Family"] = as.character(data[,"Family"])
data[,"Family"] = iconv(data[,"Family"], from="UTF-8", "cp1251")

head(data, 2)

# Замена пустых строковых занченйи на NA
for (n in names(data)) {
  if(is.factor(data[,n])) {
    data[,n] <- as.character(data[,n])    
    data[data[,n] == "",n] = NA
    data[,n] <- as.factor(data[,n])    
  }
}

# Body mass index - индекс массы тела
# Замена BMI = 0.0 на NA
data[data[,"BMI"] < 0.01, "BMI"] = NA

data[8, "BMI"]
data[is.na(data[,"Mass"]), ]
data[is.na(data[,"Mass"]), "BMI"]
#  NA 21 24


# Опечатка в данных: русские символы вместо латинских
data[ (iconv( data[,"ITGA2"], "UTF-8", "CP1251" ) == "СТ"), "ITGA2"] = 'CT'
data[,"ITGA2"] = as.factor(as.character(data[,"ITGA2"]))

data

# Удаляем пациента с формой заболевания Target = 2: поражена голова, а печень здорова
data = data[ data[,"Target"] !=2, ]

# Вставляем пропущенный пол пациентки
data[ grepl("Женская_Фамилия", data[,"FIO"]), "Sex" ] = 2
data[,"Sex"]


# Создаём колонки для дебюта заболевания.
# Колонка "DebutOrgan" - первого поражённого органа. Значения: 1-9.
#    1. печеночная патология, 
#    2. неврологич, 
#    3. почечная, 
#    4. эндокринная, 
#    5. сибсы, 
#    6. васкулит, 
#    7. гемолитич. анемия, 
#    8. разрыв селезенки.
#    9. обследование по др.патологии
# Заменяем её на бинарные признаки - дебют заболевания: 

data[,"DebutOrgan"] = as.character(data[,"DebutOrgan"])

data[,"DebutLiver"]   = as.integer(grepl("1", data[,"DebutOrgan"]))
data[,"DebutNeuro"]   = as.integer(grepl("2", data[,"DebutOrgan"]))
data[,"DebutKidney"]  = as.integer(grepl("3", data[,"DebutOrgan"]))
data[,"DebutEndocr"]  = as.integer(grepl("4", data[,"DebutOrgan"]))
data[,"DebutSibs"]    = as.integer(grepl("5", data[,"DebutOrgan"]))
data[,"DebutVasku"]   = as.integer(grepl("6", data[,"DebutOrgan"]))
data[,"DebutGemAnem"] = as.integer(grepl("7", data[,"DebutOrgan"]))
data[,"DebutSelez"]   = as.integer(grepl("8", data[,"DebutOrgan"]))
data[,"DebutOther"]   = as.integer(grepl("9", data[,"DebutOrgan"]))

data = data[,names(data) != "DebutOrgan"]

# ----------------------------------------------------------------------
# Хотим предсказывать неврологические осложнения.
# Diagnosis that we want to predict: Target == 3. - Illness in head.
# ----------------------------------------------------------------------
unique(data[,"Target"])
data[,"TargetHead"] = as.integer(data[,"Target"] == 3)


# ----------------------------------------------------------------------
# Список колонок для анализа
names_cleaned = names(data)
columns_1st = c("Target","FamilyID", "TargetRelativeMax")
names_cleaned = c( columns_1st, names_cleaned[!names_cleaned %in% columns_1st] )
names_anonymized = names_cleaned[!names_cleaned %in% c("FIO","Family")]

paste(names(data), sep = '', collapse = ', ')

# ----------------------------------------------------------------------
# Save cleaned data
#?write.table
write.table( data[,names_cleaned], 
             file = paste(my_dir,paste("Wilson_cleaned.", "csv", sep=""),sep="/"), 
             col.names = TRUE, row.names = FALSE, fileEncoding="CP1251", 
             quote = FALSE
             , sep = ";"
)

write.table( data[,names_anonymized], 
             file = paste(my_dir,paste("Wilson_anonym.", "csv", sep=""),sep="/"), 
             col.names = TRUE, row.names = FALSE, fileEncoding="CP1251", 
             quote = FALSE
             , sep = ";"
)
# ----------------------------------------------------------------------

names_anonymized

# Первая модель
model_1 = lm(TargetHead ~ Sex + Advanced + Activity + KKF + F2 + F5 + F7 + F13 + ITGA2 + ITGB3 + PAI_1 + FGB + MTHFR_677 + MTHFR_1298, data=data)
summary(model_1)

