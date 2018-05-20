# Source file encoding: Windows cp-1251.
#
# Reading source file with RStudio:
# 1) File - Open File...
# 2) File - Reopen with Encodinig... - Show all encodings - Windows cp-1251.
# 3) Set Cyrillic locale http://blog.rolffredheim.com/2013/01/r-and-foreign-characters.html
Sys.setlocale("LC_CTYPE", "russian")


# Загрузка пакета для чтения данных из xlsx.
# Требуется: Java. For 64-bit Windows and R, manually download and install 64-bit Java.
#            Default Java installer sets up a 32-bit Java.
#            https://www.r-statistics.com/2012/08/how-to-load-the-rjava-package-after-the-error-java_home-cannot-be-determined-from-the-registry/
if (!('xlsx' %in% installed.packages()[,"Package"])) {
  install.packages("xlsx", dependencies = TRUE)
}
require(xlsx);


# Чтение данных
my_dir='d:/Galya/Wilson/data';
filename <- "Wilson_FamilyID.xlsx"
data <- read.xlsx(file=paste(my_dir,filename,sep="/"), sheetName="Sheet1", endRow=86, header=TRUE)

# Размер таблицы
c(nrow(data), ncol(data))
# 85 26

# Установим имена колонок
datnames = c(
  "FIO", "Family", "FamilyID",          # FamilyID - номер семьи
  "Sex", "Height", "Mass", "BMI",       # Пол, рост, масса, BMI
  "Target",                             # Форма болезни: 1 абдоминальная, 2 неврологическая, 3 смешанная
  "TargetRelativeMax",                  # Наихудшая форма болезни у родственников
  "DebutOrgan", "DebutAge",             # Дебют болезни: поражённые органы и возраст больного
  "Cirrhosis", "ChildPugh", "Advanced", # Стадия цирроза
  "Activity",                           # Активность ЦП (прогрессирование)
  "KKF",                                # Кольцо Кайзера-Флейшера 
  "F2", "F5", "F7", "F13", "ITGA2",     # Результаты генетических анализов
  "ITGB3", "PAI_1", "FGB",
  "MTHFR_677", "MTHFR_1298"
  );
names(data) = datnames


str(data)
# Опечатка в колонке ITGA2: 4 разных значения
# $ F13              : Factor w/ 3 levels "GG","GT","TT": 1 1 1 2 2 1 3 1 3 1 ...
# $ ITGA2            : Factor w/ 4 levels "CC","CT", ...: 2 1 1 2 4 2 1 2 4 2 ...
# $ ITGB3            : Factor w/ 3 levels "CC","TC","TT": 3 3 3 2 2 3 3 3 3 3 ...

# Опечатка в колонке data[,"ITGA2"]: русские символы вместо латинских
data$ITGA2[ ! data$ITGA2 %in% c('CC', 'CT', 'TT') ] = 'CT'
data[,"ITGA2"] = as.factor(as.character(data[,"ITGA2"]))
str(data[,"ITGA2"])
# Factor w/ 3 levels "CC","CT","TT": 2 1 1 2 3 2 1 2 3 2 ...


# Прямая замена в RStudio -- трудности с кодировкой.
# Проще поправить вручную в Excel.
# data[ (iconv( data[,"ITGA2"], "UTF-8", "CP1251" ) == "СТ"), "ITGA2"] = 'CT'
# data[ (iconv( data[,"ITGA2"], "UTF-8", "CP1251" ) == "NO"), "ITGA2"] = 'CT'
# data[,"ITGA2"] = as.factor(as.character(data[,"ITGA2"]))
# str(data[,"ITGA2"])
# Factor w/ 3 levels "CC","CT","TT": 2 1 1 2 3 2 1 2 3 2 ...



# Замена пустых строк на NA
for (n in names(data)) {
  if(is.factor(data[,n])) {
    data[,n] <- as.character(data[,n])    
    data[data[,n] == "",n] = NA
    data[,n] <- as.factor(data[,n])    
  }
}

# Body mass index - индекс массы тела
# Замена BMI = 0.0 на NA
data[8, "BMI"]
data[data[,"BMI"] < 0.01, "BMI"] = NA
data[8, "BMI"]
data[is.na(data[,"Mass"]), ]
data[is.na(data[,"Mass"]), "BMI"]
#  NA 21 24


# Удаляем пациента с формой заболевания Target = 2: 
# поражена голова, а печень здорова
data = data[ data[,"Target"] !=2, ]


# У одной пацинтки пропущен пол. Других пропусков пола нет.
# Вставляем пропущенный пол.
data[is.na(data[,"Sex"]), "Sex"] = 2

# patient_name = "Женская_Фамилия"     # Отредактировано
# data[ grepl(patient_name, data[,"FIO"]), "Sex" ] = 2
# data[,"Sex"]


# Создаём колонки для дебюта заболевания.
# Колонка "DebutOrgan" - первого поражённого органа. Значения: 1-9.
#    1. печеночная патология, 
#    2. неврологич, 
#    3. почечная, 
#    4. эндокринная, 
#    5. сибсы (siblings - братья и сёстры), 
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
# Целевой показатель: "TargetHead".
# Хотим предсказывать неврологические осложнения -- смешанную форму 
# (печень и неврология) болезни Вильсона-Коновалова: Target == 3.
# ----------------------------------------------------------------------
# Diagnosis that we want to predict: mixed form
# (liver and neurological) of Wilson desease: Target == 3.
# ----------------------------------------------------------------------
unique(data[,"Target"]);
data[,"TargetHead"] = as.integer(data[,"Target"] == 3);


# ----------------------------------------------------------------------
# Список колонок для анализа
names_cleaned = names(data)
columns_1st = c("Target","FamilyID", "TargetRelativeMax")
names_cleaned = c( columns_1st, names_cleaned[!names_cleaned %in% columns_1st] )
names_anonymized = names_cleaned[!names_cleaned %in% c("FIO","Family")]

paste(names(data), sep = '', collapse = ', ')
paste(names_cleaned, sep = '', collapse = ', ')
paste(names_anonymized, sep = '', collapse = ', ')


# Имена и фамилии пациентов
data[,"FIO"] = as.character(data[,"FIO"])
data[,"FIO"] = iconv(data[,"FIO"], from="UTF-8", "cp1251")
data[,"Family"] = as.character(data[,"Family"])
data[,"Family"] = iconv(data[,"Family"], from="UTF-8", "cp1251")

head(data, 2)


# ----------------------------------------------------------------------
# Save cleaned data
#?write.table
# Проблемы с кодировкой

csv_dir = 'd:/Galya/Wilson/data_csv'

# Настройка кодировки: "CP1252"
write.table( data[,names_cleaned], 
             file = paste(csv_dir, paste("Wilson_cleaned.", "csv", sep=""),sep="/"), 
             col.names = TRUE, row.names = FALSE, 
             fileEncoding="CP1251", 
             quote = FALSE
             , sep = ";"
)


#write.table( data[,names_cleaned], 
#             file = paste(csv_dir, paste("Wilson_cleaned.utf-8.", "csv", sep=""),sep="/"), 
#             col.names = TRUE, row.names = FALSE, fileEncoding="utf-8", 
#             quote = FALSE
#             , sep = ";"
#)


warnings()
write.table( data[,names_anonymized], 
             file = paste(csv_dir, paste("Wilson_anonym.", "csv", sep=""),sep="/"), 
             col.names = TRUE, row.names = FALSE, fileEncoding="CP1251", 
             quote = FALSE
             , sep = ";"
)
# ----------------------------------------------------------------------
