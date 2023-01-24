library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

#Life2018-2021 <- 82
#El Moro 22 <- 3
#Cabanes 22 <- 1
#Life 2022 <- 0
#Catalunya <- 11
#total without removing 0 and NAs = 97


# El Moro -----------------------------------------------------------------

elmoro22 <- read_excel("~/R/Github/Paludicola/Data/El Moro 22.xlsx")

#Filtering our specie of study: Acrocephalus paludicola, creating a new df
paluelmoro22 <- filter(elmoro22, Especie == "Acrocephalus paludicola")

#Selecting the columns we need
paluelmoro22 <- select(paluelmoro22, Especie, Fecha, Toponimo, Ala, 
                       P3, Peso, Grasa, Musculo, Tarso)


# Cabanes 22 --------------------------------------------------------------

Cabanes2022 <- read_excel("~/R/Github/Paludicola/Data/Cabanes 22.xlsx")

#Filtering our specie of study: Acrocephalus paludicola, creating a new df
palucabanes22 <- filter(Cabanes2022, NOMBRECI == "Acrocephalus paludicola")

#Selecting the columns we need
palucabanes22 <- select(palucabanes22, DATA, ALA, PR3, PES, GREIX,
                        muscul, NOMBRECI, NomLoc, TARS)

#Rearrange columns, to be in the same order than the previous one
colnames(palucabanes22)

col_order <- c("NOMBRECI", "DATA", "NomLoc", "ALA", "PR3", "PES", "GREIX",
               "muscul", "TARS")

palucabanes22 <- palucabanes22[, col_order]

#Renaming the columns to have the same name than the previous one
colnames(palucabanes22) <- c('Especie', 'Fecha', 'Paraje', 'Ala', 'P3', 
                             'Peso', 'Grasa', 'Musculo', 'Tarso')


# Life 2022 ---------------------------------------------------------------

Life2022 <- read_excel("~/R/Github/Paludicola/Data/Life 2022.xlsx", sheet = "Datos")

#Filtering our specie of study: Acrocephalus paludicola, creating a new df
Life2022 <- filter(Life2022, NombreEspecie == "Acrocephalus paludicola")

#There are 0 A. paludicola in this database, so it's better to just delete it
rm(Life2022)


# Life 2018-2021 ----------------------------------------------------------

Life2021 <- read_excel("~/R/Github/Paludicola/Data/Life2018-2021.xlsx")

Life2021 <- filter(Life2021, Especie == "Acrocephalus paludicola")

#Selecting, renaming and rearranging columns
Life2021 <- select(Life2021, Especie, Fecha, Paraje, GR, Ms, Ala, P8, Tarso, Peso)

colnames(Life2021) <- c('Especie', 'Fecha', 'Paraje', 'Grasa', 'Musculo', 'Ala', 'P3', 'Tarso', 'Peso')

col_order <- c('Especie', 'Fecha','Paraje', 'Ala', 'P3', 'Peso', 'Grasa', 'Musculo', 'Tarso')

Life2021 <- Life2021[, col_order]


# Catalunya ---------------------------------------------------------------

palucat <- read_excel("~/R/Github/Paludicola/Data/Catalunya.xlsx")

#We are working with data from 2018 to 2022, so we need to filter this data
#to get every data from 1-1-2018. In this df, "DATA" means "Date", because is
#in catalonian

palucat <- filter(palucat, DATA > '2018-01-01')

#Selecting columns, renaming them and rearranging
palucat <- select(palucat, DATA, ESPECIE, ALA, PR3, PES, GREIX, muscul, 
                  TARS, Localitat)

colnames(palucat) <- c('Fecha', 'Especie', 'Ala', 'P3', 'Peso', 'Grasa', 
                       'Musculo', 'Tarso', 'Paraje')

palucat <- palucat[, col_order]


# Paraje/Place ------------------------------------------------------------------

#Paraje means "place", and each df is from a different place, except Life2021
#which contains data from 3 different places, so we will take this df as the model
#The fact is that every df has the name of the place written on a different
#way, so we need to edit them.

#Also the df "paluelmoro22" has a different name for the "Paraje" column, so we need
#to edit that column's name too

colnames(paluelmoro22)[3] <- "Paraje"

#Writting the place's name in the same way as Life2021
palucat$Paraje <- 'Estany de Palau'

paluelmoro22$Paraje <- 'Marjal del Moro'

palucabanes22$Paraje <- 'Prat de Cabanes-Torreblanca'



# Date/Fecha --------------------------------------------------------------

#We need to change the format of the dates. "Fecha" means "Date".

#Working with dates is new for me, so I made backup dfs of each df, and we 
#will work with the new ones.
palucab1 <- palucabanes22

palumoro1 <- paluelmoro22

palucat1 <- palucat

Life1 <- Life2021


#Changing the data type of the column with "as.Date", using "format =" to change
#the format of the date. Be careful with capital letters, here I use Y as a capital
#letter, which is XXXX. If you change it to y, instead of Y, it's going to be only XX
palumoro1$Fecha <- format(as.Date(palumoro1$Fecha, format = "%d/%m/%Y"),
                          "%Y-%m-%d")

palucab1$Fecha <- format(as.Date(palucab1$Fecha, format = "%d/%m/%Y"),
                         "%Y-%m-%d")


# Changing data type/ Cambiando el tipo de datos ---------------------------------

#To be able to bind two different df, the columns must have the same type of data
#You can use str(df) to see which kind of data every column has


#ElMoro
str(palumoro1)

palumoro1 <- palumoro1 %>%   #Ala = Wing, P3 = 3rd Primary Feather, Peso = Weight
  mutate_at('Ala', as.numeric) %>%  #so all of them need to be numeric in order to
  mutate_at('P3', as.numeric) %>%   #work with them
  mutate_at('Peso', as.numeric) %>%
  mutate(Fecha = as.Date(Fecha, format = "%Y-%m-%d"))


#Cabanes
str(palucab1)

palucab1 <- palucab1 %>%
  mutate(Fecha = as.Date(Fecha, format = "%Y-%m-%d"))


#Catalunya
str(palucat1)

palucat1$Tarso <- str_replace_all(palucat1$Tarso, ",", ".") #To change the commas
                                                            #for dots

palucat1 <- palucat1 %>% #Tarso = Tarsus, so it needs to be numeric
  mutate_at('Tarso', as.numeric)

palucat1 <- palucat12 %>% #Changing the date to the same format as the previous df
  mutate(Fecha = as.Date(Fecha, format = "%Y-%m-%d"))


#Life 2018-2021
str(Life1)

Life1 <- Life1 %>% #Peso = Weight, so it needs to be numeric
  mutate_at('Peso', as.numeric)

Life1 <- Life1 %>% #Changing the date to the same format as the previous df
  mutate(Fecha = as.Date(Fecha, format = "%Y-%m-%d"))


# Binding dataframes(df)/Juntando bases de datos --------------------------

#Binding the df in the same order that we cleaned them. We are creating a new
#dataframe in each step, with the name of the 2 original dataframes, in case we need them
#but they are automatically deleted at the end with rm(df), leaving only the final 
#dataframe called "Paludicola"

elmorocabanes <- bind_rows(palumoro1, palucab1)

elmorocabanescat <- bind_rows(elmorocabanes, palucat1)

paludicola <- bind_rows(elmorocabanescat, Life1)

rm(elmorocabanes)
rm(elmorocabanescat)


# New variables -----------------------------------------------------------

#new backup df, but now we work with the older one (it's just because the name is
#shorter, but you can change this)
paludicolabackup <- paludicola

#We are going to work with the Latitude of each place, so we need to create a new 
#variable. There are 5 places: 
#Estany de Palau, Marjal de Pego-Oliva, Marjal del Moro, Prat de Cabanes-Torreblanca,
#Tancat de la Ratlla

#As you can see, those are quite long names so we can just pick up the las 2 letters
#because all of them are different, and just add a new variable. It's easier to work 
#with but probably it's worse when it's time to show this to other person, so it's up to you

paludicola <- paludicola %>%
  mutate(CoordDec = case_when(
    endsWith(Paraje, "au") ~ "42.2784",
    endsWith(Paraje, "va") ~ "38.8674",
    endsWith(Paraje, "ro") ~ "39.6270",
    endsWith(Paraje, "ca") ~ "40.1790",
    endsWith(Paraje, "lla") ~ "39.3272",
    
  ))

#Extract the year of each date. Year = Anyo
paludicola <- paludicola %>%
  mutate(Anyo = format(Fecha, format="%Y"))

#Extract the day of each date. Day = Dia
paludicola <- paludicola %>%
  mutate(Dia = format(Fecha, format="%d"))

#We need to change every new variable to a numeric format, because we need to work with them
paludicola <- paludicola %>%
  mutate_at('CoordDec', as.numeric) %>%
  mutate_at('Anyo', as.numeric) %>%
  mutate_at('Dia', as.numeric)

str(paludicola)



# Deleting NAs/Borrando NAs -----------------------------------------------


#That's it. Now we have a dataset with 97 observations and 12 variables, each one
#with the correct format. Now notice that the dataset still has some rows with NAs or
#incorrect values (much lower than the usual)

#We have 6 variables to study: Ala (Wing), P3(3rd Feather), Grasa(Fat), Musculo(Muscle)
#Peso(Weight) and Tarso(Tarsus). We start looking for NAs values in each variable

table(is.na(paludicola$Ala))
table(is.na(paludicola$Musculo))
table(is.na(paludicola$Grasa))
table(is.na(paludicola$Peso))
table(is.na(paludicola$P3))
table(is.na(paludicola$Tarso))

#Notice that Tarso has the most NAs value (14/97), while the other have 3 o 4 NAs

#we save the paludicola df to the old backup, just in case we need the dataframe
#without deleting anything
paludicolabackup <- paludicola
#this is just to restore it
paludicola <- paludicolabackup

#deleting every NAs from all the variables except Tarso
paludicola <- paludicola[!is.na(paludicola$Ala),]
paludicola <- paludicola[!is.na(paludicola$Musculo),]
paludicola <- paludicola[!is.na(paludicola$Grasa),]

#Now we have 91, and we started with 97. 6 deleted.

#For each value, there are some "allowed" values: for example, Ala, P3, Peso and
#Tarso are biometric measures, so they can't be 0. But Musculo and Grasa are some
#values that the technician give to the bird, so they go from 0-8

#We need to check if the values are "normal". For example, a 0 in Ala is not valid
#but if we find it in Grasa it's OK

summary(paludicola$Ala)
summary(paludicola$P3)
summary(paludicola$Peso)
summary(paludicola$Musculo)
summary(paludicola$Grasa)

#so we select the values >0 for each continuos variable (Ala, P3, Peso)


paludicola <- filter(paludicola, Ala > "0")
paludicola <- filter(paludicola, P3 > "0")
paludicola <- filter(paludicola, Peso > "0")

#Now, from 91 data to 87. We check them again. Now the 3 variables have "usual" values

#If we don't use the variable Tarso, we will have a sample size of 87.
#But if we check the NAs and the summary of the Tarso variable, we see that there are
#7 NAs and the minimum value is 0 (which is not possible), so the final dataframe
#will have 79 or less individuals
table(is.na(paludicola$Tarso))
summary(paludicola$Tarso)

#we create a new dataframe using the Tarso variable, so we need to filter it

paludicolatarso <- filter(paludicola, Tarso > "0")

#So, we have a dataframe with 87 obs. using 5 variables(Ala,P3,Peso,Musculo y Grasa)
View(paludicola)

#And another one, with 79 obs. using 6 variables (the same as paludicola + Tarso)
View(paludicolatarso)

#####################THE END###############




