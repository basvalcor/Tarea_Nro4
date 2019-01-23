#Receta de diseño
#Funcion para obtener el digito verificador de rut
#dv: int -> int
#Def dv: funcion para generar digito verificador para un rut valido
#Ejemplo: dv(18906532)->9

dv <- function(rut){
  rut = as.character(rut)
  x = as.numeric(rev(strsplit(rut,NULL)[[1]]))
  Multiplo = rep(2:7,length.out=length(x))
  y = sum(x*Multiplo)
  z = 11 - y + floor(y/11)*11
  key = c(1:11)
  val = c(1:9,"k",0)
  dv = val[match(z, key)]
  return(dv)
}

#Test
dv(18906532)

"Receta de diseño"
"Definicion de tiempo: tiempo que se demora en codificar Ruts "
"Definicion de Ruts: crea una lista de 5.000 numeros de ruts aleatorios unicos con su 
digito verificador, todos estos sin repetición "


time<-proc.time()
Ruts<-list()
for(i in 18933987:(18933987+5000)){
  rut<-print(paste(i,"-",dv(i)))
  Ruts <- c(Ruts,rut)
}
proc.time()-time

#Contar: String->String
#Def contar: "strsplit(a,b)[[1]]":
#             función que cuenta palabras, donde a es el contenido y b un separador

#Ejemplo: Contar<-strsplit(" El perrito de Rita me irrita. Dile a Rita
#         que cambie el perrito por perrita "," ")[[1]] 
#          -> [1] ""        "El"      "perrito" "de"      "Rita"    "me"      "irrita." "Dile"   
#             [9] "a"       "Rita"    "que"     "cambie"  "el"      "perrito" "por"     "perrita"

#Minuscula: String->String
#Def Minuscula: considera todas las letras como minuscula 
#Ejemplo: Minuscula(Contar)
# ->   [1] ""        "el"      "perrito" "de"      "rita"    "me"      "irrita." "dile"   
#      [9] "a"       "rita"    "que"     "cambie"  "el"      "perrito" "por"     "perrita"   "perrita"

#Unico: String->String
#Def Unico: Cuenta solo una palabra sin repetir y en minisculas
#Ejemplo: unico(Minuscula)
# ->   [1] ""        "el"      "perrito" "de"      "rita"    "me"      "irrita." "dile"   
#      [9] "a"       "que"     "cambie"  "por"     "perrita"


Minuscula<-tolower(Contar)
Unico<-unique(Minuscula)
Minuscula2<-tolower(Contar2)
Unico2<-unique(Minuscula2)

#Test1
Contar
Minuscula
Unico

#Test2
Contar2
Minuscula2
Unico2


#Contado las palabras
ContandoLlamas<-list()
for(i in 1:length(Unico2)){
  print(Unico2[i])
  contador <- 0
  for (y in 1:length(Minuscula2)){
    print(Minuscula2[y])
    if(Unico2[i]==Minuscula2[y]){
      print(contador <- contador+1)
    }
  }
  ContandoLlamas<-c(ContandoLlamas,contador) 
}

#Contar Llama
a<-" Porque la llama que llama estando en llamas me llama, alguien mas llama"
b<-" "
c<-"Porque la llama que llama estando en llamas me llama , alguien mas llama"
Contar<-strsplit(a,b)[[1]]
Contar2<-strsplit(c,b)[[1]]


"Receta de diseño"
"contarSaldoNegativo: string-> int"
"Definicion contarSaldoNegativo: contar la cantidad de clientes con saldos negativos"
"Ejemplo: contarSaldoNegativo (clientes)"

clientes <- list (list(1,"blas",-22000),list(2,"juan",-50000),list(3,"robert",1000),list(4,"amaro",-25000))

contarSaldoNegativo <- function(listaclientes){
  GenteSaldoNegativo <- 0
  for (cliente in listaclientes){
    if (cliente[3] < 0){
      GenteSaldoNegativo<- (GenteSaldoNegativo+1)
    }
  }
  return(GenteSaldoNegativo)
}

#Test
contarSaldoNegativo(clientes)




"Receta de diseño"
"sinvocales: string -> string sin vocales"
"Definicion sinvocales: retornar x frase sin sus vocales"
"Ejemplos: Chile campeon , El partido termino con 0 goles "

sinVocales <- function(oracion){
  stringsinvocales <- ""
  strspliteada <- strsplit(oracion, "")[[1]]  
  for (letra in strspliteada){
    if(letra != "a" && letra != "e" && letra != "i" && letra != "o" && letra != "u" && letra != "A" && letra != "E"
       && letra != "I" && letra != "O" && letra != "U") {
      stringsinvocales <- paste(stringsinvocales, letra, sep="")
    }
  }
  return(stringsinvocales)
}

#Test
Frase <- "Chile campeon"
sinVocales(Frase)
Frase_2 <- "El partido termino con 0 goles"
sinVocales(Frase_2)