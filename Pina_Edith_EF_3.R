####Problema 3
#Usa funciones y ciclo while para pedirle al usuario una secuencia de tres nucleótidos y que genere
#1. El aminoácido correspondiente
#2. La secuencia complementaria#####

###Para leer las secuencias y los datos de entrada se utiliza DNAString, funcion de BioStrings
##Se carga la libreía
library(Biostrings)

####Se empleo function, en este caso se escribio ningun argumento porque no se necesitan entradas se van a asignar cuando se llene lo demas
#en el primer objeto se utilizo la función readline() en donde como argumento se coloco prompt paste, para que en la consola 
#se imprima el mensaje y así colocar la secuencia que tu elijas de tres nucleotios
##el segundo objeto se <- es para indiicar que el primer objeto que se va a llenar en la consola sea leído como una secuencia de DNA
###entonces desppues se emplean las condicionantes para que nos dias que aa es
##en el primer if tenemos que si dna es igual a determinadas secuencias como GGG, GGt... el aminoacido es Glicina y en la pantalla se va a imprimir
##un enunciado en el cual diga el aa que es
####else if, pero si se parece a estas secuencias es otro aa, se realizo lo mismo para los 20 aa
###Para determinar la secuencia complementaria, se empleo un ciclo while
###En este caso el objeto x se le asigna el valor 1 para que sepa en donde iniciar, tambien requerimos el objeto
#donde se va a llenar y se va a colocar la secuencia. 
##En el ciclo while tenemos que una bariable x, la cual va a ir cambiando de posición, conforme se vaya complementado
##If x es igual a A, se llena con T, en el caso de no ser tenemos else if, pero sí es T se llena con A, y si aun no se cumple con esa condición se emplea otra
###al final debemos de añadir un objeto al cual se le vaya sumando un 1 luego de haber cumplido una letra.Sino el ciclo es infinito
programa3 <- function(){
  se <- readline(prompt = paste("Escriba una secuencia de tres nucleÃ³tidos : "))
  dna <- DNAString(se)
  if ( (dna == DNAString("GGG")) | (dna==DNAString("GGT")) | (dna==DNAString("GGA")) | (dna==DNAString("GGG")) ) {
    print(paste("El aminoacido es Glicina"))
  } else if ( (dna == DNAString("TTT")) | (dna == DNAString("TTC")) ){
    print(paste("El aminoacido es Fenilalanina"))
  } else if( (dna==DNAString("TTA")) | (dna==DNAString("TTG")) |(dna==DNAString("CTT")) | (dna==DNAString("CTC")) |
             (dna==DNAString("CTA")) | (dna==DNAString("CTG")) ){
    print(paste("El aminoacido es Leucina"))
  }else if( (dna==DNAString("TCT")) | (dna==DNAString("TCC")) |(dna==DNAString("TCA")) | (dna==DNAString("TCG")) |
            (dna==DNAString("AGT")) | (dna==DNAString("AGC")) ){
    print(paste("El aminoacido es Serina"))
  }else if( (dna==DNAString("TAT")) | (dna==DNAString("TAC")) ) {
    print(paste("El aminoacido es Tirosina"))
  }else if( (dna==DNAString("TGT")) | (dna==DNAString("TGC")) ) {
    print(paste("El aminoacido es Cisteína"))
  }else if( (dna==DNAString("TGG")) ){
    print(paste("El aminoacido es Triptofano"))
  }else if( (dna==DNAString("CCT")) | (dna==DNAString("CCC")) |(dna==DNAString("CCA")) | (dna==DNAString("CCG")) ){
    print(paste("El aminoacido es Prolina"))
  }else if( (dna==DNAString("CAT")) | (dna==DNAString("CAC")) ){
    print(paste("El aminoacido es Histidina"))
  }else if( (dna==DNAString("CAA")) | (dna==DNAString("CAG")) ){
    print(paste("El aminoacido es Glutamina"))
  }else if( (dna==DNAString("CGT")) | (dna==DNAString("CGC"))  | (dna==DNAString("CGA")) | (dna==DNAString("CGG")) | 
            (dna==DNAString("AGA")) | (dna==DNAString("AGG"))){
    print(paste("El aminoacido es Arginina"))
  }else if( (dna==DNAString("ATT")) | (dna==DNAString("ATC"))  | (dna==DNAString("ATA")) ){
    print(paste("El aminoacido es Isoleucina"))
  }else if( (dna==DNAString("ATG"))  ){
    print(paste("El aminoacido es Metionina"))
  }else if( (dna==DNAString("ACT")) | (dna==DNAString("ACC"))  | (dna==DNAString("ACA")) | (dna==DNAString("ACG")) ){
    print(paste("El aminoacido es Treonina"))
  }else if( (dna==DNAString("AAT")) | (dna==DNAString("AAC")) ){
    print(paste("El aminoacido es Asparagina"))
  }else if( (dna==DNAString("AAA")) | (dna==DNAString("AAG")) ){
    print(paste("El aminoacido es Lisina"))
  }else if( (dna==DNAString("GTT")) | (dna==DNAString("GTC"))  | (dna==DNAString("GTA")) | (dna==DNAString("GTG")) ){
    print(paste("El aminoacido es Valina"))
  }else if( (dna==DNAString("GCT")) | (dna==DNAString("GCC"))  | (dna==DNAString("GCA")) | (dna==DNAString("GCG")) ){
    print(paste("El aminoacido es Alanina"))
  }else if( (dna==DNAString("GAT")) | (dna==DNAString("GAC")) ){
    print(paste("El aminoacido es Acido aspartico"))
  }else if( (dna==DNAString("GAA")) | (dna==DNAString("GAG")) ){
    print(paste("El aminoacido es Acido glutamico"))
  }else if( (dna==DNAString("TAA")) | (dna==DNAString("TAG")) | (dna==DNAString("TGA"))){
    print(paste("Codones de paro"))
  }
  x <- 1
  comp <- DNAString()
  while  ( x <= length(dna) ){
    if ( dna[x] == DNAString("A")  ) {
      comp <- c(comp, "T")
    }else if ( dna[x] == DNAString("T")  ) {
      comp <- c(comp, "A")
    }else if ( dna[x] == DNAString("G")  ) {
      comp <- c(comp, "C")
    }else if ( dna[x] == DNAString("C")  ) {
      comp <- c(comp, "G")
    }
    x <- x + 1 
  }
  print(paste("La secuencia complementaria es ", comp))
}

programa3() ####Al correr esta parte te va a preguntar por tu secuencia de tres nucleotidos en la consola, con base a los que llenes
            #te dira que aa y te dara la secuencia complementaria 
