import Huffman.{ArbolH, Hoja, codificar, cars, codificarRapido, codigoEnBits, combinar, convertir, crearArbolDeHuffman, decodificar, hacerNodoArbolH, hastaQue, listaDeHojasOrdenadas, listaUnitaria, mezclarTablasDeCodigos, ocurrencias, peso}

val arbolEjemplo = hacerNodoArbolH (hacerNodoArbolH ( Hoja ( 'x' , 1 ) , Hoja ( 'e' , 1 ) ) , Hoja ( 't', 2) )

// Esta funcion crea una lista de caracteres correspondiente a una cadena dada
def cadenaALista(cad: String): List[Char] = cad.toList

val lc= cadenaALista ( "La_vida_es_dura")
val lc2= cadenaALista ( "Me_gusta_programar")
val lc3= cadenaALista ( "Soy_mayra")
val lc4= cadenaALista ( "Me_duele_la_espalda")
val lc5= cadenaALista ( "H")

val lho= listaDeHojasOrdenadas ( ocurrencias( lc ) )
val lho2=listaDeHojasOrdenadas ( ocurrencias( lc2 ) )
val lho3=listaDeHojasOrdenadas ( ocurrencias( lc3 ) )
val lho4=listaDeHojasOrdenadas ( ocurrencias( lc4 ) )
val lho5=listaDeHojasOrdenadas ( ocurrencias( lc5 ) )

// Verificar si hay un solo arbol en la lista
listaUnitaria ( lho )
listaUnitaria ( lho2 )
listaUnitaria ( lho3 )
listaUnitaria ( lho4 )
listaUnitaria ( lho5 )

// Crear arbol
val arbol1  = crearArbolDeHuffman ( lc )
val arbol2  = crearArbolDeHuffman ( lc2 )
val arbol3  = crearArbolDeHuffman ( lc3 )
val arbol4  = crearArbolDeHuffman ( lc4 )
val arbol5  = crearArbolDeHuffman ( lc5 )

// Convertir
val codigo1 = convertir ( arbol1 )
val codigo2 = convertir ( arbol2 )
val codigo3 = convertir ( arbol3 )
val codigo4 = convertir ( arbol4 )
val codigo5 = convertir ( arbol5 )

// Mezclar tablas de codigos
mezclarTablasDeCodigos ( codigo1 , codigo2 )
mezclarTablasDeCodigos ( codigo2 , codigo3 )
mezclarTablasDeCodigos ( codigo3 , codigo4 )
mezclarTablasDeCodigos ( codigo4 , codigo5 )
mezclarTablasDeCodigos ( codigo5 , codigo1 )

// Codificar rapido
codificarRapido ( arbol1 ) ( lc )
codificarRapido ( arbol2 ) ( lc2 )
codificarRapido ( arbol3 ) ( lc3 )
codificarRapido ( arbol4 ) ( lc4 )
codificarRapido ( arbol5 ) ( lc5 )

/*
 Codigo en bits
 */
// Codigo 1 "La vida es dura"
codigoEnBits (codigo1)('d')

// Codigo 2 "Me gusta programar"
codigoEnBits (codigo2)('e')

// Codigo 3 "Soy mayra"
codigoEnBits (codigo3)('o')

// Codigo 4 "Me duele la espalda"
codigoEnBits (codigo4)('d')

// Codigo 5 "H"
codigoEnBits (codigo5)('H')

/*
 Codificar (Arroja el mismo resultado de la funcion codificarRapido
 */
val codificar1 = codificar(arbol1)(List('L','a','_','v','i','d','a','_','e','s','_','d','u','r','a'))
val codificar2 = codificar(arbol2)(List('M','e','_','g','u','s','t','a','_','p','r','o','g','r','a','m','a','r'))
val codificar3 = codificar(arbol3)(List('S','o','y','_','m','a','y','r','a'))
val codificar4 = codificar(arbol4)(List('M','e','_','d','u','e','l','e','_','l','a','_','e','s','p','a','l','d','a'))
val codificar5 = codificar(arbol5)(List('H'))

// Combinar
combinar ( List(arbol1,arbol2))
combinar ( List(arbol2,arbol3))
combinar ( List(arbol3,arbol4))
combinar ( List(arbol4,arbol5))
combinar ( List(arbol5,arbol1))

// Decodificar
decodificar( arbol1 , List(1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0))
decodificar( arbol2 , List(1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1) )
decodificar( arbol3 , List(1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0) )
decodificar( arbol4 , List(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1) )
decodificar( arbol5 , List() )

// Hasta que
hastaQue(listaUnitaria, combinar)(lho)
hastaQue(listaUnitaria, combinar)(lho2)
hastaQue(listaUnitaria, combinar)(lho3)
hastaQue(listaUnitaria, combinar)(lho4)
hastaQue(listaUnitaria, combinar)(lho5)
