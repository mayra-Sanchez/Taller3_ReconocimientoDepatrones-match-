package object Huffman {

  import scala.language.postfixOps

  /**
   * Representación de árbol
   */
  abstract class ArbolH

  case class Nodo(izq: ArbolH, der: ArbolH, cars: List[Char], peso: Int) extends ArbolH

  case class Hoja(car: Char, peso: Int) extends ArbolH

  /**
   * Dado un árbol de Huffman devuelven su peso y la lista de caracteres que codifica
   */
  def peso(arbol: ArbolH): Int = arbol match {
    case Nodo(_, _, _, w) => w
    case Hoja(_, w) => w
  }

  def cars(arbol: ArbolH): List[Char] = arbol match {
    case Nodo(_, _, cs, _) => cs
    case Hoja(c, _) => List(c)
  }

  def hacerNodoArbolH(izq: ArbolH, der: ArbolH) =
    Nodo(izq, der, cars(izq) ::: cars(der), peso(izq) + peso(der))

  /**
   * Construyendo arbol de Huffman
   */

  // Devuelve la lista con la frecuencia en que cada carácter aparece en el texto.
  def ocurrencias(cars: List[Char]): List[(Char, Int)] = {
    (Map[Char, Int]() /: cars) {
      (m, c) =>
        m + (c -> (m.get(c) map {
          _ + 1
        } getOrElse 1))
    } toList
  }

  /* Devuelve la lista de hojas del árbol de Huffman correspondiente, ordenada ascendentemente
  por la frecuencia de cada carácter*/
  def listaDeHojasOrdenadas(frecs: List[(Char, Int)]): List[Hoja] = {
    frecs map { a => Hoja(a._1, a._2) } sortBy {
      peso _
    }
  }

  // Devuelve true si solo hay un árbol en la lista, y false en caso contrario
  def listaUnitaria(arboles: List[ArbolH]): Boolean = {
    arboles.size == 1
  }

  /*
  Recibe una lista de árboles de Huffman ordenada ascendentemente por el peso de cada árbol, toma los dos primeros
  (los de menor peso) si los hay, y devuelve una lista de árboles de Huffman ordenada ascendentemente con los mismos
  árboles originales, salvo los dos primeros.
   */
  def combinar(arboles: List[ArbolH]): List[ArbolH] = arboles match {
    case Nil | _ :: Nil => arboles
    case x :: y :: ts => hacerNodoArbolH(x, y) :: ts sortBy {
      peso _
    }
  }

  /* La función currificada hastaQue, recibe una pareja condición y acción (cond : List[ArbolH] => Boolean,
  mezclar : List[ArbolH] => List[ArbolH]), y luego una lista ordenada de árboles de Huffman, y devuelve una lista
  de árboles de Huffman correspondiente a aplicar la acción mezclar repetidamente sobre la lista original y sus resultados */
  def hastaQue(cond: List[ArbolH] => Boolean, mezclar: List[ArbolH] => List[ArbolH])
              (listaOrdenadaArboles: List[ArbolH]): List[ArbolH] = {
    if (cond(listaOrdenadaArboles)) listaOrdenadaArboles else hastaQue(cond, mezclar)(mezclar(listaOrdenadaArboles))
  }

  // Recibe un texto en forma de lista de caracteres y devuelve el árbol de Huffman asociado a ese texto.
  def crearArbolDeHuffman(cars: List[Char]): ArbolH = {
    hastaQue(listaUnitaria, combinar)((listaDeHojasOrdenadas _ compose ocurrencias) (cars)).head
  }

  /**
   * Decodificando
   */
  type Bit = Int

  /*
  recibe un árbol de Huffman y una lista de bits correspondiente a la codificación de un mensaje con ese árbol,
  y devuelve la lista de caracteres correspondiente al mensaje decodificado.
   */
  def decodificar(arbol: ArbolH, bits: List[Bit]): List[Char] = {
    def decodificarOtro(arbol: ArbolH, bs: List[Bit]): (Char, List[Bit]) =
      (bs, arbol) match {
        case (xs, Hoja(c, _)) => (c, xs)
        case (Nil, _) => sys.error("Esta secuencia termina en medio del arbol")
        case (x :: xs, Nodo(l, r, _, _)) => decodificarOtro(if (x == 0) l else r, xs)
        case _ => sys.error("Esto puede suceder")
      }

    if (bits.isEmpty) Nil else {
      val (c, remainder) = decodificarOtro(arbol, bits)
      c :: decodificar(arbol, remainder)
    }
  }

  /**
   * Codificando
   */
  // parte ineficiente
  def codificar(arbol: ArbolH)(texto: List[Char]): List[Bit] = {
    def codificarChar(arbol: ArbolH, c: Char): List[Bit] = {
      require(cars(arbol).contains(c))
      arbol match {
        case Hoja(x, _)       => Nil
        case Nodo(l, r, _, _) =>
          if (cars(l).contains(c))
            0 :: codificarChar(l, c)
          else 1 :: codificarChar(r, c)
      }
    }
    def _codificar(arbol: ArbolH)(text: List[Char]): List[List[Bit]] =
      text match {
        case Nil     => Nil
        case c :: cs => codificarChar(arbol, c) :: _codificar(arbol)(cs)
      }
    _codificar(arbol)(texto).flatten
  }

  // parte eficiente
  type TablaCodigos = List[(Char, List[Bit])]

  // Recibe una tabla de códigos, y luego un carácter, y devuelve la lista de Bits correspondiente a ese carácter según la tabla.
  def codigoEnBits (tabla: TablaCodigos) (car: Char): List [Bit] = {
    tabla find { case (c, bs) => c == car } map { _._2 } getOrElse Nil
  }

  /*
    recibe dos tablas de códigos correspondientes a dos subárboles (izquierdo y derecho) de un árbol de Huffman,
    y devuelve la tabla de códigos correspondiente al árbol del que hacen parte.
   */
  def mezclarTablasDeCodigos(a: TablaCodigos, b: TablaCodigos): TablaCodigos = a union b

  val izquierda: TablaCodigos => TablaCodigos = ct  => BitP(0)(ct)
  val derecha: TablaCodigos => TablaCodigos = ct => BitP(1)(ct)
  val BitP: Bit => TablaCodigos => TablaCodigos = b => ct => ct map { case (c, bs) => (c, b :: bs) }

  // Recibe un árbol de Huffman, y devuelve la tabla de códigos correspondiente a ese árbol
 def convertir (arbol: ArbolH): TablaCodigos = arbol match {
   case Hoja(car, peso) => List((car, Nil))
   case Nodo(izq, der, cars, peso) => mezclarTablasDeCodigos(izquierda(convertir(izq)), derecha(convertir(der)))
 }

  /*
   Recibe un árbol de Huffman y luego una lista de caracteres correspondiente al mensaje a codificar con ese árbol,
   y devuelve la lista de bits correspondiente al mensaje codificado, haciéndolo a través de una tabla de códigos
   para ser más eficientes.
   */
  def codificarRapido(arbol: ArbolH)(texto: List[Char]): List[Bit] = texto map codigoEnBits(convertir(arbol)) flatten

}