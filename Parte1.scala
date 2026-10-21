object TareaScala extends App {

  // Ejercicio 1. Operaciones como valores
  val duplicar: Int => Int = x => x * 2
  val cuadrado: Int => Int = x => x * x
  val esPar: Int => Boolean = x => x % 2 == 0

  println(duplicar(5))
  println(cuadrado(4))
  println(esPar(8))

  // Ejercicio 2. Lambdas matemáticas
  val cubo: Int => Int = x => x * x * x
  val promedio: (Double, Double) => Double = (a, b) => (a + b) / 2.0
  val mayor: (Int, Int) => Int = (a, b) => if (a > b) a else b
  val esMultiploDeTres: Int => Boolean = x => x % 3 == 0

  println(cubo(3))
  println(promedio(10.0, 20.0))
  println(mayor(15, 8))
  println(esMultiploDeTres(9))

  // Ejercicio 3. Aplicar una transformación
  def transformar(numero: Int, operacion: Int => Int): Int =
    operacion(numero)

  println(transformar(5, x => x * 2))
  println(transformar(5, x => x * x))

  // Ejercicio 4. Calculadora flexible
  def calcular(a: Double, b: Double, operacion: (Double, Double) => Double): Double =
    operacion(a, b)

  println(calcular(10.0, 5.0, (x, y) => x + y))
  println(calcular(10.0, 5.0, (x, y) => x - y))

  // Ejercicio 5. Crear multiplicadores
  def crearMultiplicador(factor: Int): Int => Int =
    x => x * factor

  val duplicarE5 = crearMultiplicador(2)
  val triplicarE5 = crearMultiplicador(3)
  val porDiezE5 = crearMultiplicador(10)

  println(duplicarE5(7))
  println(triplicarE5(7))
  println(porDiezE5(7))

  // Ejercicio 6. Aplicación parcial con descuentos
  def aplicarDescuento(precio: Double, porcentaje: Double): Double =
    precio * (1.0 - porcentaje / 100.0)

  val descuento10: Double => Double = precio => aplicarDescuento(precio, 10.0)
  val descuento20: Double => Double = precio => aplicarDescuento(precio, 20.0)
  val descuento50: Double => Double = precio => aplicarDescuento(precio, 50.0)

  println(descuento10(500))
  println(descuento20(500))

  // Ejercicio 7. Currificación de operaciones
  def elevar(exponente: Int)(base: Int): Int =
    math.pow(base.toDouble, exponente.toDouble).toInt

  val cuadradoE7 = elevar(2) _
  val cuboE7 = elevar(3) _
  val cuartaPotencia = elevar(4) _

  println(cuadradoE7(5))
  println(cuboE7(3))
  println(cuartaPotencia(2))

  // Ejercicio 8. Composición de funciones
  val limpiar: String => String = s => s.trim
  val convertirMinusculas: String => String = s => s.toLowerCase
  val longitud: String => Int = s => s.length
  val esLongitudValida: Int => Boolean = len => len >= 5

  val validarTexto: String => Boolean = limpiar andThen convertirMinusculas andThen longitud andThen esLongitudValida

  println(validarTexto("   Scala   "))
  println(validarTexto("  abc "))

  // Ejercicio 9. Sumatoria y producto genéricos
  def sumar(desde: Int, hasta: Int, transformar: Int => Int): Int =
    if (desde > hasta) 0
    else transformar(desde) + sumar(desde + 1, hasta, transformar)

  def producto(desde: Int, hasta: Int, transformar: Int => Int): Int =
    if (desde > hasta) 1
    else transformar(desde) * producto(desde + 1, hasta, transformar)

  println(sumar(1, 4, x => x))
  println(sumar(1, 3, x => x * x))
  println(producto(1, 4, x => x))

  // Ejercicio 10. Transformación y búsqueda genérica
  def transformarTexto(texto: String, transformacion: Char => Char): String =
    if (texto.isEmpty) ""
    else transformacion(texto.head).toString + transformarTexto(texto.tail, transformacion)

  def buscarCaracter(texto: String, condicion: Char => Boolean): Boolean =
    if (texto.isEmpty) false
    else if (condicion(texto.head)) true
    else buscarCaracter(texto.tail, condicion)

  println(transformarTexto("scala", c => c.toUpper))
  println(buscarCaracter("Scala2026", c => c.isDigit))
  println(buscarCaracter("Scala", c => c == 'z'))

}