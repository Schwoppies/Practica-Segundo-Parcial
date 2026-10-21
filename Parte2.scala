object SistemaCineApp extends App {

  // Ejercicio 1. Clase Pelicula y objeto acompañante
  class Pelicula(
    val titulo: String,
    val duracionMinutos: Int,
    val clasificacion: String
  )

  object Pelicula {
    def apply(
      titulo: String,
      duracionMinutos: Int,
      clasificacion: String
    ): Pelicula = {
      val duracionValida = if (duracionMinutos <= 0) 90 else duracionMinutos
      new Pelicula(titulo, duracionValida, clasificacion)
    }
  }

  // Ejercicio 2. Clase FuncionCine y objeto acompañante
  class FuncionCine(
    val pelicula: Pelicula,
    val sala: Int,
    val hora: String,
    val precioEntrada: Double,
    val asientosDisponibles: Int
  ) {

    // Ejercicio 3. Método puedeReservar
    def puedeReservar(cantidad: Int): Boolean = {
      asientosDisponibles >= cantidad
    }

    // Ejercicio 4. Método costoReserva
    def costoReserva(cantidad: Int): Double = {
      cantidad * precioEntrada
    }

    // Ejercicio 5. Método reservar
    def reservar(cantidad: Int): FuncionCine = {
      if (puedeReservar(cantidad)) {
        new FuncionCine(
          pelicula,
          sala,
          hora,
          precioEntrada,
          asientosDisponibles - cantidad
        )
      } else {
        this
      }
    }

    // Ejercicio 6. Método descripcion
    def descripcion: String = {
      s"${pelicula.titulo}\nSala: $sala\nHora: $hora\nPrecio: $precioEntrada Bs\nAsientos disponibles: $asientosDisponibles"
    }
  }

  object FuncionCine {
    def apply(
      pelicula: Pelicula,
      sala: Int,
      hora: String,
      precioEntrada: Double,
      asientosDisponibles: Int
    ): FuncionCine = {
      val precioValido = if (precioEntrada >= 0.0) precioEntrada else 0.0
      val asientosValidos = if (asientosDisponibles >= 0) asientosDisponibles else 0
      val salaValida = if (sala >= 1) sala else 1

      new FuncionCine(pelicula, salaValida, hora, precioValido, asientosValidos)
    }

    // Ejercicio 7. Método de fábrica funcionEstandar
    def funcionEstandar(
      pelicula: Pelicula,
      sala: Int,
      hora: String
    ): FuncionCine = {
      FuncionCine(pelicula, sala, hora, 30.0, 50)
    }
  }

  // Ejercicio 8. Objetos de prueba
  val pelicula1 = Pelicula(
    "Dune",
    155,
    "PG-13"
  )

  val funcion1 = FuncionCine(
    pelicula1,
    3,
    "20:30",
    35.0,
    40
  )

  val funcion2 = FuncionCine.funcionEstandar(
    pelicula1,
    1,
    "16:00"
  )

  // Ejercicio 9. Pruebas requeridas
  println(funcion1.descripcion)
  println(funcion1.puedeReservar(5))
  println(funcion1.costoReserva(5))

  val funcionReservada = funcion1.reservar(5)

  println(funcion1.asientosDisponibles)
  println(funcionReservada.asientosDisponibles)

  // Ejercicio 10. Restricciones (val, sin var, apply, tipos y retornos explícitos cumplidos)

}