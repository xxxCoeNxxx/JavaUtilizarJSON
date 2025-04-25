import com.mongodb.client.{MongoClients, MongoCollection, MongoDatabase}
import org.bson.Document
import scala.io.StdIn
import upickle.default.{read => uread, write => uwrite, ReadWriter, macroRW} // Importamos macroRW para generar automáticamente el ReadWriter
import os.{read => _, _} // Excluimos el método read de os-lib
import scala.collection.mutable.ArrayBuffer
import play.api.libs.json._

case class Libro(titulo: String, idAutor: String, autor: String, anio: String, editorial: String, isbn: String)
implicit val rwLibro: ReadWriter[Libro] = macroRW
//implicit val libroFormat: OFormat[Libro] = Json.format[Libro]

case class Autor(id: String, nombre: String, apellidos: String, numLibros: String)
implicit val rwAutor: ReadWriter[Autor] = macroRW
//implicit val autorFormat: OFormat[Autor] = Json.format[Autor]

object Ej11 {
    private val client = MongoClients.create("mongodb://localhost:27017")
    private val database: MongoDatabase = client.getDatabase("biblioteca")
    private val collection: MongoCollection[Document] = database.getCollection("libros")
    //private val collectionAutores: MongoCollection[Document] = database.getCollection("autores")



    def main(args: Array[String]): Unit = {
        var salir = false
        while (!salir) {
            mostrarMenu()
            StdIn.readLine() match {
                case "1" => crearLibro()
                case "2" => listarLibros()
                case "3" => listarAutores()
                case "4" => actualizarLibro()
                case "5" => eliminarSeleccionado()
                case "6" => leerLibrosDeJSON()
                case "7" => leerAutoresDeJSON()
                case "8" => salir = true
                case _ => println("Introduce una opción válida del 1 al 5")
            }
        }
        client.close()
    }

    private def mostrarMenu(): Unit = {
        println("\n --- MENU ---")
        println("1. Crear un libro")
        println("2. Listar los libros")
        println("3. Listar los autores")
        println("4. Actualizar libro")
        println("5. Eliminar libro o autor")
        println("6. Leer libros de JSON")
        println("7. Leer autores de JSON")
        println("8. Salir")
        println("----------------------")
    }

    private def crearLibro(): Unit = {
        val libro = pedirDatosLibro()
        val document = new Document()
            .append("titulo", libro.titulo)
            .append("autor", libro.autor)
            .append("anio", libro.anio)
            .append("editorial", libro.editorial)
            .append("ISBN", libro.isbn)

        collection.insertOne(document)
        println("Nuevo libro creado")
        println(s"ISBN: ${libro.isbn}")

        val autoresCollection = database.getCollection("autores")
        val filtro = new Document().append("id", libro.idAutor)
        val autorDoc = autoresCollection.find(filtro).first()
        if (autorDoc != null) {
            val numLibrosActual = autorDoc.getString("numLibros").toInt + 1
            val nuevoNumLibros = numLibrosActual

            val actualizacion = new Document("$set", new Document("numLibros", nuevoNumLibros.toString))
            autoresCollection.updateOne(filtro, actualizacion)
            println(s"El número de libros del autor ${libro.autor} se ha actualizado a $nuevoNumLibros")
        } else {
                println(s"No se encontró al autor ${libro.autor} para actualizar el número de libros.")
            }
    }

    private def listarLibros(): Unit = {
        println("\n--- LISTA DE LIBROS ---")
        val resultadoLibros = collection.find().iterator()
        while (resultadoLibros.hasNext) {
            mostrarLibro(resultadoLibros.next())
        }
    }

    private def listarAutores(): Unit = {
        println("\n--- LISTA AUTORES ---")
        val resultadoAutores = collection.find().iterator()
        while (resultadoAutores.hasNext) {
            mostrarAutor(resultadoAutores.next())
        }
    }

    private def actualizarLibro(): Unit = {
        println("Ingrese el ISBN del libro a actualizar: ")
        val isbn = StdIn.readLine()
        val filtro = new Document("isbn", isbn)
        val libroExistente = collection.find(filtro).first()

        if (libroExistente != null) {
            val nuevosDatos = pedirDatosLibro()
            val actualizacion = new Document("$set", new Document()
                .append("titulo", nuevosDatos.titulo)
                .append("autor", nuevosDatos.autor)
                .append("anio", nuevosDatos.anio)
                .append("editorial", nuevosDatos.editorial)
                .append("isbn", nuevosDatos.isbn))
            collection.updateOne(filtro, actualizacion)
            println("\nLibro actualizado")
        } else {
            println("Libro no encontrado")
        }
    }

    private def mostrarLibro(document: Document): Unit = {
        println(s"""
         Titulo: ${document.getString("titulo")}
         Autor: ${document.getString("autor")}
         Anio: ${document.getString("anio")}
         Editorial: ${document.getString("editorial")}
         ISBN: ${document.getString("ISBN")}
        ---------------------------------------------""")
    }

    private def mostrarAutor(document: Document): Unit = {
        println(s"""
         ID: ${document.getString("id")}
         Nombre: ${document.getString("nombre")}
         Apellidos: ${document.getString("apellidos")}
         NumLibros: ${document.getString("numLibros")}
        ---------------------------------------------""")
    }

    private def eliminarLibro(): Unit = {
        println("Introduce el ISBN del libro a eliminar")
        val isbn = StdIn.readLine()
        val filtro = new Document("isbn", isbn)
        val resultado = collection.deleteOne(filtro)

        if (resultado.getDeletedCount > 0) {
            println("\nLibro eliminado")
        } else {
            println("\nLibro no encontrado")
        }
    }

    private def eliminarAutor(): Unit = {
        println("Introduce el ID del autor a eliminar")
        val id = StdIn.readLine()
        val filtro = new Document("id", id)
        val resultado = collection.deleteOne(filtro)

        if (resultado.getDeletedCount > 0) {
            println("\nAutor eliminado")
        } else {
            println("\nAutor no encontrado")
        }
    }

    private def eliminarSeleccionado(): Unit = {
        println("Pulsa 1 para eliminar un libro o 2 para eliminar un autor")
        val opcion = StdIn.readLine()
        opcion match {
            case "1" => eliminarLibro()
            case "2" => eliminarAutor()
            case _ => println ("Opción no válida")
        }
    }

    private def pedirDatosLibro(): Libro = {
        println("Titulo: ")
        val titulo = StdIn.readLine()
        println("Nombre del autor: ")
        val nombre = StdIn.readLine()
        println("Apellidos del autor: ")
        val apellidos = StdIn.readLine()
        println("Anio: ")
        val anio = StdIn.readLine()
        println("Editorial: ")
        val editorial = StdIn.readLine()
        println("ISBN: ")
        val isbn = StdIn.readLine()

        // Buscar el autor por nombre y apellidos
        val autoresCollection = database.getCollection("autores")
        val filtro = new Document()
            .append("nombre", nombre)
            .append("apellidos", apellidos)
        val autorDoc = autoresCollection.find(filtro).first()
        val idAutor = if (autorDoc != null) autorDoc.getString("id")
        else {
            println("Autor no encontrado, quieres crearlo? (si/no)")
            if (StdIn.readLine().toLowerCase == "si") {
                // Hay que buscar el id más alto y aumentar el valor en 1
                val maxIdAutor = autoresCollection.find().sort(new Document("id", -1)).limit(1).first()
                val nuevoId = if (maxIdAutor != null) {
                    val idActual = maxIdAutor.getString("id").toInt
                    (idActual + 1).toString
                } else {
                    "1" // si no hay autores, nuevoId será 1 diréctamente en string
                }

                val nuevoAutor = new Document()
                    .append("id", nuevoId)
                    .append("nombre", nombre)
                    .append("apellidos", apellidos)
                    .append("numLibros", "0")
                autoresCollection.insertOne(nuevoAutor)
                println(s"Autor creado con ID: $nuevoId")
                nuevoId
            } else {
                println("No se puede crear el libro sin autor")
                return null // Salimos de la función si no se puede crear el libro
            }
        }
        val autorCompleto = s"$nombre $apellidos"
        Libro(titulo, idAutor, autorCompleto, anio, editorial, isbn)
    }

// UTILIZANDO JSON
    private def leerLibrosDeJSON (): Unit = {
        val rutaJSON = os.pwd / "libro.json"
        val contLibrosJSON = os.read(rutaJSON)
        val librosJSON = uread[List[Libro]](contLibrosJSON)
    /*  val jsValue = Json.parse(librosJSON)
        val librosLista: List[Libro] = jsValue.as[List[Libro]] 
        librosLista.foreach(libro => {  */
        librosJSON.foreach { libro => 
            val addLibrosToDB = new Document()
            .append("titulo", libro.titulo)
            .append("autor", libro.autor)
            .append("anio", libro.anio)
            .append("editorial", libro.editorial)
            .append("ISBN", libro.isbn)
            collection.insertOne(addLibrosToDB)
        }
    }

    private def leerAutoresDeJSON (): Unit = {
        val rutaJSON = os.pwd / "autor.json"
        val contAutoresJSON = os.read(rutaJSON)
        val autoresJSON = uread[List[Autor]](contAutoresJSON)
        autoresJSON.foreach { autor =>
            val addAutoresToDB = new Document()
            .append("id", autor.id)
            .append("nombre", autor.nombre)
            .append("apellidos", autor.apellidos)
            .append("numlibros", autor.numLibros)
            collection.insertOne(addAutoresToDB)
            }
    }


}
