// https://en.wikipedia.org/wiki/ISO_3166-1#Officially_assigned_code_elements
// cargamos jQuery
var script = document.createElement("script");
script.src = "https://code.jquery.com/jquery-3.2.1.min.js"; // Aquí había un error en la sintaxis, con una comillas de más y le faltaba un punto y coma.
document.body.appendChild(script);

// cargamos D3
var script = document.createElement("script");
script.src = "//d3js.org/d3.v4.min.js";
document.body.appendChild(script);

var filas = $("#mw-content-text > div > table:nth-child(29) tbody tr");
// filas almacena todas las filas de la tabla en forma de array

// "#mw-content-text > div > table:nth-child(29) tbody tr" es un selector CSS. Si en algún momento la página web
// cambia su estructura puede que este selector quede desactualizado.

console.log("La tabla tiene: " + filas.length + " filas.");
// Si por ejemplo en este log veis el mensaje "La tabla tiene 0 filas" efectivamente el selector no está funcionando.

// Para utilizar un selector actualizado podéis utilizar las herramientas de desarrollador de Chrome como
// aparece en la última imagen del enunciado de la actividad.
// En el panel 'Elements' de las herramientas de desarrollador de Chrome
// tenéis que posicionaros sobre el elemento sobre el cual queráis obtener el selector → botón derecho → Copy → Copy Selector.
// El selector se habr copiado a vuestro portapapeles. Solo os quedará actualizar la variable filas con el contenido.

// Creamos un array vacío que iremos rellenando con el método push
// en cada iteración
var csv = [];

// Como la variable filas es un array podemos recorrerla con un bucle for
for (var i = 0; i < filas.length; i++) {
  // filas[i] vale cada fila en cada iteración

  var fila = $(filas[i]).find("td");
  // Con el método find de jQuery accedemos a
  // todos los hijos 'td' que tiene la fila
  // De nuevo obtenemos un array
  // Podremos acceder a su contenido mediante el índice

  var nombre = $(fila[0]).text().trim();
  // Con el método .text() extraemos el contenido del elemento 'td'
  // fila[0] apunta a la primera columna de la fila
  // fila[1] apunta a la segunda, etc.

  var alpha2 = $(fila[1]).text().trim();
  var alpha3 = $(fila[2]).text().trim();
  var numericCode = $(fila[3]).text().trim();
  var subdivision = $(fila[4]).text().trim();
  var independiente = $(fila[5]).text().trim();

  if (independiente === "Yes") {
    //====> Coloque aqui el if para filtrar los países que cumplan con el requerimiento  antes de ser almacendo en el push.

    csv.push({
      Nombre: nombre,
      "Código Alpha-2": alpha2,
      "Código Alpha-3": alpha3,
      CodigoNumerico: numericCode,
      Subdivision: subdivision,
      Independiente: independiente
    });
  }

  // Aquí hemos creado un Objeto javascript, un JSON
  // Pares nombre-valor ("Nombre":nombre) nombre es la variable arriba declarada
  // Los nombres a la izquierda ("Nombre,"Código Alpha-2")
  // serán los nombres de las columnas
  // Con 'push' añadimos una nueva fila al array en cada iteración
}

var csvFormatted = d3.csvFormat(csv);
// con este método de la librería D3 transformamos el array 'csv'
// al formato csv.

copy(csvFormatted);
// Con este método copiamos la variable al portapapeles
// Abre un archivo de texto vacío y pega el contenido
// Asígnale extensión csv.
