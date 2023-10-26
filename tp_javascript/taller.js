// Ejercicio 1
function ejercicio1() {
  Malambo = undefined;
}

// Ejercicio 2
function ejercicio2() {
  nuevoRobot = undefined;

  Chacarera = undefined;
}

// Ejercicio 3
function ejercicio3() {

}

// Ejercicio 4
function ejercicio4() {
  Robot = undefined;
}

// Ejercicio 5
function ejercicio5() {
  Milonga = undefined;
}

// Ejercicio 6
function ejercicio6() {

}

// Ejercicio 7
function ejercicio7() {

}

// Editen esta función para que devuelva lo que quieran mostrar en la salida.
function calcularResultado() {
  let res = "";
  res += "<b>Ejercicio 1</b>\n" + crearTest(1, testEjercicio1)();
  res += "\n";
  res += "<b>Ejercicio 2</b>\n" + crearTest(2, testEjercicio2)();
  res += "\n";
  res += "<b>Ejercicio 3</b>\n" + crearTest(3, testEjercicio3)();
  res += "\n";
  res += "<b>Ejercicio 4</b>\n" + crearTest(4, testEjercicio4)();
  res += "\n";
  res += "<b>Ejercicio 5</b>\n" + crearTest(5, testEjercicio5)();
  res += "\n";
  res += "<b>Ejercicio 6</b>\n" + crearTest(6, testEjercicio6)();
  res += "\n";
  res += "<b>Ejercicio 7</b>\n" + crearTest(7, testEjercicio7)();
  return res;
}

// Agreguen aquí los tests representados como funciones que toman un objeto res como argumento.
  // Pueden llamar a res.write para escribir en la salida
  // Pueden llamar a res.test para
    // probar que una condición se cumple (pasándole la condición como único argumento)
    // o para probar que dos valores son iguales (pasándole dos argumentos)

// Test Ejercicio 1
function testEjercicio1(res) {
  res.write("Nombre de Malambo: " + Malambo.nombre);
  res.test(Malambo.nombre, "Malambo");
  res.write("Peso de Malambo: " + Malambo.peso);
  res.test(Malambo.peso, 500);
}

// Test Ejercicio 2
function testEjercicio2(res) {
  res.write("Nombre de Chacarera: " + Chacarera.nombre);
  res.test(Chacarera.nombre, "Chacarera");
  res.write("Peso de Chacarera: " + Chacarera.peso);
  res.test(Chacarera.peso, 1500);
  res.test(!Chacarera.isPrototypeOf(Malambo));
  res.test(Malambo.isPrototypeOf(Chacarera));
}

// Test Ejercicio 3
function testEjercicio3(res) {
  res.write("Presentación de Malambo: " + Malambo.presentarse());
  res.test(Malambo.presentarse(), "Hola, soy Malambo y me encanta limpiar.");
  res.write("Presentación de Chacarera: " + Chacarera.presentarse());
  res.test(Chacarera.presentarse(), "Hola, soy Chacarera y me encanta cortar el pasto.");

  res.write("--");
  let peso_malambo = Malambo.peso;
  res.write("Peso de Malambo antes de limpiar: " + peso_malambo);
  res.test(Malambo.limpiar(), "limpiar");
  peso_malambo ++;
  res.write("Peso de Malambo después de limpiar: " + peso_malambo);
  res.test(Malambo.peso, peso_malambo);
}

// Test Ejercicio 4
function testEjercicio4(res) {
  let C = new Robot("C", 100, "..", function(){return null});
  res.test(C.presentarse(), "Hola, soy C y me encanta ...");
  res.test(!Malambo.isPrototypeOf(C));
  res.test(Robot.prototype.isPrototypeOf(C));

  let malambo_sabe_limpiar = "limpiar" in Malambo
  let chacarera_sabe_limpiar = "limpiar" in Chacarera
  let A = nuevoRobot("A", 250, ".");
  let A_sabe_limpiar = "limpiar" in A;
  let B = new Robot("B", 250, ".", function(){return null});
  let B_sabe_limpiar = "limpiar" in B;
  res.write("Malambo" + si_o_no(malambo_sabe_limpiar) + "sabe limpiar.");
  res.write("Chacarera" + si_o_no(chacarera_sabe_limpiar) + "sabe limpiar.");
  res.write("Si creo un nuevo muñeco con la función original, este" + si_o_no(A_sabe_limpiar) + "sabe limpiar.");
  res.write("Si creo un nuevo muñeco con la función constructora, este" + si_o_no(B_sabe_limpiar) + "sabe limpiar.");
  res.test(malambo_sabe_limpiar);
  res.test(chacarera_sabe_limpiar);
  res.test(A_sabe_limpiar);
  res.test(!B_sabe_limpiar);
}

// Test Ejercicio 5
function testEjercicio5(res) {
  res.write("Peso de Malambo: " + Malambo.peso);
  res.write("Peso de Chacarera: " + Chacarera.peso);
  let peso_malambo = Malambo.peso;
  let peso_chacarera = Chacarera.peso;
  let resultado_1 = Milonga.mensajear(Malambo,Chacarera,"limpiar")
  res.write("Resultado Malambo -> limpiar -> Chacarera: " + resultado_1);
  res.test(resultado_1, "limpiar");
  peso_malambo++;
  peso_chacarera++;
  res.write("Peso de Malambo: " + Malambo.peso);
  res.write("Peso de Chacarera: " + Chacarera.peso);
  res.test(Malambo.peso, peso_malambo);
  res.test(Chacarera.peso, peso_chacarera);

  res.write("--");
  let A = new Robot("A", 1, "fA", function(){return "fB";})
  let B = new Robot("B", 1, "fB", function(){return "fC";})
  let resultado_2 = Milonga.mensajear(A, B, "fA")
  res.write("Resultado A -> fA -> B: " + resultado_2);
  let resultado_3 = Milonga.mensajear(A, B, "fB")
  res.write("Resultado A -> fB -> B: " + resultado_3);
  let resultado_4 = Milonga.mensajear(B, A, "fA")
  res.write("Resultado B -> fA -> A: " + resultado_4);
  res.test(resultado_2, "fA");
  res.test(resultado_3, "fC");
  res.test(resultado_4, "fC");
}

// Test Ejercicio 6
function testEjercicio6(res) {
  let C = new RobotMensajero("C", 1000, "..", function(){return null});
  res.test(C.presentarse(), "Hola, soy C y me encanta ...");
  res.test(!Milonga.isPrototypeOf(C));
  res.test(RobotMensajero.prototype.isPrototypeOf(C));
  let peso_malambo = Malambo.peso;
  let peso_chacarera = Chacarera.peso;
  let resultado_1 = C.mensajear(Malambo,Chacarera,"limpiar")
  res.write("Resultado Malambo -> limpiar -> Chacarera: " + resultado_1);
  res.test(resultado_1, "limpiar");
  peso_malambo++;
  peso_chacarera++;
  res.write("Peso de Malambo: " + Malambo.peso);
  res.write("Peso de Chacarera: " + Chacarera.peso);
  res.test(Malambo.peso, peso_malambo);
  res.test(Chacarera.peso, peso_chacarera);
  res.test(C.hasOwnProperty("mensajear"), false);
}

// Test Ejercicio 7
function testEjercicio7(res) {
  let A = new Robot("A", 0.2, "a", function(){return "a"});
  let B = new Robot("B", 0.3, "b", function(){return "b"});
  let C = new Robot("C", 0.3, "c", function(){return "c"});

  res.test("a" in A);
  res.write("Directiva de A antes de cambiar su directiva a \"a\": " + A.directiva);
  A.reprogramar("a");
  res.test(A.directiva, "...");
  res.assert(!("a" in A), "A no debería tener el método \"a\" después de reprogramar.");
  res.write("Directiva de A después de cambiar su directiva a \"a\": " + A.directiva);

  res.write("Directiva de A antes de ayudar a B: " + A.directiva);
  res.write("Ayudante de B antes de pedir ayuda a A: " + nombre(B.ayudante));
  res.assert(!("b" in A), "A no debería tener el método \"b\" después de reprogramar.");
  B.solicitarAyuda(A);
  res.assert("b" in A, "A debería tener el método \"b\" después de de que B le pide ayuda.");
  res.test(A.b(), "b");
  res.write("Directiva de A después de ayudar a B: " + A.directiva);
  res.write("Ayudante de B después de pedir ayuda a A: " + nombre(B.ayudante));

  res.write("--");

  res.write("Directiva de C antes de ayudar a B: " + C.directiva);
  res.write("Ayudante de B antes de pedir ayuda a C: " + nombre(B.ayudante));
  res.write("Ayudante de A antes de que B le pida ayuda a C: " + nombre(A.ayudante));
  res.test(B.ayudante, A);
  res.test(!("ayudante" in A));
  res.test(A.directiva, "b");
  B.solicitarAyuda(C);
  res.write("Directiva de C después de ayudar a B: " + C.directiva);
  res.write("Ayudante de B después de pedir ayuda a C: " + nombre(B.ayudante));
  res.write("Ayudante de A después de que B le pida ayuda a C: " + nombre(A.ayudante));
  res.test(B.ayudante, A);
  res.test(A.ayudante, C);
  res.assert(!("ayudante" in C),"C no debería tener ayudante.");
  res.test(A.directiva, "b");
  res.test(C.directiva, "b");

  res.write("--");

  let malambo_sabe_pedir_ayuda = "solicitarAyuda" in Malambo
  let chacarera_sabe_limpiar = "limpiar" in Chacarera
  let chacarera_sabe_pedir_ayuda = "solicitarAyuda" in Chacarera
  let chacarera_sabe_presentarse = "presentarse" in Chacarera
  res.write("Malambo" + si_o_no(malambo_sabe_pedir_ayuda) + "sabe pedir ayuda.");
  res.write("Chacarera" + si_o_no(chacarera_sabe_limpiar) + "sabe limpiar.");
  res.write("Chacarera" + si_o_no(chacarera_sabe_pedir_ayuda) + "sabe pedir ayuda.");
  res.write("Chacarera" + si_o_no(chacarera_sabe_presentarse) + "sabe presentarse.");
  res.test(malambo_sabe_pedir_ayuda);
  res.test(chacarera_sabe_pedir_ayuda);
}

function nombre(objeto) {
  if (objeto) return objeto.nombre;
  return "Ninguno";
}

function si_o_no(bool) {
  return (bool ? " " : " <b>NO</b> ")
}

// Función auxiliar que crea un test genérico a partir de un número i y una función f
function crearTest(i, f) {
  return function() {
    eval("ejercicio"+i)();
    let res = {
      text:"",
      write: function(s) {
        this.text += s + "\n";
      },
      test: function(actual, expected) {
        if (expected !== undefined) {
          if (actual !== expected) {
            fail(i, "Se esperaba " + expected + " pero se obtuvo: " + actual)}
        } else {
          if (actual !== true) {
            fail(i, "Falló la condición del test.")
          }
        }
      },
      assert: function(actual, message) {
        if (actual !== true) {
          fail(i, "Falló la condición del test: " + message);
        }
      }
    };
    try {
      f(res);
    } catch (e) {
      fail(i, e);
    }
    return res.text;
  }
}

let Malambo = undefined
let nuevoRobot = undefined
let Chacarera = undefined
let Robot = undefined
let Milonga = undefined
