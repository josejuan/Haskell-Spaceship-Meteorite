# Spaceship Meteorite (Haskell + GLUT)

Muy divertido el programarlo y muy didáctico para percibir mejor un lenguaje.

Algunas características:

* Como única dependencia relevante GLUT (OpenGL).
* Va en Linux y Windows (aunque no está _"cabalizado"_).
* La gestión de objetos e interrelaciones incluida IO (ej. teclado) está abstraído (no está _"a capón"_).
* El cálculo de la dinámica aunque simple (Euler) es _"realista"_ con medidas estandar (m, sg, newton, ...).
* El cálculo de colisiones es un sencillo pero efectivo _sweep_.
* La creación de partículas también está abstraído, aunque sólo se implementa el _"spray"_.

Algunas cuestiones/mejoras:

* Aunque se usan _lens_, puede hacerse un uso mucho mejor.
* El uso de la mónada _StateT_ junto con _lens_ puede mejorar bastante el código.

[[SpaceShipMeteorite.gif]]
![Spaceship sample video](/SpaceShipMeteorite.gif?raw=true "Sample video")

https://youtu.be/_-XpkepoDhQ


