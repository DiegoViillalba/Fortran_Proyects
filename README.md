
Física Computacional - Proyectos en Fortran

Este repositorio contiene los códigos y análisis desarrollados en Fortran para diversos problemas numéricos en el marco del curso de Física Computacional. Los proyectos abarcan desde interpolación numérica hasta el cálculo de eigenvalores, pasando por derivación, integración, y resolución de sistemas de ecuaciones diferenciales.

Contenido del Repositorio

1. Proyecto: Cálculo de la Ecuación Secular
	•	Descripción: Resolución numérica de la ecuación secular para un sistema de 3 resortes acoplados con cargas positivas.
	•	Técnicas utilizadas:
	•	Método de diferencias finitas para aproximar derivadas.
	•	Método de la potencia para cálculo de eigenvalores y eigenvectores.
	•	Archivos:
	•	p1.f90: Genera la matriz de potencial usando diferencias finitas.
	•	p2.f90: Calcula los eigenvalores y eigenvectores de la matriz usando el método de la potencia.
	•	Resultados:
	•	Modos normales obtenidos: ω₁ = 5 y ω₂ = 15.
	•	Comparación numérica vs. analítica con precisión alta.

2. Tarea 2: Interpolación Numérica
	•	Descripción: Implementación de los métodos de interpolación de Newton y Lagrange para analizar datos experimentales (temperatura específica del agua).
	•	Técnicas utilizadas:
	•	Interpolación polinómica.
	•	Derivación numérica para análisis de mínimos.
	•	Archivos:
	•	lagrange.f90: Interpolación de Lagrange.
	•	newton.f90: Interpolación de Newton.
	•	Resultados:
	•	Gráficas de las interpolaciones y diferencias entre métodos.
	•	Diferencia máxima entre Newton y Lagrange: ￼.

3. Tarea 3: Derivación Numérica
	•	Descripción: Uso de diferencias finitas para aproximar derivadas y resolver ecuaciones diferenciales.
	•	Técnicas utilizadas:
	•	Diferencias progresivas, regresivas y centradas.
	•	Métodos de orden 4 para mejorar precisión.
	•	Archivos:
	•	derivada.f90: Aproximación de derivadas de primer y segundo orden.
	•	ecuacion_diferencial.f90: Resolución numérica de una ecuación diferencial con condiciones iniciales.
	•	Resultados:
	•	Gráficas de las derivadas calculadas.
	•	Análisis del campo eléctrico en un sistema con resistencia e inductancia.

4. Tarea 4: Integración Numérica
	•	Descripción: Aplicación de métodos de integración como la regla de Simpson para resolver ecuaciones diferenciales y calcular áreas.
	•	Técnicas utilizadas:
	•	Regla de Simpson compuesta.
	•	Fórmulas de cuadratura para polinomios cúbicos.
	•	Archivos:
	•	simpson.f90: Implementación de la regla de Simpson para intervalos uniformes.
	•	Resultados:
	•	Cálculo de la distancia recorrida por un automóvil hasta detenerse.
	•	Demostración teórica y práctica de la regla 3/8 para polinomios cúbicos.

5. Tarea 5: Cálculo de Eigenvalores
	•	Descripción: Resolución del sistema de tres masas acopladas con muelles utilizando métodos numéricos.
	•	Técnicas utilizadas:
	•	Interpolación de Newton para encontrar el polinomio característico.
	•	Método de la potencia e inverso para cálculo de eigenvalores y eigenvectores.
	•	Archivos:
	•	eigenvalores.f90: Resolución del sistema de ecuaciones diferenciales mediante eigenvalores.
	•	Resultados:
	•	Modos normales obtenidos: ￼, ￼, ￼.

Requisitos
	•	Compilador Fortran: GNU Fortran (gfortran) o similar.
	•	Herramientas adicionales: gnuplot para generar gráficas.

Instrucciones de Uso
	1.	Clona el repositorio:

git clone https://github.com/tu-usuario/fortran-fisica-computacional.git
cd fortran-fisica-computacional


	2.	Compila el programa deseado:

gfortran nombre_programa.f90 -o nombre_programa


	3.	Ejecuta el programa:

./nombre_programa


	4.	(Opcional) Usa gnuplot para graficar resultados.

Autor

Desarrollado por Diego Antonio Villalba González como parte del curso de Física Computacional en la Facultad de Ciencias, UNAM.

¿Quieres agregar algo más, como un enlace a los PDFs de las tareas o secciones específicas? 😊
