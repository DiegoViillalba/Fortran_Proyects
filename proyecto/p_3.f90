! Proyecto : Ecuación secular de un sistema de perturbaciones con cargas

! Diego Antonio Villalba Gonzalez
! Jonathan Alexis Hernández Cuevas
! Hirám Noé López Sánchez

! Programa : Analitic result

!!--- Instrucciones-----
! El programa calcula los valores propios (modos normales) del sistema de acuerdo al desarrollo analitico que mostramos.
program p_3
    implicit none
    real :: k, m, x1, x2, a, Q
    real :: omega1, omega2

    k = 5.0
    m = 1.0
    x1 = -2.0
    x2 = 2.0
    a = 1.0
    Q = 1.0

    omega1 = k / m
    omega2 = (3 * k + Q / (x2 - x1 + a)) / m

    print *, "El valor de (omega1)**2 es:", omega1
    print *, "El valor de (omega2)**2 es:", omega2

end program p_3

