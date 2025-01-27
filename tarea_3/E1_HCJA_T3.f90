!Derivada numérica segundo orden
program e1
    implicit double precision(a-h,o-z)  ! Definimos las variables de tipo doble precisión
    real, dimension (0:5) :: t, f, df, E  ! Creamos arreglos acorde a las necesidades, como el tiempo, función, derivada y campo eléctrico
    real :: L  ! Definimos la inductancia
    integer :: R, n, i  ! Definimos la resistencia y el número de puntos
    real :: dt  ! Definimos la variacion del tiempo

    n = 4  ! Número de puntos a usar para la derivada numérica
    dt = 0.1  ! Definimos valor de tiempo (delta t)
    L = 0.05  ! Definimos el valor de la inductancia
    R = 2  ! Definimos el valor de la resistencia

    ! Leemos el archivo con los datos de corriente (tiempo y función)
    open(unit=69, file= 'datos_corriente.dat', status='OLD')
    do i=0, n
        read(69,*) t(i), f(i)  ! Leemos los datos de tiempo y función desde el archivo
    enddo
    close(69)

    ! Calculamos la derivada numérica para los puntos interiores
    do i=1, n-1
        df(i) = (f(i+1) - f(i-1)) / (2.0 * dt)
    enddo

    ! Calculamos la derivada para el primer y último punto (usando fórmulas de diferencia finita)
    df(0) = (-3.0 * f(0) + 4.0 * f(1) - f(2)) / (2.0 * dt)
    df(n) = (3.0 * f(n) - 4.0 * f(n-1) + f(n-2)) / (2.0 * dt)

    ! Imprimimos los valores de tiempo, función y derivada
    print *, "  t        f(t)        df/dt"
    do i=0, n
        print '(F6.2, 5X, F6.4, 5X, F8.4)', t(i), f(i), df(i)
    enddo

    ! Calculamos el campo eléctrico usando la ecuación: E(t) = L * df/dt + R * f(t)
    do i=0, n
        E(i) = (L * df(i)) + (R * f(i))
    enddo

    ! Imprimimos los valores del campo eléctrico
    print *, 'Tabla de valores del campo electrico'
    print *, "  t          E(t)"
    do i=0, n
        print '(F6.2, 5X, F8.4)', t(i), E(i)
    enddo

end program segundo_ejercicio
