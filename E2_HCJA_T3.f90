program tercer_ejercico
    implicit double precision (a-h,o-z)
    real t(0:100), x(0:100)
    double precision h
    integer i

    ! Definimos el paso y las condiciones iniciales
    h = 0.01
    x(0) = 0.0    ! Condición inicial: x(0)
    x(1) = 0.0    ! Condición inicial: x'(0)

    ! Calculamos los valores de t
    do i = 0, 100
        t(i) = i * h
    end do

    ! Abrimos el archivo para guardar los resultados
    open(unit=8, file='resultado_ecuacion.dat')
    write(8, *) t(0), x(0)
    write(8, *) t(1), x(1)

    ! Iteramos usando diferencias finitas
    do i = 1, 99
        x(i+1) = (2.0 + (2.0/h)) * x(i) - (1.0 + (2.0/h)) * x(i-1) + h*h * (t(i)**2 - 1.0)
        write(8, *) t(i+1), x(i+1)
    end do

    ! Cerramos el archivo
    close(8)
end program tercer_ejercico
