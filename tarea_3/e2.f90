program e2
    implicit double precision (a-h,o-z)
    real t(0:100), x(0:100)
    double precision h
    integer i

    ! Initial condictions
    h = 0.01
    x(0) = 0.0    
    x(1) = 0.0   

    ! time step
    do i = 0, 100
        t(i) = i * h
    end do

    ! Begin numerical analisis
    open(unit=8, file='e2.dat')
    write(8, *) t(0), x(0)
    write(8, *) t(1), x(1)

    ! Finite differences
    do i = 1, 99
        x(i+1) = (2.0 + (2.0/h)) * x(i) - (1.0 + (2.0/h)) * x(i-1) + h*h * (t(i)**2 - 1.0)
        write(8, *) t(i+1), x(i+1)
        print *,t(i+1), x(i+1)
    end do

    ! Cerramos el archivo
    close(8)
    call system('gnuplot grafica3.gp')
end 
