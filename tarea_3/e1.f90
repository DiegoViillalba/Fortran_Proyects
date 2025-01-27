!Derivada numérica segundo orden
program e1
    ! We declare the variabes, continuos and vector like
    implicit double precision(a-h,o-z) 
    real, dimension (0:5) :: t, f, df, E  ! Current, Electric field
    real :: L , dt ! Indcuctance, time diferential
    integer :: R, n, i  ! Resistance

    ! Initial conditions 

    n = 4  ! Points
    dt = 0.1  
    L = 0.05  
    R = 2 

    ! Read file
    open(unit=10, file= 'datos_corriente.dat', status='OLD')
    do i=0, n
        read(10,*) t(i), f(i)
    enddo
    close(10)

    print *,'Current values'

    do i=0,n
        print *,i,t(i),f(i)
    enddo

    !Finite differences
    df(0) = (-3.0 * f(0) + 4.0 * f(1) - f(2)) / (2.0 * dt)
    df(n) = (3.0 * f(n) - 4.0 * f(n-1) + f(n-2)) / (2.0 * dt)

    !  derivative
    do i=1, n-1
        df(i) = (f(i+1) - f(i-1)) / (2.0 * dt)
    enddo

    open(unit=14,file='e1_1.dat')

    print *, "  t        f(t)        df(t)"
    do i=0, n
        print *, t(i), f(i), df(i)
        write(14,*) t(i),f(i), df(i)

    end do 
    close(14)

    call system('gnuplot grafica.gp')

    ! Electric field E(t) = L * df/dt + R * f(t)
    open(unit=15,file='e1_2.dat')
    do i=0, n
        E(i) = (L * df(i)) + (R * f(i))
        write(15,*) t(i),E(i)
    end do 
    close(15)

    call system('gnuplot grafica2.gp')


    ! Electric field
    print *, 'Tabla de valores del campo electrico'
    print *, "  t          E(t)"
    do i=0, n
        print *, t(i), E(i)
    enddo

end
