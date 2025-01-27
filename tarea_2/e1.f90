! Interpolación de Lagrange
program e1

    implicit double precision (a-h, o-z) 
    dimension x(0:6), y(0:6)
    double precision p,prod,s,sum,fy,dfy,df

    ! Debemos de evaluar el calor especifico para el aumento en cada grado, 
    ! con lo cual comenzamos cargando los datos del archivo

    n=5 !Numero de datos en el archivo

    print *,'Temperature values'

    open(unit=10,file= 'cp.dat', status='OLD')
    do i=0,n
        read(10,*) x(i),y(i)
    enddo
    close(10)

    do i=0,n
        print *,i,x(i),y(i)
    enddo

    print *,'Lagrange interpolation'
    

    ! Lagrange interpolation iterartions for the first function


    open(unit=12,file='interpolation.dat')
    do k =0,100

        l = real(k)

        fy = f(l)

        print *,'y(',k,') = ' , fy

        write(12,*) k, fy
    
    end do 
    close(12)

    call system('gnuplot grafica.gp')


    print *,'Derivatives'

    ! Calculate derivatives in every point by finite diferences, with a first order aproximation

    open(unit=14,file='interpolation_derivative.dat')

    do k =0, 200

        l = real(k)*0.5

        fdy = df(l)

        print *,'dy(',k*0.5,') = ' , fdy
        write(14,*) k,fdy

    end do 
    close(14)
    call system('gnuplot grafica2.gp')

    ! Assuming smooth function with the minimum value between 0 an 100, we can create a simple function for every value

    do k=0,200
    if(abs(df(k)).lt.5E-003) then
        print *, 'valor mínimo de temperatura',k,f(k)
    endif
    enddo


end

!----- FUNCTIONS --------

! ----- Finite differences derivative ----

function df(k)
    implicit double precision (a-h, o-z)

    fk = f(k - 1)
    fh = f(k + 1)

    df = (fh-fk)/2

endfunction

! ----- LAGRANGE INTERPOLATION FUNCTION

function f(k)

    implicit double precision (a-h, o-z) 
    dimension x(0:6), y(0:6)
    double precision p,prod,s,sum,fy

    ! Debemos de evaluar el calor especifico para el aumento en cada grado, 
    ! con lo cual comenzamos cargando los datos del archivo

    n=5 !Numero de datos en el archivo

    open(unit=10,file= 'cp.dat', status='OLD')
    do i=0,n
        read(10,*) x(i),y(i)
    enddo
    close(10)
    
    s=0.
    sum=0.
    p=1.
    prod=1.

        ! Sumation part
        do i=0,n 
            p=1.
            prod=1.

            ! Product part
            do j=0,n
                if(j.ne.i) then
                    p = (k-x(j))/(x(i)-x(j))
                    prod = prod*p
                endif
            enddo
            s = prod * y(i)
            sum = sum + s
        enddo
    f= sum

endfunction


