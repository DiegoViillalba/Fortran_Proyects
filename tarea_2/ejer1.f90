! Programa del primer ejercicio de la segunda tarea. Hernández Cuevas Jonathan Alexis
program ejer1

    implicit double precision (a-h, o-z) 
    dimension x(0:6), y(0:6)
    double precision p,prod,s,sum,fy,dfy,df

    ! Cargamos los datos del archivo cp.dat
    n=5 ! Número de datos
    print *,'Temperature values'
    open(unit=8,file= 'cp.dat', status='OLD')
    ! Inicializamos ciclo do para la lectura
    do i=0,n
        read(8,*) x(i),y(i)
    enddo
    close(8)
    ! Inicializamos ciclo do para la lectura
    do i=0,n
        print *,i,x(i),y(i)
    enddo

    print *,'Lagrange interpolation'
    

    ! Realizamos las interpolaciones de Lagrange para la primera función
    open(unit=10,file='interpolation_ja.dat')
    do k =0,100
        l = real(k)
        fy = f(l)
        print *,'y(',k,') = ' , fy
        write(10,*) k, fy
    end do 
    close(10)
    
! Realizamos la grafica
    call system('gnuplot grafica.gp')
    print *,'Derivatives'

    ! Calculamos las derivadas en cada punto de primer orden
    open(unit=12,file='interpolation_derivative_ja.dat')
    do k =0, 100
        l = real(k)
        fdy = df(l)
        print *,'dy(',k,') = ' , fdy
        write(12,*) k,fdy
    end do 
    close(12)
! Realizamos la grafica
    call system('gnuplot grafica2.gp')

! Creamos una funcion simple para cada valor, la cual tenga el valor minimo, por planteamiento esta entre el 0 y el 100
    do k=0,100
    if(abs(df(k)).lt.5E-003) then
        print *, 'valor mínimo de temperatura',k,f(k)
    endif
    enddo
end

! Realizamos un apartado donde se encuentren las funciones a ocupar en el desarrollo de nuestro programa

! Definimos las derivadas finitas

function df(k)
    implicit double precision (a-h, o-z)
    fk = f(k - 1)
    fh = f(k + 1)
    df = (fh-fk)/2
endfunction

! Definimos la funcion de interpolacion de lagrange

function f(k)
    implicit double precision (a-h, o-z) 
    dimension x(0:6), y(0:6)
    double precision p,prod,s,sum,fy

    ! Se evalua el calor especifico para el aumento en cada grado

    n=5 ! Numero de datos

    open(unit=8,file= 'cp.dat', status='OLD')
    do i=0,n
        read(8,*) x(i),y(i)
    enddo
    close(8)
    s=0.
    sum=0.
    p=1.
    prod=1.

        ! Sumamos por partes
        do i=0,n 
            p=1.
            prod=1.

            ! Se realiza un producto por partes
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