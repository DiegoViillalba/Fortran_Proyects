program e2

    COMPLEX z0,zn,fz,dfz,c,f,df,fzn
    double precision nm


    error=5.e-7 

    n=1000

    z0 = (-0.5,1.5)

    print *,'complex guess 1', z0

    do i=0 , n

        fz = f(z0)
        dfz = df(z0)

        c = fz/dfz
        zn = z0 - (c) 

        fzn = f(zn)

        nm = sqrt(real(fzn)**2 + aimag(fzn)**2)
        if(nm.lt.error) then
            print *, 'root 1', zn, nm
            exit
        endif

        z0 = zn

    end do

    z0 = (0.5,-0.7)

    print *,'complex guess 2', z0

    do i=0 , n

        fz = f(z0)
        dfz = df(z0)

        c = fz/dfz
        zn = z0 - (c) 

        fzn = f(zn)

        nm = sqrt(real(fzn)**2 + aimag(fzn)**2)
        if(nm.lt.error) then
            print *, 'root 2', zn, nm
            exit
        endif

        z0 = zn

    end do

    z0 = (0.5,0.7)

    print *,'complex guess 3', z0

    do i=0 , n

        fz = f(z0)
        dfz = df(z0)

        c = fz/dfz
        zn = z0 - (c) 

        fzn = f(zn)

        nm = sqrt(real(fzn)**2 + aimag(fzn)**2)
        if(nm.lt.error) then
            print *, 'root 3', zn, nm
            exit
        endif

        z0 = zn

    end do

    z0 = (-0.5,-1.5)

    print *,'complex guess 4', z0

    do i=0 , n

        fz = f(z0)
        dfz = df(z0)

        c = fz/dfz
        zn = z0 - (c) 

        fzn = f(zn)

        nm = sqrt(real(fzn)**2 + aimag(fzn)**2)
        if(nm.lt.error) then
            print *, 'root 4', zn, nm
            exit
        endif

        z0 = zn

    end do


end


function f(z)

    COMPLEX f,z
    f = 2 - z +( 2 * (z**2) )+ (z**4)

endfunction

function df(z)

    COMPLEX df,z
    
    df = - 1 + (4 * (z)) + 4*(z**3)


endfunction
