program e3

implicit double precision (a-h, o-z) 
double precision m,fa,fb,fm
dimension x(0:100), fx(0:100)
    a=0.
    b=1.
    n=100 
    h=(b-a)/n 
    error=1.e-7 

    do i=0,n
        x(i)=a+h*i
        fx(i)=f1(x(i))
        !print *,i,x(i), fx(i) 
    end do 



    do i=0,n-1
        prod=fx(i)*fx(i+1)
        !print *, prod.LT.0.
        if(prod.LT.0.) then 
            print *, 'El valor de f1 se encuentra entre ',x(i), ' y ' , x(i+1)

            do k=0,n
                u = x(i)
                v = x(i+1)
                m = (u+v)/2
                fm = f1(m)
                fa = f1(u)
                fb = f1(v)

                if(abs(fm).LT.1.) then
                    print *, 'f1 vale',m
                    exit
                endif
                

                if(fm.gt.0.) then 

                    !print *, 'mayor ',b
                    v = m
                    !fb = fm
                    m = (u+v)/2.

                else
                    !print *, 'menor ',a
                    u = m
                    !fa = fm
                    m = (u+v)/2.
                endif
            end do

            xa=x(i+1)-fx(i+1)/((fx(i+1)-fx(i))/(x(i+1)-x(i)))
                do j=1,20
                    fa=f1(xa)
                    xb=x(i+1)-fx(i+1)/((fx(i+1)-fa)/(x(i+1)-xa))
                    !print *,j,xb,f1(xb)
                    if(abs(f1(xb)).lt.error) exit
                    xa=xb
                end do
        end if

    end do

    do i=0,n
        x(i)=a+h*i
        fx(i)=f2(x(i))
        !print *,i,x(i), fx(i) 
    end do 



    do i=0,n-1
        prod=fx(i)*fx(i+1)
        !print *, prod.LT.0.
        if(prod.LT.0.) then 
            print *, 'El valor de f2 se encuentra entre ',x(i), ' y ' , x(i+1)
            
            do k=0,n
                u = x(i)
                v = x(i+1)
                m = (u+v)/2
                fm = f1(m)
                fa = f1(u)
                fb = f1(v)

                if(abs(fm).LT.1.) then
                    print *, 'f2 vale',m
                    exit
                endif
                

                if(fm.gt.0.) then 

                    !print *, 'mayor ',b
                    v = m
                    !fb = fm
                    m = (u+v)/2.

                else
                    !print *, 'menor ',a
                    u = m
                    !fa = fm
                    m = (u+v)/2.
                endif
            end do

            xa=x(i+1)-fx(i+1)/((fx(i+1)-fx(i))/(x(i+1)-x(i)))
                do j=1,20
                    fa=f1(xa)
                    xb=x(i+1)-fx(i+1)/((fx(i+1)-fa)/(x(i+1)-xa))
                    !print *,j,xb,f1(xb)
                    if(abs(f2(xb)).lt.error) exit
                    xa=xb
                end do
        end if

    end do
end

function f1(x)
    implicit double precision (a-h, o-z) 
    f1= 1.14 - 2.0 * LOG10((0.0025/0.1)+(9.35/(30000.*sqrt(x)))) - 1/sqrt(x)
endfunction


function f2(x)
    implicit double precision (a-h, o-z) 
    f2= 1.14 - 2.0 * LOG10((0.0001/0.1)+(9.35/(35000000.*sqrt(x)))) - 1/sqrt(x)
endfunction
