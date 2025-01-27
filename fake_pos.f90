program fake_pos

implicit double precision (a-h, o-z) 
dimension x(0:100), fx(0:100)
    a=0.
    b=1.
    n=100 
    h=(b-a)/n 
    error=1.e-7 

    do i=0,n
        x(i)=a+h*i
        fx(i)=fun(x(i))
        print *,i,x(i), fx(i) 
    end do 

    do i=0,n-1
   
        prod=fx(i)*fx(i+1)
        !print *, prod.LT.0.
        if(prod.LT.0.) then 
            print *, 'entre ',x(i), ' y ' , x(i+1), '  Hay una raiz'
            xa=x(i+1)-fx(i+1)/((fx(i+1)-fx(i))/(x(i+1)-x(i)))
                do j=1,20
                    fa=fun(xa)
                    xb=x(i+1)-fx(i+1)/((fx(i+1)-fa)/(x(i+1)-xa))
                    print *,j,xb,fun(xb)
                    if(abs(fun(xb)).lt.error) exit
                    xa=xb
                end do
        end if

    end do
end

function fun(x)
    implicit double precision (a-h, o-z) 
    fun = -19.*(x-0.5)*(x-1.)+exp(x)-exp(-2.*x)

endfunction

