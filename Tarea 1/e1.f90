program e1
  implicit double precision (a-h, o-z) 
  dimension x(0:100), fx(0:100)
  double precision m,fa,fb,fm

    a = 0.
    b = 0.5
    n = 1000
    error=1.e-7 
    m = (a+b)/2.
    

    do i=0,n
        
        fm = f(m)
        fa = f(a)
        fb = f(b)

        !print *, 'fm',fm
        !print *, 'm',m

        if(abs(fm).LT.error) then
            print *, 'raiz ',m
            exit
        endif
        
        

        if(fm.gt.0.) then 

            !print *, 'mayor ',b
            b = m
            !fb = fm
            m = (a+b)/2.

        else
            !print *, 'menor ',a
            a = m
            !fa = fm
            m = (a+b)/2.
        endif
    end do
end

function f(x)
    implicit double precision (a-h, o-z) 
    f = (70 + (1.463 / x * x )) * (x - 0.0394 ) - (0.08314 * 215)

endfunction
