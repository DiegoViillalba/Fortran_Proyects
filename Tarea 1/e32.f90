program e32
  implicit double precision (a-h, o-z) 
  dimension x(0:100), fx(0:100)
  double precision m,fa,fb,fm

    a = 0.
    b = 0.2
    n = 10000
    error=1.e-7 
    m = (a+b)/2.
    

    do i=0,n
        
        fm = f1(m)
        fa = f1(a)
        fb = f1(b)

        !print *, 'fm',fm
        !print *, 'm',m

        if(abs(fm).LT.error) then
            print *, 'f1 ',m
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

    do i=0,n
        
        fm = f2(m)
        fa = f2(a)
        fb = f2(b)

        print *, 'fm',fm
        print *, 'm',m

        if(abs(fm).LT.error) then
            print *, 'f2 ',m
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

function f1(x)
    implicit double precision (a-h, o-z) 
    f1= 1.14 - 2.0 * LOG10((0.0025/0.1)+(9.35/(30000.*sqrt(x)))) - 1/sqrt(x)
endfunction

function f2(x)
    implicit double precision (a-h, o-z) 
    f2= 1.14 - 2.0 * LOG10((0.0001/0.1)+(9.35/(35000000.*sqrt(x)))) - 1/sqrt(x)
endfunction