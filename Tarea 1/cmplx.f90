program cmplx
    COMPLEX z,v,c,f
    v = (1,0)
    z = (2.,0.)

    c = f(z)

    print *,'result',c

end

function f(z)
    !implicit double precision (a-h, o-z) 
    COMPLEX f,z
    !f = 2 - x + 2* (x**2) + (x**4)
    f = z**2 -1

endfunction


