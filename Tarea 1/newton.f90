PROGRAM newton
  IMPLICIT NONE
  COMPLEX :: z0, z, f, df
  COMPLEX, DIMENSION(:), ALLOCATABLE :: coef
  INTEGER :: i, max_iter, n
  REAL :: tol
  LOGICAL :: converged

  ! Parámetros del algoritmo
  max_iter = 1000  ! Número máximo de iteraciones
  tol = 1.0e-6     ! Tolerancia para la convergencia
  
  ! Grado del polinomio
  PRINT *, 'Introduce el grado del polinomio:'
  READ *, n
  ALLOCATE(coef(0:n))
  
  ! Leer los coeficientes del polinomio
  PRINT *, 'Introduce los coeficientes del polinomio (parte real y parte imaginaria):'
  DO i = 0, n
     PRINT *, 'Coeficiente de x^', i, ':'
     READ *, coef(i)
  END DO

  ! Valor inicial para Newton
  PRINT *, 'Introduce el valor inicial (parte real y parte imaginaria):'
  READ *, z0
  
  z = z0
  converged = .FALSE.

  ! Iterar usando el método de Newton
  DO i = 1, max_iter
     CALL polinomio(z, coef, n, f, df)
     IF (ABS(f) < tol) THEN
        converged = .TRUE.
        EXIT
     END IF
     z = z - f / df
  END DO
  
  IF (converged) THEN
     PRINT *, 'La raíz encontrada es: ', z
  ELSE
     PRINT *, 'No se encontró convergencia en ', max_iter, ' iteraciones.'
  END IF

  CONTAINS

    ! Subrutina para evaluar el polinomio y su derivada
    SUBROUTINE polinomio(z, coef, n, f, df)
      COMPLEX, INTENT(IN) :: z
      COMPLEX, DIMENSION(0:n), INTENT(IN) :: coef
      COMPLEX, INTENT(OUT) :: f, df
      INTEGER, INTENT(IN) :: n
      INTEGER :: i

      f = coef(n)
      df = 0.0
      DO i = n-1, 0, -1
         df = df * z + f
         f = f * z + coef(i)
      END DO
    END SUBROUTINE polinomio

END PROGRAM newton
