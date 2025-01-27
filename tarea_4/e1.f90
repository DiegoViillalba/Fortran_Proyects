PROGRAM e1
  IMPLICIT NONE
  REAL :: v0, v1, step, distance
  INTEGER :: n

  ! Valores iniciales
  v0 = 50.0     ! Velocidad inicial en m/s
  v1 = 15.0     ! Velocidad final en m/s (primer caso)
  n = 1000      ! Número de intervalos
  step = (v1 - v0) / REAL(n)

  ! Cálculo de la distancia recorrida hasta que v = 15 m/s
  distance = SimpsonIntegration(v0, v1, step, n)
  PRINT *, 'Distancia recorrida hasta 15 m/s: ', distance, ' m'

  ! Cálculo de la distancia recorrida hasta detenerse (v = 0)
  v1 = 0.0      ! Actualizar la velocidad final a 0 m/s
  step = (v1 - v0) / REAL(n)
  distance = SimpsonIntegration(v0, v1, step, n)
  PRINT *, 'Distancia recorrida hasta detenerse: ', distance, ' m'

CONTAINS

  FUNCTION f(v) RESULT(dx)
    REAL :: v, dx
    dx = -5400.0 * v / (8.276 * v**2 + 2000.0)
  END FUNCTION f

  FUNCTION SimpsonIntegration(v0, v1, step, n) RESULT(dist)
    REAL :: v0, v1, step, dist
    INTEGER :: n, i
    REAL :: v

    dist = 0.0
    DO i = 0, n
      v = v0 + i * step
      IF (i == 0 .OR. i == n) THEN
        dist = dist + f(v)
      ELSE IF (MOD(i, 2) == 0) THEN
        dist = dist + 2.0 * f(v)
      ELSE
        dist = dist + 4.0 * f(v)
      END IF
    END DO

    dist = dist * step / 3.0
  END FUNCTION SimpsonIntegration

END