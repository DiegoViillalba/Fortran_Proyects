PROGRAM e3
  IMPLICIT NONE
  REAL :: time(0:10), velocity(0:10), distance, h
  INTEGER :: i, n

  ! Datos de tiempo y velocidad
  DO i = 0, 10
    time(i) = REAL(i)          ! Tiempo en segundos
    velocity(i) = REAL(i)       ! Velocidad en m/s (v = t en este caso)
  END DO

  ! Número de intervalos y tamaño del paso
  n = 10
  h = 1.0    ! Intervalo de tiempo en segundos

  ! Calcular la distancia usando el método de Simpson
  distance = 0.0
  DO i = 0, n, 2
    IF (i == 0 .OR. i == n) THEN
      distance = distance + velocity(i)
    ELSE IF (MOD(i, 2) == 0) THEN
      distance = distance + 2.0 * velocity(i)
    ELSE
      distance = distance + 4.0 * velocity(i)
    END IF
  END DO

  distance = distance * h / 3.0

  PRINT *, 'La distancia total recorrida por el automóvil es: ', distance, ' metros'

END