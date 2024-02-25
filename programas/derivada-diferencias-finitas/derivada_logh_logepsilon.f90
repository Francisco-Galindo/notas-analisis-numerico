Program derivada
        IMPLICIT NONE
        REAL (KIND=8) x, h, dfdx, f, dfdxreal, epsilon_t ! Números flotantes de 4 bytes
        INTEGER i

        OPEN(UNIT=21, FILE='logh-vs-log-epsilon.dat',STATUS='replace', ERR=500)

        x=2.d0 !'d0' denota doble precisión
        h=1.d0
        dfdxreal = 11.56898065039286d0

        DO i = 1, 100, 1
                h = h * (0.99d0)**(i - 1)
                dfdx = (f(x + h) - f(x)) / h ! Derivada numérica
                epsilon_t = ABS((dfdx - dfdxreal) / dfdxreal)
                WRITE(21,*) i, ",", h, ",", dfdx, ",", epsilon_t
        END DO

        CLOSE(UNIT=21, STATUS='keep', ERR=500)

500 End Program derivada

FUNCTION f(x)
        IMPLICIT NONE
        REAL (KIND=8) f,x

        f = 3.d0 * (x**2) + 2.d0 * EXP(-3.d0 * x) + SIN(x)
END FUNCTION
