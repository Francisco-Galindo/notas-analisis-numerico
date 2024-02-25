Program derivada
        IMPLICIT NONE
        REAL (KIND=8) x, h, dfdx, f ! Numeros flotantes de 4 bytes
        x=2.d0 !'d0' denota doble precision
        h=0.000001d0

        dfdx = (f(x + h) - f(x)) / h ! Derivada numerica

        WRITE(*,*) 'dfdx = ', dfdx

End Program derivada

FUNCTION f(x)
        IMPLICIT NONE
        REAL (KIND=8) f,x

        f = 3.d0 * (x**2) + 2.d0 * EXP(-3.d0 * x) + SIN(x)
END FUNCTION
