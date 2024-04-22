PROGRAM METODO_SECANTE
        IMPLICIT NONE
        REAL (KIND=8) xnmenos1, xn, xnmas1, f, df, h
        REAL (KIND=8) epsilon_s, epsilon_a
        INTEGER n, i, iter_max
        OPEN(UNIT=25, FILE='secante.dat', STATUS='replace', ERR=500)

        xn = -20.d0
        h = 0.0001d0
        xnmenos1 = xn - h
        n = 4
        epsilon_s = 0.5d0 * 10.d0 ** (2 - n)
        iter_max = 100

        WRITE(25,'(A1,18x,A2,18x,A4,15x,A9)') 'k','xn','xn+1','epsilon_a'

        26 FORMAT(I4,2x,F20.11,2x,F20.11,2x,F20.11)

        DO i = 1, iter_max, 1
                xnmas1 = xn - f(xn) / Df(xn, xnmenos1)
                epsilon_a = ABS((xnmas1 - xn) / xnmas1) * 100.d0
                WRITE(25, 26) i, xn, xnmas1, epsilon_a
                xnmenos1 = xn
                xn = xnmas1
                IF (epsilon_a < epsilon_s) EXIT
        END DO

        CLOSE(UNIT=25, STATUS='keep', ERR=500)
500 END PROGRAM METODO_SECANTE

FUNCTION f(x)
    REAL (KIND=8) f, x
    f = DSIN(x) * DEXP(DCOS(x)) * DLOG(DCOSH(x)) - 4.d0 * x + x**3
END FUNCTION f

FUNCTION Df(xn,xnmenos1)
    REAL (KIND=8) f, Df, xn, xnmenos1
    Df = (f(xnmenos1) - f(xn)) / (xnmenos1 - xn)
END FUNCTION Df
