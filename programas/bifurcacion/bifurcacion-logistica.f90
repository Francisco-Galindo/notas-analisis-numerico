PROGRAM bifurcacion_logistica
        IMPLICIT NONE
        REAL (KIND=8) x0, r0, r, rf, Deltar, xnmas1, f
        INTEGER n, i, j
        OPEN(UNIT=25,FILE='bif.dat',STATUS='replace',ERR=500)

        x0 = 0.5d0 ! Valor inicial
        r0 = 0d0 ! límite inferior
        rf = 1.6d0 ! límite superior

        n = 500
        Deltar=(rf - r0) / DBLE(n)

        DO i = 0, n, 1
                r = r0 + DBLE(i) * Deltar
                DO j = 1, n, 1
                        xnmas1 = f(r, x0)
                        IF (j >= 200) THEN
                                WRITE(25, *) r, xnmas1
                        END IF
                        x0 = xnmas1
                END DO
        END DO
        CLOSE(UNIT=25,STATUS='keep',ERR=500)
500 END PROGRAM bifurcacion_logistica

FUNCTION f(r, xn)
        IMPLICIT NONE
        REAL (KIND=8) f, r, xn
        f = r * xn * (1 - xn)
END FUNCTION f
