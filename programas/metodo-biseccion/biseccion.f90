PROGRAM biseccion
        IMPLICIT NONE
        REAL (KIND=16) root, bisect, eps_s
        INTEGER n

        n = 14
        eps_s = 0.5d0 * (10.d0 ** (2 - n))
        root = bisect(-0._16, 5._16, eps_s, 0)
END PROGRAM biseccion

FUNCTION f(x)
        IMPLICIT NONE
        REAL (KIND=16) f, x
        f = (x**2) - 4.d0 - SIN(x)
END FUNCTION

RECURSIVE FUNCTION bisect(xl, xu, eps_s, depth) RESULT(res)
        IMPLICIT NONE
        REAL (KIND=16) xl, xu, xr, res, prev, f, eps_s, eps_a
        INTEGER depth

        xr = (xl + xu) / 2.d0

        eps_a = ABS((xr - xl) / xr) * 100
        IF (depth > 100 .or. eps_a < eps_s) THEN
                res = xr
                WRITE(*,*) 'root:', res, 'eps_a', eps_a
                RETURN
        END IF

        IF (f(xl) * f(xr) < 0) THEN
                res = bisect(xl, xr, eps_s, depth + 1)
        ELSE
                res = bisect(xr, xu, eps_s, depth + 1)
        END IF
        res = 0
END FUNCTION
