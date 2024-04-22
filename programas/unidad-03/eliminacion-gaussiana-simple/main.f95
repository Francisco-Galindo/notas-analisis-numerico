PROGRAM GAUSS_SIMPLE
        IMPLICIT NONE
        INTEGER,PARAMETER:: n = 3
        REAL (KIND = 8)::A(1:n,1:n),b(1:n)
        INTEGER i,j
        CHARACTER (LEN=6) LONGITUD_RENGLON

        WRITE(LONGITUD_RENGLON, '(i6)') n

        A(1,1) = 3.d0; A(1,2) = -0.1d0; A(1,3) = -0.2d0
        A(2,1) = 0.1d0; A(2,2) = 7.d0; A(2,3) = -0.3d0
        A(3,1) = 0.3d0; A(3,2) = -0.2d0; A(3,3) = 10.d0

        b(1) = 7.85d0; b(2) = -19.3d0; b(3) = 71.4d0

        WRITE(*,*) 'Sistema de ecuaciones a resolver Ax=b'
        DO i=1,n,1
                WRITE(*,'('//LONGITUD_RENGLON//'(F17.8,2x),A1,f17.8)') A(i,1), A(i,2), A(i,3), '=', b(i)
        END DO

        CALL ELIMINACION_GAUSSIANA(A, b, n)

        WRITE(*,*)
        DO i=1,n,1
                WRITE(*,'('//LONGITUD_RENGLON//'(F17.8,2x),A1,f17.8)') A(i,1), A(i,2), A(i,3), '=', b(i)
        END DO

END PROGRAM

SUBROUTINE ELIMINACION_GAUSSIANA(A, b, n)
        IMPLICIT NONE
        INTEGER i,j,k,n
        REAL (KIND = 8)::A(1:n,1:n),b(1:n)
        REAL (KIND = 8) factor

        DO k=1,n-1,1
                DO i=k+1,n,1
                        factor=A(i,k)/A(k,k)
                        DO j=k+1,n,1
                                A(i,j) =  A(i,J) - factor*A(k,j)
                        END DO
                        b(i) = b(i) - factor*b(k)
                END DO
        END DO

END SUBROUTINE ELIMINACION_GAUSSIANA
