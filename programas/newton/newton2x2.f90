PROGRAM NR_DOS_VARIABLES
        IMPLICIT NONE
        REAL (KIND=8) tolerancia
        REAL (KIND=8)u,v,dudx,dudy,dvdx,dvdy,JACOBIANO
        REAL (KIND=8)xi,yi,h,ximas1,yimas1,eps_ax,eps_ay
        INTEGER n,maxiter,i

        OPEN(UNIT=22,FILE='Newton-Raphson 2 variables.dat',STATUS='replace',ERR=500)

        n=8 !cifras significativas
        tolerancia=0.5d0*10.d0**(2-n) !tolerancia [%]
        maxiter=100 !número de iteraciones máximas
        h=0.0001d0 !incremento para las derivadas
        !Valores iniciales del método
        xi=-0.7d0
        yi=1.5d0

        WRITE(*,'(A1,14x,A2,15x,A2,17x,A6,10x,A6,15x,A6,10x,A6,10x,A6)') 'i','xi','yi','ximas1','yimas1','eps_ax','eps_ay'
        WRITE(22,'(A1,14x,A2,15x,A2,17x,A6,10x,A6,15x,A6,10x,A6,10x,A6)') 'i','xi','yi','ximas1','yimas1','eps_ax','eps_ay'
        50 FORMAT(I3,x,f18.12,x,f18.12,x,f18.12,x,f18.12,x,f18.12,x,f18.12)

        !Inicia ciclo de iteraciones
        DO i=1,maxiter,1
                !Cálculo de la raíz
                ximas1=xi-(u(xi,yi)*dvdy(xi,yi,h)-v(xi,yi)*dudy(xi,yi,h))/JACOBIANO(xi,yi,h)
                yimas1=yi-(v(xi,yi)*dudx(xi,yi,h)-u(xi,yi)*dvdx(xi,yi,h))/JACOBIANO(xi,yi,h)
                !Cálculo de los errores relativos
                eps_ax=ABS((ximas1-xi)/ximas1)*100.d0 ![%]
                eps_ay=ABS((yimas1-yi)/yimas1)*100.d0 ![%]
                !Impresión de resultados
                WRITE(*,50)i,xi,yi,ximas1,yimas1,eps_ax,eps_ay
                WRITE(22,50)i,xi,yi,ximas1,yimas1,eps_ax,eps_ay
                !Condición de convergencia
                IF (eps_ax.LT.tolerancia .AND. eps_ay.LT.tolerancia) EXIT
                !Se reciclan valores en caso de no converger
                xi=ximas1
                yi=yimas1
        END DO

        CLOSE(UNIT=22,STATUS='keep',ERR=500)
500 END PROGRAM




!*****u(x,y)
FUNCTION u(x,y)
        IMPLICIT NONE
        REAL (KIND=8) u,x,y
        u = y - x**2.d0 - 1.d0
END FUNCTION
!*****v(x,y)
FUNCTION v(x,y)
        IMPLICIT NONE
        REAL (KIND=8) v,x,y
        v = y - 2.d0 * COS(x)
END FUNCTION
!**********Derivadas parciales
FUNCTION dudx(x,y,h)
        IMPLICIT NONE
        REAL (KIND=8) dudx,u,x,y,h
        dudx=(-u(x+2.d0*h,y)+8.d0*u(x+h,y)-8.d0*u(x-h,y)+u(x-2.d0*h,y))/(12.d0*h)
END FUNCTION

FUNCTION dudy(x,y,h)
        IMPLICIT NONE
        REAL (KIND=8) dudy,u,x,y,h
        dudy=(-u(x,y+2.d0*h)+8.d0*u(x,y+h)-8.d0*u(x,y-h)+u(x,y-2.d0*h))/(12.d0*h)
END FUNCTION

FUNCTION dvdx(x,y,h)
        IMPLICIT NONE
        REAL (KIND=8) dvdx,v,x,y,h
        dvdx=(-v(x+2.d0*h,y)+8.d0*v(x+h,y)-8.d0*v(x-h,y)+v(x-2.d0*h,y))/(12.d0*h)
END FUNCTION

FUNCTION dvdy(x,y,h)
        IMPLICIT NONE
        REAL (KIND=8) dvdy,v,x,y,h
        dvdy=(-v(x,y+2.d0*h)+8.d0*v(x,y+h)-8.d0*v(x,y-h)+v(x,y-2.d0*h))/(12.d0*h)
END FUNCTION


FUNCTION JACOBIANO(x,y,h)
        IMPLICIT NONE
        REAL (KIND=8) JACOBIANO,dudx,dudy,dvdx,dvdy,x,y,h
        JACOBIANO=dudx(x,y,h)*dvdy(x,y,h)-dudy(x,y,h)*dvdx(x,y,h)
END FUNCTION
