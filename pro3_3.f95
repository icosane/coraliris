Program Pro2 !суммирование ряда для различных значений x
Implicit None
Real:: xm = -0.9, xk=0.9, xh=0.1 , x

Real:: eps = 1E-04
Integer:: N = 0
Integer:: Nmax = 100
Real:: An = 0, Sn = 0, F
Real Delta
WRITE(*,"(4A)") ( " No X An Sn
F Delta " )
WRITE(*,"(4A)") ( "--------------------------------------------
---- " )
do x = xm, xk+xh/2, xh
Sn=-x
An=-x
Do while (ABS(An) > eps)
An=((-(x**2)*(2*N+1))/(2*N+3))*An
Sn = Sn+An
F=(-atan(x))
Delta=ABS(F-Sn)
N = N+1
End Do
WRITE(*,"(I3.2,E12.3,E12.3,E12.3,F9.5,F9.5)")N, X, An, Sn, F,
Delta
N = 0
End do
End Program Pro2