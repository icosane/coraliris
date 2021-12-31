Program Pro2 !суммирование ряда - для одного х
Implicit None
Real:: x = -0.7
Real:: eps = 1E-10
Integer:: N = 0
Real:: An = 0.00, Sn = 0, F
Real Delta
Open(1,file='input.txt')
WRITE(*,"(4A)") ( " No X An Sn
F Delta " )
WRITE(*,"(4A)") ( "--------------------------------------------
---- " )
Sn=-x
An=-x
do while (x<1)
Do while (ABS(An) > eps)
An = ((-(x**2)*(2*N+1))/(2*N+3))*An
Sn = Sn+An
F = (-atan(x))
Delta=ABS(F-Sn)
WRITE(*,"(I3.2,E12.1,E12.3,E12.3,F9.5,F9.5)")N, X, An, Sn, F,
Delta

N = N +1
End Do
N = 1
x = x+0.1
WRITE(*,"(I3.2,E12.1,E12.3,E12.3,F9.5,F9.5)")N, X, An, Sn, F,
Delta
End do
End Program Pro2