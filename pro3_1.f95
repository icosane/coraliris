Program Pro1 !суммирование ряда - для одного х
Implicit None
Real:: x = -0.7
Real:: eps = 1E-10
Integer:: N = 0
Real:: An = 0, Sn = 0, F
Real Delta
Open(1, file = 'Out.txt')!Result
Open(2, file = 'An.txt')!An от N
Open(3, file = 'Sn.txt')!Sn от N
Open(4, file = 'F.txt')! F
Write(1, *) 'Точность вычисления',eps
Sn=-x
An=-x
Do while (ABS(An) > eps)
An=((-(x**2)*(2*N+1))/(2*N+3))*An
Sn=Sn+An
Write(2, *)N,An
Write(3, *)N,Sn
N=N+1
F=(-atan(x))
Delta=ABS(F-Sn)
Write(1, *)Delta
Write(4, *)N, F
End Do
End Program Pro1