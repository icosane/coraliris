Program Region
Implicit none
Real x,y
Integer alpha 
Integer, parameter:: inside = 1, outside = 2
Logical A, B, C 
Open(4, FILE= '4.txt')
Open(5, FILE= '5.txt')
Open(6, FILE= '6.txt')
Open(7, FILE= '7.txt')
open(8, file='8.txt')
open(9, file='9.txt')
Open(inside, FILE= 'in.txt')
Open(outside, FILE= 'out.txt')
Write(4,*)0,1 ; Write(4,*)0,3
close(4)
Write(5,*)0,0 ; Write(5,*)-3,0
close(5)
Write(6,*)0,0 ; Write(6,*)0,-3
close(6)
Do alpha = 0, 360, 3 !окружность
x = 3*cosd(Real(alpha)); y =3*sind(Real(alpha))
Write(7,*)x,y;
End Do 
close(7)
!Do alpha = 180, 270, 3
!x = 3*cosd(Real(alpha)); y =3*sind(Real(alpha))
!Write(8,*)x,y;
!End Do 
Do x = 0,2.56,0.1 !парабола
	y = (x-1)**2
	Write(9,*)x,y;
End Do
close(9)

Do x = -3, 3, 0.2
 Do y = -3, 3, 0.2
	A= y>=0 .and. y-(x-1)**2>=0 .and. x>=0 .and. x**2+y**2<=9
	B=x**2+y**2<=9 .and. x<=0 .and. y<=0
  
  If (A .or. B ) then 
   Write(inside, *) x, y 
  Else 
   Write(outside, *) x, y 
  End if 
 End Do 
End Do

End Program Region