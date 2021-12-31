Program Formula ! тема: вычисления по формулам
	Implicit None 
	Integer::k
	Real::x=1.4,y=2.8,alpha=0.66,beta=0.82
	Real::y1,y2,y1_1,y1_z,y1_z_h 
	Real::z1,z2
	!Open(6,file='result.txt') 
	Open(1,file='input.txt') 

	do k=1,3
		read (1,*)x,y,alpha,beta
		print *, 'at x=',x
		y1_1 = (x*y)**(1./3) ! повторяющаяся часть формулы 
		y1_z_h = (((x**4)**(1./3)) - 8*y*((x**(1./3)))) !Часть знаменателя
		y1_z = (y1_z_h / y1_1) ! Знаменатель
		y1 = ((x**(2./3)) + 2*y1_1 + 4*(y**(2./3)))/y1_z

	
		print *, 'y1=', y1
		y2 = 1/(((x/y)**(1./3))-2)
		print *, 'y2=', y2
		z1 = ((Cos(alpha)-Cos(beta))**2) - ((Sin(alpha) - Sin(beta))**2)
		z2 = -4*(Sin((alpha-beta)/2)**2)*Cos(alpha+beta)
		print *, 'z1=', z1
		print *, 'z2=', z2
	end do
	close(6)
	close(1)
End Program Formula 