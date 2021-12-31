Program Matrix; Implicit None
Integer,Allocatable,dimension(:,:)::A ! динамическая матрица
Integer,Allocatable,dimension(:,:):: B 
Integer:: x,y ! размеры матрицы x*y
Integer:: i, j ! индексы строк и столбцов
Integer:: k,p,s=0,u
Character(len=8)::l
do u=1,2 !ввод двух разных матриц
    if (u == 1) then
    l = 'In.txt'
    end if
    if (u == 2) then
    l = 'In1.txt'
    end if
Open(1,file=l) ! файл с исходной матрицей
if (u == 1) then
    l = 'out.txt'
    end if
    if (u == 2) then
    l = 'out1.txt'
    end if
Open(2,file=l) ! результирующий файл 
Read(1,*)x,y ! ввод размеров (строк, столбцов)

Allocate(A(1:x,1:y),B(1:x,1:y)) !выделение места в пямяти
do i = 1,x !чтение массива А из файла
    Read(1,*) ( A(i,j), j = 1,y )
end do

do i = 1,x !формирование матрицы B
    do j=1,y
        s=0
        do k=i,x
            do p=1,j
                s=s+A(k,p)
            enddo
        B(i,j)=s
        enddo
    end do
end do

!----------output
write(2,'(A,I1,A,I1,A)')'В исходной матрице ',x,' строк(и) и ',y,' столбца(ов)'
write(2,'(A)')'Исходная матрица: '
do i=1,x
    write(2,*) A(i,:)
end do

write(2,'(A)')'                        '
write(2,'(A)')'Преобразованная матрица: '
do i=1,x
    write(2,*) B(i,:)
end do

Deallocate(A) ! освобождение динамической памяти
Deallocate(B)
enddo
End Program Matrix