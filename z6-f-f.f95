subroutine multiply(A1,B1,x,y,z,q,v,f)
    implicit none
    real, dimension(:,:), intent(in):: A1,B1
    real, dimension(:,:), intent(out)::v
    logical, intent(out)::f
    integer, intent(in)::x,y,z,q
    real temp
    integer i,j,k
    f = .FALSE.
    if (z==q) then
        f = .TRUE.
        do i=1,x 
            do j=1,y
                temp = 0
                do k=1,z 
                    temp = temp + (A1(i,k)*B1(k,j))
                enddo
                v(i,j)=temp
            enddo
        enddo
        else
            f = .FALSE.
        endif
        if (f .eqv. .FALSE.) then
            write(2,'(A)')'Получение результата невозможно'&
            'из-за несовпадения размеров матрицы и вектора'
            stop
        end if
end subroutine multiply

Program Matrix
    implicit none
    integer:: N,M,N2,M2,i,j
    Real,Allocatable,dimension(:,:):: A,B,C,t,r,res,ra,rb,rf,tf
    logical::p 
    Interface
    subroutine multiply(A1,B1,x,y,z,q,v,f)
        implicit none
        real, dimension(:,:), intent(in):: A1,B1
        real, dimension(:,:), intent(out)::v
        logical, intent(out)::f
        integer, intent(in)::x,y,z,q
    end subroutine multiply
    End Interface
    Open(2,file='out.txt') !выходной файл
    Open(1,file='in.txt') !входной файл               
    Read(1,*)N,M !size of arrays A,B
    Allocate(A(1:N,1:M),B(1:N,1:M)) !allocating memory for arrays A,B
    do i = 1,N !reading array A
        Read(1,*) ( A(i,j), j = 1,M )
    end do
    do i = 1,N !reading arrays B
        Read(1,*) ( B(i,j), j = 1,M )
    end do
    Read(1,*)N2,M2 !size of rezulting array or a vector

    if ((N2<N) .and. (M2<N)) then
        allocate(C(1:N2+1,1:M2+1))
    else
        allocate(C(1:N2,1:M2))
    endif
    
    
    do i = 1,N2 !reading vector
        Read(1,*) ( C(i,j), j = 1,M2 )
    end do

    !allocating memory for different stuff
    allocate (t(1:N,1:M),r(1:N,1:M),res(N,M2),ra(1:N,1:M),rb(1:N,1:M),rf(1:N,1:M),tf(1:N,1:M))
    
    !сумма матриц
    do i=1,N
        do j=1,M 
            t(i,j) = A(i,j) + B(i,j)
            r(i,j) = t(i,j)
        enddo     
    enddo
    
    !outputs
    write(2,'(A,I1,A,I1,A,A,I2)')'В исходных матрицах ',N,' строк(и) и ',M,' столбца(ов)'
    write(2,'(A,I2)')'Вектор имеет длину ',N2
    write(2,'(A)')'Исходная матрица A: '
    do i=1,N
        write(2,'(10(F6.2,2X))') A(i,:)
    end do

    write (2,'(A30)')' '
    write(2,'(A)')'Исходная матрица B: '
    do i=1,N
        write(2,'(10(F6.2,2X))') B(i,:)
    end do

    write (2,'(A30)')' '
    write(2,'(A)')'Исходный вектор V: '
    do i=1,N2
        write(2,'(10(F6.2,2X))') C(i,:)
    end do

    write (2,'(A30)')' '
    write (2,'(A30)')' '
    
    !матрица А
    Call multiply(A,C,N,M2,M,N2,res,p)
    write (2,'(A85)')'Результирующие вектора A*V и B*V соответственно:'
    if (p .eqv. .FALSE.) then
        write (2,*)'Получение результата невозможно'&
        'из-за несовпадения размеров матрицы и вектора'
    else
        write (2,'(10(F6.2,2X))')res
        ra = res
    endif

    !матрица B
    Call multiply(B,C,N,M2,M,N2,res,p)
    if (p .eqv. .FALSE.) then
        write (2,*)'Получение результата невозможно'&
        'из-за несовпадения размеров матрицы и вектора'
    else
        write (2,'(10(F6.2,2X))')res
        rb = res
    endif

    write (2,'(A30)')' '
    write (2,'(A)')'Проверка встроенной функцией matmul' 
    write(2,'(10(F6.2,2X))')Matmul(A,C)
    !write (2,'(A30)')' '
    write(2,'(10(F6.2,2X))')Matmul(B,C)
   
    write (2,'(A30)')' '
    write (2,'(A30)')'Cумма матриц A+B: '
    do i=1,N
        write(2,'(10(F6.2,2X))') r(i,:)
    end do

    Call multiply(r,C,N,M2,M,N2,res,p)
    write (2,'(A30)')' '
    write (2,'(A7)')'(A+B)V:'
    write (2,'(10(F6.2,2X))',advance='no')res

    write (2,'(A30)')' '
    write (2,'(A)')'Проверка встроенной функцией matmul' 
    write(2,'(10(F6.2,2X))')Matmul(r,C)

    do i=1,N
        do j=1,M2
            tf(i,j) = ra(i,j) + rb(i,j)
            rf(i,j) = tf(i,j) 
        enddo    
    enddo
    write (2,'(A30)')' '
    write (2,'(A7)')'AV + BV:'
    do i=1,N
        do j=1,M2
        write(2,'(10(F6.2,2X))',advance='no')rf(i,j)
    end do
    enddo
    
    Deallocate(A,B,C,t,r,res,ra,rb,rf,tf) !очистка памяти
end program Matrix