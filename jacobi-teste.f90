program jacobi_T

use eigens_jacobi

implicit none
real, dimension(3,3):: m, v
real, dimension(3) :: e
real :: tol, rn
integer :: nmax,it,i,j, ierr

do i=1,3
   do j=i,3
!      call random_number(rn)
!      m(i,j)=rn*(-1)**(i+j)
!      m(j,i)=m(i,j)
      m(i,j)=1./(i+j-1)
      m(j,i)=m(i,j)
   enddo
enddo

write(*,*)'nmax,tol'
read(*,*)nmax,tol

call jacobi_eigens(m,e,v,tol,ierr,nmax,it)

write(*,*)'it=',it,'ierr=',ierr

write(*,*)'Matrix'
do i=1,3
   write(*,*)(m(i,j),j=1,3)
enddo
write(*,*)'Eigenvalues'
write(*,*)(e(i),i=1,3)
write(*,*)'Eigenvectors'
do i=1,3
   write(*,*)(v(i,j),j=1,3)
enddo

end program jacobi_T
