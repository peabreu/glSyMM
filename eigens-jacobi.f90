Module eigens_jacobi
contains
  Subroutine eigens(inertial_tensor, eigenvalues, eigenvectors)
    
    Implicit None
    Real, Dimension(3,3) :: inertial_tensor
    Real, Dimension(3)   :: eigenvalues
    Real, Dimension(3,3) :: eigenvectors
    real :: tol
    integer :: ierr, nmax, it, i,j
    tol=1e-5
    nmax=150
    call jacobi_eigens(inertial_tensor,eigenvalues,eigenvectors,tol,ierr,nmax,it)
    write(*,*)'ierr=',ierr
    write(*,*)
    write(*,*)"Eigenvalues"
    do i=1,3
       write(*,*)eigenvalues(i)
    enddo
    write(*,*)
    write(*,*) "Eigenvectors"
    do i =1,3
       write(*,*)(eigenvectors(i,j),j=1,3)
    enddo
    write(*,*)
  End Subroutine eigens
  
  subroutine jacobi_eigens(matrix2,eigenvalues,eigenvectors,tol,ierr,nmax,it)
    implicit none
    real, dimension(3,3), intent(in) :: matrix2
    real, intent(in) :: tol
    integer, intent(in) :: nmax

    integer, intent(out) :: ierr,it    
    real, dimension(3), intent(out) :: eigenvalues
    real, dimension(3,3), intent(out) :: eigenvectors
! this subroutine calculates the eigenvectors and eigenvalues
! for a 3x3 matrix using a jacobi-like method
    integer :: i,j, im,jm
    logical :: conv
    integer, parameter :: dp=kind(1.0D0)
    real(kind=dp) :: am, aux, S, C, theta
    real(kind=dp), dimension(3,3) :: matrix
! initialization of variables
    matrix=matrix2
    ierr=0
    do i=1,3
       do j=i,3
          if (j == i) then
             eigenvectors(i,j)=1.0_dp
          else
             eigenvectors(i,j)=0.0_dp
             eigenvectors(j,i)=0.0_dp
          endif
       enddo
    enddo
! Main Loop
    do it=1,nmax

       conv=.true.
       am=tol

       do i=1,3
          do j=1,3
             if (i /= j .AND. abs(matrix(i,j)) > am) then
                conv=.false.
                am=abs(matrix(i,j))
                im=i
                jm=j
             endif
          enddo
          eigenvalues(i)=matrix(i,i)
       enddo
       
       if (conv) return

       if (abs(matrix(im,im)-matrix(jm,jm)) /= 0.0_dp) then
          theta=0.5_dp*atan(2.0_dp*matrix(im,jm)/(matrix(im,im)-matrix(jm,jm)))
       else
          theta=0.25_dp*acos(-1.0_dp)
       endif
       C=cos(theta)
       S=sin(theta)
       do i=1,3
          aux=matrix(i,im)
          matrix(i,im)=matrix(i,im)*C+matrix(i,jm)*S
          matrix(i,jm)=-aux*S+matrix(i,jm)*C

          aux=eigenvectors(i,im)
          eigenvectors(i,im)=eigenvectors(i,im)*C+eigenvectors(i,jm)*S
          eigenvectors(i,jm)=-aux*S+eigenvectors(i,jm)*C
       enddo

       do i=1,3
          aux=matrix(im,i)
          matrix(im,i)=matrix(im,i)*C+matrix(jm,i)*S
          matrix(jm,i)=-aux*S+matrix(jm,i)*C          
       enddo
       
!       write(*,*)'Matrix at iteration', it
!       do i=1,3
!          write(*,*)(matrix(i,j),j=1,3)
!       enddo

    enddo
    ierr=1
    write(*,*)' **************************** '
    write(*,*)' *                          * '
    write(*,*)' * No convergence in Jacobi * '
    write(*,*)' *                          * '
    write(*,*)' **************************** '

  end subroutine jacobi_eigens

end Module eigens_jacobi
