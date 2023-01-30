  Subroutine eigens(inertial_tensor_r, eigenvalues_r, eigenvectors_r)
!    Use la_precision, Only : wp => sp
!    Use f95_lapack
    
    Implicit None
    Real, Dimension(3,3), Intent(in) :: inertial_tensor_r
    Real, Dimension(3), Intent(out) :: eigenvalues_r
    Real, Dimension(3,3), Intent(out) :: eigenvectors_r
!    integer :: wp = kind(1.0) num funciona ...

    Real, Dimension(3,3) :: inertial_tensor
    Real, Dimension(3)   :: eigenvalues
    Real, Dimension(3,3) :: eigenvectors
    Real, Dimension (3,3):: a
    Real, Dimension(3):: d,e,tau
    Real, Dimension(100)::work
    Character(len=1)::uplo
    Integer::lda,lwork,n,info
    uplo='L'
    lwork=96
    lda=3
    a=inertial_tensor_r
!    ndim=3
      Write(*,*)'Reduction to tridiagonal form'
    Call ssytrd(uplo,3,a,lda,d,e,tau,work,lwork,info)
    If (info.Ne.0) Stop'Reduction to tridiagonal form failed '
      Write(*,*)'Info=',info
!      write(*,*)'Optimal lwork=',work(1)

      Write(*,*)'Generate orthogonal matrix Q'
    lwork=64
    Call sorgtr (uplo, 3, a, lda, tau, work, lwork, info)
    If (info.Ne.0) Stop'Q Orthogonal matrix formation failed '
      Write(*,*)'Info=',info
!      write(*,*)'Optimal lwork=',work(1)

      Write(*,*)'Calculation of eigenvalues and eigenvectors'
    uplo='V'
    Call ssteqr(uplo,3,d,e,a,lda,work,info)
    If (info.Ne.0) Stop'Eigen-(values and/or vectors) failed'
    Write(*,*)'Info=',info
    Write(*,*)a ! eigenvectors ???
    Write(*,*)d ! eigenvalues
    eigenvalues_r=d
    eigenvectors_r=a

  End Subroutine eigens
