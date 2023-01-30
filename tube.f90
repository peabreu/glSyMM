! $Id: tube.f90,v 1.7 2004/04/12 21:15:03 paulo Exp $
! tube fortran 90 version, Copyrigth (c) 2001 by Paulo E. Abreu <qtabreu@ci.uc.pt>
! conversion from original version written by Jamie Zawinski

! original copyrigth from C version

!/* tube, Copyright (c) 2001 Jamie Zawinski <jwz@jwz.org>
! * Utility functions to create tubes and cones in GL.
! *
! * Permission to use, copy, modify, distribute, and sell this software and its
! * documentation for any purpose is hereby granted without fee, provided that
! * the above copyright notice appear in all copies and that both that
! * copyright notice and this permission notice appear in supporting
! * documentation.  No representations are made about the suitability of this
! * software for any purpose.  It is provided "as is" without express or 
! * implied warranty.
! */

module draw_tube
use opengl_gl
!use opengl_glu
use opengl_glut
implicit none
public :: tube, unit_tube
private
real,parameter::PI=3.14159265358979323846_gldouble
contains

subroutine unit_tube (faces2, smooth, wire)
! routine draws a unit tube: basis centered on the origin, top centered on (0,0,1)
  implicit none

  integer, intent(in):: faces2
  logical, intent(in):: smooth,wire

  integer:: i
  real(kind=glfloat):: step, s2
  real(kind=glfloat):: th,x,y,x0,y0
  integer:: z=0,faces

  real(kind=glfloat):: xf,yf,zf

  faces=faces2

  step = PI*2/faces
  s2=step/2

  !   /* side walls */
  call glFrontFace(GL_CCW)

  if (wire) then
     call glBegin(GL_LINES)
  else
     if (smooth) then
        call glBegin(GL_QUAD_STRIP)
     else
        call glBegin(GL_QUADS)
     endif
  endif

  th=0.0_glfloat
  x=1.0_glfloat
  y=0.0_glfloat

!  if (.not.smooth) then
!     x0=cos(s2)
!     y0=sin(s2)
!  endif

!  if (smooth) then 
!     faces=faces+1
!  endif

  if (smooth) then
     faces=faces+1
  else
     x0=cos(s2)
     y0=sin(s2)
  endif

  do i=0,faces-1
     if (smooth) then
        call glNormal3f(x,0.0_glfloat,y)
     else
        call glNormal3f(x0,0.0_glfloat,y0)
     endif

     call glVertex3f(x,0.0_glfloat,y)
     call glVertex3f(x,1.0_glfloat,y)
     
     th=th+step
     x=cos(th)
     y=sin(th)
     
     if (.not.smooth) then
        x0=cos(th+s2)
        y0=sin(th+s2)
        
        call glVertex3f(x,1.0_glfloat,y)
        call glVertex3f(x,0.0_glfloat,y)
     endif
  enddo
  call glEnd()
  ! End caps
  do z=0,1
     if (z==0) then
        call glFrontFace(GL_CCW)
        call glNormal3f(0.0_glfloat,-1.0_glfloat,0.0_glfloat)        
     else
        call glFrontFace(GL_CW)
        call glNormal3f(0.0_glfloat,1.0_glfloat,0.0_glfloat)
     endif

     if (wire) then
        call glBegin(GL_LINE_LOOP)
     else
        call glBegin(GL_TRIANGLE_FAN)
     endif
     zf=z
     if (.not.wire) call glVertex3f(0.0_glfloat,zf,0.0_glfloat) 
     th=0.0_glfloat
     do i=0,faces
        x=cos(th)
        y=sin(th)
!        zf=z ! really necessary ? dont think so ...
        call glVertex3f(x,zf,y)
        th=th+step
     enddo
     call glEnd()
  enddo
end subroutine unit_tube

subroutine tube(x1,y1,z1,x2,y2,z2,diameter,cap_size,faces,smooth,wire)
  use opengl_gl
  implicit none
  real(kind=glfloat),intent(in)::x1,y1,z1,x2,y2,z2,diameter,cap_size
  integer,intent(in)::faces
  logical,intent(in)::smooth,wire

  real(kind=glfloat)::length,angle,a,b,c

!  type(gluquadricobj), pointer:: quad
  real(kind=gldouble) :: base, top, height
  integer(kind=glint) :: slices, stacks

  if (diameter<=0) stop 'Invalid tube diameter'

  a=x2-x1
  b=y2-y1
  c=z2-z1

  length=sqrt(a*a + b*b + c*c)
  angle=acos(a/length)

  call glPushMatrix()
  call glTranslatef(x1,y1,z1)
  call glScalef(length,length,length)

  if (c==0.0_glfloat .and. b==0.0_glfloat) then
     call glRotatef(angle/(pi/180),0.0_glfloat,1.0_glfloat,0.0_glfloat)
  else
     call glRotatef(angle/(pi/180),0.0_glfloat,-c,b)
  endif

  call glRotatef(-90.0_glfloat,0.0_glfloat,0.0_glfloat,1.0_glfloat)
  call glScalef(diameter/length,1.0_glfloat,diameter/length)

  !  /* extend the endpoints of the tube by the cap size in both directions */
  if (cap_size /= 0.0_glfloat) then
     c=cap_size/length
     call glTranslatef (0.0_glfloat, -c, 0.0_glfloat)
     call glScalef (1.0_glfloat, 1.0_glfloat+c+c, 1.0_glfloat)
  endif

  call unit_tube(faces,smooth,wire)

!  allocate(quad)
!  quad=gluNewQuadric()
!  base=1.0_gldouble
!  top=1.0_gldouble
!  height=1.0_gldouble
!  slices=20_glint
!  stacks=1_glint
!  call glrotatef(-90.0_glfloat,1.0_glfloat,0.0_glfloat,0.0_glfloat)
!!!  call glColor3f(0.5_glfloat,0.4_glfloat,0.6_glfloat)
!  call gluQuadricOrientation(quad,GLU_OUTSIDE)
!  call gluQuadricDrawStyle(quad,GLU_FILL)
!  call gluQuadricNormals(quad,GLU_SMOOTH)
!  call  gluQuadricOrientation (quad,GLU_OUTSIDE)
!  call gluCylinder(quad,base,top,height,slices,stacks)
!  deallocate(quad)

  call glPopMatrix()
end subroutine tube
end module draw_tube

!The statements 

!   if (a > b)
!       z = a;
!   else
!       z = b;

!compute in z the maximum of a and b. 
!The conditional expression, written with the ternary operator ``?:'',
!provides an alternate way to write this
!and similar constructions. In the expression 

!   expr1 ? expr2 : expr3

!the expression expr1 is evaluated first. 
!If it is non-zero (true), then the expression expr2 is evaluated, 
!and that is the value of the conditional expression. 
!Otherwise expr3 is evaluated, and that is the value. 
!Only one of expr2 and expr3 is evaluated. Thus to set z to the maximum of a and b,

!   z = (a > b) ? a : b;    /* z = max(a, b) */

!In the expression 
!
!   expr1 ? expr2 : expr3
!
! the expression expr1 is evaluated first. 
! If it is non-zero (true), then the expression expr2 is evaluated,
! and that is the value of the conditional expression.
! Otherwise expr3 is evaluated, and that is the value.
! Only one of expr2 and expr3 is evaluated.
!
!Logical AND Operator
! The && operator groups left-to-right.
! It returns 1 if both its operands compare unequal to zero, 0 otherwise. 
!
!Logical OR Operator
!The || operator groups left-to-right.
! It returns 1 if either of its operands compare unequal to zero, and 0 otherwise.
