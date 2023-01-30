module moving_axis

use opengl_gl
use opengl_glu
use opengl_glut

implicit none

real(kind=gldouble), save:: esfera2D(2),esfera2Dclick(2)
real(kind=glfloat), save:: c1, s1, c2, s2
logical,save ::  moving_a=.False.
Real(kind=glfloat), Save :: mirror_size
Real (kind=glfloat),Dimension(4)::&
     mirror_color=(/ 0.7_glfloat, 0.7_glfloat, 0.7_glfloat, 0.4_glfloat /), &
     axis_color=(/ 0.2_glfloat, 0.2_glfloat, 1.0_glfloat, 0.5_glfloat /), &
     mirror_color_mov=(/ 0.8_glfloat, 0.8_glfloat, 0.8_glfloat, 0.4_glfloat /), &
     axis_color_mov=(/ 1.0_glfloat, 105/255.0_glfloat, 180/255.0_glfloat, 0.5_glfloat /)
!     axis_color_mov=(/ 105/255.0_glfloat, 255/255.0_glfloat, 180/255.0_glfloat, 0.5_glfloat /)
Real(kind=glfloat),Save::h,w,d,inc
Real,Save::x1,y1,z1,x2,y2,z2
Real(kind=glfloat),Save::scale,real_size, slow=0.4_glfloat
Real(kind=glfloat),dimension(3),Save::ex,ey,ez
Real(kind=glfloat),Save::spin=0.0
Real(kind=glfloat), Dimension (3), Save :: vec
Logical :: scaled
contains

  Subroutine create_maxis_list2()

    Implicit None
    real(kind=gldouble), parameter :: PI = 3.141592653589793_gldouble
    Real(kind=glfloat),Parameter :: zero=0.0_glfloat, one=1.0_glfloat
    Real(kind=glfloat) :: maxsize=6.5_glfloat
    !Draw moving axes 
    ! antialias axis ...
    
    Call glNewList(13,GL_COMPILE)
    write(*,*)'axis2 S',glGetError()

    Call glEnable(GL_LINE_SMOOTH)
    Call glEnable(GL_BLEND)

    Call glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
    Call glHint(GL_LINE_SMOOTH_HINT, GL_DONT_CARE)
    ! Thicker axis lines 
    Call glLineWidth(6.0_glfloat)
    
    Call glShadeModel(GL_FLAT)
!    Call glClearColor(background(1),background(2),background(3),background(4))
    Call glDepthFunc(GL_LEQUAL)
    Call glEnable(GL_DEPTH_TEST)
    
!    Call glPushMatrix()
    call glMatrixMode(GL_MODELVIEW)
    Call glBegin(GL_LINES)
    ! glow in the dark axis !!!
!    call glMaterialfv(GL_FRONT, GL_EMISSION, oldie)
    
    w=Max(Abs(x2-x1),1.0)*0.6
    h=Max(Abs(y2-y1),1.0)*0.6
    d=Max(Abs(z2-z1),1.0)*0.6
    
    inc=0.2
    w=w/2+2*inc
    h=h/2+2*inc
    d=d/2+2*inc
    
!    write(*,*)'w,h,d',w,h,d
    real_size=Max(w,h,d)
!    If (real_size>maxsize) Then
!       scale=maxsize/real_size
!       Call glScalef(1/scale,1/scale,1/scale)
!       scaled=.true.
!    Endif
    
!    write(*,*)'c1,c2,s1,s2',c1,c2,s1,s2
    Call glColor4f(127.0_glfloat/255, 255.0_glfloat/255, zero/255 ,one)
    ! x axis
    Call glVertex3f(zero,zero,zero)
    Call glVertex3f(w*ex(1),w*ex(2),w*ex(3))
!    Call glVertex3f(w,zero,zero)
    Call glColor4f(255.0_glfloat/255, 255.0_glfloat/255, zero/255 ,one)
    ! y axis
    Call glVertex3f(zero,zero,zero)
    Call glVertex3f(h*ey(1),h*ey(2),h*ey(3))
!    Call glVertex3f(zero,h,zero)
    Call glColor4f(255.0_glfloat/255, zero, 255.0_glfloat/255 ,one)
    ! z axis
    Call glVertex3f(zero,zero,zero)
    Call glVertex3f(d*ez(1),d*ez(2),d*ez(3))
!    Call glVertex3f(zero,zero,d)
    
    ! Draw crude x, y and z to label the axes
!    w=w*ex(1)+inc
!    Call glVertex3f(w*ex(1),-0.1_glfloat*ex(2),0.1_glfloat*ex(3)) ! X
!    Call glVertex3f(w*ex(1),0.1_glfloat*ex(2),-0.1_glfloat*ex(3))
!    Call glVertex3f(w*ex(1),-0.1_glfloat*ex(2),-0.1_glfloat*ex(3))
!    Call glVertex3f(w*ex(1),0.1_glfloat*ex(2),0.1_glfloat*ex(3))
!    h=h+inc
!    Call glVertex3f(0.1_glfloat,h,0.1_glfloat) ! Y
!    Call glVertex3f(zero,h,zero)
!    Call glVertex3f(-0.1_glfloat,h,0.1_glfloat)
!    Call glVertex3f(0.1_glfloat,h,-0.1_glfloat)
!    d=d+inc
!    Call glVertex3f(-0.1_glfloat,0.1_glfloat,d) ! Z
!    Call glVertex3f(0.1_glfloat,0.1_glfloat,d)
!    Call glVertex3f(0.1_glfloat,0.1_glfloat,d)
!    Call glVertex3f(-0.1_glfloat,-0.1_glfloat,d)
!    Call glVertex3f(-0.1_glfloat,-0.1_glfloat,d)
!    Call glVertex3f(0.1_glfloat,-0.1_glfloat,d)
!    If (scaled) Then
!       scale=maxsize/real_size
!       Call glScalef(1/scale,1/scale,1/scale)
!    Endif
!    Call glPopMatrix()
    Call glEnd()
!    call glpopmatrix()
!    If (scaled) Then 
!       scale=maxsize/real_size
!       Call glScalef(scale,scale,scale)
!    endif
    Call glShadeModel(GL_SMOOTH)
    call glEndList()
    write(*,*)'axis2',glGetError()
  End Subroutine create_maxis_list2
end module moving_axis


module view_modifier

! This module provides facilities to modify the view in an OpenGL window.
! The mouse buttons and keyboard arrow keys can be used to zoom, pan,
! rotate and change the scale.  A menu or submenu can be used to select which
! buttons perform which function and to reset the view to the initial settings.
! This is limited to one window.

! William F. Mitchell
! william.mitchell@nist.gov
! Mathematical and Computational Sciences Division
! National Institute of Standards and Technology
! April, 1998

! To use this module:
!
! 1) put a USE view_modifier statement in any program unit that calls a
!    procedure in this module
!
! 2) set the initial operation assignments, view and scale below the
!    "Initial configuration" comment below
!
! 3) call view_modifier_init after glutCreateWindow
!    This is a function that returns integer(kind=glcint) menuid.  The menuid
!    is the ID returned by glutCreateMenu.  You can either use the view_modifier
!    menu as your menu by calling glutAttachMenu immediately after
!    view_modifier_init, as in
!       menuid = view_modifier_init()
!       call glutAttachMenu(GLUT_RIGHT_BUTTON)
!    or by using the menuid to attach a submenu to your own menu, as in
!       call glutAddSubMenu("View Modifier",menuid)
!
! 4) in any callback functions that update the display, put
!       call reset_view
!    as the first executable statement
!
! Note that view_modifier_init sets the callback functions for glutMouseFunc,
! glutMotionFunc and glutSpecialFunc, so don't call these yourself
!
! The menu allows you to select what operation is attached to the left and
! middle mouse buttons and arrow keys, reset to the initial view, and quit.
! The right mouse button should be used for the menu.

use opengl_gl
use opengl_glu
use opengl_glut
use moving_axis
implicit none
private
public :: view_modifier_init, reset_view, view_from_above
private :: ZOOM, PAN, ROTATE, SCALEX, SCALEY, SCALEZ, RESET, QUIT, &
           PI, &
           left_button_func, middle_button_func, arrow_key_func, &
           init_lookat, init_lookfrom, &
           init_xscale_factor, init_yscale_factor, init_zscale_factor, &
           angle, shift, xscale_factor, yscale_factor, zscale_factor, &
           moving_left, moving_middle, begin_left, begin_middle, &
           cart2sphere, sphere2cart, cart3D_plus_cart3D, cart3D_minus_cart3D, &
           reset_to_init, mouse, motion, arrows, &
           menu_handler, set_left_button, set_middle_button, set_arrow_keys

integer(kind=glcint), parameter :: ZOOM = 1, PAN = 2, ROTATE = 3, SCALEX = 4, &
                      SCALEY = 5, SCALEZ = 6
integer(kind=glcint), parameter :: RESET = 10, QUIT = 11
real(kind=gldouble), parameter :: PI = 3.141592653589793_gldouble

type, private :: cart2D ! 2D cartesian coordinates
   real(kind=gldouble) :: x, y
end type cart2D

type, private :: cart3D ! 3D cartesian coordinates
   real(kind=gldouble) :: x, y, z
end type cart3D

type, private :: sphere3D ! 3D spherical coordinates
   real(kind=gldouble) :: theta, phi, rho
end type sphere3D

type(cart2D), save :: angle
type(cart3D), save :: shift
real(kind=gldouble), save :: xscale_factor, yscale_factor, zscale_factor
logical, save :: moving_left, moving_middle
type(cart2D), save :: begin_left, begin_middle

interface operator(+)
   module procedure cart3D_plus_cart3D
end interface
interface operator(-)
   module procedure cart3D_minus_cart3D
end interface

! ------- Initial configuration -------

! Set the initial operation performed by each button and the arrow keys.
! The operations are ZOOM, PAN, ROTATE, SCALEX, SCALEY, and SCALEZ

integer, save ::   left_button_func = ROTATE, &
                 middle_button_func = ZOOM, &
                     arrow_key_func = PAN

! Set the initial view as the point you are looking at, the point you are
! looking from, and the scale factors

type(cart3D), parameter :: &
     init_lookat = cart3D(0.0_gldouble, 0.0_gldouble, 0.0_gldouble), &
   init_lookfrom = cart3D(20.0_gldouble, -40.0_gldouble, 10.0_gldouble)

real(kind=gldouble), parameter :: &
   init_xscale_factor = 1.0_gldouble, &
   init_yscale_factor = 1.0_gldouble, &
   init_zscale_factor = 1.0_gldouble

! -------- end of Initial configuration ------

contains

!          -------------
subroutine reset_to_init
!          -------------

! This resets the view to the initial configuration

type(sphere3D) :: slookfrom

slookfrom = cart2sphere(init_lookfrom-init_lookat)
angle%x = -180.0_gldouble*slookfrom%theta/PI - 90.0_gldouble
angle%y = -180.0_gldouble*slookfrom%phi/PI
shift%x = 0.0_gldouble
shift%y = 0.0_gldouble
shift%z = -slookfrom%rho
xscale_factor = init_xscale_factor
yscale_factor = init_yscale_factor
zscale_factor = init_zscale_factor

call glutPostRedisplay

return
end subroutine reset_to_init

!          ----------
subroutine reset_view
!          ----------

! This routine resets the view to the current orientation and scale

call glMatrixMode(GL_MODELVIEW)
call glPopMatrix
call glPushMatrix
call glTranslated(shift%x, shift%y, shift%z)
call glRotated(angle%x, 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
call glRotated(angle%y, cos(PI*angle%x/180.0_gldouble), &
               -sin(PI*angle%x/180.0_gldouble), 0.0_gldouble)
call glTranslated(-init_lookat%x, -init_lookat%y, -init_lookat%z)
call glScaled(xscale_factor,yscale_factor,zscale_factor)

return
end subroutine reset_view

!          ---------------
subroutine view_from_above
!          ---------------

! This sets the view to be from straight above

  type(sphere3D) :: slookfrom

  slookfrom = cart2sphere(cart3D(0.0,0.0,1.0))
  angle%x = -180.0_gldouble*slookfrom%theta/PI
  angle%y = -180.0_gldouble*slookfrom%phi/PI
  
  call glutPostRedisplay
  
return
end subroutine view_from_above

!          -----
subroutine mouse(button, state, x, y)
!          -----
integer(kind=glcint), intent(in out) :: button, state, x, y

! This gets called when a mouse button changes
! write(*,*)moving_a
if (moving_a) then
   if (button == GLUT_LEFT_BUTTON .and. state == GLUT_DOWN) then
      moving_left = .true.
      begin_left = cart2D(x,y)
   endif
   if (button == GLUT_LEFT_BUTTON .and. state == GLUT_UP) then
      moving_left = .false.
   endif
else
   if (button == GLUT_LEFT_BUTTON .and. state == GLUT_DOWN) then
      moving_left = .true.
      begin_left = cart2D(x,y)
   endif
   if (button == GLUT_LEFT_BUTTON .and. state == GLUT_UP) then
      moving_left = .false.
   endif
  if (button == GLUT_MIDDLE_BUTTON .and. state == GLUT_DOWN) then
     moving_middle = .true.
     begin_middle = cart2D(x,y)
  endif
  if (button == GLUT_MIDDLE_BUTTON .and. state == GLUT_UP) then
     moving_middle = .false.
  endif
endif
end subroutine mouse

!          ------
subroutine motion(x, y)
!          ------
integer(kind=glcint), intent(in out) :: x, y

! This gets called when the mouse moves

integer :: button_function
type(cart2D) :: begin
real(kind=gldouble) :: factor

! Determine and apply the button function

if (moving_left) then
   if (moving_a) then
      button_function = left_button_func
      esfera2Dclick(1)=begin_left%x
      esfera2Dclick(2)=begin_left%y
   else
      button_function = left_button_func
      begin = begin_left
   endif
else if(moving_middle) then
   button_function = middle_button_func
   begin = begin_middle
end if

select case(button_function)
case (ZOOM)
   if (y < begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(begin%y-y))
   else if (y > begin%y) then
      factor = 1.0_gldouble + .002_gldouble*(y-begin%y)
   else
      factor = 1.0_gldouble
   end if
   shift%z = factor*shift%z
case (PAN)
   shift%x = shift%x + .01*(x - begin%x)
   shift%y = shift%y - .01*(y - begin%y)
case (ROTATE)
   if (moving_a) then
      esfera2D(1) = esfera2D(1) + (x - esfera2Dclick(1))/5
      esfera2D(2) = esfera2D(2) + (y - esfera2Dclick(2))/5

      c1=cos(esfera2D(1)*PI/180)
      s1=sin(esfera2D(1)*PI/180)
      c2=cos(esfera2D(2)*PI/180)
      s2=sin(esfera2D(2)*PI/180)

      ex(1)=2*c1*s1**2*c2-2*c1*s1**2+c1
      ex(2)=(2*s1**3-s1)*c2-2*s1**3+2*s1
      ex(3)=-s1*s2

      ey(1)=(2*s1**3-2*s1)*c2-2*s1+s1
      ey(2)=(c1-2*c1*s1**2)*c2+2*c1*s1**2
      ey(3)=c1*s2

      ez(1)=2*c1*s1*s2
      ez(2)=(2*s1**2-1)*s2
      ez(3)=c2
      
      write(*,*)'glIsList(13)=',glIsList(13)
      if (glIsList(13)) then
         call glDeleteLists(13,1)
         write(*,*)'delete list', glGetError()
      endif
      call create_maxis_list2()
   else
      angle%x = angle%x + (x - begin%x)
      angle%y = angle%y + (y - begin%y)
   endif
case (SCALEX)
   if (y < begin%y) then
     factor = 1.0_gldouble + .002_gldouble*(begin%y-y)
   else if (y > begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(y-begin%y))
   else
      factor = 1.0_gldouble
   end if
   xscale_factor = xscale_factor * factor
case (SCALEY)
   if (y < begin%y) then
      factor = 1.0_gldouble + .002_gldouble*(begin%y-y)
   else if (y > begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(y-begin%y))
   else
      factor = 1.0_gldouble
   end if
   yscale_factor = yscale_factor * factor
case (SCALEZ)
   if (y < begin%y) then
      factor = 1.0_gldouble + .002_gldouble*(begin%y-y)
   else if (y > begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(y-begin%y))
   else
      factor = 1.0_gldouble
   end if
   zscale_factor = zscale_factor * factor
end select

! update private variables and redisplay

if (moving_left) then
   begin_left = cart2D(x,y)
   esfera2Dclick(1) = begin_left%x
   esfera2Dclick(2) = begin_left%y
else if(moving_middle) then
   begin_middle = cart2D(x,y)
endif

if (moving_left .or. moving_middle) then
   call glutPostRedisplay
endif

return

end subroutine motion

!          ------
subroutine arrows(key, x, y)
!          ------
integer(glcint), intent(in out) :: key, x, y

! This routine handles the arrow key operations

real(kind=gldouble) :: factor

select case(arrow_key_func)
case(ZOOM)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble + .02_gldouble
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case default
      factor = 1.0_gldouble
   end select
   shift%z = factor*shift%z
case(PAN)
   select case(key)
   case(GLUT_KEY_LEFT)
      shift%x = shift%x - .02
   case(GLUT_KEY_RIGHT)
      shift%x = shift%x + .02
   case(GLUT_KEY_DOWN)
      shift%y = shift%y - .02
   case(GLUT_KEY_UP)
      shift%y = shift%y + .02
   end select
case(ROTATE)
   select case(key)
   case(GLUT_KEY_LEFT)
      angle%x = angle%x - 1.0_gldouble
   case(GLUT_KEY_RIGHT)
      angle%x = angle%x + 1.0_gldouble
   case(GLUT_KEY_DOWN)
      angle%y = angle%y + 1.0_gldouble
   case(GLUT_KEY_UP)
      angle%y = angle%y - 1.0_gldouble
   end select
case(SCALEX)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble + .02_gldouble
   case default
      factor = 1.0_gldouble
   end select
   xscale_factor = xscale_factor * factor
case(SCALEY)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble + .02_gldouble
   case default
      factor = 1.0_gldouble
   end select
   yscale_factor = yscale_factor * factor
case(SCALEZ)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble + .02_gldouble
   case default
      factor = 1.0_gldouble
   end select
   zscale_factor = zscale_factor * factor

end select
   
call glutPostRedisplay

return
end subroutine arrows

!          ------------
subroutine menu_handler(value)
!          ------------
integer(kind=glcint), intent(in out) :: value

! This routine handles the first level entries in the menu

select case(value)

case(RESET)
   call reset_to_init
case(QUIT)
   stop

end select

return
end subroutine menu_handler

!          ---------------
subroutine set_left_button(value)
!          ---------------
integer(kind=glcint), intent(in out) :: value

! This routine sets the function of the left button as given by menu selection

left_button_func = value

return
end subroutine set_left_button

!          -----------------
subroutine set_middle_button(value)
!          -----------------
integer(kind=glcint), intent(in out) :: value

! This routine sets the function of the middle button as given by menu selection

middle_button_func = value

return
end subroutine set_middle_button

!          --------------
subroutine set_arrow_keys(value)
!          --------------
integer(kind=glcint), intent(in out) :: value

! This routine sets the function of the arrow keys as given by menu selection

arrow_key_func = value

return
end subroutine set_arrow_keys

!        ------------------
function view_modifier_init() result(menuid)
!        ------------------
integer(kind=glcint) :: menuid

! This initializes the view modifier variables and sets initial view.
! It should be called immediately after glutCreateWindow

integer(kind=glcint) :: button_left, button_middle, arrow_keys

! set the callback functions

call glutMouseFunc(mouse)
call glutMotionFunc(motion)
call glutSpecialFunc(arrows)

! create the menu

button_left = glutCreateMenu(set_left_button)
call glutAddMenuEntry("rotate",ROTATE)
call glutAddMenuEntry("zoom",ZOOM)
call glutAddMenuEntry("pan",PAN)
call glutAddMenuEntry("scale x",SCALEX)
call glutAddMenuEntry("scale y",SCALEY)
call glutAddMenuEntry("scale z", SCALEZ)
button_middle = glutCreateMenu(set_middle_button)
call glutAddMenuEntry("rotate",ROTATE)
call glutAddMenuEntry("zoom",ZOOM)
call glutAddMenuEntry("pan",PAN)
call glutAddMenuEntry("scale x",SCALEX)
call glutAddMenuEntry("scale y",SCALEY)
call glutAddMenuEntry("scale z", SCALEZ)
arrow_keys = glutCreateMenu(set_arrow_keys)
call glutAddMenuEntry("rotate",ROTATE)
call glutAddMenuEntry("zoom",ZOOM)
call glutAddMenuEntry("pan",PAN)
call glutAddMenuEntry("scale x",SCALEX)
call glutAddMenuEntry("scale y",SCALEY)
call glutAddMenuEntry("scale z", SCALEZ)
menuid = glutCreateMenu(menu_handler)
call glutAddSubMenu("left mouse button",button_left)
call glutAddSubMenu("middle mouse button",button_middle)
call glutAddSubMenu("arrow keys",arrow_keys)
call glutAddMenuEntry("reset to initial view",RESET)
call glutAddMenuEntry("quit",QUIT)

! set the perspective

call glMatrixMode(GL_PROJECTION)
call gluPerspective(10.0_gldouble, 1.0_gldouble, 0.1_gldouble, 200.0_gldouble)

! set the initial view

call glPushMatrix
call reset_to_init

return
end function view_modifier_init

!        -----------
function sphere2cart(spoint) result(cpoint)
!        -----------
type(sphere3D), intent(in) :: spoint
type(cart3D) :: cpoint

! This converts a 3D point from spherical to cartesean coordinates

real(kind=gldouble) :: t,p,r

t=spoint%theta
p=spoint%phi
r=spoint%rho

cpoint%x = r*cos(t)*sin(p)
cpoint%y = r*sin(t)*sin(p)
cpoint%z = r*cos(p)

return
end function sphere2cart

!        -----------
function cart2sphere(cpoint) result(spoint)
!        -----------
type(cart3D), intent(in) :: cpoint
type(sphere3D) :: spoint

! This converts a 3D point from cartesean to spherical coordinates

real(kind=gldouble) :: x,y,z

x=cpoint%x
y=cpoint%y
z=cpoint%z

spoint%rho = sqrt(x*x+y*y+z*z)
if (x==0.0_gldouble .and. y==0.0_gldouble) then
   spoint%theta = 0.0_gldouble
else
   spoint%theta = atan2(y,x)
end if
if (spoint%rho == 0.0_gldouble) then
   spoint%phi = 0.0_gldouble
else
   spoint%phi = acos(z/spoint%rho)
endif

return
end function cart2sphere

!        ------------------
function cart3D_plus_cart3D(cart1,cart2) result(cart3)
!        ------------------
type(cart3D), intent(in) :: cart1, cart2
type(cart3D) :: cart3

! Compute the sum of two 3D cartesean points

cart3%x = cart1%x + cart2%x
cart3%y = cart1%y + cart2%y
cart3%z = cart1%z + cart2%z

return
end function cart3D_plus_cart3D

!        -------------------
function cart3D_minus_cart3D(cart1,cart2) result(cart3)
!        -------------------
type(cart3D), intent(in) :: cart1, cart2
type(cart3D) :: cart3

! Compute the difference of two 3D cartesean points

cart3%x = cart1%x - cart2%x
cart3%y = cart1%y - cart2%y
cart3%z = cart1%z - cart2%z

return
end function cart3D_minus_cart3D

end module view_modifier

