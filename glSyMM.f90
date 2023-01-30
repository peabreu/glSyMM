! cvs generated header
! $Id: glSyMM.f90,v 1.35 2004/04/01 20:52:53 paulo Exp $
!
Module glSyMM_mod
  Use opengl_gl
!  Use opengl_glu
  Use opengl_glut
  Use moving_axis
  Implicit None
  real(kind=gldouble), parameter :: PI = 3.141592653589793_gldouble
  Integer,Dimension (:), Allocatable, Save:: z
  Real(kind=glfloat),Parameter :: zero=0.0_glfloat, one=1.0_glfloat
  Real(kind=glfloat), Dimension(4) :: & 
       atom_ambient = (/ 0.8_glfloat, 0.8_glfloat, 0.8_glfloat, one /), &
       atom_diffuse = (/ 0.4_glfloat, 0.4_glfloat, 0.4_glfloat, one /), &
       atom_specular = (/ 0.9_glfloat, 0.9_glfloat, 0.9_glfloat, one /), &
       
       bond_ambient = (/ 0.8_glfloat, 0.8_glfloat, 0.8_glfloat, one /), &
       bond_diffuse = (/ 0.4_glfloat, 0.4_glfloat, 0.4_glfloat, one /), &
       bond_specular = (/ 0.3_glfloat, 0.3_glfloat, 0.3_glfloat, one /), &
       
       light_ambient = (/ 0.26_glfloat, 0.26_glfloat, 0.26_glfloat, one /), &
       light_diffuse = (/ 0.2_glfloat, 0.2_glfloat, 0.2_glfloat, one /), &
       light_specular = (/ 0.13_glfloat, 0.13_glfloat, 0.13_glfloat, one /)
  
  Real(kind=glfloat),Dimension(92,3),Save :: iupac_colors
  Real(kind=glfloat),Dimension(92),Save :: v_radius
  Real(kind=glfloat),Dimension(92),Save :: vdW_radius
  Real(kind=glclampf), Dimension(4),Save :: background
  Real(kind=glclampf), Dimension(4),Save :: & 
       background_default= (/ 20/255.0_glclampf, 0/255.0_glclampf, 29/255.0_glclampf,&
       1.0_glclampf /)
  Character(len=2), Dimension(103), Save :: symbols
  Real, Dimension(103), Save :: atomic_masses
  Real(kind=gldouble), Dimension(:), Allocatable, Save::gl_radius
  Data v_radius/0.200,0.286,0.340,0.589,0.415,0.400,0.400, &
       0.400,0.320,0.423,0.485,0.550,0.675,0.600, &
       0.525,0.510,0.495,0.508,0.665,0.495,0.720, &
       0.735,0.665,0.675,0.675,0.670,0.615,0.750, &
       0.760,0.725,0.610,0.585,0.605,0.610,0.605, &
       0.524,0.735,0.560,0.890,0.780,0.740,0.735, &
       0.675,0.700,0.725,0.750,0.795,0.845,0.815, &
       0.730,0.730,0.735,0.700,0.577,0.835,0.670, &
       0.935,0.915,0.910,0.905,0.900,0.900,0.995, &
       0.895,0.880,0.875,0.870,0.865,0.860,0.970, &
       0.860,0.785,0.715,0.685,0.675,0.685,0.660, &
       0.750,0.750,0.850,0.775,0.770,0.770,0.840, &
       1.000,1.000,1.000,0.950,0.940,0.895,0.805, &
       0.790/
  Data vdW_radius/0.430,0.741,0.880,0.550,1.030,0.900,0.880, &
       0.880,0.840,0.815,1.170,1.300,1.550,1.400, &
       1.250,1.220,1.190,0.995,1.530,1.190,1.640, &
       1.670,1.530,1.550,1.550,1.540,1.530,1.700, &
       1.720,1.650,1.420,1.370,1.410,1.420,1.410, &
       1.069,1.670,1.320,1.980,1.760,1.680,1.670, &
       1.550,1.600,1.650,1.700,1.790,1.890,1.830, &
       1.660,1.660,1.670,1.600,1.750,1.870,1.540, &
       2.070,2.030,2.020,2.010,2.000,2.000,2.190, &
       1.990,1.960,1.950,1.940,1.930,1.920,2.140, &
       1.920,1.770,1.630,1.570,1.550,1.570,1.520, &
       1.700,1.700,1.900,1.750,1.740,1.740,1.880, &
       0.200,0.200,0.200,2.100,2.080,1.990,1.810, &
       1.780/
  ! the array atomic_masses contains the molar masses for atoms
  Data atomic_masses/1.00794, 4.002602, 6.941, 9.01218, 10.811, 12.011 &
       ,14.00674, 15.9994, 18.998403, 20.1797, 22.989768, 24.305 &
       ,26.981539, 28.0855, 30.973762, 32.066, 35.4527, 39.948, 39.0983 &
       ,40.078, 44.95591, 47.88, 50.9415, 51.9961, 54.93805, 55.847 &
       ,58.9332, 58.6934, 63.546, 65.39, 69.723, 72.61, 74.92159, 78.96 &
       ,79.904, 83.8, 85.4678, 87.62, 88.90585, 91.224, 92.90638, 95.94 &
       ,97.9072, 101.07, 102.9055, 106.42, 107.8682, 112.411, 114.818 &
       ,118.71, 121.760, 127.6, 126.90447, 131.29, 132.90543, 137.327 &
       ,138.9055, 140.115, 140.90765, 144.24, 144.9127, 150.36, 151.965 &
       ,157.25, 158.92534, 162.50, 164.93032, 167.26, 168.93421, 173.04 &
       ,174.967, 178.49, 180.9479, 183.84, 186.207, 190.23, 192.22 &
       ,195.08, 196.96654, 200.59, 204.3833, 207.2, 208.98037, 208.9824 &
       ,209.9871, 222.0176, 223.0197, 226.0254, 227.0278, 232.0381 &
       ,231.03588, 238.0289, 237.048, 244.0642, 243.0614, 247.0703 &
       ,247.0703, 251.0796, 252.083, 257.0951, 258.1, 259.1009, 262.11/
  ! an array containing the symbols of the elements
  ! strict IUPAC symbols ie first letter uppercase second letter lowercase
  Data symbols/"H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", &
       "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", &
       "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", &
       "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", &
       "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", &
       "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", &
       "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", &
       "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", &
       "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", &
       "Es", "Fm", "Md", "No", "Lr"/  
    
  ! derived types and operations
  Type atom
     Integer :: z
     Character(len=2) :: symbol
     Real(kind=glfloat), Dimension (3) :: coords
  End Type atom
  
  Interface Operator(==)
     Module Procedure compare_atoms
  End Interface

  Type atom_data
     Character(len=2) :: symbol
     Real(kind=glfloat) :: vdW
     Real(kind=glfloat) :: radius
     Real(kind=glfloat), Dimension(3) :: color
     Real(kind=glfloat) :: mass
  End Type atom_data
  
  Type(atom_data), Dimension(92), Save :: atomic_data
  Type(atom), Allocatable, Dimension(:), Save :: molecule, molecule_OP
  Type(atom), Allocatable, Dimension(:), Save :: molecule_CM, molecule_IA
  Real(kind=glfloat), Dimension(3) :: center_of_mass
  Real, Dimension(3,3) :: inertial_tensor
  Real, Dimension(3) :: eigenvalues
  Real, Dimension (3,3) :: eigenvectors
  Real(kind=glfloat) :: maxsize=6.5_glfloat
  Logical, Save :: bonds, atoms, ghosts=.False., reflection=.True.
  Logical, Save :: light0_on=.true., light1_on=.false., light2_on=.true., light3_on=.false.
  Integer, Parameter :: &
       mirror_xy=1, &
       mirror_yz=2, &
       mirror_zx=3, &
       no_mirror=4
  Integer, Parameter :: &
       axis_x=1, &
       axis_y=2, &
       axis_z=3, &
       no_axis=4
  Logical,Save :: mxy=.False.,myz=.False.,mzx=.False.
  Logical,Save :: a_X=.False.,a_Y=.False.,a_Z=.False.
  Logical,Save :: a_mxy, inversion=.False.
  Integer,Save :: atoms_list=1, bonds_list=2, axis_list=3, &
       mirrorXY_list=4, mirrorYZ_list=5, mirrorZX_list=6, &
       axisX_list=7, axisY_list=8, axisZ_list=9, ghosts_list=10,&
       ghosts_list_inverted=11, ghosts_list_reflection=12, ghosts_list_stay=20, &
       moving_axis_list=13
  
!  Public :: bonds, atoms, make_menu
  
Contains
  Subroutine glSyMM_display()
    use opengl_gl
    use opengl_glu
    use opengl_glut
    use moving_axis
  end Subroutine glSyMM_display


  Subroutine create_mirror_list(plane)
    Use opengl_gl
    Use opengl_glu
    Use opengl_glut
    Use moving_axis
    
    Implicit None
    Character(1), Intent(in):: plane
    ! on entry plane contains the basis vector perpendicular to the mirror plane
    Select Case (plane)
    Case ('x') ! plane YZ
       Call glPushMatrix()
       Call glColor4fv(mirror_color)
       Call glBegin(GL_QUADS)
       Call glVertex3f(zero,-mirror_size,mirror_size)
       Call glVertex3f(zero,mirror_size,mirror_size)
       Call glVertex3f(zero,mirror_size,-mirror_size)
       Call glVertex3f(zero,-mirror_size,-mirror_size)
       Call glEnd
       Call glPopMatrix()
    Case ('y') ! plane ZX
       Call glPushMatrix()
       Call glColor4fv(mirror_color)
       Call glBegin(GL_QUADS)
       Call glVertex3f(-mirror_size,zero,mirror_size)
       Call glVertex3f(mirror_size,zero,mirror_size)
       Call glVertex3f(mirror_size,zero,-mirror_size)
       Call glVertex3f(-mirror_size,zero,-mirror_size)
       Call glEnd
       Call glPopMatrix()
    Case ('z') ! plane XY
       Call glPushMatrix()
       Call glColor4fv(mirror_color)
       Call glBegin(GL_QUADS)
       Call glVertex3f(-mirror_size,mirror_size,zero)
       Call glVertex3f(mirror_size,mirror_size,zero)
       Call glVertex3f(mirror_size,-mirror_size,zero)
       Call glVertex3f(-mirror_size,-mirror_size,zero)
       Call glEnd
       Call glPopMatrix()
    End Select
  End Subroutine create_mirror_list
  
  Subroutine draw_axis_list(axis)
    Use opengl_gl
    Use opengl_glu
    Use opengl_glut
    Use moving_axis
    Implicit None
    Character(1), Intent(in):: axis
    Select Case (axis)
    Case ('x') ! X axis
       Call glPushMatrix()
       Call glLineWidth(8.0_glfloat)
       Call glColor4fv(axis_color)
       Call glBegin(GL_LINES)
       Call glVertex3f(-mirror_size,zero,zero)
       Call glVertex3f(mirror_size,zero,zero)
       Call glEnd
       Call glPopMatrix()
    Case ('y') ! Y axis
       Call glPushMatrix()
       Call glLineWidth(8.0_glfloat)
       Call glColor4fv(axis_color)
       Call glBegin(GL_LINES)
       Call glVertex3f(zero,-mirror_size,zero)
       Call glVertex3f(zero,mirror_size,zero)
       Call glEnd
       Call glPopMatrix()
    Case ('z') ! Z axis
       Call glPushMatrix()
       Call glLineWidth(8.0_glfloat)
       Call glColor4fv(axis_color)
       Call glBegin(GL_LINES)
       Call glVertex3f(zero,zero,-mirror_size)
       Call glVertex3f(zero,zero,mirror_size)
       Call glEnd
       Call glPopMatrix()
    End Select
  End Subroutine draw_axis_list

  Subroutine init_radius
    Implicit None
    Integer::i
    Do i=1,92
       vdW_radius(i)=vdW_radius(i)/2.0
       v_radius(i)=v_radius(i)/2.0
       atomic_data(i)%vdW=vdW_radius(i)/2.0
       atomic_data(i)%radius=v_radius(i)/2.0
    Enddo
    
    !     vdW_radius(1)=1.17/3 ! hydrogen
    !     vdW_radius(6)=1.75/3 ! carbon
    !     vdW_radius(7)=1.55/3 ! nitrogen
    !     vdW_radius(8)=1.40/3 ! oxygen
    !     vdW_radius(15)= 1.28/3 ! phosphorus
    !     vdW_radius(16)= 1.80/3 ! sulfur
  End Subroutine init_radius
  
  Subroutine init_colors
    Implicit None
    Integer::i
    Do i=1,92
       iupac_colors(i,1)=0.0 ! default (green4, lightgreen)
       iupac_colors(i,2)=139/255.0
       iupac_colors(i,3)=0.0
    Enddo
    
    iupac_colors(1,1)=1.0 ! hydrogen (white,grey70)
    iupac_colors(1,2)=1.0
    iupac_colors(1,3)=1.0
    
    iupac_colors(6,1)=153/255.0 ! carbon (grey60,white)
    iupac_colors(6,2)=153/255.0
    iupac_colors(6,3)=153/255.0
    
    iupac_colors(7,1)=176/255.0 ! nitrogen  (lightsteelblue3,slateblue1)
    iupac_colors(7,2)=196/255.0
    iupac_colors(7,3)=222/255.0
    
    iupac_colors(8,1)=255.0/255.0 ! oxygen (red,lightpink)
    iupac_colors(8,2)=0.0/255.0
    iupac_colors(8,3)=0.0/255.0
    
    iupac_colors(15,1)=147/255.0 ! phosphorus (mediumpurple,palevioletred)
    iupac_colors(15,2)=112/255.0
    iupac_colors(15,3)=219/255.0
    
    iupac_colors(16,1)=139/255.0 ! sulfur (yellow4,yellow1)
    iupac_colors(16,2)=139/255.0
    iupac_colors(16,3)=0.0/255.0
    
    Do i=1,92
       atomic_data(i)%color(1)=iupac_colors(i,1)
       atomic_data(i)%color(2)=iupac_colors(i,2)
       atomic_data(i)%color(3)=iupac_colors(i,3)
    Enddo
    
  End Subroutine init_colors

  Subroutine init_symbols()
    Implicit None
    Integer :: i
    Do i=1,92
       atomic_data(i)%symbol=symbols(i)
    Enddo
  End Subroutine init_symbols

  Subroutine init_masses()
    Implicit None
    Integer :: i
    Do i=1,92
       atomic_data(i)%mass=atomic_masses(i)
    Enddo
  End Subroutine init_masses
  
  Function cm(a) !result(molecule_CM)
    Real(kind=glfloat),Dimension(3) :: cm
    Type(atom), Dimension(:) :: a
!    real(kind=glfloat), dimension(3) :: molecule_cm
    Real :: mT
    Integer :: natoms, i
    cm=0.0
    mT=0.0
    natoms=Ubound(a,1)
    Do i=1,natoms
       mT=mT+atomic_data(a(i)%z)%mass
       cm(1)=cm(1)+a(i)%coords(1)*atomic_data(a(i)%z)%mass
       cm(2)=cm(2)+a(i)%coords(2)*atomic_data(a(i)%z)%mass
       cm(3)=cm(3)+a(i)%coords(3)*atomic_data(a(i)%z)%mass
    Enddo
    cm(1)=cm(1)/mT
    cm(2)=cm(2)/mT
    cm(3)=cm(3)/mT
    Write(*,*)'mT=',mT
  End Function cm
    
  Logical Function compare_atoms(a,b)
! this function only compares by coordinates and atomic number ... phear !
    Implicit None
    Type(atom), Intent(in) :: a,b
    Real(kind=glfloat) :: tol=0.02
    compare_atoms=((a%z==b%z).And. &
         (Abs(a%coords(1)-b%coords(1))<tol) .And. &
         (Abs(a%coords(2)-b%coords(2))<tol) .And. &
         (Abs(a%coords(3)-b%coords(3))<tol))
  End Function compare_atoms

  Subroutine make_menu(submenuid)
    Use opengl_gl
    Use opengl_glu
    Use opengl_glut
    Use view_modifier
    Implicit None
    
    Integer(kind=glcint), Intent(in) :: submenuid
    Integer(kind=glcint) :: menuid, mirror_menu, axis_menu, top_menu, invertion_menu,&
         improper_rotation_menu, top_menuid, background_menu, &
         cx_menuid, cy_menuid, cz_menuid, sx_menuid, sy_menuid, sz_menuid, lights_menuid
         
    cx_menuid = glutCreateMenu(general_c_axis)
    Call glutAddMenuEntry("2",12)
    Call glutAddMenuEntry("3",13)
    Call glutAddMenuEntry("4",14)
    Call glutAddMenuEntry("5",15)
    Call glutAddMenuEntry("6",16)
    Call glutAddMenuEntry("7",17)
    Call glutAddMenuEntry("8",18)

    cy_menuid = glutCreateMenu(general_c_axis)
    Call glutAddMenuEntry("2",22)
    Call glutAddMenuEntry("3",23)
    Call glutAddMenuEntry("4",24)
    Call glutAddMenuEntry("5",25)
    Call glutAddMenuEntry("6",26)
    Call glutAddMenuEntry("7",27)
    Call glutAddMenuEntry("8",28)

    cz_menuid = glutCreateMenu(general_c_axis)
    Call glutAddMenuEntry("2",32)
    Call glutAddMenuEntry("3",33)
    Call glutAddMenuEntry("4",34)
    Call glutAddMenuEntry("5",35)
    Call glutAddMenuEntry("6",36)
    Call glutAddMenuEntry("7",37)
    Call glutAddMenuEntry("8",38)

    sx_menuid = glutCreateMenu(general_s_axis)
    Call glutAddMenuEntry("1",11)
    Call glutAddMenuEntry("2",12)
    Call glutAddMenuEntry("3",13)
    Call glutAddMenuEntry("4",14)
    Call glutAddMenuEntry("5",15)
    Call glutAddMenuEntry("6",16)
    Call glutAddMenuEntry("7",17)
    Call glutAddMenuEntry("8",18)

    sy_menuid = glutCreateMenu(general_s_axis)
    Call glutAddMenuEntry("1",21)
    Call glutAddMenuEntry("2",22)
    Call glutAddMenuEntry("3",23)
    Call glutAddMenuEntry("4",24)
    Call glutAddMenuEntry("5",25)
    Call glutAddMenuEntry("6",26)
    Call glutAddMenuEntry("7",27)
    Call glutAddMenuEntry("8",28)

    sz_menuid = glutCreateMenu(general_s_axis)
    Call glutAddMenuEntry("1",31)
    Call glutAddMenuEntry("2",32)
    Call glutAddMenuEntry("3",33)
    Call glutAddMenuEntry("4",34)
    Call glutAddMenuEntry("5",35)
    Call glutAddMenuEntry("6",36)
    Call glutAddMenuEntry("7",37)
    Call glutAddMenuEntry("8",38)

    improper_rotation_menu = glutCreateMenu(draw_improper)
    Call glutAddMenuEntry("axis X",axis_x)
    Call glutAddMenuEntry("axis Y",axis_y)
    Call glutAddMenuEntry("axis Z",axis_z)
    Call glutAddMenuEntry("no axis",no_axis)
    
    mirror_menu = glutCreateMenu(draw_mirror)
    Call glutAddMenuEntry("mirror XY",mirror_xy)
    Call glutAddMenuEntry("mirror YZ",mirror_yz)
    Call glutAddMenuEntry("mirror ZX",mirror_zx)
    Call glutAddMenuEntry("no mirror",no_mirror)
    
    axis_menu = glutCreateMenu(draw_axis)
    Call glutAddMenuEntry("axis X",axis_x)
    Call glutAddMenuEntry("axis Y",axis_y)
    Call glutAddMenuEntry("axis Z",axis_z)
    Call glutAddMenuEntry("no axis",no_axis)

    invertion_menu = glutCreateMenu(invert_menu)
    Call glutAddMenuEntry("invert",1)

    background_menu = glutCreateMenu(background_color_menu)
    Call glutAddMenuEntry("Black",0)
    Call glutAddMenuEntry("White",1)
    Call glutAddMenuEntry("Grey",2)
    Call glutAddMenuEntry("LightGrey",3)
    Call glutAddMenuEntry("DarkGrey",4)
    Call glutAddMenuEntry("Peachy",5)
    Call glutAddMenuEntry("DarkBlue",6)
    Call glutAddMenuEntry("DarkGreen",7)
    Call glutAddMenuEntry("Default",8)

    lights_menuid = glutCreateMenu(lights_menu)
    Call glutAddMenuEntry("Light0",0)
    Call glutAddMenuEntry("Light1",1)
    Call glutAddMenuEntry("Light2",2)
    Call glutAddMenuEntry("Light3",3)
    
    ! is this needed or can one use glutnullfunc ???
    !      top_menu = glutCreateMenu(does_nothing)
    menuid = glutCreateMenu(menu_handler)
    Call glutAddSubMenu("View Modifier",submenuid)
    Call glutAddSubMenu("Background",background_menu)
    Call glutAddSubMenu("Lights",lights_menuid)
    Call glutAddSubMenu("Mirror planes",mirror_menu)
!    Call glutAddSubMenu("Proper Rotation Axis",axis_menu)
!    Call glutAddSubMenu("Imroper Rotation Axis",improper_rotation_menu)
    Call glutAddSubMenu("Inversion Center", invertion_menu)

!    Call glutAddSubMenu("TEST", cx_menuid)
    Call glutAddSubMenu ("Cx axis", cx_menuid)
    Call glutAddSubMenu ("Cy axis", cy_menuid)
    Call glutAddSubMenu ("Cz axis", cz_menuid)

    Call glutAddSubMenu ("Sx axis", sx_menuid)
    Call glutAddSubMenu ("Sy axis", sy_menuid)
    Call glutAddSubMenu ("Sz axis", sz_menuid)
!    Call glutAddMenu ("Dummy",top_menu)

!    call glutAddSubMenu("left mouse button",button_left)
!    call glutAddSubMenu("middle mouse button",button_middle)
!    call glutAddSubMenu("arrow keys",arrow_keys)

!    call glutAddMenuEntry("reset to initial view",10) ! RESET
    call glutAddMenuEntry("view from above",11) ! ABOVE

    call glutAddMenuEntry("quit",12) ! QUIT
    call glutAttachMenu(GLUT_RIGHT_BUTTON)
  End Subroutine make_menu

!          ------------
  subroutine menu_handler(value)
!          ------------
    use view_modifier
    integer(kind=glcint), intent(in out) :: value

! This routine handles the first level entries in the menu

    select case(value)
       
!    case(10) !RESET)
!       call reset_to_init
    case(11)! ABOVE)
       call view_from_above
    case(12) !QUIT)
       stop
       
    end select

    return
  end subroutine menu_handler

  
  Subroutine does_nothing(value)
    implicit None
    integer, intent(in out) :: value
  end Subroutine does_nothing

  Subroutine draw_mirror(selection)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Integer, Intent(in out) :: selection
    Select Case (selection)
    Case (mirror_xy)
       mxy=.Not.mxy
       Call reflect_on_plane('z')
    Case (mirror_yz)
       myz=.Not.myz
       Call reflect_on_plane('x')
    Case (mirror_zx)
       mzx=.Not.mzx
       Call reflect_on_plane('y')
    Case (no_mirror)
       mxy=.False.
       myz=.False.
       mzx=.False.
    End Select
  End Subroutine draw_mirror




  Subroutine general_c_axis(selection)
    Use opengl_gl
    !    Use opengl_glu
    Use opengl_glut
    ! selection=a*10+o
    ! a= axis (x=1,y=2,z=3)
    ! o= order of rotation
    Implicit None
    Integer, Intent(in out) :: selection
    Real(kind=glfloat) :: ang
    Integer :: kk, a, o
    o=mod(selection,10)
    a=selection/10
    ang=360./o
    !    write(*,*)"a=",a,"o=",o,selection
    Select Case (a)
    Case (axis_x)
          a_X=.True.
          a_Y=.False.
          a_Z=.False.
          Call rotate_around_axis('x',ang)
       Case (axis_y)
          a_X=.False.
          a_Y=.True.
          a_Z=.False.
       Call rotate_around_axis('y',ang)
    Case (axis_z)
          a_X=.False.
          a_Y=.False.
          a_Z=.True.
       Call rotate_around_axis('z',ang)
    Case (no_axis)
       a_X=.False.
       a_Y=.False.
       a_Z=.False.
       ghosts=.False.
    End Select
  End Subroutine general_c_axis


  Subroutine general_s_axis(selection)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Integer, Intent(in out) :: selection
    Real(kind=glfloat) :: ang
    Integer :: kk,a,o
! selection=a*10+o
! a= axis (x=1,y=2,z=3)
! o= order of improper rotation
    o=mod(selection,10)
    a=selection/10
    ang=360./o
!    write(*,*)"a=",a,"o=",o,selection

    Select Case (a)
    Case (axis_x)
       a_X=.True.
       a_Y=.False.
       a_Z=.False.
       Call improper_rotation('x',ang)
    Case (axis_y)
       a_X=.False.
       a_Y=.True.
       a_Z=.False.
       Call improper_rotation('y',ang)
    Case (axis_z)
       a_X=.False.
       a_Y=.False.
       a_Z=.True.
       Call improper_rotation('z',ang)
    Case (no_axis)
       a_X=.False.
       a_Y=.False.
       a_Z=.False.
       ghosts=.False.
    End Select
  End Subroutine general_s_axis
  
  Subroutine draw_axis(selection)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Integer, Intent(in out) :: selection
    Real(kind=glfloat) :: ang
    Integer :: kk
    Select Case (selection)
    Case (axis_x)
       a_X=.Not.a_X
       Write(*,'(a)',advance='no')'Rotation angle= '
       Read(*,*)ang
       Call rotate_around_axis('x',ang)
    Case (axis_y)
       a_Y=.Not.a_Y
       Write(*,'(a)',advance='no')'Rotation angle= '
       Read(*,*)ang
       Call rotate_around_axis('y',ang)
    Case (axis_z)
       Write(*,'(a)',advance='no')'Rotation angle= '
       Read(*,*)ang
       a_Z=.Not.a_Z
       Call rotate_around_axis('z',ang)
    Case (no_axis)
       a_X=.False.
       a_Y=.False.
       a_Z=.False.
       ghosts=.False.
    End Select
  End Subroutine draw_axis


  Subroutine draw_improper(selection)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Integer, Intent(in out) :: selection
    Real(kind=glfloat) :: ang
    Integer :: kk
    Select Case (selection)
    Case (axis_x)
       a_X=.True.
       Write(*,'(a)',advance='no')'Rotation angle= '
       Read(*,*)ang
       Call improper_rotation('x',ang)
    Case (axis_y)
       Write(*,'(a)',advance='no')'Rotation angle= '
       Read(*,*)ang
       a_Y=.True.
       Call improper_rotation('y',ang)
    Case (axis_z)
       Write(*,'(a)',advance='no')'Rotation angle= '
       Read(*,*)ang
       a_Z=.True.
       Call improper_rotation('z',ang)
    Case (no_axis)
       a_X=.False.
       a_Y=.False.
       a_Z=.False.
       ghosts=.False.
    End Select
  End Subroutine draw_improper

  Subroutine background_color_menu(selection)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Integer, Intent(in out) :: selection
!    Real(kind=glclampf), Dimension(4),Save :: background
!    Real(kind=glclampf), Dimension(4),Save :: background_default
    Select Case (selection)
    Case (0) ! Black
       background = (/ 0.0_glclampf, 0.0_glclampf, 0.0_glclampf, 1.0_glclampf /)
    Case (1) ! White
       background= (/ 1.0_glclampf, 1.0_glclampf, 1.0_glclampf,   1.0_glclampf /)     
    Case (2) ! Grey
       background= (/ 127/255.0_glclampf, 127/255.0_glclampf, 127/255.0_glclampf,&
      1.0_glclampf /)
    Case (3) ! LightGrey
       background= (/ 180/255.0_glclampf, 180/255.0_glclampf, 180/255.0_glclampf,&
      1.0_glclampf /)
    Case (4) ! DarkGrey
       background= (/ 50/255.0_glclampf, 50/255.0_glclampf, 50/255.0_glclampf,&
      1.0_glclampf /)
    Case (5) ! Peachy
       background= (/ 205/255.0_glclampf, 179/255.0_glclampf, 139/255.0_glclampf,&
            1.0_glclampf /)
    Case (6) ! DarkBlue
       background= (/ 20/255.0_glclampf, 20/255.0_glclampf, 80/255.0_glclampf,&
            1.0_glclampf /)
    Case (7) ! DarkGreen
       background= (/ 20/255.0_glclampf, 80/255.0_glclampf, 20/255.0_glclampf,&
            1.0_glclampf /)
    Case (8) ! Default
       background= background_default
    End Select
    Call glClearColor(background(1),background(2),background(3),background(4))
!    write(*,*)background,'sub'
  End Subroutine background_color_menu

  Subroutine invert_menu(selection)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Integer, Intent(in out) :: selection
!    inversion=.not.inversion
    Call invert_around_center()
  End Subroutine invert_menu

  Subroutine lights_menu(selection)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Integer, Intent(in out) :: selection
    Select Case (selection)
    Case (0)
       light0_on=.not. light0_on
       if (light0_on) then 
          call glEnable(GL_LIGHT0)
       else
          call glDisable(GL_LIGHT0)
       endif
    Case (1)
       light1_on=.not. light1_on
       if (light1_on) then 
          call glEnable(GL_LIGHT1)
       else
          call glDisable(GL_LIGHT1)
       endif
    Case (2)
       light2_on=.not. light2_on
       if (light2_on) then 
          call glEnable(GL_LIGHT2)
       else
          call glDisable(GL_LIGHT2)
       endif
    Case (3)
       light3_on=.not. light3_on
       if (light3_on) then 
          call glEnable(GL_LIGHT3)
       else
          call glDisable(GL_LIGHT3)
       endif
    End Select
!    write(*,*)light0_on,light1_on,light2_on,light3_on
  End Subroutine lights_menu

!  Subroutine does_nothing()
!    Use opengl_gl
!    Use opengl_glu
!    Use opengl_glut
!    Implicit None
!    Integer, Intent(in out) :: selection
!  End Subroutine does_nothing
  
  Subroutine rotate_around_axis(axis,angle)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Use moving_axis

    Implicit None
    Character(1), Intent (in) :: axis
    Real(kind=glfloat),Parameter :: zero=0.0_glfloat, one=1.0_glfloat
    !      Character(1) :: axis='x'
    Real(kind=glfloat), Dimension (3) :: vec
    Real(kind=glfloat) :: spin=zero
    Real(kind=glfloat) :: angle,factor, c1,s1,c2,s2
    Integer :: kk,steps=12
!    write(*,*)'im SO here!'
!    write(*,*)moving_a, esfera2D

    Select Case (axis)
    Case ('x')
       if (moving_a) then
!          c1=cos(esfera2D(1)*PI/180)
!          s1=sin(esfera2D(1)*PI/180)
!          c2=cos(esfera2D(2)*PI/180)
!          s2=sin(esfera2D(2)*PI/180)
!          vec= (/c1, -s1*c2, s1*s2 /)
!          vec= (/ 2*c1*s1**2*c2-2*c1*s1**2+c1, (2*s1**3-2*s1)*c2-2*s1**3+s1, 2*c1*s1*s2 /)
          vec= ex
       else
          vec= (/ one, zero, zero /)
       endif
    Case ('y')
       if (moving_a) then 
!          c1=cos(esfera2D(1)*PI/180)
!          s1=sin(esfera2D(1)*PI/180)
!          c2=cos(esfera2D(2)*PI/180)
!          s2=sin(esfera2D(2)*PI/180)
!          vec= (/ s1, c1*c2, -c1*s2/)
!          vec= (/ (1-2*c1**2)*s1*c2+2*c1**2*s1, (2*c1**3-c1)*c2-2*c1**3+2*c1, (1-2*c1**2)*s2 /)
          vec= ey
       else
          vec= (/ zero, one, zero /)
       endif
    Case ('z')
       if (moving_a) then
!          c1=cos(esfera2D(1)*PI/180)
!          s1=sin(esfera2D(1)*PI/180)
!          c2=cos(esfera2D(2)*PI/180)
!          s2=sin(esfera2D(2)*PI/180)
!          vec= (/ zero, s2, c2 /)
!          vec= (/ -s1*s2, c1*s2, c2/)
          vec= ez
       else
          vec= (/ zero, zero, one /)
       endif
    Case default
       write(*,*) 'dunno how to rotate around that axis '//axis
       Stop
    End Select
    steps=steps/slow
    factor=angle/steps
    Do kk=0,steps
       spin=kk*factor
       Call glClear(Ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
       Call glPushMatrix()
       
       Call glCallList(axis_list)

       If (moving_a) then
!          call glPushMatrix()
!          call glRotated(esfera2D(1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
!          call glRotated(esfera2D(2), cos(PI*esfera2d(1)/180.0_gldouble), &
!               -sin(PI*esfera2D(1)/180.0_gldouble), 0.0_gldouble)
          Call glCallList(moving_axis_list)
!                                                                               !
!                                                                               !
!                                                                               !
!                         x2(1-c)+c  xy(1-c)-zs xz(1-c)+ys 0                    !
!                         yx(1-c)+zs y2(1-c)+c  yz(1-c)-xs 0                    !
!                         xz(1-c)-ys yz(1-c)+xs z2(1-c)+c  0                    !
!                             0          0          0      1                    !
!                                                                               !
!       Where  c=cos(angle),  s=sin(angle),  and  ||(x,y,z)||=1 (if not, the GL !
!       will normalize this vector).                                            !
!                                                                               !
!          Call glPopMatrix()
       endif
       
       If (atoms) Call glCallList(atoms_list)
       If (bonds) Call glCallList(bonds_list)
       
       if (moving_a) then
          call glPushMatrix()
          call glRotated(esfera2D(1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
          call glRotated(esfera2D(2), cos(PI*esfera2d(1)/180.0_gldouble), &
               -sin(PI*esfera2D(1)/180.0_gldouble), 0.0_gldouble)

          If (mxy) call create_mirror_list('z') 
          If (myz) call create_mirror_list('x') 
          If (mzx) call create_mirror_list('y')
       
          If (a_x) Call draw_axis_list('x')
          If (a_y) Call draw_axis_list('y')
          If (a_z) Call draw_axis_list('z')
          call glPopMatrix()
       else
          If (mxy) Call glCallList(mirrorXY_list)
          If (myz) Call glCallList(mirrorYZ_list)
          If (mzx) Call glCallList(mirrorZX_list)
       
          If (a_x) Call glCallList(axisX_list)
          If (a_y) Call glCallList(axisY_list)
          If (a_z) Call glCallList(axisZ_list)
       endif

       Call glPopMatrix()
       
       Call glPushMatrix()
       If (real_size>maxsize) Then
          scale=maxsize/real_size
          Call glScalef(scale,scale,scale)
       Endif
       
       Call glRotatef(spin,vec(1),vec(2),vec(3))
       Call glCallList(ghosts_list)
       Call glFlush()
       Call glutSwapBuffers()
       If (real_size>maxsize) Then
          scale=maxsize/real_size
          Call glScalef(1/scale,1/scale,1/scale)
       Endif
       
       Call glPopMatrix()
    End Do
    
    spin=steps*factor
    Call glNewList(ghosts_list_stay,GL_COMPILE)
    Call glPushMatrix()
    If (real_size>maxsize) Then
       scale=maxsize/real_size
       Call glScalef(scale,scale,scale)
    Endif
    
    Call glRotatef(spin,vec(1),vec(2),vec(3))
    Call glCallList(ghosts_list)
    
    If (real_size>maxsize) Then
       scale=maxsize/real_size
       Call glScalef(1/scale,1/scale,1/scale)
    Endif
    
    Call glPopMatrix()
    Call glEndList()
    
    ghosts=.True.
  End Subroutine rotate_around_axis

  Subroutine invert_around_center()
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Real(kind=glfloat),Parameter :: zero=0.0_glfloat, one=1.0_glfloat
    Real(kind=glfloat), Dimension (:,:),Allocatable :: vel
    Integer :: kk,steps=12,natoms,jj, istat
    natoms=Ubound(molecule,1)
    Allocate(vel(1:natoms,1:3),stat=istat)
    If (istat/=0) Stop 'Failed to allocate vel'
    steps=steps/slow
    Do kk=1,natoms
       Do jj=1,3
          vel(kk,jj)=-2*molecule(kk)%coords(jj)/steps
       End Do
    End Do
    Do kk=0,steps
       Call glClear(Ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
       Call glPushMatrix()
       
       Call glCallList(axis_list)
!       If (moving_a) Call glCallList(moving_axis_list)
       If (moving_a) then
!          call glPushMatrix()
!          call glRotated(esfera2D(1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
!          call glRotated(esfera2D(2), cos(PI*esfera2d(1)/180.0_gldouble), &
!               -sin(PI*esfera2D(1)/180.0_gldouble), 0.0_gldouble)
          Call glCallList(moving_axis_list)
!          call glPopMatrix()
       endif

       
       If (atoms) Call glCallList(atoms_list)
       If (bonds) Call glCallList(bonds_list)
       
       if (moving_a) then
          call glPushMatrix()
          call glRotated(esfera2D(1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
          call glRotated(esfera2D(2), cos(PI*esfera2d(1)/180.0_gldouble), &
               -sin(PI*esfera2D(1)/180.0_gldouble), 0.0_gldouble)

          If (mxy) call  create_mirror_list('z') 
          If (myz) call  create_mirror_list('x') 
          If (mzx) call  create_mirror_list('y')
       
          If (a_x) Call draw_axis_list('x')
          If (a_y) Call draw_axis_list('y')
          If (a_z) Call draw_axis_list('z')
          call glPopMatrix()
       else
          If (mxy) Call glCallList(mirrorXY_list)
          If (myz) Call glCallList(mirrorYZ_list)
          If (mzx) Call glCallList(mirrorZX_list)
          
          If (a_x) Call glCallList(axisX_list)
          If (a_y) Call glCallList(axisY_list)
          If (a_z) Call glCallList(axisZ_list)
          
       endif       
       Call glPopMatrix()
       
       Call glPushMatrix()
       If (real_size>maxsize) Then
          scale=maxsize/real_size
          Call glScalef(scale,scale,scale)
       Endif
       Do jj=1,natoms
          molecule_OP(jj)%coords(1)=molecule(jj)%coords(1)+vel(jj,1)*kk
          molecule_OP(jj)%coords(2)=molecule(jj)%coords(2)+vel(jj,2)*kk
          molecule_OP(jj)%coords(3)=molecule(jj)%coords(3)+vel(jj,3)*kk
       Enddo
       Call draw_atoms(0.5_glfloat)
       Call glFlush()
       Call glutSwapBuffers()
       If (real_size>maxsize) Then
          scale=maxsize/real_size
          Call glScalef(1/scale,1/scale,1/scale)
       Endif
       
       Call glPopMatrix()
    End Do
    
    Call glNewList(ghosts_list_stay,GL_COMPILE)
    Call glPushMatrix()
    If (real_size>maxsize) Then
       scale=maxsize/real_size
       Call glScalef(scale,scale,scale)
    Endif
    
    Call glCallList(ghosts_list_inverted)
    
    If (real_size>maxsize) Then
       scale=maxsize/real_size
       Call glScalef(1/scale,1/scale,1/scale)
    Endif
    
    Call glPopMatrix()
    Call glEndList()
    
    ghosts=.True.
    Deallocate(vel)
  End Subroutine invert_around_center

  Subroutine reflect_on_plane(plane)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Real(kind=glfloat),Parameter :: zero=0.0_glfloat, one=1.0_glfloat
    Real(kind=glfloat), Dimension (:,:),Allocatable :: vel
    Integer :: kk,steps=12,natoms,jj, istat
    Character(1)::plane
    steps=steps/slow
    natoms=Ubound(molecule,1)
    Allocate(vel(1:natoms,1:3),stat=istat)
    If (istat/=0) Stop 'Failed to allocate vel'
    vel=zero
    Do kk=1,natoms
       Select Case (plane)
       Case ('x') ! Reflection on yz plane
          vel(kk,1)=-2*molecule(kk)%coords(1)/steps
          
       Case ('y') ! Reflection on xz plane
          vel(kk,2)=-2*molecule(kk)%coords(2)/steps
          
       Case ('z') ! Reflection on xy plane
          vel(kk,3)=-2*molecule(kk)%coords(3)/steps
       End Select
    End Do
    Do kk=0,steps
       Call glClear(Ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
       Call glPushMatrix()
       
       Call glCallList(axis_list)
       If (moving_a) then
!          call glPushMatrix()
!          call glRotated(esfera2D(1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
!          call glRotated(esfera2D(2), cos(PI*esfera2d(1)/180.0_gldouble), &
!               -sin(PI*esfera2D(1)/180.0_gldouble), 0.0_gldouble)
          Call glCallList(moving_axis_list)
!          call glPopMatrix
       endif

!       If (moving_a) Call glCallList(moving_axis_list)
       
       If (atoms) Call glCallList(atoms_list)
       If (bonds) Call glCallList(bonds_list)
       
       if (moving_a) then
          call glPushMatrix()
          call glRotated(esfera2D(1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
          call glRotated(esfera2D(2), cos(PI*esfera2d(1)/180.0_gldouble), &
               -sin(PI*esfera2D(1)/180.0_gldouble), 0.0_gldouble)

          If (mxy) call  create_mirror_list('z') 
          If (myz) call  create_mirror_list('x') 
          If (mzx) call  create_mirror_list('y')
       
          If (a_x) Call draw_axis_list('x')
          If (a_y) Call draw_axis_list('y')
          If (a_z) Call draw_axis_list('z')
          call glPopMatrix()
       else
          If (mxy) Call glCallList(mirrorXY_list)
          If (myz) Call glCallList(mirrorYZ_list)
          If (mzx) Call glCallList(mirrorZX_list)
       
          If (a_x) Call glCallList(axisX_list)
          If (a_y) Call glCallList(axisY_list)
          If (a_z) Call glCallList(axisZ_list)
       endif

       Call glPopMatrix()
       
       Call glPushMatrix()
       If (real_size>maxsize) Then
          scale=maxsize/real_size
          Call glScalef(scale,scale,scale)
       Endif
       Do jj=1,natoms
          molecule_OP(jj)%coords(1)=molecule(jj)%coords(1)+vel(jj,1)*kk
          molecule_OP(jj)%coords(2)=molecule(jj)%coords(2)+vel(jj,2)*kk
          molecule_OP(jj)%coords(3)=molecule(jj)%coords(3)+vel(jj,3)*kk
       Enddo
       Call draw_atoms(0.5_glfloat)
       Call glFlush()
       Call glutSwapBuffers()
       If (real_size>maxsize) Then
          scale=maxsize/real_size
          Call glScalef(1/scale,1/scale,1/scale)
       Endif
       
       Call glPopMatrix()
    End Do
    
    Call glNewList(ghosts_list_reflection,GL_COMPILE)
    Call glPushMatrix()
    If (real_size>maxsize) Then
       scale=maxsize/real_size
       Call glScalef(scale,scale,scale)
    Endif
    
       Call draw_atoms(0.5_glfloat)
    
    If (real_size>maxsize) Then
       scale=maxsize/real_size
       Call glScalef(1/scale,1/scale,1/scale)
    Endif
    
    Call glPopMatrix()
    Call glEndList()
    
    ghosts=.False.
    reflection=.True.
    Deallocate(vel)
  End Subroutine reflect_on_plane

  Subroutine improper_rotation(axis,angle)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Real(kind=glfloat),Parameter :: zero=0.0_glfloat, one=1.0_glfloat
    Real(kind=glfloat), Dimension (:,:),Allocatable :: vel
    Integer :: kk,steps=12,natoms,jj, istat
    Character(1),Intent(in)::axis
    character(1)::foo
    Real,Intent(in)::angle
    Real(kind=glfloat), Dimension (3) :: vec
    Real::c,s,ang_rad
    Real(kind=glfloat),Dimension(3)::coords
    Real(kind=glfloat) :: factor
    steps=steps/slow
    natoms=Ubound(molecule,1)
    Allocate(vel(1:natoms,1:3),stat=istat)
    If (istat/=0) Stop 'Failed to allocate vel'

! Rotation around axis
!    ang_rad=angle*3.14159265358979323846/180 ! modificar UGLY ...
    ang_rad=angle*acos(-1.0)/180
    c=Cos(ang_rad)
    s=Sin(ang_rad)
    Do kk=1,natoms
       coords(1)=molecule(kk)%coords(1)
       coords(2)=molecule(kk)%coords(2)
       coords(3)=molecule(kk)%coords(3)
       Select Case (axis)
       Case ('x')
          molecule(kk)%coords(2)=coords(2)*c+coords(3)*s
          molecule(kk)%coords(3)=coords(3)*c-coords(2)*s
       Case ('y')
          molecule(kk)%coords(3)=coords(3)*c+coords(1)*s
          molecule(kk)%coords(1)=coords(1)*c-coords(3)*s
       Case ('z')
          molecule(kk)%coords(1)=coords(1)*c+coords(2)*s
          molecule(kk)%coords(2)=coords(2)*c-coords(1)*s
       End Select
    Enddo

    Call rotate_around_axis(axis,angle)
    ghosts=.False.  
! fake pause
! blergh (UGLY and INSECURE!) dont install as r00t xD
! i really cant be bothered about this :/
    call system("sleep 0.75")
!    write(*,*)'Press key for reflection on mirror plane'
!    read(*,'(a)')foo
! Reflection on mirror plane perpendicular to the rotation axis
    vel=zero
    Do kk=1,natoms
       Select Case (axis)
       Case ('x') ! Reflection on yz plane
          myz=.True.
          vel(kk,1)=-2*molecule(kk)%coords(1)/steps
       Case ('y') ! Reflection on xz plane
          mzx=.True.
          vel(kk,2)=-2*molecule(kk)%coords(2)/steps
       Case ('z') ! Reflection on xy plane
          mxy=.True.
          vel(kk,3)=-2*molecule(kk)%coords(3)/steps
       End Select
    End Do
    Do kk=0,steps
       Call glClear(Ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
       Call glPushMatrix()
       
       Call glCallList(axis_list)
       If (moving_a) then
!          Call glPushMatrix()
!          call glRotated(esfera2D(1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
!          call glRotated(esfera2D(2), cos(PI*esfera2d(1)/180.0_gldouble), &
!               -sin(PI*esfera2D(1)/180.0_gldouble), 0.0_gldouble)
          Call glCallList(moving_axis_list)
!          Call glPopMatrix()
       endif

!       If (moving_a) Call glCallList(moving_axis_list)
       
       If (atoms) Call glCallList(atoms_list)
       If (bonds) Call glCallList(bonds_list)
       
       if (moving_a) then
          call glPushMatrix()
          call glRotated(esfera2D(1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
          call glRotated(esfera2D(2), cos(PI*esfera2d(1)/180.0_gldouble), &
               -sin(PI*esfera2D(1)/180.0_gldouble), 0.0_gldouble)

          If (mxy) call  create_mirror_list('z') 
          If (myz) call  create_mirror_list('x') 
          If (mzx) call  create_mirror_list('y')
       
          If (a_x) Call draw_axis_list('x')
          If (a_y) Call draw_axis_list('y')
          If (a_z) Call draw_axis_list('z')
          call glPopMatrix()
       else
          If (mxy) Call glCallList(mirrorXY_list)
          If (myz) Call glCallList(mirrorYZ_list)
          If (mzx) Call glCallList(mirrorZX_list)
       
          If (a_x) Call glCallList(axisX_list)
          If (a_y) Call glCallList(axisY_list)
          If (a_z) Call glCallList(axisZ_list)
       endif

       Call glPopMatrix()
       
       Call glPushMatrix()
       If (real_size>maxsize) Then
          scale=maxsize/real_size
          Call glScalef(scale,scale,scale)
       Endif
       Do jj=1,natoms
          molecule_OP(jj)%coords(1)=molecule(jj)%coords(1)+vel(jj,1)*kk
          molecule_OP(jj)%coords(2)=molecule(jj)%coords(2)+vel(jj,2)*kk
          molecule_OP(jj)%coords(3)=molecule(jj)%coords(3)+vel(jj,3)*kk
       Enddo
       Call draw_atoms(0.5_glfloat)
       Call glFlush()
       Call glutSwapBuffers()
       If (real_size>maxsize) Then
          scale=maxsize/real_size
          Call glScalef(1/scale,1/scale,1/scale)
       Endif
       
       Call glPopMatrix()
    End Do
    
    Call glNewList(ghosts_list_stay,GL_COMPILE)
    Call glPushMatrix()
    If (real_size>maxsize) Then
       scale=maxsize/real_size
       Call glScalef(scale,scale,scale)
    Endif
    
       Call draw_atoms(0.5_glfloat)
    
    If (real_size>maxsize) Then
       scale=maxsize/real_size
       Call glScalef(1/scale,1/scale,1/scale)
    Endif
    
    Call glPopMatrix()
    Call glEndList()
    
    ghosts=.True.
    Deallocate(vel)
  End Subroutine improper_rotation

  Subroutine draw_atoms(alpha)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Real(kind=glfloat),Optional,Intent(in) ::alpha
    Real(kind=glfloat) :: real_alpha, radius
    Real(kind=glfloat), Dimension(3) :: glcoords
    Integer(kind=glint) :: slices, stacks
    Integer :: natoms,i,istat
         
    If (.Not.Present(alpha)) Then 
       real_alpha=one
    Else
       real_alpha=alpha
    Endif
    natoms=Ubound(molecule_op,1)
!    Allocate(molecule_OP(1:natoms),stat=istat)
!    If (istat/=0) Stop 'Failed to allocate molecule_OP'
    Do i=1,natoms
       Call glPushMatrix()
       
       Call glMaterialfv(GL_FRONT, GL_AMBIENT, atom_ambient)
       Call glMaterialfv(GL_FRONT, GL_DIFFUSE, atom_diffuse)
       Call glMaterialfv(GL_FRONT, GL_SPECULAR, atom_specular)
       Call glMaterialf(GL_FRONT, GL_SHININESS, 128.0_glfloat)

!       gl_coords(1)=molecule_OP(i)%coords(1)
!       gl_coords(2)=molecule_OP(i)%coords(2)
!       gl_coords(3)=molecule_OP(i)%coords(3)
       
!       Call glTranslatef(gl_coords(1),gl_coords(2),gl_coords(3))
       Call glTranslatef(molecule_OP(i)%coords(1),  molecule_OP(i)%coords(2),&
            molecule_OP(i)%coords(3))

       radius=v_radius(z(i))*2.5
       
       Call glEnable(GL_COLOR_MATERIAL)
       Call glColor4f(iupac_colors(z(i),1),iupac_colors(z(i),2),iupac_colors(z(i),3)&
            ,real_alpha)
       slices=30_glint
       stacks=30_glint
!       Call glutSolidSphere(0.3_glfloat,20_glint,20_glint)
       Call glutSolidSphere(gl_radius(i),slices,stacks)
       Call glPopMatrix()
    Enddo
!    deallocate(molecule_op)
  End Subroutine draw_atoms

End Module glSyMM_mod


!--------------------------------------------------------------------------

! This is a simple program to demonstrate the use of the view_modifier module
! It consists of a module with the callback functions and a main program.

Module view_demo_callbacks
Use opengl_gl
Use opengl_glut
Use view_modifier
Use glSyMM_mod
Private
Public :: display

Contains

Subroutine display()
Implicit None
! This gets called when the display needs to be redrawn

Call reset_view()
!call glClearDepth(0.5_glclampd)
Call glClear(Ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

Call glCallList(axis_list)
If (moving_a) then
!   Call glPushMatrix()
!   call glRotated(esfera2D(1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
!   call glRotated(esfera2D(2), cos(PI*esfera2d(1)/180.0_gldouble), &
!               -sin(PI*esfera2D(1)/180.0_gldouble), 0.0_gldouble)
   Call glCallList(moving_axis_list)
!   Call glPopMatrix()
endif

!If (moving_a) Call glCallList(moving_axis_list)

If (atoms) Call glCallList(atoms_list)
If (bonds) Call glCallList(bonds_list)

if (moving_a) then
   call glPushMatrix()
   call glRotated(esfera2D(1), 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
   call glRotated(esfera2D(2), cos(PI*esfera2d(1)/180.0_gldouble), &
        -sin(PI*esfera2D(1)/180.0_gldouble), 0.0_gldouble)

   If (mxy) call create_mirror_list('z') 
   If (myz) call create_mirror_list('x') 
   If (mzx) call create_mirror_list('y')
       
   If (a_x) Call draw_axis_list('x')
   If (a_y) Call draw_axis_list('y')
   If (a_z) Call draw_axis_list('z')
   call glPopMatrix()
else
   If (mxy) Call glCallList(mirrorXY_list)
   If (myz) Call glCallList(mirrorYZ_list)
   If (mzx) Call glCallList(mirrorZX_list)
   
   If (a_x) Call glCallList(axisX_list)
   If (a_y) Call glCallList(axisY_list)
   If (a_z) Call glCallList(axisZ_list)
endif


If (ghosts) Call glCallList(ghosts_list_stay)

If (reflection) Call glCallList (ghosts_list_reflection)

Call glFlush()
Call glutSwapBuffers()

Return
End Subroutine display

End Module view_demo_callbacks

Module windows_ops
  
Contains
  Subroutine MyReshape(width,height)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Implicit None
    Integer(kind=glcint) :: width, height
    Real(kind=gldouble) :: a
    ! if glcint is not the same as glsizei, width and height will
    ! need to be copied to variables of the later kind
    
    Call glViewport(0_glint, 0_glint, width, height)
    !    call glMatrixMode(GL_PROJECTION)
    !    call glLoadIdentity()
    !    if (width <= height) then
    !       a=height/width
    !       call gluOrtho2D(0.0_gldouble, 1.0_gldouble, 0.0_gldouble, 1.0_gldouble*a)
    !    else
    !       a=width/height
    !       call gluOrtho2D(0.0_gldouble, 1.0_gldouble*a, 0.0_gldouble, 1.0_gldouble)
    !    endif
    !    call glMatrixMode(GL_MODELVIEW)
    !    call glLoadIdentity()
    Return
  End Subroutine Myreshape
  
  Subroutine keyboard(ich, x, y)
    Use opengl_gl
!    Use opengl_glu
    Use opengl_glut
    Use glSyMM_mod
    Implicit None
    
    Integer :: ich,x,y
    Character :: ch

    Integer :: kk
    Real(kind=glfloat)::spin=0.0_glfloat
    
    ch = Achar(ich)
    Select Case(ch)
    Case (Achar(27))      ! escape
       Stop
    Case ('b')
       bonds = .Not. bonds
       Call glutPostRedisplay()
    Case ('a')
       atoms = .Not. atoms
       Call glutPostRedisplay()
!    Case ('B') ! ghost bonds
!       ghost_bonds = .Not. ghost_bonds
!       Call glutPostRedisplay()
    Case ('A')
       ghosts = .Not. ghosts
       Call glutPostRedisplay()
    Case ('S','s')
       Write(*,'(a)',advance='no')'Slowdown factor= '
       Read(*,*)slow
       Case('M','m')
          mxy=.False.
          myz=.False.
          mzx=.False.
          call glutPostRedisplay()
       Case('X','x')
          a_X=.False.
          a_Y=.False.
          a_Z=.False.
          call glutPostRedisplay()
       Case('L','l')
!          moving_a=.Not. moving_a
          call glutPostRedisplay()
    Case default 
       Call glutPostRedisplay()
    End Select
  End Subroutine keyboard
  
End Module windows_ops

Program glSyMM
  
Use opengl_gl
!Use opengl_glu
Use opengl_glut
Use glSyMM_mod
Use windows_ops
Use view_modifier
Use view_demo_callbacks
Use draw_tube
Use moving_axis
Use eigens_jacobi
Implicit None

! using SyMM as a backend (maybe through command line argument)
! to determine the point group of a molecule perform the found 
! opperations on the molecule
!Real(kind=glfloat),Parameter :: zero=0.0_glfloat, one=1.0_glfloat
Integer :: winid, menuid, submenuid

! define vectors for light and material of atoms and bonds
! lights at infinity along a tetrahedron vertices

Real(kind=glfloat), Dimension(4) :: &
     pos = (/ 0.0_glfloat, 100.0_glfloat, 100.0_glfloat, zero /), &
     pos1 = (/ 0.0_glfloat, -100.0_glfloat, 100.0_glfloat, zero /), &
     pos2 = (/ 100.0_glfloat, 0.0_glfloat, -100.0_glfloat, zero /), &
     pos3 = (/ -100.0_glfloat, 00.0_glfloat, -100.0_glfloat, zero /), &

     white = (/ one, one, one, zero /), &
     black = (/ zero, zero, zero, zero /), &
     grey  = (/ 0.5_glfloat, 0.5_glfloat, 0.5_glfloat, zero /), &
     green = (/ zero, one, zero, zero /), &
     red   = (/ one, zero, zero, zero /), &
     oldie = (/ 0.05, 0.05, 0.05, one /)

Integer :: ios, i, j, istat, ii
Integer, Parameter :: maxatoms=100
Character(len=80) :: title
Integer :: natoms
Real, Dimension (:,:), Allocatable:: coords, moving

Real(kind=glfloat),Dimension(4)::gl_colour
Real(kind=glfloat), Dimension(3)::gl_coords
Integer::faces
Real(kind=glfloat)::diameter,cap_size,dist,axis_size
Logical::wire,smooth

!Type(gluquadricobj), Pointer:: quad

Real(kind=gldouble) :: base, top, height
Integer(kind=glint) :: slices, stacks


! declarations for command line arguments
Integer(kind=glcint) :: num_arg
Character(len=256), Allocatable, Dimension(:) :: args
Integer, External :: iargc

num_arg = iargc()+1
If (num_arg<2) Then
   Write(*,*)'Usage: glSyMM coords_file.xyz' 
   Stop
End If
Allocate(args(num_arg))
args(1) = "glSyMM"
Do i=2,num_arg
   Call getarg(i-1,args(i))
Enddo 
!call glutinit(num_arg,args)

!call glutAddMenuEntry("Start motion", 6)

!Call glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER,GL_TRUE)
!call glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,GL_TRUE)
background = background_default
Call glClearColor(background(1),background(2),background(3),background(4))
!Call glClear(Ior(GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT) )

!Call glEnable(GL_CULL_FACE)
!Call glCullFace(GL_FRONT)

!Call glEnable(GL_BLEND)
!Call glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
!Call glShadeModel(GL_SMOOTH)

! Open the xyz (using atomic number for now) file containing the coordinates and
! read the number of atoms (natoms), title, and the coordinates

! Initializations
Call init_radius()
Call init_colors()
Call init_masses()
Call init_symbols()

!do i=1,nargs
!   select case (args(i))
!   case('-2')
!      double=.true.
!   case default
!      filename=args(i)
!   end select
!end do

Open (10,file=args(2),status='old',action='read',iostat=ios,err=9999)
Read(10,*) natoms
Write(*,*) natoms
If ((natoms<1).Or.(natoms>1000)) Then
   Write(*,*)'Bad number of atoms',natoms
   Stop
Else
   Allocate(z(1:natoms),stat=istat)
   If (istat/=0) Stop 'Failed to allocate z'
   Allocate(coords(1:natoms,1:3),stat=istat)
   If (istat/=0) Stop 'Failed to allocate coords'
   Allocate(moving(1:natoms,1:3),stat=istat)
   If (istat/=0) Stop 'Failed to allocate moving'
   Allocate(gl_radius(1:natoms),stat=istat)
   If (istat/=0) Stop 'Failed to allocate gl_radius'
   Allocate(molecule(1:natoms),stat=istat)
   If (istat/=0) Stop 'Failed to allocate molecule'
   Allocate(molecule_OP(1:natoms),stat=istat)
   If (istat/=0) Stop 'Failed to allocate molecule_OP'
   Allocate(molecule_CM(1:natoms),stat=istat)
   If (istat/=0) Stop 'Failed to allocate molecule_CM'
   Allocate(molecule_IA(1:natoms),stat=istat)
   If (istat/=0) Stop 'Failed to allocate molecule_IA'

Endif
Read(10,*)title
Write(*,*)title
write(*,*)'atomic number x y z'
Do i=1,natoms
   Read(10,*)z(i),coords(i,1),coords(i,2),coords(i,3)
!   coords(i,1)=coords(i,1)*2
!   coords(i,2)=coords(i,2)*2
!   coords(i,3)=coords(i,3)*2
   Write(*,*)z(i),coords(i,1),coords(i,2),coords(i,3)
   molecule(i)%z=z(i)
   molecule_CM(i)%z=z(i)
   molecule_OP(i)%z=z(i)
   molecule_IA(i)%z=z(i)
   molecule(i)%coords(1)=coords(i,1)
   molecule(i)%coords(2)=coords(i,2)
   molecule(i)%coords(3)=coords(i,3)
Enddo
center_of_mass=cm(molecule)
Write(*,*)'cM=',center_of_mass
write(*,*)
   write(*,*) "atomic number x_cm y_cm z_cm (translation to center-of-mass)"
Do i=1,natoms
   coords(i,1)=coords(i,1)-center_of_mass(1)
   coords(i,2)=coords(i,2)-center_of_mass(2)
   coords(i,3)=coords(i,3)-center_of_mass(3)

   molecule(i)%coords(1)=coords(i,1)
   molecule(i)%coords(2)=coords(i,2)
   molecule(i)%coords(3)=coords(i,3)

   molecule_CM(i)%coords(1)=coords(i,1)
   molecule_CM(i)%coords(2)=coords(i,2)
   molecule_CM(i)%coords(3)=coords(i,3)
   write(*,*) molecule_CM(i)%z,molecule_CM(i)%coords(1),molecule_CM(i)%coords(2),molecule_CM(i)%coords(3)
End Do
!write(*,*)'CM=',cm(molecule)
!Write(*,*)x1,y1,z1,x2,y2,z2

inertial_tensor=calc_inertia_tensor()
write(*,*)
write(*,*) "Inertial Tensor"
do i =1,3
   write(*,*)(inertial_tensor(i,j),j=1,3)
enddo
write(*,*)
!Calculation of eigenvalues and eigenvectors 
Call eigens(inertial_tensor, eigenvalues, eigenvectors)
! Rotation to principal axis of inertia
   write(*,*) "atomic number x_ai y_ai z_ai (rotation to principal axis of inertia)"
Do i=1, natoms
   coords(i,:)=Matmul(coords(i,:),eigenvectors)
   
   molecule(i)%coords(1)=coords(i,1)
   molecule(i)%coords(2)=coords(i,2)
   molecule(i)%coords(3)=coords(i,3)
   
   molecule_IA(i)%coords(1)=coords(i,1)
   molecule_IA(i)%coords(2)=coords(i,2)
   molecule_IA(i)%coords(3)=coords(i,3)
   write(*,*) molecule_IA(i)%z,molecule_IA(i)%coords(1),molecule_IA(i)%coords(2),molecule_IA(i)%coords(3)
   If (i==1) Then 
      x1=coords(i,1)
      x2=coords(i,1)
      
      y1=coords(i,2)
      y2=coords(i,2)
      
      z1=coords(i,3)
      z2=coords(i,3)
   Else
      x1=Max(coords(i,1),x1)
      y1=Max(coords(i,2),y1)
      z1=Max(coords(i,3),z1)
      
      x2=Min(coords(i,1),x2)
      y2=Min(coords(i,2),y2)
      z2=Min(coords(i,3),z2)
   Endif   
Enddo
   write(*,*)
Call glEnable(GL_NORMALIZE)

Call glutInit
Call glutInitDisplayMode(Ior(GLUT_DOUBLE,Ior(GLUT_RGB,GLUT_DEPTH)))
Call glutInitWindowSize(500_glcint,500_glcint) ! size of window
! Create a window

winid = glutCreateWindow("glSyMM ")!//title)
submenuid = view_modifier_init()
!submenuid = view_modifier_init()
!Call glutAddSubMenu("View Modilfier",menuid)
Call make_menu(submenuid)
!Call glutAttachMenu(GLUT_RIGHT_BUTTON)

! Set the display and keyboard callbacks

Call glutDisplayFunc(display)
Call glutKeyboardFunc(keyboard)

w=Abs(x2-x1)
h=Abs(y2-y1)
d=Abs(z2-z1)
real_size=Max(w,h,d)
!write(*,*)'Real Size=',real_size

mirror_size=real_size
If (real_size>5)  then 
   mirror_size=4
end If
! Create the images
Do ii=1,13
   scaled=.False.
   Select Case (ii)
   Case (1)
      Call glNewList(atoms_list,GL_COMPILE)
!      call glTranslatef(-center_of_mass(1),-center_of_mass(2),-center_of_mass(3))
      atoms=.True.
      bonds=.False.
      If (real_size>maxsize) Then
         scale=maxsize/real_size
         Call glScalef(scale,scale,scale)
         scaled=.True.
      Endif
   Case (2)
      Call glNewList(bonds_list,GL_COMPILE)
!      call glTranslatef(-center_of_mass(1),-center_of_mass(2),-center_of_mass(3))
      atoms=.False.
      bonds=.True.
      If (real_size>maxsize) Then
         scale=maxsize/real_size
         Call glScalef(scale,scale,scale)
         scaled=.True.
      Endif
   Case (3)
      Call glNewList(axis_list,GL_COMPILE)
!      call glTranslatef(-center_of_mass(1),-center_of_mass(2),-center_of_mass(3))
      atoms=.False.
      bonds=.False.
      If (real_size>maxsize) Then
         scale=maxsize/real_size
         Call glScalef(scale,scale,scale)
         scaled=.True.
      Endif
      Call create_axis_list()
   Case (4)
      Call glNewList(mirrorXY_list,GL_COMPILE)
      Call create_mirror_list('z')
      atoms=.False.
      bonds=.False.
   Case (5)
      Call glNewList(mirrorYZ_list, GL_COMPILE)
      Call create_mirror_list('x')
      atoms=.False.
      bonds=.False.
   Case (6)
      Call glNewList(mirrorZX_list, GL_COMPILE)
      Call create_mirror_list('y')
      atoms=.False.
      bonds=.False.
   Case (7)
      Call glNewList(axisX_list, GL_COMPILE)
      Call draw_axis_list('x')
      atoms=.False.
      bonds=.False.
   Case (8)
      Call glNewList(axisY_list, GL_COMPILE)
      Call draw_axis_list('y')
      atoms=.False.
      bonds=.False.
   Case (9)
      Call glNewList(axisZ_list, GL_COMPILE)
      Call draw_axis_list('z')
      atoms=.False.
      bonds=.False.
   Case (10)
      Call glNewList(ghosts_list,GL_COMPILE)
      atoms=.False.
      bonds=.False.
!      call glTranslatef(-center_of_mass(1),-center_of_mass(2),-center_of_mass(3))
      If (real_size>maxsize) Then
         scale=maxsize/real_size
         Call glScalef(scale,scale,scale)
         scaled=.True.
      Endif
!      Call glTranslatef(-x1+w/2,-y1+h/2,-z1+d/2)
   
!      Call glEnable(GL_NORMALIZE)

      If (scaled) Then 
         scale=maxsize/real_size
         Call glScalef(1/scale,1/scale,1/scale)
      Endif
      
      Call create_atoms_list(0.5_glfloat)
   Case (11)
      Call glNewList(ghosts_list_inverted,GL_COMPILE)
      atoms=.False.
      bonds=.False.
      If (real_size>maxsize) Then
         scale=maxsize/real_size
         Call glScalef(scale,scale,scale)
         scaled=.True.
      Endif
   
      Call glEnable(GL_NORMALIZE)

      If (scaled) Then 
         scale=maxsize/real_size
         Call glScalef(1/scale,1/scale,1/scale)
      Endif
      
      Call create_atoms_list(0.5_glfloat,.True.)
      
!      Call create_bonds_list(0.25_glfloat)
!      Call glTranslatef(x1-w/2,y1-h/2,z1-d/2)
!      call glTranslatef(center_of_mass(1),center_of_mass(2),center_of_mass(3))
   Case (12)
      write(*,*)'glGetError=',glGetError()
      Call glNewList(12,GL_COMPILE)
   Case (13)
      Call glNewList(moving_axis_list,GL_COMPILE)
!      call glTranslatef(-center_of_mass(1),-center_of_mass(2),-center_of_mass(3))
      atoms=.False.
      bonds=.False.
      If (real_size>maxsize) Then
         scale=maxsize/real_size
         Call glScalef(scale,scale,scale)
         scaled=.True.
      Endif
      Call glEnable(GL_NORMALIZE)
      Call create_maxis_list()
      If (scaled) Then 
         scale=maxsize/real_size
         Call glScalef(1/scale,1/scale,1/scale)
      Endif
   End Select

   ! Draw a point at the light Position
   !   Call glPushMatrix()
   !   Call glPointsize(5.0_glfloat)
   !   Call glBegin(GL_POINTS)
   !   Call glColor3f(zero,zero,zero)
   !   Call glVertex4fv(pos)
   !   Call glEnd()
   !   Call glPopMatrix()

   w=Abs(x2-x1)
   h=Abs(y2-y1)
   d=Abs(z2-z1)

!   Call glTranslatef(-x1+w/2,-y1+h/2,-z1+d/2)
!   call glTranslatef(-center_of_mass(1),-center_of_mass(2),-center_of_mass(3))
   
   If (atoms) Call create_atoms_list()
   If (bonds) Call create_bonds_list()
   
   Call glEnable(GL_NORMALIZE)
!   call glTranslatef(center_of_mass(1),center_of_mass(2),center_of_mass(3))
!   Call glTranslatef(x1-w/2,y1-h/2,z1-d/2)

!   If (real_size>maxsize)Then
   If (scaled) Then 
      scale=maxsize/real_size
      Call glScalef(1/scale,1/scale,1/scale)
!      write(*,*)scale
   Endif
   Call glEndList()
!   write(*,*)ii,glGetError()
Enddo

atoms=.True.
bonds=.True.

! Set the lighting

Call glClearColor(background(1),background(2),background(3),background(4))

Call glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse)
Call glLightfv(GL_LIGHT0, GL_SPECULAR, light_specular)
Call glLightfv(GL_LIGHT0, GL_AMBIENT, light_ambient)
Call glLightfv(GL_LIGHT0, GL_POSITION, pos)

Call glLightfv(GL_LIGHT1, GL_DIFFUSE, light_diffuse)
Call glLightfv(GL_LIGHT1, GL_SPECULAR, light_specular)
Call glLightfv(GL_LIGHT1, GL_AMBIENT, light_ambient)
Call glLightfv(GL_LIGHT1, GL_POSITION, pos1)

Call glLightfv(GL_LIGHT2, GL_DIFFUSE, light_diffuse)
Call glLightfv(GL_LIGHT2, GL_SPECULAR, light_specular)
Call glLightfv(GL_LIGHT2, GL_AMBIENT, light_ambient)
Call glLightfv(GL_LIGHT2, GL_POSITION, pos2)

Call glLightfv(GL_LIGHT3, GL_DIFFUSE, light_diffuse)
Call glLightfv(GL_LIGHT3, GL_SPECULAR, light_specular)
Call glLightfv(GL_LIGHT3, GL_AMBIENT, light_ambient)
Call glLightfv(GL_LIGHT3, GL_POSITION, pos3)

Call glLightModelfv(GL_LIGHT_MODEL_AMBIENT,grey-0.48)
Call glEnable(GL_LIGHTING)
Call glEnable(GL_LIGHT0)
!Call glEnable(GL_LIGHT1)
Call glEnable(GL_LIGHT2)
!Call glEnable(GL_LIGHT3)
Call glEnable(GL_DEPTH_TEST)
Call glDepthFunc(GL_LEQUAL)
!Call glutIdleFunc(rotate_around_axis)
! Let glut take over
Call glutMainLoop()

Stop ! this is the end ...

9999 Write(*,*)'Error opening file '//args(2)

Contains 
  Subroutine create_axis_list()
    Implicit None
    
    !Draw axes so we know the orientation
    ! antialias axis ...
    Call glEnable(GL_LINE_SMOOTH)
    Call glEnable(GL_BLEND)

    Call glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
    Call glHint(GL_LINE_SMOOTH_HINT, GL_DONT_CARE)
    ! thicker axis lines 
    Call glLineWidth(4.0_glfloat)
    
    Call glShadeModel(GL_FLAT)
!    Call glClearColor(background(1),background(2),background(3),background(4))
    Call glDepthFunc(GL_LEQUAL)
    Call glEnable(GL_DEPTH_TEST)
    
    Call glBegin(GL_LINES)
!    Call glColor4f(255.0_glfloat/255,65.0_glfloat/255,zero/255,one)
 	Call glColor4f(.0_glfloat/255, 180.0_glfloat/255, zero/255 ,one)   
    ! glow in the dark axis !!!
!    call glMaterialfv(GL_FRONT, GL_EMISSION, oldie)
    
    w=Max(Abs(x2-x1),1.0)
    h=Max(Abs(y2-y1),1.0)
    d=Max(Abs(z2-z1),1.0)
    
    inc=0.2
    w=w/2+3*inc
    h=h/2+3*inc
    d=d/2+3*inc
    ! x axis
    Call glVertex3f(zero,zero,zero)
    Call glVertex3f(w,zero,zero)
    ! y axis
    Call glVertex3f(zero,zero,zero)
    Call glVertex3f(zero,h,zero)
    ! z axis
    Call glVertex3f(zero,zero,zero)
    Call glVertex3f(zero,zero,d)
    
    ! Draw crude x, y and z to label the axes
    w=w+inc
    Call glVertex3f(w,-0.1_glfloat,0.1_glfloat) ! X
    Call glVertex3f(w,0.1_glfloat,-0.1_glfloat)
    Call glVertex3f(w,-0.1_glfloat,-0.1_glfloat)
    Call glVertex3f(w,0.1_glfloat,0.1_glfloat)
    h=h+inc
    Call glVertex3f(0.1_glfloat,h,0.1_glfloat) ! Y
    Call glVertex3f(zero,h,zero)
    Call glVertex3f(-0.1_glfloat,h,0.1_glfloat)
    Call glVertex3f(0.1_glfloat,h,-0.1_glfloat)
    d=d+inc
    Call glVertex3f(-0.1_glfloat,0.1_glfloat,d) ! Z
    Call glVertex3f(0.1_glfloat,0.1_glfloat,d)
    Call glVertex3f(0.1_glfloat,0.1_glfloat,d)
    Call glVertex3f(-0.1_glfloat,-0.1_glfloat,d)
    Call glVertex3f(-0.1_glfloat,-0.1_glfloat,d)
    Call glVertex3f(0.1_glfloat,-0.1_glfloat,d)
    Call glEnd()
!    call glpopmatrix()
    Call glShadeModel(GL_SMOOTH)
  End Subroutine create_axis_list

  Subroutine create_maxis_list()

    use moving_axis
    Implicit None
    real(kind=gldouble), parameter :: PI = 3.141592653589793_gldouble
    !Draw moving axes 
    ! antialias axis ...
    Call glEnable(GL_LINE_SMOOTH)
    Call glEnable(GL_BLEND)

    Call glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
    Call glHint(GL_LINE_SMOOTH_HINT, GL_DONT_CARE)
    ! Thicker axis lines 
    Call glLineWidth(5.0_glfloat)
    
    Call glShadeModel(GL_FLAT)
!    Call glClearColor(background(1),background(2),background(3),background(4))
    Call glDepthFunc(GL_LEQUAL)
    Call glEnable(GL_DEPTH_TEST)
    
    Call glPushMatrix()
    call glMatrixMode(GL_MODELVIEW)
    Call glBegin(GL_LINES)
    Call glColor4f(127.0_glfloat/255, 255.0_glfloat/255, zero/255 ,one)
    ! glow in the dark axis !!!
!    call glMaterialfv(GL_FRONT, GL_EMISSION, oldie)
    
    w=Max(Abs(x2-x1),1.0)*1.05
    h=Max(Abs(y2-y1),1.0)*1.05
    d=Max(Abs(z2-z1),1.0)*1.05
    
    inc=0.2
    w=w/2+3*inc
    h=h/2+3*inc
    d=d/2+3*inc
    ! x axis
    Call glVertex3f(zero,zero,zero)
    call glVertex3f(w*ex(1),w*ex(2),w*ex(3))
!    Call glVertex3f(w*(2*c1*s1**2*c2-2*c1*s1**2+c1), w*((2*s1**3-2*s1)*c2-2*s1**3+s1), w*(2*c1*s1*s2))
!    Call glVertex3f(w,zero,zero)
    ! y axis
    Call glVertex3f(zero,zero,zero)
    call glVertex3f(h*ey(2),h*ey(2),h*ey(3))
!    Call glVertex3f(h*((1-2*c1**2)*s1*c2+2*c1**2*s1), h*((2*c1**3-c1)*c2-2*c1**3+2*c1), h*((1-2*c1**2)*s2))
!    Call glVertex3f(zero,h,zero)
    ! z axis
    Call glVertex3f(zero,zero,zero)
    call glVertex3f(d*ez(1),d*ez(2),d*ez(3))
!    Call glVertex3f(d*((1-2*c1**2)*s1*c2+2*c1**2*s1), d*((2*c1**3-c1)*c2-2*c1**3+2*c1), d*((1-2*c1**2)*s2))
!    Call glVertex3f(zero,zero,d)
    
    ! Draw crude x, y and z to label the axes
!    w=w+inc
!    Call glVertex3f(w,-0.1_glfloat,0.1_glfloat) ! X
!    Call glVertex3f(w,0.1_glfloat,-0.1_glfloat)
!    Call glVertex3f(w,-0.1_glfloat,-0.1_glfloat)
!    Call glVertex3f(w,0.1_glfloat,0.1_glfloat)
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
    Call glEnd()
    Call glPopMatrix()
    Call glShadeModel(GL_SMOOTH)
  End Subroutine create_maxis_list
  
  Subroutine create_atoms_list(alpha,inverted)
    Implicit None
    Real(kind=glfloat),Optional,Intent(in) ::alpha
    Logical, Optional, Intent (in) :: inverted
    Real(kind=glfloat) :: real_alpha
    Logical :: inv

    If (.Not.Present(alpha)) Then 
       real_alpha=one
    Else
       real_alpha=alpha
    Endif

    If (.Not.Present(inverted)) Then 
       inv=.False.
    Else
       inv=.True.
    Endif
    
    Do i=1,natoms
       Call glPushMatrix()
       
       Call glMaterialfv(GL_FRONT, GL_AMBIENT, atom_ambient)
       Call glMaterialfv(GL_FRONT, GL_DIFFUSE, atom_diffuse)
       Call glMaterialfv(GL_FRONT, GL_SPECULAR, atom_specular)
       Call glMaterialf(GL_FRONT, GL_SHININESS, 128.0_glfloat)
       
       If (inv) Then
          gl_coords(1)=-coords(i,1)
          gl_coords(2)=-coords(i,2)
          gl_coords(3)=-coords(i,3)
       Else       
          gl_coords(1)=coords(i,1)
          gl_coords(2)=coords(i,2)
          gl_coords(3)=coords(i,3)
       Endif
       
       Call glTranslatef(gl_coords(1),gl_coords(2),gl_coords(3))

       gl_radius(i)=v_radius(z(i))*2.5
       
       Call glEnable(GL_COLOR_MATERIAL)
       Call glColor4f(iupac_colors(z(i),1),iupac_colors(z(i),2),iupac_colors(z(i),3)&
            ,real_alpha)
       slices=30_glint
       stacks=30_glint
       Call glutSolidSphere(gl_radius(i),slices,stacks)
       Call glPopMatrix()
    Enddo
  End Subroutine create_atoms_list

  Subroutine create_bonds_list(alpha)
    Implicit None
    Real(kind=glfloat),Optional,Intent(in) ::alpha
    Real(kind=glfloat) :: real_alpha
    real :: check
    If (.Not.Present(alpha)) Then 
       real_alpha=one
    Else
       real_alpha=alpha
    Endif
    smooth=.True.
    wire=.False.
    cap_size=0.03
    diameter=0.075
    faces=12
    
    Do i=1,natoms
       Do j=i+1,natoms
          dist=Sqrt((coords(i,1)-coords(j,1))**2+(coords(i,2)-coords(j,2))**2&
               +(coords(i,3)-coords(j,3))**2)
          ! replace the following test with a more suitable one
          ! if dist<(vdW(1)+vdW(2))/2 then 
          ! or something similar ...
!!!!          If (dist<1.75) Then
          check=(vdW_radius(z(i))+vdW_radius(z(j)))*1.88972599
!          write(*,*)dist,check
          If (dist<check.and.z(i)<20.and.z(j)<20) Then
             ! bonds with low shininess
             Call glPushMatrix()
             Call glDepthFunc(GL_LESS)
!             Call glEnable(GL_BLEND)
             !               call glDepthMask(.false.)
             !Call glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
             !Call glEnable(GL_NORMALIZE)
             !Call glShadeModel(GL_SMOOTH)
             
             Call glMaterialfv(GL_FRONT, GL_AMBIENT, bond_ambient)
             Call glMaterialfv(GL_FRONT, GL_DIFFUSE, bond_diffuse)
             Call glMaterialfv(GL_FRONT, GL_SPECULAR, bond_specular)
             Call glMaterialf(GL_FRONT, GL_SHININESS, 40.0_glfloat)
             Call glEnable(GL_COLOR_MATERIAL)
             Call glColor4f(238.0_glfloat/255,232.0_glfloat/255,170.0_glfloat/255&
                  ,real_alpha)
             !         write(*,*)i,'---->',j
             Call tube(coords(i,1),coords(i,2),coords(i,3),&
                  coords(j,1),coords(j,2),coords(j,3),&
                  diameter,cap_size,faces,smooth,wire)
             Call glPopMatrix()
          Endif
       Enddo
    Enddo
  End Subroutine create_bonds_list


  Function calc_inertia_tensor()
    Implicit None
    Real,Dimension(3,3) :: calc_inertia_tensor
    Integer::i,k,j
    Real::mass
    Real,Dimension(3)::coord
    calc_inertia_tensor=0.0
    Do i=1,3
       Do j=1,i-1
          Do k=1,natoms

             mass=atomic_data(molecule_CM(k)%z)%mass 
             coord(1)=molecule_CM(k)%coords(1)
             coord(2)=molecule_CM(k)%coords(2)
             coord(3)=molecule_CM(k)%coords(3)
             
             calc_inertia_tensor(i,j)=calc_inertia_tensor(i,j)-mass*coord(i)*coord(j)
          Enddo
          calc_inertia_tensor(j,i)=calc_inertia_tensor(i,j)
       Enddo
    Enddo
    Do k=1,natoms

       mass=atomic_data(molecule_CM(k)%z)%mass ! mass here not atomic number ...
       coord(1)=molecule_CM(k)%coords(1)
       coord(2)=molecule_CM(k)%coords(2)
       coord(3)=molecule_CM(k)%coords(3)

       calc_inertia_tensor(1,1)=calc_inertia_tensor(1,1)+mass*(coord(2)**2+coord(3)**2)
       calc_inertia_tensor(2,2)=calc_inertia_tensor(2,2)+mass*(coord(3)**2+coord(1)**2)
       calc_inertia_tensor(3,3)=calc_inertia_tensor(3,3)+mass*(coord(1)**2+coord(2)**2)
    Enddo
    
  End Function calc_inertia_tensor
  

End Program glSyMM
