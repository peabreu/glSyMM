Module colors_radius
Use opengl_gl
Use glSyMM_mod, Only : atomic_data
Implicit None
Public :: iupac_colors, vdW_radius, v_radius, init_radius, init_colors
Private
Real(kind=glfloat),Dimension(92,3),Save :: iupac_colors
Real(kind=glfloat),Dimension(92),Save :: v_radius
Real(kind=glfloat),Dimension(92),Save :: vdW_radius
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

Contains
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

End Module colors_radius
