Summary:  This project aims to develop teaching aids for group theory in chemistry. It consists of two parts: SyMM a backend which determines the point group of a molecule and glSyMM which allows one to display the symmetry opperations on a molecule using OpenGL.
Name: glSyMM
Version: 0.99.0
Release: 1
License: GPL
Group: Applications/Science
URL: http://glsymm.sourceforge.net
Source0: %{name}-%{version}.tar.bz2
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: intel-ifort8

%description
glSyMM pretends to be a point symmetry display system. Its main purpose is to
identify (using as back-end SyMM) the point group of a molecule and to perform
the symmetry operations on the molecule.

    - Inversion on the symmetry center: atoms move from a position the
    "inverted" position (x,y,z) --> (-x,-y,-z)

    - Rotation around one axis; the axis is drawn and the rotation occurs.

    - Reflection on a mirror plane.

    - Improper rotation: a combination of the above two operations

%prep

%setup -q

%build
make
%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/local/bin/
install -m 755 glSyMM $RPM_BUILD_ROOT/usr/local/bin/glSyMM
%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
/usr/local/bin/glSyMM
%doc GNU_GPL examples glSyMM.txt ch2 sh2 h2o Oh *.xyz

%changelog
* Wed Jan 26 2005 Paulo E. Abreu <qtabreu@ci.uc.pt> - 
- Initial build.

