HGamer3D
========

 A tool to use Haskell for game programming. See http://www.hgamer3D.org .

 News, August 2015

 After a lot of frustration with the old setup (too complex, too error phrone, difficult to compile on linux, headaches with import of graphics, headaches with different shading approaches on different architectures, too many dependencies, complex build chain ...) I finally decided, to build a new version on a different approach.

 The result is now on master, version 0.6. This version is different! It is based on a different C++ library (Urho3D) and I introduced a new build system (CMake). Also the binding strategy is different and this also reduced a lot of the complexity of the former versions. The backdraw is - not all functionality is ready now, so for this version only basic 3D graphics and some mouse input work, but having accomplished this step, adding functions is much easier now, so hopefully as time progresses new functions will emerge.

 A remark to version numbers, all versions now, starting with a major "0" may have API breakage, when the minor version number change. This will change, starting with version 1.x.x.

 regards
 uotbw

