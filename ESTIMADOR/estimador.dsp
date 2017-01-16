# Microsoft Developer Studio Project File - Name="estimador" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=estimador - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "estimador.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "estimador.mak" CFG="estimador - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "estimador - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "estimador - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "estimador - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "estimador - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "estimador - Win32 Release"
# Name "estimador - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Group "trazador"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\trazador\dasum.f
# End Source File
# Begin Source File

SOURCE=.\trazador\daxpy.f
# End Source File
# Begin Source File

SOURCE=.\trazador\ddot.f
# End Source File
# Begin Source File

SOURCE=.\trazador\dgefa.f
# End Source File
# Begin Source File

SOURCE=.\trazador\dgesl.f
# End Source File
# Begin Source File

SOURCE=.\trazador\dscal.f
# End Source File
# Begin Source File

SOURCE=.\trazador\idamax.f
# End Source File
# Begin Source File

SOURCE=.\trazador\incoef3.f
# End Source File
# Begin Source File

SOURCE=.\trazador\lectura.f
DEP_F90_LECTU=\
	".\Debug\herramientas.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\trazador\rayisoconv3d.f
# End Source File
# End Group
# Begin Group "jacobiano"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\jacobiano\fun_jac.f90

!IF  "$(CFG)" == "estimador - Win32 Release"

!ELSEIF  "$(CFG)" == "estimador - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"fun_jac"

# End Source File
# End Group
# Begin Group "optimizador"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\optimizador\evalf.f90
DEP_F90_EVALF=\
	".\Debug\herramientas.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\optimizador\evalg.f90
DEP_F90_EVALG=\
	".\Debug\fun_jac.mod"\
	".\Debug\herramientas.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\optimizador\proj.f90
# End Source File
# Begin Source File

SOURCE=.\optimizador\spg.f90

!IF  "$(CFG)" == "estimador - Win32 Release"

!ELSEIF  "$(CFG)" == "estimador - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"Spectral_Projected_Grad"

# End Source File
# End Group
# Begin Group "herramientas"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\HERRAMIENTAS\herramientas.f90

!IF  "$(CFG)" == "estimador - Win32 Release"

!ELSEIF  "$(CFG)" == "estimador - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"herramientas"

# End Source File
# End Group
# Begin Source File

SOURCE=.\estimador.f90
DEP_F90_ESTIM=\
	".\Debug\fun_jac.mod"\
	".\Debug\herramientas.mod"\
	".\Debug\Spectral_Projected_Grad.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Group "MODELOS LIBRO"

# PROP Default_Filter ""
# Begin Group "modelo 3"

# PROP Default_Filter ""
# Begin Group "cambio v"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\e18.txt
# End Source File
# Begin Source File

SOURCE=.\m32.txt
# End Source File
# Begin Source File

SOURCE=.\m33.txt
# End Source File
# End Group
# Begin Group "cambio h No. 1"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\h111.txt
# End Source File
# Begin Source File

SOURCE=.\h222.txt
# End Source File
# Begin Source File

SOURCE=.\h333.txt
# End Source File
# End Group
# End Group
# Begin Group "modelo 2"

# PROP Default_Filter ""
# Begin Group "camb veloc No. 1"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\m2.txt
# End Source File
# Begin Source File

SOURCE=.\m21.txt
# End Source File
# Begin Source File

SOURCE=.\m23.txt
# End Source File
# End Group
# Begin Group "camb h"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\h11.txt
# End Source File
# Begin Source File

SOURCE=.\h22.txt
# End Source File
# Begin Source File

SOURCE=.\h33.txt
# End Source File
# End Group
# End Group
# Begin Group "modelo1"

# PROP Default_Filter ""
# Begin Group "camb veloc"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\m1.txt
# End Source File
# Begin Source File

SOURCE=.\m11.txt
# End Source File
# Begin Source File

SOURCE=.\m12.txt
# End Source File
# End Group
# Begin Group "cambio h"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\h1.txt
# End Source File
# Begin Source File

SOURCE=.\h2.txt
# End Source File
# Begin Source File

SOURCE=.\h3.txt
# End Source File
# End Group
# End Group
# Begin Group "modelo4"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\m4.txt
# End Source File
# End Group
# End Group
# Begin Source File

SOURCE=.\e1.txt
# End Source File
# Begin Source File

SOURCE=.\e10.txt
# End Source File
# Begin Source File

SOURCE=.\e11.txt
# End Source File
# Begin Source File

SOURCE=.\e12.txt
# End Source File
# Begin Source File

SOURCE=.\e13.txt
# End Source File
# Begin Source File

SOURCE=.\e14.txt
# End Source File
# Begin Source File

SOURCE=.\e15.txt
# End Source File
# Begin Source File

SOURCE=.\e16.txt
# End Source File
# Begin Source File

SOURCE=.\e17.txt
# End Source File
# Begin Source File

SOURCE=.\E2.TXT
# End Source File
# Begin Source File

SOURCE=.\e3.txt
# End Source File
# Begin Source File

SOURCE=.\E4.TXT
# End Source File
# Begin Source File

SOURCE=.\E5.TXT
# End Source File
# Begin Source File

SOURCE=.\E6.TXT
# End Source File
# Begin Source File

SOURCE=.\e7.txt
# End Source File
# Begin Source File

SOURCE=.\e8.txt
# End Source File
# Begin Source File

SOURCE=.\e9.txt
# End Source File
# Begin Source File

SOURCE=.\modelo1.txt
# End Source File
# End Group
# End Target
# End Project
