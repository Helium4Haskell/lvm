# Microsoft Developer Studio Project File - Name="runtime" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=runtime - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "runtime.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "runtime.mak" CFG="runtime - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "runtime - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "runtime - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "runtime - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "common" /I "../config/cl-i386-pc-windows" /I "." /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /out:"Release/lvmrun.exe"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "runtime - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "." /I "common" /I "../config/cl-i386-pc-windows" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "DEBUG" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# SUBTRACT BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /out:"Debug/lvmrun.exe" /pdbtype:sept
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "runtime - Win32 Release"
# Name "runtime - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Group "core"

# PROP Default_Filter "*.c"
# Begin Source File

SOURCE=.\core\ccall.c
# End Source File
# Begin Source File

SOURCE=.\core\dynamic.c
# End Source File
# Begin Source File

SOURCE=.\core\evaluator.c
# End Source File
# Begin Source File

SOURCE=.\core\fail.c
# End Source File
# Begin Source File

SOURCE=.\core\instr.c
# End Source File
# Begin Source File

SOURCE=.\core\loader.c
# End Source File
# Begin Source File

SOURCE=.\core\main.c
# End Source File
# Begin Source File

SOURCE=.\core\misc.c
# End Source File
# Begin Source File

SOURCE=.\core\module.c
# End Source File
# Begin Source File

SOURCE=.\core\primfloat.c
# End Source File
# Begin Source File

SOURCE=.\core\print.c
# End Source File
# Begin Source File

SOURCE=.\core\schedule.c
# End Source File
# Begin Source File

SOURCE=.\core\signals.c
# End Source File
# Begin Source File

SOURCE=.\core\stack.c
# End Source File
# Begin Source File

SOURCE=.\core\stats.c
# End Source File
# Begin Source File

SOURCE=.\core\sys.c
# End Source File
# Begin Source File

SOURCE=.\core\systhread.c
# End Source File
# Begin Source File

SOURCE=.\core\thread.c
# End Source File
# End Group
# Begin Group "heap"

# PROP Default_Filter "*.c"
# Begin Source File

SOURCE=.\heap\alloc.c
# End Source File
# Begin Source File

SOURCE=.\heap\bytes.c
# End Source File
# Begin Source File

SOURCE=.\heap\compact.c
# End Source File
# Begin Source File

SOURCE=.\heap\custom.c
# End Source File
# Begin Source File

SOURCE=.\heap\finalise.c
# End Source File
# Begin Source File

SOURCE=.\heap\fixed.c
# End Source File
# Begin Source File

SOURCE=.\heap\freelist.c
# End Source File
# Begin Source File

SOURCE=.\heap\gc_ctrl.c
# End Source File
# Begin Source File

SOURCE=.\heap\globroots.c
# End Source File
# Begin Source File

SOURCE=.\heap\major_gc.c
# End Source File
# Begin Source File

SOURCE=.\heap\memory.c
# End Source File
# Begin Source File

SOURCE=.\heap\minor_gc.c
# End Source File
# Begin Source File

SOURCE=.\heap\roots.c
# End Source File
# Begin Source File

SOURCE=.\heap\weak.c
# End Source File
# End Group
# Begin Group "prim"

# PROP Default_Filter "*.c"
# Begin Source File

SOURCE=.\prim\primio.c
# End Source File
# Begin Source File

SOURCE=.\prim\prims.c
# End Source File
# Begin Source File

SOURCE=.\prim\primstring.c
# End Source File
# Begin Source File

SOURCE=.\prim\primsys.c
# End Source File
# End Group
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
