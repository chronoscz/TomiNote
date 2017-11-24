if not defined LAZDIR (
  set LAZDIR=C:\lazarus
)
%LAZDIR%\lazbuild.exe --lazarusdir=%LAZDIR% --build-mode="Release" --cpu=i386 --os=win32 ../../src/TomiNote.lpr
copy ..\..\src\TomiNote.exe ..\..\src\lib\i386-win32
%LAZDIR%\lazbuild.exe --lazarusdir=%LAZDIR% --build-mode="Release" --cpu=x86_64 --os=win64 ../../src/TomiNote.lpr
copy ..\..\src\TomiNote.exe ..\..\src\lib\x86_64-win64

"C:\Program Files (x86)\Inno Setup 5\iscc.exe" TomiNote.iss