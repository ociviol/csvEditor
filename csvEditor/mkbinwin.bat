
C:\lazarus\fpc\3.0.4\bin\x86_64-win64\fpc.exe -B -MObjFPC -Scghi -CX -O3 -XX -WG -l -vewnhibq -FiC:\Dev\GitHub\csvEditor\csvEditor\lib\x86_64-win64 -Fu..\..\Utils -FuC:\lazarus\lcl\units\x86_64-win64\win32 -FuC:\lazarus\lcl\units\x86_64-win64 -FuC:\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\lazarus\packager\units\x86_64-win64 -FuC:\Dev\GitHub\csvEditor\csvEditor\ -FUC:\Dev\GitHub\csvEditor\csvEditor\lib\x86_64-win64\ -FEC:\Dev\GitHub\csvEditor\csvEditor\ -o"..\precompiled binairies\Windows\csvEditor.exe"  -dLCL -dLCLwin32 csvEditorWin.lpr

//copy .\Bin-Win\*.* "..\precompiled binairies\Windows"
cd "..\precompiled binairies\Windows"
del csvEditor.zip
..\..\..\Tools\7z a -tzip -sdel csvEditorWin.zip *.exe 
del *.exe 
pause
