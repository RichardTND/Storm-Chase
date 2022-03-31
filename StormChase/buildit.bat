del stormchase.prg
c:\64tass\64tass stormchase.asm -ostormchase.prg 
ifnotexist stormchase.prg goto stoprunning
c:\exomizer\win32\exomizer.exe sfx $4800 stormchase.prg -o stormchase.prg -x1
c:\vice_runtime\x64sc.exe stormchase.prg
stoprunning:
end