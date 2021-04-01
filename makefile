RootDir: RootDir.exe
	RootDir

RootDir.obj: RootDir.asm ..\s2macros.asm
	c:\nasm\nasm -fobj -lRootDir.lst RootDir.asm

RootDir.exe: RootDir.obj RootDir.def
	link386 /nologo RootDir,RootDir.exe,RootDir.map,os2386,RootDir.def
