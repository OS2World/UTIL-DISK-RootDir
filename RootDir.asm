;RootDir: Dump Win-95 longnames (the brain-dead ~1 style) from the root directory of a floppy to stdout (a la DIR roughly) - will show ~1 name, space, long name (aka "dead name" ie "brain dead")
;Note: Dead names consist of Unicode characters. This program assumes that the high bytes are all 0, and dumps the low bytes only. This could cause odd problems with (1) Chinese/Japanese etc characters; (2) maybe endashes, emdashes, "smart" quotes, etc.
;Eventually, maybe, I'll make a REXX-callable DLL which lists to a stem, same way
;Note: This is all the docs you get, apart from the brief README.TXT. Sorry, but I mainly built this for my own use.
;For a more powerful program, which (unfortunately) doesn't work on FAT12, look for "wir" (pun on "dir" presumably).
;Note: Currently the path is hardcoded into the data segment. To work on a different drive, just change the value of 'path' below. It *must* be a drive letter, a colon, and a null byte.
;This program is open source. If you like it, please send me an email at talldad@kepl.com.au - I'd love to hear from you!
;The version number is the first entry in the data area (immediately before the CR/LF), and consists of:
;1) A one letter code - C for Chris Angelico's version, other people please use other codes.
;2) Three bytes, storing the major, minor, revision components of the version number.
;The latest version of this program should always be available at http://www.kepl.com.au/esstu/RootDir.HTML (on The Esstu Pack).

;The long names are dumped in 13-character blocks (due to the internal structure of a brain-dead long name - 13 characters per directory entry), as follows:
;First block is the DOS name. Eight chars for the base name, space, three for the extension, one more space.
;The rest of the blocks have the long name, 13 bytes at a time. The last block is null-terminated and then seems to be padded with 0xFF's (don't rely on this though).

;Each file name is sent to STDOUT, followed by a CR/LF pair. Hidden files and deleted files are displayed along with everything else.

;This source should compile with nasm (I use 0.98.24), with the Esstu Macros library (s2macros.asm) version 1.1.0 in the parent directory. This is referred to once, in the %include line; change it if required.
;The makefile assumes that nasm is installed as c:\nasm\nasm.exe - correct if wrong.

;Version history:
;1.0.0 - initial release.
;1.0.1 - added this version history, more comments, and a CMD for applying .LONGNAME EAs. Also removed two bugs - the first entry was being ignored, and something was PUSHing more than it POPped.

%include "..\s2macros.asm" ;Handy macros

section _TEXT class=CODE use32 flat
..start: ;Set the entry point for the EXE
main: ;"Ordinary" label to allow local labels (can't have local labels after ..start)

callos2 DosOpen,path,handle,action,0,0,1,1000001101000000b,0 ;Open the drive as a single file
or eax,eax
jnz near .finnoclose ;If error, exit (without closing)
callos2 DosRead,[handle],data,0x20,nwritten ;Read in 32 bytes, and thus get the BIOS Parameter Block
or eax,eax
jnz near .fin ;If error, exit (first closing the file)

;Set EAX to word[data+0x0E]+byte[data+0x10]*word[data+0x16]
;0x0E is the number of sectors in the reserved area; 0x10 is the number of copies of the FAT (usu 2); 0x16 is sectors per FAT.
mov ax,[data+0x0E] ;Yes, AX not EAX - need 16-bit fetch
movzx ecx,byte[data+0x10] ;Get a single byte into a 32-bit register
.loop:
add ax,[data+0x16]
loop .loop
;EAX now has the sector number of the root directory.

movzx ebx,word[data+0x0B] ;Get bytes per sector (a word value) into a dword register
mul ebx ;Multiply EAX*EBX and put the result in EDX:EAX (but since the high words of EAX and EBX should be 0, EDX should end up 0). I could do this with word arithmetic (mul bx), but it's more convenient to have the result in EAX instead of DX:AX.
movzx ecx,word[data+0x11] ;Number of root directory entries
cmp ecx,maxdirents ;Check against the maximum number of directory entries - there's storage sufficient for maxdirents directory entries.
jbe .nomax
mov ecx,maxdirents ;Max out ecx
.nomax:
push ecx ;Store the count of entries, for later
shl ecx,5 ;Multiply by 32 to get number of bytes to read (32 bytes per entry)
push ecx ;Store the number of bytes, for later

;EAX now holds the number of bytes in the pre-directory-entry information, which equals the byte offset at which to start reading. We could set the file pointer to this, but the commented-out call below seems to crash the program.
;callos2 DosSetFilePtr,[handle],eax,0,[newptr] ;There's something wrong with this line.
sub eax,0x20 ;We've already read in 0x20 bytes
callos2 DosRead,[handle],data,eax,nwritten ;So we read in the right number of bytes - the target position minus 20 hex.
or eax,eax
jnz near .fin ;If error, quit
pop ecx ;Get back the number of bytes to read
callos2 DosRead,[handle],data,ecx,nwritten ;And read them
or eax,eax
jnz near .fin ;If error, quit
mov esi,data ;Get a pointer
pop ecx ;And the count

;OK, the main loop.
.getdirent:
cmp byte[esi],0 ;If the first byte of the file name is a null, we've reached the end of the list (DOS standard since v2.0).
jz near .enddirent
cmp byte[esi+0x0B],0x0F ;The extra entries have attribute 0x0F - they're hidden, system, read-only volume labels!
jnz .notdeadname ;If it's not 0x0F, it's not a (brain-)dead name
mov edi,longname ;If it is, we'll get a pointer to the right place.
push ecx ;Save the main loop's ecx - we need another loop here.
movzx ecx,byte[esi] ;This byte is the index number of the entry. 01 for 1st, 02 for 2nd, etc.
test cl,0x40
pushf ;Will use these flags later.
and cl,0x3F ;The _last_ entry (ie tail end of name) has its index 0x40 higher - 0x41, 0x42 or whatever. I'm using this value further down - hence the pushf.
.addedi:
add edi,13 ;Add 13*ecx to edi
loop .addedi
;At this point EDI has a pointer into longname. This pointer is such that filling in all the entries will give a 13-byte hole, then the long name, null-terminated. See below for what the 13-byte header is used for!

;Skip one byte, grab 5 characters, skip 3 bytes, grab 6, skip two, grab two.
push esi ;We need to preserve esi
inc esi ;Skip the first byte
mov ecx,5
.loop1:
lodsw ;Grab a word, store a byte - ignore the high byte of each Unicode character
stosb
loop .loop1
add esi,3
mov ecx,6
.loop2:
lodsw
stosb
loop .loop2
inc esi
inc esi
lodsw
stosb
lodsw
stosb
pop esi ;Restore the main loop's esi

popf ;Get back the flags stored above
jz .notlast
sub edi,longname
mov [nbytes],edi ;Store the number of bytes, if this one had 0x40 on it.
.notlast:

pop ecx ;Restore the main loop's ecx
jmp .endloop ;Skip the notdeadname code
.notdeadname: ;This is a real entry.
mov edi,longname ;Now _here_ we use the first 13 bytes!
push ecx ;Save ecx and esi - we need them.
push esi
mov ecx,8 ;Copy 8 bytes of base name
rep movsb
mov al,' ' ;Add a space
stosb
mov ecx,3 ;Three bytes of extension
rep movsb
stosb ;Another space - AL still has ' '
callos2 DosWrite,1,longname,[nbytes],nwritten ;Send it to STDOUT
say crlf ;And follow it with a CRLF
mov dword[nbytes],13 ;Reset [nbytes] in case the next entry doesn't have a long name
pop esi ;Restore esi and ecx
pop ecx

.endloop: ;Here the two routines finish, and the loop resumes.
add esi,32 ;Point to the next directory entry
dec ecx ;Can't use loop if the target is too high. Could use call for the routines, or slim them down, or something, but I don't feel like it.
jnz near .getdirent
;loop .getdirent
.enddirent:

.finok:
xor eax,eax ;Zero eax to indicate successful termination
.fin:
push eax ;Save eax - if .fin was jmp'd it will have an error code.
callos2 DosClose,[handle] ;Close the file. This returns a value in eax - almost certainly 0.
pop eax ;And restore eax
.finnoclose: ;JMP here to terminate without closing - only used if error on DosOpen.
callos2 DosExit,1,eax ;Exit all threads, return eax

group DGROUP STACK ;This is the only thing in DGROUP. The linker expects this setup, I think
section STACK stack class=STCK use32
resb 32768 ;32KB stack - can increase or decrease as required

section _DATA dword public class=DATA use32 flat
version db "C",1,0,1 ;A "C" for "Chris" signifying my version, then three bytes, major minor revision, storing the version number.
msg crlf,13,10 ;Used in the main loop - 'say crlf'
nwritten dd 0 ;Used by all DosWrites (and DosReads!)

handle dd 0 ;Gets the file handle from DosOpen
action dd 0 ;Gets the open action from DosOpen - ignored.
path db "A:",0 ;The path - can be changed as required.
newptr dd 0 ;From DosSetFilePtr
nbytes dd 13 ;For the longname display

maxdirents equ 224 ;Size of root directory on 1.44MB floppy. The number of entries listed in the boot sector is capped at this value - any further entries will be ignored.
data resb maxdirents*32 ;Big enough to hold the data for maxdirents directory entries
longname resb 20480-($-$$) ;Plenty of space - fill so the _DATA segment is 20KB. Can be shrunk; but in this version don't let the total size of data+longname fall too short - DosRead is used instead of DosSetFilePtr.
