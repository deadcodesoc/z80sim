z80sim is an incomplete Z80 simulator written in Python.

$ ./run_z80.py test/call100h.rom 
0000 DI
0001 CALL 0100
0100 RET
0004 HALT
AF =0000 BC =0000 DE =0000 HL =0000 IR=0004 IX=0000 IY=0000 SP=0000 PC=0004 
AF'=0000 BC'=0000 DE'=0000 HL'=0000

Use hex2byte.py to create new "ROMs" from hexadecimal data.
