#!/usr/bin/env python

"""Read Hexadecimal values in ASCII and write bytes."""

import binascii


def write_data(sin=None, sout=None):
    for data in sin:
        if not data: break
        if data == '': continue
        values = ''.join(data.split())  # remove spaces
        sout.write(binascii.a2b_hex(values))


if __name__ == '__main__':
    import sys
    sys.argc = len(sys.argv)
    if sys.argc == 1:
        sh = sys.stdin
        sr = sys.stdout
    elif sys.argc == 2:
        sh = open(sys.argv[1])
        sr = sys.stdout
    else:
        sh = open(sys.argv[1])
        sr = open(sys.argv[2], 'w')
    write_data(sh, sr)
    sh.close()
    sr.close()
