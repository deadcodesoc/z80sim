"""Z-80 Simulator."""

# Interrupt: http://z80.info/interrup.htm

from ctypes import c_byte, c_ubyte, c_short, c_ushort


class CPU(object):

    """Z-80 CPU Simulator.

    Registers: AF, IR, BC, DE, HL, AF', BC', DE', HL',
    SP, PC, IX, IY.

    Flags (F):
    7  6  5  4  3  2   1  0  (bits)
    S  Z  X  H  X  P/V N  C  (flags)
    
    C = Carry Flag
    N = Add/Subtract
    P/V = Parity/Overflow Flag
    H = Half Carry Flag
    Z = Zero Flag
    S = Sign Flag
    X = Not Used

    C  = C flag is set
    NC = C flag is reset
    Z  = Z flag is set
    NZ = Z flag is reset
    M  = S flag is set
    P  = S flag is reset
    PE = P/V is set
    PO = P/V is reset
    
    NZ        Jump if zero flag = 0. (last Z flag instr <>0)
    Z         Jump if zero flag = 1. (last Z flag instr = 0)
    NC        Jump if carry flag = 0. (last C instr = no carry)
    C         Jump if carry flag = 1. (last C instr = carry)
    PO        Jump if parity odd, parity flag = 0.
    PE        Jump if parity even, parity flag = 1.
    P         Jump if sign positive, sign flag = 0.
    M         Jump if sign negative (minus), sign flag = 1.
    """

    Flags = { 'S':   0b10000000,
              'Z':   0b01000000,
              'H':   0b00010000,
              'P/V': 0b00000100,
              'N':   0b00000010,
              'C':   0b00000001, }

    def __init__(self, **regs):
        self.pc = c_ushort(regs.get('pc', 0))
        self.sp = c_ushort(regs.get('sp', 0))
        self.ix = c_ushort(regs.get('ix', 0))
        self.iy = c_ushort(regs.get('iy', 0))
        self.af = c_byte(regs.get('a', 0)), c_byte(regs.get('f', 0))
        self.bc = c_byte(regs.get('b', 0)), c_byte(regs.get('c', 0))
        self.de = c_byte(regs.get('d', 0)), c_byte(regs.get('e', 0))
        self.hl = c_byte(regs.get('h', 0)), c_byte(regs.get('l', 0))
        self.ir = c_byte(regs.get('i', 0)), c_byte(regs.get('r', 0))
        self._af = c_byte(0), c_byte(0)
        self._bc = c_byte(0), c_byte(0)
        self._de = c_byte(0), c_byte(0)
        self._hl = c_byte(0), c_byte(0)
        self.decode_register = [
            (self.bc[0], 'B'), (self.bc[1], 'C'),
            (self.de[0], 'D'), (self.de[1], 'E'),
            (self.hl[0], 'H'), (self.hl[1], 'L'),
            None, (self.af[0], 'A') ]
        self.halted = True
        self.iff1 = False
        self.iff2 = False
        self.im = 0
        self.memory = None
        self.io = None

    def __str__(self):
        msg = (
            'AF =%04X' % self._get_register('af'),
            'BC =%04X' % self._get_register('bc'),
            'DE =%04X' % self._get_register('de'),
            'HL =%04X' % self._get_register('hl'),
            'IR=%04X' % self._get_register('ir'),
            'IX=%04X' % self.ix.value, 'IY=%04X' % self.iy.value,
            'SP=%04X' % self.sp.value, 'PC=%04X' % self.pc.value,
            '\nAF\'=%04X' % self._get_register('_af'),
            'BC\'=%04X' % self._get_register('_bc'),
            'DE\'=%04X' % self._get_register('_de'),
            'HL\'=%04X' % self._get_register('_hl'), )
        return ' '.join(msg)

    def _get_register(self, name):
        r = getattr(self, name.lower())
        msb, lsb = c_ubyte(r[0].value).value, c_ubyte(r[1].value).value
        num = (msb << 8) | lsb
        return c_ushort(num).value

    def _push(self, lsb, msb):
        self.sp.value -= 2
        self.memory[self.sp.value] = lsb
        self.memory[self.sp.value+1] = msb

    def _pop(self):
        lsb = self.memory[self.sp.value]
        msb = self.memory[self.sp.value+1]
        self.sp.value += 2
        return lsb, msb

    def _inc_bc(self):
        bc_plus1 = c_ushort(self._get_register('bc') + 1)
        self.bc[0].value = bc_plus1.value >> 8
        self.bc[1].value = bc_plus1.value & 255

    def _dec_bc(self):
        bc_minus1 = c_ushort(self._get_register('bc') - 1)
        self.bc[0].value = bc_minus1.value >> 8
        self.bc[1].value = bc_minus1.value & 255

    def _inc_de(self):
        de_plus1 = c_ushort(self._get_register('de') + 1)
        self.de[0].value = de_plus1.value >> 8
        self.de[1].value = de_plus1.value & 255

    def _inc_hl(self):
        hl_plus1 = c_ushort(self._get_register('hl') + 1)
        self.hl[0].value = hl_plus1.value >> 8
        self.hl[1].value = hl_plus1.value & 255

    @property
    def flags(self):
        msg = ['7   6   5   4   3   2   1   0',
               'S   Z   X   H   X   P/V N   C',]
        f = c_ubyte(self.af[1].value).value
        bits = '{0:08b}'.format(f)
        bits = '   '.join(list(bits))
        msg.append(bits)
        return '\n'.join(msg)

    def set_flag(self, flag):
        self.af[1].value |= CPU.Flags[flag.upper()]

    def reset_flag(self, flag):
        self.af[1].value &= ~CPU.Flags[flag.upper()]

    def get_flag(self, flag):
        return bool(self.af[1].value & CPU.Flags[flag.upper()])

    def _update_flags_8bit_add(self, current=None, last=None):
        if current is None:
            current = self.af[0].value
        if last is None:
            last = current
        if current < last:  # overflow
            self.set_flag('C')
        else:
            self.reset_flag('C')
        if current == 0:
            self.set_flag('Z')
        else:
            self.reset_flag('Z')
        if current < 0:
            self.set_flag('S')
        else:
            self.reset_flag('S')
        if last == 0x7F:
            self.set_flag('P/V')
        else:
            self.reset_flag('P/V')
        if current & 0b00001000 and last & 0b00000100:
            self.set_flag('H')
        else:
            self.reset_flag('H')
        self.reset_flag('N')

    def _update_flags_8bit_sub(self, current=None, last=None):
        if current is None:
            current = self.af[0].value
        if last is None:
            last = current
        if current < 0:
            self.set_flag('S')
        else:
            self.reset_flag('S')
        if current == 0:
            self.set_flag('Z')
        else:
            self.reset_flag('Z')
        if current < 0:
            self.set_flag('S')
        else:
            self.reset_flag('S')
        if last == 0x7F:
            self.set_flag('P/V')
        else:
            self.reset_flag('P/V')
        if current & 0b00001000 and last & 0b00000100:
            self.set_flag('H')
        else:
            self.reset_flag('H')
        self.set_flag('N')

    def _update_flags_16bit_add(self, current, last=None):
        if current is None:
            current = self.af[0].value
        if last is None:
            last = current
        if current < last:  # overflow
            self.set_flag('C')
        else:
            self.reset_flag('C')
        if current & 0b0000100000000000 and last & 0b0000010000000000:
            self.set_flag('H')
        else:
            self.reset_flag('H')
        self.reset_flag('N')

    def _update_flags_16bit_sub(self, current, last=None):
        if last is None:
            last = current
        if current < 0:
            self.set_flag('S')
        else:
            self.reset_flag('S')
        if current == 0:
            self.set_flag('Z')
        else:
            self.reset_flag('Z')
        self.set_flag('N')
        # FIXME: C + H

    def _update_flags_logical(self, current, last=None):
        if last is None:
            last = current
        if current < 0:
            self.set_flag('S')
        else:
            self.reset_flag('S')
        if current == 0:
            self.set_flag('Z')
        else:
            self.reset_flag('Z')
        if current % 2 == 0:
            self.set_flag('P/V')
        else:
            self.reset_flag('P/V')
        self.reset_flag('H')
        self.reset_flag('N')
        self.reset_flag('C')

    def decode(self):
        """Fetch and execute opcode."""
        op = self.memory[self.pc.value]
        pc = '%04X' % self.pc.value
        m = self._decode_opcode(op)
        return pc, m

    def _increment_register_r(self):
        r = self.ir[1].value & 128  # save bit 7
        self.ir[1].value &= 127     # clear bit 7
        self.ir[1].value += 1
        if self.ir[1].value == 128:
            self.ir[1].value = 0
        self.ir[1].value |= r       # restore bit 7

    def _decode_opcode(self, op):
        self._increment_register_r()
        if op == 0x00:
            self.pc.value += 1
            return 'NOP'

        # LD r,r'
        if (op >> 6) == 0b01:
            src = self.decode_register[(op & 0b00000111)]
            dst = self.decode_register[(op & 0b00111000) >> 3]
            if src and dst:
                dst[0].value = src[0].value
                self.pc.value += 1
                return 'LD %s,%s' % (dst[1], src[1])

        # LD r,n
        if (op >> 6) == 0b00 and (op & 0b00000111) == 0b110:
            dst = self.decode_register[(op & 0b00111000) >> 3]
            if dst:
                num = self.memory[self.pc.value+1]
                dst[0].value = num
                self.pc.value += 2
                return 'LD %s,%02X' % (dst[1], num)
        # LD r,(HL)
        if (op >> 6) == 0b01 and (op & 0b00000111) == 0b110:
            dst = self.decode_register[(op & 0b00111000) >> 3]
            if dst:
                addr = self._get_register('HL')
                dst[0].value = self.memory[addr]
                self.pc.value += 1
                return 'LD %s,(HL)' % dst[1]

        # LD (HL),r
        if (op >> 6) == 0b01110:
            src = self.decode_register[(op & 0b00000111)]
            if src:
                addr = self._get_register('HL')
                self.memory[addr] = c_ubyte(src[0].value).value
                self.pc.value += 1
                return 'LD (HL),%s' % src[1]

        if op == 0x01:
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            self.bc[0].value = msb
            self.bc[1].value = lsb
            self.pc.value += 3
            return 'LD BC,%02X%02X' % (msb, lsb)
        if op == 0x02:
            self.memory[self._get_register('BC')] = self.af[0].value
            self.pc.value += 1
            return 'LD (BC),A'
        if op == 0x03:
            self._inc_bc()
            self.pc.value += 1
            return 'INC BC'
        #if op == 0x06:
        #    num = self.memory[self.pc.value+1]
        #    self.bc[0].value = num
        #    self.pc.value += 1
        #    return 'LD B,%02X' % num
        if op == 0x08:
            a, f = self.af[0].value, self.af[1].value
            self.af[0].value, self.af[1].value = self._af[0].value, self._af[1].value
            self._af[0].value, self._af[1].value = a, f
            self.pc.value += 1
            return "EX AF,AF'"
        if op == 0x09:
            hl = self._get_register('HL')
            bc = self._get_register('BC')
            x = hl - bc
            lsb = c_byte(x & 255)
            msb = c_byte(x >> 8)
            self.hl[0].value = msb.value
            self.hl[1].value = lsb.value
            self._update_flags_16bit_add(x, hl)
            self.pc.value += 1
            return 'ADD HL,BC'
        if op == 0x0a:
            self.af[0] = self.memory[self._get_register('BC')]
            self.pc.value += 1
            return "LD A,(BC)"
        if op == 0x0b:
            bc_minus1 = c_ushort(self._get_register('bc') - 1)
            self.bc[0].value = bc_minus1.value >> 8
            self.bc[1].value = bc_minus1.value & 255
            self.pc.value += 1
            return 'DEC BC'
        #if op == 0x0e:
        #    num = self.memory[self.pc.value+1]
        #    self.bc[1].value = num
        #    self.pc.value += 1
        #    return 'LD C,%02X' % num
        if op == 0x0f:
            self.reset_flag('N')
            self.reset_flag('H')
            a = self.af[0].value
            self.af[0].value = (self.af[0].value >> 1)
            if a & 0x0b00000001:
                self.set_flag('C')
                self.af[0].value |= 0b10000000
            else:
                self.reset_flag('C')
                self.af[0].value &= 0b01111111
            self.pc.value += 1
            return 'RRCA'
        if op == 0x10:
            num = c_byte(self.memory[self.pc.value+1]).value
            self._dec_b()
            if self.bc[0].value == 0:
                self.pc.value += 2
            else:
                self.pc.value += (2+num)
            return 'DJNZ %02X' % num
        if op == 0x11:
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            self.de[0].value = msb
            self.de[1].value = lsb
            self.pc.value += 3
            return 'LD DE,%02X%02X' % (msb, lsb)
        if op == 0x12:
            self.memory[self._get_register('DE')] = self.af[0].value
            self.pc.value += 1
            return 'LD (DE),A'
        if op == 0x13:
            self._inc_de()
            self.pc.value += 1
            return 'INC DE'
        if op == 0x18:
            num = c_byte(self.memory[self.pc.value+1]).value
            self.pc.value += (2+num)
            return 'JR %02X' % num
        if op == 0x1a:
            self.af[0] = self.memory[self._get_register('DE')]
            self.pc.value += 1
            return 'LD A,(DE)'
        if op == 0x1b:
            de_minus1 = c_ushort(self._get_register('de') - 1)
            self.de[0].value = de_minus1.value >> 8
            self.de[1].value = de_minus1.value & 255
            self.pc.value += 1
            return 'DEC DE'
        if op == 0x1f:
            self.reset_flag('N')
            self.reset_flag('H')
            a = self.af[0].value
            self.af[0].value = (self.af[0].value >> 1)
            if self.get_flag('C') == True:
                self.af[0].value |= 0b10000000
            else:
                self.af[0].value &= 0b01111111
            if a & 0x0b00000001:
                self.set_flag('C')
            else:
                self.reset_flag('C')
            self.pc.value += 1
            return 'RRA'
        if op == 0x20:
            num = c_byte(self.memory[self.pc.value+1]).value
            self.pc.value += 2
            if self.get_flag('Z') == False:
                self.pc.value += num
            return 'JR NZ,%02X' % num
        if op == 0x21:
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            self.hl[0].value, self.hl[1].value = msb, lsb
            self.pc.value += 3
            return 'LD HL,%02X%02X' % (msb, lsb)
        if op == 0x22:
            addr = (self.memory[self.pc.value+1] << 8) + self.memory[self.pc.value+2]
            self.memory[addr] = self.hl[1].value
            self.memory[addr+1] = self.hl[0].value
            self.pc.value += 3
            return 'LD (%04X),HL' % addr
        if op == 0x23:
            self._inc_hl()
            self.pc.value += 1
            return 'INC HL'
        if op == 0x24:
            h = self.hl[0].value
            self.hl[0].value -= 1
            self.pc.value += 1
            self._update_flags_8bit_add(self.hl[1].value, h)
            return 'INC H'
        if op == 0x25:
            h = self.hl[0].value
            self.hl[0].value -= 1
            self._update_flags_8bit_sub(self.hl[0].value, h)
            self.pc.value += 1
            return 'DEC H'
        if op == 0x28:
            e = c_byte(self.memory[self.pc.value+1]).value
            self.pc.value += 2
            if self.get_flag('Z') == True:
                self.pc.value += e
            return 'JR Z,%02X' % e
        if op == 0x2a:
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            self.hl[0].value = msb
            self.hl[1].value = lsb
            self.pc.value += 3
            return 'LD HL,(%02X%02X)' % (msb, lsb)
        if op == 0x2b:
            hl_minus1 = c_ushort(self._get_register('hl') - 1)
            self.hl[0].value = hl_minus1.value >> 8
            self.hl[1].value = hl_minus1.value & 255
            self.pc.value += 1
            return 'DEC HL'
        if op == 0x2f:
            self.af[0].value = ~self.af[0].value
            self.pc.value += 1
            return 'CPL'
        if op == 0x30:
            e = c_byte(self.memory[self.pc.value+1]).value
            self.pc.value += 2
            if self.get_flag('C') == False:
                self.pc.value += e
            return 'JR NC,%02X' % e
        if op == 0x31:
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            addr = (msb << 8) + lsb
            self.sp.value = addr
            self.pc.value += 3
            return 'LD SP,%04X' % addr
        if op == 0x32:
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            addr = (msb << 8) + lsb
            self.memory[addr] = c_ubyte(self.af[0].value).value
            self.pc.value += 3
            return 'LD (%x),A' % addr
        if op == 0x33:
            self.sp.value += 1
            self.pc.value += 1
            return 'INC SP'
        if op == 0x35:
            addr = self._get_register('hl')
            try:
                self.memory[addr] -= 1
            except OverflowError:
                self.memory[addr] = 0xff
            self.pc.value += 1
            return 'DEC (HL)'
        if op == 0x36:
            num = self.memory[self.pc.value+1]
            addr = (self.hl[0].value << 8) + self.hl[1].value
            self.memory[addr] = num
            self.pc.value += 2
            return 'LD (HL),%x' % num
        if op == 0x38:
            e = c_byte(self.memory[self.pc.value+1]).value
            self.pc.value += 2
            if self.get_flag('C') == True:
                self.pc.value += e
            return 'JR C,%x' % e
        if op == 0x3a:
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            addr = lsb + (msb << 8)
            self.af[0].value = self.memory[addr]
            self.pc.value += 3
            return 'LD A,(%x)' % addr
        if op == 0x3b:
            self.sp.value -= 1
            self.pc.value += 1
            return 'DEC SP'
        if op == 0x3c:
            a = self.af[0].value
            self.af[0].value += 1
            self._update_flags_8bit_add(self.af[0].value, a)
            self.pc.value += 1
            return 'INC A'
        #if op == 0x3e:
        #    num = self.memory[self.pc.value+1]
        #    self.af[0].value = num
        #    self.pc.value += 2
        #    return 'LD A,%02X' % num
        #if op == 0x47:
        #    self.bc[0].value = self.af[0].value
        #    self.pc.value += 1
        #    return 'LD B,A'
        #if op == 0x4f:
        #    self.bc[1].value = self.af[0].value
        #    self.pc.value += 1
        #    return 'LD C,A'
        #if op == 0x60:
        #    self.hl[0].value = self.bc[0].value
        #    self.pc.value += 1
        #    return 'LD H,B'
        #if op == 0x61:
        #    self.hl[0].value = self.bc[1].value
        #    self.pc.value += 1
        #    return 'LD H,C'
        #if op == 0x68:
        #    self.hl[1].value = self.bc[0].value
        #    self.pc.value += 1
        #    return 'LD L,B'
        #if op == 0x69:
        #    self.hl[1].value = self.bc[1].value
        #    self.pc.value += 1
        #    return 'LD L,C'
        #if op == 0x6a:
        #    self.hl[1].value = self.de[0].value
        #    self.pc.value += 1
        #    return 'LD L,D'
        #if op == 0x6b:
        #    self.hl[1].value = self.de[1].value
        #    self.pc.value += 1
        #    return 'LD L,E'
        if op == 0x70:
            addr = self._get_register('HL')
            self.memory[addr] = c_ubyte(self.bc[0].value).value
            self.pc.value += 1
            return 'LD (HL),B'
        if op == 0x71:
            addr = self._get_register('HL')
            self.memory[addr] = c_ubyte(self.bc[1].value).value
            self.pc.value += 1
            return 'LD (HL),C'
        if op == 0x72:
            addr = self._get_register('HL')
            self.memory[addr] = c_ubyte(self.de[0].value).value
            self.pc.value += 1
            return 'LD (HL),D'
        if op == 0x73:
            addr = self._get_register('HL')
            self.memory[addr] = c_ubyte(self.de[1].value).value
            self.pc.value += 1
            return 'LD (HL),E'
        if op == 0x74:
            addr = self._get_register('HL')
            self.memory[addr] = c_ubyte(self.hl[0].value).value
            self.pc.value += 1
            return 'LD (HL),H'
        if op == 0x75:
            addr = self._get_register('HL')
            self.memory[addr] = c_ubyte(self.hl[1].value).value
            self.pc.value += 1
            return 'LD (HL),L'
        if op == 0x76:
            self.halted = True
            return 'HALT'
        if op == 0x77:
            self.pc.value += 1
            addr = self._get_register('HL')
            self.memory[addr] = c_ubyte(self.af[0].value).value
            return 'LD (HL),A'
        #if op == 0x78:
        #    self.af[0].value = self.bc[0].value
        #    self.pc.value += 1
        #    return 'LD A,B'
        #if op == 0x79:
        #    self.af[0].value = self.bc[1].value
        #    self.pc.value += 1
        #    return 'LD A,C'
        #if op == 0x7c:
        #    self.af[0].value = self.hl[0].value
        #    self.pc.value += 1
        #    return 'LD A,H'
        #if op == 0x7d:
        #    self.af[0].value = self.hl[1].value
        #    self.pc.value += 1
        #    return 'LD A,H'
        if op == 0x88:
            c = int(self.get_flag('C'))
            a = self.af[0].value
            self.af[0].value = self.af[0].value + self.bc[0].value + c
            self._update_flags_8bit_add(self.af[0].value, a)
            self.pc.value += 1
            return 'ADC A,B'
        if op == 0xa7:
            self.af[0].value &= self.af[0].value
            self._update_flags_logical(self.af[0].value)
            self.pc.value += 1
            return 'AND A'
        if op == 0xaf:
            self.af[0].value ^= self.af[0].value
            self._update_flags_logical(self.af[0].value)
            self.pc.value += 1
            return 'XOR A'
        if op == 0xb0:
            self.af[0].value |= self.bc[1].value
            self._update_flags_logical(self.af[0].value)
            self.pc.value += 1
            return 'OR B'
        if op == 0xb1:
            self.af[0].value |= self.bc[1].value
            self._update_flags_logical(self.af[0].value)
            self.pc.value += 1
            return 'OR C'
        if op == 0xb7:
            self.af[0].value |= self.af[0].value
            self._update_flags_logical(self.af[0].value)
            self.pc.value += 1
            return 'OR A'
        if op == 0xbc:
            a = self.af[0].value
            h = self.hl[0].value
            self._update_flags_8bit_sub(a-h, a)
            self.pc.value += 1
            return 'CP H'
        if op == 0xbe:
            addr = (self.hl[0].value << 8) + self.hl[1].value
            num = self.memory[addr]
            self._update_flags_8bit_sub(a-num, a)
            self.pc.value += 1
            return 'CP (HL)'
        if op == 0xc1:
            lsb, msb = self._pop()
            self.bc[0].value, self.bc[1].value = msb, lsb
            self.pc.value += 1
            return 'POP BC'
        if op == 0xc3:
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            addr = lsb + (msb << 8)
            self.pc.value = addr
            return 'JP %04X' % addr
        if op == 0xc5:
            self._push(self.bc[1].value, self.bc[0].value)
            self.pc.value += 1
            return 'PUSH BC'
        if op == 0xc7:
            addr = self.pc.value+1
            self._push(addr & 255, addr >> 8)
            self.pc.value = 0x0
            return 'RST 00H'
        if op == 0xcd:
            raddr = self.pc.value+3
            lsb = raddr & 255
            msb = raddr >> 8
            self._push(lsb, msb)
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            addr = (msb << 8) + lsb
            self.pc.value = addr
            return 'CALL %04X' % addr
        if op == 0xcf:
            addr = self.pc.value+1
            self._push(addr & 255, addr >> 8)
            self.pc.value = 0x8
            return 'RST 08H'
        if op == 0xd7:
            addr = self.pc.value+1
            self._push(addr & 255, addr >> 8)
            self.pc.value = 0x10
            return 'RST 10H'
        if op == 0xc9:
            lsb, msb  = self._pop()
            addr = (msb << 8) + lsb
            self.pc.value = addr
            return 'RET'
        if op == 0xcf:
            addr = self.pc.value+1
            self._push(addr & 255, addr >> 8)
            self.pc.value = 0x08
            return 'RST 08H'
        if op == 0xd3:
            port = self.memory[self.pc.value+1]
            self.io[port] = c_ubyte(self.af[0].value).value
            self.pc.value += 2
            return 'OUT (%02X),A' % port
        if op == 0xd5:
            self._push(self.de[1].value, self.de[0].value)
            self.pc.value += 1
            return 'PUSH DE'
        if op == 0xd6:
            # FIXME: update flags
            num = self.memory[self.pc.value+1]
            self.af[0].value -= num
            self.pc.value += 2
            return 'SUB %02X' % num
        if op == 0xe7:
            addr = self.pc.value+1
            self._push(addr & 255, addr >> 8)
            self.pc.value = 0x20
            return 'RST 20H'
        if op == 0xd9:
            b, c = self.bc[0].value, self.bc[1].value
            self.bc[0].value, self.bc[1].value = self._bc[0].value, self._bc[1].value
            self._bc[0].value, self._bc[1].value = b, c
            d, e = self.de[0].value, self.de[1].value
            self.de[0].value, self.de[1].value = self._de[0].value, self._de[1].value
            self._de[0].value, self._de[1].value = d, e
            h, l = self.hl[0].value, self.hl[1].value
            self.hl[0].value, self.hl[1].value = self._hl[0].value, self._hl[1].value
            self._hl[0].value, self._hl[1].value = h, l
            self.pc.value += 1
            return 'EXX'
        if op == 0xdb:
            port = self.memory[self.pc.value+1]
            self.af[0].value = self.io[port]
            self.pc.value += 2
            return 'IN A,(%02X)' % port
        if op == 0xdf:
            addr = self.pc.value+1
            self._push(addr & 255, addr >> 8)
            self.pc.value = 0x18
            return 'RST 18H'
        if op == 0xe3:
            lsb, msb = self.memory[self.sp], self.memory[self.sp+1]
            self.memory[self.sp+1], self.memory[self.sp] = self.hl[0].value, self.hl[1].value
            self.hl[0].value = msb, self.hl[1].value = lsb
            self.pc.value += 1
            return 'EX (SP),HL'
        if op == 0xe9:
            self.pc.value = self.hl.value
            return 'JP (HL)'
        if op == 0xeb:
            d, e = self.de[0].value, self.de[1].value
            self.de[0].value, self.de[1].value = self.hl[0].value, self.hl[1].value
            self.hl[0].value, self.hl[1].value = d, e
            self.pc.value += 1
            return 'EX DE,HL'
        if op == 0xed:
            return self._decode_opcode_ed()
        if op == 0xef:
            addr = self.pc.value+1
            self._push(addr & 255, addr >> 8)
            self.pc.value = 0x28
            return 'RST 28H'
        if op == 0xf3:
            self.iff1 = False
            self.pc.value += 1
            return 'DI'
        if op == 0xf6:
            num = self.memory[self.pc.value+1]
            self.af[0].value |= num
            self._update_flags_logical(self.af[0].value)
            self.pc.value += 2
            return 'OR %02X' % num
        if op == 0xf7:
            addr = self.pc.value+1
            self._push(addr & 255, addr >> 8)
            self.pc.value = 0x30
            return 'RST 30H'
        if op == 0xfa:
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            addr = (msb << 8) + lsb
            if self.get_flag('S') == True:
                self.pc.value = addr
            else:
                self.pc.value += 3
            return 'JP M,%04X' % addr
        if op == 0xfb:
            self.iff1 = True
            self.pc.value += 1
            return 'EI'
        if op == 0xfc:
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            addr = (msb << 8) + lsb
            if self.get_flag('S') == True:
                raddr = self.pc.value+3
                lsb = raddr & 255
                msb = raddr >> 8
                self._push(lsb, msb)
                self.pc.value = addr
            else:
                self.pc.value += 3
            return 'CALL M,%04X' % addr
        if op == 0xfd:
            return self._decode_opcode_fd()
        if op == 0xff:
            addr = self.pc.value+1
            self._push(addr & 255, addr >> 8)
            self.pc.value = 0x38
            return 'RST 38H'
        raise RuntimeError("bad opcode '%02X'\n%s" % (op, self))

    def _decode_opcode_dd(self):
        self.pc.value += 1
        op = self.memory[self.pc.value]
        if op == 0xe9:
            self.pc.value = self.ix.value
            return 'JP (IX)'
        raise RuntimeError("bad opcode 'DD %02X'\n%s" % (op, self))

    def _decode_opcode_fd(self):
        self.pc.value += 1
        op = self.memory[self.pc.value]
        if op == 0x21:
            lsb = self.memory[self.pc.value+1]
            msb = self.memory[self.pc.value+2]
            self.iy.value = (msb << 8) + lsb
            self.pc.value += 3
            return 'LD IY,%02X%02X' % (msb, lsb)
        if op == 0xe9:
            self.pc.value = self.iy.value
            return 'JP (IY)'
        raise RuntimeError("bad opcode 'FD %02X'\n%s" % (op, self))

    def _decode_opcode_ed(self):
        self.pc.value += 1
        op = self.memory[self.pc.value]
        if op == 0x40:
            self.bc[0].value = self.io[self.bc[1].value]
            self.pc.value += 1
            return 'IN B,(C)'
        if op == 0x41:
            self.io[self.bc[1].value] = self.bc[0].value
            self.pc.value += 1
            return 'OUT (C),B'
        if op == 0x42:
            c = int(self.get_flag('C'))
            hl = self._get_register('HL')
            bc = self._get_register('BC')
            x = hl - bc - c
            lsb = c_byte(x & 255)
            msb = c_byte(x >> 8)
            self.hl[0].value = msb.value
            self.hl[1].value = lsb.value
            self._update_flags_16bit_sub(x, hl)
            self.pc.value += 1
            return 'SBC HL,BC'
        if op == 0x47:
            self.ir[0].value = self.af[0].value
            self.pc.value += 1
            return 'LD I,A'
        if op == 0x4F:
            self.ir[1].value = self.af[0].value
            self.pc.value += 1
            return 'LD R,A'
        if op == 0x57:
            self.af[0].value = self.ir[0].value
            self.pc.value += 1
            return 'LD A,I'
        if op == 0x5F:
            self.af[0].value = self.ir[1].value
            self.pc.value += 1
            return 'LD A,R'
        if op == 0x46:
            self.im = 0
            self.pc.value += 1
            return 'IM 0'
        if op == 0x56:
            self.im = 1
            self.pc.value += 1
            return 'IM 1'
        if op == 0x5E:
            self.im = 2
            self.pc.value += 1
            return 'IM 2'
        if op == 0xb0:
            # (DE) <- (HL), DE++, HL++, BC--
            de = self._get_register('BC')
            hl = self._get_register('HL')
            self.memory[de] = self.memory[hl]
            self._inc_de()
            self._inc_hl()
            self._dec_bc()
            self.reset_flag('N')
            self.reset_flag('P/V')
            self.reset_flag('H')
            if self._get_register('bc') == 0:
                self.pc.value += 1
            else:
                self.pc.value -= 2
            return 'LDIR'
        raise RuntimeError("bad opcode 'ED %02X'\n%s" % (op, self))

    def reset(self):
        self.halted = True
        self.iff1 = True
        self.iff1_mode = 0
        self.ix.value, self.iy.value, self.pc.value, self.sp.value = 0, 0, 0, 0
        self.af[0].value, self.af[1].value = 0, 0
        self.bc[0].value, self.bc[1].value = 0, 0
        self.de[0].value, self.de[1].value = 0, 0
        self.hl[0].value, self.hl[1].value = 0, 0
        self.ir[0].value, self.ir[1].value = 0, 0
        self._af[0].value, self._af[1].value = 0, 0
        self._bc[0].value, self._bc[1].value = 0, 0
        self._de[0].value, self._de[1].value = 0, 0
        self._hl[0].value, self._hl[1].value = 0, 0

    def step(self):
        pc, m = self.decode()
        print pc, m

    def start(self, at_position=None):
        if at_position is not None:
            self.pc.value = at_position
        self.halted = False
        while not self.halted:
            try:
                self.step()
            except KeyboardInterrupt:
                print
                self.halted = True

    def nmi(self):
        self.pc.value = 0x66
        # FIXME: iff1/2


if __name__ == '__main__':
    cpu = CPU()
    print cpu
