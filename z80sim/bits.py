"""Z-80 Simulator."""

def set_bit(n, value):
    return value | (1 << n)

def reset_bit(n, value):
    return value & ~(1 << n) 

def toggle_bit(n, value):
    return value ^ (1 << n)

def test_bit(n, value):
    return value & (1 << n)

def to_imm16(lsb, msb):
    return (msb << 8) + lsb

def to_imm8s(n):
    msb = n >> 8
    lsb = n & 255
    return lsb, msb