"""Z-80 Simulator."""

import array
import cpu


class Machine(object):
    """Generic Z-80 Machine."""

    def __init__(self, cpu_model=None, memory=None, io=None):
        if cpu_model is None:
            cpu_model = cpu.CPU()
        if memory is None:
            memory = array.array('B', [0] * 2**16)
        if io is None:
            io = array.array('B', [0] * 2**8)
        self.cpu = cpu_model
        self.memory = memory
        self.io = io
        self.cpu.memory = memory
        self.cpu.io = io

    def __str__(self):
        return 'Generic Z-80 CPU. RAM: %d bytes' % len(self.memory)

    def load(self, code, base=0):
        for i, byte in enumerate(code):
            self.memory[base+i] = byte
        return i+1

    def load_rom(self, romfile, base=0):
        return self.load((ord(x) for x in open(romfile).read()))

    def start(self, position=None):
        self.cpu.start(position)

    def step(self):
        self.cpu.step()


if __name__ == '__main__':
    m = Machine()
    print m
