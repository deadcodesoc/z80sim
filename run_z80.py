#!/usr/bin/env python

"""Z-80 Simulator."""

import cmd
import readline

import z80sim


class Console(cmd.Cmd):
    
    intro = """Z-80 Simulator (use ^D or quit to exit)
Copyright (c) 2017 Ruda Moura"""
    prompt = '> '

    def do_regs(self, line):
        print self.machine.cpu

    def help_regs(self):
        print "FOo"

    def do_flags(self, line):
        print self.machine.cpu.flags

    def do_reset(self, line):
        self.machine.cpu.reset()

    def do_load(self, filename):
        try:
            self.machine.load_rom(filename)
        except IOError as e:
            print e

    def do_start(self, line):
        try:
            self.machine.cpu.start()
        except RuntimeError as e:
            print e

    do_run = do_start

    def do_continue(self, line):
        try:
            self.machine.start(self.machine.cpu.pc.value)
        except RuntimeError as e:
            print e

    def do_step(self, line):
        try:
            self.machine.step()
        except RuntimeError as e:
            print e

    do_next = do_step

    def do_quit(self, line):
        return True

    do_exit = do_quit

    def do_EOF(self, line):
        print 'Halted!'
        return True

    def do_nmi(self, line):
        self.machine.cpu.nmi()

    def do_ports(self, line):
        dump(self.machine.io)

    do_io = do_ports

    def do_mem(self, line):
        args = line.split()
        if args[0:]:
            addr = int(args[0], 16)
        else:
            addr = self.machine.cpu.pc.value
        if args[1:]:
            max_ = int(args[1], 16)
        else:   
            max_ = addr + 256
        dump(self.machine.memory, addr, max_)

    def cmdloop(self):
        try:
            cmd.Cmd.cmdloop(self)
        except KeyboardInterrupt as e:
            self.cmdloop()


def dump(memory, start=0, max=None, width=16):
    if max is None:
        max = len(memory)
    for i, byte in enumerate(memory):
        if i < start:
            continue
        if i == max: break
        if (i % width) == 0:
            print '\n%04X\t' % i,
        print '%02X' % byte,
    print


if __name__ == '__main__':
    import sys
    m = z80sim.Machine()
    if sys.argv[1:]:
        data = open(sys.argv[1]).read()
        m.load_rom(sys.argv[1])
        m.start()
        print m.cpu
    else:
        cons = Console()
        cons.machine = m
        cons.cmdloop()
