#!/usr/bin/python
from __future__ import print_function
from __future__ import unicode_literals
import struct
import sys

import serial

class UartIf(object):
    def read_reply(self, exp_c, exp_d = None):
        r = self.ser.read(3)
        if len(r) != 3:
            print('Connection dropped!', file=sys.stderr)
            sys.exit(1)
        c, d = struct.unpack(b'<BH', r)
        if c != exp_c or (exp_d is not None and d != exp_d):
            print(('Response error: expected {}:{}, got {}:{}'
                  ).format(0, data, c, d), file=sys.stderr)
            sys.exit(1)
        return d

    def lt24_reset(self, data = 0):
        d = struct.pack('<BH', 0, data)
        print(repr(d))
        self.ser.write(d)
        self.read_reply(0, data)

    def lt24_command(self, data):
        d = struct.pack('<BH', 1, data)
        print(repr(d))
        self.ser.write(d)
        self.read_reply(1, data)

    def lt24_write(self, data):
        d = struct.pack('<BH', 2, data)
        print(repr(d))
        self.ser.write(d)
        self.read_reply(2, data)

    def lt24_read_fm(self, data = 0):
        d = struct.pack('<BH', 3, data)
        print(repr(d))
        self.ser.write(d)
        d = self.read_reply(3)
        return d

    def lt24_read_id(self, data = 0):
        d = struct.pack('<BH', 4, data)
        print(repr(d))
        self.ser.write(d)
        d = self.read_reply(4)
        return d

    def run(self):
        self.ser = serial.Serial('/dev/ttyUSB0', 115200)
        
