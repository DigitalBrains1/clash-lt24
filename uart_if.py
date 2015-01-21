#!/usr/bin/python
from __future__ import print_function
from __future__ import unicode_literals
import struct
import sys
import codecs

import serial

class UartIf(object):
    def send_command(self, c, d):
        s = struct.pack('<BH', c, d)
        print('> {}'.format(codecs.encode(s, 'hex_codec')))
        self.ser.write(s)
        
    def read_reply(self, exp_c, exp_d = None):
        r = self.ser.read(3)
        if len(r) != 3:
            print('Connection dropped!', file=sys.stderr)
            sys.exit(1)
        print('< {}'.format(codecs.encode(r, 'hex_codec')))
        c, d = struct.unpack(b'<BH', r)
        if c != exp_c or (exp_d is not None and d != exp_d):
            print(('Response error: expected {}:{}, got {}:{}'
                  ).format(0, data, c, d), file=sys.stderr)
            sys.exit(1)
        return d

    def lt24_reset(self, data = 0):
        self.send_command(0, data)
        self.read_reply(0, data)

    def lt24_command(self, data):
        self.send_command(1, data)
        self.read_reply(1, data)

    def lt24_write(self, data):
        self.send_command(2, data)
        self.read_reply(2, data)

    def lt24_read_fm(self, data = 0):
        self.send_command(3, data)
        d = self.read_reply(3)
        return d

    def lt24_read_id(self, data = 0):
        self.send_command(4, data)
        d = self.read_reply(4)
        return d

    def run(self):
        self.ser = serial.Serial('/dev/ttyUSB0', 115200)
        
