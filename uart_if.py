#!/usr/bin/python
from __future__ import print_function
from __future__ import unicode_literals
import struct
import sys
import codecs

import serial

class UartIf(object):

    RDDMADCTL = 0x0B # 8.2.6 Read Display MADCTL (page 95)
    SLPOUT    = 0x11 # 8.2.12 Sleep Out (page 101)
    DISPON    = 0x29 # 8.2.19 Display ON (page 109)
    RAMWR     = 0x2C # 8.2.22 Memory Write (page 114)
    RGBSET    = 0x2D # 8.2.23 Color Set (page 115)
    RAMRD     = 0x2E # 8.2.24 Memory Read (page 116)
    MADCTL    = 0x36 # 8.2.29 Memory Access Control (page 127)
    COLMOD    = 0x3A # 8.2.33 Pixel Format Set (page 134)

    def send_command(self, c, d):
        s = struct.pack('<BH', c, d)
        print('> {}'.format(codecs.encode(s, 'hex_codec')))
        self.ser.write(s)

    def read_reply(self, exp_c, exp_d = None):
        r = self.ser.read(3)
        if len(r) != 3:
            print('Connection dropped!', file=sys.stderr)
            raise Exception()
        print('< {}'.format(codecs.encode(r, 'hex_codec')))
        c, d = struct.unpack(b'<BH', r)
        if c != exp_c or (exp_d is not None and d != exp_d):
            print(('Response error: expected {}:{}, got {}:{}'
                  ).format(exp_c, exp_d, c, d), file=sys.stderr)
            raise Exception()
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

    def trivial_palette(self):
        self.lt24_command(self.RGBSET)
        for r in range(31):
            self.lt24_write(r * 2)
        for g in range(63):
            self.lt24_write(g)
        for b in range(31):
            self.lt24_write(b * 2)

    def init_lt24(self):
        self.lt24_reset()
        self.lt24_command(self.MADCTL)
        self.lt24_write(0b1000) # BGR subpixel order
        self.lt24_command(self.COLMOD)
        self.lt24_write(0b101) # 16bpp MCU interface
        self.trivial_palette()
        self.lt24_command(self.SLPOUT)
        self.lt24_command(self.DISPON)

    def gradient(self, color, length):
        if color == 0:
            # Red
            shift = 11
            add = 0
        elif color == 1:
            # Green
            shift = 6
            add = 1
        else:
            # Blue
            shift = 0
            add = 0
        for brightness in range(add, length + add):
            pixel = brightness << shift
            self.lt24_write(pixel)

    def colorbars(self):
        self.lt24_command(self.RAMWR)
        color = 0
        for colblock in range(3):
            for coloffset in range(3):
                for elementwidth in range(32):
                    for rowblock in range(2):
                        for element in range(3):
                            color = (rowblock + element + coloffset) % 3
                            self.gradient(color, 32)
                    color = (rowblock + element + coloffset + 1) % 3
                    self.gradient(color, 32)
                    color = (rowblock + element + coloffset + 2) % 3
                    self.gradient(color, 16)
        for elementwidth in range(32):
            for rowblock in range(2):
                for element in range(3):
                    color = (rowblock + element + coloffset + 2) % 3
                    self.gradient(color, 32)
                    color = (color + 1) % 3
            self.gradient(color, 32)
            color = (color + 1) % 3
            self.gradient(color, 16)

