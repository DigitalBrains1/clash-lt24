{-
 - Copyright (c) 2015, Peter Lebbing <peter@digitalbrains.com>
 - All rights reserved.
 - 
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 - 
 - 1. Redistributions of source code must retain the above copyright notice,
 - this list of conditions and the following disclaimer.
 - 
 - 2. Redistributions in binary form must reproduce the above copyright notice,
 - this list of conditions and the following disclaimer in the documentation
 - and/or other materials provided with the distribution.
 - 
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 - AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 - IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 - ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 - LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 - CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 - SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 - INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 - CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 - ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 - POSSIBILITY OF SUCH DAMAGE.
 -}

module LT24.Commands where

cRDDMADCTL :: Num a => a
cSLPOUT :: Num a => a
cDISPON :: Num a => a
cCASET :: Num a => a
cPASET :: Num a => a
cRAMWR :: Num a => a
cRGBSET :: Num a => a
cRAMRD :: Num a => a
cMADCTL :: Num a => a
cCOLMOD :: Num a => a
cWrite_Memory_Continue :: Num a => a
cRead_Memory_Continue :: Num a => a

cRDDMADCTL             = 0x0B -- 8.2.6 Read Display MADCTL (page 95)
cSLPOUT                = 0x11 -- 8.2.12 Sleep Out (page 101)
cDISPON                = 0x29 -- 8.2.19 Display ON (page 109)
cCASET                 = 0x2A -- 8.2.20 Column Address Set (page 110)
cPASET                 = 0x2B -- 8.2.21 Page Address Set (page 112)
cRAMWR                 = 0x2C -- 8.2.22 Memory Write (page 114)
cRGBSET                = 0x2D -- 8.2.23 Colour Set (page 115)
cRAMRD                 = 0x2E -- 8.2.24 Memory Read (page 116)
cMADCTL                = 0x36 -- 8.2.29 Memory Access Control (page 127)
cCOLMOD                = 0x3A -- 8.2.33 Pixel Format Set (page 134)
cWrite_Memory_Continue = 0x3C -- 8.2.34 Write_Memory_Continue (page 135)
cRead_Memory_Continue  = 0x3E -- 8.2.35 Read_Memory_Continue (page 137)
