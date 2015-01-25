module LT24.Commands where

cRDDMADCTL :: Num a => a
cSLPOUT :: Num a => a
cDISPON :: Num a => a
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
cRAMWR                 = 0x2C -- 8.2.22 Memory Write (page 114)
cRGBSET                = 0x2D -- 8.2.23 Color Set (page 115)
cRAMRD                 = 0x2E -- 8.2.24 Memory Read (page 116)
cMADCTL                = 0x36 -- 8.2.29 Memory Access Control (page 127)
cCOLMOD                = 0x3A -- 8.2.33 Pixel Format Set (page 134)
cWrite_Memory_Continue = 0x3C -- 8.2.34 Write_Memory_Continue (page 135)
cRead_Memory_Continue  = 0x3E -- 8.2.35 Read_Memory_Continue (page 137)
