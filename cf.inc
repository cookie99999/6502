  ; registers
  CF = $df40
  CF_DATA = CF+0
  CF_ERR = CF+1
  CF_FEATURE = CF+1
  CF_SEC_COUNT = CF+2
  CF_SECTOR_NUM = CF+3
  CF_LBA_7_0 = CF+3
  CF_CYL_LOW = CF+4
  CF_LBA_15_8 = CF+4
  CF_CYL_HIGH = CF+5
  CF_LBA_23_16 = CF+5
  CF_HEAD = CF+6
  CF_LBA_27_24 = CF+6
  CF_STAT = CF+7
  CF_CMD = CF+7

  ; status flags
  CF_STAT_ERR = $01
  CF_STAT_CORR = $04
  CF_STAT_DRQ = $08
  CF_STAT_DSC = $10
  CF_STAT_DWF = $20
  CF_STAT_RDY = $40
  CF_STAT_BSY = $80

  ; ata commands
  CF_INIT_PARAMS = $91
  CF_IDENTIFY = $ec
  CF_READ_SEC = $20
  CF_WRITE_SEC = $30
  CF_SET_FEATURE = $ef
