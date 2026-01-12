  ; port b masks
  CF_WR = $01
  CF_RD = $02
  CF_A0 = $04
  CF_A1 = $08
  CF_A2 = $10
  CF_CS0 = $20
  CF_CS1 = $40

  ; registers
  CF_DATA = 0
  CF_ERR = 1
  CF_FEATURE = 1
  CF_SEC_COUNT = 2
  CF_SECTOR_NUM = 3
  CF_LBA_7_0 = 3
  CF_CYL_LOW = 4
  CF_LBA_15_8 = 4
  CF_CYL_HIGH = 5
  CF_LBA_23_16 = 5
  CF_HEAD = 6
  CF_LBA_27_24 = 6
  CF_STAT = 7
  CF_CMD = 7

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
