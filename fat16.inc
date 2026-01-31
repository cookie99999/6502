  ; mbr offsets
  FAT_MBR_P1 = $01be ; partition table entries
  FAT_MBR_P2 = $01ce
  FAT_MBR_P3 = $01de
  FAT_MBR_P4 = $01ee
  FAT_MBR_SIG = $01fe ; 0x55aa signature

  ; partition table entries
  FAT_PART_ATTR = $00 ; $80 = active/bootable
  FAT_PART_START_C = $01
  FAT_PART_START_H = $02
  FAT_PART_START_S = $03
  FAT_PART_TYPE = $04
  FAT_PART_END_C = $05
  FAT_PART_END_H = $06
  FAT_PART_END_S = $07
  FAT_PART_START_LBA = $08 ; 4 bytes
  FAT_PART_NUM_SECTORS = $0c ; 4 bytes

  ; boot parameter block/vbr offsets
  FAT_BPB_OEM_ID = $03 ; 8 bytes
  FAT_BPB_BYTES_PER_SECTOR = $0b ; 2 bytes
  FAT_BPB_SECTORS_PER_CLUSTER = $0d
  FAT_BPB_RES_SECTORS = $0e ; 2 bytes, num reserved sectors
  FAT_BPB_NUM_FATS = $10
  FAT_BPB_NUM_ROOT_ENTRIES = $11 ; 2 bytes
  FAT_BPB_TOTAL_SECTORS = $13 ; 2 bytes, 0 = >ffff sectors, see $20 for real count
  FAT_BPB_DESC_TYPE = $15 ; media descriptor type
  FAT_BPB_SECTORS_PER_FAT = $16 ; 2 bytes
  FAT_BPB_SECTORS_PER_TRACK = $18 ; 2 bytes
  FAT_BPB_NUM_HEADS = $1a ; 2 bytes
  FAT_BPB_HIDDEN_SECTORS = $1c ; 4 bytes
  FAT_BPB_LARGE_SECTOR_COUNT = $20 ; 4 bytes
  ; EBR
  FAT_BPB_DRIVE_NUM = $24
  FAT_BPB_NT_FLAGS = $25
  FAT_BPB_SIG = $26 ; should be $28 or $29
  FAT_BPB_VOL_SERIAL = $27 ; 4 bytes
  FAT_BPB_VOL_LABEL = $2b ; 11 bytes, space padded
  FAT_BPB_SYS_ID = $36 ; 8 bytes, space padded
  FAT_BPB_CODE = $3e ; 448 bytes
  FAT_BPB_PART_SIG = $01fe ; $aa55 signature

  ; directory entry offsets
  FAT_DIR_NAME = $00 ; 8.3 space padded
  FAT_DIR_ATTR = $0b
  FAT_DIR_NT_CASE = $0c ; bit 3 set = basename lowercase bit 4 set = ext lowercase
  FAT_DIR_CTIME_CSECS = $0d ; mkfs usually sets 0 or 100, windows uses actual centiseconds
  FAT_DIR_CTIME = $0e ; h 5 bits m 6 bits s/2 5 bits
  FAT_DIR_CTIME_DATE = $10 ; y 7 bits m 4 bits d 5 bits
  FAT_DIR_ATIME_DATE = $12 ; same format as above
  FAT_DIR_CLUSTER_HIGH = $14 ; always 0 on fat12/16
  FAT_DIR_MTIME = $16 ; same format as ctime
  FAT_DIR_MTIME_DATE = $18 ; "
  FAT_DIR_CLUSTER = $1a ; number of first cluster
  FAT_DIR_SIZE = $1c ; size in bytes

  ; long file name entries
  FAT_LFN_INDEX = $00 ; bit 7 = deleted, bit 6 = last lfn, 5-0 = index
  FAT_LFN_DATA_1 = $01 ; first 5 long chars
  FAT_LFN_ATTR = $0b ; always 0f for lfns
  FAT_LFN_TYPE = $0c ; zero for name entries
  FAT_LFN_CSUM = $0d
  FAT_LFN_DATA_2 = $0e ; next 6 long chars
  FAT_LFN_ZERO = $1a ; always zero
  FAT_LFN_DATA_3 = $1c ; last 2 long chars
