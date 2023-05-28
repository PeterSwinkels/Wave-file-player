DEFINT A-Z

TYPE DTAStr
 Reserved AS STRING * 20
 Attribute AS INTEGER
 FileTime AS INTEGER
 FileDate AS INTEGER
 FileSize AS LONG
 FileName AS STRING * 13
END TYPE

TYPE RegTypeX
 ax AS INTEGER
 bx AS INTEGER
 cx AS INTEGER
 dx AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 flags AS INTEGER
 ds AS INTEGER
 es AS INTEGER
END TYPE

TYPE SBSettingsStr
 BaseAddress AS INTEGER
 DMAAddress AS INTEGER
 DMAChannel AS INTEGER
 DMALength AS INTEGER
 DMAPage AS INTEGER
END TYPE

TYPE WaveDataStr
 DataID AS STRING * 4
 DataSize AS LONG
END TYPE

TYPE WaveFactStr
 Padding AS STRING * 2
 FactID AS STRING * 4
 FactSize AS LONG
 FactDataSize AS LONG
END TYPE

TYPE WaveHeaderStr
 RIFFID AS STRING * 4
 RIFFSize AS LONG
 WAVEID AS STRING * 4
 FormatID AS STRING * 4
 FormatSize AS LONG
 Format AS INTEGER
 Channels AS INTEGER
 Frequency AS LONG
 ByteRate AS LONG
 Alignment AS INTEGER
 BitsPerSample AS INTEGER
END TYPE

TYPE WaveParametersStr
 EMSHandle AS INTEGER
 Frequency AS LONG
 Length AS LONG
END TYPE

TYPE WavePlayerStr
 ChunkLength AS LONG
 ChunkPageCount AS INTEGER
 CurrentPage AS INTEGER
 Paused AS INTEGER
 Remainder AS LONG
END TYPE

CONST EMSPAGESIZE = &H4000&

DECLARE FUNCTION EMSAllocatePages (PageCount AS INTEGER)
DECLARE FUNCTION EMSError$ (ErrorCode AS INTEGER)
DECLARE FUNCTION EMSFreeHandles ()
DECLARE FUNCTION EMSFreePages ()
DECLARE FUNCTION EMSInstalled ()
DECLARE FUNCTION EMSPageFrameAddress ()
DECLARE FUNCTION FileExists (FileName AS STRING)
DECLARE FUNCTION LoadWave (WaveFile AS STRING, WaveParameters AS WaveParametersStr, EMSPagesReserved AS INTEGER)
DECLARE FUNCTION SBBaseAddress ()
DECLARE FUNCTION SBBytesLeft& (SB AS SBSettingsStr)
DECLARE FUNCTION SBDMAChannel ()
DECLARE FUNCTION SBInitialize (SB AS SBSettingsStr)
DECLARE FUNCTION SBInUse (BytesLeft AS LONG)
DECLARE SUB Delay (Interval AS SINGLE)
DECLARE SUB DisplayVolume (x AS INTEGER, y AS INTEGER, LeftSide AS INTEGER, RightSide AS INTEGER)
DECLARE SUB DMASetChannel (SB AS SBSettingsStr, Enabled AS INTEGER)
DECLARE SUB EMSCopyBaseToEMS (Length AS LONG, SourceSegment AS INTEGER, SourceOffset AS INTEGER, DestinationHandle AS INTEGER, DestinationOffset AS INTEGER, DestinationPage AS INTEGER)
DECLARE SUB EMSDeallocatePages (Handle AS INTEGER)
DECLARE SUB EMSMapPages (PhysicalStart AS INTEGER, LogicalStart AS INTEGER, PageCount AS INTEGER, Handle AS INTEGER)
DECLARE SUB GetShiftStatus (LeftShift AS INTEGER, RightShift AS INTEGER)
DECLARE SUB Initialize ()
DECLARE SUB InterruptX (intnum AS INTEGER, inreg AS RegTypeX, outreg AS RegTypeX)
DECLARE SUB Main (WaveFile AS STRING)
DECLARE SUB Quit (Message AS STRING, WaveParameters AS WaveParametersStr)
DECLARE SUB SBGetVolume (SB AS SBSettingsStr, LeftSide AS INTEGER, RightSide AS INTEGER)
DECLARE SUB SBPause (SB AS SBSettingsStr)
DECLARE SUB SBPlayWave (WaveParameters AS WaveParametersStr, WavePlayer AS WavePlayerStr, SB AS SBSettingsStr)
DECLARE SUB SBResume (SB AS SBSettingsStr)
DECLARE SUB SBSetVolume (SB AS SBSettingsStr, LeftSide AS INTEGER, RightSide AS INTEGER)
DECLARE SUB SBSpeakerOff (SB AS SBSettingsStr)
DECLARE SUB SBSpeakerOn (SB AS SBSettingsStr)
DECLARE SUB SBWriteDSP (SB AS SBSettingsStr, Byte AS INTEGER)

DIM SHARED EMSErrorCode AS INTEGER

Initialize
Main COMMAND$

SUB Delay (Interval AS SINGLE)
DIM StartTime AS SINGLE

 StartTime = TIMER
 DO
 LOOP UNTIL TIMER >= StartTime + Interval OR TIMER <= Interval
END SUB

SUB DisplayVolume (x AS INTEGER, y AS INTEGER, LeftSide AS INTEGER, RightSide AS INTEGER)
 LOCATE x, y: PRINT "Volume:"
 LOCATE , y
 PRINT "  Left: ["; STRING$(LeftSide, "*"); SPACE$(&HF - LeftSide); "]"
 LOCATE , y
 PRINT " Right: ["; STRING$(RightSide, "*"); SPACE$(&HF - RightSide); "]"
END SUB

SUB DMASetChannel (SB AS SBSettingsStr, Enabled AS INTEGER)
 OUT &HA, (ABS(Enabled) * &H4) OR SB.DMAChannel
 OUT &HB, &H48 OR SB.DMAChannel
 OUT &HC, &H0
END SUB

FUNCTION EMSAllocatePages (PageCount AS INTEGER)
DIM Registers AS RegTypeX

 Registers.ax = &H4300
 Registers.bx = PageCount
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100

 EMSAllocatePages = Registers.dx
END FUNCTION

SUB EMSCopyBaseToEMS (Length AS LONG, SourceSegment AS INTEGER, SourceOffset AS INTEGER, DestinationHandle AS INTEGER, DestinationOffset AS INTEGER, DestinationPage AS INTEGER)
DIM CopyInformation AS STRING
DIM Registers AS RegTypeX

 CopyInformation = MKL$(Length) + CHR$(&H0) + MKI$(&H0) + MKI$(SourceOffset) + MKI$(SourceSegment) + CHR$(&H1) + MKI$(DestinationHandle) + MKI$(DestinationOffset) + MKI$(DestinationPage)

 Registers.ax = &H5700
 Registers.ds = VARSEG(CopyInformation)
 Registers.si = SADD(CopyInformation)
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

SUB EMSDeallocatePages (Handle AS INTEGER)
DIM Registers AS RegTypeX

 Registers.ax = &H4500
 Registers.dx = Handle
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

FUNCTION EMSError$ (ErrorCode AS INTEGER)
DIM Message AS STRING
DIM Registers AS RegTypeX

 SELECT CASE EMSErrorCode
  CASE &H0: Message = "No error."
  CASE &H80: Message = "Internal error."
  CASE &H81: Message = "Hardware malfunction."
  CASE &H82: Message = "Busy."
  CASE &H83: Message = "Invalid handle."
  CASE &H84: Message = "Undefined function requested by application."
  CASE &H85: Message = "No more handles available."
  CASE &H86: Message = "Error in save or restore of mapping context."
  CASE &H87: Message = "Insufficient memory pages in system."
  CASE &H88: Message = "Insufficient memory pages available."
  CASE &H89: Message = "Zero pages requested."
  CASE &H8A: Message = "Invalid logical page number encountered."
  CASE &H8B: Message = "Invalid physical page number encountered."
  CASE &H8C: Message = "Page-mapping hardware state save area is full."
  CASE &H8D: Message = "Save of mapping context failed."
  CASE &H8E: Message = "Restore of mapping context failed."
  CASE &H8F: Message = "Undefined subfunction."
  CASE &H90: Message = "Undefined attribute type."
  CASE &H91: Message = "Feature not supported."
  CASE &H92: Message = "Successful, but a portion of the source region has been overwritten."
  CASE &H93: Message = "Length of source or destination region exceeds length of region allocated to either source or destination handle."
  CASE &H94: Message = "Conventional and expanded memory regions overlap."
  CASE &H95: Message = "Offset within logical page exceeds size of logical page."
  CASE &H96: Message = "Region length exceeds 1 MB"
  CASE &H97: Message = "Source and destination EMS regions have same handle and overlap."
  CASE &H98: Message = "Memory source or destination type undefined."
  CASE &H9A: Message = "Specified alternate map register or DMA register set not supported."
  CASE &H9B: Message = "All alternate map register or DMA register sets currently allocated."
  CASE &H9C: Message = "Alternate map register or DMA register sets not supported."
  CASE &H9D: Message = "Undefined or unallocated alternate map register or DMA register set."
  CASE &H9E: Message = "Dedicated DMA channels not supported."
  CASE &H9F: Message = "Specified dedicated DMA channel not supported."
  CASE &HA0: Message = "No such handle name."
  CASE &HA1: Message = "A handle found had no name, or duplicate handle name."
  CASE &HA2: Message = "Attempted to wrap around 1 MB conventional address space."
  CASE &HA3: Message = "Source array corrupted."
  CASE &HA4: Message = "Operating system denied access."
  CASE ELSE: Message = "Undefined error: 0x" + HEX$(EMSErrorCode) + "."
 END SELECT

 EMSError$ = Message
END FUNCTION

FUNCTION EMSFreeHandles
DIM Registers AS RegTypeX
DIM TotalHandles AS INTEGER
DIM UsedHandles AS INTEGER

 Registers.ax = &H4B00
 InterruptX &H67, Registers, Registers
 UsedHandles = Registers.bx

 Registers.ax = &H5402
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
 TotalHandles = Registers.bx

 EMSFreeHandles = TotalHandles - UsedHandles
END FUNCTION

FUNCTION EMSFreePages
DIM Registers AS RegTypeX

 Registers.ax = &H4200
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
 EMSFreePages = Registers.bx
END FUNCTION

FUNCTION EMSInstalled
DIM EMM AS STRING
DIM Position AS INTEGER
DIM Registers AS RegTypeX

 Registers.ax = &H3567
 InterruptX &H21, Registers, Registers

 DEF SEG = Registers.es
 FOR Position = &HA TO &H11
  EMM = EMM + CHR$(PEEK(Position))
 NEXT Position

 EMSInstalled = (EMM = "EMMXXXX0")
END FUNCTION

SUB EMSMapPages (PhysicalStart AS INTEGER, LogicalStart AS INTEGER, PageCount AS INTEGER, Handle AS INTEGER)
DIM MapInformation AS STRING
DIM Page AS INTEGER
DIM Registers AS RegTypeX

 FOR Page = 0 TO PageCount - 1
  MapInformation = MapInformation + MKI$(LogicalStart + Page) + MKI$(PhysicalStart + Page)
 NEXT Page

 Registers.ax = &H5000
 Registers.cx = PageCount
 Registers.dx = Handle
 Registers.ds = VARSEG(MapInformation)
 Registers.si = SADD(MapInformation)
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

FUNCTION EMSPageFrameAddress
DIM Registers AS RegTypeX

 Registers.ax = &H4100
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
 EMSPageFrameAddress = Registers.bx
END FUNCTION

FUNCTION FileExists (FileName AS STRING)
DIM Attributes AS INTEGER
DIM DTA AS DTAStr
DIM Pattern AS STRING
DIM Registers AS RegTypeX

 Registers.ax = &H1A00
 Registers.dx = VARPTR(DTA)
 Registers.ds = VARSEG(DTA)
 InterruptX &H21, Registers, Registers

 Attributes = 0
 Pattern = FileName + CHR$(0)

 Registers.ax = &H4E00
 Registers.cx = Attributes
 Registers.dx = SADD(Pattern)
 Registers.ds = VARSEG(Pattern)
 InterruptX &H21, Registers, Registers

 FileExists = (INSTR(DTA.FileName, CHR$(0)) > 1)
END FUNCTION

SUB GetShiftStatus (LeftShift AS INTEGER, RightShift AS INTEGER)
DIM Status AS INTEGER

 DEF SEG = &H40
 Status = PEEK(&H17)
 RightShift = ((Status AND &H1) = &H1)
 LeftShift = ((Status AND &H2) = &H2)
END SUB

SUB Initialize
 SCREEN 0
 WIDTH 80, 25
 PALETTE
 COLOR 7, 0
 CLS

 EMSErrorCode = &H0
END SUB

FUNCTION LoadWave (WaveFile AS STRING, WaveParameters AS WaveParametersStr, EMSPagesReserved AS INTEGER)
DIM Buffer AS STRING
DIM DataRead AS LONG
DIM FileH AS INTEGER
DIM MaximumLength AS LONG
DIM Offset AS INTEGER
DIM Page AS INTEGER
DIM Success AS INTEGER
DIM WaveData AS WaveDataStr
DIM WaveFact AS WaveFactStr
DIM WaveHeader AS WaveHeaderStr

 Success = -1

 FileH = FREEFILE
 OPEN WaveFile FOR BINARY LOCK READ WRITE AS FileH
  GET #FileH, , WaveHeader
  IF WaveHeader.FormatSize = 18 THEN GET #FileH, , WaveFact
  GET #FileH, , WaveData

  IF WaveHeader.RIFFID = "RIFF" AND WaveHeader.WAVEID = "WAVE" AND WaveHeader.FormatID = "fmt " AND WaveHeader.Format = 1 AND WaveHeader.Channels = 1 AND WaveHeader.Frequency <= 32000 AND WaveHeader.Alignment = 1 AND WaveData.DataID = "data" THEN
   WaveParameters.Frequency = WaveHeader.Frequency
   WaveParameters.Length = LOF(FileH) - LEN(WaveHeader)
   MaximumLength = EMSPagesReserved * EMSPAGESIZE
   IF WaveParameters.Length > MaximumLength THEN WaveParameters.Length = MaximumLength

   Buffer = STRING$(&H7FFF, &H0)
   DataRead = 0
 
   DO
    IF DataRead > WaveParameters.Length THEN Buffer = STRING$(WaveParameters.Length - DataRead, &H0)

    GET #FileH, , Buffer

    Page = DataRead \ EMSPAGESIZE
    Offset = DataRead - Page * EMSPAGESIZE
   
    EMSCopyBaseToEMS LEN(Buffer), VARSEG(Buffer), SADD(Buffer), WaveParameters.EMSHandle, Offset, Page
    DataRead = DataRead + LEN(Buffer)
   LOOP UNTIL DataRead >= WaveParameters.Length
   Buffer = ""
  ELSE
   Success = 0
  END IF
 CLOSE FileH

 LoadWave = Success
END FUNCTION

SUB Main (WaveFile AS STRING)
DIM BytesLeft AS LONG
DIM EMSPagesToReserve AS INTEGER
DIM KeyStroke AS STRING
DIM LeftShift AS INTEGER
DIM LeftSide AS INTEGER
DIM SB AS SBSettingsStr
DIM RightShift AS INTEGER
DIM RightSide AS INTEGER
DIM WaveParameters AS WaveParametersStr
DIM WavePlayer AS WavePlayerStr

 WaveFile = LTRIM$(RTRIM$(WaveFile))
 WavePlayer.Paused = 0
 SB.BaseAddress = SBBaseAddress
 SB.DMAChannel = SBDMAChannel
 SBSetVolume SB, &HF, &HF

 LOCATE 1, 1: PRINT "Wave Player v1.02 - by: Peter Swinkels ***2021***"
 PRINT

 IF WaveFile = "" THEN
  Quit "Specify the wave file to be played as the command line argument.", WaveParameters
 ELSE
  IF FileExists(WaveFile) THEN
   IF SBInitialize(SB) THEN
    IF EMSInstalled THEN
     EMSPagesToReserve = EMSFreePages
     IF EMSPagesToReserve > 0 THEN
      WaveParameters.EMSHandle = EMSAllocatePages(EMSPagesToReserve)
      IF LoadWave(WaveFile, WaveParameters, EMSPagesToReserve) THEN
       LOCATE 3, 1
       PRINT " Playing: "; WaveFile
       PRINT
       PRINT " Base address: 0x"; HEX$(SB.BaseAddress)
       PRINT " DMA channel: 0x"; HEX$(SB.DMAChannel)
       SBGetVolume SB, LeftSide, RightSide
       DisplayVolume 10, 2, LeftSide, RightSide
       
       LOCATE 23, 1
       PRINT " Escape = Quit  P = Pause/Resume  -/+ [+Left/Right Shift] = Volume "

       SBSpeakerOn SB
  
       WavePlayer.CurrentPage = 0
       WavePlayer.Remainder = WaveParameters.Length

       DO
        IF WavePlayer.Remainder = &H0& THEN
         EXIT DO
        ELSEIF WavePlayer.Remainder > &HFFFF& THEN
         WavePlayer.ChunkLength = &HFFFF&
        ELSE
         WavePlayer.ChunkLength = WavePlayer.Remainder
        END IF
   
        WavePlayer.ChunkPageCount = (WavePlayer.ChunkLength \ EMSPAGESIZE) + 1
        SBPlayWave WaveParameters, WavePlayer, SB
       
        DO
         BytesLeft = SBBytesLeft&(SB)
         LOCATE 8, 3: PRINT "Data remaining: "; LTRIM$(RTRIM$(STR$(WavePlayer.Remainder - (EMSPAGESIZE - BytesLeft)))); "/"; LTRIM$(RTRIM$(STR$(WaveParameters.Length))); " bytes."; SPACE$(5)

         KeyStroke = INKEY$
         SELECT CASE KeyStroke
          CASE "p", "P"
           WavePlayer.Paused = NOT WavePlayer.Paused
           IF WavePlayer.Paused THEN SBPause SB ELSE SBResume SB
          CASE "-", "+"
           GetShiftStatus LeftShift, RightShift
           IF NOT (LeftShift OR RightShift) THEN LeftShift = -1: RightShift = -1

           SBGetVolume SB, LeftSide, RightSide
           
           SELECT CASE KeyStroke
            CASE "-"
             IF LeftShift AND LeftSide > &H0 THEN LeftSide = LeftSide - &H1
             IF RightShift AND RightSide > &H0 THEN RightSide = RightSide - &H1
            CASE "+"
             IF LeftShift AND LeftSide < &HF THEN LeftSide = LeftSide + &H1
             IF RightShift AND RightSide < &HF THEN RightSide = RightSide + &H1
           END SELECT

           SBSetVolume SB, LeftSide, RightSide
           DisplayVolume 10, 2, LeftSide, RightSide
          CASE CHR$(27)
           EXIT DO
         END SELECT
        LOOP WHILE SBInUse(BytesLeft)
    
        WavePlayer.CurrentPage = WavePlayer.CurrentPage + WavePlayer.ChunkPageCount
        WavePlayer.Remainder = WavePlayer.Remainder - WavePlayer.ChunkLength
       LOOP UNTIL KeyStroke = CHR$(27)
    
       SBSpeakerOff SB
       Quit "", WaveParameters
      ELSE
       Quit WaveFile + " - unsupported WAVE file format.", WaveParameters
      END IF
     ELSE
      Quit "Not enough free EMS pages.", WaveParameters
     END IF
    ELSE
     Quit "No EMS driver detected.", WaveParameters
    END IF
   ELSE
    Quit "Could not initialize the Sound Blaster.", WaveParameters
   END IF
  ELSE
   Quit "Could not open the file: " + WaveFile + ".", WaveParameters
  END IF
 END IF
END SUB

SUB Quit (Message AS STRING, WaveParameters AS WaveParametersStr)
 IF (NOT EMSErrorCode = &H0) OR (NOT Message = "") THEN
  IF NOT Message = "" THEN PRINT Message
  IF NOT EMSErrorCode = &H0 THEN
   PRINT "EMS Error code: 0x"; HEX$(EMSErrorCode); ""
   PRINT EMSError$(EMSErrorCode)
  END IF
  DO WHILE INKEY$ = ""
  LOOP
 END IF

 EMSDeallocatePages WaveParameters.EMSHandle

 SCREEN 0
 WIDTH 80, 25
 PALETTE
 COLOR 7, 0
 CLS
 SYSTEM
END SUB

FUNCTION SBBaseAddress
DIM BaseAddress AS INTEGER
DIM Position1 AS INTEGER
DIM Position2 AS INTEGER
DIM Settings AS STRING

 BaseAddress = &H220

 Settings = LTRIM$(RTRIM$(UCASE$(ENVIRON$("BLASTER"))))

 Position1 = INSTR(Settings, "A")
 IF Position1 > 0 THEN
  Position2 = INSTR(Position1, Settings, " ")
  BaseAddress = VAL("&H" + MID$(Settings, Position1 + 1, (Position2 - Position1)))
 END IF

 SBBaseAddress = BaseAddress
END FUNCTION

FUNCTION SBBytesLeft& (SB AS SBSettingsStr)
 SBBytesLeft& = INP(SB.DMALength) OR INP(SB.DMALength) * &H100&
END FUNCTION

FUNCTION SBDMAChannel
DIM DMAChannel AS INTEGER
DIM Position1 AS INTEGER
DIM Position2 AS INTEGER
DIM Settings AS STRING

 DMAChannel = &H1

 Settings = LTRIM$(RTRIM$(UCASE$(ENVIRON$("BLASTER"))))

 Position1 = INSTR(Settings, "D")
 IF Position1 > 0 THEN
  Position2 = INSTR(Position1, Settings, " ")
  DMAChannel = VAL("&H" + MID$(Settings, Position1 + 1, (Position2 - Position1)))
 END IF

 SBDMAChannel = DMAChannel
END FUNCTION

SUB SBGetVolume (SB AS SBSettingsStr, LeftSide AS INTEGER, RightSide AS INTEGER)
DIM Volume AS INTEGER

 OUT SB.BaseAddress + &H4, &H22
 Volume = INP(SB.BaseAddress + &H5)
 LeftSide = Volume \ &H10
 RightSide = Volume AND &HF
END SUB

FUNCTION SBInitialize (SB AS SBSettingsStr)
DIM Success AS INTEGER

 Success = -1

 OUT SB.BaseAddress + &H6, &H1
 WAIT SB.BaseAddress + &H6, &H80
 OUT SB.BaseAddress + &H6, &H0

 Delay .03

 Success = ((INP(SB.BaseAddress + &HE) AND &H80) = &H80) AND (INP(SB.BaseAddress + &HA) = &HAA)

 IF Success THEN
  SB.DMAAddress = SB.DMAChannel + &H1
  SB.DMALength = SB.DMAChannel + &H2

  SELECT CASE SB.DMAChannel
   CASE 0
    SB.DMAPage = &H87
   CASE 1
    SB.DMAPage = &H83
   CASE 2
    SB.DMAPage = &H81
   CASE 3
    SB.DMAPage = &H82
   CASE ELSE
    Success = 0
  END SELECT
 END IF

 SBInitialize = Success
END FUNCTION

FUNCTION SBInUse (BytesLeft AS LONG)
 SBInUse = NOT (BytesLeft = &H0 OR BytesLeft = &HFFFF&)
END FUNCTION

SUB SBPause (SB AS SBSettingsStr)
 SBWriteDSP SB, &HD0
END SUB

SUB SBPlayWave (WaveParameters AS WaveParametersStr, WavePlayer AS WavePlayerStr, SB AS SBSettingsStr)
DIM EMSPageFrameFlatAddress AS LONG

 EMSMapPages 0, WavePlayer.CurrentPage, WavePlayer.ChunkPageCount, WaveParameters.EMSHandle

 DMASetChannel SB, -1

 EMSPageFrameFlatAddress = (&H10000 + EMSPageFrameAddress) * &H10&

 OUT SB.DMAAddress, EMSPageFrameFlatAddress AND &HFF&
 OUT SB.DMAAddress, (EMSPageFrameFlatAddress AND &HFF00&) \ &H100&

 OUT SB.DMAPage, (&H10000 + EMSPageFrameAddress) / &H1000&

 OUT SB.DMALength, (WavePlayer.ChunkLength - &H1&) AND &HFF&
 OUT SB.DMALength, ((WavePlayer.ChunkLength - &H1&) AND &HFF00&) \ &H100&

 DMASetChannel SB, 0

 SBWriteDSP SB, &H40
 SBWriteDSP SB, ((&H100& - &HF4240) \ WaveParameters.Frequency)

 SBWriteDSP SB, &H14
 SBWriteDSP SB, ((WavePlayer.ChunkLength - &H1&) AND &HFF&)
 SBWriteDSP SB, (((WavePlayer.ChunkLength - &H1&) AND &HFF00&) \ &H100&)
END SUB

SUB SBResume (SB AS SBSettingsStr)
 SBWriteDSP SB, &HD4
END SUB

SUB SBSetVolume (SB AS SBSettingsStr, LeftSide, RightSide)
 OUT SB.BaseAddress + &H4, &H22
 OUT SB.BaseAddress + &H5, (LeftSide * &H10 OR RightSide)
END SUB

SUB SBSpeakerOff (SB AS SBSettingsStr)
 SBWriteDSP SB, &HD3
END SUB

SUB SBSpeakerOn (SB AS SBSettingsStr)
 SBWriteDSP SB, &HD1
END SUB

SUB SBWriteDSP (SB AS SBSettingsStr, Byte)
 WAIT SB.BaseAddress + &HC, &H80, &H80
 OUT SB.BaseAddress + &HC, Byte
END SUB

