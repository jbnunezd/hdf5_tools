!==================================================================================================!
#include "main.h"
!==================================================================================================!
!
!==================================================================================================!
MODULE MOD_MAIN_vars
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER            :: SCREEN_WIDTH
INTEGER,PARAMETER  :: UNIT_SCREEN = 6
!--------------------------------------------------------------------------------------------------!
INTERFACE InitializeMain
  MODULE PROCEDURE InitializeMain
END INTERFACE

INTERFACE PrintError
  MODULE PROCEDURE PrintError
END INTERFACE

INTERFACE PrintHeader
  MODULE PROCEDURE PrintHeader
END INTERFACE

INTERFACE CreateSeparatingLine
  MODULE PROCEDURE CreateSeparatingLine
END INTERFACE

INTERFACE HighlightText
  MODULE PROCEDURE HighlightText
END INTERFACE
!--------------------------------------------------------------------------------------------------!
!
!
!
!==================================================================================================!
CONTAINS
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE InitializeMain()
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!

CALL TerminalWidth(SCREEN_WIDTH)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMain
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE TerminalWidth(Width)
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER,INTENT(OUT) :: Width
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER             :: UNIT_FILE
CHARACTER(LEN=256)  :: PrintLine
CHARACTER(LEN=256)  :: temp
!--------------------------------------------------------------------------------------------------!

PrintLine = "printf " // "%`tput cols`s" // "|tr ' ' '='" // " > TerminalWidth.txt "
CALL EXECUTE_COMMAND_LINE(PrintLine)

OPEN(NEWUNIT=UNIT_FILE,FILE='TerminalWidth.txt',STATUS='OLD',ACTION='READ')
READ(UNIT_FILE,*) temp
CLOSE(UNIT_FILE,STATUS="DELETE")
Width = LEN(TRIM(temp))

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE TerminalWidth
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE CreateSeparatingLine(Symbol,SeparatingLine)
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
CHARACTER(LEN=1),INTENT(IN)               :: Symbol
CHARACTER(LEN=:),ALLOCATABLE,INTENT(OUT)  :: SeparatingLine
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER :: ii
!--------------------------------------------------------------------------------------------------!

SeparatingLine = ""
DO ii=1,SCREEN_WIDTH
  SeparatingLine = SeparatingLine//TRIM(Symbol)
END DO

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateSeparatingLine
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE PrintError(SourceFile,SourceLine,CompDate,CompTime,ErrorMessage)
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER,         INTENT(IN) :: SourceLine
CHARACTER(LEN=*),INTENT(IN) :: SourceFile
CHARACTER(LEN=*),INTENT(IN) :: CompDate
CHARACTER(LEN=*),INTENT(IN) :: CompTime
CHARACTER(LEN=*),INTENT(IN) :: ErrorMessage
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
CHARACTER(LEN=1)             :: Sep
CHARACTER(LEN=:),ALLOCATABLE :: StrLine1
CHARACTER(LEN=:),ALLOCATABLE :: StrLine2
! CHARACTER(LEN=:),ALLOCATABLE :: StrKey
! CHARACTER(LEN=:),ALLOCATABLE :: StrMessage
CHARACTER(LEN=256) :: StrKey
CHARACTER(LEN=256) :: StrMessage
!--------------------------------------------------------------------------------------------------!

Sep = ":"

CALL CreateSeparatingLine("=",StrLine1)
CALL CreateSeparatingLine("-",StrLine2)
StrLine1 = HighlightText(StrLine1,"bold","yellow")
StrLine2 = HighlightText(StrLine2,"bold","yellow")

WRITE(UNIT_SCREEN,*)
WRITE(UNIT_SCREEN,*)
WRITE(UNIT_SCREEN,"(A)") TRIM(StrLine1)
!--------------------------------------------------------------------------------------------------!
StrMessage = "FATAL ERROR"
StrMessage = HighlightText(StrMessage,"bold","yellow")
WRITE(UNIT_SCREEN,'(1X,(A))',ADVANCE='YES') TRIM(StrMessage)
!--------------------------------------------------------------------------------------------------!
WRITE(UNIT_SCREEN,"(A)") TRIM(StrLine2)
!--------------------------------------------------------------------------------------------------!
StrKey = "Error Message"
StrKey = HighlightText(StrKey,"bold","yellow")
StrMessage = HighlightText(ErrorMessage,"normal","white")
WRITE(UNIT_SCREEN,'(1X,*(A,T33))',ADVANCE='NO') TRIM(StrKey)
WRITE(UNIT_SCREEN,'(1X,A1)',ADVANCE='NO') TRIM(Sep)
WRITE(UNIT_SCREEN,'(1X,*(A,T38))',ADVANCE='YES') TRIM(StrMessage)
!--------------------------------------------------------------------------------------------------!
WRITE(UNIT_SCREEN,"(A)") TRIM(StrLine2)
!--------------------------------------------------------------------------------------------------!
StrKey = "File Name"
StrKey = HighlightText(StrKey,"bold","yellow")
StrMessage = HighlightText(SourceFile,"normal","white")
WRITE(UNIT_SCREEN,'(1X,*(A,T33))',ADVANCE='NO') TRIM(StrKey)
WRITE(UNIT_SCREEN,'(1X,A1)',ADVANCE='NO') TRIM(Sep)
WRITE(UNIT_SCREEN,'(1X,*(A,T38))',ADVANCE='YES') TRIM(StrMessage)
!--------------------------------------------------------------------------------------------------!
StrKey = "Line Number"
StrKey = HighlightText(StrKey,"bold","yellow")
WRITE(StrMessage,"(I5)") SourceLine
StrMessage = ADJUSTL(TRIM(StrMessage))
StrMessage = HighlightText(StrMessage,"normal","white")
WRITE(UNIT_SCREEN,'(1X,*(A,T33))',ADVANCE='NO') TRIM(StrKey)
WRITE(UNIT_SCREEN,'(1X,A1)',ADVANCE='NO') TRIM(Sep)
WRITE(UNIT_SCREEN,'(1X,*(A,T38))',ADVANCE='YES') TRIM(StrMessage)
!--------------------------------------------------------------------------------------------------!
StrKey = "Compilation Date"
StrKey = HighlightText(StrKey,"bold","yellow")
StrMessage = TRIM(CompDate)//", "//TRIM(CompTime)
StrMessage = HighlightText(StrMessage,"normal","white")
WRITE(UNIT_SCREEN,'(1X,*(A,T33))',ADVANCE='NO') TRIM(StrKey)
WRITE(UNIT_SCREEN,'(1X,A1)',ADVANCE='NO') TRIM(Sep)
WRITE(UNIT_SCREEN,'(1X,*(A,T38))',ADVANCE='YES') TRIM(StrMessage)
!--------------------------------------------------------------------------------------------------!
WRITE(UNIT_SCREEN,"(A)") TRIM(StrLine1)
WRITE(UNIT_SCREEN,*)

CALL EXIT(1)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintError
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE PrintHeader(Text1,TextColor)
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: Text1
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: TextColor
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrText
CHARACTER(LEN=256) :: HighlightColor
CHARACTER(LEN=:),ALLOCATABLE :: StrLine
!--------------------------------------------------------------------------------------------------!

IF (PRESENT(TextColor)) THEN
  HighlightColor = TextColor
ELSE
  HighlightColor = "blue"
END IF

StrText = HighlightText(Text1,"bold",HighlightColor)
CALL CreateSeparatingLine("-",StrLine)
StrLine = HighlightText(StrLine,"bold",HighlightColor)

WRITE(UNIT_SCREEN,"(A)") TRIM(StrLine)
WRITE(UNIT_SCREEN,"(1X,A)") TRIM(StrText)
WRITE(UNIT_SCREEN,"(A)") TRIM(StrLine)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintHeader
!==================================================================================================!
!
!
!
!==================================================================================================!
FUNCTION HighlightText(text,text_style,fg_color,bg_color)
!==================================================================================================!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)          :: text
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: text_style
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: fg_color
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: bg_color
CHARACTER(LEN=256)                   :: HighlightText
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
CHARACTER(LEN=16) :: EscSeqStyle
!--------------------------------------------------------------------------------------------------!

EscSeqStyle = "[0"
IF (PRESENT(text_style)) THEN
  SELECT CASE(text_style)
    CASE('normal')
      EscSeqStyle = TRIM(EscSeqStyle)//";0"
    CASE('bold')
      EscSeqStyle = TRIM(EscSeqStyle)//";1"
    CASE('faint')
      EscSeqStyle = TRIM(EscSeqStyle)//";2"
    CASE('italic')
      EscSeqStyle = TRIM(EscSeqStyle)//";3"
    CASE('underline')
      EscSeqStyle = TRIM(EscSeqStyle)//";4"
    CASE('blink_slow')
      EscSeqStyle = TRIM(EscSeqStyle)//";5"
    CASE('blink_fast')    
      EscSeqStyle = TRIM(EscSeqStyle)//";6"
    CASE DEFAULT
      EscSeqStyle = TRIM(EscSeqStyle)//";0"
  END SELECT
ELSE
  EscSeqStyle = TRIM(EscSeqStyle)//";0"
END IF

IF (PRESENT(fg_color)) THEN
  SELECT CASE(fg_color)
    CASE('black')
      EscSeqStyle = TRIM(EscSeqStyle)//";30"
    CASE('red')
      EscSeqStyle = TRIM(EscSeqStyle)//";31"
    CASE('green')
      EscSeqStyle = TRIM(EscSeqStyle)//";32"
    CASE('yellow')
      EscSeqStyle = TRIM(EscSeqStyle)//";33"
    CASE('blue')
      EscSeqStyle = TRIM(EscSeqStyle)//";34"
    CASE('magenta')
      EscSeqStyle = TRIM(EscSeqStyle)//";35"
    CASE('cyan')
      EscSeqStyle = TRIM(EscSeqStyle)//";36"
    CASE('white')
      EscSeqStyle = TRIM(EscSeqStyle)//";37"
    CASE DEFAULT
      EscSeqStyle = TRIM(EscSeqStyle)//";37"
  END SELECT
ELSE
  EscSeqStyle = TRIM(EscSeqStyle)//";39"
END IF

IF (PRESENT(bg_color)) THEN
  SELECT CASE(bg_color)
    CASE('black')
      EscSeqStyle = TRIM(EscSeqStyle)//";40"
    CASE('red')
      EscSeqStyle = TRIM(EscSeqStyle)//";41"
    CASE('green')
      EscSeqStyle = TRIM(EscSeqStyle)//";42"
    CASE('yellow')
      EscSeqStyle = TRIM(EscSeqStyle)//";43"
    CASE('blue')
      EscSeqStyle = TRIM(EscSeqStyle)//";44"
    CASE('magenta')
      EscSeqStyle = TRIM(EscSeqStyle)//";45"
    CASE('cyan')
      EscSeqStyle = TRIM(EscSeqStyle)//";46"
    CASE('white')
      EscSeqStyle = TRIM(EscSeqStyle)//";47"
    CASE DEFAULT
      EscSeqStyle = TRIM(EscSeqStyle)//";47"
  END SELECT
ELSE
  EscSeqStyle = TRIM(EscSeqStyle)//";49"
END IF

HighlightText = CHAR(27)//TRIM(EscSeqStyle)//"m"
HighlightText = TRIM(HighlightText)//TRIM(text)
HighlightText = TRIM(HighlightText)//CHAR(27)//"[m"

!==================================================================================================!
END FUNCTION HighlightText
!==================================================================================================!
!
!
!
!--------------------------------------------------------------------------------------------------!
END MODULE MOD_MAIN_vars
!==================================================================================================!
