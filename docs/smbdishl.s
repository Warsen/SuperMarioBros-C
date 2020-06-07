; =============== Original message:
;
; SMBDIS.ASM - A COMPREHENSIVE SUPER MARIO BROS. DISASSEMBLY
; by doppelganger (doppelheathen@gmail.com)
; This file is provided for your own use as-is.  It will require the character rom data
; and an iNES file header to get it to work.
; There are so many people I have to thank for this, that taking all the credit for
; myself would be an unforgivable act of arrogance. Without their help this would
; probably not be possible.  So I thank all the peeps in the nesdev scene whose insight into
; the 6502 and the NES helped me learn how it works (you guys know who you are, there's no
; way I could have done this without your help), as well as the authors of x816 and SMB
; Utility, and the reverse-engineers who did the original Super Mario Bros. Hacking Project,
; which I compared notes with but did not copy from.  Last but certainly not least, I thank
; Nintendo for creating this game and the NES, without which this disassembly would
; only be theory.
;
; Assembles with x816.
;
; ; =============== end Original message
; -----------------------------------------------------------------------------------------------
;
;   This is a revision of smbdis.asm that uses structured code and high-level constructs where 
; possible. This was a project intended to: test and refine the macro code with a real world assembly
; program, demonstrate it's usefulness, and to share an interesting, and hopefully easier to read
; version of smb source code. 
; 
; Most of doppelganger's comments remain from the original file. I have added some new ones, 
; and revised some to fit with the changes to the code structure, and removed some that I felt redundant.
;
; NOTES:
;   This requires ca65 to assemble, please use a recent snapshot.
;   This is all one flat assembly, no importing from files not directly referenced below.
;   This requires the files specified in the include lines below.
;
;   This isn't as complete as I would like, there are more improvements that could be made,
; such as breaking up this huge file into modules, better block vs goto choices in the conversion,
; cleaner local variables, better choices for defining procedures, however I am no longer interested 
; in pursing it further at this time. Presented as-is, there is no license restrictions for this 
; file whatsoever. 
; Please refer to ca65hl.h for any license restrictions that may apply however.
;
; I'm sure there are some things I forgot to add. There are certainly some mistakes.
; Also, although this did take some time, I would like to say the original smbdis.asm 
; was were all the hard work was completed.
; 
; -- Movax12
; PS, If you wish to contact me please do so via forums.nesdev.com in private message.
;
; -----------------------------------------------------------------------------------------------

.include "nes_constants.h"  ; generic NES system constant ports and masks
.include "ca65hl.h"         ; highlevel macro code for ca65
.include "smbdis.h"         ; macros specific to this project
; .include "ndxdebug.h"     ; thefox's ndx macros, not required, but pretty great
.include "memory.h"         ; memory layout for this project, including constants   

.segment "INESHDR"
  .byte $4E,$45,$53,$1A                           ;  magic signature
  .byte 2                                         ;  PRG ROM size in 16384 byte units
  .byte 1                                         ;      CHR
  .byte $01                                       ;  mirroring type and mapper number lower nibble
  .byte $00                                       ;  mapper number upper nibble

customchars ; load custom charset

.code

    ; ------------------------------------------------------------------------------------------------
    
.proc reset 

    sei
    cld

    mb PPU_CTRL := #%00010000                     ; Set background patterns to $1000
    ldx #$ff
    txs

    repeat
    until lda PPU_STATUS == bit7 set

    repeat
    until lda PPU_STATUS == bit7 set

    ldy #ColdBootOffset                           ; load default cold boot pointer
    ldx #$05                                      ; this is where we check for a warm boot

    repeat                                        ; check each score digit in the top score
        if TopScoreDisplay[ x ] >= #10 goto coldboot                   ; to see if we have a valid digit
    until dex == negative

    if WarmBootValidation = #$a5                  ; second checkpoint, check to see if another location has a specific value
        ldy #WarmBootOffset                       ; if passed both, load warm boot pointer
    endif

    coldboot:

    jsr InitializeMemory                          ; clear memory using pointer in Y, depending on cold/warmboot

    sta SND_DELTA_REG+1                           ; reset delta counter load register , a := #0 after InitializeMemory
    sta OperMode                                  ; reset primary mode of operation

    mb a, WarmBootValidation := #$a5              ; set warm boot flag with reg a
    mb PseudoRandomBitReg := a                    ; set seed for pseudorandom register
    mb SND_MASTERCTRL_REG := #%00001111           ; enable all sound channels except dmc
    mb PPU_MASK := #%00000110                     ; turn off clipping for OAM and background

    jsr MoveAllSpritesOffscreen
    jsr InitializeNameTables                      ; initialize both name tables

    inc DisableScreenFlag                         ; set flag to disable screen output

    mb a := Mirror_PPU_CTRL | #%10000000          ; enable NMIs

    jsr WritePPU_CTRL                             ; write to CTRL port and to mirror

    : jmp :-
.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

VRAM_AddrTable_Low:

    .lobytes VRAM_Buffer1, WaterPaletteData, GroundPaletteData
    .lobytes UndergroundPaletteData, CastlePaletteData, VRAM_Buffer1_Offset
    .lobytes VRAM_Buffer2, VRAM_Buffer2, BowserPaletteData
    .lobytes DaySnowPaletteData, NightSnowPaletteData, MushroomPaletteData
    ; Thanks messages:
    .lobytes MarioThanksMessage, LuigiThanksMessage
    ; world 1 - 7 message
    .lobytes MushroomRetainerSaved
    ; world 8 messages:
    .lobytes PrincessSaved1, PrincessSaved2, WorldSelectMessage1
    .lobytes WorldSelectMessage2

VRAM_AddrTable_High:

    .hibytes VRAM_Buffer1, WaterPaletteData, GroundPaletteData
    .hibytes UndergroundPaletteData, CastlePaletteData, VRAM_Buffer1_Offset
    .hibytes VRAM_Buffer2, VRAM_Buffer2, BowserPaletteData
    .hibytes DaySnowPaletteData, NightSnowPaletteData, MushroomPaletteData
    .hibytes MarioThanksMessage, LuigiThanksMessage, MushroomRetainerSaved
    .hibytes PrincessSaved1, PrincessSaved2, WorldSelectMessage1
    .hibytes WorldSelectMessage2

VRAM_Buffer_Offset:
    .lobytes VRAM_Buffer1_Offset, VRAM_Buffer2_Offset

.code

    ; ------------------------------------------------------------------------------------------------
   
.proc NonMaskableInterrupt

    ; locals
        VRAM_Pointer = temp_byte ; address is also used as temp in pseudorandom
    ; end local

    
    ; disable NMIs in mirror reg, save all other bits:
    mb Mirror_PPU_CTRL := Mirror_PPU_CTRL & #(<~CT_NMI)
    ; alter name table address to be $2800, ($2000), save other bits
    mb PPU_CTRL        := a               & #%01111110  
    mb a               := Mirror_PPU_MASK & #%11100110  ; disable OAM and background display by default

    if ldy DisableScreenFlag == zero              ; if not set:
        mb a := Mirror_PPU_MASK | #%00011110      ; reenable bits and save them
    endif

    mb Mirror_PPU_MASK := a                       ; save bits for later but not in register at the moment

    mb PPU_MASK := a & #%11100111                 ; disable screen for now

    ldx PPU_STATUS                                ; reset flip-flop and reset scroll registers to zero
    lda #0
    jsr InitScroll
    ; reg a still 0
    sta OAM_ADDRESS                               ; reset spr-ram address register
    mb OAM_DMA := #2                              ; perform spr-ram DMA access on $0200-$02ff

    ldx VRAM_Buffer_AddrCtrl                      ; load control for pointer to buffer contents

    mb VRAM_Pointer[ 0 ] := VRAM_AddrTable_Low[ x ]                    ; set indirect at temp_byte to pointer
    mb VRAM_Pointer[ 1 ] := VRAM_AddrTable_High[ x ]

    jsr UpdateScreen                              ; update screen with buffer contents
    ldy #0

    if ldx VRAM_Buffer_AddrCtrl = #$06            ; check for usage of VRAM_Buffer2
        iny                                       ; get offset based on usage
    endif

    mb x := VRAM_Buffer_Offset[ y ]

    lda #0                                        ; clear buffer header at last location
    sta VRAM_Buffer1_Offset,x
    sta VRAM_Buffer1,x
    sta VRAM_Buffer_AddrCtrl                      ; reinit address control to VRAM_Buffer1

    mb PPU_MASK := Mirror_PPU_MASK                ; copy mirror of $2001 to register
    jsr SoundEngine                               ; play sound
    jsr ReadJoypads                               ; read joypads
    jsr PauseRoutine                              ; handle pause
    jsr UpdateTopScore

    if GamePauseStatus >> 1 == carry clear        ; check for pause status
    
        ; if TimerControl is zero do timers, OR decrement it and do timers if now zero
        if ( a := TimerControl == zero ) || ( dec TimerControl == zero)

            mb x := #$14                          ; load end offset for end of frame timers
            
            ; decrement interval timer control:
            ; if expired, interval timers will decrement along with frame timers
            if dec IntervalTimerControl == negative
                mb IntervalTimerControl := #$14
                mb x := #$23
            endif

            repeat                                ; check current timer
                if Timers[ x ]                    ; if current timer still valid:
                    dec Timers,x                  ; decrement the current timer
                endif                             ; move onto next timer - one less than zero..
            until dex == negative                 ; loop will go from $23 or $14 to $0
        endif
        inc FrameCounter                          ; increment frame counter
    endif

    ldx #0
    ldy #$07

    mb temp_byte := PseudoRandomBitReg & #%00000010                    ; get first memory location of LFSR bytes,  keep d1
    mb a := PseudoRandomBitReg[ 1 ] & #%00000010 ^ temp_byte           ; perform exclusive-OR on d1 from first and second bytes

    clc                                           ; if neither or both are set, carry will be clear
    if zero clear
        sec                                       ; if one or the other is set, carry will be set
    endif

    repeat
        ror PseudoRandomBitReg,x                  ; rotate carry into d7, and rotate last bit into carry
        inx                                       ; increment to next byte
    until dey == zero

    if lda Sprite0HitDetectFlag

        repeat ; OP: Use bit/V flag
            mb a := PPU_STATUS & #%01000000       ; wait for sprite 0 flag to clear, which will not happen until vblank has ended
        until zero

        if GamePauseStatus >> 1 == carry clear    ; if not in pause, do sprite stuff
            jsr MoveSpritesOffscreen
            jsr SpriteShuffler
        endif

        do
            mb a := PPU_STATUS & #%01000000       ; do sprite #0 hit detection
        while zero

        ldy #$14                                  ; small delay, to wait until we hit horizontal blank time
        repeat
        until dey == zero

    endif

    mb PPU_SCROLL := HorizontalScroll             ; set scroll registers from variables
    mb PPU_SCROLL := VerticalScroll

    lda Mirror_PPU_CTRL                           ; load saved mirror of $2000
    pha                                           ; keep it safe in the stack
        sta PPU_CTRL

        if GamePauseStatus >> 1 == carry clear
            jsr OperModeExecutionTree                 ; if not in pause mode do one of many, many possible subroutines
        endif

        lda PPU_STATUS                                ; reset flip-flop
    pla
    mb PPU_CTRL := a | #%10000000                 ; reactivate NMIs
    rti                                           ; we are done until the next frame!
.endproc


    ; ------------------------------------------------------------------------------------------------
    
.proc PauseRoutine
    
    ; are we in Victorymode OR are we in game mode AND running game engine?
    if lda OperMode = #MODE_VICTORY || ( a = #MODE_GAMEPLAY && OperMode_Task = #3 )

        if GamePauseTimer                         ; check if pause timer is still counting down
            dec GamePauseTimer                    ; if so, decrement and leave
            rts
        endif ; else

        if SavedJoypad1Bits & #BUTTON_START
        ; exit if bit7 set, joypad reading routine makes this unnecessary
            if GamePauseStatus & #%10000000 goto Exit

            mb GamePauseTimer := #$2b             ; timer value

            lda GamePauseStatus                   ; will be #0 or #1
            mb y := a
            mb PauseSoundQueue := y + 1           ; set pause sfx queue for next pause mode 1, or 2

            mb a := a ^ #%00000001 | #%10000000   ; invert d0 and set d7 .. d7 = don't run this code block

        else Z clear                           ; (tell the else to use bne as unconditional)
            lda GamePauseStatus                   ; clear bit7 so the code above can be run again (residual)
            and #%01111111
        endif

        sta GamePauseStatus
    endif
    Exit:
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc SpriteShuffler
    
    ; locals
        ; temp_byte - used for value to add to sprite offset
    ; End locals

    ; OP: remove temp_byte and replace with immediate value

    ldy AreaType                                  ; load level type, likely residual code OP
    mb temp_byte := # ( 10 * 4 )                  ; sprite #10
    ldx #14                                      ; start at the end of OAM data offsets

    repeat

        if SprDataOffset[ x ] >= temp_byte        ; the preset value if less, skip this part ; OP change this to immediate #$28

            ldy SprShuffleAmtOffset               ; get current offset to preset value we want to add
            mb a := a + SprShuffleAmt[ y ]        ; get shuffle amount, add to current sprite offset
            if carry set                          ; if exceeded $ff, do second add
                mb a := a + temp_byte             ; add preset value 40 to offset to skip first ten sprites again 
            endif

            mb SprDataOffset[ x ] := a            ; store new offset here or old one if 2nd add skipped

        endif

    until dex == negative                         ; move backwards to next one

    mb x := SprShuffleAmtOffset + 1

    if x = #3                                     ; if offset was = 2
        ldx #0                                    ; init to 0
    endif

    stx SprShuffleAmtOffset                       ; Will always be 0, 1, 2

    ldx #$08                                      ; load offsets for values and storage
    ldy #2

    repeat                                        ; y from 2 to 0
    ; store first one unmodified, but add eight to the second and eight more to the third one
    ; note that due to the way X is set up, this code loads into the misc sprite offsets

        mb a := SprDataOffset [ 5 + y ]
        mb      SprDataOffset [ x + 15 - 2 ] := a
        mb      SprDataOffset [ x + 15 - 1 ] := a + #8
        mb      SprDataOffset [ x + 15     ] := a + #8 ; add another #8

        mb x := x - 3

    until dey == negative

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc OperModeExecutionTree
    

    lda OperMode                                  ; this is the heart of the entire program,
    jsr JumpEngine                                ; most of what goes on starts here

    .word TitleScreenMode
    .word GameMode
    .word VictoryMode
    .word GameOverMode

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc MoveAllSpritesOffscreen

    .global MoveSpritesOffscreen

    ldy #0                                        ; this routine moves all sprites off the screen
    BIT_Skip2                                     ; BIT instruction opcode - skip over next two bytes trick

    MoveSpritesOffscreen:
    ldy #4                                        ; this routine moves all but sprite 0
    lda #$f8                                      ; off the screen

    repeat
        mb Sprite[ y ]::Y_Position := a           ; write 248 into OAM data's Y coordinate
    until y := y + 4 == zero
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc TitleScreenMode

    lda OperMode_Task
    jsr JumpEngine

    .word InitializeGame
    .word ScreenRoutines
    .word PrimaryGameSetup
    .word GameMenuRoutine

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

    ; This data is prefeixed with the buffer offset value, then the address,
    ; size / vertical PPU mode and two data values that are adjusted by the code.

WSelectBufferTemplate:    .byte $04, $20, $73, $01, $00, $00

.code

.proc GameMenuRoutine

    ldy #0                                                             ; default value for worldselect via B button
    mb a := SavedJoypad1Bits | SavedJoypad2Bits
    
    set_long_branch +
    if (a <> #BUTTON_START) && a <> ( #(BUTTON_A+BUTTON_START))        ; If start not pressed, and start and A not pressed then:
    set_long_branch -

        if a <> #BUTTON_SELECT                    ; if select not pressed:

            if ldx DemoTimer == zero
                sta SelectTimer                   ; set controller bits here if running demo
                jsr DemoEngine                    ; run through the demo actions
                if carry set goto ResetTitle      ; carry flag set = demo over
                jmp RunDemo                       ; otherwise, run game engine for demo
            endif ; else:

            if !ldx WorldSelectEnableFlag || a <> #BUTTON_B goto NullJoypad
            iny                                   ; increment Y = b was pressed and Worldselect is enabled
        endif

        if !DemoTimer goto ResetTitle             ; if select or B pressed, check demo timer one last time

        mb DemoTimer := #$18                      ; otherwise reset demo timer

        if !SelectTimer                           ; check select/B button timer,

            mb SelectTimer := #$10                ; reset select button timer
            if not y = #1                         ; was the B button pressed earlier?  if not then:

                mb NumberOfPlayers := NumberOfPlayers ^ #1             ; must have been the select button, therefore
                jsr DrawMushroomIcon              ; change number of players and draw icon accordingly

            else ; valid B pressed:

                mb x, a := WorldSelectNumber + 1         ; increment world select number with x
                mb WorldSelectNumber := a & #%00000111   ; keep only from 0 to 7, store as current world select number

                jsr SetStartWorld                    ; on return, x = #0
                do ; x = 0 to 5
                    ; write template for world select in vram buffer, as well as set VRAM_Buffer1_Offset to 4
                    mb VRAM_Buffer1[ x - 1 ] := WSelectBufferTemplate[ x ]      
                    inx
                while cpx #$06 == negative        ; until equal to 6 .. why N use flag?
                ; get world number from variable and increment for proper display, and put in blank byte before
                mb y, VRAM_Buffer1[ 3 ] := WorldNumber + 1

            endif
        endif

        NullJoypad:

        mb SavedJoypad1Bits := #0                 ; clear joypad bits for player 1

        RunDemo:

        jsr GameCoreRoutine                       ; run game engine
        if GameEngineSubroutine <> #$06 goto ExitMenu                  ; check to see if we're running lose life routine

        ResetTitle:

        mb a:= #0                                 ; reset game modes, disable
        mb OperMode             := a              ; sprite 0 check and disable
        mb OperMode_Task        := a
        mb Sprite0HitDetectFlag := a
        inc DisableScreenFlag                     ; screen output
        rts
    endif
    
    ; else, from here, start was pressed, or start + A

    if !ldy DemoTimer goto ResetTitle             ; if timer for demo has expired, reset modes

    if a << 1 == carry set                        ; A BUTTON pressed as well, could OP: N flag
        lda ContinueWorld                         ; load previously saved world number for secret
        jsr SetStartWorld                            ; continue function when pressing A + start
    endif

    jsr LoadAreaPointer

    inc Hidden1UpFlag                             ; set 1-up box flag for both players
    inc OffScr_Hidden1UpFlag
    inc FetchNewGameTimerFlag                     ; set fetch new game timer flag
    inc OperMode                                  ; set next game mode

    mb PrimaryHardMode := WorldSelectEnableFlag   ; World select is on, hard mode must be on as well

    lda #0
    sta OperMode_Task                             ; set game mode here, and clear demo timer
    sta DemoTimer

    ldx #$17
    lda #0
    do
        sta ScoreAndCoinDisplay,x                 ; clear player scores and coin displays
    while dex == positive

    ExitMenu:
    rts
    
    SetStartWorld:

    sta WorldNumber                               ; start both players at the first area
    sta OffScr_WorldNumber                        ; of the previously saved world number

    ldx #0                                        ; note that on power-up using this function
    stx AreaNumber                                ; will make no difference
    stx OffScr_AreaNumber

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg
    ; This data is prefeixed with the buffer offset value, then the address,
    ; size / vertical PPU mode and three data values that are adjusted by the code.

MushroomIconData:      .byte $07, $22, $49, $83, $ce, $24, $24, $00

    ; ------------------------------------------------------------------------------------------------

.code

.proc DrawMushroomIcon

    ldy #$07                                      ; read eight bytes to be read by transfer routine
    repeat
        mb VRAM_Buffer1[ y - 1 ] := MushroomIconData[ y ]     ; note that the default position is set for a 1 - player game
    until dey == negative

    if NumberOfPlayers                            ; check number of players
        mb VRAM_Buffer1[ 3 ] := #$24
        mb VRAM_Buffer1[ 5 ] := #$ce; then load shroom icon tile in 2-player position
    endif
    rts
.endproc

dataseg

DemoActionData:
    .byte $01, $80, $02, $81, $41, $80, $01
    .byte $42, $c2, $02, $80, $41, $c1, $41, $c1
    .byte $01, $c1, $01, $02, $80, $00

DemoTimingData:
    .byte $9b, $10, $18, $05, $2c, $20, $24
    .byte $15, $5a, $10, $20, $28, $30, $20, $10
    .byte $80, $20, $30, $30, $01, $ff, $00

.code

    ; ------------------------------------------------------------------------------------------------

.proc DemoEngine

    ldx DemoAction                                ; load current demo action

    if !DemoActionTimer                           ; load current action timer, if timer done counting down then:
        inx
        inc DemoAction                            ; if expired, increment action, X, and
        sec                                       ; set carry by default for demo over
        mb a := DemoTimingData[ x - 1 ]           ; get next timer
        if sta DemoActionTimer == zero goto Exit  ; store as current timer. if timer already at zero, skip
    endif

    mb SavedJoypad1Bits := DemoActionData[ x - 1 ]
    dec DemoActionTimer                           ; decrement action timer
    clc                                           ; clear carry if demo still going
    Exit:
    rts
    
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc VictoryMode

    jsr VictoryModeSubroutines                    ; run victory mode subroutines

        if OperMode_Task                              ; get victory mode task, if on bridge collapse, skip enemy processing
            mb x, ObjectOffset := #0                  ; reset enemy object offset
            jsr EnemiesAndLoopsCore                   ; and run enemy code
        endif

        jsr RelativePlayerPosition                    ; get player's relative coordinates
        jmp PlayerGfxHandler                          ; draw the player, then leave

    VictoryModeSubroutines:
    
    lda OperMode_Task

    jsr JumpEngine

    .word BridgeCollapse
    .word SetupVictoryMode
    .word PlayerVictoryWalk
    .word PrintVictoryMessages
    .word PlayerEndWorld

    ; ------------------------------------------------------------------------------------------------
    
    SetupVictoryMode:
    ; get page location of right side of screen increment to next page
    mb x, DestinationPageLoc := ScreenRight_PageLoc + 1

    mb EventMusicQueue := #MUSIC_EndOfCastle      ; play win castle music
    jmp IncModeTask_B                             ; jump to set next major task in victory mode
    
    ; ------------------------------------------------------------------------------------------------

    PlayerVictoryWalk:

    ldy #0                                        ; set value here to not walk player by default
    sty VictoryWalkControl                        ; OP:  do this after the if block
    ; get player's page location compare with destination page location, if page locations don't match, or X position
    ; less than $60 :

    if Player_PageLoc <> DestinationPageLoc || Player_X_Position < #$60
        inc VictoryWalkControl
        iny                                       ; note Y will be used to walk the player
    endif

    tya                                           ; put contents of Y in A and
    jsr AutoControlPlayer                         ; use A to move player to the right or not

    if ScreenLeft_PageLoc <> DestinationPageLoc   ; check page location of left side of screen

        mb ScrollFractional := ScrollFractional + #$80                 ; save fractional movement amount
        mb y := #1 + C                          ; set 1 pixel per frame, plus carry

        jsr ScrollScreen                          ; do sub to scroll the screen
        jsr UpdScrollVar                          ; do another sub to update screen and scroll variables
        inc VictoryWalkControl                    ; increment value to stay in this routine
    endif


    if not VictoryWalkControl goto IncModeTask_A  ; load value set here, if zero, branch to change modes
    rts                                           ; otherwise leave
    
    ; ------------------------------------------------------------------------------------------------

    PrintVictoryMessages:

    if SecondaryMsgCounter goto IncMsgCounter     ; if set, increment message counters

    if PrimaryMsgCounter        ; otherwise load primary message counter, if not to zero
    ; (this comparison is residual code, counter never reaches 9)
        if a >= #$09 goto IncMsgCounter           ; if at 9 or above, branch elsewhere 

        if ldy WorldNumber = #WORLD8              ; check world number
            if a < #3 goto IncMsgCounter          ; check primary message counter again, if not at 3, increment
            sbc #1                                ; otherwise subtract one..carry is set
            jmp ThankPlayer                       ; and skip to next part
        endif

        ; a is still "primary message counter" - if not at 2 yet (world 1-7 only), branch
        if a < #2 goto IncMsgCounter              

    endif
    
    ; ------------------------------------------------------------------------------------------------

    ThankPlayer:
    ; put primary message counter into Y
    ; if counter nonzero, skip this part, do not print first message

    if tay == zero
        if lda CurrentPlayer == zero goto EvalForMusic  ; if mario, branch
        iny
        bne EvalForMusic                          ; do an unconditional branch to the same place
    endif

    iny                                           ; increment Y to do world 8's message

    if WorldNumber = #WORLD8 goto EvalForMusic
    ; otherwise decrement Y for world 1-7's message 
    ; if counter at 4 (world 1-7 only) branch to set victory end timer

    mb y := y - 1

    if y < #4

        if y < #3                                 ; if counter at 3 (world 1-7 only),  branch to keep counting
            EvalForMusic:
            
            ; if counter at 3 (WORLD8 only), load victory music first..
            ; otherwise print message only. (note world 1-7 will only reach this code if counter = 0, and will always branch) 
            if y = #3                             
                
                mb EventMusicQueue := #MUSIC_Victory                
            endif
            ; add $0c or 12 to counter thus giving an appropriate value,
            ; ($0c-$0d = first), ($0e = world 1-7's), ($0f-$12 = world 8's)
            mb a := y
            mb VRAM_Buffer_AddrCtrl := a + #$0c   ; write message counter to vram address controller
        endif

        IncMsgCounter:
        mb SecondaryMsgCounter := SecondaryMsgCounter + #4
        mb PrimaryMsgCounter   := PrimaryMsgCounter + C              ; add carry
        cmp #$07                                  ; check primary counter one more time

    endif  
    ; This could be optimzed for HL macro if this endif was at same endif as 
    ; "if greaterORequal" (no effect on the game logic), carry will always be set

    if greaterORequal                             ; if not reached value yet, branch to leave
        mb WorldEndTimer := #$06                  ; otherwise set world end timer

        IncModeTask_A:
        inc OperMode_Task                         ; move onto next task in mode
    endif

    rts                                           ; leave

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PlayerEndWorld

    if WorldEndTimer goto Exit                    ; check to see if world end timer finished counting down, leave if not

    if ldy WorldNumber < #WORLD8                  ; if world is less than world 8 :
        lda #0
        sta AreaNumber                            ; otherwise initialize area number used as offset
        sta LevelNumber                           ; and level number control to start at area 1
        sta OperMode_Task                         ; initialize secondary mode of operation

        inc WorldNumber                           ; increment world number to move onto the next world
        jsr LoadAreaPointer                       ; get area address offset for the next area
        inc FetchNewGameTimerFlag                 ; set flag to load game timer from header

        mb OperMode := #MODE_GAMEPLAY             ; set mode of operation to game mode
        Exit:
        rts                                       ; and leave
    endif
    ; check to see if B button was pressed on either controller

    if SavedJoypad1Bits | SavedJoypad2Bits & #BUTTON_B
        mb WorldSelectEnableFlag := #1
        mb NumberofLives := #$FF                  ; remove onscreen player's lives
        jsr TerminateGame                         ; do sub to continue other player or end game
    endif

    rts

.endproc
    ; data is used as tiles for numbers
    ; that appear when you defeat enemies

dataseg

FloateyNumTileData:
    .byte $ff, $ff                                ; dummy
    .byte $f6, $fb                                ;  "100"
    .byte $f7, $fb                                ;  "200"
    .byte $f8, $fb                                ;  "400"
    .byte $f9, $fb                                ;  "500"
    .byte $fa, $fb                                ;  "800"
    .byte $f6, $50                                ;  "1000"
    .byte $f7, $50                                ;  "2000"
    .byte $f8, $50                                ;  "4000"
    .byte $f9, $50                                ;  "5000"
    .byte $fa, $50                                ;  "8000"
    .byte $fd, $fe                                ;  "1-UP"
    ; high nybble is digit number, low nybble is number to
    ; add to the digit of the player's score

ScoreUpdateData:
    .byte $ff                                     ; dummy
    .byte $41, $42, $44, $45, $48
    .byte $31, $32, $34, $35, $38
    .byte $00                                     ; 1 up

.code

    ; ------------------------------------------------------------------------------------------------

.proc FloateyNumbersRoutine
    ; OP FIX this, this branch is stupid and should branch to a closer RTS
    if lda FloateyNum[ x ]::Control == zero goto PlayerEndWorld::Exit    

    if a >= #$0b
        mb FloateyNum[ x ]::Control := #$0b       ; set to $0b, thus keeping it in range
    endif

    tay                                           ; use as Y

    if !FloateyNum_Timer[ x ]                     ; check value here
        mb FloateyNum[ x ]::Control := a          ; #0   ; initialize floatey number control and leave
        rts
    endif

    dec FloateyNum_Timer,x                        ; decrement value here
    ; if FloateyNum_Timer = 2b (before decrement)
    if a = #$2b
        ; if FloateyNum[ x ]::Control = #$0b
        if y = #$0b                               ; check offset for $0b
            inc NumberofLives                     ; give player one extra life (1-up)
            mb Square2SoundQueue := #SFX_ExtraLife                     ; and play the 1-up sound
        endif
        ; use as X offset, essentially the digit to be incremented
        mb x := ScoreUpdateData[ y ] >> 4
        ; mask out the high nybble, store as amount to add to the digit
        mb DigitModifier[ x ] := ScoreUpdateData[ y ] & #%00001111

        jsr AddToScore                            ; update the score accordingly
    endif

    ldy Enemy_SprDataOffset,x                     ; get OAM data offset for enemy object
    
    
    ; WHAT is this for??
    ; if enemy is not spiny AND not Pplant AND ( is hammerbro OR ( Not either cheepcheep AND
    ; ( greater or equal to  tallenemy OR state  less than #2)) )
    mb a := Enemy_ID[ x ]

    if a <> #OBJECTID_Spiny && a <> #OBJECTID_PiranhaPlant && ( a = #OBJECTID_HammerBro || \
    ( a <> #OBJECTID_GreyCheepCheep && a <> #OBJECTID_RedCheepCheep && ( a >= #OBJECTID_TallEnemy || Enemy_State[ x ] < #2 ) ) )
        ldx SprDataOffset_Ctrl                    ; load some kind of control bit
        ldy Alt_SprDataOffset,x                   ; get alternate OAM data offset
        ldx ObjectOffset                          ; get enemy object offset again
    endif
    
    
    if FloateyNum[ x ]::Y_Pos >= #$18             ; if coordinate is below the status bar:
        mb FloateyNum[ x ]::Y_Pos := a -c #1      ; carry is set, so subtract #1 here
    endif

    mb a := FloateyNum[ x ]::Y_Pos -c #8          ; subtract eight and dump into the
    jsr DumpTwoSpr                                ; left and right sprite's Y coordinates

    mb Sprite[ y     ]::X_Position := FloateyNum[ x ]::X_Pos           ; X coordinate of left sprite
    mb Sprite[ y + 4 ]::X_Position := a + #8      ; X coordinate of right sprite

    mb a := #2
    mb Sprite[ y     ]::Attributes := a           ; set palette control in attribute bytes
    mb Sprite[ y + 4 ]::Attributes := a           ; of left and right sprites

    mb x := FloateyNum[ x ]::Control * 2          ; and as offset for look-up table
    mb Sprite[ y     ]::Tilenumber := FloateyNumTileData[ x ]          ; display first half of number of points
    mb Sprite[ y + 4 ]::Tilenumber := FloateyNumTileData[ x  + 1 ]     ; display the second half

    ldx ObjectOffset                              ; get enemy object offset and leave
    rts

.endproc


    ; ------------------------------------------------------------------------------------------------

.proc ScreenRoutines
    lda ScreenRoutineTask                         ; run one of the following subroutines
    jsr JumpEngine

    .word InitScreen
    .word SetupIntermediate
    .word WriteTopStatusLine
    .word WriteBottomStatusLine
    .word DisplayTimeUp
    .word ResetSpritesAndScreenTimer
    .word DisplayIntermediate
    .word ResetSpritesAndScreenTimer
    .word AreaParserTaskControl
    .word GetAreaPalette
    .word GetBackgroundColor
    .word GetAlternatePalette1
    .word DrawTitleScreen
    .word ClearBuffersDrawIcon
    .word WriteTopScore
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitScreen
    jsr MoveAllSpritesOffscreen                   ; initialize all sprites including sprite #0
    jsr InitializeNameTables                      ; and erase both name and attribute tables
    if lda OperMode == zero goto NextSubtask

    ldx #3                                        ; into buffer pointer
    jmp SetVRAMAddr_A
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc SetupIntermediate

    lda BackgroundColorCtrl                       ; save current background color control
    pha                                           ; and player status to stack
        lda PlayerStatus
        pha
            mb PlayerStatus := #0                 ; set background color to black and player status to not fiery
            mb BackgroundColorCtrl := #2          ; this is the ONLY time background color control is set to less than 4
            jsr GetPlayerColors
        pla                                       ; we only execute this routine for
        sta PlayerStatus                          ; the intermediate lives display
    pla                                           ; and once we're done, we return bg
    sta BackgroundColorCtrl                       ; color ctrl and player status from stack

    jmp IncSubtask                                ; then move onto the next task

.endproc

dataseg

AreaPalette:
    .byte $01, $02, $03, $04

.code

.proc GetAreaPalette

    .export SetVRAMAddr_A

    ldy AreaType                                  ; select appropriate palette to load
    ldx AreaPalette,y                             ; based on area type
    SetVRAMAddr_A:
    stx VRAM_Buffer_AddrCtrl                      ; store offset into buffer control

.endproc

.proc NextSubtask
    jmp IncSubtask                                ; move onto next task
.endproc


dataseg

BGColorCtrl_Addr:
    .byte $00, $09, $0a, $04

BackgroundColors:
    .byte $22, $22, $0f, $0f                      ; used by area type if bg color ctrl not set
    .byte $0f, $22, $0f, $0f                      ; used by background color control if set

PlayerColors:
    .byte $22, $16, $27, $18                      ; mario's colors
    .byte $22, $30, $27, $19                      ; luigi's colors
    .byte $22, $37, $27, $16                      ; fiery (used by both)

.code

    ; ------------------------------------------------------------------------------------------------

.proc GetBackgroundColor

    ; locals
        LoopCounter = temp_byte
    ; end locals

    .export GetPlayerColors
    .export SetVRAMOffset
    ; check background color control, if not set, increment task and fetch palette and put appropriate palette into vram

    if ldy BackgroundColorCtrl
        mb VRAM_Buffer_AddrCtrl := BGColorCtrl_Addr[ y - 4 ]           ; note that if set to 5-7, $0301 will not be read
    endif

    inc ScreenRoutineTask                         ; increment to next subtask and plod on through

    GetPlayerColors:

    mb x := VRAM_Buffer1_Offset                   ; get current buffer offset
    mb y := #0

    if CurrentPlayer != zero                      ; check which player is on the screen, not zero is luigi
        mb y:= #4                                 ; load offset for luigi
    endif

    if PlayerStatus = #2                          ; check player status, if fiery, load alternate offset for fiery player
        mb y := #$08
    endif

    mb LoopCounter := #3                          ; do four colors
    repeat                                        ; fetch player colors and store them
        mb VRAM_Buffer1[ 3 + x ] := PlayerColors[ y ]                  ; in the buffer
        iny
        inx
    until dec LoopCounter == negative

    mb x := VRAM_Buffer1_Offset                   ; load original offset from before

    if ldy BackgroundColorCtrl == zero            ; if this value is four or greater, it will be set 
        mb y := AreaType                          ; otherwise use area type bits from area offset as offset
    endif

    mb VRAM_Buffer1[ 3 + x ] := BackgroundColors[ y ]
    mb VRAM_Buffer1[ 0 + x ] := #$3f              ; set for sprite palette address
    mb VRAM_Buffer1[ 1 + x ] := #$10
    mb VRAM_Buffer1[ 2 + x ] := #4                ; write length byte to buffer
    mb VRAM_Buffer1[ 7 + x ] := #0                ; now the null terminator

    mb a := x                                     ; move the buffer pointer ahead 7 bytes     
    mb a := a + #$07

    SetVRAMOffset:
    mb VRAM_Buffer1_Offset := a                   ; store as new vram buffer offset
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc GetAlternatePalette1

    .export SetVRAMAddr_B

    if AreaStyle = #1                             ; check for mushroom level style
        lda #$0b                                  ; if found, load appropriate palette
        SetVRAMAddr_B:
        sta VRAM_Buffer_AddrCtrl
    endif
    jmp IncSubtask                                ; now onto the next task

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc WriteTopStatusLine
    lda #0                                        ; select main status bar
    jsr WriteGameText                             ; output it
    jmp IncSubtask                                ; onto the next task
.endproc

    ; ------------------------------------------------------------------------------------------------
    ; This procedure just writes the level and sublevel to the VRAM buffer for display under "WORLD"

.proc WriteBottomStatusLine

    jsr GetSBNybbles                              ; write player's score and coin tally to screen
    ldx VRAM_Buffer1_Offset

    mb VRAM_Buffer1[ 0 + x ] := #$20              ; write address for world-area number on screen
    mb VRAM_Buffer1[ 1 + x ] := #$73
    mb VRAM_Buffer1[ 2 + x ] := #3                ; write length for it

    mb y, a := WorldNumber + 1                    ; number increment for proper number display
    mb VRAM_Buffer1[ 3 + x ] := a                 ; first the world number
    mb VRAM_Buffer1[ 4 + x ] := # '-'             ; next the dash

    mb y, a := LevelNumber + 1
    mb VRAM_Buffer1[ 5 + x ] := a
    mb VRAM_Buffer1[ 6 + x ] := #0                ; put null terminator on
    txa
    mb VRAM_Buffer1_Offset := a + #$06            ; move the buffer offset up by 6 bytes

    jmp IncSubtask

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DisplayTimeUp

    if GameTimerExpiredFlag                       ; if game timer not expired, increment task, control 2 tasks forward
        mb GameTimerExpiredFlag := #0             ; reset timer expiration flag
        lda #2                                    ; output time-up screen to buffer
        jmp OutputInter
    endif
    inc ScreenRoutineTask                         ; increment control task 2 tasks forward
    jmp IncSubtask
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DisplayIntermediate

    .export OutputInter


    if OperMode != zero                           ; check primary mode of operation, if in title screen mode, skip this

        if a = #MODE_GAMEOVER goto GameOverInter  ; are we in game over mode, if so, proceed to display game over screen
        ; otherwise check for mode of alternate entry, AND check if we are on castle level (#$03) or Lives display isn't set
        if ! AltEntranceControl && ( ldy AreaType = #3 || ! DisableIntermediate )

            jsr DrawPlayer_Intermediate           ; put player in appropriate place for
            lda #1                                ; lives display, then output lives display to buffer

            OutputInter:

            jsr WriteGameText
            jsr ResetScreenTimer
            lda #0
            sta DisableScreenFlag                 ; reenable screen output
            rts

            GameOverInter:

            lda #$12                              ; set screen timer
            sta ScreenTimer
            lda #3                                ; output game over screen to buffer
            jsr WriteGameText
            jmp IncModeTask_B
        endif
    endif

    lda #$08                                      ; set for specific task and leave
    sta ScreenRoutineTask
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc AreaParserTaskControl

    inc DisableScreenFlag                         ; turn off screen
    repeat
        jsr AreaParserTaskHandler                 ; render column set of current area
    until lda AreaParserTaskNum == zero           ; repeat until check number of tasks is zero


    if dec ColumnSets == negative                 ; do we need to render more column sets?
        inc ScreenRoutineTask                     ; if not, move on to the next task
    endif

    mb VRAM_Buffer_AddrCtrl := #06                ; ; set vram buffer to output rendered column set on next NMI

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DrawTitleScreen
    ; locals
        RAMPointer = temp_byte
    ; end locals

    if lda OperMode != zero goto IncModeTask_B    ; are we in title screen mode? if not, exit

    lda #>TitleScreenDataOffset                   ; load address $1ec0 into
    sta PPU_ADDRESS                               ; the vram address register
    lda #<TitleScreenDataOffset
    sta PPU_ADDRESS

    mb    RAMPointer[ 1 ] := #3                      ; put address $0300 into the indirect
    mb y, RAMPointer      := #0
    
    lda PPU_DATA                                  ; do one garbage read
    ; 313 bytes copied; 320 bytes at the end of chr are data

    repeat                                        ; get title screen from chr-rom
        mb (RAMPointer)[ y ] := PPU_DATA          ; store 256 bytes into buffer
        if iny == zero                            ; if not past 256 bytes, do not increment
            inc RAMPointer + 1                    ; otherwise increment high byte of indirect
        endif
    until RAMPointer[ 1 ] = #4 && y >= #$3a       ; until address= $043a

    lda #$05                                      ; set buffer transfer control to $0300,
    jmp SetVRAMAddr_B                             ; increment task and exit

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ClearBuffersDrawIcon

    .export IncSubtask

    if lda OperMode != zero goto IncModeTask_B    ; check game mode, if not title screen mode, leave

    ldx #0                                        ; otherwise, clear buffer space
    ; a := 0 here
    repeat
        mb VRAM_Buffer1[ x - 1 ] := a
        mb VRAM_Buffer1[ $100 + x - 1 ] := a
    until dex == zero

    jsr DrawMushroomIcon                          ; draw player select icon

    IncSubtask:
    inc ScreenRoutineTask                         ; move onto next task
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc WriteTopScore
    lda #$fa                                      ; run display routine to display top score on title
    jsr UpdateNumber
.endproc

.proc IncModeTask_B
    inc OperMode_Task                             ; move onto next mode
    rts
.endproc

dataseg

GameText:

TopStatusBarLine:

    vram_string 3, 2, "MARIO"               ; x and y location of string on the screen, followed by string
    vram_string 18, 2, "WORLD  TIME"        ; only these two are set to use this macro.
    .byte $20, $68, $05
    .byte "0  ",$2e, "x"                          ;  score trailing digit and coin display, $2e = coin icon

    .byte $23, $c0, $7f, $aa                      ;  attribute table data, clears name table 0 to palette 2
    .byte $23, $c2, $01, $ea                      ;  attribute table data, used for coin icon in status bar
    .byte $ff                                     ;  end of data block

WorldLivesDisplay:
    .byte $21, $cd, $07
    .byte "  x    "

    .byte $21, $4b, $09
    .byte "WORLD  - "                             ;  "WORLD  - " used on lives display
    .byte $22, $0c, $47, $24                      ;  possibly used to clear time up
    .byte $23, $dc, $01, $ba                      ; attribute table data for crown if more than 9 lives
    .byte $ff

TwoPlayerTimeUp:
    .byte $21, $cd, $05
    .byte "MARIO"
OnePlayerTimeUp:
    .byte $22, $0c, $07
    .byte "TIME UP"
    .byte $ff

TwoPlayerGameOver:
    .byte $21, $cd, $05
    .byte "MARIO"
OnePlayerGameOver:
    .byte $22, $0b, $09
    .byte "GAME OVER"
    .byte $ff

WarpZoneWelcome:
    .byte $25, $84, $15
    .byte "WELCOME TO WARP ZONE!"
    .byte $26, $25, $01, $24                      ;  placeholder for left pipe
    .byte $26, $2d, $01, $24                      ;  placeholder for middle pipe
    .byte $26, $35, $01, $24                      ;  placeholder for right pipe
    .byte $27, $d9, $46, $aa                      ;  attribute data
    .byte $27, $e1, $45, $aa
    .byte $ff

LuigiName:
    .byte "LUIGI"                                 ;  "LUIGI", no address or length

WarpZoneNumbers:
    .byte "432", $00                              ;  warp zone numbers, note spaces on middle
    .byte " 5 ", $00                              ;  zone, partly responsible for
    .byte "876", $00                              ;  the minus world

GameTextOffsets:
    .byte TopStatusBarLine-GameText, TopStatusBarLine-GameText
    .byte WorldLivesDisplay-GameText, WorldLivesDisplay-GameText
    .byte TwoPlayerTimeUp-GameText, OnePlayerTimeUp-GameText
    .byte TwoPlayerGameOver-GameText, OnePlayerGameOver-GameText
    .byte WarpZoneWelcome-GameText, WarpZoneWelcome-GameText

.code

    ; ------------------------------------------------------------------------------------------------

.proc WriteGameText

    pha                                           ; save text number to stack
        mb y := a * 2                              ; multiply by 2 and use as offset
        
        ; if offset greater than top status bar or world/lives display:
        if y >= #4                                 
            if y >= #$08                          ; if set to do time-up or game over, branch to check players
                ldy #$08                          ; otherwise warp zone, therefore set offset
            endif
            if lda NumberOfPlayers == zero        ; if one player increment offset by one to not print name
                iny                               ; ( if there are two, use current offset to also print name)
            endif
        endif

        ldx GameTextOffsets,y                     ; get offset to message we want to print
        ldy #0

        repeat
            if GameText[ x ] = #$FF break         ; check for terminator
            mb VRAM_Buffer1[ y ] := a             ; otherwise write data to buffer
            inx                                   ; and increment increment
        until iny == zero                         ; do this for 256 bytes if no terminator found

        mb VRAM_Buffer1[ y ] := #0                ; put null terminator at end
    pla                                           ; pull original text number from stack
    tax

    if a >= #4 goto PrintWarpZoneNumbers          ; are we printing warp zone? 4 5 or 6 are valid for warp

    if ( dex != zero ) goto CheckPlayerName       ; are we printing the world/lives display? if NOT check player's name

    mb a := NumberofLives + #1                    ;  check number of lives and increment by one for display

    if a >= #10                                   ; more than 9 lives?
    ; c set
        mb a := a -c #10                          ; if so, subtract 10 and put a crown tile next to the difference
        mb y, VRAM_Buffer1[ 7 ] := #$9f           ; ...strange things happen if the number of lives exceeds 19
    endif

    mb VRAM_Buffer1[ 8 ] := a
    ; write world and level numbers (incremented for display)
    ; to the buffer in the spaces surrounding the dash
    mb y, VRAM_Buffer1[ 19 ] := WorldNumber + 1
    mb y, VRAM_Buffer1[ 21 ] := LevelNumber + 1   ; we're done here

    rts
.endproc

    ; x has message number
    ; ------------------------------------------------------------------------------------------------

.proc CheckPlayerName

    if NumberOfPlayers                            ; check number of players,  if only 1 player (zero) , leave

        lda CurrentPlayer                         ; load current player

        ; check to see if current message number is for time up and we are not in game over
        if ( dex == zero) && ( ldy OperMode <> #MODE_GAMEOVER  )
            eor #%00000001                        ; time up, invert d0 to do other player
        endif

        if a >> 1 == carry set                    ; If set, change name
            ldy #4
            do                                    ; change VRAM_Buffer1[ 7 ] to [ 3 ]
                mb VRAM_Buffer1[ 3 + y ]  :=  LuigiName[ y ]           ; otherwise, replace "MARIO" with "LUIGI"
            while dey == positive
        endif

    endif
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PrintWarpZoneNumbers

    ; carry always set here
    ; subtract 4 and then shift to the left twice to get proper warp zone number offset
    ; a = 4 , 5 or 6, refferring to the warp zones (2,3,4), (5), and (6,7,8)
    ; so x will hold 0, 4, or 8

    mb x := a -c #4 << 2

    ldy #0
    repeat
    ; print warp zone numbers into the placeholders from earlier
        mb VRAM_Buffer1[ 27 + y ] := WarpZoneNumbers[ x ]
        inx
        mb y := y + 4                             ; put a number in every fourth space
    until y >= #( 4 * 3)

    lda #$2c                                      ; load new buffer pointer at end of message
    jmp SetVRAMOffset

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ResetSpritesAndScreenTimer

    .export ResetScreenTimer

    if lda ScreenTimer == zero                    ; check if screen timer has expired

        jsr MoveAllSpritesOffscreen               ; if yes, reset sprites now
        ResetScreenTimer:

        mb ScreenTimer := #$07                    ; reset timer again
        inc ScreenRoutineTask                     ; move onto next task
    endif
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc RenderAreaGraphics

    ; locals
        tempVRAMBufferOffset    = temp_byte       ; - temp vram buffer offset
        tempMTileBufferOffset   = temp_byte + 1   ; - temp metatile buffer offset
        tempMetatileGfxTblOffet = temp_byte + 2   ; - temp metatile graphics table offset
        AttribBits              = temp_byte + 3   ; - used to store attribute bits
        AttribRow               = temp_byte + 4   ; - used to determine attribute table row
        AttribCol               = temp_byte + 5   ; - used to determine attribute table column
        GfxTblAddr              = temp_byte + 6   ; - metatile graphics table address low ( 2 bytes )        
    ; end locals
    
    
    ; store LSB of where we're at

    mb AttribCol := CurrentColumnPos & #1


    mb y, tempVRAMBufferOffset := VRAM_Buffer2_Offset        ; store vram buffer offset

    mb VRAM_Buffer2[ y + 1 ] := CurrentNTAddr_Low                      ; get current name table address we're supposed to render
    mb VRAM_Buffer2[ y     ] := CurrentNTAddr_High
    ; store length byte of 26 here with d7 set to increment by 32 (in columns)
    mb VRAM_Buffer2[ 2 + y ] := #($80+26)
    mb AttribRow := #0                                  ; init attribute row
    tax

    do
        stx tempMTileBufferOffset                 ; store init value of 0 or incremented offset for buffer

        mb AttribBits := MetatileBuffer[ x ] & #%11000000   ; get first metatile number, and mask out all but 2 MSB

        asl                                       ; note that metatile format is:
        rol                                       ; %xx000000 - attribute table bits,
        rol                                       ; %00xxxxxx - metatile number
        tay                                       ; rotate bits to d1-d0 and use as offset here

        mb GfxTblAddr      := MetatileGraphics_Low[ y ]                ; get address to graphics table from here
        mb GfxTblAddr[ 1 ] := MetatileGraphics_High[ y ]

        mb tempMetatileGfxTblOffet := MetatileBuffer[ x ] * 4                     ; multiply by 4 and use as tile offset
        
        ; get current task number for level processing and
        ; mask out all but LSB, then invert LSB, multiply by 2
        ; to get the correct column position in the metatile,
        ; then add to the tile offset so we can draw either side
        ; of the metatiles

        mb y := AreaParserTaskNum & #%00000001 ^ #%00000001 * 2 +c tempMetatileGfxTblOffet

        ldx tempVRAMBufferOffset                             ; use vram buffer offset from before as X

        ; get first tile number (top left or top right) and store
        mb VRAM_Buffer2[ 3 + x ] := (GfxTblAddr)[ y ]
        iny
        mb VRAM_Buffer2[ 4 + x ] := (GfxTblAddr)[ y ]    ; now get the second (bottom left or bottom right) and store

        ldy AttribRow                             ; get current attribute row

        ; get LSB of current column where we're at, and
        if AttribCol goto RightCheck             ; branch if set (clear = left attrib, set = right)
        ; get current row we're rendering,  branch if LSB set (clear = top left, set = bottom left)
        if temp_byte[ 1 ] >> 1 == carry clear
            rol AttribBits                     ; rotate attribute bits 3 to the left
            rol AttribBits                     ; thus in d1-d0, for upper left square
            rol AttribBits
            jmp SetAttrib
            
            RightCheck:
            
            ; get LSB of current row we're rendering branch if set (clear = top right, set = bottom right)
            if temp_byte[1] >> 1 == carry set goto NextMTRow 
            lsr AttribBits                     ; shift attribute bits 4 to the right
            lsr AttribBits                     ; thus in d3-d2, for upper right square
            lsr AttribBits
            lsr AttribBits
        else
            lsr AttribBits                     ; shift attribute bits 2 to the right
            lsr AttribBits                     ; thus in d5-d4 for lower left square
            
            NextMTRow:
            
            inc AttribRow                         ; move onto next attribute row
        endif

        SetAttrib:                                ; add new attribute bits
        mb AttributeBuffer[ y ] := AttributeBuffer[ y ] | AttribBits 
        
        inc tempVRAMBufferOffset                  ; increment vram buffer offset by 2
        inc tempVRAMBufferOffset
        ldx tempMTileBufferOffset                 ; get current gfx buffer row, and check for
        inx                                       ; the bottom of the screen
    while x < #$0d                                ; if not there yet, loop back

    ; get current vram buffer offset, increment by 3 (for name table address and length bytes)
    
    mb y := tempVRAMBufferOffset + 3
    
    lda #0
    sta VRAM_Buffer2,y                            ; put null terminator at end of data for name table
    sty VRAM_Buffer2_Offset                       ; store new buffer offset
    inc CurrentNTAddr_Low                         ; increment name table address low

    if ! CurrentNTAddr_Low & #%00011111           ; check current low byte, if no wraparound, just skip this part
        mb CurrentNTAddr_Low := #$80              ; if wraparound occurs, make sure low byte stays just under the status bar
        ; and then invert d2 of the name table address high to move onto the next appropriate name table
        mb CurrentNTAddr_High := CurrentNTAddr_High ^ #%00000100
    endif

    jmp SetVRAMCtrl                               ; jump to set buffer to VRAM_Buffer2 and leave
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RenderAttributeTables

    ; locals
        AttribAddressHI = temp_byte
        AttribAddressLO = temp_byte + 1
    ; end locals

    .export SetVRAMCtrl
    ; get low byte of next name table address to be written to, mask out all but 5 LSB,

    mb AttribAddressLO := CurrentNTAddr_Low & #%00011111 - #04 & #%00011111        ; mask out bits again and store OP

    mb a := CurrentNTAddr_High

    if carry clear                                ; invert d2 if carry not set
        eor #%00000100
    endif

    mb AttribAddressHI := a & #%00000100 | #$23   ; add $2300 to the high byte and store
    ; get low byte - 4, divide by 4, add offset for attribute table and store
    mb AttribAddressLO := AttribAddressLO / 4 +c #$c0
    ; we should now have the appropriate block of attribute table in our temp address:
    
    ldx #0
    ldy VRAM_Buffer2_Offset                       ; get buffer offset

    do                                            ; (x = 0 to 6 )
        mb VRAM_Buffer2[ y ] := AttribAddressHI
        ; get low byte, add 8 because we want to start below the status bar:
        mb VRAM_Buffer2[ 1 + y ] := AttribAddressLO + #$08
        mb AttribAddressLO := a                   ; also store in local again
        ; fetch current attribute table byte
        mb VRAM_Buffer2[ 3 + y ] := AttributeBuffer[ x ]
        mb VRAM_Buffer2[ 2 + y ] := #1            ; store length of 1 in buffer
        mb AttributeBuffer[ x ] := a >> 1         ; a = #0 clear current byte in attribute buffer
        mb y := y + 4

    while inx < #$07                              ; increment attribute offset

    mb VRAM_Buffer2[ y ] := a                     ; put null terminator at the end
    mb VRAM_Buffer2_Offset := y                   ; store offset in case we want to do any more

    SetVRAMCtrl:

    mb VRAM_Buffer_AddrCtrl := #$06               ; set buffer to VRAM_Buffer2 and leave
    rts

.endproc


    ; ------------------------------------------------------------------------------------------------

dataseg


ColorRotatePalette:
    .byte $27, $27, $27, $17, $07, $17

BlankPalette:
    .byte $3f, $0c, $04, $ff, $ff, $ff, $ff, $00
    ; used based on area type

Palette3Data:
    .byte $0f, $07, $12, $0f
    .byte $0f, $07, $17, $0f
    .byte $0f, $07, $17, $1c
    .byte $0f, $07, $17, $00
    
    ;    ----------------------------------------------------------------------------------------------
    ; flash gold color for coins and ? blocks
    ;    ----------------------------------------------------------------------------------------------

.code

.proc ColorRotation

    ; locals
        counter = temp_byte                       ; used as temporary counter in 
    ; end locals
    
    ; get frame counter, do if zero to do this every eighth frame, AND vram buffer offest  less than 49
    if !FrameCounter & #%00000111 && ldx VRAM_Buffer1_Offset < #$31

        mb y := a                                 ; set y to 0
        ; load palette address and dummy values to VRAM buffer: blank palette for palette 3
        repeat
            mb VRAM_Buffer1[ x ] := BlankPalette[ y ]
            inx
        until iny >= #$08
        
        ; overwrite blank values palette in vram buffer:

        ldx VRAM_Buffer1_Offset                   ; reload current vram buffer offset
        mb counter := #3
        mb y := AreaType * 4                      ; water: y=0, ground y=4, underground y=8, castle y = 12

        repeat
            mb VRAM_Buffer1 [ 3 + x ] := Palette3Data [ y ]
            iny
            inx
        until dec counter == negative             ; do this until the palette is all copied

        ldx VRAM_Buffer1_Offset                   ; reload current vram buffer offset
        ldy ColorRotateOffset                     ; get color cycling offset
        ; get and store current color in second color slot of palette:
        mb VRAM_Buffer1[ 4 + x ] := ColorRotatePalette [ y ]

        mb VRAM_Buffer1_Offset := VRAM_Buffer1_Offset + #$07           ; increment buffer

        inc ColorRotateOffset                     ; increment color cycling offset
        if ColorRotateOffset >= #$06              ; check to see if it's still in range
            mb ColorRotateOffset := #0            ; otherwise, init to keep it in range
        endif

    endif
    rts                                           ; leave

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

BlockGfxData:
    .byte $45, $45, $47, $47
    .byte $47, $47, $47, $47
    .byte $57, $58, $59, $5a
    .byte $24, $24, $24, $24
    .byte $26, $26, $26, $26

    ; ------------------------------------------------------------------------------------------------

.code

.proc RemoveCoin_Axe

    ldy #<VRAM_Buffer2                            ; set low byte so offset points to VRAM_Buffer2
    lda #3                                        ; load offset for default blank metatile

    if ldx AreaType == zero                       ; check area type,  if water load offset for blank metatile used in water
        mb a := #4
    endif

    jsr PutBlockMetatile                          ; do a sub to write blank metatile to vram buffer

    mb VRAM_Buffer_AddrCtrl := #$06               ; set vram address controller to VRAM_Buffer2 and leave

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ReplaceBlockMetatile

    jsr WriteBlockMetatile                        ; write metatile to vram buffer to replace block object
    inc Block_ResidualCounter                     ; increment unused counter (residual code)
    dec Block_RepFlag,x                           ; decrement flag (residual code)
    rts                                           ; leave

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DestroyBlockMetatile
    lda #0                            ; force blank metatile if branched/jumped to this point, fall through to next proc
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc WriteBlockMetatile
    ; logic for different block types

    ldy #3                                        ; load offset for blank metatile
    ; check contents of A for blank metatile, branch if blank
    ; (unconditional if branched from DestroyBlockMetatile )
    if a = #0 goto UseBufferOffset                

    ldy #0                                        ; load offset for brick metatile w/ line
    ; use offset if metatile is brick with coins (w/ line) OR breakable brick w/ line
    if a = #$58 || a = #$51  goto UseBufferOffset 

    iny                                           ; 1
    ; use offset if metatile is brick with coins (w/o line) OR is breakable brick w/o line
    if a = #$5d || a = #$52  goto UseBufferOffset        
    ; if any other metatile, increment offset for empty block
    iny                                           ; 2

    UseBufferOffset:

    tya                                           ; put Y in A
    ldy VRAM_Buffer1_Offset                       ; get vram buffer offset
    iny                                           ; move onto next byte
    jsr PutBlockMetatile                          ; get appropriate block data and write to vram buffer

    MoveVOffset:

    dey                                           ; decrement vram buffer offset add 10 bytes to it
    mb a := y
    mb a := a + #10
    jmp SetVRAMOffset                             ; branch to store as new vram buffer offset

.endproc


.proc PutBlockMetatile
    ; Draw a block to VRAM_Buffer1
    
    ; Entry:    x = offset control bit
    ;           y = VramBufferOffset + 1 .. why plus 1?
    ;           a = metatile offset
    ; locals
        OffsetCtrlBit      = temp_byte            ; - temp store for offset control bit
        VramBufferOffset   = temp_byte + 1        ; - temp vram buffer offset
        VerticalHighNybble = temp_byte + 2        ; - temp store for vertical high nybble in block buffer routine
        AdderHighByteNT    = temp_byte + 3        ; - temp adder for high byte of name table addres
        NametableAddrLO    = temp_byte + 4        ; - name table address low/high
        NametableAddrHI    = temp_byte + 5        ; - name table address low/high
        BlockBufferPointer = temp_byte + 6        ; - block buffer address low/high
    ; end locals

    stx OffsetCtrlBit                             ; store control bit from SprDataOffset_Ctrl
    sty VramBufferOffset                          ; store vram buffer offset for next byte
    mb x := a * 4                                 ; multiply A by four and use as X

    ldy #$20                                      ; load high byte for name table 0
    ; get low byte of block buffer pointer check to see if we're
    ; on odd-page block buffer if not, use current high byte
    if lda BlockBufferPointer >= #$D0
        ldy #$24                                  ; load high byte for name table 1
    endif

    sty AdderHighByteNT

    mb NametableAddrLO := a & #$0f * 2            ; BlockBufferPointer low nybble * 2
    mb NametableAddrHI := #0

    mb a := VerticalHighNybble + #$20             ; add 32 pixels for the status bar

    asl
    rol NametableAddrHI
    asl
    rol NametableAddrHI

    mb NametableAddrLO := a +c NametableAddrLO
    mb NametableAddrHI := NametableAddrHI +c #0 + AdderHighByteNT

    ldy VramBufferOffset

    RemBridge:
    ; write top left and top right tile numbers into first spot
    mb VRAM_Buffer1[ 2 + y ] := BlockGfxData[ x ]
    mb VRAM_Buffer1[ 3 + y ] := BlockGfxData[ 1 + x ]
    ; write bottom left and bottom right tiles numbers into second spot
    mb VRAM_Buffer1[ 7 + y ] := BlockGfxData[ 2 + x ]
    mb VRAM_Buffer1[ 8 + y ] := BlockGfxData[ 3 + x ]
    ; write low byte of name table into first slot
    mb VRAM_Buffer1[ y ] := NametableAddrLO
    ; add 32 bytes to value, write low byte of name table to into second slot
    mb VRAM_Buffer1[ 5 + y ] := a + #$20
    ; write high byte of name table address to both slots
    mb VRAM_Buffer1[-1 + y ] := NametableAddrHI
    mb VRAM_Buffer1[ 4 + y ] := a
    ; put length of 2 into both slots
    mb VRAM_Buffer1[ 1 + y ] := #2
    mb VRAM_Buffer1[ 6 + y ] := a
    ; put null terminator at end
    mb VRAM_Buffer1[ 9 + y ] := #0

    ldx OffsetCtrlBit                             ; get offset control bit here
    rts                                           ; and leave

.endproc

    ; ------------------------------------------------------------------------------------------------
    ; METATILE GRAPHICS TABLE

dataseg


MetatileGraphics_Low:
    .byte <Palette0_MTiles, <Palette1_MTiles, <Palette2_MTiles, <Palette3_MTiles

MetatileGraphics_High:
    .byte >Palette0_MTiles, >Palette1_MTiles, >Palette2_MTiles, >Palette3_MTiles

Palette0_MTiles:
    .byte $24, $24, $24, $24                      ; blank
    .byte $27, $27, $27, $27                      ; black metatile
    .byte $24, $24, $24, $35                      ; bush left
    .byte $36, $25, $37, $25                      ; bush middle
    .byte $24, $38, $24, $24                      ; bush right
    .byte $24, $30, $30, $26                      ; mountain left
    .byte $26, $26, $34, $26                      ; mountain left bottom/middle center
    .byte $24, $31, $24, $32                      ; mountain middle top
    .byte $33, $26, $24, $33                      ; mountain right
    .byte $34, $26, $26, $26                      ; mountain right bottom
    .byte $26, $26, $26, $26                      ; mountain middle bottom
    .byte $24, $c0, $24, $c0                      ; bridge guardrail
    .byte $24, $7f, $7f, $24                      ; chain
    .byte $b8, $ba, $b9, $bb                      ; tall tree top, top half
    .byte $b8, $bc, $b9, $bd                      ; short tree top
    .byte $ba, $bc, $bb, $bd                      ; tall tree top, bottom half
    .byte $60, $64, $61, $65                      ; warp pipe end left, points up
    .byte $62, $66, $63, $67                      ; warp pipe end right, points up
    .byte $60, $64, $61, $65                      ; decoration pipe end left, points up
    .byte $62, $66, $63, $67                      ; decoration pipe end right, points up
    .byte $68, $68, $69, $69                      ; pipe shaft left
    .byte $26, $26, $6a, $6a                      ; pipe shaft right
    .byte $4b, $4c, $4d, $4e                      ; tree ledge left edge
    .byte $4d, $4f, $4d, $4f                      ; tree ledge middle
    .byte $4d, $4e, $50, $51                      ; tree ledge right edge
    .byte $6b, $70, $2c, $2d                      ; mushroom left edge
    .byte $6c, $71, $6d, $72                      ; mushroom middle
    .byte $6e, $73, $6f, $74                      ; mushroom right edge
    .byte $86, $8a, $87, $8b                      ; sideways pipe end top
    .byte $88, $8c, $88, $8c                      ; sideways pipe shaft top
    .byte $89, $8d, $69, $69                      ; sideways pipe joint top
    .byte $8e, $91, $8f, $92                      ; sideways pipe end bottom
    .byte $26, $93, $26, $93                      ; sideways pipe shaft bottom
    .byte $90, $94, $69, $69                      ; sideways pipe joint bottom
    .byte $a4, $e9, $ea, $eb                      ; seaplant
    .byte $24, $24, $24, $24                      ; blank, used on bricks or blocks that are hit
    .byte $24, $2f, $24, $3d                      ; flagpole ball
    .byte $a2, $a2, $a3, $a3                      ; flagpole shaft
    .byte $24, $24, $24, $24                      ; blank, used in conjunction with vines

Palette1_MTiles:
    .byte $a2, $a2, $a3, $a3                      ; vertical rope
    .byte $99, $24, $99, $24                      ; horizontal rope
    .byte $24, $a2, $3e, $3f                      ; left pulley
    .byte $5b, $5c, $24, $a3                      ; right pulley
    .byte $24, $24, $24, $24                      ; blank used for balance rope
    .byte $9d, $47, $9e, $47                      ; castle top
    .byte $47, $47, $27, $27                      ; castle window left
    .byte $47, $47, $47, $47                      ; castle brick wall
    .byte $27, $27, $47, $47                      ; castle window right
    .byte $a9, $47, $aa, $47                      ; castle top w/ brick
    .byte $9b, $27, $9c, $27                      ; entrance top
    .byte $27, $27, $27, $27                      ; entrance bottom
    .byte $52, $52, $52, $52                      ; green ledge stump
    .byte $80, $a0, $81, $a1                      ; fence
    .byte $be, $be, $bf, $bf                      ; tree trunk
    .byte $75, $ba, $76, $bb                      ; mushroom stump top
    .byte $ba, $ba, $bb, $bb                      ; mushroom stump bottom
    .byte $45, $47, $45, $47                      ; breakable brick w/ line
    .byte $47, $47, $47, $47                      ; breakable brick
    .byte $45, $47, $45, $47                      ; breakable brick (not used)
    .byte $b4, $b6, $b5, $b7                      ; cracked rock terrain
    .byte $45, $47, $45, $47                      ; brick with line (power-up)
    .byte $45, $47, $45, $47                      ; brick with line (vine)
    .byte $45, $47, $45, $47                      ; brick with line (star)
    .byte $45, $47, $45, $47                      ; brick with line (coins)
    .byte $45, $47, $45, $47                      ; brick with line (1-up)
    .byte $47, $47, $47, $47                      ; brick (power-up)
    .byte $47, $47, $47, $47                      ; brick (vine)
    .byte $47, $47, $47, $47                      ; brick (star)
    .byte $47, $47, $47, $47                      ; brick (coins)
    .byte $47, $47, $47, $47                      ; brick (1-up)
    .byte $24, $24, $24, $24                      ; hidden block (1 coin)
    .byte $24, $24, $24, $24                      ; hidden block (1-up)
    .byte $ab, $ac, $ad, $ae                      ; solid block (3-d block)
    .byte $5d, $5e, $5d, $5e                      ; solid block (white wall)
    .byte $c1, $24, $c1, $24                      ; bridge
    .byte $c6, $c8, $c7, $c9                      ; bullet bill cannon barrel
    .byte $ca, $cc, $cb, $cd                      ; bullet bill cannon top
    .byte $2a, $2a, $40, $40                      ; bullet bill cannon bottom
    .byte $24, $24, $24, $24                      ; blank used for jumpspring
    .byte $24, $47, $24, $47                      ; half brick used for jumpspring
    .byte $82, $83, $84, $85                      ; solid block (water level, green rock)
    .byte $24, $47, $24, $47                      ; half brick (???)
    .byte $86, $8a, $87, $8b                      ; water pipe top
    .byte $8e, $91, $8f, $92                      ; water pipe bottom
    .byte $24, $2f, $24, $3d                      ; flag ball (residual object)

Palette2_MTiles:
    .byte $24, $24, $24, $35                      ; cloud left
    .byte $36, $25, $37, $25                      ; cloud middle
    .byte $24, $38, $24, $24                      ; cloud right
    .byte $24, $24, $39, $24                      ; cloud bottom left
    .byte $3a, $24, $3b, $24                      ; cloud bottom middle
    .byte $3c, $24, $24, $24                      ; cloud bottom right
    .byte $41, $26, $41, $26                      ; water/lava top
    .byte $26, $26, $26, $26                      ; water/lava
    .byte $b0, $b1, $b2, $b3                      ; cloud level terrain
    .byte $77, $79, $77, $79                      ; bowser's bridge

Palette3_MTiles:
    .byte $53, $55, $54, $56                      ; question block (coin)
    .byte $53, $55, $54, $56                      ; question block (power-up)
    .byte $a5, $a7, $a6, $a8                      ; coin
    .byte $c2, $c4, $c3, $c5                      ; underwater coin
    .byte $57, $59, $58, $5a                      ; empty block
    .byte $7b, $7d, $7c, $7e                      ; axe

    ; ------------------------------------------------------------------------------------------------
    ; VRAM BUFFER DATA FOR LOCATIONS IN PRG-ROM

WaterPaletteData:
    .byte $3f, $00, $20
    .byte $0f, $15, $12, $25
    .byte $0f, $3a, $1a, $0f
    .byte $0f, $30, $12, $0f
    .byte $0f, $27, $12, $0f
    .byte $22, $16, $27, $18
    .byte $0f, $10, $30, $27
    .byte $0f, $16, $30, $27
    .byte $0f, $0f, $30, $10
    .byte $00

GroundPaletteData:
    .byte $3f, $00, $20
    .byte $0f, $29, $1a, $0f
    .byte $0f, $36, $17, $0f
    .byte $0f, $30, $21, $0f
    .byte $0f, $27, $17, $0f
    .byte $0f, $16, $27, $18
    .byte $0f, $1a, $30, $27
    .byte $0f, $16, $30, $27
    .byte $0f, $0f, $36, $17
    .byte $00

UndergroundPaletteData:
    .byte $3f, $00, $20
    .byte $0f, $29, $1a, $09
    .byte $0f, $3c, $1c, $0f
    .byte $0f, $30, $21, $1c
    .byte $0f, $27, $17, $1c
    .byte $0f, $16, $27, $18
    .byte $0f, $1c, $36, $17
    .byte $0f, $16, $30, $27
    .byte $0f, $0c, $3c, $1c
    .byte $00

CastlePaletteData:
    .byte $3f, $00, $20
    .byte $0f, $30, $10, $00
    .byte $0f, $30, $10, $00
    .byte $0f, $30, $16, $00
    .byte $0f, $27, $17, $00
    .byte $0f, $16, $27, $18
    .byte $0f, $1c, $36, $17
    .byte $0f, $16, $30, $27
    .byte $0f, $00, $30, $10
    .byte $00

DaySnowPaletteData:
    .byte $3f, $00, $04
    .byte $22, $30, $00, $10
    .byte $00

NightSnowPaletteData:
    .byte $3f, $00, $04
    .byte $0f, $30, $00, $10
    .byte $00

MushroomPaletteData:
    .byte $3f, $00, $04
    .byte $22, $27, $16, $0f
    .byte $00

BowserPaletteData:
    .byte $3f, $14, $04
    .byte $0f, $1a, $30, $27
    .byte $00

MarioThanksMessage:
    .byte $25, $48, $10
    .byte "THANK YOU MARIO!", 0

LuigiThanksMessage:
    .byte $25, $48, $10
    .byte "THANK YOU LUIGI!", 0

MushroomRetainerSaved:
    .byte $25, $c5, $16
    .byte "BUT OUR PRINCESS IS IN"
    .byte $26, $05, $0f
    .byte "ANOTHER CASTLE!", 0

PrincessSaved1:
    .byte $25, $a7, $13
    .byte "YOUR QUEST IS OVER.", 0

PrincessSaved2:
    .byte $25, $e3, $1b
    .byte "WE PRESENT YOU A NEW QUEST.", 0

WorldSelectMessage1:
    .byte $26, $4a, $0d
    .byte "PUSH BUTTON B", 0

WorldSelectMessage2:
    .byte $26, $88, $11
    .byte "TO SELECT A WORLD", 0

    ; ------------------------------------------------------------------------------------------------

.code

.proc JumpEngine
    ; note that if an RTS is performed in next routine
    ; it will return to the execution before the sub
    ; that called this routine

    JumpTableAddr   = temp_byte + 4
    Pointer         = temp_byte + 6

    mb y := a * 2                                 ; number of index in list to jump to * 2 (two bytes per item)
    pla                                           ; pull saved return address from stack
    mb JumpTableAddr[ 0 ] := a                    ; save to indirect
    pla
    mb JumpTableAddr[ 1 ] := a

    mb y := y + 1                                 ; fix rts address
    mb Pointer[ 0 ] := (JumpTableAddr)[ y ]

    mb y := y + 1
    mb Pointer[ 1 ] := (JumpTableAddr)[ y ]

    jmp (Pointer)                                 ; jump to the address we loaded

.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc InitializeNameTables

    lda PPU_STATUS                                ; reset flip-flop

    mb a := Mirror_PPU_CTRL | #%00010000 & #%11110000  ; set sprites for first 4k and background for second 4k, clear low half
    jsr WritePPU_CTRL

    lda #$24                                      ; set vram address to start of name table 1
    jsr WriteNTAddr
    lda #$20                                      ; and then set it to name table 0

    WriteNTAddr:

    sta PPU_ADDRESS
    lda #0
    sta PPU_ADDRESS

    ldx #4
    ldy #$c0
    lda # ' '                                     ; clear name table with blank tile
    repeat
        repeat                                    ; 960 , $3c0h needs to be cleared
            sta PPU_DATA                          ; first loop is only $c0 (192) three more loops are 3 * 256 = 960 bytes exactly
        until dey == zero
    until dex == zero

    ldy #64                                       ; now to clear the attribute table (with zero this time)
    mb a := x                                     ; x is zero
    sta VRAM_Buffer1_Offset                       ; init vram buffer 1 offset
    sta VRAM_Buffer1                              ; init vram buffer 1

    repeat
        sta PPU_DATA
    until dey == zero

    sta HorizontalScroll                          ; reset scroll variables
    sta VerticalScroll
    jmp InitScroll                                ; initialize scroll registers to zero and rts
.endproc
   
    ; ------------------------------------------------------------------------------------------------
    
.proc ReadJoypads


    ; temp_byte - temp joypad bit
    
    ; reads from JOYPAD_PORT in order: A, B, Select, Start, Up, Down, Left, Right.
    ; stored in same order

    lda #1                                        ; reset and clear strobe of joypad ports
    sta JOYPAD_PORT
    lsr
    mb x := a                                     ; start with joypad 1's port
    sta JOYPAD_PORT
    jsr ReadPortBits
    inx                                           ; increment for joypad 2's port


    ReadPortBits:

    ldy #$08
    repeat
        pha                                       ; push previous bit onto stack
            ; read current bit on joypad port
            mb temp_byte := JOYPAD_PORT[ x ]      ; check d1 and d0 of port output
            lsr                                   ; this is necessary on the old
            ora temp_byte                         ; famicom systems in japan
            lsr
        pla                                       ; read bits from stack
        rol                                       ; rotate bit from carry flag
    until dey == zero

    sta SavedJoypadBits,x                         ; save controller status here always
    pha
    ; check for select or start is currently pressed AND was pressed last time
    ; if it was pressed last time, mask it out - start or select cannot ever be read again until
    ; the button is released

    if a & #( BUTTON_SELECT | BUTTON_START ) & JoypadBitMask[ x ]
        pla
        mb SavedJoypadBits[ x ] := a & #( <~( BUTTON_SELECT | BUTTON_START ) )  ; block select and start bits and leave
        rts
    endif

    pla
    sta JoypadBitMask,x                           ; save with all bits in place and leave
    rts
.endproc

    ; nothing actually calls this directly, entry is at UpdateScreen
    
.proc WriteBufferToScreen                         

    ; Buffer format is ppu_address_hi, ppu_address_low, length, data, data..., $00 = end
    ; length => bit7 set = increment PPU addess by 32, bit6 set = repeat next byte, bit5-bit0 = num of data bytes

    .export InitScroll, UpdateScreen

    ; locals
        VRAM_Buffer_pointer = temp_byte
    ; end locals
    
    repeat

        mb PPU_ADDRESS := a                       ; store high byte of vram address
        iny
        mb PPU_ADDRESS := (VRAM_Buffer_pointer)[ y ]                   ; store low byte of vram address
        iny

        mb a := (VRAM_Buffer_pointer)[ y ] << 1   ; load next byte (third), to left and save in stack
        pha

            mb a := Mirror_PPU_CTRL | #%00000100                       ; set ppu to increment by 32 by default

            if carry clear                        ; if d7 of third byte was clear, ppu will
                and #%11111011                    ; only increment by 1
            endif

            jsr WritePPU_CTRL                     ; write contents of A to PPU_CTRL and Mirror_PPU_CTRL

        pla                                       ; pull from stack

        if a << 1 == carry set                    ; if d6 of third byte was set, repeat byte
            ora #%00000010                        ; set d1 and increment Y once since it won't be inc in the loop
            iny
        endif

        mb a := a >> 2                            ; shift back to the right to get proper length, d1 will now be in carry
        mb x := a

        repeat
            if carry clear                        ; if carry clear, increment index
                iny                               ; otherwise, skip this and load same byte
            endif
            mb PPU_DATA := (VRAM_Buffer_pointer)[ y ]
        until dex == zero                         ; repeat for data size in reg x

        sec

        mb a := y
        mb VRAM_Buffer_pointer := a +c VRAM_Buffer_pointer             ; add end length plus one to the indirect at VRAM_Buffer_pointer

        mb VRAM_Buffer_pointer[ 1 ] := #0 +c VRAM_Buffer_pointer[ 1 ]                ; add carry to high byte of pointer

        mb PPU_ADDRESS := #$3f                    ; residual code?
        mb PPU_ADDRESS := #0                      ; sets ppu address to $3f00
        sta PPU_ADDRESS                           ; then reinitializes it for some reason
        sta PPU_ADDRESS

        UpdateScreen:                             ; NMI jumps into this code here

        ldx PPU_STATUS                            ; reset flip-flop
        ldy #0                                    ; load first byte from indirect as a pointer

    until lda (VRAM_Buffer_pointer)[ y ] == zero  ; if byte is zero we have no further updates to make here

    InitScroll:
    sta PPU_SCROLL                                ; store contents of A into scroll registers
    sta PPU_SCROLL                                ; and end whatever subroutine led us here
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc WritePPU_CTRL
    sta PPU_CTRL                                  ; write contents of A to PPU register 1
    sta Mirror_PPU_CTRL                           ; and its mirror
    rts
.endproc


dataseg

StatusBarData:
    .byte $f0, $06                                ;  top score display on title screen
    .byte $62, $06                                ;  player score
    .byte $62, $06
    .byte $6d, $02                                ;  coin tally
    .byte $6d, $02
    .byte $7a, $03                                ;  game timer

StatusBarOffset:
    .byte $06, $0c, $12, $18, $1e, $24

    ; ------------------------------------------------------------------------------------------------

.code

.proc PrintStatusBarNumbers
    sta temp_byte                                 ; store player-specific offset
    jsr OutputNumbers                             ; use first nybble to print the coin display

    lda temp_byte                                 ; move high nybble to low
    mb a := a >> 4                                ; and print to score display
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc OutputNumbers

    number_length = temp_byte + 3
    save_x        = temp_byte + 2

    clc                                           ; add 1 to low nybble
    adc #1
    and #%00001111                                ; mask out high nybble

    if a < #$06

        pha                                       ; save incremented value to stack for now and
          mb y := a << 1                          ; shift to left and use as offset
          ldx VRAM_Buffer1_Offset                 ; get current buffer pointer
          lda #$20                                ; put at top of screen by default

          if y = #0                               ; are we writing top score on title screen?
              lda #$22                            ; if so, put further down on the screen
          endif

          sta VRAM_Buffer1,x
          
          ; write low vram address and length of thing we're printing to the buffer
          mb VRAM_Buffer1   [ 1 + x ] := StatusBarData[ y ]            
          mb a, VRAM_Buffer1[ 2 + x ] := StatusBarData[ 1 + y ]        ; length
          mb number_length := a                   ; save length byte in counter

          mb save_x := x          ; and buffer pointer elsewhere for now ;OP just reload it from VRAM_Buffer1_Offset
        pla                       ; pull original incremented value from stack

        mb x := a
        ; load offset to value we want to write
        ; subtract from length byte we read before
        mb y := StatusBarOffset[ x ] - StatusBarData[ 1 + y ]          ; use value as offset to display digits
        ldx save_x

        repeat
            mb VRAM_Buffer1[ 3 + x ] := DisplayDigits[ y ]
            inx
            iny
        until dec number_length == zero           ; do this until all the digits are written

        mb VRAM_Buffer1[ 3 + x ] := #0            ; put null terminator at end
        mb VRAM_Buffer1_Offset := x + 3           ; increment buffer pointer by 3
    endif

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DigitsMathRoutine

    if OperMode <> #MODE_TITLESCREEN
        ldx #$05
        repeat

            mb a := DigitModifier[ x ] + DisplayDigits[ y ]            ; add to current digit

            if negative goto BorrowOne            ; if result is a negative number, branch to subtract
            if a >= #10  goto CarryOne            ; if digit greater than $09, branch to add

            StoreNewD:                            ; return here

            mb DisplayDigits[ y ] := a            ; store as new score or game timer digit
            dey                                   ; move onto next digits in score or game timer
        until dex == negative

    endif
    ; bug here? It loops from 6 to 0 (7 bytes). DigitModifier is 6 bytes
    lda #0                                        ; store zero here
    ldx #$06                                      ; start with the last digit
    repeat                                        ; x = 6 to 0
        mb DigitModifier[ x - 1 ] := a            ; #0     ; initialize the digit amounts to increment
    until dex == negative                         ; do this until they're all reset, then leave

    rts

    ; ------------------------------------------------------------------------------------------------

    BorrowOne:
    dec DigitModifier-1,x                         ; decrement the previous digit, then put $09 in
    lda #$09                                      ; the game timer digit we're currently on to "borrow
    bne StoreNewD                                 ; the one", then do an unconditional branch back

    CarryOne:
    sec                                           ; subtract ten from our digit to make it a
    sbc #10                                       ; proper BCD number, then increment the digit
    inc DigitModifier-1,x                         ; preceding current digit to "carry the one" properly
    jmp StoreNewD                                 ; go back to just after we branched here

.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc UpdateTopScore

    ldx #$05                                      ; start with mario's score
    jsr TopScoreCheck
    ldx #$0b                                      ; now do luigi's score

    TopScoreCheck:

    ldy #$05                                      ; start with the lowest digit
    sec

    repeat
        lda PlayerScoreDisplay,x                  ; subtract each player digit from each high score digit
        sbc TopScoreDisplay,y                     ; from lowest to highest, if any top score digit exceeds
        dex                                       ; any player digit, borrow will be set until a subsequent
    until dey == negative                         ; subtraction clears it (player digit is higher than top)

    if carry set                                  ; check to see if borrow is still set, if so, set high score
        inx                                       ; increment X and Y once to the start of the score
        iny
        do
            lda PlayerScoreDisplay,x              ; store player's score digits into high score memory area
            sta TopScoreDisplay,y
            inx
            iny
        while  y < #$06                           ; do this until we have stored them all
    endif

    rts

.endproc

dataseg

DefaultSprOffsets:
    .byte $04, $30, $48, $60, $78, $90, $a8, $c0
    .byte $d8, $e8, $24, $f8, $fc, $28, $2c

Sprite0Data:
    .byte $18, $ff, $23, $58

.code

.proc InitializeGame

    ldy #$6f                                      ; clear all memory as in initialization procedure,
    jsr InitializeMemory                          ; but this time, clear only as far as $076f

    ldy #$1f                                      ; 31

    repeat
        sta SoundMemory,y                         ; clear out memory used
    until dey == negative                         ; by the sound engines

    lda #$18                                      ; set demo timer
    sta DemoTimer

    jsr LoadAreaPointer

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitializeArea

    ldy #$4b                                      ; clear all memory again, only as far as $074b
    jsr InitializeMemory                          ; this is only necessary if branching from

    ldx #$21
    lda #0

    repeat
        sta Timers,x                              ; clear out memory between
    until dex == negative                         ; $0780 and $07a1

    lda HalfwayPage
    if ldy AltEntranceControl != zero             ; if AltEntranceControl not set, use halfway page, if any found
        lda EntrancePage                          ; otherwise use saved entry page number here
    endif

    sta ScreenLeft_PageLoc                        ; set as value here
    sta CurrentPageLoc                            ; also set as current page
    sta BackloadingFlag                           ; set flag here if halfway page or saved entry page number found

    jsr GetScreenPosition                         ; get pixel coordinates for screen borders

    ldy #$20                                      ; if on odd numbered page, use $2480 as start of rendering

    if and #%00000001 != zero                     ; otherwise use $2080, this address used later as name table
        ldy #$24                                  ; address for rendering of game area
    endif

    sty CurrentNTAddr_High                        ; store name table address
    ldy #$80
    sty CurrentNTAddr_Low
    mb BlockBufferColumnPos := a << 4             ; store LSB of page number in high nybble of block buffer column position


    dec AreaObjectLength                          ; set area object lengths for all empty
    dec AreaObjectLength+1
    dec AreaObjectLength+2
    ; set value for renderer to update 12 column sets
    mb ColumnSets := #$0b                         ; 12 column sets = 24 metatile columns = 1 1/2 screens

    jsr GetAreaDataAddrs                          ; get enemy and level addresses and load header
    
    ; if PrimaryHardMode OR ( WorldNumber > World 5 )   OR ( Wordlnumber = world 5 AND Levelnumber >= LEVEL3 )
    ; (if WorldNumber >= #WORLD5  is true then "not equal" is evaluated. If not equal it means that at this step it  greater than 5
    ; so it is if Worldnumber >= 5 && ( greater than 5 OR level greater or equal to 3 )


    if PrimaryHardMode || WorldNumber >= #WORLD5 && ( (not equal) ||  LevelNumber >= #LEVEL3   )
        inc SecondaryHardMode                     ; set secondary hard mode flag for areas 5-3 and beyond
    endif

    if HalfwayPage
        mb PlayerEntranceCtrl := #2               ; if halfway page set, overwrite start position from header
    endif


    mb AreaMusicQueue := #MUSIC_Silence           ; silence music
    mb DisableScreenFlag := #1                    ; disable screen output
    inc OperMode_Task                             ; increment one of the modes
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PrimaryGameSetup

    mb a := #1
    mb FetchNewGameTimerFlag := a                 ; set flag to load game timer from header
    mb PlayerSize := a                            ; set player's size to small

    mb a := #2
    mb NumberofLives := a                         ; give each player three lives
    mb OffScr_NumberofLives := a

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc SecondaryGameSetup

    mb DisableScreenFlag := #0                    ; enable screen output
    tay                                           ; y := #0

    repeat                                        ; y = 0 to 255
        mb VRAM_Buffer1[ y - 1 ] := a             ; #0       ; clear buffer at $0300-$03ff
    until iny == zero

    sta GameTimerExpiredFlag                      ; clear game timer exp flag
    sta DisableIntermediate                       ; clear skip lives display flag
    sta BackloadingFlag                           ; clear value here

    mb BalPlatformAlignment := #$ff               ; initialize balance platform assignment flag

    lda ScreenLeft_PageLoc                        ; get left side page location
    lsr Mirror_PPU_CTRL                           ; shift LSB of ppu register #1 mirror out
    and #1                                        ; mask out all but LSB of page location
    ror                                           ; rotate LSB of page location into carry then onto mirror
    rol Mirror_PPU_CTRL                           ; this is to set the proper PPU name table

    jsr GetAreaMusic                              ; load proper music into queue

    mb SprShuffleAmt[ 2 ] := #$38                 ; load sprite shuffle amounts to be used later
    mb SprShuffleAmt[ 1 ] := #$48
    mb SprShuffleAmt[ 0 ] := #$58

    ldx #$0e                                      ; load default OAM offsets into $06e4-$06f2
    repeat                                        ; x = 14 to 0
        mb SprDataOffset[ x ] := DefaultSprOffsets[ x ]
    until dex == negative                         ; do this until they're all set

    ldy #3                                        ; set up sprite #0
    repeat                                        ; y = 3 to 0
        mb Sprite[ y ]::Data := Sprite0Data[ y ]
    until dey == negative

    jsr DoNothing2                                ; OP these jsrs doesn't do anything useful
    jsr DoNothing1
    inc Sprite0HitDetectFlag                      ; set sprite #0 check flag
    inc OperMode_Task                             ; increment to next task
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitializeMemory
    ; Clear ram, but skip the top of the stack and start with the value in reg y for page 7
    ; If called by reset:
    ; If warm boot, start at $07D7, which will leave the following memory alone:
    ;
    ; TopScoreDisplay,DisplayDigits,PlayerScoreDisplay,
    ; ScoreAndCoinDisplay,GameTimerDisplay,WorldSelectEnableFlag
    ; ContinueWorld,WarmBootValidation
    ;
    ; Otherwise y will start at $07fe (clear everything but WarmBootValidation )
    ;
    ; also called by InitializeArea, and InitializeGame
    ;
    ; locals
        RAMPointer = temp_byte + 6
    ; end locals
    

    ldx #$07                                      ; set initial high byte to $0700-$07ff
    mb a, RAMPointer := #0                           ; set initial low byte to start of page

    repeat

        mb RAMPointer[ 1 ] := x
        repeat
            if x <> #1  ||  y < #$60              ; $0160-$01ff = do not clear (leave stack alone)
                mb (RAMPointer)[ y ] := a            ; #0
            endif
        until y - 1 = #$FF                        ; do this all bytes in page have been erased (could be 'until negative')

    until dex == negative                         ; do this until all pages of memory have been erased
    rts

.endproc

dataseg

MusicSelectData:
    .byte MUSIC_Water, MUSIC_Ground, MUSIC_Underground, MUSIC_Castle
    .byte MUSIC_Cloud, MUSIC_PipeIntro

.code

    ; ------------------------------------------------------------------------------------------------

.proc GetAreaMusic

    ; if in title screen mode, do nothing
    if lda OperMode                               

        ; check for specific alternate mode of entry, 
        ; if found, skip this without checking starting position,from area object data header
        if AltEntranceControl <> #2               

            ldy #$05                              ; select music for pipe intro scene by default
            lda PlayerEntranceCtrl
            ; check value from level header for certain values 
            ; load music for pipe intro scene if header: start position either value $06 or $07
            if a = #$06 || a = #$07 goto StoreMusic                    

        endif

        ldy AreaType                              ; load area type as offset for music bit

        if CloudTypeOverride                      ; check for cloud type override
            ldy #4                                ; select music for cloud type level if found
        endif

        StoreMusic:

        mb AreaMusicQueue := MusicSelectData[ y ]             ; otherwise select appropriate music for level type
    endif

    rts
.endproc

dataseg

PlayerStarting_X_Pos:
    .byte $28, $18
    .byte $38, $28

AltYPosOffset:
    .byte $08, $00

PlayerStarting_Y_Pos:
    .byte $00, $20, $b0, $50, $00, $00, $b0, $b0
    .byte $f0

PlayerBGPriorityData:
    .byte $00, $20, $00, $00, $00, $00, $00, $00

GameTimerData:
    .byte $20                                     ; dummy byte, used as part of bg priority data
    .byte $04, $03, $02

.code

    ; ------------------------------------------------------------------------------------------------
    
.proc Entrance_GameTimerSetup

    mb Player_PageLoc    := ScreenLeft_PageLoc    ; set current page for area objects as page location for player
    mb VerticalForceDown := #$28                  ; for fractional movement downwards if necessary
    mb PlayerFacingDir   := #1                    ; set high byte of player position set facing direction so that player faces right
    sta Player_Y_HighPos

    mb Player_State := #0                         ; set player state to on the ground by default
    dec Player_CollisionBits                      ; initialize player's collision bits

    mb y, HalfwayPage := #0                       ; initialize halfway page

    if AreaType == zero                           ; check area type: if water type, set swimming flag, otherwise do not set
        iny
    endif

    sty SwimmingFlag
    ldx PlayerEntranceCtrl                        ; get starting position loaded from header


    if ldy AltEntranceControl && y <> #1
        mb x := AltYPosOffset[ y - 2 ]            ; if not 0 or 1, override $0710 with new offset in X
    endif
    
    ; load appropriate horizontal position and vertical positions for the player, using
    ; AltEntranceControl as offset for horizontal and either $0710 or value that overwrote $0710 as offset for vertical
    ; set player sprite attributes using offset in X

    mb Player_X_Position := PlayerStarting_X_Pos[ y ]                  
    mb Player_Y_Position := PlayerStarting_Y_Pos[ x ]                  
    mb Player_SprAttrib  := PlayerBGPriorityData[ x ]                  
    jsr GetPlayerColors                           ; get appropriate player palette

    
    ; if game timer is set and game timer flag is also set, use value of game timer control for first digit of game timer
    if ldy GameTimerSetting && FetchNewGameTimerFlag
    
        mb GameTimerDisplay := GameTimerData[ y ]                      

        mb GameTimerDisplay[ 2 ] := #1            ; set last digit of game timer to 1
        mb GameTimerDisplay[ 1 ] := a >> 1        ; #0            ; set second digit of game timer
        sta FetchNewGameTimerFlag                 ; clear flag for game timer reset
        sta StarInvincibleTimer                   ; clear star mario timer

    endif

    if ldy JoypadOverride

        mb Player_State := #3                     ; set player state to climbing
        ldx #0                                    ; set offset for first slot, for block object
        jsr InitBlock_XY_Pos
        mb Block_Y_Position := #$f0               ; set vertical coordinate for block object
        ldx #$05                                  ; set offset in X for last enemy object buffer slot
        ldy #0                                    ; set offset in Y for object coordinates used earlier
        jsr Setup_Vine                            ; do a sub to grow vine

    endif

    if ldy AreaType == zero                       ; if level water-type:
        jsr SetupBubble                           ;  execute sub to set up air bubbles
    endif

    
    mb GameEngineSubroutine := #$07               ; set to run player entrance subroutine
    rts

.endproc


dataseg

    ; page numbers are in order from -1 to -4

HalfwayPageNybbles:

    .byte $56, $40
    .byte $65, $70
    .byte $66, $40
    .byte $66, $40
    .byte $66, $40
    .byte $66, $60
    .byte $65, $70
    .byte $00, $00

.code

    ; ------------------------------------------------------------------------------------------------
    
.proc PlayerLoseLife

    inc DisableScreenFlag                         ; disable screen and sprite 0 check

    mb Sprite0HitDetectFlag := #0
    mb EventMusicQueue := #MUSIC_Silence          ; Silence music

    if dec NumberofLives == negative              ; take one life from player, if negative they are dead

        mb OperMode_Task := #0                    ; initialize mode task,
        mb OperMode := #MODE_GAMEOVER             ; switch to game over mode                            ; and leave
        rts

    endif

    mb x := WorldNumber * 2                       ; x as offset

    if LevelNumber & #2                           ; if in area -3 or -4, increment ; offset by one byte, otherwise leave offset alone
        inx
    endif

    mb y := HalfwayPageNybbles[ x ]               ; get halfway page number with offset
    mb a := LevelNumber >> 1                      ; check area number's LSB

    tya                                           ; if in area -2 or -4, use lower nybble

    if carry clear                                ; move higher nybble to lower if area number is -1 or -3
        mb a := a >> 4
    endif
    ; greater than:
    ; left side of screen must be at the halfway page, otherwise player must start at the start
    if a & #%00001111 <> ScreenLeft_PageLoc && greaterORequal          
        lda #0                                    ; beginning of the level
    endif

    sta HalfwayPage                               ; store as halfway page for player

    jsr TransposePlayers                          ; switch players around if 2-player game
    jmp ContinueGame                              ; continue the game

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc GameOverMode

    lda OperMode_Task
    jsr JumpEngine

    .word SetupGameOver
    .word ScreenRoutines
    .word RunGameOver

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc SetupGameOver

    mb ScreenRoutineTask    := #0                 ; reset screen routine task control for title screen, game,
    mb Sprite0HitDetectFlag := a                  ; and game over modes..disable sprite 0 check
    mb EventMusicQueue      := #MUSIC_GameOver    ; put game over music in secondary queue

    inc DisableScreenFlag                         ; disable screen output
    inc OperMode_Task                             ; set secondary mode to 1
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RunGameOver

    mb DisableScreenFlag := #0                    ; reenable screen
    ; check controller for NOT start pressed AND Screentimer not expired
    if !SavedJoypad1Bits & #BUTTON_START && ScreenTimer goto ContinueExit             ; not ready to procced yet

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc TerminateGame

    mb EventMusicQueue := #MUSIC_Silence

    if jsr TransposePlayers == carry clear goto ContinueGame           ; check if other player can keep playing

    mb ContinueWorld := WorldNumber               ; otherwise put world number into secret continue function variable

    lda #0
    asl                                           ; OP residual ASL instruction
    sta OperMode_Task                             ; reset all modes to title screen and
    sta ScreenTimer                               ; leave
    sta OperMode
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ContinueGame

    jsr LoadAreaPointer                           ; update level pointer with
    mb PlayerSize := #1                           ; actual world and area numbers, then; reset player's size, status, and
    inc FetchNewGameTimerFlag                     ; set game timer flag to reload

    mb a := #0                                    ; game timer from header
    mb TimerControl := a                          ; also set flag for timers to count again
    mb PlayerStatus := a
    mb GameEngineSubroutine := a                  ; reset task for game core
    mb OperMode_Task := a                         ; set modes and leave
    ; if in game over mode, switch back to
    mb OperMode := #1                             ; game mode, because game is still on

.endproc

    ContinueExit:
    rts

    ; ------------------------------------------------------------------------------------------------
    
.proc TransposePlayers

    sec                                           ; set carry flag by default to end game

    if NumberOfPlayers && OffScr_NumberofLives == positive             ; if two player game, AND does offscreen player have any lives left?

        mb CurrentPlayer := CurrentPlayer ^ #1    ; invert which player is on the screen

        ldx #$06
        repeat                                    ; x = 6 to 0
            mb a:= OnscreenPlayerInfo[ x ]        ; transpose the information
            pha                                   ; of the onscreen player, with offscreen
                mb OnscreenPlayerInfo[ x ] := OffscreenPlayerInfo[ x ]
            pla
            mb OffscreenPlayerInfo[ x ] := a
        until dex == negative

        clc                                       ; clear carry flag to get game going
    endif

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------
    ; OP

.proc DoNothing1
    lda #$ff                                      ; this is residual code, this value is
    sta $06c9                                     ; not used anywhere in the program
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DoNothing2
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc AreaParserTaskHandler

    if ldy AreaParserTaskNum == zero              ; check number of tasks here  ; if already set, go ahead
        mb y, AreaParserTaskNum := #$08           ; otherwise, set eight by default
    endif

    dey
    mb a := y                                     ; a will have task from 0 to 7
    jsr AreaParserTasks

    if dec AreaParserTaskNum == zero              ; if all tasks not complete do not , render attribute table yet
        jsr RenderAttributeTables
    endif

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc AreaParserTasks
    jsr JumpEngine

    .word IncrementColumnPos
    .word RenderAreaGraphics
    .word RenderAreaGraphics
    .word AreaParserCore
    .word IncrementColumnPos
    .word RenderAreaGraphics
    .word RenderAreaGraphics
    .word AreaParserCore

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc IncrementColumnPos

    inc CurrentColumnPos                          ; increment column where we're at

    if ! CurrentColumnPos & #%00001111            ; mask out higher nybble
        sta CurrentColumnPos                      ; if no bits left set, wrap back to zero (0-f)
        inc CurrentPageLoc                        ; and increment page number where we're at
    endif

    inc BlockBufferColumnPos                      ; increment column offset where we're at

    mb BlockBufferColumnPos := BlockBufferColumnPos & #%00011111       ; mask out all but 5 LSB (0-1f); and save

    rts

.endproc

    
dataseg

BSceneDataOffsets:
    .byte $00, $30, $60

BackSceneryData:
    .byte $93, $00, $00, $11, $12, $12, $13, $00                       ; clouds
    .byte $00, $51, $52, $53, $00, $00, $00, $00
    .byte $00, $00, $01, $02, $02, $03, $00, $00
    .byte $00, $00, $00, $00, $91, $92, $93, $00
    .byte $00, $00, $00, $51, $52, $53, $41, $42
    .byte $43, $00, $00, $00, $00, $00, $91, $92

    .byte $97, $87, $88, $89, $99, $00, $00, $00                       ; mountains and bushes
    .byte $11, $12, $13, $a4, $a5, $a5, $a5, $a6
    .byte $97, $98, $99, $01, $02, $03, $00, $a4
    .byte $a5, $a6, $00, $11, $12, $12, $12, $13
    .byte $00, $00, $00, $00, $01, $02, $02, $03
    .byte $00, $a4, $a5, $a5, $a6, $00, $00, $00

    .byte $11, $12, $12, $13, $00, $00, $00, $00                       ; trees and fences
    .byte $00, $00, $00, $9c, $00, $8b, $aa, $aa
    .byte $aa, $aa, $11, $12, $13, $8b, $00, $9c
    .byte $9c, $00, $00, $01, $02, $03, $11, $12
    .byte $12, $13, $00, $00, $00, $00, $aa, $aa
    .byte $9c, $aa, $00, $8b, $00, $01, $02, $03

BackSceneryMetatiles:
    .byte $80, $83, $00                           ; cloud left
    .byte $81, $84, $00                           ; cloud middle
    .byte $82, $85, $00                           ; cloud right
    .byte $02, $00, $00                           ; bush left
    .byte $03, $00, $00                           ; bush middle
    .byte $04, $00, $00                           ; bush right
    .byte $00, $05, $06                           ; mountain left
    .byte $07, $06, $0a                           ; mountain middle
    .byte $00, $08, $09                           ; mountain right
    .byte $4d, $00, $00                           ; fence
    .byte $0d, $0f, $4e                           ; tall tree
    .byte $0e, $4e, $4e                           ; short tree

FSceneDataOffsets:
    .byte $00, $0d, $1a

ForeSceneryData:
    .byte $86, $87, $87, $87, $87, $87, $87       ; in water
    .byte $87, $87, $87, $87, $69, $69

    .byte $00, $00, $00, $00, $00, $45, $47       ; wall
    .byte $47, $47, $47, $47, $00, $00

    .byte $00, $00, $00, $00, $00, $00, $00       ; over water
    .byte $00, $00, $00, $00, $86, $87

TerrainMetatiles:
    .byte $69, $54, $52, $62

TerrainRenderBits:
    .byte %00000000, %00000000                    ; no ceiling or floor
    .byte %00000000, %00011000                    ; no ceiling, floor 2
    .byte %00000001, %00011000                    ; ceiling 1, floor 2
    .byte %00000111, %00011000                    ; ceiling 3, floor 2
    .byte %00001111, %00011000                    ; ceiling 4, floor 2
    .byte %11111111, %00011000                    ; ceiling 8, floor 2
    .byte %00000001, %00011111                    ; ceiling 1, floor 5
    .byte %00000111, %00011111                    ; ceiling 3, floor 5
    .byte %00001111, %00011111                    ; ceiling 4, floor 5
    .byte %10000001, %00011111                    ; ceiling 1, floor 6
    .byte %00000001, %00000000                    ; ceiling 1, no floor
    .byte %10001111, %00011111                    ; ceiling 4, floor 6
    .byte %11110001, %00011111                    ; ceiling 1, floor 9
    .byte %11111001, %00011000                    ; ceiling 1, middle 5, floor 2
    .byte %11110001, %00011000                    ; ceiling 1, middle 4, floor 2
    .byte %11111111, %00011111                    ; completely solid top to bottom

    ; ------------------------------------------------------------------------------------------------

.code

.proc AreaParserCore

    ; locals
        TerrainCeilingEtc   = temp_byte      ; - used as counter, store for low nybble for background, ceiling byte for terrain
        TerrainFloorByte    = temp_byte + 1  ; - used to store floor byte for terrain
        TerrainMetaTile     = temp_byte + 7  ; - used to store terrain metatile
        BlockBufferPointer  = temp_byte + 6  ; - used to store block buffer address
    ; end locals


    ; check to see if we are starting right of start,  if not, go ahead and render background, foreground and terrain
    if BackloadingFlag                            
        jsr ProcessAreaData                       ; otherwise skip ahead and load level data
    endif

    ldx #$0c
    lda #0
    repeat                                        ; x = 12 to 0
        mb MetatileBuffer[ x ] := a               ; clear out metatile buffer
    until dex == negative

    if ldy BackgroundScenery                      ; do we need to render the background scenery?
    ; check for every third page
        lda CurrentPageLoc
        while cmp #3 == positive do               ; if less than three we're there
            mb a := a - #3                        ; if 3 or more, subtract 3 and
        endwhile N clear

        mb a := a << 4                            ; move results to higher nybble

        mb x := a +c BSceneDataOffsets[ y - 1 ] +c CurrentColumnPos    ; add with carry

        if lda BackSceneryData[ x ]               ; load data from sum of offsets -  if zero, no scenery for that part

            pha
                mb TerrainCeilingEtc := a & #$0f - #1 ; clear h.nybble and subtract one (because low nybble is $01-$0c)
                mb x := a * 2 +c TerrainCeilingEtc    ; multiply by three (shift to left and add result to old one) carry is clear
            pla                                   ; get high nybble from stack, move low

            mb y := a >> 4                        ; use as second offset (used to determine height)
            mb TerrainCeilingEtc := #3            ; use previously saved memory location for counter

            repeat
                mb MetatileBuffer[ y ] := BackSceneryMetatiles[ x ]    ; load metatile data from offset of (lsb - 1) * 3
                inx
                iny
            until y = #$0b || dec TerrainCeilingEtc == zero                    ; decrement until counter expires, or y = $0b

        endif
    endif

    if ldx ForegroundScenery                      ; check for foreground data needed or not

        mb y := FSceneDataOffsets[ x - 1 ]        ; load offset from location offset by header value, then
        ldx #0                                    ; reinit X
        repeat
            if a := ForeSceneryData[ y ]          ; load data until counter expires
               mb MetatileBuffer[ x ] := a        ; do not store if zero found
            endif
            iny
            inx
        until x = #$0d                            ; store up to end of metatile buffer

    endif

    if ldy AreaType == zero && WorldNumber = #WORLD8                   ;  if set as water level and world number eight,
        lda #$62                                  ; use castle wall metatile as terrain type
    else
        lda TerrainMetatiles,y                    ; otherwise get appropriate metatile for area type
        if ldy CloudTypeOverride                  ; check for cloud type override , if not set, keep value otherwise
            lda #$88                              ; use cloud block terrain
        endif
    endif

    mb TerrainMetaTile := a                        ; store value here
    ldx #0                                        ; initialize X, use as metatile buffer offset

    mb y := TerrainControl * 2                    ; multiply by 2 and use as yet another offset

    do
        mb TerrainCeilingEtc := TerrainRenderBits[ y ]    ; get one of the terrain rendering bit data
        mb TerrainFloorByte  := y + 1                ; increment Y and use as offset next time around

        if CloudTypeOverride && x <> #0           ; skip if value here is zero, otherwise, check if we're doing the ceiling byte
            mb TerrainCeilingEtc := TerrainCeilingEtc & #%00001000                     ; if not, mask out all but d3
        endif

        ldy #0                                    ; start at beginning of bitmasks
        repeat

            if lda Bitmasks[ y ] : bit TerrainCeilingEtc                      ; if set, write terrain to buffer
                mb MetatileBuffer[ x ] := TerrainMetaTile
            endif

            inx                                   ; continue until end of buffer
            if x = #$0d goto ExitLoop             ; if we're at the end, break out of this loop

            if AreaType = #2 && x = #$0b          ; check world type for underground area, and if we're at the bottom of the screen
                mb TerrainMetaTile := #$54         ; override old terrain type with ground level terrain type
            endif

            iny                                   ; increment bitmasks offset in Y
        until y = #$08                           ; if not all bits checked, loop back

        ldy TerrainFloorByte

    while Z clear                                 ; unconditional branch, use Y to load next byte

    ExitLoop:

    jsr ProcessAreaData                           ; do the area data loading routine now
    lda BlockBufferColumnPos
    jsr GetBlockBufferAddr                        ; get block buffer address from where we're at
    ldx #0
    ldy #0                                        ; init index regs and start at beginning of smaller buffer

    repeat
        sty TerrainCeilingEtc

        mb a := MetatileBuffer[ x ] & #%11000000 << 1
        rol                                       ; make %xx000000 into %000000xx
        rol
        tay                                       ; use as offset in Y

        ; reload original unmasked value here, check for certain values depending on bits set
        if MetatileBuffer[ x ] < BlockBuffLowBounds[ y ]               
            lda #0                                ; if less, init value before storing
        endif

        ldy TerrainCeilingEtc                     ; get offset for block buffer
        mb (BlockBufferPointer)[ y ] := a         ; store value into block buffer

        mb a := y
        mb y := a + #$10                          ; add 16 (move down one row) to offset
        inx                                       ; increment column value
    until x >= #$0d                               ; continue until we pass last row, then leave

    rts

.endproc


dataseg


    ; numbers lower than these with the same attribute bits will not be stored in the block buffer
    
BlockBuffLowBounds:

    .byte $10, $51, $88, $c0

    ; ------------------------------------------------------------------------------------------------

.code

.proc ProcessAreaData

    repeat

        ldx #2                                    ; start at the end of area object buffer

        repeat

            stx ObjectOffset
            mb BehindAreaParserFlag := #0         ; reset flag
            ldy AreaDataOffset                    ; get offset of area data pointer

            ; get first byte of area object - if not end of area && buffer negative
            if (AreaData)[ y ] <> #$fd && ( lda AreaObjectLength[ x ] == negative)                                  

                iny
                ; check for page select bit (d7), branch if not set AND check page select
                if (AreaData)[ y ] << 1 == carry && !AreaObjectPageSel
                    inc AreaObjectPageSel         ; if not already set, set it now
                    inc AreaObjectPageLoc         ; and increment page location
                endif

                dey

                if (AreaData)[ y ] & #$0f = #$0d                       ; mask out high nybble -> row 13?
                    iny                           ; if so, reread second byte of level object
                    lda (AreaData),y
                    dey                           ; decrement to get ready to read first byte
                    ; check for d6 set (if not, object is page control) OR if page select is set, do not reread
                    if a & #%01000000 || AreaObjectPageSel goto CheckRear

                    iny                           ; if d6 not set, reread second byte

                    ; mask out all but 5 LSB and store in page control
                    mb AreaObjectPageLoc := (AreaData)[ y ] & #%00011111                                

                    inc AreaObjectPageSel         ; increment page select
                    jmp NextAObj

                endif

                ; check flag for saved page number and branch if set,  to render the object (otherwise bg might not look right)
                if a <> #$0e  || !BackloadingFlag                        ; row 14?                  
                    CheckRear:
                    ; check to see if current page of level object is behind current page of renderer
                    if AreaObjectPageLoc < CurrentPageLoc goto SetBehind                                
                endif
            endif

            jsr DecodeAreaData                    ; do sub and do not turn on flag
            jmp ChkLength

            SetBehind:
            inc BehindAreaParserFlag              ; turn on flag if object is behind renderer

            NextAObj:

            jsr IncAreaObjOffset                  ; increment buffer offset and move on

            ChkLength:

            ldx ObjectOffset                      ; get buffer offset
            if lda AreaObjectLength[ x ] == positive                   ; check object length for anything stored here
                dec AreaObjectLength,x            ; decrement length or get rid of it
            endif

        until dex == negative                     ; decrement buffer offset and loopback unless exceeded buffer
        
    ; check for flag set if objects were behind renderer AND  check for flag set if starting right of page 0
    until BehindAreaParserFlag == zero && BackloadingFlag == zero      
    Exit:
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc IncAreaObjOffset
    inc AreaDataOffset                             ; increment offset of level pointer
    inc AreaDataOffset
    mb AreaObjectPageSel := #0                    ; reset page select
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DecodeAreaData

    ; locals
        temp_adder = temp_byte + 7
    ; end locals

    if AreaObjectLength[ x ] == positive          ; check current buffer flag
        mb y := AreaObjOffsetBuffer[ x ]          ; if not, get offset from buffer
    endif

    ldx #$10                                      ; load offset of 16 for special row 15

    if lda (AreaData)[ y ] = #$fd goto ProcessAreaData::Exit           ; if end of level RTS

    if a & #$0f <> #$0f                           ; row 15?; if so, keep the offset of 16
        ldx #$08                                  ; otherwise load offset of 8 for special row 12
        if a <> #$0c                              ; row 12?  ; if so, keep the offset value of 8
            ldx #0                                ; otherwise nullify value by default
        endif
    endif

    stx temp_adder                                ; store whatever value we just loaded here
    ldx ObjectOffset                              ; get object offset again

    if a = #$0e                                   ; row 14?
        mb temp_adder := #0                       ; if so, load offset with 0
        lda #$2e                                  ; and load A with another value
        bne NormObj                               ; unconditional branch
    endif

    if a = #$0d                                   ; row 13?

        lda #$22                                  ; if so, load offset with 34
        sta temp_adder
        iny                                       ; get next byte

        if !(AreaData)[ y ] & #%01000000 goto LeavePar                 ; if d6 clear, branch to RTS (we handled this earlier)

        if (AreaData)[ y ] & #%01111111 = #$4b    ; check for loop command in low nybble, (plus d6 set for object other than page control)
            inc LoopCommand                       ; if loop command, set loop command flag
        endif

        and #%00111111                            ; mask out d7 and d6

    else                                          ; not row 13:
        if a < #$0c                               ; less than row 12
            iny                                   ; if not, get second byte of level object
            if (AreaData)[ y ] & #%01110000 == zero    ; mask out all but d6-d4 -  if any bits set, branch to handle large object

                mb $07 := #$16                    ; otherwise set offset of 24 for small object
                lda (AreaData),y                  ; reload second byte of level object
                and #%00001111                    ; mask out higher nybble and jump
                jmp NormObj
            endif

            sta temp_byte                         ; store value here (branch for large objects)

            if a = #$70 && (AreaData)[ y ] & #%00001000 != zero        ; check for vertical pipe object
                mb temp_byte := #0                ; otherwise, nullify value for warp pipe
            endif

            lda temp_byte                         ; get value and jump ahead
        else                                      ; branch here for rows 12-15
            iny
            lda (AreaData),y
            and #%01110000                        ; get next byte and mask out all but d6-d4
        endif

        mb a := a >> 4                            ; move d6-d4 to lower nybble
    endif

    NormObj:

    sta temp_byte                                 ; store value here (branch for small objects and rows 13 and 14)

    if AreaObjectLength[ x ] == negative          ; if NOT used already

        if AreaObjectPageLoc <> CurrentPageLoc    ; make sure the object we've loaded is not on the same page as the renderer

            ldy AreaDataOffset                    ; if not, get old offset of level pointer
            lda (AreaData),y                      ; and reload first byte
            and #%00001111
            ; row 14?,  if so, check backloading flag, goto render obj
            if a = #$0e && BackloadingFlag goto StrAObj
            LeavePar:
            rts
        endif

        if BackloadingFlag                        ; if it's been initialized branch to column-wise check
            lda #0                                ; if not, initialize both backloading and
            sta BackloadingFlag                   ; behind-renderer flags and leave
            sta BehindAreaParserFlag
            sta ObjectOffset
            LoopCmdE:
            rts
        endif

        ldy AreaDataOffset                        ; get first byte again
        lda (AreaData),y
        and #%11110000                            ; mask out low nybble and move high to low
        lsr
        lsr
        lsr
        lsr

        if a <> CurrentColumnPos goto LeavePar    ; is this where we're at? rts if not

        StrAObj:
        lda AreaDataOffset                        ; if so, load area obj offset and store in buffer
        sta AreaObjOffsetBuffer,x
        jsr IncAreaObjOffset                      ; do sub to increment to next object data
    endif

    lda temp_byte                                 ; get stored value and add offset to it
    clc                                           ; then use the jump engine with current contents of A
    adc $07
    jsr JumpEngine
.endproc

    ; large objects (rows $00-$0b or 00-11, d6-d4 set)

    .word VerticalPipe                            ; used by warp pipes
    .word AreaStyleObject
    .word RowOfBricks
    .word RowOfSolidBlocks
    .word RowOfCoins
    .word ColumnOfBricks
    .word ColumnOfSolidBlocks
    .word VerticalPipe                            ; used by decoration pipes
    ; objects for special row $0c or 12

    .word Hole_Empty
    .word PulleyRopeObject
    .word Bridge_High
    .word Bridge_Middle
    .word Bridge_Low
    .word Hole_Water
    .word QuestionBlockRow_High
    .word QuestionBlockRow_Low
    ; objects for special row $0f or 15

    .word EndlessRope
    .word BalancePlatRope
    .word CastleObject
    .word StaircaseObject
    .word ExitPipe
    .word FlagBalls_Residual
    ; small objects (rows 0-$0b or 00-11, d6-d4 all clear)

    .word QuestionBlock                           ; power-up
    .word QuestionBlock                           ; coin
    .word QuestionBlock                           ; hidden, coin
    .word Hidden1UpBlock                          ; hidden, 1-up
    .word BrickWithItem                           ; brick, power-up
    .word BrickWithItem                           ; brick, vine
    .word BrickWithItem                           ; brick, star
    .word BrickWithCoins                          ; brick, coins
    .word BrickWithItem                           ; brick, 1-up
    .word WaterPipe
    .word EmptyBlock
    .word Jumpspring
    ; objects for special row $0d or 13 (d6 set)

    .word IntroPipe
    .word FlagpoleObject
    .word AxeObj
    .word ChainObj
    .word CastleBridgeObj
    .word ScrollLockObject_Warp
    .word ScrollLockObject
    .word ScrollLockObject
    .word AreaFrenzy                              ; flying cheep-cheeps
    .word AreaFrenzy                              ; bullet bills or swimming cheep-cheeps
    .word AreaFrenzy                              ; stop frenzy
    .word DecodeAreaData::LoopCmdE
    ; object for special row $0e or 14

    .word AlterAreaAttributes

    ; ------------------------------------------------------------------------------------------------
    
.proc AlterAreaAttributes

    ldy AreaObjOffsetBuffer,x                     ; load offset for level object data saved in buffer
    iny                                           ; load second byte
    lda (AreaData),y
    pha                                           ; save in stack for now
        if ! a & #%01000000                       ; if d6 is not set; OP: use bit/bvc
            pla
            pha                                   ; pull and push offset to copy to A
                mb TerrainControl := a & #$0F     ; mask out high nybble and store as
            pla

            mb a := a & #%00110000                ; pull and mask out all but d5 and d4
            mb BackgroundScenery := a >> 4        ; move bits to lower nybble and store as new background scenery bits

            rts
        endif
    pla

    ; if four or greater, set color control bits and nullify foreground scenery bits
    if a & #%00000111 >= #4                                    
        sta BackgroundColorCtrl
        lda #0
    endif

    sta ForegroundScenery                         ; set new foreground scenery bits
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc ScrollLockObject_Warp

    .export ScrollLockObject

    ldx #4                                        ; load value of 4 for game text routine as default warp zone: display: (4-3-2)

    if lda WorldNumber != zero                    ; then check world number
        inx                                       ; if world number > 1, increment for next warp zone: display: (5)
        if y := AreaType - 1 == zero              ; if ground area type (=1), increment for last warp zone
            inx                                   ; (8-7-6) and move on
        endif
    endif

    txa
    sta WarpZoneControl                           ; store number here to be used by warp zone routine
    jsr WriteGameText                             ; print text and warp zone numbers
    lda #OBJECTID_PiranhaPlant
    jsr KillEnemies                               ; load identifier for piranha plants and do sub

    ScrollLockObject:

    mb ScrollLock := ScrollLock ^ #%00000001      ; invert scroll lock to turn it on

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc KillEnemies


    ; locals
        EnemyToKill = temp_byte
    ; end locals
    

    sta EnemyToKill                                 ; store identifier here
    lda #0
    ldx #4                   
    repeat
        if ldy Enemy_ID[ x ] = EnemyToKill        ; check for identifier in enemy object buffer
            sta Enemy_Flag,x                      ; if found, deactivate enemy object flag
        endif
    until dex == negative                         ; do this until all slots are checked

    rts
.endproc


dataseg

FrenzyIDData:
    .byte OBJECTID_FlyCheepCheepFrenzy, OBJECTID_BBill_CCheep_Frenzy, OBJECTID_Stop_Frenzy

.code

    ; ------------------------------------------------------------------------------------------------

.proc AreaFrenzy

    ldx temp_byte                                 ; use area object identifier bit as offset
    mb a := FrenzyIDData[ x - 8 ]                 ; note that x starts at 8

    ldy #$05
    repeat
        ; check regular slots of enemy object buffer, if all slots checked and enemy object not found, branch to store
        if dey == negative goto ExitAFrenzy       
    until a = Enemy_ID[ y ]                       ; check for enemy object in buffer versus frenzy object

    lda #0                                        ; if enemy object already present, nullify queue and leave

    ExitAFrenzy:
    sta EnemyFrenzyQueue                          ; store enemy into frenzy queue
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc AreaStyleObject

    ; locals
        ObjectLength = temp_byte + 6
        RowOffset    = temp_byte + 7 ; - starts with adder from area parser, used to store row offset
    ; end locals


    lda AreaStyle                               ; load level object style and jump to the right sub
    jsr JumpEngine
    .word TreeLedge                             ; also used for cloud type levels
    .word MushroomLedge
    .word BulletBillCannon

    ; ------------------------------------------------------------------------------------------------

    TreeLedge:

    jsr GetLrgObjAttrib                           ; get row and length of green ledge

    if AreaObjectLength[ x ]                      ; check length counter for expiration

        if negative
            tya
            sta AreaObjectLength,x                ; store lower nybble into buffer flag as length of ledge
            ; are we at the start of the level?
            if !CurrentPageLoc | CurrentColumnPos goto MidTreeLedge

            lda #$16
            jmp NoUnder                           ; render start of tree ledge
        endif

        MidTreeLedge:

        ldx RowOffset

        mb MetatileBuffer[ x ] := #$17            ; render middle of tree ledge; note that this is also used if ledge position is
        lda #$4c                                  ; at the start of level for continuous effect

        jmp AllUnder

    endif
    ; else  ;  AreaObjectLength[ x ] == zero

    lda #$18                                      ; render end of tree ledge
    jmp NoUnder
    
    ; ------------------------------------------------------------------------------------------------

    MushroomLedge: 
   
    jsr ChkLrgObjLength                           ; get shroom dimensions
    sty ObjectLength                              ; store length here for now

    if carry set
        mb MushroomLedgeHalfLen[ x ] := AreaObjectLength[ x ] / 2
        lda #$19                                  ; render start of mushroom
        jmp NoUnder
    endif

    lda #$1b                                      ; if at the end, render end of mushroom

    if ldy AreaObjectLength[ x ] == zero goto NoUnder

    mb ObjectLength := MushroomLedgeHalfLen[ x ]  ; get divided length and store where length

    ldx RowOffset
    mb MetatileBuffer[ x ] := #$1a                ; render middle of mushroom

    if y <> ObjectLength goto MushLExit           ; if we are smack dab in the center: rts 

    inx
    mb MetatileBuffer[ x ] := #$4f                ; render stem top of mushroom underneath the middle
    lda #$50

    AllUnder:

    inx
    ldy #$0f                                      ; set $0f to render all way down
    jmp RenderUnderPart                           ; now render the stem of mushroom

    NoUnder:

    ldx RowOffset                                 ; load row of ledge
    ldy #0                                        ; set 0 for no bottom on this part
    jmp RenderUnderPart

.endproc


    ; tiles used by pulleys and rope object
    
dataseg

PulleyRopeMetatiles:
    .byte $42, $41, $43

.code

.proc PulleyRopeObject

    jsr ChkLrgObjLength                           ; get length of pulley/rope object

    ldy #0                                        ; initialize metatile offset
    if carry clear                                ; if starting, render left pulley
        iny
        if !AreaObjectLength[ x ]                 ; if not at the end, render rope
            iny                                   ; otherwise render right pulley
        endif

    endif

    mb MetatileBuffer := PulleyRopeMetatiles[ y ]                      ; render at the top of the screen
    
.endproc

    MushLExit: ; make label global
    rts
    
    ; .endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

CastleMetatiles:
    .byte $00, $45, $45, $45, $00
    .byte $00, $48, $47, $46, $00
    .byte $45, $49, $49, $49, $45
    .byte $47, $47, $4a, $47, $47
    .byte $47, $47, $4b, $47, $47
    .byte $49, $49, $49, $49, $49
    .byte $47, $4a, $47, $4a, $47
    .byte $47, $4b, $47, $4b, $47
    .byte $47, $47, $47, $47, $47
    .byte $4a, $47, $4a, $47, $4a
    .byte $4b, $47, $4b, $47, $4b

.code

.proc CastleObject

    ; locals
        RowCounter      = temp_byte + 6    ; - used to store upper limit of rows
        RowOffset       = temp_byte + 7    ; - starts with adder from area parser, used to store row offset
    ; end locals

    jsr GetLrgObjAttrib                           ; save lower nybble as starting row
    sty RowOffset                                 ; if starting row is above $0a, game will crash!!!
    ldy #4
    jsr ChkLrgObjFixedLength                      ; load length of castle if not already loaded

    txa
    pha                                           ; save obj buffer offset to stack
        ldy AreaObjectLength,x                    ; use current length as offset for castle data
        ldx RowOffset                             ; begin at starting row
        
        mb RowCounter := #$0b                     ; load upper limit of number of rows to print

        repeat

            mb MetatileBuffer[ x ] := CastleMetatiles[ y ]             ; load current byte using offset
            inx                                   ; store in buffer and increment buffer offset
            if RowCounter                            ; have we reached upper limit yet?
                ; if not, increment column-wise
                ; to byte in next row
                mb y := y + 5
                dec RowCounter                      ; move closer to upper limit
            endif
        until x = #$0b                            ; have we reached the row just before floor?
    pla
    tax                                           ; get obj buffer offset from before

    if CurrentPageLoc                             ; if we're at page 0, we do not need to do anything else
        ; check length, ; if length not almost about to expire AND
        ; check starting row for tall castle ($00) OR check to see if we're at the second column
        if lda AreaObjectLength[ x ] <> #1 && ( ldy RowOffset || a <> #3  )

            ; if not tall castle, check to see if we're at the third column
            if a <> #2 goto Exit                   ; if we aren't and the castle is tall, don't create flag yet

            jsr GetAreaObjXPosition               ; otherwise, obtain and save horizontal pixel coordinate
            pha
                jsr FindEmptyEnemySlot            ; find an empty place on the enemy object buffer
            pla

            mb Enemy_X_Position [ x ] := a        ; then write horizontal coordinate for star flag
            mb Enemy_PageLoc    [ x ] := CurrentPageLoc                ; set page location for star flag
            mb Enemy_Y_HighPos  [ x ] := #1       ; set vertical high byte
            mb Enemy_Flag       [ x ] := a        ; set flag for buffer
            mb Enemy_Y_Position [ x ] := #$90     ; set vertical coordinate
            mb Enemy_ID         [ x ] := #OBJECTID_StarFlagObject               ; set star flag value in buffer itself
            rts
        endif                                     ; else:

        ldy #$52                                  ; put brick at floor to stop player at end of level
        sty MetatileBuffer + 10                     ; this is only done if we're on the second column
    endif
    Exit:
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc WaterPipe

    jsr GetLrgObjAttrib                           ; get row and lower nybble
    ldy AreaObjectLength,x                        ; get length (residual code, water pipe is 1 col thick)
    ldx $07                                       ; get row
    mb MetatileBuffer[ x ] := #$6b                ; draw something here and below it
    mb MetatileBuffer[ x + 1 ] := #$6c
    rts

.endproc


.proc IntroPipe

    ldy #3                                        ; check if length set, if not set, set it
    jsr ChkLrgObjFixedLength
    ldy #$0a                                      ; set fixed value and render the sideways part
    jsr RenderSidewaysPipe
    if carry clear                                ; if carry clear, draw vertical pipe part
        ldx #$06                                  ; blank everything above the vertical pipe part
        repeat
            mb MetatileBuffer[ x ] := #0          ; because otherwise it will look like exit pipe
        until dex == negative

        mb MetatileBuffer[ 7 ] := VerticalPipeData[ y ]
    endif
    rts
.endproc


dataseg

SidePipeShaftData:
    .byte $15, $14                                ; used to control whether or not vertical pipe shaft
    .byte $00, $00                                ; is drawn, and if so, controls the metatile number
SidePipeTopPart:
    .byte $15, $1e                                ; top part of sideways part of pipe
    .byte $1d, $1c
SidePipeBottomPart:
    .byte $15, $21                                ; bottom part of sideways part of pipe
    .byte $20, $1f

.code

.proc ExitPipe

    ldy #3                                        ; check if length set, if not set, set it
    jsr ChkLrgObjFixedLength
    jsr GetLrgObjAttrib                           ; get vertical length, then plow on through RenderSidewaysPipe

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RenderSidewaysPipe
    
    ; locals
        PipeShaftLengthV = temp_byte + 5 ; - used to store length of vertical shaft in RenderSidewaysPipe
        HLength          = temp_byte + 6 ; - used to store leftover horizontal length in RenderSidewaysPipe
    ; end locals
    


    mb PipeShaftLengthV := y - 2; decrement twice to make room for shaft at bottom

    mb y, HLength := AreaObjectLength[ x ]

    ldx PipeShaftLengthV                          ; get vertical length plus one, use as buffer offset
    inx
    ; OP bad code, no cmp needed:
    if SidePipeShaftData[ y ] <> #0               ; check for value 0 based on horizontal offset 
        ldx #0                                    ; if found, do not draw the vertical pipe shaft
        ldy PipeShaftLengthV                      ; init buffer offset and get vertical length
        jsr RenderUnderPart                       ; and render vertical shaft using tile number in A
        clc                                       ; clear carry flag to be used by IntroPipe
    endif

    ldy HLength                                   ; render side pipe part at the bottom

    mb MetatileBuffer[ x ]      := SidePipeTopPart[ y ]     ; note that the pipe parts are stored backwards horizontally
    mb MetatileBuffer[ x + 1 ]  := SidePipeBottomPart[ y ]
    rts

.endproc

dataseg

VerticalPipeData:
    .byte $11, $10                                ; used by pipes that lead somewhere
    .byte $15, $14
    .byte $13, $12                                ; used by decoration pipes
    .byte $15, $14

.code

.proc VerticalPipe

    ; locals
        RowOffset       = temp_byte + 7    ; - starts with adder from area parser, used to store row offset
        VerticalLength  = temp_byte + 6 ; - vertical length
    ; end locals

    jsr GetPipeHeight                             ; OP this routine could be moved (not copied) here and jsr eliminated

    if temp_byte                                  ; if value wasn't nullified earlier
        mb y := y + 4                             ; add four if usage control bit was not set
    endif

    tya                                           ; save y
    pha
    ; if not at world 1-1 AND  not on second column of pipe AND check for an empty moving data buffer space
        if AreaNumber | WorldNumber && ldy AreaObjectLength[ x ] && jsr FindEmptyEnemySlot == carry clear

            jsr GetAreaObjXPosition               ; get horizontal pixel coordinate, a := CurrentColumnPos * 16
            ; add eight to put the piranha plant in the center
            mb Enemy_X_Position [ x ] := a + #$08                      ; store as enemy's horizontal coordinate
            mb Enemy_PageLoc    [ x ] := CurrentPageLoc + C          ; add carry, store as enemy's page coordinate

            lda #1
            mb Enemy_Y_HighPos  [ x ] := a
            mb Enemy_Flag       [ x ] := a        ; activate enemy flag

            jsr GetAreaObjYPosition               ; get piranha plant's vertical coordinate, return in reg a
            mb Enemy_Y_Position [ x ] := a
            mb Enemy_ID         [ x ] := #OBJECTID_PiranhaPlant
            jsr InitPiranhaPlant

        endif
    pla       ; get value saved earlier and use as Y. OP : Use reg a in second condition above and don't push y to stack
    tay

    mb x := RowOffset                        ; get buffer offset
    ; draw the appropriate pipe with the Y we loaded earlier :
    mb MetatileBuffer[ x ] := VerticalPipeData[ y ]                    ; render the top of the pipe

    inx

    mb a := VerticalPipeData[ y + 2 ]             ; render the rest of the pipe
    mb y := VerticalLength                        ; subtract one from length and render the part underneath
    dey
    jmp RenderUnderPart

    GetPipeHeight:

    ldy #1                                        ; check for length loaded, if not, load
    jsr ChkLrgObjFixedLength                      ; pipe length of 2 (horizontal)
    jsr GetLrgObjAttrib

    mb a := y                                     ; get saved lower nybble as height save only the three lower bits as
    mb VerticalLength := a & #$07                 ; vertical length, then load Y with
    mb y := AreaObjectLength[ x ]                 ; length left over

    rts

.endproc

.proc FindEmptyEnemySlot

    ldx #0                                        ; start at first enemy slot

    repeat
        clc                                       ; clear carry flag by default
        if !Enemy_Flag[ x ] break                 ; check enemy buffer for nonzero if zero, leave
        inx
    until x = #$05                                ; also this will reuslt in set carry ( could be a greater equal compare)
    rts                                           ; if all values nonzero, carry flag is set

.endproc

.proc Hole_Water

    jsr ChkLrgObjLength                           ; get low nybble and save as length

    mb MetatileBuffer[ 10 ] := #$86               ; render waves
    ldx #$0b
    ldy #1                                        ; now render the water underneath
    lda #$87
    jmp RenderUnderPart

.endproc


.proc QuestionBlockRow_High
    lda #3                                        ; start on the fourth row
    BIT_Skip2                                     ; BIT instruction opcode, skip next two bytes
.endproc

.proc QuestionBlockRow_Low

    lda #$07                                      ; start on the eighth row
    pha                                           ; save whatever row to the stack for now
        jsr ChkLrgObjLength                       ; get low nybble and save as length
    pla

    tax                                           ; render question boxes with coins
    mb MetatileBuffer[ x ] := #$c0

    rts
.endproc

.proc Bridge_High
    lda #$06                                      ; start on the seventh row from top of screen
    BIT_Skip2                                     ; BIT instruction opcode
.endproc

.proc Bridge_Middle
    lda #$07                                      ; start on the eighth row
    BIT_Skip2                                     ; BIT instruction opcode
.endproc

.proc Bridge_Low

    lda #$09                                      ; start on the tenth row

    pha                                           ; save whatever row to the stack for now
        jsr ChkLrgObjLength                       ; get low nybble and save as length
    pla

    tax                                           ; render bridge railing
    mb MetatileBuffer[ x ] := #$0b

    inx
    ldy #0                                        ; now render the bridge itself
    lda #$63
    jmp RenderUnderPart

.endproc


.proc FlagBalls_Residual

    jsr GetLrgObjAttrib                           ; get low nybble from object byte
    ldx #2                                        ; render flag balls on third row from top
    lda #$6d                                      ; of screen downwards based on low nybble
    jmp RenderUnderPart

.endproc


.proc FlagpoleObject

    mb MetatileBuffer := #$24                     ; render flagpole ball on top

    ldx #1                                        ; now render the flagpole shaft
    ldy #$08
    lda #$25
    jsr RenderUnderPart

    mb MetatileBuffer[ 10 ] := #$61               ; render solid block at the bottom

    jsr GetAreaObjXPosition                       ; get pixel coordinate of where the flagpole is,

    mb Enemy_X_Position [ 5 ] := a - #8           ; subtract eight pixels and use as horizontal coord
    mb Enemy_PageLoc    [ 5 ] := CurrentPageLoc - C                  ; borrow
    mb Enemy_Y_Position [ 5 ] := #$30             ; set vertical coordinate for flag
    mb FlagpoleFNum_Y_Pos     := #$b0             ; set initial vertical coordinate for flagpole's floatey number
    mb Enemy_ID         [ 5 ] := #OBJECTID_FlagpoleFlagObject    ; set flag identifier, note that identifier and coordinates

    inc Enemy_Flag + 5                            ; use last space in enemy object buffer
    rts

.endproc

.proc EndlessRope
    ldx #0                                        ; render rope from the top to the bottom of screen
    ldy #$0f
    jmp DrawRope
.endproc

.proc BalancePlatRope

    .export DrawRope

    txa                                           ; OP : use a temp - save object buffer offset for now
    pha

        ldx #1                                    ; blank out all from second row to the bottom
        ldy #$0f                                  ; with blank used for balance platform rope
        lda #$44
        jsr RenderUnderPart

    pla                                           ; get back object buffer offset
    tax

    jsr GetLrgObjAttrib                           ; get vertical length from lower nybble
    ldx #1

    DrawRope:

    lda #$40                                      ; render the actual rope
    jmp RenderUnderPart

.endproc

dataseg

CoinMetatileData:
    .byte $c3, $c2, $c2, $c2

.code

.proc RowOfCoins
    ldy AreaType                                  ; get area type
    lda CoinMetatileData,y                        ; load appropriate coin metatile
    jmp GetRow
.endproc

dataseg

C_ObjectRow:
    .byte $06, $07, $08

C_ObjectMetatile:
    .byte $c5, $0c, $89

.code

.proc CastleBridgeObj

    .export ChainObj, EmptyBlock, AxeObj

    ldy #$0c                                      ; load length of 13 columns
    jsr ChkLrgObjFixedLength
    jmp ChainObj

    AxeObj:

    mb VRAM_Buffer_AddrCtrl := #$08               ; load bowser's palette into sprite portion of palette

    ChainObj:

    mb y := temp_byte                             ; get value loaded earlier from decoder
    mb x := C_ObjectRow[ y - 2 ]                  ; get appropriate row and metatile for object
    mb a := C_ObjectMetatile[ y - 2 ]

    jmp ColObj

        EmptyBlock:

        jsr GetLrgObjAttrib                       ; get row location
        ldx $07
        lda #$c4

    ColObj:

    ldy #0                                        ; column length of 1
    jmp RenderUnderPart

.endproc


dataseg

SolidBlockMetatiles:
    .byte $69, $61, $61, $62

BrickMetatiles:
    .byte $22, $51, $52, $52
    .byte $88                                     ; used only by row of bricks object

.code

.proc RowOfBricks

    ldy AreaType                                  ; load area type obtained from area offset pointer
    if CloudTypeOverride                          ; check for cloud type override
        ldy #4                                    ; if cloud type, override area type
    endif
    mb a := BrickMetatiles[ y ]                   ; get appropriate metatile
    jmp GetRow                                    ; and go render it

.endproc

.proc RowOfSolidBlocks
    ldy AreaType                                  ; load area type obtained from area offset pointer
    mb a := SolidBlockMetatiles[ y ]              ; get metatile
.endproc

.proc GetRow

    .export DrawRow

    pha                                           ; store metatile here
        jsr ChkLrgObjLength                       ; get row number, load length

        DrawRow:

        ldx $07
        ldy #0                                    ; set vertical height of 1
    pla

    jmp RenderUnderPart                           ; render object

.endproc

.proc ColumnOfBricks

    .export ColumnOfSolidBlocks

    ldy AreaType                                  ; load area type obtained from area offset
    lda BrickMetatiles,y                          ; get metatile (no cloud override as for row)
    jmp skip

        ColumnOfSolidBlocks:

        ldy AreaType                              ; load area type obtained from area offset
        lda SolidBlockMetatiles,y                 ; get metatile

    skip:

    pha                                           ; save metatile to stack for now
        jsr GetLrgObjAttrib                       ; get length and row
    pla                                           ; restore metatile

    ldx $07                                       ; get starting row
    jmp RenderUnderPart                           ; now render the column

.endproc


.proc BulletBillCannon

    jsr GetLrgObjAttrib                           ; get row and length of bullet bill cannon
    ldx $07                                       ; start at first row
    mb MetatileBuffer[ x ] := #$64                ; render bullet bill cannon
    inx

    if dey == positive                            ; done yet?

        mb MetatileBuffer[ x ] := #$65            ; if not, render middle part
        inx

        if dey == not negative                    ; done yet?
            lda #$66                              ; if not, render bottom until length expires
            jsr RenderUnderPart
        endif

    endif

    ldx Cannon_Offset                             ; get offset for data used by cannons and whirlpools

    jsr GetAreaObjYPosition                       ; get proper vertical coordinate for cannon
    mb Cannon_Y_Position[ x ] := a                ; and store it here


    mb Cannon_PageLoc[ x ] := CurrentPageLoc      ; store page number for cannon here
    jsr GetAreaObjXPosition                       ; get proper horizontal coordinate for cannon
    mb Cannon_X_Position[ x ] := a                ; and store it here

    inx                                           ; increment and check offset
    if x >= #$06                                  ; if not yet reached sixth cannon, branch to save offset
        ldx #0                                    ; otherwise initialize it
    endif

    stx Cannon_Offset                             ; save new offset and leave

    rts

.endproc

dataseg

StaircaseHeightData:
    .byte $07, $07, $06, $05, $04, $03, $02, $01, $00

StaircaseRowData:
    .byte $03, $03, $04, $05, $06, $07, $08, $09, $0a

.code

.proc StaircaseObject
    jsr ChkLrgObjLength                           ; check and load length

    if carry set                                  ; if length already loaded, skip init part
        lda #$09                                  ; start past the end for the bottom
        sta StaircaseControl                      ; of the staircase
    endif

    dec StaircaseControl                          ; move onto next step (or first if starting)
    ldy StaircaseControl
    ldx StaircaseRowData,y                        ; get starting row and height to render
    lda StaircaseHeightData,y
    tay
    lda #$61                                      ; now render solid block staircase
    jmp RenderUnderPart
.endproc

.proc Jumpspring

    jsr GetLrgObjAttrib
    jsr FindEmptyEnemySlot                        ; find empty space in enemy object buffer

    jsr GetAreaObjXPosition                       ; get horizontal coordinate for jumpspring
    sta Enemy_X_Position,x                        ; and store

    lda CurrentPageLoc                            ; store page location of jumpspring
    sta Enemy_PageLoc,x

    jsr GetAreaObjYPosition                       ; get vertical coordinate for jumpspring
    sta Enemy_Y_Position,x                        ; and store
    sta Jumpspring_FixedYPos,x                    ; store as permanent coordinate here

    mb Enemy_ID[ x ] := #OBJECTID_JumpspringObject   ; write jumpspring object to enemy object buffer

    ldy #1
    sty Enemy_Y_HighPos,x                         ; store vertical high byte

    inc Enemy_Flag,x                              ; set flag for enemy object buffer
    ldx $07

    mb MetatileBuffer[ x ]     := #$67            ; draw metatiles in two rows where jumpspring is
    mb MetatileBuffer[ 1 + x ] := #$68

    rts
.endproc

    

.proc Hidden1UpBlock

    if !Hidden1UpFlag goto ExitDecBlock
    mb Hidden1UpFlag := #0                        ; if set, init for the next one
    jmp BrickWithItem                             ; jump to code shared with unbreakable bricks

.endproc

.proc QuestionBlock
    jsr GetAreaObjectID                           ; get value from level decoder routine
    jmp DrawQBlk                                  ; go to render it
.endproc

.proc BrickWithCoins
    mb BrickCoinTimerFlag := #0                   ; initialize multi-coin timer flag
.endproc

.proc BrickWithItem

    jsr GetAreaObjectID                           ; save area object ID
    sty $07
    lda #0                                        ; load default adder for bricks with lines
    ldy AreaType                                  ; check level type for ground level

    if dey != zero                                ; if ground type, do not start with 5
        lda #$05                                  ; otherwise use adder for bricks without lines
    endif

    mb y := a + $07                               ; add object ID to adder use as offset for metatile

.endproc

.proc DrawQBlk

    lda BrickQBlockMetatiles,y                    ; get appropriate metatile for brick (question block
    pha                                           ; if branched to here from question block routine)
    jsr GetLrgObjAttrib                           ; get row from location byte
    jmp DrawRow                                   ; now render the object

.endproc

.proc GetAreaObjectID

    lda temp_byte                                 ; get value saved from area parser routine
    sec
    sbc #0                                        ; possibly residual code OP
    tay                                           ; save to Y

.endproc

    ExitDecBlock:
    rts


dataseg

HoleMetatiles:
    .byte $87, $00, $00, $00

.code

.proc Hole_Empty

    if jsr ChkLrgObjLength == carry set && AreaType == zero            ; get lower nybble and save as length

        ldx Whirlpool_Offset                      ; get offset for data used by cannons and whirlpools
        jsr GetAreaObjXPosition                   ; get proper vertical coordinate of where we're at

        mb Whirlpool_LeftExtent[ x ] := a - #$10                       ; sub 16, store as left extent of whirlpool
        mb Whirlpool_PageLoc[ x ] := CurrentPageLoc - C              ; save as page location of whirlpool
        
        ; multiply by 16 to get size of whirlpool
        ; note that whirlpool will always be two blocks bigger than actual size of hole
        ; and extend one block beyond each edge

        mb a := y + 2
        mb Whirlpool_Length[ x ] := a * 16        ; save size of whirlpool here
        if x := x + 1 >= #5
            ldx #0
        endif

        stx Whirlpool_Offset                      ; save new offset here
    endif

    ldx AreaType                                  ; get appropriate metatile, then
    lda HoleMetatiles,x                           ; render the hole proper
    ldx #$08
    ldy #$0f                                      ; start at ninth row and go to bottom, run RenderUnderPart
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RenderUnderPart

    repeat
        sty AreaObjectHeight                      ; store vertical length to render

        ; check current spot to see if there's something, we need to keep, if nothing, draw it:
        if y := MetatileBuffer[ x ] == zero goto DrawThisRow           
        ; if middle part (tree ledge), wait until next row, OR middle part (mushroom ledge), wait until next row:
        if y = #$17 || y = #$1a goto WaitOneRow   
        ; if question block w/ coin, overwrite, OP: could be optimized to one cpy and a greater than (both cpy to $c0):
        if y = #$c0 goto DrawThisRow              
        ; if any other metatile with palette 3, wait until next row:
        if y >= #$c0 goto WaitOneRow              ; this is really only greater than at this point
        ; if NOT cracked rock terrain, overwrite
        if y <> #$54 goto DrawThisRow             
        ; if stem top of mushroom, wait until next row
        if a = #$50 goto WaitOneRow               

        DrawThisRow:

        sta MetatileBuffer,x                      ; render contents of A from routine that called this

        WaitOneRow:

        inx
        if x >= #$0d break                        ; stop rendering if we're at the bottom of the screen
        ldy AreaObjectHeight                      ; decrement, and stop rendering if there is no more length
    until dey == negative

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ChkLrgObjLength
    jsr GetLrgObjAttrib                           ; get row location and size (length if branched to from here)
.endproc

.proc ChkLrgObjFixedLength
    lda AreaObjectLength,x                        ; check for set length counter
    clc                                           ; clear carry flag for not just starting
    if negative                                   ; if counter not set, load it, otherwise leave alone
        tya                                       ; save length into length counter
        sta AreaObjectLength,x
        sec                                       ; set carry flag if just starting
    endif
    rts
.endproc


.proc GetLrgObjAttrib

    mb y := AreaObjOffsetBuffer[ x ]              ; get offset saved from area obj decoding routine
    mb temp_byte [ 7 ] := (AreaData)[ y ] & #%00001111                 ; get first byte of level object

    iny

    mb y := (AreaData)[ y ] & #%00001111          ; get next byte, save lower nybble (length or height)
    rts
.endproc

.proc GetAreaObjXPosition
    mb a := CurrentColumnPos * 16                 ; multiply current offset where we're at by 16 to obtain horizontal pixel coordinate
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc GetAreaObjYPosition
    mb a := temp_byte[ 7 ] * 16 + #32             ; multiply value by 16, add 32 pixels for the status bar
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------
    
dataseg

BlockBufferAddr:
    .byte <Block_Buffer_1, <Block_Buffer_2
    .byte >Block_Buffer_1, >Block_Buffer_2

.code

.proc GetBlockBufferAddr

    ; locals
        BlockBufferPointer = temp_byte + 6 ; used to store block buffer address used as indirect
    ; end locals
    
    pha                                           ; take value of A, save
        mb y := a >> 4                            ; move high nybble to low
        ; use nybble as pointer to high byte of indirect here
        mb temp_byte [ 7 ] := BlockBufferAddr[ 2 + y ]                 ; high byte
    pla

    mb temp_byte [ 6 ] := a & #%00001111 + BlockBufferAddr[ y ]        ; low byte
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------
    ; unused space

dataseg

    .byte $ff, $ff

    ; ------------------------------------------------------------------------------------------------

AreaDataOfsLoopback:
    .byte $12, $36, $0e, $0e, $0e, $32, $32, $32, $0a, $26, $40

    ; ------------------------------------------------------------------------------------------------

.code

.proc LoadAreaPointer

    .export GetAreaType

    jsr FindAreaPointer                           ; find it and store it here
    sta AreaPointer

    GetAreaType:

    and #%01100000                                ; mask out all but d6 and d5
    asl
    rol
    rol
    rol                                           ; make %0xx00000 into %000000xx
    sta AreaType                                  ; save 2 MSB as area type
    rts

.endproc


.proc FindAreaPointer
    ldy WorldNumber                               ; load offset from world variable

    mb y := WorldAddrOffsets[ y ] + AreaNumber
    mb a := AreaAddrOffsets[ y ]                  ; from there we have our area pointer
    rts
.endproc

.proc GetAreaDataAddrs

    lda AreaPointer                               ; use 2 MSB for Y
    jsr GetAreaType
    tay
    
    mb AreaAddrsLOffset := AreaPointer & #%00011111  ; save as low offset
    
    ; offset for level data := base value with 2 altered MSB, then add base value to 5 LSB
    mb y := EnemyAddrHOffsets[ y ] + AreaAddrsLOffset
    
    mb EnemyDataLow  := EnemyDataAddrLow[ y ]
    mb EnemyDataHigh := EnemyDataAddrHigh[ y ]
    
    ldy AreaType                                  ; use area type as offset
    
    mb y := AreaDataHOffsets[ y ] + AreaAddrsLOffset
    mb AreaDataLow  := AreaDataAddrLow[ y ]        ; use this offset to load another pointer
    mb AreaDataHigh := AreaDataAddrHigh[ y ]
    
    ldy #0                                        ; load first byte of header
    
    lda (AreaData),y
    pha                                           ; save it to the stack for now
        if a & #%00000111 >= #4                   ; save 3 LSB for foreground scenery or bg color control
            sta BackgroundColorCtrl               ; if 4 or greater, save value here as bg color control
            lda #0
        endif
        sta ForegroundScenery                     ; if less, save value here as foreground scenery
    pla                                           ; pull byte from stack and push it back
    pha
        ; shift bits over to LSBs
        mb PlayerEntranceCtrl := a & #%00111000 >> 3
    pla                                           ; pull byte again but do not push it back
    
    and #%11000000                                ; save 2 MSB for game timer setting
    clc
    rol                                           ; rotate bits over to LSBs
    rol
    rol
    sta GameTimerSetting                          ; save value here as game timer setting
    
    iny
    lda (AreaData),y                              ; load second byte of header
    pha                                           ; save to stack
        ; mask out all but lower nybble
        mb TerrainControl := a & #%00001111
    pla                                           ; pull and push byte to copy it to A
    pha
        ; save 2 MSB for background scenery type, shift bits to LSBs 
        mb BackgroundScenery := a & #%00110000 >> 4
    pla
    and #%11000000
    clc
    rol                                           ; rotate bits over to LSBs
    rol
    rol

    if a = #%00000011                             ; if set to 3, store here     ; and nullify other value
        sta CloudTypeOverride                     ; otherwise store value in other place
        lda #0
    endif

    sta AreaStyle
    mb AreaDataLow  := AreaDataLow + #2
    mb AreaDataHigh := AreaDataHigh + C
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------
    ; GAME LEVELS DATA

dataseg

WorldAddrOffsets:
    .byte World1Areas-AreaAddrOffsets, World2Areas-AreaAddrOffsets
    .byte World3Areas-AreaAddrOffsets, World4Areas-AreaAddrOffsets
    .byte World5Areas-AreaAddrOffsets, World6Areas-AreaAddrOffsets
    .byte World7Areas-AreaAddrOffsets, World8Areas-AreaAddrOffsets

AreaAddrOffsets:
World1Areas: .byte $25, $29, $c0, $26, $60
World2Areas: .byte $28, $29, $01, $27, $62
World3Areas: .byte $24, $35, $20, $63
World4Areas: .byte $22, $29, $41, $2c, $61
World5Areas: .byte $2a, $31, $26, $62
World6Areas: .byte $2e, $23, $2d, $60
World7Areas: .byte $33, $29, $01, $27, $64
World8Areas: .byte $30, $32, $21, $65

    ; bonus area data offsets, included here for comparison purposes
    ; underground bonus area  - c2
    ; cloud area 1 (day)      - 2b
    ; cloud area 2 (night)    - 34
    ; water area (5-2/6-2)    - 00
    ; water area (8-4)        - 02
    ; warp zone area (4-2)    - 2f

EnemyAddrHOffsets:
    .byte $1f, $06, $1c, $00

EnemyDataAddrLow:
    .byte <E_CastleArea1, <E_CastleArea2, <E_CastleArea3, <E_CastleArea4, <E_CastleArea5, <E_CastleArea6
    .byte <E_GroundArea1, <E_GroundArea2, <E_GroundArea3, <E_GroundArea4, <E_GroundArea5, <E_GroundArea6
    .byte <E_GroundArea7, <E_GroundArea8, <E_GroundArea9, <E_GroundArea10, <E_GroundArea11, <E_GroundArea12
    .byte <E_GroundArea13, <E_GroundArea14, <E_GroundArea15, <E_GroundArea16, <E_GroundArea17, <E_GroundArea18
    .byte <E_GroundArea19, <E_GroundArea20, <E_GroundArea21, <E_GroundArea22, <E_UndergroundArea1
    .byte <E_UndergroundArea2, <E_UndergroundArea3, <E_WaterArea1, <E_WaterArea2, <E_WaterArea3

EnemyDataAddrHigh:
    .byte >E_CastleArea1, >E_CastleArea2, >E_CastleArea3, >E_CastleArea4, >E_CastleArea5, >E_CastleArea6
    .byte >E_GroundArea1, >E_GroundArea2, >E_GroundArea3, >E_GroundArea4, >E_GroundArea5, >E_GroundArea6
    .byte >E_GroundArea7, >E_GroundArea8, >E_GroundArea9, >E_GroundArea10, >E_GroundArea11, >E_GroundArea12
    .byte >E_GroundArea13, >E_GroundArea14, >E_GroundArea15, >E_GroundArea16, >E_GroundArea17, >E_GroundArea18
    .byte >E_GroundArea19, >E_GroundArea20, >E_GroundArea21, >E_GroundArea22, >E_UndergroundArea1
    .byte >E_UndergroundArea2, >E_UndergroundArea3, >E_WaterArea1, >E_WaterArea2, >E_WaterArea3

AreaDataHOffsets:
    .byte $00, $03, $19, $1c

AreaDataAddrLow:
    .byte <L_WaterArea1, <L_WaterArea2, <L_WaterArea3, <L_GroundArea1, <L_GroundArea2, <L_GroundArea3
    .byte <L_GroundArea4, <L_GroundArea5, <L_GroundArea6, <L_GroundArea7, <L_GroundArea8, <L_GroundArea9
    .byte <L_GroundArea10, <L_GroundArea11, <L_GroundArea12, <L_GroundArea13, <L_GroundArea14, <L_GroundArea15
    .byte <L_GroundArea16, <L_GroundArea17, <L_GroundArea18, <L_GroundArea19, <L_GroundArea20, <L_GroundArea21
    .byte <L_GroundArea22, <L_UndergroundArea1, <L_UndergroundArea2, <L_UndergroundArea3, <L_CastleArea1
    .byte <L_CastleArea2, <L_CastleArea3, <L_CastleArea4, <L_CastleArea5, <L_CastleArea6

AreaDataAddrHigh:
    .byte >L_WaterArea1, >L_WaterArea2, >L_WaterArea3, >L_GroundArea1, >L_GroundArea2, >L_GroundArea3
    .byte >L_GroundArea4, >L_GroundArea5, >L_GroundArea6, >L_GroundArea7, >L_GroundArea8, >L_GroundArea9
    .byte >L_GroundArea10, >L_GroundArea11, >L_GroundArea12, >L_GroundArea13, >L_GroundArea14, >L_GroundArea15
    .byte >L_GroundArea16, >L_GroundArea17, >L_GroundArea18, >L_GroundArea19, >L_GroundArea20, >L_GroundArea21
    .byte >L_GroundArea22, >L_UndergroundArea1, >L_UndergroundArea2, >L_UndergroundArea3, >L_CastleArea1
    .byte >L_CastleArea2, >L_CastleArea3, >L_CastleArea4, >L_CastleArea5, >L_CastleArea6
    ; ENEMY OBJECT DATA
    ; level 1-4/6-4
E_CastleArea1:
    .byte $76, $dd, $bb, $4c, $ea, $1d, $1b, $cc, $56, $5d
    .byte $16, $9d, $c6, $1d, $36, $9d, $c9, $1d, $04, $db
    .byte $49, $1d, $84, $1b, $c9, $5d, $88, $95, $0f, $08
    .byte $30, $4c, $78, $2d, $a6, $28, $90, $b5
    .byte $ff
    ; level 4-4
E_CastleArea2:
    .byte $0f, $03, $56, $1b, $c9, $1b, $0f, $07, $36, $1b
    .byte $aa, $1b, $48, $95, $0f, $0a, $2a, $1b, $5b, $0c
    .byte $78, $2d, $90, $b5
    .byte $ff
    ; level 2-4/5-4
E_CastleArea3:
    .byte $0b, $8c, $4b, $4c, $77, $5f, $eb, $0c, $bd, $db
    .byte $19, $9d, $75, $1d, $7d, $5b, $d9, $1d, $3d, $dd
    .byte $99, $1d, $26, $9d, $5a, $2b, $8a, $2c, $ca, $1b
    .byte $20, $95, $7b, $5c, $db, $4c, $1b, $cc, $3b, $cc
    .byte $78, $2d, $a6, $28, $90, $b5
    .byte $ff
    ; level 3-4
E_CastleArea4:
    .byte $0b, $8c, $3b, $1d, $8b, $1d, $ab, $0c, $db, $1d
    .byte $0f, $03, $65, $1d, $6b, $1b, $05, $9d, $0b, $1b
    .byte $05, $9b, $0b, $1d, $8b, $0c, $1b, $8c, $70, $15
    .byte $7b, $0c, $db, $0c, $0f, $08, $78, $2d, $a6, $28
    .byte $90, $b5
    .byte $ff
    ; level 7-4
E_CastleArea5:
    .byte $27, $a9, $4b, $0c, $68, $29, $0f, $06, $77, $1b
    .byte $0f, $0b, $60, $15, $4b, $8c, $78, $2d, $90, $b5
    .byte $ff
    ; level 8-4
E_CastleArea6:
    .byte $0f, $03, $8e, $65, $e1, $bb, $38, $6d, $a8, $3e, $e5, $e7
    .byte $0f, $08, $0b, $02, $2b, $02, $5e, $65, $e1, $bb, $0e
    .byte $db, $0e, $bb, $8e, $db, $0e, $fe, $65, $ec, $0f, $0d
    .byte $4e, $65, $e1, $0f, $0e, $4e, $02, $e0, $0f, $10, $fe, $e5, $e1
    .byte $1b, $85, $7b, $0c, $5b, $95, $78, $2d, $90, $b5
    .byte $ff
    ; level 3-3
E_GroundArea1:
    .byte $a5, $86, $e4, $28, $18, $a8, $45, $83, $69, $03
    .byte $c6, $29, $9b, $83, $16, $a4, $88, $24, $e9, $28
    .byte $05, $a8, $7b, $28, $24, $8f, $c8, $03, $e8, $03
    .byte $46, $a8, $85, $24, $c8, $24
    .byte $ff
    ; level 8-3
E_GroundArea2:
    .byte $eb, $8e, $0f, $03, $fb, $05, $17, $85, $db, $8e
    .byte $0f, $07, $57, $05, $7b, $05, $9b, $80, $2b, $85
    .byte $fb, $05, $0f, $0b, $1b, $05, $9b, $05
    .byte $ff
    ; level 4-1
E_GroundArea3:
    .byte $2e, $c2, $66, $e2, $11, $0f, $07, $02, $11, $0f, $0c
    .byte $12, $11
    .byte $ff
    ; level 6-2
E_GroundArea4:
    .byte $0e, $c2, $a8, $ab, $00, $bb, $8e, $6b, $82, $de, $00, $a0
    .byte $33, $86, $43, $06, $3e, $b4, $a0, $cb, $02, $0f, $07
    .byte $7e, $42, $a6, $83, $02, $0f, $0a, $3b, $02, $cb, $37
    .byte $0f, $0c, $e3, $0e
    .byte $ff
    ; level 3-1
E_GroundArea5:
    .byte $9b, $8e, $ca, $0e, $ee, $42, $44, $5b, $86, $80, $b8
    .byte $1b, $80, $50, $ba, $10, $b7, $5b, $00, $17, $85
    .byte $4b, $05, $fe, $34, $40, $b7, $86, $c6, $06, $5b, $80
    .byte $83, $00, $d0, $38, $5b, $8e, $8a, $0e, $a6, $00
    .byte $bb, $0e, $c5, $80, $f3, $00
    .byte $ff
    ; level 1-1
E_GroundArea6:
    .byte $1e, $c2, $00, $6b, $06, $8b, $86, $63, $b7, $0f, $05
    .byte $03, $06, $23, $06, $4b, $b7, $bb, $00, $5b, $b7
    .byte $fb, $37, $3b, $b7, $0f, $0b, $1b, $37
    .byte $ff
    ; level 1-3/5-3
E_GroundArea7:
    .byte $2b, $d7, $e3, $03, $c2, $86, $e2, $06, $76, $a5
    .byte $a3, $8f, $03, $86, $2b, $57, $68, $28, $e9, $28
    .byte $e5, $83, $24, $8f, $36, $a8, $5b, $03
    .byte $ff
    ; level 2-3/7-3
E_GroundArea8:
    .byte $0f, $02, $78, $40, $48, $ce, $f8, $c3, $f8, $c3
    .byte $0f, $07, $7b, $43, $c6, $d0, $0f, $8a, $c8, $50
    .byte $ff
    ; level 2-1
E_GroundArea9:
    .byte $85, $86, $0b, $80, $1b, $00, $db, $37, $77, $80
    .byte $eb, $37, $fe, $2b, $20, $2b, $80, $7b, $38, $ab, $b8
    .byte $77, $86, $fe, $42, $20, $49, $86, $8b, $06, $9b, $80
    .byte $7b, $8e, $5b, $b7, $9b, $0e, $bb, $0e, $9b, $80
    ; end of data terminator here is also used by pipe intro area
E_GroundArea10:
    .byte $ff
    ; level 5-1
E_GroundArea11:
    .byte $0b, $80, $60, $38, $10, $b8, $c0, $3b, $db, $8e
    .byte $40, $b8, $f0, $38, $7b, $8e, $a0, $b8, $c0, $b8
    .byte $fb, $00, $a0, $b8, $30, $bb, $ee, $42, $88, $0f, $0b
    .byte $2b, $0e, $67, $0e
    .byte $ff
    ; cloud level used in levels 2-1 and 5-2
E_GroundArea12:
    .byte $0a, $aa, $0e, $28, $2a, $0e, $31, $88
    .byte $ff
    ; level 4-3
E_GroundArea13:
    .byte $c7, $83, $d7, $03, $42, $8f, $7a, $03, $05, $a4
    .byte $78, $24, $a6, $25, $e4, $25, $4b, $83, $e3, $03
    .byte $05, $a4, $89, $24, $b5, $24, $09, $a4, $65, $24
    .byte $c9, $24, $0f, $08, $85, $25
    .byte $ff
    ; level 6-3
E_GroundArea14:
    .byte $cd, $a5, $b5, $a8, $07, $a8, $76, $28, $cc, $25
    .byte $65, $a4, $a9, $24, $e5, $24, $19, $a4, $0f, $07
    .byte $95, $28, $e6, $24, $19, $a4, $d7, $29, $16, $a9
    .byte $58, $29, $97, $29
    .byte $ff
    ; level 6-1
E_GroundArea15:
    .byte $0f, $02, $02, $11, $0f, $07, $02, $11
    .byte $ff
    ; warp zone area used in level 4-2
E_GroundArea16:
    .byte $ff
    ; level 8-1
E_GroundArea17:
    .byte $2b, $82, $ab, $38, $de, $42, $e2, $1b, $b8, $eb
    .byte $3b, $db, $80, $8b, $b8, $1b, $82, $fb, $b8, $7b
    .byte $80, $fb, $3c, $5b, $bc, $7b, $b8, $1b, $8e, $cb
    .byte $0e, $1b, $8e, $0f, $0d, $2b, $3b, $bb, $b8, $eb, $82
    .byte $4b, $b8, $bb, $38, $3b, $b7, $bb, $02, $0f, $13
    .byte $1b, $00, $cb, $80, $6b, $bc
    .byte $ff
    ; level 5-2
E_GroundArea18:
    .byte $7b, $80, $ae, $00, $80, $8b, $8e, $e8, $05, $f9, $86
    .byte $17, $86, $16, $85, $4e, $2b, $80, $ab, $8e, $87, $85
    .byte $c3, $05, $8b, $82, $9b, $02, $ab, $02, $bb, $86
    .byte $cb, $06, $d3, $03, $3b, $8e, $6b, $0e, $a7, $8e
    .byte $ff
    ; level 8-2
E_GroundArea19:
    .byte $29, $8e, $52, $11, $83, $0e, $0f, $03, $9b, $0e
    .byte $2b, $8e, $5b, $0e, $cb, $8e, $fb, $0e, $fb, $82
    .byte $9b, $82, $bb, $02, $fe, $42, $e8, $bb, $8e, $0f, $0a
    .byte $ab, $0e, $cb, $0e, $f9, $0e, $88, $86, $a6, $06
    .byte $db, $02, $b6, $8e
    .byte $ff
    ; level 7-1
E_GroundArea20:
    .byte $ab, $ce, $de, $42, $c0, $cb, $ce, $5b, $8e, $1b, $ce
    .byte $4b, $85, $67, $45, $0f, $07, $2b, $00, $7b, $85
    .byte $97, $05, $0f, $0a, $92, $02
    .byte $ff
    ; cloud level used in levels 3-1 and 6-2
E_GroundArea21:
    .byte $0a, $aa, $0e, $24, $4a, $1e, $23, $aa
    .byte $ff
    ; level 3-2
E_GroundArea22:
    .byte $1b, $80, $bb, $38, $4b, $bc, $eb, $3b, $0f, $04
    .byte $2b, $00, $ab, $38, $eb, $00, $cb, $8e, $fb, $80
    .byte $ab, $b8, $6b, $80, $fb, $3c, $9b, $bb, $5b, $bc
    .byte $fb, $00, $6b, $b8, $fb, $38
    .byte $ff
    ; level 1-2
E_UndergroundArea1:
    .byte $0b, $86, $1a, $06, $db, $06, $de, $c2, $02, $f0, $3b
    .byte $bb, $80, $eb, $06, $0b, $86, $93, $06, $f0, $39
    .byte $0f, $06, $60, $b8, $1b, $86, $a0, $b9, $b7, $27
    .byte $bd, $27, $2b, $83, $a1, $26, $a9, $26, $ee, $25, $0b
    .byte $27, $b4
    .byte $ff
    ; level 4-2
E_UndergroundArea2:
    .byte $0f, $02, $1e, $2f, $60, $e0, $3a, $a5, $a7, $db, $80
    .byte $3b, $82, $8b, $02, $fe, $42, $68, $70, $bb, $25, $a7
    .byte $2c, $27, $b2, $26, $b9, $26, $9b, $80, $a8, $82
    .byte $b5, $27, $bc, $27, $b0, $bb, $3b, $82, $87, $34
    .byte $ee, $25, $6b
    .byte $ff
    ; underground bonus rooms area used in many levels
E_UndergroundArea3:
    .byte $1e, $a5, $0a, $2e, $28, $27, $2e, $33, $c7, $0f, $03, $1e, $40, $07
    .byte $2e, $30, $e7, $0f, $05, $1e, $24, $44, $0f, $07, $1e, $22, $6a
    .byte $2e, $23, $ab, $0f, $09, $1e, $41, $68, $1e, $2a, $8a, $2e, $23, $a2
    .byte $2e, $32, $ea
    .byte $ff
    ; water area used in levels 5-2 and 6-2
E_WaterArea1:
    .byte $3b, $87, $66, $27, $cc, $27, $ee, $31, $87, $ee, $23, $a7
    .byte $3b, $87, $db, $07
    .byte $ff
    ; level 2-2/7-2
E_WaterArea2:
    .byte $0f, $01, $2e, $25, $2b, $2e, $25, $4b, $4e, $25, $cb, $6b, $07
    .byte $97, $47, $e9, $87, $47, $c7, $7a, $07, $d6, $c7
    .byte $78, $07, $38, $87, $ab, $47, $e3, $07, $9b, $87
    .byte $0f, $09, $68, $47, $db, $c7, $3b, $c7
    .byte $ff
    ; water area used in level 8-4
E_WaterArea3:
    .byte $47, $9b, $cb, $07, $fa, $1d, $86, $9b, $3a, $87
    .byte $56, $07, $88, $1b, $07, $9d, $2e, $65, $f0
    .byte $ff
    ; AREA OBJECT DATA
    ; level 1-4/6-4
L_CastleArea1:
    .byte $9b, $07
    .byte $05, $32, $06, $33, $07, $34, $ce, $03, $dc, $51
    .byte $ee, $07, $73, $e0, $74, $0a, $7e, $06, $9e, $0a
    .byte $ce, $06, $e4, $00, $e8, $0a, $fe, $0a, $2e, $89
    .byte $4e, $0b, $54, $0a, $14, $8a, $c4, $0a, $34, $8a
    .byte $7e, $06, $c7, $0a, $01, $e0, $02, $0a, $47, $0a
    .byte $81, $60, $82, $0a, $c7, $0a, $0e, $87, $7e, $02
    .byte $a7, $02, $b3, $02, $d7, $02, $e3, $02, $07, $82
    .byte $13, $02, $3e, $06, $7e, $02, $ae, $07, $fe, $0a
    .byte $0d, $c4, $cd, $43, $ce, $09, $de, $0b, $dd, $42
    .byte $fe, $02, $5d, $c7
    .byte $fd
    ; level 4-4
L_CastleArea2:
    .byte $5b, $07
    .byte $05, $32, $06, $33, $07, $34, $5e, $0a, $68, $64
    .byte $98, $64, $a8, $64, $ce, $06, $fe, $02, $0d, $01
    .byte $1e, $0e, $7e, $02, $94, $63, $b4, $63, $d4, $63
    .byte $f4, $63, $14, $e3, $2e, $0e, $5e, $02, $64, $35
    .byte $88, $72, $be, $0e, $0d, $04, $ae, $02, $ce, $08
    .byte $cd, $4b, $fe, $02, $0d, $05, $68, $31, $7e, $0a
    .byte $96, $31, $a9, $63, $a8, $33, $d5, $30, $ee, $02
    .byte $e6, $62, $f4, $61, $04, $b1, $08, $3f, $44, $33
    .byte $94, $63, $a4, $31, $e4, $31, $04, $bf, $08, $3f
    .byte $04, $bf, $08, $3f, $cd, $4b, $03, $e4, $0e, $03
    .byte $2e, $01, $7e, $06, $be, $02, $de, $06, $fe, $0a
    .byte $0d, $c4, $cd, $43, $ce, $09, $de, $0b, $dd, $42
    .byte $fe, $02, $5d, $c7
    .byte $fd
    ; level 2-4/5-4
L_CastleArea3:
    .byte $9b, $07
    .byte $05, $32, $06, $33, $07, $34, $fe, $00, $27, $b1
    .byte $65, $32, $75, $0a, $71, $00, $b7, $31, $08, $e4
    .byte $18, $64, $1e, $04, $57, $3b, $bb, $0a, $17, $8a
    .byte $27, $3a, $73, $0a, $7b, $0a, $d7, $0a, $e7, $3a
    .byte $3b, $8a, $97, $0a, $fe, $08, $24, $8a, $2e, $00
    .byte $3e, $40, $38, $64, $6f, $00, $9f, $00, $be, $43
    .byte $c8, $0a, $c9, $63, $ce, $07, $fe, $07, $2e, $81
    .byte $66, $42, $6a, $42, $79, $0a, $be, $00, $c8, $64
    .byte $f8, $64, $08, $e4, $2e, $07, $7e, $03, $9e, $07
    .byte $be, $03, $de, $07, $fe, $0a, $03, $a5, $0d, $44
    .byte $cd, $43, $ce, $09, $dd, $42, $de, $0b, $fe, $02
    .byte $5d, $c7
    .byte $fd
    ; level 3-4
L_CastleArea4:
    .byte $9b, $07
    .byte $05, $32, $06, $33, $07, $34, $fe, $06, $0c, $81
    .byte $39, $0a, $5c, $01, $89, $0a, $ac, $01, $d9, $0a
    .byte $fc, $01, $2e, $83, $a7, $01, $b7, $00, $c7, $01
    .byte $de, $0a, $fe, $02, $4e, $83, $5a, $32, $63, $0a
    .byte $69, $0a, $7e, $02, $ee, $03, $fa, $32, $03, $8a
    .byte $09, $0a, $1e, $02, $ee, $03, $fa, $32, $03, $8a
    .byte $09, $0a, $14, $42, $1e, $02, $7e, $0a, $9e, $07
    .byte $fe, $0a, $2e, $86, $5e, $0a, $8e, $06, $be, $0a
    .byte $ee, $07, $3e, $83, $5e, $07, $fe, $0a, $0d, $c4
    .byte $41, $52, $51, $52, $cd, $43, $ce, $09, $de, $0b
    .byte $dd, $42, $fe, $02, $5d, $c7
    .byte $fd
    ; level 7-4
L_CastleArea5:
    .byte $5b, $07
    .byte $05, $32, $06, $33, $07, $34, $fe, $0a, $ae, $86
    .byte $be, $07, $fe, $02, $0d, $02, $27, $32, $46, $61
    .byte $55, $62, $5e, $0e, $1e, $82, $68, $3c, $74, $3a
    .byte $7d, $4b, $5e, $8e, $7d, $4b, $7e, $82, $84, $62
    .byte $94, $61, $a4, $31, $bd, $4b, $ce, $06, $fe, $02
    .byte $0d, $06, $34, $31, $3e, $0a, $64, $32, $75, $0a
    .byte $7b, $61, $a4, $33, $ae, $02, $de, $0e, $3e, $82
    .byte $64, $32, $78, $32, $b4, $36, $c8, $36, $dd, $4b
    .byte $44, $b2, $58, $32, $94, $63, $a4, $3e, $ba, $30
    .byte $c9, $61, $ce, $06, $dd, $4b, $ce, $86, $dd, $4b
    .byte $fe, $02, $2e, $86, $5e, $02, $7e, $06, $fe, $02
    .byte $1e, $86, $3e, $02, $5e, $06, $7e, $02, $9e, $06
    .byte $fe, $0a, $0d, $c4, $cd, $43, $ce, $09, $de, $0b
    .byte $dd, $42, $fe, $02, $5d, $c7
    .byte $fd
    ; level 8-4
L_CastleArea6:
    .byte $5b, $06
    .byte $05, $32, $06, $33, $07, $34, $5e, $0a, $ae, $02
    .byte $0d, $01, $39, $73, $0d, $03, $39, $7b, $4d, $4b
    .byte $de, $06, $1e, $8a, $ae, $06, $c4, $33, $16, $fe
    .byte $a5, $77, $fe, $02, $fe, $82, $0d, $07, $39, $73
    .byte $a8, $74, $ed, $4b, $49, $fb, $e8, $74, $fe, $0a
    .byte $2e, $82, $67, $02, $84, $7a, $87, $31, $0d, $0b
    .byte $fe, $02, $0d, $0c, $39, $73, $5e, $06, $c6, $76
    .byte $45, $ff, $be, $0a, $dd, $48, $fe, $06, $3d, $cb
    .byte $46, $7e, $ad, $4a, $fe, $82, $39, $f3, $a9, $7b
    .byte $4e, $8a, $9e, $07, $fe, $0a, $0d, $c4, $cd, $43
    .byte $ce, $09, $de, $0b, $dd, $42, $fe, $02, $5d, $c7
    .byte $fd
    ; level 3-3
L_GroundArea1:
    .byte $94, $11
    .byte $0f, $26, $fe, $10, $28, $94, $65, $15, $eb, $12
    .byte $fa, $41, $4a, $96, $54, $40, $a4, $42, $b7, $13
    .byte $e9, $19, $f5, $15, $11, $80, $47, $42, $71, $13
    .byte $80, $41, $15, $92, $1b, $1f, $24, $40, $55, $12
    .byte $64, $40, $95, $12, $a4, $40, $d2, $12, $e1, $40
    .byte $13, $c0, $2c, $17, $2f, $12, $49, $13, $83, $40
    .byte $9f, $14, $a3, $40, $17, $92, $83, $13, $92, $41
    .byte $b9, $14, $c5, $12, $c8, $40, $d4, $40, $4b, $92
    .byte $78, $1b, $9c, $94, $9f, $11, $df, $14, $fe, $11
    .byte $7d, $c1, $9e, $42, $cf, $20
    .byte $fd
    ; level 8-3
L_GroundArea2:
    .byte $90, $b1
    .byte $0f, $26, $29, $91, $7e, $42, $fe, $40, $28, $92
    .byte $4e, $42, $2e, $c0, $57, $73, $c3, $25, $c7, $27
    .byte $23, $84, $33, $20, $5c, $01, $77, $63, $88, $62
    .byte $99, $61, $aa, $60, $bc, $01, $ee, $42, $4e, $c0
    .byte $69, $11, $7e, $42, $de, $40, $f8, $62, $0e, $c2
    .byte $ae, $40, $d7, $63, $e7, $63, $33, $a7, $37, $27
    .byte $43, $04, $cc, $01, $e7, $73, $0c, $81, $3e, $42
    .byte $0d, $0a, $5e, $40, $88, $72, $be, $42, $e7, $87
    .byte $fe, $40, $39, $e1, $4e, $00, $69, $60, $87, $60
    .byte $a5, $60, $c3, $31, $fe, $31, $6d, $c1, $be, $42
    .byte $ef, $20
    .byte $fd
    ; level 4-1
L_GroundArea3:
    .byte $52, $21
    .byte $0f, $20, $6e, $40, $58, $f2, $93, $01, $97, $00
    .byte $0c, $81, $97, $40, $a6, $41, $c7, $40, $0d, $04
    .byte $03, $01, $07, $01, $23, $01, $27, $01, $ec, $03
    .byte $ac, $f3, $c3, $03, $78, $e2, $94, $43, $47, $f3
    .byte $74, $43, $47, $fb, $74, $43, $2c, $f1, $4c, $63
    .byte $47, $00, $57, $21, $5c, $01, $7c, $72, $39, $f1
    .byte $ec, $02, $4c, $81, $d8, $62, $ec, $01, $0d, $0d
    .byte $0f, $38, $c7, $07, $ed, $4a, $1d, $c1, $5f, $26
    .byte $fd
    ; level 6-2
L_GroundArea4:
    .byte $54, $21
    .byte $0f, $26, $a7, $22, $37, $fb, $73, $20, $83, $07
    .byte $87, $02, $93, $20, $c7, $73, $04, $f1, $06, $31
    .byte $39, $71, $59, $71, $e7, $73, $37, $a0, $47, $04
    .byte $86, $7c, $e5, $71, $e7, $31, $33, $a4, $39, $71
    .byte $a9, $71, $d3, $23, $08, $f2, $13, $05, $27, $02
    .byte $49, $71, $75, $75, $e8, $72, $67, $f3, $99, $71
    .byte $e7, $20, $f4, $72, $f7, $31, $17, $a0, $33, $20
    .byte $39, $71, $73, $28, $bc, $05, $39, $f1, $79, $71
    .byte $a6, $21, $c3, $06, $d3, $20, $dc, $00, $fc, $00
    .byte $07, $a2, $13, $21, $5f, $32, $8c, $00, $98, $7a
    .byte $c7, $63, $d9, $61, $03, $a2, $07, $22, $74, $72
    .byte $77, $31, $e7, $73, $39, $f1, $58, $72, $77, $73
    .byte $d8, $72, $7f, $b1, $97, $73, $b6, $64, $c5, $65
    .byte $d4, $66, $e3, $67, $f3, $67, $8d, $c1, $cf, $26
    .byte $fd
    ; level 3-1
L_GroundArea5:
    .byte $52, $31
    .byte $0f, $20, $6e, $66, $07, $81, $36, $01, $66, $00
    .byte $a7, $22, $08, $f2, $67, $7b, $dc, $02, $98, $f2
    .byte $d7, $20, $39, $f1, $9f, $33, $dc, $27, $dc, $57
    .byte $23, $83, $57, $63, $6c, $51, $87, $63, $99, $61
    .byte $a3, $06, $b3, $21, $77, $f3, $f3, $21, $f7, $2a
    .byte $13, $81, $23, $22, $53, $00, $63, $22, $e9, $0b
    .byte $0c, $83, $13, $21, $16, $22, $33, $05, $8f, $35
    .byte $ec, $01, $63, $a0, $67, $20, $73, $01, $77, $01
    .byte $83, $20, $87, $20, $b3, $20, $b7, $20, $c3, $01
    .byte $c7, $00, $d3, $20, $d7, $20, $67, $a0, $77, $07
    .byte $87, $22, $e8, $62, $f5, $65, $1c, $82, $7f, $38
    .byte $8d, $c1, $cf, $26
    .byte $fd
    ; level 1-1
L_GroundArea6:
    .byte $50, $21
    .byte $07, $81, $47, $24, $57, $00, $63, $01, $77, $01
    .byte $c9, $71, $68, $f2, $e7, $73, $97, $fb, $06, $83
    .byte $5c, $01, $d7, $22, $e7, $00, $03, $a7, $6c, $02
    .byte $b3, $22, $e3, $01, $e7, $07, $47, $a0, $57, $06
    .byte $a7, $01, $d3, $00, $d7, $01, $07, $81, $67, $20
    .byte $93, $22, $03, $a3, $1c, $61, $17, $21, $6f, $33
    .byte $c7, $63, $d8, $62, $e9, $61, $fa, $60, $4f, $b3
    .byte $87, $63, $9c, $01, $b7, $63, $c8, $62, $d9, $61
    .byte $ea, $60, $39, $f1, $87, $21, $a7, $01, $b7, $20
    .byte $39, $f1, $5f, $38, $6d, $c1, $af, $26
    .byte $fd
    ; level 1-3/5-3
L_GroundArea7:
    .byte $90, $11
    .byte $0f, $26, $fe, $10, $2a, $93, $87, $17, $a3, $14
    .byte $b2, $42, $0a, $92, $19, $40, $36, $14, $50, $41
    .byte $82, $16, $2b, $93, $24, $41, $bb, $14, $b8, $00
    .byte $c2, $43, $c3, $13, $1b, $94, $67, $12, $c4, $15
    .byte $53, $c1, $d2, $41, $12, $c1, $29, $13, $85, $17
    .byte $1b, $92, $1a, $42, $47, $13, $83, $41, $a7, $13
    .byte $0e, $91, $a7, $63, $b7, $63, $c5, $65, $d5, $65
    .byte $dd, $4a, $e3, $67, $f3, $67, $8d, $c1, $ae, $42
    .byte $df, $20
    .byte $fd
    ; level 2-3/7-3
L_GroundArea8:
    .byte $90, $11
    .byte $0f, $26, $6e, $10, $8b, $17, $af, $32, $d8, $62
    .byte $e8, $62, $fc, $3f, $ad, $c8, $f8, $64, $0c, $be
    .byte $43, $43, $f8, $64, $0c, $bf, $73, $40, $84, $40
    .byte $93, $40, $a4, $40, $b3, $40, $f8, $64, $48, $e4
    .byte $5c, $39, $83, $40, $92, $41, $b3, $40, $f8, $64
    .byte $48, $e4, $5c, $39, $f8, $64, $13, $c2, $37, $65
    .byte $4c, $24, $63, $00, $97, $65, $c3, $42, $0b, $97
    .byte $ac, $32, $f8, $64, $0c, $be, $53, $45, $9d, $48
    .byte $f8, $64, $2a, $e2, $3c, $47, $56, $43, $ba, $62
    .byte $f8, $64, $0c, $b7, $88, $64, $bc, $31, $d4, $45
    .byte $fc, $31, $3c, $b1, $78, $64, $8c, $38, $0b, $9c
    .byte $1a, $33, $18, $61, $28, $61, $39, $60, $5d, $4a
    .byte $ee, $11, $0f, $b8, $1d, $c1, $3e, $42, $6f, $20
    .byte $fd
    ; level 2-1
L_GroundArea9:
    .byte $52, $31
    .byte $0f, $20, $6e, $40, $f7, $20, $07, $84, $17, $20
    .byte $4f, $34, $c3, $03, $c7, $02, $d3, $22, $27, $e3
    .byte $39, $61, $e7, $73, $5c, $e4, $57, $00, $6c, $73
    .byte $47, $a0, $53, $06, $63, $22, $a7, $73, $fc, $73
    .byte $13, $a1, $33, $05, $43, $21, $5c, $72, $c3, $23
    .byte $cc, $03, $77, $fb, $ac, $02, $39, $f1, $a7, $73
    .byte $d3, $04, $e8, $72, $e3, $22, $26, $f4, $bc, $02
    .byte $8c, $81, $a8, $62, $17, $87, $43, $24, $a7, $01
    .byte $c3, $04, $08, $f2, $97, $21, $a3, $02, $c9, $0b
    .byte $e1, $69, $f1, $69, $8d, $c1, $cf, $26
    .byte $fd
    ; pipe intro area
L_GroundArea10:
    .byte $38, $11
    .byte $0f, $26, $ad, $40, $3d, $c7
    .byte $fd
    ; level 5-1
L_GroundArea11:
    .byte $95, $b1
    .byte $0f, $26, $0d, $02, $c8, $72, $1c, $81, $38, $72
    .byte $0d, $05, $97, $34, $98, $62, $a3, $20, $b3, $06
    .byte $c3, $20, $cc, $03, $f9, $91, $2c, $81, $48, $62
    .byte $0d, $09, $37, $63, $47, $03, $57, $21, $8c, $02
    .byte $c5, $79, $c7, $31, $f9, $11, $39, $f1, $a9, $11
    .byte $6f, $b4, $d3, $65, $e3, $65, $7d, $c1, $bf, $26
    .byte $fd
    ; cloud level used in levels 2-1 and 5-2
L_GroundArea12:
    .byte $00, $c1
    .byte $4c, $00, $f4, $4f, $0d, $02, $02, $42, $43, $4f
    .byte $52, $c2, $de, $00, $5a, $c2, $4d, $c7
    .byte $fd
    ; level 4-3
L_GroundArea13:
    .byte $90, $51
    .byte $0f, $26, $ee, $10, $0b, $94, $33, $14, $42, $42
    .byte $77, $16, $86, $44, $02, $92, $4a, $16, $69, $42
    .byte $73, $14, $b0, $00, $c7, $12, $05, $c0, $1c, $17
    .byte $1f, $11, $36, $12, $8f, $14, $91, $40, $1b, $94
    .byte $35, $12, $34, $42, $60, $42, $61, $12, $87, $12
    .byte $96, $40, $a3, $14, $1c, $98, $1f, $11, $47, $12
    .byte $9f, $15, $cc, $15, $cf, $11, $05, $c0, $1f, $15
    .byte $39, $12, $7c, $16, $7f, $11, $82, $40, $98, $12
    .byte $df, $15, $16, $c4, $17, $14, $54, $12, $9b, $16
    .byte $28, $94, $ce, $01, $3d, $c1, $5e, $42, $8f, $20
    .byte $fd
    ; level 6-3
L_GroundArea14:
    .byte $97, $11
    .byte $0f, $26, $fe, $10, $2b, $92, $57, $12, $8b, $12
    .byte $c0, $41, $f7, $13, $5b, $92, $69, $0b, $bb, $12
    .byte $b2, $46, $19, $93, $71, $00, $17, $94, $7c, $14
    .byte $7f, $11, $93, $41, $bf, $15, $fc, $13, $ff, $11
    .byte $2f, $95, $50, $42, $51, $12, $58, $14, $a6, $12
    .byte $db, $12, $1b, $93, $46, $43, $7b, $12, $8d, $49
    .byte $b7, $14, $1b, $94, $49, $0b, $bb, $12, $fc, $13
    .byte $ff, $12, $03, $c1, $2f, $15, $43, $12, $4b, $13
    .byte $77, $13, $9d, $4a, $15, $c1, $a1, $41, $c3, $12
    .byte $fe, $01, $7d, $c1, $9e, $42, $cf, $20
    .byte $fd
    ; level 6-1
L_GroundArea15:
    .byte $52, $21
    .byte $0f, $20, $6e, $44, $0c, $f1, $4c, $01, $aa, $35
    .byte $d9, $34, $ee, $20, $08, $b3, $37, $32, $43, $04
    .byte $4e, $21, $53, $20, $7c, $01, $97, $21, $b7, $07
    .byte $9c, $81, $e7, $42, $5f, $b3, $97, $63, $ac, $02
    .byte $c5, $41, $49, $e0, $58, $61, $76, $64, $85, $65
    .byte $94, $66, $a4, $22, $a6, $03, $c8, $22, $dc, $02
    .byte $68, $f2, $96, $42, $13, $82, $17, $02, $af, $34
    .byte $f6, $21, $fc, $06, $26, $80, $2a, $24, $36, $01
    .byte $8c, $00, $ff, $35, $4e, $a0, $55, $21, $77, $20
    .byte $87, $07, $89, $22, $ae, $21, $4c, $82, $9f, $34
    .byte $ec, $01, $03, $e7, $13, $67, $8d, $4a, $ad, $41
    .byte $0f, $a6
    .byte $fd
    ; warp zone area used in level 4-2
L_GroundArea16:
    .byte $10, $51
    .byte $4c, $00, $c7, $12, $c6, $42, $03, $92, $02, $42
    .byte $29, $12, $63, $12, $62, $42, $69, $14, $a5, $12
    .byte $a4, $42, $e2, $14, $e1, $44, $f8, $16, $37, $c1
    .byte $8f, $38, $02, $bb, $28, $7a, $68, $7a, $a8, $7a
    .byte $e0, $6a, $f0, $6a, $6d, $c5
    .byte $fd
    ; level 8-1
L_GroundArea17:
    .byte $92, $31
    .byte $0f, $20, $6e, $40, $0d, $02, $37, $73, $ec, $00
    .byte $0c, $80, $3c, $00, $6c, $00, $9c, $00, $06, $c0
    .byte $c7, $73, $06, $83, $28, $72, $96, $40, $e7, $73
    .byte $26, $c0, $87, $7b, $d2, $41, $39, $f1, $c8, $f2
    .byte $97, $e3, $a3, $23, $e7, $02, $e3, $07, $f3, $22
    .byte $37, $e3, $9c, $00, $bc, $00, $ec, $00, $0c, $80
    .byte $3c, $00, $86, $21, $a6, $06, $b6, $24, $5c, $80
    .byte $7c, $00, $9c, $00, $29, $e1, $dc, $05, $f6, $41
    .byte $dc, $80, $e8, $72, $0c, $81, $27, $73, $4c, $01
    .byte $66, $74, $0d, $11, $3f, $35, $b6, $41, $2c, $82
    .byte $36, $40, $7c, $02, $86, $40, $f9, $61, $39, $e1
    .byte $ac, $04, $c6, $41, $0c, $83, $16, $41, $88, $f2
    .byte $39, $f1, $7c, $00, $89, $61, $9c, $00, $a7, $63
    .byte $bc, $00, $c5, $65, $dc, $00, $e3, $67, $f3, $67
    .byte $8d, $c1, $cf, $26
    .byte $fd
    ; level 5-2
L_GroundArea18:
    .byte $55, $b1
    .byte $0f, $26, $cf, $33, $07, $b2, $15, $11, $52, $42
    .byte $99, $0b, $ac, $02, $d3, $24, $d6, $42, $d7, $25
    .byte $23, $84, $cf, $33, $07, $e3, $19, $61, $78, $7a
    .byte $ef, $33, $2c, $81, $46, $64, $55, $65, $65, $65
    .byte $ec, $74, $47, $82, $53, $05, $63, $21, $62, $41
    .byte $96, $22, $9a, $41, $cc, $03, $b9, $91, $39, $f1
    .byte $63, $26, $67, $27, $d3, $06, $fc, $01, $18, $e2
    .byte $d9, $07, $e9, $04, $0c, $86, $37, $22, $93, $24
    .byte $87, $84, $ac, $02, $c2, $41, $c3, $23, $d9, $71
    .byte $fc, $01, $7f, $b1, $9c, $00, $a7, $63, $b6, $64
    .byte $cc, $00, $d4, $66, $e3, $67, $f3, $67, $8d, $c1
    .byte $cf, $26
    .byte $fd
    ; level 8-2
L_GroundArea19:
    .byte $50, $b1
    .byte $0f, $26, $fc, $00, $1f, $b3, $5c, $00, $65, $65
    .byte $74, $66, $83, $67, $93, $67, $dc, $73, $4c, $80
    .byte $b3, $20, $c9, $0b, $c3, $08, $d3, $2f, $dc, $00
    .byte $2c, $80, $4c, $00, $8c, $00, $d3, $2e, $ed, $4a
    .byte $fc, $00, $d7, $a1, $ec, $01, $4c, $80, $59, $11
    .byte $d8, $11, $da, $10, $37, $a0, $47, $04, $99, $11
    .byte $e7, $21, $3a, $90, $67, $20, $76, $10, $77, $60
    .byte $87, $07, $d8, $12, $39, $f1, $ac, $00, $e9, $71
    .byte $0c, $80, $2c, $00, $4c, $05, $c7, $7b, $39, $f1
    .byte $ec, $00, $f9, $11, $0c, $82, $6f, $34, $f8, $11
    .byte $fa, $10, $7f, $b2, $ac, $00, $b6, $64, $cc, $01
    .byte $e3, $67, $f3, $67, $8d, $c1, $cf, $26
    .byte $fd
    ; level 7-1
L_GroundArea20:
    .byte $52, $b1
    .byte $0f, $20, $6e, $45, $39, $91, $b3, $04, $c3, $21
    .byte $c8, $11, $ca, $10, $49, $91, $7c, $73, $e8, $12
    .byte $88, $91, $8a, $10, $e7, $21, $05, $91, $07, $30
    .byte $17, $07, $27, $20, $49, $11, $9c, $01, $c8, $72
    .byte $23, $a6, $27, $26, $d3, $03, $d8, $7a, $89, $91
    .byte $d8, $72, $39, $f1, $a9, $11, $09, $f1, $63, $24
    .byte $67, $24, $d8, $62, $28, $91, $2a, $10, $56, $21
    .byte $70, $04, $79, $0b, $8c, $00, $94, $21, $9f, $35
    .byte $2f, $b8, $3d, $c1, $7f, $26
    .byte $fd
    ; cloud level used in levels 3-1 and 6-2
L_GroundArea21:
    .byte $06, $c1
    .byte $4c, $00, $f4, $4f, $0d, $02, $06, $20, $24, $4f
    .byte $35, $a0, $36, $20, $53, $46, $d5, $20, $d6, $20
    .byte $34, $a1, $73, $49, $74, $20, $94, $20, $b4, $20
    .byte $d4, $20, $f4, $20, $2e, $80, $59, $42, $4d, $c7
    .byte $fd
    ; level 3-2
L_GroundArea22:
    .byte $96, $31
    .byte $0f, $26, $0d, $03, $1a, $60, $77, $42, $c4, $00
    .byte $c8, $62, $b9, $e1, $d3, $06, $d7, $07, $f9, $61
    .byte $0c, $81, $4e, $b1, $8e, $b1, $bc, $01, $e4, $50
    .byte $e9, $61, $0c, $81, $0d, $0a, $84, $43, $98, $72
    .byte $0d, $0c, $0f, $38, $1d, $c1, $5f, $26
    .byte $fd
    ; level 1-2
L_UndergroundArea1:
    .byte $48, $0f
    .byte $0e, $01, $5e, $02, $a7, $00, $bc, $73, $1a, $e0
    .byte $39, $61, $58, $62, $77, $63, $97, $63, $b8, $62
    .byte $d6, $07, $f8, $62, $19, $e1, $75, $52, $86, $40
    .byte $87, $50, $95, $52, $93, $43, $a5, $21, $c5, $52
    .byte $d6, $40, $d7, $20, $e5, $06, $e6, $51, $3e, $8d
    .byte $5e, $03, $67, $52, $77, $52, $7e, $02, $9e, $03
    .byte $a6, $43, $a7, $23, $de, $05, $fe, $02, $1e, $83
    .byte $33, $54, $46, $40, $47, $21, $56, $04, $5e, $02
    .byte $83, $54, $93, $52, $96, $07, $97, $50, $be, $03
    .byte $c7, $23, $fe, $02, $0c, $82, $43, $45, $45, $24
    .byte $46, $24, $90, $08, $95, $51, $78, $fa, $d7, $73
    .byte $39, $f1, $8c, $01, $a8, $52, $b8, $52, $cc, $01
    .byte $5f, $b3, $97, $63, $9e, $00, $0e, $81, $16, $24
    .byte $66, $04, $8e, $00, $fe, $01, $08, $d2, $0e, $06
    .byte $6f, $47, $9e, $0f, $0e, $82, $2d, $47, $28, $7a
    .byte $68, $7a, $a8, $7a, $ae, $01, $de, $0f, $6d, $c5
    .byte $fd
    ; level 4-2
L_UndergroundArea2:
    .byte $48, $0f
    .byte $0e, $01, $5e, $02, $bc, $01, $fc, $01, $2c, $82
    .byte $41, $52, $4e, $04, $67, $25, $68, $24, $69, $24
    .byte $ba, $42, $c7, $04, $de, $0b, $b2, $87, $fe, $02
    .byte $2c, $e1, $2c, $71, $67, $01, $77, $00, $87, $01
    .byte $8e, $00, $ee, $01, $f6, $02, $03, $85, $05, $02
    .byte $13, $21, $16, $02, $27, $02, $2e, $02, $88, $72
    .byte $c7, $20, $d7, $07, $e4, $76, $07, $a0, $17, $06
    .byte $48, $7a, $76, $20, $98, $72, $79, $e1, $88, $62
    .byte $9c, $01, $b7, $73, $dc, $01, $f8, $62, $fe, $01
    .byte $08, $e2, $0e, $00, $6e, $02, $73, $20, $77, $23
    .byte $83, $04, $93, $20, $ae, $00, $fe, $0a, $0e, $82
    .byte $39, $71, $a8, $72, $e7, $73, $0c, $81, $8f, $32
    .byte $ae, $00, $fe, $04, $04, $d1, $17, $04, $26, $49
    .byte $27, $29, $df, $33, $fe, $02, $44, $f6, $7c, $01
    .byte $8e, $06, $bf, $47, $ee, $0f, $4d, $c7, $0e, $82
    .byte $68, $7a, $ae, $01, $de, $0f, $6d, $c5
    .byte $fd
    ; underground bonus rooms area used in many levels
L_UndergroundArea3:
    .byte $48, $01
    .byte $0e, $01, $00, $5a, $3e, $06, $45, $46, $47, $46
    .byte $53, $44, $ae, $01, $df, $4a, $4d, $c7, $0e, $81
    .byte $00, $5a, $2e, $04, $37, $28, $3a, $48, $46, $47
    .byte $c7, $07, $ce, $0f, $df, $4a, $4d, $c7, $0e, $81
    .byte $00, $5a, $33, $53, $43, $51, $46, $40, $47, $50
    .byte $53, $04, $55, $40, $56, $50, $62, $43, $64, $40
    .byte $65, $50, $71, $41, $73, $51, $83, $51, $94, $40
    .byte $95, $50, $a3, $50, $a5, $40, $a6, $50, $b3, $51
    .byte $b6, $40, $b7, $50, $c3, $53, $df, $4a, $4d, $c7
    .byte $0e, $81, $00, $5a, $2e, $02, $36, $47, $37, $52
    .byte $3a, $49, $47, $25, $a7, $52, $d7, $04, $df, $4a
    .byte $4d, $c7, $0e, $81, $00, $5a, $3e, $02, $44, $51
    .byte $53, $44, $54, $44, $55, $24, $a1, $54, $ae, $01
    .byte $b4, $21, $df, $4a, $e5, $07, $4d, $c7
    .byte $fd
    ; water area used in levels 5-2 and 6-2
L_WaterArea1:
    .byte $41, $01
    .byte $b4, $34, $c8, $52, $f2, $51, $47, $d3, $6c, $03
    .byte $65, $49, $9e, $07, $be, $01, $cc, $03, $fe, $07
    .byte $0d, $c9, $1e, $01, $6c, $01, $62, $35, $63, $53
    .byte $8a, $41, $ac, $01, $b3, $53, $e9, $51, $26, $c3
    .byte $27, $33, $63, $43, $64, $33, $ba, $60, $c9, $61
    .byte $ce, $0b, $e5, $09, $ee, $0f, $7d, $ca, $7d, $47
    .byte $fd
    ; level 2-2/7-2
L_WaterArea2:
    .byte $41, $01
    .byte $b8, $52, $ea, $41, $27, $b2, $b3, $42, $16, $d4
    .byte $4a, $42, $a5, $51, $a7, $31, $27, $d3, $08, $e2
    .byte $16, $64, $2c, $04, $38, $42, $76, $64, $88, $62
    .byte $de, $07, $fe, $01, $0d, $c9, $23, $32, $31, $51
    .byte $98, $52, $0d, $c9, $59, $42, $63, $53, $67, $31
    .byte $14, $c2, $36, $31, $87, $53, $17, $e3, $29, $61
    .byte $30, $62, $3c, $08, $42, $37, $59, $40, $6a, $42
    .byte $99, $40, $c9, $61, $d7, $63, $39, $d1, $58, $52
    .byte $c3, $67, $d3, $31, $dc, $06, $f7, $42, $fa, $42
    .byte $23, $b1, $43, $67, $c3, $34, $c7, $34, $d1, $51
    .byte $43, $b3, $47, $33, $9a, $30, $a9, $61, $b8, $62
    .byte $be, $0b, $d5, $09, $de, $0f, $0d, $ca, $7d, $47
    .byte $fd
    ; water area used in level 8-4
L_WaterArea3:
    .byte $49, $0f
    .byte $1e, $01, $39, $73, $5e, $07, $ae, $0b, $1e, $82
    .byte $6e, $88, $9e, $02, $0d, $04, $2e, $0b, $45, $09
    .byte $4e, $0f, $ed, $47
    .byte $fd

    ; ------------------------------------------------------------------------------------------------
    ; unused space
    .byte $ff

    ; ------------------------------------------------------------------------------------------------
    ; indirect jump routine called when
    ; $0770 is set to 1
.code

.proc GameMode
    lda OperMode_Task
    jsr JumpEngine

    .word InitializeArea
    .word ScreenRoutines
    .word SecondaryGameSetup
    .word GameCoreRoutine
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc GameCoreRoutine

    .export UpdScrollVar

    ldx CurrentPlayer                             ; get which player is on the screen
    ; use appropriate player's controller bits
    mb SavedJoypadBits := SavedJoypadBits[ x ]
    jsr GameRoutines                              ; execute one of many possible subs

    if OperMode_Task < #3     ; check major task of operating mode ; if we are supposed to be here,; branch to the game engine itself
        rts
    endif

    jsr ProcFireball_Bubble                       ; process fireballs and air bubbles

    ldx #0
    repeat
        stx ObjectOffset                          ; put incremented offset in X as enemy object offset
        jsr EnemiesAndLoopsCore                   ; process enemy objects
        jsr FloateyNumbersRoutine                 ; process floatey numbers
        inx
    until x = #$06                                ; do these two subroutines until the whole buffer is done

    jsr GetPlayerOffscreenBits                    ; get offscreen bits for player object
    jsr RelativePlayerPosition                    ; get relative coordinates for player object
    jsr PlayerGfxHandler                          ; draw the player
    jsr BlockObjMT_Updater                        ; replace block objects with metatiles if necessary

    
    mb x,ObjectOffset := #1                         ; set offset for second
    jsr BlockObjectsCore                          ; process second block object

    dex
    stx ObjectOffset                              ; set offset for first
    jsr BlockObjectsCore                          ; process first block object
    jsr MiscObjectsCore                           ; process misc objects (hammer, jumping coins)
    jsr ProcessCannons                            ; process bullet bill cannons
    jsr ProcessWhirlpools                         ; process whirlpools
    jsr FlagpoleRoutine                           ; process the flagpole
    jsr RunGameTimer                              ; count down the game timer
    jsr ColorRotation                             ; cycle one of the background colors
    ; if Player_Y_HighPos < #2
    if lda Player_Y_HighPos : cmp #2 == negative                       ; if player is below the screen, don't bother with the music

        ; if star mario invincibility timer at zero, skip this part OP: call jsr directly-this is the only way to hit the jsr in this block
        if !StarInvincibleTimer goto ClrPlrPal    

        if a = #4 && !IntervalTimerControl        ; if at a certain point, AND if interval timer expired,re-attain appropriate level music
            jsr GetAreaMusic
        endif

    endif

    ldy StarInvincibleTimer                       ; get invincibility timer
    lda FrameCounter                              ; get frame counter

    if y < #$08                                   ; if star timer still above certain point, branch to cycle player's palette quickly
        lsr                                       ; otherwise, divide by 8 to cycle every eighth frame
        lsr
    endif

    lsr                                           ; if branched here, divide by 2 to cycle every other frame
    jsr CyclePlayerPalette                        ; do sub to cycle the palette (note: shares fire flower code)

    jmp SaveAB                                    ; then skip this sub to finish up the game engine
        ClrPlrPal:
        jsr ResetPalStar                          ; do sub to clear player's palette bits in attributes
    SaveAB:

    mb PreviousA_B_Buttons := A_B_Buttons         ; save current A and B button
    mb Left_Right_Buttons  := #0                   ; nullify left and right buttons temp variable

    UpdScrollVar:

    if VRAM_Buffer_AddrCtrl <> #$06               ; if vram address controller set to 6 (one of two $0341s) then branch to leave

        if lda AreaParserTaskNum == zero          ; otherwise check number of tasks
            ; get horizontal scroll in 0-31 or $00-$20 range, check to see if exceeded $21, branch to leave if not
            if lda ScrollThirtyTwo : cmp #$20 == negative goto ExitEng

            mb ScrollThirtyTwo := ScrollThirtyTwo -c #$20 ; subtract $20 to set appropriately
            mb VRAM_Buffer2_Offset := #0          ; reset vram buffer offset VRAM_Buffer2

        endif

        jsr AreaParserTaskHandler                 ; update the name table with more level graphics

    endif

    ExitEng:
    rts                                           ; and after all that, we're finally done!

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ScrollHandler

    .export ScrollScreen

    mb Player_X_Scroll := Player_X_Scroll + Platform_X_Scroll
    ; check scroll lock flag AND check player's x screen position >= 80 pixels to the right, AND  if timer related to player's side collision expired,
    ; AND Player_X_Scroll >= 1

    if !ScrollLock && Player_Pos_ForScroll >= #$50 && !SideCollisionTimer && ldy Player_X_Scroll : dey == positive

        iny
        if y >= #2                                ; if value $01, branch and do not decrement
            dey                                   ; otherwise decrement by one
        endif

        if Player_Pos_ForScroll >= #$70           ; check player's horizontal screen position; if less than 112 pixels to the right, branch
            ldy Player_X_Scroll                   ; otherwise get original value undecremented
        endif

        ScrollScreen:

        tya
        
        sta ScrollAmount                          ; save value here
        mb ScrollThirtyTwo := a + ScrollThirtyTwo ; add to value already set here
        
        tya

        mb ScreenLeft_X_Pos := a + ScreenLeft_X_Pos ; save as new left side coordinate
        sta HorizontalScroll                      ; save here also
        
        ; add carry to page location for left
        mb ScreenLeft_PageLoc := ScreenLeft_PageLoc + C
        
        ; get LSB of page location
        mb temp_byte := a & #1                    ; save as temp variable for PPU register 1 mirror
        
        ; save all bits except d0
        ; get saved bit here and save in PPU register 1
        ; mirror to be used to set name table later
        mb Mirror_PPU_CTRL := Mirror_PPU_CTRL & #%11111110 | temp_byte
        
        jsr GetScreenPosition                     ; figure out where the right side is
        mb ScrollIntervalTimer := #$08            ; set scroll timer (residual, not used elsewhere
    else
        mb ScrollAmount := #0                     ; initialize value here
    endif

    ldx #0                                        ; set X for player offset
    jsr GetXOffscreenBits                         ; get horizontal offscreen bits for player
    sta temp_byte                                 ; save them here
    ldy #0                                        ; load default offset (left side)

    if a << 1 == carry clear                      ; if d7 of offscreen bits are set,; branch with default offset
        iny                                       ; otherwise use different offset (right side)
        if !temp_byte & #%00100000 goto InitPlatScrl                   ; if not set, branch ahead of this part
    endif
    ; get left or right side coordinate based on offset
    ; subtract amount based on offset
    ; store as player position to prevent movement further

    mb Player_X_Position := ScreenEdge_X_Pos[ y ] - X_SubtracterData[ y ]
    mb Player_PageLoc := ScreenEdge_PageLoc[ y ] - C                 ; subtract borrow, save as player's page location

    if Left_Right_Buttons <> OffscrJoypadBitsData[ y ]                 ; check saved controller bits against bits based on offset
        mb Player_X_Speed := #0                   ; nullify horizontal speed of player
    endif

    InitPlatScrl:

    mb Platform_X_Scroll := #0                    ; nullify platform force imposed on scroll
    rts
.endproc

dataseg

X_SubtracterData:
    .byte $00, $10

OffscrJoypadBitsData:
    .byte $01, $02

    ; ------------------------------------------------------------------------------------------------

.code

.proc GetScreenPosition

    ; get coordinate of screen's left boundary + 255 pixels
    ; store as coordinate of screen's right boundary

    mb ScreenRight_X_Pos  := ScreenLeft_X_Pos + #$ff
    mb ScreenRight_PageLoc := ScreenLeft_PageLoc + C
    
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc GameRoutines
    lda GameEngineSubroutine                      ; run routine based on number (a few of these routines are
    jsr JumpEngine                                ; merely placeholders as conditions for other routines)

    .word Entrance_GameTimerSetup
    .word Vine_AutoClimb
    .word SideExitPipeEntry
    .word VerticalPipeEntry
    .word FlagpoleSlide
    .word PlayerEndLevel
    .word PlayerLoseLife
    .word PlayerEntrance
    .word PlayerCtrlRoutine
    .word PlayerChangeSize
    .word PlayerInjuryBlink
    .word PlayerDeath
    .word PlayerFireFlower
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PlayerEntrance

    if AltEntranceControl <> #2         ; check for mode of alternate entry; if found, skip this block to enter from pipe or with vine

        lda #0
        ; if vertical position above a certain; point, nullify controller bits and continue; with player movement code, do not return
        if ldy Player_Y_Position < #$30 goto AutoControlPlayer   
        ; optimise to if PlayerEntranceCtrl <> #$06  && asr = carry clear goto PlayerRdy
        if PlayerEntranceCtrl <> #$06  &&  a <> #$07  goto PlayerRdy; check player entry bits from header

        if lda Player_SprAttrib == zero           ; check for sprite attributes
            lda #1
            jmp AutoControlPlayer                 ; force player to walk to the right
        endif

        jsr EnterSidePipe                         ; execute sub to move player to the right
        if dec ChangeAreaTimer goto ExitEntr      ; decrement timer for change of area:  branch to exit if not yet expired

        inc DisableIntermediate                   ; set flag to skip world and lives display
        jmp NextArea                              ; jump to increment to next area and set modes

    endif

    if lda JoypadOverride == zero                 ; if controller override bits set here,; branch to enter with vine

        lda #$ff                                  ; otherwise, set value here then execute sub
        jsr MovePlayerYAxis                       ; to move player upwards (note $ff = -1)
    ; if player risen to a certain point (this requires pipes to be at specific height to look/function right) branch to the last part, otherwise leave
        if Player_Y_Position < #$91 goto PlayerRdy
        rts

    endif

    if Vine::Height = #$60                        ; if vine reached maximum height then:

        lda Player_Y_Position                     ; get player's vertical coordinate
        cmp #$99                                  ; check player's vertical coordinate against preset value
        ldy #0                                    ; load default values to be written to
        lda #1                                    ; this value moves player to the right off the vine

        if greaterORequal                         ; if vertical coordinate >= preset value:

            lda #3
            sta Player_State                      ; set player state to climbing
            iny                                   ; increment value in Y
            lda #$08                              ; set block in block buffer to cover hole, then
            sta Block_Buffer_1+$b4                ; use same value to force player to climb

        endif


        sty DisableCollisionDet                   ; set collision detection disable flag
        jsr AutoControlPlayer                     ; use contents of A to move player up or right, execute sub

        if Player_X_Position >= #$48              ; check player's horizontal position, if far enough to the right, then:

            PlayerRdy:

            mb GameEngineSubroutine := #$08       ; set routine to be executed by game engine next frame
            mb PlayerFacingDir      := #1         ; set to face player to the right
            mb a := a >> 1                        ; #0
            mb AltEntranceControl  := a           ; init mode of entry
            mb DisableCollisionDet := a           ; init collision detection disable flag
            mb JoypadOverride      := a           ; nullify controller override bits
        endif

    endif

    ExitEntr:
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------
    ; $07 - used to hold upper limit of high byte when player falls down hole

.proc AutoControlPlayer
      sta SavedJoypadBits                         ; override controller bits with contents of A if executing here
.endproc

.proc PlayerCtrlRoutine

    if GameEngineSubroutine <> #$0b               ; if certain value is set, branch to skip controller bit loading

        if lda AreaType == zero                   ; are we in a water type area?
            ; if nearing the bottom of the screen or  not in the vertical area between status bar or bottom,
            if ( ldy Player_Y_HighPos : dey != zero ) || Player_Y_Position >= #$d0
                mb SavedJoypadBits := #0          ; disable controller bits
            endif
        endif

        mb A_B_Buttons        := SavedJoypadBits & #%11000000
        mb Left_Right_Buttons := SavedJoypadBits & #%00000011
        mb Up_Down_Buttons    := SavedJoypadBits & #%00001100
        ; if pressing down AND  on the ground AND left or right pressed
        ; OP: remove  "  && ( ldy Left_Right_Buttons != zero )   " - might as well just nullify left and right
        if ( a & #BUTTON_DOWN ) && ( lda Player_State == zero ) && ( ldy Left_Right_Buttons != zero )
            lda #0
            sta Left_Right_Buttons                ; if pressing down while on the ground,
            sta Up_Down_Buttons                   ; nullify directional bits
        endif

    endif

    jsr PlayerMovementSubs                        ; run movement subroutines

    ldy #1                                        ; is player small?
    if PlayerSize == BigMario                     ; playersize =  zero means BIG
        ldy #0                                    ; check for if crouching
        if CrouchingFlag                          ;  if not, branch ahead
            ldy #2                                ; if big and crouching, load y with 2
        endif
    endif

    sty Player_BoundBoxCtrl                       ; set contents of Y as player's bounding box size control

    lda #1                                        ; set moving direction to right by default
    if ldy Player_X_Speed                         ; check player's horizontal speed, if not moving at all horizontally, skip this part

        if negative                               ; if moving to the right, use default moving direction
            asl                                   ; otherwise change to move to the left ( reg a = 2)
        endif
        sta Player_MovingDir                      ; set moving direction
    endif


    jsr ScrollHandler                             ; move the screen if necessary
    jsr GetPlayerOffscreenBits                    ; get player's offscreen bits
    jsr RelativePlayerPosition                    ; get coordinates relative to the screen
    ldx #0                                        ; set offset for player object
    jsr BoundingBoxCore                           ; get player's bounding box coordinates
    jsr PlayerBGCollision                         ; do collision detection and process
    ; if player is lower down than 64th pixel AND if not running end-of-level routine
    ; AND not running player entrance routine AND if running routines > $04

    if Player_Y_Position >= #$40 && GameEngineSubroutine <> #$05 && a <> #$07 && a >= #4
        ; nullify player's background priority flag
        mb Player_SprAttrib := Player_SprAttrib & #%11011111
    endif
        ; same as Player_Y_HighPos >= #2
    if lda Player_Y_HighPos : cmp #2 == positive                       ; check player's vertical high byte
        mb x, ScrollLock := #1                    ; set scroll lock
        mb y, $07 := #4                           ; set value here
        ldx #0                                    ; use X as flag, and clear for cloud level
        ; check game timer expiration flag AND check for cloud type override
        if ldy GameTimerExpiredFlag || ( ldy CloudTypeOverride == zero)

            inx                                   ; set flag in X for player death
            ldy GameEngineSubroutine

            if y  <> #$0b                         ; check for some other routine running
                if ldy DeathMusicLoaded == zero   ; check value here
                    iny
                    sty EventMusicQueue           ; otherwise play death music
                    sty DeathMusicLoaded          ; and set value here
                endif
                mb y, $07 := #$06                 ; change value here
            endif

        endif

        if cmp $07 == positive                    ; compare vertical high byte with value set here

            if dex == negative goto CloudExit     ; if flag was clear, branch to set modes and other values

            if ldy EventMusicBuffer == zero       ; check to see if music is still playing
                lda #$06                          ; if not set to run lose life routine
                sta GameEngineSubroutine          ; on next frame
            endif
        endif
    endif
    rts

    CloudExit:

    lda #0
    sta JoypadOverride                            ; clear controller override bits if any are set
    jsr SetEntr                                   ; do sub to set secondary mode
    inc AltEntranceControl                        ; set mode of entry to 3
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc Vine_AutoClimb
    ; check to see whether player reached position above the status bar yet and if so, set modes
    if Player_Y_HighPos || Player_Y_Position >= #$e4
        mb JoypadOverride := #%00001000           ; set controller bits override to up
        mb y, Player_State := #3                  ; set player state to climbing
        jmp AutoControlPlayer
    endif

.endproc

.proc SetEntr
    lda #2                                        ; set starting position to override
    sta AltEntranceControl
    jmp ChgAreaMode                               ; set modes
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc VerticalPipeEntry

    lda #1                                        ; set 1 as movement amount
    jsr MovePlayerYAxis                           ; do sub to move player downwards
    jsr ScrollHandler                             ; do sub to scroll screen with saved force if necessary

    ldy #0                                        ; load default mode of entry
    if lda WarpZoneControl != zero goto ChgAreaPipe   ; check warp zone control variable/flag  if set, branch to use mode 0
    iny
    if AreaType <> #3 goto ChgAreaPipe            ;  if not castle type level, use mode 1
    iny
    jmp ChgAreaPipe                               ; otherwise use mode 2
.endproc

.proc MovePlayerYAxis
    clc
    adc Player_Y_Position                         ; add contents of A to player position
    sta Player_Y_Position
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc SideExitPipeEntry

    jsr EnterSidePipe                             ; execute sub to move player to the right
    ldy #2
.endproc


.proc ChgAreaPipe

    .export ChgAreaMode

    if dec ChangeAreaTimer == zero                ; decrement timer for change of area

        sty AltEntranceControl                    ; when timer expires set mode of alternate entry

        ChgAreaMode:

        inc DisableScreenFlag                     ; set flag to disable screen output

        mb OperMode_Task := #0                    ; set secondary mode of operation
        mb Sprite0HitDetectFlag := a              ; 0                ; disable sprite 0 check

    endif

    rts                                           ; leave
.endproc

.proc EnterSidePipe

    mb Player_X_Speed := #$08                     ; set player's horizontal speed
    ldy #1                                        ; set controller right button by default
    
    if !Player_X_Position & #%00001111            ; mask out higher nybble of player's horizontal position
        sta Player_X_Speed                        ; if lower nybble = 0, set as horizontal speed
        tay                                       ; and nullify controller bit override here
    endif

    tya                                           ; use contents of Y to
    jsr AutoControlPlayer                         ; execute player control routine with ctrl bits nulled
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PlayerChangeSize

    if TimerControl = #$f8                        ; check master timer control for specific moment in time
        jmp InitChangeSize                        ; otherwise run code to get growing/shrinking going.. OP this could be a branch
    endif

    if a = #$c4                                   ; check again for another specific moment
        jsr DonePlayerTask                        ; otherwise do sub to init timer control and set routine
    endif
    rts                                           ; and then leave
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PlayerInjuryBlink

    .export InitChangeSize

    if TimerControl < #$f0                        ; check master timer control
        if a = #$c8  goto DonePlayerTask          ; check again for another specific point
        jmp PlayerCtrlRoutine                     ; otherwise run player control routine
    endif
    ; if timercontrol =zero make mario shrink
    if zero set                                   ; this is not unconditional as documented

        InitChangeSize:

        if not ldy PlayerChangeSizeFlag           ; if growing/shrinking flag not set
            sty PlayerAnimCtrl                    ; otherwise initialize player's animation frame control
            inc PlayerChangeSizeFlag              ; set growing/shrinking flag

            mb PlayerSize := PlayerSize ^ #1      ; invert player's size
        endif

    endif

    rts                                           ; leave
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PlayerDeath

    if TimerControl >= #$f0 goto ExitDeath        ; check master timer control
    jmp PlayerCtrlRoutine                         ; otherwise run player control routine

.endproc

.proc DonePlayerTask
    mb TimerControl := #0                         ; initialize master timer control to continue timers   
    mb GameEngineSubroutine := #$08               ; set player control routine to run next frame
    rts                                           ; leave
.endproc

.proc PlayerFireFlower

    .export CyclePlayerPalette, ResetPalStar

    if TimerControl <> #$c0                       ; check master timer control

        mb a := FrameCounter / 4                  ; get frame counter / 4

        CyclePlayerPalette:

        mb temp_byte := a & #%00000011            ; mask out all but d1-d0 (previously d3-d2)
        ; get player attributes, save any other bits but
        ; palette bits, add new pallette bits
        mb Player_SprAttrib := Player_SprAttrib & #%11111100 | temp_byte
        rts

    endif

    jsr DonePlayerTask                            ; do sub to init timer control and run player control routine

    ResetPalStar:

    mb Player_SprAttrib := Player_SprAttrib & #%11111100               ; force palette 0

    rts
.endproc
    ; OP remove this rts, point label to previous
    ExitDeath:
    rts                                           ; leave from death routine

    ; ------------------------------------------------------------------------------------------------

.proc FlagpoleSlide

    if ( Enemy_ID [ 5 ] = #OBJECTID_FlagpoleFlagObject )   ; check special use enemy slot for flagpole flag object
        lda FlagpoleSoundQueue                    ; load flagpole sound
        sta Square1SoundQueue                     ; into square 1's sfx queue
        lda #0
        sta FlagpoleSoundQueue                    ; init flagpole sound queue

        if ldy Player_Y_Position :  y < #$9e      ; far enough, and if so, branch with no controller bits set, otherwise force player to climb down ( lda #4)
            lda #4                                ;  (to slide
        endif
        jmp AutoControlPlayer                     ; jump to player control routine
    endif

    inc GameEngineSubroutine                      ; increment to next routine (this may
    rts                                           ; be residual code)

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

Hidden1UpCoinAmts:    .byte $15, $23, $16, $1b, $17, $18, $23, $63

.code

.proc PlayerEndLevel

    .export NextArea

    lda #1                                        ; force player to walk to the right
    jsr AutoControlPlayer
    ; check player's vertical position; if player is not yet off the flagpole, skip this part
    if Player_Y_Position >= #$ae && ScrollLock
        mb EventMusicQueue  := #MUSIC_EndOfLevel                       ; load win level music in event music queue
        mb ScrollLock       := #0                 ; turn off scroll lock to skip this part later
    endif

    if Player_CollisionBits >> 1 != carry         ; get player collision bits, ; check for d0 set
        if !StarFlagTaskControl                   ; if star flag task control not
            inc StarFlagTaskControl               ; set task control now (this gets ball rolling!)
        endif
        lda #%00100000                            ; set player's background priority bit to
        sta Player_SprAttrib                      ; give illusion of being inside the castle
    endif

    if StarFlagTaskControl = #$05                 ; if star flag task control not yet set; beyond last valid task number, branch to leave

        inc LevelNumber                           ; increment level number used for game logic
        if LevelNumber = #3                       ; check to see if we have yet reached level -4

            ldy WorldNumber                       ; get world number as offset
            ; check third area coin tally against minimum value, if player has not collected enough, leave flag clear
            if CoinTallyFor1Ups >= Hidden1UpCoinAmts[ y ] 
                inc Hidden1UpFlag                 ; otherwise set hidden 1-up box control flag
            endif

        endif

        NextArea:

        inc AreaNumber                            ; increment area number used for address loader
        jsr LoadAreaPointer                       ; get new level pointer
        inc FetchNewGameTimerFlag                 ; set flag to load new game timer
        jsr ChgAreaMode                           ; do sub to set secondary mode, disable screen and sprite 0
        sta HalfwayPage                           ; reset halfway page to 0 (beginning)
        
        mb EventMusicQueue := #MUSIC_Silence      ; silence music and leave
    endif

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PlayerMovementSubs

    lda #0                                        ; set A to init crouch flag by default
    if ldy PlayerSize == zero                     ; is player small? ( 0 is big ) ; OP - this could combined to one IF
        if lda Player_State goto ProcMove         ; check state of player ; if not on the ground, branch
        lda Up_Down_Buttons                       ; load controller bits for up and down
        and #%00000100                            ; single out bit for down button
    endif

    sta CrouchingFlag                             ; store value in crouch flag

    ProcMove:

    jsr PlayerPhysicsSub                          ; run sub related to jumping and swimming

    if lda PlayerChangeSizeFlag == zero           ; if not growing/shrinking flag set,

        if Player_State <> #3                     ; if climbing, reset timer
            ldy #$18
            sty ClimbSideTimer
        endif

        jsr JumpEngine

        .word OnGroundStateSub
        .word JumpSwimSub
        .word FallingSub
        .word ClimbingSub

    endif
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc OnGroundStateSub
    jsr GetPlayerAnimSpeed                        ; do a sub to set animation frame timing

    if lda Left_Right_Buttons != zero             ; if left/right controller bits not set, skip instruction
        sta PlayerFacingDir                       ; otherwise set new facing direction
    endif

    jsr ImposeFriction                            ; do a sub to impose friction on player's walk/run
    jsr MovePlayerHorizontally                    ; do another sub to move player horizontally
    sta Player_X_Scroll                           ; set returned value as player's movement speed for scroll
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc FallingSub
    mb VerticalForce := VerticalForceDown         ; dump vertical movement force for falling into main one
    jmp LRAir                                     ; movement force, then skip ahead to process left/right movement
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc JumpSwimSub

    .export LRAir

    if ldy Player_Y_Speed == positive goto Falling                     ; if player's vertical speed zero ; or moving downwards, branch to falling

    if ! A_B_Buttons & #BUTTON_A & PreviousA_B_Buttons                 ; if not pressed
    ; compare to see if player is in mid-jump
    ; or just starting to jump, if just starting, skip ahead
        if JumpOrigin_Y_Position - Player_Y_Position >= DiffToHaltJump

            Falling:
            mb VerticalForce := VerticalForceDown                      ; otherwise dump falling into main fractional
        endif
    endif

    if SwimmingFlag                               ; if swimming flag set:

        jsr GetPlayerAnimSpeed                    ; do a sub to get animation frame timing
        if Player_Y_Position < #$14               ; check vertical position against preset value
            mb VerticalForce := #$18              ; otherwise set fractional
        endif

        if lda Left_Right_Buttons != zero         ; check left/right controller bits (check for swimming)
            sta PlayerFacingDir                   ; if pressed set facing direction accordingly
        endif

    endif

    LRAir:

    if Left_Right_Buttons                         ; check left/right controller bits (check for jumping/falling)
        jsr ImposeFriction
    endif

    jsr MovePlayerHorizontally                    ; do a sub to move player horizontally
    sta Player_X_Scroll                           ; set player's speed here, to be used for scroll later
    if GameEngineSubroutine = #$0b
        mb VerticalForce := #$28                  ; otherwise set fractional
    endif

    jmp MovePlayerVertically                      ; jump to move player vertically, then leave
.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

ClimbAdderLow:
    .byte $0e, $04, $fc, $f2
ClimbAdderHigh:
    .byte $00, $00, $ff, $ff

.code

.proc ClimbingSub

    ; locals
        adder_temp = temp_byte
    ; end locals

    mb Player_YMoveForceFractional := Player_YMoveForceFractional + Player_Y_MoveForce

    ldy #0                                        ; set default adder here
    if lda Player_Y_Speed == negative             ; get player's vertical speed, if moving upwards
        dey                                       ; set adder to $ff ( -1)
    endif
    sty adder_temp                                ; store adder here

    mb Player_Y_Position := a +c Player_Y_Position
    mb Player_Y_HighPos  := Player_Y_HighPos +c adder_temp             ; add carry to player's page location


    if Left_Right_Buttons & Player_CollisionBits

        if ldy ClimbSideTimer == zero             ; otherwise check if timer expired

            mb y, ClimbSideTimer := #$18          ; otherwise set timer now
            ldx #0                                ; set default offset here
            ldy PlayerFacingDir                   ; get facing direction

            if lsr == carry clear                 ; move right button controller bit to carry
                mb x := x + 2                     ; if controller right not pressed increment offset by 2 bytes
            endif

            if dey != zero                        ; check to see if facing right
                inx                               ; if not , increment by 1 byte
            endif

            mb Player_X_Position := Player_X_Position + ClimbAdderLow[ x ]
            mb Player_PageLoc    := Player_PageLoc +c ClimbAdderHigh[ x ]
            ; invert bits while player is on vine to face player in opposite direction
            mb PlayerFacingDir   := Left_Right_Buttons ^ #%00000011    

        endif

        rts

    endif

    sta ClimbSideTimer                            ; initialize timer here
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

JumpMForceData:
    .byte $20, $20, $1e, $28, $28, $0d, $04

FallMForceData:
    .byte $70, $70, $60, $90, $90, $0a, $09

PlayerYSpdData:
    .byte $fc, $fc, $fc, $fb, $fb, $fe, $ff

InitMForceData:
    .byte $00, $00, $00, $00, $00, $80, $00

MaxLeftXSpdData:
    .byte $d8, $e8, $f0

MaxRightXSpdData:
    .byte $28, $18, $10
    .byte $0c                                     ; used for pipe intros

FrictionData:
    .byte $e4, $98, $d0

Climb_Y_SpeedData:
    .byte $00, $ff, $01

Climb_Y_MForceData:
    .byte $00, $20, $ff

.code

.proc PlayerPhysicsSub

    ; locals
        FrictionOffset = temp_byte                    ; - used to store offset to friction data
    ; end locals
    
    if Player_State = #3                          ; check player state, if climbing:
        ldy #0
        ; get controller bits for up/down; check against player's collision detection bits, if pressing up/down:
        if a := Up_Down_Buttons & Player_CollisionBits   
            iny
            if ! a & #BUTTON_UP                   ; check for pressing up only
                iny
            endif
        endif

        mb x, Player_Y_MoveForce := Climb_Y_MForceData[ Y ]            ; store as vertical movement force

        lda #$08                                  ; load default animation timing
        mb x, Player_Y_Speed := Climb_Y_SpeedData[ y ]                 ; store as vertical speed

        if positive                               ; if climbing down, use default animation timing value
            lsr                                   ; otherwise divide timer setting by 2
        endif

        sta PlayerAnimTimerSet                    ; store animation timer setting and leave
        rts

    endif

    ; ------------------------------------------------------------------------------------------------
    
    ; OP -> two 'jmp X_Physics' a few branches apart

    ; else, if not climbing: 
    ; if JumpspringAnimCtrl set OR 'A' not pressed OR Previously 'A' was pressed
    if JumpspringAnimCtrl || ! A_B_Buttons & #BUTTON_A || a & PreviousA_B_Buttons
        NoJump:
        jmp X_Physics
    endif
    
    ; if here, There is no jumpspring and a valid A button press:

    if lda Player_State       ; check player state, not zero = not on ground
        
        if !SwimmingFlag goto NoJump   ; if swimming flag not set (comment this line for air jumping)
        ; if here, we are swimming:
        ; if jump/swim timer zero AND player's vertical speed UP
        if !JumpSwimTimer && Player_Y_Speed == negative
            jmp X_Physics                         ; if timer at zero and player still rising, do not swim
        endif
    endif

    ; ------------------------------------------------------------------------------------------------
    ; ; REWRITE ABOVE SECTION:
    ; ; logic to skip jumping:
    ; ; if JumpspringAnimCtrl OR A not pressed OR Previously A was pressed 
    ;   OR ( Player_State not on ground AND ( not swimming OR ( Jumpswintimer expired AND still moving UP))
    ;
    ;    if JumpspringAnimCtrl || ( !A_B_Buttons & #BUTTON_A ) || ( a & PreviousA_B_Buttons ) \
    ;        || Player_State && ( !SwimmingFlag || !JumpSwimTimer && lda Player_Y_Speed == negative )
    ;        jmp X_Physics                            ;  jump X axis code
    ;    endif
    ;
    ; ------------------------------------------------------------------------------------------------
    ; Do Y Physics (Jumping or Swimming):

    mb JumpSwimTimer         := #$20              ; set jump/swim timer
    mb y                     := #0
    mb Player_YMoveForceFractional      := y                 ; #0
    mb Player_Y_MoveForce    := y                 ; #0
    mb JumpOrigin_Y_HighPos  := Player_Y_HighPos
    mb JumpOrigin_Y_Position := Player_Y_Position
    mb Player_State          := #1                ; set player state to jumping/swimming

    lda Player_XSpeedAbsolute
    ; check value related to walking/running speed
    if a < #$09 goto CheckSwimming
    iny                                           ; 1
    if a < #$10 goto CheckSwimming
    iny                                           ; 2
    if a < #$19 goto CheckSwimming
    iny                                           ; 3
    if a < #$1c goto CheckSwimming
    iny                                           ; 4

    CheckSwimming:

    mb DiffToHaltJump := #1

    if SwimmingFlag
        ldy #5
        if Whirlpool_Flag
            iny                                   ; 6
        endif
    endif
    
    ; store appropriate jump/swim
    mb VerticalForce      := JumpMForceData[ y ]                       ; data here
    mb VerticalForceDown  := FallMForceData[ y ]
    mb Player_Y_MoveForce := InitMForceData[ y ]
    mb Player_Y_Speed     := PlayerYSpdData[ y ]

    if SwimmingFlag                               ; if swimming
        mb Square1SoundQueue := #SFX_EnemyStomp   ; load swim/goomba stomp sound
        if Player_Y_Position >= #$14 goto X_Physics                    ; if below a certain point, branch
        mb Player_Y_Speed := #0                   ; otherwise reset player's vertical speed to stay below water
    else                                          ; not swimming:
        lda #SFX_BigJump                          ; load big mario's jump sound by default
        if ldy PlayerSize == SmallMario           ; if mario is small
            lda #SFX_SmallJump                    ; load small mario's jump sound
        endif
        sta Square1SoundQueue                     ; store appropriate jump sound in square 1 sfx queue
    endif

    X_Physics:

    mb y, temp_byte := #0                         ; init value here

    if Player_State != zero                       ; if mario is not on the ground
        if Player_XSpeedAbsolute >= #$19 goto GetXPhy                  ; check something that seems to be related to mario's speed
    else C clear                                  ; mario on ground:
        iny
        if lda AreaType != zero                   ; check area type, if not water:
            dey                                   ; decrement Y by default for non-water type area
            if Left_Right_Buttons = Player_MovingDir                   ; get left/right controller bits, check against moving direction
                if A_B_Buttons & #BUTTON_B goto SetRunningTimer        ; check for b button pressed if pressed, skip ahead to set timer
                if RunningTimer goto GetXPhy      ; check for running timer set
            endif
        endif
    endif

    iny                                           ; if running timer not set or level type is water,
    inc temp_byte                                 ; increment Y again and temp variable in memory

    if RunningSpeed || Player_XSpeedAbsolute >= #$21                   ; if running speed set or speed => $21 increment temp_byte

        inc temp_byte
    ; endif could go here for better code, but waste of cycles..

        jmp GetXPhy                               ; and jump ahead

            SetRunningTimer:
            mb RunningTimer := #$0a               ; if b button pressed and pressing direction of movment, set running timer

    endif
    GetXPhy:


    mb MaximumLeftSpeed := MaxLeftXSpdData[ y ]   ; get maximum speed to the left

    if GameEngineSubroutine = #$07                ; check for specific routine running
        ldy #3
    endif

    mb MaximumRightSpeed := MaxRightXSpdData[ y ]                      ; get maximum speed to the right
    ldy temp_byte                                 ; get other value in memory
    mb FrictionAdderLow := FrictionData[ y ]      ; get value using value in memory as offset
    mb FrictionAdderHigh := #0                    ; init something here

    if PlayerFacingDir <> Player_MovingDir        ; check facing direction against moving direction
        asl FrictionAdderLow                      ; otherwise shift d7 of friction adder low into carry
        rol FrictionAdderHigh                     ; then rotate carry onto d0 of friction adder high
    endif

    rts                                           ; and then leave

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

PlayerAnimTmrData:
    .byte $02, $04, $07

.code

.proc GetPlayerAnimSpeed

    ldy #0                                        ; initialize offset in Y
    if Player_XSpeedAbsolute >= #$1c goto SetRunningSpeed

    iny
    if a < #$0e
        iny
    endif

    lda SavedJoypadBits
    if a & #(<~BUTTON_A)                          ; if A not pressed, but something else pressed:

        if a & # (BUTTON_LEFT+BUTTON_RIGHT ) = Player_MovingDir
            lda #0
            SetRunningSpeed:
            sta RunningSpeed                      ; store zero or running speed here
        elseif Player_XSpeedAbsolute < #$0b       ; check player's walking/running speed again
            mb Player_MovingDir   := PlayerFacingDir  ; use facing direction to set moving direction
            mb a,Player_X_Speed   := #0           ; nullify player's horizontal speed
            mb Player_X_MoveForce := a ; #0       ; and fractional for player
        endif

    endif

    mb PlayerAnimTimerSet := PlayerAnimTmrData[ y ]                    ; get animation timer setting using Y as offset
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ImposeFriction

    ; enter here with controller bits in reg a

    if a & Player_CollisionBits = #0              ; if no bits set:  OP : redundant compare here
        lda Player_X_Speed
        if zero     goto SetAbsSpd                ; if player has no horizontal speed, branch ahead to last part
        if positive goto RghtFrict                ; if player moving to the right, branch to slow
        if negative goto LeftFrict                ; otherwise logic dictates player moving left, branch to slow
    endif                                         ; else:

    if a >> 1 == carry set                        ; if right pressed:
        LeftFrict:                                ; we are here if right pressed or coasting to the left

        mb Player_X_MoveForce := Player_X_MoveForce + FrictionAdderLow
        mb Player_X_Speed := Player_X_Speed +c FrictionAdderHigh       ; add value plus carry to horizontal speed, set as new horizontal speed
        ; compare against maximum value for right movement, if horizontal speed greater negatively, branch
        if cmp MaximumRightSpeed == negative goto XSpdSign

        mb Player_X_Speed := MaximumRightSpeed    ; otherwise set preset value as horizontal speed thus slowing the player's left movement down
    else                                          ; if left pressed:

        RghtFrict:                                ; here if left pressed or coasting to the right

        mb Player_X_MoveForce := Player_X_MoveForce - FrictionAdderLow

        mb Player_X_Speed := Player_X_Speed -c FrictionAdderHigh
        ; compare against maximum value for left movement if horizontal speed greater positively, branch
        if cmp MaximumLeftSpeed == positive goto XSpdSign
        mb Player_X_Speed := MaximumLeftSpeed     ; otherwise set preset value as horizontal speed, thus slowing the player's right movement down

        XSpdSign:
        ; if player not moving or moving to the right, branch and leave horizontal speed value unmodified
        if cmp #0 == negative
            mb a := a ^ #$ff + #1                 ; two's compliment
        endif

    endif

    SetAbsSpd:

    sta Player_XSpeedAbsolute                     ; store walking/running speed here and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ProcFireball_Bubble

    if PlayerStatus >= #2                         ; check player's status; if fiery:
        ; if B button pressed AND not pressed last frame
        if A_B_Buttons & #BUTTON_B && not a & PreviousA_B_Buttons
            ; if here then button pressed for fireball, check if it is okay to fire a new fireball:
            mb x := FireballCounter & #%00000001                       ; get LSB and use as offset for buffer
            ; if room for a new fireball (max two) AND Player not too high/ too low AND not crouching AND Not climbing vine
            if !Fireball_State[ x ] && y := Player_Y_HighPos - 1 == zero && !CrouchingFlag && Player_State <> #3

                mb Square1SoundQueue := #SFX_Fireball                  ; play fireball sound effect
                mb Fireball_State[ x ] := #2        ; load state, $02 = new fireball
                mb y, FireballThrowingTimer := PlayerAnimTimerSet      ; copy animation frame timer setting into fireball throwing timer

                mb PlayerAnimTimer := y - 1       ; decrement and store in player's animation timer
                inc FireballCounter               ; increment fireball counter

            endif
        endif

        ldx #0
        jsr FireballObjCore                       ; process first fireball object
        ldx #1
        jsr FireballObjCore                       ; process second fireball object, then do air bubbles

    endif
    ; bubbles!
    if lda AreaType == zero                       ; if not water type level, skip the rest of this

        ldx #2                                    ; otherwise load counter and use as offset
        repeat                                    ; 2 to 0
            stx ObjectOffset                      ; store offset
            jsr BubbleCheck                       ; check timers and coordinates, create air bubble
            jsr RelativeBubblePosition            ; get relative coordinates
            jsr GetBubbleOffscreenBits            ; get offscreen information
            jsr DrawBubble                        ; draw the air bubble
        until dex == negative

    endif

    rts                                           ; then leave
.endproc

dataseg

FireballXSpdData:

    .byte $40, <-$40                              ; posiitve fireball spedd, negative fireball speed.

.code

.proc FireballObjCore                             ; paramater of 0 or 1 in reg x for which fireball

    ; locals
        DownForce           = temp_byte     ; - used to store downward movement force
        MaxVerticalSpeed    = temp_byte + 2 ; - used to store maximum vertical speed
    ; end locals
    

    stx ObjectOffset                              ; store offset as current object
    ; OP : could use negative flag instead of asl
    if Fireball_State[ x ] << 1 == carry set goto FireballExplosion      ; check for d7 = 1, if so, get relative coordinates and draw explosion

    if ldy Fireball_State[ x ]                      ; if fireball active

        if dey != zero                            ; if fireball state greater than 1: (#$02 = birthing new fireball )

            mb Fireball_X_Position  [ x ] := Player_X_Position +c #4   ; carry is clear
            mb Fireball_PageLoc     [ x ] := Player_PageLoc + C
            mb Fireball_Y_Position  [ x ] := Player_Y_Position         ; get player's vertical position and store,
            mb Fireball_Y_HighPos   [ x ] := #1   ; set high byte of vertical position,
            mb y := PlayerFacingDir - 1           ; get player's facing dir, dec to use as offset
            mb Fireball_X_Speed     [ x ] := FireballXSpdData[ y ]     ; set horizontal speed of fireball accordingly
            mb Fireball_Y_Speed     [ x ] := #4   ; set vertical speed of fireball
            mb Fireball_BoundBoxCtrl[ x ] := #7   ; set bounding box size control for fireball
            dec Fireball_State,x                  ; decrement state to 1 to skip this part from now on ( state of #1 means fireball is bouncing)

        endif

        mb a := x
        mb x := a + #7                            ; use as fireball offset for next routines
    ; set paramaters for subs
        mb DownForce := #$50                      ; set downward movement force here
        mb MaxVerticalSpeed := #3                   ; set maximum speed here
        lda #0
        jsr ImposeGravity                         ; do sub here to impose gravity on fireball and move vertically
        jsr MoveObjectHorizontally                ; do another sub to move it horizontally

        ldx ObjectOffset                          ; return fireball offset to X

        jsr RelativeFireballPosition              ; get relative coordinates
        jsr GetFireballOffscreenBits              ; get offscreen information
        jsr GetFireballBoundBox                   ; get bounding box coordinates
        jsr FireballBGCollision                   ; do fireball to background collision detection

        if !FBall_OffscreenBits & #%11001100      ; get fireball offscreen bits, if any bits still set, skip to kill fireball
            jsr FireballEnemyCollision            ; do fireball to enemy collision detection and deal with collisions
            jmp DrawFireball                      ; draw fireball appropriately and leave
        endif
        ;else:

        mb Fireball_State[ x ] := #0              ; erase fireball state

    endif

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc FireballExplosion

    jsr RelativeFireballPosition
    jmp DrawExplosion_Fireball

.endproc

.proc BubbleCheck

    ; locals
        RandomNum = temp_byte + 7
    ; end locals

    .export SetupBubble

    mb RandomNum := PseudoRandomBitReg[ 1 + x]  & #1   ; store pseudorandom bit here

    if Bubble_Y_Position[ x ] = #$f8              ; get vertical coordinate for air bubble, if null, do new bubble

        if AirBubbleTimer goto Exit               ; if air bubble timer not expired, leave ..OP: do this check first

        SetupBubble:

        ldy #0                                    ; load default value here

        if PlayerFacingDir >> 1 == carry set      ; get player's facing direction, move d0 to carry
            ldy #$08                              ; load alternate value here, so bubble is on the correct side of mario
        endif

        mb a := y                                 ; use value loaded as adder

        mb Bubble_X_Position[ x ]   := a +c Player_X_Position    ; add to player's horizontal position, save as horizontal position for airbubble
        mb Bubble_PageLoc[ x ]      := Player_PageLoc + C      ; add carry to player's page location, save as page location for airbubble
        ; add eight pixels to player's vertical position
        mb Bubble_Y_Position[ x ]   := Player_Y_Position + #$08        ; save as vertical position for air bubble
        mb Bubble_Y_HighPos[ x ]    := #1         ; set vertical high byte for air bubble
        mb y := RandomNum                         ; get pseudorandom bit, use as offset
        mb AirBubbleTimer := BubbleTimerData[ y ]                      ; set air bubble timer

    endif

    mb y := RandomNum                             ; get pseudorandom bit again, use as offset (0 or 1)
    ; subtract pseudorandom amount from fractional variable
    mb Bubble_YMoveForceFractional[ x ] := Bubble_YMoveForceFractional[ x ] - Bubble_MForceData[ y ]
    mb a := Bubble_Y_Position[ x ] - C          ; sub carry

    if a < #$20                                   ; if bubble above water:
        lda #$f8                                  ; set offscreen coordinate
    endif

    mb Bubble_Y_Position[ x ] := a                ; store as new vertical coordinate for air bubble

    Exit:
    rts

.endproc

dataseg

Bubble_MForceData:
    .byte $ff, $50

BubbleTimerData:
    .byte $40, $20

.code

    ; ------------------------------------------------------------------------------------------------

.proc RunGameTimer
    ; get primary mode of operation, if not title screen mode AND
    ; if routine number eight or greater AND not death routine AND player not below screen AND Timer expired
    if OperMode && GameEngineSubroutine >= #$08 && a <> #$0b && Player_Y_HighPos < #2 && !GameTimerCtrlTimer

        ; if game timer not yet at digits at 000
        if GameTimerDisplay | GameTimerDisplay[1] | GameTimerDisplay[2]                                 
            ; if first digit a 1 AND second and third digits are zero = 100 timer
            if ( y := GameTimerDisplay - 1 == zero )  && !GameTimerDisplay[1] | GameTimerDisplay[2]
                mb EventMusicQueue := #MUSIC_TimeRunningOut
            endif

            mb GameTimerCtrlTimer := #$18         ; reset game timer control

            mb y := #$23                          ; set offset for last digit
            mb DigitModifier[ 5 ] := #$ff         ; set value to decrement game timer digit
            jsr DigitsMathRoutine                 ; do sub to decrement game timer slowly
            mb a := #$a4                          ; set status nybbles to update game timer display
            jmp PrintStatusBarNumbers             ; do sub to update the display, don't come back

        endif
        ; else: timer = zero:

        mb PlayerStatus := a                      ; a = 0, init player status
        jsr ForceInjury                           ; do sub to kill the player (note player is small here)
        inc GameTimerExpiredFlag                  ; set game timer expiration flag
    endif

    Exit:
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc WarpZoneObject
    ; check for scroll lock flag OR check to see if player's vertical coordinate has  same bits set as in vertical high byte (why?)
    if !ScrollLock || Player_Y_Position & Player_Y_HighPos goto RunGameTimer::Exit
    ; if here, a is zero:
    sta ScrollLock                                ; otherwise nullify scroll lock flag
    inc WarpZoneControl                           ; increment warp zone flag to make warp pipes for warp zone
    jmp EraseEnemyObject                          ; kill this object

.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc ProcessWhirlpools

    ; locals
        RightSideWPPage = temp_byte + 1 ; - used to store page location of right extent of whirlpool
        RightSideWP     = temp_byte + 2 ; - used to store right extent of whirlpool
    ; end locals
    
    if AreaType == zero                           ; check for water type level, if not, leave

        sta Whirlpool_Flag                        ; otherwise initialize whirlpool flag with 0
        if !TimerControl                          ; if master timer control not set

            ldy #4                                ; otherwise start with last whirlpool data
            repeat                                ; count reg y from 4 to 0
                ; get left extent of whirlpool and add length of whirlpool, store result as right extent here
                mb RightSideWP := Whirlpool_LeftExtent[ y ] + Whirlpool_Length[ y ]
                if lda Whirlpool_PageLoc[ y ] != zero                  ; get page location,
                    mb RightSideWPPage := a + C                       ; add carry
                    mb a := Player_X_Position - Whirlpool_LeftExtent[ y ]    ; get player's horizontal position, subtract left extent
                    mb a := Player_PageLoc -c Whirlpool_PageLoc[ y ]   ; get player's page location, subtract borrow

                    if positive                   ; if player too far left, branch to get next data
                        mb a := RightSideWP - Player_X_Position
                        mb a := RightSideWPPage -c Player_PageLoc       ; get right extent's page location, subtract borrow
                        if positive goto WhirlpoolActivate             ; if player within right extent, branch to whirlpool code
                    endif
                endif
            until dey == negative                 ;  do this until all whirlpools are checked

        endif
    endif

    rts
    
.endproc
    
.proc WhirlpoolActivate

    ; locals
        WPPage      = temp_byte     ; - used to store whirlpool length / 2, page location of center, movement force exerted on player
        WPCenter    = temp_byte + 1 ; - store center of whirlpool
        MaxVSpeed   = temp_byte + 2 ; - store maximum vertical speed
    ; end locals


    mb WPPage     := Whirlpool_Length [ y ] / 2
    mb WPCenter   := Whirlpool_LeftExtent [ y ] + WPPage           ; save center of whirlpool here
    mb WPPage     := Whirlpool_PageLoc [ y ] + C                   ; add C save as page location of whirlpool center

    if FrameCounter >> 1 == carry                 ; get frame counter, every other frame do this:

        mb a := WPCenter - Player_X_Position      ; center, subtract player's horizontal coordinate
        mb a := WPPage -c Player_PageLoc          ; get page location of center subtract borrow

        if negative                               ; if player to the left of center, branch
            mb Player_X_Position := Player_X_Position - #1             ; otherwise slowly pull player left, towards the center
            mb a := Player_PageLoc - C          ; sub borrow
        else
            if Player_CollisionBits >> 1 == carry clear goto WhPull    ; get player's collision bits
            mb Player_X_Position := Player_X_Position + #1             ; otherwise slowly pull player right, towards the center
            mb a := Player_PageLoc + C          ; add carry
        endif

        sta Player_PageLoc                        ; set player's new page location
    endif

    WhPull:
    ; set up params for gravity sub
    mb WPPage := #$10                             ; set vertical movement force
    mb Whirlpool_Flag := #1                       ; set whirlpool flag to be used later
    mb MaxVSpeed := a ; #1                        ; also set maximum vertical speed
    mb x := a >> 1    ; #0                        ; set X for player offset
    jmp ImposeGravity                             ; jump to put whirlpool effect on player vertically, do not return

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

FlagpoleScoreMods:
    .byte $05, $02, $08, $04, $01

FlagpoleScoreDigits:
    .byte $03, $03, $04, $04, $04

.code


.proc FlagpoleRoutine

    mb x, ObjectOffset := #$05                    ; set enemy object offset to special use slot

    if Enemy_ID[ x ] = #OBJECTID_FlagpoleFlagObject
        ; if flagpole slide routine isn't running OR if player state is not climbing
        if GameEngineSubroutine <> #4 || Player_State <> #3 goto Skip    ; OP optimize this condition as part of the one below
        ; if player or flag not down all the way:
        if Enemy_Y_Position[ x ] < #$aa && Player_Y_Position < #$a2
            ; add movement amount to fractional
            mb Enemy_YMoveForceFractional [ x ] := Enemy_YMoveForceFractional[ x ] +c #$ff   ; save fractional
            ; get flag's vertical coordinate add 1 plus carry to move flag, and
            mb Enemy_Y_Position [ x ] := Enemy_Y_Position [ x ] +c #1  ; store vertical coordinate
            ; subtract movement amount from fractional
            mb FlagpoleFNum_YMFDummy := FlagpoleFNum_YMFDummy - #$ff   ; save fractional
            ; subtract one plus borrow to move floatey number,
            mb FlagpoleFNum_Y_Pos := FlagpoleFNum_Y_Pos -c #1          ; and store vertical coordinate here

            Skip:                                 ; from here execution will just jump to the 3 x jsr
        else
            ldy FlagpoleScore                     ; get score offset from earlier (when player touched flagpole)
            lda FlagpoleScoreMods,y               ; get amount to award player points
            ldx FlagpoleScoreDigits,y             ; get digit with which to award points
            sta DigitModifier,x                   ; store in digit modifier
            jsr AddToScore                        ; do sub to award player points depending on height of collision

            mb GameEngineSubroutine := #$05       ; set to run end-of-level subroutine on next frame
        endif

        jsr GetEnemyOffscreenBits                 ; get offscreen information
        jsr RelativeEnemyPosition                 ; get relative coordinates
        jsr FlagpoleGfxHandler                    ; draw flagpole flag and floatey number

    endif

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

Jumpspring_Y_PosData:
    .byte $08, $10, $08, $00

.code

.proc JumpspringHandler

    jsr GetEnemyOffscreenBits                     ; get offscreen information
    ; only do this if no TimerControl active and Jumpspring is active
    if not TimerControl && JumpspringAnimCtrl

        mb y := a                                 ; subtract one from frame control,
        mb a := y - 1                             ; the only way a poor nmos 6502 can
        ; if JumpspringAnimCtrl frame was 3 exactly move down
        if ! a & #%00000010
            inc Player_Y_Position
            inc Player_Y_Position                 ; move player's vertical position down two pixels
        else
            dec Player_Y_Position                 ; move player's vertical position up two pixels
            dec Player_Y_Position
        endif
        ; get permanent vertical position add value using frame control as offset
        mb Enemy_Y_Position[ x ] := Jumpspring_FixedYPos[ x ] + Jumpspring_Y_PosData[ y ]
        ; check frame control offset (second frame is 00)
        ; if offset not yet at third frame ($01), skip to next part
        ; AND Button A pressed and not pressed last frame

        if (y >= #1) && (A_B_Buttons & #BUTTON_A) && (! a & PreviousA_B_Buttons)
            mb JumpspringForce := #$f4
        endif

        if y = #3                                 ; frame 4
            mb Player_Y_Speed := JumpspringForce  ; store jumpspring force as player's new vertical speed
            mb JumpspringAnimCtrl := #0           ; initialize jumpspring frame control
        endif
    endif

    jsr RelativeEnemyPosition                     ; get jumpspring's relative coordinates
    jsr EnemyGfxHandler                           ; draw jumpspring
    jsr OffscreenBoundsCheck                      ; check to see if we need to kill it

    if JumpspringAnimCtrl && not JumpspringTimer                       ; if frame control at zero, don't bother
        mb JumpspringTimer := #4                  ; otherwise initialize jumpspring timer
        inc JumpspringAnimCtrl                    ; increment frame control to animate jumpspring
    endif

    rts                                           ; leave
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc Setup_Vine

    mb Enemy_ID         [ x ] := #OBJECTID_VineObject                     ; load identifier for vine object
    mb Enemy_Flag       [ x ] := #1                                    ; set flag for enemy object buffer
    mb Enemy_PageLoc    [ x ] := Block_PageLoc[ y ]                    ; copy page location from previous object
    mb Enemy_X_Position [ x ] := Block_X_Position[ y ]                 ; copy horizontal coordinate from previous object
    mb Enemy_Y_Position [ x ] := Block_Y_Position[ y ]                 ; copy vertical coordinate from previous object
    ; load vine flag/offset to next available vine slot, if set at all, don't bother to store vertical
    if ldy Vine::FlagOffset == zero
        sta Vine::Start_Y_Position                ; store vertical coordinate here
    endif

    mb a := x                                     ; store object offset to next available vine slot
    mb Vine::ObjOffset[ y ] := a                  ; using vine flag as offset
    inc Vine::FlagOffset                          ; increment vine flag offset
    mb Square2SoundQueue := #SFX_GrowVine         ; load vine grow sound
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

VineHeightData:
    .byte $30, $60

.code

.proc VineObjectHandler

    ; locals
        BlockBufferPointer = temp_byte + 6
        VerticalHighNybble = temp_byte + 2 ; - used as vertical high nybble of block buffer offset
    ; end locals

    if x = #$05                                   ; check enemy offset for special/last slot

        ldy Vine::FlagOffset
        dey                                       ; decrement vine flag in Y, use as offset

        if Vine::Height <> VineHeightData[ y ]    ; if vine has not reached certain height:

            if FrameCounter >> 2 == carry set     ; shift d1 into carry, (2 frames every 4)
                mb Enemy_Y_Position[ 5 ] := Enemy_Y_Position[ 5 ] -c #1   ; one pixel every frame it's time (carry is set)
                inc Vine::Height                  ; increment vine height
            endif
        endif

        if Vine::Height >= #8                     ; if vine large enough:

            jsr RelativeEnemyPosition             ; get relative coordinates of vine,
            jsr GetEnemyOffscreenBits             ; and any offscreen bits
            ldy #0                                ; initialize offset used in draw vine sub

            repeat
                jsr DrawVine                      ; draw vine
                iny                               ; increment offset
            until y = Vine::FlagOffset            ; do not yet match, loop back to draw more vine

            if Enemy_OffscreenBits & #%00001100   ; if saved offscreen bits set:

                dey                               ; decrement Y to get proper offset again

                do
                    mb x := Vine[ y ]::ObjOffset  ; get enemy object offset for this vine object
                    jsr EraseEnemyObject          ; kill this vine object
                while dey == positive             ; if any vine objects left, loop back to kill it
                ; reg A now has #0
                sta Vine::FlagOffset              ; initialize vine flag/offset
                sta Vine::Height                  ; initialize vine height

            endif

            if Vine::Height >= #$20               ; if vine big (greater than 31 pixels tall)
                ldx #$06                          ; set offset in X to last enemy slot
                lda #1                            ; set A to obtain horizontal in $04, but we don't care
                ldy #$1b                          ; set Y to offset to get block at ($04, $10) of coordinates
                jsr BlockBufferCollision          ; do a sub to get block buffer address set, return contents

                if ldy VerticalHighNybble < #$d0 && lda (BlockBufferPointer)[ y ] == zero
                    ; if vertical high nybble offset beyond extent of
                    ; current block buffer, branch to leave, do not write
                    ; otherwise check contents of block buffer at pointer are emprty
                    mb (BlockBufferPointer)[ y ] := #$26               ; otherwise, write climbing metatile to block buffer
                endif
            endif
        endif
    endif

    ldx ObjectOffset                              ; get enemy object offset and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

CannonBitmasks:
    .byte %00001111, %00000111

.code

.proc ProcessCannons
    ; if not water area:
    if AreaType

        ldx #2
        repeat

            stx ObjectOffset                      ; start at third enemy slot
            if not Enemy_Flag[ x ]                ; check enemy buffer flag

                mb a := PseudoRandomBitReg[ 1 + x ]                    ; otherwise get part of LSFR
                ldy SecondaryHardMode             ; get secondary hard mode flag, use as offset
                mb a := a & CannonBitmasks[ y ]   ; mask out bits of LSFR as decided by flag

                if ( a < #$06 ) && ( tay : lda Cannon_PageLoc[ y ] )

                    if Cannon_Timer[ y ]          ; get cannon timer
                        mb Cannon_Timer[ y ] := a - C   ; decrement (note carry will always be clear here) to count timer down
                    elseif !TimerControl          ; if master timer control not set:
                        ; create bullet
                        mb Cannon_Timer      [ y ] := #$0e             ; first, reset cannon timer
                        mb Enemy_PageLoc     [ x ] := Cannon_PageLoc[ y ]              ; save as page location of bullet bill
                        mb Enemy_X_Position  [ x ] := Cannon_X_Position[ y ]           ; save as horizontal coordinate of bullet bill
                        mb Enemy_Y_Position  [ x ] := Cannon_Y_Position[ y ] - #$08    ; save as vertical coordinate of bullet bill
                        mb Enemy_Y_HighPos   [ x ] := #1               ; set vertical high byte of bullet bill
                        mb Enemy_Flag        [ x ] := a      ; #1
                        mb Enemy_State       [ x ] := a >> 1 ; #0
                        mb Enemy_BoundBoxCtrl[ x ] := #$9              ; set bounding box size control for bullet bill
                        mb Enemy_ID          [ x ] := #OBJECTID_BulletBill_CannonVar  ; load identifier for bullet bill

                        jmp Next3Slt              ; move onto next slot

                    endif
                endif
            endif

            if Enemy_ID[ x ] = #OBJECTID_BulletBill_CannonVar                   ; check enemy identifier for bullet bill (cannon variant)

                jsr OffscreenBoundsCheck          ; check to see if it went offscreen

                if Enemy_Flag[ x ]                ; check enemy buffer flag
                    jsr GetEnemyOffscreenBits     ; get offscreen information
                    jsr BulletBillHandler         ; then do sub to handle bullet bill
                endif
            endif

            Next3Slt:

        until dex == negative                     ; do this until first three slots are checked

    endif

    rts                                           ; then leave
.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

BulletBillXSpdData:
    .byte $18, <-$18

.code

.proc BulletBillHandler

    if !TimerControl                              ; if master timer control set

        if !Enemy_State[ x ]                      ; if bullet bill's state not set

            if Enemy_OffscreenBits & #%00001100 = #%00001100 goto KillBB
            ldy #1                                ; set to move right by default
            ; get horizontal difference between player and bullet bill
            if jsr PlayerEnemyDiff == positive    ; if enemy to the right of player:
                iny                               ; increment to move left
            endif

            mb Enemy_MovingDir[ x ] := y          ; set bullet bill's moving direction
            dey                                   ; decrement to use as offset

            mb Enemy_X_Speed[ x ] := BulletBillXSpdData[ y ]           ; and store it
            ; if less than a certain amount, player is too close
            ; to cannon either on left or right side, thus branch
            if temp_byte +c #$28 < #$50 goto KillBB                    ; get horizontal difference

            mb Enemy_State    [ x ] := #1
            mb EnemyFrameTimer[ x ] := #$0a
            mb Square2SoundQueue    := #SFX_Blast                      ; play fireworks/gunfire sound

        endif

        if Enemy_State[ x ] & #%00100000          ; check enemy state for d5 set

            jsr MoveD_EnemyVertically             ; do sub to move bullet bill vertically

        endif

        jsr MoveEnemyHorizontally                 ; do sub to move bullet bill horizontally

    endif

    jsr GetEnemyOffscreenBits                     ; get offscreen information
    jsr RelativeEnemyPosition                     ; get relative coordinates
    jsr GetEnemyBoundBox                          ; get bounding box coordinates
    jsr PlayerEnemyCollision                      ; handle player to enemy collisions
    jmp EnemyGfxHandler                           ; draw the bullet bill and leave

    KillBB:

    jsr EraseEnemyObject                          ; kill bullet bill and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

HammerEnemyOfsData:
    .byte $04, $04, $04, $05, $05, $05
    .byte $06, $06, $06

HammerXSpdData:
    .byte $10, $f0

.code

.proc SpawnHammerObj
    ; a random number from 0 to 8
    if !PseudoRandomBitReg[ 1 ] & #%00000111
        mb a := PseudoRandomBitReg[ 1 ] & #%00001000
    endif

    tay                                           ; use either d3 or d2-d0 for offset here
    ; if any values loaded in $2a-$32 where offset is then leave with carry clear
    if !Misc_State[ y ]

        ldx HammerEnemyOfsData,y                  ; get offset of enemy slot to check using Y as offset
        if !Enemy_Flag[ x ]                       ; check enemy buffer flag at offset if buffer flag set, branch to leave with carry clear

            mb x, a := ObjectOffset
            mb HammerEnemyOffset[ y ] := a
            mb Misc_State       [ y ] := #$90
            mb Misc_BoundBoxCtrl[ y ] := #$07
            sec
            rts
        endif
    endif

    ldx ObjectOffset                              ; get original enemy object offset
    clc                                           ; return with carry clear
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc ProcHammerObj

    if !TimerControl

        mb a := Misc_State[ x ] & #%01111111      ; otherwise get hammer's state,mask out d7

        ldy HammerEnemyOffset,x                   ; get enemy object offset that spawned this hammer

        if a <> #2                                ; check hammer's state

            if greaterORequal goto SetHPos        ; if greater than 2, branch elsewhere

            ; setup gravity
            mb a              := x
            mb x              := a + #$0d         ; add 13 bytes to X to use proper misc object
            mb temp_byte      := #$10             ; set downward movement force
            mb temp_byte[ 1 ] := #$0f             ; set upward movement force (not used)
            mb temp_byte[ 2 ] := #4               ; set maximum vertical speed

            lda #0                                ; set A to impose gravity on hammer
            jsr ImposeGravity                     ; do sub to impose gravity on hammer and move vertically
            jsr MoveObjectHorizontally            ; do sub to move it horizontally
            ldx ObjectOffset                      ; get original misc object offset

        else                                      ; a = #2 :

            mb Misc_Y_Speed[ x ] := #$fe          ; set hammer's vertical speed
            mb Enemy_State[ y ] := Enemy_State[ y ] & #%11110111

            mb x := Enemy_MovingDir[ y ]          ; get enemy's moving direction
            dex                                   ; decrement to use as offset

            mb a := HammerXSpdData[ x ]           ; get proper speed to use based on moving direction
            ldx ObjectOffset                      ; reobtain hammer's buffer offset
            mb Misc_X_Speed[ x ] := a             ; set hammer's horizontal speed

            SetHPos:                              ; a > #2

            dec Misc_State,x                      ; decrement hammer's state

            mb Misc_X_Position  [ x ] := Enemy_X_Position [ y ] + #2
            mb Misc_PageLoc     [ x ] := Enemy_PageLoc    [ y ] + C
            mb Misc_Y_Position  [ x ] := Enemy_Y_Position [ y ] - #10
            mb Misc_Y_HighPos   [ x ] := #1

            bne RunHSubs                          ; unconditional branch to skip first routine

        endif

        jsr PlayerHammerCollision                 ; handle collisions

    endif

    RunHSubs:

    jsr GetMiscOffscreenBits                      ; get offscreen information
    jsr RelativeMiscPosition                      ; get relative coordinates
    jsr GetMiscBoundBox                           ; get bounding box coordinates
    jsr DrawHammer                                ; draw the hammer

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc CoinBlock

    ; locals
        VerticalHighNybble = temp_byte + 2 ; - used to store vertical high nybble offset from block buffer routine
        BlockBufferPointer = temp_byte + 6 ; - used to store low byte of block buffer address
    ; end locals


    .export SetupJumpCoin

    jsr FindEmptyMiscSlot                         ; set offset for empty or last misc object buffer slot
    ; on return y hold empty slot index

    mb Misc_PageLoc   [ y ] := Block_PageLoc   [ x ]                   ; store as page location of misc object
    mb Misc_X_Position[ y ] := Block_X_Position[ x ] | #5              ; store as horizontal coordinate of misc object
    mb Misc_Y_Position[ y ] := Block_Y_Position[ x ] -c #$10

    jmp JCoinC                                    ; jump to rest of code as applies to this misc object
    ; OP this section should probably be moved to the calling routine since it is only called frm there
        SetupJumpCoin:

        jsr FindEmptyMiscSlot                                          ; set offset for empty or last misc object buffer slot
        mb Misc_PageLoc     [ y ] := Block_PageLoc2[ x ]               ; and save as page location for misc object
        mb Misc_X_Position  [ y ] := BlockBufferPointer << 4 | #5          ; low byte of block buffer offset
        ; get vertical high nybble offset from earlier
        ; add 32 pixels for the status bar
        mb Misc_Y_Position[ y ] := VerticalHighNybble +c #$20              ; store as vertical coordinate

    JCoinC:

    mb Misc_Y_Speed[ y ]   := #$fb                ; set vertical speed
    mb a                   := #1
    mb Misc_Y_HighPos[ y ] := a                   ; set vertical high byte
    mb Misc_State[ y ]     := a                   ; set state for misc object
    mb Square2SoundQueue   := a                   ; SFX_CoinGrab

    stx ObjectOffset                              ; store current control bit as misc object offset
    jsr GiveOneCoin                               ; update coin tally on the screen and coin amount variable
    inc CoinTallyFor1Ups                          ; increment coin tally used to activate 1-up block flag
    rts
.endproc

.proc FindEmptyMiscSlot

    ; OP: this code is probably buggy, it starts at slot 8 and only checks 8, 7, 6, then defaults to 8

    ldy #$08                                      ; start at end of misc objects buffer
    repeat
        if lda Misc_State[ y ] == zero goto UseSlot
    until dey = #$05

    ldy #$08                                      ; if no empty slots found, use last slot

    UseSlot:

    sty JumpCoinMiscOffset                        ; store offset of misc object buffer here (residual)
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc MiscObjectsCore

    .export MiscLoopBack

    ldx #$08                                      ; set at end of misc object buffer

    repeat

        stx ObjectOffset                          ; store misc object offset here

        if Misc_State[ x ]                        ; check misc object state

            if a << 1 == carry set                ; if d7 not set, jumping coin, thus skip to rest of code here
                jsr ProcHammerObj                 ; otherwise go to process hammer,
                jmp MiscLoopBack                  ; then check next slot
            endif

            if y := Misc_State[ x ] - 1 != zero   ; check misc object state, decrement to see if it's set to 1

                inc Misc_State,x                  ; otherwise increment state to either start off or as timer
                ; whether its jumping coin (state 0 only) or floatey number:

                mb Misc_X_Position[ x ] := Misc_X_Position[ x ] + ScrollAmount       ; store as new horizontal coordinate
                mb Misc_PageLoc[ x ] := Misc_PageLoc[ x ] + C        ; add carry

                if Misc_State[ x ] <> #$30 goto RunJCSubs
                mb Misc_State[ x ] := #0          ; otherwise nullify object state

            else ; jumping coin:
           
                mb a := x
                mb x := a + #$0d                  ; add 13 bytes to offset for next subroutine

                ; gravity setup:
                mb temp_byte := #$50              ; set downward movement amount
                mb temp_byte[ 2 ] := #$06         ; set maximum vertical speed
                mb temp_byte[ 1 ] := a / 2        ; / 2 and as upward movement amount (apparently residual)

                lda #0                            ; set A to impose gravity on jumping coin
                jsr ImposeGravity                 ; do sub to move coin vertically and impose gravity on it

                ldx ObjectOffset                  ; get original misc object offset
                if Misc_Y_Speed[ x ] = #$05       ; check vertical speed
                    inc Misc_State,x              ; increment state to change to floatey number
                endif

                RunJCSubs:

                jsr RelativeMiscPosition          ; get relative coordinates
                jsr GetMiscOffscreenBits          ; get offscreen information
                jsr GetMiscBoundBox               ; get bounding box coordinates (why?)
                jsr JCoinGfxHandler               ; draw the coin or floatey number
            endif
        endif

        MiscLoopBack:

    until dex == negative
    rts                                           ; then leave
.endproc

 ; ------------------------------------------------------------------------------------------------

dataseg

CoinTallyOffsets:
    .byte $17, $1d

ScoreOffsets:
    .byte $0b, $11

StatusBarNybbles:
    .byte $02, $13

.code

    ; ------------------------------------------------------------------------------------------------

.proc GiveOneCoin

    mb DigitModifier[ 5 ] := #1                   ; set digit modifier to add 1 coin; to the current player's coin tally

    ldx CurrentPlayer                             ; get current player on the screen
    ldy CoinTallyOffsets,x                        ; get offset for player's coin tally

    jsr DigitsMathRoutine                         ; update the coin tally

    inc CoinTally                                 ; increment onscreen player's coin amount

    if CoinTally = #100
        mb CoinTally := #0                        ; otherwise, reinitialize coin amount
        inc NumberofLives                         ; give the player an extra life
        mb Square2SoundQueue := #SFX_ExtraLife    ; play 1-up sound
    endif

    mb DigitModifier[ 4 ] := #2                   ; set digit modifier to award 200 points to the player

.endproc


.proc AddToScore
    ldx CurrentPlayer                             ; get current player
    mb y := ScoreOffsets[ x ]                     ; get offset for player's score
    jsr DigitsMathRoutine                         ; update the score internally with value in digit modifier
.endproc

.proc GetSBNybbles
    ldy CurrentPlayer                             ; get current player
    mb a := StatusBarNybbles[ y ]                 ; get nybbles based on player, use to update score and coins
.endproc

.proc UpdateNumber

    jsr PrintStatusBarNumbers                     ; print status bar numbers based on nybbles, whatever they be

    ldy VRAM_Buffer1_Offset
    ; check highest digit of score
    ; if zero, overwrite with space tile for zero suppression, only write if more than 1mill points

    if lda VRAM_Buffer1[ y - 6 ] == zero
        mb VRAM_Buffer1[ y - 6 ] := #$24
    endif

    ldx ObjectOffset                              ; get enemy object buffer offset

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc SetupPowerUp

    .export PwrUpJmp

    mb Enemy_ID          [ 5 ] := #OBJECTID_PowerUpObject           ; load power-up identifier into special use slot of enemy object buffer
    mb Enemy_PageLoc     [ 5 ] := Block_PageLoc   [ x ]
    mb Enemy_X_Position  [ 5 ] := Block_X_Position[ x ]
    mb Enemy_Y_HighPos   [ 5 ] := #1                                ; set vertical high byte of power-up object
    mb Enemy_Y_Position  [ 5 ] := Block_Y_Position[ x ] - #8        ; subtract 8 pixels
    
    PwrUpJmp:                                     ; this is a residual jump point in enemy object jump `
    
    mb a, Enemy_State    [ 5 ] := #1              ; set power-up object's state
    mb Enemy_Flag        [ 5 ] := a               ; set buffer flag
    mb Enemy_BoundBoxCtrl[ 5 ] := #3              ; set bounding box size control for power-up object
    ; PowerUpType: 0 = shroom, 1 = fireflower, 2 = star, 3 = 1up shroom
    if PowerUpType < #2                           ;  if not star or 1-up:
        ; if player not fiery, use status as power-up type
        ; 0 = small, 1 = super, 2 = firey
        if PlayerStatus >= #2                     ; otherwise check player's current status
            lsr                                   ; otherwise shift right to force value to 1 = fire flower type
        endif

        sta PowerUpType                           ; store type here

    endif

    mb Enemy_SprAttrib[ 5 ] := #%00100000         ; set background priority bit
    mb Square2SoundQueue    := #SFX_GrowPowerUp   ; load power-up reveal sound and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PowerUpObjHandler


    mb x, ObjectOffset := #$05                    ; set object offset for last slot in enemy object buffer

    if Enemy_State[ 5 ]                           ; check power-up object's state

        if a << 1 == carry set                    ; shift to check if d7 was set in object state OP: use N flag

            if TimerControl goto RunPUSubs        ; if master timer control set branch ahead to enemy object routines

            if lda PowerUpType != zero && a <> #3                      ; if not either kind of mushroom, then:

                if a <> #2 goto RunPUSubs         ; if not star, branch elsewhere to skip movement, code could be if (a = 1)
                ; execution here means it's a star:

                jsr MoveJumpingEnemy              ; otherwise impose gravity on star power-up and make it jump
                jsr EnemyJump                     ; note that green paratroopa shares the same code here
                jmp RunPUSubs                     ; then jump to other power-up subroutines

            endif

            jsr MoveNormalEnemy                   ; do sub to make mushrooms move
            jsr EnemyToBGCollisionDet             ; deal with collisions

        else

            if !FrameCounter & #%00000011         ; every 4 frames

                dec Enemy_Y_Position + 5          ; otherwise decrement vertical coordinate slowly
                mb a := Enemy_State[ 5 ]          ; load power-up object state
                inc Enemy_State + 5               ; increment state for next frame (to make power-up rise)

                if a >= #$11                      ; if power-up object state not yet past 16th pixel,

                    mb Enemy_X_Speed    [ x ] := #$10                  ; otherwise set horizontal speed
                    mb Enemy_State      [ 5 ] := #%10000000            ; and then set d7 in power-up object's state
                    mb Enemy_SprAttrib  [ 5 ] := a << 1                ; a = 0        ; initialize background priority bit set here
                    rol                           ; carry is set, a := 1
                    mb Enemy_MovingDir  [ x ] := a                     ; set moving direction

                endif
            endif

            if Enemy_State[ 5 ] < #$06 goto exit                       ; check power-up object's state if power-up has risen enough

        endif

        RunPUSubs:

        jsr RelativeEnemyPosition                 ; get coordinates relative to screen
        jsr GetEnemyOffscreenBits                 ; get offscreen bits
        jsr GetEnemyBoundBox                      ; get bounding box coordinates
        jsr DrawPowerUp                           ; draw the power-up object
        jsr PlayerEnemyCollision                  ; check for collision with player
        jsr OffscreenBoundsCheck                  ; check to see if it went offscreen

    endif

    exit:

    rts                                           ; and we're done

.endproc
    
    ; These apply to all routines in this section unless otherwise noted:

dataseg

BlockYPosAdderData:
    .byte $04, $12

.code

.proc PlayerHeadCollision

    ; locals
        WhichBlockhit       = temp_byte           ; used to store metatile from block buffer routine
        VerticalHighNybble  = temp_byte + 2       ; used to store vertical high nybble offset from block buffer routine
        SavedMetatile       = temp_byte + 5       ; used to store metatile stored in A at beginning of PlayerHeadCollision
        BlockBufferPointer  = temp_byte + 6       ; used as block buffer address indirect
    ; end locals

    pha                                           ; store metatile number to stack

        lda #$11                                  ; load unbreakable block object state by default
        ldx SprDataOffset_Ctrl                    ; load offset control bit here
        
        if ldy PlayerSize == BigMario             ; check player's size
            lda #$12                              ; load breakable block object state
        endif
        
        mb Block_State[ x ] := a                  ; #$12 or #$11                   ; store into block object buffer
        jsr DestroyBlockMetatile                  ; store blank metatile in vram buffer to write to name table
        
        ldx SprDataOffset_Ctrl                    ; load offset control bit
        ;  get vertical high nybble offset used in block buffer routine set as vertical coordinate for block object
        mb Block_Orig_YPos[ x ] := VerticalHighNybble    
        
        tay
        
        mb Block_BBuf_Low[ x ] := BlockBufferPointer    ; get low byte of block buffer address used in same routine
        mb a := (BlockBufferPointer)[ y ]         ; get contents of block buffer at old address at $06, $07
        ; returns carry set if found block:
        jsr BlockBumpedChk                        ; do a sub to check which block player bumped head on
        
        sta WhichBlockhit                         ; store metatile here
        ; if small, use metatile itself as contents of A if big use #0
        if ldy PlayerSize == BigMario             ; check player's size
            tya                                   ; init A (note: big = 0)
        endif
        
        
        if carry set                              ; if match was found in previous sub:
            ; load unbreakable state into block object buffer
            mb y, Block_State[ x ] := #$11        ; note this applies to both player sizes
        
            lda #$c4                              ; load empty block metatile into A for now
            ; if not a coin block, branch ahead to store empty block metatile
            if ldy WhichBlockhit <> #$58 && y <> #$5d goto PutMTileB
            ; execution here it is a coin block:
        
            if lda BrickCoinTimerFlag == zero     ; check brick coin timer flag
                mb BrickCoinTimer := #$0b         ; if not set, set brick coin timer, 12 * 20 = 240 frames = 4 seconds
                inc BrickCoinTimerFlag            ; and set flag linked to it
            endif
            ; check brick coin timer
        
            if lda BrickCoinTimer == zero         ; if expired
            ldy #$c4                              ; use empty block metatile
            endif
        
            tya                                   ; put metatile into A
        
        endif
        
        PutMTileB:
        
        mb Block_Metatile[ x ] := a               ; store whatever metatile be appropriate here
        
        jsr InitBlock_XY_Pos                      ; get block object horizontal coordinates saved
        
        mb y := VerticalHighNybble                ; get vertical high nybble offset
        mb (BlockBufferPointer)[ y ] := #$23      ; write blank metatile $23 to block buffer
        mb BlockBounceTimer := #$10               ; set block bounce timer
        
    pla                                           ; pull original metatile from stack

    mb SavedMetatile := a

    ldy #0                                        ; set default offset

    if CrouchingFlag || PlayerSize == SmallMario
        iny                                       ; increment for small or big and crouching
    endif

    mb Block_Y_Position[ x ] := Player_Y_Position + BlockYPosAdderData[ y ] & #$F0

    if ldy Block_State[ x ] <> #$11               ; if doesn't equal unbreakable
        jsr BrickShatter                          ; execute code for breakable brick
    else
        jsr BumpBlock                             ; execute code for unbreakable brick or question block
    endif

    mb SprDataOffset_Ctrl := SprDataOffset_Ctrl ^ #1      ; invert control bit used by block objects and floatey nums
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitBlock_XY_Pos
    ; calculate the position of the background block that was hit based on mario's sprite position
    mb Block_X_Position [ x ] := Player_X_Position + #8 & #$F0         ; mask out low nybble to give 16-pixel correspondence
    mb Block_PageLoc    [ x ] := Player_PageLoc + C                    ; add C, save as page location of block object
    mb Block_PageLoc2   [ x ] := a                                     ; save elsewhere to be used later
    mb Block_Y_HighPos  [ x ] := Player_Y_HighPos                      ; save vertical high byte of player into
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc BumpBlock

    ; locals
        SavedMetatile       = temp_byte + 5       ; used to store metatile stored in A at beginning of PlayerHeadCollision
    ; end locals

    jsr CheckTopOfBlock                           ; check to see if there's a coin directly above this block

    mb Square1SoundQueue := #SFX_Bump             ; play bump sound

    lda #0
    mb Block_X_Speed     [ x ] := a               ; initialize horizontal speed for block object
    mb Block_Y_MoveForce [ x ] := a               ; init fractional movement force
    mb Player_Y_Speed          := a               ; init player's vertical speed
    mb Block_Y_Speed     [ x ] := #$fe            ; set vertical speed for block object

    mb a := SavedMetatile                         ; get original metatile from stack

    jsr BlockBumpedChk                            ; do a sub to check which block player bumped head on

    if carry clear goto ExitBlockChk              ; if no match was found, branch to rts

    tya                                           ; move block number to A
    if a >= #$09                                  ; if block number is greater than 8
        sbc #$05                                  ; subtract 5 for second set to get proper number
    endif

    jsr JumpEngine                                ; run appropriate subroutine depending on block number


    .word MushFlowerBlock
    .word CoinBlock
    .word CoinBlock
    .word ExtraLifeMushBlock
    .word MushFlowerBlock
    .word VineBlock
    .word StarBlock
    .word CoinBlock
    .word ExtraLifeMushBlock

    MushFlowerBlock:

    lda #0                                        ; load mushroom/fire flower into power-up type
    BIT_Skip2                                     ; BIT instruction opcode

    StarBlock:

    lda #2                                        ; load star into power-up type
    BIT_Skip2                                     ; BIT instruction opcode

    ExtraLifeMushBlock:

    lda #3                                        ; load 1-up mushroom into power-up type
    sta PowerUpType                               ; store correct power-up type
    jmp SetupPowerUp

    VineBlock:

    ldx #$05                                      ; load last slot for enemy object buffer
    ldy SprDataOffset_Ctrl                        ; get control bit
    jsr Setup_Vine                                ; set up vine object

    ExitBlockChk:

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

BrickQBlockMetatiles:
    .byte $c1, $c0, $5f, $60                      ; used by question blocks
    ; these two sets are functionally identical, but look different
    .byte $55, $56, $57, $58, $59                 ; used by ground level types
    .byte $5a, $5b, $5c, $5d, $5e                 ; used by other level types

.code

.proc BlockBumpedChk

    ldy #$0d                                      ; start at end of metatile data

    repeat
        if a = BrickQBlockMetatiles[ y ] \
        goto MatchBump                            ; check to see if current metatile matches metatile in block buffer
    until dey == negative
    clc                                           ; if none match, return with carry clear
    MatchBump:
    rts                                           ; note carry is set if found match

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc BrickShatter

    jsr CheckTopOfBlock                           ; check to see if there's a coin directly above this block

    mb Block_RepFlag[ x ] := #SFX_BrickShatter    ; set flag for block object to immediately replace metatile
    mb NoiseSoundQueue := a                       ; load brick shatter sound

    jsr SpawnBrickChunks                          ; create brick chunk objects

    mb Player_Y_Speed := #(<-2)                   ; set vertical speed for player
    mb DigitModifier[ 5 ] := #$05                 ; set digit modifier to give player 50 points

    jsr AddToScore                                ; do sub to update the score

    ldx SprDataOffset_Ctrl                        ; load control bit and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc CheckTopOfBlock
    
    ; locals
        BlockBufferPointer = temp_byte + 6
    ; end locals

    ldx SprDataOffset_Ctrl                        ; load control bit

    if ldy $02 != zero                            ; get vertical high nybble offset used in block buffer
        mb a := y                                 ; otherwise set to A
        mb a, $02 := a - #$10   ; subtract $10 to move up one row in the block buffer, store as new vertical high nybble offset
        mb y := a
        ; get contents of block buffer in same column, one row up, if it is a coin:
        if (BlockBufferPointer)[ y ] = #$c2
            mb (BlockBufferPointer)[ y ] := #0           ; put blank metatile where coin was
            jsr RemoveCoin_Axe                    ; write blank metatile to vram buffer
            ldx SprDataOffset_Ctrl                ; get control bit
            jsr SetupJumpCoin                     ; create jumping coin object and update coin variables
        endif
    endif

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc SpawnBrickChunks


    mb Block_Orig_XPos      [ x ] := Block_X_Position[ x ]     ; set horizontal coordinate of block object, as original horizontal coord
    mb a, Block_X_Speed     [ x ] := #$f0         ; set horizontal speed for brick chunk objects
    mb Block_X_Speed    [ 2 + x ] := a
    mb Block_Y_Speed        [ x ] := #$fa         ; set vertical speed for one
    mb Block_Y_Speed    [ 2 + x ] := #$fc         ; set vertical speed for the other
    mb a, Block_Y_MoveForce [ x ] := #0           ; init fractional movement force for both
    mb Block_Y_MoveForce[ 2 + x ] := a
    mb Block_PageLoc    [ 2 + x ] := Block_PageLoc[ x ]                ; copy page location
    mb Block_X_Position [ 2 + x ] := Block_X_Position[ x ]             ; copy horizontal coordinate
    mb Block_Y_Position [ 2 + x ] := Block_Y_Position[ x ] + #8        ; add 8 pixels to vertical coordinate
    mb Block_Y_Speed        [ x ] := #$fa         ; set vertical speed...again??? (redundant) OP

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc BlockObjectsCore

    if lda Block_State[ x ] != zero               ; get state of block object

        and #$0f                                  ; mask out high nybble
        pha                                       ; push to stack

        tay                                       ; put state in Y for now
        ; add 9 bytes to offset (note two block objects are created
        ; when using brick chunks, but only one offset for both)
        mb a := x
        mb x := a + #9
        ; if not solid:
        if y - 1                                  ; if block_state[ x ] & $0F <> 1 ( 1 = solid)

            jsr ImposeGravityBlock                ; do sub to impose gravity on one block object object
            jsr MoveObjectHorizontally            ; do another sub to move horizontally

            mb a := x
            mb x := a + #2                        ; move index to next block object OP : inx, inx

            jsr ImposeGravityBlock                ; do sub to impose gravity on other block object
            jsr MoveObjectHorizontally            ; do another sub to move horizontally

            ldx ObjectOffset                      ; get block object offset used for both
            jsr RelativeBlockPosition             ; get relative coordinates
            jsr GetBlockOffscreenBits             ; get offscreen information
            jsr DrawBrickChunks                   ; draw the brick chunks

            pla                                   ; get "Block_State[ x ] & $0F"
                ; check vertical high byte of block object,if above screen kill it
                if ldy Block_Y_HighPos[ x ] == zero goto UpdateState
            pha                                   ; save state back into stack
            ; check to see if bottom block object went to the bottom of the screen:
            if #$f0 < Block_Y_Position[ 2 + x ]
                mb Block_Y_Position[ 2 + x ] := a         ; #$f0       ; otherwise set offscreen coordinate
            endif
            
            ; if top block is still above the bottom of the screen, pull block object state from stack
            ; and branch to save state

            if Block_Y_Position[ x ] < #$f0 : pla goto UpdateState          ; get top block object's vertical coordinate

        else C set    ; here, block_state[ x ] & $0F = 1 ( 1 = solid) "bump"
            jsr ImposeGravityBlock                ; do sub to impose gravity on block object

            ldx ObjectOffset                      ; get block object offset
            jsr RelativeBlockPosition             ; get relative coordinates
            jsr GetBlockOffscreenBits             ; get offscreen information
            jsr DrawBlock                         ; draw the block

            ; if still above amount, still bouncing, not time to kill block yet, thus branch
            if Block_Y_Position[ x ] & #$0f >= #$05 : pla goto UpdateState
              
            mb Block_RepFlag[ x ] := #1           ; set flag to replace metatile

        endif

        lda #0                                    ; if branched here, nullify object state

    endif
    UpdateState:

    sta Block_State,x                             ; store contents of A in block object state
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------
    ; $02 - used to store offset to block buffer
    ; $06-$07 - used to store block buffer address

.proc BlockObjMT_Updater

    ldx #1                                        ; set offset to start with second block object

    repeat  ; x = 1 to 0

        stx ObjectOffset                          ; set offset here
        ; if vram buffer not used AND flag for block object replace set
        if VRAM_Buffer1 == zero && Block_RepFlag[ x ]

            mb temp_byte[ 6 ] := Block_BBuf_Low[ x ]      ; store block buffer low address
            mb temp_byte[ 7 ] := #$05                     ; set high byte of block buffer address
            ; get original vertical coordinate of block object, use as offset to block buffer
            mb temp_byte[ 2 ] := Block_Orig_YPos[ x ]

            tay

            mb ($06)[ y ] := Block_Metatile[ x ]  ; get metatile to be writtenwrite it to the block buffer

            jsr ReplaceBlockMetatile              ; do sub to replace metatile where block object is

            mb Block_RepFlag[ x ] := #0           ; clear block object flag

        endif

    until dex == negative

    rts                                           ; then leave
.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc MoveEnemyHorizontally

    inx                                           ; increment offset for enemy offset
    jsr MoveObjectHorizontally                    ; position object horizontally according to
    ldx ObjectOffset                              ; counters, return with saved value in A,
    rts                                           ; put enemy offset back in X and leave

.endproc

.proc MovePlayerHorizontally

    lda JumpspringAnimCtrl                        ; if jumpspring currently animating,
    bne ExXMove                                   ; branch to leave
    tax                                           ; otherwise set zero for offset to use player's stuff

.endproc                                          ; continue:

.proc MoveObjectHorizontally
    
    ; get currently saved value (horizontal speed, secondary counter, whatever)
    ; locals
        XSpeedHi    = temp_byte      ; - used to store high nybble of horizontal speed as adder
        XSpeedLo    = temp_byte + 1  ; - used to store low nybble of horizontal speed
        PageAdder   = temp_byte + 2  ; - used to store adder to page location
    ; end locals

    mb XSpeedLo := SprObject_X_Speed[ x ] << 4

    if SprObject_X_Speed[ x ] >> 4 >= #8          ; get saved value again
        ora #%11110000                            ; alter high nybble
    endif

    sta XSpeedHi                                  ; save result here
    ldy #0                                        ; load default Y value here
    if cmp #0 == negative                         ; if result positive, leave Y alone
        dey                                       ; otherwise decrement Y for -1
    endif

    mb PageAdder := y                             ; save Y here
    mb SprObject_X_MoveForce[ x ] := SprObject_X_MoveForce[ x ] + XSpeedLo
    ; save carry
    lda #0                                        ; init A
    rol                                           ; rotate carry into d0
    pha                                           ; push onto stack
        ror                                       ; rotate d0 back onto carry
        ; add carry plus saved value (high nybble moved to low, plus $f0 if necessary) to object's horizontal position
        mb SprObject_X_Position[ x ] := SprObject_X_Position[ x ] +c XSpeedHi
        ; add carry plus 0 or - 1 the object's page location and save
        mb SprObject_PageLoc[ x ] := SprObject_PageLoc[ x ] +c PageAdder

    pla
    clc                                           ; pull old carry from stack and add
    adc XSpeedHi                                  ; to high nybble moved to low

.endproc

ExXMove: rts

    ; ------------------------------------------------------------------------------------------------
    
.proc MovePlayerVertically

    ldx #0                                        ; set X for player offset
    lda TimerControl
    bne NoJSChk                                   ; if master timer control set, branch ahead
    lda JumpspringAnimCtrl                        ; otherwise check to see if jumpspring is animating
    bne ExXMove                                   ; branch to leave if so

    NoJSChk:

    lda VerticalForce                             ; dump vertical force
    sta temp_byte
    lda #4                                        ; set maximum vertical speed here
    jmp ImposeGravitySprObj                       ; then jump to move player vertically

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc MoveD_EnemyVertically

    .export MoveFallingPlatform

    ldy #$3d                                      ; set quick movement amount downwards
    lda Enemy_State,x                             ; then check enemy state
    cmp #$05                                      ; if not set to unique state for spiny's egg, go ahead
    bne ContVMove                                 ; and use, otherwise set different movement amount, continue on

    MoveFallingPlatform:

    ldy #$20                                      ; set movement amount

    ContVMove:

    jmp SetHiMax                                  ; jump to skip the rest of this

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc MoveRedPTroopaDown

    .export MoveRedPTroopaUp

    ldy #0                                        ; set Y to move downwards
    jmp MoveRedPTroopa                            ; skip to movement routine ; OP bit command here

        MoveRedPTroopaUp:                         ; ProcMoveRedPTroopa jumps in here
        ldy #1                                    ; set Y to move upwards

    MoveRedPTroopa:

    inx                                           ; increment X for enemy offset

    ; do gravity params
    mb temp_byte      := #3                       ; set downward movement amount here
    mb temp_byte[ 1 ] := #$06                     ; set upward movement amount here
    mb temp_byte[ 2 ] := #2                       ; set maximum speed here

    tya                                           ; set movement direction in A, and
    jmp DoGravity                                 ; jump to move this thing

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc MoveDropPlatform
    ldy #$7f                                      ; set movement amount for drop platform
    bne *+4                                       ; skip ahead of other value set here; OP: use bit command
.endproc

.proc MoveEnemySlowVert

    ldy #$0f                                      ; set movement amount for bowser/other objects
    ; branch here:
    lda #2                                        ; set maximum speed in A
    bne SetXMoveAmt                               ; unconditional branch

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc MoveJumpEnemyVertically
    
    ldy #$1c                                      ; set movement amount for podoboo/other objects
    .export SetHiMax
    SetHiMax:
    lda #3                                        ; set maximum speed in A
.endproc

.proc SetXMoveAmt
    sty temp_byte                                 ; set movement amount here
    inx                                           ; increment X for enemy offset
    jsr ImposeGravitySprObj                       ; do a sub to move enemy object downwards
    ldx ObjectOffset                              ; get enemy object buffer offset and leave
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

MaxSpdBlockData:
    .byte $06, $08

.code

ResidualGravityCode:
    ldy #0                                        ; this part appears to be residual,
    BIT_Skip2                                     ; no code branches or jumps to it...

.proc ImposeGravityBlock

    ldy #1                                        ; set offset for maximum speed
    mb temp_byte := #$50                          ; set movement amount here
    lda MaxSpdBlockData,y                         ; get maximum speed

.endproc

.proc ImposeGravitySprObj

    sta $02                                       ; set maximum speed here
    lda #0                                        ; set value to move downwards
    jmp ImposeGravity                             ; jump to the code that actually moves it

.endproc



.proc MovePlatformDown

    lda #0                                        ; save value to stack (if branching here, execute next
    BIT_Skip2                                     ; part as BIT instruction)

.endproc

.proc MovePlatformUp

    .export DoGravity

    lda #1                                        ; save value to stack
    pha
        ldy Enemy_ID,x                            ; get enemy object identifier
        inx                                       ; increment offset for enemy object

        lda #$05                                  ; load default value here
        if y = #$29                               ; OP: residual comparison, object #29 never executes
            lda #$09                              ; residual code
        endif

        ; Gravity setup:
        mb temp_byte      := a                    ; #$05                ; save downward movement amount here
        mb temp_byte[ 1 ] := #$0a                 ; save upward movement amount here
        mb temp_byte[ 2 ] := #3                   ; save maximum vertical speed here

    pla                                           ; get value from stack OP: we only save #1 so l
    tay                                           ; use as Y, then move onto code shared by red koopa

    DoGravity:

    jsr ImposeGravity                             ; do a sub to move object gradually
    ldx ObjectOffset                              ; get enemy object offset and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ImposeGravity
    
    ; non zero reg a indicates to do upwards movement
    
    ; parameters:
    ; locals
        dw_force    = temp_byte                       ; used for downward force
        up_force    = temp_byte + 1                   ; used for upward force
        max_v_speed = temp_byte + 2                   ; used for maximum vertical speed
        adder_temp  = temp_byte + 7                   ; used as adder_temp for vertical position and for negative max speed
    ; end locals
    
    pha                                           ; push value to stack

        mb SprObject_YMoveForceFractional[ x ] := SprObject_YMoveForceFractional[ x ] + SprObject_Y_MoveForce[ x ]

        ldy #0                                    ; set Y to zero by default
        if SprObject_Y_Speed[ x ] == negative     ; if currently moving upward
            dey                                   ; decrement Y
        endif
        mb adder_temp := y                        ; store Y here, 0 or FF
        ; store as new vertical position, add vertical position to vertical speed plus carry
        ; a = SprObject_Y_Speed[ x ]
        mb SprObject_Y_Position [ x ] := a +c SprObject_Y_Position[ x ]
        mb SprObject_Y_HighPos  [ x ] := SprObject_Y_HighPos[ x ] +c adder_temp
        mb SprObject_Y_MoveForce[ x ] := SprObject_Y_MoveForce[ x ] + dw_force
        mb SprObject_Y_Speed    [ x ] := SprObject_Y_Speed[ x ] + C
        ; compare to maximum speed
        ; a = SprObject_Y_Speed[ x ]
        if cmp max_v_speed == positive && SprObject_Y_MoveForce[ x ] >= #$80
            mb SprObject_Y_Speed    [ x ] := max_v_speed               ; keep vertical speed within maximum value
            mb SprObject_Y_MoveForce[ x ] := #0   ; clear fractional
        endif

    pla

    if not zero

        mb y := max_v_speed ^ #$FF                ; get two's compliment of maximum speed
        mb adder_temp := y + 1

        mb SprObject_Y_MoveForce[ x ] := SprObject_Y_MoveForce[ x ] - up_force
        ; subtract borrow from vertical speed and store
        mb SprObject_Y_Speed[ x ] := SprObject_Y_Speed[ x ] - C      ; sub carry
        ; a = SprObject_Y_Speed[ x ]
        if cmp adder_temp == negative && SprObject_Y_MoveForce[ x ] < #$80 ; compare vertical speed to two's compliment
            mb SprObject_Y_Speed    [ x ] := adder_temp                ; keep vertical speed within maximum value
            mb SprObject_Y_MoveForce[ x ] := #$ff                      ; clear fractional
        endif

    endif

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc EnemiesAndLoopsCore

    lda Enemy_Flag,x                              ; check data here for MSB set

    pha                                           ; save in stack

        if a << 1 == carry clear                  ; if bit 7 set of Enemy_Flag[ x ]

            pla                                   ; get from stack

            if not zero                           ; if reg a set jump to run enemy subroutines
                jmp RunEnemyObjectsCore
            endif

            if AreaParserTaskNum & #$07 = #$07 goto Exit               ; if at a specific task, jump and leave

            jmp ProcLoopCommand                   ; otherwise, jump to process loop command/load enemies

        endif

    pla                                           ; get data from stack

    mb y := a & #%00001111                        ; mask out high nybble
    ; use as pointer and load with different offset
    if ! Enemy_Flag[ y ]                          ; if second enemy flag not set, also clear first one
        mb Enemy_Flag[ x ] := a
    endif

    Exit:

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg
    ; loop command data
LoopCmdWorldNumber:                               ; world number is one less than world displayed

    .byte $03, $03, $06, $06, $06, $06, $06, $06, $07, $07, $07

LoopCmdPageNumber:
    .byte $05, $09, $04, $05, $06, $08, $09, $0a, $06, $0b, $10

LoopCmdYPosition:
    .byte $40, $b0, $b0, $80, $40, $40, $80, $40, $f0, $f0, $f0

.code

.proc ExecGameLoopback

    mb Player_PageLoc       := Player_PageLoc       - #4               ; send player back four pages
    mb CurrentPageLoc       := CurrentPageLoc       - #4               ; send current page back four pages
    mb ScreenLeft_PageLoc   := ScreenLeft_PageLoc   - #4
    mb ScreenRight_PageLoc  := ScreenRight_PageLoc  - #4
    mb AreaObjectPageLoc    := AreaObjectPageLoc    - #4

    lda #0                                        ; initialize page select for both
    sta EnemyObjectPageSel                        ; area and enemy objects
    sta AreaObjectPageSel
    sta EnemyDataOffset                           ; initialize enemy object data offset
    sta EnemyObjectPageLoc                        ; and enemy object page control

    mb AreaDataOffset := AreaDataOfsLoopback[ y ] ; adjust area object offset based on which loop command we encountered

    rts

.endproc


.proc ProcLoopCommand

    if LoopCommand && ( CurrentColumnPos == zero )                     ; check if loop command was found AND at correct Column

        ldy #$0b                                  ; start at the end of each set of loop data

        repeat

            if dey == negative goto ChkEnemyFrenzy                     ; if no matching loops, do next
            ; check to see if one of the world numbers matches our current world number AND
            ; if one of the page numbers matches the page we're currently on:

        until WorldNumber = LoopCmdWorldNumber[ y ] && CurrentPageLoc = LoopCmdPageNumber[ y ]
        ; fall through to check player postion:

        if Player_Y_Position = LoopCmdYPosition[ y ] && Player_State = #0
            ; check to see if the player is at the correct position AND  on solid ground (i.e. not jumping or falling)
            ; if not, branch to check for world 7
            if WorldNumber <> #WORLD7 goto InitMultLoop                ; are we in world 7? (check performed on correct
            ; vertical position and on solid ground)
            ; if not, initialize flags used there, otherwise

            inc MultiLoopCorrectCntr              ; increment counter for correct progression

            IncLoopPass:

            inc MultiLoopPassCntr                 ; increment master multi-part counter
            if MultiLoopPassCntr <> #3 goto InitLCmd                   ; have we done all three parts?
            if MultiLoopCorrectCntr = #3 goto InitMultLoop             ; if so, have we done them all correctly?

        else Z clear
            ; are we in world 7? (check performed on incorrect vertical position or not on solid ground)
            if WorldNumber = #WORLD7 goto IncLoopPass                  
        endif


        jsr ExecGameLoopback                      ; if player is not in right place, loop back
        jsr KillAllEnemies

        InitMultLoop:

        lda #0                                    ; initialize counters used for multi-part loop commands
        sta MultiLoopPassCntr
        sta MultiLoopCorrectCntr

        InitLCmd:

        lda #0                                    ; initialize loop command flag
        sta LoopCommand

    endif

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ChkEnemyFrenzy

    PageExtRightBoundry    = temp_byte + 6
    PageExtRightBoundryHiN = temp_byte + 7

    if lda EnemyFrenzyQueue                       ; check for enemy object in frenzy queue

        mb Enemy_ID         [ x ] := a            ; store as enemy object identifier here
        mb Enemy_Flag       [ x ] := #1           ; activate enemy object flag
        mb a, Enemy_State   [ x ] := #0           ; initialize state and frenzy queue
        mb EnemyFrenzyQueue       := a

        jmp InitEnemyObject                       ; and then jump to deal with this enemy

    endif

    ldy EnemyDataOffset                           ; get offset of enemy object data

    set_long_branch +
    if lda (EnemyData)[ y ] = #$ff goto CheckFrenzyBuffer              ; if found EOD terminator
    set_long_branch -
    ; a = (EnemyData)[ y ]

    if a & #%00001111 <> #$0e && x >= #$05        ; if low nybble <> #$0e AND at end of buffer
    
    ; not sure what this was intended for, exactly this part is quite possibly residual code
    ; but it has the effect of keeping enemies out of the sixth slot

        iny
        if (EnemyData)[ y ] & #%00111111 <> #$2e
            rts
        endif

    endif

    mb PageExtRightBoundryHiN   := ScreenRight_X_Pos + #$30 & #%11110000       ; add 48 pixels and store high nybble
    mb PageExtRightBoundry      := ScreenRight_PageLoc + C           ; add carry

    mb y := EnemyDataOffset + 1

    if (EnemyData)[ y ] << 1 == carry set && EnemyObjectPageSel == zero
        inc EnemyObjectPageSel                    ; set page select
        inc EnemyObjectPageLoc                    ; and increment page control
    endif

    dey                                           ; y := EnemyDataOffset
    if (EnemyData)[ y ] & #$0f = #$0f && EnemyObjectPageSel == zero    ; if low half is 15 AND other is not set

        iny
        mb EnemyObjectPageLoc := (EnemyData)[ y ] & #%00111111         ; store as page control for enemy object data

        inc EnemyDataOffset                       ; increment enemy object data offset 2 bytes
        inc EnemyDataOffset
        inc EnemyObjectPageSel                    ; set page select for enemy object data and
        jmp ProcLoopCommand                       ; jump back to process loop commands again
    endif

    mb Enemy_PageLoc[ x ] := EnemyObjectPageLoc   ; store page control as page location for enemy object
    mb Enemy_X_Position[ x ] := (EnemyData)[ y ] & #%11110000          ; get first byte of enemy object, store column position

    cmp ScreenRight_X_Pos                         ; check column position against right boundary to establish carry

    if Enemy_PageLoc[ x ] -c ScreenRight_PageLoc == less               ; If Enemy_PageLoc is onscreen from the right?

        if (EnemyData)[ y ] & #%00001111 = #$0e goto ParseRow0e        ; check for special row $0e
        jmp CheckThreeBytes                       ; if not found, unconditional jump

    endif

    lda PageExtRightBoundryHiN                    ; check right boundary + 48 against
    cmp Enemy_X_Position,x                        ; column position without subtracting,

    lda PageExtRightBoundry                       ; then subtract borrow from page control temp
    if a -c Enemy_PageLoc[ x ] == less goto CheckFrenzyBuffer          ; plus carry


    mb Enemy_Y_HighPos [ x ] := #1                ; store value in vertical high byte
    mb Enemy_Y_Position[ x ] := (EnemyData)[ y ] * 16

    if a = #$e0 goto ParseRow0e                   ; do one last check for special row $0e

    iny

    if (EnemyData)[ y ] & #%01000000 && ! SecondaryHardMode goto IncrementEDataOffsetX2

    mb a := (EnemyData)[ y ] & #%00111111
    if a >= #$37 && a < #$3f goto DoGroup

    if a = #OBJECTID_Goomba && ldy PrimaryHardMode
        lda #OBJECTID_BuzzyBeetle
    endif

    mb Enemy_ID  [ x ] := a                       ; store enemy object number into buffer
    mb Enemy_Flag[ x ] := #1                      ; set flag for enemy in buffer

    jsr InitEnemyObject

    if Enemy_Flag[ x ] goto IncrementEDataOffsetX2                     ; check to see if flag is set

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc CheckFrenzyBuffer

    .export InitEnemyObject

    if ! EnemyFrenzyBuffer                        ; if enemy object stored in frenzy buffer
        if Vine::FlagOffset <> #1 goto Exit ; rts ; otherwise check vine flag offset
        lda #OBJECTID_VineObject                  ; otherwise put vine in enemy identifier
    endif

    sta Enemy_ID,x                                ; store contents of frenzy buffer into enemy identifier value

    InitEnemyObject:

    mb Enemy_State[ x ] := #0                     ; initialize enemy state
    jsr CheckpointEnemyID                         ; jump ahead to run jump engine and subroutines

    Exit:

    rts

.endproc


.proc DoGroup
    jmp HandleGroupEnemies                        ; handle enemy group objects
.endproc

.proc ParseRow0e

    .export CheckThreeBytes
    ; .export IncrementEDataOffsetX2

    iny                                           ; increment Y to load third byte of object
    iny

    mb a := (EnemyData)[ y ] >> 5                 ; making %xxx00000 into %00000xxx
    ; Compare to current world (this allows multiple uses
    ; of the same area, like the underground bonus areas)

    if a = WorldNumber
        dey
        mb AreaPointer := (EnemyData)[ y ]        ; use as offset to addresses for level and enemy object data
        iny
        ; get third byte again, and this time mask out
        ; the 3 MSB from before, save as page number to be
        mb EntrancePage := (EnemyData)[ y ] & #%00011111  ; used upon entry to area, if area is entered
    endif

    jmp :+

        CheckThreeBytes:                          ; only one entry here - OP: move this code maybe needed here du to branch??
        mb y := EnemyDataOffset                   ; load current offset for enemy object data
        if (EnemyData)[ y ] & #%00001111 <> #$0e goto IncrementEDataOffsetX2   

    :

    inc EnemyDataOffset                           ; if row = $0e, increment three bytes

.endproc

.proc IncrementEDataOffsetX2                      ; Inc2B

    inc EnemyDataOffset                           ; otherwise increment two bytes
    inc EnemyDataOffset

    mb EnemyObjectPageSel := #0                   ; init page select for enemy objects
    ldx ObjectOffset                              ; reload current offset in enemy buffers
    rts                                           ; and leave

.endproc

.proc CheckpointEnemyID

    if Enemy_ID[ x ] < #$15
    ; carry clear here
        tay                                       ; save identifier in Y register for now
          mb Enemy_Y_Position      [ x ] := Enemy_Y_Position[ x ] +c #$08          ; enemy object's vertical coordinate ($00-$14 only)
          mb EnemyOffscrBitsMasked [ x ] := #1    ; set offscreen masked bit
        tya                                       ; get identifier back and use as offset for jump engine
    endif

    jsr JumpEngine
    ; jump engine table for newly loaded enemy objects

    .word InitNormalEnemy                         ; for objects $0-$0f
    .word InitNormalEnemy
    .word InitNormalEnemy
    .word InitRedKoopa
    .word NoInitCode
    .word InitHammerBro
    .word InitGoomba
    .word InitBloober
    .word InitBulletBill
    .word NoInitCode
    .word InitCheepCheep
    .word InitCheepCheep
    .word InitPodoboo
    .word InitPiranhaPlant
    .word InitJumpGPTroopa
    .word InitRedPTroopa

    .word InitHorizFlySwimEnemy                   ; for objects $10-$1f
    .word InitLakitu
    .word InitEnemyFrenzy
    .word NoInitCode
    .word InitEnemyFrenzy
    .word InitEnemyFrenzy
    .word InitEnemyFrenzy
    .word InitEnemyFrenzy
    .word EndFrenzy
    .word NoInitCode
    .word NoInitCode
    .word InitShortFirebar
    .word InitShortFirebar
    .word InitShortFirebar
    .word InitShortFirebar
    .word InitLongFirebar

    .word NoInitCode                              ; for objects $20-$2f
    .word NoInitCode
    .word NoInitCode
    .word NoInitCode
    .word InitBalPlatform
    .word InitVertPlatform
    .word LargeLiftUp
    .word LargeLiftDown
    .word InitHoriPlatform
    .word InitDropPlatform
    .word InitHoriPlatform
    .word PlatLiftUp
    .word PlatLiftDown
    .word InitBowser
    .word PwrUpJmp                                ; possibly dummy value
    .word Setup_Vine

    .word NoInitCode                              ; for objects $30-$36
    .word NoInitCode
    .word NoInitCode
    .word NoInitCode
    .word NoInitCode
    .word InitRetainerObj
    .word EndOfEnemyInitCode                      ; OP

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc NoInitCode
    rts                                           ; this executed when enemy object has no init code
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitGoomba
    jsr InitNormalEnemy                           ; set appropriate horizontal speed
    jmp SmallBBox                                 ; set $09 as bounding box control, set other values
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitPodoboo

    mb a, Enemy_Y_HighPos   [ x ] := #2           ; set enemy position to below the bottom of the screen
    mb Enemy_Y_Position     [ x ] := a      ; #2
    mb EnemyIntervalTimer   [ x ] := a >> 1 ; #1  ; set timer for enemy
    mb Enemy_State          [ x ] := a >> 1 ; #0  ; initialize enemy state, then jump to use
    jmp SmallBBox                                 ; $09 as bounding box size and set other things

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitRetainerObj

    mb Enemy_Y_Position[ x ] := #$b8              ; fixed v pos. princess/mushroom retainer object
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

NormalXSpdData:
    .byte $f8, $f4

.code

.proc InitNormalEnemy

    .export SetEnemySpeed
    ; OP : just ldy PrimaryHardMode rather than branching ??

    ldy #1                                        ; load offset of 1 by default

    if !PrimaryHardMode
        dey  ; y := 0                             ; if not set, decrement offset
    endif

    mb a := NormalXSpdData[ y ]                   ; get appropriate horizontal speed

    SetEnemySpeed:

    mb Enemy_X_Speed[ x ] := a                    ; store as speed for enemy object
    jmp TallBBox                                  ; branch to set bounding box control and other data

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitRedKoopa

    jsr InitNormalEnemy                           ; load appropriate horizontal speed
    mb Enemy_State[ x ] := #1                     ; set enemy state for red koopa troopa $03
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

HBroWalkingTimerData:
    .byte $80, $50

.code

.proc InitHammerBro

    lda #0                                        ; init horizontal speed and timer used by hammer bro
    mb HammerThrowingTimer  [ x ] := a            ; apparently to time hammer throwing
    mb Enemy_X_Speed        [ x ] := a

    ldy SecondaryHardMode                         ; get secondary hard mode flag

    mb EnemyIntervalTimer[ x ] := HBroWalkingTimerData[ y ]            ; set value as delay for hammer bro to walk left

    lda #$0b                                      ; set specific value for bounding box size control
    jmp SetBoundingBox

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitHorizFlySwimEnemy
    lda #0                                        ; initialize horizontal speed
    jmp SetEnemySpeed
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitBloober

    mb BlooperMoveSpeed[ x ] := #0

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc SmallBBox

    lda #$09                                      ; set specific bounding box size control
    bne ::SetBoundingBox                          ; unconditional branch

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitRedPTroopa

    .export TallBBox

    ldy #$30                                      ; load central position adder for 48 pixels down

    ; set vertical coordinate into location to be used as original vertical coordinate
    mb RedPTroopaOrigXPos[ x ] := Enemy_Y_Position[ x ] 

    if negative
        ldy #$e0                                  ; if => $80, load position adder for 32 pixels up
    endif

    tya                                           ; send central position adder to A

    mb RedPTroopaCenterYPos[ x ] := a +c Enemy_Y_Position[ x ]         ; store as central vertical coordinate

    TallBBox:

    lda #3                                        ; set specific bounding box size control

.endproc

.proc SetBoundingBox

    mb Enemy_BoundBoxCtrl[ x ] := a               ; set bounding box control here
    mb Enemy_MovingDir   [ x ] := #2              ; set moving direction for left
    ; continue below:
.endproc
.proc InitSpeedAndMoveForce0

    lda #0                                        ; initialize vertical speed
    sta Enemy_Y_Speed,x                           ; and movement force
    sta Enemy_Y_MoveForce,x
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitBulletBill

    mb Enemy_MovingDir   [ x ] := #2              ; set moving direction for left
    mb Enemy_BoundBoxCtrl[ x ] := #9              ; set bounding box control for $09
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitCheepCheep

    jsr SmallBBox                                 ; set vertical bounding box, speed, init others

    mb CheepCheepMoveMFlag  [ x ] := PseudoRandomBitReg [ x ] & #%00010000    ; save as random movement flag of some sort
    mb CheepCheepOrigYPos   [ x ] := Enemy_Y_Position   [ x ]          ; save original vertical coordinate here

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitLakitu

    .export SetupLakitu

    if ! EnemyFrenzyBuffer                        ; check to see if an enemy is already in the frenzy buffer

        SetupLakitu:

        mb LakituReappearTimer := #0              ; erase counter for lakitu's reappearance
        jsr InitHorizFlySwimEnemy                 ; set $03 as bounding box, set other attributes
        jmp TallBBox2                             ; OP: set $03 as bounding box again (not necessary) and leave

    endif
    ; else:

    jmp EraseEnemyObject                          ; kill lakitu if EnemyFrenzyBuffer active

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

PRDiffAdjustData:
    .byte $26, $2c, $32, $38
    .byte $20, $22, $24, $26
    .byte $13, $14, $15, $16

.code

.proc LakituAndSpinyHandler

    if !FrenzyEnemyTimer && x < #5                ; if timer here expired AND x < 5

        mb FrenzyEnemyTimer := #$80               ; set timer

        ldy #4                                    ; start with the last enemy slot
        repeat                                    ; y = 4 to 0
            if Enemy_ID [ y ] = #OBJECTID_Lakitu goto CreateSpiny               ; check all enemy slots to see
        until dey == negative

        inc LakituReappearTimer                   ; increment reappearance timer

        if LakituReappearTimer >= #7

            ldx #4                                ; start with the last enemy slot again

            repeat
                if !Enemy_Flag[ x ] goto CreateLakitu                  ; check enemy buffer flag for non-active enemy slot
            until dex == negative                 ; branch until all slots are checked
            bmi Skip                              ; if no empty slots were found, unconditinal branch

                CreateLakitu:

                mb Enemy_State [ x ] := #0
                mb Enemy_ID    [ x ] := #OBJECTID_Lakitu   ; create lakitu enemy object
                jsr SetupLakitu                   ; do a sub to set up lakitu

                lda #$20
                jsr PutAtRightExtent              ; finish setting up lakitu

            Skip:

            ldx ObjectOffset                      ; get enemy object buffer offset again and leave
        endif
    endif

    Exit:
    rts
    
    ; ------------------------------------------------------------------------------------------------

    CreateSpiny:
    
    ; temp_byte 1 to 3 - used to hold pseudorandom difference adjusters
    
    ; if player above a certain point OR if lakitu is not in normal state, branch to leave
    if Player_Y_Position < #$2c || Enemy_State[ y ] != zero goto Exit
    ; store horizontal coordinates (high and low) of lakitu
    ; into the coordinates of the spiny we're going to create

    mb Enemy_PageLoc   [ x ] := Enemy_PageLoc[ y ]
    mb Enemy_X_Position[ x ] := Enemy_X_Position[ y ]
    mb Enemy_Y_HighPos [ x ] := #1                                     ; put spiny within vertical screen unit
    mb Enemy_Y_Position[ x ] := Enemy_Y_Position[ y ] - #8             ; put spiny eight pixels above where lakitu is

    mb y := PseudoRandomBitReg[ x ] & #%00000011                       ; get 2 LSB of LSFR and save to Y
    mb x := #2

    repeat                                        ; x = 2 to 0, y = rnd * 3, rnd + 4, rnd + 8

        mb temp_byte[ 1 + x ] := PRDiffAdjustData[ y ]
        mb y := y + 4

    until dex == negative                         ; loop until all three are written

    ldx ObjectOffset                              ; get enemy object buffer offset
    jsr PlayerLakituDiff                          ; move enemy, change direction, get value - difference

    if ldy Player_X_Speed < #$08                  ; check player's horizontal speed
        tay                                       ; otherwise save value in A to Y for now
            if PseudoRandomBitReg[ 1 + x ] & #%00000011
                mb a := y
                mb y := a ^ #$FF                  ; get two's compliment of Y
                iny
            endif
        tya                                       ; OP put value from A in Y back to A (they will be lost anyway)
    endif

    jsr SmallBBox                                 ; set bounding box control, init attributes, lose contents of A

    ldy #2
    ; set horizontal speed to zero because previous contents of A were lost...branch here will never be taken for
    ; the same reason

    mb Enemy_X_Speed[ x ] := a ; #0

    if cmp #0 == positive
        dey
    endif

    mb Enemy_MovingDir[ x ] := y ; #1             ; set moving direction to the right always ( bug )
    mb Enemy_Y_Speed  [ x ] := #$fd               ; set vertical speed to move upwards
    mb Enemy_Flag     [ x ] := #1                 ; enable enemy object by setting flag
    mb Enemy_State    [ x ] := #$05               ; put spiny in egg state and leave

    Exit2:

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

FirebarSpinSpdData:
    .byte $28, $38, $28, $38, $28

FirebarSpinDirData:
    .byte $00, $00, $10, $10, $00

.code

.proc InitLongFirebar
    jsr DuplicateEnemyObj                         ; create enemy object for long firebar
.endproc

.proc InitShortFirebar

    mb FirebarSpinState_Low[ x ] := #0            ; initialize low byte of spin state
    mb y := Enemy_ID[ x ] - #$1b                  ; subtract $1b from enemy identifier to get proper offset for firebar data

    mb FirebarSpinSpeed    [ x ] := FirebarSpinSpdData[ y ]            ; get spinning speed of firebar
    mb FirebarSpinDirection[ x ] := FirebarSpinDirData[ y ]            ; get spinning direction of firebar
    mb Enemy_Y_Position    [ x ] := Enemy_Y_Position  [ x ] + #4       ; add four pixels to vertical coordinate
    mb Enemy_X_Position    [ x ] := Enemy_X_Position  [ x ] + #4       ; add four pixels to horizontal coordinate
    mb Enemy_PageLoc       [ x ] := Enemy_PageLoc     [ x ] + C      ; add carry to page location

    jmp TallBBox2                                 ; set bounding box control (not used) and leave

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

FlyCCXPositionData:
    .byte $80, $30, $40, $80
    .byte $30, $50, $50, $70
    .byte $20, $40, $80, $a0
    .byte $70, $40, $90, $68

FlyCCXSpeedData:
    .byte $0e, $05, $06, $0e
    .byte $1c, $20, $10, $0c
    .byte $1e, $22, $18, $14

FlyCCTimerData:
    .byte $10, $60, $20, $48

.code

.proc InitFlyingCheepCheep

    ; locals
        Random1 = temp_byte
        Random2 = temp_byte + 1
    ; end locals

    if FrenzyEnemyTimer goto LakituAndSpinyHandler::Exit2              ; rts

    jsr SmallBBox                                 ; jump to set bounding box size $09 and init other values

    mb y := PseudoRandomBitReg[ 1 + x ] & #%00000011                   ; set pseudorandom offset here from 0 to 3
    mb FrenzyEnemyTimer := FlyCCTimerData[ y ]    ; load timer with pseudorandom offset

    ldy #3                                        ; load Y with default value
    if SecondaryHardMode                          ; if set increment Y to allow as many as four onscreen
        iny
    endif

    sty Random1                                   ; store 3 or 4 here depending on SecondaryHardMode

    if x >= Random1 goto LakituAndSpinyHandler::Exit2                  ; if x > y, branch to rts

    mb Random1 := PseudoRandomBitReg[ x ] & #%00000011
    mb Random2 := a
    mb Enemy_Y_Speed[ x ] := #$fb                 ; set vertical speed for cheep-cheep

    lda #0                                        ; load default value
    if ldy Player_X_Speed != zero
        lda #4
        if y >= #$19                              ; if moving to the right quickly
            asl                                   ; a = #8
        endif
    endif

    pha                                           ; a = either #0, #4, or #8
        mb Random1 := a + Random1                 ; add rnd value from earlier and save

        if PseudoRandomBitReg[ 1 + x ] & #%00000011                    ; there is a 3/4 chance
            mb Random1 := PseudoRandomBitReg[ 2 + x ] & #%00001111     ; we override with a new random value from 0 to 15
        endif
    pla                                           ; get value from stack we saved earlier

    mb y := a + Random2
    mb Enemy_X_Speed[ x ] := FlyCCXSpeedData[ y ]                      ; get horizontal speed using pseudorandom offset
    mb Enemy_MovingDir[ x ] := #1                 ; set to move towards the right

    if !Player_X_Speed                            ; if pl ayer not moving left or right

        ldy Random1                               ; get first LSFR or third LSFR lower nybble
        tya

        if a & #%00000010
            mb Enemy_X_Speed[ x ] := Enemy_X_Speed[ x ] ^ #$ff + #1    ; change speed direction
            inc Enemy_MovingDir,x                 ; increment to move towards the left
        endif
    endif

    tya                                           ; get first LSFR or third LSFR lower nybble again

    if a & #%00000010
        mb Enemy_X_Position[ x ] := Player_X_Position + FlyCCXPositionData[ y ]
        mb a := Player_PageLoc + C              ; add carry
    else
        mb Enemy_X_Position[ x ] := Player_X_Position - FlyCCXPositionData[ y ]
        mb a := Player_PageLoc - C              ; sub carry
    endif

    mb Enemy_PageLoc[ x ] := a                    ; save as enemy's page location


    mb a := #1
    mb Enemy_Flag      [ x ] := a                 ; set enemy's buffer flag
    mb Enemy_Y_HighPos [ x ] := a                 ; set enemy's high vertical byte
    mb Enemy_Y_Position[ x ] := #$f8              ; put enemy below the screen, and we are done

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------
    

.proc InitBowser

    jsr DuplicateEnemyObj                         ; jump to create another bowser object
    stx BowserFront_Offset                        ; save offset of first here
    mb a, BowserBodyControls    := #0             ; initialize bowser's body controls
    mb BridgeCollapseOffset     := a              ; and bridge collapse offset
    mb BowserOrigXPos           := Enemy_X_Position[ x ]               ; store original horizontal position here
    mb a, BowserFireBreathTimer := #$df           ; store something here
    mb Enemy_MovingDir[ x ]     := a              ; and in moving direction
    mb BowserFeetCounter        := #$20           ; set bowser's feet timer and in enemy timer
    mb EnemyFrameTimer[ x ]     := a
    mb BowserHitPoints          := #$05           ; give bowser 5 hit points

    lsr                                           ; a = 2
    sta BowserMovementSpeed                       ; set default movement speed here
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DuplicateEnemyObj

    ldy #$ff                                      ; start at beginning of enemy slots
    do
        iny                                       ; increment one slot
    while Enemy_Flag[ y ]                         ; check enemy buffer flag for empty slot

    sty DuplicateObj_Offset                       ; otherwise set offset here
    txa                                           ; transfer original enemy buffer offset
    mb Enemy_Flag       [ y ] := a | #%10000000   ; store with d7 set
    mb Enemy_PageLoc    [ y ] := Enemy_PageLoc   [ x ]                 ; copy page location and horizontal coordinates
    mb Enemy_X_Position [ y ] := Enemy_X_Position[ x ]                 ; from original enemy to new enemy
    mb Enemy_Flag       [ x ] := #1               ; set flag as normal for original enemy
    mb Enemy_Y_HighPos  [ y ] := a                ; #1     ; set high vertical byte for new enemy
    mb Enemy_Y_Position [ y ] := Enemy_Y_Position[ x ]                 ; copy vertical coordinate from original to new

    Exit:
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

FlameYPosData:
    .byte $90, $80, $70, $90

FlameYMFAdderData:
    .byte $ff, $01

.code

.proc InitBowserFlame

    .export PutAtRightExtent

    if FrenzyEnemyTimer goto DuplicateEnemyObj::Exit                   ; if timer not expired yet, branch to leave

    mb Enemy_Y_MoveForce[ x ] := a                ; #0                ; reset something here
    mb NoiseSoundQueue := NoiseSoundQueue | #SFX_BowserFlame           ; load bowser's flame sound into queue

    ldy BowserFront_Offset                        ; get bowser's buffer offset
    if Enemy_ID[ y ] <> #OBJECTID_Bowser                   ; check for bowser

        jsr SetFlameTimer                         ; get timer data based on flame counter

        mb a := a + #32                           ; add 32 frames by default
        if ldy SecondaryHardMode
            mb a := a - #16                       ; subtract 16 frames for secondary hard mode
        endif

        sta FrenzyEnemyTimer                      ; set timer accordingly
        mb BowserFlamePRandomOfs[ x ] := PseudoRandomBitReg[ x ] & #%00000011                                 ; rnd 0 to 3

        tay
        lda FlameYPosData,y                       ; load vertical position based on pseudorandom offset

        PutAtRightExtent:

        mb Enemy_Y_Position [ x ] := a            ; set vertical position
        mb Enemy_X_Position [ x ] := ScreenRight_X_Pos + #$20          ; place enemy 32 pixels beyond right side of screen
        mb Enemy_PageLoc    [ x ] := ScreenRight_PageLoc + C         ; add carry

    else                                          ; enemy is bowser:

        mb Enemy_X_Position [ x ] := Enemy_X_Position[ y ] - #$0e      ; subtract 14 pixels save as flame's horizontal position
        mb Enemy_PageLoc    [ x ] := Enemy_PageLoc[ y ]                ; copy page location from bowser to flame
        mb Enemy_Y_Position [ x ] := Enemy_Y_Position[ y ] + #8        ; save as flame's vertical position
        mb Enemy_YMoveForceFractional  [ x ] := PseudoRandomBitReg[ x ] & #%00000011                               ; rnd: 0 to 3

        tay                                       ; use as offset
        lda FlameYPosData,y                       ; get value here using bits as offset

        ldy #0                                    ; load default offset
        if a >= Enemy_Y_Position[ x ]             ; compare value to flame's current vertical position
            iny
        endif

        mb Enemy_Y_MoveForce[ x ] := FlameYMFAdderData[ y ]
        mb EnemyFrenzyBuffer := #0                ; clear enemy frenzy buffer

    endif

    mb Enemy_BoundBoxCtrl[ x ] := #8

    lda #1                                        ; set high byte of vertical and
    mb Enemy_Y_HighPos  [ x ] := a                ; enemy buffer flag
    mb Enemy_Flag       [ x ] := a
    lsr                                           ; a = #0
    mb Enemy_X_MoveForce[ x ] := a                ; initialize horizontal movement force, and
    mb Enemy_State      [ x ] := a                ; enemy state

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

FireworksXPosData:
    .byte $00, $30, $60, $60, $00, $20

FireworksYPosData:
    .byte $60, $40, $70, $40, $60, $30

.code

.proc InitFireworks

    if !FrenzyEnemyTimer

        mb FrenzyEnemyTimer := #$20               ; otherwise reset timer
        dec FireworksCounter                      ; decrement for each explosion

        ldy #$06                                  ; start at last slot, find star flag object slot
        repeat
            dey
        until Enemy_ID[ y ] = #OBJECTID_StarFlagObject

        mb a := Enemy_X_Position[ y ] - #$30      ; subtract 48 pixels from star flag and save to stack

        pha                                       ; the stack
            mb temp_byte := Enemy_PageLoc[ y ] - C       ; subtract the carry from the page location
            mb y := FireworksCounter + Enemy_State[ y ]    ; add state of star flag object (possibly not necessary) and use as offset
        pla                                       ; get saved horizontal coordinate of star flag - 48 pixels

        mb Enemy_X_Position     [ x ] := a + FireworksXPosData[ y ]    ; store as the fireworks object horizontal coordinate
        mb Enemy_PageLoc        [ x ] := temp_byte + C               ; add carry and store as page location for the fireworks object
        mb Enemy_Y_Position     [ x ] := FireworksYPosData[ y ]
        lda #1
        mb Enemy_Y_HighPos      [ x ] := a        ; store in vertical high byte
        mb Enemy_Flag           [ x ] := a        ; and activate enemy buffer flag
        lsr                                       ; a:= #0
        mb ExplosionGfxCounter  [ x ] := a        ; initialize explosion counter
        mb ExplosionTimerCounter[ x ] := #$08     ; set explosion timing counter

    endif

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

Bitmasks:
    .byte %00000001, %00000010, %00000100, %00001000, %00010000, %00100000, %01000000, %10000000

BulletOrCheepCheepYPosData:
    .byte $40, $30, $90, $50, $20, $60, $a0, $70

SwimmingCheepCheepID:
    .byte OBJECTID_GreyCheepCheep, OBJECTID_RedCheepCheep

.code

.proc BulletBillCheepCheep

    if ! FrenzyEnemyTimer

        if AreaType == zero                       ; are we in a water-type level?

            if x >= #3 goto Exit
            ; complex method of randomly spawning a GreyCheepCheep ($0a) or RedCheepCheep ($0b) :

            ldy #0                                ; load default offset
            if PseudoRandomBitReg[ x ] >= #$aa
                iny                               ; otherwise increment
            endif

            if WorldNumber <> #WORLD2
                iny
            endif

            tya
            mb y := a & #%00000001                ; mask out all but last bit of offset
            mb a := SwimmingCheepCheepID[ y ]     ; load identifier for cheep-cheeps
            
            ; ------------------------------------------------------------------------------------------------

            SetBulletBill_CheepCheep_ID:

            mb Enemy_ID[ x ] := a                 ; store either a CheepCheep (red or grey) or bulletBill

            if BitMFilter = #$ff
                mb BitMFilter := #0               ; initialize vertical position filter
            endif

            mb a := PseudoRandomBitReg[ x ] & #%00000111               ; rnd 0 to 7

            while tay : lda Bitmasks[ y ] : bit BitMFilter != zero do
                mb a := y + 1
                mb a := a & #%00000111            ; mask out all but 3 LSB thus keeping it 0-7
            endwhile

            mb BitMFilter := a | BitMFilter       ; add bit to already set bits in filter

            lda BulletOrCheepCheepYPosData,y      ; load vertical position using offset
            jsr PutAtRightExtent                  ; set vertical position and other values
            sta Enemy_YMoveForceFractional,x                 ; initialize fractional

            mb FrenzyEnemyTimer := #$20           ; set timer
            jmp CheckpointEnemyID                 ; process our new enemy object

        endif
    ; else: not in a water level:
    ; loop until a BulletBill frenzy is found, if not found and y = 5, create one:

        ldy #$ff                                  ; start at beginning of enemy slots
        repeat
            iny                                   ; move onto the next slot
            if y >= #$05 goto FireBulletBill      ; branch to play sound if we've done all slots
        until Enemy_Flag[ y ] && Enemy_ID[ y ] = #OBJECTID_BulletBill_FrenzyVar  
        ; until flag is set AND it is a BulletBill
    endif

    Exit:

    rts                                           ; if found, leave

    FireBulletBill:

    mb Square2SoundQueue := Square2SoundQueue | #SFX_Blast
    lda #OBJECTID_BulletBill_FrenzyVar                     ; load identifier for bullet bill object
    bne SetBulletBill_CheepCheep_ID               ; unconditional branch

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc HandleGroupEnemies

    GroupYPos           = temp_byte               ; - used to store Y position of group enemies
    GroupID             = temp_byte + 1           ; - used to store enemy ID
    GroupRightSidePage  = temp_byte + 2           ; - used to store page location of right side of screen
    GroupXPos           = temp_byte + 3           ; - used to store X position of right side of screen
    ; from CheckRightBounds:
    ; if a >= #$37 && a < #$3f goto DoGroup           ; Dogroup jumps here:

    ldy #OBJECTID_GreenKoopa                               ; load value for green koopa troopa
    ; subtract $37 from second byte read, reg a will now be 0 to 7
    mb a := a - #$37

    pha                                           ; save result in stack for now

        if a < #4
            pha                                   ; save another copy to stack
                ldy #OBJECTID_Goomba                       ; load value for goomba enemy
                if PrimaryHardMode                ; OP: eliminate stack usage in this block and use reg x to test PrimaryHardMode
                    ldy #OBJECTID_BuzzyBeetle              ; or for buzzy beetle
                endif
            pla                                   ; get second copy from stack
        endif

        mb GroupID := y                           ; store Green Koopa, or OBJECTID_Goomba, or OBJECTID_BuzzyBeetle

        ldy #$b0                                  ; load default y coordinate
        if a & #%00000010                         ; check to see if d1 was set
            ldy #$70                              ; load alt y coordinate
        endif

        sty GroupYPos                             ; save y coordinate here
        mb GroupRightSidePage := ScreenRight_PageLoc                   ; get page number of right edge of screen
        mb GroupXPos := ScreenRight_X_Pos         ; get pixel coordinate of right edge

        ldy #2                                    ; load two enemies by default
    pla                                           ; get first copy from stack

    if a >> 1 == C set
        iny                                       ; increment to three enemies
    endif

    sty NumberofGroupEnemies

    repeat

        ldx #$ff                                  ; start at beginning of enemy buffers
        repeat
            inx                                   ; increment and bail if no slots
            if x >= #$05 goto Exit
        until ! Enemy_Flag[ x ]

        mb Enemy_ID         [ x ] := GroupID
        mb Enemy_PageLoc    [ x ] := GroupRightSidePage
        mb Enemy_X_Position [ x ] := GroupXPos

        mb GroupXPos          := a + #24          ; add 24 pixels for next enemy
        mb GroupRightSidePage := GroupRightSidePage + C              ; add carry for page

        mb Enemy_Y_Position [ x ] := GroupYPos
        mb Enemy_Y_HighPos  [ x ] := #1           ; put enemy within the screen vertically
        mb Enemy_Flag       [ x ] := a            ; #1

        jsr CheckpointEnemyID                     ; process each enemy object separately

    until dec NumberofGroupEnemies == zero        ; do this until we run out of enemy objects

    Exit:

    jmp IncrementEDataOffsetX2                    ; jump to increment data offset and leave

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitPiranhaPlant

    mb PiranhaPlant_Y_Speed [ x ] := #1
    lsr                                           ; a := #0
    mb Enemy_State          [ x ] := a            ; #0                   ; initialize enemy state and what would normally
    mb PiranhaPlant_MoveFlag[ x ] := a            ; be used as vertical speed, but not in this case
    mb PiranhaPlantDownYPos [ x ] := Enemy_Y_Position[ x ]             ; save original vertical coordinate here
    mb PiranhaPlantUpYPos   [ x ] := a - #24      ; save original vertical coordinate - 24 pixels here

    lda #$09
    jmp SetBoundingBox2                           ; set specific value for bounding box control

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitEnemyFrenzy

    mb EnemyFrenzyBuffer := Enemy_ID[ x ]

    mb a := a - #$12                              ; adjust value for jumptable
    jsr JumpEngine
    ; frenzy object jump table

    .word LakituAndSpinyHandler
    .word NoFrenzyCode
    .word InitFlyingCheepCheep
    .word InitBowserFlame
    .word InitFireworks
    .word BulletBillCheepCheep

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc NoFrenzyCode
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc EndFrenzy

    ldy #$05                                      ; start at last slot
    repeat
        if Enemy_ID[ y ] = #OBJECTID_Lakitu
            mb Enemy_State[ y ] := #1
        endif
    until dey == negative

    mb EnemyFrenzyBuffer := #0                    ; empty enemy frenzy buffer
    mb Enemy_Flag[ x ]   := a                     ; #0            ; disable enemy buffer flag for this object
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitJumpGPTroopa

    .export TallBBox2, SetBoundingBox2

    mb Enemy_MovingDir [ x ] := #2                ; set for movement to the left
    mb Enemy_X_Speed   [ x ] := #$f8              ; set horizontal speed

    TallBBox2:

    lda #3                                        ; set specific value for bounding box control

    SetBoundingBox2:

    mb Enemy_BoundBoxCtrl[ x ] := a               ; set bounding box control then leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitBalPlatform

    dec Enemy_Y_Position, x                       ; raise vertical position by two pixels
    dec Enemy_Y_Position, x

    if ! ldy SecondaryHardMode                    ; if secondary hard mode flag not set,
        ldy #2                                    ; set value here
        jsr PosPlatform                           ; do a sub to add or subtract pixels
    endif

    ldy #$ff                                      ; set default value here for now

    mb Enemy_State[ x ] := BalPlatformAlignment   ; set platform alignment to object state here

    if negative                                   ; if old alignment $ff, put $ff as alignment for negative
    ; y := x
        txa                                       ; if old contents already $ff, put
        tay                                       ; object offset as alignment to make next positive
    endif

    sty BalPlatformAlignment                      ; store whatever value's in Y here

    mb a, Enemy_MovingDir[ x ] := #0              ; init moving direction

    tay                                           ; y := #0                           ; init Y
    jsr PosPlatform                               ; do a sub to add 8 pixels, then run shared code here

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitDropPlatform

    mb PlatformCollisionFlag[ x ] := #$ff
    jmp CommonPlatformCode

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitHoriPlatform

    mb XMoveSecondaryCounter[ x ] := #0           ; init one of the moving counters
    jmp CommonPlatformCode                        ; jump ahead to execute more code

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc InitVertPlatform

    ldy #$40                                      ; set default value here

    if Enemy_Y_Position[ x ] == negative          ; check vertical position
        mb a := a ^ #$ff + #1                     ; two's compliment
        ldy #$c0                                  ; get alternate value to add to vertical position
    endif

    sta YPlatformTopYPos,x                        ; save as top vertical position
    tya
    mb YPlatformCenterYPos[ x ] := a + Enemy_Y_Position[ x ]           ; save result as central vertical position

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc CommonPlatformCode

    .export SPBBox

    jsr InitSpeedAndMoveForce0                    ; do a sub to init certain other values

    SPBBox:

    lda #$05                                      ; set default bounding box size control
    ; check NOT castle-type level AND NOT SecondaryHardmode
    if ldy AreaType <> #3 && ! ldy SecondaryHardMode
        lda #$06
    endif

    sta Enemy_BoundBoxCtrl,x                      ; set bounding box size control here and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc LargeLiftUp
    jsr PlatLiftUp                                ; execute code for platforms going up
    jmp LargeLiftBBox                             ; overwrite bounding box for large platforms
.endproc

.proc LargeLiftDown
    jsr PlatLiftDown                              ; execute code for platforms going down
.endproc

.proc LargeLiftBBox
    jmp SPBBox                                    ; jump to overwrite bounding box size control
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PlatLiftUp

    mb Enemy_Y_MoveForce[ x ] := #$10             ; set movement amount here
    mb Enemy_Y_Speed    [ x ] := #$ff             ; set moving speed for platforms going up
    jmp CommonSmallLift                           ; skip ahead to part we should be executing

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PlatLiftDown

    mb Enemy_Y_MoveForce[ x ] := #$f0             ; set movement amount here
    mb Enemy_Y_Speed    [ x ] := #0               ; set moving speed for platforms going down

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc CommonSmallLift

    ldy #1
    jsr PosPlatform                               ; do a sub to add 12 pixels due to preset value

    mb Enemy_BoundBoxCtrl[ x ] := #4              ; set bounding box control for small platforms
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

PlatPosDataLow:
    .byte $08,$0c,$f8

PlatPosDataHigh:
    .byte $00,$00,$ff

.code

.proc PosPlatform

    mb Enemy_X_Position[ x ] := Enemy_X_Position[ x ] + PlatPosDataLow[ y ]  
    mb Enemy_PageLoc   [ x ] := Enemy_PageLoc[ x ] +c PlatPosDataHigh[ y ]   
    rts                                           ; and go back

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc EndOfEnemyInitCode
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RunEnemyObjectsCore

    ldx ObjectOffset                              ; get offset for enemy object buffer

    lda #0                                        ; load value 0 for jump engine by default

    if ldy Enemy_ID[ x ] >= #$15
    ; carry is set
        tya                                       ; subtract $14 from the value and use
        mb a := a -c #$14                         ; as value for jump engine
    endif

    jsr JumpEngine

    .word RunNormalEnemies                        ; for objects temp_byte-$14

    .word RunBowserFlame                          ; for objects $15-$1f
    .word RunFireworks
    .word NoRunCode
    .word NoRunCode
    .word NoRunCode
    .word NoRunCode
    .word RunFirebarObj
    .word RunFirebarObj
    .word RunFirebarObj
    .word RunFirebarObj
    .word RunFirebarObj

    .word RunFirebarObj                           ; for objects $20-$2f
    .word RunFirebarObj
    .word RunFirebarObj
    .word NoRunCode
    .word RunLargePlatform
    .word RunLargePlatform
    .word RunLargePlatform
    .word RunLargePlatform
    .word RunLargePlatform
    .word RunLargePlatform
    .word RunLargePlatform
    .word RunSmallPlatform
    .word RunSmallPlatform
    .word RunBowser
    .word PowerUpObjHandler
    .word VineObjectHandler

    .word NoRunCode                               ; for objects $30-$35
    .word RunStarFlagObj
    .word JumpspringHandler
    .word NoRunCode
    .word WarpZoneObject
    .word RunRetainerObj

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc NoRunCode
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RunRetainerObj
    jsr GetEnemyOffscreenBits
    jsr RelativeEnemyPosition
    jmp EnemyGfxHandler
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RunNormalEnemies

    lda #0                                        ; init sprite attributes
    sta Enemy_SprAttrib,x

    jsr GetEnemyOffscreenBits
    jsr RelativeEnemyPosition
    jsr EnemyGfxHandler
    jsr GetEnemyBoundBox
    jsr EnemyToBGCollisionDet
    jsr EnemiesCollision
    jsr PlayerEnemyCollision

    if ! ldy TimerControl                         ; if master timer control not set
        jsr EnemyMovementSubs
    endif

    jmp OffscreenBoundsCheck

    EnemyMovementSubs:

    lda Enemy_ID,x
    jsr JumpEngine

    .word MoveNormalEnemy                         ; only objects $0-$14 use this table
    .word MoveNormalEnemy
    .word MoveNormalEnemy
    .word MoveNormalEnemy
    .word MoveNormalEnemy
    .word ProcHammerBro
    .word MoveNormalEnemy
    .word MoveBloober
    .word MoveBulletBill
    .word NoMoveCode
    .word MoveSwimmingCheepCheep
    .word MoveSwimmingCheepCheep
    .word MovePodoboo
    .word MovePiranhaPlant
    .word MoveJumpingEnemy
    .word ProcMoveRedPTroopa
    .word MoveFlyGreenPTroopa
    .word MoveLakitu
    .word MoveNormalEnemy
    .word NoMoveCode                              ; dummy
    .word MoveFlyingCheepCheep

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc NoMoveCode
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RunBowserFlame

    jsr ProcBowserFlame
    jsr GetEnemyOffscreenBits
    jsr RelativeEnemyPosition
    jsr GetEnemyBoundBox
    jsr PlayerEnemyCollision
    jmp OffscreenBoundsCheck

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RunFirebarObj
    jsr ProcFirebar
    jmp OffscreenBoundsCheck
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RunSmallPlatform

    jsr GetEnemyOffscreenBits
    jsr RelativeEnemyPosition
    jsr SmallPlatformBoundBox
    jsr SmallPlatformCollision
    jsr RelativeEnemyPosition
    jsr DrawSmallPlatform
    jsr MoveSmallPlatform
    jmp OffscreenBoundsCheck

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RunLargePlatform

    jsr GetEnemyOffscreenBits
    jsr RelativeEnemyPosition
    jsr LargePlatformBoundBox
    jsr LargePlatformCollision

    if ! TimerControl
        jsr LargePlatformSubroutines
    endif

    jsr RelativeEnemyPosition
    jsr DrawLargePlatform
    jmp OffscreenBoundsCheck

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc LargePlatformSubroutines

    mb a := Enemy_ID[ x ] - #$24                  ; subtract $24 to get proper offset for jump table

    jsr JumpEngine

    .word BalancePlatform                         ; table used by objects $24-$2a
    .word YMovingPlatform
    .word MoveLargeLiftPlat
    .word MoveLargeLiftPlat
    .word XMovingPlatform
    .word DropPlatform
    .word RightPlatform

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc EraseEnemyObject

    lda #0                                        ; clear all enemy object variables
    mb Enemy_Flag         [ x ] := a
    mb Enemy_ID           [ x ] := a
    mb Enemy_State        [ x ] := a
    mb FloateyNum::Control[ x ] := a
    mb EnemyIntervalTimer [ x ] := a
    mb ShellChainCounter  [ x ] := a
    mb Enemy_SprAttrib    [ x ] := a
    mb EnemyFrameTimer    [ x ] := a

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc MovePodoboo

    if ! EnemyIntervalTimer[ x ]                  ; check enemy timer

        jsr InitPodoboo                           ; otherwise set up podoboo again

        mb Enemy_Y_MoveForce [ x ] := PseudoRandomBitReg[ 1 + x ] | #%10000000
        mb EnemyIntervalTimer[ x ] := a & #%00001111 | #$06            ; set for at least six intervals
        mb Enemy_Y_Speed     [ x ] := #$f9        ; set vertical speed to move podoboo upwards

    endif

    jmp MoveJumpEnemyVertically                     ; branch to impose gravity on podoboo

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

HammerThrowTmrData:
    .byte $30, $1c

XSpeedAdderData:
    .byte $00, <-24, $00, 24

RevivedXSpeed:
    .byte 8, <-8, 12, <-12

.code

.proc ProcHammerBro


    set_long_branch +
    if Enemy_State[ x ] & #%00100000 goto MoveDefeatedEnemy            ; check hammer bro's enemy state for d5 set
    set_long_branch -

    if !HammerBroJumpTimer[ x ] goto HammerBroJumpCode                 ; if expired, branch to jump
    dec HammerBroJumpTimer,x                      ; otherwise decrement jump timer
    if Enemy_OffscreenBits & #%00001100 goto MoveHammerBroXDir         ; if hammer bro a little offscreen, skip to movement code

    if !HammerThrowingTimer[ x ]                  ; check hammer throwing timer
        ldy SecondaryHardMode                     ; get secondary hard mode flag as offset
        mb HammerThrowingTimer[ x ] := HammerThrowTmrData[ y ]         ; set as new timer
        if jsr SpawnHammerObj == carry set
            ; set d3 in enemy state for hammer throw
            mb Enemy_State[ x ] := Enemy_State[ x ] | #%00001000
            jmp MoveHammerBroXDir                 ; jump to move hammer bro OP: bne always to jmp
        endif
    endif

    dec HammerThrowingTimer,x                     ; decrement timer
    jmp MoveHammerBroXDir                         ; jump to move hammer bro
.endproc


dataseg

HammerBroJumpLData:
    .byte $20, $37

.code

.proc HammerBroJumpCode

    .export HammerBroJump
    ; temp_byte - used as bitmask

    if Enemy_State[ x ] & #%00000111 <> #1        ; get hammer bro's enemy state (jumping?)

        mb temp_byte := #0                        ; save into temp variable for now

        ldy #$fa                                  ; set default vertical speed
        if lda Enemy_Y_Position[ x ] == bit7 clear ; check hammer bro's vertical coordinate, if on the top half of the screen, then:
            ldy #$fd                              ; set alternate vertical speed
            cmp #$70                              ; check to see if hammer bro is above the middle of screen
            inc temp_byte                         ; increment preset value to $01
            if greaterORequal                     ; if above the middle of the screen, use current speed and $01
                dec temp_byte                     ; otherwise return value to temp_byte, get part of LSFR, mask out all but LSB
                if !PseudoRandomBitReg[ x + 1 ] & #1                   ; if d0 of LSFR set, branch and use current speed and temp_byte
                    ldy #$fa                      ; otherwise reset to default vertical speed
                endif
            endif
        endif

        HammerBroJump:

        mb Enemy_Y_Speed[ x ] := y                ; set vertical speed for jumping
        mb Enemy_State  [ x ] := Enemy_State[ x ] | #1                 ; set d0 in enemy state for jumping
        mb y := temp_byte & PseudoRandomBitReg[ x + 2 ]                ; use as offset

        if lda SecondaryHardMode == zero          ; check secondary hard mode flag
            tay                                   ; if secondary hard mode flag clear, set offset to 0
        endif

        mb EnemyFrameTimer[ x ] := HammerBroJumpLData[ y ]             ; get jump length timer data
        ; get RND, set d7 and d6, ; store in jump timer
        mb HammerBroJumpTimer[ x ] := PseudoRandomBitReg[ x + 1 ] | #%11000000

    endif

.endproc

.proc MoveHammerBroXDir

    ldy #<-4                                      ; move hammer bro a little to the left
    if ! FrameCounter & #%01000000                ; change hammer bro's direction every 64 frames
        ldy #4                                    ; if d6 NOT set in counter, move him a little to the right
    endif

    mb Enemy_X_Speed[ x ] := y                    ; store horizontal speed

    ldy #1                                        ; set to face right by default
    if jsr PlayerEnemyDiff == positive            ; if enemy to the right of player:
        iny                                       ; set to face left
        if !EnemyIntervalTimer[ x ]               ; check walking timer
            mb Enemy_X_Speed[ x ] := #(<-8)       ; make the hammer bro walk left towards player
        endif
    endif

    mb Enemy_MovingDir[ x ] := y                  ; set moving direction

.endproc

.proc MoveNormalEnemy

    ldy #0                                        ; init Y to leave horizontal movement as-is

    if !Enemy_State[ x ] & #%01000000             ; check enemy state for d6 not set

        if  Enemy_State[ x ] << 1 == C set goto SteadM                 ; if bit7 set
        if  Enemy_State[ x ] & #%00100000  goto MoveDefeatedEnemy      ; OP: use BIT and avoid reloads
        if !Enemy_State[ x ] & #%00000111  goto SteadM                 ; if enemy in normal state
    ; if enemy not in state used by spiny's egg AND >=3
        if a <> #$05 && a >= #3 goto ReviveStunned

    endif

    jsr MoveD_EnemyVertically                     ; do a sub here to move enemy downwards
    ldy #0

    if lda Enemy_State[ x ] <> #2                 ; check for enemy state $02
        if !a & #%01000000 || Enemy_ID[ x ] = #OBJECTID_PowerUpObject goto SteadM                                ; if d6 not set
    else Z clear
    ; Enemy_State[ x ] = #2:
        jmp MoveEnemyHorizontally
    endif

    ldy #1                                        ; increment Y to slow horizontal movement

    SteadM:

    lda Enemy_X_Speed,x                           ; get current horizontal speed
    pha                                           ; save to stack
        if negative                               ; if moving left:
            iny
            iny                                   ; increment Y to next data
        endif

        mb Enemy_X_Speed[ x ] := a + XSpeedAdderData[ y ]              ; CHECK: slow down speed and save as horizontal speed temporarily
        jsr MoveEnemyHorizontally                 ; then do a sub to move horizontally
    pla
    sta Enemy_X_Speed,x                           ; get old horizontal speed from stack and return to

    rts                                           ; original memory location, then leave

.endproc

.proc ReviveStunned

    if EnemyIntervalTimer[ x ] goto KillGoomba
    ; Timer expired:
    mb Enemy_State[ x ] := a                      ; #0          ; otherwise initialize enemy state to normal
    mb y := FrameCounter & #1                     ; get d0 of frame counter
    mb Enemy_MovingDir[ x ] := y + 1              ; store as pseudorandom movement direction

    dey                                           ; decrement for use as pointer

    if PrimaryHardMode                            ; check primary hard mode flag
        iny
        iny                                       ; increment 2 bytes to next data
    endif

    mb Enemy_X_Speed[ x ] := RevivedXSpeed[ y ]   ; load and store new horizontal speed

    rts

.endproc

.proc MoveDefeatedEnemy
    jsr MoveD_EnemyVertically                   ; execute sub to move defeated enemy downwards
    jmp MoveEnemyHorizontally                   ; now move defeated enemy horizontally
.endproc

.proc KillGoomba

    if a = #$0e && Enemy_ID[ x ] = #OBJECTID_Goomba        ; Enemy timer at 14 AND Enemy is goomba
        jsr EraseEnemyObject                      ; kill this goomba object
    endif

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc MoveJumpingEnemy
    jsr MoveJumpEnemyVertically                     ; do a sub to impose gravity on green paratroopa
    jmp MoveEnemyHorizontally                     ; jump to move enemy horizontally
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ProcMoveRedPTroopa

    if ! Enemy_Y_Speed[ x ] | Enemy_Y_MoveForce[ x ]                   ; if no speed or force

        mb Enemy_YMoveForceFractional[ x ] := a              ; #0
        
        ; if current => original, skip ahead
        if Enemy_Y_Position[ x ] >= RedPTroopaOrigXPos[ x ] goto MoveRedPTUpOrDown

        if !FrameCounter & #%00000111             ; get frame counter
            inc Enemy_Y_Position,x                ; increment red paratroopa's vertical position every 8 frames
        endif
        rts
    endif

    MoveRedPTUpOrDown:

    if Enemy_Y_Position[ x ] >= RedPTroopaCenterYPos[ x ]              ; check current vs. central vertical coordinate
        jmp MoveRedPTroopaUp                      ; move upwards
    endif
    jmp MoveRedPTroopaDown                        ; move downwards

.endproc

    ; ------------------------------------------------------------------------------------------------
   

.proc MoveFlyGreenPTroopa

    ; temp_byte - used to store adder for movement, also used as adder for platform
    ; $01 - used to store maximum value for secondary counter

    jsr XMoveCntr_GreenPTroopa                    ; do sub to increment primary and secondary counters
    jsr MoveWithXMCntrs                           ; do sub to move green paratroopa accordingly, and horizontally

    ldy #1                                        ; set Y to move green paratroopa down

    if !FrameCounter & #%00000011                 ; every 4 frames

        if ! FrameCounter & #%01000000            ; check if it should be reversed
            ldy #<-1                              ; and set Y to move green paratroopa up
        endif

        sty temp_byte                             ; store adder here
        ; add or sub v pos: give green paratroopa a wavy flight
        mb Enemy_Y_Position[ x ] := Enemy_Y_Position[ x ] + temp_byte   
    endif

    rts

.endproc

.proc XMoveCntr_GreenPTroopa

    .export XMoveCntr_Platform

    SecondaryCounterMAX = temp_byte + 1

    lda #$13                                      ; load preset maximum value for secondary counter

    XMoveCntr_Platform:

    sta SecondaryCounterMAX                       ; store value here

    if !FrameCounter & #%00000011                 ; every 4 frames

        ldy XMoveSecondaryCounter,x               ; get secondary counter

        if XMovePrimaryCounter[ x ] >> 1 == carry set goto DecSeXM     ; if d0 of primary counter set, branch
        if y = SecondaryCounterMAX goto IncPXM    ; compare secondary counter to preset maximum value
        inc XMoveSecondaryCounter,x               ; increment secondary counter and leave
    endif

    rts

    IncPXM:

        inc XMovePrimaryCounter,x                 ; increment primary counter and leave
        rts

        DecSeXM:

    if tya == zero goto IncPXM                    ; put secondary counter in A, if zero branch back

    dec XMoveSecondaryCounter,x                   ; otherwise decrement secondary counter and leave
    rts

.endproc

.proc MoveWithXMCntrs


    lda XMoveSecondaryCounter,x                   ; save secondary counter to stack
    pha

        ldy #1                                    ; set value here by default

        if !XMovePrimaryCounter[ x ] & #%00000010                      ; if d1 of primary counter is clear
            mb XMoveSecondaryCounter[ x ] := XMoveSecondaryCounter[ x ] ^ #$ff + #1     
            ldy #2                                ; load alternate value here
        endif

        sty Enemy_MovingDir,x                     ; store as moving direction
        jsr MoveEnemyHorizontally
        sta temp_byte                             ; save value obtained from sub here

    pla                                           ; get secondary counter from stack
    sta XMoveSecondaryCounter,x                   ; and return to original place
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

BlooberBitmasks:
    .byte %00111111, %00000011

.code

.proc MoveBloober

    if !Enemy_State[ x ] & #%00100000             ; check enemy state for d5 clear = blooper alive

        ldy SecondaryHardMode                     ; use secondary hard mode flag as offset

        if !PseudoRandomBitReg[ 1 + x ] & BlooberBitmasks[ y ]

            txa

            if a >> 1 == carry set                ; check to see if on second or fourth slot (1 or 3)
                ldy Player_MovingDir              ; load player's moving direction
            else C set

                ldy #2                            ; set left moving direction by default
                if jsr PlayerEnemyDiff == negative                     ; get horizontal difference between player and bloober
                    dey                           ; if enemy to the left of player, set right moving direction
                endif

            endif

            sty Enemy_MovingDir,x                 ; set moving direction of bloober, then continue on here

        endif


        jsr ProcSwimmingB                         ; execute sub to make bloober swim characteristically

        mb a := Enemy_Y_Position[ x ] - Enemy_Y_MoveForce[ x ]
        if a >= #$20                              ; check to see if position is above edge of status bar
            mb Enemy_Y_Position[ x ] := a         ; otherwise, set new vertical position, make bloober swim
        endif
    ; if Enemy_MovingDir[ x ] = 1
        if y := Enemy_MovingDir[ x ] - 1 == zero                       ; check moving direction

            mb Enemy_X_Position[ x ] := Enemy_X_Position[ x ] + BlooperMoveSpeed[ x ]   
            mb Enemy_PageLoc[ x ] := Enemy_PageLoc[ x ] + C          ; store as new page location and leave
            rts

        endif

        mb Enemy_X_Position[ x ] := Enemy_X_Position[ x ] - BlooperMoveSpeed[ x ]
        mb Enemy_PageLoc[ x ] := Enemy_PageLoc[ x ] - C
        rts

    endif
    ; else: dead bloober
    jmp MoveEnemySlowVert                         ; jump to move defeated bloober downwards

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ProcSwimmingB

    if !BlooperMoveCounter[ x ] & #%00000010      ; get enemy's movement counter

        mb a := FrameCounter & #%00000111         ; get 3 LSB of frame counter
        pha                                       ; and save it to the stack
            if BlooperMoveCounter[ x ] >> 1 == carry set goto SlowSwim
        pla                                       ; pull 3 LSB of frame counter from the stack

        if zero                                   ; if !FrameCounter & #%00000111 ; every 8 frames:
             ; add to movement force to speed up swim
            mb a, Enemy_Y_MoveForce[ x ] := Enemy_Y_MoveForce[ x ] + #1                                
            mb BlooperMoveSpeed[ x ] := a         ; set as movement speed

            if a = #2
                inc BlooperMoveCounter,x          ; at certian value increment movement counter
            endif
        endif

        rts

        SlowSwim:

        pla                                       ; pull 3 LSB of frame counter from the stack

        if zero ; if !FrameCounter & #%00000111 ; every 8 frames:
            mb Enemy_Y_MoveForce[ x ] := Enemy_Y_MoveForce[ x ] - #1   ; subtract from movement force to slow swim

            if !sta BlooperMoveSpeed[ x ]
                inc BlooperMoveCounter,x          ; increment movement counter if no speed
                mb EnemyIntervalTimer[ x ] := #2                       ; set enemy's timer
            endif
        endif
        rts

    endif
    ; else:
    ; BlooperMoveCounter[ x ] & #%00000010 ; bit is set

    if EnemyIntervalTimer[ x ]                    ; get enemy timer

        Floatdown:

        if FrameCounter >> 1 == carry clear       ; if d0 clear - every other frame
            inc Enemy_Y_Position,x                ; otherwise increment vertical coordinate (down)
        endif

        rts

    endif
    ; compare Y pos with player's vertical coordinate, if player below enemy
    if Enemy_Y_Position[ x ] +c #16 < Player_Y_Position goto Floatdown

    mb BlooperMoveCounter[ x ] := #0              ; otherwise nullify movement counter
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc MoveBulletBill


    set_long_branch +
    ; if set, jump to move defeated bullet bill downwards
    if Enemy_State[ x ] & #%00100000 goto MoveJumpEnemyVertically

    set_long_branch -

    mb Enemy_X_Speed[ x ] := # <(-24)             ; set bullet bill's horizontal speed
    jmp MoveEnemyHorizontally
    ; note: this bullet bill
    ; object occurs in frenzy object $17, not from cannons

.endproc

    ; ------------------------------------------------------------------------------------------------
    
dataseg

SwimCCXMoveData:
    .byte $40, $80
    .byte $04, $04                                ; residual data, not used

.code

.proc MoveSwimmingCheepCheep

    ; locals
        SwimMoveDataBuffer = temp_byte + 2  ; - used to hold preset values
        EnemyStateBuffer   = temp_byte + 3  ; - used to hold enemy state
    ; end locals

    set_long_branch +
    ; jump to move defeated cheep-cheep downwards
    if Enemy_State[ x ] & #%00100000 goto MoveEnemySlowVert

    set_long_branch -

    sta EnemyStateBuffer                          ; save enemy state in $03

    mb y := Enemy_ID[ x ] - #10                   ; subtract ten for cheep-cheep identifiers use as offset
    mb SwimMoveDataBuffer := SwimCCXMoveData[ y ]

    mb Enemy_X_MoveForce[ x ] := Enemy_X_MoveForce[ x ] - SwimMoveDataBuffer
    mb Enemy_X_Position [ x ] := Enemy_X_Position[ x ]  - C           ; and save as new horizontal coordinate
    mb Enemy_PageLoc    [ x ] := Enemy_PageLoc[ x ]     - C

    mb SwimMoveDataBuffer := #$20                                ; save new value here

    if x >= #2                                    ; check enemy object offset is 2 or more

        if CheepCheepMoveMFlag[ x ] >= #$10
            mb Enemy_YMoveForceFractional[ x ] := Enemy_YMoveForceFractional[ x ] + SwimMoveDataBuffer     ; add to obtain carry
            mb Enemy_Y_Position     [ x ] := Enemy_Y_Position[ x ] +c EnemyStateBuffer     ; add carry + enemy state to slowly move it downward
            mb a := Enemy_Y_HighPos [ x ] + C                             
        else                                                                
            mb Enemy_YMoveForceFractional[ x ] := Enemy_YMoveForceFractional[ x ] - SwimMoveDataBuffer     ; subtract preset value to fractional
            mb Enemy_Y_Position     [ x ] := Enemy_Y_Position[ x ] -c EnemyStateBuffer   ; subtract borrow ,enemy state to slowly move it upwards
            mb a := Enemy_Y_HighPos [ x ] - C   ; subtract borrow from page location
        endif

        mb Enemy_Y_HighPos[ x ] := a              ; save new page location here
        ldy #0                                    ; load movement speed to upwards by default

        if Enemy_Y_Position[ x ] - CheepCheepOrigYPos[ x ] == negative
            ldy #$10                              ; load movement speed to downwards
            mb a := a ^ #$ff + #1                 ; twos compliment to get abs value
        endif

        if a >= #$0f                              ; coordinates < 15 pixels, leave movement speed alone
            tya
            mb CheepCheepMoveMFlag[ x ] := a      ; otherwise change movement speed
        endif
    endif

    rts

.endproc
    
    ; ------------------------------------------------------------------------------------------------

dataseg

FirebarPosLookupTbl:
    .byte $00, $01, $03, $04, $05, $06, $07, $07, $08
    .byte $00, $03, $06, $09, $0b, $0d, $0e, $0f, $10
    .byte $00, $04, $09, $0d, $10, $13, $16, $17, $18
    .byte $00, $06, $0c, $12, $16, $1a, $1d, $1f, $20
    .byte $00, $07, $0f, $16, $1c, $21, $25, $27, $28
    .byte $00, $09, $12, $1b, $21, $27, $2c, $2f, $30
    .byte $00, $0b, $15, $1f, $27, $2e, $33, $37, $38
    .byte $00, $0c, $18, $24, $2d, $35, $3b, $3e, $40
    .byte $00, $0e, $1b, $28, $32, $3b, $42, $46, $48
    .byte $00, $0f, $1f, $2d, $38, $42, $4a, $4e, $50
    .byte $00, $11, $22, $31, $3e, $49, $51, $56, $58

FirebarMirrorData:
    .byte $01, $03, $02, $00

FirebarTblOffsets:
    .byte $00, $09, $12, $1b, $24, $2d
    .byte $36, $3f, $48, $51, $5a, $63

FirebarYPos:
    .byte $0c, $18

.code

.proc ProcFirebar

    ; locals
        Xcoord_Temp      = temp_byte + 6          ;  used to store either screen X coordinate or sprite data offset
        YCoord_Temp      = temp_byte + 7          ;  used to store screen Y coordinate
        FireBarMaxLength = $ed                    ;  used to hold maximum length of firebar
        SpinStateHiByte  = $ef                    ;  used to hold high byte of spinstate
        LoopCounter      = temp_byte              ;  loop counter
    ; end locals

    jsr GetEnemyOffscreenBits                     ; get offscreen information

    if !Enemy_OffscreenBits & #%00001000          ; if d3 clear

        if !TimerControl                          ; if master timer control set, branch
            mb a := FirebarSpinSpeed[ x ]         ; load spinning speed of firebar
            jsr FirebarSpin                       ; modify current spinstate
            mb FirebarSpinState_High[ x ] := a & #%00011111            ; and store as new high byte of spinstate
        endif

        mb a := FirebarSpinState_High[ x ]        ; get high byte of spinstate

        if ldy Enemy_ID[ x ] >= #$1f && ( a = #$08 || a = #$18 )       ; if >= $1f (long firebar) and spinstate is 8 or 18
            mb FirebarSpinState_High[ x ] := a + #1
        endif

        sta SpinStateHiByte                       ; save high byte of spinning thing, modified or otherwise

        jsr RelativeEnemyPosition                 ; get relative coordinates to screen
        jsr GetFirebarPosition                    ; do a sub here (residual, too early to be used now)

        ldy Enemy_SprDataOffset,x                 ; get OAM data offset

        mb Sprite[ y ]::Y_Position := Enemy_Rel_YPos                   ; store as Y in OAM data
        mb YCoord_Temp := a                       ; also save here

        mb Sprite[ y ]::X_Position := Enemy_Rel_XPos                   ; store as X in OAM data
        mb Xcoord_Temp := a                       ; also save here

        mb LoopCounter := #1                      ; set $01 value here (not necessary) OP

        jsr FirebarCollision                      ; draw fireball part and do collision detection

        ldy #$05                                  ; load value for short firebars by default
        if Enemy_ID[ x ] >= #$1f                  ; are we doing a long firebar?
            ldy #$0b                              ; load value for long firebars
        endif
        sty FireBarMaxLength                      ; store maximum value for length of firebars

        mb LoopCounter := #0                      ; initialize counter here
        repeat

            mb a := SpinStateHiByte               ; load high byte of spinstate

            jsr GetFirebarPosition                ; get fireball position data depending on firebar part
            jsr DrawFirebar_Collision             ; position it properly, draw it and do collision detection

            if LoopCounter = #4                   ; check which firebar part
                ldy DuplicateObj_Offset           ; if we arrive at fifth firebar part,
                mb Xcoord_Temp := Enemy_SprDataOffset[ y ]   ; get offset from long firebar and load OAM data using long firebar offset
            endif

            inc LoopCounter                       ; move onto the next firebar part

        until LoopCounter >= FireBarMaxLength

    endif

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DrawFirebar_Collision

    ; locals
        SpinStateHi_XAdder  = temp_byte + 1       ;  - used for oscillated high byte of spin state or to hold horizontal adder
        SpinStateHi_YAdder  = temp_byte + 2       ;  - used for oscillated high byte of spin state or to hold vertical adder
        MirrorData          = temp_byte + 3       ;  - used for mirror data
        MirrorDataTemp      = temp_byte + 5       ;  - used to evaluate mirror data
        SprDataOffset       = temp_byte + 6       ;  - used to store sprite data offset
        Firebar_ScreenYPos  = temp_byte + 7       ;  - used to store screen Y coordinate
    ; end locals
    
   
    mb MirrorDataTemp := MirrorData               ; store mirror data elsewhere
    ldy SprDataOffset                             ; load OAM data offset for firebar
    lda SpinStateHi_XAdder                        ; load horizontal adder we got from position loader

    if lsr MirrorDataTemp == carry clear          ; shift LSB of mirror data
        mb a := a ^ #$ff +c #1                    ; get two's compliment of horizontal adder
    endif

    mb Sprite[ y ]::X_Position := a + Enemy_Rel_XPos
    mb SprDataOffset := a

    if a < Enemy_Rel_XPos                         ; compare X coordinate of sprite to original X of firebar
        mb a := Enemy_Rel_XPos - SprDataOffset
    else
    ; OP: carry will be set
        mb a := a - Enemy_Rel_XPos                ; subtract original X from the current sprite X
    endif


    if a >= #$59                                  ; if difference of coordinates within a certain range,
        lda #$f8                                  ; otherwise, load offscreen Y coordinate
    else Z clear
    ; a < #$59 :
        if Enemy_Rel_YPos <> #$f8                 ; if vertical relative coordinate offscreen,

            lda SpinStateHi_YAdder                ; load vertical adder we got from position loader

            if lsr MirrorDataTemp == carry clear                       ; shift LSB of mirror data one more time
                mb a := a ^ #$ff +c #1
            endif
            mb a := a + Enemy_Rel_YPos            ; add vertical coordinate relative to screen

        endif
    endif

    mb Sprite[ y ]::Y_Position := a               ; store as Y coordinate here
    mb Firebar_ScreenYPos := a

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc FirebarCollision
    ; caller:   .proc ProcFirebar
    ;           fallthrough: DrawFirebar_Collision above
    
    ; locals
        ProcFirebars_LoopCounter = temp_byte + 0
        Player_Sprite1_XPos      = temp_byte + 4
        LoopCounter              = temp_byte + 5   ;  - Count loop interations to help determine no collision
        Firebar_ScreenXPos       = temp_byte + 6
        Firebar_ScreenYPos       = temp_byte + 7
    ; end locals

    jsr DrawFirebar                               ; run sub here to draw current tile of firebar

    tya                                           ; return OAM data offset and save
    pha                                           ; to the stack for now
        if !StarInvincibleTimer | TimerControl    ; skip if star mario invincibility timer or TimerControl Active

            sta LoopCounter                       ; otherwise initialize counter

            if y := Player_Y_HighPos - 1 == zero                       ; if y = 1

                ldy Player_Y_Position             ; get player's vertical position

                if PlayerSize == SmallMario || CrouchingFlag           ; get player's size
                ; increment our counter twice to exit loop below immediately
                    inc LoopCounter
                    inc LoopCounter

                    mb a := y
                    mb y := a + #24               ; then add 24 pixels to the player's V coord (down)

                endif

                tya                               ; get vertical coordinate, altered or otherwise, from Y

                LoopForever:                      ; LoopCounter from 0 to 2
                ; subtract vertical position of firebar from the vertical coordinate of the player

                    if a := a - Firebar_ScreenYPos == negative         ; if player higher on the screen than firebar
                        mb a := a ^ #$ff + #1     ; two's compliment
                    endif

                    if a < #$08 && Firebar_ScreenXPos < #$f0           ; if difference < 8 pixels AND firebar not on far right of screen

                        mb Player_Sprite1_XPos := Sprite[ 4 ]::X_Position + #4
                        ; subtract horizontal coordinate of firebar
                        ; from the X coordinate of player's sprite 1
                        if a - Firebar_ScreenXPos == negative          ; if X coordinate to the right of firebar
                            mb a := a ^ #$ff + #1                      ; two's compliment
                        endif

                        if a < #$08 goto ExitLoop                      ; if difference < 8 pixels, collision, thus branch

                    endif

                    if LoopCounter = #2 goto ExitProc                  ; branch to increment OAM offset and leave, no collision
                    ; this section: LoopCounter = 0 to 1
                    ldy LoopCounter               ; OP tay              ; use loop count as offset
                    mb a := Player_Y_Position + FirebarYPos[ y ]       ; add value at offset to player's vertical coordinate

                    inc LoopCounter               ; then increment temp and jump back

                jmp LoopForever
                ExitLoop:

                ldx #1                            ; set movement direction by default
                if Player_Sprite1_XPos < Firebar_ScreenXPos
                    inx                           ; otherwise increment movement direction
                endif

                stx Enemy_MovingDir               ; store movement direction here
                ldx #0

                lda ProcFirebars_LoopCounter      ; save loopcounter from ProcFirebar ; OP .. not needed? Optimize caller as well
                pha
                    jsr InjurePlayer              ; perform sub to hurt or kill player
                pla
                sta ProcFirebars_LoopCounter
            endif
        endif
        ExitProc:

    pla                                           ; get OAM data offset from before

    mb Firebar_ScreenXPos := a + #4
    ldx ObjectOffset                              ; get enemy object buffer offset and leave
    rts

.endproc

.proc GetFirebarPosition
    ; callers : ProcFirebar
    
    ; locals
        ProcFirebars_LoopCounter = temp_byte      ;  - the loop counter from above this
        SpinStateHi_XAdder  = temp_byte + 1       ;  - used for oscillated high byte of spin state or to hold horizontal adder
        SpinStateHi_YAdder  = temp_byte + 2       ;  - used for oscillated high byte of spin state or to hold vertical adder
        MirrorData          = temp_byte + 3       ;  - used for mirror data
    ; end locals

    pha                                           ; save high byte of spinstate to the stack
        if a & #%00001111 >= #9                   ; mask out low nybble
            mb a := a ^ #$0f + #1
        endif

        sta SpinStateHi_XAdder                    ; store result, modified or not, here
        ldy ProcFirebars_LoopCounter              ; load number of firebar ball where we're at

        mb y := FirebarTblOffsets[ y ] + SpinStateHi_XAdder            ; add oscillated high byte of spinstate to index
        mb SpinStateHi_XAdder := FirebarPosLookupTbl[ y ]              ; store as horizontal adder
    pla                                           ; pull whatever was in A from the stack

    pha                                           ; save it again because we still need it
        mb a := a + #8                            ; add eight this time, to get vertical adder
        if a & #%00001111 >= #9                   ; mask out high nybble
            mb a := a ^ #$0f + #1                 ; otherwise get two's compliment
        endif

        sta SpinStateHi_YAdder                    ; store result here
        ldy ProcFirebars_LoopCounter
        ; this time add value in $02 to offset here and use as offset load offset to firebar position data again
        mb y := FirebarTblOffsets[ y ] + SpinStateHi_YAdder
        mb SpinStateHi_YAdder := FirebarPosLookupTbl[ y ]
    pla                                           ; pull out whatever was in A one last time

    
    mb y := a / 8
    mb MirrorData := FirebarMirrorData[ y ]       ;  mirroring data

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

PRandomSubtracter:
    .byte $f8, $a0, $70, $bd, $00

FlyCCBPriority:
    .byte $20, $20, $20, $00, $00

.code

.proc MoveFlyingCheepCheep

    if Enemy_State[ x ] & #%00100000              ; check cheep-cheep's enemy state
        mb Enemy_SprAttrib[ x ] := #0             ; clear sprite attributes
        jmp MoveJumpEnemyVertically                 ; and jump to move defeated cheep-cheep downwards
    endif

    jsr MoveEnemyHorizontally                     ; move cheep-cheep horizontally based on speed and force

    ldy #$0d                                      ; set vertical movement amount
    lda #$05                                      ; set maximum speed
    jsr SetXMoveAmt                               ; branch to impose gravity on flying cheep-cheep

    mb y := Enemy_Y_MoveForce[ x ] >> 4           ; save as offset (note this tends to go into reach of code)

    if Enemy_Y_Position[ x ] - PRandomSubtracter[ y ] == negative      ; if result within bottom half of screen
        mb a := a ^ #$ff + #1
    endif

    if a < #$08                                   ; if result or two's compliment greater than eight,
        mb Enemy_Y_MoveForce[ x ] := Enemy_Y_MoveForce[ x ] + #$10
        mb y := a >> 4
    endif
    ; load bg priority data and store (this is very likely broken or residual code, value is overwritten before
    ; drawing it next frame), then leave
    mb Enemy_SprAttrib[ x ] := FlyCCBPriority[ y ]

    rts

.endproc

dataseg

LakituDiffAdj:
    .byte $15, $30, $40

.code

.proc MoveLakitu

    ; locals
        Difference_Adjust   = temp_byte + 1       ; 3 bytes
        Horizontal_Diff     = temp_byte
    ; end locals
    
    ; check lakitu's enemy state

    set_long_branch +

        if Enemy_State[ x ] & #%00100000 goto MoveD_EnemyVertically

    set_long_branch -

    if Enemy_State[ x ]                           ; if lakitu's enemy state set

        mb a, LakituMoveDirection[ x ] := #0      ; initialize moving direction to move to left
        mb EnemyFrenzyBuffer := a                 ; #0                  ; initialize frenzy buffer
        lda #$10                                  ; load horizontal speed

    else Z clear

        mb EnemyFrenzyBuffer := #OBJECTID_Spiny            ; set spiny identifier in frenzy buffer

        ldy #2
        repeat                                    ; y = 2 to 0
            mb Difference_Adjust[ y ] := LakituDiffAdj[ y ]
        until dey == negative

        jsr PlayerLakituDiff                      ; execute sub to set speed and create spinys

    endif

    sta LakituMoveSpeed,x                         ; set movement speed returned from sub

    ldy #1                                        ; set moving direction to right by default

    if ! LakituMoveDirection[ x ] & #1            ; if not moving right
        mb LakituMoveSpeed[ x ] := LakituMoveSpeed[ x ] ^ #$ff + #1    ; get two's compliment of moving speed
        iny                                       ; increment moving direction to left
    endif

    sty Enemy_MovingDir,x                         ; store moving direction

    jmp MoveEnemyHorizontally                     ; move lakitu horizontally

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PlayerLakituDiff

    ; locals
        PlayerEDiffAmount = temp_byte
        Difference_Adjust = temp_byte + 1         ; 3 bytes
    ; end locals

    ldy #0                                        ; set Y for default value

    if jsr PlayerEnemyDiff == negative            ; if enemy is on the left of the player
        ; get two's compliment for abs:
        iny                                       ; increment Y for left of player
        mb PlayerEDiffAmount := PlayerEDiffAmount ^ #$FF + #1          ; get abs value
    endif


    if PlayerEDiffAmount >= #$3c

        mb PlayerEDiffAmount := #$3c              ; set maximum distance
        ; check if lakitu is in our current enemy slot, 0 = mario on the left, 1 = mario on the right
        ; if moving away from the player: (LakituMoveDirection = 0 means left)
        if Enemy_ID[ x ] = #OBJECTID_Lakitu && tya : a <> LakituMoveDirection[ x ]

            if LakituMoveDirection[ x ]           ; if moving to the left beyond maximum distance, ??
                dec LakituMoveSpeed,x             ; decrement horizontal speed
                if LakituMoveSpeed[ x ] goto Exit           ; if horizontal speed not yet at zero, branch to leave
            endif

            tya
            ; set horizontal direction depending difference between enemy and player
            mb LakituMoveDirection[ x ] := a

        endif
    endif

    mb PlayerEDiffAmount := PlayerEDiffAmount & #%00111100 / 4

    ldy #0                                        ; init offset
    if Player_X_Speed && ScrollAmount

        iny                                       ; increment offset
        if Player_X_Speed >= #$19 && ScrollAmount >= #2
            iny                                   ; increment once more
        endif
        ; if (NOT Spiny) OR ((PlayerX not moving) AND (EnemyY not Moving))
        if ( Enemy_ID[ x ] <> #OBJECTID_Spiny || !Player_X_Speed )
            if !Enemy_Y_Speed[ x ]                ; OP residual bad code? Player_X_Speed should always be non-zero here
                ldy #0                            ; reinit offset
            endif
        endif

    endif

    lda Difference_Adjust,y                       ; get one of three saved values from earlier
    ldy PlayerEDiffAmount                         ; get saved horizontal difference

    repeat
        mb a := a - #1
    until dey == negative

    Exit:

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------


dataseg

BridgeCollapseData:
    .byte $1a                                     ; axe
    .byte $58                                     ; chain
    .byte $98, $96, $94, $92, $90, $8e, $8c       ; bridge
    .byte $8a, $88, $86, $84, $82, $80

.code

.proc BridgeCollapse

    ; locals
        NameTableAddress = temp_byte + 4          ; 2 bytes
    ; end locals

    ldx BowserFront_Offset                        ; get enemy offset for bowser

    if Enemy_ID[ x ] = #OBJECTID_Bowser                    ; check enemy object identifier for bowser
        stx ObjectOffset                          ; store as enemy offset here
        ; if bowser still in normal state removal metatile
        if !lda Enemy_State[ x ] goto RemoveBridge
        ; if bowser's state has d6 set AND if bowser high enough that he still needs to move down
        if a & #%01000000 && Enemy_Y_Position[ x ] < #$e0 goto MoveD_Bowser
    endif

    mb EventMusicQueue := #MUSIC_Silence          ; silence music
    inc OperMode_Task                             ; move onto next secondary mode in autoctrl mode
    jmp KillAllEnemies                            ; jump to empty all enemy slots and then leave

    MoveD_Bowser:

    jsr MoveEnemySlowVert                         ; do a sub to move bowser downwards
    jmp BowserGfxHandler                          ; jump to draw bowser's front and rear, then leave

    RemoveBridge:

    if dec BowserFeetCounter == zero              ; decrement timer to control bowser's feet

        mb BowserFeetCounter := #4                ; set timer now
        mb BowserBodyControls := BowserBodyControls ^ #1               ; invert bit to control bowser's feet
        ; prepare parameters for jsr:
        mb NameTableAddress[ 1 ] := #$22          ; put high byte of name table address here for now

        ldy BridgeCollapseOffset                  ; get bridge collapse offset here
        mb NameTableAddress[ 0 ] := BridgeCollapseData[ y ]            ; load low byte of name table address and store here

        mb y := VRAM_Buffer1_Offset + 1
        ldx #$0c                                  ; set offset for tile data for sub to draw blank metatile
        jsr PutBlockMetatile::RemBridge           ; do sub here to remove bowser's bridge metatiles

        ldx ObjectOffset                          ; get enemy offset
        jsr WriteBlockMetatile::MoveVOffset       ; set new vram buffer offset
        ; both sounds at the same time for bride sound:
        mb Square2SoundQueue    := #SFX_Blast
        mb NoiseSoundQueue      := #SFX_BrickShatter

        inc BridgeCollapseOffset                  ; increment bridge collapse offset
        if BridgeCollapseOffset = #$0f            ; if bridge collapse offset has reached the end
            jsr InitSpeedAndMoveForce0            ; initialize whatever vertical speed bowser has
            mb Enemy_State[ x ] := #%01000000     ; set bowser's state to one of defeated states (d6 set)
            mb Square2SoundQueue := #SFX_BowserFall                    ; play bowser defeat sound
        endif
    endif

    jmp BowserGfxHandler                          ; jump to code that draws bowser

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

PRandomRange:
    .byte $21, $41, $11, $31

.code

.proc RunBowser

    .export KillAllEnemies

    if Enemy_State[ x ] & #%00100000
        ; if above a certain point, branch to move defeated bowser
        if Enemy_Y_Position[ x ] < #$e0 goto BridgeCollapse::MoveD_Bowser
        ; Erase all flames and things
        KillAllEnemies:

        ldx #4                                    ; start with last enemy slot
        repeat
            jsr EraseEnemyObject                  ; branch to kill enemy objects
        until dex == negative
        ; a returns with #0
        sta EnemyFrenzyBuffer                     ; empty frenzy buffer
        ldx ObjectOffset                          ; get enemy object offset and leave
        rts

    endif

    mb EnemyFrenzyBuffer := #0                    ; empty frenzy buffer

    set_long_branch +

     if TimerControl goto TimerControlSkip
    ; check bowser's mouth??
     if BowserBodyControls == bit7 clear

    set_long_branch -

        if !dec BowserFeetCounter                 ; decrement timer to control bowser's feet
            mb BowserFeetCounter  := #$20         ; reset timer
            mb BowserBodyControls := BowserBodyControls ^ #%00000001   ; to control bowser's feet
        endif

        if !FrameCounter & #%00001111             ; every 16th frame
            mb Enemy_MovingDir[ x ] := #2         ; reset moving direction
        endif
        ; if timer not expired AND if bowser to the left of the player
        if EnemyFrameTimer[ x ] && jsr PlayerEnemyDiff == negative
            mb Enemy_MovingDir[ x ] := #1         ; set bowser to move and face to the right
            mb BowserMovementSpeed  := #2         ; set movement speed
            mb EnemyFrameTimer[ x ] := #$20       ; set timer here
            sta BowserFireBreathTimer             ; set timer used for bowser's flame
            ; if bowser to the right past a certain point
            if Enemy_X_Position[ x ] >= #$c8 goto HammerChk
        endif

        if !FrameCounter & #%00000011             ; every 4 frames

            if Enemy_X_Position[ x ] = BowserOrigXPos
                mb y := PseudoRandomBitReg[ x ] & #%00000011
                mb MaxRangeFromOrigin := PRandomRange[ y ]
            endif

            mb Enemy_X_Position[ x ] := Enemy_X_Position[ x ] + BowserMovementSpeed                       
            ; if bowser moving and facing to the left
            if ldy Enemy_MovingDir[ x ] <> #1

                ldy #<-1                          ; move left
                ; if new Bowser X pos - BowserOrigXPos == negative ; left of original
                if a - BowserOrigXPos == negative                      ; horizontal position
                    mb a := a ^ #$ff + #1         ; get abs distance from orignal pos
                    ldy #1                        ; move right
                endif

                if a >= MaxRangeFromOrigin        ; compare difference with pseudorandom value
                    sty BowserMovementSpeed       ; change bowser's movement speed
                endif
            endif
        endif
    endif

    HammerChk:

    if !EnemyFrameTimer[ x ]                      ; If timer expired

        jsr MoveEnemySlowVert                     ; move bowser downwards
        ; throw hammers every 4th frame if on W6 or higher
        if WorldNumber >= #WORLD6 && !FrameCounter & #%00000011
            jsr SpawnHammerObj                    ; execute sub on every fourth frame to spawn misc object (hammer)
        endif

        if Enemy_Y_Position[ x ] < #$80 goto CheckFlames               ; get current vertical position. OP use bit7
        mb y := PseudoRandomBitReg[ x ] & #%00000011
        mb EnemyFrameTimer[ x ] := PRandomRange[ y ]

        TimerControlSkip:                         ; OP: this label could point to CheckFlames

    elseif a = #1                                 ; if timer about to expire
        dec Enemy_Y_Position,x                    ; decrement vertical coordinate
        jsr InitSpeedAndMoveForce0                ; initialize movement amount
        mb Enemy_Y_Speed[ x ] := #<(-2)           ; set vertical speed to move bowser upwards
    endif

    CheckFlames:

    if lda WorldNumber = #WORLD8 || a < #WORLD6
        if !BowserFireBreathTimer                 ; check world number here

            mb BowserFireBreathTimer := #$20      ; set timer here
            mb BowserBodyControls := BowserBodyControls ^ #%10000000   ; invert bowser's mouth bit to move bowser's mouth

            if bit7 goto CheckFlames              ; if bowser's mouth open, loop back: OP: wtf? just close mouth

            jsr SetFlameTimer                     ; get timing for bowser's flame

            if ldy SecondaryHardMode
                mb a := a - #16                   ; 16 less frames delay
            endif

            sta BowserFireBreathTimer             ; set value as timer here
            mb EnemyFrenzyBuffer := #OBJECTID_BowserFlame
        endif
    endif
    ; fall through:
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc BowserGfxHandler

    jsr ProcessBowserHalf                         ; do a sub here to process bowser's front

    ldy #$10                                      ; load default value here to position bowser's rear
    if Enemy_MovingDir[ x ] >> 1 == carry set     ; check moving direction
        ldy #$f0                                  ; moving right, load alternate positioning value here
    endif
    tya                                           ; move bowser's rear object position value to A
    mb a := a + Enemy_X_Position[ x ]             ; add to bowser's front object horizontal coordinate

    ldy DuplicateObj_Offset                       ; get bowser's rear object offset

    mb Enemy_X_Position [ y ] := a                ; store A as bowser's rear horizontal coordinate
    mb Enemy_Y_Position [ y ] := Enemy_Y_Position[ x ] + #8            ; add eight pixels to bowser's front object
    mb Enemy_State      [ y ] := Enemy_State[ x ]                      ; copy enemy state directly from front to rear
    mb Enemy_MovingDir  [ y ] := Enemy_MovingDir[ x ]                  ; copy moving direction also

    lda ObjectOffset                              ; save enemy object offset of front to stack
    pha
        mb x,ObjectOffset := DuplicateObj_Offset                       ; put enemy object offset of rear as current
        mb Enemy_ID[ x ]  := #OBJECTID_Bowser              ; set bowser's enemy identifier in rear object
        jsr ProcessBowserHalf                     ; do a sub here to process bowser's rear
    pla
    sta ObjectOffset                              ; get original enemy object offset

    tax
    mb BowserGfxFlag := #0                        ; nullify bowser's front/rear graphics flag

    Exit:
    rts

.endproc

.proc ProcessBowserHalf
    
    inc BowserGfxFlag                           ; increment bowser's graphics flag, then run subroutines
    jsr RunRetainerObj                          ; to get offscreen bits, relative position and draw bowser (finally!)
    
    if Enemy_State[ x ] goto BowserGfxHandler::Exit          ; if either enemy object not in normal state, branch to leave
    
    mb Enemy_BoundBoxCtrl[ x ] := #$0a          ; set bounding box size control
    jsr GetEnemyBoundBox                        ; get bounding box coordinates
    jmp PlayerEnemyCollision                    ; do player-to-enemy collision detection

.endproc

    ; ------------------------------------------------------------------------------------------------  

dataseg

FlameTimerData:
    .byte $bf, $40, $bf, $bf, $bf, $40, $40, $bf

.code

.proc SetFlameTimer

    ldy BowserFlameTimerCtrl                      ; load counter as offset
    inc BowserFlameTimerCtrl                      ; increment

    mb BowserFlameTimerCtrl := BowserFlameTimerCtrl & #%00000111       ; keep in range of 0-7
    mb a := FlameTimerData[ y ]                   ; load value to be used then leave

    Exit:
    rts

.endproc


.proc ProcBowserFlame

    ; locals
        TileNumber    = temp_byte
        TileAttribute = temp_byte + 1
    ; end locals
    

    if !TimerControl

        lda #$40                                  ; load default movement force
        if ldy SecondaryHardMode
            lda #$60                              ; load alternate movement force to go faster
        endif

        sta temp_byte                             ; store value here
        mb Enemy_X_MoveForce[ x ] := Enemy_X_MoveForce[ x ] - temp_byte
        mb Enemy_X_Position [ x ] := Enemy_X_Position[ x ] -c #1
        ; subtract borrow from page location
        mb Enemy_PageLoc[ x ] := Enemy_PageLoc[ x ] - C

        ldy BowserFlamePRandomOfs,x               ; get some value here and use as offset

        if Enemy_Y_Position[ x ] <> FlameYPosData[ y ]
            mb Enemy_Y_Position[ x ] := a + Enemy_Y_MoveForce[ x ]
        endif
    endif

    jsr RelativeEnemyPosition                     ; get new relative coordinates

    if Enemy_State[ x ] goto SetFlameTimer::Exit  ; if bowser's flame not in normal state

    mb TileNumber := #$51                         ; write first tile number
    ; flip the flame up and down every 2 frames
    ldy #%00000010                                ; load attributes without vertical flip by default
    if FrameCounter & #%00000010
        ldy #%10000010                            ; otherwise write value with vertical flip bit set
    endif

    sty TileAttribute                             ; set bowser's flame sprite attributes here
    ldy Enemy_SprDataOffset,x                     ; get OAM data offset
    ldx #0
    ; Draw Flames:
    repeat

        mb Sprite[ y ]::Y_Position := Enemy_Rel_YPos
        mb Sprite[ y ]::Tilenumber := TileNumber                       ; write current tile number into OAM data

        inc TileNumber                            ; increment tile number to draw more bowser's flame

        mb Sprite[ y ]::Attributes := TileAttribute
        mb Sprite[ y ]::X_Position := Enemy_Rel_XPos
        mb Enemy_Rel_XPos := a + #8
        mb y := y + 4                             ; increment Y four times to move onto the next OAM

    until inx >= #3


    ldx ObjectOffset                              ; reload original enemy offset
    jsr GetEnemyOffscreenBits                     ; get offscreen information

    ldy Enemy_SprDataOffset,x                     ; get OAM data offset

    mb a := Enemy_OffscreenBits >> 1              ; get enemy object offscreen bits
    pha
        if carry set                              ; move sprite offscreen, this part likely
            mb Sprite[ y + 12 ]::Y_Position := #$f8                    ; residual since flame is only made of three sprites
        endif
    pla                                           ; get bits from stack

    mb a := a >> 1                                ; move d1 to carry and move bits back to stack
    pha
        if carry set
            mb Sprite[ y + 8 ]::Y_Position := #$f8                     ; move third sprite offscreen
        endif
    pla                                           ; get bits from stack again

    mb a := a >> 1                                ; move d2 to carry and move bits back to stack again
    pha
        if carry set
            mb Sprite[ y + 4 ]::Y_Position := #$f8                     ; move third sprite offscreen
        endif
    pla                                           ; get bits from stack one last time

    mb a := a >> 1                                ; move d3 to carry
    if carry set
        mb Sprite[ y ]::Y_Position := #$f8        ; move third sprite offscreen
    endif

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RunFireworks

    if !dec ExplosionTimerCounter[ x ]            ; decrement explosion timing counter here
        mb ExplosionTimerCounter[ x ] := #$08     ; reset counter
        inc ExplosionGfxCounter,x                 ; increment explosion graphics counter

        if ExplosionGfxCounter[ x ] >= #3 goto FireworksSoundScore
    endif

    jsr RelativeEnemyPosition                     ; get relative coordinates of explosion

    mb Fireball_Rel_YPos := Enemy_Rel_YPos
    mb Fireball_Rel_XPos := Enemy_Rel_XPos

    ldy Enemy_SprDataOffset,x                     ; get OAM data offset
    lda ExplosionGfxCounter,x                     ; get explosion graphics counter
    jsr DrawExplosion_Fireworks                   ; do a sub to draw the explosion then leave

    rts

    FireworksSoundScore:

    mb Enemy_Flag[ x ] := #0
    mb Square2SoundQueue := #SFX_Blast            ; play fireworks/gunfire sound
    mb DigitModifier[ 4 ] := #$05                 ; set part of score modifier for 500 points
    jmp AwardEndAreaPoints                        ; jump to award points accordingly then leave

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

StarFlagYPosAdder:
    .byte $00, $00, $08, $08

StarFlagXPosAdder:
    .byte $00, $08, $00, $08

StarFlagTileData:
    .byte $54, $55, $56, $57

.code

.proc RunStarFlagObj

    mb EnemyFrenzyBuffer := #0

    if StarFlagTaskControl >= #5 goto StarFlagExit

    jsr JumpEngine

    .word StarFlagExit
    .word GameTimerFireworks
    .word AwardGameTimerPoints
    .word RaiseFlagSetoffFWorks
    .word DelayToAreaEnd

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc GameTimerFireworks

    ldy #$05                                      ; set default state for star flag object
    mb a := GameTimerDisplay[ 2 ]                 ; get game timer's last digit

    if a = #1 goto SetFireWorksCounter

    ldy #3
    if a = #3 goto SetFireWorksCounter

    ldy #0
    if a = #6 goto SetFireWorksCounter
    lda #$ff                                      ; otherwise set value for no fireworks

    SetFireWorksCounter:

    sta FireworksCounter                          ; set fireworks counter here
    sty Enemy_State,x                             ; set whatever state we have in star flag object


    IncrementSFTask1:
    inc StarFlagTaskControl                       ; increment star flag object task number


.endproc

    StarFlagExit:
    rts
    
    ; ------------------------------------------------------------------------------------------------

.proc AwardGameTimerPoints

    .export AwardEndAreaPoints
    ; if no time left on game timer at all, branch to next task
    if !GameTimerDisplay | GameTimerDisplay[ 1 ] | GameTimerDisplay[ 2 ] goto GameTimerFireworks::IncrementSFTask1
    ; check frame counter for d2 set (do for four frames every four frames)

    if FrameCounter & #%00000100
        mb Square2SoundQueue := #SFX_TimerTick    ; load timer tick sound
    endif

    ldy #$23                                      ; set offset here to subtract from game timer's last digit

    mb DigitModifier[ 5 ] := # (<-1)              ; set adder here to $ff, or -1, to subtract one from the last digit of the game timer
    jsr DigitsMathRoutine                         ; subtract digit

    mb DigitModifier[ 5 ] := #$05                 ; set now to add 50 points

    AwardEndAreaPoints:

    ldy #$0b                                      ; load offset for mario's score by default
    if CurrentPlayer != zero                      ; check player on the screen
        ldy #$11                                  ; otherwise load offset for luigi's score
    endif
    ; award 50 points per game timer interval or 500 points per fireworks explosion
    jsr DigitsMathRoutine
    ; shift player to high nybble and add four to set nybble for game timer
    mb a := CurrentPlayer << 4 | #4
    jmp UpdateNumber                              ; jump to print the new score and game timer and rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RaiseFlagSetoffFWorks

    .export DrawStarFlag

    if Enemy_Y_Position[ x ] >= #$72              ; otherwise, raise star flag by one pixel if too low
        dec Enemy_Y_Position,x
    else                                          ; Flag is all the way up:

        lda FireworksCounter                      ; check fireworks counter
        ; if no fireworks left or set to go off, branch
        if zero || bit7 set goto DrawFlagSetTimer
        mb EnemyFrenzyBuffer := #OBJECTID_Fireworks        ; otherwise set fireworks object in frenzy queue

    endif

    DrawStarFlag:

    jsr RelativeEnemyPosition                     ; get relative coordinates of star flag
    ldy Enemy_SprDataOffset,x                     ; get OAM data offset
    ldx #3                                        ; do four sprites

    repeat                                        ; X = 3 to 0

        mb Sprite[ y ]::Y_Position := Enemy_Rel_YPos + StarFlagYPosAdder[ x ]
        mb Sprite[ y ]::Tilenumber := StarFlagTileData[ x ]
        mb Sprite[ y ]::Attributes := #%00100010
        mb Sprite[ y ]::X_Position := Enemy_Rel_XPos + StarFlagXPosAdder[ x ]

        mb y := y + 4                             ; move onto next sprite

    until dex == negative

    ldx ObjectOffset                              ; get enemy object offset and leave

    rts
    
    ; ------------------------------------------------------------------------------------------------

    DrawFlagSetTimer:

    jsr DrawStarFlag                              ; do sub to draw star flag

    mb EnemyIntervalTimer[ x ] := #$06            ; set interval timer here

    IncrementSFTask2:

    inc StarFlagTaskControl                       ; move onto next task
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DelayToAreaEnd

    jsr DrawStarFlag                              ; do sub to draw star flag
    ; if interval timer not set AND if event music buffer empty branch to increment task
    if !EnemyIntervalTimer[ x ] && !EventMusicBuffer goto RaiseFlagSetoffFWorks::IncrementSFTask2

    rts                                           ; otherwise leave

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc MovePiranhaPlant

    ; temp_byte - used to store horizontal difference between player and piranha plant

    ; locals
        MaxVerticalCoord    = temp_byte
        PlayerDistance      = temp_byte
    ; end locals

    if !Enemy_State[ x ] && !EnemyFrameTimer[ x ]                      ; if both not set:
        if !PiranhaPlant_MoveFlag[ x ]            ; if not moving ( pausing at top or inside pipe ):
            if PiranhaPlant_Y_Speed[ x ] == positive                   ; if it was last moving down

                jsr PlayerEnemyDiff
                if negative
                    mb PlayerDistance := PlayerDistance ^ #$FF + #1    ; get abs of distance
                endif

                if PlayerDistance < #$21 goto Exit                     ; if player within a certain distance, leave
            endif
            ; Reverse Plant Speed:
            mb PiranhaPlant_Y_Speed[ x ] := PiranhaPlant_Y_Speed[ x ] ^ #$ff + #1
            inc PiranhaPlant_MoveFlag,x           ; increment to set movement flag

        endif
        ; Move PPLant

        mb a := PiranhaPlantDownYPos[ x ]         ; get original vertical coordinate (lowest point)

        if ldy PiranhaPlant_Y_Speed[ x ] == negative                   ; if moving up
            mb a := PiranhaPlantUpYPos[ x ]       ; get other vertical coordinate (highest point)
        endif
        ; Move PiranhaPlant Up and Down:

        sta MaxVerticalCoord                      ; save vertical max (up or down) coordinate here
        ; if odd frame and no TimerControl:
        if FrameCounter >> 1 == C set && !TimerControl

            mb Enemy_Y_Position[ x ] := Enemy_Y_Position[ x ] + PiranhaPlant_Y_Speed[ x ]

            if a = MaxVerticalCoord                     ; compare against low or high coordinate
                mb PiranhaPlant_MoveFlag[ x ] := #0     ; clear movement flag
                mb EnemyFrameTimer      [ x ] := #$40   ; set timer to delay piranha plant movement
            endif
        endif
    endif

    Exit:

    mb Enemy_SprAttrib[ x ] := #%00100000         ; attributes to give illusion of being inside pipe
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------    

.proc FirebarSpin

    ; locals
        SpinngSpeed = temp_byte + 7
    ; end locals

    sta SpinngSpeed                               ; save spinning speed here
    ; if moving clockwise:
    if !FirebarSpinDirection[ x ]

        ldy #$18                                  ; possibly residual ldy OP
        mb FirebarSpinState_Low[ x ] := FirebarSpinState_Low[ x ] + SpinngSpeed
        mb a := FirebarSpinState_High[ x ] + C
        rts

    endif
    ; else, spin counter clockwise

    ldy #$08                                      ; possibly residual ldy OP

    mb FirebarSpinState_Low[ x ] := FirebarSpinState_Low[ x ] - SpinngSpeed
    mb a := FirebarSpinState_High[ x ] - C
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------
    

.proc BalancePlatform   ; Process the 'scale' balance platform pair

    ; locals
        ; temp_byte - used to hold collision flag, Y movement force + 5 or low byte of name table for rope
        NametableHiRope  = temp_byte + 1  ; - used to hold high byte of name table for rope
        PageLocationRope = temp_byte + 2 ; - used to hold page location of rope
    ; end locals

    if Enemy_Y_HighPos[ x ] = #3                  ; check high byte of vertical position
        jmp EraseEnemyObject                      ; if far below screen, kill the object and leave
    endif
    ; get object's state (set to $ff or other platform offset), if doing other balance platform, leave
    if lda Enemy_State[ x ] == bit7 set
        rts
    endif

    tay                                           ; save offset for other platform

    lda PlatformCollisionFlag,x                   ; get collision flag of platform
    sta temp_byte                                 ; store here

    if Enemy_MovingDir[ x ]                       ; get moving direction
        jmp PlatformFall                          ; if set, jump here
    endif
    ; Check for fall
    ; if Enemy_Y_Position[ x ] <= #$2d
    if #$2d >= Enemy_Y_Position[ x ]              ; check if platform is above a certain point
        ; if Other platform offset <> PlatformCollisionFlag[ x ]
        if y <> temp_byte                         ; if collision flag is not the same as enemy state:
            mb Enemy_Y_Position[ x ] := a + #2
            jmp StopPlatforms                     ; to make platforms stop
        endif                                     ; else:
        MakePlatformFall:

        jmp InitPlatformFall                      ; make platforms fall

    endif
    ; if Enemy_Y_Position[ y ] <= #$2d
    if a >= Enemy_Y_Position[ y ]                 ; check if other platform is above a certain point

        if x = temp_byte goto MakePlatformFall

        mb Enemy_Y_Position[ y ] := a + #2
        jmp StopPlatforms                         ; jump to stop movement and do not return

    endif
    ; ChkToMoveBalPlat:

    lda Enemy_Y_Position,x                        ; save vertical position to stack
    pha

        if PlatformCollisionFlag[ x ] == bit7     ; get collision flag

            mb temp_byte := Enemy_Y_MoveForce[ x ] + #5                ; add $05 to contents of moveforce

            lda Enemy_Y_Speed,x
            adc #0                                ; add carry to vertical speed

            if negative goto PlatDn               ; branch if moving downwards
            if not zero goto PlatUp               ; branch elsewhere if moving upwards

            lda temp_byte
            cmp #$0b                              ; check if there's still a little force left
            if less goto PlatSt                   ; if not enough, branch to stop movement
            if greaterORequal goto PlatUp         ; otherwise keep branch to move upwards (branch always)

        endif

        ; if collision flag matches current enemy object offset
        if a = ObjectOffset goto PlatDn

        PlatUp:

        jsr MovePlatformUp                        ; do a sub to move upwards
        jmp DoOtherPlatform                       ; jump ahead to remaining code

        PlatSt:

        jsr StopPlatforms                         ; do a sub to stop movement
        jmp DoOtherPlatform                       ; jump ahead to remaining code

        PlatDn:

        jsr MovePlatformDown                      ; do a sub to move downwards

        DoOtherPlatform:

        ldy Enemy_State,x                         ; get offset of other platform
    pla                                           ; get old vertical coordinate from stack

    mb a := a - Enemy_Y_Position[ x ]             ; get difference of old vs. new coordinate
    mb Enemy_Y_Position[ y ] := a + Enemy_Y_Position[ y ]         ;  add difference to vertical coordinate of other

    if PlatformCollisionFlag[ x ] == bit7 clear
        tax                                       ; put offset which collision occurred here
        jsr PositionPlayerOnVPlat                 ; and use it to position player accordingly
    endif
    ; Erase the rope:

    ldy ObjectOffset                              ; get enemy object offset
    ; check to see if current platform is moving AND there is room in the Buffer
    if Enemy_Y_Speed[ y ] | Enemy_Y_MoveForce[ y ] && ldx VRAM_Buffer1_Offset < #$20

        lda Enemy_Y_Speed,y
        pha                                       ; save two copies of vertical speed to stack
            pha
                jsr SetupPlatformRope             ; do a sub to figure out where to put new bg tiles
                ; write name table address to vram buffer
                mb VRAM_Buffer1[ x ] := NametableHiRope                 ; first the high byte, then the low
                mb VRAM_Buffer1[ 1 + x ] := temp_byte
                mb VRAM_Buffer1[ 2 + x ] := #2

                if Enemy_Y_Speed[ y ] == positive                      ; if platform moving down
                ; tiles for left and right rope
                    mb VRAM_Buffer1[ 3 + x ] := #$a2
                    mb VRAM_Buffer1[ 4 + x ] := #$a3
                else
                ; put blank tiles in vram buffer to erase rope1
                    mb VRAM_Buffer1[ 3 + x ] := #$24
                    mb VRAM_Buffer1[ 4 + x ] := a
                endif

                mb a, y := Enemy_State[ y ]       ; get offset of other platform from state
            pla                                   ; pull second copy of vertical speed from stack

            eor #$ff                              ; invert bits to reverse speed
            jsr SetupPlatformRope                 ; do sub again to figure out where to put bg tiles

            mb VRAM_Buffer1[ 5 + x ] := NametableHiRope
            mb VRAM_Buffer1[ 6 + x ] := temp_byte
            mb VRAM_Buffer1[ 7 + x ] := #2        ; set length again for 2 bytes
        pla                                       ; pull first copy of vertical speed from stack
        
        if negative ; if moving downward
            mb VRAM_Buffer1[ 8 + x ] := #$a2      ; otherwise put tile numbers for left
            mb VRAM_Buffer1[ 9 + x ] := #$a3      ; transfer buffer
        else
            ; erase rope 2
            mb VRAM_Buffer1[ 8 + x ] := #$24
            mb VRAM_Buffer1[ 9 + x ] := a
        endif

        mb VRAM_Buffer1[ 10 + x ] := #0           ; put null terminator at the end
        mb VRAM_Buffer1_Offset := VRAM_Buffer1_Offset + #10

    endif
    ; exit rope section

    ldx ObjectOffset                              ; get enemy object buffer offset and leave
    rts
    ; sub from if block above:
    SetupPlatformRope:

    pha                                           ; save Enemy_Y_Speed[ y ]
        mb a := Enemy_X_Position[ y ] + #8
        if !ldx SecondaryHardMode                 ; if secondary hard mode flag set,
            mb a:= a + #$10                       ; add sixteen more pixels
        endif

        pha                                       ; save modified horizontal coordinate to stack
            mb PageLocationRope := Enemy_PageLoc[ y ] + C    ; add carry to page location
        pla                                       ; pull modified horizontal coordinate

        mb temp_byte := a & #%11110000 / 8        ; store result here as part of name table low byte
        ldx Enemy_Y_Position,y                    ; get vertical coordinate
    pla                                           ; restore Enemy_Y_Speed[ y ]

    if negative                                   ; skip this part if moving downwards or not at all
        txa
        mb a := a + #8
        tax                                       ; save as X
    endif

    txa                                           ; move vertical coordinate to A
    ldx VRAM_Buffer1_Offset                       ; get vram buffer offset

    asl
    rol                                           ; rotate d7 to d0 and d6 into carry
    pha                                           ; save modified vertical coordinate to stack
        rol                                       ; rotate carry to d0, thus d7 and d6 are at 2 LSB
        ; mask out all bits but d7 and d6, then set
        ; d5 to get appropriate high byte of name table
        mb NametableHiRope := a & #%00000011 | #%00100000
        mb NametableHiRope := PageLocationRope & #1 << 2 | NametableHiRope

    pla                                           ; get modified vertical coordinate from stack
    mb temp_byte := a & #%11100000 + temp_byte    ; save as name table low byte
    ; if vertical position below the
    ; bottom of the screen
    if Enemy_Y_Position[ y ] >= #$e8
    ; mask out d6 of low byte of name table address
        mb temp_byte := temp_byte & #%10111111
    endif

    rts
    
    ; ------------------------------------------------------------------------------------------------

    InitPlatformFall:

    tya                                           ; move offset of other platform from Y to X
    tax
    jsr GetEnemyOffscreenBits                     ; get offscreen bits

    lda #$06
    jsr SetupFloateyNumber                        ; award 1000 points to player

    mb FloateyNum::X_Pos[ x ] := Player_Rel_XPos  ; put floatey number coordinates where player is
    mb FloateyNum::Y_Pos[ x ] := Player_Y_Position
    mb Enemy_MovingDir  [ x ] := #1               ; set moving direction as flag for

    StopPlatforms:

    jsr InitSpeedAndMoveForce0                    ; initialize vertical speed and low byte
    sta Enemy_Y_Speed,y                           ; for both platforms and leave
    sta Enemy_Y_MoveForce,y

    rts

    PlatformFall:

    tya                                           ; save offset for other platform to stack
    pha
        jsr MoveFallingPlatform                   ; make current platform fall
    pla
    tax                                           ; pull offset from stack and save to X

    jsr MoveFallingPlatform                       ; make other platform fall
    ldx ObjectOffset
    if PlatformCollisionFlag[ x ] == positive     ; if playerstanding on either platform,
        tax                                       ; transfer collision flag offset as offset to X
        jsr PositionPlayerOnVPlat                 ; and position player appropriately
    endif

    ldx ObjectOffset                              ; get enemy object buffer offset and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc YMovingPlatform

    .export ChkYPCollision

    if !Enemy_Y_Speed[ x ] | Enemy_Y_MoveForce[ x ]                    ; if platform moving up or down, skip ahead to

        sta Enemy_YMoveForceFractional,x                     ; initialize fractional

        if Enemy_Y_Position[ x ] < YPlatformTopYPos[ x ]               ; if current vertical position < top position
            if !FrameCounter & #%00000111         ; check for every eighth frame
                inc Enemy_Y_Position,x            ; increase vertical position every eighth frame
            endif
            jmp ChkYPCollision                    ; skip ahead to last part
        endif
    endif

    if Enemy_Y_Position[ x ] >= YPlatformCenterYPos[ x ]
        jsr MovePlatformUp                        ; start slowing descent/moving upwards
    else
        jsr MovePlatformDown                      ; start slowing ascent/moving downwards
    endif

    ChkYPCollision:

    if PlatformCollisionFlag[ x ] == positive     ; if collision flag set here
        jsr PositionPlayerOnVPlat                 ; position player appropriately
    endif
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc XMovingPlatform
    
    ; temp_byte - used as adder to position player hotizontally
    
    .export PositionPlayerOnHPlat

    lda #$0e                                      ; load preset maximum value for secondary counter
    jsr XMoveCntr_Platform                        ; do a sub to increment counters for movement
    jsr MoveWithXMCntrs                           ; do a sub to move platform accordingly, and return value

    if PlatformCollisionFlag[ x ] == positive     ; if collision with player

        PositionPlayerOnHPlat:

        mb Player_X_Position := Player_X_Position + temp_byte          ; player accordingly in horizontal position

        lda Player_PageLoc                        ; get player's page location

        if ldy temp_byte == positive              ; check to see if saved value here is positive or negative
            adc #0                                ; otherwise add carry to page location
        else
            sbc #0                                ; subtract borrow from page location
        endif

        sta Player_PageLoc                        ; save result to player's page location
        sty Platform_X_Scroll                     ; put saved value from second sub here to be used later
        jsr PositionPlayerOnVPlat                 ; position player vertically and appropriately

    endif

    rts                                           ; and we are done here

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DropPlatform

    if PlatformCollisionFlag[ x ] == positive
        jsr MoveDropPlatform                      ; do a sub to move platform down very quickly
        jsr PositionPlayerOnVPlat                 ; do a sub to position player appropriately
    endif
    rts                                           ; leave

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RightPlatform

    ; temp_byte - residual value from sub

    jsr MoveEnemyHorizontally                     ; move platform with current horizontal speed, if any
    sta temp_byte                                 ; store saved value here (residual code) OP

    if PlatformCollisionFlag[ x ] == positive     ; if collision between player

        mb Enemy_X_Speed[ x ] := #$10             ; otherwise set new speed (gets moving if motionless)
        jsr PositionPlayerOnHPlat                 ; use saved value from earlier sub to position player

    endif

    rts                                           ; then leave

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc MoveLargeLiftPlat
    jsr MoveLiftPlatforms                         ; execute common to all large and small lift platforms
    jmp ChkYPCollision                            ; branch to position player correctly
.endproc

.proc MoveSmallPlatform
    jsr MoveLiftPlatforms                         ; execute common to all large and small lift platforms
    jmp ChkSmallPlatCollision                     ; branch to position player correctly
.endproc

.proc MoveLiftPlatforms

    .export ChkSmallPlatCollision

    if !TimerControl

        mb Enemy_YMoveForceFractional[ x ] := Enemy_YMoveForceFractional[ x ] + Enemy_Y_MoveForce[ x ]
        ; add whatever vertical speed is set to current
        ; vertical position plus carry to move up or down
        mb Enemy_Y_Position[ x ] := Enemy_Y_Position[ x ] +c Enemy_Y_Speed[ x ]

        rts

        ChkSmallPlatCollision:

        if PlatformCollisionFlag[ x ]             ; get bounding box counter saved in collision flag
            jsr PositionPlayerOnS_Plat            ; use to position player correctly
        endif

    endif

    rts
    ; then leave
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc OffscreenBoundsCheck

    ; locals
        ExtendedLeftPage      = temp_byte       ; - page location of extended left boundary
        ExtendedLeftBoundary  = temp_byte + 1   ; - extended left boundary position
        ExtendedRightPage     = temp_byte + 2   ; - page location of extended right boundary
        ExtendedRightBoundary = temp_byte + 3   ; - extended right boundary position
    ; end locals
    
    if Enemy_ID[ x ] <> #OBJECTID_FlyingCheepCheep

        lda ScreenLeft_X_Pos                      ; get horizontal coordinate for left side of screen
        ; add 56 pixels to coordinate if hammer bro or piranha plant
        if ldy Enemy_ID[ x ] = #OBJECTID_HammerBro || y = #OBJECTID_PiranhaPlant
            mb a: = a +c #56
        endif
        ; subtract 72 pixels regardless of enemy object
        mb ExtendedLeftBoundary := a -c #$48
        mb ExtendedLeftPage := ScreenLeft_PageLoc - C                       ; store result here
        ; add 72 pixels to the right side horizontal coordinate
        mb ExtendedRightBoundary := ScreenRight_X_Pos +c #$48
        mb ExtendedRightPage := ScreenRight_PageLoc + C       ; and store result here

        lda Enemy_X_Position,x                    ; compare horizontal coordinate of the enemy object
        cmp ExtendedLeftBoundary                  ; to modified horizontal left edge coordinate to get carry
        ; if Enemy_X_Position,x < $01 => carry clear
        ; if Enemy_X_Position,x >= $01 => carry set
        ; then subtract it from the page coordinate of the enemy
        ; if enemy object is too far left, branch to erase it
        if Enemy_PageLoc[ x ] -c ExtendedLeftPage == positive

            lda Enemy_X_Position,x                ; compare horizontal coordinate of the enemy object
            cmp ExtendedRightBoundary             ; to modified horizontal right edge coordinate to get carry
            ; then subtract it from the page coordinate of the enemy object
            ; if enemy object is on the screen or is one of these, leave, do not erase enemy
            ; 2nd compare is for a spiny's egg
            ; the rest, y = Enemy_ID
            if Enemy_PageLoc[ x ] -c ExtendedRightPage == negative \
            || lda Enemy_State[ x ] = #5                           \
            ||                    y = #OBJECTID_PiranhaPlant       \
            ||                    y = #OBJECTID_FlagpoleFlagObject \
            ||                    y = #OBJECTID_StarFlagObject     \
            ||                    y = #OBJECTID_JumpspringObject     goto Exit   ; rts
        endif

        jsr EraseEnemyObject                      ; erase object if necessary

    endif
    Exit:

    rts                                           ; leave

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg
    .byte $ff, $ff, $ff
    
.code

.proc FireballEnemyCollision

    ; locals
        LoopEnemyIndex = temp_byte + 1
    ; end locals
    
    ; check to see if fireball state is set AND d7 in state is clear AND do every other frame OP: use N flag

    if lda Fireball_State[ x ] && a << 1 == carry clear && FrameCounter >> 1 == carry clear

        mb a := x
        mb y := a * 4 + #$1c                      ; use fireball's bounding box coordinates ??

        ldx #4

        repeat                                    ; x = 4 to 0

            stx LoopEnemyIndex                    ; store enemy object offset here

            tya
            pha                                   ; push fireball offset to the stack
                if !Enemy_State[ x ] & #%00100000 && Enemy_Flag[ x ] && ( lda Enemy_ID[ x ] < #$24 || a >= #$2b )
                    ; check for goomba or not in defeated state
                    if a <> #OBJECTID_Goomba || Enemy_State[ x ] < #2
                        if !EnemyOffscrBitsMasked[ x ]
                            mb a := x
                            mb x := a * 4 + #4    ; use enemy's bounding box coordinates
                            jsr SprObjectCollisionCore                 ; do fireball-to-enemy collision detection
                            ldx ObjectOffset      ; return fireball's original offset
                            if carry set          ; if carry set then collision
                                mb Fireball_State[ x ] := #%10000000   ; set d7 in enemy state
                                ldx LoopEnemyIndex                     ; get enemy offset
                                jsr HandleEnemyFBallCol                ; jump to handle fireball to enemy collision
                            endif
                        endif
                    endif

                endif
            pla                                   ; pull fireball offset from stack
            tay                                   ; put it in Y

            ldx LoopEnemyIndex                    ; get enemy object offset

        until dex == negative

    endif

    ldx ObjectOffset                              ; get original fireball offset and leave
    rts

.endproc

dataseg

BowserIdentities:
    .byte OBJECTID_Goomba, OBJECTID_GreenKoopa, OBJECTID_BuzzyBeetle, OBJECTID_Spiny
    .byte OBJECTID_Lakitu, OBJECTID_Bloober, OBJECTID_HammerBro, OBJECTID_Bowser

.code

.proc HandleEnemyFBallCol

    ; locals
        CurrentEnemyOffset = temp_byte + 1
    ; end locals

    .export ShellOrBlockDefeat

    jsr RelativeEnemyPosition                     ; get relative coordinate of enemy

    ldx CurrentEnemyOffset                        ; get current enemy object offset

    if lda Enemy_Flag[ x ] == bit7

        mb x := a & #%00001111                    ; use low nybble as enemy offset
        if Enemy_ID[ x ] = #OBJECTID_Bowser goto HurtBowser                     ; check enemy identifier for bowser
    ; not bowser:
        ldx CurrentEnemyOffset                    ; otherwise retrieve current enemy offset again

    endif
    ; branch if found to leave (buzzy beetles fireproof)
    if lda Enemy_ID[ x ] <> #OBJECTID_BuzzyBeetle

        if a = #OBJECTID_Bowser                            ; check for bowser one more time (necessary if d7 of flag was clear)

            HurtBowser:

            if dec BowserHitPoints != zero goto Exit                   ; decrement bowser's hit points

            jsr InitSpeedAndMoveForce0            ; otherwise do sub to init vertical speed and movement force

            sta Enemy_X_Speed,x                   ; initialize horizontal speed
            sta EnemyFrenzyBuffer                 ; init enemy frenzy buffer
            mb Enemy_Y_Speed[ x ] := #(<-2)       ; set vertical speed to make defeated bowser jump a little

            ldy WorldNumber                       ; use world number as offset
            ; get enemy identifier to replace bowser with
            mb Enemy_ID[ x ] := BowserIdentities[ y ]

            lda #$20                              ; set A to use starting value for state
            if y < #3                             ; check to see if on world 1-3
                ora #3                            ; add 3 to enemy state
            endif

            sta Enemy_State,x                     ; set defeated enemy state
            mb Square2SoundQueue := #SFX_BowserFall                    ; load bowser defeat sound
            ldx CurrentEnemyOffset                ; get enemy offset
            lda #$09                              ; award 5000 points to player for defeating bowser

        else Z clear                              ; not bowser:
            ; anything matched here cannot be affected by fireballs OP: move this with buzzybeetle check?
            if a = #OBJECTID_BulletBill_FrenzyVar || a = #OBJECTID_Podoboo || a >= #$15 goto Exit

            ShellOrBlockDefeat:

            if Enemy_ID[ x ] = #OBJECTID_PiranhaPlant
                mb Enemy_Y_Position[ x ] := Enemy_Y_Position[ x ] +c #$18     ; add 24 pixels to enemy object's vertical position
            endif

            jsr ChkToStunEnemies                  ; do yet another sub
            ; mask out 2 MSB of enemy object's state
            ; set d5 to defeat enemy and save as new state
            mb Enemy_State[ x ] := Enemy_State[ x ] & #%00011111 | #%00100000

            lda #2                                ; award 200 points by default

            if ldy Enemy_ID[ x ] = #OBJECTID_HammerBro
                lda #$06                          ; award 1000 points for hammer bro
            endif
            if y = #OBJECTID_Goomba                        ; check for goomba
                lda #1                            ; award 100 points for goomba
            endif

        endif

        jsr SetupFloateyNumber                    ; update necessary score variables
        mb Square1SoundQueue := #SFX_EnemySmack   ; play smack enemy sound

    endif

    Exit:
    rts                                           ; and now let's leave

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc PlayerHammerCollision
    ; odd frames only
    if FrameCounter >> 1 == carry set && ! TimerControl |  Misc_OffscreenBits
    ; y = x * 4 + 36 : to get proper offset for misc object bounding box coordinates
        mb a := x
        mb y := a * 4 + #$24

        jsr PlayerCollisionCore                   ; do player-to-hammer collision detection
        ldx ObjectOffset                          ; get misc object offset

        if C set                                  ; if collision:
            if Misc_Collision_Flag[ x ] goto Exit                      ; otherwise read collision flag
            mb Misc_Collision_Flag[ x ] := #1     ; otherwise set collision flag now
            mb Misc_X_Speed[ x ] := Misc_X_Speed[ x ] ^ #$ff + #1
            if StarInvincibleTimer goto Exit      ; if star mario invincibility timer set,
            jmp InjurePlayer                      ; otherwise jump to hurt player, do not return
        endif
        mb Misc_Collision_Flag[ x ] := #0         ; clear collision flag
    endif

    Exit:
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc HandlePowerUpCollision
    ; callers: PlayerEnemyCollision

    jsr EraseEnemyObject                          ; erase the power-up object
    lda #$06
    jsr SetupFloateyNumber                        ; award 1000 points to player by default
    
    mb Square2SoundQueue := #SFX_PowerUpGrab                         ; play the power-up sound
    
    if PowerUpType >= #2
        if a = #3 goto SetFor1Up                  ; if 1-up mushroom, branch
        mb StarInvincibleTimer := #$23            ; set star mario invincibility timer, and load the star mario music
        mb AreaMusicQueue := #MUSIC_StarPower     ; into the area music queue, then leave
        rts
    endif ; else
    
    ; Mushroom powerup:
    
    if ! PlayerStatus goto UpToSuper          ; if player status small, make super

    if a <> #1 goto Exit                      ; if player status is big, continue
    
    ; here: Player is Super, not firey
    ldx ObjectOffset                          ; get enemy offset, not necessary
    
    mb PlayerStatus := #2                     ; set player status to fiery
    jsr GetPlayerColors                       ; run sub to change colors of player
    ldx ObjectOffset                          ; get enemy offset again, and again not necessary
    lda #$0c                                  ; set value to be used by subroutine tree (fiery)
    jmp UpToFiery                             ; jump to set values accordingly
    
        SetFor1Up:
        
        mb FloateyNum[ x ]::Control := #$0b                                  ; change 1000 points into 1-up instead and then leave
        rts
        
        
        UpToSuper:
        
        ; Small player found a powerup:
        
        mb PlayerStatus := #1                         ; set player status to super      
        lda #$09                                      ; set value to be used by subroutine tree (super)

    UpToFiery: 
    
    ldy #0                                        ; set value to be used as new player state
    jsr SetPRout                                  ; set values to stop certain things in motion
    Exit: 
    rts
    
.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

ResidualXSpdData:
    .byte $18, $e8

KickedShellXSpdData:
    .byte $30, $d0

DemotedKoopaXSpdData:
    .byte $08, $f8
.code

.proc PlayerEnemyCollision

    if FrameCounter >> 1 == carry set goto HandlePowerUpCollision::Exit        ; check counter for d0 set, exit
    
    ; if player object is on screen vertically AND current enemy is onscreen && set to run player control routine on next frame
    ; && bit 5 is not set in enemy state
    if jsr CheckPlayerVertical == carry clear && !EnemyOffscrBitsMasked[ x ] && GameEngineSubroutine = #8 \
        && !Enemy_State[ x ] & #%00100000

        jsr GetEnemyBoundBoxOfs                   ; get bounding box offset for current enemy object
        jsr PlayerCollisionCore                   ; do collision detection on player vs. enemy

        ldx ObjectOffset                          ; get enemy object buffer offset

        if carry set goto CheckForPUpCollision    ; if collision in PlayerCollisionCore
            ; otherwise, clear d0 of current enemy object's collision bits
        mb Enemy_CollisionBits[ x ] := Enemy_CollisionBits[ x ] & #%11111110 

    endif

    rts
    ; execution here means there is a collision:
    CheckForPUpCollision:

    set_long_branch +,-

    if y := Enemy_ID[ x ] = #OBJECTID_PowerUpObject goto HandlePowerUpCollision

    set_long_branch -,-

    if !StarInvincibleTimer goto HandlePECollisions
    ; invincible star active:
    jmp ShellOrBlockDefeat                        ; kill as if hit with a shell, or from beneath

.endproc


dataseg

KickedShellPtsData:
    .byte $0a, $06, $04
.code


.proc HandlePECollisions

    ; nothing actually calls this except for branch at the end of previous proc
    ; check enemy collision bits for d0 set, or for being offscreen at all

    if !Enemy_CollisionBits[ x ] & #%00000001 | EnemyOffscrBitsMasked[ x ]

        mb Enemy_CollisionBits[ x ] := #1 | Enemy_CollisionBits[ x ]

        if y = #OBJECTID_Spiny                           goto ChkForPlayerInjury
        if y = #OBJECTID_PiranhaPlant || y = #OBJECTID_Podoboo    goto InjurePlayer
        if y = #OBJECTID_BulletBill_CannonVar            goto ChkForPlayerInjury
        if y >= #$15 || lda AreaType == zero    goto InjurePlayer
        ; branch if d7 of enemy state was set OR branch if enemy is in normal or falling state
        if Enemy_State[ x ] << 1 == carry set || Enemy_State[ x ] & #%00000111 < #2 goto ChkForPlayerInjury         ; OP

        if Enemy_ID[ x ] <> #OBJECTID_Goomba               ; If not a defeated goomba:


            mb Square1SoundQueue := #SFX_EnemySmack                    ; play smack enemy sound
            mb Enemy_State[ x ]  := Enemy_State[ x ] | #%10000000

            jsr EnemyFacePlayer                   ; set moving direction and get offset

            mb Enemy_X_Speed[ x ] := KickedShellXSpdData[ y ]          ; load and set horizontal speed data with offset
            mb a := #3 + StompChainCounter        ; add three to whatever the stomp counter contains

            if ldy EnemyIntervalTimer[ x ] < #3   ; check shell enemy's timer
                lda KickedShellPtsData , y        ; set points based on proximity to timer expiration
            endif

            jsr SetupFloateyNumber                ; set values for floatey number now
        endif
    endif

    rts

.endproc

.proc ChkForPlayerInjury

    .export InjurePlayer

    if Player_Y_Speed == positive && not zero goto EnemyStomped
    ; if enemy object >= $07
    if Enemy_ID[ x ] >= #OBJECTID_Bloober
        ; if this player's position above enemy's on screen:
        if Player_Y_Position + #12 < Enemy_Y_Position[ x ] goto EnemyStomped
    endif

    if StompTimer goto EnemyStomped
    
    ; check to see if injured invincibility timer still counting down
    if InjuryTimer goto * + 63 ; forward branch to InjurePlayer::Exit 
    ; if no InjuryTimer, turn enemy to face player:
    if Player_Rel_XPos >= Enemy_Rel_XPos          ; if player's relative position to the right of enemy's
        jmp ChkEnemyFaceRight
    endif
    ; else player is left of enemy:
    if Enemy_MovingDir[ x ] = #1                  ; if enemy moving towards the right
        jmp TurnEnemy
    endif
    ; After ChkEnemyFaceRight, TurnEnemy execution returns here:

.endproc

.proc InjurePlayer                                ; many routines jump here

    .export ForceInjury, SetPRout

    if InjuryTimer goto Exit                      ; check again injured invincibility timer

    ForceInjury:

    if ldx PlayerStatus                           ; if not small

        sta PlayerStatus                          ; set player's status to small

        mb InjuryTimer := #$08                    ; set injured invincibility timer
        mb Square1SoundQueue :=  a << 1           ; 4   ; play pipedown/injury sound

        jsr GetPlayerColors                       ; change player's palette if necessary
        lda #$0a                                  ; set subroutine to run on next frame

        SetKRout:

        ldy #1                                    ; set new player state
        SetPRout:

        sta GameEngineSubroutine                  ; $0a, or $0b         ; load new value to run subroutine on next frame
        sty Player_State                          ; 1          ; store new player state

        mb y, TimerControl := #$ff                ; set master timer control flag to halt timers
        iny
        mb ScrollAmount := y                      ; initialize scroll speed

        Exit:

        ldx ObjectOffset                          ; get enemy offset and leave
        rts

    endif 
      ; else: Small Mario, x = 0 here
      
      stx Player_X_Speed                          ; halt player's horizontal movement by initializing speed
      inx
      stx EventMusicQueue                         ; set event music queue to death music
      mb Player_Y_Speed := #$fc                   ; set new vertical speed
      lda #$0b                                    ; set subroutine to run on next frame
      bne SetKRout                                ; unconditional branch to set up mario's death
    ; end

.endproc

dataseg

StompedEnemyPtsData:
    .byte $02, $06, $05, $06

.code

.proc EnemyStomped

    if Enemy_ID[ x ] = #OBJECTID_Spiny goto InjurePlayer

    mb Square1SoundQueue := #SFX_EnemyStomp       ; otherwise stomp/swim sound

    lda Enemy_ID,x

    ldy #0                                        ; initialize points data offset for stomped enemies
    if a = #OBJECTID_FlyingCheepCheep     \
    || a = #OBJECTID_BulletBill_FrenzyVar \
    || a = #OBJECTID_BulletBill_CannonVar \
    || a = #OBJECTID_Podoboo goto EnemyStompedPts          ; OP: remove podooboo?

        iny                                       ; 1 
        if a = #OBJECTID_HammerBro  goto EnemyStompedPts
        iny                                       ; 2 
        if a = #OBJECTID_Lakitu     goto EnemyStompedPts
        iny                                       ; 3 
        if a <> #OBJECTID_Bloober   goto ChkForDemoteKoopa

    EnemyStompedPts:

        lda StompedEnemyPtsData,y                 ; load points data using offset in Y
        jsr SetupFloateyNumber                    ; run sub to set floatey number controls

        lda Enemy_MovingDir,x
        pha                                       ; save enemy movement direction to stack
            jsr SetStun                           ; run sub to kill enemy
        pla
        sta Enemy_MovingDir,x                     ; return enemy movement direction from stack


        mb Enemy_State[ x ] := #%00100000         ; set d5 in enemy state
        jsr InitSpeedAndMoveForce0                ; nullify vertical speed, physics-related thing,
        mb Enemy_X_Speed[ x ] := a                ; and horizontal speed

        mb Player_Y_Speed := #(<-3)               ; set player's vertical speed, to give bounce
        rts

    ChkForDemoteKoopa:
    ; no matches found in enemy list above:

    if a < #$09 goto HandleStompedShellEnemy

    mb Enemy_ID[ x ] := a & #%00000001            ; demote koopa paratroopas to ordinary troopas

    mb y, Enemy_State[ x ] := #0                  ; return enemy to normal state
    lda #3                                        ; award 400 points to the player
    jsr SetupFloateyNumber
    jsr InitSpeedAndMoveForce0                    ; nullify physics-related thing and vertical speed
    jsr EnemyFacePlayer                           ; turn enemy around if necessary

    mb Enemy_X_Speed[ x ] := DemotedKoopaXSpdData[ y ]                 ; set appropriate moving speed based on direction
    jmp SetVSpeedBounce                           ; then move onto something else

.endproc

dataseg

RevivalRateData:
    .byte $10, $0b

.code

.proc HandleStompedShellEnemy

    .export SetVSpeedBounce

    mb Enemy_State[ x ] := #4                     ; set defeated state for enemy

    inc StompChainCounter                         ; increment the stomp counter

    mb a := StompChainCounter + StompTimer

    jsr SetupFloateyNumber                        ; award points accordingly
    inc StompTimer                                ; increment stomp timer of some sort

    ldy PrimaryHardMode                           ; check primary hard mode flag

    mb EnemyIntervalTimer[ x ] := RevivalRateData[ y ]                 ; set as enemy timer to revive stomped enemy

    SetVSpeedBounce:

    mb Player_Y_Speed := #$fc                     ; set player's vertical speed for bounce    
    rts
    
.endproc

.proc ChkEnemyFaceRight

    .export TurnEnemy

    set_long_branch +
    if Enemy_MovingDir[ x ] = #1 goto InjurePlayer ; check to see if enemy is moving to the right
    set_long_branch -


    TurnEnemy:

    jsr EnemyTurnAround                           ; turn the enemy around, if necessary
    jmp InjurePlayer                              ; go back to hurt player

.endproc



.proc EnemyFacePlayer

    ldy #1                                        ; set to move right by default
    if jsr PlayerEnemyDiff == negative            ; get horizontal difference between player and enemy
        iny                                       ; if enemy is to the LEFT
    endif

    mb Enemy_MovingDir[ x ] := y                  ; set moving direction here
    dey                                           ; then decrement to use as a proper offset
    rts

.endproc

.proc SetupFloateyNumber
    ; set number of points control for floatey numbers
    mb FloateyNum[ x ]::Control := a
    mb FloateyNum_Timer[ x ]  := #$30             ; set timer for floatey numbers
    mb FloateyNum[ x ]::Y_Pos := Enemy_Y_Position[ x ]                 ; set vertical coordinate
    mb FloateyNum[ x ]::X_Pos := Enemy_Rel_XPos   ; set horizontal coordinate and leave

    Exit:

    rts

.endproc


dataseg

SetBitsMask:
    .byte %10000000, %01000000, %00100000, %00010000, %00001000, %00000100, %00000010

ClearBitsMask:
    .byte %01111111, %10111111, %11011111, %11101111, %11110111, %11111011, %11111101

.code

.proc EnemiesCollision

    ; locals
        SecondEOffset = temp_byte + 1
    ; end locals
    
    ; If even frame OR area is water, branch and RTS
    if FrameCounter >> 1 == C clear || AreaType == zero goto SetupFloateyNumber::Exit       ; if water area type, leave

    if lda Enemy_ID[ x ] < #$15 && ! a = #OBJECTID_Lakitu && ! a = #OBJECTID_PiranhaPlant \
    && !EnemyOffscrBitsMasked[ x ] && jsr GetEnemyBoundBoxOfs : dex == positive     ; ( last condition means x >= 1 )

        repeat

            stx SecondEOffset                     ; save enemy object buffer offset for second enemy here

            tya                                   ; save first enemy's bounding box offset to stack
            pha
                if Enemy_Flag[ x ] && lda Enemy_ID[ x ] < #$15 && ! a = #OBJECTID_Lakitu \
                   && ! a = #OBJECTID_PiranhaPlant && !EnemyOffscrBitsMasked[ x ]
                    ; get second enemy object's bounding box offset
                    mb a := x
                    mb x := a * 4 + #4

                    jsr SprObjectCollisionCore    ; do collision detection using the two enemies here

                    ldx ObjectOffset              ; use first enemy offset for X
                    ldy SecondEOffset             ; use second enemy offset for Y

                    if carry set                  ; if carry set, collision
                        if ! Enemy_State[ x ] | Enemy_State[ y ] & #$80                ; check both enemy states for d7 set
                            if Enemy_CollisionBits[ y ] & SetBitsMask[ x ] goto NextEnemy
                            ; make sure the bits are set... ?
                            mb Enemy_CollisionBits[ y ] := Enemy_CollisionBits[ y ] | SetBitsMask[ x ]
                        endif

                        jsr ProcEnemyCollisions   ; react according to the nature of collision

                    else                          ; no collision:
                        mb Enemy_CollisionBits[ y ] := Enemy_CollisionBits[ y ] & ClearBitsMask[ x ]
                    endif

                endif

                NextEnemy:
            pla                                   ; get first enemy's bounding box offset from the stack
            tay                                   ; use as Y again

            ldx SecondEOffset                     ; get and decrement second enemy's object buffer offset

        until dex == negative

    endif

    ldx ObjectOffset                              ; get enemy object buffer offset
    rts                                           ; leave
    
    ; ------------------------------------------------------------------------------------------------

    ProcEnemyCollisions:

    if !Enemy_State[ y ] | Enemy_State[ x ] & #%00100000              ; if d5 clear in both

        if Enemy_State[ x ] < #$06 goto ProcSecondEnemyColl

        if Enemy_ID[ x ] <> #OBJECTID_HammerBro            ; if hammer bro found in alt state, branch to leave

            if Enemy_State[ y ] << 1 == C set     ; OP : use N flag
                lda #$06
                jsr SetupFloateyNumber            ; award 1000 points for killing enemy
                jsr ShellOrBlockDefeat            ; then kill enemy, then load
                ldy SecondEOffset                 ; original offset of second enemy
            endif
            ; shells
            tya                                   ; move Y to X
            tax

            jsr ShellOrBlockDefeat                ; kill second enemy
            ldx ObjectOffset

            mb a := ShellChainCounter[ x ] + #4   ; get chain counter for shell

            ldx SecondEOffset
            jsr SetupFloateyNumber                ; award appropriate number of points for second enemy
            ldx ObjectOffset                      ; load original offset of first enemy
            inc ShellChainCounter, x              ; increment chain counter for additional enemies
        endif
    endif

    Exit:
    rts

    ProcSecondEnemyColl:

    if Enemy_State[ y ] >= #$06                   ; if first enemy state < $06, branch elsewhere

        if Enemy_ID[ y ] = #OBJECTID_HammerBro goto Exit   ; if hammer bro found in alt state, branch to leave

        jsr ShellOrBlockDefeat                    ; otherwise, kill first enemy
        ldy SecondEOffset
        mb a := ShellChainCounter[ y ] + #4       ; get chain counter for shell

        ldx ObjectOffset
        jsr SetupFloateyNumber                    ; award appropriate number of points for first enemy
        ldx SecondEOffset                         ; load original offset of second enemy
        inc ShellChainCounter, x                  ; increment chain counter for additional enemies
        rts

    endif

    tya                                           ; move Y (SecondEOffset) to X
    tax
    jsr EnemyTurnAround                           ; do the sub here using value from Second
    ldx ObjectOffset                              ; then do it again using value from ObjOffset
    ; fall through to next proc
.endproc

.proc EnemyTurnAround

    .export ForceEnemyTurnAround

    if  lda Enemy_ID[ x ] <> #OBJECTID_PiranhaPlant && a <> #OBJECTID_Lakitu && a <> #OBJECTID_HammerBro && \
        ( a = #OBJECTID_Spiny || a = #OBJECTID_GreenParatroopaJump || a < #$07 )
        ; get two's compliment for horizontal speed:
        ForceEnemyTurnAround:
        mb y := Enemy_X_Speed[ x ] ^ #$ff
        mb Enemy_X_Speed    [ x ] := y + 1        ; store as new horizontal speed
        mb Enemy_MovingDir  [ x ] := Enemy_MovingDir[ x ] ^ #%00000011         ; invert direction
    endif

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc LargePlatformCollision

    ; locals
        PlatformVPos = temp_byte
    ; end locals

    mb PlatformCollisionFlag[ x ] := #$ff

    if !TimerControl && Enemy_State[ x ] == bit7 clear

        if Enemy_ID[ x ] = #$24
            mb a, x := Enemy_State[ x ]           ; set state as enemy offset here
            jsr ChkForPlayerC_LargeP              ; perform code with state offset, then original offset, in X
        endif

        ChkForPlayerC_LargeP:

        if jsr CheckPlayerVertical == C clear     ; figure out if player is below a certain point

            txa
            jsr GetEnemyBoundBoxOfsArg            ; get bounding box offset in Y

            mb PlatformVPos := Enemy_Y_Position[ x ]                   ; temp variable for now

            txa                                   ; send offset we're on to the stack
            pha
                jsr PlayerCollisionCore           ; do player-to-platform collision detection
            pla                                   ; retrieve offset from the stack
            tax

            if C set
                jsr ProcLPlatCollisions           ; otherwise collision, perform sub
            endif
        endif
    endif

    ldx ObjectOffset                              ; get enemy object buffer offset and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------


.proc SmallPlatformCollision

    ; locals
        ; temp_byte - counter for bounding boxes
    ; end locals

    if !TimerControl

        sta PlatformCollisionFlag,x               ; otherwise initialize collision flag

        if jsr CheckPlayerVertical == C clear     ; If player onscreen

            mb temp_byte := #2                    ; load counter here for 2 bounding boxes

            repeat

                ldx ObjectOffset                  ; get enemy object offset
                jsr GetEnemyBoundBoxOfs           ; get bounding box offset in Y

                if and #%00000010 goto Exit

                if BoundingBox_UL_YPos[ y ] >= #$20                    ; check top of platform's bounding box
                    if jsr PlayerCollisionCore == C set goto ProcSPlatCollisions     ; skip ahead if collision
                endif
                ; move bounding box vertical coordinates 128 pixels downwards
                mb BoundingBox_UL_YPos[ y ] := BoundingBox_UL_YPos[ y ] + #$80
                mb BoundingBox_DR_YPos[ y ] := BoundingBox_DR_YPos[ y ] + #$80

            until dec temp_byte == zero           ; loop back until both bounding boxes are checked
        endif
    endif
    Exit:

    ldx ObjectOffset                              ; get enemy object buffer offset, then leave
    rts

    ProcSPlatCollisions:

    ldx ObjectOffset                              ; return enemy object buffer offset to X, then continue

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ProcLPlatCollisions

    ; get difference by subtracting the top
    ; of the player's bounding box from the bottom
    ; of the platform's bounding box,
    ; check if it is less than 4 AND check to see if player's vertical speed is upward
    ; Check for bottom collision:

    if BoundingBox_DR_YPos[ y ] - BoundingBox_UL_YPos < #4 && Player_Y_Speed == negative
        mb Player_Y_Speed := #1                   ; set vertical speed of player to kill jump
    endif
    
    ; Check for top collision:
    ; get difference by subtracting the top of the platform's bounding box from the bottom
    ; of the player's bounding box.  if difference close enough AND if player's vertical speed downward
    
    ; player                ; platform
    if BoundingBox_DR_YPos - BoundingBox_UL_YPos[ y ] < #6 && Player_Y_Speed == positive

        lda temp_byte                             ; get saved bounding box counter from earlier
        ; if platform is not one of the two small platform objects
        if ldy Enemy_ID[ x ] <> #$2b && y <> #$2c
            txa                                   ; use enemy object buffer offset for collision flag
        endif
        ; Set collision flag:

        ldx ObjectOffset                          ; get enemy object buffer offset
        mb PlatformCollisionFlag[ x ] := a        ; save either bounding box counter or enemy offset here
        mb Player_State := #0                     ; set player state to normal then leave
        rts
    endif
    
    ; Platform Side Collisions:
    ; set value here to indicate possible horizontal collision on left side of platform

    mb temp_byte := #1                            ; hit left side by default
    ; get difference by subtracting platform's left edge for player's right edge,
    ;
    if BoundingBox_DR_XPos - BoundingBox_UL_XPos[ y ] < #8 goto SideCollision            ; if we hit the left side:
        ; no side collison yet:
       inc temp_byte                              ; increment value set here for possible right side of object collision
        ; get difference by subtracting player's left edge from platform's right edge
      
        ; error? clc here instead of sec?
        if BoundingBox_DR_XPos[ y ] : clc : a -c BoundingBox_UL_XPos < #9                 ; if we hit the right side:

            SideCollision:
            jsr ImpedePlayerMove                  ; deal with horizontal collision

        endif
    ; No Side Collision:

    ldx ObjectOffset                              ; return with enemy object buffer offset
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

PlayerPosSPlatData:
    .byte $80, $00

.code

.proc PositionPlayerOnS_Plat

    tay                                           ; use bounding box counter saved in collision flag
    ; add positioning data using offset to the vertical coord
    mb a := Enemy_Y_Position[ x ] + PlayerPosSPlatData[ y - 1 ]
    BIT_Skip2                                     ; BIT instruction opcode
    ; fall through and skip first instruction
.endproc

.proc PositionPlayerOnVPlat

    lda Enemy_Y_Position,x                        ; get vertical coordinate
    ; if certain routine NOT being executed on this frame AND if vertical high byte = onscreen
    if ldy GameEngineSubroutine <> #$0b && ldy Enemy_Y_HighPos[ x ] = #1
        ; subtract 32 pixels from players vertical coordinate for the player object's height
        mb Player_Y_Position := a - #$20

        mb a := y                                 ; a := Enemy_Y_HighPos[ x ]
        ; subtract borrow and store as player's new vertical high byte
        mb Player_Y_HighPos := a - C

        lda #0
        mb Player_Y_Speed     := a                ; initialize vertical speed and low byte of force
        mb Player_Y_MoveForce := a                ; and then leave
    endif

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc CheckPlayerVertical
    ; if player object is NOT completely offscreen vertically AND
    ; if player high vertical byte  = 1
    if Player_OffscreenBits < #$f0 && y := Player_Y_HighPos - 1 == zero
        lda Player_Y_Position                     ; if on the screen, check to see how far down
        cmp #$d0                                  ; the player is vertically
    endif

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc GetEnemyBoundBoxOfs

    .export GetEnemyBoundBoxOfsArg

    lda ObjectOffset                              ; get enemy object buffer offset

    GetEnemyBoundBoxOfsArg:
    ; to skip player's bounding box:
    mb y := a * 4 + #4

    lda Enemy_OffscreenBits                       ; get offscreen bits for enemy object
    and #%00001111                                ; save low nybble
    cmp #%00001111                                ; check for all bits set

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------
    
dataseg

PlayerBGUpperExtent:
    .byte $20, $10

.code

.proc PlayerBGCollision

    ; locals
        ; temp_byte, 
        ; temp_byte + 1                         ; - used to hold many values, essentially temp variables
        BBufferYPosLoNybble = temp_byte + 4     ; - holds lower nybble of vertical coordinate from block buffer routine
        BlockBufferPointer =  $eb               ; - used to hold block buffer adder
    ; end locals

    ; if collision detection disabled flag set,
    if !DisableCollisionDet && lda GameEngineSubroutine <> #$0b && a >= #4

        lda #1                                    ; load default player state for swimming
        if !ldy SwimmingFlag                      ; if swimming flag not set,
        ; if player state not zero && not climbing
            if Player_State && a <> #3 goto CheckOnScreen
            lda #2                                ; load default player state for falling
        endif

        sta Player_State                          ; set whatever player state is appropriate

        CheckOnScreen:
        ; check player's vertical high byte for still on the screen
        if Player_Y_HighPos = #1
            mb Player_CollisionBits := #$ff       ; initialize player's collision flag
            if Player_Y_Position < #$cf goto CheckCollisionSize        ; if high enough to bump head
        endif
    endif

    rts
    
    ; ------------------------------------------------------------------------------------------------

    CheckCollisionSize:

    ldy #2                                        ; load default offset

    if !CrouchingFlag && PlayerSize == BigMario
        dey                                       ; 1
        if !SwimmingFlag
            dey                                   ; 0
        endif
    endif

    mb BlockBufferPointer := BlockBufferAdderData[ y ]           ; get value using offset
    tay                                           ; put value into Y, as offset for block buffer routine

    ldx PlayerSize                                ; get player's size as offset

    if CrouchingFlag
        inx                                       ; otherwise increment size as offset
    endif
    
    ; Head Check: ----------------------------------------------------------------------
    ; if player is low enough to hit head based on big vs small/crouching
    if Player_Y_Position >= PlayerBGUpperExtent[ x ]

        if jsr BlockBufferCollideHead == not zero                      ; do player-to-bg collision detection on top of player

            if jsr CheckForCoinMTiles == C set goto AwardTouchedCoin   ; check to see if player touched coin with their head

            if ldy Player_Y_Speed == negative && ldy BBufferYPosLoNybble >= #4         ; if player moving upwards
            ; AND check lower nybble of vertical coordinate returned from collision detection routine

                if jsr CheckForSolidMTiles == C clear                  ; check to see what player's head bumped on
                ; execution here: not solid or climbable object
                ; if water area or blockbounce timer not expired:
                    if ldy AreaType == zero || ldy BlockBounceTimer goto NullYSpeed         ; otherwise check area type

                    jsr PlayerHeadCollision       ; otherwise do a sub to process collision

                else                              ; Solid Or Climbable object:
                    if a <> #$26                  ; if not a climbing metatile:
                        mb Square1SoundQueue := #SFX_Bump
                    endif

                    NullYSpeed:

                    mb Player_Y_Speed := #1       ; set vertical speed to nullify jump or swim
                endif
            endif
        endif
    endif
    
    ; Check feet for Collision ----------------------------------------------------------

    ldy BlockBufferPointer                                       ; get block buffer adder offset

    if Player_Y_Position < #$cf                   ; if player high enough on the screen
        ; do player-to-bg collision detection on bottom left of player
        ; and check to see if player touched coin with their left foot
        if jsr BlockBufferCollideFeet : jsr CheckForCoinMTiles == C set goto AwardTouchedCoin        ; jump to handle coin
        ; execution here, no coin found yet, check other foot:
        pha                                       ; save bottom left metatile to stack
            jsr BlockBufferCollideFeet            ; do player-to-bg collision detection on bottom right of player
            sta temp_byte                         ; save bottom right metatile here
        pla
        sta $01                                   ; pull bottom left metatile and save here

        if zero                                   ; if bottom left metatile is zero, check bottom right for anything
            if !temp_byte goto DoPlayerSideCheck                       ; bottom right is zero too, leave this section, Check sides
            if jsr CheckForCoinMTiles == C set    ; check to see if player touched coin with their right foot
                AwardTouchedCoin:
                jmp HandleCoinMetatile            ; follow the code to erase coin and award to player 1 coin
            endif
        endif
        
    ; ------------------------------------------------------------------------------------------------
    
        ; End initial part of feet: at this point, we checked if there is a coin at either foot and jumped if so.
        ; If no coin, execution here means we found something else at player's feet:
        ; check to see if player landed on climbable metatiles OR Player moving UP

        if jsr CheckForClimbMTiles == C set || ldy Player_Y_Speed == negative goto DoPlayerSideCheck

        if a = #$c5                               ; check for axe
            jmp HandleAxeMetatile
        endif
        ; equal means true (found)
        if jsr ChkInvisibleMTiles == equal goto DoPlayerSideCheck      ; do sub to check for hidden coin or 1-up blocks

        if !ldy JumpspringAnimCtrl                ; if jumpspring NOT animating right now
            ; check lower nybble of vertical coordinate returned from collision detection routine
            if ldy BBufferYPosLoNybble >= #5
                mb temp_byte := Player_MovingDir                       ; use player's moving direction as temp variable
                jmp ImpedePlayerMove              ; jump to impede player's movement in that direction
            endif

            jsr ChkForLandJumpSpring              ; do sub to check for jumpspring metatiles and deal with it
            ; mask out lower nybble of player's vertical position
            mb Player_Y_Position := #$f0 & Player_Y_Position
            jsr HandlePipeEntry                   ; do sub to process potential pipe entry
            lda #0
            mb Player_Y_Speed      := a           ; initialize vertical speed and fractional
            mb Player_Y_MoveForce  := a           ; movement force to stop player's vertical movement
            mb StompChainCounter   := a           ; initialize enemy stomp counter
        endif
        mb Player_State := #0                     ; set player's state to normal
    endif
    
    ; Check sides for collision: ------------------------------------------------------

    DoPlayerSideCheck:
    ; increment b. buffer offset 2 bytes to use adders for side collisions
    mb y := BlockBufferPointer + 2
    mb temp_byte := #2                            ; set value here to be used as counter

    repeat
        mb BlockBufferPointer := y + 1
        ; top half of player ..?

        if Player_Y_Position >= #$20              ; if player is below status bar:

            if a >= #$e4 break                    ; rts            ; player too far down the screen
            ; do player-to-bg collision detection on one half of player
            ; if collided with 'something' and not a sideways pipe (top) and not a water pipe (top)
            if jsr BlockBufferCollideSide && a <> #$1c && a <> #$6b
                ; if player didn't bump anything climbable, branch
                if jsr CheckForClimbMTiles == C clear goto CheckSideMTiles
            endif
        endif
        
        ; Bottom Half:

        mb y := BlockBufferPointer + 1
        ; if too high or low on the screen, exit loop
        if lda Player_Y_Position < #8 || a >= #$d0 break               ; rts
        ; if something found, branch
        if jsr BlockBufferCollideSide goto CheckSideMTiles

    until dec temp_byte == zero                   ; run code until both sides of player are checked

    rts                                           ; leave

    CheckSideMTiles:
    ; check for hidden or coin 1-up blocks
    if jsr ChkInvisibleMTiles == not equal

        if jsr CheckForClimbMTiles == C set       ; check for climbable metatiles
            jmp HandleClimbing
        endif
        ; check to see if player touched coin
        if jsr CheckForCoinMTiles == C set goto HandleCoinMetatile
        ; check for jumpspring metatiles
        if jsr ChkJumpspringMetatiles == C set
            if JumpspringAnimCtrl goto Exit       ; rts                     ; leave if not expired: jumpspring animation control
            jmp StopPlayerMove                    ; otherwise jump to impede player's movement
        endif
            ; if player's state is normal ( not swimming, not falling, not climbing)
            ; AND if facing dir = 1 (right)
            ; AND collided with either kind of sideways pipe
            ; OP: remove compare to zero
            if ldy Player_State = #0 && y := PlayerFacingDir - 1 == zero && ( a = #$6c || a = #$1f )

                if !Player_SprAttrib              ; check player's attributes
                    mb y, Square1SoundQueue := #SFX_PipeDown_Injury    ; load pipedown/injury sound if attribs clear
                endif
                ; set background priority bit in player attributes
                mb Player_SprAttrib := a | #%00100000

                if Player_X_Position & #%00001111
                    ldy #0                        ; set default offset for timer setting data
                    if ScreenLeft_PageLoc         ; load page location for left side of screen
                        iny                       ; increment offset if not zero
                    endif
                    mb ChangeAreaTimer := AreaChangeTimerData[ y ]     ; set timer for change of area as appropriate
                endif
                ; if running player entrance routine or
                ; NOT player control routine, leave
                if GameEngineSubroutine = #7 || a <> #8 goto Exit      ; rts

                mb GameEngineSubroutine := #2     ; otherwise set sideways pipe entry routine to run
                rts                               ; and leave
            endif                                 ; else

        StopPlayerMove:
        jsr ImpedePlayerMove                      ; stop player's movement

    endif
    Exit:
    rts                                           ; leave

.endproc
    
    ; ------------------------------------------------------------------------------------------------
    
dataseg

AreaChangeTimerData:
    .byte $a0, $34

.code

.proc HandleCoinMetatile

    jsr EraseCoinMetaTile                         ; do sub to erase coin metatile from block buffer
    inc CoinTallyFor1Ups                          ; increment coin tally used for 1-up blocks
    jmp GiveOneCoin                               ; update coin amount and tally on the screen

.endproc

.proc HandleAxeMetatile

    mb OperMode_Task := #0                        ; reset secondary mode
    mb OperMode := #2                             ; set primary mode to autoctrl mode
    mb Player_X_Speed := #$18                     ; set horizontal speed and continue to erase axe metatile
    ; continue and erase as if a coin:
.endproc

.proc EraseCoinMetaTile

    ; locals    
        BBufferYPosHiNybble = temp_byte + 2     ; - high nybble of vertical coordinate from block buffer
        BlockBufferPointer    = temp_byte + 6     ; - block buffer address
    ; end locals

    ldy BBufferYPosHiNybble                                       ; load vertical high nybble offset for block buffer
    mb (BlockBufferPointer)[ y ] := #0                           ; load blank metatile store to remove old contents from block buffer
    jmp RemoveCoin_Axe                            ; update the screen accordingly

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

ClimbXPosAdder:
    .byte <-7, 7                                  ; place player either 7 pixels to the left or to the right of the vine

ClimbPLocAdder:                                   ; sub 1 or do nothing depending on side of vine
    .byte <-1, 0

FlagpoleYPosData:
    .byte $18, $22, $50, $68, $90                 ; Y coords that correspond to point zones

.code

.proc HandleClimbing

    ; locals    
        BBufferYPosHiNybble = temp_byte + 2     ; - high nybble of vertical coordinate from block buffer
        BBufferXPosLoNybble = temp_byte + 4     ; - low nybble of horizontal coordinate from block buffer
        BlockBufferPointer  = temp_byte + 6     ; - block buffer address
    ; end locals


    ; check low nybble of horizontal coordinate returned from collision detection
    ; routine against certain values, this makes actual physical part of vine or flagpole thinner than 16 pixels
    
    if ldy BBufferXPosLoNybble < #6 || y >= #10
        rts                                       ; leave if too far left or too far right
    endif
    ; else continue :

    ; if found flagpole ball or flagpole shaft:
    if a = #$24 || a = #$25

        if GameEngineSubroutine = #$05 goto PutPlayerOnVine            ; check for end-of-level routine running

        mb PlayerFacingDir := #1                  ; set player's facing direction to right
        inc ScrollLock                            ; set scroll lock flag
        ; check for flagpole slide routine NOT already running
        if GameEngineSubroutine <> #4

            lda #OBJECTID_BulletBill_CannonVar    ; load identifier for bullet bills (cannon variant)
            jsr KillEnemies                       ; get rid of them
            mb EventMusicQueue := #MUSIC_Silence                       ; silence music
            mb FlagpoleSoundQueue := a >> 1       ; load flagpole sound into flagpole sound queue

            ldx #4                                ; start at end of vertical coordinate data
            mb FlagpoleCollisionYPos := Player_Y_Position    ; store player's vertical coordinate here to be used later

            repeat                                ; .. until all data is checked (use last one if all checked)
            until a >= FlagpoleYPosData[ x ] || dex == zero

            stx FlagpoleScore                     ; store offset here to be used later
        endif
        mb GameEngineSubroutine := #4             ; set value to run flagpole slide routine

    elseif a = #$26 && Player_Y_Position < #$20
        ; if vine climbing metatile AND player in status bar area:
        mb GameEngineSubroutine := #1             ; set to run autoclimb routine next frame
    endif

    PutPlayerOnVine:

    mb Player_State   := #3                         ; set player state to climbing
    mb Player_X_Speed := #0                       ; nullify player's horizontal speed and fractional horizontal movement force
    mb Player_X_MoveForce := a                    ; 0
    ; if < 16 pixels difference, alter facing direction
    if Player_X_Position - ScreenLeft_X_Pos < #$10
        mb PlayerFacingDir := #2                  ; force player to face left
    endif

    ldy PlayerFacingDir                           ; get current facing direction, use as offset

    mb Player_X_Position := BlockBufferPointer << 4 + ClimbXPosAdder[ y - 1 ]         ; add/ sub pixels depending on facing direction
    ; if low byte page location zero, adjust page location
    if ! lda BlockBufferPointer
        mb Player_PageLoc := ScreenRight_PageLoc + ClimbPLocAdder[ y - 1 ]
    endif

    rts                                           ; finally, we're done!

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ChkInvisibleMTiles

    if a <> #$5f                                  ; check for hidden coin block
        cmp #$60                                  ; check for hidden 1-up block
    endif

    rts                                           ; leave with zero flag set if either found

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ChkForLandJumpSpring

    if jsr ChkJumpspringMetatiles == C set        ; do sub to check if player landed on jumpspring
        mb VerticalForce      := #$70             ; otherwise set vertical movement force for player
        mb JumpspringForce    := #(<-7)           ; set default jumpspring force
        mb JumpspringTimer    := #3               ; set jumpspring timer to be used later
        mb JumpspringAnimCtrl := a >> 1           ; #1                      ; set jumpspring animation control to start animating
    endif
    rts

.endproc

.proc ChkJumpspringMetatiles

    if a <> #$67                                  ; check for top jumpspring metatile
        cmp #$68                                  ; check for bottom jumpspring metatile
        clc                                       ; clear carry flag
        if not equal goto NoJumpSpring            ; branch to use cleared carry if not found
    endif
    sec                                           ; set carry if found
    NoJumpSpring:
    rts                                           ; leave

.endproc


.proc HandlePipeEntry

    ; if pressing down AND right foot = warp pipe right AND left foot = warp pipe left
    if Up_Down_Buttons & #%00000100 && temp_byte = #$11 && lda $01 = #$10

        mb ChangeAreaTimer      := #$30           ; set timer for change of area
        mb GameEngineSubroutine := #3             ; set to run vertical pipe entry routine on next frame
        mb Square1SoundQueue    := #SFX_PipeDown_Injury                ; load pipedown/injury sound
        mb Player_SprAttrib     := #%00100000     ; set background priority bit in player's attributes

        if lda WarpZoneControl
            ; calc offset to warp zone numbers (starts at left pipe)
            mb x := a & #%00000011 * 4
            ; if player farther right than this past the first pipe
            if lda Player_X_Position >= #$60
                inx                               ; increment for middle pipe
                ; if player past this then past middle, must be right pipe
                if a >= #$a0
                    inx                           ; increment for last pipe
                endif
            endif
            ; get worldnumber and decrement for use as world number and y offset

            mb y, WorldNumber := WarpZoneNumbers[ x ] - 1
            mb x := WorldAddrOffsets[ y ]         ; get offset to where this world's area offsets are

            mb AreaPointer := AreaAddrOffsets[ x ]                     ; get area offset based on world offset   

            mb EventMusicQueue    := #MUSIC_Silence                    ; silence music
            mb EntrancePage       := #0           ; initialize starting page number
            mb AreaNumber         := a            ; initialize area number used for area address offset
            mb LevelNumber        := a            ; initialize level number used for world display
            mb AltEntranceControl := a            ; initialize mode of entry

            inc Hidden1UpFlag                     ; set flag for hidden 1-up blocks
            inc FetchNewGameTimerFlag             ; set flag to load new game timer
        endif
    endif

    rts                                           ; leave!!!

.endproc

.proc ImpedePlayerMove
    
    ; locals    
        ; temp_byte - used as to restrict specific movement
    ; end locals
    
    lda #0                                        ; initialize value here
    ldy Player_X_Speed                            ; get player's horizontal speed
    ; if temp_byte = 1 then player moving left, hit right side of object
    ; hit right side object:
    if x := temp_byte - 1 == zero                 ; check value set earlier for moving direction
        inx                                       ; #1                               ; X has 0
        if cpy #0 == negative goto SetBitsAndExit                      ; if player moving to the left
        lda #<-1                                  ; load A with value to be used later
    else                                          ; hit left side object:
        ldx #2
        if cpy #1 == positive goto SetBitsAndExit                      ; if player moving to the right
        lda #1                                    ; otherwise load A with value to be used here
    endif

    mb y, SideCollisionTimer := #$10              ; set timer: no scrolling for 16 frames
    mb y, Player_X_Speed := #0                    ; nullify player's horizontal speed
    ; OP: why not just sta to temp_byte?
    if cmp #0 == negative                         ; if a is -1 (#$ff)
        dey                                       ; decrement Y now
    endif
    sty temp_byte                                 ; store Y as high bits of horizontal adder

    mb Player_X_Position := a + Player_X_Position                      ; position to move player left or right
    mb Player_PageLoc := Player_PageLoc +c temp_byte                   ; add high bits and carry to

    SetBitsAndExit:                               ; invert bit and leave
    ; X is either 1 or 2 here: mask out collision bit from Player_CollisionBits
    mb a := x
    mb Player_CollisionBits := a ^ #$ff & Player_CollisionBits         ; store to clear bit
    rts

.endproc


dataseg

SolidMTileUpperExt:
    .byte $10, $61, $88, $c4

.code

.proc CheckForSolidMTiles

    jsr GetMTileAttrib                            ; find appropriate offset based on metatile's 2 MSB
    cmp SolidMTileUpperExt,x                      ; compare current metatile with solid metatiles
    rts

.endproc

dataseg

ClimbMTileUpperExt:
    .byte $24, $6d, $8a, $c6

.code

.proc CheckForClimbMTiles

    jsr GetMTileAttrib                            ; find appropriate offset based on metatile's 2 MSB
    cmp ClimbMTileUpperExt,x                      ; compare current metatile with climbable metatiles
    rts

.endproc

.proc CheckForCoinMTiles

    if a <> #$c2 && a <> #$c3                     ; check for regular coin or underwater coin
        clc                                       ; clear carry and leave
        rts
    endif                                         ; else:

    mb Square2SoundQueue := #SFX_CoinGrab         ; load coin grab sound and leave
    rts

.endproc

.proc GetMTileAttrib

    tay                                           ; save metatile value into Y
        and #%11000000                            ; mask out all but 2 MSB
        asl
        rol                                       ; shift and rotate d7-d6 to d1-d0
        rol
        tax                                       ; use as offset for metatile data
    tya                                           ; get original metatile value back

    Exit:

    rts                                           ; leave

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

EnemyBGCStateData:
    .byte $01, $01, $02, $02, $02, $05

EnemyBGCXSpdData:
    .byte $10, <-$10
.code

.proc EnemyToBGCollisionDet

    ; locals
        BBufferYPos             = temp_byte + 2     ; - vertical coordinate from block buffer routine
        BlockBufferPointer      = temp_byte + 6     ; - address from block buffer routine
    ; end locals


    ; if Enemy State d6 set OR Enemy YPos not in range OR enemy is a spiny that is high on the screen
    if Enemy_State[ x ] & #%00100000 || jsr CheckRangeEnemyYPos == less || \
    ( ldy Enemy_ID[ x ] = #OBJECTID_Spiny && Enemy_Y_Position[ x ] < #$25) goto GetMTileAttrib::Exit    ; rts

    if y = #OBJECTID_GreenParatroopaJump                   ; check for some other enemy object
        jmp EnemyJump
    endif

    if y = #OBJECTID_HammerBro                             ; check for hammer bro
        jmp HammerBroBGColl
    endif

    if y <> #OBJECTID_Spiny && y <> #OBJECTID_PowerUpObject && y >= #$07 goto ChkToStunEnemies_Exit     ; rts
    ; if y = #OBJECTID_Spiny || y = #OBJECTID_PowerUpObject || y < $07, continue:

    if !jsr ChkUnderEnemy                         ; if NO block underneath enemy:
        NoEToBGCollision:
        jmp ChkForRedKoopa
    endif
    
    ; Handle E To BG Collision:
    
    ; ------------------------------------------------------------------------------------------------
    
    ; if something is underneath enemy, find out what.. 
    ; if blank $26, coins, or hidden blocks, jump, enemy falls through
    if !jsr ChkForNonSolids goto NoEToBGCollision                      ; OP: This could be structured better with above code

    if a <> #$23 goto LandEnemyProperly           ; check for blank metatile $23 and branch if not found
    ; a = #23:

    ldy BBufferYPos                                    ; get vertical coordinate used to find block
    ; store default blank metatile in that spot so we won't trigger this routine accidentally again
    mb (BlockBufferPointer)[ y ] := #0

    if Enemy_ID[ x ] < #$15
        if a = #OBJECTID_Goomba
            jsr KillEnemyAboveBlock
        endif
        lda #1
        jsr SetupFloateyNumber                    ; award 100 points for hitting block beneath enemy
    endif

.endproc


.proc ChkToStunEnemies

    .export SetStun
    
    ; Note:
    ; OBJECTID_TallEnemy             = $09
    ; OBJECTID_PiranhaPlant          = $0d
    ; OBJECTID_GreenParatroopaJump   = $0e
    ; OBJECTID_RedParatroopa         = $0f
    ; OBJECTID_GreenParatroopaFly    = $10
    ; OP: this condition can be optimized.
    ; if a = $09, $0d, $0e, $0f or $10, it will be modified, but it will never be $0d here due to
    ; calling routine modifying reg A when PiranhaPlant found.

    if a >= #$09 && a < #$11 && ( a < #$0a || a >= #$0d )
        ; erase all but LSB, essentially turning enemy object into green or red koopa troopa to demote them
        mb Enemy_ID[ x ] := a & #1
    endif

    SetStun:

    mb Enemy_State[ x ] := Enemy_State[ x ] & #%11110000 | #%00000010

    dec Enemy_Y_Position,x
    
    dec Enemy_Y_Position,x                        ; subtract two pixels from enemy's vertical position
    
    ; this next if means:
    ; if not blooper: a = -1
    ; if blooper and not water level : a = - 3    ; Blooper out of water?
    
    if Enemy_ID[ x ] <> #OBJECTID_Bloober
        lda #(< -3)                               ; set speed for Blooper out of water
        if ldy AreaType != zero goto NotWater
    endif

    lda #(< -1)                                   ; default V speed

    NotWater:

    mb Enemy_Y_Speed[ x ] := a                    ; set vertical speed now

    ldy #1                                        ; = right
    if jsr PlayerEnemyDiff == negative            ; if enemy is to the left of player
        iny                                       ; 2 = left
    endif
    
    ; check for either bulletbill type, if not a bulletbill, set appropriate direction
    if lda Enemy_ID[ x ] <> #OBJECTID_BulletBill_CannonVar && a <> #OBJECTID_BulletBill_FrenzyVar
        sty Enemy_MovingDir,x                     ; store as moving direction
    endif

    dey                                           ; decrement and use as offset

    mb Enemy_X_Speed[ x ] := EnemyBGCXSpdData[ y ]                     ; 16 or -16

.endproc

    ChkToStunEnemies_Exit:                        ; global scope for forward branch
    rts
    
    ; ------------------------------------------------------------------------------------------------    

.proc LandEnemyProperly

    ; locals
        BBufferYPosLoNybble = temp_byte + 4     ; - low nybble of vertical coordinate from block buffer routine
    ; end locals
    
    ; check lower nybble of vertical coordinate saved earlier
    ; used to determine whether enemy landed from falling
    
    ; if ( BBufferYPosLoNybble in range from $8 to $C inclusive )
    if BBufferYPosLoNybble - #8 < #5
        if !Enemy_State[ x ] & #%01000000         ; if d6 not set
            if Enemy_State[ x ] << 1 == C set

                SideCheckA:
                jmp DoEnemySideCheck              ; if lower nybble < $0d, d7 set but d6 not set, jump here

            endif

            if !Enemy_State[ x ] goto SideCheckA                       ; OP: organize better
            ; Check enemy landed state:

            if a <> #$05                          ; if not a spiny's egg
                if a < #3                         ; if koopa or buzzy beetle type state
                    ; load enemy state again (why?) OP: this and this whole section
                    if !Enemy_State[ x ] = #2 goto ProcEnemyDirection                              
                    ; here, state is 1 or 0

                    lda #$10                      ; load default timer here
                    if ldy Enemy_ID[ x ] = #OBJECTID_Spiny                      ; check enemy identifier for spiny
                        lda #0                    ; set timer for zero if spiny
                    endif
                    ; Set stun timer:
                    sta EnemyIntervalTimer,x      ; set timer here
                    mb Enemy_State[ x ] := #3     ; set state here, for upside-down koopas and buzzy beetles
                    jsr EnemyLanding              ; then land it properly

                endif
                rts                               ; then leave
            endif

            ProcEnemyDirection:

            if Enemy_ID[ x ] <> #OBJECTID_Goomba

                if a = #OBJECTID_Spiny                     ; check for spiny
                    mb Enemy_MovingDir[ x ] := #1                      ; send enemy moving to the right by default
                    mb Enemy_X_Speed  [ x ] := #$08                    ; set horizontal speed accordingly
                    ; if timed appropriately, spiny will skip over trying to face the player
                    if !FrameCounter & #%00000111 goto LandEnemyInitState                               ; every
                endif

                ldy #1                            ; load 1 for enemy to face the left (inverted here)
                if jsr PlayerEnemyDiff == negative                     ; if enemy to the left of player
                    iny
                endif

                tya
                if a = Enemy_MovingDir[ x ]       ; compare direction in A with current direction in memory
                    jsr ChkForBump_HammerBroJ     ; if equal, not facing in correct dir, do sub to turn around
                endif
            endif
        endif

        LandEnemyInitState:

        jsr EnemyLanding                          ; land enemy properly

        if !Enemy_State[ x ] & #%10000000         ; if d7 not set OP: N flag
            mb Enemy_State[ x ] := #0             ; initialize enemy state: this will also turn spiny's egg into spiny
            rts
        endif
        ; nullify d6 of enemy state, save other bits
        mb Enemy_State[ x ] := Enemy_State[ x ] & #%10111111

        rts
    endif
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc ChkForRedKoopa


    if Enemy_ID[ x ] = #OBJECTID_RedKoopa && !Enemy_State[ x ] goto ChkForBump_HammerBroJ
    ; Enemy state is not normal, continue:

    lda Enemy_State,x

    tay

    if a << 1 == C set                            ; OP use N flag
    ; set d6
        mb a := Enemy_State[ x ] | #%01000000     ; OP ror instead of load
    else
    ; load sate form data
        mb a := EnemyBGCStateData[ y ]            ; load new enemy state with old as offset
    endif

    sta Enemy_State,x                             ; set as new state

.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc DoEnemySideCheck

    ; locals
        Bitmask = temp_byte ; - used to store bitmask (not used but initialized here)
        CounterTemp = $eb   ; - used in DoEnemySideCheck as counter and to compare moving directions
    ; end locals

    if Enemy_Y_Position[ x ] >= #$20              ; if enemy below status bar:

        ldy #$16                                  ; start by finding block to the left of enemy ($00,$14)

        mb CounterTemp := #2                              ; set value here in what is also used as OAM data offset

        repeat

            if lda CounterTemp = Enemy_MovingDir[ x ]     ; compare value against moving direction
                lda #1                            ; set flag in A for save horizontal coordinate
                if jsr BlockBufferChk_Enemy == not zero                ; find block to left or right of enemy object
                    if jsr ChkForNonSolids == not zero goto ChkForBump_HammerBroJ ; branch if not found
                endif
            endif

            dec CounterTemp                               ; move to the next direction
        ; increment Y, loop only if Y < $18, thus we check
        ; enemy ($00, $14) and ($10, $14) pixel coordinates
        until iny >= #$18
    endif

    rts
.endproc

.proc ChkForBump_HammerBroJ
    ; if we're not on the special use slot,
    ; ( sound will never be played if branching from ChkForRedKoopaChkForRedKoopa )
    if x <> #$05 && Enemy_State[ x ] << 1 == C set
        mb Square1SoundQueue := #SFX_Bump         ; play bump sound
    endif

    if Enemy_ID[ x ] = #OBJECTID_HammerBro
        mb temp_byte := #0                        ; initialize value here for bitmask
        ldy #(<-6)                                ; load default vertical speed for jumping
        jmp HammerBroJump                         ; jump to code that makes hammer bro jump
    endif
    ; Invert E direction
    jmp ForceEnemyTurnAround                      ; jump to turn the enemy around and return

.endproc

    ; ------------------------------------------------------------------------------------------------
    

.proc PlayerEnemyDiff

    ; temp_byte - used to hold horizontal difference between player and enemy
    
    mb temp_byte := Enemy_X_Position[ x ] - Player_X_Position
    mb a := Enemy_PageLoc[ x ] -c Player_PageLoc
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc EnemyLanding

    jsr InitSpeedAndMoveForce0
    ; probably used to set enemy object neatly down
    mb Enemy_Y_Position[ x ] := Enemy_Y_Position[ x ] & #%11110000 | #%00001000
    rts

.endproc

.proc CheckRangeEnemyYPos
    ; this is an odd routine :
    ; it is only called twice and the command after it returns
    ; is always bcc ( branch if less )
    ;
    ; it adds #$3e ( 62 ) which causes (194 - 255) to wrap to (0 to 61)
    ; it then compares to #$44 ( 68 )
    ; this means only orignal values from 0 - 5 and 194 - 255 branch on a bcc,
    ; ( 6 to 193 branch on a bcs )
    ; see: http://forums.nesdev.com/viewtopic.php?p=106530#p106530

    lda Enemy_Y_Position,x
    clc
    adc #<(256 - 194)                             ; 194 = bottom of screen
    cmp #<(6 + ( 256 - 194))                      ; 6   = topof screen
    rts                                           ; and leave with flags set for conditional branch

.endproc

.proc EnemyJump
    ; if bottom ( add 24 for object height) of enemy vertical coord in range from 30 to 217
    if jsr CheckRangeEnemyYPos == C set
        ; if green paratroopa falling and speed is in range 251(-5) to 254 (-2)
        if Enemy_Y_Speed[ x ] + #2 >= #3 && jsr ChkUnderEnemy == not equal && jsr ChkForNonSolids == not equal
            jsr EnemyLanding                      ; change vertical coordinate and speed
            mb Enemy_Y_Speed[ x ] := #$fd         ; make the paratroopa jump again
        endif
    endif

    jmp DoEnemySideCheck                          ; check for horizontal blockage, then leave

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc HammerBroBGColl

    .export KillEnemyAboveBlock

    if jsr ChkUnderEnemy == not equal             ; check to see if hammer bro is standing on anything
        ; check for blank metatile $23
        if a = #$23
            KillEnemyAboveBlock:
            jsr ShellOrBlockDefeat                ; do this sub to kill enemy
            mb Enemy_Y_Speed[ x ] := #$fc         ; alter vertical speed of enemy and leave
            rts
        endif
        ; else UnderHammerBro:
        if !EnemyFrameTimer[ x ]                  ; check timer used by hammer bro
        ; save d7 and d3 from enemy state, nullify other bits
            mb Enemy_State[ x ] := Enemy_State[ x ] & #%10001000
            jsr EnemyLanding                      ; modify vertical coordinate, speed and something else
            jmp DoEnemySideCheck                  ; then check for horizontal blockage and leave
        endif
    endif
    ; else:
    ; if hammer bro is not standing on anything, set d0
    ; in the enemy state to indicate jumping or falling, then leave

    mb Enemy_State[ x ] := Enemy_State[ x ] | #1

    rts

.endproc

.proc ChkUnderEnemy

    lda #0                                        ; set flag in A for save vertical coordinate
    ldy #$15                                      ; set Y to check the bottom middle (8,18) of enemy object
    jmp BlockBufferChk_Enemy                      ; hop to it!

.endproc

.proc ChkForNonSolids

    if a = #$26 goto NSFnd                        ; blank metatile used for vines?
    if a = #$c2 goto NSFnd                        ; regular coin?
    if a = #$c3 goto NSFnd                        ; underwater coin?
    if a = #$5f goto NSFnd                        ; hidden coin block?
    cmp #$60                                      ; hidden 1-up block?

    NSFnd:
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc FireballBGCollision

    ; if fireball below status bar && Something below fireball && it's a solid tile
    if Fireball_Y_Position[ x ] >= #$18 && jsr BlockBufferChk_FBall == not equal && jsr ChkForNonSolids == not equal
        ; if fireball moving upwards OR it's already bouncing
        if Fireball_Y_Speed[ x ] == negative || FireballBouncingFlag[ x ] goto InitFireballExplode
        ; else
        mb Fireball_Y_Speed[ x ] := #(<-3)        ; otherwise set vertical speed to move upwards (give it bounce)
        mb FireballBouncingFlag[ x ] := #1        ; set bouncing flag
        ; modify vertical coordinate to land it properly
        mb Fireball_Y_Position[ x ] := Fireball_Y_Position[ x ] & #$f8

        rts                                       ; leave
    endif
    ; else:

    mb FireballBouncingFlag[ x ] := #0            ; clear bouncing flag by default
    rts  
    
    ; ------------------------------------------------------------------------------------------------

    InitFireballExplode:

    mb Fireball_State[ x ] := #$80                ; set exploding flag in fireball's state
    mb Square1SoundQueue := #SFX_Bump             ; load bump sound
    rts                                           ; leave

.endproc

    ; ------------------------------------------------------------------------------------------------
    
dataseg

    ; this data added to relative coordinates of sprite objects
    ; stored in order: left edge, top edge, right edge, bottom edge

BoundBoxCtrlData:
    .byte $02, $08, $0e, $20
    .byte $03, $14, $0d, $20
    .byte $02, $14, $0e, $20
    .byte $02, $09, $0e, $15
    .byte $00, $00, $18, $06
    .byte $00, $00, $20, $0d
    .byte $00, $00, $30, $0d
    .byte $00, $00, $08, $08
    .byte $06, $04, $0a, $08
    .byte $03, $0e, $0d, $14
    .byte $00, $02, $10, $15
    .byte $04, $04, $0c, $1c

.code

.proc GetFireballBoundBox
    txa                                           ; add seven bytes to offset
    mb x := a + #7
    ldy #2                                        ; set offset for relative coordinates
    bne DoBoundingBox                             ; unconditional branch
.endproc

.proc GetMiscBoundBox                             ; OP: move this section to the start of .proc and do inx:inx or something
    txa                                           ; add nine bytes to offset
    mb x := a + #9
    ldy #$06                                      ; set offset for relative coordinates
.endproc

.proc DoBoundingBox
    jsr BoundingBoxCore                           ; get bounding box coordinates
    jmp CheckRightScreenBBox                      ; jump to handle any offscreen coordinates
.endproc

    ; ------------------------------------------------------------------------------------------------

    ; temp_byte - used to hold one of bitmasks, or offset
    
.proc GetEnemyBoundBox
    mb y, temp_byte := #$48                       ; store bitmask here for now
    ldy #$44                                      ; store default bitmask here for now and jump over
    jmp GetMaskedOffScrBits
.endproc

.proc SmallPlatformBoundBox
    mb y, temp_byte := #$08                       ; store bitmask here for now
    ldy #4                                        ; store default bitmask here for now
.endproc

.proc GetMaskedOffScrBits

    mb $01 := Enemy_X_Position[ x ] - ScreenLeft_X_Pos
    ; if enemy object is on screen
    if Enemy_PageLoc[ x ] -c ScreenLeft_PageLoc == positive && a | $01
        ldy temp_byte                             ; if to the right of left edge, use value in temp_byte for A
    endif

    mb a := y                                     ; otherwise use contents of Y

    mb EnemyOffscrBitsMasked[ x ] := a & Enemy_OffscreenBits           ; save masked offscreen bits here
    if not zero goto MoveBoundBoxOffscreen        ; if anything set here, branch
    jmp SetupEOffsetFBBox                         ; otherwise, do something else

.endproc

.proc LargePlatformBoundBox

    inx                                           ; increment X to get the proper offset
    jsr GetXOffscreenBits                         ; then jump directly to the sub for horizontal offscreen bits
    dex                                           ; decrement to return to original offse
    ; if completely offscreen, branch to put entire bounding
    ; box offscreen, otherwise start getting coordinates
    if a >= #$fe goto MoveBoundBoxOffscreen
.endproc

.proc SetupEOffsetFBBox

    mb a := x                                     ; add 1 to offset to properly address OP
    mb x := a + #1
    ldy #1                                        ; load 1 as offset here, same reason
    jsr BoundingBoxCore                           ; do a sub to get the coordinates of the bounding box
    jmp CheckRightScreenBBox                      ; jump to handle offscreen coordinates of bounding box

.endproc

.proc MoveBoundBoxOffscreen

    txa                                           ; multiply offset by 4
    mb y := a * 4
    lda #$ff
    mb EnemyBoundingBoxCoord[ 0 + y ] := a        ; load value into four locations here and leave
    mb EnemyBoundingBoxCoord[ 1 + y ] := a
    mb EnemyBoundingBoxCoord[ 2 + y ] := a
    mb EnemyBoundingBoxCoord[ 3 + y ] := a
    rts

.endproc

.proc BoundingBoxCore

    ; locals
        savex     = temp_byte ; - used to hold one of bitmasks, or offset
        RelativeX = temp_byte + 1   ; - used for relative X coordinate, also used to store middle screen page location
        RelativeY = temp_byte + 2   ; - used for relative Y coordinate, also used to store middle screen coordinate
    ; end locals

    stx savex                                 ; save offset here
    ; store object coordinates relative to screen
    ; vertically and horizontally, respectively
    mb RelativeY := SprObject_Rel_YPos[ y ]
    mb RelativeX := SprObject_Rel_XPos[ y ]
    mb a := x
    mb a := a * 4

    pha
        tay                                       ; use as offset for Y, X is left alone

        mb x := SprObj_BoundBoxCtrl[ x ] * 4
        ; add the first number in the bounding box data to the
        ; relative horizontal coordinate using enemy object offset
        ; and store somewhere using same offset * 4
        mb BoundingBox_UL_Corner[ y ] := RelativeX + BoundBoxCtrlData[ x ]
        ; add the third number in the bounding box data to the
        mb BoundingBox_LR_Corner[ y ] := RelativeX + BoundBoxCtrlData[ x + 2 ] ; relative horizontal coordinate and store

        inx                                       ; increment both offsets
        iny
        ; add the second number to the relative vertical coordinate
        ; using incremented offset and store using the other
        mb BoundingBox_UL_Corner[ y ] := RelativeY + BoundBoxCtrlData[ x ]
        ; add the fourth number to the relative vertical coordinate
        mb BoundingBox_LR_Corner[ y ] := RelativeY + BoundBoxCtrlData[ x + 2 ]

    pla                                           ; get original offset loaded into temp_byte * y from stack
    tay                                           ; use as Y
    ldx savex                                     ; get original offset and use as X again
    rts

.endproc

.proc CheckRightScreenBBox

    ; locals
        MiddleX = temp_byte + 2  ; horizontal coordinate of middle
        PageX   = temp_byte + 1  ; page location of middle
    ; end locals

    mb MiddleX := ScreenLeft_X_Pos + #$80
    mb PageX := ScreenLeft_PageLoc + C

    lda SprObject_X_Position,x                    ; get horizontal coordinate
    cmp MiddleX                                   ; compare against middle horizontal coordinate for carry
    ; if object is on the reight side:
    if SprObject_PageLoc[ x ] -c $01 == greaterORequal
    ; check right-side edge of bounding box for offscreen
    ; if offscreen:
        if BoundingBox_DR_XPos[ y ] == positive
            lda #$ff                              ; load offscreen value here to use on one or both horizontal sides
            if ldx BoundingBox_UL_XPos[ y ] == positive                ; if left side is offscreen
                sta BoundingBox_UL_XPos,y         ; store offscreen value
            endif
            sta BoundingBox_DR_XPos,y             ; store offscreen value for right side
        endif
        ldx ObjectOffset                          ; get object offset and leave
        rts
    endif
    ; check left-side edge of bounding box: if offscreen:
    if BoundingBox_UL_XPos[ y ] == negative && a >= #$a0
        lda #0
        if ldx BoundingBox_DR_XPos[ y ] == negative                    ; check right-side edge of bounding box: if offscreen:
            sta BoundingBox_DR_XPos,y             ; store offscreen value for right side
        endif
        sta BoundingBox_UL_XPos,y                 ; store offscreen value for left side
    endif

    ldx ObjectOffset                              ; get object offset and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc PlayerCollisionCore
    ldx #0                                        ; initialize X to use player's bounding box for comparison
.endproc

.proc SprObjectCollisionCore

    ; locals
        SecondObjectOffset = temp_byte + 6  ; - second object's offset
        Counter = temp_byte + 7             ; - counter
    ; end locals
    
    sty SecondObjectOffset                            ; save contents of Y here
    
    mb Counter := #1                                  ; save value 1 here as counter, compare horizontal coordinates first
    repeat                                            ; Counter 1 to 0
    ; compare left/top coordinate of first and second objects' bounding boxes
    ; if first left/top => second, branch

        if lda BoundingBox_UL_Corner[ y ] < BoundingBox_UL_Corner[ x ]

            if a >= BoundingBox_LR_Corner[ x ]
                ; check that first box didn't wrap, then check for collision
                if equal || lda BoundingBox_LR_Corner[ y ] < BoundingBox_UL_Corner[ y ] \
                    || a >= BoundingBox_UL_Corner[ x ] goto CollisionFound

                ldy SecondObjectOffset            ; otherwise return with carry clear and Y = $0006
                rts                               ; note horizontal wrapping never occurs

            endif


            if BoundingBox_LR_Corner[ x ] < BoundingBox_UL_Corner[ x ] || \
               BoundingBox_LR_Corner[ y ] >= BoundingBox_UL_Corner[ x ] goto CollisionFound
            ; no collision:
            ldy SecondObjectOffset
            rts

        endif

        if  a <> BoundingBox_UL_Corner[ x ] && a > BoundingBox_LR_Corner [ x ] && \
            ( a <= BoundingBox_LR_Corner[ y ] || lda BoundingBox_LR_Corner[ y ] < BoundingBox_UL_Corner[ x ] )
            ; no collision:
            clc                                   ; clear carry, then load value set earlier, then leave
            ldy SecondObjectOffset                ; like previous ones, if horizontal coordinates do not collide, we do
            rts                                   ; not bother checking vertical ones, because what's the point?
        endif

        CollisionFound:

        inx                                       ; increment offsets on both objects to check
        iny                                       ; the vertical coordinates

    until dec Counter == negative

    sec                                           ; otherwise we already did both sets, therefore collision, so set carry
    ldy SecondObjectOffset                        ; load original value set here earlier, then leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc BlockBufferChk_Enemy

    .export BlockBufferChk_FBall

    pha                                           ; save contents of A to stack
        txa
        clc                                       ; add 1 to X to run sub with enemy offset in mind OP
        adc #1
        tax
    pla                                           ; pull A from stack and jump elsewhere

    jmp BBChk_E

        ResidualMiscObjectCode:                   ; nothing calls this code OP

        txa
        clc                                       ; supposedly used once to set offset for
        adc #$0d                                  ; miscellaneous objects
        tax
        ldy #$1b                                  ; supposedly used once to set offset for block buffer data
        jmp ResJmpM                               ; probably used in early stages to do misc to bg collision detection

            BlockBufferChk_FBall:

            ldy #$1a                              ; set offset for block buffer adder data
            txa
            clc
            adc #$07                              ; add seven bytes to use
            tax

        ResJmpM:

        lda #0                                    ; set A to return vertical coordinate

    BBChk_E:

    jsr BlockBufferCollision                      ; do collision detection subroutine for sprite object
    ldx ObjectOffset                              ; get object offset
    cmp #0                                        ; check to see if object bumped into anything
    rts

.endproc

dataseg

BlockBufferAdderData:
    .byte $00, $07, $0e

BlockBuffer_X_Adder:                              ; 0 - 26
    .byte $08, $03, $0c, $02, $02, $0d, $0d, $08
    .byte $03, $0c, $02, $02, $0d, $0d, $08, $03
    .byte $0c, $02, $02, $0d, $0d, $08, $00, $10
    .byte $04, $14, $04, $04

BlockBuffer_Y_Adder:                              ; 0 - 26
    .byte $04, $20, $20, $08, $18, $08, $18, $02
    .byte $20, $20, $08, $18, $08, $18, $12, $20
    .byte $20, $18, $18, $18, $18, $18, $14, $14
    .byte $06, $06, $08, $10

.code

BlockBufferCollideFeet:
    iny                                           ; if branched here, increment to next set of adders

BlockBufferCollideHead:
    lda #0                                        ; set flag to return vertical coordinate
    BIT_Skip2                                     ; BIT instruction opcode

BlockBufferCollideSide:
    lda #1                                        ; set flag to return horizontal coordinate
    ldx #0                                        ; set offset for player object

.proc BlockBufferCollision

    ; lcoals    
        YCoord             = temp_byte + 2  ; - modified y coordinate
        Metatile           = temp_byte + 3  ; - stores metatile involved in block buffer collisions
        BlockOffset        = temp_byte + 4  ; - comes in with offset to block buffer adder data, goes out with low nybble x/y coordinate
        XCoord             = temp_byte + 5  ; - modified x coordinate
        BlockBufferPointer = temp_byte + 6  ; - block buffer address
    ; end locals

    pha                                           ; save contents of A to stack
        sty BlockOffset                           ; save contents of Y here
        ; add horizontal coordinate
        ; of object to value obtained using Y as offset
        mb XCoord := BlockBuffer_X_Adder[ y ] + SprObject_X_Position[ x ]
        ; add carry to page location and get LSB
        mb a := SprObject_PageLoc[ x ] +C #0 & #1

        mb a := a >> 1 | XCoord                   ; move to carry, OR old value (essintially load XCoord)

        ror                                       ; rotate carry to MSB of A

        lsr                                       ; and effectively move high nybble to
        lsr                                       ; lower, LSB which became MSB will be
        lsr                                       ; d4 at this point

        jsr GetBlockBufferAddr                    ; get address of block buffer into BlockBufferPointer, $07

        ldy BlockOffset                                   ; get old contents as offset
        ; ( subtract 32 pixels for the status bar)
        mb YCoord := SprObject_Y_Position[ x ] + BlockBuffer_Y_Adder[ y ] & #%11110000 - #$20

        tay                                       ; use as offset for block buffer
        mb Metatile := (BlockBufferPointer)[ y ]  ; and store here
        ldy BlockOffset                           ; get old contents of Y again

    pla                                           ; pull A from stack
    if zero
        lda SprObject_Y_Position,x                ; if A = 0, load vertical coordinate
    else
        lda SprObject_X_Position,x                ; otherwise load horizontal coordinate
    endif

    mb BlockOffset := a & #$0f                    ; and mask out high nybble; store masked out result here

    lda Metatile                                  ; get saved content of block buffer
    rts                                           ; and leave

.endproc

    ; ------------------------------------------------------------------------------------------------
    ; unused byte

dataseg

    .byte $ff

    ; ------------------------------------------------------------------------------------------------
    
VineYPosAdder:
    .byte $00, $30

.code

.proc DrawVine

    ; temp_byte - offset to vine Y coordinate adder
    ; $02 - offset to sprite data

    sty temp_byte                                 ; save offset here

    mb a := Enemy_Rel_YPos + VineYPosAdder[ y ]   ; add value using offset in Y to get value

    ldx Vine::ObjOffset,y                         ; get offset to vine

    mb y, $02 := Enemy_SprDataOffset[ x ]         ; get sprite data offset
    jsr SixSpriteStacker                          ; stack six sprites on top of each other vertically

    lda Enemy_Rel_XPos                            ; get relative horizontal coordinate
    mb Sprite[  0 + y ]::X_Position := a          ; store in first, third and fifth sprites
    mb Sprite[  8 + y ]::X_Position := a
    mb Sprite[ 16 + y ]::X_Position := a

    mb a := a + #6                                ; add six pixels to second, fourth and sixth sprites
    mb Sprite[  4 + y ]::X_Position := a          ; to give characteristic staggered vine shape to
    mb Sprite[ 12 + y ]::X_Position := a          ; our vertical stack of sprites
    mb Sprite[ 20 + y ]::X_Position := a

    mb a := #%00100001                            ; set bg priority and palette attribute bits
    mb Sprite[  0 + y ]::Attributes := a          ; set in first, third and fifth sprites
    mb Sprite[  8 + y ]::Attributes := a
    mb Sprite[ 16 + y ]::Attributes := a

    mb a := a | #%01000000                        ; additionally, set horizontal flip bit
    mb Sprite[  4 + y ]::Attributes := a          ; for second, fourth and sixth sprites
    mb Sprite[ 12 + y ]::Attributes := a
    mb Sprite[ 20 + y ]::Attributes := a

    ldx #$05                                      ; set tiles for six sprites

    repeat
        mb Sprite[ y ]::Tilenumber := #$e1
        mb y := y + 4
    until dex == negative                         ; loop until all sprites are done

    ldy $02                                       ; get original offset

    if !temp_byte                                 ; get offset to vine adding data
        mb Sprite[ y ]::Tilenumber := #$e0        ; set other tile number for top of vine
    endif

    ldx #0                                        ; start with the first sprite again
    repeat
    ; if two coordinates are less than 100/$64 pixels
        if Vine::Start_Y_Position - Sprite[ y ]::Y_Position >= #$64    ; subtract top-most sprite's Y coordinate
            mb Sprite[ y ]::Y_Position := #$f8    ; otherwise move sprite offscreen
        endif
        mb y := y + 4                             ; move offset to next OAM data
    until inx = #6

    ldy temp_byte                                 ; return offset set earlier
    rts

.endproc

.proc SixSpriteStacker

    ldx #$06                                      ; do six sprites

    repeat

        mb Sprite[ y ]::Data := a                 ; store X or Y coordinate into OAM data
        mb a := a + #8
        mb y := y + 4

    until dex == zero

    ldy $02                                       ; get saved OAM data offset and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

FirstSprXPos:
    .byte $04, $00, $04, $00

FirstSprYPos:
    .byte $00, $04, $00, $04

SecondSprXPos:
    .byte $00, $08, $00, $08

SecondSprYPos:
    .byte $08, $00, $08, $00

FirstSprTilenum:
    .byte $80, $82, $81, $83

SecondSprTilenum:
    .byte $81, $83, $80, $82

HammerSprAttrib:
    .byte $03, $03, $c3, $c3

.code

.proc DrawHammer

    ldy Misc_SprDataOffset,x                      ; get misc object OAM data offset

    if TimerControl || Misc_State[ x ] & #%01111111 <> #1
        ldx #0                                    ; reset offset here
    else Z set
    ; mask out all but d1-d0 (changes every four frames)
        mb x := FrameCounter >> 2 & #%00000011    ; use as timing offset
    endif

    mb Sprite[ y     ]::Y_Position := Misc_Rel_YPos + FirstSprYPos[ x ] ; store as sprite Y coordinate for first sprite
    mb Sprite[ 4 + y ]::Y_Position := a + SecondSprYPos[ x ]

    mb Sprite[ y     ]::X_Position := Misc_Rel_XPos + FirstSprXPos[ x ] ; store as sprite X coordinate for first sprite
    mb Sprite[ 4 + y ]::X_Position := a + SecondSprXPos[ x ]           ; store as sprite X coordinate for second sprite

    mb Sprite[ y     ]::Tilenumber := FirstSprTilenum[ x ]             ; get and store tile number of first sprite
    mb Sprite[ 4 + y ]::Tilenumber := SecondSprTilenum[ x ]            ; get and store tile number of second sprite

    mb a := HammerSprAttrib[ x ]
    mb Sprite[ y     ]::Attributes := a           ; get and store attribute bytes for both
    mb Sprite[ 4 + y ]::Attributes := a           ; note in this case they use the same data

    ldx ObjectOffset                              ; get misc object offset

    if Misc_OffscreenBits & #%11111100            ; check offscreen bits
        mb Misc_State[ x ] := #0                  ; otherwise nullify misc object state
        lda #$f8
        jsr DumpTwoSpr                            ; do sub to move hammer sprites offscreen
    endif
    rts                                           ; leave

.endproc

    ; ------------------------------------------------------------------------------------------------
    

dataseg

FlagpoleScoreNumTiles:
    .byte $f9, $50
    .byte $f7, $50
    .byte $fa, $fb
    .byte $f8, $fb
    .byte $f6, $fb

.code

.proc FlagpoleGfxHandler

    ; locals
        Tile1       = temp_byte         ; - Tiles
        Tile2       = temp_byte + 1     ;
        YCoord      = temp_byte + 2     ; - Y position
        FlipControl = temp_byte + 3     ; - moving direction, used to flip enemies horizontally
        SpriteAtt   = temp_byte + 4     ; - enemy's sprite attributes
        XCoord      = temp_byte + 5     ; - X position
    ; end locals
    

    ldy Enemy_SprDataOffset,x                     ; get sprite data offset for flagpole flag

    mb Sprite[ y ]::X_Position := Enemy_Rel_XPos  ; get relative horizontal coordinate store as X coordinate for first sprite

    mb a := a + #$08                              ; add eight pixels and store
    mb Sprite[ 4 + y ]::X_Position := a           ; as X coordinate for second and third sprites
    mb Sprite[ 8 + y ]::X_Position := a

    mb XCoord := a + #$0c                            ; add twelve more pixels to be used later by floatey number

    lda Enemy_Y_Position,x                        ; get vertical coordinate
    jsr DumpTwoSpr                                ; and do sub to dump into first and second sprites

    mb Sprite[ 8 + y]::Y_Position := a +c #$08    ; and store into third sprite
    mb YCoord := FlagpoleFNum_Y_Pos               ; get vertical coordinate for floatey number
    mb FlipControl := #1                          ; set value for flip which will not be used, and
    mb SpriteAtt := a                             ; #1           ; attribute byte for floatey number
    mb Sprite[ 0 + y ]::Attributes := a           ; set attribute bytes for all three sprites
    mb Sprite[ 4 + y ]::Attributes := a
    mb Sprite[ 8 + y ]::Attributes := a

    mb a := #$7e
    mb Sprite[ 0 + y ]::Tilenumber := a           ; put triangle shaped tile
    mb Sprite[ 8 + y ]::Tilenumber := a           ; into first and third sprites
    mb Sprite[ 4 + y ]::Tilenumber := #$7f        ; put skull tile into second sprite

    if FlagpoleCollisionYPos                      ; get vertical coordinate at time of collision
        tya
        mb y         := a + #$0c                  ; add 12 bytes to sprite data offset
        mb x         := FlagpoleScore * 2         ; get offset used to award points for touching flagpole
        mb temp_byte := FlagpoleScoreNumTiles[ x ]
        mb a         := FlagpoleScoreNumTiles[ 1 + x ]
        jsr DrawOneSpriteRow                      ; use it to render floatey number
    endif
    ; Check if Flag is offscreen:
    ldx ObjectOffset                              ; get object offset for flag
    ldy Enemy_SprDataOffset,x                     ; get OAM data offset

    if !Enemy_OffscreenBits & #%00001110 goto ExitDumpSpr
.endproc

    ; ------------------------------------------------------------------------------------------------
    ; Easier to leave all these global:

MoveSixSpritesOffscreen:

    lda #$f8                                      ; set offscreen coordinate if jumping here

    DumpSixSpr:

    mb Sprite[ y + 20 ]::Data := a                ; dump A contents
    mb Sprite[ y + 16 ]::Data := a                ; into third row sprites

    DumpFourSpr:

    mb Sprite[ y + 12 ]::Data := a                ; into second row sprites

    DumpThreeSpr:

    mb Sprite[ y + 8  ]::Data := a

    DumpTwoSpr:

    mb Sprite[ y + 4  ]::Data := a                ; and into first row sprites
    mb Sprite[ y      ]::Data := a

    ExitDumpSpr:
    rts

    ; ------------------------------------------------------------------------------------------------

.proc DrawLargePlatform

    mb y, $02 := Enemy_SprDataOffset[ x ]         ; get OAM data offset
    mb y := y + 3                                 ; add 3 to it for offset to X coordinate

    lda Enemy_Rel_XPos                            ; get horizontal relative coordinate
    jsr SixSpriteStacker                          ; store X coordinates using A as base, stack horizontally

    ldx ObjectOffset
    lda Enemy_Y_Position,x                        ; get vertical coordinate
    jsr DumpFourSpr                               ; dump into first four sprites as Y coordinate
    ; check for castle-type level
    if ldy AreaType = #3 || ldy SecondaryHardMode
        lda #$f8                                  ; load offscreen coordinate if flag set or castle-type level
    endif
    ; SetLast2Platform:

    ldy Enemy_SprDataOffset,x                     ; get OAM data offset
    mb Sprite[ 16 + y ]::Y_Position := a          ; store vertical coordinate or offscreen
    mb Sprite[ 20 + y ]::Y_Position := a          ; coordinate into last two sprites as Y coordinate

    lda #$5b                                      ; load default tile for platform (girder)
    if ldx CloudTypeOverride
        lda #$75                                  ; load other tile for platform (puff)
    endif

    ldx ObjectOffset                              ; get enemy object buffer offset
    iny                                           ; increment Y for tile offset
    jsr DumpSixSpr                                ; dump tile number into all six sprites

    lda #2                                        ; set palette controls
    iny                                           ; increment Y for sprite attributes
    jsr DumpSixSpr                                ; dump attributes into all six sprites

    inx                                           ; increment X for enemy objects
    jsr GetXOffscreenBits                         ; get offscreen bits again

    dex
    ldy Enemy_SprDataOffset,x                     ; get OAM data offset

    asl                                           ; rotate d7 into carry, save remaining
    pha                                           ; bits to the stack
        if carry set
            mb Sprite[     y ]::Y_Position := #$f8                     ; if d7 was set, move first sprite offscreen
        endif
    pla                                           ; get bits from stack
    asl                                           ; rotate d6 into carry
    pha                                           ; save to stack
        if carry set
            mb Sprite[ 4 + y ]::Y_Position := #$f8
        endif
    pla                                           ; get bits from stack
    asl                                           ; rotate d5 into carry
    pha                                           ; save to stack
        if carry set
            mb Sprite[ 8 + y ]::Y_Position := #$f8
        endif
    pla                                           ; get bits from stack
    asl                                           ; rotate d4 into carry
    pha                                           ; save to stack
        if carry set
            mb Sprite[ 12 + y ]::Y_Position := #$f8
        endif
    pla                                           ; get bits from stack
    asl                                           ; rotate d3 into carry
    pha                                           ; save to stack
        if carry set
            mb Sprite[ 16 + y ]::Y_Position := #$f8
        endif
    pla                                           ; get bits from stack
    asl                                           ; rotate d2 into carry

    if carry set                                  ; save to stack
        mb Sprite[ 20 + y ]::Y_Position := #$f8
    endif

    if Enemy_OffscreenBits << 1 == carry set      ; check d7 of offscreen bits
        jsr MoveSixSpritesOffscreen               ; otherwise branch to move all sprites offscreen
    endif

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DrawFloateyNumber_Coin

    if FrameCounter >> 1 == carry clear           ; every even frame
        dec Misc_Y_Position,x                     ; decrement vertical coordinate
    endif

    lda Misc_Y_Position,x                         ; get vertical coordinate

    jsr DumpTwoSpr                                ; dump into both sprites

    mb Sprite[ 0 + y ]::X_Position := Misc_Rel_XPos                    ; store as X coordinate for first sprite
    mb Sprite[ 4 + y ]::X_Position := a + #8      ; store as X coordinate for second sprite

    lda #2
    mb Sprite[ 0 + y ]::Attributes := a           ; store attribute byte in both sprites
    mb Sprite[ 4 + y ]::Attributes := a
    ; '200'
    mb Sprite[ 0 + y ]::Tilenumber := #$f7
    mb Sprite[ 4 + y ]::Tilenumber := #$fb

    jmp JCoinGfxHandler_Exit                      ; OP then jump to leave (why not an rts here instead?)

.endproc

dataseg

JumpingCoinTiles:
    .byte $60, $61, $62, $63

.code

.proc JCoinGfxHandler

    .export JCoinGfxHandler_Exit

    ldy Misc_SprDataOffset,x                      ; get coin/floatey number's OAM data offset

    if Misc_State[ x ] >= #2 goto DrawFloateyNumber_Coin

    mb Sprite[ 0 + y ]::Y_Position := Misc_Y_Position[ x ]             ; Y coordinate for first sprite
    mb Sprite[ 4 + y ]::Y_Position := a + #8      ; store as Y coordinate for second sprite

    lda Misc_Rel_XPos                             ; get relative horizontal coordinate
    mb Sprite[ 0 + y ]::X_Position := a
    mb Sprite[ 4 + y ]::X_Position := a           ; store as X coordinate for first and second sprites

    mb x := FrameCounter >> 1 & #%00000011        ; use as graphical offset

    lda JumpingCoinTiles,x                        ; load tile number
    iny                                           ; increment OAM data offset to write tile numbers
    jsr DumpTwoSpr                                ; do sub to dump tile number into both sprites
    dey                                           ; decrement to get old offset

    mb Sprite[ 0 + y ]::Attributes := #2          ; set attribute byte in first sprite
    mb Sprite[ 4 + y ]::Attributes := #$82        ; set attribute byte with vertical flip in second sprite

    ldx ObjectOffset                              ; get misc object offset

    JCoinGfxHandler_Exit:

    rts                                           ; leave
.endproc

    ; ------------------------------------------------------------------------------------------------
   
dataseg


PowerUpGfxTable:
    .byte $76, $77, $78, $79                      ; regular mushroom
    .byte $d6, $d6, $d9, $d9                      ; fire flower
    .byte $8d, $8d, $e4, $e4                      ; star
    .byte $76, $77, $78, $79                      ; 1-up mushroom

PowerUpAttributes:
    .byte $02, $01, $02, $01

.code

.proc DrawPowerUp
    
    ; locals
        Tile1       = temp_byte         ; - Tiles
        Tile2       = temp_byte + 1     ;
        YCoord      = temp_byte + 2     ; - Y position
        FlipControl = temp_byte + 3     ; - moving direction, used to flip enemies horizontally
        SpriteAtt   = temp_byte + 4     ; - enemy's sprite attributes
        XCoord      = temp_byte + 5     ; - X position
        LoopCounter = temp_byte + 7     ; - number of rows to draw
    ; end locals
    
    ldy Enemy_SprDataOffset+5                     ; get power-up's sprite data offset

    mb YCoord := Enemy_Rel_YPos + #8
    mb XCoord := Enemy_Rel_XPos                      ; get relative horizontal coordinate

    ldx PowerUpType                               ; get power-up type
    ; get attribute data for power-up type, add background priority bit if set
    mb SpriteAtt := PowerUpAttributes[ x ] | Enemy_SprAttrib[ 5 ]

    txa
    pha                                           ; save power-up type to the stack
        mb x := a * 4                             ; multiply by four to get proper offset
        lda #1
        sta LoopCounter                                   ; set counter here to draw two rows of sprite object
        sta FlipControl                                   ; init d1 of flip control

        repeat
            mb Tile1 := PowerUpGfxTable[ x ]
            mb a     := PowerUpGfxTable[ x + 1 ]  ; load right tile
            jsr DrawOneSpriteRow                  ; branch to draw one row of our power-up object
        until dec LoopCounter == negative

        ldy Enemy_SprDataOffset+5                 ; get sprite data offset again
    pla                                           ; pull saved power-up type from the stack
    ; if not a mushroom (regular or 1-up)
    if not zero && a <> #3
        sta Tile1                                ; store power-up type here for now
        ; get frame counter / 2 to change colors every two frames
        ; mask out all but d1 and d0 (previously d2 and d1)

        
        mb a, Sprite[ y ]::Attributes  := FrameCounter >> 1 & #%00000011 | Enemy_SprAttrib[ 5 ]
        mb Sprite[ 4 + y ]::Attributes := a       ; top right sprites for fire flower and star
        
        ; if temp_byte > 1 ( if not fireflower)
        if x := Tile1 - 1 != zero
            mb Sprite[  8 + y ]::Attributes := a                       ; otherwise set new palette bits  for bottom left
            mb Sprite[ 12 + y ]::Attributes := a                       ; and bottom right sprites as well for star only
        endif
        ; set horizontal flip bit for top right, bottom right sprite
        ; note these are only done for fire flower and star power-ups
        mb Sprite[  4 + y ]::Attributes := Sprite[  4 + y ]::Attributes | #%01000000
        mb Sprite[ 12 + y ]::Attributes := Sprite[ 12 + y ]::Attributes | #%01000000

    endif
    ; jump to check to see if power-up is offscreen at all, then leave
    jmp SprObjectOffscrChk

.endproc


    ; tiles arranged in top left, right, middle left, right, bottom left, right order

dataseg

EnemyGraphicsTable:
    .byte $fc, $fc, $aa, $ab, $ac, $ad            ; buzzy beetle frame 1
    .byte $fc, $fc, $ae, $af, $b0, $b1            ;              frame 2
    .byte $fc, $a5, $a6, $a7, $a8, $a9            ; koopa troopa frame 1
    .byte $fc, $a0, $a1, $a2, $a3, $a4            ;              frame 2
    .byte $69, $a5, $6a, $a7, $a8, $a9            ; koopa paratroopa frame 1
    .byte $6b, $a0, $6c, $a2, $a3, $a4            ;                  frame 2
    .byte $fc, $fc, $96, $97, $98, $99            ; spiny frame 1
    .byte $fc, $fc, $9a, $9b, $9c, $9d            ;       frame 2
    .byte $fc, $fc, $8f, $8e, $8e, $8f            ; spiny's egg frame 1
    .byte $fc, $fc, $95, $94, $94, $95            ;             frame 2
    .byte $fc, $fc, $dc, $dc, $df, $df            ; bloober frame 1
    .byte $dc, $dc, $dd, $dd, $de, $de            ;         frame 2
    .byte $fc, $fc, $b2, $b3, $b4, $b5            ; cheep-cheep frame 1
    .byte $fc, $fc, $b6, $b3, $b7, $b5            ;             frame 2
    .byte $fc, $fc, $70, $71, $72, $73            ; goomba
    .byte $fc, $fc, $6e, $6e, $6f, $6f            ; koopa shell frame 1 (upside-down)
    .byte $fc, $fc, $6d, $6d, $6f, $6f            ;             frame 2
    .byte $fc, $fc, $6f, $6f, $6e, $6e            ; koopa shell frame 1 (rightsideup)
    .byte $fc, $fc, $6f, $6f, $6d, $6d            ;             frame 2
    .byte $fc, $fc, $f4, $f4, $f5, $f5            ; buzzy beetle shell frame 1 (rightsideup)
    .byte $fc, $fc, $f4, $f4, $f5, $f5            ;                    frame 2
    .byte $fc, $fc, $f5, $f5, $f4, $f4            ; buzzy beetle shell frame 1 (upside-down)
    .byte $fc, $fc, $f5, $f5, $f4, $f4            ;                    frame 2
    .byte $fc, $fc, $fc, $fc, $ef, $ef            ; defeated goomba
    .byte $b9, $b8, $bb, $ba, $bc, $bc            ; lakitu frame 1
    .byte $fc, $fc, $bd, $bd, $bc, $bc            ;        frame 2
    .byte $7a, $7b, $da, $db, $d8, $d8            ; princess
    .byte $cd, $cd, $ce, $ce, $cf, $cf            ; mushroom retainer
    .byte $7d, $7c, $d1, $8c, $d3, $d2            ; hammer bro frame 1
    .byte $7d, $7c, $89, $88, $8b, $8a            ;            frame 2
    .byte $d5, $d4, $e3, $e2, $d3, $d2            ;            frame 3
    .byte $d5, $d4, $e3, $e2, $8b, $8a            ;            frame 4
    .byte $e5, $e5, $e6, $e6, $eb, $eb            ; piranha plant frame 1
    .byte $ec, $ec, $ed, $ed, $ee, $ee            ;               frame 2
    .byte $fc, $fc, $d0, $d0, $d7, $d7            ; podoboo
    .byte $bf, $be, $c1, $c0, $c2, $fc            ; bowser front frame 1
    .byte $c4, $c3, $c6, $c5, $c8, $c7            ; bowser rear frame 1
    .byte $bf, $be, $ca, $c9, $c2, $fc            ;        front frame 2
    .byte $c4, $c3, $c6, $c5, $cc, $cb            ;        rear frame 2
    .byte $fc, $fc, $e8, $e7, $ea, $e9            ; bullet bill
    .byte $f2, $f2, $f3, $f3, $f2, $f2            ; jumpspring frame 1
    .byte $f1, $f1, $f1, $f1, $fc, $fc            ;            frame 2
    .byte $f0, $f0, $fc, $fc, $fc, $fc            ;            frame 3

EnemyGfxTableOffsets:
    .byte $0c, $0c, $00, $0c, $0c, $a8, $54, $3c
    .byte $ea, $18, $48, $48, $cc, $c0, $18, $18
    .byte $18, $90, $24, $ff, $48, $9c, $d2, $d8
    .byte $f0, $f6, $fc

EnemyAttributeData:
    .byte $01, $02, $03, $02, $01, $01, $03, $03
    .byte $03, $01, $01, $02, $02, $21, $01, $02
    .byte $01, $01, $02, $ff, $02, $02, $01, $01
    .byte $02, $02, $02

EnemyAnimTimingBMask:
    .byte $08, $18

JumpspringFrameOffsets:
    .byte $18, $19, $1a, $19, $18

.code

.proc EnemyGfxHandler

    ; locals
        SpriteTileNumbers   =  temp_byte     ; temp_byte and temp_byte + 1 - used in DrawEnemyObjRow to hold sprite tile numbers
        YCoord              =  temp_byte + 2 ; - used to store Y position
        FlipControl         =  temp_byte + 3 ; - used to store moving direction, used to flip enemies horizontally
        SpriteAttributes    =  temp_byte + 4 ; - used to store enemy's sprite attributes
        XCoord              =  temp_byte + 5 ; - used to store X position
        SpriteDataOffset    =  $eb ; - used to hold sprite data offset
        AltEnemyStateBuffer =  $ec ; - used to hold either altered enemy state or special value used in gfx handler as condition
        EnemyStateBuffer    =  $ed ; - used to hold enemy state from buffer
        EnemyIdCode         =  $ef ; - used to hold enemy code used in gfx handler (may or may not resemble Enemy_ID values)    
    ; end locals    


    .export SprObjectOffscrChk

    mb YCoord     := Enemy_Y_Position[ x ]    ; get enemy object vertical position
    mb XCoord     := Enemy_Rel_XPos                   ; get enemy object horizontal position relative to screen
    mb y, SpriteDataOffset := Enemy_SprDataOffset[ x ]         ; get sprite data offset


    mb VerticalFlipFlag := #0                     ; initialize vertical flip flag by default

    mb FlipControl := Enemy_MovingDir[ x ]                ; get enemy object moving direction
    mb SpriteAttributes := Enemy_SprAttrib[ x ]                ; get enemy object sprite attributes
    ; if enemy object piranha plant && piranha plant moving downwards && Timer for movement expired:
    ; (plant inside a pipe?)
    if Enemy_ID[ x ] = #OBJECTID_PiranhaPlant && ldy PiranhaPlant_Y_Speed[ x ] == positive && ldy EnemyFrameTimer[ x ]
        rts
    endif
    ; Check for retainer:

    mb a, EnemyStateBuffer := Enemy_State[ x ]
    mb y := a & #%00011111                        ; nullify all but 5 LSB and use as default Y

    if Enemy_ID[ x ] = #OBJECTID_RetainerObject
        ldy #0                                    ; if found, nullify saved state in Y
        mb FlipControl := #1                  ; set value that will not be used OP
        lda #$15                                  ; set value $15 as code for mushroom retainer/princess object
    endif
    ; Check for bullet bill cannon type:

    if a = #OBJECTID_BulletBill_CannonVar         ; otherwise check for bullet bill object
        dec YCoord                       ; decrement saved vertical position
        lda #3
        if ldy EnemyFrameTimer[ x ]               ; get timer for enemy object
            ora #%00100000                        ; set priority bit
        endif
        sta SpriteAttributes                      ; set new sprite attributes
        mb y, EnemyStateBuffer := #0              ; nullify saved enemy state both in Y and in memory location here
        lda #$08                                  ; set specific value to unconditionally branch once
    endif
    ; Check for Jumpspring:

    if a = #OBJECTID_JumpspringObject                      ; check for jumpspring object
        ldy #3                                    ; set enemy state -2 MSB here for jumpspring object
        ldx JumpspringAnimCtrl                    ; get current frame number for jumpspring object
        lda JumpspringFrameOffsets,x              ; load data using frame number as offset
    endif
    ; Check for Podoboo:

    sta EnemyIdCode                                       ; store saved enemy object value here
    sty AltEnemyStateBuffer                                    ; and Y here (enemy state -2 MSB if not changed)
    ; if podoboo object && if moving downwards
    ldx ObjectOffset
    if a = #OBJECTID_Podoboo && Enemy_Y_Speed[ x ] == positive
        inc VerticalFlipFlag
    endif
    
    if BowserGfxFlag
        ldy #$16
        if a <> #1                                ; if not 1, draw bowser's rear
            iny
        endif
        sty EnemyIdCode                            ; set to $16, or $17 if bowser
    endif    

    if ldy EnemyIdCode = #OBJECTID_Goomba
        if Enemy_State[ x ] >= #2                 ; if in defeated state:
            mb x, AltEnemyStateBuffer := #4
        endif
        ; goomba animate:
        ; check for d5 clear in enemy object AND or timer disable flag is clear
        ; AND check for every eighth frame for eight frames
        if ! a & #%00100000 | TimerControl && !FrameCounter & #%00001000
            mb FlipControl := FlipControl ^ #%00000011            ; invert bits to flip horizontally every eight frames
        endif
    endif
    ; Check Bowser front:

    mb SpriteAttributes := EnemyAttributeData[ y ] | SpriteAttributes       ; E object as offset, and add to bits already loaded
    lda EnemyGfxTableOffsets,y                    ; load value based on enemy object as offset
    tax                                           ; save as X
    ldy AltEnemyStateBuffer                            ; get previously saved value

    if BowserGfxFlag
        if a = #1                                 ; if drawing front part

            if BowserBodyControls == bit7 set     ; check bowser's body control bits
                ldx #$de                          ; otherwise load offset for second frame(control's bowser's mouth)
            endif
            ; if bowser defeated:
            if EnemyStateBuffer & #%00100000
                FlipBowserOver:
                stx VerticalFlipFlag              ; set vertical flip flag to nonzero
            endif

            DrawBowser:
            jmp DrawEnemyObject                   ; draw bowser's graphics no
        endif
        
        ; CheckBowserRear:

        if BowserBodyControls & #1                ; check bowser's body control bits
            ldx #$e4                              ; otherwise load offset for second frame
        endif
        ; if bowser not defeated, branch back to jmp
        if !$ed & #%00100000 goto DrawBowser
        ; bowser defeated: subtract 16 pixels from saved V coord
        mb YCoord := YCoord - #$10
        jmp FlipBowserOver                        ; jump to set vertical flip flag
    endif
    ; Check for Spiny: check if graphics offset value loaded is for spiny
    if x = #$24
    ; if in egg state:
        if y = #$05
            ldx #$30                              ; set to spiny egg offset
            mb FlipControl := #2              ; set enemy direction to reverse sprites horizontally
            mb AltEnemyStateBuffer := #5          ; set enemy state : OP use sty
        endif
    else                                          ; not spiny:
        ; Check for Lakitu
        if x = #$90
            ; if d6 not set and timer in range, load alt frame for lakitu
            if !$ed & #%00100000 && FrenzyEnemyTimer < #$10
                ldx #$96
            endif
            jmp CheckDefeatedState                ; skip this next part if we found lakitu but alt frame not needed
        endif
        
        ; Check upside down shell:
        ; if enemy object < #4 AND state >= #2
        if EnemyIdCode < #4 && y >= #2
            ldx #$5a                              ; set for upside-down koopa shell by default
            if ldy EnemyIdCode = #OBJECTID_BuzzyBeetle
                ldx #$7e                          ; set for upside-down buzzy beetle shell if found
                inc YCoord               ; increment vertical position by one pixel
            endif
        endif
        ; check for value set here
        ; if enemy state < 2, do not change to shell, if
        ; enemy state => 2 but not = $04, leave shell upside-down
        if AltEnemyStateBuffer = #4
            ldx #$72                              ; set right-side up buzzy beetle shell by default
            inc YCoord                   ; increment saved vertical position by one pixel
            if ldy EnemyIdCode <> #OBJECTID_BuzzyBeetle            ; check for buzzy beetle object
                ldx #$66                          ; change to right-side up koopa shell if not found
                inc YCoord               ; and increment saved vertical position again
            endif
            ; Check for dead Goomba:
            if y = #OBJECTID_Goomba               ; check for goomba object (necessary if previously failed buzzy beetle object test)
            ldx #$54                              ; load for regular goomba
                if !$ed & #%00100000              ; note that this only gets performed if enemy state => $02
                    ldx #$8a                      ; load offset for defeated goomba
                    dec YCoord           ; set different value and decrement saved vertical position
                endif
            endif
        endif

    endif                                         ; end "is this spiny" IF block
    ; CheckForHammerBro:

    ldy ObjectOffset
    if lda EnemyIdCode = #OBJECTID_HammerBro

        if ! EnemyStateBuffer goto CheckToAnimateEnemy
        if ! a & #%00001000 goto CheckDefeatedState

        ldx #$b4                                  ; otherwise load offset for different frame
    else Z clear                                  ; not hammerbro:
        if x <> #$48                              ; If not cheep-cheep offset:

            if EnemyIntervalTimer[ y ] >= #5 goto CheckDefeatedState

            if x = #$3c                           ; check for bloober offset loaded
                if a = #1 goto CheckDefeatedState
                inc YCoord               ; increment saved vertical coordinate three pixels
                inc YCoord
                inc YCoord
                jmp CheckAnimationStop            ; and do something else
            endif

        endif
    endif

    CheckToAnimateEnemy:

    if lda EnemyIdCode <> #OBJECTID_Goomba && a <> #OBJECTID_BulletBill_FrenzyVar && a <> #OBJECTID_Podoboo && a < #$18

        ldy #0                                    ; OP residual op with reg y here
        if a = #$15                               ; check for mushroom retainer/princess object
            iny                                   ; residual instruction
            if WorldNumber >= #WORLD8 goto CheckDefeatedState          ; are we on world 8?
            ldx #$a2                              ; otherwise, set for mushroom retainer object instead
            mb AltEnemyStateBuffer := #3                           ; set alternate state here
        else Z clear
            ; Check for animation frame:
            ; mask it (partly residual, one byte not ever used)
            if !FrameCounter & EnemyAnimTimingBMask[ y ]

                CheckAnimationStop:
                ; check for d7 or d5, and TimerControl all at zero:
                if ! EnemyStateBuffer & #%10100000 | TimerControl
                    txa
                    mb x := a + #6                ; ad #6 to offset to animate various enemy objects
                endif

            endif
        endif
    endif

    CheckDefeatedState:

    if EnemyStateBuffer & #%00100000 && lda EnemyIdCode >= #4
        mb y, VerticalFlipFlag := #1              ; set vertical flip flag
        dey                                       ; #0
        sty AltEnemyStateBuffer                        ; init saved value here
    endif

    DrawEnemyObject:

    ldy SpriteDataOffset                          ; load sprite data offset
    jsr DrawEnemyObjRow                           ; draw six tiles of data
    jsr DrawEnemyObjRow                           ; into sprite data
    jsr DrawEnemyObjRow

    ldx ObjectOffset                              ; get enemy object offset
    ldy Enemy_SprDataOffset,x                     ; get sprite data offset

    
    if EnemyIdCode = #OBJECTID_BulletBill_FrenzyVar
        SkipToOffScrChk:
        jmp SprObjectOffscrChk                    ; jump if found
    endif
    ; CheckForVerticalFlip:

    if VerticalFlipFlag                           ; check if vertical flip flag is set here

        mb a := Sprite[ y ]::Attributes | #%10000000                   ; set bit for vertical flip

        mb y := y + 2                             ; increment two bytes so that we store the vertical flip
        jsr DumpSixSpr                            ; in attribute bytes of enemy obj sprite data
        mb y := y - 2                             ; now go back to the Y coordinate offset

        tya
        tax                                       ; give offset to X

        if EnemyIdCode <> #OBJECTID_HammerBro && a <> #OBJECTID_Lakitu && a < #$15
        ; if not selected objects or => $15, set offset in X for next row
            mb a := x
            mb x := a + #8
        endif
        ; Flip Enemy Vertically:

        mb a := Sprite[ x ]::Tilenumber           ; load first or second row tiles
        pha
            mb a := Sprite[ 4 + x ]::Tilenumber
            pha
                mb Sprite[ 0 + x ]::Tilenumber := Sprite[ 16 + y ]::Tilenumber
                mb Sprite[ 4 + x ]::Tilenumber := Sprite[ 20 + y ]::Tilenumber
            pla                                   ; pull first or second row tiles from stack
            mb Sprite[ 20 + y]::Tilenumber := a   ; and save in third row
        pla
        mb Sprite[ 16 + y ]::Tilenumber := a

    endif
    ; CheckForESymmetry:
    ; are we drawing bowser at all?
    if BowserGfxFlag goto SkipToOffScrChk

    lda EnemyIdCode
    ldx AltEnemyStateBuffer                            ; get alternate enemy state
    if a = #OBJECTID_HammerBro                    ; check for hammer bro object
        jmp SprObjectOffscrChk                    ; jump if found
    endif

    if a <> #OBJECTID_Bloober && a <> #OBJECTID_PiranhaPlant && a <> #OBJECTID_Podoboo
    ; if spiny ( and not an egg)
        if a = #OBJECTID_Spiny && x <> #$05 goto CheckToMirrorLakitu

        if a = #$15                               ; check for princess/mushroom retainer object
    ; set horizontal flip on bottom right sprite
            mb Sprite[ 20 + y]::Attributes := #$42                     ; note that palette bits were already set earlier
        endif

        if x < #2 goto CheckToMirrorLakitu

    endif
    ; MirrorEnemyGfx:

    if !BowserGfxFlag                             ; if enemy object is bowser, skip all of this

        mb a := Sprite[ y ]::Attributes & #%10100011
        mb Sprite[  0 + y ]::Attributes := a      ; save vertical flip, priority, and palette bits
        mb Sprite[  8 + y ]::Attributes := a      ; in left sprite column of enemy object OAM data
        mb Sprite[ 16 + y ]::Attributes := a

        ora #%01000000                            ; set horizontal flip

        if x = #$05                               ; check for state used by spiny's egg
            ora #%10000000                        ; otherwise set vertical flip
        endif

        mb Sprite[  4 + y ]::Attributes := a      ; set bits of right sprite column
        mb Sprite[ 12 + y ]::Attributes := a      ; of enemy object sprite data
        mb Sprite[ 20 + y ]::Attributes := a

        if x = #4                                 ; check alternate enemy state
    ; get second row left sprite attributes
            mb a := Sprite[ 8 + y ]::Attributes | #%10000000
            mb Sprite[  8 + y ]::Attributes := a                       ; store bits with vertical flip in
            mb Sprite[ 16 + y ]::Attributes := a                       ; second and third row left sprites
            mb a := a | #%01000000
            mb Sprite[ 12 + y ]::Attributes := a                       ; store with horizontal and vertical flip in
            mb Sprite[ 20 + y ]::Attributes := a                       ; second and third row right sprites
        endif
    endif

    CheckToMirrorLakitu:

    if EnemyIdCode <> #OBJECTID_Lakitu goto CheckToMirrorJSpring
    
    ; Lakitu found:
    if !VerticalFlipFlag
        ; save vertical flip and palette bits in third row left sprite
        mb Sprite[ 16 + y ]::Attributes := Sprite[ 16 + y ]::Attributes & #%10000001
        ; set horizontal flip and palette bits in third row right sprite
        mb a, Sprite[ 20 + y ]::Attributes := Sprite[ 20 + y ]::Attributes | #%01000001

        if ldx FrenzyEnemyTimer >= #$10 goto SprObjectOffscrChk        ; branch if timer has not reached a certain range

        mb Sprite[ 12 + y ]::Attributes := a      ; otherwise set same for second row right sprite
        ; preserve vertical flip and palette bits for left sprite
        mb Sprite[ 8 + y ]::Attributes := a & #%10000001

    else C clear
        ; get first row left sprite attributes, save vertical flip and palette bits
        mb Sprite[ y ]::Attributes := Sprite[ y ]::Attributes & #%10000001
        ; get first row right sprite attributes,  set horizontal flip and palette bits
        ; note that vertical flip is left as-is
        mb Sprite[ 4 + y ]::Attributes := Sprite[ 4 + y ]::Attributes | #%01000001

        CheckToMirrorJSpring:

        if EnemyIdCode >= #$18                              ; check for jumpspring object (any frame)

            lda #$82
            mb Sprite[  8 + y ]::Attributes := a                       ; set vertical flip and palette bits of
            mb Sprite[ 16 + y ]::Attributes := a                       ; second and third row left sprites

            mb a, Sprite[ 12 + y ]::Attributes := a | #%01000000       ; set, in addition to those, horizontal flip
            mb    Sprite[ 20 + y ]::Attributes := a                    ; for second and third row right sprites
        endif
    endif


    SprObjectOffscrChk:                           ; this is exported, called by DrawPowerUp

    ldx ObjectOffset                              ; get enemy buffer offset
    mb a := Enemy_OffscreenBits >> 3              ; puts d2 into carry

    pha                                           ; save to stack
        if carry set
            lda #4                                ; set for right column sprites
            jsr MoveESprColOffscreen              ; and move them offscreen
        endif
    pla                                           ; get from stack

    mb a := a >> 1                                ; move d3 to carry
    pha                                           ; save to stack
        if carry set
            lda #0                                ; set for left column sprites,
            jsr MoveESprColOffscreen              ; move them offscreen
        endif
    pla                                           ; get from stack again

    mb a := a >> 2                                ; move d5 to carry this time
    pha                                           ; save to stack again
        if carry set
            lda #$10                              ; set for third row of sprites
            jsr MoveESprRowOffscreen              ; and move them offscreen
        endif
    pla

    mb a := a >> 1                                ; move d6 into carry
    pha                                           ; save to stack
        if carry set
            lda #$08                              ; set for second and third rows
            jsr MoveESprRowOffscreen              ; move them offscreen
        endif
    pla                                           ; get from stack once more

    if a >> 1 == carry set                        ; move d7 into carry
        jsr MoveESprRowOffscreen                  ; move all sprites offscreen (A should be 0 by now)
        if Enemy_ID[ x ] <> #OBJECTID_Podoboo && Enemy_Y_HighPos[ x ] = #2      ; if not yet past the bottom of the screen, branch
            jsr EraseEnemyObject
        endif
    endif

    rts

    DrawEnemyObjRow:
    ; load two tiles of enemy graphics
    mb SpriteTileNumbers := EnemyGraphicsTable[ x ]
    mb a := EnemyGraphicsTable[ 1 + x ]

.endproc

.proc DrawOneSpriteRow

    sta $01 ; store sprite tile number
    jmp DrawSpriteObject                          ; draw them

.endproc

.proc MoveESprRowOffscreen

    mb y := a + Enemy_SprDataOffset[ x ]
    lda #$f8
    jmp DumpTwoSpr                                ; move first row of sprites offscreen

.endproc

.proc MoveESprColOffscreen

    mb y := a + Enemy_SprDataOffset[ x ]          ; use as offset

    jsr MoveColOffscreen                          ; move first and second row sprites in column offscreen
    mb Sprite[ 16 + y ]::Data := a                ; move third row sprite in column offscreen
    rts

.endproc


dataseg

DefaultBlockObjTiles:
    .byte $85, $85, $86, $86                      ; brick w/ line (these are sprite tiles, not BG!)

.code

.proc DrawBlock

    ; locals
        SpriteTileNumbers   =  temp_byte     ; temp_byte and temp_byte + 1 - used in DrawEnemyObjRow to hold sprite tile numbers
        YCoord              =  temp_byte + 2 ; - used to store Y position
        FlipControl         =  temp_byte + 3 ; - used to store moving direction, used to flip enemies horizontally
        SpriteAttributes    =  temp_byte + 4 ; - used to store enemy's sprite attributes
        XCoord              =  temp_byte + 5 ; - used to store X position
    ; end locals    

    .export MoveColOffscreen, ChkLeftCo


    mb YCoord  := Block_Rel_YPos           ; get relative vertical coordinate of block object
    mb XCoord  := Block_Rel_XPos           ; get relative horizontal coordinate of block object
    mb SpriteAttributes := #3                       ; set attribute byte here

    mb FlipControl  := a >> 1                   ; set a := 1, then store.. set horizontal flip bit here (will not be used)

    ldy Block_SprDataOffset,x                     ; get sprite data offset

    ldx #0                                        ; reset X for use as offset to tile data
    repeat
        mb SpriteTileNumbers := DefaultBlockObjTiles[ x ]                      ; get left tile number
        mb a := DefaultBlockObjTiles[ 1 + x ]     ; get right tile number
        jsr DrawOneSpriteRow                      ; do sub to write tile numbers to first row of sprites
    until x = #4                                  ; sub increments x = x + 2

    ldx ObjectOffset                              ; get block object offset
    ldy Block_SprDataOffset,x                     ; get sprite data offset

    if AreaType <> #1                             ; check for ground level type area
        mb Sprite::Tilenumber[ y ] := #$86        ; remove brick tiles with lines
        mb Sprite::Tilenumber[ 4 + y ] := a       ; and replace then with lineless brick tiles
    endif

    if Block_Metatile[ x ] = #$c4                 ; check replacement metatile for used block

        lda #$87                                  ; set A for used block tile
        iny                                       ; increment Y to write to tile bytes
        jsr DumpFourSpr                           ; do sub to dump into all four sprites
        dey                                       ; return Y to original offset

        lda #3                                    ; set palette bits
        if ldx AreaType : dex != zero             ; if not ground level type area
            lsr                                   ; set to $01
        endif

        ldx ObjectOffset                          ; put block object offset back in X

        mb Sprite[      y ]::Attributes := a      ; store attribute byte as-is in first sprite
        mb Sprite[  4 + y ]::Attributes := a | #%01000000              ; set horizontal flip bit for second sprite
        mb Sprite[ 12 + y ]::Attributes := a | #%10000000              ; set both flip bits for fourth sprite
        mb Sprite[  8 + y ]::Attributes := a & #%10000011              ; set vertical flip bit for third sprite
    endif
    ; Block Offscr:

    lda Block_OffscreenBits                       ; get offscreen bits for block object
    pha                                           ; save to stack
        if a & #%00000100                         ; check to see if d2 in offscreen bits are set
            mb Sprite[  4 + y ]::Y_Position := #$f8                    ; move offscreen two OAMs on the right side
            mb Sprite[ 12 + y ]::Y_Position := a                       ; $f8
        endif
    pla                                           ; pull offscreen bits from stack

    ChkLeftCo:

    if a & #%00001000                             ; check to see if d3 in offscreen bits are set
        MoveColOffscreen:
        lda #$f8                                  ; move offscreen two OAMs
        mb Sprite[ 0 + y ]::Y_Position := a       ; on the left side (or two rows of enemy on either side
        mb Sprite[ 8 + y ]::Y_Position := a       ; if branched here from enemy graphics handler)
    endif

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DrawBrickChunks

    ; locals
        ; temp_byte - used to hold palette bits for attribute byte or relative X position
    ; end locals

    mb temp_byte := #2                            ; set palette bits here

    lda #$75                                      ; set tile number for ball (something residual, likely)

    if ldy GameEngineSubroutine <> #5             ; if end-of-level routine not running,
        mb temp_byte := #3                        ; set different palette bits
        lda #$84                                  ; and set tile number for brick chunks
    endif

    ldy Block_SprDataOffset,x                     ; get OAM data offset
    iny                                           ; increment to start with tile bytes in OAM
    jsr DumpFourSpr                               ; do sub to dump tile number into all four sprites
    ; move low nybble to high, get what was originally d3-d2 of low nybble, add palette bit
    mb a := FrameCounter << 4 & #%11000000 | temp_byte

    iny                                           ; increment offset for attribute bytes
    jsr DumpFourSpr                               ; do sub to dump attribute data into all four sprites
    dey
    dey                                           ; decrement offset to Y coordinate

    lda Block_Rel_YPos                            ; get first block object's relative vertical coordinate
    jsr DumpTwoSpr                                ; do sub to dump current Y coordinate into two sprites
    ; get first block object's relative horizontal coordinate, save into X coordinate of first sprite
    mb Sprite[ y ]::X_Position := Block_Rel_XPos
    ; subtract coordinate of left side from original coordinate, store result as relative horizontal coordinate of original
    mb temp_byte := Block_Orig_XPos[ x ] - ScreenLeft_X_Pos
    ; difference of relative original - current, add original relative position, plus 6 pixels to position second brick chunk correctly:
    mb Sprite[ 4 + y ]::X_Position := a - Block_Rel_XPos +c temp_byte +c #6
    mb Sprite[ 8 + y ]::Y_Position := Block_Rel_YPos[ 1 ]              ; get second block object's relative vertical coordinate
    mb Sprite[ 12 + y]::Y_Position := a           ; dump into Y coordinates of third and fourth sprites
    ; get second block object's relative horizontal coordinate
    ; save into X coordinate of third sprite
    mb Sprite[ 8 + y ]::X_Position := Block_Rel_XPos[ 1 ]
    ; use original relative horizontal position, get difference of relative positions of original - current
    ; add original relative position to result, plus 6 pixels to position fourth brick chunk correctly
    ; save into X coordinate of fourth sprite
    mb Sprite[ 12 + y ]::X_Position := temp_byte - Block_Rel_XPos[ 1 ] +c temp_byte +c #6

    lda Block_OffscreenBits                       ; get offscreen bits for block object
    jsr ChkLeftCo                                 ; do sub to move left half of sprites offscreen if necessary

    if Block_OffscreenBits << 1 == C set
        lda #$f8
        jsr DumpTwoSpr                            ; move top sprites offscreen
    endif
    ; here, remove two right side chunks if offscreen to the right
    ; if relative position on right side of screen AND left side coord is greater than right side coord
    if temp_byte == negative && Sprite[ y ]::X_Position >= Sprite[ 4 + y ]::X_Position
        lda #$f8                                  ; move right half of sprites offscreen
        mb Sprite[  4 + y ]::Y_Position := a
        mb Sprite[ 12 + y ]::Y_Position := a
    endif

    rts                                           ; leave

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DrawFireball

    ldy FBall_SprDataOffset,x                                          ; get fireball's sprite data offset
    mb Sprite[ y ]::Y_Position := Fireball_Rel_YPos                    ; store as sprite Y coordinate
    mb Sprite[ y ]::X_Position := Fireball_Rel_XPos                    ; store as sprite X coordinate, then do shared code

.endproc

.proc DrawFirebar

    mb a := FrameCounter >> 2

    pha
    ; set either tile $64 or $65 as fireball tile every four frames
        mb Sprite[ y ]::Tilenumber := a & #1 ^ #$64
    pla                                           ; get from stack

    lsr                                           ; put bit 3 into carry
    lsr
    ; flip both ways every eight frames
    lda #2
    if C set
        ora #%11000000
    endif

    mb Sprite[ y ]::Attributes := a               ; store attribute byte and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

ExplosionTiles:
    .byte $68, $67, $66

.code

.proc DrawExplosion_Fireball

    .export DrawExplosion_Fireworks

    ldy Alt_SprDataOffset,x                       ; get OAM data offset of alternate sort for fireball's explosion
    lda Fireball_State,x                          ; load fireball state
    inc Fireball_State,x                          ; increment state for next frame
    ; check if fireball is still valid, if so, draw explosion
    if a >> 1 & #%00000111 < #3

        DrawExplosion_Fireworks:

        tax                                       ; use whatever's in A for offset
        lda ExplosionTiles,x                      ; get tile number using offset
        iny                                       ; increment Y (contains sprite data offset)
        jsr DumpFourSpr                           ; and dump into tile number part of sprite data
        dey                                       ; decrement Y so we have the proper offset again

        ldx ObjectOffset                          ; return enemy object buffer offset to X
        ; subtract four pixels vertically for first and third sprites
        mb Sprite[     y ]::Y_Position := Fireball_Rel_YPos - #4
        mb Sprite[ 8 + y ]::Y_Position := a       ; same
        ; add eight pixels vertically
        ; for second and fourth sprites
        mb Sprite[  4 + y ]::Y_Position := a + #8
        mb Sprite[ 12 + y ]::Y_Position := a      ; same
        ; subtract four pixels horizontally
        ; for first and second sprites
        mb Sprite[ 0 + y ]::X_Position := Fireball_Rel_XPos - #4
        mb Sprite[ 4 + y ]::X_Position := a       ; same
        ; add eight pixels horizontally
        ; for third and fourth sprites
        mb Sprite[  8 + y ]::X_Position := a + #8
        mb Sprite[ 12 + y ]::X_Position := a      ; same

        mb Sprite[  0 + y ]::Attributes := #%00000010                  ; set no flip at all for first sprite
        mb Sprite[  4 + y ]::Attributes := #%10000010                  ; set vertical flip for second sprite
        mb Sprite[  8 + y ]::Attributes := #%01000010                  ; set horizontal flip for third sprite
        mb Sprite[ 12 + y ]::Attributes := #%11000010                  ; set both flips for fourth sprite

        rts

    endif

    mb Fireball_State[ x ] := #0                  ; clear fireball state to kill it
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DrawSmallPlatform

    ldy Enemy_SprDataOffset,x                     ; get OAM data offset
    lda #$5b                                      ; load tile number for small platforms
    iny                                           ; increment offset for tile numbers
    jsr DumpSixSpr                                ; dump tile number into all six sprites
    iny                                           ; increment offset for attributes

    lda #2                                        ; load palette controls
    jsr DumpSixSpr                                ; dump attributes into all six sprites
    dey                                           ; decrement for original offset
    dey

    lda Enemy_Rel_XPos                            ; get relative horizontal coordinate
    mb Sprite[  0 + y ]::X_Position := a
    mb Sprite[ 12 + y ]::X_Position := a          ; dump as X coordinate into first and fourth sprites

    mb a := a + #8
    mb Sprite[  4 + y ]::X_Position := a          ; dump into second and fifth sprites
    mb Sprite[ 16 + y ]::X_Position := a

    mb a := a + #8
    mb Sprite[  8 + y ]::X_Position := a          ; dump into third and sixth sprites
    mb Sprite[ 20 + y ]::X_Position := a

    lda Enemy_Y_Position,x                        ; get vertical coordinate
    tax
    pha                                           ; save to stack
        if x < #$20                               ; if vertical coordinate above status bar
            lda #$f8                              ; move first three sprites offscreen
        endif

        jsr DumpThreeSpr                          ; dump vertical coordinate into Y coordinates
    pla                                           ; pull from stack

    mb x := a + #$80                              ; add 128 pixels

    if x < #$20                                   ; if above status bar (taking wrap into account)
        lda #$f8                                  ; move last three sprites offscreen
    endif

    mb Sprite[ 12 + y ]::Y_Position := a          ; dump vertical coordinate + 128 pixels
    mb Sprite[ 16 + y ]::Y_Position := a          ; into Y coordinates
    mb Sprite[ 20 + y ]::Y_Position := a

    lda Enemy_OffscreenBits                       ; get offscreen bits
    pha                                           ; save to stack
        if a & #%00001000                         ; check d3
            lda #$f8                              ; if d3 was set, move first and
            mb Sprite[  0 + y ]::Y_Position := a                       ; fourth sprites offscreen
            mb Sprite[ 12 + y ]::Y_Position := a
        endif
    pla                                           ; move out and back into stack
    pha
        if a & #%00000100                         ; check d2
            lda #$f8                              ; if d2 was set, move second and
            mb Sprite[  4 + y ]::Y_Position := a                       ; fifth sprites offscreen
            mb Sprite[ 16 + y ]::Y_Position := a
        endif
    pla                                           ; get from stack

    if a & #%00000010                             ; check d1
        lda #$f8                                  ; if d1 was set, move third and
        mb Sprite[  8 + y ]::Y_Position := a      ; sixth sprites offscreen
        mb Sprite[ 20 + y ]::Y_Position := a
    endif

    ldx ObjectOffset                              ; get enemy object offset and leave
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DrawBubble
    ; if player's vertical high position = 1 ( in screen ) and Not offscreen
    if ldy Player_Y_HighPos : dey == zero && !Bubble_OffscreenBits & #%00001000
        ldy Bubble_SprDataOffset,x                ; get air bubble's OAM data offset
        mb Sprite[ y ]::X_Position := Bubble_Rel_XPos
        mb Sprite[ y ]::Y_Position := Bubble_Rel_YPos
        mb Sprite[ y ]::Tilenumber := #$74        ; put air bubble tile into OAM
        mb Sprite[ y ]::Attributes := #2          ; set attribute byte
    endif

    rts                                           ; leave

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

PlayerGfxTblOffsets:
    .byte $20, $28, $c8, $18, $00, $40, $50, $58
    .byte $80, $88, $b8, $78, $60, $a0, $b0, $b8
    
    ; tiles arranged in order, 2 tiles per row, top to bottom

PlayerGraphicsTable:
    ; big player table
    .byte $00, $01, $02, $03, $04, $05, $06, $07                       ; walking frame 1
    .byte $08, $09, $0a, $0b, $0c, $0d, $0e, $0f                       ;         frame 2
    .byte $10, $11, $12, $13, $14, $15, $16, $17                       ;         frame 3
    .byte $18, $19, $1a, $1b, $1c, $1d, $1e, $1f                       ; skidding
    .byte $20, $21, $22, $23, $24, $25, $26, $27                       ; jumping
    .byte $08, $09, $28, $29, $2a, $2b, $2c, $2d                       ; swimming frame 1
    .byte $08, $09, $0a, $0b, $0c, $30, $2c, $2d                       ;          frame 2
    .byte $08, $09, $0a, $0b, $2e, $2f, $2c, $2d                       ;          frame 3
    .byte $08, $09, $28, $29, $2a, $2b, $5c, $5d                       ; climbing frame 1
    .byte $08, $09, $0a, $0b, $0c, $0d, $5e, $5f                       ;          frame 2
    .byte $fc, $fc, $08, $09, $58, $59, $5a, $5a                       ; crouching
    .byte $08, $09, $28, $29, $2a, $2b, $0e, $0f                       ; fireball throwing
    ; small player table
    .byte $fc, $fc, $fc, $fc, $32, $33, $34, $35                       ; walking frame 1
    .byte $fc, $fc, $fc, $fc, $36, $37, $38, $39                       ;         frame 2
    .byte $fc, $fc, $fc, $fc, $3a, $37, $3b, $3c                       ;         frame 3
    .byte $fc, $fc, $fc, $fc, $3d, $3e, $3f, $40                       ; skidding
    .byte $fc, $fc, $fc, $fc, $32, $41, $42, $43                       ; jumping
    .byte $fc, $fc, $fc, $fc, $32, $33, $44, $45                       ; swimming frame 1
    .byte $fc, $fc, $fc, $fc, $32, $33, $44, $47                       ;          frame 2
    .byte $fc, $fc, $fc, $fc, $32, $33
    SwimTileRepOffset:    .byte         $48, $49                       ;          frame 3
    .byte $fc, $fc, $fc, $fc, $32, $33, $90, $91                       ; climbing frame 1
    .byte $fc, $fc, $fc, $fc, $3a, $37, $92, $93                       ;          frame 2
    .byte $fc, $fc, $fc, $fc, $9e, $9e, $9f, $9f                       ; killed
    ; used by both player sizes
    .byte $fc, $fc, $fc, $fc, $3a, $37, $4f, $4f                       ; small player standing
    .byte $fc, $fc, $00, $01, $4c, $4d, $4e, $4e                       ; intermediate grow frame
    .byte $00, $01, $4c, $4d, $4a, $4a, $4b, $4b                       ; big player standing

SwimKickTileNum:
    .byte $31, $46

.code

.proc PlayerGfxHandler

    ; locals
        ; temp_byte - used to store player's vertical offscreen bits
    ; end locals

    if InjuryTimer && FrameCounter >> 1 == C set goto Exit
    ; HERE: player's injured invincibility timer is not set OR we are on an even frame

    if GameEngineSubroutine <> #$0b

        if !PlayerChangeSizeFlag                  ; if grow/shrink flag NOT set
            ; if swimming flag not set && Player not in normal state ( OP: cmp? )
            if ldy SwimmingFlag && Player_State <> #0

                jsr FindPlayerAction              ; jump and return

                if FrameCounter & #%00000100 goto Exit                 ; check frame counter for d2 set (8 frames every 8 )

                tax                               ; #0
                ldy Player_SprDataOffset          ; get player sprite data offset

                if PlayerFacingDir >> 1 == C clear                     ; get player's facing direction
                    mb y := y + 4                 ; move to next OAM data if facing left
                endif
                ; if tile number of seventh/eighth sprite <> tile in player graphics table
                ; increment X for second tile
                if PlayerSize == SmallMario
                    if Sprite[ 24 + y ]::Tilenumber = SwimTileRepOffset goto Exit
                    inx
                endif
                ; overwrite tile number in sprite 7/8
                ; to animate player's feet when swimming
                mb Sprite[ 24 + y ]::Tilenumber := SwimKickTileNum[ x ]

                Exit:

                rts

            endif

            FindPlayerAction:

            jsr ProcessPlayerAction               ; find proper offset to graphics table by player's actions
            jmp PlayerGfxProcessing               ; draw player, then process for fireball throwing

        endif
        ; Change size:

        jsr HandleChangeSize                      ; find proper offset to graphics table for grow/shrink
        jmp PlayerGfxProcessing                   ; draw player, then process for fireball throwing

    endif                                         ; else GameEngineSubroutine = #$0b
    ; Player killed:

        ldy #$0e                                  ; load offset for player killed
        lda PlayerGfxTblOffsets,y                 ; get offset to graphics table

    PlayerGfxProcessing:

    sta PlayerGfxOffset                           ; store offset to graphics table here

    lda #4
    jsr RenderPlayerSub                           ; draw player based on offset loaded
    jsr ChkForPlayerAttrib                        ; set horizontal flip bits as necessary

    if FireballThrowingTimer

        ldy #0                                    ; set value to initialize by default
            lda PlayerAnimTimer                   ; get animation frame timer
            cmp FireballThrowingTimer             ; compare to fireball throw timer
        sty FireballThrowingTimer                 ; initialize fireball throw timer

        if less                                   ; if animation frame timer < fireball throw timer
            sta FireballThrowingTimer             ; store animation timer into fireball throw timer

            ldy #$07                              ; load offset for throwing
            mb PlayerGfxOffset := PlayerGfxTblOffsets[ y ]             ; store it for use later

            ldy #4                                ; set to update four sprite rows by default
            if Player_X_Speed | Left_Right_Buttons ; check for horizontal speed or left/right button press
                dey                               ; otherwise set to update only three sprite rows
            endif

            tya                                   ; save in A for use
            jsr RenderPlayerSub                   ; in sub, draw player object again
        endif
    endif
    ; PlayerOffscreenChk:
    ; get player's offscreen bits move vertical bits to low nybble
    mb temp_byte := Player_OffscreenBits >> 4
    ldx #3                                        ; check all four rows of player sprites
    ; add 24 bytes to start at bottom row
    mb y := Player_SprDataOffset + #$18

    repeat
        lda #$f8                                  ; load offscreen Y coordinate just in case

        lsr temp_byte                             ; shift bit into carry
        if C set
            jsr DumpTwoSpr                        ; dump offscreen Y coordinate into sprite data
        endif

        mb a := y
        mb y := a - #8                            ; sub to do the next row up
    until dex == negative                         ; do this until all sprite rows are checked
    rts                                           ; then we are done!

.endproc

 ; ------------------------------------------------------------------------------------------------

dataseg

IntermediatePlayerData:
    .byte $58, $01, $00, $60, $ff, $04

.code

.proc DrawPlayer_Intermediate

    ; load data to display player as he always
    ; appears on world/lives display

    ldx #$05
    repeat                                        ; x = 5 to 0
        mb $02[ x ] := IntermediatePlayerData[ x ]
    until dex == negative

    ldx #$b8                                      ; load offset for small standing
    ldy #4                                        ; load sprite data offset
    jsr DrawPlayerLoop                            ; draw player accordingly
    ; get empty sprite attributes, set horizontal flip bit for bottom-right sprite
    mb Sprite[ 32 ]::Attributes := Sprite[ 36 ]::Attributes | #%01000000

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------
    
.proc RenderPlayerSub

    
    ; locals
        Tile1       = temp_byte         ; - Tiles
        Tile2       = temp_byte + 1     ;
        YCoord      = temp_byte + 2     ; - Y position
        FlipControl = temp_byte + 3     ; - moving direction, used to flip enemies horizontally
        SpriteAtt   = temp_byte + 4     ; - enemy's sprite attributes
        XCoord      = temp_byte + 5     ; - X position
        LoopCounter = temp_byte + 7     ; - number of rows to draw
    ; end locals
    
    .export DrawPlayerLoop
    

    sta LoopCounter                               ; store number of rows of sprites to draw
    mb a, Player_Pos_ForScroll := Player_Rel_XPos                      ; store player's relative horizontal position
    sta XCoord                                    ; store it here also

    mb YCoord      := Player_Rel_YPos             ; store player's vertical position
    mb FlipControl := PlayerFacingDir             ; store player's facing direction
    mb SpriteAtt   := Player_SprAttrib            ; store player's sprite attributes

    ldx PlayerGfxOffset                           ; load graphics table offset
    ldy Player_SprDataOffset                      ; get player's sprite data offset

    DrawPlayerLoop:

    repeat
        mb Tile1 := PlayerGraphicsTable[ x ]                       ; load player's left side
        lda PlayerGraphicsTable + 1,x             ; now load right side
        jsr DrawOneSpriteRow
    until dec LoopCounter == zero                 ; decrement rows of sprites to draw

    rts

.endproc

.proc ProcessPlayerAction

    if Player_State = #3 goto ActionClimbing      ; if climbing, branch here
    if            a = #2 goto ActionFalling       ; if falling, branch here

    if a = #1                                     ; if jumping:
        if SwimmingFlag goto ActionSwimming       ; if swimming flag set, branch elsewhere
        ldy #$06                                  ; load offset for crouching
        if CrouchingFlag goto NonAnimatedActs     ; get crouching flag
        ldy #0                                    ; otherwise load offset for jumping
    else
        ; not jumping, on ground:
        ldy #$06                                  ; load offset for crouching
        if !CrouchingFlag                         ; get crouching flag
            ldy #2                                ; load offset for standing
            if Player_X_Speed | Left_Right_Buttons                     ; check player's horizontal speed
                ; if moving slow enough OR pressing the same direction as movment, branch
                if Player_XSpeedAbsolute < #9 || \
                    Player_MovingDir & PlayerFacingDir goto ActionWalkRun
                iny                               ; otherwise increment to skid offset ($03)
            endif
        endif
    endif

    NonAnimatedActs:

    jsr GetGfxOffsetAdder                         ; do a sub here to get offset adder for graphics table

    mb PlayerAnimCtrl := #0                       ; initialize animation frame control
    lda PlayerGfxTblOffsets,y                     ; load offset to graphics table using size as offset

    rts

    ActionFalling:

    ldy #4                                        ; load offset for walking/running
    jsr GetGfxOffsetAdder                         ; get offset to graphics table
    jmp GetCurrentAnimOffset                      ; execute instructions for falling state

    ActionWalkRun:

    ldy #4                                        ; load offset for walking/running
    jsr GetGfxOffsetAdder                         ; get offset to graphics table
    jmp FourFrameExtent                           ; execute instructions for normal state

    ActionClimbing:

    ldy #$05                                      ; load offset for climbing
    if !Player_Y_Speed goto NonAnimatedActs       ; if no speed, branch, use offset as-is

    jsr GetGfxOffsetAdder                         ; otherwise get offset for graphics table
    jmp ThreeFrameExtent                          ; then skip ahead to more code

    ActionSwimming:

    ldy #1                                        ; load offset for swimming
    jsr GetGfxOffsetAdder
    ; check jump/swim timer | Animation control and BUTTON_A not pressed
    if !JumpSwimTimer | PlayerAnimCtrl && A_B_Buttons << 1 == carry clear

        GetCurrentAnimOffset:

        lda PlayerAnimCtrl                        ; get animation frame control
        jmp GetOffsetFromAnimCtrl                 ; jump to get proper offset to graphics table
    endif

    FourFrameExtent:
    lda #3                                        ; load upper extent for frame control
    jmp :+                                        ; jump to get offset and animate player object
        ThreeFrameExtent:
        lda #2                                    ; load upper extent for frame control for climbing
    :

    sta temp_byte                                 ; store upper extent here
    jsr GetCurrentAnimOffset                      ; get proper offset to graphics table
    pha                                           ; save offset to stack
        if !PlayerAnimTimer                       ; load animation frame timer
            mb PlayerAnimTimer := PlayerAnimTimerSet                   ; get animation frame timer amount
            ; if frame control + 1 >= upper extent
            if a := PlayerAnimCtrl + #1 >= temp_byte
                lda #0                            ; initialize frame control
            endif
            sta PlayerAnimCtrl                    ; store as new animation frame control
        endif
    pla                                           ; get offset to graphics table from stack and leave
    rts

    GetGfxOffsetAdder:

    if PlayerSize == SmallMario                   ; get player's size
        mb a := y
        mb y := a + #$08                          ; add eight bytes to offset for small player
    endif

    rts

.endproc

dataseg

ChangeSizeOffsetAdder:
      .byte $00, $01, $00, $01, $00, $01, $02, $00, $01, $02
      .byte $02, $00, $02, $00, $02, $00, $02, $00, $02, $00

.code

.proc HandleChangeSize

    .export ChkForPlayerAttrib, GetOffsetFromAnimCtrl

    ldy PlayerAnimCtrl                            ; get animation frame control
    ; Every fourth frame, otherwise branch ahead
    if !FrameCounter & #%00000011
        iny                                       ; increment frame control
        if y >= #$0a                              ; check for preset upper extent
            mb y, PlayerChangeSizeFlag := #0      ; initialize both grow/shrink flag and animation frame control
        endif
        sty PlayerAnimCtrl                        ; store proper frame control
    endif

    if PlayerSize == BigMario                     ; get player's size
        lda ChangeSizeOffsetAdder,y               ; get offset adder based on frame control as offset
        ldy #$0f                                  ; load offset for player growing

        GetOffsetFromAnimCtrl:
    ; multiply animation frame control by eight to get proper offset adder
        mb a := a * 8
        mb a := a +c PlayerGfxTblOffsets[ y ]     ; add to offset to graphics table
        rts                                       ; and return with result in A

    endif
    ; ShrinkPlayer:

    tya
    ; add ten bytes to frame control as offset.
    ; this thing apparently uses two of the swimming frames to draw the player shrinking
    mb x := a + #$0a

    ldy #$09                                      ; load offset for small player swimming
    if !ChangeSizeOffsetAdder[ x ]                ; get what would normally be offset adder
        ldy #1                                    ; otherwise load offset for big player swimming
    endif

    lda PlayerGfxTblOffsets,y                     ; get offset to graphics table based on offset loaded
    rts                                           ; and leave

    ChkForPlayerAttrib:

    ldy Player_SprDataOffset                      ; get sprite data offset

    if GameEngineSubroutine <> #$0b               ; if not kill?
        ; if offset is crouching or standing or intermediate growing then skip to fourth row OAM attributes only
        if PlayerGfxOffset = #$50 || a = #$b8 || a = #$c0 goto C_S_IGAtt
        if a <> #$c8 goto ExPlyrAt                ; if none of these, branch to leave
    endif
    ; no flip for first of third row sprites
    mb Sprite[ 16 + y ]::Attributes := Sprite[ 16 + y ]::Attributes & #%00111111
    ; set horizontal flip bit for second sprite in the third row
    mb Sprite[ 20 + y ]::Attributes := Sprite[ 20 + y ]::Attributes & #%00111111 | #%01000000

    C_S_IGAtt:
    ; mask out horizontal and vertical flip bits for fourth row sprites and save
    mb Sprite[ 24 + y]::Attributes := Sprite[ 24 + y]::Attributes &  #%00111111
    ; set horizontal flip bit for second sprite in the fourth row
    mb Sprite::Attributes[ 28 + y ] := Sprite::Attributes[ 28 + y ] & #%00111111 | #%01000000

    ExPlyrAt:

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc RelativePlayerPosition
    ldx #0                                        ; set offsets for relative cooordinates
    ldy #0                                        ; routine to correspond to player object
    jmp GetObjRelAndOfs                           ; get the coordinates
.endproc

.proc RelativeBubblePosition
    ldy #1                                        ; set for air bubble offsets
    jsr GetProperObjOffset                        ; modify X to get proper air bubble offset
    ldy #3
    jmp GetObjRelAndOfs                           ; get the coordinates
.endproc

.proc RelativeFireballPosition
    ldy #0                                        ; set for fireball offsets
    jsr GetProperObjOffset                        ; modify X to get proper fireball offset
    ldy #2
.endproc

.proc GetObjRelAndOfs
    jsr GetObjRelativePosition                    ; get the coordinates
    ldx ObjectOffset                              ; return original offset
    rts                                           ; leave
.endproc

.proc RelativeMiscPosition
    ldy #2                                        ; set for misc object offsets
    jsr GetProperObjOffset                        ; modify X to get proper misc object offset
    ldy #$06
    jmp GetObjRelAndOfs                           ; get the coordinates
.endproc

.proc RelativeEnemyPosition
    lda #1                                        ; get coordinates of enemy object
    ldy #1                                        ; relative to the screen
    jmp VariableObjOfsRelPos
.endproc

.proc RelativeBlockPosition
    lda #$09                                      ; get coordinates of one block object
    ldy #4                                        ; relative to the screen

    jsr VariableObjOfsRelPos

    inx                                           ; adjust offset for other block object if any
    inx
    lda #$09
    iny                                           ; adjust other and get coordinates for other one
.endproc

.proc VariableObjOfsRelPos
    stx temp_byte                                 ; store value to add to A here
    clc
    adc temp_byte                                 ; add A to value stored
    tax                                           ; use as enemy offset
    jsr GetObjRelativePosition
    ldx ObjectOffset                              ; reload old object offset and leave
    rts
.endproc

.proc GetObjRelativePosition
    ; load vertical coordinate low
    mb SprObject_Rel_YPos[ y ] := SprObject_Y_Position[ x ]
    ; load horizontal coordinate
    ; subtract left edge coordinate
    mb SprObject_Rel_XPos[ y ] := SprObject_X_Position[ x ] - ScreenLeft_X_Pos
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc GetPlayerOffscreenBits
    ldx #0                                        ; set offsets for player-specific variables
    ldy #0                                        ; and get offscreen information about player
    jmp GetOffScreenBitsSet
.endproc

.proc GetFireballOffscreenBits
    ldy #0                                        ; set for fireball offsets
    jsr GetProperObjOffset                        ; modify X to get proper fireball offset
    ldy #2                                        ; set other offset for fireball's offscreen bits
    jmp GetOffScreenBitsSet                       ; and get offscreen information about fireball
.endproc

.proc GetBubbleOffscreenBits
    ldy #1                                        ; set for air bubble offsets
    jsr GetProperObjOffset                        ; modify X to get proper air bubble offset
    ldy #3                                        ; set other offset for airbubble's offscreen bits
    jmp GetOffScreenBitsSet                       ; and get offscreen information about air bubble
.endproc

.proc GetMiscOffscreenBits
    ldy #2                                        ; set for misc object offsets
    jsr GetProperObjOffset                        ; modify X to get proper misc object offset
    ldy #$06                                      ; set other offset for misc object's offscreen bits
    jmp GetOffScreenBitsSet                       ; and get offscreen information about misc object
.endproc


dataseg

ObjOffsetData:
    .byte $07, $16, $0d

.code

.proc GetProperObjOffset

    txa                                           ; move offset to A
    ; add amount of bytes to offset depending on setting in Y
    mb x := a + ObjOffsetData[ y ]
    rts

.endproc

.proc GetEnemyOffscreenBits
    lda #1                                        ; set A to add 1 byte in order to get enemy offset
    ldy #1                                        ; set Y to put offscreen bits in Enemy_OffscreenBits
    jmp SetOffscrBitsOffset
.endproc

.proc GetBlockOffscreenBits
    lda #$09                                      ; set A to add 9 bytes in order to get block obj offset
    ldy #4                                        ; set Y to put offscreen bits in Block_OffscreenBits
.endproc

.proc SetOffscrBitsOffset
    ; add A to X
    stx temp_byte
    mb x := a + temp_byte
.endproc

.proc GetOffScreenBitsSet

    tya                                           ; save offscreen bits offset to stack for now
    pha
        jsr RunOffscrBitsSubs
        mb $00 := a << 4 | $00
    pla                                           ; get offscreen bits offset from stack
    tay


    mb SprObject_OffscrBits[ y ] := temp_byte     ; get value here and store elsewhere
    ldx ObjectOffset
    rts
    ; ------------------------------------------------------------------------------------------------

    RunOffscrBitsSubs:

    jsr GetXOffscreenBits                         ; do subroutine here

    mb temp_byte := a >> 4

    jmp GetYOffscreenBits

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

XOffscreenBitsData:
      .byte $7f, $3f, $1f, $0f, $07, $03, $01, $00
      .byte $80, $c0, $e0, $f0, $f8, $fc, $fe, $ff

DefaultXOnscreenOfs:
      .byte $07, $0f, $07

.code

.proc GetXOffscreenBits

    ; locals
        SavedOffset = temp_byte + 4    ; - used to store proper offset
        Adder       = temp_byte + 5    ; - used as adder in DividePDiff
        Diff        = temp_byte + 6    ; - used to store preset value used to compare to pixel difference in $07
        FromEdge    = temp_byte + 7    ; - used to store difference between coordinates of object and screen edges
    ; end locals

    stx SavedOffset                                       ; save position in buffer to here
    ldy #1                                        ; start with right side of screen

    repeat
        mb FromEdge := ScreenEdge_X_Pos[ y ] - SprObject_X_Position[ x ]
        ; get page location of edge
        ; subtract from page location of object position
        mb a := ScreenEdge_PageLoc[ y ] -c SprObject_PageLoc[ x ]

        mb x := DefaultXOnscreenOfs[ y ]          ; load default offset value here
        
        ; if beyond right edge or in front of left edge, branch
        if cmp #0 == positive
            mb x := DefaultXOnscreenOfs[ 1 + y ]                       ; load alternate offset value here
            ; if one page or more to the left of either edge, branch
            if cmp #1 == negative
                mb Diff := #$38                    ; value to check against in DividePDiff
                lda #$08                          ; value to add on left or bottom check
                jsr DividePDiff
            endif
        endif

        mb a := XOffscreenBitsData[ x ]           ; get bits here
        ldx SavedOffset                                   ; reobtain position in buffer

    until a <> #0 || dey == negative

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg

YOffscreenBitsData:
    .byte $00, $08, $0c, $0e
    .byte $0f, $07, $03, $01
    .byte $00

DefaultYOnscreenOfs:
    .byte $04, $00, $04

HighPosUnitData:
    .byte $ff, $00

.code

.proc GetYOffscreenBits

    ; locals
        SavedOffset = temp_byte + 4    ; - used to store proper offset
        Adder       = temp_byte + 5    ; - used as adder in DividePDiff
        Diff        = temp_byte + 6    ; - used to store preset value used to compare to pixel difference in $07
        FromEdge    = temp_byte + 7    ; - used to store difference between coordinates of object and screen edges
    ; end locals
    
    stx SavedOffset                                       ; save position in buffer to here
    ldy #1                                        ; start with top of screen

    repeat                                        ; y = 1 to 0
    ; load coordinate for edge of vertical unit, subtract from vertical coordinate of object
        mb FromEdge := HighPosUnitData[ y ] - SprObject_Y_Position[ x ]
        mb a := #1 -c SprObject_Y_HighPos[ x ]

        mb x := DefaultYOnscreenOfs[ y ]          ; load default offset value here
        if cmp #0 == positive
        ; if NOT under top of the screen or beyond bottom, branch
            ldx DefaultYOnscreenOfs+1,y           ; load alternate offset value here
            ; if NOT one vertical unit or more above the screen
            if cmp #1 == negative
                mb Diff := #$20                    ; value to check against in DividePDiff
                lda #4                            ; value to add on left or bottom check
                jsr DividePDiff
            endif
        endif

        mb a := YOffscreenBitsData[ x ]           ; get offscreen data bits using offset
        ldx SavedOffset                                   ; reobtain position in buffer

    until a <> #0 || dey == negative

    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

.proc DividePDiff
    ; callers : GetXOffscreenBits, GetYOffscreenBits
    
    ; locals
        Diff        = temp_byte + 6    ; - used to store preset value used to compare to pixel difference in $07
        FromEdge    = temp_byte + 7    ; - used to store difference between coordinates of object and screen edges
        Adder       = temp_byte + 5
    ; end locals

    sta Adder                                     ; store current value in A here

    if lda FromEdge < Diff                        ; if less than than max allowed by caller:
        mb a := a / 8 & #%0000111
        if y < #1                                 ; right side of the screen or top?
            adc Adder                             ; add value in A if bottom or left
        endif
        tax                                       ; use as offset
    endif

    rts

.endproc

    ; ------------------------------------------------------------------------------------------------    

.proc DrawSpriteObject

    ; locals
        Tile1       = temp_byte         ; - Tiles
        Tile2       = temp_byte + 1     ;
        YCoord      = temp_byte + 2     ; - Y position
        FlipControl = temp_byte + 3     ; - moving direction, used to flip enemies horizontally
        SpriteAtt   = temp_byte + 4     ; - enemy's sprite attributes
        XCoord      = temp_byte + 5     ; - X position
    ; end locals

    ; draw two sprites, side by side:
    
    lda FlipControl                               ; get saved flip control bits
    lsr
    lsr                                           ; move d1 into carry

    lda Tile1
    if C set                                      ; d1 set:
        mb Sprite[ 4 + y ]::Tilenumber := a       ; store first tile into second sprite
        mb Sprite[ y ]::Tilenumber := Tile2       ; and second into first sprite
        lda #$40                                  ; activate horizontal flip OAM attribute
    else Z clear                                  ; no H flip bit:
        mb Sprite[ y ]::Tilenumber := a           ; store first tile into first sprite
        mb Sprite[ 4 + y]::Tilenumber := Tile2    ; and second into second sprite
        lda #0                                    ; clear bit for horizontal flip
    endif

    mb a := a | SpriteAtt                         ; add other OAM attributes if necessary
    mb Sprite[ 0 + y ]::Attributes := a           ; store sprite attributes
    mb Sprite[ 4 + y ]::Attributes := a
    mb Sprite[ 0 + y ]::Y_Position := YCoord      ; now the y coordinates note because they are
    mb Sprite[ 4 + y ]::Y_Position := a           ; YCoord ; side by side, they are the same

    mb Sprite[ y    ]::X_Position := $05          ; store x coordinate, then
    mb Sprite[ 4 + y]::X_Position := a + #8
    mb YCoord := YCoord + #8                      ; add eight pixels to the next y coord

    tya                                           ; add eight to the offset in Y to move to the next two spr
    mb y := a + #8  
    inx                                           ; increment offset to return it to the
    inx                                           ; routine that called this subroutine
    rts

.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg
    ; unused space
    .byte $ff, $ff, $ff, $ff, $ff, $ff

    ; ------------------------------------------------------------------------------------------------

.code

.proc SoundEngine

    if !OperMode                                  ; are we in title screen mode?
        sta SND_MASTERCTRL_REG                    ; if so, disable sound and leave
        rts
    endif

    mb JOYPAD_PORT2 := #$ff                       ; disable irqs and set frame counter mode???

    mb SND_MASTERCTRL_REG := #$0f                 ; enable first four channels
    ; if in Pause mode OR something in Pause queue:
    if PauseModeFlag || PauseSoundQueue = #1
    ; In pause or pause sound queued:

        if !PauseSoundBuffer                      ; check pause sfx buffer

            if lda PauseSoundQueue == zero goto SkipSoundSubroutines

            sta PauseSoundBuffer                  ; if queue full, store in buffer and activate
            sta PauseModeFlag                     ; pause mode to interrupt game sounds

            lda #0                                ; disable sound and clear sfx buffers
            sta SND_MASTERCTRL_REG
            sta Square1SoundBuffer
            sta Square2SoundBuffer
            sta NoiseSoundBuffer

            mb SND_MASTERCTRL_REG := #$0f         ; enable sound again
            mb Squ1_SfxLenCounter := #$2a         ; store length of sound in pause counter

            PlayTone44:

            lda #$44                              ; play first tone

        else Z clear
            ; Continue Pause:

            if lda Squ1_SfxLenCounter <> #$24     ; if $24, a = #$64 2nd tone
                if a = #$1e goto PlayTone44       ; if $1e, a = #$44 1st tone again
                if a <> #$18 goto DecPauseCounter                      ; if $18, a = #$64 2nd tone
            endif                                 ; if anything else, no changes to sound, dec counter, rts
            ; store reg contents and play the pause sfx
            lda #$64

        endif
        ; jsr to make pause sound
        ldx #$84
        ldy #$7f
        jsr PlaySqu1Sfx

        DecPauseCounter:

        dec Squ1_SfxLenCounter                    ; decrement pause sfx counter
        if not zero goto SkipSoundSubroutines

        mb SND_MASTERCTRL_REG := #0               ; disable sound if in pause mode and not currently playing the pause sfx                                 

        if PauseSoundBuffer = #2                  ; if no longer playing pause sfx, check to see
            mb PauseModeFlag := #0                ; clear pause mode to allow game sounds again
        endif

        mb PauseSoundBuffer := #0                 ; clear pause sfx buffer

    else Z set                                    ; no Pause Mode, run Sound Subroutines:

        jsr Square1SfxHandler                     ; play sfx on square channel 1
        jsr Square2SfxHandler                     ;  ''  ''  '' square channel 2
        jsr NoiseSfxHandler                       ;  ''  ''  '' noise channel
        jsr MusicHandler                          ; play music on all channels
        lda #0                                    ; clear the music queues
        sta AreaMusicQueue
        sta EventMusicQueue
    endif

    SkipSoundSubroutines:

    lda #0                                        ; clear the sound effects queues
    sta Square1SoundQueue
    sta Square2SoundQueue
    sta NoiseSoundQueue
    sta PauseSoundQueue

    ldy DAC_Counter                               ; load some sort of counter
    ; if = 1, 2 or 3
    if AreaMusicBuffer & #%00000011               ; check for specific music
        inc DAC_Counter                           ; increment and check counter
        if y < #$30 goto StrWave                  ; if not there yet, just store it
    endif

    tya                                           ; set flags?
    if not zero
        dec DAC_Counter                           ; decrement counter
    endif

    StrWave:
    sty SND_DELTA_REG+1                           ; store into DMC load register (??)
    rts                                           ; we are done here

.endproc

    ; ------------------------------------------------------------------------------------------------

.proc Dump_Squ1_Regs
    sty SND_SQUARE1_REG+1                         ; dump the contents of X and Y into square 1's control regs
    stx SND_SQUARE1_REG
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

PlaySqu1Sfx:

    jsr Dump_Squ1_Regs                            ; do sub to set ctrl regs for square 1, then set frequency regs

    SetFreq_Squ1:
    ldx #0                                        ; set frequency reg offset for square 1 sound channel


    Dump_Freq_Regs:

    tay
    if lda FreqRegLookupTbl[ 1 + y ]              ; use previous contents of A for sound reg offset
        ; first byte goes into LSB of frequency divider:
        mb SND_REGISTER[ 2 + x ] := a
        ; second byte goes into 3 MSB plus extra bit for length counter:
        mb SND_REGISTER[ 3 + x ] := FreqRegLookupTbl[ y ] | #%00001000
    endif

    rts
    
    ; end PlaySqu1Sfx
    ; ----------------------------------------------------------------------------------------------c    

.proc Dump_Sq2_Regs
    stx SND_SQUARE2_REG                           ; dump the contents of X and Y into square 2's control regs
    sty SND_SQUARE2_REG+1
    rts
.endproc

    ; -----------------------------------------------------------------------------------------------

PlaySqu2Sfx:

    jsr Dump_Sq2_Regs                             ; do sub to set ctrl regs for square 2, then set frequency regs
    
    SetFreq_Squ2:
    
    ldx #4                                        ; set frequency reg offset for square 2 sound channel
    bne Dump_Freq_Regs                            ; unconditional branch

    SetFreq_Tri:
    
    ldx #8                                        ; set frequency reg offset for triangle sound channel
    bne Dump_Freq_Regs                            ; unconditional branch

    
    ; end PlaySqu2Sfx
    ; -----------------------------------------------------------------------------------------------

dataseg

SwimStompEnvelopeData:
    .byte $9f, $9b, $98, $96, $95, $94, $92, $90
    .byte $90, $9a, $97, $95, $93, $92

.code

    ; -----------------------------------------------------------------------------------------------

PlayFlagpoleSlide:

    mb Squ1_SfxLenCounter := #$40                 ; store length of flagpole sound
    lda #$62                                      ; load part of reg contents for flagpole sound
    jsr SetFreq_Squ1
    ldx #$99                                      ; now load the rest

    bne FlagpoleSound2ndPart
    
    ; end PlayFlagpoleSlide
    ; ----------------------------------------------------------------------------------------------c    

PlaySmallJump:

    lda #$26                                      ; branch here for small mario jumping sound
    bne SetupJumpSound                            ; OP BIT opcode here

        PlayBigJump:
        lda #$18                                  ; branch here for big mario jumping sound

    SetupJumpSound:

    ldx #$82                                      ; note that small and big jump borrow each others' reg contents
    ldy #$a7                                      ; anyway, this loads the first part of mario's jumping sound
    jsr PlaySqu1Sfx

    mb Squ1_SfxLenCounter := #$28                 ; store length of sfx for both jumping sounds

    ; end PlaySmallJump    
    ; ----------------------------------------------------------------------------------------------c    

ContinueSndJump:

    if Squ1_SfxLenCounter = #$25                  ; jumping sounds seem to be composed of three parts
        ldx #$5f                                  ; load second part
        ldy #$f6

    else Z clear

        if a <> #$20 goto DecJumpSoundCounter
        ; here, a = $20:
        ldx #$48                                  ; load third part

        FlagpoleSound2ndPart:
        ldy #$bc                                  ; the flagpole slide sound shares part of third part
    endif

    jsr Dump_Squ1_Regs
    bne DecJumpSoundCounter                       ; unconditional
    
    
    ; end ContinueSndJump    
    ; ----------------------------------------------------------------------------------------------c    

PlayFireballThrow:
    lda #$05
    ldy #$99                                      ; load reg contents for fireball throw sound
    bne Fthrow                                    ; unconditional branch

    ; end PlayFireballThrow   
    ; ----------------------------------------------------------------------------------------------c    

PlayBump:
    lda #$0a                                      ; load length of sfx and reg contents for bump sound
    ldy #$93

    Fthrow:
    ldx #$9e                                      ; the fireball sound shares reg contents with the bump sound
    sta Squ1_SfxLenCounter
    lda #$0c                                      ; load offset for bump sound
    jsr PlaySqu1Sfx
    
    ; end PlayBump    
    ; ----------------------------------------------------------------------------------------------c    

ContinueBumpThrow:

    if Squ1_SfxLenCounter = #6                    ; check for second part of bump sound
        mb SND_SQUARE1_REG[ 1 ] := #$bb           ; load second part directly
    endif

    DecJumpSoundCounter:

    bne BranchToDecLength1                        ; unconditional branch

    ; end ContinueBumpThrow    
    ; ----------------------------------------------------------------------------------------------c    

.proc Square1SfxHandler

    if ldy Square1SoundQueue                                           ; check for sfx in queue

        sty Square1SoundBuffer                                         ; if found, put in buffer
        if bit7 set goto PlaySmallJump                                 ; bit 7: small jump
        if lsr Square1SoundQueue == C set goto PlayBigJump             ; bit 0: big jump
        if lsr Square1SoundQueue == C set goto PlayBump                ; bit 1: bump
        if lsr Square1SoundQueue == C set goto PlaySwimStomp           ; bit 2: swim/stomp
        if lsr Square1SoundQueue == C set goto PlaySmackEnemy          ; bit 3: smack enemy
        if lsr Square1SoundQueue == C set goto PlayPipeDownInj         ; bit 4: pipedown/injury
        if lsr Square1SoundQueue == C set goto PlayFireballThrow       ; bit 5: fireball throw
        if lsr Square1SoundQueue == C set goto PlayFlagpoleSlide       ; bit 6: slide flagpole
    endif

    if Square1SoundBuffer                                              ; check for sfx in buffer
        if bit7 set goto ContinueSndJump                               ; bit 7: small mario jump
        if a >> 1 == C set goto ContinueSndJump                        ; bit 0: big mario jump
        if a >> 1 == C set goto ContinueBumpThrow                      ; bit 1: bump
        if a >> 1 == C set goto ContinueSwimStomp                      ; bit 2: swim/stomp
        if a >> 1 == C set goto ContinueSmackEnemy                     ; bit 3: smack enemy
        if a >> 1 == C set goto ContinuePipeDownInj                    ; bit 4: pipedown/injury
        if a >> 1 == C set goto ContinueBumpThrow                      ; bit 5: fireball throw
        if a >> 1 == C set goto DecrementSfx1Length                    ; bit 6: slide flagpole
    endif
    rts
.endproc

    ; -----------------------------------------------------------------------------------------------

PlaySwimStomp:

    mb Squ1_SfxLenCounter := #$0e                 ; store length of swim/stomp sound
    ldy #$9c                                      ; store reg contents for swim/stomp sound
    ldx #$9e
    lda #$26
    jsr PlaySqu1Sfx

    ; end PlaySwimStomp
    ; -----------------------------------------------------------------------------------------------

ContinueSwimStomp:

    ldy Squ1_SfxLenCounter
    ; look up reg contents in data section based on length of sound left, used to control sound's envelope
    mb SND_SQUARE1_REG := SwimStompEnvelopeData[ y - 1 ]
    if y = #$06
    ; when the length counts down to a certain point, put this directly into the LSB of square 1's frequency divider
        mb SND_SQUARE1_REG + 2 := #$9e
    endif

    BranchToDecLength1:
    bne DecrementSfx1Length                       ; unconditional branch (regardless of how we got here)
    
    ; end ContinueSwimStomp
    ; -----------------------------------------------------------------------------------------------

PlaySmackEnemy:

    lda #$0e                                      ; store length of smack enemy sound
    ldy #$cb
    ldx #$9f
    sta Squ1_SfxLenCounter
    lda #$28                                      ; store reg contents for smack enemy sound
    jsr PlaySqu1Sfx
    bne DecrementSfx1Length                       ; unconditional branch

        ContinueSmackEnemy:

        if ldy Squ1_SfxLenCounter = #8            ; check about halfway through
            mb SND_SQUARE1_REG + 2 := #$a0        ; if we're at the about-halfway point, make the second tone
            lda #$9f
        else Z clear
            lda #$90                              ; this creates spaces in the sound, giving it its distinct noise
        endif
        sta SND_SQUARE1_REG

    ; end PlaySmackEnemy
    ; -----------------------------------------------------------------------------------------------

DecrementSfx1Length:

    if dec Squ1_SfxLenCounter == zero             ; decrement length of sfx
        StopSquare1Sfx:

        mb x, Square1SoundBuffer := #0            ; if end of sfx reached, clear buffer
        mb x, SND_MASTERCTRL_REG := #$0e
        mb x, SND_MASTERCTRL_REG := #$0f
    endif

    rts

    ; end DecrementSfx1Length
    ; -----------------------------------------------------------------------------------------------

PlayPipeDownInj:

    mb Squ1_SfxLenCounter := #$2f                 ; load length of pipedown sound

    ContinuePipeDownInj:
    ; some bitwise logic, forces the regs
    ; to be written to only during six specific times
    ; during which d4 must be set and d1-0 must be clear
    lda Squ1_SfxLenCounter
    if a >> 1 == C clear && a >> 1 == C clear && a & #%00000010
        ldy #$91                                  ; and this is where it actually gets written in
        ldx #$9a
        lda #$44
        jsr PlaySqu1Sfx
    endif

    jmp DecrementSfx1Length

    ; end PlayPipeDownInj
    ; -----------------------------------------------------------------------------------------------

dataseg

ExtraLifeFreqData:
    .byte $58, $02, $54, $56, $4e, $44

PowerUpGrabFreqData:
    .byte $4c, $52, $4c, $48, $3e, $36, $3e, $36, $30
    .byte $28, $4a, $50, $4a, $64, $3c, $32, $3c, $32
    .byte $2c, $24, $3a, $64, $3a, $34, $2c, $22, $2c
    ; residual frequency data
    .byte $22, $1c, $14

PUp_VGrow_FreqData:
    .byte $14, $04, $22, $24, $16, $04, $24, $26                       ; used by both
    .byte $18, $04, $26, $28, $1a, $04, $28, $2a
    .byte $1c, $04, $2a, $2c, $1e, $04, $2c, $2e                       ; used by vinegrow
    .byte $20, $04, $2e, $30, $22, $04, $30, $32

.code

PlayCoinGrab:
    lda #$35                                      ; load length of coin grab sound
    ldx #$8d                                      ; and part of reg contents
    bne CGrab_TTickRegL
    
    ; end PlayCoinGrab
    ; -----------------------------------------------------------------------------------------------

PlayTimerTick:
    lda #$06                                      ; load length of timer tick sound
    ldx #$98                                      ; and part of reg contents
    
    ; end PlayTimerTick
    ; -----------------------------------------------------------------------------------------------

CGrab_TTickRegL:
    sta Squ2_SfxLenCounter
    ldy #$7f                                      ; load the rest of reg contents
    lda #$42                                      ; of coin grab and timer tick sound
    jsr PlaySqu2Sfx
    
    ; .end CGrab_TTickRegL
    ; -----------------------------------------------------------------------------------------------

ContinueCGrabTTick:
    ; check for time to play second tone yet
    ; timer tick sound also executes this, not sure why
    if Squ2_SfxLenCounter = #$30
        mb SND_SQUARE2_REG + 2 := #$54            ; if so, load the tone directly into the reg
    endif

    bne DecrementSfx2Length

    ; end ContinueCGrabTTick
    ; -----------------------------------------------------------------------------------------------

PlayBlast:

    mb Squ2_SfxLenCounter := #$20                 ; load length of fireworks/gunfire sound
    ldy #$94                                      ; load reg contents of fireworks/gunfire sound
    lda #$5e
    bne :+                                        ; unconditional

        ContinueBlast:

        if Squ2_SfxLenCounter <> #$18 goto DecrementSfx2Length         ; check for time to play second part
        ; if = #$18
        ldy #$93                                  ; load second part reg contents then
        lda #$18
    :
    bne BlstSJp                                   ; unconditional branch to load rest of reg contents
    
    ; end PlayBlast
    ; -----------------------------------------------------------------------------------------------

PlayPowerUpGrab:

    mb Squ2_SfxLenCounter := #$36                 ; load length of power-up grab sound

    ContinuePowerUpGrab:
    ; alter frequency every other frame
    if a := Squ2_SfxLenCounter >> 1 == C clear
        tay                                       ; use as offset
        mb a := PowerUpGrabFreqData[ y - 1 ]      ; use length left over / 2 for frequency offset
        ldx #$5d                                  ; store reg contents of power-up grab sound
        ldy #$7f

        LoadSqu2Regs:

        jsr PlaySqu2Sfx
    endif
    
    ; end PlayPowerUpGrab
    ; -----------------------------------------------------------------------------------------------

DecrementSfx2Length:

    if !dec Squ2_SfxLenCounter                    ; decrement length of sfx

        EmptySfx2Buffer:

        mb x, Square2SoundBuffer := #0            ; initialize square 2's sound effects buffer

        StopSquare2Sfx:

        mb x, SND_MASTERCTRL_REG := #$0d          ; stop playing the sfx
        mb x, SND_MASTERCTRL_REG := #$0f
    endif
    rts
    
    ; end DecrementSfx2Length
    ; -----------------------------------------------------------------------------------------------

.proc Square2SfxHandler
    ; special handling for the 1-up sound to keep it
    ; from being interrupted by other sounds on square 2
    if Square2SoundBuffer & #SFX_ExtraLife goto ContinueExtraLife

    if ldy Square2SoundQueue                      ; check for sfx in queue
        sty Square2SoundBuffer                    ; if found, put in buffer and check for the following
        if bit7 set                       goto PlayBowserFall          ; bit 7: bowser fall
        if lsr Square2SoundQueue == C set goto PlayCoinGrab            ; bit 0: coin grab
        if lsr Square2SoundQueue == C set goto PlayGrowPowerUp         ; bit 1: power-up reveal
        if lsr Square2SoundQueue == C set goto PlayGrowVine            ; bit 2: vine grow
        if lsr Square2SoundQueue == C set goto PlayBlast               ; bit 3: fireworks/gunfire
        if lsr Square2SoundQueue == C set goto PlayTimerTick           ; bit 4: timer tick
        if lsr Square2SoundQueue == C set goto PlayPowerUpGrab         ; bit 5: power-up grab
        if lsr Square2SoundQueue == C set goto PlayExtraLife           ; bit 6: 1-up
    endif
    ; Check Sfx2 Buffer:

    if Square2SoundBuffer
        if bit7 set        goto ContinueBowserFall                     ; bit 7: bowser fall
        if a >> 1 == C set goto Cont_CGrab_TTick                       ; bit 0: coin grab
        if a >> 1 == C set goto ContinueGrowItems                      ; bit 1: power-up reveal
        if a >> 1 == C set goto ContinueGrowItems                      ; bit 2: vine grow
        if a >> 1 == C set goto ContinueBlast                          ; bit 3: fireworks/gunfire
        if a >> 1 == C set goto Cont_CGrab_TTick                       ; bit 4: timer tick
        if a >> 1 == C set goto ContinuePowerUpGrab                    ; bit 5: power-up grabbcc
        if a >> 1 == C set goto ContinueExtraLife                      ; bit 6: 1-up ; not needed
    endif
    rts
    
.endproc

    ; -----------------------------------------------------------------------------------------------

    Cont_CGrab_TTick:
    jmp ContinueCGrabTTick

    JumpToDecLength2:
    jmp DecrementSfx2Length
    
    ; -----------------------------------------------------------------------------------------------

PlayBowserFall:

    mb Squ2_SfxLenCounter := #$38                 ; load length of bowser defeat sound
    ldy #$c4                                      ; load contents of reg for bowser defeat sound
    lda #$18

    BlstSJp:
    bne :+

        ContinueBowserFall:
    ; if effect near the end, continue below
        if Squ2_SfxLenCounter <> #8 goto DecrementSfx2Length

        ldy #$a4                                  ; load the rest of reg contents for bowser defeat sound
        lda #$5a
    :
    ldx #$9f                                      ; the fireworks/gunfire sound shares part of reg contents here

    EL_LRegs:
    bne LoadSqu2Regs                              ; this is an unconditional branch outta here
    
    ; end PlayBowserFall
    ; -----------------------------------------------------------------------------------------------

PlayExtraLife:

    mb Squ2_SfxLenCounter := #$30                 ; load length of 1-up sound

    ContinueExtraLife:
    lda Squ2_SfxLenCounter
    ; do this until all bits checked, if none set, continue
    ldx #3                                        ; load new tones only every eight frames
    repeat
        if a >> 1 == C set goto JumpToDecLength2                       ; if any bits set here, branch to dec the length
    until dex == zero

    tay
    mb a := ExtraLifeFreqData[ y - 1 ]            ; load our reg contents
    ldx #$82
    ldy #$7f
    bne EL_LRegs                                  ; unconditional branch
    
    ; .end PlayExtraLife
    ; -----------------------------------------------------------------------------------------------

PlayGrowPowerUp:

    lda #$10                                      ; load length of power-up reveal sound
    bne :+                                        ; OP: use BIT trick here

        PlayGrowVine:
        lda #$20                                  ; load length of vine grow sound

    :
    sta Squ2_SfxLenCounter

    mb SND_SQUARE2_REG + 1 := #$7f                ; load contents of reg for both sounds directly
    mb Sfx_SecondaryCounter := #0                 ; start secondary counter for both sounds

    ContinueGrowItems:

    inc Sfx_SecondaryCounter                      ; increment secondary counter for both sounds

    ; this sound doesn't decrement the usual counter.
    mb y := Sfx_SecondaryCounter / 2              ; divide by 2 to get the offset

    if y <> Squ2_SfxLenCounter                    ; if we haen't reached the end yet:
        mb SND_SQUARE2_REG := #$9d                ; load contents of other reg directly
        mb a := PUp_VGrow_FreqData[ y ]           ; use secondary counter / 2 as offset for frequency regs
        jsr SetFreq_Squ2
        rts
    endif                                         ; else , at end, stop sounds:

    jmp EmptySfx2Buffer                           ; branch to stop playing sounds
    
    ; end PlayGrowPowerUp.
    ; -----------------------------------------------------------------------------------------------

dataseg

BrickShatterFreqData:
      .byte $01, $0e, $0e, $0d, $0b, $06, $0c, $0f
      .byte $0a, $09, $03, $0d, $08, $0d, $06, $0c
.code

PlayBrickShatter:

    mb Noise_SfxLenCounter := #$20                ; load length of brick shatter sound

    ContinueBrickShatter:

    if Noise_SfxLenCounter >> 1 == C set

        tay
        ldx BrickShatterFreqData,y                ; load reg contents of brick shatter sound
        lda BrickShatterEnvData,y

        PlayNoiseSfx:

        sta SND_NOISE_REG                         ; play the sfx
        stx SND_NOISE_REG + 2

        mb SND_NOISE_REG[ 3 ] := #$18

    endif
    ; DecrementSfx3Length:

    if !dec Noise_SfxLenCounter                   ; decrement length of sfx
        mb SND_NOISE_REG := #$f0                  ; if done, stop playing the sfx
        mb NoiseSoundBuffer := #0
    endif

    rts

    ; end PlayBrickShatter
    ; -----------------------------------------------------------------------------------------------

NoiseSfxHandler:

    if ldy NoiseSoundQueue                        ; check for sfx in queue
        sty NoiseSoundBuffer                      ; if found, put in buffer
        if lsr NoiseSoundQueue == C set goto PlayBrickShatter          ; brick shatter
        if lsr NoiseSoundQueue == C set goto PlayBowserFlame           ; bowser flame
    endif
    ; CheckNoiseBuffer:

    if lda NoiseSoundBuffer                       ; check for sfx in buffer
        if a >> 1 == C set goto ContinueBrickShatter                   ; brick shatter
        if a >> 1 == C set goto ContinueBowserFlame                    ; bowser flame
    endif
    rts

    PlayBowserFlame:

    mb Noise_SfxLenCounter := #$40                ; load length of bowser flame sound

    ContinueBowserFlame:

    mb y := Noise_SfxLenCounter >> 1
    ldx #$0f                                      ; load reg contents of bowser flame sound
    mb a := BowserFlameEnvData[ y - 1 ]
    bne PlayNoiseSfx                              ; unconditional branch here
    
    ; end NoiseSfxHandler
    ; -----------------------------------------------------------------------------------------------

ContinueMusic:
    jmp HandleSquare2Music                        ; if we have music, start with square 2 channel
    
    ; -----------------------------------------------------------------------------------------------

MusicHandler:

    if !EventMusicQueue                           ; check event music queue
        if AreaMusicQueue goto LoadAreaMusic
        if EventMusicBuffer | AreaMusicBuffer goto ContinueMusic
        rts                                       ; no music, then leave
    endif

    LoadEventMusic:

    sta EventMusicBuffer                          ; copy event music queue contents to buffer

    if a = #MUSIC_Death                           ; is it death music?
        jsr StopSquare1Sfx                        ; stop sfx in square 1 and 2
        jsr StopSquare2Sfx                        ; but clear only square 1's sfx buffer
    endif

    mb x, AreaMusicBuffer_Alt := AreaMusicBuffer  ; save current area music buffer to be re-obtained later
    mb y, NoteLengthTblAdder := #0                ; default value for additional length byte offset
    mb AreaMusicBuffer       := y                 ; #0     ; clear area music buffer

    if a = #MUSIC_TimeRunningOut                  ; is it time running out music?

        mb x, NoteLengthTblAdder := #$08          ; load offset to be added to length byte of header
        bne :+                                    ; unconditional branch

            LoadAreaMusic:

            if a = #4                             ; is it underground music?
                jsr StopSquare1Sfx
            endif

            ldy #$10                              ; start counter used only by ground level music

            do
                sty GroundMusicHeaderOfs

                HandleAreaMusicLoopB:

                mb y, EventMusicBuffer := #0      ; clear event music buffer
                mb  AreaMusicBuffer    := a       ; copy area music queue contents to buffer
                if a <> #1 break                  ; is it ground level music?

                inc GroundMusicHeaderOfs          ; increment but only if playing ground level music
                if ldy GroundMusicHeaderOfs <> #$32 goto LoadHeader    ; is it time to loopback ground level music? branch with alt offset

                ldy #$11                          ; if GroundMusicHeaderOfs is #$32, load this
            while Z clear                         ; unconditional branch
            ; Find area music header:
            ; load Y for offset of area music, residual instruction here
            mb y, MusicOffset_Square2 := #$08
        :
    endif
    
    ; Find Event Music Header:
    repeat
        iny                          ; increment Y pointer based on previously loaded queue contents
        lsr                          ; bit shift and increment until we find a set bit for music
    until C set

    LoadHeader:

    mb a, y := MusicHeaderData[ y - 1 ]          ; load offset for header

    mb NoteLenLookupTblOfs  := MusicHeaderData[ y ]                    ; now load the header
    mb MusicDataLow         := MusicHeaderData[ 1 + y ]
    mb MusicDataHigh        := MusicHeaderData[ 2 + y ]
    mb MusicOffset_Triangle := MusicHeaderData[ 3 + y ]
    mb MusicOffset_Square1  := MusicHeaderData[ 4 + y ]
    mb MusicOffset_Noise    := MusicHeaderData[ 5 + y ]

    sta NoiseDataLoopbackOfs

    lda #1                                        ; initialize music note counters
    mb Squ2_NoteLenCounter  := a
    mb Squ1_NoteLenCounter  := a
    mb Tri_NoteLenCounter   := a
    mb Noise_BeatLenCounter := a

    lda #0                                        ; initialize music data offset for square 2
    mb MusicOffset_Square2 := a
    mb AltRegContentFlag   := a                   ; initialize alternate control reg data used by square 1
    mb SND_MASTERCTRL_REG := #$0b                 ; disable triangle channel and reenable it
    mb SND_MASTERCTRL_REG := #$0f

    ; end Music Handler
    ; -----------------------------------------------------------------------------------------------

HandleSquare2Music:

    if !dec Squ2_NoteLenCounter                   ; decrement square 2 note length

        ldy MusicOffset_Square2                   ; increment square 2 music offset and fetch data
        inc MusicOffset_Square2

        if lda (MusicData)[ y ]
            if positive goto Squ2NoteHandler      ; if non-negative, data is a note
            if not zero goto Squ2LengthHandler    ; unconditional otherwise it is length data
        endif

        ; check secondary buffer for time running out music
        ; load previously saved contents of primary buffer
        ; and start playing the song again if there is one

        if EventMusicBuffer <> #MUSIC_TimeRunningOut || !AreaMusicBuffer_Alt

            if a & #MUSIC_Victory goto VictoryMLoopBack
            ; if any area music except pipe intro, music loops
            if !AreaMusicBuffer & #%01011111

                mb AreaMusicBuffer  := #0         ; clear primary and secondary buffers and initialize
                mb EventMusicBuffer := a          ; #0              ; control regs of square and triangle channels
                mb SND_TRIANGLE_REG := a          ; #0
                mb SND_SQUARE1_REG := #$90
                mb SND_SQUARE2_REG := a           ; #$90
                rts
            endif
        endif

        MusicLoopBack:
        jmp HandleAreaMusicLoopB

        VictoryMLoopBack:
        jmp LoadEventMusic

        Squ2LengthHandler:

        jsr ProcessLengthData                     ; store length of note
        sta Squ2_NoteLenBuffer
        ldy MusicOffset_Square2                   ; fetch another byte (MUST NOT BE LENGTH BYTE!)
        inc MusicOffset_Square2
        lda (MusicData),y

        Squ2NoteHandler:

        if !ldx Square2SoundBuffer                ; is there a sound playing on this channel?
            if jsr SetFreq_Squ2                   ; no, then play the note
                jsr LoadControlRegs               ; if not a rest, load control regs for square  
            endif
            sta Squ2_EnvelopeDataCtrl             ; save contents of A
            jsr Dump_Sq2_Regs                     ; dump X and Y into square 2 control regs                                                                               
        endif
        mb Squ2_NoteLenCounter := Squ2_NoteLenBuffer                   ; save length in square 2 note counter
    endif

    ; Misc aquare 2 music tasks:
    ; if no sound playing on square:
    ; check for death music or d4 set on secondary buffer
    if !Square2SoundBuffer && !EventMusicBuffer & #%10010001           ; note that regs for death music or d4 are loaded by default

        if ldy Squ2_EnvelopeDataCtrl              ; check for contents saved from LoadControlRegs
            dec Squ2_EnvelopeDataCtrl             ; decrement if not zero
        endif

        jsr LoadEnvelopeData                      ; do a load of envelope data to replace default
        sta SND_SQUARE2_REG                       ; based on offset set by first load unless playing
        mb x, SND_SQUARE2_REG[ 1 ] := #$7f        ; death music or d4 set on secondary buffer

    endif
    ; HandleSquare1Music:

    if !ldy MusicOffset_Square1 goto HandleTriangleMusic               ; if zero, skip ahead to the triangle channel

    if !dec Squ1_NoteLenCounter                   ; decrement square 1 note length
    ; Fetch Square 1 Music Data:

        do
            ldy MusicOffset_Square1               ; increment square 1 music offset and fetch data
            inc MusicOffset_Square1
            if lda (MusicData)[ y ] break
            mb SND_SQUARE1_REG      := #$83       ; store some data into control regs for square 1
            mb SND_SQUARE1_REG[ 1 ] := #$94       ; and fetch another byte of data, used to give death music its unique sound
            mb AltRegContentFlag    := a          ; #$94
        while Z clear                             ; unconditional branch
        ; Squ1NoteHandler:
        jsr AlternateLengthHandler
        sta Squ1_NoteLenCounter                   ; save contents of A in square 1 note counter
        ; if there a sound playing on square 1, goto
        if ldy Square1SoundBuffer goto HandleTriangleMusic

        txa
        and #%00111110                            ; change saved data to appropriate note format
        if jsr SetFreq_Squ1                       ; play the note
            jsr LoadControlRegs                   ; if Freq was zero
        endif

        sta Squ1_EnvelopeDataCtrl                 ; save envelope offset
        jsr Dump_Squ1_Regs
    endif

    ; Misc square 1 music tasks:

    if !Square1SoundBuffer                         ; if there is no sound playing on square 1:
        if !EventMusicBuffer & #%10010001

            if ldy Squ1_EnvelopeDataCtrl          ; check saved envelope offset
                dec Squ1_EnvelopeDataCtrl         ; decrement unless already zero
            endif

            jsr LoadEnvelopeData                  ; do a load of envelope data
            sta SND_SQUARE1_REG                   ; based on offset set by first load

        endif

        ; DeathMusic AltReg:

        if !AltRegContentFlag                     ; check for alternate control reg data
            lda #$7f                              ; load this value if zero, the alternate value
        endif

        sta SND_SQUARE1_REG+1                     ; if nonzero, and let's move on
    endif

    ; -----------------------------------------------------------------------------------------------

HandleTriangleMusic:

    lda MusicOffset_Triangle

    if !dec Tri_NoteLenCounter                    ; decrement triangle note length

        ldy MusicOffset_Triangle                  ; increment square 1 music offset and fetch data
        inc MusicOffset_Triangle
        ; if zero, skip all this and move on to noise
        if lda (MusicData)[ y ]

            if negative                           ; if negative, data is length data ( ; otherwise, it is a note )
                jsr ProcessLengthData
                sta Tri_NoteLenBuffer

                mb SND_TRIANGLE_REG := #$1f       ; load some default data for triangle control reg
                ldy MusicOffset_Triangle          ; fetch another byte
                inc MusicOffset_Triangle
                if !lda (MusicData)[ y ] goto SetTriCtrlReg            ; check once more for nonzero data
            endif
            ; Triangle Note Handler:
            jsr SetFreq_Tri
            ; save length in triangle note counter
            mb x, Tri_NoteLenCounter := Tri_NoteLenBuffer
            ; check for death music or d4 set on secondary buffer
            ; check primary buffer for water or castle level music
            ; if playing any other primary, or death or d4, go on to noise routine

            if !EventMusicBuffer & #%01101110 && !AreaMusicBuffer & #%00001010 goto HandleNoiseMusic

            txa                                   ; if playing water or castle music or any secondary

            if a < #$12                           ; besides death music or d4 set, check length of note
                if EventMusicBuffer & #MUSIC_EndOfCastle               ; check for win castle music again if not playing a long note
                    lda #$0f                      ; load value $0f if playing the win castle music and playing a short
                    bne SetTriCtrlReg             ; note, load value $1f if playing water or castle level music or any
                endif
                lda #$1f                          ; secondary besides death and d4 except win castle or win castle and playing
                bne SetTriCtrlReg                 ; a short note, and load value $ff if playing a long note on water, castle
            endif

            lda #$ff                              ; or any secondary (including win castle) except death and d4

            SetTriCtrlReg:
        endif
        sta SND_TRIANGLE_REG                      ; save final contents of A into control reg for triangle
    endif

    ; end HandleTriangleMusic
    ; -----------------------------------------------------------------------------------------------

HandleNoiseMusic:
    ; check if playing underground or castle music, if so dec Noise_BeatLenCounter and it is zero:
    if AreaMusicBuffer & #%11110011 && !dec Noise_BeatLenCounter
    
        ; Fetch noise beat data:
        do
            ldy MusicOffset_Noise                 ; increment noise beat offset and fetch data
            inc MusicOffset_Noise
            if lda (MusicData)[ y ] break
            ; if data is zero, reload original noise beat offset and loopback next time around
            mb MusicOffset_Noise := NoiseDataLoopbackOfs
        while Z clear                             ; unconditional branch
        ;  Noise Beat Handler:

        jsr AlternateLengthHandler
        sta Noise_BeatLenCounter                  ; store length in noise beat counter
        txa

        if a & #%00111110                         ; if beat data:
            if a <> #$30                          ; check the beat data and play the appropriate noise
                if a <> #$20
                    if ! a & #%00010000 goto SilentBeat
                    ; short beat
                    lda #$1c                      ; short beat data
                    ldx #3
                    ldy #$18
                    bne PlayBeat
                endif
                ; Strong Beat:
                lda #$1c                          ; strong beat data
                ldx #$0c
                ldy #$18
                bne PlayBeat
            endif
            ; Long Beat:
            lda #$1c                              ; long beat data
            ldx #3
            ldy #$58
            bne PlayBeat

        endif

        SilentBeat:

        lda #$10                                  ; silence

        PlayBeat:

        sta SND_NOISE_REG                         ; load beat data into noise regs
        stx SND_NOISE_REG+2
        sty SND_NOISE_REG+3

    endif
    ExitMusicHandler:

    rts
    
    ; end HandleNoiseMusic
    ; -----------------------------------------------------------------------------------------------

.proc AlternateLengthHandler
    tax                                           ; save a copy of original byte into X
    ror                                           ; save LSB from original byte into carry
    txa                                           ; reload original byte and rotate three times
    rol                                           ; turning xx00000x into 00000xxx, with the
    rol                                           ; bit in carry as the MSB here
    rol
.endproc

    ; -----------------------------------------------------------------------------------------------

.proc ProcessLengthData
    ; clear all but the three LSBs add offset loaded from first header byte
    ; add extra if time running out music
    mb y := a & #%00000111 + NoteLenLookupTblOfs +c NoteLengthTblAdder
    mb a := MusicLengthLookupTbl[ y ]             ; load length
    rts
.endproc

    ; -----------------------------------------------------------------------------------------------

.proc LoadControlRegs

    if EventMusicBuffer & #MUSIC_EndOfCastle
        lda #4                                    ; this value is only used for win castle music
    else Z clear
        if AreaMusicBuffer & #%01111101           ; check primary buffer for water music
            lda #$08                              ; this is the default value for all other music
        else Z clear
            lda #$28                              ; this value is used for water music and all other event music
        endif
    endif

    ldx #$82                                      ; load contents of other sound regs for square 2
    ldy #$7f
    rts
.endproc

    ; -----------------------------------------------------------------------------------------------

.proc LoadEnvelopeData

    if EventMusicBuffer & #MUSIC_EndOfCastle      ; check secondary buffer for win castle music
        lda EndOfCastleMusicEnvData,y             ; load data from offset for win castle music
        rts
    endif                                         ; else:

    if AreaMusicBuffer & #%01111101               ; check primary buffer for water music
        lda AreaMusicEnvData,y                    ; load default data from offset for all other music
        rts
    endif                                         ; else:

    lda WaterEventMusEnvData,y                    ; load data from offset for water music and all other event music
    rts
.endproc

    ; ------------------------------------------------------------------------------------------------

dataseg
    ; music header offsets
    
MusicHeaderData:

    MHD = MusicHeaderData

    .byte DeathMusHdr-MHD                         ; event music
    .byte GameOverMusHdr-MHD
    .byte VictoryMusHdr-MHD
    .byte WinCastleMusHdr-MHD
    .byte GameOverMusHdr-MHD
    .byte EndOfLevelMusHdr-MHD
    .byte TimeRunningOutHdr-MHD
    .byte SilenceHdr-MHD

    .byte GroundLevelPart1Hdr-MHD                 ; area music
    .byte WaterMusHdr-MHD
    .byte UndergroundMusHdr-MHD
    .byte CastleMusHdr-MHD
    .byte Star_CloudHdr-MHD
    .byte GroundLevelLeadInHdr-MHD
    .byte Star_CloudHdr-MHD
    .byte SilenceHdr-MHD

    .byte GroundLevelLeadInHdr -MHD                ; ground level music layout
    .byte GroundLevelPart1Hdr  -MHD,    GroundLevelPart1Hdr -MHD
    .byte GroundLevelPart2AHdr -MHD,    GroundLevelPart2BHdr-MHD, GroundLevelPart2AHdr-MHD, GroundLevelPart2CHdr-MHD
    .byte GroundLevelPart2AHdr -MHD,    GroundLevelPart2BHdr-MHD, GroundLevelPart2AHdr-MHD, GroundLevelPart2CHdr-MHD
    .byte GroundLevelPart3AHdr -MHD,    GroundLevelPart3BHdr-MHD, GroundLevelPart3AHdr-MHD, GroundLevelLeadInHdr-MHD
    .byte GroundLevelPart1Hdr  -MHD,    GroundLevelPart1Hdr -MHD
    .byte GroundLevelPart4AHdr -MHD,    GroundLevelPart4BHdr-MHD, GroundLevelPart4AHdr-MHD, GroundLevelPart4CHdr-MHD
    .byte GroundLevelPart4AHdr -MHD,    GroundLevelPart4BHdr-MHD, GroundLevelPart4AHdr-MHD, GroundLevelPart4CHdr-MHD
    .byte GroundLevelPart3AHdr -MHD,    GroundLevelPart3BHdr-MHD, GroundLevelPart3AHdr-MHD, GroundLevelLeadInHdr-MHD
    .byte GroundLevelPart4AHdr -MHD,    GroundLevelPart4BHdr-MHD, GroundLevelPart4AHdr-MHD, GroundLevelPart4CHdr-MHD
    
    ; music headers
    ; header format is as follows:
    ; 1 byte - length byte offset
    ; 2 bytes -  music data address
    ; 1 byte - triangle data offset
    ; 1 byte - square 1 data offset
    ; 1 byte - noise data offset (not used by secondary music)

TimeRunningOutHdr:      .byte $08, <TimeRunOutMusData, >TimeRunOutMusData, $27, $18
Star_CloudHdr:          .byte $20, <Star_CloudMData, >Star_CloudMData, $2e, $1a, $40
EndOfLevelMusHdr:       .byte $20, <WinLevelMusData, >WinLevelMusData, $3d, $21
ResidualHeaderData:     .byte $20, $c4, $fc, $3f, $1d
UndergroundMusHdr:      .byte $18, <UndergroundMusData, >UndergroundMusData, $00, $00
SilenceHdr:             .byte $08, <SilenceData, >SilenceData, $00
CastleMusHdr:           .byte $00, <CastleMusData, >CastleMusData, $93, $62
VictoryMusHdr:          .byte $10, <VictoryMusData, >VictoryMusData, $24, $14
GameOverMusHdr:         .byte $18, <GameOverMusData, >GameOverMusData, $1e, $14
WaterMusHdr:            .byte $08, <WaterMusData, >WaterMusData, $a0, $70, $68
WinCastleMusHdr:        .byte $08, <EndOfCastleMusData, >EndOfCastleMusData, $4c, $24
GroundLevelPart1Hdr:    .byte $18, <GroundM_P1Data, >GroundM_P1Data, $2d, $1c, $b8
GroundLevelPart2AHdr:   .byte $18, <GroundM_P2AData, >GroundM_P2AData, $20, $12, $70
GroundLevelPart2BHdr:   .byte $18, <GroundM_P2BData, >GroundM_P2BData, $1b, $10, $44
GroundLevelPart2CHdr:   .byte $18, <GroundM_P2CData, >GroundM_P2CData, $11, $0a, $1c
GroundLevelPart3AHdr:   .byte $18, <GroundM_P3AData, >GroundM_P3AData, $2d, $10, $58
GroundLevelPart3BHdr:   .byte $18, <GroundM_P3BData, >GroundM_P3BData, $14, $0d, $3f
GroundLevelLeadInHdr:   .byte $18, <GroundMLdInData, >GroundMLdInData, $15, $0d, $21
GroundLevelPart4AHdr:   .byte $18, <GroundM_P4AData, >GroundM_P4AData, $18, $10, $7a
GroundLevelPart4BHdr:   .byte $18, <GroundM_P4BData, >GroundM_P4BData, $19, $0f, $54
GroundLevelPart4CHdr:   .byte $18, <GroundM_P4CData, >GroundM_P4CData, $1e, $12, $2b
DeathMusHdr:            .byte $18, <DeathMusData, >DeathMusData, $1e, $0f, $2d
    ; ------------------------------------------------------------------------------------------------
    ; MUSIC DATA
    ; square 2/triangle format
    ; d7 - length byte flag (0-note, 1-length)
    ; if d7 is set to 0 and d6-d0 is nonzero:
    ; d6-d0 - note offset in frequency look-up table (must be even)
    ; if d7 is set to 1:
    ; d6-d3 - unused
    ; d2-d0 - length offset in length look-up table
    ; value of temp_byte in square 2 data is used as null terminator, affects all sound channels
    ; value of temp_byte in triangle data causes routine to skip note
    ; square 1 format
    ; d7-d6, d0 - length offset in length look-up table (bit order is d0,d7,d6)
    ; d5-d1 - note offset in frequency look-up table
    ; value of temp_byte in square 1 data is flag alternate control reg data to be loaded
    ; noise format
    ; d7-d6, d0 - length offset in length look-up table (bit order is d0,d7,d6)
    ; d5-d4 - beat type (0 - rest, 1 - short, 2 - strong, 3 - long)
    ; d3-d1 - unused
    ; value of temp_byte in noise data is used as null terminator, affects only noise
    ; all music data is organized into sections (unless otherwise stated):
    ; square 2, square 1, triangle, noise

Star_CloudMData:
    .byte $84, $2c, $2c, $2c, $82, $04, $2c, $04, $85, $2c, $84, $2c, $2c
    .byte $2a, $2a, $2a, $82, $04, $2a, $04, $85, $2a, $84, $2a, $2a, $00
    .byte $1f, $1f, $1f, $98, $1f, $1f, $98, $9e, $98, $1f
    .byte $1d, $1d, $1d, $94, $1d, $1d, $94, $9c, $94, $1d
    .byte $86, $18, $85, $26, $30, $84, $04, $26, $30
    .byte $86, $14, $85, $22, $2c, $84, $04, $22, $2c
    .byte $21, $d0, $c4, $d0, $31, $d0, $c4, $d0, $00

GroundM_P1Data:
    .byte $85, $2c, $22, $1c, $84, $26, $2a, $82, $28, $26, $04
    .byte $87, $22, $34, $3a, $82, $40, $04, $36, $84, $3a, $34
    .byte $82, $2c, $30, $85, $2a

SilenceData:
    .byte $00
    .byte $5d, $55, $4d, $15, $19, $96, $15, $d5, $e3, $eb
    .byte $2d, $a6, $2b, $27, $9c, $9e, $59
    .byte $85, $22, $1c, $14, $84, $1e, $22, $82, $20, $1e, $04, $87
    .byte $1c, $2c, $34, $82, $36, $04, $30, $34, $04, $2c, $04, $26
    .byte $2a, $85, $22

GroundM_P2AData:
    .byte $84, $04, $82, $3a, $38, $36, $32, $04, $34
    .byte $04, $24, $26, $2c, $04, $26, $2c, $30, $00
    .byte $05, $b4, $b2, $b0, $2b, $ac, $84
    .byte $9c, $9e, $a2, $84, $94, $9c, $9e
    .byte $85, $14, $22, $84, $2c, $85, $1e
    .byte $82, $2c, $84, $2c, $1e

GroundM_P2BData:
    .byte $84, $04, $82, $3a, $38, $36, $32, $04, $34
    .byte $04, $64, $04, $64, $86, $64, $00
    .byte $05, $b4, $b2, $b0, $2b, $ac, $84
    .byte $37, $b6, $b6, $45
    .byte $85, $14, $1c, $82, $22, $84, $2c
    .byte $4e, $82, $4e, $84, $4e, $22

GroundM_P2CData:
    .byte $84, $04, $85, $32, $85, $30, $86, $2c, $04, $00
    .byte $05, $a4, $05, $9e, $05, $9d, $85
    .byte $84, $14, $85, $24, $28, $2c, $82
    .byte $22, $84, $22, $14
    .byte $21, $d0, $c4, $d0, $31, $d0, $c4, $d0, $00

GroundM_P3AData:
    .byte $82, $2c, $84, $2c, $2c, $82, $2c, $30
    .byte $04, $34, $2c, $04, $26, $86, $22, $00
    .byte $a4, $25, $25, $a4, $29, $a2, $1d, $9c, $95

GroundM_P3BData:
    .byte $82, $2c, $2c, $04, $2c, $04, $2c, $30, $85, $34, $04, $04, $00
    .byte $a4, $25, $25, $a4, $a8, $63, $04
    ; triangle data used by both sections of third part
    .byte $85, $0e, $1a, $84, $24, $85, $22, $14, $84, $0c

GroundMLdInData:
    .byte $82, $34, $84, $34, $34, $82, $2c, $84, $34, $86, $3a, $04, $00
    .byte $a0, $21, $21, $a0, $21, $2b, $05, $a3
    .byte $82, $18, $84, $18, $18, $82, $18, $18, $04, $86, $3a, $22
    ; noise data used by lead-in and third part sections
    .byte $31, $90, $31, $90, $31, $71, $31, $90, $90, $90, $00

GroundM_P4AData:
    .byte $82, $34, $84, $2c, $85, $22, $84, $24
    .byte $82, $26, $36, $04, $36, $86, $26, $00
    .byte $ac, $27, $5d, $1d, $9e, $2d, $ac, $9f
    .byte $85, $14, $82, $20, $84, $22, $2c
    .byte $1e, $1e, $82, $2c, $2c, $1e, $04

GroundM_P4BData:
    .byte $87, $2a, $40, $40, $40, $3a, $36
    .byte $82, $34, $2c, $04, $26, $86, $22, $00
    .byte $e3, $f7, $f7, $f7, $f5, $f1, $ac, $27, $9e, $9d
    .byte $85, $18, $82, $1e, $84, $22, $2a
    .byte $22, $22, $82, $2c, $2c, $22, $04

DeathMusData:
    .byte $86, $04                                ; death music share data with fourth part c of ground level music

GroundM_P4CData:
    .byte $82, $2a, $36, $04, $36, $87, $36, $34, $30, $86, $2c, $04, $00
    .byte $00, $68, $6a, $6c, $45                 ; death music only
    .byte $a2, $31, $b0, $f1, $ed, $eb, $a2, $1d, $9c, $95
    .byte $86, $04                                ; death music only
    .byte $85, $22, $82, $22, $87, $22, $26, $2a, $84, $2c, $22, $86, $14
    ; noise data used by fourth part sections
    .byte $51, $90, $31, $11, $00

CastleMusData:
    .byte $80, $22, $28, $22, $26, $22, $24, $22, $26
    .byte $22, $28, $22, $2a, $22, $28, $22, $26
    .byte $22, $28, $22, $26, $22, $24, $22, $26
    .byte $22, $28, $22, $2a, $22, $28, $22, $26
    .byte $20, $26, $20, $24, $20, $26, $20, $28
    .byte $20, $26, $20, $28, $20, $26, $20, $24
    .byte $20, $26, $20, $24, $20, $26, $20, $28
    .byte $20, $26, $20, $28, $20, $26, $20, $24
    .byte $28, $30, $28, $32, $28, $30, $28, $2e
    .byte $28, $30, $28, $2e, $28, $2c, $28, $2e
    .byte $28, $30, $28, $32, $28, $30, $28, $2e
    .byte $28, $30, $28, $2e, $28, $2c, $28, $2e, $00

    .byte $04, $70, $6e, $6c, $6e, $70, $72, $70, $6e
    .byte $70, $6e, $6c, $6e, $70, $72, $70, $6e
    .byte $6e, $6c, $6e, $70, $6e, $70, $6e, $6c
    .byte $6e, $6c, $6e, $70, $6e, $70, $6e, $6c
    .byte $76, $78, $76, $74, $76, $74, $72, $74
    .byte $76, $78, $76, $74, $76, $74, $72, $74

    .byte $84, $1a, $83, $18, $20, $84, $1e, $83, $1c, $28
    .byte $26, $1c, $1a, $1c

GameOverMusData:
    .byte $82, $2c, $04, $04, $22, $04, $04, $84, $1c, $87
    .byte $26, $2a, $26, $84, $24, $28, $24, $80, $22, $00
    .byte $9c, $05, $94, $05, $0d, $9f, $1e, $9c, $98, $9d
    .byte $82, $22, $04, $04, $1c, $04, $04, $84, $14
    .byte $86, $1e, $80, $16, $80, $14

TimeRunOutMusData:
    .byte $81, $1c, $30, $04, $30, $30, $04, $1e, $32, $04, $32, $32
    .byte $04, $20, $34, $04, $34, $34, $04, $36, $04, $84, $36, $00

    .byte $46, $a4, $64, $a4, $48, $a6, $66, $a6, $4a, $a8, $68, $a8
    .byte $6a, $44, $2b

    .byte $81, $2a, $42, $04, $42, $42, $04, $2c, $64, $04, $64, $64
    .byte $04, $2e, $46, $04, $46, $46, $04, $22, $04, $84, $22

WinLevelMusData:
    .byte $87, $04, $06, $0c, $14, $1c, $22, $86, $2c, $22
    .byte $87, $04, $60, $0e, $14, $1a, $24, $86, $2c, $24
    .byte $87, $04, $08, $10, $18, $1e, $28, $86, $30, $30
    .byte $80, $64, $00

    .byte $cd, $d5, $dd, $e3, $ed, $f5, $bb, $b5, $cf, $d5
    .byte $db, $e5, $ed, $f3, $bd, $b3, $d1, $d9, $df, $e9
    .byte $f1, $f7, $bf, $ff, $ff, $ff, $34
    .byte $00                                     ; unused byte

    .byte $86, $04, $87, $14, $1c, $22, $86, $34, $84, $2c
    .byte $04, $04, $04, $87, $14, $1a, $24, $86, $32, $84
    .byte $2c, $04, $86, $04, $87, $18, $1e, $28, $86, $36
    .byte $87, $30, $30, $30, $80, $2c
    ; square 2 and triangle use the same data, square 1 is unused
UndergroundMusData:
    .byte $82, $14, $2c, $62, $26, $10, $28, $80, $04
    .byte $82, $14, $2c, $62, $26, $10, $28, $80, $04
    .byte $82, $08, $1e, $5e, $18, $60, $1a, $80, $04
    .byte $82, $08, $1e, $5e, $18, $60, $1a, $86, $04
    .byte $83, $1a, $18, $16, $84, $14, $1a, $18, $0e, $0c
    .byte $16, $83, $14, $20, $1e, $1c, $28, $26, $87
    .byte $24, $1a, $12, $10, $62, $0e, $80, $04, $04
    .byte $00
    ; noise data directly follows square 2 here unlike in other songs
WaterMusData:
    .byte $82, $18, $1c, $20, $22, $26, $28
    .byte $81, $2a, $2a, $2a, $04, $2a, $04, $83, $2a, $82, $22
    .byte $86, $34, $32, $34, $81, $04, $22, $26, $2a, $2c, $30
    .byte $86, $34, $83, $32, $82, $36, $84, $34, $85, $04, $81, $22
    .byte $86, $30, $2e, $30, $81, $04, $22, $26, $2a, $2c, $2e
    .byte $86, $30, $83, $22, $82, $36, $84, $34, $85, $04, $81, $22
    .byte $86, $3a, $3a, $3a, $82, $3a, $81, $40, $82, $04, $81, $3a
    .byte $86, $36, $36, $36, $82, $36, $81, $3a, $82, $04, $81, $36
    .byte $86, $34, $82, $26, $2a, $36
    .byte $81, $34, $34, $85, $34, $81, $2a, $86, $2c, $00

    .byte $84, $90, $b0, $84, $50, $50, $b0, $00

    .byte $98, $96, $94, $92, $94, $96, $58, $58, $58, $44
    .byte $5c, $44, $9f, $a3, $a1, $a3, $85, $a3, $e0, $a6
    .byte $23, $c4, $9f, $9d, $9f, $85, $9f, $d2, $a6, $23
    .byte $c4, $b5, $b1, $af, $85, $b1, $af, $ad, $85, $95
    .byte $9e, $a2, $aa, $6a, $6a, $6b, $5e, $9d

    .byte $84, $04, $04, $82, $22, $86, $22
    .byte $82, $14, $22, $2c, $12, $22, $2a, $14, $22, $2c
    .byte $1c, $22, $2c, $14, $22, $2c, $12, $22, $2a, $14
    .byte $22, $2c, $1c, $22, $2c, $18, $22, $2a, $16, $20
    .byte $28, $18, $22, $2a, $12, $22, $2a, $18, $22, $2a
    .byte $12, $22, $2a, $14, $22, $2c, $0c, $22, $2c, $14, $22, $34, $12
    .byte $22, $30, $10, $22, $2e, $16, $22, $34, $18, $26
    .byte $36, $16, $26, $36, $14, $26, $36, $12, $22, $36
    .byte $5c, $22, $34, $0c, $22, $22, $81, $1e, $1e, $85, $1e
    .byte $81, $12, $86, $14

EndOfCastleMusData:
    .byte $81, $2c, $22, $1c, $2c, $22, $1c, $85, $2c, $04
    .byte $81, $2e, $24, $1e, $2e, $24, $1e, $85, $2e, $04
    .byte $81, $32, $28, $22, $32, $28, $22, $85, $32
    .byte $87, $36, $36, $36, $84, $3a, $00

    .byte $5c, $54, $4c, $5c, $54, $4c
    .byte $5c, $1c, $1c, $5c, $5c, $5c, $5c
    .byte $5e, $56, $4e, $5e, $56, $4e
    .byte $5e, $1e, $1e, $5e, $5e, $5e, $5e
    .byte $62, $5a, $50, $62, $5a, $50
    .byte $62, $22, $22, $62, $e7, $e7, $e7, $2b

    .byte $86, $14, $81, $14, $80, $14, $14, $81, $14, $14, $14, $14
    .byte $86, $16, $81, $16, $80, $16, $16, $81, $16, $16, $16, $16
    .byte $81, $28, $22, $1a, $28, $22, $1a, $28, $80, $28, $28
    .byte $81, $28, $87, $2c, $2c, $2c, $84, $30

VictoryMusData:
    .byte $83, $04, $84, $0c, $83, $62, $10, $84, $12
    .byte $83, $1c, $22, $1e, $22, $26, $18, $1e, $04, $1c, $00

    .byte $e3, $e1, $e3, $1d, $de, $e0, $23
    .byte $ec, $75, $74, $f0, $f4, $f6, $ea, $31, $2d

    .byte $83, $12, $14, $04, $18, $1a, $1c, $14
    .byte $26, $22, $1e, $1c, $18, $1e, $22, $0c, $14
    ; unused space
    .byte $ff, $ff, $ff

FreqRegLookupTbl:
    .byte $00, $88, $00, $2f, $00, $00
    .byte $02, $a6, $02, $80, $02, $5c, $02, $3a
    .byte $02, $1a, $01, $df, $01, $c4, $01, $ab
    .byte $01, $93, $01, $7c, $01, $67, $01, $53
    .byte $01, $40, $01, $2e, $01, $1d, $01, $0d
    .byte $00, $fe, $00, $ef, $00, $e2, $00, $d5
    .byte $00, $c9, $00, $be, $00, $b3, $00, $a9
    .byte $00, $a0, $00, $97, $00, $8e, $00, $86
    .byte $00, $77, $00, $7e, $00, $71, $00, $54
    .byte $00, $64, $00, $5f, $00, $59, $00, $50
    .byte $00, $47, $00, $43, $00, $3b, $00, $35
    .byte $00, $2a, $00, $23, $04, $75, $03, $57
    .byte $02, $f9, $02, $cf, $01, $fc, $00, $6a

MusicLengthLookupTbl:
    .byte $05, $0a, $14, $28, $50, $1e, $3c, $02
    .byte $04, $08, $10, $20, $40, $18, $30, $0c
    .byte $03, $06, $0c, $18, $30, $12, $24, $08
    .byte $36, $03, $09, $06, $12, $1b, $24, $0c
    .byte $24, $02, $06, $04, $0c, $12, $18, $08
    .byte $12, $01, $03, $02, $06, $09, $0c, $04

EndOfCastleMusicEnvData:
    .byte $98, $99, $9a, $9b

AreaMusicEnvData:
    .byte $90, $94, $94, $95, $95, $96, $97, $98

WaterEventMusEnvData:
    .byte $90, $91, $92, $92, $93, $93, $93, $94
    .byte $94, $94, $94, $94, $94, $95, $95, $95
    .byte $95, $95, $95, $96, $96, $96, $96, $96
    .byte $96, $96, $96, $96, $96, $96, $96, $96
    .byte $96, $96, $96, $96, $95, $95, $94, $93

BowserFlameEnvData:
    .byte $15, $16, $16, $17, $17, $18, $19, $19
    .byte $1a, $1a, $1c, $1d, $1d, $1e, $1e, $1f
    .byte $1f, $1f, $1f, $1e, $1d, $1c, $1e, $1f
    .byte $1f, $1e, $1d, $1c, $1a, $18, $16, $14

BrickShatterEnvData:
    .byte $15, $16, $16, $17, $17, $18, $19, $19
    .byte $1a, $1a, $1c, $1d, $1d, $1e, $1e, $1f

    ; ------------------------------------------------------------------------------------------------
    ; INTERRUPT VECTORS

.segment "VECTORS"

    .word NonMaskableInterrupt, reset, $fff0    ; IRQ unused

.segment "CHRROM"
.incbin "smb_chr.chr"