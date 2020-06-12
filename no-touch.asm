;-----------------------------------------------------------;
;                         Constants                         ;
;-----------------------------------------------------------;

; NES-specific constants
PPUCTRL   = $2000        ; PPU Control Register 1
PPUMASK   = $2001        ; PPU Control Register 2
PPUSTATUS = $2002        ; PPU Status Register
OAMADDR   = $2003        ; Sprite Memory Address
;         = $2004        ; Sprite Memory Data
PPUSCROLL = $2005        ; Background Scroll
PPUADDR   = $2006        ; PPU Memory Address
PPUDATA   = $2007        ; PPU Memory Data
;         = $4000        ; APU Square Wave 1 Register 1
;         = $4001        ; APU Square Wave 1 Register 2
;         = $4002        ; APU Square Wave 1 Register 3
;         = $4003        ; APU Square Wave 1 Register 4
;         = $4004        ; APU Square Wave 2 Register 1
;         = $4005        ; APU Square Wave 2 Register 2
;         = $4006        ; APU Square Wave 2 Register 3
;         = $4007        ; APU Square Wave 2 Register 4
;         = $4008        ; APU Triangle Wave Register 1
;         = $4009        ; APU Triangle Wave Register 2
;         = $400A        ; APU Triangle Wave Register 3
;         = $400B        ; APU Triangle Wave Register 4
;         = $400C        ; APU Noise Register 1
;         = $400D        ; APU Noise Register 2
;         = $400E        ; APU Noise Register 3
;         = $400F        ; APU Noise Register 4
DMC       = $4010        ; DMC Register 1
;         = $4011        ; DMC Register 2
;         = $4012        ; DMC Register 3
;         = $4013        ; DMC Register 4
OAMDMA    = $4014        ; DMA Access to Sprite Memory
;         = $4015        ; Toggle Individual Sound Channels
P1        = $4016        ; Joystick 1
P2        = $4017        ; Joystick 2

KEY_A      = %10000000
KEY_B      = %01000000
KEY_SELECT = %00100000
KEY_START  = %00010000
KEY_UP     = %00001000
KEY_DOWN   = %00000100
KEY_LEFT   = %00000010
KEY_RIGHT  = %00000001

; Custom constants
PLYR_SPRITE = $0200
CAT_SPRITE  = $0210

GRAVITY     = $01



;-----------------------------------------------------------;
;                       iNES Header                         ;
;-----------------------------------------------------------;
; The 16 byte iNES header gives the emulator all
; the information about the game including mapper,
; graphics mirroring, and PRG/CHR sizes. You can
; include all this inside your asm file at the
; very beginning.

  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring enabled

;; Variables go here
  .rsset $0000 ; start variables at ram location 0 in zero page memory

controller    .rs 1  ; controller 1 button vector
framecount    .rs 1  ; counts frames

playerx       .rs 1  ; player x position
playery       .rs 1  ; player y position
playervy      .rs 1  ; player y velocity (negative is up)
playermovingleft  .rs 1  ; player is moving left
playermovingright .rs 1  ; player is moving right
playerfacingleft  .rs 1  ; player is facing left

ground        .rs 1  ; y value of the ground
inAir         .rs 1  ; if player is out of the ground
canJump       .rs 1  ; if player can jump (i.e. has unpressed Up since last press)
jumpTimer     .rs 1  ; time while player can keep pressing Up to jump higher

playerSpriteState  .rs 1 ; which sprite offset is player at
playerSpriteOffset .rs 1 ; helper variable for offset
playerSpriteChange .rs 1 ; another helper variable this seems dumb

bgLo  .rs 1
bgHi  .rs 1
counterLo     .rs 1
counterHi     .rs 1

scrollX       .rs 1  ; horizontal scroll count
scrollY       .rs 1  ; vertical scroll count

;-----------------------------------------------------------;
;                          Bank 0                           ;
;-----------------------------------------------------------;

  .bank 0
  .org $C000
RESET:
  sei          ; ignore IRQs (interrupt requests)
  cld          ; disable decimal mode
  ldx #$40     ; loads value 0x40 on X
  stx $4017    ; disable APU frame IRQ
  ldx #$ff     ; loads value 0xFF on X
  txs          ; Set up stack (puts 0xFF on Stack pointer)
  inx          ; now X = 0 (x++ -> 0xFF + 1)
  stx PPUCTRL  ; disable NMI
  stx PPUMASK  ; disable rendering
  stx DMC      ; disable DMC IRQs (DPCM?)

  ; If the user presses Reset during vblank, the PPU may reset
  ; with the vblank flag still true.  This has about a 1 in 13
  ; chance of happening on NTSC or 2 in 9 on PAL. Clear the
  ; flag now so the vblankwait1 loop sees an actual vblank.
  bit PPUSTATUS

  ; First of two waits for vertical blank to make sure that the
  ; PPU has stabilized
vblankwait1:
  bit PPUSTATUS     ; gets first 2 bits of value in PPUSTATUS
  bpl vblankwait1   ; branches if Negative flag is clear

  ; We now have about 30,000 cycles to burn before the PPU stabilizes.
  ; One thing we can do with this time is put RAM in a known state.
  ; Here we fill it with $00, which matches what (say) a C compiler
  ; expects for BSS. Conveniently, X is still 0.
  txa           ; A = X, sets Z(ero) flag

clrmem:             ; Here we clear internal memory ($0000 - $07FF)
  lda #$00          ; Load 0 on A
  sta $0000, x      ; Set $0000 + X = 0
  sta $0100, x      ; Set $0100 + X = 0
  ; We skip $0200,x on purpose. Usually, RAM page 2 is used for the
  ; display list to be copied to OAM. OAM needs to be initialized to
  ; #$EF-#$FF, not 0, or you'll get a bunch of garbage sprites at (0, 0).
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x

  lda #$FE
  sta $0200, x

  inx
  bne clrmem

  ; Other things you can do between vblank waits are set up audio
  ; or set up other mapper registers.

vblankwait2:      ; Second wait for vblank, PPU is ready after this
  bit PPUSTATUS   ; Poke PPUSTATUS a bit
  bpl vblankwait2 ; Branch on result PLus (Z and N flags are 0)

  lda #%00000000  ; Reset PPU Mask
  sta PPUMASK

;;;;;;;;;;;;;;;;;;;;;;
; Load game palletes
LoadPalettes:
  lda PPUSTATUS         ; read PPU status to reset the high/low latch

  ; Tell CPU where $2007 should be stored, that is, where is VRAM.
  ; VRAM is what the CPU uses to store stuff being drawn, I think.
  ; This address can go from $0000 to $3FFF. Setting here to $3F00
  ; for unknown reasons ¯\_(ツ)_/¯
  lda #$3F              ; = 0011 1111
  sta PPUADDR           ; write the high byte of $(3F)00 address
  lda #$00              ; = 0000 0000
  sta PPUADDR           ; write the low byte of $3F(00) address

  ldx #$00              ; start loop counter at 0
LoadPalettesLoop:
  lda palette, x        ; load data from address (palette + x)
  sta PPUDATA           ; write to PPU (tinyurl.com/NES-PPUDATA)
  inx                   ; X = X + 1
  cpx #$20              ; Compare X to 0x1F (=31) -> copying 32 bytes
                          ; 16 bytes for background palette
                          ; 16 bytes for foreground palette
  bne LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero


  ldx #$00                  ; start loop counter at 0
LoadSpritesLoop:
  lda playersprite, x       ; load data from address (playersprite + x)
  sta PLYR_SPRITE, x        ; store into RAM address ($0200 + x)

  lda catsprite, x          ; load data from address (catsprite+x)
  sta CAT_SPRITE, x         ; store into RAM address ($0210 + x)

  inx                       ; X = X + 1
  cpx #$10                  ; Compare X to 0x0F (=15)
  bne LoadSpritesLoop       ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                            ; if X was equal to 15, keep going down

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA #$00
  STA bgLo            ; put the low byte of the address of background into pointer
  LDA #HIGH(background)
  STA bgHi            ; put the high byte of the address into pointer
  
  LDX #$00            ; start at pointer + 0
  LDY #$00
OutsideLoop:
InsideLoop:
  LDA [bgLo], y       ; copy one background byte from address in pointer plus Y
  STA $2007           ; this runs 256 * 4 times
  
  INY                 ; inside loop counter
  CPY #$00
  BNE InsideLoop      ; run the inside loop 256 times before continuing down
  
  INC bgHi            ; low byte went 0 to 256, so high byte needs to be changed now
  
  INX
  CPX #$04
  BNE OutsideLoop     ; run the outside loop 256 times before continuing down

LoadAttribute:
  lda PPUSTATUS           ; read PPU status to reset the high/low latch
  lda #$23                ; A = 0x23
  sta PPUADDR             ; write the high byte of $(23)C0 address
  lda #$C0                ; A = 0xC0
  sta PPUADDR             ; write the low byte of $23(C0) address

  ldx #$00                ; start out at 0
LoadAttributeLoop:
  lda attributes, x        ; A = attribute + X
  sta PPUDATA             ; write to PPU Data (tinyurl.com/NES-PPUDATA)
  inx                     ; X = X + 1
  cpx #$08                ; Compare X to hex $40, decimal 64
  bne LoadAttributeLoop   ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                          ; if compare was equal to 64, keep going down

  lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  ;     VPHBSINN
  ;     ||||||||
  ;     ||||||++-- Base nametable address
  ;     ||||||     (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
  ;     |||||+---- VRAM address increment per CPU read/write of $2007
  ;     |||||      (0: add 1, going across; 1: add 32, going down)
  ;     ||||+----- Sprite pattern table address for 8x8 sprites
  ;     ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
  ;     |||+------ Background pattern table address (0: $0000; 1: $1000)
  ;     ||+------- Sprite size (0: 8x8; 1: 8x16)
  ;     |+-------- PPU master/slave select
  ;     |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
  ;     +--------- Generate an NMI at the start of the
  ;                vertical blanking interval (0: off; 1: on)
  sta PPUCTRL      ; tinyurl.com/NES-PPUCTRL

  lda #%00011110   ; enable sprites, enable background, no clipping on left side
  ;     BGRsbMmG
  ;     ||||||||
  ;     |||||||+-- Greyscale (0: normal color, 1: produce a greyscale display)
  ;     ||||||+--- 1: Show background in leftmost 8 pixels of screen, 0: Hide
  ;     |||||+---- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
  ;     ||||+----- 1: Show background
  ;     |||+------ 1: Show sprites
  ;     ||+------- Emphasize red*
  ;     |+-------- Emphasize green*
  ;     +--------- Emphasize blue*
  ; * NTSC colors. PAL and Dendy swaps green and red
  sta PPUMASK      ; tinyurl.com/NES-PPUMASK

;;;;;;;;;;;;;;;;;;;;
; Initialze game variables
InitializeState:
  lda #$80
  sta playerx           ; player.x = 0x80 (=128)

  lda #$00
  sta controller        ; controller = 0
  sta playermovingright ; player.movingRight = 0
  sta playermovingleft  ; player.movingLeft = 0
  sta playerfacingleft  ; player.facing = 0
  sta playervy          ; player.velocity.y = 0
  sta playerSpriteState
  sta playerSpriteOffset
  sta playerSpriteChange

  lda #$C7
  sta ground            ; ground = 0xC8 (=200)
  sta playery           ; player.y = ground


;; ----------------------------------------------------------------------------- ;;
;; ----------------------------------------------------------------------------- ;;
;; ----------------------------------------------------------------------------- ;;

Loop:
  ; infinite loop to keep the game from exiting
  ; NMI will interrupt this loop to run the game
  jmp Loop

;; ----------------------------------------------------------------------------- ;;
;; ----------------------------------------------------------------------------- ;;
;; ----------------------------------------------------------------------------- ;;


; Actual update code
NMI: ; Non-Maskable Interrupt (draws screen)
  pha         ; Push A to the stack
  txa         ; Transfer X to A
  pha         ; Push X to the stack
  tya         ; Transfer Y to A
  pha         ; Push Y to the stack

  ;; Load graphics into PPU from the memory
  ;;- Why $0200?
  lda #$00      ; set the low byte $02(00) of the RAM address
  sta OAMADDR   ; (tinyurl.com/NES-OAMADDR)
  lda #$02      ; set the high byte $(02)00 of the RAM address,
  sta OAMDMA    ; start the transfer (tinyurl.com/NES-OAMDMA)

  inc framecount

  jsr Draw      ; Jump to subroutine Draw (and then return here)
  jsr Update    ; Jump to subroutine Update (and then return here)

  ;; Scroll stuff
  ;; Music stuff

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  sta PPUCTRL
  lda #%00011110   ; enable sprites, enable background, no clipping on left side
  sta PPUMASK
  lda #$00        ;;tell the ppu there is no background scrolling
  sta PPUSCROLL
  sta PPUSCROLL

  pla        ; Pull Y from the stack
  tay        ; Transfer A to Y
  pla        ; Pull X from the stack
  tax        ; Transfer A to X
  pla        ; Pull A from the stack
  rti        ; Return from interrupt


;; ----------------------------------------------------------------------------- ;;
;; ----------------------------------------------------------------------------- ;;
;; ----------------------------------------------------------------------------- ;;


Draw:
  jsr DrawBackground ; Jump to subroutine DrawBackground (and then return here)
  jsr DrawSprites    ; Jump to subroutine DrawSprites (and then return here)

  ; TODO: Changing this also doesn't seem to have any effect
  ; This is the PPU clean up section, so rendering the next frame starts properly.
  lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  ;     VPHBSINN
  ;     ||||||||
  ;     ||||||++-- Base nametable address
  ;     ||||||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
  ;     |||||+---- VRAM address increment per CPU read/write of $2007
  ;     |||||     (0: add 1, going across; 1: add 32, going down)
  ;     ||||+----- Sprite pattern table address for 8x8 sprites
  ;     ||||      (0: $0000; 1: $1000; ignored in 8x16 mode)
  ;     |||+------ Background pattern table address (0: $0000; 1: $1000)
  ;     ||+------- Sprite size (0: 8x8; 1: 8x16)
  ;     |+-------- PPU master/slave select
  ;     |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
  ;     +--------- Generate an NMI at the start of the
  ;                vertical blanking interval (0: off; 1: on)
  sta PPUCTRL      ; tinyurl.com/NES-PPUCTRL

  lda #%00011110   ; enable sprites, enable background, no clipping on left side
  ;     BGRsbMmG
  ;     ||||||||
  ;     |||||||+-- Greyscale (0: normal color, 1: produce a greyscale display)
  ;     ||||||+--- 1: Show background in leftmost 8 pixels of screen, 0: Hide
  ;     |||||+---- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
  ;     ||||+----- 1: Show background
  ;     |||+------ 1: Show sprites
  ;     ||+------- Emphasize red*
  ;     |+-------- Emphasize green*
  ;     +--------- Emphasize blue*
  ; * NTSC colors. PAL and Dendy swap green and red
  sta PPUMASK        ; tinyurl.com/NES-PPUMASK

  bit PPUSTATUS      ; tinyurl.com/NES-PPUSTATUS

  lda playerx        ; Scroll X
  sta PPUSCROLL
  lda playery           ; Scroll Y
  sta PPUSCROLL      ; tinyurl.com/NES-PPUSCROLL

  rts                ; Return from subroutine


;;;;;;;;;;;;;;;;;;;;;
; Draw Sprites
DrawSprites:

DrawPlayer:
  ldx #$00              ; start X at 0
  ldy #$00              ; start Y at 0
DrawPlayerLoop:
  clc                          ; CLear Carry flag before addition
  lda squarespriteoffset, y    ; add player x with sprite offset
  adc playerx                  ; playeroffset[0] + center of screen
  sta $0203, x                 ; store into RAM address ($0203 + x)
  iny                          ; increment sprite offset counter

  clc                          ; CLear Carry flag before addition
  lda squarespriteoffset, y    ; add player y with sprite offset
  adc playery
  sta $0200, x                 ; store into RAM address ($0200 + x)

  iny                          ; increment sprite offset counter again
  inx                          ; X = X + 4 loop to the next sprite
  inx
  inx
  inx
  cpx #$20              ; Compare X to 0x20 (=32) meaning all 4 player sprites done
  bne DrawPlayerLoop    ; Branch to DrawPlayerLoop if compare was not 32
                        ; If compare was equal to 32, keep going down

DrawCat:
  ldx #$00              ; start X at 0
  ldy #$00              ; start Y at 0
DrawCatLoop:
  clc                          ; Clear Carry Flag
  lda squarespriteoffset, y    ; add cat x with sprite offset
  adc #$20                     ; squarespriteoffset[0] + CAT X
  sta $0213, x                 ; store into RAM address ($0213 + x)
  iny                          ; increment sprite offset counter

  clc                          ; Clear Carry Flag
  lda squarespriteoffset, y    ; add cat y with sprite offset
  adc ground
  sta $0210, x                 ; store into RAM address ($0200 + x)

  iny
  inx                          ; X = X + 4 loop to the next sprite
  inx
  inx
  inx
  cpx #$20              ; Compare X to hex $20, decimal 32 meaning all 4 Player sprites done
  bne DrawCatLoop       ; Branch to DrawCatLoop if compare was not 32
                        ; if compare was equal to 32, keep going down

  rts                   ; Return from DrawSprites


DrawBackground:
  LDX $1E
  CPX #$01
  BNE FlagClear    ; Check if NMI flag $1E is set

FlagSet:           ; Set up background update
  LDA #$00         ; Disable Rendering
  STA PPUMASK

  LDA PPUSTATUS    ; Reset PPU Hi/Lo latch
  LDA #$20         ; Store Hi/Lo bytes of the background nametables
  STA PPUADDR      ; $2000
  LDA #$00
  STA PPUADDR
  LDY #$00
  LDX #$04

  LDA #%00001000     ; Disable NMI
  STA PPUCTRL

BgLoop:         ; 16 bit loop for drawing background and loading attributes
  LDA [bgLo], y
  STA PPUDATA
  INY
  BNE BgLoop
  INC bgHi
  DEX
  BNE BgLoop

  LDA #$00
  STA $1E             ; Clear NMI flag
  LDA #%10001000      ; Enable NMI
  STA PPUCTRL
  rts

FlagClear:           ; Read controller and draw background
  LDA #%00011110     ; Enable rendering
  STA PPUMASK
  LDA #$10
  STA PPUSCROLL
  STA PPUSCROLL

  rts

Update:
  jsr LatchController
  jsr ReadLeft
  jsr ReadRight
  jsr ReadUp
  jsr UpdatePlayerPosition
  jsr UpdatePlayerSprite
  rts

UpdatePlayerPosition:
  clc              ; clear carry flag so we can test for it ahead

  lda framecount   ; get frame counter
  lsr A            ; check for d0 set
  lsr A            ; check for d0 set
  bcs SkipGravity  ; branch to leave on every other frame

  ;=> player.velocity.y += GRAVITY
  lda playervy     ; A = player.velocity.y
  adc #GRAVITY     ; A += GRAVITY
  sta playervy     ; player.velocity.y = A

SkipGravity:
  ;=> player.y += player.velocity.y
  clc              ; Clear carry flag to not mess up additions
  lda playery      ; A = player.y
  adc playervy     ; A += player.velocity.y
  sta playery      ; player.y = A

  ;=> if(player.y >= ground)
  cmp ground              ; 'tmp' = (ground - player.y)
  bcc PlayerOnGroundDone  ; if('tmp' > 0) leave
  ; Otherwise, player Y should be ground Y
  lda ground       ; A = ground
  sta playery      ; player.y = A
  lda #$00         ; A = 0
  sta playervy     ; player.velocity.y = A
  sta inAir        ; inAir = A
  jmp PlayerOnGroundDone
PlayerOnGroundDone:
  rts              ; Return from subroutine


UpdatePlayerSprite:
  clc
  lda playervy
  bmi UpdateRisingSprite   ; if Y velocity is negative, change to rising sprite
  beq UpdateCheckHorizontal ; if Y velocity is zero, check for horizontal movement

  ; Not rising and has Y velocity, so player is rising
  lda #$04                ; update to downwards sprite
  sta playerSpriteOffset
  jmp ApplyUpdateToSprite

UpdateRisingSprite:
  lda #$02                ; update to upwards sprite
  sta playerSpriteOffset
  jmp ApplyUpdateToSprite

UpdateCheckHorizontal:
  lda playermovingleft    ; if player is moving either left
  adc playermovingright   ; or right, update the sprite accordingly
  bne UpdateMovingSprite

  lda #$00                ; otherwise, set sprite as standing still dog
  sta playerSpriteOffset
  jmp ApplyUpdateToSprite

UpdateMovingSprite:
  clc
  lda framecount            ; get frame counter
  and #%00001000
  lsr A
  lsr A
  lsr A
  cmp playerSpriteChange
  bne IncrementSpriteOffset
  jmp ApplyUpdateToSprite

IncrementSpriteOffset:
  sta playerSpriteChange

  clc
  ldy playerSpriteState
  iny                     ; increment twice so we skip right side of sprite and
  iny                     ; go to the next left side (pos 00, then 02, then 04..)
  cpy #$06
  bcs WrapOffset          ; If > 6, invalid sprite offset
  jmp StoreOffset
WrapOffset:
  ldy #$00                ; Otherwise, wrap back to 0
StoreOffset:
  sty playerSpriteState
  sty playerSpriteOffset
ApplyUpdateToSprite:
  lda playerfacingleft
  bne ApplyUpdateFacingLeft

ApplyUpdateFacingRight:
  ldx #$00         ; Load 'unflipped' (#$00) attribute to X
  ; To unflip the sprite, this is what
  ; we want playersprite to become:
  ;         Y   tile attr  X
  ; $0200  [00] [01] [02] [03]
  ;  [00]  $xx, $00, $00, $xx
  ;  [04]  $xx, $01, $00, $xx
  ;  [08]  $xx, $10, $00, $xx
  ;  [0C]  $xx, $11, $00, $xx
  clc
  lda playerSpriteOffset
  ; FIXME: Potential overflow?
  sta $0201    ; Top left tile
  adc #$01
  sta $0205    ; Top right tile
  adc #$10
  sta $020D    ; Bottom right tile
  sec
  sbc #$01
  sta $0209    ; Bottom left tile

  jmp UpdatePlayerAttributes

ApplyUpdateFacingLeft:
  ldx #%01000000         ; Load 'flipped' (#$40) attribute to X
  ; To flip the sprite, this is what
  ; we want playersprite to become:
  ;         Y   tile attr  X
  ; $0200  [00] [01] [02] [03]
  ;  [00]  $xx, $01, $40, $xx
  ;  [04]  $xx, $00, $40, $xx
  ;  [08]  $xx, $11, $40, $xx
  ;  [0C]  $xx, $10, $40, $xx
  clc
  lda playerSpriteOffset
  ; FIXME: Potential overflow?
  sta $0205    ; Top left tile
  adc #$01
  sta $0201    ; Top right tile
  adc #$10
  sta $0209    ; Bottom right tile
  sec
  sbc #$01
  sta $020D    ; Bottom left tile

UpdatePlayerAttributes:
  ; Set or unset 'flipped' attribute
  ; X should be either #$00 or #$40 (#%01000000)
  ; Attribute byte:
  ;  76 5░ ░░ 10
  ;  || || || ||
  ;  || || || ++- Palette (4 to 7) of sprite
  ;  || |+-++---- Unimplemented
  ;  || +-------- Priority (0: in front of background; 1: behind background)
  ;  |+---------- Flip sprite horizontally
  ;  +----------- Flip sprite vertically

  txa         ; Fetch 'flipped' attribute from X
  sta $0202   ; Store everywhere
  sta $0206
  sta $020A
  sta $020E

  rts


;--------------------------------------------------;


LatchController:
  LDA #$01
  STA P1
  LDA #$00
  STA P1            ; tell both the controllers to latch buttons
PollController:
  ldx #$00          ; 8 buttons total
PollControllerLoop:
  lda P1            ; load joystick 1
  lsr A             ; shift right
  ROL controller    ; rotate left button vector in mem location $0003
  inx
  cpx #$08
  bne PollControllerLoop
  rts

ReadRight:
  lda controller
  and #%00000001       ; only look at bit #0
; bit:     7   6   5   4   3   2   1   0
; button:  A   B  Sel Sta Up Down Left Right
  beq ReadRightUnpressed ; branch to ReadRightUnpressed if button is NOT pressed (0)
                         ; otherwise, instructions for when button IS pressed (1)
  clc                    ; make sure the carry flag is clear
  lda playerx
  adc #$01
  sta playerx

  lda #$01
  sta playermovingright

  lda #$00
  cmp playerfacingleft ; If player is already facing right (facing left = 0)
  beq ReadRightDone    ; then skip sprite flipping
  sta playerfacingleft ; Otherwise, set it to face right (facing left = 0)

  jmp ReadRightDone

ReadRightUnpressed:
  lda #$00
  sta playermovingright

ReadRightDone:
  rts


ReadLeft:
  lda controller
  and #%00000010       ; only look at bit 1
; bit:     7   6   5   4   3   2   1   0
; button:  A   B  Sel Sta Up Down Left Right
  beq ReadLeftUnpressed ; branch to ReadLeftDone if button is NOT pressed (0)
                        ; otherwise, instructions for when button IS pressed (1)
  clc                   ; make sure the carry flag is clear
  lda playerx           ; A = player.x
  adc #$FF              ; A += -1
  sta playerx           ; player.x = A

  lda #$01
  sta playermovingleft

  lda #$01
  cmp playerfacingleft ; If player is already facing left (= 1),
  beq ReadLeftDone     ; then skip sprite flipping
  sta playerfacingleft ; Otherwise, set it to face left

  jmp ReadLeftDone

ReadLeftUnpressed:
  lda #$00
  sta playermovingleft

ReadLeftDone:
  rts


ReadUp:
  lda controller   ; get controller 1
  and #%00001000   ; if Up is pressed
; bit:     7   6   5   4   3   2   1   0
; button:  A   B  Sel Sta Up Down Left Right
  beq ReadUpUnpressed   ; branch to ReadUpDone if button is NOT pressed (0)

  lda inAir
  cmp #$01         ; if player is already in air, don't jump again
  beq ReadUpKeepJumping

  lda canJump      ; if player cannot jump (i.e. hasn't unpressed Up)
  beq ReadUpDone   ; then leave

  lda #$FE         ; set player Y velocity to -1
  sta playervy

  lda #$01
  sta inAir        ; player is now in air

  lda #00          ; player can no longer jump
  sta canJump      ; until Up is unpressed

  lda #$0A         ; reset jump height timer
  sta jumpTimer
  jmp ReadUpDone

ReadUpUnpressed:   ; player no longer pressing Up
  lda #00          ; reset jump height timer
  sta jumpTimer
  ; FIXME: it's possible to buffer jumps; if you unpress Up mid jump, then hold
  ; it down again, the dog will auto jump when it hits the ground again
  lda #01          ; player can now jump again
  sta canJump
  jmp ReadUpDone

ReadUpKeepJumping:
  lda jumpTimer    ; if jump height timer has run out,
  beq ReadUpDone   ; leave; otherwise,
  dec jumpTimer    ; decrement jump timer
  lda #$FC         ; set player Y velocity to -2
  sta playervy

ReadUpDone:        ; handling this button is done
  rts


;-----------------------------------------------------------;
;                          Bank 1                           ;
;-----------------------------------------------------------;
  .bank 1
  .org $E000
palette:
  ;; Background Palletes (0-3)
  .db $35,$08,$18,$10  ; Background palette 1 (brown)
  .db $31,$1B,$2A,$36  ; Background palette 2 (green)
  .db $31,$36,$37,$10  ; Background palette 3 (beige)
  .db $31,$0F,$33,$36  ; Background palette 4

  ;;  Character Palletes (4-7)
  .db $31,$0F,$33,$36  ; Sprite palette 1
  .db $31,$0F,$33,$36  ; Sprite palette 2
  .db $31,$0F,$33,$36  ; Sprite palette 3
  .db $31,$0F,$33,$36  ; Sprite palette 4
  ;  $31 - blue (sky)
  ;  $0F - black (outlines)
  ;  $33 - purple (dog)
  ;  $36 - orange (cat)

playersprite:
; 1st byte encodes the y position
; 2nd byte encodes the tile index loaded into the PPU
; 3rd byte encodes any sprite attributes
; 4th byte encodes the x position
  ;   vert tile attr horiz
  .db $80, $00, $00, $80   ; sprite 0  (top left)
  .db $80, $01, $00, $88   ; sprite 1  (top right)
  .db $88, $10, $00, $80   ; sprite 2  (bottom left)
  .db $88, $11, $00, $88   ; sprite 3  (bottom right)

;; Attribute byte:
;   76 5░ ░░ 10
;   || || || ||
;   || || || ++- Palette (4 to 7) of sprite
;   || |+-++---- Unimplemented
;   || +-------- Priority (0: in front of background; 1: behind background)
;   |+---------- Flip sprite horizontally
;   +----------- Flip sprite vertically

catsprite:
  ;   vert tile attr horiz
  .db $80, $20, $00, $80   ; sprite 0  (top left)
  .db $80, $21, $00, $88   ; sprite 1  (top right)
  .db $88, $30, $00, $80   ; sprite 2  (bottom left)
  .db $88, $31, $00, $88   ; sprite 3  (bottom right)


squarespriteoffset:
  ; On-screen offset
  ;    x    y
  .db $F8, $F8; (-8, -8)  (top left)
  .db $00, $F8; ( 0, -8)  (top right)
  .db $F8, $00; (-8,  0)  (bottom left)
  .db $00, $00; ( 0,  0)  (bottom right)


background:
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

  .db $12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12
  .db $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22

  .db $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22
  .db $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22

  .db $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22
  .db $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22

  .db $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22
  .db $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22

  .db $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22
  .db $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22

  .db $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22
  .db $22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22

attributes:  ;8 x 8 = 64 bytes
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

; .org $FFFA -> starting at $FFFA
; .dw means 'store dataword', in the NES that means 16 bits, 2 bytes
; stores in little endian order, i.e. least significant byte first
  .org $FFFA     ; first of the three vectors starts here
  ; After this $FFFA + 2 = $FFFC
  .dw NMI        ; when an NMI happens (once per frame if enabled) the 
                 ; processor will jump to the label NMI
  ; After this $FFFC + 2 = $FFFE
  .dw RESET      ; when the processor first turns on or is reset, it will jump
                 ; to the label RESET
  ; After this $FFFE + 1 = $FFFF
  .dw 0          ; external interrupt IRQ is not used


;-----------------------------------------------------------;
;                          Bank 2                           ;
;-----------------------------------------------------------;

  .bank 2
  .org $0000
  .incbin "no-touch.chr"