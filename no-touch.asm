;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   Constants    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
                    ;;;;
PPUCTRL   = $2000   ;;;; PPU Control Register 1
PPUMASK   = $2001   ;;;; PPU Control Register 2
PPUSTATUS = $2002   ;;;; PPU Status Register
OAMADDR   = $2003   ;;;; Sprite Memory Address
;         = $2004   ;;;; Sprite Memory Data
PPUSCROLL = $2005   ;;;; Background Scroll
PPUADDR   = $2006   ;;;; PPU Memory Address
PPUDATA   = $2007   ;;;; PPU Memory Data
;         = $4000   ;;;; APU Square Wave 1 Register 1
;         = $4001   ;;;; APU Square Wave 1 Register 2
;         = $4002   ;;;; APU Square Wave 1 Register 3
;         = $4003   ;;;; APU Square Wave 1 Register 4
;         = $4004   ;;;; APU Square Wave 2 Register 1
;         = $4005   ;;;; APU Square Wave 2 Register 2
;         = $4006   ;;;; APU Square Wave 2 Register 3
;         = $4007   ;;;; APU Square Wave 2 Register 4
;         = $4008   ;;;; APU Triangle Wave Register 1
;         = $4009   ;;;; APU Triangle Wave Register 2
;         = $400A   ;;;; APU Triangle Wave Register 3
;         = $400B   ;;;; APU Triangle Wave Register 4
;         = $400C   ;;;; APU Noise Register 1
;         = $400D   ;;;; APU Noise Register 2
;         = $400E   ;;;; APU Noise Register 3
;         = $400F   ;;;; APU Noise Register 4
;         = $4010   ;;;; DMC Register 1
;         = $4011   ;;;; DMC Register 2
;         = $4012   ;;;; DMC Register 3
;         = $4013   ;;;; DMC Register 4
OAMDMA    = $4014   ;;;; DMA Access to Sprite Memory
;         = $4015   ;;;; Enable/Disable Individual Sound Channels
JOYSTICK1 = $4016   ;;;; Joystick 1
JOYSTICK2 = $4017   ;;;; Joystick 2
                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

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
  .inesmir 1   ; background mirroring

;; Variables go here
  .rsset $0000 ; start variables at ram location 0 in zero page memory

loopCount     .rs 1 ; count the loop
playerx       .rs 1 ; player x pos
playervx      .rs 1 ; player x vel
playery       .rs 1 ; player y pos
playervy      .rs 1 ; player y vel (negative is up)
playerfacing  .rs 1 ; direction player is facing
controller    .rs 1 ; controller 1 button vector

gravity       .rs 1 ; gravity

ground        .rs 1 ; y value of the ground
inAir         .rs 1

backgroundLo  .rs 1
backgroundHi  .rs 1
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
  sei          ; ignore IRQs
  cld          ; disable decimal mode
  ldx #$40     ; loads value 0x40 on X
  stx $4017    ; disable APU frame IRQ
  ldx #$ff     ; loads value 0xFF on X
  txs          ; Set up stack             (puts 0xFF on Stack pointer)
  inx          ; now X = 0                (x++ -> 0xFF + 1)
  stx PPUCTRL  ; disable NMI
  stx PPUMASK  ; PPUMASK - disable rendering
  stx $4010    ; disable DMC IRQs         (puts 0x00 on $4010 [part of controller/audio access ports])

  ; If the user presses Reset during vblank, the PPU may reset
  ; with the vblank flag still true.  This has about a 1 in 13
  ; chance of happening on NTSC or 2 in 9 on PAL.  Clear the
  ; flag now so the vblankwait1 loop sees an actual vblank.
  bit PPUSTATUS

  ; First of two waits for vertical blank to make sure that the
  ; PPU has stabilized
vblankwait1:
  bit PPUSTATUS     ; (gets first 2 bits of value on $2002 - PPUSTATUS)
  bpl vblankwait1   ; (branches if Negative flag is clear)

  ; We now have about 30,000 cycles to burn before the PPU stabilizes.
  ; One thing we can do with this time is put RAM in a known state.
  ; Here we fill it with $00, which matches what (say) a C compiler
  ; expects for BSS. Conveniently, X is still 0.
  txa           ; A = X, sets Z(ero) flag

clrmem:             ; Here we set everything to 0x00
  lda #$00          ; Load 0x00 on A
  sta $0000, x      ; Set $0000 + X = 0
  sta $0100, x      ; Set $0100 + X = 0
  ; We skip $0200,x on purpose. Usually, RAM page 2 is used for the
  ; display list to be copied to OAM. OAM needs to be initialized to
  ; $EF-$FF, not 0, or you'll get a bunch of garbage sprites at (0, 0).
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  lda #$FE
  sta $0200, x

  inx              ; x is now 0x01
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
  ; for unknown reasons.
  ; 
  lda #$3F              ; = 0x0011 1111
  sta PPUADDR           ; write the high byte of $3F00 address
  lda #$00              ; = 0x0000 0000
  sta PPUADDR           ; write the low byte of $3F00 address

  ldx #$00              ; start loop counter at 0
LoadPalettesLoop:
  lda palette, x        ; load data from address (palette + x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; ...
  sta PPUDATA             ; write to PPU (tinyurl.com/NES-PPUDATA)
  inx                   ; X = X + 1
  cpx #$20              ; Compare X to hex $20, decimal 32 - copying 32 bytes
                          ; 16 bytes for background palette
                          ; 16 bytes for foreground palette
  bne LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero

LoadPlayer:
  ldx #$00                  ; start loop counter at 0
LoadSpritesLoop:
  lda playersprite, x       ; load data from address (playersprite.right + x)
  sta $0200, x              ; store into RAM address ($0200 + x)

  lda catsprite, x
  sta $0210, x

  inx                       ; X = X + 1
  cpx #$0F                  ; Compare X to hex $0F, decimal 15
  bne LoadSpritesLoop       ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                            ; if compare was equal to 15, keep going down

;; Background is 960 bytes 240 * 4
LoadBackground:
  lda PPUSTATUS         ; read PPU status to reset the high/low latch

  lda #$20
  sta PPUADDR           ; write the high byte of $(20)00 address
  lda #$00
  sta PPUADDR           ; write the low byte of $20(00) address

  ldx #$00              ; start out at 0

  ;; we need to copy more that 256
  lda #LOW(background)  ; Get low byte of <background>
  sta backgroundLo      ; Store it in <backgroundLo>
  lda #HIGH(background) ; Get high byte of <background>
  sta backgroundHi      ; Store it in <backgroundHi>

  ;; 960 bytes = $03C0
  lda #$C0;
  sta counterLo
  lda #$03
  sta counterHi

  ldy #$00                ; Y will always be 0, we just need it to be initialized
                          ; to 0 ; so indirect index mode works in the square
                          ; bracket. That is, "lda [backgroundLo], y" works, but
                          ; simply "lda [backgroundLo]" doesn't.
LoadBackgroundLoop:
  lda [backgroundLo], y   ; load data from background
  sta PPUDATA             ; write to PPU data port to copy to background data
                          ; tinyurl.com/NES-PPUDATA

  clc                     ; clear the carry bit
  lda backgroundLo        ; A = backgroundLo
  adc #$01                ; A++
  sta backgroundLo        ; backgroundLo = A

  lda backgroundHi        ; A = backgroundHi
  adc #$00                ; add 0, but if there is a carry (overflow) add 1
  sta backgroundHi        ; inc pointer to the next byte if necessary
  ;; This basically functions as 2 nested FOR loops
  ;; for(backgroundHi = 0; ; backgroundHi++)
  ;;     for(backgroundLo = 0; ; backgroundLo++)

  lda counterLo           ; load the counter low byte
  SEC                     ; set cary flag
  SBC #$01                ; subtract with carry by 1
  STA counterLo           ; store the low byte of the counter
  LDA counterHi           ; load the high byte
  SBC #$00                ; sub 0, but there is a carry
  STA counterHi           ; decrement the loop counter

  LDA counterLo           ; load the low byte
  CMP #$00                ; see if it is zero, if not loop
  BNE LoadBackgroundLoop
  LDA counterHi
  CMP #$00                ; see if the high byte is zero, if not loop
  BNE LoadBackgroundLoop  ; if the loop counter isn't 0, keep copying

LoadAttribute:
  lda PPUSTATUS           ; read PPU status to reset the high/low latch
  lda #$23                ; A = 0x23
  sta PPUADDR             ; write the high byte of $(23)C0 address
  lda #$C0                ; A = 0xC0
  sta PPUADDR             ; write the low byte of $23(C0) address

  ldx #$00                ; start out at 0
LoadAttributeLoop:
  lda attribute, x        ; A = attribute + X
  sta PPUDATA             ; write to PPU Data (tinyurl.com/NES-PPUDATA)
  inx                     ; X = X + 1
  cpx #$40                ; Compare X to hex $40, decimal 64
  bne LoadAttributeLoop   ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                          ; if compare was equal to 64, keep going down

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
  ; * NTSC colors. PAL and Dendy swaps green and red
  sta PPUMASK      ; tinyurl.com/NES-PPUMASK


;;;;;;;;;;;;;;;;;;;;
; Initialze game variables
InitialzeState:
  lda #$80
  sta playerx   ; player.x = 0x80 = 128

  lda #$00
  sta playery   ; player.y = 0

  sta controller   ; controller = 0
  sta playerfacing ; player.facing = 0
  sta playervx     ; player.velocity.x = 0
  sta playervy     ; player.velocity.y = 0
  sta loopCount    ; loopCount = 0

  lda #$C8
  sta ground    ; ground = 0xC8 = 200

  lda #$02
  sta gravity   ; gravity = 3

Loop:
  ; infinite loop to keep the game from exiting
  ; NMI will interrupt this loop to run the game
  jmp Loop

; Main game loop
NMI: ; Non-Maskable Interrupt (draws screen)
  pha         ; Push A to the stack
  txa
  pha         ; Push X to the stack
  tya
  pha         ; Push Y to the stack

  ;; Load graphics into PPU from the memory
  lda #$00
  sta OAMADDR   ; set the low byte $02(00) of the RAM address (tinyurl.com/NES-OAMADDR)
  lda #$02
  sta OAMDMA    ; set the high byte $(02)00 of the RAM address, start the transfer (tinyurl.com/NES-OAMDMA)

  jsr Draw      ; Jump to subroutine Draw (and then return here)
  jsr Update    ; Jump to subroutine Update (and then return here)

  ;; Scroll stuff
  ;; Music stuff

  pla        ; Pull Y from the stack
  tay
  pla        ; Pull X from the stack
  tax
  pla        ; Pull A from the stack
  rti        ; Return from interrupt

Draw:
  jsr DrawSprites  ; Jump to subroutine DrawSprites (and then return here)

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

  lda #%00011100   ; enable sprites, enable background, no clipping on left side
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
  sta PPUMASK      ; tinyurl.com/NES-PPUMASK

  bit PPUSTATUS      ; tinyurl.com/NES-PPUSTATUS
  lda #$00         ; Scroll X
  sta PPUSCROLL        ; PPUSCROLL
  lda #$00         ; Scroll Y
  sta PPUSCROLL      ; tinyurl.com/NES-PPUSCROLL

  rts                  ; Return from subroutine


;;;;;;;;;;;;;;;;;;;;;
; Draw Sprites
DrawSprites:

DrawPlayer:
  ldx #$00              ; start X at 0
  ldy #$00              ; start Y at 0
DrawPlayerLoop:
  clc                          ; Clear Carry Flag
  lda squarespriteoffset, y    ; add player x with sprite offset
  adc playerx                  ; playeroffset[0] + playerx
  sta $0203, x                 ; store into RAM address ($0203 + x)
  iny                          ; increment sprite offset counter

  clc                          ; Clear Carry Flag
  lda squarespriteoffset, y    ; add player y with sprite offset
  adc playery
  sta $0200, x                 ; store into RAM address ($0200 + x)

  iny
  inx                          ; X = X + 4 loop to the next sprite
  inx
  inx
  inx
  cpx #$20              ; Compare X to hex $20, decimal 32 meaning all 4 Player sprites done
  bne DrawPlayerLoop    ; Branch to DrawPlayerLoop if compare was not 32
                        ; if compare was equal to 32, keep going down

DrawCat:
  ldx #$00              ; start X at 0
  ldy #$00              ; start Y at 0
DrawCatLoop:
  clc                          ; Clear Carry Flag
  lda squarespriteoffset, y    ; add cat x with sprite offset
  adc #$20                     ; squarespriteoffset[0] + CAT X
  sta $0213, x                 ; store into RAM address ($0203 + x)
  iny                          ; increment sprite offset counter

  clc                          ; Clear Carry Flag
  lda squarespriteoffset, y    ; add cat y with sprite offset
  adc #$C8
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

Update:
  jsr LatchController
  jsr PollController
  jsr ReadLeft
  jsr ReadRight
  jsr ReadBothHorizontal
  jsr ReadUp
  inc loopCount             ; loopCount++
  jsr UpdatePlayerPosition
  rts

ReadBothHorizontal:
  clc
  lda controller
  and #%00000011       ; only look at bits 0,1
; bit:     7   6   5   4   3   2   1   0
; button:  A   B  Sel Sta Up Down Left Right
  beq StopHorizontalMovement  ; branch if neither are being pressed
  rts

StopHorizontalMovement:
  lda #$00
  sta playervx
  rts

UpdatePlayerPosition:
  clc              ; Clear carry flag to not mess up additions

  ;=> if(loopCount != 10)
  ;=>     goto SkipGravity;
  lda loopCount    ; A = loopCount
  cmp #$0A         ; 'tmp' = (10 - loopCount)
  bne SkipGravity  ; if('tmp' != 0) goto SkipGravity;

  ;=> player.velocity.y += gravity
  lda playervy     ; A = player.velocity.y
  adc gravity      ; A += gravity
  sta playervy     ; player.velocity.y = A

  ;=> loopCount = 0
  lda #$00         ; A = 0
  sta loopCount    ; loopCount = A
SkipGravity:
  clc              ; Clear carry flag to not mess up additions

  ; player.y += player.velocity.y
  lda playery      ; A = player.y
  adc playervy     ; A += player.velocity.y
  sta playery      ; player.y = A

  ; if(player.y >= ground)
  cmp ground       ; 'tmp' = (ground - player.y)
  bcs PutPlayerOnGround  ; if('tmp' <= 0) goto PutPlayerOnGround
PlayerOnGroundDone:
  clc              ; Clear carry flag to not mess up additions
  lda playervx     ; A = player.velocity.x
  adc playerx      ; A += player.x
  sta playerx      ; player.x = A
  rts              ; Return from subroutine
PutPlayerOnGround:
  lda ground       ; A = ground
  sta playery      ; player.y = A
  lda #$00         ; A = 0
  sta playervy     ; player.velocity.y = 0
  sta inAir        ; inAir = 0
  jmp PlayerOnGroundDone


LatchController:
  LDA #$01
  STA JOYSTICK1
  LDA #$00
  STA JOYSTICK1    ; tell both the controllers to latch buttons
  rts

PollController:
  ldx #$00          ; 8 buttons total
PollControllerLoop:
  lda JOYSTICK1     ; load joystick 1
  lsr A             ; shift right
  ROL controller    ; rotate left button vector in mem location $0003
  inx
  cpx #$08
  bne PollControllerLoop
  rts

ReadRight:
  lda controller
  and #%00000001       ; only look at bit 0
; bit:     7   6   5   4   3   2   1   0
; button:  A   B  Sel Sta Up Down Left Right
  beq ReadRightDone    ; branch to ReadRightDone if button is NOT pressed (0)
                       ; add instructions here to do something when button IS pressed (1)
  clc                  ; make sure the carry flag is clear
  lda #$01
  sta playervx

  lda #$00
  cmp playerfacing     ; If player is already facing right, skip
  beq ReadRightDone
  sta playerfacing

; To unflip the sprite, this is what
; we want playersprite to become:

;         Y   tile attr  X
; $0200  [00] [01] [02] [03]
;  [00]  $xx, $00, $00, $xx
;  [04]  $xx, $01, $00, $xx
;  [08]  $xx, $10, $00, $xx
;  [0C]  $xx, $11, $00, $xx

  ;lda #$00    ; A is already = 0 from above
  sta $0202    ; Make attribute bytes = 0
  sta $0206    ; Make attribute bytes = 0
  sta $020A    ; Make attribute bytes = 0
  sta $020E    ; Make attribute bytes = 0

  sta $0201    ; Top left tile
  lda #$01
  sta $0205    ; Top right tile
  lda #$10
  sta $0209    ; Bottom left tile
  lda #$11
  sta $020D    ; Bottom right tile

ReadRightDone:         ; handling this button is done
  rts


ReadLeft:
  lda controller
  and #%00000010       ; only look at bit 1
; bit:     7   6   5   4   3   2   1   0
; button:  A   B  Sel Sta Up Down Left Right
  beq ReadLeftDone     ; branch to ReadLeftDone if button is NOT pressed (0)
                       ; add instructions here to do something when button IS pressed (1)
  clc                  ; make sure the carry flag is clear
  lda #$FF
  sta playervx

  lda #$01
  cmp playerfacing     ; If player is already facing left, skip
  beq ReadRightDone
  sta playerfacing     ; playerfacing = 1

; To flip the sprite, this is what
; we want playersprite to become:

;         Y   tile attr  X
; $0200  [00] [01] [02] [03]
;  [00]  $xx, $01, $40, $xx
;  [04]  $xx, $00, $40, $xx
;  [08]  $xx, $11, $40, $xx
;  [0C]  $xx, $10, $40, $xx
  ;lda #$01    ; A is already = 1 from above
  sta $0201    ; Top left tile
  lda #$00
  sta $0205    ; Top right tile
  lda #$11
  sta $0209    ; Bottom left tile
  lda #$10
  sta $020D    ; Bottom right tile

  lda #$40
  sta $0202    ; Attribute byte
  sta $0206    ; Attribute byte
  sta $020A    ; Attribute byte
  sta $020E    ; Attribute byte

ReadLeftDone:          ; handling this button is done
  rts


ReadUp:
  lda controller  ; Get controller 1
  and #%00001000  ; If Up is pressed
; bit:     7   6   5   4   3   2   1   0
; button:  A   B  Sel Sta Up Down Left Right
  beq ReadUpDone   ; branch to ReadUpDone if button is NOT pressed (0)

  lda inAir
  cmp #$01         ; if player is in air, don't do jump again
  beq ReadUpDone

  LDA ground
  STA playery
  LDA #$FA
  STA playervy

  lda #$01
  sta inAir       ; player is now in air
ReadUpDone:       ; handling this button is done
  rts

palette:
  ;; Background Palletes (0-3)
  .db $2A,$0F,$0F,$0F,  $25,$0F,$0F,$0F,  $0F,$0F,$0F,$0F,  $0F,$0F,$0F,$0F
  ;;  Character Palletes (4-7)
  .db $31,$0F,$33,$36,  $0F,$0F,$0F,$0F,  $0F,$0F,$0F,$0F,  $0F,$0F,$0F,$0F
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

squarespriteoffset:
  ; On-screen offset
  ;    x    y
  .db $F8, $F8; (-8, -8)  (top left)
  .db $00, $F8; ( 0, -8)  (top right)
  .db $F8, $00; (-8,  0)  (bottom left)
  .db $00, $00; ( 0,  0)  (bottom right)

catsprite:
  ;   vert tile attr horiz
  .db $80, $20, $00, $80   ; sprite 0  (top left)
  .db $80, $21, $00, $88   ; sprite 1  (top right)
  .db $88, $30, $00, $80   ; sprite 2  (bottom left)
  .db $88, $31, $00, $88   ; sprite 3  (bottom right)


;;; TODO: Why does altering background not affect anything? :c
background:
  ; nametable 960 bytes long for the background
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01
  .db $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01  ;;ground

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;row 1
  .db $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11, $11,$11  ;;all sky

  .db $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01
  .db $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01  ;;ground

  .db $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10
  .db $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10  ;; dirt

  .db $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10
  .db $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10  ;; dirt

  .db $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10
  .db $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10, $10,$10  ;; dirt


;;; TODO: Why does altering attribute not change anything? :c
attribute:
  ; 64 bytes following a nametable
  .db $01,$01,$01,$01,  $02,$02,$02,$02
  .db $01,$01,$01,$01,  $02,$02,$02,$02
  .db $01,$01,$01,$01,  $02,$02,$02,$02
  .db $01,$01,$01,$01,  $02,$02,$02,$02

  .db $03,$03,$03,$03,  $00,$00,$00,$00
  .db $03,$03,$03,$03,  $00,$00,$00,$00
  .db $03,$03,$03,$03,  $00,$00,$00,$00
  .db $03,$03,$03,$03,  $00,$00,$00,$00


;-----------------------------------------------------------;
;                          Bank 1                           ;
;-----------------------------------------------------------;

; .org means starting at $FFFA
; .dw means store dataword, in the NES that means 16 bits 2 bytes
; stores in little endian order, that is, least significant byte first
  .bank 1
  .org $FFFA     ; first of the three vectors starts here
nescallback:
  ; After this $FFFA + 2 = $FFFC
  .dw NMI        ; when an NMI happens (once per frame if enabled) the 
                 ; processor will jump to the label NMI:
  ; After this $FFFC + 2 = $FFFE
  .dw RESET      ; when the processor first turns on or is reset, it will jump
                 ; to the label RESET:
  ; After this $FFFE + 1 = $FFFF
  .dw 0          ; external interrupt IRQ are not used

;-----------------------------------------------------------;
;                          Bank 2                           ;
;-----------------------------------------------------------;

  .bank 2
  .org $0000
  .incbin "no-touch.chr"