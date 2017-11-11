# 6502-Assembler
This is an Assembler for the MOS 6502.
To assemble a file, run `Python asm.py <source-file> <target-file>`, for example `Python asm.py Kwirk/Kwirk.asm Kwirk/Kwirk.hex`
* If `<target-file>` is ending in ".txt" or ".hex", then the file will be assembled into a file consisting only of a hexadecimal string.
* Otherwise a binary file will be created
* If `<target-file>` is omitted, the assembled code will be copied to clipboard as a hexadecimal string.

ASM-Syntax
==========

Addressing-Modes
---------------
* Imlicit: `OPC` e.g. `RTS`
* Accumulator: `OPC A` e.g. `ROR A` (Operations on Accumulator are explicit)
* Immediate: `OPC #<byte>` e.g. `ORA #0x44`
* Absolute: `OPC <Word>` e.g. `JMP 0x4400`
* Absolute,x: `OPC <Word>,x` e.g. `ADC 0x4400,x`
* Absolute,y: `OPC <Word>,y` e.g. `SBC 0x4400,y`
* Zero Page: `OPC <byte>` e.g. `AND 0x44`
* Zero Page,x: `OPC <byte>,x` e.g. `LDY 0x44,x`
* Zero Page,y: `OPC <byte>,y` e.g. `LDX 0x44,y`
* Indirect,x: `OPC (<byte>,x)` e.g. `STA (0x44,x)`
* Indirect,y: `OPC (<byte>),y` e.g. `LDA (0x44),y`
* Relative: `OPC <signedByte>` e.g. `BEQ 0x44`

Comments
--------
Comments start with `;` and end with a line break

Constants
---------
* Identifiers of byte constants must start with a **lowercase** letter and are followed by alphanumeric characters or underscores.  
  `maxLives = 0x05`
* Identifiers of Word constants must start with a **uppercase** letter and are followed by alphanumeric characters or underscores.  
  `Character_rom = 0x8000`
  
* Addresses for variables can also be defined as follows:

      @0x00       ;Adresses start from 0x00
      VAR foo     ;foo is set to 0x00
      VAR bar[4]  ;bar is set to 0x01, 4 bytes are reserved instead of just one. bar can be used as an array of lenght 4
      VAR myVar   ;myVar is set to 0x05
      VAR WdAddr  ;WdAddr is set to 0x0006 (This is a Word because of the capital fist letter)
      VAR other   ;other is set to 0x07 (WdAddr itself is a Word, but points to a byte, so only 1 byte gets reserved)

* The high / low byte of a Word can be accessed by using the suffixes `.HI` / `.LO`
    
      MyWord = 0x1234
      LDA #MyWord.HI   ;0x12 gets loaded into accumulator

Code Positions
--------------
* The origin is set via `ORIGIN = <Word>`
* Addresses in code can be set via `@<Word>`. The space from the last code/data to the address is filled with 0x00.
* To add code/data at the start of a page, `@NPAGE` can be used. The rest of the page before is then filled with 0x00

Labels in code/data
-------------------
* Labels can be set via either `==<Label>==` or `<Label>:`. The only difference is, that the assembler makes sure, that in the first case no code 'runs' into the Label and that it can only be reached via jumps/subroutine calls/interrupts. So `==<Label>==` should be used in most cases (the use of brackets make the need for `<Label>:` quite rare)

      ==My_subroutine==
      ;Code
      RTS
* Note that such Labels store an address, which is a Word. Therefore such labels MUST start with an uppercase letter.
     
Brackets
--------
Instead of labels, curly brackets `{ }` can be used for branching. Brackets can be nested but must allways be closed. To branch to brackets, the following keywords can be used:
* `BR` Breaks the current bracket, goes to the `}` that closed the current bracket level.
* `RE` Returns to the `{` that opened the current bracket level.
* `SK` Skips the next bracket group `{  }`
* `BR,BR` breaks 2 bracket levels, `BR,SK` breaks, then skips, `BR,RE` breaks, then returns.
With those elements, most control statements can be created without using labels:

**IF:**

    LDA myBool
    BEQ SK
    {
        ;Code for true
    }
    ;Anyways
    

**IF/ELSE:**

    LDA myBool
    BEQ SK
    {
        ;Code for true
        JMP BR,SK
    }
    {
        ;Code for false
    }
    ;Anyways

**ELSE-IF/SWICH:**

    LDA myNum
    {
        CMP #1
        BNE SK
        {
            ;Code for myNum == 1
            JMP BR,BR
        }
        CMP #2
        BNE SK
        {
            ;Code for myNum == 2
            JMP BR,BR
        }
        ; Code for other cases/ default
    }
    
**FOR-LOOP:**

    LDX iter
    {
        BEQ BR
        ;Code that gets looped <iter> times
        DEX
        JMP RE
    }
  
**WHILE-LOOP:**

    {
        LDA myBool
        BEQ BR
        ;Code
        JMP RE
    }

Numbers
-------
* Hexadecimal numbers are prefixed with `0x` e.g. `0xF0`
* Binary numbers are prefixed with `0b` e.g. `0b11110000`
* Decimal numbers don't need additional prefix, e.g. `240`
* Constants can be used in combination with a plus/minus operator
    
      @0x00               
      VAR foo[2]          ;Could be a vector

      ORIGIN = 0x8000
      ==Init==
      LDA #0x00
      STA foo             ;Write to low byte
      LDA #0x90
      STA foo+1           ;Write to high byte
      ;...


DATA
----
Data can be inserted with the `DATA` keyword:
* `DATA "<string>"` puts decodes an ASCII string and adds the value of the chars. You can add unprintable and non ASCII characters by escaping a hex number. `DATA "FOO\13BAR\00"` will add `0x464F4F1342415200`
* `DATA <byte>` inserts a single byte, e.g. `DATA 0xBA` or `DATA myConst`
* `DATA <Word>` inserts the 2 bytes of a Word in little endian format. So if `Reset_vector = 0x8000`, then `DATA Reset_vector` will produce `0x0080`
* `DATA <relPath>` where `<relPath>` is the path to a hex-file, relative to the assembled file (not the assembler). The hex-file must contain only a hex-string. `DATA Gfx/Sprite.hex` 
