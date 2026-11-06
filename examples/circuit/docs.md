
# Logic / Math

- Not,And,Or,Nand,Xor,Nor,Xnor have bit/word varients that behave as expected. There are also 3bit versions of And/Or. 
- Inc adds 1 to the input
- Neg is Inc(Not(input))
- Add (word. extra carry bit in/out), Full_adder (bit)
- Mul
- Div,Mod are unsigned
- Equal, Less_u (unsigned), Less_s (signed)
- Lsl/Lsr (logical shift left/right) shift in zeros
- Asr (arithmetic shift right) shifts in copies of the sign bit
- Rol/Ror (roll left/right) shift in the bits that were shifted out the other side
- Mux chooses between two values. inputs are (C, F, T) and output is (C ? T : F)
- Constant outputs a fixed value. (Off,On are 1bit 0,1 respectively)
- Clz/Ctz (count leading/trailing zeroes)

> v1: seperate 8/16/32/64 bit versions of all the components.  
> v1: DivMod are fused in one component.  
> v1: Mul also outputs the high half of a double width multiplication.  

## Switch_word

The inputs are (Value, Condition) (on the left,bottom). 
When Condition=1, output=Value. When Condition=0, output=Z (different from zero). 
Logically equivilent to and-ing the Value with the Condition bit (spread out to a mask of the right size). 
Z coerces to zero when passed to the input pins of most components. 
However, it is NOT an error to have seperate components output both Z and 1 to the same wire at the same time (the wire will get the value 1). 
This is convenient when you want to have different areas of your circuit compute values 
and later choose only one of them to be used somewhere else. 
You can have them all go into switches that output to the same wire and only enable one switch at a time. 
This usage is logically equivilent to a chain of Mux but might allow neater wiring. 
Many components with disable pins output Z when disabled. 

> there is also a Switch_bit

## Static_indexer

Performs a logical shift and a mask by a configured Size/Shift. 
Negative Shift shifts left, positive Shift shifts right. 
After the shift, the output is truncated down to Size bits.

> v1: IndexerBit/IndexerByte are more limited versions of Static_indexer

## Multi-Bit

Splitter,Maker,Concatenator all come in N=2/4/8 varients. 
- Splitter splits one (`n*N`)-bit input into N seperate n-bit outputs. 
- Maker combines N n-bit inputs into one (`n*N`)-bit output. 
- Concatenator combines N inputs (of different widths) into one output (the sum of the input widths). 

## Decoder

There are 1/2/3 bit varieties with 2/4/8 output pins respectively. 
The input is the index of one output pin to be 1, the other outputs are 0. 
The input is one wire of width 1/2/3 based on the size of the decoder. 
The 3bit decoder has an extra Disable input pin. All outputs are 0 when Disable=1. 

> v1: 2/3 bit decoders have multiple 1bit input pins

# Auto Width

The logical/math components can infer their width based on (the maximum of) the widths of their input wires. 
- When a small wire is used in an input pin expecting a larger value, the extra high bits are treated as zero. 
- When a large wire is used in an input pin expecting a smaller value, the high bits are truncated/ignored. 
  - for example, bit pins will treat odd inputs as 1 and even as 0.

## Static_eval

Does a compile time computation on the widths of wires. 
The widths of the two inputs are available in the expression as variables A and B. 
The two outputs are the result as a value (with width 64) and the result as a width (with value 0). 

## Static_value

Adds a numeric setting to the outside of the enclosing CC. 
The setting can be different on each instantiation of the CC. 
The two outputs are the result as a value (with width 64) and the result as a width (with value 0). 

## Static_indexer_config 

Like the Static_indexer except that the Size/Shift are input as wire widths (top left and rop right respectively) instead of as settings. 
This allows Size/Shift to vary on each instantiation of the CC. 
Note that Size/Shift are the widths of the wires not the runtime values on the wires. 

# Memory

## Ram_v2

Stores a large number of bytes. 
The ram itself has no pins, loads/stores are done by linked ports stacked on top of the ram. 
Ports execute in order from the top down. 
Addresses wrap around if they overflow the size of the ram. 
The ram's initial data can be all zeroes or manually specified with a (link_id).bin file. 
Endianness (big,little) of all connected ports are controlled by a setting. 

## Load_port

The inputs are (Enable, Address). 
When Enable=1, output is the value previously stored at Address in the linked ram. 
Bit width (8,16,32,64) is controlled by a setting.
Placed on top of a ram (or other port).

## Store_port

The inputs are (Enable, Address, Value). 
When Enable=1, Value is stored at Address in the linked ram. 
Bit width (8,16,32,64) is controlled by a setting.
Placed on top of a ram (or other port). 

## Register_word

The input pins are (Write, Value). When Write=1, Value is stored in the register. 
The output is the stored value. 

> there is also a Register_bit
> v1: there's an extra input pin that enables outputting the value

## Counter

Like Register_word except that when the Write pin isn't set,
the stored value is incremented by 1 each tick. 

## Delay_line_word

Input is stored and used as the output on the next tick. Outputs 0 on the first tick of the simulation. 

> there is also a Delay_line_bit

## Legacy Rams (v1)

- Ram/RamFast (Load, Save, Address, value x4)
  - when width is (8,16,32,64) only one of the value/output pins is used.
    width 128/256 uses the extra pins to access 2/4 consecutive 64 bit values.
- RamDualLoad (Load, Save, Address, Value, Load2, Address2) 
  - like Ram but can do an extra load at a different address. 
- Program (Address)
  - loads 4 consecutive values. initial data is set by assembling a program. 
- Rom (Load, Save, Address, Value)
  - non-zero initial data. 
- FileLoader (Enable, Offset)
  - outputs 8 bytes (little endian) from a selected file at the input byte Offset. 
    (the whole file is read into memory once when the simulation starts) 
  - Offset=MAX_u64 (all 1s) outputs the length of the file instead. 

# Output

## Segment_display

Each bit of the input toggles one segment (7 lines of the number plus a dot). 
Like how old clocks would show numbers. 

> The v1 SegmentDisplay has a setting to choose colour (v2 is always white) and an Enable pin (nothing is displayed when Enable=0) 

## Screen

Displays graphics based on the contents of a linked Ram_v2 component. 
The input pins are (Enable, Setting, Value) (starting at the top). 
The screen always shows graphics, the Enable pin controls whether to write settings this tick. 
When Enable=1, the Setting pin chooses which setting to change and the Value pin is the new value for that setting. 

- Setting=0; mode (Ascii8=0, Ascii32=1, Pixel8=2, Pixel32=3)
- Setting=1; 64bit offset (in bytes) in the linked memory to start reading from
- Setting=2;
  - Pixel8/Pixel32; 8bit resolution. the screen is 4:3 scaled by resolution+1. ex. the max resolution=255 is 1024 pixels wide and 768 pixels tall. 
  - Ascii8; 8bit foreground colour
- Setting=3;
  - Ascii8; 8bit background colour
- Setting=4;
  - Ascii8/Ascii32; font (Not Yet Implemented)

How the data in memory is interpreted depends on the mode. 
- Ascii8; each byte is an ascii character on a 96x40 grid. foreground/background are chosen globally with the settings pins. 
- Ascii32; each character is 4 bytes (padding, background, foreground, character) on a 96x40 grid
- Pixel8; each byte is an 8bit colour for one pixel
- Pixel32; each pixel is 4 bytes (padding, red, green, blue). (blue is least significant). 

8bit colours have (3, 3, 2) bits for (red, green, blue) respectively (blue is least significant). 

## Halt

When input=1, the simulation stops with a configurable message. 

## Probe_wire_bit

The input controls the colour of the probe (0=red, 1=green). 
When a probe is inside a CC, it can be seen from the outside (position is the same 1/8 scale as io pins). 

> there's also Probe_wire_word which i dont render specially yet

## DotMatrixDisplay (v1)

Shows a 6x8 grid of pixels.
The inputs are top:(EnableX, Pixels) and left:(EnableY, Colour).
If either EnableX=0 or EnableY=0 no changes are made. 
Otherwise, Pixels is a bit mask choosing which pixels to set to Colour (others keep their old colour). 
The middle 6 bytes of Pixel are the mask, each bit controls one pixel. 
There is a setting to choose between row/column major indexing of the pixels in the bit mask. 
Colour is 4 bytes (R, G, B, padding). 
Changes are cached until the most significant byte of Pixels is 1 (then the new state is displayed). 
This avoids visual tearing if your updates are spread over multiple frames. 
Since the displays are small you will likely want to tile them to show larger images. 

## Console (v1)

Displays 80x24 characters based on the contents of a linked ram (legacy/v1) component. 
The input is offset (measured in characters) to skip in the ram. 

There's a setting to toggle colour mode. 
- Normal; each character is 1 ascii byte. white text on black background. 
- Colour; each character is 8 bytes (padding, RGB background, RGB foreground, character)

## SpriteDisplay (v1)

# Input

## Keyboard

Stores a queue of keyboard events (press cmd+shift+k to toggle capture). 
The top output is 1 when there is an event in the queue available to be read. 
The other two outputs are (KeyDown bit, KeyValue byte) of the current event. 
When the input is 1, the queue will move to the next event. 

How keys map to values is controlled by a setting. 
In character mode, ascii keys will be the corisponding value. 
In position mode, the high 4 bits are the row and the low 4 bits are the column. 

> v1: keyboard outputs KeyUp instead of KeyDown and the layout of position mode is different. 

## Push_button

Left clicking on the button causes it to output 1 (for as long as its held down). 

## Time

Outputs the number of nanoseconds since the unix epoch (1970 Jan 1).

> v1: Timing component outputs in microseconds and has an enable pin.

# IO 

## CC 

- Cc_input
- Cc_output is bidirectional. Width is always automatically inferred. 

inner components will recalculate their auto widths based on the io wire widths at each usage site. 


> v1: unidirectional Output and seperate Bidirectional pins. 
> v1: Output coerces Z to 0 and OutputZ has an extra enable pin and outputs Z when enable=0. 

## Arch 

Level_output_switched, Level_input_switched

# Notes About The Editor

- Pins are at the ends of green(input) and blue(output) lines coming out of components. 
- You can't have multiple pins in the same cell. Place them 1 apart and connect them with a wire. 
- Only the large nodes on the wires transmit data. The middle segments of different wires can overlap freely. 
- For components with non-commutative pins, hover a pin to show its label.

While the simulation is running:
- you can hover a pin to see its value (only if it's connected to a wire). 
- you can hold shift to draw a line between a graphical component and its entry in the code view on the right
