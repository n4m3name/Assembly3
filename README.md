# Control-Assembly

Assignment 3 of CSC230 - Intro to Computer Architecture at UVic, written in AVR Assembly

An AVR assembly project implementing an interactive LCD display controller with button-based input handling and multiple display modes.

## Features

- Real-time button state detection and debouncing
- Multi-mode LCD display output:
  - Button state indication
  - Character selection and editing
  - Position tracking
- Timer-based interrupt handling
- PWM-based timing control
- Custom character set manipulation

## Implementation Details

**Core Components**
- Timer-based interrupt handlers
- ADC button input processing
- LCD display control
- Character set management

**Display Modes**
- Button state display (R/U/D/L indicators)
- Character editing with up/down navigation
- Position control with left/right movement
- Dynamic character set selection

## Technical Specifications

**Hardware Requirements**
- ATmega2560 microcontroller
- LCD display module
- Button interface shield
- 16MHz clock frequency

**Memory Usage**
- Program memory: Character sets and control logic
- Data memory: Button states and display buffers
- SRAM: Runtime variables and display content

## Button Controls

| Button | Function |
|--------|----------|
| Up | Increment character value |
| Down | Decrement character value |
| Left | Move cursor left |
| Right | Move cursor right |

## Timer Configuration

- Timer1: Button sampling (10ms intervals)
- Timer3: LCD display updates (100ms intervals)
- Timer4: Character set updates (500ms intervals)

## Build Instructions

1. Compile using AVR-GCC:
```bash
avr-gcc -mmcu=atmega2560 -DF_CPU=16000000UL -O2 -o main.elf main.asm
```

2. Create hex file:
```bash
avr-objcopy -O ihex main.elf main.hex
```

3. Flash to microcontroller:
```bash
avrdude -p m2560 -c [programmer] -U flash:w:main.hex
```

## Authors

- Original: Mike Zastre
- Modified: Evan Strasdin

Based on AVR ATmega2560 architecture and LCD shield interface.
