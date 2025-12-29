# Developer Guide: plclib-cl and plclib-cl-web

## Table of Contents
1. [Overview](#overview)
2. [Architecture](#architecture)
3. [plclib-cl Library](#plclib-cl-library)
4. [plclib-cl-web Application](#plclib-cl-web-application)
5. [Key Concepts](#key-concepts)
6. [Code Flow](#code-flow)
7. [Supporting Codebases](#supporting-codebases)
8. [Development Guide](#development-guide)

---

## Overview

These codebases implement a Programmable Logic Controller (PLC) simulator in Common Lisp:

- **plclib-cl**: Core PLC library implementing fundamental PLC operations (timers, counters, I/O, logic)
- **plclib-cl-web**: Web-based interface for programming and monitoring PLCs using Instruction List (IL) programs

### What is a PLC?
A PLC is an industrial computer used to control machinery. It reads inputs (sensors, buttons), executes logic, and controls outputs (motors, lights). PLCs execute programs in continuous scan cycles.

### Supported Programming Language
The web interface supports **IEC 61131-3 Instruction List (IL)**, a low-level assembly-like language for PLCs.

---

## Architecture

```
┌─────────────────────────────────────────────────────┐
│              plclib-cl-web                          │
│  ┌─────────────────────────────────────────────┐  │
│  │         Web Server (Hunchentoot)            │  │
│  │    - REST API                                │  │
│  │    - Static files                            │  │
│  └─────────────────────────────────────────────┘  │
│                      ▼                              │
│  ┌─────────────────────────────────────────────┐  │
│  │      IL Parser & Executor                    │  │
│  │    - Parse IL programs                       │  │
│  │    - Execute instructions                    │  │
│  │    - Convert IL to Ladder Diagram            │  │
│  └─────────────────────────────────────────────┘  │
│                      ▼                              │
│  ┌─────────────────────────────────────────────┐  │
│  │         PLC State Manager                    │  │
│  │    - Inputs/Outputs                          │  │
│  │    - Internal relays                         │  │
│  │    - Execution control                       │  │
│  └─────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────┘
                      ▼
┌─────────────────────────────────────────────────────┐
│                 plclib-cl                           │
│  ┌─────────────────────────────────────────────┐  │
│  │     Core PLC Components                      │  │
│  │  - Timers                                    │  │
│  │  - Counters                                  │  │
│  │  - Pin Manager (I/O)                         │  │
│  │  - Logic Operations                          │  │
│  │  - Stack Operations                          │  │
│  │  - Shift Registers                           │  │
│  │  - Pulse Detection                           │  │
│  └─────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────┘
```

---

## plclib-cl Library

### File Structure

```
plclib-cl/
├── plclib-cl.asd        # System definition
├── package.lisp         # Package exports
├── types.lisp           # Type definitions and global state
├── pin-manager.lisp     # I/O pin management
├── timers.lisp          # Timer functions
├── counters.lisp        # Counter class
├── shift.lisp           # Shift register class
├── stack.lisp           # Stack for boolean operations
├── pulse.lisp           # Edge detection
├── logic.lisp           # Boolean logic operations
├── io.lisp              # Input/output functions
├── plclib.lisp          # Main PLC scan and control
└── examples.lisp        # Usage examples
```

### Core Components

#### 1. Types and Global State (`types.lisp`)

**Purpose**: Define data types and global state variables for the PLC system.

**Key Types**:
```lisp
(deftype pin-number () '(integer 0 255))      ; Pin addresses
(deftype digital-value () '(integer 0 1))    ; Binary values
(deftype analog-value () '(integer 0 1023))  ; 10-bit ADC values
(deftype pin-mode () '(member :input :output :input-pullup))
```

**Key Structures**:
```lisp
(defstruct pin-state
  number              ; Pin number
  mode                ; :input, :output, :input-pullup
  current-value       ; Current pin value
  previous-value      ; Previous value (for edge detection)
  last-update)        ; Timestamp
```

**Global State**:
- `*pin-states*`: Hash table storing all pin states
- `*scan-value*`: Current scan accumulator value
- `*scan-time*`: Current scan time in milliseconds
- `*serial-enabled*`: Enable/disable serial debug output

**Time Function**:
```lisp
(defun get-current-time-ms ())  ; Platform-specific millisecond timer
```

#### 2. Pin Manager (`pin-manager.lisp`)

**Purpose**: Manage I/O pin states and provide hardware abstraction.

**Key Functions**:

```lisp
(defun get-pin-state (pin-number))
  ; Get or create pin state for pin-number
  ; Returns: pin-state structure

(defun set-pin-mode (pin-number mode))
  ; Set pin mode (:input, :output, :input-pullup)

(defun update-pin-value (pin-number value))
  ; Update pin value, save previous value
  ; Automatically tracks state changes for edge detection

(defun read-pin-value (pin-number))
  ; Read current pin value

(defun simulate-digital-read (pin-number))
  ; Simulate reading digital input (0 or 1)

(defun simulate-digital-write (pin-number value))
  ; Simulate writing digital output
```

**How It Works**:
- All pins are stored in `*pin-states*` hash table
- First access to a pin automatically creates its state structure
- Previous values are tracked for edge detection
- In real hardware, this would interface with GPIO drivers

#### 3. Timers (`timers.lisp`)

**Purpose**: Implement PLC timer functions (delays, pulses, cycles).

**Timer Structure**:
```lisp
(defstruct timer-state
  start-time    ; When timer started
  duration      ; Timer duration in ms
  active        ; Is timer currently running?
  output)       ; Current timer output (t/nil)
```

**Timer Functions**:

```lisp
(defun timer-on (input timer-id duration-ms))
  ; ON-DELAY: Output turns ON after delay when input is TRUE
  ; Returns: timer output (t or nil)
  
(defun timer-off (input timer-id duration-ms))
  ; OFF-DELAY: Output turns OFF after delay when input becomes FALSE
  
(defun timer-pulse (input timer-id duration-ms))
  ; PULSE: Output pulses for duration when input triggers
  
(defun timer-cycle (enable timer-id on-duration-ms off-duration-ms))
  ; CYCLE: Oscillate between on/off while enabled
```

**How Timers Work**:
1. Each timer is identified by a unique `timer-id` (any symbol/keyword)
2. Timers stored in `*timers*` hash table
3. On first call, timer state is created
4. Each scan cycle, timer checks elapsed time vs. duration
5. Timer output changes based on type and elapsed time

**Example**:
```lisp
; Wait 2 seconds after button press before turning on light
(timer-on (input 0) :delay-timer 2000)  ; Returns t after 2 seconds
(output 10 (timer-on (input 0) :delay-timer 2000))
```

#### 4. Counters (`counters.lisp`)

**Purpose**: Count events (up/down counters with preset values).

**Counter Class**:
```lisp
(defclass counter ()
  ((count-value ...)         ; Current count
   (preset-value ...)        ; Target count
   (up-input-prev ...)       ; Previous up input (for edge detection)
   (down-input-prev ...)     ; Previous down input
   (reset-input-prev ...)))  ; Previous reset input
```

**Key Methods**:

```lisp
(defun make-counter (&optional preset))
  ; Create counter with preset value

(defmethod counter-up ((counter counter) input))
  ; Increment count on rising edge of input
  ; Returns: t if count >= preset

(defmethod counter-down ((counter counter) input))
  ; Decrement count on rising edge (minimum 0)

(defmethod counter-reset ((counter counter) reset-input))
  ; Reset count to 0 on rising edge

(defmethod counter-done-p ((counter counter)))
  ; Check if count >= preset value

(defmethod counter-value ((counter counter)))
  ; Get current count
```

**How Counters Work**:
1. Create counter instance with preset value
2. Call `counter-up` each scan with input signal
3. Counter detects rising edges (0→1 transition)
4. Count increments only on rising edge
5. `counter-done-p` returns t when count >= preset

**Example**:
```lisp
; Count 10 button presses
(let ((my-counter (make-counter 10)))
  (counter-up my-counter (input 5))
  (when (counter-done-p my-counter)
    (output 10 t)))  ; Turn on light after 10 presses
```

#### 5. Shift Registers (`shift.lisp`)

**Purpose**: Shift binary data left/right (used for sequential control).

**Shift Register Class**:
```lisp
(defclass shift-register ()
  ((bits ...)              ; Bit storage (integer)
   (size ...)              ; Number of bits (1-64)
   (shift-input-prev ...) ; Previous shift input
   (reset-input-prev ...))) ; Previous reset input
```

**Key Methods**:

```lisp
(defun make-shift-register (&optional size))
  ; Create shift register (default 16 bits)

(defmethod shift-left ((shifter ...) shift-input data-input))
  ; Shift bits left, load data-input into LSB
  
(defmethod shift-right ((shifter ...) shift-input data-input))
  ; Shift bits right, load data-input into MSB

(defmethod shift-load ((shifter ...) load-input load-value))
  ; Load entire value into register

(defmethod shift-reset ((shifter ...) reset-input))
  ; Clear all bits to 0
```

**How It Works**:
1. Bits stored as integer (uses bit operations)
2. Shift operations trigger on rising edge of shift-input
3. Data input (t/nil) determines bit shifted in
4. Mask ensures only N bits are used

**Example**:
```lisp
; Create 8-bit shift register for LED chaser
(let ((shifter (make-shift-register 8)))
  (shift-load shifter t #b00000001)  ; Load initial pattern
  (shift-left shifter (input 3) nil) ; Shift left on input 3
  ; Use (shift-bits shifter) to get current pattern
)
```

#### 6. Stack (`stack.lisp`)

**Purpose**: Boolean stack for complex logic expressions (similar to RPN calculator).

**Stack Class**:
```lisp
(defclass stack ()
  ((values ...)     ; List of boolean values
   (max-size ...))) ; Maximum stack depth
```

**Key Methods**:

```lisp
(defun make-stack (&optional max-size))
(defmethod stack-push ((stack ...) value))
(defmethod stack-pop ((stack ...)))
(defmethod stack-and ((stack ...)))  ; Pop 2, push AND result
(defmethod stack-or ((stack ...)))   ; Pop 2, push OR result
(defmethod stack-xor ((stack ...)))  ; Pop 2, push XOR result
(defmethod stack-not ((stack ...)))  ; Pop 1, push NOT result
```

**How It Works**:
1. Used for evaluating complex boolean expressions
2. Push operands onto stack
3. Operations pop operands, push results
4. Similar to Reverse Polish Notation (RPN)

#### 7. Pulse Detection (`pulse.lisp`)

**Purpose**: Detect rising and falling edges of signals.

**Pulse Detector Class**:
```lisp
(defclass pulse-detector ()
  ((previous-input ...)
   (rising-edge-output ...)
   (falling-edge-output ...)))
```

**Key Functions**:

```lisp
(defun make-pulse-detector ())

(defmethod rising-edge ((detector ...) input))
  ; Returns t for one scan when input transitions 0→1
  
(defmethod falling-edge ((detector ...) input))
  ; Returns t for one scan when input transitions 1→0
```

**How It Works**:
1. Stores previous input state
2. Compares current input to previous
3. Detects transitions and outputs pulse for one scan
4. Updates previous state

**Example**:
```lisp
; Toggle output on button press
(let ((detector (make-pulse-detector))
      (toggle-state nil))
  (when (rising-edge detector (input 2))
    (setf toggle-state (not toggle-state))
    (output 10 toggle-state)))
```

#### 8. Logic Operations (`logic.lisp`)

**Purpose**: Boolean logic and comparison functions.

**Boolean Operations**:
```lisp
(defun plc-and (&rest inputs))  ; All inputs true
(defun plc-or (&rest inputs))   ; Any input true
(defun plc-xor (&rest inputs))  ; Odd number of inputs true
(defun plc-not (input))         ; Logical inverse
(defun plc-nand (&rest inputs))
(defun plc-nor (&rest inputs))
```

**Comparison Functions**:
```lisp
(defun compare-eq (val1 val2))   ; Equal
(defun compare-ne (val1 val2))   ; Not equal
(defun compare-gt (val1 val2))   ; Greater than
(defun compare-lt (val1 val2))   ; Less than
(defun compare-ge (val1 val2))   ; Greater or equal
(defun compare-le (val1 val2))   ; Less or equal
```

**Utility Functions**:
```lisp
(defun in-range (value min-val max-val))
(defun scale-linear (input in-min in-max out-min out-max))
(defun hysteresis (input on-threshold off-threshold previous-state))
```

#### 9. I/O Functions (`io.lisp`)

**Purpose**: High-level interface for reading inputs and writing outputs.

**Input Functions**:
```lisp
(defun input (pin-number))
  ; Read digital input (returns 0 or 1)

(defun input-not (pin-number))
  ; Read inverted input (useful for NC contacts)

(defun input-analog (pin-number))
  ; Read analog input (0-1023)
```

**Output Functions**:
```lisp
(defun output (pin-number value))
  ; Write digital output
  ; Also updates *scan-value* global

(defun output-not (pin-number value))
  ; Write inverted output

(defun output-pwm (pin-number value))
  ; Write PWM value (0-255)
```

**Serial Functions**:
```lisp
(defun serial-begin (&optional baud-rate))
  ; Enable serial output for debugging

(defun serial-println (message))
  ; Print debug message

(defun serial-print-value (label value))
  ; Print labeled value for debugging
```

**Utility Functions**:
```lisp
(defun map-analog (value in-min in-max out-min out-max))
  ; Scale analog value to different range

(defun constrain-value (value min-val max-val))
  ; Limit value to range
```

#### 10. Main PLC Control (`plclib.lisp`)

**Purpose**: Main scan cycle and PLC control functions.

**Global State**:
```lisp
*plc-running*     ; Is PLC running?
*scan-count*      ; Number of scans completed
*scan-cycle-time* ; Target scan time (ms)
*max-scan-time*   ; Max scan duration
*min-scan-time*   ; Min scan duration
*avg-scan-time*   ; Average scan duration
```

**Key Functions**:

```lisp
(defun plc-init ())
  ; Initialize PLC system
  ; - Reset all state
  ; - Clear pin states
  ; - Clear timers
  ; - Reset counters

(defun plc-scan ())
  ; Execute one scan cycle
  ; - Update scan time
  ; - Increment scan count
  ; - Calculate scan statistics
  ; Returns: scan statistics plist

(defun plc-start ())
  ; Start PLC operation (sets *plc-running* to t)

(defun plc-stop ())
  ; Stop PLC operation

(defun get-plc-status ())
  ; Get comprehensive status information
  ; Returns plist with scan times, pin count, etc.
```

**Scan Cycle**:
1. Record start time
2. Update `*scan-time*`
3. Increment `*scan-count*`
4. User program executes (between scans)
5. Calculate scan duration
6. Update statistics (min/max/avg scan time)

**Helper Macros**:
```lisp
(defmacro plc-rung (condition &body outputs))
  ; Define ladder logic rung
  ; Example: (plc-rung (input 0) (output 10 t))

(defmacro contact-no (pin))     ; Normally open contact
(defmacro contact-nc (pin))     ; Normally closed contact
(defmacro coil (pin value))     ; Output coil
```

---

## plclib-cl-web Application

### File Structure

```
plclib-cl-web/
├── plclib-cl-web.asd       # System definition
├── package.lisp            # Package exports
├── start.lisp              # Startup script
└── src/
    ├── plc-state.lisp      # PLC state management
    ├── il-parser.lisp      # IL instruction parser
    ├── il-executor.lisp    # IL instruction executor
    ├── il-to-ld.lisp       # IL to Ladder Diagram converter
    ├── ld-renderer.lisp    # SVG renderer for ladder diagrams
    ├── file-ops.lisp       # File I/O operations
    └── web-server.lisp     # Web server and API
```

### Component Details

#### 1. PLC State Manager (`plc-state.lisp`)

**Purpose**: Manage PLC state variables and provide thread-safe access.

**Global State**:
```lisp
*inputs*           ; Array[16] of digital inputs X0-X15
*outputs*          ; Array[16] of digital outputs Y0-Y15
*relays*           ; Hash table of internal relays M0-M99
*current-program*  ; IL source code
*parsed-program*   ; Parsed instruction list
*execution-state*  ; :stopped, :running, :single-step
*scan-count*       ; Scan cycle counter
*state-lock*       ; Thread synchronization lock
```

**Key Functions**:

```lisp
(defun initialize-plc ())
  ; Initialize PLC system
  ; - Call plclib-cl:plc-init
  ; - Set up 16 inputs (pins 0-15)
  ; - Set up 16 outputs (pins 100-115)
  ; - Clear all state

(defun get-input (index))
  ; Get input X0-X15 value (0 or 1)

(defun set-input (index value))
  ; Set input X0-X15
  ; Also updates plclib-cl pin simulation

(defun get-output (index))
  ; Get output Y0-Y15 value

(defun set-output (index value))
  ; Set output Y0-Y15
  ; Maps to plclib-cl pins 100-115

(defun get-relay (name))
  ; Get internal relay M<name>
  ; Example: (get-relay "M5")

(defun set-relay (name value))
  ; Set internal relay

(defun toggle-input (index))
  ; Toggle input between 0 and 1

(defun get-plc-state ())
  ; Get complete state as plist
  ; Returns: inputs, outputs, relays, execution-state, scan-count
```

**Thread Safety**:
- All state modifications use `bt:with-lock-held`
- Prevents race conditions during continuous execution
- Uses bordeaux-threads for cross-platform compatibility

#### 2. IL Parser (`il-parser.lisp`)

**Purpose**: Parse IEC 61131-3 Instruction List programs into data structures.

**Instruction Structure**:
```lisp
(:line <line-number>
 :opcode <opcode-keyword>
 :operand <operand-string>
 :device <device-cons>)
```

**Device Format**:
```lisp
; Device is cons: (type . number)
; Examples:
;   "X0" → (#\X . 0)
;   "Y5" → (#\Y . 5)
;   "M10" → (#\M . 10)
```

**Key Functions**:

```lisp
(defun parse-device (device-str))
  ; Parse "X0" into (#\X . 0)
  
(defun parse-il-line (line line-number))
  ; Parse single IL instruction
  ; Returns: instruction structure or NIL (for empty/comments)
  
(defun parse-il (program-text))
  ; Parse entire IL program
  ; Returns: list of instruction structures
  ; Signals error on parse failure
```

**Supported Opcodes**:
```lisp
*valid-opcodes* =
  '(:LD :LDI      ; Load (normal/inverted)
    :ST :STN      ; Store (normal/inverted)
    :AND :ANDN    ; AND (normal/inverted)
    :OR :ORN      ; OR (normal/inverted)
    :XOR :XORN    ; XOR (normal/inverted)
    :OUT          ; Output to device
    :SET :RST     ; Set/Reset (latching)
    :MPS :MRD :MPP ; Stack operations
    :ANB :ORB)    ; Block AND/OR
```

**IL Syntax**:
```
; Comment lines start with semicolon
LD X0         ; Load input X0
AND X1        ; AND with input X1
OUT Y0        ; Output to Y0

LDI X2        ; Load inverted X2 (normally closed)
SET M5        ; Set relay M5
```

#### 3. IL Executor (`il-executor.lisp`)

**Purpose**: Execute parsed IL instructions on the PLC state.

**Execution State**:
```lisp
*accumulator*    ; Current result (0 or 1)
*branch-stack*   ; Stack for MPS/MRD/MPP operations
```

**Key Functions**:

```lisp
(defun get-device-value (device))
  ; Get current value of X, Y, M, T, C device
  
(defun set-device-value (device value))
  ; Set value of Y, M, T, C device
  
(defun execute-instruction (instruction))
  ; Execute single IL instruction
  ; Updates *accumulator* and device states
  
(defun execute-program-scan ())
  ; Execute one complete scan of program
  ; 1. Reset accumulator and stack
  ; 2. Execute all instructions in sequence
  ; 3. Increment scan count
  ; 4. Call plclib-cl:plc-scan
```

**Instruction Execution Details**:

```lisp
; LD/LDI - Load device into accumulator
(defun execute-ld (device inverted))
  ; *accumulator* = device-value (inverted if LDI)

; AND/ANDN - AND accumulator with device
(defun execute-and (device inverted))
  ; *accumulator* = *accumulator* AND device-value

; OR/ORN - OR accumulator with device
(defun execute-or (device inverted))
  ; *accumulator* = *accumulator* OR device-value

; OUT - Output accumulator to device
(defun execute-out (device))
  ; device-value = *accumulator*

; SET/RST - Latch/unlatch device
(defun execute-set (device))
  ; If accumulator=1, set device=1 (latched)
  
(defun execute-rst (device))
  ; If accumulator=1, reset device=0

; MPS/MRD/MPP - Stack operations for branches
(defun execute-mps ())  ; Push accumulator
(defun execute-mrd ())  ; Read (peek) stack
(defun execute-mpp ())  ; Pop stack

; ANB/ORB - Block operations
(defun execute-anb ())  ; AND accumulator with popped value
(defun execute-orb ())  ; OR accumulator with popped value
```

**Continuous Execution**:

```lisp
(defun start-continuous-execution ())
  ; Start background thread
  ; Loop: execute scan, sleep 10ms
  
(defun stop-continuous-execution ())
  ; Stop background thread gracefully
  
(defun execute-single-step ())
  ; Execute one scan cycle only
```

#### 4. IL to Ladder Diagram Converter (`il-to-ld.lisp`)

**Purpose**: Convert IL instructions into ladder diagram representation.

**Data Structures**:

```lisp
(defstruct ld-element
  type     ; :contact-no, :contact-nc, :coil, :set, :reset
  device   ; "X0", "Y5", "M10"
  row      ; Row position in rung
  col)     ; Column position in rung

(defstruct ld-rung
  number    ; Rung number (1, 2, 3...)
  elements  ; List of ld-element
  branches) ; Has parallel branches?
```

**Key Functions**:

```lisp
(defun split-into-rungs (instructions))
  ; Split IL into rungs (sequences ending with OUT/SET/RST)
  ; Returns: list of instruction lists

(defun convert-rung-to-ld (rung-instructions rung-number))
  ; Convert one rung's IL to ladder elements
  ; Returns: ld-rung structure

(defun il-to-ladder (instructions))
  ; Convert entire IL program to ladder diagram
  ; Returns: list of ld-rung structures
```

**Conversion Logic**:

```
IL Instruction    →    Ladder Element
─────────────────────────────────────
LD X0             →    ──] [── (NO contact, X0)
LDI X1            →    ──]/[── (NC contact, X1)
AND X2            →    ──] [── (series NO contact, X2)
OR X3             →    ──] [── (parallel NO contact, X3)
OUT Y0            →    ──( )── (coil, Y0)
SET M5            →    ──(S)── (set coil, M5)
RST M5            →    ──(R)── (reset coil, M5)

MPS/MRD/MPP handle parallel branches
ANB/ORB merge parallel branches
```

**Branch Handling**:
1. `MPS` saves position for branch start
2. New row created for parallel path
3. `MRD` reads saved position (peek)
4. `MPP` restores position (pop)
5. `ANB`/`ORB` merge branches back together

#### 5. Ladder Diagram Renderer (`ld-renderer.lisp`)

**Purpose**: Render ladder diagrams as SVG graphics.

**Rendering Parameters**:
```lisp
*element-width*  = 80    ; Pixels per element column
*element-height* = 50    ; Pixels per element row
*rail-margin*    = 20    ; Space for power rails
*rung-spacing*   = 80    ; Vertical space between rungs
*contact-width*  = 60    ; Contact symbol width
*coil-width*     = 60    ; Coil symbol width
```

**Key Functions**:

```lisp
(defun get-device-state (device-str))
  ; Get current state (0/1) for color coding
  
(defun element-color (device-str element-type))
  ; Return "red" if energized, "black" if not
  ; NO contact: red when closed (1)
  ; NC contact: red when open (0)
  
(defun render-contact-no (x y device))
  ; Render --] [-- symbol
  
(defun render-contact-nc (x y device))
  ; Render --]/[-- symbol with diagonal
  
(defun render-coil (x y device))
  ; Render ( ) symbol
  
(defun render-set-coil (x y device))
  ; Render (S) symbol
  
(defun render-reset-coil (x y device))
  ; Render (R) symbol

(defun render-rung (rung start-y rail-left rail-right))
  ; Render complete rung with all elements
  ; Handles parallel branches with vertical connections
  
(defun render-ladder-svg (rungs))
  ; Render complete ladder diagram
  ; Returns: SVG string
```

**SVG Output Structure**:
```xml
<svg width="800" height="...">
  <!-- Left power rail (blue vertical line) -->
  <!-- Right power rail (blue vertical line) -->
  
  <!-- Each rung: -->
  <text>Rung number</text>
  <line>Horizontal connection to left rail</line>
  <!-- Contacts and coils -->
  <line>Horizontal connection to right rail</line>
  
  <!-- Parallel branches have vertical lines -->
</svg>
```

**Color Coding**:
- Black: De-energized, not conducting
- Red: Energized, conducting current
- Blue: Power rails (always on)

#### 6. File Operations (`file-ops.lisp`)

**Purpose**: Load and save IL programs from/to files.

**Key Functions**:

```lisp
(defun load-program-from-file (filepath))
  ; Read IL program from file
  ; Returns: program text string
  
(defun save-program-to-file (filepath program-text))
  ; Write IL program to file
  
(defun load-program (filepath))
  ; Load, parse, and set as current program
  ; Validates syntax
  
(defun save-program (filepath))
  ; Save current program to file
```

#### 7. Web Server (`web-server.lisp`)

**Purpose**: HTTP server providing REST API and web interface.

**Server State**:
```lisp
*server*      ; Hunchentoot server instance
*server-port* ; Default: 8080
```

**API Endpoints**:

```
GET  /                    → HTML interface
POST /api/load-program    → Load IL program
GET  /api/state           → Get PLC state (inputs, outputs, scan count)
POST /api/toggle-input    → Toggle input X0-X15
POST /api/run             → Start continuous execution
POST /api/stop            → Stop execution
POST /api/step            → Execute single scan
POST /api/reset           → Reset PLC to initial state
GET  /api/ladder          → Get ladder diagram SVG
GET  /api/program         → Get current program text
POST /api/save-program    → Save program to file
GET  /api/download        → Download program as file
POST /api/upload          → Upload program file
GET  /api/examples        → List example programs
GET  /api/load-example    → Load example program
```

**Key Handler Functions**:

```lisp
(defun start-server (&optional port))
  ; 1. Initialize PLC
  ; 2. Setup static file serving
  ; 3. Start Hunchentoot server
  ; 4. Print access URL
  
(defun stop-server ())
  ; 1. Stop continuous execution
  ; 2. Stop Hunchentoot server
```

**JSON Response Format**:
```json
{
  "success": true,
  "message": "...",
  "data": { ... }
}

// Or on error:
{
  "success": false,
  "error": "Error message"
}
```

---

## Key Concepts

### PLC Scan Cycle

A PLC operates in continuous scan cycles:

```
1. Read Inputs     → Read all input values from hardware
2. Execute Logic   → Process ladder logic/IL program
3. Update Outputs  → Write all output values to hardware
4. Repeat          → Go back to step 1
```

In our simulator:
- Inputs are simulated via web interface
- Logic executes IL instructions sequentially
- Outputs update simulated state
- Scan repeats every ~10ms when running

### IL (Instruction List) Programming

IL is a stack-based language similar to assembly:

```
LD X0      ; Load input X0 into accumulator
AND X1     ; AND accumulator with X1
OUT Y0     ; Output accumulator to Y0

; This implements: Y0 = X0 AND X1
```

**Accumulator**: Holds current boolean result (0 or 1)

**Common Patterns**:

```
; Simple output
LD X0
OUT Y0

; AND logic (series contacts)
LD X0
AND X1
AND X2
OUT Y0

; OR logic (parallel contacts)
LD X0
OR X1
OR X2
OUT Y0

; Complex logic with branches
LD X0       ; Load X0
AND X1      ; AND with X1
MPS         ; Push result (save for later)
AND X2      ; AND with X2
OUT Y0      ; First output
MRD         ; Read saved result (peek)
AND X3      ; AND with X3
OUT Y1      ; Second output
MPP         ; Pop saved result (discard)
```

### Ladder Diagram Representation

Ladder diagrams are graphical representations of logic:

```
    X0      X1        Y0
─┬──] [────] [────────( )──┬─
 │                         │
 └─────────────────────────┘
 
Meaning: Y0 = X0 AND X1
```

**Elements**:
- `] [` - Normally Open contact (true when input is 1)
- `]/[` - Normally Closed contact (true when input is 0)
- `( )` - Output coil
- `(S)` - Set coil (latching on)
- `(R)` - Reset coil (latching off)

**Series = AND**, **Parallel = OR**

---

## Code Flow

### Starting the Web Server

```
1. User runs: sbcl --load start.lisp
   ↓
2. start.lisp loads plclib-cl-web system
   ↓
3. Calls (plclib-cl-web:start-server)
   ↓
4. start-server:
   - Calls initialize-plc
   - Sets up static file serving
   - Starts Hunchentoot on port 8080
   ↓
5. Server ready at http://localhost:8080
```

### Loading and Running a Program

```
1. User enters IL program in web interface
   ↓
2. POST /api/load-program with program text
   ↓
3. set-program stores text
   ↓
4. parse-and-validate-il parses program
   ↓
5. set-parsed-program stores instructions
   ↓
6. User clicks "Run"
   ↓
7. POST /api/run
   ↓
8. start-continuous-execution:
   - Creates background thread
   - Loop:
     - execute-program-scan
     - sleep 10ms
   ↓
9. Each scan:
   - Reset accumulator and stack
   - Execute each instruction
   - Update outputs
   - Increment scan count
```

### Executing IL Instructions

```
Program:
LD X0
AND X1
OUT Y0

Execution flow:
1. execute-ld:
   - Get value of X0 (e.g., 1)
   - *accumulator* = 1

2. execute-and:
   - Get value of X1 (e.g., 1)
   - *accumulator* = 1 AND 1 = 1

3. execute-out:
   - Set Y0 = *accumulator* (1)
   - Y0 turns on
```

### Rendering Ladder Diagram

```
1. User views Ladder tab
   ↓
2. Browser requests GET /api/ladder
   ↓
3. render-current-program-svg:
   - get-ladder-diagram
   - il-to-ladder (convert IL to LD structure)
   - render-ladder-svg (generate SVG)
   ↓
4. SVG sent to browser
   ↓
5. Browser displays diagram with colors:
   - Red = energized
   - Black = de-energized
```

---

## Supporting Codebases

### Testing and Validation

To test changes:

```lisp
; Load system
(ql:quickload :plclib-cl)
(ql:quickload :plclib-cl-web)

; Test core library
(plclib-cl:run-all-examples)

; Test specific component
(plclib-cl:example-timers)

; Start web interface
(plclib-cl-web:start-server)

; Test manually via browser
; http://localhost:8080
```

### Example Programs

See `plclib-cl/examples.lisp` for usage examples:
- Basic I/O
- Timers
- Counters
- Shift registers
- Edge detection
- Complete PLC programs

---

## Development Guide

### Adding a New IL Instruction

1. **Add to valid opcodes** (`il-parser.lisp`):
```lisp
(defparameter *valid-opcodes*
  '(:LD :LDI ... :YOUR-NEW-OPCODE))
```

2. **Add parser support** (if needed) (`il-parser.lisp`):
```lisp
; Usually no changes needed unless special syntax
```

3. **Add executor** (`il-executor.lisp`):
```lisp
(defun execute-your-opcode (device)
  "Execute YOUR-NEW-OPCODE instruction"
  ; Your logic here
  ...)

; Add to execute-instruction case statement:
(case opcode
  ...
  (:YOUR-NEW-OPCODE (execute-your-opcode device))
  ...)
```

4. **Add ladder conversion** (`il-to-ld.lisp`):
```lisp
; Add case in convert-rung-to-ld:
(case opcode
  ...
  (:YOUR-NEW-OPCODE
   (push (make-ld-element
          :type :your-element-type
          :device (device-to-string device)
          :row row
          :col col)
         elements))
  ...)
```

5. **Add renderer** (if new element type) (`ld-renderer.lisp`):
```lisp
(defun render-your-element (x y device)
  "Render your custom element"
  ; Generate SVG
  ...)

; Add to render-element case:
(case (ld-element-type element)
  ...
  (:your-element-type (render-your-element x y device))
  ...)
```

### Adding a New plclib-cl Function

1. **Define function** (appropriate file):
```lisp
(defun your-new-function (args)
  "Documentation"
  ; Implementation
  ...)
```

2. **Export symbol** (`package.lisp`):
```lisp
(:export
 ...
 #:your-new-function)
```

3. **Add example** (`examples.lisp`):
```lisp
(defun example-your-feature ()
  "Demonstrate your new feature"
  ...)
```

4. **Update documentation** (this file):
- Add to relevant section
- Explain purpose and usage

### Adding a Web API Endpoint

1. **Define handler** (`web-server.lisp`):
```lisp
(hunchentoot:define-easy-handler (your-handler :uri "/api/your-endpoint")
    (param1 param2)
  (handler-case
      (progn
        ; Your logic
        (json-success (list (cons :result result))))
    (error (e)
      (json-error (format nil "~A" e)))))
```

2. **Test endpoint**:
```bash
curl http://localhost:8080/api/your-endpoint?param1=value
```

### Debugging Tips

**Enable serial output**:
```lisp
(plclib-cl:serial-begin)
; Now all serial-println calls will output
```

**Check PLC state**:
```lisp
(plclib-cl:get-plc-status)
(plclib-cl:print-plc-status)
```

**Inspect web state**:
```lisp
(plclib-cl-web::get-plc-state)
(plclib-cl-web::get-execution-state)
```

**Common issues**:
- Thread lock deadlock: Check for nested `bt:with-lock-held`
- Parse errors: Use `cl-ppcre:split` debugging
- Execution errors: Add `format t` debugging in execute functions

### Code Style Guidelines

**Naming**:
- Use kebab-case: `my-function-name`
- Predicates end in `-p`: `counter-done-p`
- Global variables use earmuffs: `*global-var*`
- Classes use nouns: `pulse-detector`, `counter`

**Documentation**:
- Add docstrings to all functions
- Explain parameters and return values
- Include usage examples for complex functions

**Error Handling**:
- Use `handler-case` for recoverable errors
- Signal meaningful error messages
- Don't swallow errors silently

**Threading**:
- Always use `bt:with-lock-held` for shared state
- Test multithreaded scenarios
- Avoid holding locks during I/O

---

## Conclusion

This guide provides a foundation for understanding and maintaining the plclib-cl and plclib-cl-web codebases. Key takeaways:

1. **plclib-cl** = Core PLC library with timers, counters, I/O
2. **plclib-cl-web** = Web interface for IL programming
3. **Scan cycle** = Continuous read-execute-write loop
4. **IL** = Stack-based instruction list language
5. **Ladder diagram** = Graphical representation of logic

For more details, read the source code with this guide as a reference. The code is well-structured and follows Common Lisp conventions.

Happy hacking! 🚀
