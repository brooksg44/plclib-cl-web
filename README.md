# PLC Simulator - Web-Based IL/LD

A web-based PLC simulator supporting IEC 61131-3 Instruction List (IL) programming with automatic conversion to Ladder Diagram (LD) display. Built with Common Lisp using the plclib-cl engine.

## Features

- **IL Programming**: Full support for IEC 61131-3 IL instruction subset
- **Ladder Diagram Display**: Automatic conversion and SVG rendering in LDMicro style
- **16 I/O Support**: 16 digital inputs (X0-X15) and 16 digital outputs (Y0-Y15)
- **Run Modes**: Continuous Run and Single Step execution
- **Web Interface**: Clean, responsive web UI using Hunchentoot
- **Real-time Updates**: Live I/O monitoring during execution
- **Interactive Controls**: Toggle inputs, monitor outputs in real-time

## Supported IL Instructions

### Basic Instructions
- `LD device` - Load normally open contact
- `LDI device` - Load normally closed contact
- `AND device` - AND with normally open contact
- `ANDN device` - AND with normally closed contact
- `OR device` - OR with normally open contact
- `ORN device` - OR with normally closed contact
- `XOR device` - XOR with normally open contact
- `XORN device` - XOR with normally closed contact

### Output Instructions
- `OUT device` - Output result to device
- `SET device` - Set device (latching on)
- `RST device` - Reset device (latching off)

### Branch Instructions
- `MPS` - Memory push (save accumulator to stack)
- `MRD` - Memory read (read top of stack)
- `MPP` - Memory pop (restore from stack)
- `ANB` - AND block (combine parallel branches with AND)
- `ORB` - OR block (combine parallel branches with OR)

### Device Types
- `X0-X15` - Digital inputs
- `Y0-Y15` - Digital outputs
- `M0-M99` - Internal relays

## Installation

### Prerequisites
- SBCL or other Common Lisp implementation
- Quicklisp
- plclib-cl library (should be in ~/common-lisp/plclib-cl)

### Dependencies
The following dependencies will be automatically loaded by Quicklisp:
- hunchentoot (web server)
- cl-ppcre (regex parsing)
- cl-json (JSON handling)
- bordeaux-threads (threading)

### Setup

1. The project is already in your Common Lisp systems directory:
   ```
   ~/common-lisp/plclib-cl-web/
   ```

2. Start your Common Lisp REPL (e.g., SBCL):
   ```bash
   sbcl
   ```

3. Load the system:
   ```lisp
   (ql:quickload :plclib-cl-web)
   ```

4. Start the web server:
   ```lisp
   (plclib-cl-web:start-server)
   ```

5. Open your web browser to:
   ```
   http://localhost:8080
   ```

## Usage

### Writing IL Programs

Enter your IL program in the left panel editor. Example:

```il
; Simple AND logic
LD X0
AND X1
OUT Y0

; OR logic with memory
LD X2
OR M0
OUT M0
OUT Y1

; Start/Stop circuit
LD X3      ; Start button
OR M1      ; Self-latch
ANDN X4    ; Stop button
OUT M1
OUT Y2
```

### Loading a Program

#### Option 1: Type Directly
1. Type or paste your IL program in the editor
2. Click "Load Program"
3. The ladder diagram will automatically appear in the right panel
4. If there are syntax errors, they will be displayed below the editor

#### Option 2: Upload IL File
1. Click "📁 Upload File" button
2. Select an .il file from your computer
3. The program will load automatically

#### Option 3: Load Example
1. Click "📚 Examples" button
2. Choose from 8 pre-built example programs
3. The example loads into the editor
4. Modify and test as needed

### Saving Programs

1. Write or load a program in the editor
2. Click "💾 Download" button
3. Save the .il file to your computer
4. Load it later using the Upload feature

### Running the Simulator

1. **Run**: Starts continuous execution at 10ms scan cycle
2. **Stop**: Stops continuous execution
3. **Step**: Executes a single scan cycle
4. **Reset**: Resets all I/O and stops execution

### Controlling I/O

- **Inputs (X0-X15)**: Click to toggle between 0 and 1
  - Green = ON (1)
  - Dark gray = OFF (0)
- **Outputs (Y0-Y15)**: Display only (controlled by program)
  - Green = ON (1)
  - Dark gray = OFF (0)

## Example Programs

The simulator includes 8 ready-to-use example programs in the `examples/` directory:

1. **01_and_gate.il** - Simple AND gate logic
2. **02_or_gate.il** - Simple OR gate logic
3. **03_start_stop.il** - Start/Stop motor circuit with self-latching
4. **04_set_reset.il** - SET/RESET latching instructions
5. **05_multiple_outputs.il** - Multiple independent circuits
6. **06_complex_branches.il** - Complex logic with MPS/ORB
7. **07_traffic_light.il** - Traffic light controller
8. **08_conveyor_belt.il** - Industrial conveyor system

### Loading Examples

**Via Web Interface:**
1. Click the "📚 Examples" button
2. Select an example from the list
3. Test by toggling inputs

**Via File System:**
```bash
cd ~/common-lisp/plclib-cl-web/examples
cat 01_and_gate.il
```

### Quick Example: Start/Stop Circuit
```il
; X0 = Start, X1 = Stop, M0 = Memory, Y0 = Motor
LD X0
OR M0
ANDN X1
OUT M0
OUT Y0
```

See `examples/README.md` for detailed descriptions and test procedures.

## Architecture

### Components

- **plc-state.lisp**: PLC state management (I/O, relays, program storage)
- **il-parser.lisp**: IL instruction parser
- **il-executor.lisp**: IL program executor using plclib-cl
- **il-to-ld.lisp**: IL to LD converter
- **ld-renderer.lisp**: SVG ladder diagram renderer
- **file-ops.lisp**: File I/O operations
- **web-server.lisp**: Hunchentoot web server and REST API
- **static/**: HTML, CSS, and JavaScript frontend

### REST API Endpoints

- `GET /` - Main UI page
- `POST /api/load-program` - Load IL program from text
- `GET /api/state` - Get current PLC state
- `POST /api/toggle-input` - Toggle input
- `POST /api/run` - Start continuous execution
- `POST /api/stop` - Stop execution
- `POST /api/step` - Single step execution
- `POST /api/reset` - Reset PLC
- `GET /api/ladder` - Get ladder diagram SVG
- `GET /api/download` - Download current program as .il file
- `POST /api/upload` - Upload IL program file
- `GET /api/examples` - List available example programs
- `POST /api/load-example` - Load specific example by filename

## Stopping the Server

In the REPL:
```lisp
(plclib-cl-web:stop-server)
```

## Troubleshooting

### Port Already in Use
If port 8080 is already in use, start the server on a different port:
```lisp
(plclib-cl-web:start-server 8081)
```

### Dependencies Not Found
Make sure Quicklisp is properly installed and plclib-cl is in your local-projects:
```lisp
(ql:quickload :plclib-cl)
(ql:quickload :plclib-cl-web)
```

### Parsing Errors
- Make sure each instruction is on its own line
- Comments must start with semicolon (;)
- Device names are case-insensitive but must be valid (X0-X15, Y0-Y15, M0-M99)

## License

MIT License

## Author

Gregory Brooks

## Acknowledgments

- Based on plclib-cl Common Lisp PLC library
- Inspired by LDMicro ladder diagram editor
- IEC 61131-3 IL standard
