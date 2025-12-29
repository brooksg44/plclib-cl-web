# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Project Overview

A web-based PLC (Programmable Logic Controller) simulator that implements IEC 61131-3 Instruction List (IL) programming with automatic conversion to Ladder Diagram (LD) display. Built with Common Lisp using SBCL, Hunchentoot web server, and the plclib-cl engine.

## Development Environment

### Starting the Application

**Method 1: Using start script (recommended)**
```bash
sbcl --load start.lisp
```

**Method 2: From REPL**
```lisp
(ql:quickload :plclib-cl-web)
(plclib-cl-web:start-server)
```

**Method 3: Different port**
```lisp
(plclib-cl-web:start-server 8081)
```

### Stopping the Server
```lisp
(plclib-cl-web:stop-server)
```

### Dependencies
The system uses ASDF and Quicklisp. Key dependencies:
- **plclib-cl** - Core PLC library (must be in `~/common-lisp/plclib-cl`)
- **hunchentoot** - Web server
- **cl-ppcre** - Regular expressions for IL parsing
- **cl-json** - JSON API responses
- **bordeaux-threads** - Cross-platform threading

### Loading and Testing
```lisp
; Load the system
(ql:quickload :plclib-cl-web)

; Test core library functions
(plclib-cl:run-all-examples)

; Start server
(plclib-cl-web:start-server)

; Access at http://localhost:8080
```

## Architecture

### Core Component Flow
```
Web Interface (HTML/JS/CSS)
    ↓ HTTP API
Web Server (web-server.lisp)
    ↓
IL Parser (il-parser.lisp) → Parsed Instructions
    ↓
IL Executor (il-executor.lisp) → Uses plclib-cl
    ↓
PLC State (plc-state.lisp) → Inputs/Outputs/Relays
    ↓
IL to LD Converter (il-to-ld.lisp) → Ladder Structure
    ↓
LD Renderer (ld-renderer.lisp) → SVG Output
```

### File Structure
- **plclib-cl-web.asd** - System definition with dependencies and load order
- **package.lisp** - Package exports and API surface
- **start.lisp** - Quick start script
- **src/plc-state.lisp** - Thread-safe PLC state management
- **src/il-parser.lisp** - IL instruction parser
- **src/il-executor.lisp** - IL instruction executor with scan cycle
- **src/il-to-ld.lisp** - IL to Ladder Diagram converter
- **src/ld-renderer.lisp** - SVG ladder diagram renderer
- **src/file-ops.lisp** - File I/O operations
- **src/web-server.lisp** - REST API and HTTP handlers
- **static/** - Web UI assets (HTML, CSS, JavaScript)
- **examples/** - 8 example IL programs

### State Management

**Global State Variables** (in plc-state.lisp):
- `*inputs*` - Array[16] of digital inputs X0-X15
- `*outputs*` - Array[16] of digital outputs Y0-Y15  
- `*relays*` - Hash table of internal relays M0-M99
- `*current-program*` - IL source code string
- `*parsed-program*` - List of parsed instruction structures
- `*execution-state*` - One of `:stopped`, `:running`, `:single-step`
- `*execution-thread*` - Background thread for continuous execution
- `*state-lock*` - Bordeaux-threads lock for thread safety

**Thread Safety**: All state modifications use `bt:with-lock-held (*state-lock*)` to prevent race conditions during continuous execution at 10ms scan cycles.

### Execution Model

**PLC Scan Cycle**:
1. Reset accumulator and branch stack
2. Execute IL instructions sequentially
3. Update outputs based on accumulator values
4. Increment scan counter
5. Call `plclib-cl:plc-scan` for timing
6. Sleep 10ms (for continuous mode)

**Accumulator Model**: IL is stack-based with a single accumulator holding boolean results (0 or 1). Instructions like LD, AND, OR modify the accumulator; OUT writes it to devices.

**Branch Stack**: Used for MPS/MRD/MPP instructions to handle parallel ladder logic branches (like opening/closing parentheses in boolean expressions).

## Common Development Tasks

### Adding a New IL Instruction

1. Add opcode to `*valid-opcodes*` in **il-parser.lisp**
2. Implement `execute-your-opcode` function in **il-executor.lisp**
3. Add case to `execute-instruction` switch statement
4. Add ladder conversion logic in **il-to-ld.lisp** `convert-rung-to-ld`
5. If new element type, add renderer in **ld-renderer.lisp**

### Adding a Web API Endpoint

Add handler in **web-server.lisp**:
```lisp
(hunchentoot:define-easy-handler (handler-name :uri "/api/endpoint")
    (param1 param2)
  (handler-case
      (progn
        ;; Your logic here
        (json-success (list (cons :result value))))
    (error (e)
      (json-error (format nil "~A" e)))))
```

Response format:
```json
{"success": true, "message": "...", "data": {...}}
{"success": false, "error": "Error message"}
```

### Debugging

**Enable debug output**:
```lisp
(plclib-cl:serial-begin)
(plclib-cl:serial-println "Debug message")
```

**Inspect state**:
```lisp
(plclib-cl-web::get-plc-state)
(plclib-cl-web::get-execution-state)
(plclib-cl:get-plc-status)
```

**Common issues**:
- Thread deadlock: Check for nested `bt:with-lock-held`
- Parse errors: Verify IL syntax (one instruction per line, semicolon comments)
- Execution errors: Add `(format t ...)` in execute functions

## Code Conventions

### Naming
- Use kebab-case: `my-function-name`
- Predicates end in `-p`: `counter-done-p`
- Global variables use earmuffs: `*global-var*`
- Private functions in package with `::`

### Documentation
- Add docstrings to all exported functions
- Include parameter types and return values
- Complex functions need usage examples

### Threading
- Always use `bt:with-lock-held (*state-lock*)` when modifying shared state
- Never hold locks during I/O operations
- Test with continuous execution (background thread) running

### Error Handling
- Use `handler-case` for recoverable errors in API handlers
- Signal meaningful error messages with context
- Don't swallow errors silently in execution loop

## IL Programming Language

**Supported Instructions**:
- **Load**: LD (normal open), LDI (normal closed)
- **Logic**: AND, ANDN, OR, ORN, XOR, XORN
- **Output**: OUT (output), SET (latch on), RST (latch off)
- **Branch**: MPS (push), MRD (read), MPP (pop), ANB (AND block), ORB (OR block)

**Device Types**:
- X0-X15 (digital inputs)
- Y0-Y15 (digital outputs)
- M0-M99 (internal relays)

**Example Program**:
```il
; Start/Stop circuit with self-latch
LD X0      ; Start button
OR M0      ; Self-latch
ANDN X1    ; Stop button (normally closed)
OUT M0     ; Update latch
OUT Y0     ; Motor output
```

## Integration with plclib-cl

This project depends on the **plclib-cl** library which must be located at `~/common-lisp/plclib-cl`. The core library provides:

- Pin management and I/O simulation
- Timers (on-delay, off-delay, pulse, cycle)
- Counters (up/down with preset values)
- Shift registers
- Stack operations for boolean logic
- Pulse/edge detection
- Scan cycle management

The web application wraps plclib-cl's functionality with an IL parser/executor and web interface. Changes to core PLC behavior should be made in plclib-cl; changes to IL language support, web UI, or ladder rendering go in plclib-cl-web.

## Testing

No formal test suite exists. Manual testing workflow:

1. Start server: `sbcl --load start.lisp`
2. Open browser: http://localhost:8080
3. Load example programs from **examples/** directory (01-08)
4. Toggle inputs and verify outputs match expected behavior
5. Verify ladder diagram displays correctly
6. Test Run/Stop/Step/Reset controls
7. Test file upload/download functionality

For IL parser/executor changes, test with all 8 example programs to ensure no regressions.

## REST API Endpoints

- `GET /` - Main UI page
- `POST /api/load-program` - Load IL program from text
- `GET /api/state` - Get current PLC state (I/O, scan count)
- `POST /api/toggle-input` - Toggle input X0-X15
- `POST /api/run` - Start continuous execution
- `POST /api/stop` - Stop execution
- `POST /api/step` - Execute single scan
- `POST /api/reset` - Reset PLC to initial state
- `GET /api/ladder` - Get ladder diagram SVG
- `GET /api/download` - Download program as .il file
- `POST /api/upload` - Upload IL program file
- `GET /api/examples` - List available examples
- `POST /api/load-example` - Load specific example

## Additional Documentation

- **README.md** - User-facing documentation and installation
- **DEVELOPER_GUIDE.md** - Comprehensive technical documentation (1300+ lines covering plclib-cl and plclib-cl-web architecture)
- **QUICKSTART.md** - Quick start guide for users
- **examples/README.md** - Example programs with test procedures
