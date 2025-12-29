# PLC Simulator - Quick Start Guide

## Starting the Simulator

### Method 1: Using the start script (Recommended)
```bash
cd ~/common-lisp/plclib-cl-web
sbcl --load start.lisp
```

### Method 2: From REPL
```bash
sbcl
```

Then in the REPL:
```lisp
(ql:quickload :plclib-cl-web)
(plclib-cl-web:start-server)
```

### Method 3: From anywhere with SBCL
```bash
sbcl --eval "(ql:quickload :plclib-cl-web)" --eval "(plclib-cl-web:start-server)"
```

## Using the Simulator

1. **Open browser**: http://localhost:8080

2. **Load a program** (choose one method):
   - **Type directly**: Enter IL code in editor, click "Load Program"
   - **Upload file**: Click "📁 Upload File", select .il file
   - **Load example**: Click "📚 Examples", choose from 8 examples

3. **View ladder diagram**: Auto-displays in right panel

4. **Toggle inputs**: Click X0-X15 buttons to change input states

5. **Run program**: 
   - Click "Run" for continuous execution
   - Click "Step" for single scan
   - Click "Stop" to halt execution
   - Click "Reset" to clear all

6. **Save your work**: Click "💾 Download" to save as .il file

## Example Program

Copy this into the editor:

```il
; Simple start/stop circuit
LD X0
OR M0
ANDN X1
OUT M0
OUT Y0
```

Instructions:
1. Load the program
2. Click X0 (simulates pressing start button)
3. M0 and Y0 turn ON (latch)
4. Release X0 - Y0 stays ON
5. Click X1 (simulates pressing stop button)
6. Y0 turns OFF

## Stopping the Server

In the REPL:
```lisp
(plclib-cl-web:stop-server)
```

Or press Ctrl+C twice

## Common Issues

**"Package PLCLIB-CL-WEB not found"**
- Make sure you're in ~/common-lisp/plclib-cl-web directory
- Run: `(ql:quickload :plclib-cl-web)`

**"Port already in use"**
- Start on different port: `(plclib-cl-web:start-server 8081)`

**"Parse error"**
- Check syntax: one instruction per line
- Valid devices: X0-X15, Y0-Y15, M0-M99
- Comments start with semicolon

**Browser shows "Cannot connect"**
- Verify server started: check REPL for "started on port 8080"
- Check firewall settings
- Try: http://127.0.0.1:8080

## Key Shortcuts

- **Ctrl+Enter**: Load program (when focused on editor)
- **F5**: Refresh browser to update ladder display

## File Locations

- **Examples**: ~/common-lisp/plclib-cl-web/examples.il
- **Source**: ~/common-lisp/plclib-cl-web/src/
- **Web UI**: ~/common-lisp/plclib-cl-web/static/

## Trying the Examples

The simulator includes 8 example programs:

1. Click "📚 Examples" button
2. Choose an example (start with "01_and_gate")
3. Program loads automatically
4. Follow the test procedure in the comments
5. Toggle inputs and observe outputs

**Example Test Flow:**
- Load "03_start_stop.il"
- Click X0 (start button) - Y0 turns ON
- Release X0 - Y0 stays ON (latched)
- Click X1 (stop button) - Y0 turns OFF

## Next Steps

- Try all 8 examples in order (01 through 08)
- Read examples/README.md for learning path
- Read README.md for full documentation
- Experiment with different IL instructions
- Build your own PLC programs!
- Save and share your programs
