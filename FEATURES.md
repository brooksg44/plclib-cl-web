# PLC Simulator Features

## Core Functionality

### IL Programming Support
- ✅ Full IEC 61131-3 IL instruction subset
- ✅ LD, LDI, AND, ANDN, OR, ORN, XOR, XORN
- ✅ OUT, SET, RST instructions
- ✅ MPS, MRD, MPP, ANB, ORB branch operations
- ✅ Comments with semicolon prefix
- ✅ Real-time syntax validation

### Ladder Diagram Display
- ✅ Automatic IL to LD conversion
- ✅ SVG-based rendering (LDMicro style)
- ✅ Power rails (left and right)
- ✅ Normally open contacts (--] [--)
- ✅ Normally closed contacts (--]/[--)
- ✅ Output coils ( )
- ✅ SET coils (S)
- ✅ RESET coils (R)
- ✅ Device labels
- ✅ Rung numbering

### I/O System
- ✅ 16 digital inputs (X0-X15)
- ✅ 16 digital outputs (Y0-Y15)
- ✅ Interactive input toggle buttons
- ✅ Real-time output indicators
- ✅ Visual ON/OFF feedback (green/gray)
- ✅ Internal memory relays (M0-M99)

### Execution Control
- ✅ Run mode (continuous execution, 10ms scan cycle)
- ✅ Stop mode
- ✅ Single Step mode
- ✅ Reset (clear all I/O and state)
- ✅ Scan counter
- ✅ Execution state indicator
- ✅ Thread-safe operation

### File Operations
- ✅ Upload IL files from computer
- ✅ Download programs as .il files
- ✅ Drag-and-drop file support
- ✅ Browser-based file handling
- ✅ No server-side file storage required

### Example Programs
- ✅ 8 ready-to-use example programs
- ✅ One-click example loading
- ✅ Examples browser in UI
- ✅ Commented test procedures
- ✅ Progressive difficulty levels
- ✅ Real-world applications

### Web Interface
- ✅ Single-page application
- ✅ Split-panel layout (editor + ladder)
- ✅ Responsive design
- ✅ Dark theme
- ✅ Professional PLC-style UI
- ✅ Real-time updates
- ✅ Error display
- ✅ Status indicators

### Technical Features
- ✅ Common Lisp backend
- ✅ Hunchentoot web server
- ✅ REST API
- ✅ JSON data exchange
- ✅ plclib-cl integration
- ✅ Thread-safe state management
- ✅ Regex-based IL parser
- ✅ SVG graphics generation

## Example Programs Included

1. **01_and_gate.il** - Basic AND logic
2. **02_or_gate.il** - Basic OR logic
3. **03_start_stop.il** - Motor start/stop with latching
4. **04_set_reset.il** - SET/RST latching
5. **05_multiple_outputs.il** - Multiple circuits
6. **06_complex_branches.il** - Branch operations (MPS/ORB)
7. **07_traffic_light.il** - Traffic light controller
8. **08_conveyor_belt.il** - Industrial conveyor system

## REST API Endpoints

### Program Management
- `POST /api/load-program` - Load program from text
- `POST /api/upload` - Upload IL file
- `GET /api/download` - Download current program
- `GET /api/program` - Get current program text
- `GET /api/examples` - List available examples
- `POST /api/load-example` - Load specific example

### Execution Control
- `POST /api/run` - Start continuous execution
- `POST /api/stop` - Stop execution
- `POST /api/step` - Execute single scan
- `POST /api/reset` - Reset PLC state

### I/O Control
- `POST /api/toggle-input` - Toggle specific input
- `GET /api/state` - Get complete PLC state

### Visualization
- `GET /api/ladder` - Get ladder diagram SVG

## Browser Compatibility

- ✅ Chrome/Chromium
- ✅ Firefox
- ✅ Safari
- ✅ Edge
- ✅ Modern mobile browsers

## File Format

### IL Program Files (.il)
- Plain text format
- UTF-8 encoding
- One instruction per line
- Comments start with semicolon
- No special headers required
- Cross-platform compatible

## Performance

- Scan cycle: 10ms (adjustable)
- UI update rate: 200ms
- Real-time ladder diagram updates
- Instant input response
- No lag in continuous mode

## Safety & Limitations

- Simulation only (no hardware control)
- Educational/testing purposes
- All state in memory (no persistence)
- Reset on server restart
- Single-user mode

## Future Enhancement Possibilities

- Timer instructions (TON, TOF, TP)
- Counter preset values
- Data registers
- Math operations
- Comparison instructions
- Multiple program files
- Program versioning
- Hardware I/O drivers
- Multi-user support
- Program debugging tools
- Breakpoints
- Watch variables
- Execution trace
