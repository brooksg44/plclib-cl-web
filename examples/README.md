# PLC Simulator Example Programs

This directory contains example IL programs that demonstrate various PLC programming concepts.

## Available Examples

### 1. 01_and_gate.il
Simple AND gate logic
- **Purpose**: Y0 = X0 AND X1
- **Demonstrates**: Basic AND operation

### 2. 02_or_gate.il
Simple OR gate logic
- **Purpose**: Y0 = X0 OR X1
- **Demonstrates**: Basic OR operation

### 3. 03_start_stop.il
Start/Stop motor circuit with self-latching
- **Purpose**: Classic motor control with memory
- **Demonstrates**: Self-latching, memory relay usage

### 4. 04_set_reset.il
SET/RESET latching instructions
- **Purpose**: Latching output using SET and RST
- **Demonstrates**: SET and RST instructions

### 5. 05_multiple_outputs.il
Multiple independent logic circuits
- **Purpose**: Three separate circuits in one program
- **Demonstrates**: Multiple rungs, different logic types

### 6. 06_complex_branches.il
Complex logic with branch operations
- **Purpose**: Parallel branches with OR combination
- **Demonstrates**: MPS/ORB branch operations

### 7. 07_traffic_light.il
Simple traffic light controller
- **Purpose**: Two-way traffic light control
- **Demonstrates**: Interlocking, inverted logic

### 8. 08_conveyor_belt.il
Conveyor belt system with safety features
- **Purpose**: Industrial conveyor control
- **Demonstrates**: Multiple conditions, safety interlocks, warning outputs

## Using Examples

### From Web Interface
1. Click the "📚 Examples" button in the program editor
2. Select an example from the list
3. The program will load into the editor and display as ladder diagram
4. Toggle inputs to test the program behavior

### From File System
All example files are plain text `.il` files that can be:
- Opened in any text editor
- Modified and saved with your own changes
- Used as templates for your programs

## Learning Path

Recommended order for learning:
1. **01_and_gate.il** - Start with basic logic
2. **02_or_gate.il** - Learn OR operations
3. **05_multiple_outputs.il** - Combine multiple circuits
4. **03_start_stop.il** - Understand memory and latching
5. **04_set_reset.il** - Learn SET/RST instructions
6. **06_complex_branches.il** - Master branch operations
7. **07_traffic_light.il** - Apply to practical scenario
8. **08_conveyor_belt.il** - Complete industrial example

## Tips for Testing

- Use the "Step" button to execute one scan at a time
- Watch how the ladder diagram highlights active contacts
- Try different input combinations
- Observe output responses
- Experiment by modifying the programs

## Creating Your Own Examples

You can create new example files:
1. Create a new `.il` file in this directory
2. Write your IL program
3. Add comments explaining the program
4. Restart the server to see it in the examples list
