; PLC Simulator Example Programs
; Uncomment the example you want to test

; Example 1: Simple AND gate
; X0 AND X1 => Y0
LD X0
AND X1
OUT Y0

; Example 2: OR gate
; X2 OR X3 => Y1
LD X2
OR X3
OUT Y1

; Example 3: NOT gate
; NOT X4 => Y2
LDI X4
OUT Y2

; Example 4: Start/Stop with memory latch
; X5 (start) sets M0
; X6 (stop) resets M0
; M0 controls Y3
LD X5
SET M0

LD X6
RST M0

LD M0
OUT Y3

; Example 5: Complex logic with branches
; (X7 AND X8) OR (X9 AND X10) => Y4
LD X7
AND X8
MPS

LD X9
AND X10
ORB
OUT Y4
