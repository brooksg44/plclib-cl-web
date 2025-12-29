; Example 5: Multiple Outputs
; Description: Multiple independent logic circuits
; 
; Circuit 1: X0 AND X1 => Y0
; Circuit 2: X2 OR X3 => Y1
; Circuit 3: NOT X4 => Y2
;
; Test procedure:
; 1. Load this program
; 2. Test each circuit independently
; 3. Y0 requires both X0 and X1
; 4. Y1 activates with X2 or X3
; 5. Y2 is inverse of X4

; Circuit 1: AND gate
LD X0
AND X1
OUT Y0

; Circuit 2: OR gate
LD X2
OR X3
OUT Y1

; Circuit 3: NOT gate
LDI X4
OUT Y2
