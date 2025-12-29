; Example 8: Conveyor Belt System
; Description: Conveyor with start, stop, and emergency stop
; X0 = Start button
; X1 = Stop button
; X2 = Emergency stop (normally closed - inverted)
; X3 = Safety guard sensor
; M0 = Conveyor running memory
; Y0 = Conveyor motor
; Y1 = Warning light
;
; Test procedure:
; 1. Load this program
; 2. Turn ON X3 (safety guard closed)
; 3. Press X0 (start) - conveyor runs (Y0 ON)
; 4. Press X1 (stop) - conveyor stops
; 5. Press X2 (emergency stop) - immediate stop, warning light ON

; Conveyor logic: (Start OR Running) AND NOT Stop AND NOT E-Stop AND Safety
LD X0
OR M0
ANDN X1
ANDN X2
AND X3
OUT M0
OUT Y0

; Warning light: ON when emergency stop pressed
LD X2
OUT Y1
