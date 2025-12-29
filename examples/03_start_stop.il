; Example 3: Start/Stop Circuit with Memory
; Description: Classic motor start/stop circuit with self-latching
; X0 = Start button (momentary)
; X1 = Stop button (momentary)
; M0 = Memory/latch relay
; Y0 = Motor output
;
; Test procedure:
; 1. Load this program
; 2. Press X0 (start) - Y0 turns ON and stays ON
; 3. Release X0 - Y0 stays ON (latched through M0)
; 4. Press X1 (stop) - Y0 turns OFF
; 5. Release X1 - Y0 stays OFF

LD X0
OR M0
ANDN X1
OUT M0
OUT Y0
