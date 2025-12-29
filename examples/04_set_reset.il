; Example 4: SET/RESET Latching
; Description: Using SET and RST instructions for latching
; X0 = Set button
; X1 = Reset button
; Y0 = Latched output
;
; Test procedure:
; 1. Load this program
; 2. Press X0 (set) - Y0 turns ON
; 3. Release X0 - Y0 stays ON (latched)
; 4. Press X1 (reset) - Y0 turns OFF
; 5. Release X1 - Y0 stays OFF

LD X0
SET Y0

LD X1
RST Y0
