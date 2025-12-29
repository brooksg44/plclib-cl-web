; Example 1: Simple AND Gate
; Description: Y0 turns ON only when both X0 AND X1 are ON
; 
; Test procedure:
; 1. Load this program
; 2. Turn ON X0 - Y0 stays OFF
; 3. Turn ON X1 - Y0 turns ON
; 4. Turn OFF either X0 or X1 - Y0 turns OFF

LD X0
AND X1
OUT Y0
