; Example 2: OR Gate
; Description: Y0 turns ON when either X0 OR X1 is ON
; 
; Test procedure:
; 1. Load this program
; 2. Turn ON X0 - Y0 turns ON
; 3. Turn OFF X0, turn ON X1 - Y0 stays ON
; 4. Turn OFF both X0 and X1 - Y0 turns OFF

LD X0
OR X1
OUT Y0
