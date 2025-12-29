; Example 6: Complex Logic with Branches
; Description: Using MPS/ORB for parallel logic branches
; Logic: (X0 AND X1) OR (X2 AND X3) => Y0
;
; Test procedure:
; 1. Load this program
; 2. Turn ON X0 and X1 - Y0 turns ON
; 3. Turn OFF X0 and X1
; 4. Turn ON X2 and X3 - Y0 turns ON
; 5. Y0 is ON if either pair is ON

LD X0
AND X1
MPS

LD X2
AND X3
ORB
OUT Y0
