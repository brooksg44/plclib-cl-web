; Example 7: Simple Traffic Light Controller
; Description: Manual traffic light control
; X0 = North/South GO
; X1 = East/West GO
; Y0 = North/South Green
; Y1 = North/South Red
; Y2 = East/West Green
; Y3 = East/West Red
;
; Test procedure:
; 1. Load this program
; 2. Turn ON X0 - N/S green (Y0), E/W red (Y3)
; 3. Turn OFF X0, turn ON X1 - N/S red (Y1), E/W green (Y2)
; 4. Default: all red

; North/South Green when X0 is ON and X1 is OFF
LD X0
ANDN X1
OUT Y0

; North/South Red when X0 is OFF or X1 is ON
LDI X0
OR X1
OUT Y1

; East/West Green when X1 is ON and X0 is OFF
LD X1
ANDN X0
OUT Y2

; East/West Red when X1 is OFF or X0 is ON
LDI X1
OR X0
OUT Y3
