(DEFUN C:TXTFIX ()
(SETVAR "CMDECHO" 0)
(PRINC "\nPlease Select Text(s) ÇëÑ¡ÔñÎÄ×Ö :")
(IF (SETQ SS (SSGET '((0 . "TEXT")))) (PROGN
 (PRINC SS)
 (SETQ SL (SSLENGTH SS) I 0)
 (REPEAT SL
  (SETQ SN (SSNAME SS I) I (1+ I)
        DL (ENTGET SN)
        TT (CDR (ASSOC 1 DL))
        TF (ASCII TT))
  (IF (AND (> TF (ASCII "0")) (<= TF (ASCII "9"))) (PROGN
   (SETQ TN (ATOI TT))
   (IF (> (ABS (- (/ TN 50.0) (/ TN 50))) 1E-6) (PROGN
    (SETQ TT1 (ITOA (* (1+ (/ TN 50)) 50))
          DL (SUBST (CONS 1 TT1) (CONS 1 TT) DL))
    (ENTMOD DL)
   ))
  ))
 )
))
(SETVAR "CMDECHO" 1)
(PRINC)
)