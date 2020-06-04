;批量替换字符
; CHGTEXT command - rudimentary text editor

(defun C:CHTEXT (/ p l n e os as ns st s nsl osl sl si chf chm cont)
   (setq chm 0 p (ssget))            ; Select objects
   (if p (progn                      ; If any objects selected
      (setq cont t)
      (while cont
         (setq osl (strlen (setq os (getstring "\n原字符串: " t))))
         (if (= osl 0)
            (princ "Null input invalid")
            (setq cont nil)
         )
      )
      (setq nsl (strlen (setq ns (getstring "\n新字符串: " t))))
      (setq l 0 n (sslength p))
      (while (< l n)                 ; For each selected object...
         (if (OR (= "MTEXT"             ; Look for TEXT entity type (group 0)
                 (cdr (assoc 0 (setq e (entget (ssname p l))))))
		 (= "TEXT"             ; Look for TEXT entity type (group 0)
                 (cdr (assoc 0 (setq e (entget (ssname p l)))))))
            (progn
               (setq chf nil si 1)
               (setq s (cdr (setq as (assoc 1 e))))
               (while (= osl (setq sl (strlen
                             (setq st (substr s si osl)))))
                  (if (= st os)
                      (progn
                        (setq s (strcat (substr s 1 (1- si)) ns
                                        (substr s (+ si osl))))
                        (setq chf t)    ; Found old string
                        (setq si (+ si nsl))
                      )
                      (setq si (1+ si))
                  )
               )
               (if chf (progn        ; Substitute new string for old
                  (setq e (subst (cons 1 s) as e))
                  (entmod e)         ; Modify the TEXT entity
                  (setq chm (1+ chm))
               ))
            )
         )
         (setq l (1+ l))
      )
   ))
   (princ "修改了 ")                ; Print total lines changed
   (princ chm)
   (princ " 行文字.")
   (terpri)
)
