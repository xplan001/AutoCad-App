
; 对当前图形里的常规词典进行列表

(defun C:dicts ( / xdoc xdicts xi xc xii xlst xname xnl xhl myGetCount)

   (defun myGetCount ( xd xn / xitem errobj)
      (setq xitem (vla-item xd xn)  errobj (vl-catch-all-apply 'vla-get-count (list xitem)))
      (if (vl-catch-all-error-p errobj)
           "#n/a"
           (itoa errobj)
      )
   )  ; end of myerr()

   (vl-load-com)
   (textscr)
   (command "Undo" "c" "n")
   (setq xdoc (vla-get-ActiveDocument (vlax-get-Acad-Object))
         xdicts (vla-get-dictionaries xdoc)
         xdictlist (entget (namedobjdict))
         xi 0  xc (length xdictlist)  xii 0  xnl nil  xhl nil
   )
   (while (< xi xc)  ; for each element in xdictlist, the dictionary list
      (if (= (car (setq xlst (nth xi xdictlist))) 3)
          (progn
             (setq xii (1+ xii)  xi (1+ xi))
(princ (strcat (itoa xii) ". \"" (setq xname (cdr xlst)) "\"  "  (myGetCount xdicts xname) "\n"))
             (setq xnl (cons xname xnl)  xhl (cons (cdr (nth xi xdictlist)) xhl))
          )  ; end of progn(it's a dictionary item)
      )  ; end of if(it's a dictionary item)
      (setq  xi (1+ xi))
   )  ; end of while(each element in dictionary list)
   (princ (strcat "\nActiveDocument.Dictionaries.Count=" (itoa (vla-get-count xdicts)) "\n"))
   (initget 6)  ; no zero or negative value allowed
   (setq  xnl (reverse xnl)  xhl (reverse xhl)  xi (getint "\nWhich one to REMOVE by index above? <Exit>: "))
   (if (and xi (<= xi xii)) (entdel (nth (1- xi) xhl)))
   (command "Undo" "a")
   (princ)
)  ; end of (C:dicts)
(C:dicts)
(princ "\nYou can type command of DICTS to go again.")
(princ)
