
(defun c:all () 
  (setq sset nil 
    sslist nil 
  ) 
  (command "layer" "n" "temp" "s" "temp" "") 
  (command "color" "3") 
  (setq sset (ssget '((1 . ".")))) 
  (setq lena (sslength sset) 
    i 0 
  ) 
  (repeat lena 
    (setq a (ssname sset i)) 
    (setq pointa (cdr (assoc 11 (entget a)))) 
    (setq pointc (polar (polar pointa pi 6) (* pi 1.5) 2)) 
    (setq pointd (polar (polar pointa 0 3) (* pi 0.5) 3)) 
    (setq sset1 (ssget "w" pointc pointd)) ; (grdraw pointc pointd 4) 
    (setq sslist nil 
      sslist1 nil 
    ) 
    (setq sslist (std-sslist sset1)) 
    (setq sslist1 (vl-sort sslist (function (lambda (e1 e2) 
                          (< (car (cdr (assoc 11 
                                  (entget e1) 
                               ) 
                              ) 
                         ) (car (cdr (assoc 11 
                                    (entget e2) 
                                 ) 
                            ) 
                           ) 
                          ) 
                        ) 
                  ) 
          ) 
    ) 
    (if (< (length sslist1) 6) 
      (progn 
    (setq m "") 

    (foreach x sslist1 
      (setq m (strcat m (cdr (assoc 1 (entget x))))) 
    ) 
    (setq p1 (cdr (assoc 11 (entget (nth 0 sslist1))))) 
    (command "text" p1 "2.2" "0" m) 
      ) 
    ) 
    (setq i (1+ i)) 
  ) 
) 


(defun STD-SSLIST (ss / n lst) 
  (if (eq 'PICKSET (type ss)) 
    (repeat (setq n (fix (sslength ss))) ; fixed 
      (setq lst (cons (ssname ss (setq n (1- n))) lst)) 
    ) 
  ) 
)
