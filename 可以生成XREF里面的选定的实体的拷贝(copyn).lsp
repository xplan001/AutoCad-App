(defun c:copyn (/ blk blks e i lst obj tm ss)
  (setq    blks (vla-get-blocks
               (vla-get-activedocument (vlax-get-acad-object))
             )
        ss   (ssadd)
  )
  (while (and (setq e (nentselp "Select nested object to copy: "))
              (setq tm (caddr e))
              (setq blk (car (cadddr e)))
              (setq blk (vlax-ename->vla-object blk))
              (setq i (vla-item blks (vla-get-name blk)))
         )
    (if        (= (vla-get-isxref i) :vlax-false)
      (vlax-for        be i
        (if (and (setq e (entget (vlax-vla-object->ename be)))
                 (not (cdr (assoc 102 e)))
                 (setq obj (entmakex e))
                 (setq obj (vlax-ename->vla-object obj))
            )
          (progn
            (vla-transformby obj (vlax-tmatrix tm))
            (setq lst (cons (list obj (vla-get-color obj)) lst))
            (vla-put-color obj 1)
            (vla-update obj)
          )
          (princ (strcat "\nComplex entity not created [ "
                         (cdr (assoc 0 e))
                         " ]"
                 )
          )
        )
      )
      (progn
        (setq obj (vlax-ename->vla-object (entmakex (entget (car e)))))
        (vla-transformby obj (vlax-tmatrix tm))
        (setq lst (cons (list obj (vla-get-color obj)) lst))
        (vla-put-color obj 1)
        (vla-update obj)
      )
    )
  )
  (if lst
    (progn (foreach o lst
             ;;(vla-put-color (car o) (cadr o))
             (setq ss (ssadd (vlax-vla-object->ename (car o)) ss))
             (command ".draworder"
                      (vlax-vla-object->ename (car o))
                      ""
                      "front"
             )
           )
           (sssetfirst nil ss)
    )
  )
  (princ)
)