(defun c:gkm (/ NAME NEW-NAME)
  (setq        name (Vlax-Get (Vlax-Ename->Vla-Object
                         (car (entsel "\n选择要改块名的块："))
                       )
                       'Name
             )
  )
  (setq ss-text (entsel "\n选择新块名称文字:"))
  (if (= ss-text nil)
    (setq new-name (getstring "\输入新的块名:"))
    (setq new-name (Vlax-Get (Vlax-Ename->Vla-Object
                               (car ss-text)
                             )
                             'TextString
                   )
    )
  )
  (command "_.rename" "_block" name new-name)
  (princ)
)