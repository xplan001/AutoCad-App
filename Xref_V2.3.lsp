;Xref 命令：
;Reload/重载        Unload/卸载        Detach/拆离        Bind/绑定

;XFR        重载选定
;CZCZ        重载所有 OK
;XFX        卸载选定
;CZXZ        卸载所有 OK
;XFD        拆离选定
;CZCL        拆离所有
;XFB        绑定选定
;CZBD        绑定所有 OK

; -------------------- Reload selected reference files ---------------------
; XFR  
; --------------------------------------------------------------------------
(Defun C:XFR ( )        ;定義“重载选定参照文件”命令

        (setvar "cmdecho" 0)        ;命令执行过程不回显提示和输入

          (princ "\nSelect  objects on the reference file to be Reloaded:")

          (if (setq SS (ssget))
                    (progn
                              (setq Rnames "")
                              (repeat 
                                (setq I (sslength SS))
                                (setq E (ssname SS (setq I (1- I))))
                                (setq ELIST (entget E))
                                (setq Rname (cdr (assoc 2 ELIST))) ;_参照名
                                (command "-xref" "R" Rname)
                                (setq Rnames (strcat Rname ", " Rnames)) ;_strcat 连接成字符串
                              );end repeat
                              (prompt "\nFiles have been Reloaded:")
                              (princ Rnames)
                    );end progn
          );end if
        (princ)
)

(prompt "\n<重载选定参照文件> 命令：XFR  ------by sixth 2010-11-17")         ;提示如何执行程序



; ------------------- Reload all reference files ---------------------
; CZCZ
; --------------------------------------------------------------------
(Defun C:CZCZ ( )          ;定義“重新加载所有外部参照”命令
        (setvar "cmdecho" 0)                ;命令执行过程不回显提示和输入
        (command "-xref" "R" "*") 
          (princ)
)

(prompt "\n<重新加载所有外部参照> 命令：CZCZ  ------by sixth 2010-10-13")         ;提示如何执行程序



; -------------------- Unload selected reference files ---------------------
; XFX
; --------------------------------------------------------------------------
(Defun C:XFX ( )        ;定義“卸载选定参照文件”命令

        (setvar "cmdecho" 0)        ;命令执行过程不回显提示和输入

          (princ "\nSelect  objects on the reference file to be Unloaded:")

          (if (setq SS (ssget))
                    (progn
                              (setq Rnames "")
                              (repeat 
                                (setq I (sslength SS))
                                (setq E (ssname SS (setq I (1- I))))
                                (setq ELIST (entget E))
                                (setq Rname (cdr (assoc 2 ELIST))) ;_参照名
                                (command "-xref" "U" Rname)
                                (setq Rnames (strcat Rname ", " Rnames)) ;_strcat 连接成字符串
                              );end repeat
                              (prompt "\nFiles have been Unloaded:")
                              (princ Rnames)
                    );end progn
          );end if
        (princ)
)

(prompt "\n<卸载选定参照文件> 命令：XFX  ------by sixth 2010-11-17")         ;提示如何执行程序



; ------------------- Unload all reference files ---------------------
; CZXZ
; --------------------------------------------------------------------
(Defun C:CZXZ ( )          ;定義“卸载所有外部参照”命令
        (setvar "cmdecho" 0)                ;命令执行过程不回显提示和输入
        (command "-xref" "U" "*") 
          (princ)
)

(prompt "\n<卸载所有外部参照> 命令：CZXZ  ------by sixth 2010-10-13")         ;提示如何执行程序




; -------------------- Detach selected reference files ---------------------
; XFD
; --------------------------------------------------------------------------

(Defun C:XFD ( )        ;定義“拆离选定参照文件”命令

        (setvar "cmdecho" 0)        ;命令执行过程不回显提示和输入

          (princ "\nSelect  objects on the reference file to be Detached:")

          (if (setq SS (ssget))
                    (progn
                              (setq Rnames "")
                              (repeat 
                                (setq I (sslength SS))
                                (setq E (ssname SS (setq I (1- I))))
                                (setq ELIST (entget E))
                                (setq Rname (cdr (assoc 2 ELIST))) ;_参照名
                                (command "-xref" "Detach" Rname)
                                (setq Rnames (strcat Rname ", " Rnames)) ;_strcat 连接成字符串
                              );end repeat
                              (prompt "\nFiles have been Detached:")
                              (princ Rnames)
                    );end progn
          );end if
        (princ)
)

(prompt "\n<拆离选定参照文件> 命令：XFD  ------by sixth 2010-11-17")         ;提示如何执行程序




; ------------------- Detach all reference files ---------------------
; CZCL
; --------------------------------------------------------------------
(Defun C:CZCL ( )          ;定義“拆离所有外部参照”命令
        (setvar "cmdecho" 0)                ;命令执行过程不回显提示和输入
        (command "-xref" "Detach" "*") 
          (princ)
)

(prompt "\n<拆离所有外部参照> 命令：CZCL  ------by sixth 2010-10-25")         ;提示如何执行程序




; -------------------- Bind selected reference files ---------------------
; XFB
; --------------------------------------------------------------------------

(Defun C:XFB ( )        ;定義“绑定选定参照文件”命令

        (setvar "cmdecho" 0)        ;命令执行过程不回显提示和输入

          (princ "\nSelect  objects on the reference file to be Binded:")

          (if (setq SS (ssget))
                    (progn
                              (setq Rnames "")
                        (setq oldBT (getvar "BINDTYPE"))
                        (setq BT (if (not BT) oldBT BT))
                        (setq BT_tmp 
                                (getstring 
                                        (strcat "输入绑定类型[绑定(0)/插入(1)]<" (itoa BT) ">: ")
                                )
                        )
                        (if (null BT_tmp) (setq BT_tmp BT))
                        (setq BT (atoi BT_tmp))
                        (setvar "BINDTYPE" BT)
                              (repeat 
                                (setq I (sslength SS))
                                (setq E (ssname SS (setq I (1- I))))
                                (setq ELIST (entget E))
                                (setq Rname (cdr (assoc 2 ELIST))) ;_参照名
                                (command "-xref" "Bind" Rname)
                                (setq Rnames (strcat Rname ", " Rnames)) ;_strcat 连接成字符串
                              );end repeat
                        (setvar "BINDTYPE" oldBT)
                              (prompt "\nFiles have been Binded:")
                              (princ Rnames)
                    );end progn
          );end if
        (princ)
)

(prompt "\n<绑定选定参照文件> 命令：XFB  ------by sixth 2013-09-29")         ;提示如何执行程序




; ------------------- Bind all reference files ---------------------
; CZBD
; --------------------------------------------------------------------
(Defun C:CZBD ( )          ;定義“绑定所有外部参照”命令
        (setvar "cmdecho" 0)                ;命令执行过程不回显提示和输入
        (setq oldBT (getvar "BINDTYPE"))
        (setq BT (if (not BT) oldBT BT))
        (setq BT_tmp 
                (getstring 
                        (strcat "输入绑定类型[绑定(0)/插入(1)]<" (itoa BT) ">: ")
                )
        )
        (if (null BT_tmp) (setq BT_tmp BT))
        (setq BT (atoi BT_tmp))
        (setvar "BINDTYPE" BT)
        (command "-xref" "Bind" "*") 
        (setvar "BINDTYPE" oldBT)
          (princ)
)

(prompt "\n<绑定所有外部参照> 命令：CZBD  ------by sixth 2013-09-29")         ;提示如何执行程序


;添一个，绝对路径改相对路径
(Defun c:tt()
(vl-load-com)
(setq ssg (ssget "x" '((0 . "insert"))))
(setq xpath (getstring "Input xref path:"))
  (setq i 0)
  (repeat (sslength ssg)
(setq obj (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) (cdr (assoc 2 (entget (ssname ssg i))))))
(if (= (vla-get-isxref obj) :vlax-true) (vla-put-path obj (strcat xpath (vla-get-name obj) ".dwg")))
    (setq i (1+ i))
  )
(command "_xref" "r" "*")
(princ)
)

