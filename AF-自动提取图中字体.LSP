;;;    ============================================= 
;;;    |            自动提取图中字体软件           | 
;;;    |              Update: 03.06.12             | 
;;;    ============================================= 




;;;以下内容定义程序加载时提示加载次数、版权等信息 


;;;(setq cm (getvar "cmdecho")) 
;;;(setvar "cmdecho" 0) 
;;;(while (= nil (findfile "acad.fnt")) 
;;;  ;(setq fp (open "c:\acad.fnt" "w")) 
;;;  ;(setq fp (open "acad.fnt" "w")) 
;;; 
;;;  ;(write-line "3" fp) 
;;;  ;(close fp) 
;;;  ;(command "sh" "attrib acad.fnt +h") 
;;;;) 
;;;;(setq fp (open "acad.fnt" "r")) 
;;;;(setq count (read (read-line fp))) 
;;;;(close fp) 
;;;;(command "sh" "attrib acad.fnt +h") 
;;;;(if (or    (= count 3) 
;;;;    (= count 9) 
;;;;    (= count 27) 
;;;;    (= count 81) 
;;;;    (= count 243) 
;;;;    (= count 729) 
;;;;    (= count 2187) 
;;;;    (= count 6561) 
;;;;    (= count 19683) 
;;;;    (= count 59049) 
;;;;    (= count 177147) 
;;;;    (= count 531441) 
;;;;   ) 
;;;;  (progn 
;;;;;;    (if    (= count 3) 
;;;;;;      (setq alc "1 time.") 
;;;;;;    ) 
;;;;;;    (if    (= count 9) 
;;;      (setq alc "2 times.") 
;;;    ) 
;;;    (if    (= count 81) 
;;;      (setq alc "3 times.") 
;;;    ) 
;;;    (if    (= count 243) 
;;;      (setq alc "4 times.") 
;;;    ) 
;;;    (if    (= count 729) 
;;;      (setq alc "5 times.") 
;;;    ) 
;;;    (if    (= count 2187) 
;;;      (setq alc "6 times.") 
;;;    ) 
;;;    (if    (= count 6561) 
;;;      (setq alc "7 times.") 
;;;    ) 
;;;    (if    (= count 19683) 
;;;      (setq alc "8 times.") 
;;;    ) 
;;;    (if    (= count 59049) 
;;;      (setq alc "9 times.") 
;;;    ) 
;;;    (if    (= count 177147) 
;;;      (setq alc "10 times.") 
;;;    ) 
;;;    (if    (= count 531441) 
;;;      (setq alc 
;;;         "11 (bonus) times.  After this use AUTOFONT.LSP will become inoperative." 
;;;      ) 
;;;    ) 
;;;    (setq prmt (strcat "\n AUTOFONT.LSP has already been loaded " 
;;;               alc 
;;;               " \n" 
;;;           ) 
;;;    ) 
;;;  ) 
;;;  (setq    prmt 
;;;     (strcat 
;;;       "\n ** LSP program has already been loaded 10 times and is become inoperative ** ;\n" 
;;;     ) 
;;;  ) 
;;;) 
;;;(setq fp (open "acad.fnt" "r")) 
;;;(setq count (read (read-line fp))) 
;;;(close fp) 
;;;(if (or    (= count 3) 
;;;    (= count 9) 
;;;    (= count 27) 
;;;    (= count 81) 
;;;    (= count 243) 
;;;    (= count 729) 
;;;    (= count 2187) 
;;;    (= count 6561) 
;;;    (= count 19683) 
;;;    (= count 59049) 
;;;    (= count 177147) 
;;;    (= count 531441) 
;;;    ) 
;;;  (progn 
;;;    (command "sh" "attrib acad.fnt -h") 
;;;    (setq fp (open "acad.fnt" "w")) 
;;;    (setq aaa (* count 3)) 
;;;    (princ aaa fp) 
;;;    (write-line aaa fp) 
;;; 
;;;    (close fp) 
;;;    (command "sh" "attrib acad.fnt +h") 

;;;以下定义从关联表“elist”中搜索含有元素“code”的条目 
(defun dxf (code elist) (cdr (assoc code elist))) 

;;;以下定义得到所有指定类型“tbname”的集合 
(defun tnlist (tbname / tdata tblist) 
  (while (setq tdata (tblnext tbname (not tdata))) 
    (setq tblist (append tblist (list (dxf 2 tdata)))) 
  ) ;_ 结束while 
) ;_ 结束defun 

;;;以下定义得到关键字提示 
(defun ukword (bit kwd msg def / inp) 
  (if (and def (/= def "")) 
    (setq msg (strcat "\n" msg "<" def ">: ") 
      bit (* 2 (fix (/ bit 2))) 
    ) ;_ 结束setq 
    (if    (= " " (substr msg (strlen msg) 1)) 
      (setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": ")) 
      (setq msg (strcat "\n" msg ": ")) 
    ) ;_ 结束if 
  ) ;_ 结束if 
  (initget bit kwd) 
  (setq inp (getkword msg)) 
  (if inp 
    inp 
    def 
  ) ;_ 结束if 
) ;_ 结束defun 

;;;    (defun ustr    (bit msg def spflag / inp nval) 
;;;      (if (and def (/= def "")) 
;;;    (setq msg (strcat "\n" msg "<" def ">: ") 
;;;          inp (getstring msg spflag) 
;;;          inp (if (= inp "") 
;;;            def 
;;;            inp 
;;;          ) 
;;;    ) 
;;;    (progn (if (= " " (substr msg (strlen msg) 1)) 
;;;         (setq 
;;;           msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": ") 
;;;         ) 
;;;         (setq msg (strcat "\n" msg ": ")) 
;;;           ) 
;;;           (if (= bit 1) 
;;;         (while    (= "" (setq inp (getstring msg spflag))) 
;;;           (prompt "\nInvalid string.") 
;;;         ) 
;;;         (setq inp (getstring msg spflag)) 
;;;           ) 
;;;    ) 
;;;      ) 
;;;    ) 
;;;    (prompt "\nType AF to run autofont.LSP \n") 
(defun C:AF (/        cm       l      ll     counts    countn countp fp 
         x        test   testl  n     fnts    fntb   fntl   uk 
         fntf   fntc   countp si     sii    stt    fntll 
        ) 
  (princ (strcat "\n***自动提取图中字体软件V031212***")) 
  (princ (strcat "\n       [它山之石修正版]")) 
  (princ) 


  (setq    cm     (getvar "cmdecho") 
    l      (tnlist "style")        ;列出本图中定义的所有字体 
    ll     (length l)        ;所有的字体数 
    counts 0 
    countn 1 
    countp 0 
  ) ;_ 结束setq 
  (setvar "cmdecho" 0) 
  (command "shell" (strcat "md c:gelei")) 
;;;建立备份字体的文件夹 
  (while (setq n (nth counts l)) 
    (setq counts (+ counts 1)) 
    (setq fnts (dxf 3 (tblsearch "style" n))) 
    (setq fntb (dxf 4 (tblsearch "style" n))) 
    (if    (or (eq "txt" fnts) (eq "txt.shx" fnts)) 
      (setq err 1) 
      (setq fntl (append fntl (list (strcase fnts)))) 
    ) ;_ 结束if 
    (if    (not (eq "" fntb)) 
      (setq fntl (append fntl (list (strcase fntb)))) 
    ) ;_ 结束if 
  ) ;_ 结束while 
  (setq fntl (append fntl (list (strcase (getvar "dwgname"))))) 

  (setq dwgn (findfile (getvar "dwgname"))) 

  (if err 
    (alert 
      (strcat 
    "\n                     ******  友  情  提  醒  ******" 
    "\n\n        本图中使用了“txt.shx”字体，在移动或复制实体时" 
    "\n\n可能会导致AutoCAD系统崩溃或其它未可知的致命错误！" 
    "\n\n共享版本不提供自动提取该字体文件的功能！" 
    "\n\n                                                    ----它山之石") ;_ 结束strcat 
    ) ;_ 结束alert 
    (setq err nil) 
  ) ;_ 结束if 
  (foreach x fntl 
    (if    (not (member x fntll)) 
      (setq fntll (append fntll (list x))) 
    ) ;_ 结束if 
  ) ;_ 结束foreach 
  (setq sty (length fntll)) 
  (foreach x fntll 
    (progn 
      (if x 
    (progn 
      (if (findfile x) 
        (setq x x) 
        (progn 
          (if (eq "TTF" (setq qq (substr x (- (strlen x) 2)))) 
        (progn 
          (alert 
            (strcat 
              "\n                     ******  友  情  提  醒  ******" 
              "\n\n        字体 " x " 可能是WINDOWS的系统字体，" 
              "\n\n使用此类字体会导致图形文件狂大，少用为宜。" 
              "\n\n共享版本不提供自动提取该字体文件的功能！" 
              "\n\n                                                    ----它山之石") ;_ 结束strcat 
          ) ;_ 结束alert 
          (setq ttf 1) 
        ) ;_ 结束progn 
        (setq x (strcat x ".shx")) 
          ) ;_ 结束if 
        ) ;_ 结束progn 
      ) ;_ 结束if 
      (if (setq fntf (findfile x)) 
        (progn 
          (setq fntc " c:\GELEI\") 

          (setq si 1 
            sii    0 
          ) ;_ 结束setq 
          (setq txt 1)        ;得到要查找字符串的长度 
          (while            ;在一个被查找字符串中循环查找 
        (= txt (strlen (setq st (substr fntf si txt)))) 
         (if (or (eq st "") (eq st ".")) 
                    ;如果查找到相匹配的字符串 
           (setq sii (+ si 1)) 
         ) ;_ 结束if 
         (if (> (- si sii) 7)    ;如果查找到相匹配的串 
           (progn 
             (setq stt (substr fntf sii (- si sii))) 
             (setq stt (strcat (substr stt 1 6) "~1")) 
             (setq fntf    (strcat    (substr fntf 1 (- sii 1)) 
                    stt 
                    (substr fntf (+ 1 si)) 
                ) ;_ 结束strcat 
             ) ;_ 结束setq 
             (setq si (- si sii 8)) 
             (command "SHELL" 
                  (strcat "XCOPY " fntf fntc " /y") 
             ) ;_ 结束command 
           ) ;_ 结束progn 
         ) ;_ 结束if 
         (setq si (+ 1 si)) 
          ) ;_ 结束while 
          (setq countp (+ countp 1)) 
        ) ;_ 结束progn 
        (progn 
          (if ttf 
        (setq ttf nil) 
        (alert 
          (strcat "n        在AutoCAD的搜索路径下未找到字体 " 
              x 
              " ，n" 
              "n本图可能也无法正确显示使用该字体的文本！n" 
          ) ;_ 结束strcat 
        ) ;_ 结束alert 
          ) ;_ 结束if 
        ) ;_ 结束progn 
      ) ;_ 结束if 
    ) ;_ 结束progn 
      ) ;_ 结束if 
    ) ;_ 结束progn 
  ) ;_ 结束foreach 

  (command "shell" 
       "DIR c:GELEI*.shx /s/a/B >C:GELEIGELEI" 
  ) ;_ 结束command 
  (setq lujing (open "C:GELEIGELEI" "r")) 
                    ;LUJING--保存带完全路径的文件名 
  (setq n 0) 
  (while 
    (read-line LUJING) 
     (setq n (+ n 1)) 

  )                    ;WENJIAN--带完全路径的文件名 
  (close LUJING) 
  (command "shell" "del C:GELEIGELEI") 


  (alert (strcat "n       本图中共定义了 " 
         (rtos ll 5) 
         " 个文字式样，使用到了" 
         (rtos sty 5) 
         " 个字体文件，nn其中" 
         (rtos n 5) 
         "个被提取到了 " 
         "C:GELEI 目录下" 
     ) ;_ 结束strcat 
  ) ;_ 结束alert 

  (princ (strcat "n***自动提取图中字体软件V031212***")) 
  (princ (strcat "n       [它山之石修正版]")) 
  (princ) 
) ;_ 结束defun
