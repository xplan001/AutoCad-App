;;;   CHTEXT.lsp
;;;   (C) 版权 1988-1992  Autodesk 公司
;;;
;;;   版权 (C) 1988-1992  Autodesk 公司
;;;
;;;   本程序已由 Autodesk 公司注册版权, 仅于下述情况下可授与许可.
;;;   用户不得以任何形式发行或出版此程序的源码; 但允许在特定的工作中
;;;   并入此程序使用.有关条件如下:
;;;
;;;   ( i)  设计上与工作上皆纯粹针对 Autodesk 公司的产品.
;;;   (ii)  载有 版权(C) 1988-1992  Autodesk 公司 的版权通告.
;;;
;;;
;;;
;;;       AUTODESK公司提供此程序仅供作类似的参考, 而并不排除有任何错误的
;;;   可能.AUTODESK公司特此否认任何特定用途之适应性, 以及商业销售所隐含
;;;   的保证。同时亦不作出此程序执行时一定不会中断或完全无误的保证.
;;;
;;;
;;;   by Jan S. Yoder
;;;   09 March  1990
;;;
;;;   REVISIONS
;;;   1.01  22 May 1991  DTP -- Minor bug fixes.
;;;   1.02  18 June 1991  JSY, DTP -- Minor bug fixes.
;;;
;;;--------------------------------------------------------------------------;
;;; DESCRIPTION
;;;   This is a "text processor" which operates in a global manner
;;;   on all of the text entities that the user selects; i.e., the
;;;   Height, Justification, Location, Rotation, Style, Text, and
;;;   Width can all be changed globally or individually, and the
;;;   range of values for a given parameter can be listed.
;;;
;;;   The command is called with CHT from the command line at which
;;;   time the user is asked to select the objects to change.
;;;
;;;     Select text to change.
;;;     Select objects:
;;;
;;;   If nothing is selected the message "ERROR: Nothing selected."
;;;   is displayed and the command is terminated.  If more than 25
;;;   entities are selected the following message is displayed while
;;;   the text entities are sorted out from the non-text entities.
;;;   A count of the text entities found is then displayed.
;;;
;;;     Verifying the selected entities -- please wait.
;;;     nnn  text entities found.
;;;     CHText:  Height/Justification/Location/Rotation/Style/Text/Undo/Width:
;;;
;;;   A typical example of the prompts you may encounter follows:
;;;
;;;   If you select a single text entity to change and ask to change
;;;   the height, the prompt looks like this:
;;;
;;;     CHText:  Height/Justification/Location/Rotation/Style/Text/Undo/Width:
;;;     New text height for text entity. <0.08750000>:
;;;
;;;   If you select more than one text entity to change and ask to change
;;;   the height, the prompt looks like this:
;;;
;;;     CHText:  Height/Justification/Location/Rotation/Style/Text/Undo/Width:
;;;     Individual/List/<New height for all entities>:
;;;
;;;   Typing "L" at this prompt returns a prompt showing you the range of
;;;   values that you are using for your text.
;;;
;;;     Height -- Min: 0.05000000  Max: 0.10000000  Ave: 0.08392857
;;;
;;;   Typing "I" at this prompt puts you in a loop, processing the text
;;;   entities you have selected one at a time, and giving the same prompt
;;;   you get for a single text entity shown above.
;;;
;;;   Pressing RETURN at this point puts you back at the Command: prompt.
;;;   Selecting any of the other options allows you to change the text
;;;   entities selected.
;;;
;;;   All of the Release 11 text alignment options have been supported.
;;;   This is based on the system variable "DIMCLRD" being present.
;;;   If it is not present, then only the  Release 10 alignment options
;;;   are allowed.
;;;
;;;---------------------------------------------------------------------------;
;;;
;;; Main function -- no arguments
;;;
(defun chtxt (/ sset opt ssl nsset temp unctr ct_ver cht_er cht_oe
                sslen style hgt rot txt ent cht_oc cht_ot cht_oh
                loc loc1 justp justq orthom )

  (setq ct_ver "1.02")                ; Reset this local if you make a change.
  ;;
  ;; Internal error handler defined locally
  ;;
  (defun cht_er (s)                   ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
  (if (/= msg "Function cancelled")
    (if (= msg "quit / exit abort")
        (princ)
        (princ (strcat "\n错误: " s))
      )
    )
    (eval(read U:E))
    (if cht_oe                        ; If an old error routine exists
      (setq *error* cht_oe)           ; then, reset it
    )
    (if temp (redraw temp 1))
    (if cht_oc (setvar "cmdecho" cht_oc)) ; Reset command echoing
    (if cht_ot (setvar "texteval" cht_ot))
    (if cht_oh (setvar "highlight" cht_oh))
    (princ)
  )
  ;;
  ;; Body of function
  ;;
  (if *error*                         ; Set our new error handler
    (setq cht_oe *error* *error* cht_er)
    (setq *error* cht_er)
  )

  ;; Set undo groups and ends with (eval(read U:G)) or (eval(read U:E))
  (setq U:G "(command \"undo\" \"group\")"
        U:E "(command \"undo\" \"en\")"
  )

  (setq cht_oc (getvar "cmdecho"))
  (setq cht_oh (getvar "highlight"))
  (setvar "cmdecho" 0)

  (eval(read U:G))

  (princ (strcat "\n文字编辑程序, 版本 " ct_ver
                 ", (c) 1990-1991 Autodesk 公司."))
  (prompt "\n选择要修改的文字.")
  (setq sset (ssget))
  (if (null sset)
    (progn
      (princ "\n错误: 没有选到文字.")
      (exit)
    )
  )
  ;; Verify the entity set.
  (cht_ve)
  ;; This is the main option loop.
  (cht_ol)

  (if cht_oe (setq *error* cht_oe))   ; Reset old error function if error
  (eval(read U:E))
  (if cht_ot (setvar "texteval" cht_ot))
  (if cht_oh (setvar "highlight" cht_oh))
  (if cht_oc (setvar "cmdecho" cht_oc)) ; Reset command echoing
  (princ)
)
;;;
;;; Verify and sort out non-text entities from the selection set.
;;;
(defun cht_ve ()
  (setq ssl   (sslength sset)
        nsset (ssadd))
  (if (> ssl 25)
    (princ "\n确认所选的图元 -- 请稍候. ")
  )
  (while (> ssl 0)
    (setq temp (ssname sset (setq ssl (1- ssl))))
    (if (= (cdr(assoc 0 (entget temp))) "TEXT")
      (ssadd temp nsset)
    )
  )
  (setq ssl (sslength nsset)
        sset nsset
        unctr 0
  )
  (princ "找到 ")
  (print ssl)
  (princ " 个文字图元.")
)
;;;
;;; The option loop.
;;;
(defun cht_ol ()
  (setq opt T)
  (while (and opt (> ssl 0))
    (setq unctr (1+ unctr))
    (command "_.UNDO" "_GROUP")
    (initget "Location Justification Style Height Rotation Width Text Undo")
    (setq opt (getkword
      "\nH字高/J对齐/L位置/R旋转/S字型/T文字/U退回/W宽度: "))
    (if opt
      (cond
        ((= opt "Undo")
          (cht_ue)                    ; Undo the previous command.
        )
        ((= opt "Location")
          (cht_le)                    ; Change the location.
        )
        ((= opt "Justification")
          (cht_je)                    ; Change the justification.
        )
        ((= opt "Style")    (cht_pe "Style"    "style name"      7) )
        ((= opt "Height")   (cht_pe "Height"   "height"         40) )
        ((= opt "Rotation") (cht_pe "Rotation" "rotation angle" 50) )
        ((= opt "Width")    (cht_pe "Width"    "width factor"   41) )
        ((= opt "Text")
          (cht_te)                    ; Change the text.
        )
      )
      (setq opt nil)
    )
    (command "_.UNDO" "_END")
  )
)
;;;
;;; Undo an entry.
;;;
(defun cht_ue ()
  (if (> unctr 1)
    (progn
      (command "_.UNDO" "_END")
      (command "_.UNDO" "2")
      (setq unctr (- unctr 2))
    )
    (progn
      (princ "\n退回命令已全部结束")
      (setq unctr (- unctr 1))
    )
  )
)
;;;
;;; Change the location of an entry.
;;;
(defun cht_le ()
  (setq sslen (sslength sset)
        style ""
        hgt   ""
        rot   ""
        txt   ""
  )
  (command "_.CHANGE" sset "" "")
  (while (> sslen 0)
    (setq ent (entget(ssname sset (setq sslen (1- sslen))))
          opt (list (cadr (assoc 11 ent))
                    (caddr (assoc 11 ent))
                    (cadddr (assoc 11 ent)))
    )
    (prompt "\n新的文字位置: ")
    (command pause)
    (if (null loc)
      (setq loc opt)
    )
    (command style hgt rot txt)
  )
  (command)
)
;;;
;;; Change the justification of an entry.
;;;
(defun cht_je ()
  (if (getvar "DIMCLRD")
    (initget (strcat "TLeft TCenter TRight "
                     "MLeft MCenter MRight "
                     "BLeft BCenter BRight "
                     "Aligned Center Fit Left Middle Right ?"))
    (initget "Aligned Center Fit Left Middle Right ?")
  )
  (setq sslen (sslength sset))
  (setq justp (getkword (strcat "\n定位点 - "
              "A对齐/C中/F调齐/L左/M正中/R右/<?>: ")))
  (cond
    ((= justp "Left")    (setq justp 0 justq 0) )
    ((= justp "Center")  (setq justp 1 justq 0) )
    ((= justp "Right")   (setq justp 2 justq 0) )
    ((= justp "Aligned") (setq justp 3 justq 0) )
    ((= justp "Fit")     (setq justp 5 justq 0) )
    ((= justp "TLeft")   (setq justp 0 justq 3) )
    ((= justp "TCenter") (setq justp 1 justq 3) )
    ((= justp "TRight")  (setq justp 2 justq 3) )
    ((= justp "MLeft")   (setq justp 0 justq 2) )
    ((= justp "Middle")  (setq justp 4 justq 0) )
    ((= justp "MCenter") (setq justp 1 justq 2) )
    ((= justp "MRight")  (setq justp 2 justq 2) )
    ((= justp "BLeft")   (setq justp 0 justq 1) )
    ((= justp "BCenter") (setq justp 1 justq 1) )
    ((= justp "BRight")  (setq justp 2 justq 1) )
    ((= justp "?")       (setq justp nil)       )
    (T                   (setq justp nil)       )
  )
  (if justp
    (justpt) ; Process them...
    (justpn) ; List options...
  )
  (command)
)
;;;
;;; Get alignment points for "aligned" or "fit" text.
;;;
(defun justpt ()
  (while (> sslen 0)
    (setq ent (entget(ssname sset (setq sslen (1- sslen))))
          ent (subst (cons 72 justp) (assoc 72 ent) ent)
          opt (trans (list (cadr (assoc 11 ent))
                           (caddr (assoc 11 ent))
                           (cadddr (assoc 11 ent)))
                     (cdr(assoc -1 ent)) ; from ECS
                     1)               ; to current UCS
    )
    (if (getvar "DIMCLRD")
      (setq ent (subst (cons 73 justq) (assoc 73 ent) ent))
    )
    (cond
      ((or (= justp 3) (= justp 5))
        (prompt "\n新的文字定位点: ")
        (if (= (setq orthom (getvar "orthomode")) 1)
          (setvar "orthomode" 0)
        )
        (redraw (cdr(assoc -1 ent)) 3)
        (initget 1)
        (setq loc (getpoint))
        (initget 1)
        (setq loc1 (getpoint loc))
        (redraw (cdr(assoc -1 ent)) 1)
        (setvar "orthomode" orthom)
        (setq ent (subst (cons 10 loc) (assoc 10 ent) ent))
        (setq ent (subst (cons 11 loc1) (assoc 11 ent) ent))
      )
      ((or (/= justp 0) (/= justq 0))
        (redraw (cdr(assoc -1 ent)) 3)
        (prompt "\n新的文字位置: ")
        (if (= (setq orthom (getvar "orthomode")) 1)
          (setvar "orthomode" 0)
        )
        (setq loc (getpoint opt))
        (setvar "orthomode" orthom)
        (redraw (cdr(assoc -1 ent)) 1)
        (if (null loc)
          (setq loc opt)
          (setq loc (trans loc 1 (cdr(assoc -1 ent))))
        )
        (setq ent (subst (cons 11 loc) (assoc 11 ent) ent))
      )
    )
    (entmod ent)
  )
)
;;;
;;; List the options.
;;;
(defun justpn ()
  (if (getvar "DIMCLRD") (textpage))
  (princ "\n定位选项: ")
  (princ "\n\t TL顶左   TC顶中   TR顶右 ")
  (princ "\n\t ML中左   MC中心   MR中右 ")
  (princ "\n\t BL底左   BC底中   BR底右 ")
  (princ "\n\t  L左      C中      R右 ")
  (princ "\n\t  A对齐    M正中    F调齐 ")
  (if (not (getvar "DIMCLRD")) (textscr))
  (princ "\n\n按任意键返回图形屏幕.")
  (grread)
  (princ "\r                                           ")
  (graphscr)
)
;;;
;;; Change the text of an entity.
;;;
(defun cht_te ()
  (setq sslen (sslength sset))
  (initget "Globally Individually Retype")
  (setq ans (getkword
    "\n搜索及置换文字. I个别/R重新键入/<G整体>:"))
  (setq cht_ot (getvar "texteval"))
  (setvar "texteval" 1)
  (cond
    ((= ans "Individually")
      (if (= (getvar "popups") 1)
        (progn
          (initget "Yes No")
          (setq ans (getkword "\n以对话框编辑文字? <Yes>:"))
        )
        (setq ans "No")
      )

      (while (> sslen 0)
        (redraw (setq sn (ssname sset (setq sslen (1- sslen)))) 3)
        (setq ss (ssadd))
        (ssadd (ssname sset sslen) ss)
        (if (= ans "No")
          (chgtext ss)
          (command "_.DDEDIT" sn "")
        )
        (redraw sn 1)
      )
    )
    ((= ans "Retype")
      (while (> sslen 0)
        (setq ent (entget(ssname sset (setq sslen (1- sslen)))))
        (redraw (cdr(assoc -1 ent)) 3)
        (prompt (strcat "\n旧文字: " (cdr(assoc 1 ent))))
        (setq nt (getstring  T "\n新文字: "))
        (redraw (cdr(assoc -1 ent)) 1)
        (if (> (strlen nt) 0)
          (entmod (subst (cons 1 nt) (assoc 1 ent) ent))
        )
      )
    )
    (T
      (chgtext sset)                  ; Change 'em all
    )
  )
  (setvar "texteval" cht_ot)
)
;;;
;;; The old CHGTEXT command - rudimentary text editor
;;;
;;;
(defun C:CHGTEXT () (chgtext nil))

(defun chgtext (objs / last_o tot_o ent o_str n_str st s_temp
                       n_slen o_slen si chf chm cont ans)
  (if (null objs)
    (setq objs (ssget))               ; Select objects if running standalone
  )
  (setq chm 0)
  (if objs
    (progn                   ; If any objects selected
      (if (= (type objs) 'ENAME)
        (progn
          (setq ent (entget objs))
          (princ (strcat "\n现有的字串: " (cdr (assoc 1 ent))))
        )
        (if (= (sslength objs) 1)
          (progn
            (setq ent (entget (ssname objs 0)))
            (princ (strcat "\n现有的字串: " (cdr (assoc 1 ent))))
          )
        )
      )
      (setq o_str (getstring "\nMatch string   : " t))
      (setq o_slen (strlen o_str))
      (if (/= o_slen 0)
        (progn
          (setq n_str (getstring "\n新字串    : " t))
          (setq n_slen (strlen n_str))
          (setq last_o 0
                tot_o  (if (= (type objs) 'ENAME)
                         1
                         (sslength objs)
                       )
          )
          (while (< last_o tot_o)     ; For each selected object...
            (if (= "TEXT"             ; Look for TEXT entity type (group 0)
                   (cdr (assoc 0 (setq ent (entget (ssname objs last_o))))))
              (progn
                (setq chf nil si 1)
                (setq s_temp (cdr (assoc 1 ent)))
                (while (= o_slen (strlen (setq st (substr s_temp si o_slen))))
                  (if (= st o_str)
                    (progn
                      (setq s_temp (strcat
                                     (if (> si 1)
                                       (substr s_temp 1 (1- si))
                                       ""
                                     )
                                     n_str
                                     (substr s_temp (+ si o_slen))
                                   )
                      )
                      (setq chf t)    ; Found old string
                      (setq si (+ si n_slen))
                    )
                    (setq si (1+ si))
                  )
                )
                (if chf
                  (progn              ; Substitute new string for old
                    ; Modify the TEXT entity
                    (entmod (subst (cons 1 s_temp) (assoc 1 ent) ent))
                    (setq chm (1+ chm))
                  )
                )
              )
            )
            (setq last_o (1+ last_o))
          )
        )
        ;; else go on to the next line...
      )
    )
  )
  (if (/= (type objs) 'ENAME)
    (if (/= (sslength objs) 1)        ; Print total lines changed
      (princ (strcat "已改变 "
                     (rtos chm 2 0)
                     " 行文字."
             )
      )
    )
  )
  (terpri)
)
;;;
;;; Main procedure for manipulating text entities
;;; ARGUMENTS:
;;;   typ   -- Type of operation to perform
;;;   prmpt -- Partial prompt string to insert in standard prompt line
;;;   fld   -- Assoc field to be changed
;;; GLOBALS:
;;;   sset  -- The selection set of text entities
;;;
(defun cht_pe (typ prmpt fld / temp ow nw ent tw sty w hw lw
                              sslen n sn ssl)
  (if (= (sslength sset) 1)           ; Special case if there is only
                                      ; one entity selected
    ;; Process one entity.
    (cht_p1)
    ;; Else
    (progn
      ;; Set prompt string.
      (cht_sp)
      (if (= nw "List")
        ;; Process List request.
        (cht_pl)
        (if (= nw "Individual")
          ;; Process Individual request.
          (cht_pi)
          (if (= nw "Select")
            ;; Process Select request.
            (cht_ps)
            ;; Else
            (progn
              (if (= typ "Rotation")
                (setq nw (* (/ nw 180.0) pi))
              )
              (if (= (type nw) 'STR)
                (if (not (tblsearch "style" nw))
                  (progn
                    (princ (strcat "\n找不到" nw "字型."))
                  )
                  (cht_pa)
                )
                (cht_pa)
              )
            )
          )
        )
      )
    )
  )
)
;;;
;;; Change all of the entities in the selection set.
;;;
(defun cht_pa (/ cht_oh temp)
  (setq sslen (sslength sset))
  (setq cht_oh (getvar "highlight"))
  (setvar "highlight" 0)
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent
            )
    )

  )
  (setvar "highlight" cht_oh)
)
;;;
;;; Change one text entity.
;;;
(defun cht_p1 ()
  (setq temp (ssname sset 0))
  (setq ow (cdr(assoc fld (entget temp))))
  (if (= opt "Rotation")
    (setq ow (/ (* ow 180.0) pi))
  )
  (redraw (cdr(assoc -1 (entget temp))) 3)
  (initget 0)
  (if (= opt "Style")
    (setq nw (getstring (strcat "\n新的 " prmpt ". <"
                              ow ">: ")))
    (setq nw (getreal (strcat "\n新的 " prmpt ". <"
                              (rtos ow 2) ">: ")))
  )
  (if (or (= nw "") (= nw nil))
    (setq nw ow)
  )
  (redraw (cdr(assoc -1 (entget temp))) 1)
  (if (= opt "Rotation")
    (setq nw (* (/ nw 180.0) pi))
  )
  (if (= opt "Style")
    (if (null (tblsearch "style" nw))
      (princ (strcat "\n找不到" nw "字型."))

      (entmod (subst (cons fld nw)
                     (assoc fld (setq ent (entget temp)))
                     ent
              )
      )
    )
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent
            )
    )
  )
)
;;;
;;; Set the prompt string.
;;;
(defun cht_sp ()
  (if (= typ "Style")
    (progn
      (initget "Individual List New Select ")
      (setq nw (getkword (strcat "\nI个别/L列出/S选择字型/<N新的 "
                                 prmpt
                                 " 对所有的文字图元>: ")))
      (if (or (= nw "") (= nw nil) (= nw "Enter"))
        (setq nw (getstring (strcat "\nN新的 "
                                    prmpt
                                    " 对所有的文字图元: ")))
      )
    )
    (progn
      (initget "List Individual" 1)
      (setq nw (getreal (strcat "\nI个别/L列出/<新的 "
                                 prmpt
                                 " 对所有的文字图元>: ")))
    )
  )
)
;;;
;;; Process List request.
;;;
(defun cht_pl ()
  (setq unctr (1- unctr))
  (setq sslen (sslength sset))
  (setq tw 0)
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (if (= typ "Style")
      (progn
        (if (= tw 0)
          (setq tw (list (cdr(assoc fld (entget temp)))))
          (progn
            (setq sty (cdr(assoc fld (entget temp))))
            (if (not (member sty tw))
              (setq tw (append tw (list sty)))
            )
          )
        )
      )
      (progn
        (setq tw (+ tw (setq w (cdr(assoc fld (entget temp))))))
        (if (= (sslength sset) (1+ sslen)) (setq lw w hw w))
        (if (< hw w) (setq hw w))
        (if (> lw w) (setq lw w))
      )
    )
  )
  (if (= typ "Rotation")
    (setq tw (* (/ tw pi) 180.0)
          lw (* (/ lw pi) 180.0)
          hw (* (/ hw pi) 180.0))
  )
  (if (= typ "Style")
    (progn
      (princ (strcat "\n"
                     typ
                     "(s) -- "))
      (princ tw)
    )
    (princ (strcat "\n"
                     typ
                     " -- 最小: "
                     (rtos lw 2)
                     "\t 最大: "
                     (rtos hw 2)
                     "\t 平均: "
                     (rtos (/ tw (sslength sset)) 2) ))
  )
)
;;;
;;; Process Individual request.
;;;
(defun cht_pi ()
  (setq sslen (sslength sset))
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (setq ow (cdr(assoc fld (entget temp))))
    (if (= typ "Rotation")
      (setq ow (/ (* ow 180.0) pi))
    )
    (initget 0)
    (redraw (cdr(assoc -1 (entget temp))) 3)
    (if (= typ "Style")
      (progn
        (setq nw (getstring (strcat "\n新的 "
                                   prmpt
                                   ". <"
                                   ow ">: ")))
      )
      (progn
        (setq nw (getreal (strcat "\n新的 "
                                   prmpt
                                   ". <"
                                (rtos ow 2) ">: ")))
      )
    )
    (if (or (= nw "") (= nw nil))
      (setq nw ow)
    )
    (if (= typ "Rotation")
      (setq nw (* (/ nw 180.0) pi))
    )
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent
            )
    )
    (redraw (cdr(assoc -1 (entget temp))) 1)
  )
)
;;;
;;; Process the Select option.
;;;
(defun cht_ps ()
  (princ "\n搜索字型名称?  <*>: ")
  (setq sn  (strcase (getstring))
        n   -1
        nsset (ssadd)
        ssl (1- (sslength sset))
        )
  (if (or (= sn "*") (null sn) (= sn ""))
    (setq nsset sset sn "*")
    (while (and sn (< n ssl))
      (setq temp (ssname sset (setq n (1+ n))))
      (if (= (cdr(assoc 7 (entget temp))) sn)
        (ssadd temp nsset)
      )
    )
  )
  (setq ssl (sslength nsset))
  (princ "\n找到 ")
  (princ ssl)
  (princ " 个")
  (princ sn)
  (princ "字型的文字图元")
  (princ ".")
)
;;;
;;; The C: function definition.
;;;
(defun c:cht    () (chtxt))
(princ "\n\tc:CHText 已加载; 以CHT启动命令.")
(princ)
