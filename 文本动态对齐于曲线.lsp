
                                                                              ;;;;                                                                               ;;
;;                     --=={  Dynamic Text Curve Align  }==--                    ;;
;;                                                                               ;;
;;  The Program will prompt the user to either Select existing text to align,    ;;
;;  or specify New Text. The user will then be prompted to select a curve, and   ;;
;;  the text specified/selected will be dynamically aligned to the selected      ;;
;;  curve.                                                                       ;;
;;                                                                               ;;
;;  Additionally, the user can press +/- to alter the text offset from the curve ;;
;;  and furthermore, toggle the text perpendicularity by pressing 'P' during     ;;
;;  text alignment. A Background Mask can be toggled by pressing 'B' when        ;;
;;  aligning MText. The Text can also be mirrored by pressing 'M' during text    ;;
;;  alignment. The TextStyle and Text Height may be altered by pressing 'S' when ;;
;;  aligning text.                                                               ;;
;;                                                                               ;;
;;  DTRemove:-                                                                   ;;
;;  ------------                                                                 ;;
;;  This function allows the user to remove Text/Object Associativity. Upon      ;;
;;  invoking this function, the user is prompted to select either Text or        ;;
;;  object.                                                                      ;;
;;                                                                               ;;
;;  If the user picks an object, all text associativity with that object is      ;;
;;  removed. If the user picks a text object, the associativity between the      ;;
;;  text and the object that it was aligned with, is removed.                    ;;
;;                                                                               ;;
;;  The user can also press 'A' to clear all associativity within the drawing.   ;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  DTCurve/DTRemove                                           ;;
;;                                                                               ;;
;;  Notes:-                                                                      ;;
;;  ------------                                                                 ;;
;;  Text and MText Justification will be changed to Middle Center Justification  ;;
;;  to allow text to be correctly aligned.                                       ;;
;;                                                                               ;;
;;  Background Mask functionality can only be used when aligning MText.          ;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  AUTHOR:                                                                      ;;
;;                                                                               ;;
;;  Copyright ?Lee McDonnell, November 2009. All Rights Reserved.               ;;
;;                                                                               ;;
;;      { Contact: Lee Mac @ TheSwamp.org, CADTutor.net }                        ;;
;;                                                                               ;;
;;  WITH ADDITIONAL THANKS TO:                                                   ;;
;;                                                                               ;;
;;  ? Luis Esquivel (LE)                                                        ;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  VERSION:                                                                     ;;
;;                                                                               ;;
;;    ?1.0   ~才   6th November 2009   ~才   ?First Release                    ;;
;;...............................................................................;;
;;    ?1.1   ~才  13th November 2009   ~才   ?Fixed bug that allowed text to   ;;
;;                                              aligned to multiple objects.     ;;
;;...............................................................................;;
;;    ?1.2   ~才  15th November 2009   ~才   ?Added DTRemove to remove align   ;;
;;                                              associativity.                   ;;
;;...............................................................................;;
;;    ?1.3   ~才  15th November 2009   ~才   ?Added Ability to Align MText.    ;;
;;...............................................................................;;
;;    ?1.4   ~才  16th November 2009   ~才   ?Added Ability to use a           ;;
;;                                              Background Mask, when aligning   ;;
;;                                              MText.                           ;;
;;...............................................................................;;
;;    ?1.5   ~才  16th November 2009   ~才   ?General Bug Fixes.               ;;
;;...............................................................................;;
;;    ?1.6   ~才  17th November 2009   ~才   ?Updated code to work in all UCS. ;;
;;...............................................................................;;
;;    ?1.7   ~才  17th November 2009   ~才   ?Added Logo to DTRemove function. ;;
;;...............................................................................;;
;;    ?1.8   ~才  17th November 2009   ~才   ?Fixed Bug to allow correct text  ;;
;;                                              offset/perpendicularity setting  ;;
;;                                              when updating text by Reactor.   ;;
;;                                            ?General Bug Fixes.               ;;
;;...............................................................................;;
;;    ?1.9   ~才  17th November 2009   ~才   ?Fixed Text Rotation bug, for     ;;
;;                                              Vertical lines.                  ;;
;;...............................................................................;;
;;    ?2.0   ~才  17th November 2009   ~才   ?General Bug Fixes.               ;;
;;                                            ?Added ability for user to        ;;
;;                                              manually input offset distance.  ;;
;;...............................................................................;;
;;    ?2.1   ~才  19th November 2009   ~才   ?DTRemove updated to remove all   ;;
;;                                              XData from object.               ;;
;;...............................................................................;;
;;    ?2.2   ~才  20th November 2009   ~才   ?Added Mirror Text Option.        ;;
;;...............................................................................;;
;;    ?2.3   ~才  25th November 2009   ~才   ?Added TextStyle/Height Dialog.   ;;
;;...............................................................................;;
;;    ?2.4   ~才   2nd December 2009   ~才   ?Fixed Dialog Bug.                ;;
;;...............................................................................;;
;;    ?2.5   ~才   3rd December 2009   ~才   ?Added Selection highlighting.    ;;
;;...............................................................................;;
;;    ?2.6   ~才   4th December 2009   ~才   ?Upgraded DTRemove function to    ;;
;;                                              include option to Remove All     ;;
;;                                              associativity from objects.      ;;
;;...............................................................................;;
;;    ?2.7   ~才   7th December 2009   ~才   ?Re-Structured xData Storage to   ;;
;;                                              allow text to be aligned to same ;;
;;                                              part of object upon re-alignment ;;
;;...............................................................................;;
;;    ?2.8   ~才  15th February 2010   ~才   ?Fixed realignment bug when line  ;;
;;                                              length is less than text.        ;;
;;                                            ?General Bug Fixes.               ;;
;;...............................................................................;;
;;    ?2.9   ~才  16th February 2010   ~才   ?Updated Code Layout.             ;;
;;                                            ?Added call to turn off reactor   ;;
;;                                              whilst main function is running. ;;
;;...............................................................................;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
                                                                               ;;



(setq app "DTCURVE")                           ;; XData Application Name

(or *Mac$Str*  (setq *Mac$Str* "text"))        ;; First-time Default Text String
(or *Mac$Per*  (setq *Mac$Per* (/ pi 2.)))     ;; Default Perpendicularity Setting
(or *Mac$tOff* (setq *Mac$tOff* 1.))           ;; Default Offset Factor
(or *Mac$Mirr* (setq *Mac$Mirr* 0.))           ;; Default Mirror Setting
(setq *Mac$tObj* "MTEXT")                      ;; Default Text Object to be Created [TEXT/MTEXT]

(setq *Mac$Mask* nil)                          ;; Background Mask as Default [t/nil]

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=




(defun c:DTC (/  ; --=={  Local Functions  }==--

                       *error* putxdat

                      ; --=={  Local Variables  }==--

                       CANG COBJ CODE
                       DATA DC_FNAME DC_TITLE DERV DIS DOC DRFT
                       ENT
                       GR
                       HENT HI HND
                       LANG
                       MSG
                       ONME OPT OSGRV OV
                       PLST PT
                       RSLT
                       SPC STR
                       TOBJ TSTR TSZE TYP
                       UFLAG
                       VAL VL
                       XDAT XTYPE XVAL

                      ; --=={  Global Variables  }==--

                      ;  *Mac$Str*   ~  Default Text String
                      ;  *Mac$tOff*  ~  Default Offset Factor
                      ;  *Mac$Per*   ~  Default Perpendicularity Setting
                      ;  *Mac$tObj*  ~  Default Object Creation Setting (TEXT/MTEXT)
                      ;  *Mac$Mask*  ~  Default Background Mask Setting
                      ;  *Mac$Mirr*  ~  Default Text Mirror Setting

                   )
  
  (vl-load-com)

  (setq dc_fname "LMAC_DTCurve_V2.9.dcl"
        dc_title "DTCurve V2.9 Settings")
  

  ;;  --=={  Error Handler  }==--

  (defun *error* (msg)
    
    (and tObj
         (or
           (and pLst
                (mapcar
                  (function
                    (lambda (x)
                      (vlax-put tObj (car x) (cdr x)))) pLst))
             (and
               (not
                 (vlax-erased-p tObj)) (vla-delete tObj))))

    (mapcar (function set) '(tObj pLst) (list 'nil 'nil))
    (if (and uflag doc) (vla-EndUndoMark doc))

    ;(vl-bt)

    (if (not (vlr-added-p *DCurve$Align$Reactor*))
      (vlr-add *DCurve$Align$Reactor*))

    (and ov (mapcar (function setvar) vl ov))
    
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    
    (redraw)

    (princ))
  
  
  ;;...............................................................................;;


  (defun putxdat (Obj App Data / xtype xval)

    (setq xtype
      (vlax-make-variant
        (vlax-safearray-fill
          (vlax-make-safearray
            vlax-vbInteger '(0 . 1)) '(1001 1000))))

    (setq xval
      (vlax-make-variant
        (vlax-safearray-fill
          (vlax-make-safearray
            vlax-vbVariant '(0 . 1)) (list App Data))))

    (vla-setXData Obj xtype xval))
  

  ;;...............................................................................;;
  

  (if (eq 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (getvar "CLAYER"))))))
    (progn
      (princ "\n<< Current Layer Locked >>") (exit)))
  

  (setq doc (vla-get-ActiveDocument
              (vlax-get-Acad-Object))
        
        spc (if (zerop (vla-get-activespace doc))
              (if (= (vla-get-mspace doc) :vlax-true)
                (vla-get-modelspace doc)
                (vla-get-paperspace doc))
              (vla-get-modelspace doc))
        
        drft  (vla-get-drafting
                (vla-get-preferences
                  (vlax-get-acad-object)))
        
        osGrv (osmode-grvecs-lst
                (vla-get-AutoSnapMarkerColor drft)
                  (vla-get-AutoSnapMarkerSize drft)))
  

  (setq vl '("OSMODE") ov (mapcar (function getvar) vl))

  (setq str "")
  
  ;;...............................................................................;;
  

  ;; LE - disable the reactor here to avoid the call to any notifier
  ;; must be before the selection of a text
  (if (vlr-added-p *DCurve$Align$Reactor*) (vlr-remove *DCurve$Align$Reactor*))
  

  ;;...............................................................................;;

  ;;  --=={  Text Entry/Selection  }==-- 

  (princ (setq msg (strcat "\nType or Select Text <" *Mac$Str* "> : ")))
  
  (while
    (progn
      (setq gr (grread 't 15 2) code (car gr) data (cadr gr))

      (cond (  (and (= 5 code) (listp data))

               (if (and (setq hi (car (nentselp data)))
                        (vl-position (cdr (assoc 0 (entget hi))) '("MTEXT" "TEXT")))
                 
                 (redraw (setq hEnt hi) 3)
                 
                 (if hEnt (setq hEnt (redraw hEnt 4))))
             
               t)

            (  (and (= 3 code) (listp data))

               (cond (  (setq ent (car (nentselp data)))

                        (if (not (vl-position (cdr (assoc 0 (entget ent))) '("MTEXT" "TEXT")))

                          (princ (strcat "\n** Object Must be Text/MText **" msg))
                          (progn
                            (if hEnt (setq hEnt (redraw hEnt 4)))

                            (setq rslt ent ent nil))))

                     (t (princ (strcat "\n** Missed, Try Again... **" msg)))))

            (  (= 25 code)

               (setq rslt str str nil))

            (  (= 2 code)

               (cond (  (<= 32 data 126)

                        (setq str (strcat str (princ (chr data)))))

                     (  (and (< 0 (strlen str)) (= 8 data))

                        (setq str (substr str 1 (1- (strlen str))))
                        (princ (vl-list->string '(8 32 8))))

                     (  (= 13 data)

                        (setq rslt str str nil))

                     (t )))
            
             (t ))))
  

  ;;...............................................................................;;

  ;;  --=={  Functions to Handle Result of User Input  }==-- 

  (vla-StartUndoMark doc)
  (setq uFlag t)  

  (cond (  (eq "" rslt))

        (  (eq 'STR (type rslt))
         
           (setq *Mac$Str* rslt))

        (  (eq 'ENAME (type rslt))

           (setq *Mac$Str* (vla-get-TextString
                             (setq tObj (vlax-ename->vla-object rslt)))
                 
                 tSze (vla-get-Height tObj)

                 Hnd  (vla-get-Handle tObj))
                 

           (cond (  (eq "AcDbText" (vla-get-ObjectName tObj))

                    (setq pLst (list (cons 'Alignment (vlax-get tObj 'Alignment))
                                     
                                     (if (eq acAlignmentLeft (vla-get-Alignment tObj))
                                       (cons 'InsertionPoint (vlax-get tObj 'InsertionPoint))
                                       (cons 'TextAlignmentPoint (vlax-get tObj 'TextAlignmentPoint))))))

                 (t (setq pLst (list (cons 'AttachmentPoint (vlax-get tObj 'AttachmentPoint))
                                     (cons 'InsertionPoint  (vlax-get tObj 'InsertionPoint))))))


           (foreach obj (vlr-owners *DCurve$Align$Reactor*)

             (if (not (vlax-erased-p obj))
               (progn

                 (vla-GetXData obj app 'typ 'val)

                 (if (and typ val)
                   (progn

                     (setq xDat

                            (cond (  (not (setq xDat
                                            (vlax-variant-value
                                              (cadr
                                                (vlax-safearray->list val)))))

                                     nil)

                                  (  (vl-remove-if-not
                                       (function
                                         (lambda (lst)
                                           (entget (handent (car lst)))))

                                       (read xDat)))

                                  (t nil)))
                     
                     (if (vl-position Hnd (mapcar (function car) xDat))
                       (progn
                         (putxDat obj app
                           (vl-prin1-to-string
                             (vl-remove-if
                               (function
                                 (lambda (x) (eq Hnd (car x)))) xDat))))))))

               (vlr-owner-remove *DCurve$Align$Reactor* obj)))))
  

  (setq tStr *Mac$Str*)
  (or tSze (setq tSze (getvar 'TEXTSIZE)))
  

  ;;...............................................................................;;

  ;;  --=={  Curve Selection  }==-- 

  (while
    (progn
      (setq ent (nentsel "\nSelect Curve: "))

      (cond (  (vl-consp ent)

               (if (vl-catch-all-error-p
                     (vl-catch-all-apply
                       (function vlax-curve-getEndParam)
                         (list (setq cObj (vlax-ename->vla-object (car ent))))))
                 
                 (princ "\n** Object not Valid **")))

            (t (princ "\n** Missed, Try Again... **")))))
  

  ;;...............................................................................;;

  ;;  --=={  Text Setup  ~  Alignment/Creation  }==-- 

  (cond (tObj

          (if (eq "AcDbText" (vla-get-ObjectName tObj))
            (vla-put-Alignment tObj acAlignmentMiddleCenter)
            (vla-put-attachmentpoint tObj acAttachmentPointMiddleCenter)))

        (t

          (if (eq "TEXT" (strcase *Mac$tObj*))
            
            (vla-put-Alignment
              (setq tObj (vla-addText spc tStr (vlax-3D-point '(0 0 0)) tSze)) acAlignmentMiddleCenter)
            
            (vla-put-AttachmentPoint
              (setq tObj (vla-addMText spc (vlax-3D-point '(0 0 0)) 0 tStr)) acAttachmentPointMiddleCenter))))
  

  (setq oNme (vla-get-ObjectName tObj))

  (and *Mac$Mask* (eq "AcDbMText" oNme)
       (vla-put-BackgroundFill tObj :vlax-true))

  ;; Program doesn't work too well with NODE/INSERTION Snap enabled

  (setvar "OSMODE" (boole 2 (getvar "OSMODE") 72))
  

  ;;...............................................................................;;

 

  (princ (setq msg (strcat "\n[+] or [-] for [O]ffset, [P]erpendicularity Toggle"
                           "\n[M]irror Text, [S]tyle Settings"
                           (if (eq "AcDbText" oNme) "" ", [B]ackground Mask"))))

  (while
    (progn
      (setq gr (grread t 15 0) code (car gr) data (cadr gr))

      (redraw)        

      (cond (  (and (= 5 code) (listp data))
               (setq data (trans data 1 0))

               (setq pt (vlax-curve-getClosestPointto cObj data))

               (if (and (< 0 (getvar "OSMODE") 16384)
                        (setq oPt (vl-remove-if (function null)
                                    (mapcar
                                      (function
                                        (lambda (x / o)
                                          (if (setq o (osnap data x))
                                            (list (distance data o) o x data)))) (get_osmode)))))
                 
                 (setq oPt (cdar (vl-sort oPt (function (lambda (a b) (< (car a) (car b)))))))
                 (setq oPt nil))

               (and oPt (OsMark oPt))

               (setq cAng (angle pt data) lAng (+ cAng *Mac$Per*))

               (if (equal lAng (/ pi 2.) 0.001)
                 (setq lAng (/ pi 2.)))

               (if (equal lAng (/ (* 3 pi) 2.) 0.001)
                 (setq lAng (/ (* 3 pi) 2.)))

               (cond (  (and (> lAng (/ pi 2)) (<= lAng pi))
                        (setq lAng (- lAng pi)))
                   
                     (  (and (> lAng pi) (<= lAng (/ (* 3 pi) 2)))
                        (setq lAng (+ lAng pi))))

               (  (if (eq "AcDbText" oNme)
                    vla-put-TextAlignmentPoint
                    vla-put-InsertionPoint)
                 
                 tObj (vlax-3D-point (polar pt cAng (* tSze *Mac$tOff*))))

               (vla-put-Rotation tObj (+ *Mac$Mirr* lAng))

             t)

            (  (= 2 code)

               (cond (  (vl-position data '(43 61))
                      
                        (setq *Mac$tOff* (+ 0.1 *Mac$tOff*)))

                     (  (= data 45)
                      
                        (setq *Mac$tOff* (-  *Mac$tOff* 0.1)))

                     (  (vl-position data '(77 109))

                        (setq *Mac$Mirr* (- pi *Mac$Mirr*)))

                     (  (vl-position data '(83 115))

                        (TextSettings tObj)
                        (setq tSze (vla-get-Height tObj))

                        (princ "\n  ~才 ~才 ~才 ~才 ~才 ~才 ~才 ~才 ~才 ~才 ~才 ~才 ~才")
                        (princ msg))

                     (  (vl-position data '(79 111))

                        (if (setq dis (getdist
                                        (strcat "\nSpecify Offset Distance <"
                                                (rtos (* tSze *Mac$tOff*) 2 3) "> : ")))
                          (setq *Mac$tOff* (/ dis (float tSze))))

                        (princ msg))

                     (  (vl-position data '(80 112))
                      
                        (setq *Mac$Per* (- (/ pi 2.) *Mac$Per*)))

                     (  (vl-position data '(66 98))

                        (if (eq oNme "AcDbMText")
                          (vla-put-backgroundfill tObj
                            (if (eq :vlax-true (vla-get-BackGroundFill tObj)) :vlax-false :vlax-true)))

                        t)

                     (  (= 6 data)  ; F3
                      
                        (cond (  (< 0 (getvar "OSMODE") 16384)                               
                                 (setvar "OSMODE" (+ 16384 (getvar "OSMODE")))
                                 (princ "\n<Osnap off>"))
                              
                              (t (setvar "OSMODE" (- (getvar "OSMODE") 16384))
                                 (princ "\n<Osnap on>")))

                        (princ msg))
                     
                     (  (vl-position data '(13 32))

                        (SubData)

                      nil)

                     (t )))

            (  (and (= 3 code) (listp data))
               (setq data (trans data 1 0))

               (SubData)

               (if (and (< 0 (getvar "OSMODE") 16384)
                        (setq oPt (vl-remove-if (function null)
                                    (mapcar
                                      (function
                                        (lambda (x / o)
                                          (if (setq o (osnap data x))
                                            (list (distance data o) o x data)))) (get_osmode)))))
                 
                 (setq oPt  (cdar  (vl-sort oPt (function (lambda (a b) (< (car a) (car b))))))
                       pt   (osnap (caddr oPt) (cadr oPt))

                       derv (angle '(0 0 0) (vlax-curve-getFirstDeriv cObj
                                              (vlax-curve-getParamatPoint cObj
                                                (vlax-curve-getClosestPointto cObj pt))))

                       derv (- (+ derv (if (< (distance (polar pt derv 1) data)
                                              (distance (polar pt (+ pi derv) 1) data)) 0 pi)) (/ pi 2.))))

               (setq pt   (vlax-curve-getClosestPointto cObj data) cAng (if derv derv (angle pt data)) lAng (+ cAng *Mac$Per*))

               (if (equal lAng (/ pi 2.) 0.001)
                 (setq lAng (/ pi 2.)))

               (if (equal lAng (/ (* 3 pi) 2.) 0.001)
                 (setq lAng (/ (* 3 pi) 2.)))

               (cond (  (and (> lAng (/ pi 2)) (<= lAng pi))
                        (setq lAng (- lAng pi)))

                     (  (and (> lAng pi) (<= lAng (/ (* 3 pi) 2)))
                        (setq lAng (+ lAng pi))))

               (  (if (eq "AcDbText" oNme)
                    vla-put-TextAlignmentPoint
                    vla-put-InsertionPoint)

                  tObj (vlax-3D-point (polar pt cAng (* tSze *Mac$tOff*))))

               (vla-put-Rotation tObj (+ *Mac$Mirr* lAng))

             nil)

            (  (= 25 code) nil)

            (t ))))
  

  ;;...............................................................................;;


  (vla-EndUndoMark doc)
  (redraw)

  ;; LE - make the reactor active here - we are safe to allow the chance
  ;; to update the possible text around the notifiers (lines) when they are modified
  (if (not (vlr-added-p *DCurve$Align$Reactor*)) (vlr-add *DCurve$Align$Reactor*))

  (mapcar (function setvar) vl ov)
  (princ))





(defun SubData (/ p# t#)
  
  (vla-GetXData cObj app 'typ 'val)
  (setq hand (list (vla-get-Handle tObj)
                   (vla-get-Handle cObj) *Mac$tOff* *Mac$Per* *Mac$Mirr*
                   (vlax-curve-getParamatPoint cObj
                     (setq p#
                       (vlax-curve-getClosestPointto cObj
                         (setq t#
                           (vlax-safearray->list
                             (vlax-variant-value
                               (  (if (eq "AcDbText" (vla-get-ObjectName tObj))
                                    vla-get-TextAlignmentPoint
                                    vla-get-InsertionPoint)

                                 tObj)))))))
                   
                   (- (angle p# t#)
                      (angle '(0 0 0)
                        (vlax-curve-getFirstDeriv cObj
                          (vlax-curve-getParamatPoint cObj p#))))))
  (if (and typ val)
    (progn
      
;;;      (setq xdat (apply (function mapcar)
;;;                        (cons (function cons)
;;;                              (list (vlax-safearray->list typ)
;;;                                    (mapcar (function vlax-variant-value)
;;;                                            (vlax-safearray->list val)))))
;;;            
;;;            xdat (read (cdr (assoc 1000 xdat))))

      (setq xDat (vl-remove-if-not
                   (function
                     (lambda (lst)
                       (entget (handent (car lst)))))
                   
                   (read
                     (vlax-variant-value
                       (cadr
                         (vlax-safearray->list val))))))

      (if (not (vl-position (car hand) (mapcar (function car) xdat)))
        (setq xdat (append xdat (list hand)))))

    (setq xdat (list hand)))   

  (putxdat cObj app (vl-prin1-to-string xdat))

  (or (vl-position cObj (vlr-owners *DCurve$Align$Reactor*))
      (vlr-owner-add *DCurve$Align$Reactor* cObj)))


;;...............................................................................;;


;;;內躬偕爻,虜,齯滌`偕爻,虜,齯滌`偕爻,虜,齯滌`偕爻,虜,齯滌`偕爻,虜,齯滌`偕爻,虜,;;;

;;                           --=={  Reactor Callback  }==--                      ;;

;;;內躬偕爻,虜,齯滌`偕爻,虜,齯滌`偕爻,虜,齯滌`偕爻,虜,齯滌`偕爻,虜,齯滌`偕爻,虜,;;;


(defun DTCurveUpd (Obj Reac Args / *error*

                                   typ val ObjxDat tObj dist perp
                                   mirr para a#dif pt deriv cAng lAng)

  (defun *error* (msg)
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ))
  

  (cond (  (and *Reactor$Command$Call*

                (or (wcmatch *Reactor$Command$Call* "*UNDO")
                    (eq "U" *Reactor$Command$Call*))))    

        (t (if (vlax-erased-p obj)
             (vlr-owner-remove Reac Obj)

             (progn

               (or doc (setq doc (vla-get-ActiveDocument
                                   (vlax-get-acad-object))))
               
               (vla-getXdata obj app 'typ 'val)

;;;               (setq ObjxDat (vl-remove-if-not
;;;                               (function
;;;                                 (lambda (lst)
;;;                                   (entget (handent (car lst)))))
;;;                               
;;;                               (read
;;;                                 (vlax-variant-value
;;;                                   (cadr
;;;                                     (vlax-safearray->list val))))))

               (setq ObjxDat

                            (cond (  (not (setq ObjxDat
                                            (vlax-variant-value
                                              (cadr
                                                (vlax-safearray->list val)))))

                                     nil)

                                  (  (vl-remove-if-not
                                       (function
                                         (lambda (lst)
                                           (entget (handent (car lst)))))

                                       (read ObjxDat)))

                                  (t nil)))

               (foreach lst  ObjxDat

                 (setq tObj  (vlax-ename->vla-object (handent (car lst))))

                 (mapcar (function set) '(dist perp mirr para a#dif) (cddr lst))

                 (if (eq "AcDbText" (vla-get-ObjectName tObj))
                   (vla-put-Alignment tObj acAlignmentMiddleCenter)
                   (vla-put-AttachmentPoint tobj acAttachmentPointMiddleCenter))

                 (setq pt
                        
                   (cond (  (vlax-curve-getPointatParam Obj para))

                         (  (vlax-curve-getClosestPointto Obj
                              (vlax-get tObj
                                (if (eq "AcDbText" (vla-get-ObjectName tObj))
                                  'TextAlignmentPoint
                                  'InsertionPoint))))))

                 (setq para  (vlax-curve-getParamatPoint Obj pt)

                       deriv (+ (angle '(0 0 0)
                                       (vlax-curve-getFirstDeriv Obj para)) a#dif))

;;;                 (setq deriv (+ deriv (if (< (distance tpt (polar pt deriv 1))
;;;                                             (distance tpt (polar pt (+ deriv pi) 1))) 0 pi)))

                 (setq cAng deriv lAng (rem (+ perp cAng) (* 2. pi)))

                 (if (equal lAng (/ pi 2.) 0.001)
                   (setq lAng (/ pi 2.)))

                 (if (equal lAng (/ (* 3 pi) 2.) 0.001)
                   (setq lAng (/ (* 3 pi) 2.)))

                 (and (minusp lAng) (setq lAng (+ lAng (* 2 pi))))
                 
                 (cond (  (and (> lAng (/ pi 2)) (<= lAng pi))
                          (setq lAng (- lAng pi)))

                       (  (and (> lAng pi) (<= lAng (/ (* 3 pi) 2)))
                          (setq lAng (+ lAng pi))))

                 (  (if (eq "AcDbText" (vla-get-ObjectName tObj))
                      vla-put-TextAlignmentPoint
                      vla-put-InsertionPoint)

                      tObj (vlax-3D-point (polar pt cAng (* (vla-get-Height tObj) dist))))

                 (vla-put-Rotation tObj (+ mirr lAng)))))))
      
  (princ))


;;...............................................................................;;


(defun DTCurveComm (Reac Args)
  (setq *Reactor$Command$Call* (strcase (car Args)))
  (princ))


;;...............................................................................;;


(defun icon_GrList (col sze / -sze)

  (setq sze (1+ sze) -sze (- sze))

  (list col (list       0.  (1- -sze)) (list      0.      sze)
        col (list       1.  (1- -sze)) (list      1.      sze)
        col (list     -sze       sze)  (list     sze      sze)
        col (list (1- -sze) (1+  sze)) (list (1+ sze) (1+ sze))))




(defun osMark (o / s)
  
  (setq s (/ (getvar "VIEWSIZE") (cadr (getvar "SCREENSIZE")))
        o (cons (trans (car o) 1 3) (cdr o)))

  (grvecs (cdr (assoc (cadr o) osGrv))
          (list (list s 0. 0. (caar o))
                (list 0. s 0. (cadar o))
                (list 0. 0. s 0.)
                (list 0. 0. 0. 1.))))


;;...............................................................................;;


(defun get_osmode nil ; by Evgeniy Elpanov
  (mapcar
    (function cdr)
      (vl-remove-if
        (function (lambda (x) (zerop (logand (getvar "OSMODE") (car x)))))
        '((1    . "_end")
          (2    . "_mid")
          (4    . "_cen")
          (8    . "_nod")
          (16   . "_qua")
          (32   . "_int")
          (64   . "_ins")
          (128  . "_per")
          (256  . "_tan")
          (512  . "_nea")
          (2048 . "_app")))))


;;...............................................................................;;


(defun osmode-grvecs-lst (col ass / -ASS ASS COL)
   ; By Evgeniy Elpanov (Modified by Lee McDonnell)
  
  (setq -ass (- ass))
  
  (list (list "_end"
              col (list -ass -ass) (list -ass  ass)
              col (list (1-  -ass) (1- -ass)) (list (1- -ass) (1+  ass))              
              col (list -ass  ass) (list  ass  ass)
              col (list (1-  -ass) (1+  ass)) (list (1+  ass) (1+  ass))              
              col (list  ass  ass) (list  ass -ass)
              col (list (1+   ass) (1+  ass)) (list (1+  ass) (1- -ass))              
              col (list  ass -ass) (list -ass -ass)
              col (list (1+   ass) (1- -ass)) (list (1- -ass) (1- -ass)))
        
        (list "_mid"
              col (list -ass -ass) (list    0. ass)
              col (list (1-  -ass) (1- -ass)) (list 0. (1+  ass))
              col (list    0. ass) (list  ass -ass)
              col (list 0. (1+  ass)) (list (1+  ass) (1- -ass))
              col (list  ass -ass) (list -ass -ass)
              col (list (1+   ass) (1- -ass)) (list (1- -ass) (1- -ass)))
        
        (list "_cen"
              7   (list (* -ass 0.2) 0.)  (list (*  ass 0.2) 0.)
              7   (list  0. (* -ass 0.2)) (list  0.  (*  ass 0.2))
              col (list    -ass   0.)     (list (* -ass 0.86) (* ass  0.5))
              col (list (* -ass 0.86) (* ass  0.5))  (list (* -ass  0.5) (* ass 0.86))
              col (list (* -ass  0.5) (* ass 0.86))  (list 0. ass)
              col (list 0. ass) (list (* ass 0.5)    (* ass 0.86))
              col (list (* ass 0.5)   (* ass 0.86))  (list (* ass 0.86) (* ass 0.5))
              col (list (* ass 0.86)  (* ass 0.5))   (list ass 0.)
              col (list ass 0.) (list (* ass 0.86)   (* -ass 0.5))
              col (list (* ass 0.86)  (* -ass 0.5))  (list (* ass 0.5) (* -ass 0.86))
              col (list (* ass 0.5)   (* -ass 0.86)) (list 0. -ass)
              col (list 0. -ass)(list (* -ass 0.5)   (* -ass 0.86))
              col (list (* -ass 0.5)  (* -ass 0.86)) (list (* -ass 0.86) (* -ass 0.5))
              col (list (* -ass 0.86) (* -ass 0.5))  (list -ass 0.))

        (list "_nod"
              col (list -ass -ass)    (list ass ass)
              col (list -ass ass)     (list ass -ass)
              col (list -ass 0.)      (list (* -ass 0.86) (* ass 0.5))
              col (list (* -ass 0.86) (* ass 0.5))   (list (* -ass 0.5) (* ass 0.86))
              col (list (* -ass 0.5)  (* ass 0.86))  (list 0. ass)
              col (list 0. ass) (list (* ass 0.5)    (* ass 0.86))
              col (list (* ass 0.5)   (* ass 0.86))  (list (* ass 0.86) (* ass 0.5))
              col (list (* ass 0.86)  (* ass 0.5))   (list ass 0.)
              col (list ass 0.) (list (* ass 0.86)   (* -ass 0.5))
              col (list (* ass 0.86)  (* -ass 0.5))  (list (* ass 0.5) (* -ass 0.86))
              col (list (* ass 0.5)   (* -ass 0.86)) (list 0. -ass)
              col (list 0. -ass)(list (* -ass 0.5)   (* -ass 0.86))
              col (list (* -ass 0.5)  (* -ass 0.86)) (list (* -ass 0.86) (* -ass 0.5))
              col (list (* -ass 0.86) (* -ass 0.5))  (list -ass 0.))

        (list "_qua"
              col (list 0. -ass)   (list -ass 0.)
              col (list 0. (1- -ass))   (list (1- -ass) 0.)
              col (list -ass 0.)   (list 0. ass)
              col (list (1- -ass) 0.)   (list 0. (1+ ass))
              col (list 0. ass)    (list ass 0.)
              col (list 0. (1+ ass))    (list (1+ ass) 0.)
              col (list ass 0.)    (list 0. -ass)
              col (list (1+ ass) 0.)    (list 0. (1- -ass)))

        (list "_int"
              col (list -ass -ass) (list ass ass)
              col (list -ass (1+ -ass)) (list ass (1+ ass))
              col (list (1+ -ass) -ass) (list (1+ ass) ass)
              col (list -ass ass)  (list ass -ass)
              col (list -ass (1+ ass))  (list ass (1+ -ass))
              col (list (1+ -ass) ass)  (list (1+ ass) -ass))

        (list "_ins"
              col (list (* -ass 0.1) (* -ass 0.1)) (list -ass (* -ass 0.1))
              col (list -ass (* -ass 0.1)) (list -ass ass)
              col (list -ass ass) (list (* ass 0.1) ass)
              col (list (* ass 0.1) ass)   (list (* ass 0.1) (* ass 0.1))
              col (list (* ass 0.1) (* ass 0.1))   (list ass (* ass 0.1))
              col (list ass (* ass 0.1))   (list ass -ass)
              col (list ass -ass) (list (* -ass 0.1) -ass)
              col (list (* -ass 0.1) -ass) (list (* -ass 0.1) (* -ass 0.1))
              col (list (1- (* -ass 0.1)) (1- (* -ass 0.1))) (list (1- -ass) (1- (* -ass 0.1)))
              col (list (1- -ass) (1- (* -ass 0.1))) (list (1- -ass) (1+ ass))
              col (list (1- -ass) (1+ ass)) (list (1+ (* ass 0.1)) (1+ ass))
              col (list (1+ (* ass 0.1)) (1+ ass)) (list (1+ (* ass 0.1)) (1+ (* ass 0.1)))
              col (list (1+ (* ass 0.1)) (1+ (* ass 0.1))) (list (1+ ass) (1+ (* ass 0.1)))
              col (list (1+ ass) (1+ (* ass 0.1)))   (list (1+ ass) (1- -ass))
              col (list (1+ ass) (1- -ass)) (list (1- (* -ass 0.1)) (1- -ass))
              col (list (1- (* -ass 0.1))   (1- -ass)) (list (1- (* -ass 0.1)) (1- (* -ass 0.1))))

        (list "_tan"
              col (list -ass ass) (list ass ass)
              col (list (1- -ass) (1+ ass)) (list (1+ ass) (1+ ass))
              col (list -ass 0.)  (list (* -ass 0.86) (* ass 0.5))
              col (list (* -ass 0.86) (* ass 0.5)) (list (* -ass 0.5) (* ass 0.86))
              col (list (* -ass 0.5) (* ass 0.86)) (list 0. ass)
              col (list 0. ass) (list  (* ass 0.5) (* ass 0.86))
              col (list (* ass 0.5)  (* ass 0.86)) (list (* ass 0.86) (* ass 0.5))
              col (list (* ass 0.86)  (* ass 0.5)) (list ass 0.)
              col (list ass 0.) (list (* ass 0.86) (* -ass 0.5))
              col (list (* ass 0.86) (* -ass 0.5)) (list (* ass 0.5) (* -ass 0.86))
              col (list (* ass 0.5) (* -ass 0.86)) (list 0. -ass)
              col (list 0. -ass)(list (* -ass 0.5) (* -ass 0.86))
              col (list (* -ass 0.5)(* -ass 0.86)) (list (* -ass 0.86) (* -ass 0.5))
              col (list (* -ass 0.86)(* -ass 0.5)) (list -ass 0.))

        (list "_per"
              col (list -ass -ass) (list -ass ass)
              col (list (1- -ass)  (1- -ass)) (list (1- -ass) (1+ ass))
              col (list ass -ass)  (list -ass -ass)
              col (list (1+ ass)   (1- -ass)) (list (1- -ass) (1- -ass))
              col (list -ass 0.)   (list 0. 0.)
              col (list -ass -1.)  (list 0. -1.)
              col (list 0. 0.)     (list 0. -ass)
              col (list -1. 0.)    (list -1. -ass))

        (list "_nea"
              col (list -ass -ass) (list ass ass)
              col (list -ass ass)  (list ass ass)
              col (list (1- -ass)  (1+ ass)) (list (1+ ass) (1+ ass))
              col (list -ass ass)  (list ass -ass)
              col (list ass -ass)  (list -ass -ass)
              col (list (1+ ass) (1- -ass)) (list (1- -ass) (1- -ass)))

        (list "_app"
              col (list -ass -ass) (list ass ass)
              col (list ass -ass)  (list -ass ass)
              col (list -ass -ass) (list -ass ass)
              col (list (1- -ass)  (1- -ass)) (list (1- -ass) (1+ ass))
              col (list -ass ass)  (list ass ass)
              col (list (1- -ass)  (1+ ass))  (list (1+ ass) (1+ ass))
              col (list ass ass)   (list ass -ass)
              col (list (1+ ass)   (1+ ass))  (list (1+ ass) (1- -ass))
              col (list ass -ass)  (list -ass -ass)
              col (list (1+ ass)   (1- -ass)) (list (1- -ass) (1- -ass)))))


;;...............................................................................;;




(defun TextSettings (Obj / *error* txt2num dcl_write logo

                           dcTag tLst oHgt tog hgt sty)
  

  (defun *error* (e)
    (and dcTag (unload_dialog dcTag))
    (and ofile (close ofile))
    (or (wcmatch (strcase e) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " e " **")))
    (princ))

  (defun txt2num ( txt )
    
    (cond (  (distof txt 5))
          (  (distof txt 2))
          (  (distof txt 1))
          (  (distof txt 4))
          (  (distof txt 3))))
          
  (defun dcl_write (fname / wPath ofile)

    (if (not (findfile fname))

      (if (setq wPath (findfile "ACAD.PAT"))
        (progn
          (setq wPath (vl-filename-directory wPath))
          
          (or (eq "\\" (substr wPath (strlen wPath)))
              (setq wPath (strcat wPath "\\")))

          (setq ofile (open (strcat wPath fname) "w"))
          (foreach str

           '("// DT_Curve.dcl for use in conjunction with DTCurve.lsp  //"
             "// Copyright ?November 2009, by Lee McDonnell (Lee Mac) //"
             ""
             "dt_curve : dialog { key = \"stitle\";"
             ""
             "  spacer;"
             "   "
             "  : row { alignment = centered;"
             "  "
             "    : boxed_column { label = \"TextStyle\";"
             "    "
             "      : popup_list { key = \"styl\"; alignment = centered; fixed_width = true; width = 20; }"
             ""
             "      spacer_1;"
             ""
             "    }"
             ""
             "    : boxed_column { label = \"Height\";"
             ""
             "      : row { children_alignment = centered; spacer;"
             ""
             "        : edit_box { key = \"hgt\"; edit_width = 5; alignment = centered; }"
             "  "
             "        spacer;"
             ""
             "        : column {"
             ""
             "          : spacer { height = 0.01; }"
             ""
             "          : toggle { key = \"bstyl\"; label = \" By Style\"; alignment = centered; }"
             ""
             "        }"
             ""
             "      }"
             "      "
             "      spacer;"
             "      "
             "    }"
             ""
             "  }"
             ""
             "  spacer;"
             ""
             "  : row { alignment = centered;"
             ""
             "    : spacer { width = 16.06; fixed_width = true; }"
             ""
             "    ok_cancel;"
             ""
             "    : image { key = \"logo\"  ; alignment    = centered;"
             "              width = 16.06 ; fixed_width  = true;"
             "              height = 2.06 ; fixed_height = true; color = -15; }"
             "              "
             "  }"
             "  "
             "}")

            (write-line str ofile))
          
        (setq ofile (close ofile))
          
        t)  ; File written successfully
        
    nil) ; Filepath not Found
      
  t)) ; DCL file already exists


  ;;...............................................................................;;
  

  (defun logo (key)
    
    (start_image key)
    (mapcar 'vector_image
            '(22 21 1 0 0 0 0 7 0 0 0 0 1 6 6 6 6 7 43 36 27 36 30 21 21 21 22 22 22
              22 21 21 21 28 28 28 27 27 30 29 29 30 52 43 43 43 44 44 46 46 45 45 45
              45 52 52 52 51 51 51 51 51 52 62 65 66 68 68 68 68 67 67 75 75 75 74 74
              73 66 58 58 59 59 59 59 52 57 57 56 56 56 56 57 58 65 65 65 65 66 95 94
              94 92 91 91 91 90 89 89 88 87 86 85 74 74 75 75 76 77 78 79 80 81 82 83
              84 85 86 87 88 88 89 90 91 92 93 94 95 74 73 73 72 72 71 71 71 71 71 71
              71 72 72 72 73 84 83 82 81 80 79 79 78 77 77 76 76 76 76 76 77 77 78 79
              79 80 81 82 83 94 94 95 83 83 82 81 80 79 78 77 76 75 74 84 85 86 87 88
              89 89 90 91 91 91 91 92 95 94 93 92 91 90 89 89 88 87 86 85 84)

            '(20 20 23 23 23 24 24 0 0 0 0 1 1 20 1 1 1 0 2 24 7 15 0 0 0 0 1 1 23 23
              23 24 24 24 24 24 23 23 2 1 1 0 0 0 0 0 1 1 7 23 23 23 24 24 24 24 24 23
              23 1 1 1 0 10 16 19 21 22 23 24 24 24 24 24 24 23 23 22 4 4 5 5 6 6 7 24
              24 24 24 23 23 22 19 16 7 7 6 5 5 22 22 22 17 17 18 18 19 20 20 20 21 21
              21 21 22 23 23 23 24 24 24 25 25 25 25 25 25 25 25 24 24 24 23 23 22 22
              22 22 7 8 8 9 10 11 12 13 14 15 16 17 18 19 19 20 21 21 21 21 20 20 19 19
              18 17 16 15 14 13 12 12 11 10 9 9 8 8 8 7 7 7 7 4 4 4 4 4 4 4 5 5 6 6 7 7
              8 8 8 9 9 9 10 11 11 11 11 7 7 7 6 6 5 5 4 4 4 4 4 4)

            '(21 6 0 0 0 0 21 0 0 0 0 1 1 6 6 6 7 7 36 46 36 30 21 21 21 22 22 22 22 21
              21 21 28 28 28 27 27 27 29 29 30 30 43 43 43 44 44 43 46 45 45 45 45 52 52
              52 51 51 51 51 51 52 52 65 58 68 68 68 68 67 67 75 75 75 74 74 73 65 58 58
              59 59 59 59 51 57 57 56 56 56 56 57 66 62 65 65 65 66 66 94 94 95 91 91 91
              90 89 89 88 87 86 85 84 74 75 75 76 77 78 79 80 81 82 83 84 85 86 87 88 88
              89 90 91 92 93 94 95 92 73 73 72 72 71 71 71 71 71 71 71 72 72 72 73 74 83
              82 81 80 79 79 78 77 77 76 76 76 76 76 77 77 78 79 79 80 81 82 83 84 94 95
              94 83 82 81 80 79 78 77 76 75 74 74 85 86 87 88 89 89 90 91 91 91 91 92 95
              94 93 92 91 90 89 89 88 87 86 85 84 83)

            '(20 20 23 23 24 24 24 0 0 0 1 1 23 1 1 1 0 0 15 7 24 2 0 0 0 1 1 23 23 23 24
              24 24 24 24 23 23 7 1 1 0 0 0 0 0 1 1 2 23 23 23 24 24 24 24 24 23 23 1 1 1
              0 0 16 16 21 22 23 24 24 24 24 24 24 23 23 22 7 4 5 5 6 6 7 22 24 24 24 23
              23 22 19 19 10 7 6 5 5 4 22 22 22 17 18 18 19 20 20 20 21 21 21 21 22 23 23
              23 24 24 24 25 25 25 25 25 25 25 25 24 24 24 23 23 22 22 22 22 17 8 8 9 10 11
              12 13 14 15 16 17 18 19 19 20 21 21 21 21 20 20 19 19 18 17 16 15 14 13 12 12
              11 10 9 9 8 8 8 7 7 7 7 7 4 4 4 4 4 4 5 5 6 6 7 7 8 8 8 9 9 9 10 11 11 11 11
              7 7 7 6 6 5 5 4 4 4 4 4 4 4)

            '(178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178))
    
  (end_image))
  

  ;;...............................................................................;;
  

  (setq doc (cond (doc) ( (vla-get-ActiveDocument (vlax-get-acad-object)))))

  (vlax-for ts (vla-get-TextStyles doc)
    (setq tLst (cons (vla-get-Name ts) tLst)))

  (if  (= (setq oHgt (vla-get-Height Obj))
          (vla-get-Height
            (vla-item
              (vla-get-TextStyles doc)
                (vla-get-StyleName obj))))
    
    (setq tog "1")
    (setq tog "0"))

  (cond  (  (not (dcl_write dc_fname))

            (princ "\n** DCL File could not be Written **"))

         (  (<= (setq dcTag (load_dialog dc_fname)) 0)

            (princ "\n** Dialog Definition File not Found **"))

         (  (not (new_dialog "dt_curve" dcTag))

            (princ "\n** Dialog could not be Displayed **"))

         (t

            (set_tile "stitle" dc_title)

            (set_tile  "bstyl" tog)
            (set_tile  "hgt" (vl-princ-to-string oHgt))
            (mode_tile "hgt" (atoi (get_tile "bstyl")))

            (start_list "styl")
            (mapcar 'add_list (setq tLst (acad_strlsort
                                           (vl-remove-if
                                             (function
                                               (lambda (x) (or (null x) (eq "" x)))) tLst))))
            (end_list)

            (set_tile "styl" (itoa (vl-position (vla-get-StyleName Obj) tLst)))

            (logo "logo")            

            (action_tile "bstyl"
              (vl-prin1-to-string
                (quote
                  (progn
                    (set_tile "hgt" (vl-princ-to-string
                                      (cond (  (zerop (setq sHgt (vla-get-Height
                                                                   (vla-item
                                                                     (vla-get-TextStyles doc)
                                                                       (nth (atoi (get_tile "styl")) tLst))))) 1.)
                                            (sHgt))))
                    
                    (mode_tile "hgt" (atoi $value))))))

            (action_tile "styl"
              (vl-prin1-to-string
                (quote
                  (progn
                    (set_tile "hgt" (vl-princ-to-string
                                      (cond (  (zerop (setq sHgt (vla-get-Height
                                                                   (vla-item
                                                                     (vla-get-TextStyles doc)
                                                                       (nth (atoi $value) tLst))))) 1.)
                                            (sHgt))))))))
            
            (action_tile "cancel" "(done_dialog)")

            (action_tile "accept"
              (vl-prin1-to-string
                (quote
                  (progn

                    (cond (  (not (setq hgt (txt2num (get_tile "hgt"))))

                             (alert "**  Text Height must be Numerical **"))

                          (t

                             (vla-put-StyleName Obj (setq sty (nth (atoi (get_tile "styl")) tLst)))

                             (or (and (eq "0" (get_tile "bstyl"))
                                      (not (vla-put-Height Obj hgt)))

                                 (vla-put-height Obj
                                   (cond ( (zerop (setq sHgt (vla-get-Height
                                                               (vla-item
                                                                 (vla-get-TextStyles doc) sty)))) 1.)
                                         (sHgt))))                                           
                           
                             (done_dialog)))))))
          
          
            (start_dialog)
            (unload_dialog dcTag))))


;;...............................................................................;;




;; DTRemove  ~  Function to Remove Text/Object Associativity

(defun c:DTRemove (/  ; --=={  Local Functions  }==--

                        *error* putxdat logo_print

                      ; --=={  Local Variables  }==--

                        uflag doc typ val xDat gr code
                        data ent tag han cHan cObj)
  (vl-load-com)

  (setq doc   (vla-get-ActiveDocument
                (vlax-get-acad-object))

        drft  (vla-get-drafting
                (vla-get-preferences
                  (vlax-get-acad-object)))
        
        logo  (icon_GrList 3 (vla-get-AutoSnapMarkerSize drft)))
  

  ;;...............................................................................;;
  

  (defun logo_print (pt / s)
  
    (setq s  (/ (getvar "VIEWSIZE") (cadr (getvar "SCREENSIZE")))
          pt (trans pt 1 3))

    (grvecs logo (list (list s 0. 0. (car pt))
                       (list 0. s 0. (cadr pt))
                       (list 0. 0. s 0.)
                       (list 0. 0. 0. 1.))))
  

  ;;...............................................................................;;
  

  (defun *error* (msg)

    (if (and uflag doc) (vla-EndUndoMark doc))

    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))

    (redraw)
    (princ))
  

  ;;...............................................................................;;
  

  (defun putxdat (Obj App Data / xtype xval)

    (setq xtype
      (vlax-make-variant
        (vlax-safearray-fill
          (vlax-make-safearray
            vlax-vbInteger '(0 . 1)) '(1001 1000))))

    (setq xval
      (vlax-make-variant
        (vlax-safearray-fill
          (vlax-make-safearray
            vlax-vbVariant '(0 . 1)) (list App Data))))

    (vla-setXData Obj xtype xval))
  

  ;;...............................................................................;;
  

  (foreach obj (vlr-owners *DCurve$Align$Reactor*)

             (if (vlax-erased-p obj)
               (vlr-owner-remove *DCurve$Align$Reactor* obj)

               (progn
             
                 (vla-GetXData obj app 'typ 'val)

                 (if (and typ val)
                   (progn

                     (setq xDat (cons (read (cdr (assoc 1000 (apply (function mapcar)
                                                                    (cons (function cons)
                                                                          (list (vlax-safearray->list typ)
                                                                                (mapcar (function vlax-variant-value)
                                                                                        (vlax-safearray->list val)))))))) xDat)))))))

  (setq xDat (apply (function append) xDat))

  (cond (  (not (or (vlr-owners *DCurve$Align$Reactor*) xDat))    

           (princ "\n** No Text/Object Associativity Found **"))
        
        (t        

           (vla-StartUndoMark doc)
           (setq uFlag t)

           (setq msg (princ "\nSelect Text/Object to Remove Associativity, Remove [A]ll" ))
         
           (while
             (progn
               (setq gr (grread 't 15 2) code (car gr) data (cadr gr))
               (redraw)

               (cond (  (and (= 5 code) (listp data))

                        (if (setq ent (car (nentselp data)))
                          (progn

                            (setq hand (cdr (assoc 5 (entget ent))))

                            (and (or (vl-position hand (mapcar (function car)  xDat))
                                     (vl-position hand (mapcar (function cadr) xDat)))
                                 
                                 (logo_print (polar data (+ (/ pi 4.) (getvar "SNAPANG"))
                                                    (* (+ 30. (getvar "PICKBOX")) (/ (getvar "VIEWSIZE")
                                                                                     (cadr (getvar "SCREENSIZE")))))))))
                      
                        t)

                     (  (and (= 3 code) (listp data))

                        (cond (  (setq ent (car (nentselp data)))

                                 (cond (  (vl-position (cdr (assoc 0 (entget ent))) '("MTEXT" "TEXT"))

                                          (if (setq tag (assoc (setq han (cdr (assoc 5 (entget ent)))) xDat))
                                            (progn

                                              (setq cHan (cadr tag) xDat (vl-remove-if
                                                                           (function
                                                                             (lambda (x)
                                                                               (or (eq tag x)
                                                                                   (not (eq cHan (cadr x)))))) xDat))
                                              
                                              (putxDat (vla-HandletoObject doc cHan) app (vl-prin1-to-string xDat))
                                              (princ "\n~才 Text/Object Associativity Removed ~才")

                                              nil)

                                            (princ (strcat "\n** Text not Aligned **" msg))))

                                       (  (not (vl-catch-all-error-p
                                                 (vl-catch-all-apply 'vlax-curve-getEndParam
                                                   (list (setq cObj (vlax-ename->vla-object ent))))))

                                          (vla-GetXData cObj app 'typ 'val)

                                          (if (and typ val)
                                            (progn
                                              
                                              (vlr-owner-remove *DCurve$Align$Reactor* cObj)
                                              (putxDat cObj app "")                                              
                                              (entmod (list (cons -1 ent) (list -3 (list app))))
                                              
                                              (princ "\n~才 All Text Associativity with this Object Erased ~才") nil)

                                            (princ (strcat "\n** No Text/Object Associativity Found **" msg))))

                                       (t (princ (strcat "\n** Invalid Object Selection **" msg)))))

                              (t (princ (strcat "\n** Missed, Try Again... **" msg)))))

                     (  (= code 25) nil)

                     (  (= code 2)

                        (cond  (  (vl-position data '(13 32)) nil)

                               (  (vl-position data '(65 97))

                                  (foreach obj (vlr-owners *DCurve$Align$Reactor*)

                                    (vla-GetxData obj app 'typ 'val)

                                    (if (and typ val)
                                      (progn
                                        
                                        (vlr-owner-remove *DCurve$Align$Reactor* obj)
                                        (putxDat obj app "")                                        
                                        (entmod (list (cons -1 (vlax-vla-object->ename obj)) (list -3 (list app)))))))

                                  (princ "\n~才  All Associativity Removed  ~才") nil)

                               (t )))

                     (t ))))))

  (vla-EndUndoMark doc)
  (redraw)
  
  (princ))


;;...............................................................................;;




(vl-load-com)

(if (vl-catch-all-error-p
      (vl-catch-all-apply
        (function
          (lambda (/ unique xdat doc)

            (defun unique (lst / result)
              (reverse
                (while (setq itm (car lst))
                  (setq lst (vl-remove itm lst)
                        result (cons itm result)))))

            (setq *DCurve$Align$Reactor*

                   (cond (  (setq pos (vl-position "DCurve$Align$Reactor"
                                        (mapcar 'vlr-data (cdar (vlr-reactors :VLR-Object-Reactor)))))
                          
                            (nth pos (cdar (vlr-reactors :VLR-Object-Reactor))))

                         (t (setq *DCurve$Align$Reactor* (vlr-object-reactor nil "DCurve$Align$Reactor"
                                                           (list
                                                             (cons :vlr-modified 'DTCurveUpd)))))))

            (setq *DCurve$Align$Reactor$Comm*

                   (cond (  (setq pos (vl-position "DCurve$Align$Reactor$Comm"
                                        (mapcar 'vlr-data (cdar (vlr-reactors :VLR-Command-Reactor)))))
                          
                            (nth pos (cdar (vlr-reactors :VLR-Command-Reactor))))

                         (t (setq *DCurve$Align$Reactor$Comm* (vlr-command-reactor "DCurve$Align$Reactor$Comm"
                                                                (list
                                                                  (cons :vlr-CommandWillStart 'DTCurveComm)))))))
            
            (mapcar
              (function
                (lambda (object) (vlr-owner-remove *DCurve$Align$Reactor* object)))
              
              (vlr-owners *DCurve$Align$Reactor*))

            (vlax-for lay (vla-get-layouts
                            (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))

              (vlax-for obj (vla-get-Block lay)

                (vla-getXdata obj app 'typ 'val)
                
                (if val
                  (setq xdat
                    (cons
                      (vlax-variant-value
                        (cadr
                          (vlax-safearray->list val))) xdat)))

                (setq typ nil val nil)))

            (foreach handle (unique
                              (mapcar (function cadr)
                                      (apply (function append)
                                             (mapcar (function read) (vl-remove 'nil xdat)))))

              (vlr-owner-add *DCurve$Align$Reactor* (vla-HandletoObject doc handle)))))))

  (princ "\n** Error Loading DTCurve **")

;;...............................................................................;;

  (progn

    (princ "\n齯滌`偕爻  DTCurve.lsp ~ Copyright ?by Lee McDonnell  齯滌`偕爻")
    (princ "\n   ~才          ...Type \"DTCurve\" to Invoke...            ~才   ")))

(princ)
 

