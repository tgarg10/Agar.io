;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |AgarioServer (1)|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; ____________ GLOBAL VARIABLE ____________

(define SCREEN-WIDTH 1080)
(define SCREEN-HEIGHT 720)
(define MAP-WIDTH (* SCREEN-WIDTH 4))
(define MAP-HEIGHT (* SCREEN-HEIGHT 4))
(define INITIAL-PLAYER-RADIUS 40)
(define FOOD-SIZE 10)
(define INITIAL-FOOD-COUNT 100)

(define (remove-from-list loe l) (foldr remove l loe))

; ____________ STRUCTURES ____________

; low is the list of worlds
; lop is the list of players
; lof is the list of food
(define-struct US (low lop lof))
; List of Worlds, US -> US
(define (US-c-low new-low us) (make-US new-low (US-lop us) (US-lof us)))
; List of Players, US -> US
(define (US-c-lop new-lop us) (make-US (US-low us) new-lop (US-lof us)))
; List of Food, US -> US
(define (US-c-lof new-lof us) (make-US (US-low us) (US-lop us) new-lof))

; ____________ ADD WORLD ____________

; None -> Color
; Generates a random color.
(define (random-color) (list-ref (list 'OrangeRed 'Tomato 'DarkRed 'Red 'Firebrick 'Crimson 'DeepPink 'Maroon 'IndianRed 'MediumVioletRed 'VioletRed 'LightCoral 'HotPink 'PaleVioletRed 'LightPink 'RosyBrown 'Pink 'Orchid 'LavenderBlush 'Snow 'Chocolate 'SaddleBrown 'Brown 'DarkOrange 'Coral 'Sienna 'Orange 'Salmon 'Peru 'DarkGoldenrod 'Goldenrod 'SandyBrown 'LightSalmon 'DarkSalmon 'Gold 'Yellow 'Olive 'Burlywood 'Tan 'NavajoWhite 'PeachPuff 'Khaki 'DarkKhaki 'Moccasin 'Wheat 'Bisque 'PaleGoldenrod 'BlanchedAlmond 'MediumGoldenrod 'PapayaWhip 'MistyRose 'LemonChiffon 'AntiqueWhite 'Cornsilk 'LightGoldenrodYellow 'OldLace 'Linen 'LightYellow 'SeaShell 'Beige 'FloralWhite 'Ivory 'Green 'LawnGreen 'Chartreuse 'GreenYellow 'YellowGreen 'MediumForestGreen 'OliveDrab 'DarkOliveGreen 'DarkSeaGreen 'Lime 'DarkGreen 'LimeGreen 'ForestGreen 'SpringGreen 'MediumSpringGreen 'SeaGreen 'MediumSeaGreen 'Aquamarine 'LightGreen 'PaleGreen 'MediumAquamarine 'Turquoise 'LightSeaGreen 'MediumTurquoise 'Honeydew 'MintCream 'RoyalBlue 'DodgerBlue 'DeepSkyBlue 'CornflowerBlue 'SteelBlue 'LightSkyBlue 'DarkTurquoise 'Cyan 'Aqua 'DarkCyan 'Teal 'SkyBlue 'CadetBlue 'CadetBlue 'DarkSlateGray 'LightSlateGray 'SlateGray 'LightSteelBlue 'LightBlue 'PowderBlue 'PaleTurquoise 'LightCyan 'AliceBlue 'Azure 'MediumBlue 'CornflowerBlue 'DarkBlue 'MidnightBlue 'Navy 'Blue 'Indigo 'BlueViolet 'MediumSlateBlue 'SlateBlue 'Purple 'DarkSlateBlue 'DarkViolet 'DarkOrchid 'MediumPurple 'CornflowerBlue 'MediumOrchid 'Magenta 'Fuchsia 'DarkMagenta 'Violet 'Plum 'Lavender 'Thistle 'GhostWhite 'White 'WhiteSmoke 'Gainsboro 'LightGray 'Silver 'Gray 'DarkGray 'DimGray 'Black)
                                 (random 0 145)))

; Number, List -> List of List Circle Structure
; Creates a list of circles with random locations and colors.
(define (make-circles-list n cl) (if (= n 0) cl
                                     (local [(define x (random (/ SCREEN-WIDTH 2) (- MAP-WIDTH (/ SCREEN-WIDTH 2))))
                                             (define y (random (/ SCREEN-HEIGHT 2) (- MAP-HEIGHT (/ SCREEN-HEIGHT 2))))
                                             (define color (random-color))
                                             (define circl (list x y color))]
                                           (make-circles-list (- n 1) (cons circl cl)))))

(define circles-list (make-circles-list INITIAL-FOOD-COUNT empty))

; Universe, World -> Bundle
; A bundle is a structure that contains:
;   * Universe state - the new Universe structure
;   * List of mails (messages) to send to other worlds
(define (add-world us world) (local [(define new-Player (list (iworld-name world)
                                                              (random-color)
                                                              (random (/ SCREEN-WIDTH 2) (- MAP-WIDTH (/ SCREEN-WIDTH 2)))
                                                              (random (/ SCREEN-HEIGHT 2) (- MAP-HEIGHT (/ SCREEN-HEIGHT 2)))
                                                              INITIAL-PLAYER-RADIUS))]
                               (make-bundle (make-US (cons world (US-low us)) (cons new-Player (US-lop us)) (US-lof us)) 
                                            (if (empty? (US-low us))
                                                (list (make-mail world (list 'joined (cons new-Player (US-lop us)) (US-lof us))))
                                                (cons (make-mail world (list 'joined (cons new-Player (US-lop us)) (US-lof us)))
                                                      (map (lambda (w) (make-mail w (list 'new-join new-Player))) (US-low us))))
                                            empty)))

; ____________ MESSAGE RECEIVING ____________

; List of Players, String (Name) -> Player Strucutre
; Returns the List of Player Info using the player name.
(define (get-player lop name)
  (if (empty? lop)
      empty
      (if (symbol=? (list-ref (first lop) 0) name) (first lop)
          (get-player (rest lop) name))))

; Universe, World, List -> Bundle
; A bundle is a structure that contains:
;   * Universe state - the new Universe structure
;   * List of mails (messages) to send to other worlds
(define (handle-message us world msg)
  (cond [(symbol=? (first msg) 'moved)
         (local [(define new-player-info (second msg))
                 (define lop (US-lop us))
                 (define old-player-info (get-player lop (list-ref new-player-info 0)))]
               (make-bundle (US-c-lop (cons new-player-info (remove old-player-info lop)) us)
                            (map (lambda (w) (make-mail w (list 'new-movement new-player-info))) (remove world (US-low us)))
                            empty))]
        [(symbol=? (first msg) 'food-update)
         (local [(define new-food (make-circles-list (length (second msg)) empty))
                 (define new-lof (append (remove-from-list (second msg) (US-lof us)) new-food))]
           (make-bundle (US-c-lof new-lof us)
                        (cons (make-mail world (list 'new-food new-food))
                              (map (lambda (w) (make-mail w (list 'food-list (second msg) new-food))) (remove world (US-low us))))
                        empty))]
        [(symbol=? (first msg) 'player-killed)
         (make-bundle (US-c-lop (remove (second msg) (US-lop us)) us)
                      (map (lambda (w) (make-mail w (list 'player-death (second msg) (third msg)))) (remove world (US-low us)))
                      empty)]
        [(symbol=? (first msg) 'player-dead)
         (make-bundle (US-c-lop (remove (second msg) (US-lop us)) us)
                      (map (lambda (w) (make-mail w (list 'player-death (second msg) (third msg)))) (remove world (US-low us)))
                      empty)]))

; ____________ UNIVERSE (Server) ____________

(universe (make-US empty empty circles-list)
          (on-new add-world)
          (on-msg handle-message))