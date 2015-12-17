let
 val cons = fn x => fn y => fn m => (m x) y
 val car = fn z => z (fn p => fn q => p)
 val cdr = fn z => z (fn p => (fn q => p))
in
 (car (car (cons ((cons 1) 2) 3)),
 cdr (car (cons ((cons 1) 2) 3)))
end
