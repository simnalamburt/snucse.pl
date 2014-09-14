#use "main.ml"

(*
 * Use Cases
 * --------
 *
 * 1. Expect specific result
 *
 *    expect
 *    (Branch (
 *      Guide ("x", End (NameBox "x")),
 *      Branch (Guide ("y", End (NameBox "y")), End StarBox)
 *    ))
 *    "{-}";;
 *
 * 2. Expect failure
 *
 *    expect_fail
 *    (Branch (
 *      Guide ("x", Branch (End (NameBox "x"), End (NameBox "x"))),
 *      End StarBox
 *    ))
 *
 *)
open Printf
let reset = "\x1b[0m"
let red   = "\x1b[31m"
let green = "\x1b[32m"
let brown = "\x1b[33m"
let gray  = "\x1b[37m"

let counter = ref 0

let expect map result =
  counter := !counter + 1;
  let rec s key =
    match key with
    | Bar -> "-"
    | Node (left, right) -> sprintf "(%s,%s)" (s left) (s right)
  in
  let keylist = getReady map in
  let stringlist = List.map s keylist in
  let concated = String.concat ", " stringlist in
  let trial = sprintf "{%s}" concated in


  if compare trial result == 0 then
    printf "%s[%2d]%s OK\n" green !counter reset
  else
    let format = format_of_string (
      if String.length trial + String.length result <= 80 then
        "%s[%2d]%s %s%s%s != %s%s%s\n"
      else
        "\n%s[%2d]%s %s%s%s\n  != %s%s%s\n"
    ) in
    printf format red   !counter reset
                  gray  trial    reset
                  brown result   reset

let expect_fail map =
  counter := !counter + 1;
  if
    try
      let _ = getReady map in
      false
    with
    | Invalid_argument _ -> true
    | _ -> false
  then printf "%s[%2d]%s OK\n" green !counter reset
  else printf "%s[%2d]%s Expected a %sInvalid_argument%s exception\n"
    red   !counter reset
    brown          reset
;;


expect
(End StarBox)
"{-}";;

expect
(End (NameBox "x"))
"{-}";;

expect
(Guide ("x", End (NameBox "x")))
"{-}";;

expect
(Branch (Guide ("x", End (NameBox "x")), End StarBox))
"{-}";;

expect_fail
(Branch (
  Guide ("x", Branch (End (NameBox "x"), End (NameBox "x"))),
  End StarBox
));;

expect
(Branch (
  Guide ("x", End (NameBox "x")),
  Branch (Guide ("y", End (NameBox "y")), End StarBox)
))
"{-}";;

expect
(Branch (
  Guide ("x", End (NameBox "x")),
  Guide ("y", End (NameBox"y"))
))
"{-, (-,-)}";;

expect
(Branch (End (NameBox "x"), End StarBox))
"{-, (-,-)}";;

expect
(Guide ("x", Guide ("y",
  Branch (End (NameBox "x"),End (NameBox "y"))
)))
"{-, (-,-)}";;

expect
(Guide ("x", Guide ("y", Guide ("z",
  Branch (Branch (End (NameBox "x"), End (NameBox "y")), End (NameBox "z"))
))))
"{-, (-,(-,-))}";;

expect
(Guide ("x", Guide ("y", Guide ("z",
  Branch (End (NameBox "x"),Branch (End (NameBox "y"), End (NameBox "z")))
))))
"{-, (-,-)}";;

expect
(Branch(
  Guide ("x", Guide ("y", Branch (End (NameBox "y"), End (NameBox "x")))),
  Branch (End (NameBox "z"), End (NameBox "w"))
))
"{-, (-,-)}";;

expect_fail
(Branch(
  Guide ("x", Branch (End (NameBox "x"), End (NameBox "x"))),
  Guide ("y", Branch (End (NameBox "y"), End (NameBox "y")))
));;

expect_fail
(Branch(
  Guide ("z", End (NameBox "w")),
  Branch(
    Guide ("x", Branch (End(NameBox "x"), End (NameBox "x"))),
    Guide ("y", Branch (End (NameBox "y"), End (NameBox "y")))
  )
));;

expect_fail
(Branch(
  Guide ("x", (Guide ("y", Guide ("z", Guide ("w", Branch (Branch
    (End (NameBox "x"), End (NameBox "y")), Branch (End (NameBox "z"),
    End (NameBox "w")))))))),
  End StarBox
));;

expect_fail
(Branch(
  Guide ("x", (Guide ("y", Guide ("z", Guide ("w", Branch (Branch
    (End (NameBox "x"), End (NameBox "y")), Branch (End (NameBox "z"),
    End (NameBox "w")))))))),
  Guide ("a", End StarBox)
));;

expect
(Branch(
  Guide ("x", (Guide ("y", Guide ("z", Guide ("w", Branch (Branch
    (End (NameBox "x"), End (NameBox "y")), Branch (End (NameBox "z"),
    End (NameBox "w")))))))),
  Guide ("a", Branch (End (NameBox "a"),End (NameBox "b")))
))
"{-, (-,-), (-,(-,-)), ((-,(-,-)),(-,-))}";;

expect
(Branch(
  Branch(Branch(End (NameBox "a"), End(NameBox "b")), End(NameBox "c")),
  Branch(Branch(End (NameBox "d"), End(NameBox "e")), End(NameBox "f"))
))
"{-, (-,(-,-)), (-,(-,(-,-)))}";;

expect
(Branch(
  Branch(
    Branch(Branch(End (NameBox "a"), End(NameBox "b")), End(NameBox "c")),
    Branch(Branch(End (NameBox "d"), End(NameBox "e")), End(NameBox "f"))
  ),
  Guide ("x", Branch(End (NameBox "x"), End StarBox))
))
"{-, (-,-), (-,(-,-)), (-,(-,(-,(((-,-),-),-))))}";;

expect_fail
(Branch(
  Branch(
    Branch(Branch(End (NameBox "a"), End(NameBox "b")), End(NameBox "c")),
    Branch(Branch(End (NameBox "d"), End(NameBox "e")), End(NameBox "f"))
  ),
  Branch(End (NameBox "a"), End StarBox)
));;

expect
(Branch(
  Branch(
    Branch(Branch(End (NameBox "a"), End(NameBox "b")), End(NameBox "c")),
    Branch(Branch(End (NameBox "d"), End(NameBox "e")), End(NameBox "f"))
  ),
  Branch(End (NameBox "b"), End StarBox)
))
"{-, (-,(-,-)), (-,-), ((-,-),(-,(-,(-,-))))}";;

expect
(Branch(
  Branch(
    Branch(Branch(End (NameBox "a"), End(NameBox "b")), End(NameBox "c")),
    Branch(Branch(End (NameBox "d"), End(NameBox "e")), End(NameBox "f"))
  ),
  Branch(End (NameBox "c"), End StarBox)
))
"{-, (-,(-,-)), (-,-), (-,((-,-),(-,(-,-))))}";;

expect
(Branch(
  Branch(
    Branch(Branch(End (NameBox "a"), End(NameBox "b")), End(NameBox "c")),
    Branch(Branch(End (NameBox "d"), End(NameBox "e")), End(NameBox "f"))
  ),
  Branch(End (NameBox "d"), End StarBox)
))
"{-, (-,(-,-)), (-,(-,(-,((-,-),-))))}";;

expect
(Branch(
  Branch(
    Branch(Branch(End (NameBox "a"), End(NameBox "b")), End(NameBox "c")),
    Branch(Branch(End (NameBox "d"), End(NameBox "e")), End(NameBox "f"))
  ),
  Branch(End (NameBox "e"), End StarBox)
))
"{-, (-,-), ((-,-),(-,-)), (-,(-,(-,(-,-))))}";;

expect
(Branch(
  Branch(
    Branch(Branch(End (NameBox "a"), End(NameBox "b")), End(NameBox "c")),
    Branch(Branch(End (NameBox "d"), End(NameBox "e")), End(NameBox "f"))
  ),
  Branch(End (NameBox "f"), End StarBox)
))
"{-, (-,-), (-,((-,-),-)), (-,(-,(-,(-,-))))}";;

(* 19 boxes, 90 nodes *)
expect
(Branch ((Guide ("Bill", (End (NameBox "Bill")))) , (Branch ((Branch
  ((Guide ("Claudette", (Branch ((Branch ((End (NameBox "Claudette")) , (End
  (NameBox "Danny")))) , (Branch ((End (NameBox "Erika")) , (Guide ("Fred", (End
  (NameBox "Fred")))))))))) , (Branch ((Branch ((Branch ((Branch ((End (NameBox
  "Grace")) , (End (NameBox "Henri")))) , (Branch ((End (NameBox "Ida")) , (End
  (NameBox "Joaquin")))))) , (End (NameBox "Kate")))) , (Branch ((End (NameBox
  "Larry")) , (Branch ((End (NameBox "Mindy")) , (Guide ("Nicholas", (End
  (NameBox "Nicholas")))))))))))) , (Branch ((Branch ((End (NameBox "Odette")) ,
  (Guide ("Peter", (Guide ("Rose", (Branch ((End (NameBox "Rose")) , (End
  (NameBox "Sam")))))))))) , (Branch ((Branch ((Guide ("Teresa", (End (NameBox
  "Teresa")))) , (End (NameBox "Victor")))) , (End (NameBox "Wanda"))))))))))
("{-, (-,-), (-,(-,(-,-))), (-,(-,(-,(-,(-,(-,(-,-))))))), ((-,-),-), ((-,((-" ^
  ",-),-)),(-,-))}");;

(* 40 boxes, about 190 nodes *)
expect
(Branch ((End (NameBox "ab")) , (Branch ((Branch ((Branch ((Branch ((Guide
  ("ac", (Guide ("ad", (Guide ("ae", (Guide ("af", (Guide ("ag", (End (NameBox
  "ag")))))))))))) , (End (NameBox "ah")))) , (Branch ((Branch ((Branch ((Branch
  ((Branch ((Branch ((End (NameBox "ai")) , (End (NameBox "aj")))) , (Guide
  ("ak", (End (NameBox "ak")))))) , (Branch ((End (NameBox "al")) , (Guide
  ("am", (End (NameBox "am")))))) )) , (Branch ((Branch ((End (NameBox "an")) ,
  (Branch ((End (NameBox "ao")) , (End (NameBox "ap")))) )) , (Guide ("aq", (End
  (NameBox "aq")))))))) , (End (NameBox "ar")))) , (Branch ((End (NameBox "as"))
  , (Branch ((Branch ((End (NameBox "at")) , (End (NameBox "au")))) , (Branch
  ((Guide ("av", (Branch ((End (NameBox "av")) , (End (NameBox "aw")))) )) ,
  (Branch ((Guide ("ax", (End (NameBox "ax")))) , (End (NameBox "ay")))) )))))))
  ))) , (End (NameBox "az")))) , (Branch ((Branch ((Guide ("ba", (Branch
  ((Branch ((Branch ((Branch ((End (NameBox "ba")) , (Branch ((End (NameBox "bb"
  )) , (End (NameBox "bc")))))) , (End (NameBox "bd")))) , (Guide ("be", (Guide
  ("bf", (Branch ((End (NameBox "bf")) , (End (NameBox "bg")))) )))))) , (Branch
  ((Branch ((End (NameBox "bh")) , (Branch ((Branch ((End (NameBox "bi")) , (End
  (NameBox "bj")))) , (End (NameBox "bk")))) )) , (Branch ((Branch ((Branch ((
  End (NameBox "bl")) , (End (NameBox "bm")))) , (Guide ("bn", (End (NameBox
  "bn")))))) , (Branch ((Branch ((End (NameBox "bo")) , (End (NameBox "bp")))) ,
  (Branch ((End (NameBox "bq")) , (End (NameBox "br")))) )) )) )) )) )) , (End
  (NameBox "bs")))) , (End (NameBox "bt")))) )) ))
("{-, (-,-), (-,(-,-)), (-,(-,((-,((-,-),-)),(-,(-,-))))), (-,((-,-),-)), (-," ^
  "((-,-),(-,-))), (-,((-,-),(-,(-,(-,(-,-)))))), ((-,-),-)}");;

(* 74 nodes, ~350 nodes *)
expect
(Guide ("ab", (Branch ((End (NameBox "ab")) , (Guide ("ac", (Branch ((Guide
  ("ad", (Branch ((Guide ("ae", (Guide ("af", (Branch ((End (NameBox "af")) ,
  (Branch ((Branch ((Branch ((Guide ("ag", (Branch ((End (NameBox "ag")) , (End
  (NameBox "ah")))))) , (End (NameBox "ai")))) , (Branch ((End (NameBox "aj")) ,
  (Branch ((Branch ((Branch ((End (NameBox "ak")) , (End (NameBox "al")))) ,
  (Branch ((End (NameBox "am")) , (End (NameBox "an")))) )) , (End (NameBox "ao"
  )))) )))) , (Branch ((Guide ("ap", (End (NameBox "ap")))) , (End (NameBox "aq"
  )))) )) )) )))) , (End (NameBox "ar")))) )) , (Guide ("as", (Branch ((Branch
  ((Branch ((Guide ("at", (Branch ((End (NameBox "at")) , (Guide ("au", (End
  (NameBox "au")))))))) , (Guide ("av", (Branch ((Branch ((Branch ((Guide ("aw",
  (Branch ((Branch ((End (NameBox "aw")) , (End (NameBox "ax")))) , (Branch ((
  End (NameBox "ay")) , (End (NameBox "az")))) )) )) , (Guide ("ba", (Guide (
  "bb", (Branch ((End (NameBox "bb")) , (End (NameBox "bc")))) )))))) , (Branch
  ((Branch ((Branch ((Branch ((End (NameBox "bd")) , (End (NameBox "be")))) ,
  (Branch ((End (NameBox "bf")) , (End (NameBox "bg")))) )) , (End (NameBox "bh"
  )))) , (Branch ((Branch ((End (NameBox "bi")) , (Branch ((End (NameBox "bj"))
  , (End (NameBox "bk")))) )) , (Branch ((Branch ((End (NameBox "bl")) , (End
  (NameBox "bm")))) , (Branch ((End (NameBox "bn")) , (End (NameBox "bo")))) ))
  )) )) )) , (End (NameBox "bp")))))))) , (Branch ((Branch ((End (NameBox "bq"))
  , (Branch ((Guide ("br" , (Branch ((End (NameBox "br")) , (Branch ((Branch
  ((Branch ((End (NameBox "bs")) , (End (NameBox "bt")))) , (Branch ((End
  (NameBox "bu")) , (End (NameBox "bv")))) )) , (Branch ((Branch ((End (NameBox
  "bw")) , (End (NameBox "bx")))) , (End (NameBox "by")))))))))) , (Branch ((
  Guide ("bz", (End (NameBox "bz")))) , (Guide ("ca", (Branch ((End (NameBox
  "ca")) , (End (NameBox "cb")))) )))))))) , (Guide ("cc", (Branch ((Branch ((
  End (NameBox "cc")) , (Branch ((Guide ("cd", (Guide ("ce", (End (NameBox "ce")
  ))))) , (Branch ((Guide ("cf", (Branch ((End (NameBox "cf")) , (End (NameBox
  "cg")))))) , (Branch ((End (NameBox "ch")) , (End (NameBox "ci")))))))))) ,
  (Branch ((Branch ((Branch ((Branch((Branch ((End (NameBox "cj")) , (End (
    NameBox "ck")))) , (Branch ((End (NameBox "cl")) , (End (NameBox "cm"))))))
  , (Guide ("cn", (Branch ((End (NameBox "cn")) , (End (NameBox "co")))))))) ,
  (Guide ("cp", (Branch ((Guide ("cq", (End (NameBox "cq")))) , (Branch ((End
  (NameBox "cr")) , (End (NameBox "cs")))))))))) , (Branch ((Branch ((Branch ((
  End (NameBox "ct")) , (Branch ((End (NameBox "cu")) , (End (NameBox "cv"))))))
  , (Branch ((End (NameBox "cw")) , (Guide ("cx", (End (NameBox "cx")))))))),
  (Branch ((Branch ((Branch ((End (NameBox "cy")) , (End (NameBox "cz")))) ,
  (Branch ((End (NameBox "da")) , (End (NameBox "db")))))) , (Guide ("dc", (
  Branch ((End (NameBox "dc")) , (End (NameBox "dd")))))))))))))))))))) , (Guide
  ("de", (End (NameBox "de"))))))))))))))))
("{-, (-,-), (-,(-,-)), (-,(-,(-,-))), (-,(-,(-,(-,-)))), (-,(-,(-,(-,(-,((-," ^
  "-),-)))))), (-,(-,(-,(-,((-,-),-))))), (-,(-,(((-,-),-),-))), (-,(-,(((-,-" ^
  "),-),((-,-),(-,-))))), (-,((-,(-,(-,(-,((-,-),-))))),(-,(-,(-,((-,-),-))))" ^
  ")), (-,((((-,-),(-,-)),-),-)), ((-,-),-), ((-,-),(-,-)), ((-,-),(-,((-,-)," ^
  "-))), ((-,((-,-),-)),-)}");;

(* 514 nodes, 120 boxes *)
expect
(Branch ((Guide ("xebec", (End (NameBox "xebec")))) , (Branch ((End
  (NameBox "xenia")) , (Branch ((Branch ((Branch ((Branch ((Branch ((Branch ((
  Guide ("xenon", (Branch ((Branch ((Branch ((End (NameBox "xenon")) , (End (
  NameBox "xenophobia")))) , (Branch ((End (NameBox "xeric")) , (End (NameBox
  "xerophilous")))))) , (End (NameBox "xerophthalmia")))))) , (Branch ((Branch
  ((Guide ("xerophyte", (Branch ((End (NameBox "xerophyte")) , (End (NameBox
  "xylem")))))) , (Guide ("xylophone", (Guide ("y", (End (NameBox "y")))))))) ,
  (Branch ((Guide ("yacht", (Branch ((End (NameBox "yacht")) , (End (NameBox
  "yachting")))))) , (Branch ((Branch ((End (NameBox "yachtsman")) , (End (
  NameBox "yahoo")))) , (Branch ((End (NameBox "yak")) , (End (NameBox "yam"))))
  )))))))) , (Branch ((Branch ((Branch ((End (NameBox "yammer")) , (Branch ((End
  (NameBox "yank")) , (Guide ("yanqui", (End (NameBox "yanqui")))))))) , (Branch
  ((Guide ("yap", (End (NameBox "yap")))) , (Guide ("yard", (Branch ((End (
  NameBox "yard")) , (End (NameBox "yardage")))))))))) , (Branch ((Branch ((
  Branch ((End (NameBox "yardarm")) , (End (NameBox "yardman")))) , (Branch ((
  End (NameBox "yardmaster")) , (Branch ((End (NameBox "yardstick")) , (End (
  NameBox "yarn")))))))) , (Branch ((End (NameBox "yarrow")) , (End (NameBox
  "yaw")))))))))) , (Branch ((Branch ((Branch ((Branch ((End (NameBox "yawl")) ,
  (Branch ((Branch ((End (NameBox "yawn")) , (End (NameBox "yaws")))) , (Branch
  ((End (NameBox "yclept")) , (End (NameBox "ye")))))))) , (Branch ((Branch ((
  Branch ((End (NameBox "yea")) , (End (NameBox "yean")))) , (Branch ((End (
  NameBox "yeanling")) , (End (NameBox "year")))))) , (End (NameBox "yearbook"))
  )))) , (Branch ((Guide ("yearling", (Branch ((End (NameBox "yearling")) , (End
  (NameBox "yearlong")))))) , (Guide ("yearly", (Branch ((Branch ((End (NameBox
  "yearly")) , (End (NameBox "yearn")))) , (Branch ((End (NameBox "yearning")) ,
  (End (NameBox "yeast")))))))))))) , (Guide ("yeasty", (Guide ("yell", (Branch
  ((Guide ("yellow", (End (NameBox "yellow")))) , (Branch ((Branch ((End (
  NameBox "yellowish")) , (End (NameBox "yelp")))) , (Guide ("yen", (End (
  NameBox "yen")))))))))))))))) , (Branch ((Branch ((Branch ((Branch ((End (
  NameBox "yeoman")) , (Branch ((Branch ((End (NameBox "yeomanry")) , (Branch ((
  End (NameBox "yes")) , (End (NameBox "yesterday")))))) , (End (NameBox
  "yesteryear")))))) , (Guide ("yet", (Branch ((Branch ((End (NameBox "yet")) ,
  (Branch ((End (NameBox "yew")), (End (NameBox "yield")))))) , (End (NameBox
  "yielding")))))))) , (Branch ((Branch ((End (NameBox "yodel")) , (End (NameBox
  "yoga")))) , (Branch ((Branch ((Branch ((Branch ((End (NameBox "yogi")) , (End
  (NameBox "yogurt")))) , (Branch ((End (NameBox "yoke")) , (End (NameBox
  "yokel")))) )) , (Branch ((Branch ((End (NameBox "yolk")) , (End (NameBox
  "yon")))) , (Branch ((End (NameBox "yonder")) , (End (NameBox "yore")))))))) ,
  (End (NameBox "you")))))))) , (Branch ((End (NameBox "young")) , (End (NameBox
  "youngish")))))) )) , (Guide ("youngling", (Branch ((End (NameBox "youngling")
  ) , (Branch ((End (NameBox "youngster")) , (End (NameBox "younker")))))))))) ,
  (Branch ((Branch ((Branch ((Branch ((Branch ((End (NameBox "your")) , (Guide (
  "yours", (End (NameBox "yours")))))) , (Branch ((Branch ((Branch ((Branch ((
  Guide ("yourself", (End (NameBox "yourself")))) , (Branch ((End (NameBox
  "youth")) , (End (NameBox "youthful")))))) , (Guide ("yowl", (Branch ((End (
  NameBox "yowl")) , (End (NameBox "ytterbium")))) )))) , (End (NameBox
  "yttrium")))) , (Guide ("yuan", (End (NameBox "yuan")))))))) , (End (NameBox
  "yucca")))) , (Branch ((Branch ((Branch ((Branch ((End (NameBox "yule")) , (
  Branch ((End (NameBox "yuletide")) , (Branch ((Branch ((End (NameBox "yummy"))
  , (End (NameBox "z")))) , (End (NameBox "zany")))))))) , (End (NameBox "zeal")
  ))) , (Guide ("zealot", (Branch ((Branch ((End (NameBox "zealot")) , (Guide (
  "zealous", (Branch ((End (NameBox "zealous")) , (End (NameBox "zebra"))))))))
  , (Branch ((Guide ("zebu", (End (NameBox "zebu")))) , (End (NameBox "zed")))))
  ))))) , (End (NameBox "zeitgeist")))))) , (Branch ((End (NameBox "zenana")) ,
  (Branch ((Branch ((End (NameBox "zenith")) , (End (NameBox "zephyr")))) , (
  Branch ((Guide ("zeppelin", (End (NameBox "zeppelin")))) , (Branch ((Branch ((
  Branch ((Branch ((Branch ((End (NameBox "zero")) , (End (NameBox "zest")))) ,
  (Branch ((End (NameBox "zigzag")) , (End (NameBox "zinc")))))) , (Branch ((
  Branch ((End (NameBox "zing")) , (End (NameBox "zinnia")))) , (Branch ((End
  (NameBox "zip")) , (End (NameBox "zipper")))))))) , (Guide ("zippy", (End (
  NameBox "zippy")))))) , (Guide ("zircon", (Branch ((Branch ((Guide (
  "zirconium", (End (NameBox "zirconium")))) , (End (NameBox "zither")))) , (
  Branch ((Branch ((End (NameBox "zloty")) , (End (NameBox "zodiac")))) , (
  Branch ((End (NameBox "zombie")) , (End (NameBox "zonal"))))))))))))))))))))))
  ))))
("{-, (-,-), (-,(-,-)), (-,(-,(-,-))), (-,(-,(-,(-,-)))), (-,(-,(-,(-,(-,(-,(" ^
  "-,(-,(-,(((-,-),-),(-,-))))))))))), (-,(-,(-,(-,(-,(-,(-,(((-,-),-),(-,-))" ^
  "))))))), (-,(-,(-,(-,(-,(-,(((-,-),-),(-,-)))))))), (-,(-,(-,((-,-),((-,-)" ^
  ",-))))), (-,(-,(-,((-,(-,-)),-)))), (-,(-,(((((-,-),-),(-,-)),-),(-,-))))," ^
  " (-,((-,-),-)), (-,((-,(-,(-,(-,(-,(-,(((-,-),-),(-,-)))))))),(-,(-,(-,(-," ^
  "(-,(-,(((-,-),-),(-,-)))))))))), (-,(((-,-),-),(-,-))), (-,(((-,-),-),(-,(" ^
  "(-,-),-)))), (-,(((-,(-,-)),-),(-,(-,-)))), ((-,-),-), ((-,-),(-,(-,(-,(-," ^
  "-))))), ((-,(-,-)),-), (((-,-),-),(-,-)), (((-,-),-),(-,((-,-),-)))}");;
