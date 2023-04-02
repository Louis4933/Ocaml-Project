#use "Test.ml" ;;
(* Tests de la division euclidienne                                                                          *)
(* Les tests sont effectués sur des couples d'entiers compris entre -100 et 100 dont le second est *non nul* *)
(* (d'où l'utilisation du filtre pour éviter des divisions par zéro).                                        *)
let gen_intcouple =
  let gen_dividend =                            Generator.int (-100) 100
  and gen_divisor  = Generator.filter ((<>) 0) (Generator.int (-100) 100)
    in Generator.combine gen_dividend gen_divisor ;;
let red_intcouple =
  let red_dividend =                           Reduction.int
  and red_divisor  = Reduction.filter ((<>) 0) Reduction.int
    in Reduction.combine red_dividend red_divisor ;;
let test_intcouple = Test.make_test gen_intcouple red_intcouple ;;

(* Construction des tests *)
let test_quorem       = test_intcouple  (fun (a, b) -> (a = (a / b) * b + (a mod b))) ;;
let test_quorem_wrong = test_intcouple   (fun (a, b) -> (a = (a / b) * b - (a mod b))) ;;

(* Exécution des tests *)
Test.check    100 test_quorem       ;;
Test.check    100 test_quorem_wrong ;;
Test.fails_at 100 test_quorem       ;;
Test.fails_at 100 test_quorem_wrong ;;
Test.execute  100 [test_quorem ; test_quorem_wrong] ;;


(*Test sur l'addition de deux entiers*)
let gen_couple = 
  let gen_numberone =  Generator.int (-10) 10
  and gen_numbertwo  = Generator.int (-10) 10
    in Generator.combine gen_numberone gen_numbertwo ;;

let red_couple =
  let red_numberone = Reduction.int
  and red_numbertwo  = Reduction.int
    in Reduction.combine red_numberone red_numbertwo ;;

let test_couple = Test.make_test gen_couple red_couple ;;

let test_add       = test_couple  (fun (a, b) -> a = (a + b - b)) ;;
let test_add_wrong = test_couple   (fun (a, b) -> a = (a - b - b)) ;;

Test.check    100 test_add      ;;
Test.check    100 test_add_wrong ;;
Test.fails_at 100 test_add       ;;
Test.fails_at 100 test_add_wrong ;;
Test.execute  100 [test_add ; test_add_wrong] ;;


(*Test multiplication de deux floats*)

let gen_couple = 
  let gen_floatone =  Generator.float_nonneg 100.0
  and gen_floattwo  = Generator.filter ((<>) 0.0) (Generator.float_nonneg 100.0)
    in Generator.combine gen_floatone gen_floattwo ;;

let red_couple =
  let red_floatone = Reduction.float_nonneg
  and red_floattwo  = Reduction.filter ((<>) 0.0) (Reduction.float_nonneg)
    in Reduction.combine red_floatone red_floattwo ;;

let test_couple = Test.make_test gen_couple red_couple ;;

let test_multiply       = test_couple  (fun (a, b) -> (a < (((a *. b) /. b) +. 0.1)) && (a > ((a *. b) /. b -. 0.1))) ;; (* epsilon de 0.1 pour éviter les problèmes d'arrondis sur les divisions avec les float *)
let test_multiply_wrong = test_couple   (fun (a, b) -> a = (a *. b)) ;;

Test.check    100 test_multiply      ;;
Test.check    100 test_multiply_wrong ;;
Test.fails_at 100 test_multiply       ;;
Test.fails_at 100 test_multiply_wrong ;;
Test.execute  100 [test_multiply ; test_multiply_wrong] ;;



(*Test concaténation avec des string*)

let gen_stringcouple =
  let gen_string = Generator.string 10 Generator.char in
    Generator.combine gen_string gen_string ;;
let red_stringcouple =
  let red_string = Reduction.string Reduction.char in
    Reduction.combine red_string red_string ;;
let test_stringcouple = Test.make_test gen_stringcouple red_stringcouple ;;

let test_append       = test_stringcouple (fun (l1, l2) -> String.length (l1 ^ l2) = (String.length l1) + (String.length l2)) ;;
let test_append_wrong = test_stringcouple (fun (l1, l2) -> String.length (l1 ^ l2) = (String.length l1) - (String.length l2)) ;;

Test.check    100 test_append       ;;
Test.check    100 test_append_wrong ;;
Test.fails_at 100 test_append       ;;
Test.fails_at 100 test_append_wrong ;;
Test.execute  100 [test_append ; test_append_wrong] ;;

(*
(* Tests sur la concaténation de listes *)
(* Les tests sont effectués sur des listes d'au plus dix caractères. *)
let gen_charlistcouple =
  let gen_charlist = Generator.list 10 (Generator.char) in
    Generator.combine gen_charlist gen_charlist ;;
let red_charlistcouple =
  let red_charlist = Reduction.list     Reduction.char   in
    Reduction.combine red_charlist red_charlist ;;
let test_charlistcouple = Test.make_test gen_charlistcouple red_charlistcouple ;;

(* Constructon des tests *)
let test_appendChar      = test_charlistcouple (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) + (List.length l2)) ;;
let test_appendChar_wrong = test_charlistcouple (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) - (List.length l2)) ;;

(* Exécution des tests *)
Test.check    100 test_appendChar      ;;
Test.check    100 test_appendChar_wrong ;;
Test.fails_at 100 test_appendChar       ;;
Test.fails_at 100 test_appendChar_wrong ;;
Test.execute  100 [test_appendChar ; test_appendChar_wrong] ;;


(* Tests sur la concaténation de listes                                         *)
(* Les tests sont effectués sur des listes d'au plus dix entiers entre 0 et 10. *)
let gen_intlistcouple =
  let gen_intlist = Generator.list 10 (Generator.int_nonneg 10) in
    Generator.combine gen_intlist gen_intlist ;;
let red_intlistcouple =
  let red_intlist = Reduction.list     Reduction.int_nonneg     in
    Reduction.combine red_intlist red_intlist ;;
let test_intlistcouple = Test.make_test gen_intlistcouple red_intlistcouple ;;

(* Constructon des tests *)
let test_append       = test_intlistcouple (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) + (List.length l2)) ;;
let test_append_wrong = test_intlistcouple (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) - (List.length l2)) ;;

(* Exécution des tests *)
Test.check    100 test_append       ;;
Test.check    100 test_append_wrong ;;
Test.fails_at 100 test_append       ;;
Test.fails_at 100 test_append_wrong ;;
Test.execute  100 [test_append ; test_append_wrong] ;;
*)




