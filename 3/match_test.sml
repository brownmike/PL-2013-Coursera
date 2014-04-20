exception NotPassed

fun have_same_items l1 l2 = (* return true if l1 has the same items than l2 *)
    case l1 of
    head :: tail => not (List.exists (fn x => x = head) l2) 
                 orelse have_same_items tail l2
      | []           => true

fun same_items (lo1, lo2) =
    case (lo1, lo2) of
    (NONE, NONE)       => true
      | (SOME l1, SOME l2) => have_same_items l1 l2
      | _                  => false


fun test_match () =
  let val tests =
  [
   same_items(match(Const 10, Wildcard), SOME []),
   same_items(match(Unit, Wildcard), SOME []),
   same_items(match(Constructor("Test", Unit), Wildcard), SOME []),
   same_items(match(Tuple [Unit, Const 10], Wildcard), SOME []),
   same_items(match(Unit, UnitP), SOME []),
   same_items(match(Const 10, ConstP 10), SOME []),
   same_items(match(Const 10, ConstP 20), NONE),
   same_items(match(Const 10, Variable "x"), SOME [("x",Const 10)]),
   same_items(match(Constructor("Test", Const 35),
            ConstructorP("Test", Variable "y")), SOME [("y",Const 35)]),
   same_items(match(Constructor("Test", Const 35), 
            ConstructorP("Fail", Variable "y")), NONE),

   same_items(match(Tuple([Const 1, Const 2]), 
            TupleP([Variable "x", Variable "y", Variable "z"])), NONE),
   same_items(match(Tuple [Const 1, Const 2, Const 3, Const 4], 
            TupleP [Variable "w",Variable "x", Variable "y", 
                Variable "z"]), 
          SOME [("z",Const 4),("y",Const 3),("x",Const 2),("w",Const 1)]),
   same_items(match(Tuple [Const 1, Const 2, Const 3, Const 4], 
            TupleP [ConstP 1,Variable "x", ConstP 3, Variable "z"]),
          SOME [("z",Const 4),("x",Const 2)]),
   same_items(
     match(Constructor("A", 
               Tuple([Unit, Const 10, Const 20, 
                  Tuple([Unit, 
                     Constructor("B", Const 30)])])), 
       ConstructorP("A", 
            TupleP([UnitP, Variable "x", ConstP 20, 
                TupleP([UnitP, 
                    ConstructorP("B", 
                             Variable "y")])]))),
     SOME [("y", Const 30), ("x", Const 10)])
    ]
    in
    List.all (fn x => x) tests orelse raise NotPassed
    end

val test_q11 = test_match()

exception NotPassed

fun test_first_match () =
    (first_match (Const 10) [ConstP 10, Variable "x"] = SOME [] 
     andalso
     first_match (Const 10) [Variable "x", ConstP 10] = SOME [("x",Const 10)] 
     andalso
     first_match (Const 10) [Variable "x", Variable "y"] = SOME [("x",Const 10)]
     andalso
     first_match (Const 10) [UnitP, ConstructorP("Test", ConstP 10), Wildcard, 
                 Variable "y", ConstP 10] = SOME [] 
     andalso
     first_match (Const 10) [UnitP, ConstructorP("Test", ConstP 10), 
                 Variable "y", Wildcard, ConstP 10] = 
       SOME [("y", Const 10)]
     andalso first_match (Const 10) [UnitP] = NONE
     andalso
     first_match (Constructor ("foo", Unit)) [ConstructorP("foo", UnitP)] =
       SOME []
     andalso
     first_match (Constructor ("foo", Unit)) [ConstructorP("bar", UnitP)] = NONE
    )
    orelse raise NotPassed


val test_q12 = test_first_match()
