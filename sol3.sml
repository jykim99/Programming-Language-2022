datatype pattern = Wildcard | Variable of string | UnitP
                     | ConstP of int | TupleP of pattern list
                     | ConstructorP of string * pattern

datatype valu = Const of int | Unit | Tuple of valu list
                | Constructor of string * valu



(*
valu v 와 pattern p 가 주어지면 패턴이 매치되는지 아닌지 확인해야 한다.
만약 그렇다면, (string, valu) 페어를 생성한다.
순서는 상관없다.
 *)
fun match(v: valu, p: pattern) =
    case p of
        Wildcard => SOME([])
        | Variable(s) => SOME([(s, v)])
        | UnitP => (case v of
            Unit => SOME([])
            | _ => NONE)
        (* | UnitP => NONE *)
        | ConstP(num) => (case v of
            Const(n) => (if num = n then SOME([]) else NONE)
            | _ => NONE)
        (* | ConstP(num) => NONE *)
        | TupleP(pat) => (
            let fun tuple_recurse(acc: (string * valu) list option, patterns, valus) =
                case patterns of
                    [] => acc
                    | x::xs => 
                        let val tmp = match(hd(valus), hd(patterns))
                        in
                            case tmp of
                                NONE => tuple_recurse(acc, tl(patterns), tl(valus))
                                | SOME(e) => tuple_recurse(SOME(e @ valOf(acc)), tl(patterns), tl(valus))
                        end
            in
                case pat of
                    [] => SOME([])
                    | px::pxs => case v of
                        Tuple(vals) => if List.length(pat) <> List.length(vals)
                                        then NONE
                                        else    
                                            tuple_recurse(SOME([]), pat, vals)
                        | _ => NONE
            end)
        (* | TupleP(pat) => NONE *)
        (* 스트링비교 = 로 해도 되는구나!! *)
        | ConstructorP(sp, pat) => case v of
                                    Constructor(sv, vl) => let val tmp = match(vl, pat)
                                                            in
                                                                if sp = sv
                                                                then
                                                                case tmp of
                                                                    NONE => NONE
                                                                    | SOME(e) => SOME(e)
                                                                else NONE
                                                            end
                                    | _ => NONE
        (* | ConstructorP(sp, pat) => NONE *)



(*
    check_pat
    패턴 내에 존재하는 모든 값들이 서로 다른지 검사한다.
    패턴을 받아 쓰이믄 모든 value string을 반환하는 helper쓰면 좋음
    그거 구현할 때는 foldl 쓰면 유리할수도.
    List.exists 를 이용해서 겹치는게 있는지 검사해주는 helper쓰면 좋음
*)

fun check_pat (p: pattern) =
    let fun find_pattern_strings(acc: string list, pat: pattern) =
        case pat of
            Wildcard => acc
            | Variable(str) => str::acc
            | UnitP => acc
            | ConstP(_) => acc
            | ConstructorP(a, b) => find_pattern_strings(acc, b)
            | TupleP(li) =>
                case li of
                    [] => acc
                    | x::xs => find_pattern_strings(find_pattern_strings(acc, x), TupleP(xs))
        fun isDuplicated(acc: string list, li : string list) =
            case li of
                [] => true
                | x::xs =>
                    if List.exists(fn e => x = e) acc
                    then false
                    else isDuplicated(x::acc, xs)
    in
        let val strings = find_pattern_strings([], p)
        in
            isDuplicated([], strings)
        end
    end

(* val이나 | 빼먹지 좀 말자! *)
(* 이거에서 알게된거 => produce처럼 인자 따로 받는거나 튜플하나로 부르거나 똑같음 부를때도 똑같음 그니까 형도 같다고 볼 수 있음 *)
(* fun match(a: valu * pattern) =
    let val li = produce(a)
    in
        li
    end; *)


type name = string
datatype RSP =
    ROCK
    | SCISSORS
    | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament =
    PLAYER of name * (RSP strategy ref)
    | MATCH of tournament * tournament

fun onlyOne(one: RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one: RSP, two: RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one: RSP, two: RSP, three: RSP) = Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

fun next(strategyRef) =
    let val Cons(rsp, func) = !strategyRef
    in
        strategyRef := func();
        rsp
    end



(* ref 키워드 우선순위 높은듯? *)
fun whosWinner(t) = 
    let fun winner(t1, t2) =
        case t1 of 
            MATCH(a, b) =>
                (case t2 of
                    MATCH(c, d) => winner(winner(whosWinner(a), whosWinner(b)), winner(whosWinner(c), whosWinner(d)))
                    | PLAYER(_) => winner(winner(whosWinner(a), whosWinner(b)), t2)) (*여기서 괄호 안하면 밑에꺼랑 겹친다고 뜸*)
            | PLAYER(a, b) =>
                case t2 of
                    MATCH(c, d) => winner(t1, winner(whosWinner(c), whosWinner(d)))
                    | PLAYER(c, d) =>
                        let val s1_value = next(b)
                            val s2_value = next(d)
                        in
                            if ((s1_value = ROCK andalso s2_value = SCISSORS)
                                orelse
                                (s1_value = SCISSORS andalso s2_value = PAPER)
                                orelse
                                (s1_value = PAPER andalso s2_value = ROCK))
                            then
                                t1
                            else
                                if ((s1_value = ROCK andalso s2_value = ROCK)
                                orelse
                                (s1_value = SCISSORS andalso s2_value = SCISSORS)
                                orelse
                                (s1_value = PAPER andalso s2_value = PAPER))
                                then
                                    winner(t1, t2)
                                else
                                    t2
                        end
    in
        case t of
            MATCH(a, b) => winner(a, b)
            | PLAYER(_) => t
    end;

val p1 = ConstructorP("what", TupleP([TupleP([UnitP, Wildcard, ConstP(1), TupleP([Variable("b"), Variable("c"), ConstP(3)]), Variable("a")])]))
val v1 = Constructor("what", Tuple([Tuple([Unit, Const(1), Const(1), Tuple([Const(4), Unit, Const(3)]), Tuple([Const(5),Const(6)])])]))
val testM = match(v1,p1)

(* whosWinner(MATCH(PLAYER("s", ref s), MATCH(PLAYER("rp", ref rp), PLAYER("r", ref r)))); *)