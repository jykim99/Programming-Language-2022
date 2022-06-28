(* Problem 1 *)

datatype expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr

datatype formula = TRUE
    | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr

fun eval(f : formula) = 
    case f of
       TRUE => true
     | FALSE => false
     | NOT(a) => not(eval(a))
     | ANDALSO(a, b) => eval(a) andalso eval(b)
     | ORELSE(a, b) => eval(a) orelse eval(b)
     | IMPLY(a, b) =>
        let fun imply(fa, fb) =
            let val result_a = eval(fa)
                val result_b = eval(fb)
            in
            if result_b
            then true
            else not(result_a)
            end
        in
            imply(a, b)
        end
    | LESS(ea, eb) =>
        let fun less (x: expr, y: expr) =
            let fun parseExpr(e) =
                case e of
                    NUM(num) => num
                    | PLUS(e1, e2) => parseExpr(e1) + parseExpr(e2)
                    | MINUS(e1, e2) => parseExpr(e1) - parseExpr(e2)
            in
                parseExpr(x) < parseExpr(y)
            end
        in
            less(ea, eb)
        end


(* Problem 2 *)

type name = string

datatype metro = STATION of name
                | AREA of name * metro
                | CONNECT of metro * metro

fun checkMetro (metro_of_check_metro :metro) =
    let fun find(name_list_of_find : name list, metro_of_find: metro) =
        let fun contain(element_of_contain : name, list_of_contain : name list) =
            case list_of_contain of
                [] => false
                | x::xs =>
                    if element_of_contain = x
                    then true
                    else contain(element_of_contain, xs)
        in
            case metro_of_find of
                STATION(name_of_station) => contain(name_of_station, name_list_of_find)
                | AREA(name_of_area, metro_of_area) => find(name_list_of_find @ [name_of_area], metro_of_area)
                | CONNECT(connect_metro_1, connect_metro_2) => find(name_list_of_find, connect_metro_1) andalso find(name_list_of_find, connect_metro_2)
        end
    in
        find([], metro_of_check_metro)
    end


(* Problem 3 *)

datatype 'a lazyList = nullList
                    | cons of 'a * (unit -> 'a lazyList)

(* to test *)
fun toList(lazyListVal: 'a lazyList) =
    case lazyListVal of
        nullList => []
        | cons(value, nextLazyList) => value::toList(nextLazyList())

(* i *)

fun seq (first: int, last: int) =
    if first > last
    then nullList
    else cons(first, (fn () => seq(first + 1, last)))

fun infSeq(first: int) =
    cons(first, fn () => infSeq(first + 1))

fun firstN(lazyListVal: 'a lazyList, n: int) =
    case lazyListVal of
        nullList => []
        | cons(value, nextLazyList) => 
            if n > 0
            then value::firstN(nextLazyList(), n - 1)
            else []

fun Nth(lazyListVal: 'a lazyList, n: int) =
    case lazyListVal of
        nullList => NONE
        | cons(value, nextLazyListFn) => 
            if n = 1
            then SOME(value)
            else Nth(nextLazyListFn(), n - 1)

fun filterMultiples(lazyListVal: int lazyList, n: int) =
    case lazyListVal of
        nullList => nullList
        | cons(value, nextLazyListFn) =>
            if value mod n = 0
            then filterMultiples(nextLazyListFn(), n)
            else cons(value, fn () => filterMultiples(nextLazyListFn(), n))
        
(* ii *)
fun primes() =
    let fun sieve(lazyListVal: int lazyList) =
        case lazyListVal of
            nullList => nullList
            | cons(value, nextLazyListFn) =>
                cons(value, fn () => sieve(filterMultiples(nextLazyListFn(), value)))
    in
        sieve(infSeq(2))
    end        