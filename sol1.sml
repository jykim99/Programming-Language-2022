fun merge(list_a : int list, list_b : int list) =
        if null(list_a)
        then list_b
        else
                if null(list_b)
                then list_a
                else
                        if hd(list_a) < hd(list_b)
                        then hd(list_a)::merge(tl(list_a), list_b)
                        else hd(list_b)::merge(list_a, tl(list_b));

fun append (xs: int list, ys: int list) =
        if null xs
        then ys
        else hd(xs) :: append(tl(xs), ys);

fun reverse(list : int list) =
        if null(list)
        then []
        else append(reverse(tl(list)),[hd(list)]);

fun pi(a: int, b: int, c: int -> int) =
        if a > b
        then 1
        else c(a) * pi(a+1, b, c);

fun sum_list(xs: int list) = 
        if null(xs)
        then 0
        else hd(xs) + sum_list(tl(xs));

fun digits(num : int) =
        if num = 0
        then []
        else append(digits(num div 10), [num mod 10]);

fun additivePersistence(num : int) =
        if num div 10 = 0
        then 0
        else additivePersistence(sum_list(digits(num))) + 1;

fun digitalRoot(num: int) =
        if num div 10 = 0
        then num
        else digitalRoot(sum_list(digits(num)));