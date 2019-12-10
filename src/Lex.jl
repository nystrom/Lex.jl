module Lex

export null, epsilon, star, ch, str, alt, concat, deriv

import Base: cmp, isempty, match

""" Regex type """
abstract type Regex end
struct Null <: Regex end
struct Empty <: Regex end
struct Ch <: Regex
  ch :: Char
end
struct Concat <: Regex
  r1 :: Regex
  r2 :: Regex
end
struct Alt <: Regex
  r1 :: Regex
  r2 :: Regex
end
struct Star <: Regex
  r :: Regex
end

null = Null()
epsilon = Empty()
star(r) = Star(r)
ch(ch) = Ch(ch)
str(s) = begin
    if isempty(s)
        epsilon
    else
        concat(ch(s[1]), str(s[2:end]))
    end
end

""" Regex derivative WRT a character """
function deriv(a::Char, r::Null) null end
function deriv(a::Char, r::Empty) null end
function deriv(a::Char, r::Ch)
    if a == r.ch
        epsilon
    else
        null
    end
end
function deriv(a::Char, r::Concat)
    alt(concat(deriv(a, r.r1), r.r2), concat(nullable(r.r1), r.r2))
end
function deriv(a::Char, r::Alt)
    alt(deriv(a, r.r1), deriv(a, r.r2))
end
function deriv(a::Char, r::Star)
    concat(deriv(a, r.r), r)
end

""" Regex derivative WRT a string """
function deriv(a::String, r::Regex)::Regex
    if isempty(a)
        r
    else
        deriv(a[2:end], deriv(a[1], r))
    end
end

""" Create an Alt regex, normalizing """
function alt(r1::Null, r2::Regex) r2 end
function alt(r1::Regex, r2::Null) r1 end
function alt(r1::Null, r2::Null) r1 end
function alt(r1::Empty, r2::Empty) r1 end
function alt(r1::Regex, r2::Regex)
    c = cmp(r1, r2)
    if c < 0
        Alt(r1, r2)
    elseif c > 0
        Alt(r2, r1)
    else
        r1
    end
end

""" Create a Concat regex, normalizing """

function concat(r1::Null, r2::Regex) null end
function concat(r1::Null, r2::Null) null end
function concat(r1::Empty, r2::Null) null end
function concat(r1::Ch, r2::Null) null end
function concat(r1::Alt, r2::Null) null end
function concat(r1::Concat, r2::Null) null end
function concat(r1::Star, r2::Null) null end

function concat(r1::Empty, r2::Regex) r2 end
function concat(r1::Null, r2::Empty) r1 end
function concat(r1::Empty, r2::Empty) r1 end
function concat(r1::Ch, r2::Empty) r1 end
function concat(r1::Alt, r2::Empty) r1 end
function concat(r1::Concat, r2::Empty) r1 end
function concat(r1::Star, r2::Empty) r1 end

function concat(r1::Alt, r2::Alt) alt(alt(concat(r1.r1, r2.r1), concat(r1.r2, r2.r1)),
                                      alt(concat(r1.r1, r2.r2), concat(r1.r2, r2.r2))) end
function concat(r1::Alt, r2::Regex) alt(concat(r1.r1, r2), concat(r1.r2, r2)) end
function concat(r1::Regex, r2::Alt) alt(concat(r1, r2.r1), concat(r1, r2.r1)) end

function concat(r1::Regex, r2::Regex) Concat(r1, r2) end

""" If r is nullable, return Empty (true), else return Null (false) """
function nullable(r::Null) r end
function nullable(r::Empty) r end
function nullable(r::Ch) null end
function nullable(r::Concat) concat(nullable(r.r1), nullable(r.r2)) end
function nullable(r::Alt) alt(nullable(r.r1), nullable(r.r2)) end
function nullable(r::Star) epsilon end

""" Return true if the regex matches the string """
function match(r::Regex, s::String)::Bool
    isempty(nullable(deriv(s, r)))
end

""" Return true if the regex is empty """
function isempty(r::Regex) false end
function isempty(r::Empty) true end

""" Compare two regexes, structurally """
function cmp(r1::Null, r2::Null) 0 end
function cmp(r1::Null, r2::Regex) -1 end

function cmp(r1::Empty, r2::Null) 1 end
function cmp(r1::Empty, r2::Empty) 0 end
function cmp(r1::Empty, r2::Regex) -1 end

function cmp(r1::Ch, r2::Null) 1 end
function cmp(r1::Ch, r2::Empty) 1 end
function cmp(r1::Ch, r2::Ch) cmp(r1.ch, r2.ch) end
function cmp(r1::Ch, r2::Regex) -1 end

function cmp(r1::Alt, r2::Null) 1 end
function cmp(r1::Alt, r2::Empty) 1 end
function cmp(r1::Alt, r2::Ch) 1 end
function cmp(r1::Alt, r2::Alt)
    cmp1 = cmp(r1.r1, r2.r1)
    cmp2 = cmp(r1.r2, r2.r2)
    if cmp1 < 0
        cmp1
    elseif cmp1 == 0
        cmp2
    else
        cmp1
    end
end
    
function cmp(r1::Alt, r2::Regex) -1 end

function cmp(r1::Concat, r2::Null) 1 end
function cmp(r1::Concat, r2::Empty) 1 end
function cmp(r1::Concat, r2::Ch) 1 end
function cmp(r1::Concat, r2::Alt) 1 end
function cmp(r1::Concat, r2::Concat)
    cmp1 = cmp(r1.r1, r2.r1)
    cmp2 = cmp(r1.r2, r2.r2)
    if cmp1 < 0
        cmp1
    elseif cmp1 == 0
        cmp2
    else
        cmp1
    end
end
function cmp(r1::Concat, r2::Regex) -1 end

function cmp(r1::Star, r2::Null) 1 end
function cmp(r1::Star, r2::Empty) 1 end
function cmp(r1::Star, r2::Ch) 1 end
function cmp(r1::Star, r2::Alt) 1 end
function cmp(r1::Star, r2::Concat) 1 end
function cmp(r1::Star, r2::Star) 
    cmp(r1.r, r2.r)
end
function cmp(r1::Star, r2::Regex) -1 end


# -- factories
# char c = Ch c
# epsilon = Empty
# r1 <|> r2 = alt r1 r2
# r1 <*> r2 = concat r1 r2
# many r = Star r
# many1 r = concat r (Star r)
# chars "" = epsilon
# chars (c:"") = Ch c
# chars (c:cs) = Ch c <|> chars cs
# 
# digit = chars ['0'..'9']
# number = many1 digit
# lower = chars ['a'..'z']
# upper = chars ['A'..'Z']
# letter = lower <|> upper
# word = many1 letter
# ident = letter <*> (many (letter <|> digit))

end # module
