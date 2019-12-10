module Lex

export null, epsilon, star, ch, str, alt, concat
export plus, option
export chars, digit, number, lower, upper, letter, word, ident
export deriv

import Base: cmp, isempty, match, isless, isgreater, isequal

""" Regex type """
abstract type Regex end
struct Ch <: Regex
  ch :: Char
end
struct Concat <: Regex
  rs :: Vector{Regex}
end
struct Alt <: Regex
  rs :: Vector{Regex}
end
struct Star <: Regex
  r :: Regex
end

""" Regex derivative WRT a character """
function deriv(a::Char, r::Ch)
    if a == r.ch
        epsilon
    else
        null
    end
end
function deriv(a::Char, r::Concat)
    if isempty(r.rs)
        null
    elseif length(r.rs) == 1
        deriv(a, r.rs[1])
    else
        alt([concat(Regex[deriv(a, r.rs[1]), concat(r.rs[2:end])]), concat(Regex[nullable(r.rs[1]), concat(r.rs[2:end])])])
    end
end
function deriv(a::Char, r::Alt)
    if isempty(r.rs)
        null
    elseif length(r.rs) == 1
        deriv(a, r.rs[1])
    else
        alt(map(s -> deriv(a, s), r.rs))
    end
end
function deriv(a::Char, r::Star)
    concat(Regex[deriv(a, r.r), r])
end

""" Regex derivative WRT a string """
function deriv(a::String, r::Regex)::Regex
    if isempty(a)
        r
    else
        deriv(a[2:end], deriv(a[1], r))
    end
end

""" Create a Alt regex, normalizing """
function alt(rs::Vector{T})::Regex where {T <: Regex}
    rst = Regex[]
    flat(v) = for x in v
        if isa(x, Concat) flat(x.rs) else push!(rst, x) end
    end
    flat(rs)
    rs1 = filter(r -> !isnull(r), rst)
    rs2 = sort(rs1)
    if length(rs2) == 1
        rs2[1]
    else
        Alt(rs2)
    end
end


""" Return true if the regex is null """
isnull(r::Alt) = isempty(r.rs)
isnull(r::Regex) = false
""" Return true if the regex is empty """
isempty(r::Concat) = isempty(r.rs)
isempty(r::Regex) = false

""" Create a Concat regex, normalizing """
function concat(rs::Vector{T})::Regex where {T <: Regex}
    rst = Regex[]
    flat(v) = for x in v
        if isa(x, Concat) flat(x.rs) else push!(rst, x) end
    end
    flat(rs)
    if Base.any(isnull, rst)
        null
    elseif length(rst) == 1
        rst[1]
    else
        Concat(rst)
    end
end

""" If r is nullable, return Empty (true), else return Null (false) """
function nullable(r::Ch)::Regex null end
function nullable(r::Concat)::Regex concat(map(nullable, r.rs)) end
function nullable(r::Alt)::Regex alt(map(nullable, r.rs)) end
function nullable(r::Star)::Regex epsilon end

""" Return true if the regex matches the string """
function match(r::Regex, s::String)::Bool
    isempty(nullable(deriv(s, r)))
end


""" Compare two regexes, structurally """
function cmp(r1::Ch, r2::Ch) cmp(r1.ch, r2.ch) end
function cmp(r1::Ch, r2::Regex) -1 end

function cmp(r1::Alt, r2::Ch) 1 end
function cmp(r1::Alt, r2::Alt)
    cmp(r1.rs, r2.rs)
end
function cmp(r1::Alt, r2::Regex) -1 end

function cmp(r1::Concat, r2::Ch) 1 end
function cmp(r1::Concat, r2::Alt) 1 end
function cmp(r1::Concat, r2::Concat)
    cmp(r1.rs, r2.rs)
end
function cmp(r1::Concat, r2::Regex) -1 end

function cmp(r1::Star, r2::Ch) 1 end
function cmp(r1::Star, r2::Alt) 1 end
function cmp(r1::Star, r2::Concat) 1 end
function cmp(r1::Star, r2::Star) 
    cmp(r1.r, r2.r)
end
function cmp(r1::Star, r2::Regex) -1 end

function isless(r1::Regex, r2::Regex) cmp(r1, r2) < 0 end
function isgreater(r1::Regex, r2::Regex) cmp(r1, r2) < 0 end
function isequal(r1::Regex, r2::Regex) cmp(r1, r2) == 0 end

null = Alt(Regex[])
epsilon = Concat(Regex[])
star(r) = Star(r)
ch(ch) = Ch(ch)
str(s) = concat(map(ch, collect(s)))
plus(r) = concat(Regex[r, star(r)])
option(r) = alt(Regex[r, epsilon])
chars(s) = alt(map(ch, collect(s)))
digit = chars("0123456789")
number = plus(digit)
lower = chars("abcdefghijklmnopqrstuvwxyz")
upper = chars("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
letter = alt(Regex[lower, upper])
word = plus(letter)
ident = concat(Regex[letter, star(alt(Regex[letter, digit]))])



end # module
