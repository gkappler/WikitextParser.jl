using AutoHashEquals

Headline{T} = NamedTuple{(:level, :title), Tuple{Int, T}}
Base.show(io::IO, v::Type{Headline{T}}) where T = print(io, "Headline{", T, "}")

function Base.show(io::IO, v::Headline{T}) where T
    print(io,repeat("=", v.level), " ")
    print(io, v.title)
    print(io," ", repeat("=", v.level))
end

Title{S,T} = NamedTuple{(:title, :text), Tuple{S, T}}
Base.show(io::IO, v::Type{Title{S,T}}) where {S,T} = print(io, "Title{", T, "}")
function Base.show(io::IO, v::Title{S,T}) where {S,T}
    println(io,v.title)
    print(io, v.text)
end


export substnothing
substnothing(default, x) =
    x === nothing ? default : x

export WikiLink
@auto_hash_equals struct WikiLink <: AbstractToken ## todo: make Tokens String, in token_lines make tokens with name=field 
    namespace_page::Token
    anchor::Token
    label::Token
    function WikiLink(namespace::AbstractString, page::AbstractString, anchor::AbstractString, label::AbstractString, addlabel="")
        new(Token(intern(namespace), intern(page)), Token(:anchor,intern(anchor)),
            Token(:label, intern(label=="" ? page : label) * addlabel))
    end
    function WikiLink(namespace::Token, page::Token, anchor::Token, label::Token)
        new(namespace, page, anchor, label)
    end
end


Base.show(io::IO, ::MIME"text/x-wiki", x::WikiLink) =
    print(io, "[[", value(x.namespace) == "" ? "" : value(x.namespace) * ":",
          x.page,
          x.anchor == "" ? "" : "#" * x.anchor,
          x.label == "" ? "" : "|" * x.label,
          "]]")

Base.show(io::IO, x::WikiLink) =
          printstyled(io, value(x.label)=="" ? value(x.page) : value(x.label); bold=true,
                      color=36)

export @substringorempty
macro substringorempty(x,range)
    :( $(esc(x)) === nothing ? "" : $(esc(x))[$(esc(range))] )
end


export TemplateArgument, Template, LineContent
"""
Wikitext template arguments are stored with `ta=name => value`.
An unnamed template argument has `t.first==""`.
The value is 
"""
TemplateArgument{I,T} = Pair{String,Vector{Line{I,T}}}

"""
A wikitext template struct
"""
@auto_hash_equals struct Template{I,T} <: AbstractToken
    template::String
    arguments::Vector{TemplateArgument{I,T}}
    Template(t,a::Vector{TemplateArgument{I,T}}) where {I,T} =
        new{I,T}(t,[ k => v for (k,v) in a])
    Template{I,T}(t,a::Vector) where {I,T} =
        new{I,T}(t,[ k => convert(Vector{Line{I,T}},v) for (k,v) in a])
    Template(t,a::Vector) =
        new{Any,Any}(t,[ k => v for (k,v) in a])
end

import CombinedParserTools.Tokens: isinformative
CombinedParserTools.Tokens.isinformative(i::Template)  = true

Template(a::String) = Template(a,TemplateArgument{NamedString,LineContent}[])

function Base.show(io::IO, x::Template) where T 
    print(io, "{{")
    print(io, x.template)
    for a in x.arguments
        print(io, "|")
        if a isa Pair
            if !isempty(a.first)
                print(io, a.first,"=",a.second)
            else
                print(io, a.second)
            end
        else
            print(io, a)
        end        
    end
    print(io, "}}")
end

export TemplateParameter
TemplateParameter = TokenPair{Vector{LineContent}, Vector{Line{NamedString,LineContent}}}
