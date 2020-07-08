
function prepend_prefix!(v::Vector{<:Line},y)    
    for x in v
        prepend!(x.prefix.prefix, y)
        filter!(t -> !in(t,[ Token(:whitespace, "\n"),
                             Token(:literal, "—") ]), x.tokens)
    end
    filter!(t -> !isempty(t.tokens), v)
end
trimstring(x) = x
word_string(w) =
    trimstring(
        join(string.(filter(t-> !(t isa TokenPair && t.key==:paren), w[1].tokens))))

function parse_overview(namespace, title, w, t::Nothing)
    images = Line{NamedString,LineContent}[]
    inflections = Pair{String,Token}[]
    language, wordtype, genus = string((filter(t-> t isa Template,w[1].tokens)[end].arguments[1].second)), "",""
    ( word = Token(:literal, intern(word_string(w))),
      title = Token(:literal, intern(title)), 
      language = Token(:language,language),
      wordtype = Token(:wordtype,wordtype),
      inflections = inflections,
      images = images,
      wikitext = [ w[1], w[2]... ] )
end

image_argument_parser =
    Sequence(!!re"Bild[^[:digit:]]*",
             !!re"[[:digit:]]*",
             AtEnd()) do v
                 NamedString[ NamedString("image", v[2]),
                              NamedString("property", v[1]) ]
             end

function parse_overview(namespace, title, w, t::Template)
    images = Line{NamedString,LineContent}[]
    inflections = Pair{String, Token}[]
    overview_parser =
        Sequence(whitespace_maybe, !!re"[-\p{L}]+", whitespace_horizontal,
                 !!Optional(re".*[^ ]"),
                 Optional(re" *Übersicht *"),
                 !!Optional(re".*[^ ]"),
                 whitespace_maybe,
                 Repeat(newline)) do v
                     String[v[2],v[4],v[6]]
                 end
    
    language, wordtype, genus = parse(overview_parser, t.template)
    args = Pair{String,AbstractToken}[]
    for (i,a) in enumerate(t.arguments)
        if !isempty(a.second) && !isempty(a.second[1].tokens)
            val = a.second[1].tokens[1]
            imp = tryparse(image_argument_parser, a.first)
            if imp !== nothing
                append!(images, prepend_prefix!(a.second, imp))
            elseif a.first==""
                ## push!(args, "$i" => val)
            elseif a.first in ["Genus"]
                push!(args, a.first => val)
            else
                push!(inflections, a.first => Token(:literal,string(val)))
            end
        end
    end
    ( word = Token(:literal, intern(intern(word_string(w)))),
      title = Token(:literal, intern(title)), 
      language = Token(:language,language),
      wordtype = Token(:wordtype,wordtype),
      inflections = inflections,
      images = images,
      wikitext = [ w[1], w[2]..., Line(LineContent[t]) ] )
end


import CombinedParserTools
import CombinedParserTools.Tokens
function parse_overview(namespace, title, w, v::Vector{<:Line})
    parse_overview(namespace, title, w, isempty(v) ? nothing : v[1].tokens[1])
end

is_template(template::String) =
    IteratorParser(Template,template) do x
        x.name==template
    end
is_template_line(x::String,f::Function,T::Type) =
    map(f,is_template_line(x))
is_template_line(template::String) =
    IteratorParser(Line{NamedString,LineContent},template) do x
        !isempty(x.tokens) &&
            x.tokens[1] isa Template &&
            x.tokens[1].template==template
    end
is_template_line(pred::Function, T = Line{NamedString,LineContent}) =
    IteratorParser{T}("template") do x
        (x) isa Line &&
            !isempty(x.tokens) &&
            (x.tokens[1]) isa Template &&
            pred(x.tokens[1])
    end
is_template_line(T::Type, pred::Function) =
    is_template_line(pred, T)

ignore = (v) -> missing
ignore_heading = (v) -> begin
    length(v) == 1 ? [] : v[2:end]
end

wiktionary_de_content=[
    is_template_line("Übersetzungen", ignore, Missing) => (:translations, is_line()),
    is_template_line(t -> match(re"Übersicht", t.template)!==nothing) => (:overview, missing),
    is_template_line("Nebenformen", ignore, Missing) => (:variants, is_line()),
    is_template_line("Entlehnungen", ignore, Missing) => (:variants, is_line()),
    is_template_line("Nicht mehr gültige Schreibweisen") => (:deprecated, is_line()),
    is_template_line("Alternative Schreibweisen") => (:versions, is_line()),
    is_template_line("Anmerkungen", ignore, Missing) => (:annotations, is_line()),
    is_template_line("Anmerkung", ignore, Missing) => (:annotations, is_line()),
    is_template_line("Worttrennung", ignore, Missing) => (:hyphenation, is_line()),
    is_template_line("Aussprache", ignore, Missing) => (:phonetic, is_line()),
    is_template_line("Oberbegriffe", ignore, Missing) => (:superterms, is_line()),
    is_template_line("Unterbegriffe", ignore, Missing) => (:subterms, is_line()),
    is_template_line("Teilbegriffe", ignore, Missing) => (:parts, is_line()),
    is_template_line("Bedeutungen", ignore, Missing) => (:meaning, is_line()),
    is_template_line("Synonyme", ignore, Missing) => (:synonyms, is_line()),
    is_template_line("Weibliche Wortformen", ignore, Missing) => (:synonyms, is_line()),
    is_template_line("Männliche Wortformen", ignore, Missing) => (:synonyms, is_line()), # exists?
    is_template_line("Sinnverwandte Wörter", ignore, Missing) => (:synonyms, is_line()),
    is_template_line("Wortbildungen", ignore, Missing) => (:formations, is_line()),
    is_template_line("Gegenwörter", ignore, Missing) => (:antonyms, is_line()),
    is_template_line("Abkürzungen", ignore, Missing) => (:abbreviations, is_line()),
    is_template_line("Wortfamilie", ignore, Missing) => (:family, is_line()),
    is_template_line("Grammatische Merkmale", ignore, Missing) => (:grammar, is_line()),
    is_template_line("Redewendungen", ignore, Missing) => (:phrases, is_line()),
    is_template_line("Siehe auch", ignore, Missing) => (:etymology, is_line()),
    is_template_line("erweitern", ignore, Missing) => (:etymology, is_line()),
    is_template_line("Herkunft", ignore, Missing) => (:etymology, is_line()),
    is_template_line("Beispiele", ignore, Missing) => (:examples, is_line()),
    is_template_line("Beispiele fehlen") => (:examples, is_line()),
    is_template_line("erweitern") => (:examples, is_line()),
    is_template_line("Charakteristische Wortkombinationen", ignore, Missing) => (:combinations, is_line()),
    is_template_line("Referenzen", ignore, Missing) => (:references, is_line()),
    is_template_line("Quellen", ignore, Missing) => (:sources, is_line())
]

export wiktionary_defs
wiktionary_defs =
    Repeat(Sequence(
        :prefix => Repeat(is_line()),
        :word => Sequence(is_heading(x -> isequal(x.value,"2")),
                          Repeat(is_line())),
        :defs => Repeat(Sequence(is_heading(x -> isequal(x.value,"3")),
                                 greedy(wiktionary_de_content...; alt=[:lines => is_line() ]))
                        )
    ));

number_line = Sequence(
    map(value,IteratorParser(x->variable(x)==:number, Token, "index")),
    Repeat(IteratorParser(!isinformative, Token, "whitespace")),
    Repeat(IteratorParser(LineContent, "rest"))) do v
        v[1] => v[3]
    end


export wiki_meaning
function wiki_meaning(title,v;namespace = "wikt:de")
    L = Line{NamedString,LineContent}
    getval(val,p) =  p => ( ( haskey(val,p) && val[p]!==missing ) ? val[p] : L[] )
    fields = [ x.second[1] for x in wiktionary_de_content ]
    function inner(wt)
        meaning_data = Dict{String,Dict{Symbol,Vector{L}}}()
        function pushit(num,k,val)
            meandata = get!(meaning_data, num) do
                Dict{Symbol,Vector{Line{NamedString,LineContent}}}()
            end
            data = get!(() -> Line{NamedString,LineContent}[], meandata, k)
            push!(data, Line(NamedString[], val))
        end
        for (k,v) in wt[2]
            x = parse(
                Repeat(map(is_line()) do l
                       r = tryparse(number_line, l.tokens)
                       r === nothing ? "?" => l.tokens : r
                       end),
                v)
            lastnum = "?"
            for e in x
                num = e.first=="?" ? lastnum : e.first
                let is = tryparse(expand_numbers,InternedStrings.intern(num))
                    if is === nothing
                        pushit(num,k,e.second)
                    else
                        for i in is
                            pushit(i,k,e.second)
                        end
                    end
                end
                lastnum=num
            end
        end
        common = get(()->Dict{Symbol,Vector{L}}(), meaning_data,"[*]")
        for (m,val) in pairs(meaning_data)
            if !in(m, ["?","[*]"])
                for (f,v) in pairs(common)
                    if haskey(val,f)
                        append!(val[f],v)
                    else
                        val[f]=v
                    end
                end
            end
        end
        val = get(meaning_data,"?",Dict())
        base = parse_overview(namespace, title, v.word, getval(val,:overview).second)
        meanings = [ ( title=base.title
                       , word=base.word, order=m,
                       ( getval(val,p)
                         for p in fields)... )
                     for (m,val) in pairs(meaning_data)
                     if !in(m, ["?","[*]"])
                     ]
        
        ( merge(
                base,
                (;( getval(val,p)
                    for p in fields
                    if p != :overview)...
                 )),
          meanings
          )
    end
    [ inner(wt) for  wt = v.defs ]
end
