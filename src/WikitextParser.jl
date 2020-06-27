module WikitextParser
using Nullables
using InternedStrings

using CombinedParsers
import CombinedParsers: Repeat_max
using CombinedParsers.Regexp
import CombinedParsers.Regexp: word, whitespace_maybe, whitespace_horizontal
using CombinedParserTools
import CombinedParserTools: newline, inline, whitespace_newline, emptyline
using CombinedParserTools.Tokens
import CombinedParserTools.Tokens: html, attributes, simple_tokens
import TextParse
import TextParse: Numeric


dash = re" *- *"
@syntax int_range = Sequence(
    Numeric(Int), # 1
    dash,    # 2
    Numeric(Int)  # 3
) do v
    v[1]:v[3]
end;

# Julia Base.collect can be used to convert 
@syntax int_vector = map(collect, int_range);

# ## Joining numbers and ranges
@syntax numbers = map(join(
    Repeat(Either(
        map(ns -> String[ "$x" for x in ns ],
            int_vector),
        map(x  -> String["$x"], Numeric(Int)),
        !!re"[[:digit:]]+[a-z]"
    )),
    re" *, *"
)) do v
    vcat(v...)::Vector{<:AbstractString}
end;

# ## Inclusion in a wikitext parser
# Long and complicated texts like the Wikipedia can be parsed with `CombinedParsers.jl`.
# The parsers are less pain to write and execute at speeds comparably to PCRE implemented in C, the regular expressions industry standard.
# `CombinedParsers.jl` can inter-operate with Julia packages `TextParse.jl`.

@syntax expand_numbers = Sequence(2,"[",numbers,"]");

include("types.jl")

list_item = Sequence(1, !!re"[*]+|[#]+", whitespace_horizontal)

# URL_re = raw"(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'\".,<>?«»“”‘’]))"




wiki_link(token=AnyChar();namespace = "wikt:de") = 
    Sequence(
        "[[",
        Optional(Sequence(1,!!re"[^][\|#]*",":"); default=namespace),
        !!Optional(re"[^][\|#:]*"),
        !!Optional(re"#[^][\|#]+"),
        Sequence(1,
                 Optional(Sequence(2,"|"=>"", !!re"[^][]+")),
                 "]]"),
        !!re"[[:alpha:]]*"
    ) do v
        WikiLink(v[2],
                 v[3:end]...)
    end

wiki_parentheses = Dict{Any,Any}(
    pairs(CombinedParserTools.Tokens.parentheses)...,
    :italics=>("''","''"),
    :bold=>("'''","'''"),
    :bolditalics=>("''''","''''")
)

import CombinedParserTools: regex_tempered_greedy
import CombinedParserTools.Tokens: quotes, attribute_parser

function table_cell_parser(inner_word::TextParse.AbstractToken{<:AbstractToken}, node="td")
    table_delim = Either("!", "|")
    inner = lines_stop(inner_word; until=table_delim)
    Sequence(
        Optional(Sequence(
            1,
            Repeat( Sequence(
                2,
                NegativeLookahead(Either(newline,"|")),
                Either(attribute_parser, inner_word);
            )),
            ## ignore dummy format, e.g. ''formatting''
            Sequence("|", NegativeLookahead("|")))),
        (
            ## ParserPeek("Content may either follow its cell mark on the same line (after any optional HTML attributes); ",10,
            Repeat_stop(inner_word,
                        Either("\n","||","!!")))
        ## ; transform=(v,i) -> Node{A,T}(node, v[1], [Line(v[2])]))
        ## https://en.wikipedia.org/wiki/Help:Table
        , Optional( (
            ## ParserPeek("or on lines below the cell mark. Content that uses wiki markup that itself needs to start on a new line, such as with lists, headings, or nested tables, must be on its own new line.",10,
            Sequence(3, newline, NegativeLookahead(table_delim), inner)))
        , Repeat(newline)) do v
            Node( node = node, attrs = v[1], children = [ v[2], v[3]... ] )
        end
end

function table_cell_parsers(inner_word)
    Sequence(
        Either(
            Sequence(2,"!",table_cell_parser(inner_word, "th")),
            Sequence(2,re"\|(?![-}])",table_cell_parser(inner_word, "td"))),
        Repeat(
            Either(
                Sequence(2,"!!",table_cell_parser(inner_word, "th")),
                Sequence(2,"||",table_cell_parser(inner_word, "td"))))) do v
                    pushfirst!(v[2],v[1])
                end
end

with_key(key,p) =
    map(Pair{Symbol,result_type(p)},with_name(key,p)) do v
        Pair{Symbol,result_type(p)}(key, v)
    end

sloppyhtml(inner_token; stop=tuple()) =
    Either(
        instance(Token, !!re"<br */?>"i, :delimiter),
        Sequence(
            re"<nowiki>"i,
            Repeat_until(
                Either(simple_tokens...,newlinetoken),
                re"</nowiki>"i
            )) do v
        TokenPair(:nowiki, v[2])
        end,
        Sequence(
            re"<pre>"i,
            Repeat_until(
                Either(simple_tokens...,newlinetoken),
                re"</pre>"i
            )) do v
        TokenPair(:pre, v[2])
        end,
        html(Line{NamedString,AbstractToken},
             anyhtmltag) do until
        Sequence(
            1,
            lines_stop(inner_token; until= Either(stop...,until)),
            Optional(until))
        end)

tok(outer,inner) =
    map(!outer) do v
        parse(inner,v)
    end

function table_parser(inner_word)
    ## N = Node{Node{eltype(result_type(inner))}}
    N = AbstractToken
    inner_partial_html = Either(
        sloppyhtml(inner_word, stop=("|","!")),
        inner_word)
    attr_line = Repeat_until(Either(attribute_parser, inner_word),newline)
    Sequence(
        "{|", attr_line
        # 4
        , Optional(Sequence(
            2, "|+",
            tok(inline,Repeat(inner_word)),
            newline))
        # 5 
        , Repeat(table_cell_parsers(inner_partial_html))
        , Repeat(
            Sequence(N,
                     re"\|-+", attr_line,
                     Repeat(table_cell_parsers(inner_partial_html))) do v
            Node{AbstractToken,AbstractToken}("tre", v[2], vcat(v[3]...))
            end),
        Repeat(whitespace_newline),
        "|}") do v
            if !isempty(v[4])
                pushfirst!(v[5],
                           Node("tr", AbstractToken[], vcat(v[4]...)))
            end
            if !isempty(v[3])
                pushfirst!(v[5],
                           Node("caption", AbstractToken[], vcat(v[3]...)))
            end
            Node{AbstractToken,N}("table",v[2], v[5])
        end
end

export TemplateParameter
TemplateParameter = TokenPair{Vector{LineContent}, Vector{Line{NamedString,LineContent}}}

## from https://phabricator.wikimedia.org/source/mediawiki/browse/REL1_29/includes/Sanitizer.php
valid_html_tags = (
    htmlpairsStatic = [ # Tags that must be closed
                        "b", "bdi", "del", "i", "ins", "u", "font", "big", "small", "sub", "sup", "h1",
                        "h2", "h3", "h4", "h5", "h6", "cite", "code", "em", "s",
                        "strike", "strong", "tt", "var", "div", "center",
                        "blockquote", "ol", "ul", "dl", "table", "caption", "pre",
                        "ruby", "rb", "rp", "rt", "rtc", "p", "span", "abbr", "dfn",
                        "kbd", "samp", "data", "time", "mark"
                        ],
    htmlsingle = [
        "br", "wbr", "hr", "li", "dt", "dd", "meta", "link"
    ],
    htmlsingleonly = [ # Elements that cannot have close tags
                       "br", "wbr", "hr", "meta", "link"
                       ],
    htmlnest = [ # Tags that can be nested--??
                 "table", "tr", "td", "th", "div", "blockquote", "ol", "ul",
                 "li", "dl", "dt", "dd", "font", "big", "small", "sub", "sup", "span",
                 "var", "kbd", "samp", "em", "strong", "q", "ruby", "bdo"
                 ],
    tabletags = [ # Can only appear inside table, we will close them
                  "td", "th", "tr",
                  ],
    htmllist = [ # Tags used by list
                 "ul", "ol",
                 ],
    listtags = [ # Tags that can appear in a list
                 "li",
                 ],
    ref = [ "ref", "references" ],
    transclusiontags = [ "noinclude", "onlyinclude", "includeonly" ]
)
anyhtmltag=Regcomb("(?:"*join(sort(unique(vcat(valid_html_tags...)); by=lastindex,rev=true),"|")*")","i")


# Line{NamedString,LineContent},
wiki_list(wikitext;until) = Sequence(
    # :indent =>
    Either(list_item),
    # :tokens =>
    Repeat_stop(wikitext, Either(newline,until))
) do v
    Line(v[1], v[2])
end

wiki_content(wikitext;until) =
    with_name(
        :content,
        Sequence(
            map(re":*") do v
            v=="" ? NamedString[] : NamedString[NamedString(:whitespace," "^length(v))]
            end,
            Repeat_stop(wikitext,Either(newline,until))) do v
        Line(v[1], v[2])
        end
    )
wiki_lines(wikitext;kw...) = [ wiki_list(wikitext;kw...), wiki_content(wikitext;kw...) ];

function parenthesisTempered(name::Symbol, inner, open, close=open; flags="")
    with_name(name,
              map(regex_tempered_greedy(open,close,flags)) do v
              TokenPair(name, parse(inner, v))
              end
              )
end

function parenthesisTempered(name::Symbol,inner;flags="")
    parenthesisTempered(name, inner, wiki_parentheses[name]...; flags=flags)
end

function parenthesisP(name::Symbol, wikitext, open::String, close=open)
    with_name(name,
              Sequence(# TokenPair{Symbol, Vector{LineContent}},
                       open, Repeat_until(wikitext, close)) do v
              TokenPair(name,v[2])
              end)
end

function parenthesisP(name::Symbol, wikitext)
    parenthesisP(name, wikitext, wiki_parentheses[name]...)
end

lines_stop(wikitext;until) =
    join(
        Either(
            ( w for w in wiki_lines(wikitext,until=until))...
        ),
        newline
    );


function wiki_expression(wikitext, key_parser=re"[-[:alnum:]. _,*]*")
    with_name(
        :expression,
        Sequence(
            "{{#",
            word,":", whitespace_maybe,
            join(
                Sequence(
                    v -> v[1] => v[2],
                    Optional(Sequence(1,!!key_parser,"="); default=""),
                    lines_stop(wikitext; until=Either("|","}")),
                    Optional(newline))
                ## todo: in parser have default option to intern string during building instance
                ,
                "|"),
            "}}") do v
        Template(("#"*v[2]),v[5])
        end)
end

function wiki_template(wikitext, x=re"[^}{\|]+", key_parser=re"[-[:alnum:]. _,*]*")
    stop_at=Either("|","}")
    with_name(
        :template,
        Sequence(
            "{{",
            ( x === nothing ? !!Repeat_stop(wikitext, stop_at) :
              x ),
            Repeat_until(
                with_name(
                    :template_arg,
                    Sequence(
                        v -> v[3] => v[4],
                        Optional(newline),
                        "|",
                        Optional(Sequence(2,re"[ \t\n\r]*",
                                          !!key_parser,"="); default=""),
                        lines_stop(wikitext; until=stop_at),
                        Optional(newline))),
                ## todo: in parser have default option to intern string during building instance
                "}}")) do v
        Template(v[2],v[3])
        end
    )
end

function template_parameter(wikitext)
    with_name(
        :template_parameter,
        Sequence(
            "{{{",
            Repeat_stop(wikitext,Either("|","}")),
            Optional(Sequence(2,"|", lines_stop(wikitext,until=Either("|","}")))),
            "}}}") do v
        TokenPair(v[2],v[3])
        end)
end


heading(n,wikitext) =
    with_name(
        "heading $n",
        Sequence(
            Repeat(n,'='),
            whitespace_horizontal,
            Repeat_until(
                wikitext,
                Sequence(
                    whitespace_horizontal,
                    Repeat(n,'='),
                    NegativeLookahead("="))),
            Repeat_stop(wikitext, newline)
        ) do v
        Line{NamedString, LineContent}(
            [ NamedString(:headline,intern(string(n))) ],
            isempty(v[4]) ? v[3] : push!(v[3], Node(:suffix, Token[],v[4])))
        end
    )

Token(value,name::Symbol) = name => value
newlinetoken = instance(Token,!!newline,:delimiter)

wikitokens = Either(
    simple_tokens...,
    instance(Token, !!re".", :unknown))

linkparser = map(
    Sequence(!!re"https?|ftp",
             "://",
             !!re"[-[:alpha:][:digit:]?=&#+\._%:]+",
             !!Optional(re"/[-:,;~\$\p{L}[:digit:]?=&#+\./_%()*!|]*"))) do v
                 query = v[4] === nothing ? Token[] : tokenize(Repeat(wikitokens), v[4])
                 TokenPair(:link, Token[ Token(:protocol, v[1]),
                                         delim"://",
                                         Token(:domain, v[3]),
                                         query... ])
             end

wiki_external_link = Sequence(2,'[',linkparser,']')


htmlescape = instance(Token, !!re"&(?:[[:alpha:]]+|#[[:digit:]]+);"i, :htmlescape)
export wikitoken, wikitext, valid_html_tags
function wikitoken(;namespace = "wikt:de")
    wikitext=Either{LineContent}(
        bracket_number, ## todo: make a line type? see ordo [5]a-c
        bracket_reference,
        map(
            join(Repeat(2,Repeat_max,word), "·")) do v
        TokenPair(:hyphenation,
                  Token[Token(:syllable,x)
                        for x in v ])
        end)
    
    
    for p in [
        ## instance(Token, parser(Regex(" "*regex_string(enum_label)*" ")), :number),
        htmlescape
    ]
        push!(wikitext, p)
    end

    inner_newline = instance(Token, !!newline, :whitespace)


    
    push!(wikitext, linkparser)
    push!(wikitext, sloppyhtml(wikitext))

    push!(wikitext, wiki_link(wikitext;namespace = namespace));
    push!(wikitext,
          instance(Token, !!parser("[…]"), :ellipsis))
    
    ## push!(wikitext,wiki_external_link);

    push!(wikitext,template_parameter(wikitext));
    push!(wikitext,wiki_expression(wikitext));
    push!(wikitext,wiki_template(wikitext,nothing));

    push!(wikitext,table_parser(wikitext));

    push!(wikitext, parenthesisP(:htmlcomment, Either(wikitext,newlinetoken)))

    push!(wikitext, parenthesisP(:paren, wikitext)) ## used for filtering from wiki word in meaning 
    push!(wikitext, parenthesisP(:bracket, wikitext))
    push!(wikitext, parenthesisTempered(:bolditalics, Repeat(wikitext)))
    push!(wikitext, parenthesisTempered(:bold, Repeat(wikitext)))
    push!(wikitext, parenthesisTempered(:italics, Repeat(wikitext)))
    push!(wikitext, parenthesisP(:german_quote, wikitext))
    
    push!(wikitext, wikitokens)
    wikitext
end
function wikitext(;namespace = "wikt:de")
    function append_textblock_token(r, v, nl)
        if v !== missing
            if nl !== missing
                push!(v.tokens, Token(:delimiter, intern(nl)))
            end
        end
        push!(r,v)
    end
    wikitext=wikitoken(;namespace=namespace)
    textblock = alternate(
        vcat([ heading(level, wikitext) for level in reverse(1:6) ], wiki_lines(wikitext;until=Never())),
        emptyline; 
        appendf=append_textblock_token)

    wikitextParser = textblock
end

# inner_template_names = [
#     "erweitern",
#     "Wortart", re"Pl.[0-9]?", "Gen.", "n",
#     "Prät.", "Part.",
#     "Sprache","de", "en", "cs",
#     re"[a-z]", ## todo: only valid word type abbrevs!
#     re"[a-z][a-z]", ## todo: only valid language codes!
#     re"Übersetzungen[ [:alnum:]]*", "Ü",
#     "Beispiele fehlen",
#     re"Ref-[[:alpha:]]+",
#     "IPA",
#     "Hörbeispiele", "Audio",
#     "Reime", "Reim",
#     "Komp.", "Sup.", ## Komparativ, Superlativ
#     "Wikipedia",
#     "Beispiele fehlen"
# ]
## too feeble: push!(wiki_template.els[2].els.els,wiki_template)
## inner_templates = Either(inner_template_names...) ## todo: only valid language codes!)
using TranscodingStreams
using CodecBzip2
using LibExpat

export parse_bz2
function parse_bz2(f::Function,
                   of = "/home/gregor/dev/julia/FilingForest/dewiktionary-latest-pages-articles.xml.bz2",
                   wait_onwarn = false)
    open(of) do io
        unpack = Bzip2DecompressorStream(io)
        cbs = XPCallbacks()
        local start_element_name
        ##local results = Any[]
        let count = 0,
            r=Pair{String,Vector{Pair{String,Any}}}[],
            text = [],
            ignore = String[],
            value = String[ "mediawiki",
                            "base", "case", "title", "generator", "dbname", "minor",
                            "id", "sitename", "username", "comment", "parentid", "timestamp", "ns",
                            "sha1", "model", "format", "text", "restrictions", "ip" ],
            types = String[ "redirect", "siteinfo", "contributor", "revision", "namespaces", "namespace" ],
            record = String[ "page" ],
            ignored = false
            ##
            cbs.start_element = function (h, name, attrs)
                ## lname, lattrs = name, attrs todo, push only when required in end_element
                push!(r, intern(name) => [ intern(key) => intern(attrs[key]) for key in keys(attrs) ] )
                ignored = name in ignore
                ## name in record && @info "$name...."
            end
            cbs.character_data = function(h, txt)
                if !ignored
                    push!(text,txt)
                    ## text = 
                end
            end            
            function trim(s::AbstractString, chars=['\n','\r',' '])
                start=firstindex(s)
                last=lastindex(s)
                while start <= last && s[start] in chars 
                    start = nextind(s, start)
                end
                while last > start && s[last] in chars
                    last = prevind(s, last)
                end
                start>last ? "" : s[start:last]
            end

            cbs.end_element = function (h, name)
                # while isempty(r[end])
                #     pop!(r)
                # end
                ## @show ignore
                ##name in record && print(text)
                ## text = replace(text, re"[ \t\n]*|[ \t\n]*$" => "")
                name = intern(name)
                if !( name in value || name in record || name in ignore || name in types )
                    println("\n\n--\nWhat to do with $name? (r)record, (i)gnore, (v)alue\n$text\n$(r[end])")
                    strat = readline(stdin)
                    ##@show strat
                    if strat == "r"
                        push!(record, name)
                    elseif strat == "i"
                        push!(ignore, name)
                    elseif strat == "v"
                        push!(value, name)
                    end
                end
                if name in value || isempty(r[end])
                    ## @show name r[end]
                    pop!(r)
                    text =  trim(join(filter(x-> x!="",text))) #  * txt, re"[ \t\n\r]*|[ \t\n\r]*$" => "")
                    (!isempty(r) && text !="") && push!(r[end].second, name => text)
                    text = []
                elseif name in types
                    val = pop!(r)
                    push!(r[end].second, name => (; ( Symbol(k.first) => k.second for k in val.second)...))
                elseif name in record
                    val = pop!(r)
                    val = (; ( Symbol(k.first) => k.second for k in val.second)...)
                    if !(:title in propertynames(val) &&
                         match(re"Wiktionary:|MediaWiki:",val.title)!==nothing)
                        ## show(results[end])
                        if :revision in propertynames(val) && :text in propertynames(val.revision) && !startswith(val.revision.text, "{{NOINDEX}}")
                            count = count + 1
                            f(val, count)
                        end
                    end
                elseif name in ignore
                    ig = pop!(r)
                    ## @info "ignore $(ig.first)" ig.second
                else
                    error()
                end
            end
        end
        parsestream(unpack, cbs)
    end
end


ignore = (v, i) -> missing
ignore_heading = (v, i) -> begin
    @show typeof(v)
    length(@show v) == 1 ? [] : v[2:end]
end


image_argument_parser = map_at(
    (v,i) -> [ NamedString("image", v[2]), NamedString("property", v[1]) ],
    re"(Bild[^[:digit:]]*)([[:digit:]]*)$")


function prepend_prefix!(v::Vector{<:Line},y)    
    for x in v
        prepend!(x.prefix.prefix, y)
        ## dump(x.tokens)
        filter!(t -> !in(t,[ Token(:whitespace, "\n"),
                             Token(:literal, "—") ]), x.tokens)
        ## print(x)
        ## sleep(1)        
    end
    filter!(t -> !isempty(t.tokens), v)
end

word_string(w) =
    trimstring(
        join(string.(filter(t-> !(t isa TokenPair && t.key==:paren), w[1].tokens))))

function parse_overview(namespace, title, w, t::Nothing)
    ## language = v.word[1].tokens[3],
    images = Line{NamedString,LineContent}[]
    inflections = Pair{String,Token}[]
    language, wordtype, genus = string(value(filter(t-> t isa TokenPair && variable(t)==:paren,w[1].tokens)[end])[1].arguments[1].second), "",""
    ( word = Token(:literal, intern(word_string(w))),
      title = Token(:literal, intern(title)), 
      language = Token(:language,language),
      wordtype = Token(:wordtype,wordtype),
      inflections = inflections,
      images = images,
      wikitext = [ w[1], w[2]... ] )
end

function parse_overview(namespace, title, w, t::Template)
    ## language = v.word[1].tokens[3],
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
    language, wordtype, genus = tokenize(overview_parser, t.template)
    args = Pair{String,AbstractToken}[]
    for (i,a) in enumerate(t.arguments)
        # if !(length(a.second) == 2 && length(a.second[1].tokens)==2)
        #     dump(a)
        #     error(typeof(a))
        # end
        if !isempty(a.second) && !isempty(a.second[1].tokens)
            val = a.second[1].tokens[1]
            imp = tokenize(image_argument_parser, a.first; partial=:nothing)
            if imp !== nothing
                append!(images, prepend_prefix!(a.second, imp))
            elseif a.first==""
                ## push!(args, "$i" => val)
            elseif a.first in ["Genus"]
                push!(args, a.first => val)
            else
                push!(inflections, a.first => Token(:literal,string(val)))
                # imp = tokenize(
                #     re"([^[:digit:]]+|[[:digit:]]\. Person) *([[:digit:]]*)",
                #     a.first)
                # if imp !== nothing
                #     push!(inflections, Token(imp[1],string(val)))
                # else # if a.first in ["Genus"]
                #     @warn "no key" t a
                #     error(a.first)
                #     push!(args, a)
                # end
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



function parse_overview(namespace, title, w, v::Vector{<:Line})
    parse_overview(namespace, title, w, isempty(v) ? nothing : v[1].tokens[1])
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
    #:similar => is_template_line(re"Ähnlichkeiten[ 0-9]*"),
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
    IteratorParser{String}("index", x->x isa Token && variable(x)==:number, (t,i) -> value(t)),
    Repeat(IteratorParser{Token}("whitespace", x->x isa Token && !isinformative(x), (t,i) -> t)),
    Repeat(IteratorParser{LineContent}("rest", x->x isa LineContent, (t,i) -> t))) do v
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
            ## typeof(data)
            push!(data, Line(NamedString[], val))
        end
        for (k,v) in wt[2]
            x = tokenize(
                Repeat(is_line(Pair{String,Vector{LineContent}},
                               (l, i) -> let (r,i_) = tryparsenext(number_line, l.tokens)
                               isnull(r) ? "?" => l.tokens : get(r)
                               end
                               )),
                v)
            lastnum = "?"
            for e in x
                num = e.first=="?" ? lastnum : e.first
                let is = tokenize(expand_numbers,InternedStrings.intern(num); partial=nothing)
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
        ## @info "?" keys(meaning_data) first(values(meaning_data)) ## (;a=1,get(meaning_data,"?",[])...) get(meaning_data,"?",[])
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
        
        ( merge(## word_type=Token(:word_type,join(string.(filter(isinformative,wt[1].tokens)))),
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




end # module
