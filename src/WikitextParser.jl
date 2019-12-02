module WikitextParser
using Nullables
using InternedStrings

using ParserAlchemy
using ParserAlchemy.Tokens
import ParserAlchemy: inline, newline, whitenewline, whitespace, rep_delim_par, word, footnote, delimiter, indentation, wdelim
import ParserAlchemy.Tokens: tokenstring, bracket_number, bracket_reference, default_tokens
import ParserAlchemy.Tokens: html, attributes, simple_tokens
import ParserAlchemy.Tokens: NamedString, Token
import ParserAlchemy.Tokens: Node, Template, TokenPair, Line, LineContent, Paragraph
import ParserAlchemy.Tokens: IteratorParser, is_template_line, is_line, is_heading


import TextParse
import TextParse: Numeric

import BasePiracy
import BasePiracy: construct

int_range = seq(
    Vector{Int},
    # 1           # 2       # 3
    Numeric(Int), r" *(?:–|-) *", Numeric(Int); ## '–' != '-'
    # use julia `:` syntax to collect UnitRange 
    transform=(v,i)-> collect(v[1]:v[3]))

expand_numbers = seq(
    Vector{String},"[",
    alternate(
        alt(instance(Vector{String},(v,i)->["$x" for x in v], int_range),
            instance(Vector{String},(v,i)->[v], r"[0-9]+[[:alpha:]]*")),
        ## regex: allow whitespace
        r"^ *[,/] *";
        
    ), "]";
    transform=(v,i) -> vcat(v[2]...))


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



list_item = instance(
    Vector{NamedString},
    (v,i) -> [NamedString(:list,intern(v[1]))],
    r"^([*]+|[#]+) *")

# URL_re = raw"(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'\".,<>?«»“”‘’]))"

wiki_external_link =
    instance(Token,
             (v,i) -> Token(:link,intern(v[1])),
             r"^\[((?:https?|ftp)[^][]+)\]"
             );


export substnothing
substnothing(default, x) =
    x === nothing ? default : x

export WikiLink
struct WikiLink <: AbstractToken
    namespace_page::Token
    anchor::String
    label::String
    function WikiLink(namespace::AbstractString, page::AbstractString, anchor::AbstractString, label::AbstractString, addlabel="")
        new(Token(intern(namespace), intern(page)), intern(anchor),
            intern((label=="" ? page : label) * addlabel))
    end
    function WikiLink(namespace_page::Token, anchor::String, label::String)
        new(namespace_page, anchor, label)
    end
end
function BasePiracy.construct(::Type{WikiLink}; namespace_page, anchor, label)
    WikiLink(namespace_page, anchor, label)
end


Base.show(io::IO, ::MIME"text/x-wiki", x::WikiLink) =
    print(io, "[[", name(x.namespace_page) == "" ? "" : value(x.namespace) * ":",
          x.page,
          x.anchor == "" ? "" : "#" * x.anchor,
          x.label == "" ? "" : "|" * x.label,
          "]]")

Base.show(io::IO, x::WikiLink) =
          printstyled(io, x.label=="" ? value(x.namespace_page) : x.label; bold=true,
                      color=36)

export @substringorempty
macro substringorempty(x,range)
    :( $(esc(x)) === nothing ? "" : $(esc(x))[$(esc(range))] )
end

wiki_link(wikitext;namespace = "wikt:de") =
    instance(
        WikiLink,
        (v,i) -> WikiLink(substnothing(namespace,v[1]),
                          (substnothing.("",v[2:end]))...),
        r"^\[\[(?:([^][\|#]*):)?([^][\|#:]*)(?:#([^][\|#]+))?(?:\|([^][]+))?\]\]([[:alpha:]]+)?"
    )


wiki_parentheses = Dict{Any,Any}(
    pairs(ParserAlchemy.Tokens.parentheses)...,
    :italics=>("''","''"),
    :bold=>("'''","'''"),
    :bolditalics=>("''''","''''")
)

import ParserAlchemy: regex_tempered_greedy
import ParserAlchemy.Tokens: enum_label, quotes, attribute_parser

function table_cell_parser(inner_word::TextParse.AbstractToken{<:AbstractToken}, node="td")
    A = result_type(inner_word)
    T = Line{NamedString,A}
    table_delim = alt("!", "|")
    inner = lines_stop(inner_word; until=table_delim)
    seq(Node{A,T},
        opt(seq( rep( seq(NegativeLookahead(alt(newline,"|")),
                      alt(attribute_parser, inner_word);
                      transform=2)),
             ## ignore dummy format, e.g. ''formatting''
             seq("|", NegativeLookahead("|"); transform=1);
                 transform=1)),
        (
            ## ParserPeek("Content may either follow its cell mark on the same line (after any optional HTML attributes); ",10,
            rep_stop(inner_word,
                     alt("||","!!")))
        ## ; transform=(v,i) -> Node{A,T}(node, v[1], [Line(v[2])]))
        ## https://en.wikipedia.org/wiki/Help:Table
        , opt( (
            ## ParserPeek("or on lines below the cell mark. Content that uses wiki markup that itself needs to start on a new line, such as with lists, headings, or nested tables, must be on its own new line.",10,
                       seq(newline, NegativeLookahead(table_delim), inner; transform=3)))
        , rep(newline)
        ; transform=(v,i) -> Node{A,T}(node, v[1], T[ Line(v[2]), v[3]... ]))
end

function table_cell_parsers(inner_word)
    A = result_type(inner_word)
    T = Line{NamedString,A}
    seq(Vector{Node{A,T}},
        alt(seq("!",table_cell_parser(inner_word, "th"), transform=2),
            seq(r"\|(?![-}])",table_cell_parser(inner_word, "td"), transform=2)),
        rep(
            alt(seq("!!",table_cell_parser(inner_word, "th"), transform=2),
                seq("||",table_cell_parser(inner_word, "td"), transform=2))),
    transform=(v,i)->pushfirst!(v[2],v[1]))
end



function table_parser(inner_word)
    ## N = Node{Node{eltype(result_type(inner))}}
    N = AbstractToken
    seq(Node{AbstractToken,N},
        "{|", rep(alt(attribute_parser, inner_word)),newline,
        # 4
        opt(seq("|+", tok(inline,rep(inner_word)), newline; transform=2, log=false)),
        # 5 
        rep(table_cell_parsers(inner_word)),
        rep(seq(N,
                r"\|-+", tok(inline,attributes), newline,
                rep(table_cell_parsers(inner_word));
                transform=(v,i) -> Node{AbstractToken,AbstractToken}("tr", v[2], vcat(v[4]...)));
            log=false),
        ##r".*"s,
        rep(whitenewline),
        "|}",
        ; ## partial=true, log=true,
        transform=(v,i) -> begin
        if !isempty(v[5])
        pushfirst!(v[6],
              Node("tr", AbstractToken[], vcat(v[5]...)))
        end
        if !isempty(v[4])
        pushfirst!(v[6],
              Node("caption", AbstractToken[], vcat(v[4]...)))
        end
        Node{AbstractToken,N}("table",v[2], v[6])
        end)
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



wiki_list(wikitext;until) = seq(
    Line{NamedString,LineContent},
    # :indent =>
    alt(list_item),
    # :tokens =>
    rep_stop(wikitext, alt(newline,until));
    transform = (v,i) -> Line(v[1], v[2]))

wiki_content(wikitext;until) = seq(
    Line{NamedString,LineContent},
    # :indent =>
    instance(Vector{NamedString},
             (v,i)-> v=="" ? NamedString[] : NamedString[NamedString(:whitespace," "^length(v))],
             r"^:*"),
    # :tokens =>
    rep_stop(wikitext,alt(newline,until));
    transform = (v,i) -> Line(v[1], v[2])
);
wiki_lines(wikitext;kw...) = [ wiki_list(wikitext;kw...), wiki_content(wikitext;kw...) ];

function parenthesisTempered(name::Symbol, inner, open, close=open; flags="")
    instance(TokenPair{Symbol, result_type(inner)},
             (v,i) -> TokenPair(name, tokenize(inner, v[1])),
             regex_tempered_greedy(open,close,flags))
end

function parenthesisTempered(name::Symbol,inner;flags="")
    parenthesisTempered(name, inner, wiki_parentheses[name]...; flags=flags)
end

function parenthesisP(name::Symbol, wikitext, open::String, close=open)
    seq(TokenPair{Symbol, Vector{LineContent}},
        open, rep_until(wikitext, close);
        transform=(v,i) -> TokenPair(name,v[2]))
end
function parenthesisP(name::Symbol, wikitext)
    parenthesisP(name, wikitext, wiki_parentheses[name]...)
end

lines_stop(wikitext;until) =
    alternate(
        alt(wiki_lines(wikitext,until=until)...), newline;
        appendf=(r,l,nl) ->
        if l!==missing
        if nl !== missing
        push!(l.tokens,
              Token(:whitespace, intern(nl)))
        end
        push!(r,l)
        end
    );


function wiki_expression(wikitext, key_parser=r"^[-[:alnum:]. _,*]*")
    seq(
        Template{NamedString,LineContent},
        "{{#",
        word,":", opt(whitespace),
        alternate(
            seq(Pair{String, Paragraph{NamedString,LineContent}},
                opt(key_parser,"="; default="", transform_seq=1),
                lines_stop(wikitext; until=alt("|","}}")),
                opt(newline);
                ## todo: in parser have default option to intern string during building instance
                transform = (v,i) -> intern(v[1]) => v[2]),
            "|"),
        "}}"; 
        transform=(v,i) -> Template(("#"*v[2]),v[5]))
end

function wiki_template(wikitext, x=r"[^}{\|]+", key_parser=r"^[-[:alnum:]. _,*]*")
    seq(
        Template{NamedString,LineContent},
        "{{",
        ( x === nothing ? instance(String, (v,i) -> join(string.(v)), rep_stop(wikitext, alt("|","}}"))) :
          x ),
        rep_until(
            seq(Pair{String, Paragraph{NamedString,LineContent}},
                opt(newline),
                "|",
                opt(r"^[ \t\n\r]*",key_parser,"="; default="", transform_seq=2),
                lines_stop(wikitext; until=alt("|","}}")),
                opt(newline);
                ## todo: in parser have default option to intern string during building instance
                transform = (v,i) -> intern(v[3]) => v[4]),
            "}}"); 
        transform=(v,i) -> Template((v[2]),v[3]))
end

function template_parameter(wikitext)
    seq(
        TemplateParameter,
        "{{{",
        rep_stop(wikitext,alt("|","}}}")),
        opt(seq("|", lines_stop(wikitext,until="}}}");
                transform=2)),
        "}}}"; 
        transform=(v,i) -> TokenPair(v[2],v[3]))
end


heading(n,wikitext) = seq(
    Line{NamedString, LineContent},
    Regex("^={$n,$n} *"),
    tok(r"[^\n]*",rep(wikitext)),
    Regex("^ *={$n,$n} *");
    combine=true, 
    transform = (v,i) -> Line{NamedString, LineContent}([ NamedString(:headline,intern(string(n))) ] , v[2]))

export wikitoken, wikitext, valid_html_tags
function wikitoken(;namespace = "wikt:de")
    anyhtmltag=Regex("^(?:"*join(sort(unique(vcat(valid_html_tags...)); by=lastindex,rev=true),"|")*")","i")
    wikitext=alt(
        LineContent,
        bracket_number, ## todo: make a line type? see ordo [5]a-c
        bracket_reference,
        seq(TokenPair{Symbol,Vector{Token}},
            r"<nowiki>"i, regex_neg_lookahead(r"</nowiki>"i,r"(?:.|[\n])"), r"</nowiki>"i;
            transform=(v,i) -> TokenPair(:nowiki, tokenize(rep(alt(simple_tokens...)), v[2]))
            ),
        seq(TokenPair{Symbol,Vector{Token}},
            r"<pre>"i, regex_neg_lookahead(r"</pre>"i,r"(?:.|[\n])"), r"</pre>"i;
            transform=(v,i) -> TokenPair(:pre, tokenize(rep(alt(simple_tokens...)), v[2]))
            )
    )
    wikilink = wiki_link(wikitext;namespace = namespace)    

    for p in [
        ## instance(Token, parser(Regex(" "*regex_string(enum_label)*" ")), :number),
        instance(Token, r"^<br */?>"i, :delimiter)
        instance(Token, r"^&[[:alpha:]]+;"i, :htmlescape)
    ]
        push!(wikitext, p)
    end

    inner_newline = instance(Token, (v,i) -> Token(:whitespace, intern(v)), parser(newline))
    
    push!(wikitext, instance(Token, r"^(?:https?|ftp)://[-[:alpha:][:digit:]?=&#+\./_%]*", :link))
    push!(wikitext, html(Line{NamedString,AbstractToken},
                         anyhtmltag) do until
          seq(lines_stop(wikitext; until= until), until; transform=1)
          end)

    push!(wikitext,wikilink);

    push!(wikitext,instance(Token, (v,i) -> Token(:ellipsis,v),
                                Regex("^"*regex_string("[…]"))))
    push!(wikitext,wiki_external_link);

    push!(wikitext,template_parameter(wikitext));
    push!(wikitext,wiki_expression(wikitext));
    push!(wikitext,wiki_template(wikitext,nothing));

    push!(wikitext,table_parser(lines_stop(wikitext; until=alt("|","}}"))));

    push!(wikitext, parenthesisP(:htmlcomment, alt(wikitext,instance(Token,newline,:delimiter))))

    push!(wikitext, parenthesisP(:paren, wikitext)) ## used for filtering from wiki word in meaning 
##    push!(wikitext, parenthesisP(:bracket))
##    push!(wikitext, parenthesisP(:curly))
##    push!(wikitext, parenthesisP(:angle))
##    push!(wikitext, parenthesisP(:quote))
    push!(wikitext, parenthesisTempered(:bolditalics, rep(wikitext)))
    push!(wikitext, parenthesisTempered(:bold, rep(wikitext)))
    push!(wikitext, parenthesisTempered(:italics, rep(wikitext)))
##    push!(wikitext, parenthesisP(:squote))
##    push!(wikitext, parenthesisP(:german_quote))
##    push!(wikitext, parenthesisP("'''"))

    for p in simple_tokens
        push!(wikitext, p)
    end
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
    textblock = alternate(
        vcat([ heading(level, wikitext) for level in reverse(1:6) ], wiki_lines(wikitext;until=Never())),
        emptyline; 
        appendf=append_textblock_token)

    wikitextParser = textblock
end

# inner_template_names = [
#     "erweitern",
#     "Wortart", r"Pl.[0-9]?", "Gen.", "n",
#     "Prät.", "Part.",
#     "Sprache","de", "en", "cs",
#     r"[a-z]", ## todo: only valid word type abbrevs!
#     r"[a-z][a-z]", ## todo: only valid language codes!
#     r"Übersetzungen[ [:alnum:]]*", "Ü",
#     "Beispiele fehlen",
#     r"Ref-[[:alpha:]]+",
#     "IPA",
#     "Hörbeispiele", "Audio",
#     "Reime", "Reim",
#     "Komp.", "Sup.", ## Komparativ, Superlativ
#     "Wikipedia",
#     "Beispiele fehlen"
# ]
## too feeble: push!(wiki_template.els[2].els.els,wiki_template)
## inner_templates = alt(inner_template_names...) ## todo: only valid language codes!)
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
                ## text = replace(text, r"^[ \t\n]*|[ \t\n]*$" => "")
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
                    text =  trim(join(filter(x-> x!="",text))) #  * txt, r"^[ \t\n\r]*|[ \t\n\r]*$" => "")
                    (!isempty(r) && text !="") && push!(r[end].second, name => text)
                    text = []
                elseif name in types
                    val = pop!(r)
                    push!(r[end].second, name => (; val.second...))
                elseif name in record
                    val = (; pop!(r).second...)
                    if !(:title in propertynames(val) &&
                         match(r"Wiktionary:|MediaWiki:",val.title)!==nothing)
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


image_argument_parser = instance(
    Vector{NamedString},
    (v,i) -> [ NamedString("image", v[2]), NamedString("property", v[1]) ],
    r"^(Bild[^[:digit:]]*)([[:digit:]]*)$")


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

function parse_overview(namespace, w, t::Template)
    ## language = v.word[1].tokens[3],
    images = Line{NamedString,LineContent}[]
    inflections = Token[]
    overview_parser = instance(Vector{String},
                               (v,i) -> String[intern(v[1]),intern(v[2]),v[3]===nothing ? "" : intern(v[3])],
                               r"([[:alpha:]]+) ([[:alpha:]]*) ?Übersicht ?(m|f)?(?:\r?\n)*";
                               )
    language, wordtype, genus = tokenize(overview_parser, t.template)
    args = Pair{String,Token}[]
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
                push!(inflections, Token(a.first,string(val)))
                # imp = tokenize(
                #     r"^([^[:digit:]]+|[[:digit:]]\. Person) *([[:digit:]]*)",
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
    ( word = Token(namespace, intern(trimstring(
        join(string.(filter(t-> t isa Token && variable(t)!=:paren,w[1].tokens)))))),
      language = Token(:language,language),
      wordtype = Token(:wordtype,wordtype),
      inflections = inflections,
      images = images,
      wikitext = [ Line(LineContent[t]) ] )
end


      
function parse_overview(namespace, w,v::Vector{<:Line})
    parse_overview(namespace, w,isempty(v) ? Template("NoLanguage NoWordType Übersicht") : v[1].tokens[1])
end

wiktionary_de_content=[
                is_template_line("Übersetzungen", ignore, Missing) => (:translations, is_line()),
                is_template_line(t -> match(r"Übersicht", t.template)!==nothing) => (:overview, missing),
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
                #:similar => is_template_line(r"^Ähnlichkeiten[ 0-9]*"),
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
    rep(seq(
        NamedTuple,
        :prefix => rep(is_line()),
        :word => seq(is_heading(x -> isequal(x.value,"2")),
                     rep(is_line())),
        :defs => rep(seq(is_heading(x -> isequal(x.value,"3")),
                         greedy(wiktionary_de_content...; alt=[:lines => is_line() ]))
                    );
        log = false
        ##,transform = (v,i) -> v
    ));

number_line = seq(Pair{String,Vector{LineContent}},
                  IteratorParser{String}("index", x->x isa Token && variable(x)==:number, (t,i) -> value(t)),
                  rep(IteratorParser{Token}("whitespace", x->x isa Token && !isinformative(x), (t,i) -> t)),
                  rep(IteratorParser{LineContent}("rest", x->x isa LineContent, (t,i) -> t));
                  transform=(v,i) -> v[1] => v[3]);


export wiki_meaning
function wiki_meaning(v;namespace = "wikt:de")
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
                rep(is_line(Pair{String,Vector{LineContent}},
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
        base = parse_overview(namespace, v.word,getval(val,:overview).second)
        meanings = [ ( word=base.word, order=m,
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


export promote_missing
function promote_missing(x)
    nt=Vector{Pair{Symbol, Type}}()
    for i in x
        for n in propertynames(i)
            t_ = fieldtype(typeof(i),n)
            t = get!(nt, n, t_)
            t != t_
            nt[n] = promote_type(t, t_)
        end
    end
    T=NamedTuple{tuple(keys(nt)...), Tuple{values(nt)...}}
    [ convert(T, i) for i in x ]
end


end # module
