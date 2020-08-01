module WikitextParser
using CombinedParsers
import CombinedParsers: Repeat_max
using CombinedParsers.Regexp
import CombinedParsers.Regexp: word, whitespace_maybe, whitespace_horizontal
using CombinedParserTools
import CombinedParserTools: newline, inline, whitespace_newline, emptyline
using CombinedParserTools.Tokens
import CombinedParserTools.Tokens: html, attributes, LineContent

import TextParse
import TextParse: Numeric


include("types.jl")

newlinetoken = instance(Token,!!newline,:delimiter)

import CombinedParserTools: word, footnote, quotes, capitalized, delimiter
simple_tokens =
    @annotate [ :number     => !!re"[0-9]+"
                ## , !!capitalized
                , :literal  => !!word
                , :delimiter => !!delimiter
                , :footnote => !!footnote
                , :quote    => !!quotes
                , :paren    => !!re"[][{}()<>]"
                , :operator => !!re"[-+*/%&!=]"
                , :unknown  => !!re"[^][(){}\n \t\|]"
                , :delimiter => !!newline
                ]
dash = re" *[–-] *"
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

@syntax expand_numbers = Sequence(2,"[",numbers,"]");

list_item = Sequence(1, !!re"[*]+|[#]+", whitespace_horizontal) do v
    NamedString[ NamedString(:list,v) ]
end

wiki_link(token=AnyChar();namespace = "wikt:de") = 
    with_name(
        :wikilink,
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
    )

wiki_parentheses = Dict{Any,Any}(
    pairs(CombinedParserTools.Tokens.parentheses)...,
    :italics=>("''","''"),
    :bold=>("'''","'''"),
    :bolditalics=>("''''","''''")
)

import CombinedParserTools.Tokens: attribute_parser
attr_line(inner_word,stop=newline) =
    map(Atomic(Repeat_stop(
        Either(attribute_parser, inner_word),stop))) do v
            result_type(inner_word)[ t
              for t in v #parse(Repeat(el),v)
              if isinformative(t) ]
        end


function table_cell_parser(inner_word::TextParse.AbstractToken{<:AbstractToken}, node="td")
    A = result_type(inner_word)
    T = Line{NamedString,A}
    table_delim = CharIn("!|")
    inner = lines_stop(inner_word; until=table_delim)
    with_name(
        :table_cell,
        Sequence(
            Optional(
                with_name(
                    :table_attr,
                    Sequence(
                        1,
                        attr_line(inner_word,Either(newline,"|")),
                        Sequence("|", NegativeLookahead("|"))))),
            map(T,
                Repeat_stop(inner_word,
                            Either("\n","||","!!")))
            , Optional( (
                Sequence(3, newline, NegativeLookahead(table_delim), inner)))
            , Repeat(instance(Token,!!newline,:delimiter))) do v
        Node( node, v[1], T[ v[2], v[3]..., T(v[4]) ] )::Node{A,T}
        end
    )
end

function table_cell_parsers(inner_word)
    A = result_type(inner_word)
    T = Line{NamedString,A}
    with_name(
        :table_cells,
        Sequence(
            Repeat(Sequence(1,parenthesisP(:htmlcomment, inner_word),newline)),
            Either(
                Sequence(2,"!",table_cell_parser(inner_word, "th")),
                Sequence(2,re"\|(?![-}])",table_cell_parser(inner_word, "td"))
            ),
            Repeat(
                Either(
                    Sequence(2,"!!",table_cell_parser(inner_word, "th")),
                    Sequence(2,"||",table_cell_parser(inner_word, "td"))))) do v
        pushfirst!(v[3],v[2])
        pushfirst!(v[3][1].children,T(v[1]))
        v[3]
        end
    )
end

function table_parser(inner_word)
    ## N = Node{Node{eltype(result_type(inner))}}
    N = AbstractToken
    stops = ("\n|","|","!")
    inner_partial_html = Sequence(
        2,
        NegativeLookahead(Either(stops)),
        Either(
            sloppyhtml(inner_word, stop=stops),
            inner_word))
    with_name(
        :table,
        Sequence(
            "{|", Sequence(1,attr_line(inner_partial_html), newline)
            # 3
            , Optional(Sequence(
                2, "|+",
                Repeat_until(inner_word,newline)))
            # 4 
            , Repeat(table_cell_parsers(inner_partial_html))
            , Repeat(
                Sequence(
                    re"\|-+", Sequence(1,attr_line(inner_partial_html), newline),
                    Repeat(table_cell_parsers(inner_partial_html))) do v
                Node{AbstractToken,N}("tr", v[2], vcat(v[3]...))
                end),
            Repeat(whitespace_newline),
            "|}") do v
        if !isempty(v[4])
        pushfirst!(v[5],
                   Node{AbstractToken,N}("tr", AbstractToken[], vcat(v[4]...)))
        end
        if !isempty(v[3])
        pushfirst!(v[5],
                   Node{AbstractToken,N}("caption", AbstractToken[], vcat(v[3]...)))
        end
        Node{AbstractToken,N}("table",v[2], v[5])
        end
    )
end

## from https://phabricator.wikimedia.org/source/mediawiki/browse/REL1_29/includes/Sanitizer.php
valid_html_tags = (
    htmlpairsStatic =
    [ # Tags that must be closed
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
    htmlsingleonly =
    [ # Elements that cannot have close tags
      "br", "wbr", "hr", "meta", "link"
      ],
    htmlnest =
    [ # Tags that can be nested--??
      "table", "tr", "td", "th", "div", "blockquote", "ol", "ul",
      "li", "dl", "dt", "dd", "font", "big", "small", "sub", "sup", "span",
      "var", "kbd", "samp", "em", "strong", "q", "ruby", "bdo"
      ],
    tabletags =
    [ # Can only appear inside table, we will close them
      "td", "th", "tr",
      ],
    htmllist =
    [ # Tags used by list
      "ul", "ol",
      ],
    listtags =
    [ # Tags that can appear in a list
      "li",
      ],
    ref = [ "ref", "references" ],
    transclusiontags =
    [ "noinclude", "onlyinclude", "includeonly" ]
)

anyhtmltag=!caseless(Either(vcat(valid_html_tags...)))

function simple_tokens_intag(tag::AbstractString)
    tag_ = Regcomb(tag,"i")
    Sequence(Sequence('<',tag_,'>'),
             Repeat_until(
                 wikitokens,
                 Sequence("</",tag_,'>'))) do v
                     TokenPair(Symbol(tag), v[2])
                 end
end


sloppyhtml(inner_token; stop=tuple()) =
    with_name(
        :html,
        Either(
            Sequence('<',!Either(valid_html_tags.htmlsingleonly), re" */?>") do v
            Token(:html, "<"*lowercase(v[2])*"/>")
            end,
            simple_tokens_intag("nowiki"),
            simple_tokens_intag("math"),
            simple_tokens_intag("pre"),
            simple_tokens_intag("code"),
            parenthesisP(:htmlcomment, Either(inner_token,newlinetoken), stop),
            html(Line{NamedString,AbstractToken},
                 !anyhtmltag) do until
            Sequence(
                1,
                lines_stop(inner_token; until = Either(tuple(stop...,caseless(until)))),
                Optional(caseless(until)))
            end)
    )

using CombinedParsers.Regexp
# Line{NamedString,LineContent},
wiki_list(wikitext;until) = Sequence(
    # :indent =>
    list_item,
    # :tokens =>
    Repeat_stop(wikitext, Either(newline,until))
) do v
    Line(v[1], v[2])::Line{NamedString,LineContent}
end

function whitespace_prefix(v) 
    if v==""
        NamedString[]
    else
        NamedString[NamedString(:whitespace," "^length(v))]
    end
end
wiki_content(wikitext;until) =
    Sequence(
        map(whitespace_prefix,!re":*"),
        Repeat_stop(wikitext,Either(newline,until))) do v
            Line(v[1], v[2])::Line{NamedString,result_type(wikitext)}
        end
wiki_lines(wikitext;kw...) = [ wiki_list(wikitext;kw...), wiki_content(wikitext;kw...) ];

function parenthesisP(name::Symbol, wikitext, open::String, close=open, stops=tuple())
    with_name(
        name,
        Sequence(# TokenPair{Symbol, Vector{LineContent}},
                 open, Repeat_until(wikitext, sEither(close,stops...))) do v
        TokenPair(name,v[2])
        end)
end

function parenthesisP(name::Symbol, wikitext,stops=tuple())
    parenthesisP(name, wikitext, wiki_parentheses[name]...,stops)
end

lines_stop(wikitext;until) =
    join(
        Either(
            ( w for w in wiki_lines(wikitext,until=until))...
        ),
        !Sequence(NegativeLookahead(until),newline)) do v
            for e in 1:lastindex(v)-1
                push!(v[e].tokens,Token(:delimiter,"\n"))
            end
            v
        end


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
    stop_at=CharIn("|}")
    with_name(
        :template,
        Sequence(
            "{{",NegativeLookahead('{'),
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
        Template(v[3],v[4])::Template
        end
    )
end

function template_parameter(wikitext)
    inner = lines_stop(wikitext,until=CharIn("|}"))
    with_name(
        :template_parameter,
        Sequence(
            "{{{",
            Repeat_stop(Atomic(wikitext),CharIn("|}")),
            Optional(Atomic(Sequence(2,"|", inner))),
            "}}}") do v
        TokenPair(v[2],v[3])
        end)
end

using InternedStrings
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

wikitokens = Either(
    simple_tokens,
    instance(Token, !!AnyChar(), :unknown))

linkparser = map(
    Sequence(!!re"https?|ftp",
             "://",
             !!re"[-[:alpha:][:digit:]?=&#+\._%:]+",
             !!Optional(re"/[-:,;~\$\p{L}[:digit:]?=&#+\./_%()*!|]*"))) do v
                 query = v[4] === nothing ? Token[] : parse(Repeat(wikitokens), v[4])
                 TokenPair(:link, Token[ Token(:protocol, v[1]),
                                         delim"://",
                                         Token(:domain, v[3]),
                                         query... ])
             end

wiki_external_link = Sequence(2,'[',linkparser,']')


htmlescape = instance(Token, !!re"&(?:[[:alpha:]]+|#[[:digit:]]+);"i, :htmlescape)
export wikitoken, wikitext, valid_html_tags

"""
    wikitoken(;namespace = "wikt:de", parentheses=false, quotes=false)

Parser for a single text element in wikitext, possibly nested.
- `parentheses=true` nests parentheses in disabled by default due to issues when unclosed in long texts.
   (TODO: max-length parsing - second pass?)

"""
function wikitoken(;namespace = "wikt:de", parentheses=false, quotes=false)
    wikitext=Either{LineContent}(
        bracket_number, ## todo: make a line type? see ordo [5]a-c
        bracket_reference,
        map(
            join(Repeat(2,Repeat_max,!!word), "·")) do v
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


    if parentheses
        push!(wikitext, parenthesisP(:paren, wikitext)) ## used for filtering from wiki word in meaning 
        push!(wikitext, parenthesisP(:bracket, wikitext))
        push!(wikitext, parenthesisP(:curly, wikitext))
        push!(wikitext, parenthesisP(:angle, wikitext))
    end
    push!(wikitext, parenthesisP(:bolditalics, wikitext, tuple(newline)))
    push!(wikitext, parenthesisP(:bold, wikitext,tuple(newline)))
    push!(wikitext, parenthesisP(:italics, wikitext,tuple(newline)))
    if quotes
        push!(wikitext, parenthesisP(:quote, wikitext))
        push!(wikitext, parenthesisP(:squote, wikitext))
        push!(wikitext, parenthesisP(:german_quote, wikitext))
    end
    
    push!(wikitext, wikitokens)
    MemoizingParser(wikitext)
end

function append_textblock_token(v)
    push!(v[1].tokens, Token(:delimiter, intern(v[2])))
    v[1]
end

function wikitext(;namespace = "wikt:de",kw...)
    function append_textblock_token_(r, v, nl)
        if v !== missing
            if nl !== missing
                push!(v.tokens, Token(:delimiter, intern(nl)))
            end
        end
        push!(r,v)
    end
    wikitext=wikitoken(;namespace=namespace,kw...)
    textblock = join(
        Either(
            vcat([ heading(level, wikitext) for level in reverse(1:6) ], wiki_lines(wikitext;until=Never()))
        ),
        !newline) do v
            for e in 1:lastindex(v)-1
                push!(v[e].tokens,Token(:delimiter,"\n"))
            end
            v
        end
    wikitextParser = textblock
end

include("bz2.jl")

include("wiktionary-de.jl")


end # module
