using WikitextParser
using Test

@testset "WikitextParser.jl" begin
    # Write your own tests here.
    include("test-wiktionary.jl")
end


@testset "parentheses" begin
    wt = wikitext(parentheses=true)
    @test parse(wt, "(balanced parenthesis)") ==
        aline(TokenPair(:paren,AbstractToken[l"balanced",delim" ", l"parenthesis"]))

    @test parse(wt, "(unclosed parenthesis") ==
        aline(Token(:paren,"("),l"unclosed",delim" ", l"parenthesis")

    @test parse(wt, "unopened parenthesis)") ==
        aline(l"unopened",delim" ", l"parenthesis",Token(:paren,")"))

    @test parse(wt, ")") == aline(Token(:paren,")"))

    @test parse(wt, "{{t|(balanced parenthesis)}}") ==
        aline(Template("t",[""=>aline(TokenPair(
            :paren,AbstractToken[l"balanced", delim" ", l"parenthesis"]))]))
end


@testset "wikitext links" begin
    @test parse(wiki_link(AnyChar()), "[[Test|1]]") ==
        WikiLink("wikt:de", "Test", "", "1") 

    @test parse(wiki_link(), "[[Test]]") ==
        WikiLink("wikt:de", "Test", "", "")

    @test parse(wiki_link(), "[[Texas|Lone Star State]]") ==
        WikiLink("wikt:de", "Texas", "", "Lone Star State")
    # @test parse(wiki_link, "[[Texas#Racism|Lone Star State]]") ==
    # (target="Texas",section="Racism",title="Lone Star State")

    ## label should be parsed for styling info
    @test parse(wiki_link(), "[[File:wiki.png|thumb|Wikipedia logo]]") ==
        WikiLink("File", "wiki.png", "", "thumb|Wikipedia logo")

    @test parse(wiki_external_link,
                "[http://www.bictxt.de/Schule]") ==
                    TokenPair(:link,[T"http"protocol, T"://"delimiter, T"www.bictxt.de"domain,
                                     T"/"delimiter,T"Schule"capitalized])
    ## todo: bare urls

    @test parse(wikitext(parentheses=true), "([[Reich]]e)") ==
        aline(TokenPair(:paren,AbstractToken[WikiLink("wikt:de","Reich","","Reiche")]))
end

import WikitextParser: expand_numbers
@testset "number references" begin
    @test parse(expand_numbers,"[1]") == ["1"]
    @test parse(expand_numbers,"[1-3]") == ["1","2","3"]
    @test parse(expand_numbers,"[1-3,10]") == ["1","2","3","10"]
end


@testset "html" begin
    wtp = wikitext();
    ## TODO: profile and optimize
    x = wtp("<ref name=\"Georges\">{{Ref-Georges}}</ref>")
    @test x == [ Line(Token[],AbstractToken[
        Node(:ref, Token[ Token(:name,"Georges") ],
             [ Line(Token[], AbstractToken[Template{Token,AbstractToken}(
                 "Ref-Georges",[])] ) ] ) ] ) ]
    @test string(x)=="<ref name=\"Georges\">{{Ref-Georges}}</ref>"
end



@testset "wikitext templates" begin 
    @test parse(wikitext(),"{{}}")==[Line(NamedString[], AbstractToken[Template("")])]
    @test parse(wikitext(),"|")==[Line(NamedString[], AbstractToken[Token(:delimiter,"|")])]
    @test parse(wikitext(), "{{test}}")[1] == Line(LineContent[Template("test")])
    parse(wikitext(), "{{Ü-Tabelle|Ü-links|Ü-rechts}}") ==
        aline(Template("Ü-Tabelle",[""=>aline(l"Ü",delim"-",l"links"),
                                    ""=>aline(l"Ü",delim"-",l"rechts")]))
end
