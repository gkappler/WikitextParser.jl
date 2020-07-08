
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
                push!(r, intern(name) => [ intern(key) => intern(attrs[key]) for key in keys(attrs) ] )
                ignored = name in ignore
            end
            cbs.character_data = function(h, txt)
                if !ignored
                    push!(text,txt)
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
                name = intern(name)
                if !( name in value || name in record || name in ignore || name in types )
                    println("\n\n--\nWhat to do with $name? (r)record, (i)gnore, (v)alue\n$text\n$(r[end])")
                    strat = readline(stdin)
                    if strat == "r"
                        push!(record, name)
                    elseif strat == "i"
                        push!(ignore, name)
                    elseif strat == "v"
                        push!(value, name)
                    end
                end
                if name in value || isempty(r[end])
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
                        if :revision in propertynames(val) && :text in propertynames(val.revision) && !startswith(val.revision.text, "{{NOINDEX}}")
                            count = count + 1
                            f(val, count)
                        end
                    end
                elseif name in ignore
                    ig = pop!(r)
                else
                    error()
                end
            end
        end
        parsestream(unpack, cbs)
    end
end
