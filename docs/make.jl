push!(LOAD_PATH, "../src")

using Documenter, MiniEvents, MiniEvents.EventLists, MiniEvents.Scheduler

#cp("../README.md", "src/README.md")

makedocs(sitename="MiniEvents.jl",
        format   = Documenter.HTML(
            prettyurls = get(ENV, "CI", nothing) == "true",
            warn_outdated = true,
            collapselevel = 1
            ),
    	checkdocs=:exports,
        modules = [EventLists, Scheduler, MiniEvents],
        pages=[ "Introduction" => "README.md",
                        "EventLists" => "eventlists.md",
                        "Scheduler" => "scheduler.md",
                        "MiniEvents" => "minievents.md",
                        "Index" => "index.md"])
                        
deploydocs(
    repo = "github.com/mhinsch/MiniEvents.jl.git",
    )
