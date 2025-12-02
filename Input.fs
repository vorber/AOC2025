namespace AOC

module Input  =
    open System
    open System.IO
    open System.Net.Http
    open FParsec

    //TODO: don't read all at once, use streams instead
    let private readFrom = File.ReadAllLinesAsync >> Async.AwaitTask

    let private tryReadFrom file =
        if Path.Exists file then Some (readFrom file) else None

    let private saveTo file (data:string option) =
        async {
            match data with
            | Some text -> do! File.WriteAllTextAsync(file, text) |> Async.AwaitTask
            | None -> failwith "KABOOM"
        }

    let private directory year =
        let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        home + "/src/aoc" + year

    let readToken year = 
        let path =(directory year) + "/token"
        printfn "%s" path
        (File.ReadAllText path).Trim()
            

    let private fetch year day =
        let url = sprintf "https://adventofcode.com/%s/day/%s/input" year day
        let token = readToken year//Environment.GetEnvironmentVariable("AOC_SESSION_TOKEN")
        task {
            use client = new HttpClient()
            client.DefaultRequestHeaders.Add("Cookie", "session="+token)
            try
                let! content = client.GetStringAsync(url)
                return Some content
            with 
            | ex ->
                printfn "Download failed with: %s" ex.Message
                return None
        }
     |> Async.AwaitTask

    let private filePath year day =
        let dir = directory year
        dir + "/inputs/day" + day + ".txt"

    let load year day =
        let file = filePath year day
        match tryReadFrom file with
        | Some str -> str 
        | None -> async {
            let! content = fetch year day
            do! saveTo file content
            return 
                match content with
                | Some text -> text.Split('\n')
                | None -> failwith "no content"
        }
                  
    let parse year day parser =
        let file = filePath year day
        if file |> Path.Exists |> not then
            async {
                let! content = fetch year day
                do! saveTo file content
            } |> Async.RunSynchronously
        else ()
        match runParserOnFile parser "" file System.Text.Encoding.UTF8 with
        | Success (res, _, _) -> res
        | Failure (err, _, _) -> failwithf "Failed to parse: %s" err

