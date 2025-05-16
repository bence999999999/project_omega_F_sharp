namespace SzotanuloApp

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client


[<JavaScript>]
module Client =

    // A szóbejegyzés típus
    type WordEntry = {
        Id: System.Guid
        English: string
        Hungarian: string
        Learned: bool
    }

    type Page = WordList | Quiz | Stats

    type Model = {
        Words: list<WordEntry>
        Page: Page
        CurrentQuiz: option<WordEntry>
        RemainingQuiz: list<WordEntry>
        IncorrectAnswers: list<WordEntry>
    }

    let localStorageKey = "word-list"

    let loadWords () =
        let raw = JS.Window.LocalStorage.GetItem localStorageKey
        if raw = null then []
        else
            // minden szót újrakezd tanulatlanként
            Json.Deserialize<list<WordEntry>>(raw) |> List.map (fun w -> { w with Learned = false })

    let saveWords words =
        JS.Window.LocalStorage.SetItem(localStorageKey, Json.Serialize(words))

    let modelVar = Var.Create {
        Words = loadWords()
        Page = WordList
        CurrentQuiz = None
        RemainingQuiz = []
        IncorrectAnswers = []
    }

    let engInput = Var.Create ""
    let hunInput = Var.Create ""
    let answerVar = Var.Create ""
    let feedbackVar = Var.Create ""

    // egyszerű keverés 
    let shuffle list =
        let rnd = System.Random()
        list |> List.sortBy (fun _ -> rnd.Next())

    let handleAddWord _ =
        let eng = engInput.Value.Trim()
        let hun = hunInput.Value.Trim()
        if eng <> "" && hun <> "" then
            let entry = {
                Id = System.Guid.NewGuid()
                English = eng
                Hungarian = hun
                Learned = false
            }
            let updated = entry :: modelVar.Value.Words
            modelVar.Value <- { modelVar.Value with Words = updated }
            saveWords updated
            engInput.Value <- ""
            hunInput.Value <- ""

    let handleDeleteWord id _ =
        let updated = modelVar.Value.Words |> List.filter (fun w -> w.Id <> id)
        modelVar.Value <- { modelVar.Value with Words = updated }
        saveWords updated

    let resetLearnedStatus words =
        // ne legyen tanult szó
        words |> List.map (fun w -> { w with Learned = false })

    let startQuizFromWords words =
        let resetWords = words |> List.map (fun w -> { w with Learned = false })
        match shuffle resetWords with
        | [] -> ()
        | hd :: tl ->
            modelVar.Value <- {
                modelVar.Value with
                    Page = Quiz
                    CurrentQuiz = Some hd
                    RemainingQuiz = tl
                    IncorrectAnswers = []
            }

    let handleStartQuiz _ =
        let reset = resetLearnedStatus modelVar.Value.Words
        modelVar.Value <- { modelVar.Value with Words = reset }
        saveWords reset
        startQuizFromWords reset

    let handleStartIncorrectQuiz _ =
        startQuizFromWords modelVar.Value.IncorrectAnswers
    
    let handleNextQuizWord _ =
        match modelVar.Value.RemainingQuiz with
        | [] ->
            answerVar.Value <- ""
            feedbackVar.Value <- "Kvíz befejezve!"
            modelVar.Value <- { modelVar.Value with CurrentQuiz = None; RemainingQuiz = [] }
        | hd :: tl ->
            answerVar.Value <- ""
            feedbackVar.Value <- ""
            modelVar.Value <- { modelVar.Value with CurrentQuiz = Some hd; RemainingQuiz = tl }

    let handleReturnToList _ =
        modelVar.Value <- { modelVar.Value with Page = WordList; CurrentQuiz = None; RemainingQuiz = [] }

    let handleShowStats _ =
        modelVar.Value <- { modelVar.Value with Page = Stats }

    //Nézetek megoldása
    
    let wordInputView () =
        div [attr.``class`` "form-section"] [
            h3 [] [text "Új szó hozzáadása"]
            Doc.InputType.Text [attr.``class`` "input"; attr.placeholder "Angol szó"] engInput
            Doc.InputType.Text [attr.``class`` "input"; attr.placeholder "Magyar jelentés"] hunInput
            button [attr.``class`` "button add"; on.click (fun _ -> handleAddWord)] [text "➕ Hozzáad"]
        ]

    let wordListView () =
        div [attr.``class`` "container"] (
            [
                h2 [] [text "Szólista"]
                wordInputView()
            ] @
            (modelVar.Value.Words |> List.map (fun word ->
                div [attr.``class`` "word-row"] [
                    span [] [text (sprintf "%s - %s" word.English word.Hungarian)]
                    button [attr.``class`` "button delete"; on.click (fun _ -> handleDeleteWord word.Id)] [text "❌"]
                ]
            )) @ [
                button [attr.``class`` "button"; on.click (fun _ -> handleStartQuiz)] [text "▶️ Kvíz indítása"]
            ]
        )

    let statsView () =
        let total = modelVar.Value.Words.Length
        let learned = modelVar.Value.Words |> List.filter (fun w -> w.Learned) |> List.length
        let pct = if total = 0 then 0.0 else float learned / float total * 100.0

        div [attr.``class`` "container"] [
            h2 [] [text "📊 Statisztika"]
            div [attr.style "position: relative; width: 200px; height: 200px; margin: auto;"] [
                Doc.SvgElement "svg" [Attr.Create "width" "200"; Attr.Create "height" "200"; Attr.Create "viewBox" "0 0 32 32"] [
                    Doc.SvgElement "circle" [
                        Attr.Create "cx" "16"
                        Attr.Create "cy" "16"
                        Attr.Create "r" "15.915"
                        Attr.Create "fill" "none"
                        Attr.Create "stroke" "#eee"
                        Attr.Create "stroke-width" "5"
                    ] []
                    Doc.SvgElement "circle" [
                        Attr.Create "cx" "16"
                        Attr.Create "cy" "16"
                        Attr.Create "r" "15.915"
                        Attr.Create "fill" "none"
                        Attr.Create "stroke" "#00b894"
                        Attr.Create "stroke-width" "5"
                        Attr.Create "stroke-dasharray" (sprintf "%.1f %.1f" pct (100.0 - pct))
                        Attr.Create "stroke-dashoffset" "25"
                        Attr.Create "stroke-linecap" "round"
                        Attr.Create "transform" "rotate(-90 16 16)"
                    ] []
                ]
                div [attr.style "position:absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); font-weight: bold;"] [
                    text (sprintf "%.0f%%" pct)
                ]
            ]
            div [attr.style "margin-top:1rem;"] [text (sprintf "Megtanult szavak: %d / %d" learned total)]
            button [attr.``class`` "button"; on.click (fun _ -> handleStartIncorrectQuiz)] [text "🔁 Hibás szavak gyakorlása"]
            button [attr.``class`` "button secondary"; on.click (fun _ -> handleReturnToList)] [text "⬅️ Vissza"]
        ]

    let quizView () =
        match modelVar.Value.CurrentQuiz with
        | None ->
            div [attr.``class`` "container"] [
                p [] [Doc.BindView text feedbackVar.View]
                button [attr.``class`` "button secondary"; on.click (fun _ -> handleReturnToList)] [text "↩️ Vissza"]
                button [attr.``class`` "button"; on.click (fun _ -> handleShowStats)] [text "📊 Befejezés"]
            ]
        | Some word ->
            let handleCheckAnswer _ =
                if answerVar.Value.Trim().ToLower() = word.Hungarian.ToLower() then
                    feedbackVar.Value <- "✅ Helyes!"
                    let updatedWords =
                        modelVar.Value.Words |> List.map (fun w -> if w.Id = word.Id then { w with Learned = true } else w)
                    modelVar.Value <- { modelVar.Value with Words = updatedWords }
                    saveWords updatedWords
                else
                    feedbackVar.Value <- "❌ Helytelen! A jó válasz: " + word.Hungarian
                    modelVar.Value <- { modelVar.Value with IncorrectAnswers = word :: modelVar.Value.IncorrectAnswers }

            div [attr.``class`` "container"] [
                h3 [] [text (sprintf "Mi a jelentése ennek a szónak: %s?" word.English)]
                Doc.InputType.Text [attr.``class`` "input"; attr.placeholder "Írd be a magyar jelentést"] answerVar
                button [attr.``class`` "button add"; on.click (fun _ -> handleCheckAnswer)] [text "Ellenőrzés"]
                p [] [Doc.BindView text feedbackVar.View]
                button [attr.``class`` "button secondary"; on.click (fun _ -> handleNextQuizWord)] [text "🔁 Következő"]
                button [attr.``class`` "button secondary"; on.click (fun _ -> handleReturnToList)] [text "↩️ Vissza"]
                button [attr.``class`` "button"; on.click (fun _ -> handleShowStats)] [text "📊 Befejezés"]
            ]

    let mainView =
        Doc.BindView (fun model ->
            match model.Page with
            | WordList -> wordListView()
            | Quiz -> quizView()
            | Stats -> statsView()
        ) modelVar.View

    [<SPAEntryPoint>]
    let Main () =
        Doc.RunById "main" mainView
