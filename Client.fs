namespace SzotanuloApp

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client

[<JavaScript>]
module Client =

    type WordEntry = {
        Id: System.Guid
        English: string
        Hungarian: string
        Learned: bool
    }

    type Page =
        | WordList
        | Quiz
        | Stats

    type Model = {
        Words: list<WordEntry>
        Page: Page
        CurrentQuiz: option<WordEntry>
        RemainingQuiz: list<WordEntry>
        IncorrectAnswers: list<WordEntry>
    }

    let localStorageKey = "word-list"

    let loadWords () =
        match JS.Window.LocalStorage.GetItem localStorageKey with
        | null -> []
        | json ->
            Json.Deserialize<list<WordEntry>>(json)
            |> List.map (fun w -> { w with Learned = false })

    let saveWords (words: list<WordEntry>) =
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

    let shuffle list =
        let rnd = System.Random()
        list |> List.sortBy (fun _ -> rnd.Next())

    let handleAddWord (_: Dom.MouseEvent) =
        if engInput.Value <> "" && hunInput.Value <> "" then
            let entry = {
                Id = System.Guid.NewGuid()
                English = engInput.Value.Trim()
                Hungarian = hunInput.Value.Trim()
                Learned = false
            }
            let newWords = entry :: modelVar.Value.Words
            modelVar.Value <- { modelVar.Value with Words = newWords }
            saveWords newWords
            engInput.Value <- ""
            hunInput.Value <- ""
            ()
        else
            ()

    let handleDeleteWord (idToDelete: System.Guid) (_:Dom.MouseEvent) =
        let updated = modelVar.Value.Words |> List.filter (fun w -> w.Id <> idToDelete)
        modelVar.Value <- { modelVar.Value with Words = updated }
        saveWords updated

    let resetLearnedStatus words =
        words |> List.map (fun w -> { w with Learned = false })

    let startQuizFromWords (words: list<WordEntry>) =
        let resetInputWords = words |> List.map (fun w -> { w with Learned = false })
        match shuffle resetInputWords with
        | [] -> ()
        | hd :: tl ->
            modelVar.Value <- {
                modelVar.Value with
                    Page = Quiz
                    CurrentQuiz = Some hd
                    RemainingQuiz = tl
                    IncorrectAnswers = []
            }

    let handleStartQuiz (_: Dom.MouseEvent) =
        let resetWords = resetLearnedStatus modelVar.Value.Words
        modelVar.Value <- { modelVar.Value with Words = resetWords }
        saveWords resetWords
        startQuizFromWords resetWords
        

    let handleStartIncorrectQuiz (_: Dom.MouseEvent) =
        startQuizFromWords modelVar.Value.IncorrectAnswers

    let handleNextQuizWord (_: Dom.MouseEvent) =
        match modelVar.Value.RemainingQuiz with
        | [] ->
            answerVar.Value <- ""
            feedbackVar.Value <- "You've finished the quiz!"
            modelVar.Value <- { modelVar.Value with CurrentQuiz = None; RemainingQuiz = [] }
        | hd :: tl ->
            answerVar.Value <- ""
            feedbackVar.Value <- ""
            modelVar.Value <- { modelVar.Value with CurrentQuiz = Some hd; RemainingQuiz = tl }

    let handleReturnToList (_: Dom.MouseEvent) =
        modelVar.Value <- { modelVar.Value with Page = WordList; CurrentQuiz = None; RemainingQuiz = [] }

    let handleShowStats (_: Dom.MouseEvent) =
        modelVar.Value <- { modelVar.Value with Page = Stats }

    let wordInputView () =
        div [attr.``class`` "form-section"] [
            h3 [] [text "Add new word"]
            Doc.InputType.Text [attr.``class`` "input"; attr.placeholder "English word"] engInput
            Doc.InputType.Text [attr.``class`` "input"; attr.placeholder "Hungarian meaning"] hunInput
            button [attr.``class`` "button add"; on.click (fun _ -> handleAddWord)] [text "➕ Add"]
        ]

    let wordListView () =
        div [attr.``class`` "container"] [
            h2 [] [text "Word-list"]
            wordInputView()
            yield!
                modelVar.Value.Words
                |> List.map (fun word ->
                    div [attr.``class`` "word-row"] [
                        span [] [text (sprintf "%s - %s" word.English word.Hungarian)]
                        button [
                            attr.``class`` "button delete"
                            on.click (fun _ -> handleDeleteWord word.Id)
                        ] [text "❌"]
                    ]
                )
            button [
                attr.``class`` "button"
                on.click (fun _ -> handleStartQuiz)
            ] [text "▶️ Start Quiz"]
        ]

    let statsView () =
        let total = modelVar.Value.Words.Length
        let learned = modelVar.Value.Words |> List.filter (fun w -> w.Learned) |> List.length
        let learnedPct = if total = 0 then 0.0 else float learned / float total * 100.0

        div [attr.``class`` "container"] [
            h2 [] [text "📊 Summary"]
            div [attr.style "position: relative; width: 200px; height: 200px; margin: auto;"] [
                Doc.SvgElement "svg" [
                    Attr.Create "width" "200"
                    Attr.Create "height" "200"
                    Attr.Create "viewBox" "0 0 32 32"
                ] [
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
                        Attr.Create "stroke-dasharray" (sprintf "%.1f %.1f" learnedPct (100.0 - learnedPct))
                        Attr.Create "stroke-dashoffset" "25"
                        Attr.Create "stroke-linecap" "round"
                        Attr.Create "transform" "rotate(-90 16 16)"
                    ] []
                ]
                div [attr.style "position:absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); font-weight: bold;"] [
                    text (sprintf "%.0f%%" learnedPct)
                ]
            ]
            div [attr.style "margin-top:1rem;"] [
                text (sprintf "Learned words: %d / %d" learned total)
            ]
            button [
                attr.``class`` "button"
                on.click (fun _ -> handleStartIncorrectQuiz)
            ] [text "🔁 Practice incorrect answers"]
            button [
                attr.``class`` "button secondary"
                on.click (fun _ -> handleReturnToList)
            ] [text "⬅️ Back to the main page"]
        ]

    let quizView () =
        match modelVar.Value.CurrentQuiz with
        | None ->
            div [attr.``class`` "container"] [
                p [] [Doc.BindView text feedbackVar.View]
                button [
                    attr.``class`` "button secondary"
                    on.click (fun _ -> handleReturnToList)
                ] [text "↩️ Back"]
                button [
                    attr.``class`` "button"
                    on.click (fun _ -> handleShowStats)
                ] [text "📊 Finish learning"]
            ]
        | Some word ->
            let handleCheckAnswer (_: Dom.MouseEvent) =
                if answerVar.Value.Trim().ToLower() = word.Hungarian.ToLower() then
                    feedbackVar.Value <- "✅ Correct!"
                    let updatedWords =
                        modelVar.Value.Words
                        |> List.map (fun w ->
                            if w.Id = word.Id then { w with Learned = true } else w
                        )
                    modelVar.Value <- { modelVar.Value with Words = updatedWords }
                    saveWords updatedWords
                else
                    feedbackVar.Value <- "❌ Wrong! The right answer is: " + word.Hungarian
                    modelVar.Value <- { modelVar.Value with IncorrectAnswers = word :: modelVar.Value.IncorrectAnswers }

            div [attr.``class`` "container"] [
                h3 [] [text (sprintf "What's  the meaning of this word: %s?" word.English)]
                Doc.InputType.Text [attr.``class`` "input"; attr.placeholder "Write the answer in hungarian"] answerVar
                button [attr.``class`` "button add"; on.click (fun _ -> handleCheckAnswer)] [text "Check answer"]
                p [] [Doc.BindView text feedbackVar.View]
                button [
                    attr.``class`` "button secondary"
                    on.click (fun _ -> handleNextQuizWord)
                ] [text "🔁 Next word"]
                button [
                    attr.``class`` "button secondary"
                    on.click (fun _ -> handleReturnToList)
                ] [text "↩️ Back"]
                button [
                    attr.``class`` "button"
                    on.click (fun _ -> handleShowStats)
                ] [text "📊 Finish learning"]
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
