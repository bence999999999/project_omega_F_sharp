open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open WebSharper.AspNetCore
open SzotanuloApp // Ez a neved a `Client.fs`-ben definiált névtérhez

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    
    // Add services to the container.
    builder.Services
        .AddWebSharper()
        .AddAuthentication("WebSharper")
        .AddCookie("WebSharper", fun _ -> ())
    |> ignore

    let app = builder.Build()

    // Configure the HTTP request pipeline.
    if not (app.Environment.IsDevelopment()) then
        app.UseExceptionHandler("/Error")
            .UseHsts()
        |> ignore
    
    app.UseHttpsRedirection()
#if DEBUG        
        .UseWebSharperScriptRedirect(startVite = true)
#endif
        .UseDefaultFiles()
        .UseStaticFiles()
        .UseWebSharper() // FONTOS: Ez teszi elérhetővé a SPA-t
    |> ignore 
       
    app.Run()

    0 // Exit code