module Freya.Routers.Uri.Template.Benchmarks
// Freya

open Freya.Core
open Freya.Routers.Uri.Template

let value i =
    freya {
        do! Freya.Optic.set (State.value_ "test") (Some i) }

let router =
    freyaRouter {
        resource "/" (value 0)
        resource "/one" (value 1)
        resource "/one/{two}" (value 2) }

let system =
    Freya.infer router

// Benchmark

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Freya.Optics.Http
open Freya.Testing.Evaluation

let path p =
    freya {
        do! Freya.Optic.set Request.path_ p }

let paths =
    List.map path [
        "/"
        "/one"
        "/one/two" ]

type Benchmarks () =

    [<Benchmark>]
    member __.Basic () =
        List.map (flip evaluate system) paths

// Configuration

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Diagnosers
#if !NETCOREAPP2_0
open BenchmarkDotNet.Diagnostics.Windows
#endif
open BenchmarkDotNet.Exporters
open BenchmarkDotNet.Jobs

let configuration =

    ManualConfig
        .Create(DefaultConfig.Instance)
        .With(Job.MediumRun.WithGcServer(true).WithGcConcurrent(true))
        .With(MemoryDiagnoser.Default)
#if !NETCOREAPP2_0
        .With(InliningDiagnoser ())
#else
#endif
        .With(MarkdownExporter.GitHub)

// Main

[<EntryPoint>]
let main _ =

    let _ = BenchmarkRunner.Run<Benchmarks> (configuration)

    0
