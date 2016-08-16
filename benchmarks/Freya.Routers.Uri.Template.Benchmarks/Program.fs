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
open Freya.Testing

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
open BenchmarkDotNet.Diagnostics.Windows
open BenchmarkDotNet.Exporters
open BenchmarkDotNet.Jobs

let configuration =

    ManualConfig
        .Create(DefaultConfig.Instance)

         // Jobs

         // Legacy / Any

        .With(Job.Default
            .With(GarbageCollection (Server = true))
            .With(Jit.LegacyJit)
            .With(Platform.AnyCpu))

        // Ryu / Any

        .With(Job.Default
            .With(GarbageCollection (Server = true))
            .With(Jit.RyuJit)
            .With(Platform.AnyCpu))

        // Legacy / x86

        .With(Job.Default
            .With(GarbageCollection (Server = true))
            .With(Jit.LegacyJit)
            .With(Platform.X86))

        // Ryu / x86

        .With(Job.Default
            .With(GarbageCollection (Server = true))
            .With(Jit.RyuJit)
            .With(Platform.X86))

        // Legacy / x64

        .With(Job.Default
            .With(GarbageCollection (Server = true))
            .With(Jit.LegacyJit)
            .With(Platform.X64))

        // Ryu / x64

        .With(Job.Default
            .With(GarbageCollection (Server = true))
            .With(Jit.RyuJit)
            .With(Platform.X64))

        // Diagnostics

        // GC/Memory

        .With(MemoryDiagnoser ())

        // Inlining

        .With(InliningDiagnoser ())

        // Exporters

        .With(MarkdownExporter.GitHub)

// Main

[<EntryPoint>]
let main _ =

    let _ = BenchmarkRunner.Run<Benchmarks> (configuration)

    0
