#!/bin/sh
dotnet build
dotnet run --project src/PointerAnalyzer.fsproj -- x86_32