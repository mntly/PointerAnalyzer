#!/bin/sh
dotnet test src/Domain.Tests/Domain.Tests.fsproj
#dotnet test src/Domain.Tests/Domain.Tests.fsproj --collect:"XPlat Code Coverage"