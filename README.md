# Legislative Tables Tool

Written in F# requires dot.net core 6.0 or later, makes use of f# type providers to generate dotnet types from samples of the various feed files

## To Build

First restore the packet and fake cli tools using

`dotnet tool restore`

The code is built and packaged using the fake build.fsx script, issuing the following command will restore, compile and copy to the `./bundle` folder the executable together with the config and bat files used to run the various reports

`dotnet fake run .\build.fsx`


