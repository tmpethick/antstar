# AntStar

## Build

```
MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean<Paste>
```

## Run

Run the project with dotnet from CLI

```
dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll [options] [file ...]
```

Run all test levels with script

```
./antstar.sh
```

### Options

-lvl		Level file

## Notes

Order by setting all goals (except goals already in "solvable") to walls expect one (removing all boxes and agents
If it can be solved add to "solvable" ordered set.^M
n^2^M
Ordering: https://www.ijcai.org/Proceedings/89-2/Papers/013.pdf^M
