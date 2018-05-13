# AntStar

# TODO v2.0

- Clear path
- Add cost to box on path in PointerSearch
- Generalize multiagent clearPath to singleagent
- Fix all pairs shortest path preprocessing
- Merge multiagent

# Problems

- Make sure they don't stand on goals (see `onGoal`)
- high weighted goal heurstic is important so agent can move away from box


# TODO

- Heuristic based on PointerProblem (test on SAEvilCorp.lvl)
- Multiagent
  - Output actions for all agent (NOP as default for agents doing nothing)
  - run previous actions at the same time
  - merge (oh boy)
- Goal ordering: make adjustments before solving goal greedily (see SAtest3.lvl)
- Run different goal sequence permutations in parallel


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
