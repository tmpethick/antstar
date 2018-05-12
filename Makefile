
runonfile:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	cat AntStar/AntStar/levels/SAanagram.lvl > dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll

testonserver:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	java -jar environment/server.jar -g 300 -c"dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll" -l AntStar/AntStar/levels/testlevels/MAAgentObstacle.lvl

run:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	cat AntStar/AntStar/levels/SAanagram.lvl | xargs -I {} dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll {}
