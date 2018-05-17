runall:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	for filename in AntStar/AntStar/levels/testlevels/competition_levels/*.lvl; do
		echo "$$(cat $$filename)\n" | dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll 
	done

runonfile:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	echo "$$(cat AntStar/AntStar/levels/testlevels/competition_levelsSP17/MAFooBar.lvl)\n" | dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll 

testonserver:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	java -jar environment/server.jar -g 300 -c"dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll" -l AntStar/AntStar/levels/testlevels/competition_levelsSP17/MAFooBar.lvl
