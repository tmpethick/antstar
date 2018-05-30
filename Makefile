runall:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Debug;Targets=Clean
	for filename in AntStar/AntStar/levels/testlevels/competition_levels/*.lvl; do
		echo "$$(cat $$filename)\n" | dotnet AntStar/AntStar/bin/Debug/netcoreapp2.0/AntStar.dll 
	done

runonfile:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Debug;Targets=Clean
	echo "$$(cat AntStar/AntStar/levels/competition_levels/MAKaldi.lvl)\n" | dotnet AntStar/AntStar/bin/Debug/netcoreapp2.0/AntStar.dll 

runonserver:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Debug;Targets=Clean
	java -jar environment/server.jar -g 150 -c"dotnet AntStar/AntStar/bin/Debug/netcoreapp2.0/AntStar.dll" -l AntStar/AntStar/levels/competition_levels/MAKaldi.lvl

runSAtests:
	./antstar.sh "AntStar/AntStar/levels/competition_levels/SA*.lvl"

runMAtests:
	./antstar.sh "AntStar/AntStar/levels/competition_levels/MA*.lvl"

runoncserver:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	java -jar environment/cserver.jar -d AntStar/AntStar/levels/competition_levels/ -c"dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll"
