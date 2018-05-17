
runonfile:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	echo "$$(cat AntStar/AntStar/levels/competition_levels/SAAiAiCap.lvl)\n" | dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll 

testonserver:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	java -jar environment/server.jar -g 300 -c"dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll" -l AntStar/AntStar/levels/competition_levels/SAAlphaOne.lvl

runSAtests:
	./antstar.sh "AntStar/AntStar/levels/competition_levels/SA*.lvl"

runMAtests:
	./antstar.sh "AntStar/AntStar/levels/competition_levels/MA*.lvl"
