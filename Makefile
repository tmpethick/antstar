
runonfile:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	echo "$$(cat AntStar/AntStar/levels/competition_levels/SAAiAiCap.lvl)\n" | dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll 

testonserver:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
<<<<<<< HEAD
	java -jar environment/server.jar -g 300 -c"dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll" -l AntStar/AntStar/levels/competition_levels/SAAlphaOne.lvl
=======
	java -jar environment/server.jar -g 50 -c"dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll" -l AntStar/AntStar/levels/competition_levels/MACybot.lvl
>>>>>>> d63abd6743cbf7c7b7fbcfed88bc05373b1abd43

runSAtests:
	./antstar.sh "AntStar/AntStar/levels/competition_levels/SA*.lvl"

runMAtests:
	./antstar.sh "AntStar/AntStar/levels/competition_levels/MA*.lvl"
