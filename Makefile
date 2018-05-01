
runtests:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll

runtestfiles:
	./antstar.sh

testonserver:
	java -jar ../environment/server.jar -g 300 -c"dotnet ../antstar/AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll -lvl ../antstar/AntStar/Antstar/levels/SAanagram.lvl" -l ../environment/levels/SAanagram.lvl
