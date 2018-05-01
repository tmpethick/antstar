
runtests:
	MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
	dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll

runtestfiles:
	./antstar.sh
