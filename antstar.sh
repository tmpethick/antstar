#!/bin/sh
args=("$@")
MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
if [ $args[0] == '-SAcomp' ]
then
	for f in AntStar/AntStar/levels/testlevels/competition_levelsSP17/SA*
	do
		echo "-lvl" $f
		dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll "-lvl" $f 
	done
elif [ $args[0] == '-MAcomp' ]
then
	for f in AntStar/AntStar/levels/testlevels/competition_levelsSP17/MA*
	do
		echo "-lvl" $f
		dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll "-lvl" $f 
	done
elif [ $args[0] == '-SAtest' ]
then
	for f in AntStar/AntStar/levels/testlevels/SA*
	do
		echo "-lvl" $f
		dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll "-lvl" $f 
	done
elif [ $args[0] == '-MAtest' ]
then
	for f in AntStar/AntStar/levels/testlevels/MA*
	do
		echo "-lvl" $f
		dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll "-lvl" $f 
	done
fi

