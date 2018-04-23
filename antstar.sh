#!/bin/sh
FILES=AntStar/AntStar/levels/testlevels/*
args=("$@")
MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
for f in $FILES
do
	echo "-lvl" $f
	dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll "-lvl" $f 
done
