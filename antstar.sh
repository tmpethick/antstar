#!/bin/sh
# MSBuild AntStar/AntStar/AntStar.fsproj /p:Configuration=Release;Targets=Clean
INDEX=0
declare -a ARRAY
for f in $1
do
    echo $f
	ARRAY[INDEX]=$( time ( echo "$(cat $f)\n" | dotnet AntStar/AntStar/bin/Release/netcoreapp2.0/AntStar.dll 2>&1 ) 3>&1 1>&2 2>&3 )
    INDEX=$(expr $INDEX + 1)
done
echo ${ARRAY[*]}