set JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF8

mkdir Java\build

mkdir Java\build\classes

javac -classpath %AMOS_HOME%\bin\javaamos.jar;%AMOS_HOME%\jarlib\appframework-1.0.3.jar;%AMOS_HOME%\jarlib\swing-worker-1.1.jar;Java\src -d Java\build\classes Java\src\grm\Main.java

call mkdmp