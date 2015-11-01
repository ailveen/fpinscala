set SCRIPT_DIR=%~dp0
java -Xmx1024M -javaagent:D:/Dev/tools/jrebel/jrebel.jar -Xss8m -jar "%SCRIPT_DIR%sbt-launch.jar" -Dsbt.ivy.home=D:/Dev/repos/ivy %*

