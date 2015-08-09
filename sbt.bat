set SCRIPT_DIR=%~dp0
java -Xmx512M -Xss8m -jar "%SCRIPT_DIR%sbt-launch.jar" -Dsbt.ivy.home=D:/Dev/repos/ivy %*

