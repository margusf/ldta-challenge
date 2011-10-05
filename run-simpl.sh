artifact="$1"
uppercase_artifact=`echo $artifact | tr '[:lower:]' '[:upper:]'`
shift
java -classpath "/home/margus/.m2/repository/org/antlr/antlr/3.3/antlr-3.3.jar:/home/margus/.m2/repository/org/antlr/antlr-runtime/3.3/antlr-runtime-3.3.jar:/home/margus/.m2/repository/org/antlr/stringtemplate/3.2.1/stringtemplate-3.2.1.jar:/home/margus/.m2/repository/antlr/antlr/2.7.7/antlr-2.7.7.jar:/home/margus/.m2/repository/org/scala-lang/scala-library/2.8.1/scala-library-2.8.1.jar:/home/margus/.m2/repository/org/scala-lang/scala-compiler/2.8.1/scala-compiler-2.8.1.jar:/home/margus/.m2/repository/simplicitas/simplicitas-tool/1.2.0/simplicitas-tool-1.2.0.jar:/home/margus/dev/dsl/ldta-challenge/target/oberon0-$artifact.jar" "ee.cyber.simplicitas.oberonexample.OberonMain${uppercase_artifact}" --dest target/test $*

