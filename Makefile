build:
	echo "scala 2.11.8 and sbt 2.11.8 should be installed"
	make clean
	sbt package
	mv ./target/scala-2.11/id3_2.11-1.0.jar id3.jar
run:
	scala id3.jar $(properties)
clean:
	echo rm -rf *.*~ .idea target *.unprunedRuleSet *.prunedRuleSetUnsorted *.unprunedRuleSetTest *.prunedRuleSetTest *.prunedRuleSetSorted *.tree *.training *.test *.validation
	rm -rf .idea *.unprunedRuleSet *.prunedRuleSetUnsorted *.unprunedRuleSetTest *.prunedRuleSetTest *.prunedRuleSetSorted *.tree *.training *.test *.validation
