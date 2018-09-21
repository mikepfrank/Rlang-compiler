diffs: clean
	-diff -rcw ../src ../src2 --exclude='README.*' > context-diff.txt
	-diff -ruw ../src ../src2 --exclude='README.*' > unified-diff.txt
	-diff -ryw ../src ../src2 --width=260 --exclude='README.*' > side-diff.txt

clean:
	rm -f *-diff.txt
