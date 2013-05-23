---
layout: post
title: Git script to rebase all child branches following a command
date: 2012-09-25 10:27
comments: true
tags: alias, git, git-svn, ruby, scripts
---

This script looks at the current status of the DAG to find the children of the
current branch, runs an action, then rebases those children.  It is particularly
useful for users of `git-svn`, who may find themselves having to rebase all
topic branches (and sub-topics which build off those) every time they `git svn
rebase` or `git svn dcommit`.

For pure git projects, this is considered by many to be bad form, so use with
discretion.  People who like a linear history might like it.

I expect the script, in its current state, will fail in cases where the rebase
can't be done automatically, but for simple day-to-day operations it makes
`git-svn` that bit less painful to use :-)

```ruby
#!/usr/bin/ruby
#GistID: 3779324

require 'escape'

exit if ARGV.empty?

current_branch = `git symbolic-ref -q HEAD`.sub(/^refs\/heads\//, "").strip
exit if current_branch.empty?

def branch_output_to_array(output)
	output.gsub(/^[ *]*/, "").split("\n").collect{ |e| e.strip }
end

IGNORED_BRANCHES = branch_output_to_array(`git branch --no-color -r`) << "HEAD"

def branches_on(commit)
	ignored = IGNORED_BRANCHES << commit

	log = `git log --pretty=%d --simplify-by-decoration #{commit} | head -n 1`
	branches = log.sub(/^ \(([^)]+)\).*$/, '\1').split(", ")
	branches.collect{ |e| e.strip }.reject{ |b| ignored.include? b }
end

def children_of(branch)
	c = branch_output_to_array(`git branch --no-color --contains #{branch}`)
	c.reject!{ |b| b == branch }

	grandchildren = c.collect{|c| children_of c}.flatten
	c.reject{ |b| grandchildren.include? b }
end

def branch_tree_from(branch)
	siblings = branches_on branch
	children = children_of(branch).reject{|c| siblings.include? c}

	tail = siblings.collect{|s| [s]} + children.collect{|c| branch_tree_from(c)}
	tail.empty? ? [branch] : [branch, tail]
end

def rebase_all_children(tree)
	parent = tree.shift
	children = tree.shift
	children.map do |e|
		system "git rebase #{parent} #{e.first}"

		if e.size > 1
			rebase_all_children e
		end
	end
end

initial_tree = branch_tree_from current_branch

if system "git #{Escape.shell_command(ARGV)}"
	rebase_all_children initial_tree
	system "git checkout #{current_branch}"
end
```

I have an alias set up to invoke it with `git rar` ("Run and Rebase"), so that I
can type, for example, `git rar svn rebase`.
