# Show git branch/status in prompt
function git_branch {
	echo -e $(git branch 2>/dev/null | awk '/^*/{ print $2 }')
}

