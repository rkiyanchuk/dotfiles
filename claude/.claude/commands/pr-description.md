# Command to create Github Pull Request Description

Create comprehensive description of the Pull Request (PR).

Describe either the PR from current branch, or this one: #$ARGUMENTS

## Steps

1. Analyze the changes in the PR.

You can few the diff by using `gh` command:

-  `gh pr diff` - to view current PR diff
-  `gh pr diff #123` - to view diff for a PR with particular number (#$ARGUMENTS).

2. Document the changes

- Purpose and context of the changes
- Key modifications made

- Technical implementation details
- Impact on the system
- Any breaking changes or migration notes

- Related issues or follow-up tasks
- Tests created for the change

3. Ask for additional information if needed.

## Instructions

- Keep the description concise and well formatted.
- When describing the purpose of the change, use imperative form instead of
    descriptive.
- If there is nothing to specify in a section, just skip it.
