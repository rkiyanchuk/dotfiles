# Generate Description for a Github Pull Request

You are tasked with generating a comprehensive pull request (PR) description following the repository's standard template.

Describe either the PR from current branch, or this one: #$ARGUMENTS

## Steps to follow:

1. **Read the PR description template:**

   - Check `.github/pull_request_template.md` location of the current repo to
       see if there is a PR template available.
   - Read the template carefully to understand all sections and requirements

2. **Analyze the changes in the PR**

    You can few the diff by using `gh` command:

    - `gh pr diff` - to view current PR diff
    - `gh pr diff {number}` - to view diff for a PR with particular number (#$ARGUMENTS).

3. **Identify the PR to describe:**

   - Check if the current branch has an associated PR: `gh pr view --json url,number,title,state 2>/dev/null`
   - If no PR exists for the current branch, or if on master, list open PRs: `gh pr list --limit 10 --json number,title,headRefName,author`
   - Ask the user which PR they want to describe

4. **Gather comprehensive PR information:**

   - Get the full PR diff: `gh pr diff {number}`
   - If you get an error about no default remote repository, instruct the user to run `gh repo set-default` and select the appropriate repository
   - Get commit history: `gh pr view {number} --json commits`
   - Review the base branch: `gh pr view {number} --json baseRefName`
   - Get PR metadata: `gh pr view {number} --json url,title,number,state`

5. **Analyze the changes thoroughly:** (think about the code changes, their architectural implications, and potential impacts)

   - Read through the entire diff carefully
   - For context, read any files that are referenced but not shown in the diff
   - Understand the purpose and impact of each change
   - Identify user-facing changes vs internal implementation details
   - Look for breaking changes or migration requirements

6. **Generate the description:**

   - Fill out each section from the template thoroughly:
     - Answer each question/section based on your analysis
     - Be specific about problems solved and changes made
     - Focus on user impact where relevant
     - Include technical details in appropriate sections
     - Write a concise changelog entry
   - Ensure all checklist items are addressed (checked or explained)

7. **Update the PR:**

   - Update the PR description directly: `gh pr edit {number} --body-file thoughts/prs/{number}_description.md`
   - Confirm the update was successful
   - If any verification steps remain unchecked, remind the user to complete them before merging

## Important notes:

- This command works across different repositories - always read the local template
- Be thorough but concise - descriptions should be scannable
- Focus on the "why" as much as the "what"
- Include any breaking changes or migration notes prominently
- If the PR touches multiple components, organize the description accordingly
- When describing the purpose of the change, use imperative form instead of descriptive

